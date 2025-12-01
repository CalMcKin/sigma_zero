# ==============================================================================
# Difference-in-Differences Regression Analysis
# Course: SYSEN 5300 - Systems Engineering & Six Sigma
# Group: Sigma 0 (Devin Taylor, Liam Tang, Colton Jacobucci, Caleb McKinney)
# File: 5RO_NYC_CPZ_Regression.R
# Description: Performs difference-in-differences (DiD) regression analysis to estimate 
#              the causal effect of congestion pricing on PM2.5 air quality. Creates 
#              treatment/post indicators based on CPZ implementation date (January 5, 2025), 
#              incorporates congestion zone hours, weather controls, and site type controls. 
#              Estimates DiD models with baseline, simple weather, and full weather 
#              specifications. Generates model comparison tables, confidence intervals, 
#              and 2x2 DiD visualization plots. Outputs: modelsummary_results.csv, 
#              plot_2x2.png, and plot_nice.png.
# Created: 2025-09-14
# Last Updated: 2025-10-23
# ==============================================================================

# ! ! ! IMPORTANT ! ! ! #
# Script MUST be run locally due to memory limitations. 
# ! ! ! IMPORTANT ! ! ! #
#install.packages("glue")
# Hides start-up package messages & loads necessary libraries
suppressPackageStartupMessages({
  library(tidyverse)  # readr, dplyr, tibble, stringr
  library(here)       # for setting working directory
  library(fs)         # file_exists, path ops
  library(sf)         # st_read for geojson
  library(glue)       # clean messages
  library(ggplot2)    # for plotting data
  library(lubridate)  # for date-time formatting
  library(fixest)
  library(modelsummary)
})
# Checking to see if /Data exists & stops program if Data folder is missing
data_dir = here::here("Data")
if (!dir_exists(data_dir)) {
  stop(glue("Data/ not found at project root: {data_dir}"))
} else {
  message(glue("Found Data directory at: {data_dir}"))
}

# Creating a master path registry to keep track of all of the data
paths = tibble::tribble(
  ~name,             ~relpath,                                       ~type,
  # Air Quality
  "aqi_csv",         "air_quality/air_quality.csv",                  "csv",
  "aqi_metro",       "air_quality/metro.rds",                        "rds",
  "aqi_sites",       "air_quality/sites.rds",                        "rds",
  "aqi_counties",    "air_quality/counties.geojson",                 "geojson",
  "aqi_highways",    "air_quality/highways.rds",                     "rds",
  "aqi_roads",       "air_quality/roads.rds",                        "rds",
  
  # Congestion Pricing
  "cong_metro",      "congestion_pricing/metro.rds",                 "rds",
  "cong_counties",   "congestion_pricing/counties.rds",              "rds",
  "cong_vehicles",   "congestion_pricing/zone_vehicle_entries.rds",  "rds",
  "cong_zones",      "congestion_pricing/zone.rds",                 "rds",
  "cong_roads",      "congestion_pricing/roads.rds",                 "rds",

  # Weather
  "weather",         "weather/weather.csv",                          "csv",
  # Parsed data
  "vehicle_entry_locations", "vehicle_entry_locations.rds", "rds"
) %>%
  mutate(full = fs::path(data_dir, relpath))


# Creating a function that specifies which loader to use per file type
# Returns NULL if file does not exist
read_dispatch = function(file, type, cols_only = NULL) {
  if (type == "csv") {
    if (is.null(cols_only)) {
      return(readr::read_csv(file, show_col_types = FALSE))
    } else {
      return(readr::read_csv(file, col_select = tidyselect::all_of(cols_only), 
                             show_col_types = FALSE))
    }
  }
  if (type == "rds")     return(readRDS(file))
  if (type == "geojson") return(sf::st_read(file, quiet = TRUE))
}


# Loading each of the main data sets and storing it into one large master list
master_data = paths %>%
  mutate(obj = map2(full, type, ~ read_dispatch(.x, .y))) %>%
  select(name, obj) %>%
  tibble::deframe()


# Reading the raw data out of master list
aqi_raw     = master_data$aqi_csv    # Air Quality
weather_raw = master_data$weather    # Weather
sites_sf    = master_data$aqi_sites  # Site data
cpz_sf      = master_data$cong_zones # CPZ zone
counties    = master_data$aqi_counties # counties
vehicle_entry_locations = master_data$vehicle_entry_locations # Vehicle entry locations

# make sure both layers are sf + share the same CRS
if (!inherits(sites_sf, "sf")) {
  sites_sf = sf::st_as_sf(sites_sf)
}
if (!inherits(cpz_sf, "sf")) {
  cpz_sf = sf::st_as_sf(cpz_sf)
}

if (is.na(sf::st_crs(sites_sf))) {
  sites_sf = sf::st_set_crs(sites_sf, 4326)  # WGS84 lon/lat
}
if (is.na(sf::st_crs(cpz_sf))) {
  cpz_sf = sf::st_set_crs(cpz_sf, 4326)
}

if (sf::st_crs(sites_sf) != sf::st_crs(cpz_sf)) {
  cpz_sf = sf::st_transform(cpz_sf, sf::st_crs(sites_sf))  # align CRS
}

cpz_sf = sf::st_make_valid(cpz_sf)  # fix any geometry issues

# classify each site: inside CPZ = treated
# st_intersects treats boundary points as inside (safer than st_within for borders)
treated_flag = lengths(sf::st_intersects(sites_sf, cpz_sf)) > 0

# helper to normalize IDs (avoids scientific notation / type mismatches)
norm_id = function(x) sprintf("%.0f", as.numeric(x))

sites_tag = sites_sf %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(
    aqs_id_full = norm_id(aqs_id_full),  # normalize ID as character
    treated = as.integer(treated_flag)   # 1 = inside CPZ, 0 = outside
  ) %>%
  dplyr::select(aqs_id_full, site_name, treated)

# Sanity check - show both 0 and 1 (51 controls, 5 treated)
print(table(sites_tag$treated, useNA = "ifany"))


# Get shape data for the sites_map
sites_map = sites_sf %>%
  dplyr::mutate(aqs_id_full = norm_id(aqs_id_full)) %>%
  dplyr::left_join(sites_tag, by = "aqs_id_full")

# Read data set in that was processed in NYC_CPZ_Desecriptive_Stats.R
aqi_data_nyc_metro = readRDS("Data/aqi_data_nyc_metro.rds")

# CPZ Date
CPZDate = as.Date("2025-01-05")  # day CPZ went into effect

# WEATHER DATA PREPARATION
# Process weather data to match with AQI data
weather_df = weather_raw %>%
  dplyr::mutate(
    aqs_id_full = norm_id(aqs_id_full),
    datetime    = lubridate::ymd_hms(datetime, quiet = TRUE)
  ) %>%
  dplyr::select(aqs_id_full, datetime, temp, dew, humidity, precip, windspeed, 
                winddir, cloudcover, solarradiation, uvindex)

# Adding the pre & post flags to the aqi data and merging with weather
panel_2x2 = aqi_data_nyc_metro %>%
  dplyr::mutate(
    post = as.integer(date >= CPZDate),  # 1 = on/after policy date
    weekday = lubridate::wday(datetime, label = FALSE)  # 1=Sunday, 2=Monday, ..., 7=Saturday
  ) %>%
  dplyr::mutate(
    # Congestion zone hours: Weekdays (Mon-Fri, wday 2-6) 5am-9pm, 
    # Weekends (Sat-Sun, wday 1,7) 9am-9pm
    congestion_hours = dplyr::case_when(
      weekday %in% c(2:6) & hour >= 5 & hour < 21 ~ 1L,  # Weekdays: 5am-9pm (hour 5-20)
      weekday %in% c(1, 7) & hour >= 9 & hour < 21 ~ 1L,  # Weekends: 9am-9pm (hour 9-20)
      TRUE ~ 0L
    )
  ) %>%
  dplyr::left_join(weather_df, by = c("aqs_id_full", "datetime"))


# Filter out NA values and invalid readings (PM2.5 < 0)
panel_2x2 = panel_2x2 %>%
  dplyr::filter(!is.na(pm25), !is.na(treated), !is.na(post))

### IMPORTANT NOTE
### The years of 2020 through 2023 were the years during 
### The COVID Pandemic, this significantly altered traffic behavior
### These years should be removed from the data set
model_1_data = panel_2x2 %>% 
  filter(date >= "2024-01-01") %>% mutate(
  # There is a difference between NYCCAS and EPA sensors so we should control for that
  # EPA sensors are different hardware, NYCCAS controls for this and corrects 
  # data before publishing, however, EPA sensors are still often placed in 
  # higher exposure areas and sites are chosen 
  # to give the best regional data whereas NYCCAS are sited
  # to gather data on neighborhood AQI. Therefore we should control for the site types
  # this should give the model better prediction power
    site_type = as.integer(is.na(site_name)) 
  )

# Control for time in data. Get numeric month and numeric day to control for.
model_1_data = model_1_data %>% 
  mutate(month = month(date), day = wday(date))

#model_1_data = model_1_data %>% filter(pm25<100)
# Check dataset
model_1_data %>% glimpse()

#model_1_data = model_1_data %>% filter(dist<10)
# DID model WITHOUT weather controls (baseline)
did_model <- lm(pm25 ~ post + treated + (post * treated) + congestion_hours + 
                hour + day + month+dist+site_type, 
                data = model_1_data)
summary(did_model)

# Check if controlling for time increased R^2 (predictive power)
did_model_no_time <- lm(pm25 ~ post + treated + (post * treated), 
                        data = model_1_data)
summary(did_model_no_time)

# DID model WITH weather controls
# Note: Weather controls help account for meteorological confounders that may 
# affect PM2.5 levels independently of the congestion pricing policy
did_model_weather_simple <- lm(pm25 ~ post + treated + (post * treated) + 
                                congestion_hours + temp + humidity + hour + day + month+dist
                                +site_type, 
                                data = model_1_data)
summary(did_model_weather_simple)

# Add in other weather variables to increase prediction power
did_model_weather <- lm(pm25 ~ post + treated + (post * treated) + congestion_hours +
                        temp + humidity + dew + precip + windspeed + winddir + cloudcover +
                        solarradiation + uvindex + hour + day + month+dist+site_type, 
                        data = model_1_data)
summary(did_model_weather)
### MESSING AROUND
lm(pm25 ~ post + treated + (post * treated) +site_type+ temp + humidity + dew + precip + windspeed + winddir + cloudcover  + uvindex+month,
                data = model_1_data) %>% summary()
summary(did_model)




# Confidence intervals for model coefficients
conf_int <- confint(did_model_weather, level = 0.99)
print("99% Confidence Intervals for DiD Model Coefficients:")
print(conf_int)

# Extract the DiD coefficient (interaction term) and its confidence interval
did_coef_name <- "post:treated"
did_coef <- coef(did_model_weather)[did_coef_name]
did_conf_int <- confint(did_model_weather, parm = did_coef_name, level = 0.99)
message(
  "\nDiD Effect (post:treated): ",
  round(did_coef, 3),
  " µg/m³\n",
  "99% CI: [", round(did_conf_int[1], 3), ", ", round(did_conf_int[2], 3), "]"
)
confint(did_model_weather, level = 0.99)

# Model comparison table
# Display in console
modelsummary::modelsummary(
  list("Baseline DiD" = did_model, 
       "DiD with Weather Controls" = did_model_weather_simple, 
       "DiD w Weather Controls 2" = did_model_weather),
  stars = TRUE,
  title = "Difference-in-Differences Models: With and Without Weather Controls (including Congestion Zone Hours fixed effect)"
)

# Export to CSV
modelsummary::modelsummary(
  list("Baseline DiD" = did_model, 
       "DiD with Weather Controls" = did_model_weather_simple, 
       "DiD w Weather Controls 2" = did_model_weather),
  stars = TRUE,
  output = ("tables/modelsummary_results.csv")
)

#######################################
###########DiD PLOTS SECTION##########
#######################################
# To get the confidence intervals
# we will need to calculate 
# covariance to get the standard errors
# for all the combination of estimates
# we are interested in
# DESCRIPTION
# Control Pre: Intercept
# Control Post: Intercept + Post
# Treatment Pre: Intercept + Treated
# Treatment Post: Intercept+Treated+Post+Interaction
# Counterfactual Pre: Intercept + Treated (Same as Treatment PRE)
# Counterfactual Post: Intercept + Treated + Post
# In total I need to calculate 5 standard errors using vcov to get the covariance matrix
cov = vcov(did_model_weather)
c_names = names(coef(did_model_weather))
num_cf = length(c_names)
c_v = rep(0,num_cf)
names(c_v) = c_names
# For each variable in model_1_data that is also in c_v, set c_v[var] = mean(model_1_data[[var]], na.rm = TRUE)
for (var in names(c_v)) {
  if (var %in% names(model_1_data)) {
    c_v[var] = mean(model_1_data[[var]], na.rm = TRUE)
  }
}
c_v["(Intercept)"] = 0
c_v["post"] = 0
c_v["treated"] = 0
c_v["post:treated"] = 0
c_v_base = c_v
##### Get Control Pre
c_v_cpre = c_v_base
c_v_cpre["(Intercept)"] = 1
# Calculate the Variance of the combined estimate
tmp_var <- t(c_v_cpre) %*% cov %*% c_v_cpre
tmp_val <- as.numeric(tmp_var)
SE_CPRE <- sqrt(tmp_val)
##### Get Control Post
c_v_cpost = c_v_base
c_v_cpost["(Intercept)"] = 1
c_v_cpost["post"] = 1
# Calculate the Variance of the combined estimate
tmp_var <- t(c_v_cpost) %*% cov %*% c_v_cpost
tmp_val <- as.numeric(tmp_var)
SE_CPOST <- sqrt(tmp_val)
##### Get Treatment Pre
c_v_tpre = c_v_base
c_v_tpre["(Intercept)"] = 1
c_v_tpre["treated"] = 1
# Calculate the Variance of the combined estimate
tmp_var <- t(c_v_tpre) %*% cov %*% c_v_tpre
tmp_val <- as.numeric(tmp_var)
SE_TPRE <- sqrt(tmp_val)
##### Get Treatment POST:Intercept+Treated+Post+Interaction
c_v_tpost = c_v_base
c_v_tpost["(Intercept)"] = 1
c_v_tpost["treated"] = 1
c_v_tpost["post"] = 1
c_v_tpost["post:treated"] = 1
# Calculate the Variance of the combined estimate
tmp_var <- t(c_v_tpost) %*% cov %*% c_v_tpost
tmp_val <- as.numeric(tmp_var)
SE_TPOST <- sqrt(tmp_val)
# Get Counterfactual Post: Intercept + Treated + Post
c_v_cfpost = c_v_base
c_v_cfpost["(Intercept)"] = 1
c_v_cfpost["treated"] = 1
c_v_cfpost["post"] = 1
# Calculate the Variance of the combined estimate
tmp_var <- t(c_v_cfpost) %*% cov %*% c_v_cfpost
tmp_val <- as.numeric(tmp_var)
SE_CFPOST <- sqrt(tmp_val)

# Get confidence intervals for the model
confi = confint(did_model_weather, level = 0.99)
intercept_l = confi["(Intercept)",1]
intercept_u = confi["(Intercept)",2]
post_l = confi["post",1]
post_u = confi["post",2]
treated_l = confi["treated",1]
treated_u = confi["treated",2]
interaction_l = confi["post:treated",1]
interaction_u = confi["post:treated",2]
# Get coefficients from selected model
coeffs = coef(did_model_weather)
# Save the needed coefficients
# Get intercept
intercept = coeffs["(Intercept)"]
# Get Post
post = coeffs["post"]
# Get treated
treated = coeffs["treated"]
# Get interaction
interaction = coeffs["post:treated"]
did_model_weather_m_tbl = data.frame(
  # 3 groups of observations
  group = c("Control","Treated","Counterfactual"),
  # Data for post treatment
  Post = c(intercept+post,intercept+post+treated+interaction,intercept+treated+post),
  # Data for pre treatment
  Pre = c(intercept, intercept+treated,intercept+treated)
)%>% mutate(
  # Calculate differences
  diff = Post-Pre
)
# See the table
print(did_model_weather_m_tbl)
# post control = intercept + post
# control pre = intercept
# treatet pre = intercept + treated
# treated post = intercept + treated + post-did
# Plotting the results
plot_2x2 = did_model_weather_m_tbl %>%
  tidyr::pivot_longer(c(Pre, Post), names_to = "period", values_to = "mean_pm25") %>%
  dplyr::mutate(period = factor(period, levels = c("Pre","Post"))) %>%
  ggplot(aes(x = period, y = mean_pm25, group = group, color = group)) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.8) +
  labs(title = "2×2 Means of PM2.5 (NYC Metro)",
       subtitle = paste("Policy date =", as.character(CPZDate)),
       x = NULL, y = "Mean PM2.5 (µg/m³)", color = NULL) +
  theme_minimal()
print(plot_2x2)
# Save the simplified plot
ggplot2::ggsave("plots/plot_2x2.png", 
                plot = plot_2x2, 
                width = 10, 
                height = 7, 
                dpi = 300)


# Make a more complicated but nicer plot
# Based on this tutorial sent from heaven
#   https://rpubs.com/phle/r_tutorial_difference_in_differences
# Intervention value for plotting - Control Group
interven1 = (intercept+intercept+post)/2
# Intervention value for plotting - Treatment Counterfactual
interven2 = (intercept+treated+post+intercept+treated)/2
# Data frame for counterfactual observations
counterfactual = data.frame(
  obs = c("Pre","Post"),
  group = c("Treated(cf)","Treated(cf)"),
  data = c(intercept+treated, intercept+treated+post)
)
# Data frame for other data
intervention = data.frame(
  obs = c("Intervention","Intervention","Intervention","Pre","Post","Pre","Post"),
  group = c("Control",  "Treated",       "Treated(cf)","Control","Control","Treated","Treated"),
  data = c(interven1,    interven2,      interven2,      intercept,intercept+post,intercept+treated,intercept+post+treated+interaction)
)
# Join the data together for plotting
did_plot_data = bind_rows(counterfactual,intervention)

# add confidence intervals to the data
did_plot_data = did_plot_data %>%
  mutate(data_l = case_when(
    group == "Control" & obs == "Pre" ~ data - qnorm(.975)*SE_CPRE, # Pre control just intercept
    group == "Control" & obs == "Post" ~ data - qnorm(.975)*SE_CPOST, # Post control just intercept + post
    group == "Control" & obs == "Intervention" ~ data - qnorm(.975)*SE_CPRE, # Intervention control just intercept + post
    group == "Treated" & obs == "Pre" ~ data - qnorm(.975)*SE_TPRE, # Pre treated just intercept + treated
    group == "Treated" & obs == "Post" ~ data - qnorm(.975)*SE_TPOST, # Post treated just intercept + treated + post
    group == "Treated" & obs == "Intervention" ~ data - qnorm(.975)*SE_TPRE, # Intervention treated just intercept + treated + post
    group == "Treated(cf)" & obs == "Pre" ~ data - qnorm(.975)*SE_TPRE,
    group == "Treated(cf)" & obs == "Post" ~ data - qnorm(.975)*SE_CFPOST,
    group == "Treated(cf)" & obs == "Intervention" ~ data - qnorm(.975)*SE_TPRE,
    )) %>%
    mutate(data_u = case_when(
    group == "Control" & obs == "Pre" ~ data + qnorm(.975)*SE_CPRE, # Pre control just intercept
    group == "Control" & obs == "Post" ~ data + qnorm(.975)*SE_CPOST, # Post control just intercept + post
    group == "Control" & obs == "Intervention" ~ data + qnorm(.975)*SE_CPRE, # Intervention control just intercept + post
    group == "Treated" & obs == "Pre" ~ data + qnorm(.975)*SE_TPRE, # Pre treated just intercept + treated
    group == "Treated" & obs == "Post" ~ data + qnorm(.975)*SE_TPOST, # Post treated just intercept + treated + post
    group == "Treated" & obs == "Intervention" ~ data + qnorm(.975)*SE_TPRE, # Intervention treated just intercept + treated + post
    group == "Treated(cf)" & obs == "Pre" ~ data + qnorm(.975)*SE_TPRE,
    group == "Treated(cf)" & obs == "Post" ~ data + qnorm(.975)*SE_CFPOST,
    group == "Treated(cf)" & obs == "Intervention" ~ data + qnorm(.975)*SE_TPRE,
    ))

# Make sure the groups are ordered so the plotting works as expected
# Pre values first, then intervention, then post values
did_plot_data$obs = factor(did_plot_data$obs, levels=c("Pre","Intervention","Post"))
# Make the plot
did_plot = did_plot_data %>%
  ggplot(aes(x=obs,y=data, group=group,fill=group,ymin=data_l,ymax=data_u,color=group)) + # aesthetic based on data
  geom_ribbon( alpha=0.1) + 
 # geom_errorbar(position=position_dodge(width=0.5))+
  geom_line(size=1.2) + # line plot with color based on 3 groups
  geom_vline(xintercept = "Intervention", linetype="dotted", # intervention line
             color = "black", size=1.1) +

  scale_color_brewer(palette = "Accent")+ # make colors nice
  labs(x="", y="PM25 (mean)") + # Add label for y axis
  annotate( # Add nice annotation 
    "text",
    x = "Post",
    y = 12.8, # this was picked visually after generating the plot
    label = "{Difference-in-Differences}",
    angle = 90,
    size = 5 # This was picked visually after generating the plot
  )
print(did_plot)
# Save the simplified plot
ggplot2::ggsave("plots/plot_nice.png", 
                plot = did_plot, 
                width = 10, 
                height = 7, 
                dpi = 300)


