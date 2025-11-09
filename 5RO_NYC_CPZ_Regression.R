# ==============================================================================
# Project Activity – Preliminary Results
# Course: SYSEN 5300 - Systems Engineering & Six Sigma
# Group: Sigma 0 (Devin Taylor, Liam Tang, Colton Jacobucci, Caleb McKinney)
# File: <INSERT.R>
# Description: Data loading, cleaning, and preliminary DiD analysis for NYC CPZ
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
  group = c("Control","Treated","Treated(cf)","Control","Control","Treated","Treated"),
  data = c(interven1, interven2,interven2,intercept,intercept+post,intercept+treated,intercept+post+treated+interaction)
)
# Join the data together for plotting
did_plot_data = bind_rows(counterfactual,intervention)
# Make sure the groups are ordered so the plotting works as expected
# Pre values first, then intervention, then post values
did_plot_data$obs = factor(did_plot_data$obs, levels=c("Pre","Intervention","Post"))
# Make the plot
did_plot = did_plot_data %>%
  ggplot(aes(x=obs,y=data, group=group)) + # aesthetic based on data
  geom_line(aes(color=group), size=1.2) + # line plot with color based on 3 groups
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
