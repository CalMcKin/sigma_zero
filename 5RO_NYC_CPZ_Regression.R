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

# Ensure plots directory exists
if (!dir_exists("plots")) {
  dir_create("plots")
  message("Created plots/ directory")
}

# Prepare simplified DiD plot data
# Calculate mean PM2.5 for each group (treated/control) in each period (pre/post)
did_simple_data = model_1_data %>%
  dplyr::mutate(
    treated_label = dplyr::if_else(treated == 1, "Treated (Inside CPZ)", 
                                    "Control (Outside CPZ)"),
    period_label = dplyr::if_else(post == 0, "Pre-Treatment", "Post-Treatment"),
    period_numeric = dplyr::if_else(post == 0, 0, 1)  # 0 = pre, 1 = post
  ) %>%
  dplyr::group_by(treated_label, period_label, period_numeric, treated, post) %>%
  dplyr::summarise(
    mean_pm25 = mean(pm25, na.rm = TRUE),
    se_pm25 = sd(pm25, na.rm = TRUE) / sqrt(n()),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    upper_ci = mean_pm25 + 1.96 * se_pm25,
    lower_ci = mean_pm25 - 1.96 * se_pm25
  )

# Create simplified DiD plot
did_simple_plot = ggplot2::ggplot(did_simple_data, 
                                   ggplot2::aes(x = period_numeric, 
                                                y = mean_pm25, 
                                                color = treated_label,
                                                group = treated_label)) +
  ggplot2::geom_line(linewidth = 1.5, alpha = 0.7) +
  ggplot2::geom_point(size = 4, shape = 19) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = lower_ci, ymax = upper_ci), 
                         width = 0.05, linewidth = 1) +
  ggplot2::scale_x_continuous(
    breaks = c(0, 1),
    labels = c("Pre-Treatment", "Post-Treatment"),
    limits = c(-0.1, 1.1)
  ) +
  ggplot2::scale_color_manual(values = c("Treated (Inside CPZ)" = "#E74C3C", 
                                         "Control (Outside CPZ)" = "#3498DB")) +
  ggplot2::labs(
    title = "Difference-in-Differences: PM2.5 by Treatment Status",
    subtitle = "Average PM2.5 before and after CPZ implementation (with 95% CI)",
    x = "Period",
    y = "Average PM2.5 (μg/m³)",
    color = "Group",
    caption = "Treatment date: January 5, 2025"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
    plot.caption = ggplot2::element_text(size = 9, hjust = 0.5),
    legend.position = "bottom",
    legend.title = ggplot2::element_text(face = "bold"),
    axis.title = ggplot2::element_text(size = 11, face = "bold"),
    axis.text.x = ggplot2::element_text(size = 10),
    panel.grid.minor = ggplot2::element_blank()
  )

# Save the simplified plot
ggplot2::ggsave("plots/did_plot_simple.png", 
                plot = did_simple_plot, 
                width = 10, 
                height = 7, 
                dpi = 300)

print(did_simple_plot)

# Calculate and display the DiD estimate
did_estimate = did_simple_data %>%
  dplyr::select(treated_label, period_label, mean_pm25) %>%
  tidyr::pivot_wider(names_from = period_label, values_from = mean_pm25) %>%
  dplyr::mutate(
    change = `Post-Treatment` - `Pre-Treatment`
  )

message("\n=== DiD Summary ===")
print(did_estimate)

# Calculate DiD coefficient
treated_pre = did_estimate$`Pre-Treatment`[did_estimate$treated_label == "Treated (Inside CPZ)"]
treated_post = did_estimate$`Post-Treatment`[did_estimate$treated_label == "Treated (Inside CPZ)"]
control_pre = did_estimate$`Pre-Treatment`[did_estimate$treated_label == "Control (Outside CPZ)"]
control_post = did_estimate$`Post-Treatment`[did_estimate$treated_label == "Control (Outside CPZ)"]

did_coef = (treated_post - treated_pre) - (control_post - control_pre)
message(glue("\nDiD Estimate (from plot): {round(did_coef, 3)} μg/m³"))
message(glue("Treated change: {round(treated_post - treated_pre, 3)} μg/m³"))
message(glue("Control change: {round(control_post - control_pre, 3)} μg/m³"))

message("\n✓ Simplified DiD plot saved to plots/did_plot_simple.png")

panel_2x2 = model_1_data
# Meaning the treated and controls x Pre/Post groups of PM2.5
means_tbl = panel_2x2 %>%
  dplyr::group_by(treated, post) %>%
  dplyr::summarise(mean_pm25 = mean(pm25, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    group  = ifelse(treated == 1, "CPZ - Treated", "Control"),
    period = ifelse(post == 1, "Post", "Pre") 
  ) %>%
  dplyr::select(group, period, mean_pm25) %>%
  tidyr::complete(group = c("CPZ - Treated","Control"),
                  period = c("Pre","Post"),
                  fill = list(mean_pm25 = NA_real_)) %>%
  tidyr::pivot_wider(names_from = period, values_from = mean_pm25) %>%
  dplyr::mutate(Diff = Post - Pre)


print(means_tbl)
# calculating the mean difference of treated and control change
treated_change = means_tbl %>% dplyr::filter(group == "CPZ - Treated") %>% dplyr::pull(Diff)
control_change = means_tbl %>% dplyr::filter(group == "Control") %>% dplyr::pull(Diff)
did_hand       = treated_change - control_change

#reporting the percent change vs treated
treated_pre = means_tbl %>% dplyr::filter(group == "CPZ - Treated") %>% dplyr::pull(Pre)
pct_change  = 100 * did_hand / treated_pre

message(
  "2×2 DiD (means): ΔCPZ = ", round(treated_change, 3),
  ", ΔControl = ", round(control_change, 3),
  " DiD = ", round(did_hand, 3), " µg/m³ (",
  round(pct_change, 1), "% of CPZ pre-mean)"
)
inter = 17.51709
post = -1.77825
treated = -8.09942
did = -0.74844

mean_tbl2 =data.frame(
  group = c("Control","Treated"),
  post = c(inter+post,inter+did),
  pre = c(inter,inter+treated)
) %>% mutate (
  diff = post-pre
)
mean_tbl2$diff[1] - mean_tbl2$diff[2]
print(mean_tbl2)
coeffs = coef(did_model_weather)
intercept = coeffs["(Intercept)"]
post = coeffs["post"]
treated = coeffs["treated"]
diddy_treatment = coeffs["post:treated"]
did_model_weather_m_tbl = data.frame(
  group = c("Control","Treated"),
  Post = c(intercept+post,intercept+post+treated+diddy_treatment),
  Pre = c(intercept, intercept+treated)
)%>% mutate(
  diff = Post-Pre
)
-1.3738064 --0.3499375

print(did_model_weather_m_tbl)
# post control = intercept - post
# control pre = intercept
# treatet pre = intercept - treated
# treated post = intercept - treated - post-did
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
did_model <- lm(pm25 ~ post + treated + (post*treated), data = panel_2x2)
summary(did_model)
