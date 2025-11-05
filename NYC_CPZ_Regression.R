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
# Checking to see if /Data exists & stops program if Data folder is !missing
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
  
  # Transportation
  "emissions",       "transportation/emissions.rds",                 "rds",
  
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
      return(readr::read_csv(file, col_select = all_of(cols_only), show_col_types = FALSE))
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
if (!inherits(sites_sf, "sf")) sites_sf = sf::st_as_sf(sites_sf)
if (!inherits(cpz_sf,   "sf")) cpz_sf   = sf::st_as_sf(cpz_sf)

if (is.na(sf::st_crs(sites_sf))) sites_sf = sf::st_set_crs(sites_sf, 4326)  # WGS84 lon/lat
if (is.na(sf::st_crs(cpz_sf)))   cpz_sf   = sf::st_set_crs(cpz_sf,   4326)

if (sf::st_crs(sites_sf) != sf::st_crs(cpz_sf)) {
  cpz_sf = sf::st_transform(cpz_sf, sf::st_crs(sites_sf)) # align CRS
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
    aqs_id_full = norm_id(aqs_id_full),          # normalize ID as character
    treated     = as.integer(treated_flag)       # 1 = inside CPZ, 0 = outside
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
# Split data from at date when CPZ went into effect, Jan 5th 2025
CPZDate = as.Date("2025-01-05") # day CPZ went into effect
aqi_df_split = split(aqi_data_nyc_metro,aqi_data_nyc_metro$date < CPZDate) # Split DF into before and after

# WEATHER DATA PREPARATION
# Process weather data to match with AQI data
weather_df = weather_raw %>%
  dplyr::mutate(
    aqs_id_full = norm_id(aqs_id_full),
    datetime    = lubridate::ymd_hms(datetime, quiet = TRUE)
  ) %>%
  dplyr::select(aqs_id_full, datetime, temp, dew, humidity, precip, 
                windspeed, winddir, cloudcover, solarradiation, uvindex)

# Adding the pre & post flags to the aqi data and merging with weather
panel_2x2 = aqi_data_nyc_metro %>%
  dplyr::mutate(
    post = as.integer(date >= CPZDate),   # 1 = on/after policy date
    weekday = lubridate::wday(datetime, label = FALSE)  # 1=Sunday, 2=Monday, ..., 7=Saturday
  ) %>%
  dplyr::mutate(
    # Congestion zone hours: Weekdays (Mon-Fri, wday 2-6) 5am-9pm, Weekends (Sat-Sun, wday 1,7) 9am-9pm
    congestion_hours = dplyr::case_when(
      weekday %in% c(2:6) & hour >= 5 & hour < 21 ~ 1L,   # Weekdays: 5am-9pm (hour 5-20)
      weekday %in% c(1, 7) & hour >= 9 & hour < 21 ~ 1L,  # Weekends: 9am-9pm (hour 9-20)
      TRUE ~ 0L
    )
  ) %>%
  dplyr::left_join(weather_df, by = c("aqs_id_full", "datetime")) %>%
  dplyr::mutate(
    invalid_reading = as.integer(!is.na(pm25) & pm25 < 0)  # Flag invalid sensor readings (PM2.5 < 0)
  )

# Report on invalid readings before filtering
invalid_count = sum(panel_2x2$invalid_reading, na.rm = TRUE)
if (invalid_count > 0) {
  message(glue("Flagged {invalid_count} invalid PM2.5 readings (values < 0) - excluding from analysis"))
}

# Filter out NA values and invalid readings (PM2.5 < 0)
panel_2x2 = panel_2x2 %>%
  dplyr::filter(!is.na(pm25), !is.na(treated), !is.na(post))#, pm25 >= 0,pm25 < 500)

# DID model WITHOUT weather controls (baseline)
did_model <- lm(pm25 ~ post + treated + (post*treated)+ (congestion_hours), data = panel_2x2)
summary(did_model)

# DID model WITH weather controls
# Note: Weather controls help account for meteorological confounders that may 
# affect PM2.5 levels independently of the congestion pricing policy
did_model_weather_simple <- lm(pm25 ~ post + treated + (post*treated) + (congestion_hours) +
                        (temp) + (humidity), 
                        data = panel_2x2)
summary(did_model_weather_simple)

did_model_weather <- lm(pm25 ~ post + treated + (post*treated) + (congestion_hours) +
                        (temp) + (humidity) + dew + precip + windspeed + winddir + cloudcover
                        + solarradiation + uvindex, 
                        data = panel_2x2)
summary(did_model_weather)


# Model comparison table
modelsummary::modelsummary(
  list("Baseline DiD" = did_model, "DiD with Weather Controls" = did_model_weather_simple, "DiD w Weather Controls 2" = did_model_weather ),
  stars = TRUE,
  title = "Difference-in-Differences Models: With and Without Weather Controls (including Congestion Zone Hours fixed effect)",

)

#######################################
###########PLACEBO TEST SECTION########
#######################################

# Placebo test: Test for treatment effects using a fake treatment date
# before the actual CPZ implementation. If there's a significant effect
# at the fake date, it suggests violations of parallel trends assumption.

# Set placebo date (e.g., 1year before actual CPZ date)
placebo_date = as.Date("2024-01-05")  # 1 year before CPZDate

# Create placebo panel using only pre-treatment data (before actual CPZ date)
# This ensures we're testing whether there were differences before the policy
panel_placebo = aqi_data_nyc_metro %>%
  dplyr::filter(date < as.Date("2025-01-05")) %>%  # Only use data before actual treatment
  dplyr::mutate(
    post_placebo = as.integer(date >= placebo_date),  # Fake treatment indicator
    weekday = lubridate::wday(datetime, label = FALSE)
  ) %>%
  dplyr::mutate(
    # Congestion zone hours: Weekdays (Mon-Fri, wday 2-6) 5am-9pm, Weekends (Sat-Sun, wday 1,7) 9am-9pm
    congestion_hours = dplyr::case_when(
      weekday %in% c(2:6) & hour >= 5 & hour < 21 ~ 1L,   # Weekdays: 5am-9pm (hour 5-20)
      weekday %in% c(1, 7) & hour >= 9 & hour < 21 ~ 1L,  # Weekends: 9am-9pm (hour 9-20)
      TRUE ~ 0L
    )
  ) %>%
  dplyr::left_join(weather_df, by = c("aqs_id_full", "datetime")) %>%
  dplyr::mutate(
    invalid_reading = as.integer(!is.na(pm25) & pm25 < 0)
  ) %>%
  dplyr::filter(!is.na(pm25), !is.na(treated), !is.na(post_placebo))

# Report on placebo test sample size
message(glue("\nPlacebo test using fake treatment date: {placebo_date}"))
message(glue("Placebo test sample size: {nrow(panel_placebo)} observations"))
message(glue("Pre-placebo period: {min(panel_placebo$date)} to {placebo_date - 1}"))
message(glue("Post-placebo period: {placebo_date} to {max(panel_placebo$date)}"))

# Placebo DiD model WITHOUT weather controls (baseline)
placebo_did_model <- lm(pm25 ~ post_placebo + treated + (post_placebo*treated) + congestion_hours, 
                        data = panel_placebo)
summary(placebo_did_model)

# Placebo DiD model WITH simple weather controls
placebo_did_model_weather_simple <- lm(pm25 ~ post_placebo + treated + (post_placebo*treated) + congestion_hours +
                                       temp + humidity, 
                                       data = panel_placebo)
summary(placebo_did_model_weather_simple)

# Placebo DiD model WITH all weather controls
placebo_did_model_weather <- lm(pm25 ~ post_placebo + treated + (post_placebo*treated) + congestion_hours +
                                temp + humidity + dew + precip + windspeed + winddir + cloudcover +
                                solarradiation + uvindex, 
                                data = panel_placebo)
summary(placebo_did_model_weather)

# Placebo test results table
modelsummary::modelsummary(
  list("Baseline Placebo" = placebo_did_model, 
       "Placebo w/ Simple Weather" = placebo_did_model_weather_simple, 
       "Placebo w/ Full Weather" = placebo_did_model_weather),
  stars = TRUE,
  title = "Placebo Test: DiD Models Using Fake Treatment Date (2024-01-05) - Should Show No Effect"
)

# Extract and compare treatment effect coefficients
# The interaction term (post_placebo*treated) should be statistically insignificant
# if parallel trends assumption holds
placebo_coefs = data.frame(
  Model = c("Baseline Placebo", "Placebo w/ Simple Weather", "Placebo w/ Full Weather"),
  Coefficient = c(
    coef(placebo_did_model)["post_placebo:treated"],
    coef(placebo_did_model_weather_simple)["post_placebo:treated"],
    coef(placebo_did_model_weather)["post_placebo:treated"]
  ),
  P_Value = c(
    summary(placebo_did_model)$coefficients["post_placebo:treated", "Pr(>|t|)"],
    summary(placebo_did_model_weather_simple)$coefficients["post_placebo:treated", "Pr(>|t|)"],
    summary(placebo_did_model_weather)$coefficients["post_placebo:treated", "Pr(>|t|)"]
  )
)

print("\n=== Placebo Test Results Summary ===")
print(placebo_coefs)

# Interpretation: If p-values are > 0.05, this supports the parallel trends assumption
# If p-values are < 0.05, it suggests pre-existing differences between treated and control groups
if (any(placebo_coefs$P_Value < 0.05, na.rm = TRUE)) {
  message("\n⚠️  WARNING: Significant placebo effects detected. This may indicate violations of parallel trends assumption.")
} else {
  message("\n✓ Placebo test passed: No significant effects at fake treatment date. Parallel trends assumption appears valid.")
}
