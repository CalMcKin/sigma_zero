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
  "weather",         "weather/weather.csv",                          "csv"
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


# ---------------- EXPERIMENTING BELOW THIS LINE ----------------

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

# AIR QUALITY
aqi_df = aqi_raw %>%
  dplyr::mutate(
    aqs_id_full = norm_id(aqs_id_full),
    datetime    = lubridate::ymd_hms(datetime, quiet = TRUE),
    date        = as.Date(datetime),
    hour        = lubridate::hour(datetime),
    pm25        = .data[["value"]]
  ) %>%
  dplyr::select(aqs_id_full, datetime, date, hour, pm25) %>%
  dplyr::left_join(sites_tag, by = "aqs_id_full") %>%   # propagate site label to every row
  dplyr::arrange(aqs_id_full, datetime)

# there should be 56 distinct sites in the time series
aqi_df %>% dplyr::distinct(aqs_id_full) %>% nrow()

# make sure every site got a treated label
missing_ids = aqi_df %>% dplyr::filter(is.na(treated)) %>% dplyr::distinct(aqs_id_full)
if (nrow(missing_ids) > 0) {
  print(missing_ids)  # investigate ID formatting if any NA appear
}

# counts by site and by observation
aqi_df %>% dplyr::distinct(aqs_id_full, treated) %>% dplyr::count(treated)
table(aqi_df$treated, useNA = "ifany")


sites_map = sites_sf %>%
  dplyr::mutate(aqs_id_full = norm_id(aqs_id_full)) %>%
  dplyr::left_join(sites_tag, by = "aqs_id_full")

ggplot() +
  geom_sf(data = cpz_sf, fill = NA, linewidth = 0.7) +
  geom_sf(data = sites_map, aes(color = factor(treated)), size = 2) +
  scale_color_manual(values = c("0" = "gray40", "1" = "red"),
                     name = "Treated (in CPZ)") +
  coord_sf() +
  labs(title = "AQI Monitors vs. CPZ",
       subtitle = "Red = inside CPZ, Gray = outside") +
  theme_minimal()
# Only consider data from certain counties for now
ny_metro = counties %>% filter(state == "NY") %>% filter(
  name=="Bronx County" | name=="New York County" | name =="Queens County" |
    name=="Kings County")
aqi_nyc_metro = st_intersection(st_as_sf(sites_map),st_as_sf(ny_metro))
aqi_data_nyc_metro = aqi_df %>% filter(aqs_id_full %in% aqi_nyc_metro$aqs_id_full )
# Split data from at date when CPZ went into effect, Jan 5th 2025
CPZDate = as.Date("2025-01-05") # day CPZ went into effect
aqi_df_split = split(aqi_data_nyc_metro,aqi_data_nyc_metro$date < CPZDate) # Split DF into before and after
# For plotting get sensors in the zone
aqi_inside_cpz = st_intersection(st_as_sf(sites_map),st_as_sf(cpz_sf))
ggplot(aqi_inside_cpz)+
  geom_sf(data = cpz_sf,colour = "yellow",alpha = .5)+
  geom_sf(data = aqi_nyc_metro$geometry,colour = "green",alpha = .5)+
  geom_sf(colour = "red")
  