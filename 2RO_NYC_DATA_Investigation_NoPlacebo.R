# ==============================================================================
# Data Investigation: Placebo Test Feasibility
# Course: SYSEN 5300 - Systems Engineering & Six Sigma
# Group: Sigma 0 (Devin Taylor, Liam Tang, Colton Jacobucci, Caleb McKinney)
# File: 2RO_NYC_DATA_Investigation_NoPlacebo.R
# Description: Checks feasibility of placebo test for DiD analysis. Loads air quality 
#              and congestion pricing zone data, identifies monitoring sites inside 
#              and outside the CPZ, creates visualization plots of site locations. 
#              Determines that placebo test is not feasible due to insufficient 
#              pre-2019 data for sites inside the CPZ zone (earliest data starts 
#              June 2019, insufficient for meaningful placebo analysis).
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

#sites less than xxkm away from CPZ
sites_sf_f = sites_sf %>% filter(dist<5)

# Figure out how to get locations for each AQI sensor
glimpse(sites_sf)
# Join using AQI_ID_FULL
aqi_sites_inside_cpz = st_intersection(cpz_sf, sites_sf)
# See the sites
aqi_sites_inside_cpz

# Visual Inspection
sites_plot = ggplot(aqi_sites_inside_cpz)+
  geom_sf(colour = "red")+
  geom_sf_label(aes(label = aqs_id_full), colour = "black", alpha=.3,size=2,nudge_y = 0.002)+
  geom_sf(data = cpz_sf,colour = "yellow",alpha = .5)+
  geom_sf(data=sites_sf_f,color="green")+
  geom_sf_label(data =sites_sf_f, aes(label = aqs_id_full), colour = "black", alpha=.3,size=2,nudge_y = 0.002)
# Save off the plot
ggsave("plots/sites_plot.png", sites_plot, width = 10, height = 10, dpi = 300)
# Visual Inspection
sites_plot_n = ggplot(aqi_sites_inside_cpz)+
  geom_sf(colour = "red")+
  geom_sf_label(aes(label = site_name), colour = "black", alpha=.3,size=2,nudge_y = 0.002)+
  geom_sf(data = cpz_sf,colour = "yellow",alpha = .5)+
  geom_sf(data=sites_sf_f,color="green")+
  geom_sf_label(data =sites_sf_f, aes(label = site_name), colour = "black", alpha=.3,size=2,nudge_y = 0.002)
# Save off the plot
ggsave("plots/sites_plot_n.png", sites_plot_n, width = 10, height = 10, dpi = 300)

# Add FDR
aqi_sites_inside_cpz = rbind(aqi_sites_inside_cpz,sites_sf %>% filter(site_name == "FDR"))
# see data
aqi_sites_inside_cpz

# Now that we have a complete list of data lets see if the sites in the CPZ
# have data from 2018 to 2019

# To do that join lets filter on aqi_raw
aqi_raw %>% filter(
    aqs_id_full %in% aqi_sites_inside_cpz$aqs_id_full # check for sites in CPZ
    ) %>% group_by(aqs_id_full) %>% reframe(
        min = min(datetime),
    )
# Out put of this is 2019-06-06
# We cannot perform a placebo test in this case
# For the placebo test we would have needed data from inside
# the CPZ zone from 2018 to 2019 and we only have data from 2019
# in June. This would not be enough data to create a placebo test.
rm(list = ls())