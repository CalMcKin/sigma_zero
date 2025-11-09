# ==============================================================================
# Spatial Visualization Maps
# Course: SYSEN 5300 - Systems Engineering & Six Sigma
# Group: Sigma 0 (Devin Taylor, Liam Tang, Colton Jacobucci, Caleb McKinney)
# File: 4RO_NYC_CPZ_Plots.R
# Description: Creates spatial visualization maps showing the geographic layout of 
#              the analysis. Generates plots displaying the congestion pricing zone, 
#              air quality monitoring sites (treated and control groups), vehicle 
#              entry locations, and surrounding counties. Outputs high-resolution 
#              PNG files: plot_site_names.png, plot_site_ids.png, plot_site_county.png, 
#              plot_road_names.png, and plot_counties_cpz.png.
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

# Ensure counties is an sf object and has the same CRS as cpz_sf
if (!inherits(counties, "sf")) counties = sf::st_as_sf(counties)
if (is.na(sf::st_crs(counties))) counties = sf::st_set_crs(counties, 4326)
if (sf::st_crs(counties) != sf::st_crs(cpz_sf)) {
  counties = sf::st_transform(counties, sf::st_crs(cpz_sf))
}
# Get bounding box of CPZ zone and expand it slightly to include surrounding counties
cpz_bbox = sf::st_bbox(cpz_sf)
# Expand the bounding box by 0.1 degrees (approximately 11 km) to include surrounding counties
cpz_bbox_expanded = cpz_bbox + c(-0.1, -0.1, 0.1, 0.1)
names(cpz_bbox_expanded) = c("xmin", "ymin", "xmax", "ymax")
# Create a polygon from the expanded bounding box
bbox_polygon = sf::st_as_sfc(cpz_bbox_expanded)
# Filter counties that intersect with the expanded bounding box and are in NY state only
counties_surrounding = counties %>%
  dplyr::filter(state == "NY") %>%
  dplyr::filter(lengths(sf::st_intersects(., bbox_polygon)) > 0)
# Create plot of counties surrounding the CPZ zone
plot_counties_cpz = ggplot() +
  geom_sf(data = counties_surrounding, fill = "lightblue", color = "darkblue", alpha = 0.3, linewidth = 0.5) +
  geom_sf(data = cpz_sf, fill = "yellow", color = "orange", alpha = 0.7, linewidth = 1) +
  geom_sf_text(data = counties_surrounding, aes(label = name), size = 3, colour = "darkblue", fontface = "bold") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(title = "Counties Surrounding the Congestion Pricing Zone",
       subtitle = "CPZ Zone highlighted in yellow")

# Save the counties plot
ggsave("plots/plot_counties_cpz.png", plot_counties_cpz, width = 10, height = 10, dpi = 300)
# Limit the counties to the ones we will use
counties_surrounding = counties %>%
filter(state == "NY") %>% filter(
    name=="Bronx County" | name=="New York County" | name =="Queens County" |
    name=="Kings County") %>%
  dplyr::filter(lengths(sf::st_intersects(., bbox_polygon)) > 0)
# Get shape data for the sites_map
sites_map = sites_sf %>%
  dplyr::mutate(aqs_id_full = norm_id(aqs_id_full)) %>%
  dplyr::left_join(sites_tag, by = "aqs_id_full")



# Read data set in that was processed in NYC_CPZ_Desecriptive_Stats.R
aqi_data_nyc_metro = readRDS("Data/aqi_data_nyc_metro.rds")
# Split data from at date when CPZ went into effect, Jan 5th 2025
CPZDate = as.Date("2025-01-05") # day CPZ went into effect
aqi_df_split = split(aqi_data_nyc_metro,aqi_data_nyc_metro$date < CPZDate) # Split DF into before and after
# For plotting get sensors in the zone
aqi_inside_cpz = st_intersection(st_as_sf(sites_map),st_as_sf(cpz_sf))
# Only consider data from certain counties for now
ny_metro = counties %>% filter(state == "NY") %>% filter(
  name=="Bronx County" | name=="New York County" | name =="Queens County" |
    name=="Kings County")
# get NYC metro sites
aqi_nyc_metro_u = st_intersection(st_as_sf(sites_map),st_as_sf(ny_metro))
# Get only the sites in the aqi_data_nyc_metro
aqi_nyc_metro = aqi_nyc_metro_u %>% filter(aqs_id_full %in% aqi_data_nyc_metro$aqs_id_full )
# Add FDR to the aqi_inside_cpz
aqi_inside_cpz = rbind(aqi_inside_cpz,sites_map %>% filter(site_name.y == "FDR"))
# Plot the sensors and congestion zone.
plot_site_names=ggplot(aqi_inside_cpz)+
  geom_sf(data = cpz_sf,colour = "yellow",alpha = .5)+
  geom_sf(data = aqi_nyc_metro,colour = "green",alpha = .5)+
  geom_sf(data = aqi_inside_cpz, colour = "red")+
  geom_sf(data = vehicle_entry_locations, colour = "blue", alpha = .5)+
  geom_sf_text(data = aqi_nyc_metro, aes(label = site_name.y), size = 2.5, colour = "darkgreen", nudge_y = 0.001)+
  geom_sf_text(data = aqi_inside_cpz, aes(label = site_name.y), size = 2.5, colour = "darkred", nudge_y = 0.001)
  #geom_sf_label(data = vehicle_entry_locations,aes(label = road_name), colour = "black", alpha=.3,size=2,nudge_y = -0.004,nudge_x = -0.004)

# Plot the sensors and congestion zone.
plot_site_county=ggplot(aqi_inside_cpz)+
  geom_sf(data = cpz_sf,colour = "yellow",alpha = .5)+
  geom_sf(data = aqi_nyc_metro_u,colour = "green",alpha = .5)+
  geom_sf(data = aqi_inside_cpz, colour = "red")+
  geom_sf(data = vehicle_entry_locations, colour = "blue", alpha = .5)+
  geom_sf(data = counties_surrounding, fill = "lightblue", color = "darkblue", alpha = 0.1, linewidth = 0.5) +
  geom_sf_text(data = counties_surrounding, aes(label = name), size = 3, colour = "darkblue", fontface = "bold") 
  #geom_sf_text(data = aqi_nyc_metro, aes(label = site_name.y), size = 2.5, colour = "darkgreen", nudge_y = 0.001)+
  #geom_sf_text(data = aqi_inside_cpz, aes(label = site_name.y), size = 2.5, colour = "darkred", nudge_y = 0.001)
  #geom_sf_label(data = vehicle_entry_locations,aes(label = road_name), colour = "black", alpha=.3,size=2,nudge_y = -0.004,nudge_x = -0.004)



plot_road_names=ggplot(aqi_inside_cpz)+
  geom_sf(data = cpz_sf,colour = "yellow",alpha = .5)+
  #geom_sf(data = aqi_nyc_metro,colour = "green",alpha = .5)+
  geom_sf(data = aqi_inside_cpz, colour = "red")+
  geom_sf(data = vehicle_entry_locations, colour = "blue", alpha = .5)+
  #geom_sf_text(data = aqi_nyc_metro, aes(label = site_name.y), size = 2.5, colour = "darkgreen", nudge_y = 0.001)+
  #geom_sf_text(data = aqi_inside_cpz, aes(label = site_name.y), size = 2.5, colour = "darkred", nudge_y = 0.001)
  geom_sf_label(data = vehicle_entry_locations,aes(label = road_name), colour = "black", alpha=.3,size=3,nudge_y = -0.001,nudge_x = -0.001)

# Save the plots with high resolution so labels can be seen
ggsave("plots/plot_site_names.png", plot_site_names, width = 10, height = 10, dpi = 300)
ggsave("plots/plot_site_county.png", plot_site_county, width = 10, height = 10, dpi = 300)
ggsave("plots/plot_road_names.png", plot_road_names, width = 10, height = 10, dpi = 300)


# Plot the sensors and congestion zone.
plot_site_id=ggplot(aqi_inside_cpz)+
  geom_sf(data = cpz_sf,colour = "yellow",alpha = .5)+
  geom_sf(data = aqi_nyc_metro,colour = "green",alpha = .5)+
  geom_sf(data = aqi_inside_cpz, colour = "red")+
  geom_sf(data = vehicle_entry_locations, colour = "blue", alpha = .5)+
  geom_sf_text(data = aqi_nyc_metro, aes(label = aqs_id_full), size = 2.5, colour = "darkgreen", nudge_y = 0.002)+
  geom_sf_text(data = aqi_inside_cpz, aes(label = aqs_id_full), size = 2.5, colour = "darkred", nudge_y = 0.002)
  #geom_sf_label(data = vehicle_entry_locations,aes(label = road_name), colour = "black", alpha=.3,size=2,nudge_y = -0.004,nudge_x = -0.004)
ggsave("plots/plot_site_ids.png", plot_site_id, width = 10, height = 10, dpi = 300)


