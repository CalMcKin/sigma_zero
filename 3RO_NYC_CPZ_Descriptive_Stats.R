# ==============================================================================
# Calculate Descriptive Stats for Project
# Course: SYSEN 5300 - Systems Engineering & Six Sigma
# Group: Sigma 0 (Devin Taylor, Liam Tang, Colton Jacobucci, Caleb McKinney)
# File: <INSERT.R>
# Description: Data loading, cleaning. Visualize data.
#              Clean data.
# Created: 2025-09-14
# Last Updated: 2025-10-23
# ==============================================================================

# ! ! ! IMPORTANT ! ! ! #
# Script MUST be run locally due to memory limitations. 
# ! ! ! IMPORTANT ! ! ! #
#install.packages("moments")
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
  library(moments)
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
  dplyr::select(aqs_id_full, site_name, treated,dist)
# Manually add FDR
sites_tag$treated[sites_tag$site_name == "FDR"] = 1
#print(sites_tag,n=60)
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
# Get shape data for the sites_map
sites_map = sites_sf %>%
  dplyr::mutate(aqs_id_full = norm_id(aqs_id_full)) %>%
  dplyr::left_join(sites_tag, by = "aqs_id_full")
# Only consider data from certain counties for now
ny_metro = counties %>% filter(state == "NY") %>% filter(
  name=="Bronx County" | name=="New York County" | name =="Queens County" |
    name=="Kings County")
# Intersect the sites_map with the ny_metro
aqi_nyc_metro = st_intersection(st_as_sf(sites_map),st_as_sf(ny_metro))
# Filter the aqi_df to only include the sites in the aqi_nyc_metro
aqi_data_nyc_metro = aqi_df %>% filter(aqs_id_full %in% aqi_nyc_metro$aqs_id_full )
# CPZ Data
CPZDate = as.Date("2025-01-05") # day CPZ went into effect
# We can't use the data from before 2024 so might as well filter it out
aqi_data_nyc_metro = aqi_data_nyc_metro %>% filter(date > "2024-01-01")
###############################
#### Describe Data ############
###############################
describe = function(x){
  # Put our vector x in a tibble
  tibble(x) %>%
    # Calculate summary statistics
    summarize(
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      # We'll use the moments package for these two
      skew = skewness(x, na.rm = TRUE),
      kurtosis = kurtosis(x, na.rm = TRUE)) %>%
    # Let's add a caption, that compiles  all these statistics  
    mutate(
      # We'll paste() the following together
      caption = paste(
        # Listing the name of each stat, then reporting its value and rounding it, then separating with " | "
        "Process Mean: ", mean %>% round(2), " | ", 
        "SD: ", sd %>% round(2), " | ",
        "Skewness: ", skew %>% round(2), " | ",
        "Kurtosis: ", kurtosis %>% round(2), 
        # Then make sure no extra spaces separate each item
        sep = "")) %>%
    return()
}
# Describe the AQI 
aqi_desc = aqi_data_nyc_metro$pm25 %>% describe()

#'NAME: get_wigs
# Parameters:
#'@d is the data frame
#'@col is the column name for the subgroup
#' @grp is the name to group by
######
get_wigs = function(d,col,grp){
  wigs = d %>% 
group_by(!!sym(grp)) %>%
reframe(
    xbar = mean(!!sym(col)),
    sd = sd(!!sym(col)),
    n_w = n()
  ) %>% mutate(
    sigma_s = sqrt(mean(sd^2))
  ) %>% mutate (
    se = sigma_s/sqrt(n_w),
    upper = mean(xbar) + 3*se,
    lower = mean(xbar) - 3*se
  ) %>% ungroup()
  wigs
}

# GEN PLOT 
gen_ctrl_plot= function(d,x_col){
  ggplot(data = d,mapping = aes(x = !!sym(x_col), y = xbar)) +
  geom_hline(mapping = aes(yintercept = mean(xbar)), color = "lightgrey", size = 3) +
  geom_ribbon(aes(ymin = lower, ymax = upper,group=1), fill = "red", alpha = 0.5)+
  geom_line(size = 1, aes(group=1))+
  geom_point(size = 2, aes(group=1))+
  theme(axis.text = element_text(hjust = 1, angle = 90))
}

# Create an average control plot
within_group_sn_stats = get_wigs(aqi_data_nyc_metro,"pm25","site_name")
# Plot the within group stats on an average chart
within_group_sn_stats %>% glimpse()
gen_ctrl_plot(within_group_sn_stats,"site_name")

# Create an average control plot
within_group_date_stats = get_wigs(aqi_data_nyc_metro,"pm25","date")
# Plot the within group stats on an average chart
within_group_date_stats %>% glimpse()
gen_ctrl_plot(within_group_date_stats,"date")

ggplot(data = within_group_date_stats, aes(x = xbar)) +
geom_histogram()

ggplot(data = aqi_data_nyc_metro, aes(x = pm25)) +
geom_histogram()


aqi_wicpz_after_24  = aqi_after_24 %>% 
filter(treated == 1)

wig_a24_wicpz = get_wigs(aqi_wicpz_after_24,"pm25","date")

gen_ctrl_plot(wig_a24_wicpz,"date")


# Option 2: Save individual plots for each site
# Get unique site IDs
unique_sites = unique(aqi_data_nyc_metro$aqs_id_full)

# Create directory for plots if it doesn't exist
plot_dir = here("plots", "pm25_by_site")
if (!dir_exists(plot_dir)) {
  dir_create(plot_dir)
}

# Generate and save individual plots
for (site_id in unique_sites) {
  site_data = aqi_data_nyc_metro %>% 
    filter(aqs_id_full == site_id)
  
  if (nrow(site_data) > 0) {
    site_plot = ggplot(data = site_data, 
                       mapping = aes(x = datetime, y = pm25)) +
      geom_line(color = ifelse(unique(site_data$treated) == 1, "orange", "blue"), 
                alpha = 0.7, size = 0.8) +
      geom_vline(xintercept = as.POSIXct(CPZDate), linetype = "dashed", 
                 color = "red", size = 1) +
      labs(
        title = paste("PM2.5 Levels - Site ID:", site_id),
        subtitle = paste("Site Name:", unique(site_data$site_name), 
                         "| Treatment:", ifelse(unique(site_data$treated) == 1, "Treated", "Control")),
        x = "Date",
        y = "PM2.5 (μg/m³)",
        caption = paste("CPZ implementation date:", CPZDate)
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 12, face = "bold")
      )
    
    # Save plot
    ggsave(filename = here(plot_dir, paste0("pm25_", site_id, ".png")),
           plot = site_plot,
           width = 10, height = 6, dpi = 300)
  }
}

message(glue("Saved {length(unique_sites)} individual site plots to {plot_dir}"))

# Glendale, Hunts point, and Williamsburg sites do not have complete datq, drop from dataset
#aqi_data_nyc_metro_tst = aqi_data_nyc_metro %>%
aqi_data_nyc_metro = aqi_data_nyc_metro %>%
filter(aqs_id_full != "840999999912",aqs_id_full != "840999999992",aqs_id_full != "840999999993",pm25>0
       )
#filter((site_name == "Glendale" | site_name == "Hunts Point" | site_name=="Williamsburg"))
 #unique(aqi_data_nyc_metro_tst$aqs_id_full)
 unique(aqi_data_nyc_metro$aqs_id_full)
## ISSUE filtering by site name because DYPLER auto drops NA


#filter(site_name != "Glendale",site_name != "Hunts Point",site_name!="Williamsburg", !is.na(site_name))
# Check the data out
glimpse(aqi_data_nyc_metro)
# See if that helped with invalid data readings
aqi_data_nyc_metro %>% filter(pm25 < 0)
# Plot again
unique_sites = unique(aqi_data_nyc_metro$aqs_id_full)
# Generate and save individual plots
for (site_id in unique_sites) {
  site_data = aqi_data_nyc_metro %>% 
    filter(aqs_id_full == site_id)
  
  if (nrow(site_data) > 0) {
    site_plot = ggplot(data = site_data, 
                       mapping = aes(x = datetime, y = pm25)) +
      geom_line(color = ifelse(unique(site_data$treated) == 1, "orange", "blue"), 
                alpha = 0.7, size = 0.8) +
      geom_vline(xintercept = as.POSIXct(CPZDate), linetype = "dashed", 
                 color = "red", size = 1) +
      labs(
        title = paste("PM2.5 Levels - Site ID:", site_id),
        subtitle = paste("Site Name:", unique(site_data$site_name), 
                         "| Treatment:", ifelse(unique(site_data$treated) == 1, "Treated", "Control")),
        x = "Date",
        y = "PM2.5 (μg/m³)",
        caption = paste("CPZ implementation date:", CPZDate)
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 12, face = "bold")
      )
    
    # Save plot
    ggsave(filename = here(plot_dir, paste0("cleaned_pm25_", site_id, ".png")),
           plot = site_plot,
           width = 10, height = 6, dpi = 300)
  }
}










# It DID!
# Lets save the cleaned data
saveRDS(aqi_data_nyc_metro, file="Data/aqi_data_nyc_metro.rds")
# Describe the data set again!
aqi_desc_n = aqi_data_nyc_metro$pm25 %>% describe()
aqi_desc
aqi_desc_n
rm(list = ls())