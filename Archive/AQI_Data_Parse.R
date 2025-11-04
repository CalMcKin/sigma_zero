# Libraries
#install.packages(c("sf","dplyr","readr","readr","lubridate"))
#install.packages("ggplot2")
# For congestion Pricing and Air Quality
library(dplyr) # for data wrangling
library(readr) # for reading data
library(sf) # for spatial features
library(lubridate) # for date-time formatting
library(ggplot2)

path_to_data = paste0(getwd(),"/Data/") # Create Path to data
# Create paths to data
aqi_csv_path = paste0(path_to_data, "air_quality/air_quality.csv") # path to aqi CSV
aqi_metro_path = paste0(path_to_data, "air_quality/metro.rds") # path to metro.rds
aqi_sites_path = paste0(path_to_data, "air_quality/sites.rds") # path to sites.rds
aqi_counties_path = paste0(path_to_data, "air_quality/counties.geojson") # path to counties geo data
aqi_hw_path = paste0(path_to_data, "air_quality/highways.rds") # path to highway data
aqi_roads_path = paste0(path_to_data, "air_quality/roads.rds") # path to road data
# Data frames
aqi_df = read_csv(aqi_csv_path, show_col_types = FALSE)  # Create data frame by reading AQI_QUALITY.CSV 
aqi_sites_df = read_rds(aqi_sites_path) # Create data from for air quality sites
geojson_data <- st_read(aqi_counties_path) # Create GEO JSON data from GEO data from AQI Counties
geojson_data = geojson_data %>% rename(county_name = name)  # Rename column to avoid conflicts
hw_sf = st_sf(read_rds(aqi_hw_path)) # make a spatial frame from air quality high way data
hw_sf = hw_sf %>% rename(hw_fullname=fullname, # rename columns to avoid conflicts
                         hw_rttyp = rttyp,
                         hw_mtfcc = mtfcc)
road_sf = st_sf(read_rds(aqi_roads_path)) # Create spatial data frame from air quality road data
road_sf = road_sf %>% rename(road_fullname=fullname, # rename columns to avoid conflicts
                             road_rttyp = rttyp,
                             road_mtfcc = mtfcc)


# Merge and join data 
merged_aqi_df <- left_join(aqi_df, aqi_sites_df, by = "aqs_id_full") # Merge the air quality data in the CSV to the sites data via a join on the AQI ID
###### LOCATION FILTER #####
geojson_ny = geojson_data %>% filter(state == "NY" | state == "NJ" | state == "CT" | state == "PA")
geojson_ny_sf = st_as_sf(geojson_ny) # Convert GEO data to spatial data frame
merged_aqi_df_sf = st_as_sf(x=merged_aqi_df)  # convert the merged aqi data into spatial data
#################TEST SECTION##########
#short_aqi_sf = merged_aqi_df_sf[c(1:5),] # Cut down to 5 records so join can be tested
#short_aqi_sf %>% glimpse() # Make sure that worked
#result = st_join(short_aqi_sf,geojson_ny_sf, join = st_intersects) # perform the join via spatial data, join where the AQI sites intersect with the geo data
#result %>% glimpse() # make sure that worked
#################TEST SECTION END #######
# perform join on full dataset
aqi_state_merge = st_join(merged_aqi_df_sf,geojson_ny_sf, join = st_intersects) # join aqi data to geo data to give the dataset counties
# aqi_state_merge %>% glimpse()
##############TEST SECTION###########
aqi_state_merge %>% filter(state == "NA") %>% glimpse() # Should not return any rows with state as NA, if it does something failed
#################TEST SECTION END #######
# WE SHOULD ADD ROADS AND HIGHWAYS WHILE WE ARE AT IT
aqi_state_hw_merge = st_join(aqi_state_merge,hw_sf, join = st_nearest_feature) # using a spatial join to the nearest location add highways
aqi_state_hw_rd_merge = st_join(aqi_state_hw_merge,road_sf, join = st_nearest_feature) # using a spatial join to the nearest location add roads
# Save the dataset
saveRDS(aqi_state_hw_rd_merge, file="Data/air_quality/combined_data_aqi.rds")


merged_aqi_df %>% glimpse()