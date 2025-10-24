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
cong_metro_path = paste0(path_to_data, "congestion_pricing/metro.rds") # path to metro rds from congestion zone data
cong_counties_path = paste0(path_to_data, "congestion_pricing/counties.rds") # path to congestion zone county data
cong_vehicles_path = paste0(path_to_data, "congestion_pricing/zone_vehicle_entries.rds") # path to vehicle entries per zone
cong_zones_path = paste0(path_to_data, "congestion_pricing/zone.rds") # path to congestion pricing zone data
cong_roads_path = paste0(path_to_data, "congestion_pricing/roads.rds") # path to congestion pricing zone data
# Data frames
cong_zones_df = readRDS(cong_zones_path) # Create data frame for Congestion Zone data
# Merge congestion zone data
# I don't really know how to join the sensor data to the locations. It will be with either detection_group or detection region column
cong_vehicle_zone_df = readRDS(cong_vehicles_path) # Create a data frame from the data
cong_vehicle_zone_df$detection_group %>% unique() # show the unique location in the detection_group column
paste("#####")
cong_vehicle_zone_df$detection_region %>% unique() # show the unique location in the detection_region column
# Based on the outputs detection group look like road names
# Lets see if they match up with road data.
cong_road_df = readRDS(cong_roads_path)
# cong_road_df$fullname # this displays too much data to sift through
# try to detect strings
if (any(grepl("Bridge", cong_road_df$fullname))){print("hell yeah")}

#cong_road_df[grepl("Joe", cong_road_df$fullname), ] # Filter columns to only display values that contain string
# List of strings used to find matches: Brooklyn, Manhattan, Hugh, 60, East St, 
# based on this search Brooklyn Bridge is actually Brooklyn Brg
# Hugh L. Carey Tunnel is Hugh L Carey Tunl
# Manhattan Bridge is Manhattan Brg
# Williamsburg Bridge is Williamsburg Brg
# East 60th St is E 60th St
# FDR Drive is Fdr Dr	
###  Note for this one specificaly it is FDR Drive intersects East 60th st
# Holland Tunnel is Holland Tunl
# Lincoln Tunnel is Lincoln Tunnel (Toll Rd)
# Queens Midtown Tunnel is Queens Midtown Tunl
# Queensboro Bridge is Queensboro Brg	
# West 60th St is W 60th St
# West Side Highway at 60th St is Joe Dimaggio Hwy
# Note this would have to be where Joe Dimaggio intersects with W 60th st
####
# Now that we have a translation for the vehicle data locations we can join on those names to give the congestion data spatial data
# Create data frame which maps the names
tmp_map = data.frame(
  cong_name = cong_vehicle_zone_df$detection_group %>% unique()
)
tmp_map$road_name = c(
  "Brooklyn Brg",
  "Hugh L Carey Tunl",
  "Manhattan Brg",
  "Williamsburg Brg",
  "E 60th St",
  "Fdr Dr",
  "Holland Tunl",
  "Lincoln Tunnel (Toll Rd)",
  "Queens Midtown Tunl",
  "Queensboro Brg",
  "W 60th St",
  "Joe Dimaggio Hwy"
)
tmp_map

#cong_vehicle_zone_df$road_name = NA
#i = 1 # R is one based
#for (gp in cong_vehicle_zone_df$detection_group) {
#    j = 1
#    for (rd in tmp_map$cong_name){
#        if (gp == rd ){
#              cong_vehicle_zone_df$road_name[i] = tmp_map$road_name[j]
#        }
#        j = j + 1
#    }
#
#    i = i + 1
#}
##merged_aqi_df <- left_join(aqi_df, aqi_sites_df, by = "aqs_id_full")
#
#cong_vehicle_zone_df
# This is way faster than the for loop
cong_vehicle_zone_df$road_name = NA
cong_vehicle_zone_df$road_name[cong_vehicle_zone_df$detection_group == "Brooklyn Bridge"] = "Brooklyn Brg"
cong_vehicle_zone_df$road_name[cong_vehicle_zone_df$detection_group == "Hugh L. Carey Tunnel"] = "Hugh L Carey Tunl"
cong_vehicle_zone_df$road_name[cong_vehicle_zone_df$detection_group == "Manhattan Bridge"] = "Manhattan Brg"
cong_vehicle_zone_df$road_name[cong_vehicle_zone_df$detection_group == "Williamsburg Bridge"] = "Williamsburg Brg"
cong_vehicle_zone_df$road_name[cong_vehicle_zone_df$detection_group == "East 60th St"] = "E 60th St"
cong_vehicle_zone_df$road_name[cong_vehicle_zone_df$detection_group == "FDR Drive at 60th St"] = "Fdr Dr"
cong_vehicle_zone_df$road_name[cong_vehicle_zone_df$detection_group == "Holland Tunnel"] = "Holland Tunl"
cong_vehicle_zone_df$road_name[cong_vehicle_zone_df$detection_group == "Lincoln Tunnel"] = "Lincoln Tunnel (Toll Rd)"
cong_vehicle_zone_df$road_name[cong_vehicle_zone_df$detection_group == "Queens Midtown Tunnel"] = "Queens Midtown Tunl"
cong_vehicle_zone_df$road_name[cong_vehicle_zone_df$detection_group == "Queensboro Bridge"] = "Queensboro Brg"
cong_vehicle_zone_df$road_name[cong_vehicle_zone_df$detection_group == "West 60th St"] = "W 60th St"
cong_vehicle_zone_df$road_name[cong_vehicle_zone_df$detection_group == "West Side Highway at 60th St"] = "Joe Dimaggio Hwy"
#cong_vehicle_zone_df["road_name"] <- ifelse(cong_vehicle_zone_df$detection_group == "Brooklyn Bridge" , "Brooklyn Brg", NA)
#cong_vehicle_zone_df["road_name"] <- ifelse(cong_vehicle_zone_df$detection_group == "Hugh L. Carey Tunnel" , "Hugh L Carey Tunl")
cong_vehicle_zone_df %>% glimpse()

cong_road_df = cong_road_df %>% rename(road_name=fullname) # rename columns to avoid conflicts
merged_cong <- left_join(cong_vehicle_zone_df, cong_road_df, by = "road_name", relationship = "many-to-many")

glimpse(merged_cong)
road_loc <- left_join(tmp_map, cong_road_df, by = "road_name", relationship = "many-to-many")
road_loc %>% glimpse()
# We need to now join the congestion data to the sensor data
# For each of the locations found above for the traffic entries we should find the county of the locatoin
# For each of the locations found aboce for the traffic entires we should find the nearest 5 sensors to use


