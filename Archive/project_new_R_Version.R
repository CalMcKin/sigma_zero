# Libraries
#install.packages(c("sf","dplyr","readr","readr","lubridate"))
#install.packages("ggplot2")
# For congestion Pricing and Air Quality
library(dplyr) # for data wrangling
library(readr) # for reading data
library(sf) # for spatial features
library(lubridate) # for date-time formatting
library(ggplot2)


this_data = readRDS("Data/air_quality/combined_data_aqi.rds")
this_data %>% glimpse()
test = readRDS("Data/vehicle_entry_locations.rds")
test2 = readRDS("Data/merged_cong.rds")
# Lets get to work on a placebo test 
# https://chabefer.github.io/STCI/Placebo.html
# Some info on placebo tests

# We need to now join the congestion data to the sensor data
# For each of the locations found above for the traffic entries we should find the county of the locatoin
# For each of the locations found aboce for the traffic entires we should find the nearest 5 sensors to use

