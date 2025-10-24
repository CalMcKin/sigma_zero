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
# Lets get to work on a placebo test 
# https://chabefer.github.io/STCI/Placebo.html
# Some info on placebo tests