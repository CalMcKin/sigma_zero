# sigma_zero
Cornell SYSEN5300 six sigma course project group 'sigma 0'. We aim to answer the question "Does NYC air quality improve with traffic congestion pricing policy?"

## project_new_R_Version.R 
This is a posit cloud friendly R script file that should hold the main project code
Not much in here rn
## CONG_DATA_Parse.R
This script contains the R code to parse the congestion datasets and creates two RDS files
One file contains the congestion zone vehicle entry locations
The other file is the vehicle entry data with location data added
This should allow us to find nearby AQI sensors to each vehicle entry location
## AQI_Data_Parse.R
This script contains the code to parse together the air quality data. It essentially adds location data to the air quality measurements so we can find sensor measurements relevant to the entry zone locations. 
Currently saves an RDS file that contains 1.5 mil rows of aqi measurements for CT, NY, NJ, and PA. Need to narrow this to only sensors near the congestion zone.
WIP: Needs to filter out more locations and should only contain data we will use
## POSIT CAN'T HANDLE THE HEAT -> Reccomended to run Parsing scripts on local machine with plenty of memory
