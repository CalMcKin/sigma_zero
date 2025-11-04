# sigma_zero

Cornell SYSEN5300 six sigma course project group 'sigma 0'. We aim to answer the question "Does NYC air quality improve with traffic congestion pricing policy?"

## R Scripts

### CONG_DATA_Parse.R
Parses congestion pricing vehicle entry data and maps detection groups to road names. Creates spatial geometries for vehicle entry locations by matching detection group names (e.g., "Brooklyn Bridge", "FDR Drive at 60th St") to road geometries in the congestion zone. Outputs two RDS files: `vehicle_entry_locations.rds` (spatial locations of entry points) and `merged_cong.rds` (vehicle entry data with location information).

### NYC_CPZ_Descriptive_Stats.R
Data cleaning and descriptive statistics pipeline. Processes raw air quality data, tags monitoring sites as treated (inside CPZ) or control (outside CPZ), filters to NYC metro area (Bronx, Manhattan, Queens, Kings counties), merges weather data, and removes incomplete sites. Generates control charts, summary statistics, and individual PM2.5 time series plots for each monitoring site. Outputs cleaned dataset as `aqi_data_nyc_metro.rds`.

### NYC_CPZ_Regression.R
Performs difference-in-differences (DiD) regression analysis to estimate the causal effect of congestion pricing on PM2.5 air quality. Creates treatment/post indicators based on CPZ implementation date (January 5, 2025), incorporates congestion zone hours and weather controls, and estimates DiD models with and without weather covariates. Outputs model comparison tables using `modelsummary`.

### NYC_CPZ.R
Creates spatial visualization maps showing the geographic layout of the analysis. Generates plots displaying the congestion pricing zone, air quality monitoring sites (treated and control), and vehicle entry locations. Outputs high-resolution PNG files: `plot_site_names.png` (with site labels) and `plot_road_names.png` (with road entry point labels).

## Note on Script Execution

**Scripts MUST be run locally due to memory limitations.** These scripts process large datasets and require substantial RAM. Recommended to run parsing and analysis scripts on a local machine with plenty of memory.
