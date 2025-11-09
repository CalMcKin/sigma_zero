# sigma_zero

Cornell SYSEN5300 six sigma course project group 'sigma 0'. We aim to answer the question "Does NYC air quality improve with traffic congestion pricing policy?"

## R Scripts

### IMPORTANT: Script Execution Order

**If running the analysis from scratch, scripts MUST be executed in numerical order (1 through 5):**

1. `1RO_CONG_DATA_Parse.R` - Must run first
2. `2RO_NYC_DATA_Investigation_NoPlacebo.R` - Run second
3. `3RO_NYC_CPZ_Descriptive_Stats.R` - Run third
4. `4RO_NYC_CPZ_Plots.R` - Run fourth
5. `5RO_NYC_CPZ_Regression.R` - Run fifth

Each script depends on output files created by previous scripts. Skipping or reordering scripts will cause errors.

### 1RO_CONG_DATA_Parse.R
Parses congestion pricing vehicle entry data and maps detection groups to road names. Creates spatial geometries for vehicle entry locations by matching detection group names (e.g., "Brooklyn Bridge", "FDR Drive at 60th St") to road geometries in the congestion zone. 

**Outputs:**
- `Data/vehicle_entry_locations.rds` - Spatial locations of entry points
- `Data/merged_cong.rds` - Vehicle entry data with location information

### 2RO_NYC_DATA_Investigation_NoPlacebo.R
Checks feasibility of placebo test for DiD analysis. Loads air quality and congestion pricing zone data, identifies monitoring sites inside and outside the CPZ, creates visualization plots of site locations. Determines that placebo test is not feasible due to insufficient pre-2019 data for sites inside the CPZ zone (earliest data starts June 2019, insufficient for meaningful placebo analysis).

**Outputs:**
- `plots/sites_plot.png` - Site locations visualization
- `plots/sites_plot_n.png` - Site locations with names

### 3RO_NYC_CPZ_Descriptive_Stats.R
Data cleaning and descriptive statistics pipeline. Processes raw air quality data, tags monitoring sites as treated (inside CPZ) or control (outside CPZ), filters to NYC metro area (Bronx, Manhattan, Queens, Kings counties), removes incomplete sites and invalid readings. Generates control charts, summary statistics, and individual PM2.5 time series plots for each monitoring site.

**Outputs:**
- `Data/aqi_data_nyc_metro.rds` - Cleaned dataset (required for scripts 4 and 5)
- `plots/pm25_by_site/` - Individual PM2.5 time series plots for each site

### 4RO_NYC_CPZ_Plots.R
Creates spatial visualization maps showing the geographic layout of the analysis. Generates plots displaying the congestion pricing zone, air quality monitoring sites (treated and control groups), vehicle entry locations, and surrounding counties.

**Outputs:**
- `plots/plot_site_names.png` - Sites with names labeled
- `plots/plot_site_ids.png` - Sites with ID numbers labeled
- `plots/plot_site_county.png` - Sites with county boundaries
- `plots/plot_road_names.png` - Vehicle entry points with road names
- `plots/plot_counties_cpz.png` - Counties surrounding CPZ

### 5RO_NYC_CPZ_Regression.R
Performs difference-in-differences (DiD) regression analysis to estimate the causal effect of congestion pricing on PM2.5 air quality. Creates treatment/post indicators based on CPZ implementation date (January 5, 2025), incorporates congestion zone hours, weather controls, and site type controls. Estimates DiD models with baseline, simple weather, and full weather specifications. Generates model comparison tables, confidence intervals, and 2x2 DiD visualization plots.

**Outputs:**
- `tables/modelsummary_results.csv` - Model comparison table
- `plots/plot_2x2.png` - 2x2 DiD visualization
- `plots/plot_nice.png` - Enhanced DiD plot with counterfactual

## Note on Script Execution

**Scripts MUST be run locally due to memory limitations.** These scripts process large datasets and require substantial RAM. Recommended to run parsing and analysis scripts on a local machine with plenty of memory.
