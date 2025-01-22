##### CODE DESCRIPTION: 
##### This script sets parameters and runs other R scripts to (optionally) collect data
##### and produce long-term economic and demographic forecasts.

##### 1. Start Timing #####

sys_time_start <- Sys.time()

##### 2. Update and Merge Data #####

message("Running data_update_and_merge.R...")
source("data_update_and_merge.R")
message("Completed data_update_and_merge.R.")

##### 3. Setting Parameters #####

# --- General Forecast Parameters ---

# Store the "current" year after which economic forecasts start
# (does not have to be the actual current year)
current_year <- 2024

# UN population projections used in the model start from this year
# (usually the year it is issued as there is no actual data for that year yet)
UN_pop_projections_start_year <- 2024

# The UN historical demographic data starts from this year for most countries
detailed_demo_data_start_year <- 1950

# Benchmark Country for Relative Variables
benchmark_iso <- "USA"

# --- Investment Ratio Parameters ---

# Long-term investment ratio to GDP (target convergence value)
inv_lt <- 0.24

# Investment ratio convergence speed
# Ratio of difference from long term decaying yearly by this amount
inv_conv_speed <- 0.1

# Investment ratio convergence cutoff
# Ratio jumps to long term value if difference is smaller than this value (percent of GDP)
inv_conv_cutoff <- 0.005

# --- Labor Share Parameters ---

# Labor share where data is missing
labsh_generic <- 0.6

# Labor share convergence speed
# Difference from long term ratio decays yearly by this amount
labsh_conv_speed <- 0.1

# Labor share convergence cutoff
# Ratio jumps to long term value if difference is smaller than this value
labsh_conv_cutoff <- 0.005

# --- Depreciation Parameters ---

# Delta (depreciation of capital rate) where data is missing
delta_generic <- 0.04

# Depreciation convergence speed
# Difference from long term ratio decays yearly by this amount
delta_conv_speed <- 0.1

# Depreciation convergence cutoff
# Ratio jumps to long term value if difference is smaller than this value
delta_conv_cutoff <- 0.0005

# --- Employment Ratio Parameters ---

# Employment to population aged 15-64 ratio converges to this ratio
emp_per_pop_age_15_64_lt <- 0.8

# Employment to flexibly defined working age population converges to this ratio
emp_per_pop_working_age_lt <- 0.8

# Employment ratio deviation convergence speeds
# Employment to population aged 15-64
emp_per_pop_age_15_64_conv_rate <- 0.01

# Employment to working age population
emp_per_pop_working_age_conv_rate <- 0.01

# --- TFP Growth Parameters ---

# Historical TFP growth calculation period
tfp_hist_start_year <- 1999
tfp_hist_end_year <- 2019

# Weight of historical TFP growth rate in calculating the TFP growth for the first forecast year
# In later years, it is the weight of the previous year
# "Catch-up" TFP growth gets the weight of 1 - weight_of_historical_TFP_growth
weight_of_historical_TFP_growth <- 0.8

# Selects which TFP growth model to use: the one that incorporates the youth ratio
# or the plain vanilla one (only the relative level of TFP to the USA is used)
use_demogr_TFP_model <- TRUE

# --- Migration Rate Parameters ---

# The deviation of migration rate from the model prediction decays by this factor
# (following year's deviation is x times this year's)
migr_rate_dev_decay_factor <- 0.95

# These are the benchmark years to calculate the average migration rate deviation from the model
migr_dev_years_to_average <- c(2019, 2021, 2022, 2023)

# --- Resource Economy Parameters ---

# Using two limits for the mean and maximum rent level; if either is reached,
# the country is deemed as a "resource economy"
resource_country_mean_rent_limit <- 0.08
resource_country_max_rent_limit <- 0.1

# --- Capital Stock Calculation Parameters ---

# For alternative capital stock calculations,
# use this year as a starting point for accumulating capital
capital_cumulation_start_year <- 1999

# Migration model uses this time span for estimates
migr_model_start_year <- 2009
migr_model_end_year <- 2019

##### 4. Running Forecaster Script #####

message("Running long_term_GDP_forecaster.R...")
source("long_term_GDP_forecaster.R")
message("Completed long_term_GDP_forecaster.R.")

##### 5. End Timing and Report #####

sys_time_end <- Sys.time()
elapsed_time <- sys_time_end - sys_time_start
elapsed_time