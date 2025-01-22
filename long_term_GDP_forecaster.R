##### CODE DESCRIPTION #####
## This script estimates long-run GDP levels for most countries using the Solow model.
## It estimates missing inputs and generates four growth scenarios:
##    1. Baseline: Uses UN population projections and defines working age as 15-64.
##    2. High Migration: Estimates migration flows and new population numbers.
##    3. Pension Reform: Links working age to life expectancy at age 65.
##    4. Pension Reform + High Migration: Combines scenarios 2 and 3.
## Each scenario incorporates TFP growth convergence based on model estimates.
## Employment per working age population converges to specified ratios.

##### 1. INSTALLING PACKAGES, READING DATA #####

# Load required packages
required_packages <- c("data.table", "fst", "countrycode", "ggplot2")
installed_packages <- rownames(installed.packages())

# Install any missing packages
missing_packages <- setdiff(required_packages, installed_packages)
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load the packages
lapply(required_packages, require, character.only = TRUE)

# Read or update the dataset
if (!file.exists("selected_data.fst")) {
  message("selected_data.fst not found. Running data_update_and_merge.R...")
  source("data_update_and_merge.R")
} else {
  message("Loading data from selected_data.fst...")
  selected_data <- as.data.table(read.fst("selected_data.fst"))
}

##### 2. INITIAL DATA CLEANUP/MODIFICATIONS/VARIABLE CREATION #####

# Find the maximum year in the dataset
max_year <- max(selected_data$year, na.rm = TRUE)

# Latest year of the Penn World Tables (PWT) data
pwt_max_year <- selected_data[!is.na(pop_pwt), max(year, na.rm = TRUE)]

# Insert Kosovo's name (missing at the time of coding)
selected_data[iso3c == "XKX", country_name := "Kosovo"]

# Convert rent percentage to fraction
selected_data[, rent := rent / 100]

# Backfill pop_total (UN population) data to years before 1950 based on Maddison Project population variable
overlapping_isos <- selected_data[year == 1950 & !is.na(pop_total) & !is.na(pop_mpd), iso3c]

selected_data[
  year <= 1950 & iso3c %in% overlapping_isos,
  pop_total := selected_data[iso3c == .BY$iso3c & year == 1950, pop_total] / 
    selected_data[iso3c == .BY$iso3c & year == 1950, pop_mpd] * pop_mpd,
  by = iso3c
]

# For countries where pop_total is missing, use pop_mpd if available
missing_pop_total_isos <- selected_data[, .(sum_pop_total = sum(!is.na(pop_total))), by = iso3c][sum_pop_total == 0, iso3c]

selected_data[
  iso3c %in% missing_pop_total_isos,
  pop_total := pop_mpd * 1000
]

# Create total GDP variable using the UN population number
selected_data[year <= current_year, gdp := gdp_pc * pop_total / 1000]

# Calculate non-resource rent GDP
selected_data[year <= current_year, gdp_nr := gdp * (1 - rent)]

# Calculate GDP and non-resource rent GDP growth rates
selected_data[, gdp_growth := gdp / shift(gdp) - 1, by = iso3c]
selected_data[, gdp_nr_growth := gdp_nr / shift(gdp_nr) - 1, by = iso3c]

# Convert investment percentage to ratio and fill up to the current year with IMF data
selected_data[year <= current_year, inv := inv_imf / 100]

# Identify countries with completely missing IMF investment ratios
imf_inv_compl_missing <- setdiff(
  selected_data[, unique(iso3c)],
  selected_data[!is.na(inv_imf), unique(iso3c)]
)

# Use World Bank data here where IMF is not available at all
selected_data[iso3c %in% imf_inv_compl_missing, inv := inv_wb / 100 ]

# Replace negative investment rates with the average of 2015-2019
selected_data[
  inv <= 0,
  inv := mean(inv[year %in% 2015:2019], na.rm = TRUE),
  by = iso3c
]

# Identify countries with completely missing IMF or World Bank investment ratios
inv_compl_missing <- setdiff(
  selected_data[, unique(iso3c)],
  selected_data[!is.na(inv), unique(iso3c)]
)

# Calculate TFP growth rates
selected_data[, TFP_growth_pwt := (rgdpo / shift(rgdpo)) / 
                (((labsh + shift(labsh)) / 2) * emp_pwt / shift(emp_pwt) +
                   (1 - (labsh + shift(labsh)) / 2) * rnna / shift(rnna)),
              by = iso3c]

selected_data[, TFP_growth_na_pwt := (rgdpna / shift(rgdpna)) / 
                (((labsh + shift(labsh)) / 2) * emp_pwt / shift(emp_pwt) +
                   (1 - (labsh + shift(labsh)) / 2) * rnna / shift(rnna)),
              by = iso3c]

# Create a summary table to identify "resource" countries based on rent levels
resource_countries_help_table <- selected_data[year >= 1999, .(
  mean_rent = mean(rent, na.rm = TRUE),
  max_rent = max(rent, na.rm = TRUE),
  rent_change = rent[year == 2021] - rent[year == 2011],
  mean_rent_since_2009 = mean(rent[year >= 2009], na.rm = TRUE),
  max_rent_since_2009 = max(rent[year >= 2009], na.rm = TRUE)
), by = .(country_name, iso3c)]

resource_countries_help_table[is.na(mean_rent_since_2009), mean_rent_since_2009 := 0]

# Identify "resource" countries based on rent thresholds
resource_countries <- resource_countries_help_table[
  mean_rent >= resource_country_mean_rent_limit | 
    max_rent_since_2009 >= resource_country_max_rent_limit,
  iso3c
]

# (Re)create resource_dummy variable
selected_data[, resource_dummy := fifelse(iso3c %in% resource_countries, 1, 0)]

##### 3. CREATING MISSING GROWTH INPUT VARIABLES #####

##### 3.1 FILLING MISSING VALUES FOR LABOR SHARE AND DELTA #####

# Set keys for efficient data.table operations
setkey(selected_data, iso3c, year)

# Identify the latest year with non-NA labsh and delta for each country
labsh_help_table <- selected_data[!is.na(labsh), .(
  latest_year = max(year)
), by = iso3c]

delta_help_table <- selected_data[!is.na(delta), .(
  latest_year = max(year)
), by = iso3c]

# Function to apply convergence for labsh
apply_convergence_to_labsh <- function(dt, iso, latest_year) {
  
  message("Applying labsh convergence for " , iso)
  
  years_to_fill <- (latest_year + 1):max_year
  
  for (y in years_to_fill) {
    # Update labsh
    if (is.na(dt[iso3c == iso & year == y, labsh]) && !is.na(dt[iso3c == iso & year == y - 1, labsh])) {
      prev_labsh <- dt[iso3c == iso & year == y - 1, labsh]
      current_labsh <- if (abs(prev_labsh - labsh_generic) > labsh_conv_cutoff) {
        prev_labsh - labsh_conv_speed * (prev_labsh - labsh_generic)
      } else {
        labsh_generic
      }
      dt[iso3c == iso & year == y, labsh := current_labsh]
    }
  }
}

# Function to apply convergence for delta
apply_convergence_to_delta <- function(dt, iso, latest_year) {
  
  message("Applying delta convergence for " , iso)
  
  years_to_fill <- (latest_year + 1):max_year
  
  for (y in years_to_fill) {

    # Update delta
    if (is.na(dt[iso3c == iso & year == y, delta]) && !is.na(dt[iso3c == iso & year == y - 1, delta])) {
      prev_delta <- dt[iso3c == iso & year == y - 1, delta]
      current_delta <- if (abs(prev_delta - delta_generic) > delta_conv_cutoff) {
        prev_delta - delta_conv_speed * (prev_delta - delta_generic)
      } else {
        delta_generic
      }
      dt[iso3c == iso & year == y, delta := current_delta]
    }
  }
}

# Apply labsh convergence for each country
labsh_help_table[, apply_convergence_to_labsh(selected_data, iso3c, latest_year), by = iso3c]

# Apply delta convergence for each country
delta_help_table[, apply_convergence_to_delta(selected_data, iso3c, latest_year), by = iso3c]

# Mark filled data
selected_data[
  year >= 1950 & (is.na(labsh) | is.na(delta)),
  filled_labsh_or_delta_data := 1
]

selected_data [ year >= 1950 & is.na(filled_labsh_or_delta_data), filled_labsh_or_delta_data := 0]

# Fill remaining missing labsh and delta with generic values
selected_data[
  year >= 1950 & is.na(labsh),
  labsh := labsh_generic
]

selected_data[
  year >= 1950 & is.na(delta),
  delta := delta_generic
]

# Save intermediate state
write.fst(selected_data, "selected_data_s1.fst")

##### 3.2 FILLING MISSING VALUES FOR THE INVESTMENT TO GDP RATIO #####

# Calculate the latest year with available investment ratio data for each country
inv_help_table <- selected_data[!is.na(inv) & year >= capital_cumulation_start_year, .(
  latest_inv_year = max(year)
), by = iso3c]

# Function to apply investment rate convergence
apply_inv_convergence <- function(dt, iso, latest_inv_year) {
  years_to_fill <- (latest_inv_year + 1):max_year
  
  message(paste("Applying investment rate convergence for: ", iso))
  
  for (y in years_to_fill) {
    if (is.na(dt[iso3c == iso & year == y, inv]) && !is.na(dt[iso3c == iso & year == y - 1, inv])) {
      prev_inv <- dt[iso3c == iso & year == y - 1, inv]
      current_inv <- if (abs(prev_inv - inv_lt) > inv_conv_cutoff) {
        prev_inv - inv_conv_speed * (prev_inv - inv_lt)
      } else {
        inv_lt
      }
      dt[iso3c == iso & year == y, inv := current_inv]
    }
  }
}

# Apply investment rate convergence for each country
inv_help_table[, apply_inv_convergence(selected_data, iso3c, latest_inv_year), by = iso3c]

# Identify countries with completely missing (recent) investment ratios
inv_compl_missing <- setdiff(
  selected_data[, unique(iso3c)],
  selected_data[!is.na(inv) & year >= capital_cumulation_start_year, unique(iso3c)]
)


# Fill investment ratios with long-term value for missing countries
selected_data[
  year >= 1950 & iso3c %in% inv_compl_missing,
  `:=`(filled_inv_ratio_data = 1, inv = inv_lt)
]

selected_data [ year >= 1950 & is.na(filled_inv_ratio_data) , filled_inv_ratio_data := 0]

# Save intermediate state
write.fst(selected_data, "selected_data_s2.fst")

# Remove temporary tables to free memory
rm(inv_help_table, labsh_delta_help_table, inv_compl_missing)
gc()

##### 4. ESTIMATE MISSING/ALTERNATIVE CAPITAL STOCK VARIABLES #####

# Calculate capital-output ratios based on PWT data
selected_data[, k_per_gdp := rnna / rgdpna]

# Calculate capital stock using the capital-output ratio and GDP
selected_data[, capital_stock := k_per_gdp * gdp_pc * pop_total / 1000]

# Indicate the source of the capital stock variable

selected_data[ !is.na(capital_stock), capital_stock_source := "pwt"]

# Estimate the relationship between GDP per capita and capital-output ratio
k_per_gdp_model <- lm(k_per_gdp ~ poly(gdp_pc, 3),
                      data = selected_data[
                        k_per_gdp < 10 &
                          !is.na(k_per_gdp) & 
                          !is.na(gdp_pc) 
                      ])

# plot( seq(0,200000,1000), predict(k_per_gdp_model, newdata = data.table(gdp_pc = seq(0,200000,1000))))

# Create capital stock help table (identifying years when capital stock data can/need to be filled)
capital_stock_help_table <- selected_data[
  year %in% capital_cumulation_start_year:current_year & 
    is.na(k_per_gdp) & 
    !is.na(gdp_pc) & 
    !is.na(inv) &
    filled_inv_ratio_data == 0, 
  .(min_year = min(year), max_year = max(year)), 
  by = .(iso3c, country_name)
]

# Identify countries in the above table with no capital-output ratio data at all
no_k_per_gdp <- selected_data[, .(sum_k_per_gdp = sum(!is.na(k_per_gdp))), by = iso3c][sum_k_per_gdp == 0, iso3c]

# For these countries, predict and fill missing initial k_per_gdp using the estimated model
for ( iso in capital_stock_help_table[iso3c %in% no_k_per_gdp, iso3c]) {
  min_year <- capital_stock_help_table[iso3c == iso, min_year]
  # Predict k_per_gdp for the min year
  predicted_k <- predict(k_per_gdp_model, newdata = selected_data[iso3c == iso & year == min_year])
  selected_data[iso3c == iso & year == min_year, `:=`(k_per_gdp = predicted_k, filled_capital_stock_data = 1)]
  # Calculate capital stock again after prediction
  selected_data[iso3c == iso & year == min_year, `:=`(capital_stock = k_per_gdp * gdp_pc * pop_total / 1000,
                                                      capital_stock_source = "estimate based on GDP per capita")]
}

# Re-create capital stock help table (identifying years when capital stock data can/need to be filled)
capital_stock_help_table <- selected_data[
  year %in% capital_cumulation_start_year:current_year & 
    is.na(k_per_gdp) & 
    !is.na(gdp_pc) & 
    !is.na(inv) &
    filled_inv_ratio_data == 0, 
  .(min_year = min(year), max_year = max(year)), 
  by = .(iso3c, country_name)
]


# Accumulate the capital stock up to the current year or latest year possible
for (iso in unique(capital_stock_help_table$iso3c)) {
  message(paste("Filling k_per_gdp for ISO:", iso))
  
  min_year <- capital_stock_help_table[iso3c == iso, min_year]
  max_year <- capital_stock_help_table[iso3c == iso, max_year]
  years_to_fill <- (min_year ):max_year
  
  # Fill capital stock for subsequent years using the Solow model
  for (y in years_to_fill) {
    prev_capital <- selected_data[iso3c == iso & year == y - 1, capital_stock]
    current_inv <- selected_data[iso3c == iso & year == y - 1, inv]
    current_delta <- selected_data[iso3c == iso & year == y - 1, delta]
    
    # Solow model update: K = (1 - delta) * K_prev + I
    new_capital <- (1 - current_delta) * prev_capital + current_inv * (selected_data[iso3c == iso & year == y - 1, gdp_pc] * selected_data[iso3c == iso & year == y - 1, pop_total] / 1000)
    
    selected_data[iso3c == iso & year == y, `:=`(capital_stock = new_capital, capital_stock_source = "accumulation")]
    
    # Update k_per_gdp
    selected_data[iso3c == iso & year == y, k_per_gdp := capital_stock / (gdp_pc * pop_total / 1000)]
  }
}

# Still missing capital output ratio is filled in from estimate based on GDP per capita

# Identify countries in the above table with no capital-output ratio data at all
k_per_gdp_still_missing <- setdiff(selected_data[, unique(iso3c)], capital_stock_help_table [, iso3c])

capital_stock_help_table_1 <- selected_data[
  year %in% capital_cumulation_start_year:current_year &
    iso3c %in% k_per_gdp_still_missing &
    !is.na(gdp_pc),
  .(min_year = min(year), max_year = max(year)), 
  by = .(iso3c, country_name)
]

# Estimate the capital stock for missing instances where possible
for (iso in unique(capital_stock_help_table_1$iso3c)) {
  message(paste("Filling k_per_gdp for ISO:", iso))
  
  min_year <- capital_stock_help_table_1[iso3c == iso, min_year]
  max_year <- capital_stock_help_table_1[iso3c == iso, max_year]
  years_to_fill <- (min_year ):max_year
  
  # Predict k_per_gdp
  predicted_k <- predict(k_per_gdp_model, newdata = selected_data[iso3c == iso & year %in% years_to_fill])

  # Fill missing k_per_gdp using the prediction
  selected_data [iso3c == iso & year %in% years_to_fill, `:=`(k_per_gdp = predicted_k) ]
  
  # Calculate capital stock 
  selected_data[iso3c == iso & year %in% years_to_fill, `:=`(capital_stock = k_per_gdp * gdp_pc * pop_total / 1000,
                                                      capital_stock_source = "estimate based on GDP per capita")]
}


##### 4.1 ALTERNATIVE CAPITAL STOCK CALCULATION #####

# Function to calculate alternative capital stock by accumulating from a given year
calculate_capital_stock_alt <- function(dt) {

  # Identify the first year of available capital stock by country
  capital_stock_help_table1 <- dt[year >= capital_cumulation_start_year & !is.na(capital_stock) & !is.na(inv), .(
    first_capital_stock_year = min(year)
  ), by = .(iso3c, country_name)]

  # Identify the last year of available gdp data by country
  capital_stock_help_table2 <- dt[!is.na(gdp) & !is.na(inv), .(
    last_gdp_year = max(year)
  ), by = .(iso3c, country_name)]
  
  capital_stock_help_table <- merge ( capital_stock_help_table1, capital_stock_help_table2,
                                      by = c("iso3c", "country_name"))
  
  # Initialize the alternative capital stock variable
  dt[, capital_stock_alt := NA_real_]
  
  # Iterate through each country to compute the alternative capital stock
  for (iso in unique(capital_stock_help_table$iso3c)) {
    message(paste("Calculating alternative capital stock for ISO:", iso))
    
    # Determine the starting year
    first_year <- capital_stock_help_table[iso3c == iso, first_capital_stock_year]
    y_start <- max(capital_cumulation_start_year, first_year)
    
    # Determine the finishing year (latest gdp data )
    last_year <- capital_stock_help_table[iso3c == iso, last_gdp_year]
    y_end <- min(current_year, last_year)
    
    # Assign capital_stock_alt for the starting year
    dt[iso3c == iso & year == y_start, capital_stock_alt := capital_stock]
    
    # Retrieve data for the specific country
    country_data <- dt[iso3c == iso]
    
    # Calculate capital_stock_alt for subsequent years
    for (y in (y_start + 1):(y_end + 1)) {
      prev_year <- y - 1
      prev_capital_alt <- dt[iso3c == iso & year == prev_year, capital_stock_alt]
      prev_delta <- dt[iso3c == iso & year == prev_year, delta]
      prev_gdp_pc <- dt[iso3c == iso & year == prev_year, gdp_pc]
      prev_pop_total <- dt[iso3c == iso & year == prev_year, pop_total]
      prev_inv <- dt[iso3c == iso & year == prev_year, inv]
      
      if (!is.na(prev_capital_alt)) {
        new_capital_alt <- (1 - prev_delta) * prev_capital_alt + prev_inv * (prev_gdp_pc * prev_pop_total / 1000)
        dt[iso3c == iso & year == y, capital_stock_alt := new_capital_alt]
      }
    }
  }
  
  return(dt)
}

# Apply the alternative capital stock calculation
selected_data <- calculate_capital_stock_alt(selected_data)

# Create a summary table for capital stock variables
capital_stock_summary_table <- selected_data[year == current_year, .(
  iso3c,
  country_name,
  capital_stock,
  capital_stock_alt,
  ratio_alt_to_nominal = capital_stock_alt / capital_stock,
  k_per_gdp,
  k_alt_per_gdp = capital_stock_alt / gdp
)]

# Save the capital stock summary
write.fst(capital_stock_summary_table, "capital_stock_summary_table.fst")

# Remove temporary variables to free memory
rm(capital_stock_help_table)
gc()

# Save intermediate state
write.fst(selected_data, "selected_data_s3.fst")

##### 5. PROJECT EMPLOYMENT VIA EMPLOYMENT RATIOS #####

# Calculate available employment ratios through employment per total population ratios
selected_data[, emp_per_pop_age_15_64_pwt := ( (emp_pwt / pop_pwt) * pop_total ) / pop_age_15_64]
selected_data[, emp_per_pop_age_15_64_wb := ( (emp_wb / pop_wb) * (pop_total ) / pop_age_15_64)]
selected_data[, emp_per_pop_working_age_pwt := ( (emp_pwt / pop_pwt) * (pop_total ) / pop_working_age)]
selected_data[, emp_per_pop_working_age_wb := ( (emp_wb / pop_wb) * (pop_total  ) / pop_working_age)]

# Calculate the ratio of population aged 15-64 to total population
selected_data[, pop_age_15_64_per_pop := pop_age_15_64 / pop_total]

# Create mixed source employment per working age population variables, preferring World Bank data
selected_data [ !is.na(emp_per_pop_age_15_64_wb) , `:=`(emp_per_pop_age_15_64 = emp_per_pop_age_15_64_wb,
                                                        emp_source = "wb")]

selected_data [ !is.na(emp_per_pop_working_age_wb) , emp_per_pop_working_age := emp_per_pop_working_age_wb]

# Identify countries with employment data only from PWT
emp_only_pwt <- selected_data[year >= 1950, .(
  sum_emp_wb = sum(!is.na(emp_per_pop_age_15_64_wb)),
  sum_emp_pwt = sum(!is.na(emp_per_pop_age_15_64_pwt))
), by = iso3c][sum_emp_wb == 0 & sum_emp_pwt > 0, iso3c]

# Fill employment ratios for countries with only PWT data
selected_data[iso3c %in% emp_only_pwt, `:=`(
  emp_per_pop_age_15_64 = emp_per_pop_age_15_64_pwt,
  emp_per_pop_working_age = emp_per_pop_working_age_pwt,
  emp_source = "pwt"
)]

# Estimate employment ratio models based on the latest year with sufficient data
emp_model_year <- selected_data[!is.na(emp_per_pop_age_15_64_wb), .N, by = year][N > 50, max(year)]

# Fit linear models for employment ratios
emp_per_pop_age_15_64_model <- lm(emp_per_pop_age_15_64 ~ gdp_per_pop_age_15_64,
                                  data = selected_data[
                                    year == emp_model_year & 
                                      !is.na(emp_per_pop_age_15_64_wb) & 
                                      !is.na(gdp_per_pop_age_15_64) & 
                                      resource_dummy == 0 & 
                                      tax_havens_dummy == 0
                                  ])

emp_per_pop_working_age_model <- lm(emp_per_pop_working_age ~ gdp_per_pop_working_age,
                                    data = selected_data[
                                      year == emp_model_year & 
                                        !is.na(emp_per_pop_age_15_64_wb) & 
                                        !is.na(gdp_per_pop_age_15_64) & 
                                        resource_dummy == 0 & 
                                        tax_havens_dummy == 0
                                    ])

# plot(seq(0,100000,1000), predict(emp_per_pop_age_15_64_model, newdata = data.table(gdp_per_pop_age_15_64 = seq(0,100000,1000))))
# plot(seq(0,100000,1000), predict(emp_per_pop_working_age_model, newdata = data.table(gdp_per_pop_working_age = seq(0,100000,1000))))


# Predict employment ratios based on the models
selected_data[, emp_per_pop_age_15_64_predicted := predict(emp_per_pop_age_15_64_model, newdata = .SD)]
selected_data[, emp_per_pop_age_15_64_predicted := pmin(emp_per_pop_age_15_64_predicted, emp_per_pop_age_15_64_lt)]

selected_data[, emp_per_pop_working_age_predicted := predict(emp_per_pop_working_age_model, newdata = .SD)]
selected_data[, emp_per_pop_working_age_predicted := pmin(emp_per_pop_working_age_predicted, emp_per_pop_working_age_lt)]

# Calculate deviation ratios
selected_data[, emp_per_pop_age_15_64_dev := emp_per_pop_age_15_64 / emp_per_pop_age_15_64_predicted]
selected_data[, emp_per_pop_working_age_dev := emp_per_pop_working_age / emp_per_pop_working_age_predicted]

# Identify countries with completely missing employment data but with some GDP data
emp_missing <- selected_data[year >= 1950, .(
  sum_emp = sum(!is.na(emp_per_pop_age_15_64)),
  sum_gdp = sum(!is.na(gdp_per_pop_age_15_64))
), by = iso3c][sum_emp == 0 & sum_gdp > 0, iso3c]

emp_missing_wa <- selected_data[year >= 1950, .(
  sum_emp = sum(!is.na(emp_per_pop_working_age)),
  sum_gdp = sum(!is.na(gdp_per_pop_working_age))
), by = iso3c][sum_emp == 0 & sum_gdp > 0, iso3c]

# Fill employment ratios with predicted values for missing data
selected_data[iso3c %in% emp_missing & year <= current_year & !is.na(gdp_per_pop_age_15_64),
              `:=` (emp_per_pop_age_15_64 = emp_per_pop_age_15_64_predicted,
                    emp_source = "estimate")]

selected_data[iso3c %in% emp_missing_wa & year <= current_year & !is.na(gdp_per_pop_working_age),
              emp_per_pop_working_age := emp_per_pop_working_age_predicted]

# Remove temporary variables to free memory
rm(emp_missing, emp_missing_wa)
gc()

# Calculate employment in millions
selected_data[, emp := emp_per_pop_age_15_64 * pop_age_15_64 / 1000]
selected_data[, emp_pr := emp_per_pop_working_age * pop_working_age / 1000]

# Calculate deviation ratios with convergence
# Identify the latest year with deviation data for each country
emp_help_table <- selected_data[!is.na(emp_per_pop_age_15_64_dev), .(
  latest_emp_dev_year = max(year)
), by = .(iso3c, country_name)]

emp_help_table_wap <- selected_data[!is.na(emp_per_pop_working_age_dev), .(
  latest_emp_wa_dev_year = max(year)
), by = .(iso3c, country_name)]

# Merge the two help tables
emp_help_table <- merge(emp_help_table, emp_help_table_wap, by = c("iso3c", "country_name"), all = TRUE)

# Function to apply employment deviation convergence
apply_emp_convergence <- function(dt, iso, latest_year, latest_year_wa, max_year) {
  # Apply convergence for age 15-64
  if (!is.na(latest_year) && latest_year < max_year) {
    for (y in (latest_year + 1):max_year) {
      # Retrieve the current deviation
      current_dev <- dt[iso3c == iso & year == (y - 1), emp_per_pop_age_15_64_dev]
      
      # Check if current_dev is available
      if (length(current_dev) == 0 || is.na(current_dev)) {
        warning(paste("Missing emp_per_pop_age_15_64_dev for", iso, "year", y - 1))
        break
      }
      
      # Calculate the new deviation
      new_dev <- 1 + (current_dev - 1) * (1 - emp_per_pop_age_15_64_conv_rate)
      
      # Update the deviation for the current year
      dt[iso3c == iso & year == y, emp_per_pop_age_15_64_dev := new_dev]
    }
  }
  
  # Apply convergence for working age
  if (!is.na(latest_year_wa) && latest_year_wa < max_year) {
    for (y in (latest_year_wa + 1):max_year) {
      # Retrieve the current deviation for working age
      current_dev_wa <- dt[iso3c == iso & year == (y - 1), emp_per_pop_working_age_dev]
      
      # Check if current_dev_wa is available
      if (length(current_dev_wa) == 0 || is.na(current_dev_wa)) {
        warning(paste("Missing emp_per_pop_working_age_dev for", iso, "year", y - 1))
        break
      }
      
      # Calculate the new deviation for working age
      new_dev_wa <- 1 + (current_dev_wa - 1) * (1 - emp_per_pop_working_age_conv_rate)
      
      # Update the deviation for the current year
      dt[iso3c == iso & year == y, emp_per_pop_working_age_dev := new_dev_wa]
    }
  }
}

# Define (again) max_year based on the dataset 
max_year <- max(selected_data$year, na.rm = TRUE)

# Apply employment deviation convergence for each country
emp_help_table[, apply_emp_convergence(selected_data, iso3c, latest_emp_dev_year, latest_emp_wa_dev_year, max_year), by = iso3c]

# Set deviation ratios to 1 where still missing
selected_data[year >= 1950 & is.na(emp_per_pop_age_15_64_dev), emp_per_pop_age_15_64_dev := 1]
selected_data[year >= 1950 & is.na(emp_per_pop_working_age_dev), emp_per_pop_working_age_dev := 1]

# Adjust employment ratios based on deviation
selected_data[
  year <= current_year & is.na(emp_per_pop_age_15_64) & !is.na(gdp_per_pop_age_15_64),
  emp_per_pop_age_15_64 := emp_per_pop_age_15_64_predicted * emp_per_pop_age_15_64_dev
]

selected_data[
  year <= current_year & is.na(emp_per_pop_working_age) & !is.na(gdp_per_pop_working_age),
  emp_per_pop_working_age := emp_per_pop_working_age_predicted * emp_per_pop_working_age_dev
]

# Calculate employment where data is still missing
selected_data[is.na(emp), emp := emp_per_pop_age_15_64 * pop_age_15_64 / 1000]
selected_data[is.na(emp_pr), emp_pr := emp_per_pop_working_age * pop_working_age / 1000]

##### 6. FILL IN MISSING RESOURCES GDP DATA #####

# Calculate the "rent" part of GDP
selected_data[, gdp_rent := gdp * rent]

# Assume zero rent for countries with no rent data
no_rent_data_isos <- selected_data[, .(sum_rent = sum(rent, na.rm = TRUE)), by = iso3c][sum_rent == 0, iso3c]

selected_data[iso3c %in% no_rent_data_isos, gdp_rent := 0]

# Carry forward the last available "rent GDP" when it is no longer available
selected_data[, gdp_rent := nafill(gdp_rent, type = "locf"), by = iso3c]

# Calculate "non-rent" GDP (part of GDP not resulting from resource rents)
selected_data[year <= current_year, gdp_nr := gdp - gdp_rent]

##### 7. TFP: HISTORICAL GROWTH AND LEVEL CALCULATION #####

# Calculate historical yearly TFP growth
selected_data[, TFP_growth := (gdp / shift(gdp)) / 
                (((labsh + shift(labsh)) / 2) * emp / shift(emp) + 
                   (1 - (labsh + shift(labsh)) / 2) * capital_stock / shift(capital_stock)),
              by = iso3c]

# Calculate historical TFP growth for non-resource GDP
selected_data[, TFP_nr_growth := (gdp_nr / shift(gdp_nr)) / 
                (((labsh + shift(labsh)) / 2) * emp / shift(emp) + 
                   (1 - (labsh + shift(labsh)) / 2) * capital_stock / shift(capital_stock)),
              by = iso3c]

# Calculate historical TFP growth for non-resource GDP with alternative capital stock
selected_data[, TFP_nr_alt_growth := (gdp_nr / shift(gdp_nr)) / 
                (((labsh + shift(labsh)) / 2) * emp / shift(emp) + 
                   (1 - (labsh + shift(labsh)) / 2) * capital_stock_alt / shift(capital_stock_alt)),
              by = iso3c]

# Calculate employment and capital stock ratios
selected_data[, `:=`(
  emp_per_pop = 100 * emp * 1000 / pop_total,
  k_per_pop = capital_stock / pop_total,
  k_per_pop_alt = capital_stock_alt / pop_total
)]

# Calculate capital/employment and GDP per employment ratios
selected_data[, `:=`(
  k_per_emp = capital_stock / emp,
  k_per_emp_alt = capital_stock_alt / emp,
  gdp_per_emp = gdp / emp
)]

# Calculate relative capital/employment and GDP per employment ratios (relative to USA)
selected_data[, `:=`(
  k_per_emp_rel = k_per_emp / k_per_emp[iso3c == "USA"],
  k_per_emp_rel_alt = k_per_emp_alt / k_per_emp_alt[iso3c == "USA"],
  gdp_per_emp_rel = gdp_per_emp / gdp_per_emp[iso3c == "USA"]
), by = year]

# Calculate TFP level and relative TFP level
selected_data[, `:=`(
  TFP_level_calc = gdp_pc / ((emp_per_pop ^ labsh) * (k_per_pop ^ (1 - labsh))),
  TFP_level_calc_nr = (gdp_nr / pop_total * 1000) / ((emp_per_pop ^ labsh) * (k_per_pop ^ (1 - labsh)))
), by = iso3c]

selected_data[, `:=`(
  TFP_level_rel_calc = TFP_level_calc / TFP_level_calc[iso3c == "USA"],
  TFP_level_rel_calc_nr = TFP_level_calc_nr / TFP_level_calc_nr[iso3c == "USA"]
), by = year]


# Assign "official" relative TFP variable (excluding resource rent)
selected_data[year <= current_year, TFP_level_rel_nr := TFP_level_rel_calc_nr]

# Calculate TFP level with alternative capital stock
selected_data[, `:=`(
  TFP_level_calc_nr_alt = (gdp_nr / pop_total * 1000) / ((emp_per_pop ^ labsh) * (k_per_pop_alt ^ (1 - labsh)))
), by = iso3c]

selected_data[, `:=`(
  TFP_level_rel_calc_nr_alt = TFP_level_calc_nr_alt / TFP_level_calc_nr_alt[iso3c == "USA"]
), by = year]


# Create and save a summary table of TFP
TFP_summary_table <- selected_data[year == pwt_max_year, .(
  year,
  iso3c,
  country_name,
  TFP_level_rel_nr,
  TFP_level_rel_calc_nr_alt,
  ctfp,
  gdp_per_emp_rel,
  k_per_emp_rel,
  k_per_emp_rel_alt,
  k_per_gdp
)]

write.fst(TFP_summary_table, "TFP_summary_table.fst")

# Define a rolling cumulative product function
rolling_cumprod <- function(dt, col, N) {
  Reduce(`*`, shift(dt[[col]], n = 1:N, type = "lead"))
}

# Calculate 10-year forward TFP growth
selected_data[, TFP_10y_fwrd_growth := (rolling_cumprod(selected_data, "TFP_growth", 10) ^ 0.1) - 1]
selected_data[, TFP_nr_10y_fwrd_growth := (rolling_cumprod(selected_data, "TFP_nr_growth", 10) ^ 0.1) - 1]

# Calculate 10-year past TFP growth
selected_data[, TFP_10y_past_growth := shift(TFP_10y_fwrd_growth, 10), by = iso3c]

# Assess persistence in TFP growth with linear models
summary(lm(TFP_10y_fwrd_growth ~ TFP_10y_past_growth, data = selected_data[resource_dummy == 0 & tax_havens_dummy == 0]))
summary(lm(TFP_growth ~ TFP_10y_past_growth, data = selected_data[
  !is.na(TFP_growth) & 
    !is.na(TFP_10y_past_growth),
  .(TFP_growth = shift(TFP_growth, type = "lead"), TFP_10y_past_growth), 
  by = iso3c
]))

# Estimate multi-year TFP growth models

# Total TFP, including rents 
TFP_model_multiyear <- lm(
  TFP_growth ~ log(TFP_level_rel),
  weights = gdp,
  data = selected_data[
    year >= tfp_hist_start_year & 
      year <= (tfp_hist_end_year - 10) & 
      resource_dummy == 0 & 
      tax_havens_dummy == 0,
    .(iso3c, country_name, gdp, TFP_level_rel = fifelse(!is.na(TFP_level_rel_calc), TFP_level_rel_calc, ctfp), 
      TFP_growth = TFP_10y_fwrd_growth, youth_ratio)
  ][!is.na(TFP_level_rel) & !is.na(TFP_growth) & !is.na(youth_ratio)]
)

# No-rent TFP model
TFP_nr_model_multiyear <- lm(
  TFP_nr_growth ~ log(TFP_level_rel_nr),
  weights = gdp_nr,
  data = selected_data[
    year >= tfp_hist_start_year & 
      year <= (tfp_hist_end_year - 10) & 
      resource_dummy == 0 & 
      tax_havens_dummy == 0,
    .(iso3c, country_name, gdp_nr, TFP_level_rel_nr, TFP_nr_growth = TFP_nr_10y_fwrd_growth, youth_ratio)
  ][!is.na(TFP_level_rel_nr) & !is.na(TFP_nr_growth) & !is.na(youth_ratio)]
)

# Estimate mixed model incorporating resource rents
TFP_model_multiyear_mixed <- lm(
  TFP_growth ~ log(TFP_level_rel) + mean_rent + mean_rent * log(TFP_level_rel),
  weights = gdp,
  data = merge(
    resource_countries_help_table,
    selected_data[
      year >= tfp_hist_start_year & 
        year <= (tfp_hist_end_year - 10) & 
        tax_havens_dummy == 0,
      .(iso3c, country_name, gdp, TFP_level_rel = fifelse(!is.na(TFP_level_rel_calc), TFP_level_rel_calc, ctfp), 
        TFP_growth = TFP_10y_fwrd_growth, youth_ratio)
    ][!is.na(TFP_level_rel) & !is.na(TFP_growth) & !is.na(youth_ratio)],
    by = c("iso3c", "country_name")
  )[!is.na(mean_rent)]
)

# Estimate model incorporating the youth ratio ("demography")
TFP_demog_model <- lm(
  TFP_nr_growth ~ log(TFP_level_rel_nr) + poly(youth_ratio, 2),
  data = selected_data[
    year >= 1980 & 
      year <= (current_year - 10) & 
      resource_dummy == 0 & 
      tax_havens_dummy == 0,
    .(iso3c, country_name, gdp_nr, TFP_level_rel_nr, TFP_nr_growth = TFP_nr_10y_fwrd_growth, youth_ratio)
  ][!is.na(TFP_level_rel_nr) & !is.na(TFP_nr_growth) & !is.na(youth_ratio)]
)

##### 8. CREATE AND PROCESS GROWTH HELP TABLE #####

# Step 1: Create growth_help_table for TFP_growth
growth_help_table <- selected_data[!is.na(TFP_growth),
                                   .(
                                     first_full_year_TFP = min(year, na.rm = TRUE),
                                     last_full_year_TFP = max(year, na.rm = TRUE),
                                     historical_TFP_growth = NA_real_
                                   ),
                                   by = .(iso3c, country_name)]

# Step 2: Create growth_help_table_nr for TFP_nr_growth
growth_help_table_nr <- selected_data[!is.na(TFP_nr_growth),
                                      .(
                                        first_full_year_TFP_nr = min(year, na.rm = TRUE),
                                        last_full_year_TFP_nr = max(year, na.rm = TRUE),
                                        historical_TFP_nr_growth = NA_real_
                                      ),
                                      by = .(iso3c, country_name)]

# Step 3: Merge the two tables on iso3c and country_name
growth_help_table <- merge(growth_help_table, growth_help_table_nr,
                           by = c("iso3c", "country_name"),
                           all = TRUE)

# Clean up temporary table and free memory
rm(growth_help_table_nr)
gc()

# Step 4: Define a helper function to calculate geometric mean
geometric_mean <- function(x) {
  if (all(is.na(x) | x <= 0)) return(NA_real_)
  exp(mean(log(x[x > 0]), na.rm = TRUE))
}

# Step 5: Calculate historical TFP growth metrics
for (i in 1:nrow(growth_help_table)) {
  iso <- growth_help_table$iso3c[i]
  country <- growth_help_table$country_name[i]
  
  # Determine start and end years for TFP_growth
  start_year <- ifelse(growth_help_table$first_full_year_TFP[i] > (tfp_hist_start_year + 1),
                       growth_help_table$first_full_year_TFP[i],
                       tfp_hist_start_year + 1)
  
  end_year <- ifelse(growth_help_table$last_full_year_TFP[i] > tfp_hist_end_year,
                     tfp_hist_end_year,
                     growth_help_table$last_full_year_TFP[i])
  
  # Calculate geometric mean of TFP_growth over the period
  tfp_growth_values <- selected_data[iso3c == iso & year >= start_year & year <= end_year, TFP_growth]
  historical_tfp_growth <- geometric_mean(tfp_growth_values)
  
  # Assign to growth_help_table
  growth_help_table$historical_TFP_growth[i] <- historical_tfp_growth
  
  # Determine start and end years for TFP_nr_growth
  start_year_nr <- ifelse(growth_help_table$first_full_year_TFP_nr[i] > (tfp_hist_start_year + 1),
                          growth_help_table$first_full_year_TFP_nr[i],
                          tfp_hist_start_year + 1)
  
  end_year_nr <- ifelse(growth_help_table$last_full_year_TFP_nr[i] > tfp_hist_end_year,
                        tfp_hist_end_year,
                        growth_help_table$last_full_year_TFP_nr[i])
  
  # Calculate geometric mean of TFP_nr_growth over the period
  tfp_nr_growth_values <- selected_data[iso3c == iso & year >= start_year_nr & year <= end_year_nr, TFP_nr_growth]
  historical_tfp_nr_growth <- geometric_mean(tfp_nr_growth_values)
  
  # Assign to growth_help_table
  growth_help_table$historical_TFP_nr_growth[i] <- historical_tfp_nr_growth
  
  # Calculate mean_resource_rent over the period
  rent_values <- selected_data[iso3c == iso & year >= start_year & year <= end_year, rent]
  mean_resource_rent <- mean(rent_values, na.rm = TRUE)
  
  # Assign to growth_help_table
  growth_help_table$mean_resource_rent[i] <- mean_resource_rent
  
  # Get start_year_rel_TFP from TFP_level_rel_nr at start_year_nr
  start_year_rel_TFP <- selected_data[iso3c == iso & year == start_year_nr, TFP_level_rel_nr]
  
  # Assign to growth_help_table
  growth_help_table$start_year_rel_TFP[i] <- start_year_rel_TFP
}

# Step 6: Update TFP_level_rel using TFP_growth and USA's TFP_growth
# Assign initial values
selected_data[, TFP_level_rel := TFP_level_rel_calc]

# Get USA's TFP_growth data
usa_tfp_growth <- selected_data[iso3c == "USA", .(year, usa_TFP_growth = TFP_growth)]

# Get minimum of the last full year of TFP
min_last_full_year_TFP <- growth_help_table[, min(last_full_year_TFP_nr, na.rm = TRUE)]

# Iterate over each country to update TFP_level_rel
for (i in 1:nrow(growth_help_table)) {
  iso <- growth_help_table$iso3c[i]
  country <- growth_help_table$country_name[i]
  
  message(paste("Updating TFP_level_rel for ISO:", iso))
  
  # Filter country data after min_last_full_year_TFP
  country_data <- selected_data[iso3c == iso & year > min_last_full_year_TFP & year <= current_year]
  
  # Merge with USA's TFP_growth
  merged_data <- merge(country_data, usa_tfp_growth, by = "year", all.x = TRUE)
  
  # Calculate new TFP_level_rel
  merged_data[, new_TFP := TFP_level_rel * TFP_growth / usa_TFP_growth]
  
  # Update the main data.table
  selected_data[iso3c == iso & year > min_last_full_year_TFP & year <= current_year, TFP_level_rel := merged_data$new_TFP]
}

# Step 7: Calculate non-resource relative TFP up to the current year
# Assign initial values
selected_data[year <= current_year, TFP_level_rel_nr := TFP_level_rel_calc_nr]

# Get USA's TFP_nr_growth data
usa_tfp_nr_growth <- selected_data[iso3c == "USA", .(year, usa_TFP_nr_growth = TFP_nr_growth)]

# Iterate over each country to update TFP_level_rel_nr
for (i in 1:nrow(growth_help_table)) {
  iso <- growth_help_table$iso3c[i]
  country <- growth_help_table$country_name[i]
  
  message(paste("Updating TFP_level_rel_nr for ISO:", iso))
  
  # Filter country data after min_last_full_year_TFP
  country_data <- selected_data[iso3c == iso & year > min_last_full_year_TFP & year <= current_year]
  
  # Merge with USA's TFP_nr_growth
  merged_data <- merge(country_data, usa_tfp_nr_growth, by = "year", all.x = TRUE)
  
  # Calculate new TFP_level_rel_nr
  merged_data[, new_TFP_nr := TFP_level_rel_nr * TFP_nr_growth / usa_TFP_nr_growth]
  
  # Update the main data.table
  selected_data[iso3c == iso & year > min_last_full_year_TFP & year <= current_year, TFP_level_rel_nr := merged_data$new_TFP_nr]
}

# Step 8: Calculate historical TFP growth deviations from model predictions
# Calculate TFP_growth_deviation
selected_data[year %in% 1950:current_year, TFP_growth_deviation := TFP_growth / 
                (1 + predict(TFP_model_multiyear,
                             newdata = .SD)),
              by = iso3c]

# Calculate TFP_nr_demog_growth_deviation
selected_data[year %in% 1950:current_year, TFP_nr_demog_growth_deviation := TFP_nr_growth / 
                (1 + predict(TFP_demog_model,
                             newdata = .SD)),
              by = iso3c]

# Calculate TFP_nr_growth_deviation
selected_data[year %in% 1950:current_year, TFP_nr_growth_deviation := TFP_nr_growth / 
                (1 + predict(TFP_nr_model_multiyear,
                             newdata = .SD)),
              by = iso3c]

# Step 9: Recalculate historical TFP growth with deviations and other metrics
# Define a helper function to calculate additional historical metrics
calculate_additional_historical_metrics <- function(dt, growth_ht, tfp_hist_start_year, tfp_hist_end_year, min_last_full_year_TFP, current_year) {
  for (i in 1:nrow(growth_ht)) {
    iso <- growth_ht$iso3c[i]
    country <- growth_ht$country_name[i]
    
    message(paste("Calculating additional historical metrics for ISO:", iso))
    
    # Determine start and end years for TFP
    start_year <- ifelse(growth_ht$first_full_year_TFP[i] > (tfp_hist_start_year + 1),
                         growth_ht$first_full_year_TFP[i],
                         tfp_hist_start_year + 1)
    
    end_year <- ifelse(growth_ht$last_full_year_TFP[i] > tfp_hist_end_year,
                       tfp_hist_end_year,
                       growth_ht$last_full_year_TFP[i])
    
    # Determine start and end years for non-resource TFP
    start_year_nr <- ifelse(growth_ht$first_full_year_TFP_nr[i] > (tfp_hist_start_year + 1),
                            growth_ht$first_full_year_TFP_nr[i],
                            tfp_hist_start_year + 1)
    
    end_year_nr <- ifelse(growth_ht$last_full_year_TFP_nr[i] > tfp_hist_end_year,
                          tfp_hist_end_year,
                          growth_ht$last_full_year_TFP_nr[i])
    
    # Calculate historical_TFP_growth
    if (end_year >= start_year) {
      tfp_growth_values <- dt[iso3c == iso & year >= start_year & year <= end_year, TFP_growth_deviation]
      historical_tfp_growth_dev <- geometric_mean(tfp_growth_values)
      growth_ht$historical_TFP_growth_deviation[i] <- historical_tfp_growth_dev
    } else {
      growth_ht$historical_TFP_growth_deviation[i] <- NA_real_
    }
    
    # Calculate historical_TFP_nr_growth_deviation
    if (end_year_nr >= start_year_nr) {
      tfp_nr_growth_dev_values <- dt[iso3c == iso & year >= start_year_nr & year <= end_year_nr, TFP_nr_growth_deviation]
      historical_tfp_nr_growth_dev <- geometric_mean(tfp_nr_growth_dev_values)
      growth_ht$historical_TFP_nr_growth_deviation[i] <- historical_tfp_nr_growth_dev
    } else {
      growth_ht$historical_TFP_nr_growth_deviation[i] <- NA_real_
    }
    
    # Calculate historical_TFP_nr_demog_growth_deviation
    if (end_year_nr >= start_year_nr) {
      tfp_nr_demog_growth_dev_values <- dt[iso3c == iso & year >= start_year_nr & year <= end_year_nr, TFP_nr_demog_growth_deviation]
      historical_tfp_nr_demog_growth_dev <- geometric_mean(tfp_nr_demog_growth_dev_values)
      growth_ht$historical_TFP_nr_demog_growth_deviation[i] <- historical_tfp_nr_demog_growth_dev
    } else {
      growth_ht$historical_TFP_nr_demog_growth_deviation[i] <- NA_real_
    }
    
    # Calculate standard deviations
    if (end_year >= start_year) {
      tfp_growth_dev_sd <- sd(dt[iso3c == iso & year >= start_year & year <= end_year, TFP_growth_deviation], na.rm = TRUE)
      growth_ht$TFP_growth_dev_sd[i] <- tfp_growth_dev_sd
    } else {
      growth_ht$TFP_growth_dev_sd[i] <- NA_real_
    }
    
    if (end_year >= start_year) {
      gdp_growth_sd <- sd(dt[iso3c == iso & year >= start_year & year <= end_year, gdp_growth], na.rm = TRUE)
      growth_ht$gdp_growth_sd[i] <- gdp_growth_sd
    } else {
      growth_ht$gdp_growth_sd[i] <- NA_real_
    }
    
    # Calculate mean_resource_rent (already calculated earlier, but included here for completeness)
    mean_rent <- growth_ht$mean_resource_rent[i]
    
    # Calculate mean_gdp_growth
    if (end_year >= start_year) {
      gdp_end <- dt[iso3c == iso & year == end_year, gdp]
      gdp_start <- dt[iso3c == iso & year == (start_year - 1), gdp]
      if (!is.na(gdp_end) && !is.na(gdp_start) && gdp_start > 0) {
        mean_gdp_growth <- (gdp_end / gdp_start)^(1 / (end_year - start_year + 1)) - 1
      } else {
        mean_gdp_growth <- NA_real_
      }
      growth_ht$mean_gdp_growth[i] <- mean_gdp_growth
    } else {
      growth_ht$mean_gdp_growth[i] <- NA_real_
    }
    
    # Calculate mean_gdp_nr_growth
    if (end_year_nr >= start_year_nr) {
      gdp_nr_end <- dt[iso3c == iso & year == end_year_nr, gdp_nr]
      gdp_nr_start <- dt[iso3c == iso & year == (start_year_nr - 1), gdp_nr]
      if (!is.na(gdp_nr_end) && !is.na(gdp_nr_start) && gdp_nr_start > 0) {
        mean_gdp_nr_growth <- (gdp_nr_end / gdp_nr_start)^(1 / (end_year_nr - start_year_nr + 1)) - 1
      } else {
        mean_gdp_nr_growth <- NA_real_
      }
      growth_ht$mean_gdp_nr_growth[i] <- mean_gdp_nr_growth
    } else {
      growth_ht$mean_gdp_nr_growth[i] <- NA_real_
    }
  }
  
  return(growth_ht)
}

# Apply the additional historical metrics calculation
growth_help_table <- calculate_additional_historical_metrics(selected_data, growth_help_table, tfp_hist_start_year, tfp_hist_end_year, min_last_full_year_TFP, current_year)

# Step 10: Save the growth_help_table and selected_data
write.fst(growth_help_table, "growth_help_table.fst")
write.fst(selected_data, "selected_data_s4.fst")
# To reload later:
# growth_help_table <- as.data.table(read.fst("growth_help_table.fst"))
# selected_data <- as.data.table(read.fst("selected_data_s4.fst"))

##### 9. CALCULATE GROWTH, MAIN/PENSION REFORM SCENARIO #####

# Find the maximum year in the dataset
max_year <- max(selected_data$year, na.rm = TRUE)

# Step 1: Initialize Flexible Working Age Scenario Variables
selected_data[year <= current_year, `:=`(
  emp_pr = emp,
  capital_stock_pr = capital_stock,
  TFP_nr_pr_growth = TFP_nr_growth,
  TFP_level_rel_nr_pr = TFP_level_rel_nr,
  gdp_pr_growth = gdp_growth,
  gdp_pr = gdp,
  gdp_nr_pr = gdp_nr,
  gdp_pc_pr = gdp_pc
)]

# Step 2: Calculate Youth Ratio for Flexible Pension Age Scenario
selected_data[, youth_ratio_pr := youth_ratio]

# Step 3: Select the Appropriate Column for Historical TFP Growth Deviation
historical_TFP_growth_deviation_column <- ifelse(use_demogr_TFP_model,
                                                 "historical_TFP_nr_demog_growth_deviation",
                                                 "historical_TFP_nr_growth_deviation")

# Step 4: Sequential Growth Calculation

# Pre-calculate useful constant
min_last_full_year <- min(growth_help_table$last_full_year_TFP_nr, na.rm = TRUE)

# Define a helper function to calculate geometric mean
geometric_mean <- function(x) {
  if (all(is.na(x) | x <= 0)) return(NA_real_)
  exp(mean(log(x[x > 0]), na.rm = TRUE))
}

# Iterate through each year for growth calculations
for (y in (min_last_full_year + 1):max_year) {
  message(paste("Processing Year:", y))
  
  # Identify countries where last_full_year_TFP_nr < y
  current_iso_list <- growth_help_table[last_full_year_TFP_nr < y, iso3c]
  
  # Loop through each country ISO code
  for (iso in current_iso_list) {
    
    # Fetch previous and current year data for the country
    prev_data <- selected_data[iso3c == iso & year == (y - 1)]
    curr_data <- selected_data[iso3c == iso & year == y]
    
    # Skip if either previous or current year data is missing
    if (nrow(prev_data) == 0 || nrow(curr_data) == 0) next
    
    # Calculate emp_per_pop_age_15_64_predicted_new
    emp_pred <- predict(emp_per_pop_age_15_64_model, newdata = data.frame(gdp_per_pop_age_15_64 = prev_data$gdp_per_pop_age_15_64))
    emp_per_pop_age_15_64_predicted_new <- pmin(emp_pred, emp_per_pop_age_15_64_lt)
    
    # Calculate new employment per 15-64 population with deviation
    emp_per_pop_age_15_64_new <- emp_per_pop_age_15_64_predicted_new * curr_data$emp_per_pop_age_15_64_dev
    
    # Adjust emp_per_pop_age_15_64_new to long-term targets
    if (prev_data$emp_per_pop_age_15_64 <= emp_per_pop_age_15_64_lt && emp_per_pop_age_15_64_new > emp_per_pop_age_15_64_lt) {
      emp_per_pop_age_15_64_new <- emp_per_pop_age_15_64_lt
    } else if (prev_data$emp_per_pop_age_15_64 > emp_per_pop_age_15_64_lt) {
      emp_per_pop_age_15_64_new <- emp_per_pop_age_15_64_lt + 
        (1 - emp_per_pop_age_15_64_conv_rate) * (prev_data$emp_per_pop_age_15_64 - emp_per_pop_age_15_64_lt)
    }
    
    # Calculate new employment values
    emp_new <- emp_per_pop_age_15_64_new * (curr_data$pop_age_15_64 / 1000)
    
    # Calculate emp_per_pop_working_age_predicted_new
    emp_pr_pred <- predict(emp_per_pop_working_age_model, newdata = data.frame(gdp_per_pop_working_age = prev_data$gdp_per_pop_working_age))
    emp_per_pop_working_age_predicted_new <- pmin(emp_pr_pred, emp_per_pop_working_age_lt)
    
    # Calculate new employment per working age population with deviation
    emp_per_pop_working_age_new <- emp_per_pop_working_age_predicted_new * curr_data$emp_per_pop_working_age_dev
    
    #if emp_per_working_age was already above the long-term target, make it converge to the target
    emp_per_pop_working_age_new <- ifelse(prev_data$emp_per_pop_working_age > emp_per_pop_working_age_lt,
                                             emp_per_pop_working_age_lt + 
                                               (1 - emp_per_pop_working_age_conv_rate) *
                                               (prev_data$emp_per_pop_working_age - emp_per_pop_working_age_lt),
                                             emp_per_pop_working_age_new)
    
    # Calculate new employment per working age population
    emp_pr_new <- emp_per_pop_working_age_new * (curr_data$pop_working_age / 1000)
    
    # Calculate employment growth rates
    emp_new_growth <- emp_new / prev_data$emp - 1
    emp_pr_new_growth <- emp_pr_new / prev_data$emp_pr - 1
    
    # Capital stock calculations
    capital_stock_new <- ifelse(is.na(curr_data$capital_stock),
                                prev_data$capital_stock * (1 - prev_data$delta) + prev_data$inv * prev_data$gdp,
                                curr_data$capital_stock)
    
    capital_stock_pr_new <- ifelse(is.na(curr_data$capital_stock_pr),
                                   prev_data$capital_stock_pr * (1 - prev_data$delta) + prev_data$inv * prev_data$gdp_pr,
                                   curr_data$capital_stock_pr)
    
    # Calculate capital stock growth rates
    capital_stock_new_growth <- capital_stock_new / prev_data$capital_stock - 1
    capital_stock_pr_new_growth <- capital_stock_pr_new / prev_data$capital_stock_pr - 1
    
    # Predict TFP growth from the appropriate model
    if (use_demogr_TFP_model) {
      predicted_TFP_nr_growth <- 1 + predict(TFP_demog_model, newdata = data.frame(TFP_level_rel_nr = prev_data$TFP_level_rel_nr, youth_ratio = prev_data$youth_ratio))
      predicted_TFP_nr_pr_growth <- 1 + predict(TFP_demog_model, newdata = data.frame(TFP_level_rel_nr = prev_data$TFP_level_rel_nr_pr, youth_ratio = prev_data$youth_ratio_pr))
    } else {
      predicted_TFP_nr_growth <- 1 + predict(TFP_nr_model_multiyear, newdata = data.frame(TFP_level_rel_nr = prev_data$TFP_level_rel_nr))
      predicted_TFP_nr_pr_growth <- 1 + predict(TFP_nr_model_multiyear, newdata = data.frame(TFP_level_rel_nr = prev_data$TFP_level_rel_nr_pr, youth_ratio = prev_data$youth_ratio))
    }
    
    # Calculate actual TFP growth forecasts
    # Weighted mix of historical/previous year and model forecast
    if (y == (growth_help_table[ iso3c == iso, last_full_year_TFP] + 1)) { # In the first forecast year, use historical TFP growth as "history"
      TFP_nr_growth_new <- weight_of_historical_TFP_growth * growth_help_table[iso3c == iso, get(historical_TFP_growth_deviation_column)] * predicted_TFP_nr_growth +
        (1 - weight_of_historical_TFP_growth) * predicted_TFP_nr_growth
      TFP_nr_pr_growth_new <- weight_of_historical_TFP_growth * growth_help_table[iso3c == iso, get(historical_TFP_growth_deviation_column)] * predicted_TFP_nr_pr_growth +
        (1 - weight_of_historical_TFP_growth) * predicted_TFP_nr_pr_growth
    } else { # In subsequent forecast years, use previous year's TFP growth as history
      TFP_nr_growth_new <- weight_of_historical_TFP_growth * prev_data$TFP_nr_growth + 
        (1 - weight_of_historical_TFP_growth) * predicted_TFP_nr_growth
      TFP_nr_pr_growth_new <- weight_of_historical_TFP_growth * prev_data$TFP_nr_pr_growth + 
        (1 - weight_of_historical_TFP_growth) * predicted_TFP_nr_pr_growth
    }
    
    # GDP calculations
    gdp_nr_growth_new <- (1 + capital_stock_new_growth)^(1 - curr_data$labsh) * 
      (1 + emp_new_growth)^(curr_data$labsh) * 
      TFP_nr_growth_new - 1
    
    gdp_nr_pr_growth_new <- (1 + capital_stock_pr_new_growth)^(1 - curr_data$labsh) * 
      (1 + emp_pr_new_growth)^(curr_data$labsh) * 
      TFP_nr_pr_growth_new - 1
    
    gdp_nr_new <- prev_data$gdp_nr * (1 + gdp_nr_growth_new)
    gdp_nr_pr_new <- prev_data$gdp_nr_pr * (1 + gdp_nr_pr_growth_new)
    
    gdp_new <- gdp_nr_new + curr_data$gdp_rent
    gdp_pr_new <- gdp_nr_pr_new + curr_data$gdp_rent
    
    gdp_growth_new <- gdp_new / prev_data$gdp - 1
    gdp_pr_growth_new <- gdp_pr_new / prev_data$gdp_pr - 1
    
    gdp_per_pop_age_15_64_new <- (gdp_new * 1000) / curr_data$pop_age_15_64
    gdp_per_pop_working_age_new <- (gdp_pr_new * 1000) / curr_data$pop_working_age
    
    gdp_pc_new <- (gdp_new * 1000) / curr_data$pop_total
    gdp_pc_pr_new <- (gdp_pr_new * 1000) / curr_data$pop_total
    
    # Update the data table for the current year and iso
    selected_data[iso3c == iso & year == y, `:=`(
      emp_per_pop_age_15_64 = emp_per_pop_age_15_64_new,
      emp_per_pop_working_age = emp_per_pop_working_age_new,
      emp_per_pop_age_15_64_predicted = emp_per_pop_age_15_64_predicted_new,
      emp_per_pop_working_age_predicted = emp_per_pop_working_age_predicted_new,
      emp = emp_new,
      emp_pr = emp_pr_new,
      capital_stock = capital_stock_new,
      capital_stock_pr = capital_stock_pr_new,
      TFP_nr_growth = TFP_nr_growth_new,
      TFP_nr_pr_growth = TFP_nr_pr_growth_new,
      gdp_growth = gdp_growth_new,
      gdp_pr_growth = gdp_pr_growth_new,
      gdp = gdp_new,
      gdp_pr = gdp_pr_new,
      gdp_nr = gdp_nr_new,
      gdp_nr_pr = gdp_nr_pr_new,
      gdp_per_pop_age_15_64 = gdp_per_pop_age_15_64_new,
      gdp_per_pop_working_age = gdp_per_pop_working_age_new,
      gdp_pc = gdp_pc_new,
      gdp_pc_pr = gdp_pc_pr_new
    )]
  }
  
  # Identify countries where last_full_year_TFP_nr <= y for TFP_level_rel updates
  iso_list_rel <- growth_help_table[last_full_year_TFP_nr <= y, iso3c]
  
  # Loop through each country ISO code for relative TFP level calculations
  for (iso in iso_list_rel) {
    
    # Fetch previous and current year data for the country and USA
    prev_data <- selected_data[iso3c == iso & year == (y - 1)]
    curr_data <- selected_data[iso3c == iso & year == y]
    usa_data <- selected_data[iso3c == "USA" & year == y]
    
    # Skip if any required data is missing
    if (nrow(prev_data) == 0 || nrow(curr_data) == 0 || nrow(usa_data) == 0) next
    
    # Calculate new TFP_level_rel_nr
    TFP_level_rel_nr_new <- prev_data$TFP_level_rel_nr * curr_data$TFP_nr_growth / usa_data$TFP_nr_growth
    
    # Update TFP_level_rel_nr in selected_data
    selected_data[iso3c == iso & year == y, TFP_level_rel_nr := TFP_level_rel_nr_new]
    
    # Calculate new TFP_level_rel_nr_pr
    TFP_level_rel_nr_pr_new <- prev_data$TFP_level_rel_nr_pr * curr_data$TFP_nr_pr_growth / usa_data$TFP_nr_pr_growth
    
    # Update TFP_level_rel_nr_pr in selected_data
    selected_data[iso3c == iso & year == y, TFP_level_rel_nr_pr := TFP_level_rel_nr_pr_new]
  }
}

# Step 5: Create and View Growth Summary for 2050
growth_summary1 <- selected_data[year == 2050, .(
  iso3c,
  country_name,
  gdp,
  gdp_pr,
  gdp_ratio = gdp_pr / gdp,
  gdp_pc,
  gdp_pc_pr,
  emp_per_pop_age_15_64,
  emp_per_pop_working_age
)]

# View the summary table (optional)
# View(growth_summary1)

# Step 6: Recalculate and Update Various Ratios

# Calculate the rent ratio with new data
selected_data[is.na(rent), rent := gdp_rent / gdp]

# Calculate capital-employment ratios
selected_data[, `:=`(
  k_per_emp = capital_stock / emp,
  k_per_emp_pr = capital_stock_pr / emp_pr
)]

# Calculate GDP per employment ratios
selected_data[, `:=`(
  gdp_per_emp = gdp / emp,
  gdp_per_emp_pr = gdp_pr / emp_pr
)]

# Calculate relative capital-employment and GDP per employment ratios (relative to USA)
selected_data[, `:=`(
  k_per_emp_rel = k_per_emp / k_per_emp[iso3c == "USA"],
  k_per_emp_pr_rel = k_per_emp_pr / k_per_emp_pr[iso3c == "USA"],
  gdp_per_emp_rel = gdp_per_emp / gdp_per_emp[iso3c == "USA"],
  gdp_per_emp_pr_rel = gdp_per_emp_pr / gdp_per_emp_pr[iso3c == "USA"]
), by = year]

# Calculate relative GDP per capita ratios (relative to USA)
selected_data[, `:=`(
  gdp_pc_rel = gdp_pc / gdp_pc[iso3c == "USA"],
  gdp_pc_pr_rel = gdp_pc_pr / gdp_pc_pr[iso3c == "USA"]
), by = year]

# Calculate GDP per capita growth rates
selected_data[, `:=`(
  gdp_pc_growth = (gdp_pc / shift(gdp_pc) - 1),
  gdp_pc_pr_growth = (gdp_pc_pr / shift(gdp_pc_pr) - 1)
), by = iso3c]

# Recalculate GDP growth rates with newly available data
selected_data[, `:=`(
  gdp_growth = (gdp / shift(gdp) - 1),
  gdp_pr_growth = (gdp_pr / shift(gdp_pr) - 1)
), by = iso3c]

# Recalculate capital stock growth rates with newly available data
selected_data[, `:=`(
  capital_stock_growth = (capital_stock / shift(capital_stock) - 1),
  capital_stock_pr_growth = (capital_stock_pr / shift(capital_stock_pr) - 1)
), by = iso3c]

# Recalculate employment growth rates with new available data
selected_data[, `:=`(
  emp_growth = (emp / shift(emp) - 1),
  emp_pr_growth = (emp_pr / shift(emp_pr) - 1)
), by = iso3c]

# Calculate the capital-output ratio (now with the missing values filled in)
selected_data[, `:=`(
  k_per_gdp = capital_stock / (gdp_pc * pop_total / 1000),
  k_per_gdp_pr = capital_stock_pr / (gdp_pc_pr * pop_total / 1000)
)]

# Step 7: Save Intermediate Results
write.fst(selected_data, "selected_data_s5.fst")
# To reload later:
# selected_data <- as.data.table(read.fst("selected_data_s5.fst"))

##### EU COUNTRIES AGGREGATION #####

# Step 1: Define EU Countries ISO Codes
EU_isos <- c(
  'AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST', 'FIN',
  'FRA', 'DEU', 'GRC', 'HUN', 'IRL', 'ITA', 'LVA', 'LTU', 'LUX',
  'MLT', 'NLD', 'POL', 'PRT', 'ROU', 'SVK', 'SVN', 'ESP', 'SWE'
)

# Step 2: Define Columns to Sum for EU Aggregation
cols_to_sum <- c(
  "pop_total",
  "pop_age_25_64",
  "pop_age_20_64",
  "pop_age_15_64",
  "pop_working_age",
  "gdp",
  "gdp_pr",
  "capital_stock",
  "capital_stock_pr",
  "emp",
  "emp_pr"
)

# Step 3: Verify cols_to_sum is a character vector
if (!is.character(cols_to_sum)) {
  stop("cols_to_sum should be a character vector of column names.")
}

# Step 4: Ensure all cols_to_sum exist in selected_data
missing_cols <- setdiff(cols_to_sum, names(selected_data))
if (length(missing_cols) > 0) {
  stop(paste("The following columns are missing in selected_data:", paste(missing_cols, collapse = ", ")))
}

# Step 5: Aggregate EU Data by Summing Relevant Columns
EU_data <- selected_data[
  iso3c %in% EU_isos,
  lapply(.SD, sum, na.rm = TRUE),
  by = year,
  .SDcols = cols_to_sum
]

# Step 6: Calculate Additional EU Metrics
EU_data[, `:=`(
  country_name = "European Union",
  iso3c = "EUU",
  gdp_pc = gdp / pop_total * 1000,
  gdp_pc_pr = gdp_pr / pop_total * 1000,
  gdp_growth = (gdp / shift(gdp)) - 1,
  gdp_pr_growth = (gdp_pr / shift(gdp_pr)) - 1
), by = year]

# Step 7: Recalculate GDP per Capita and Growth Rates for EU
EU_data[, `:=`(
  gdp_pc = gdp / pop_total * 1000,
  gdp_pc_pr = gdp_pr / pop_total * 1000,
  gdp_pc_growth = (gdp_pc / shift(gdp_pc) - 1),
  gdp_pc_pr_growth = (gdp_pc_pr / shift(gdp_pc_pr) - 1),
  gdp_growth = (gdp / shift(gdp) - 1),
  gdp_pr_growth = (gdp_pr / shift(gdp_pr) - 1)
), by = year]

# (Optional) View the aggregated EU data
# View(EU_data)

# Step 8: (Optional) Append EU Data to selected_data
# If you want to include EU aggregation within selected_data
# selected_data <- rbind(selected_data, EU_data, fill = TRUE)
# write.fst(selected_data, "selected_data_s5.fst")
# To reload later:
# selected_data <- as.data.table(read.fst("selected_data_s5.fst"))

# Step 9: Save the Final State of EU_data separately
write.fst(EU_data, "EU_data.fst")
# To reload later:
# EU_data <- as.data.table(read.fst("EU_data.fst"))

##### MIGRATION MODEL SCRIPT #####

# Sort the data by iso3c and year
setorder(selected_data, iso3c, year)


# Rename Migration Columns
setnames(selected_data, old = c("NetMigrations",
                                "CNMR",
                                "migr_working_age_pop_15_lifexp_offset_15"),
         new = c("net_migr_UN_2024",
                 "net_migr_rate_UN_2024",
                 "migr_working_age_pop_ratio"),
         skip_absent = TRUE)

# Split Net Migration for Mid-Year Calculations

# For the First Projection Year, Add Only Half Year Migration (Scenario Populations Still Equal at start of Year)
# For Later Years, Average Current and Previous Year Migrations

selected_data[year >= UN_pop_projections_start_year,
              net_migr_UN_2024_splitted := ifelse(year == UN_pop_projections_start_year,
                                                  net_migr_UN_2024 / 2,
                                                  net_migr_UN_2024 / 2 + shift(net_migr_UN_2024) / 2),
              by = iso3c ]

# Estimate Migration Models Using LM

# Relative GDP per Employee Model
migr_model_gdp_per_emp_rel <- lm(net_migr_rate_UN_2024 ~ gdp_per_emp_rel,
                                 data = selected_data[
                                   year >= migr_model_start_year &
                                     year <= migr_model_end_year],
                                 # weights = pop_total # Enable if appropriate
)


# Absolute GDP per Employee Model
migr_model_gdp_per_emp <- lm(net_migr_rate_UN_2024 ~ gdp_per_emp,
                             data = selected_data[
                               year >= migr_model_start_year &
                                 year <= migr_model_end_year
                             ],
                             # weights = pop_total # Enable if appropriate
)

# plot(seq(0,1.2,.01), predict(migr_model_gdp_per_emp_rel, newdata = data.table(gdp_per_emp_rel=seq(0,1.2,.01))), col = "blue")
# USA_gdp_per_worker <-selected_data[iso3c =="USA" & year == current_year, gdp_per_emp]
# points(seq(0,1.2,.01), predict(migr_model_gdp_per_emp, newdata = data.table(gdp_per_emp=seq(0,1.2,.01)*USA_gdp_per_worker)), col = "red")


# Predict Migration Rates
selected_data[, predicted_migration_rate_raw := predict(migr_model_gdp_per_emp, 
                                                        newdata = data.table(gdp_per_emp = gdp_per_emp))]

selected_data[, predicted_migration_rate_raw_rel := predict(migr_model_gdp_per_emp_rel, 
                                                            newdata = data.table(gdp_per_emp_rel = gdp_per_emp_rel))]

# Capping Outliers in Historical Net Migration Rates (likely one-offs)

# Calculating Quantiles by Country
selected_data[ , quant_migr_5_pct := quantile (net_migr_rate_UN_2024, probs = 0.05, na.rm = T), by = iso3c]
selected_data[ , quant_migr_95_pct := quantile (net_migr_rate_UN_2024, probs = 0.95, na.rm = T), by = iso3c]

# Replacing Outlier Migration Rates with Prediction
selected_data [ , net_migr_rate_UN_2024_trimmed :=
                  ifelse ( net_migr_rate_UN_2024 > quant_migr_95_pct |  net_migr_rate_UN_2024 < quant_migr_5_pct ,
                           predicted_migration_rate_raw_rel,
                           net_migr_rate_UN_2024)]

# Removing Unneeded Quantile Columns
selected_data [ , `:=` (quant_migr_5_pct = NULL,
                        quant_migr_95_pct = NULL)]

# Calculate Historical Deviations from Model Predictions
selected_data[year <= (UN_pop_projections_start_year - 1), 
              `:=`(migr_rate_dev = net_migr_rate_UN_2024_trimmed - predicted_migration_rate_raw,
                   migr_rate_dev_rel = net_migr_rate_UN_2024_trimmed - predicted_migration_rate_raw_rel),
              by = iso3c]

# Leaving out Covid year 2020 from averaging
migr_dev_years_to_average <- c(2019, 2021, 2022, 2023)

# Assign Average Deviations for UN Projection Start Year
selected_data[, migr_rate_dev := fifelse(year == UN_pop_projections_start_year,
                                         mean(migr_rate_dev[year %in% migr_dev_years_to_average], na.rm = TRUE),
                                         migr_rate_dev), by = iso3c]

selected_data[, migr_rate_dev_rel := fifelse(year == UN_pop_projections_start_year,
                                             mean(migr_rate_dev_rel[year %in% migr_dev_years_to_average], na.rm = TRUE),
                                             migr_rate_dev_rel), by = iso3c]

# Find the maximum year in the dataset
max_year <- max(selected_data$year, na.rm = TRUE)

# Apply Decay Factor to Migration Rate Deviations
for (y in (UN_pop_projections_start_year + 1):max_year) {
  message("Processing Year: ", y)
  
  tryCatch({
    # Update migr_rate_dev
    selected_data[, migr_rate_dev := fifelse(year == y,
                                             shift(migr_rate_dev, type = "lag") * migr_rate_dev_decay_factor,
                                             migr_rate_dev),
                  by = iso3c]
    
    # Update migr_rate_dev_rel
    selected_data[, migr_rate_dev_rel := fifelse(year == y,
                                                 shift(migr_rate_dev_rel, type = "lag") * migr_rate_dev_decay_factor,
                                                 migr_rate_dev_rel),
                  by = iso3c]
    
    # Handle NA deviations
    selected_data[year == y & is.na(migr_rate_dev), migr_rate_dev := 0, by = iso3c]
    selected_data[year == y & is.na(migr_rate_dev_rel), migr_rate_dev_rel := 0, by = iso3c]
  }, error = function(e) {
    message("Error processing year ", y, ": ", e$message)
    
  })
}

# Set Still Missing Deviations to Zero
selected_data[year >= detailed_demo_data_start_year & is.na(migr_rate_dev), migr_rate_dev := 0]
selected_data[year >= detailed_demo_data_start_year & is.na(migr_rate_dev_rel), migr_rate_dev_rel := 0]

# Modify Future Migration Rate Forecasts with Deviation Factor
# Retrieve start year deviations
start_year_dev <- selected_data[year == UN_pop_projections_start_year, .(iso3c, start_migr_dev = migr_rate_dev)]
start_year_dev_rel <- selected_data[year == UN_pop_projections_start_year, .(iso3c, start_migr_dev_rel = migr_rate_dev_rel)]

# Merge deviations
selected_data <- merge(selected_data, start_year_dev, by = "iso3c", all.x = TRUE)
selected_data <- merge(selected_data, start_year_dev_rel, by = "iso3c", all.x = TRUE)

# Apply deviations
selected_data[, predicted_migration_rate_raw1 := fifelse(year >= UN_pop_projections_start_year,
                                                         predicted_migration_rate_raw + migr_rate_dev,
                                                         predicted_migration_rate_raw + start_migr_dev)]

selected_data[, predicted_migration_rate_raw1_rel := fifelse(year >= UN_pop_projections_start_year,
                                                             predicted_migration_rate_raw_rel + migr_rate_dev_rel,
                                                             predicted_migration_rate_raw_rel + start_migr_dev_rel)]

# Replace Missing Predictions with UN Migration Rates
selected_data[year >= UN_pop_projections_start_year & is.na(predicted_migration_rate_raw1), 
              predicted_migration_rate_raw1 := net_migr_rate_UN_2024]
selected_data[year >= UN_pop_projections_start_year & is.na(predicted_migration_rate_raw1_rel), 
              predicted_migration_rate_raw1_rel := net_migr_rate_UN_2024]

# Initialize the zero and the high migration scenarios (as in the base one in the year before the projections start)
selected_data [ year == UN_pop_projections_start_year  - 1, pop_total_zm := pop_total ]
selected_data [ year == UN_pop_projections_start_year  - 1, pop_total_hm := pop_total ]

# Clean Up Temporary Columns
selected_data[, c("start_migr_dev", "start_migr_dev_rel") := NULL]

# Visualize migration rates and deviations for a selected country

# View(selected_data[year == 2100, .(iso3c, country_name, pop_total, pop_total_hm, pop_total_zm)][order(-pop_total_zm)])
# 
# iso="COD"
# 
# ggplot(data = selected_data [ year %in% 1999:2100 & iso3c == iso,],
#        aes(year, net_migr_rate_UN_2024))+
#   #geom_line()+
#   geom_line(aes(year, predicted_migration_rate_raw_rel), col = "red")+
#   geom_line(aes(year, predicted_migration_rate_raw1_rel), col = "blue")+
#   geom_line(aes(year, migr_rate_dev_rel), col = "purple")+
#   geom_vline(xintercept = 2024, col = "black")

# For the past, total population is the same in the zero and the high migration scenarios as in the base one
selected_data[year == (UN_pop_projections_start_year - 1), 
              `:=`(pop_total_zm = pop_total, pop_total_hm = pop_total)]

# Collect the iso codes for the countries where migration data exists
UN_isos = selected_data [ !is.na(net_migr_UN_2024) & year > UN_pop_projections_start_year, unique(iso3c)]

#####  MIGRATION PROJECTION  #####

# Calculate population difference between medium scenario (UN) and zero migration scenario
selected_data[iso3c %in% UN_isos & year >= UN_pop_projections_start_year,
              pop_diff_ms := pop_total - pop_total_zm,
              by = "iso3c"]

# Calculate cumulative migrations since start of the UN projections 
selected_data[iso3c %in% UN_isos & year >= UN_pop_projections_start_year,
              cum_net_migr_UN_2024_splitted := cumsum(net_migr_UN_2024_splitted),
              by = "iso3c"]

# Calculate residual impact of migration on population implicit in the UN projections
selected_data[iso3c %in% UN_isos & year >= UN_pop_projections_start_year,
              migr_additional_impact_UN := pop_diff_ms - cum_net_migr_UN_2024_splitted,
              by = "iso3c"]

# Calculate the ratio of the "additional impact of migration" to cumulative migration
selected_data[iso3c %in% UN_isos & year >= UN_pop_projections_start_year,
              migr_add_imp_per_cum_migr := fifelse(cum_net_migr_UN_2024_splitted == 0,
                                                   NA_real_,
                                                   migr_additional_impact_UN / cum_net_migr_UN_2024_splitted ),
              by = "iso3c"]

# Collect isos for countries where cumulative migration does not change sign
no_cum_migr_sign_change <- selected_data [year >= UN_pop_projections_start_year & pop_total != pop_total_zm,
                                          .(same_sign = var(sign(cum_net_migr_UN_2024_splitted))==0),by = iso3c][same_sign == TRUE, iso3c]

# Calculate average additional population impact per cumulative migration by year (only in no sign change countries)
avg_migr_add_imp_per_cum_migr <- (selected_data[year >= UN_pop_projections_start_year & iso3c %in% no_cum_migr_sign_change,
                                                .(sum_add_imp=sum(abs(migr_additional_impact_UN), na.rm = T),
                                                  sum_cum_migr = sum( abs( cum_net_migr_UN_2024_splitted), na.rm = T  ),
                                                  add_migr_imp = sum(abs(migr_additional_impact_UN), na.rm = T)/
                                                    sum( abs( cum_net_migr_UN_2024_splitted ), na.rm = T ) ), by = year])

# Create net migration variable, fill with NAs
selected_data[year >= detailed_demo_data_start_year, predicted_net_migr := NA_real_ ]

collected_migr_corr_factor <- data.table(year = UN_pop_projections_start_year:max_year,
                                         mcf = NA_real_ )

# Calculate migration and population numbers in a loop
for (y in UN_pop_projections_start_year:max_year) {
  message("Processing Year: ", y)
  
  # Correction factor calculation
  
  # Calculate absolute migration projections
  pop_total_hm_tmp <- selected_data [iso3c %in% UN_isos & year == y - 1, .(iso3c, pop_total_hm)]
  
  
  migr_comp_tmp <- merge (pop_total_hm_tmp, selected_data[ year == y & iso3c %in% UN_isos, .(iso3c, predicted_migration_rate_raw1_rel)],
                          by = "iso3c", all.y = TRUE)
  
  migr_comp_tmp[, predicted_net_migr_tmp := predicted_migration_rate_raw1_rel * pop_total_hm / 1000 ]
  
  selected_data[iso3c %in% UN_isos & year == y, 
                predicted_net_migr_tmp := migr_comp_tmp$predicted_net_migr_tmp ]
  
  # Calculate correction factor
  sum_out_migr <- selected_data[year == y, sum(predicted_net_migr_tmp[predicted_net_migr_tmp <= 0], na.rm = TRUE)]
  sum_in_migr <- selected_data[year == y, sum(predicted_net_migr_tmp[predicted_net_migr_tmp > 0], na.rm = TRUE)]
  
  # Avoid division by zero
  migr_corr_factor <- ifelse(sum_in_migr == 0, 1, sum_out_migr / -sum_in_migr)
  
  collected_migr_corr_factor [ year == y, mcf:= migr_corr_factor ]
  
  # Correct net migration outflows or inflows depending whether intended outflows or inflows are larger
  if (migr_corr_factor >= 1) {
    selected_data[year == y, predicted_net_migr := fifelse(predicted_net_migr_tmp < 0, 
                                                           predicted_net_migr_tmp / migr_corr_factor, 
                                                           predicted_net_migr_tmp) ]
  } else {
    selected_data[year == y, predicted_net_migr := fifelse(predicted_net_migr_tmp > 0,
                                                           predicted_net_migr_tmp * migr_corr_factor,
                                                           predicted_net_migr_tmp) ]
  }
  
  # Calculate cumulative migration
  selected_data[year >= UN_pop_projections_start_year, 
                cum_predicted_net_migr := cumsum(predicted_net_migr), 
                by = iso3c]
  
  # Calculate high migration population for year y
  for (iso in unique(selected_data$iso3c)) {
    base_population <- selected_data[iso3c == iso & year == y, pop_total_zm]
    cum_migr <- selected_data[iso3c == iso & year == y, cum_predicted_net_migr]
    additional_impact_multiplier <- avg_migr_add_imp_per_cum_migr[ year == y, add_migr_imp ]
    cum_predicted_net_migr <- selected_data[iso3c == iso & year == y, cum_predicted_net_migr ]
    
    new_pop_total_hm <- base_population + cum_migr + (additional_impact_multiplier * cum_predicted_net_migr )
    
    selected_data[iso3c == iso & year == y, pop_total_hm := new_pop_total_hm]
    
    
  }
  
  # Calculate total population difference for the given year
  selected_data[year == y, pop_diff_hm := pop_total_hm - pop_total_zm, by = iso3c]
  
}

# Calculate the migration rate from the predicted migration
selected_data [ , predicted_net_migr_rate := ( predicted_net_migr / pop_total_hm ) * 1000 ]

# Visualize population paths for a selected country

# View(selected_data[year == 2100, .(iso3c, country_name, pop_total, pop_total_hm, pop_total_zm)][order(-pop_total_zm)])

# iso="USA"

# ggplot(selected_data[year >= UN_pop_projections_start_year & iso3c == iso, ]) +
#   geom_line(aes(year, pop_total/1000, color = "Medium UN Scenario")) +
#   geom_line(aes(year, pop_total_hm/1000, color = "High Migration")) +
#   geom_line(aes(year, pop_total_zm/1000, color = "Zero Migration")) +
#   ggtitle(paste0("Population Projections for ", iso)) +
#   labs(x = "Year", y = "Population, million", color = "Scenario") +
#   theme_minimal()

# Save data at this stage
write.fst(selected_data, "selected_data_s6.fst")
# selected_data <- setDT ( read.fst ("selected_data_s6.fst" ) )

# Calculate average ratio of different age groups within
# the difference between the zero migration and high migration scenarios

# Define age groups
age_groups <- c("0_14", "15_24", "15_64", "65_plus")

# Function to calculate average age ratio among "migrants" (difference between medium and zero migration case)
calculate_avg_age_ratio <- function(age_group) {
  ratio_name <- paste0("avg_ratio_", age_group, "_migrants")
  # pop_col <- paste0("pop_age_", gsub("_", "", age_group), ifelse(age_group == "15_64", "", ""), "_migrants")
  # pop_zm_col <- paste0("pop_age_", gsub("_", "", age_group), ifelse(age_group == "15_64", "", ""), "_zm_migrants")
  
  selected_data[, (ratio_name) := 
                  (sum(get(paste0("pop_age_", age_group))[(pop_total - pop_total_zm) < 0], na.rm = TRUE) - 
                     sum(get(paste0("pop_age_", age_group, "_zm"))[(pop_total - pop_total_zm) < 0], na.rm = TRUE)) /
                  (sum(pop_total[(pop_total - pop_total_zm) < 0], na.rm = TRUE) - 
                     sum(pop_total_zm[(pop_total - pop_total_zm) < 0], na.rm = TRUE)),
                by = year]
}

# Apply the function to each age group
lapply(age_groups, calculate_avg_age_ratio)

# Calculate age group numbers in the high migration scenario

# Define age group mappings
age_group_cols <- list(
  "15_64" = "pop_age_15_64_zm",
  "65_plus" = "pop_age_65_plus_zm",
  "0_14" = "pop_age_0_14_zm",
  "15_24" = "pop_age_15_24_zm"
)

# Apply calculations
for (age_group in names(age_group_cols)) {
  hm_col <- paste0("pop_age_", age_group, "_hm")
  zm_col <- age_group_cols[[age_group]]
  ratio_col <- paste0("avg_ratio_", age_group, "_migrants")
  
  selected_data[year >= UN_pop_projections_start_year, (hm_col) := get(zm_col) + get(ratio_col) * (pop_total_hm - pop_total_zm)]
}

# Flexible working age number in the high migration scenario
selected_data[year >= UN_pop_projections_start_year, 
              pop_working_age_hm := pop_working_age_zm + migr_working_age_pop_ratio * (pop_total_hm - pop_total_zm)]


# Calculate "pension age" in the flexible working age scenario
# (both for base and high migration )
selected_data [ , pop_pension_age := pop_total - ( pop_working_age + pop_age_0_14 ) ]
selected_data [ , pop_pension_age_hm := pop_total_hm - ( pop_working_age_hm + pop_age_0_14_hm ) ]

# Check if some of the new population values are below zero, set to zero and warn if yes
negative_pension_age <- selected_data[year >= UN_pop_projections_start_year & pop_pension_age_hm < 0, .N, by = iso3c]
if (nrow(negative_pension_age) > 0) {
  warning("Negative pension age populations detected for the following countries: ", 
          paste(negative_pension_age$iso3c, collapse = ", "))
  selected_data[year >= UN_pop_projections_start_year & pop_pension_age_hm < 0, 
                pop_pension_age_hm := 0]
}

variables_to_check <- c(
  "pop_total_hm",
  "pop_age_0_14_hm",
  "pop_age_15_64_hm",
  "pop_age_65_plus_hm",
  "pop_working_age_hm",
  "pop_pension_age_hm"
)

##### CHECK AND CORRECT NEGATIVE POPULATION VARIABLES #####

# Define the variables to check for negative values
variables_to_check <- c(
  "pop_total_hm",
  "pop_age_0_14_hm",
  "pop_age_15_64_hm",
  "pop_age_65_plus_hm",
  "pop_working_age_hm",
  "pop_pension_age_hm"
)

# Helper function to check and correct negative values
check_and_correct_negatives <- function(var_name) {
  # Identify rows where the variable is negative
  negatives <- selected_data[get(var_name) < 0, .(iso3c, year, value = get(var_name))]
  
  # Count the number of negative occurrences
  n_negatives <- nrow(negatives)
  
  # If any negatives are found, issue a warning and replace them with zero
  if (n_negatives > 0) {
    # Prepare a warning message with the count
    warning_msg <- sprintf(
      "Variable '%s' contains %d negative value(s). These will be set to zero.",
      var_name,
      n_negatives
    )
    
    # Issue the warning
    warning(warning_msg, call. = FALSE)
    
    # Optionally, display the first few negative cases for reference
    example_cases <- negatives[1:min(5, n_negatives)]
    message(sprintf("Examples of negative values in '%s':", var_name))
    print(example_cases)
    
    # Replace negative values with zero
    selected_data[get(var_name) < 0, (var_name) := 0]
  }
}

# Apply the helper function to each variable
lapply(variables_to_check, check_and_correct_negatives)

##### END OF NEGATIVE CHECK #####


selected_data[, total_check := pop_working_age + pop_age_0_14 + pop_pension_age]
threshold <- 1e-3
discrepancies <- selected_data[year >= UN_pop_projections_start_year & abs(pop_total - total_check) > threshold, .N, by = iso3c]
if (nrow(discrepancies) > 0) {
  warning("Population additivity issues detected for the following countries.")
  print(discrepancies)
}

# Remove the check column
selected_data[, total_check := NULL]


# Set high migration scenario equal to base for past years
selected_data[year < UN_pop_projections_start_year, `:=`(
  pop_age_0_14_hm = pop_age_0_14,
  pop_age_15_24_hm = pop_age_15_24,
  pop_age_15_64_hm = pop_age_15_64,
  pop_age_65_plus_hm = pop_age_65_plus,
  pop_total_hm = pop_total,
  pop_working_age_hm = pop_working_age,
  pop_pension_age_hm = pop_pension_age
)]

# Calculate the ratio of age groups among migrants for each country 

selected_data[year >= UN_pop_projections_start_year, `:=`
              (age_0_14_ratio_migrants_UN = fifelse((pop_total - pop_total_zm) != 0,
                                                    (pop_age_0_14 - pop_age_0_14_zm) / (pop_total - pop_total_zm),
                                                    NA_real_),
                age_15_64_ratio_migrants_UN = fifelse((pop_total - pop_total_zm) != 0,
                                                      (pop_age_15_64 - pop_age_15_64_zm) / (pop_total - pop_total_zm),
                                                      NA_real_),
                age_65_plus_ratio_migrants_UN = fifelse((pop_total - pop_total_zm) != 0,
                                                        (pop_age_65_plus - pop_age_65_plus_zm) / (pop_total - pop_total_zm),
                                                        NA_real_))]


# Save data at this stage
write.fst(selected_data, "selected_data_s7.fst")
# selected_data <- setDT ( read.fst ("selected_data_s7.fst" ) )


# Visualize age composition of migrants (UN) and migration ratios
# iso <- "SAU"
# 
# ggplot(selected_data[year >= UN_pop_projections_start_year & iso3c == iso, ]) +
#   geom_line(aes(year, age_15_64_ratio_migrants_UN, color = "15-64 Ratio")) +
#   geom_line(aes(year, age_0_14_ratio_migrants_UN, color = "0-14 Ratio")) +
#   geom_line(aes(year, age_65_plus_ratio_migrants_UN, color = "65+ Ratio")) +
#   labs(title = paste0("Age Composition of 'Migrants' for ", iso),
#        x = "Year",
#        y = "Ratio",
#        color = "Age Group") +
#   theme_minimal()
# 
# ggplot(selected_data[year >= UN_pop_projections_start_year & iso3c == iso, ]) +
#   geom_line(aes(year, predicted_net_migr, color = "Forecast Net Migration")) +
#   geom_line(aes(year, net_migr_UN_2024_splitted, color = "UN Forecast Net Migration")) +
#   geom_line(aes(year, predicted_net_migr_tmp, color = "Initial Forecast of Net Migration")) +
#   labs(title = paste0("Net Migration Projections for ", iso),
#        x = "Year",
#        y = "Net Migration, Thousand People",
#        color = "Metric") +
#   theme_minimal()
# 
# ggplot(selected_data[year >= UN_pop_projections_start_year & iso3c %in% c("USA", "CAN"), ],
#        aes(year, predicted_net_migr, color = iso3c)) +
#   geom_line() +
#   labs(title = "Net Migration Projections for USA and CAN",
#        x = "Year",
#        y = "Net Migration",
#        color = "Country") +
#   theme_minimal()
# 
# ggplot(selected_data[year >= UN_pop_projections_start_year & iso3c %in% c("USA", "CAN", "AUS"), ],
#        aes(year, predicted_net_migr_rate, color = iso3c)) +
#   geom_line() +
#   labs(title = "Net Migration Rate Projections for USA and CAN",
#        x = "Year",
#        y = "Net Migration Rate (People per Thousand Population",
#        color = "Country") +
#   theme_minimal()

#####  CALCULATE GROWTH IN THE HIGH MIGRATION SCENARIO  #####

# First create variables that are the same up to the current year in the high migration scenario as well
# Keep GDP per capita (and not GDP) constant at the previous scenario value up to the current year

selected_data [ year <= current_year, c(
  "gdp_pc_hm",
  "gdp_pc_hm_growth",
  "emp_hm",
  "gdp_pc_pr_hm",
  "gdp_pc_pr_hm_growth",
  "emp_pr_hm") := .(
    gdp_pc,
    gdp_pc_growth,
    emp,
    gdp_pc,
    gdp_pc_growth,
    emp
  )]

# Calculate youth ratios (high migration scenario)
selected_data [ , youth_ratio_hm := pop_age_15_24_hm / ( pop_age_15_64_hm + pop_age_65_plus_hm) ]

# Calculate youth ratios (pension reform, high migration scenario)
# They are the same as in the high migration scenario
selected_data [ , youth_ratio_pr_hm := youth_ratio_hm ]

# Ensure data is sorted by iso3c and year
setorder(selected_data, iso3c, year)

# Calculate GDP and GDP growth for the high migration case up to the current year
selected_data [ year <= current_year, gdp_hm := gdp_pc_hm * pop_total_hm / 1000 ]
selected_data [ year <= current_year, gdp_hm_growth := gdp_hm / shift (gdp_hm ) - 1 , by = iso3c]
selected_data [ year <= current_year, gdp_nr_hm := gdp_hm - gdp_rent ]

selected_data [ year <= current_year, gdp_pr_hm := gdp_pc_pr_hm * pop_total_hm / 1000 ]
selected_data [ year <= current_year, gdp_pr_hm_growth := gdp_pr_hm / shift (gdp_pr_hm ) - 1 , by = iso3c]
selected_data [ year <= current_year, gdp_nr_pr_hm := gdp_pr_hm - gdp_rent ]

# Calculate GDP per working age / 15-64 population up to the current year
selected_data [ year <= current_year , gdp_per_pop_age_15_64_hm := gdp_hm * 1000 / pop_age_15_64_hm ]
selected_data [ year <= current_year , gdp_per_pop_working_age_hm := gdp_pr_hm * 1000 / pop_working_age_hm ]

# Calculate employment per working age / 15-64 population up to the current year
selected_data [ year <= current_year , emp_per_pop_age_15_64_hm := ( emp_hm / pop_age_15_64_hm ) * 1000  ]
selected_data [ year <= current_year , emp_per_pop_working_age_hm := ( emp_pr_hm / pop_working_age_hm ) * 1000  ]

# Calculate employment per working age population DEVIATION up to the current year
# First calculate the prediction

# Calculate predicted employment ratio for age 15-64 population
selected_data [ year <= current_year , emp_per_pop_age_15_64_hm_predicted := 
                  predict( emp_per_pop_age_15_64_model,
                           newdata = selected_data [year <= current_year ,
                                                    .(gdp_per_pop_age_15_64 = gdp_per_pop_age_15_64_hm) ] ) ]

# Calculate predicted employment ratio for working age population
selected_data [ year <= current_year , emp_per_pop_working_age_hm_predicted :=
                  predict( emp_per_pop_working_age_model,
                           newdata = selected_data [year <= current_year ,
                                                    .(gdp_per_pop_working_age = gdp_per_pop_working_age_hm) ] ) ]

# Limit predicted values to the ceilings (which are given as parameters)
selected_data[, emp_per_pop_age_15_64_hm_predicted := fifelse(
  emp_per_pop_age_15_64_hm_predicted > emp_per_pop_age_15_64_lt,
  emp_per_pop_age_15_64_lt,
  emp_per_pop_age_15_64_hm_predicted
)]

selected_data[, emp_per_pop_working_age_hm_predicted := fifelse(
  emp_per_pop_working_age_hm_predicted > emp_per_pop_working_age_lt,
  emp_per_pop_working_age_lt,
  emp_per_pop_working_age_hm_predicted
)]

# Delete potentially existing deviation variables
selected_data [ , `:=` (emp_per_pop_age_15_64_hm_dev = NULL,
                        emp_per_pop_working_age_hm_dev = NULL ) ]

# Calculate deviation ratios
selected_data [ year <= current_year , emp_per_pop_age_15_64_hm_dev :=
                  ( emp_per_pop_age_15_64_hm /
                      emp_per_pop_age_15_64_hm_predicted) ]

selected_data [ year <= current_year , emp_per_pop_working_age_hm_dev :=
                  ( emp_per_pop_working_age_hm /
                      emp_per_pop_working_age_hm_predicted) ]

# Find the maximum year in the dataset
max_year <- max(selected_data$year, na.rm = TRUE)

# Fill in employment deviations using loops where necessary
if (UN_pop_projections_start_year <= current_year) {
  # If the UN projections start earlier or the same year as the current year,
  # run a loop to fill in the deviation values (starting point is different vs the main scenario)
  
  # Extract unique iso3c codes
  unique_iso3c <- unique(selected_data$iso3c)
  
  # Iterate over each iso3c
  for (iso in unique_iso3c) {
    # Print the current iso3c for tracking (optional)
    message("Processing ISO3C: ", iso)
    
    # Subset data for the current iso3c, sorted by year
    iso_data <- selected_data[iso3c == iso][order(year)]
    
    # Iterate over each year from (current_year + 1) to max_year
    for (y in (current_year + 1):max_year) {
      # Current year index
      current_idx <- which(iso_data$year == y)
      
      # Previous year index
      prev_idx <- which(iso_data$year == (y - 1))
      
      # Ensure that both current and previous year exist
      if (length(current_idx) == 1 && length(prev_idx) == 1) {
        
        # Update emp_per_pop_age_15_64_hm_dev
        if (is.na(iso_data$emp_per_pop_age_15_64_hm_dev[current_idx]) &&
            !is.na(iso_data$emp_per_pop_age_15_64_hm_dev[prev_idx])) {
          iso_data$emp_per_pop_age_15_64_hm_dev[current_idx] <- 
            1 + (iso_data$emp_per_pop_age_15_64_hm_dev[prev_idx] - 1) * (1 - emp_per_pop_age_15_64_conv_rate)
        }
        
        # Update emp_per_pop_working_age_hm_dev
        if (is.na(iso_data$emp_per_pop_working_age_hm_dev[current_idx]) &&
            !is.na(iso_data$emp_per_pop_working_age_hm_dev[prev_idx])) {
          iso_data$emp_per_pop_working_age_hm_dev[current_idx] <- 
            1 + (iso_data$emp_per_pop_working_age_hm_dev[prev_idx] - 1) * (1 - emp_per_pop_working_age_conv_rate)
        }
      }
    }
    
    # Assign the updated deviations back to 'selected_data'
    selected_data[iso3c == iso, `:=`(
      emp_per_pop_age_15_64_hm_dev = iso_data$emp_per_pop_age_15_64_hm_dev,
      emp_per_pop_working_age_hm_dev = iso_data$emp_per_pop_working_age_hm_dev
    )]
  }
  
} else {
  
  # If UN projections start later than the current year,
  # set deviations equal to the main scenario from current year forward
  selected_data[year >= current_year, `:=`(
    emp_per_pop_age_15_64_hm_dev = emp_per_pop_age_15_64_dev,
    emp_per_pop_working_age_hm_dev = emp_per_pop_working_age_dev
  )]
}

# Step 9: Make deviations equal to 1 where still missing after loop
selected_data[year >= UN_pop_projections_start_year & is.na(emp_per_pop_age_15_64_hm_dev), emp_per_pop_age_15_64_hm_dev := 1]
selected_data[year >= UN_pop_projections_start_year & is.na(emp_per_pop_working_age_hm_dev), emp_per_pop_working_age_hm_dev := 1]

# Calculate capital stock up to the current year if UN population projection starts earlier than the current year - 1
if (UN_pop_projections_start_year + 1  <= current_year) {
  
  # capital stock is the same as in the main scenario up to the UN population projection start year
  selected_data [ year <= UN_pop_projections_start_year , c(
    "capital_stock_hm",
    "capital_stock_pr_hm") := .(
      capital_stock,
      capital_stock
    )]
  
  # calculate capital stock up to the current year
  invisible ( lapply ( selected_data[, unique(iso3c)] , function (iso) { print ( iso )
    
    lapply (  UN_pop_projections_start_year + 1  : current_year , function ( y ) {
      
      selected_data [iso3c== iso & year == y, capital_stock_hm := ifelse ( is.na ( selected_data [ iso3c== iso & year == y, capital_stock_hm ] ) & !is.na (selected_data[iso3c== iso & year == y - 1, capital_stock_hm ] ),
                                                                           selected_data [ iso3c == iso & year == y - 1, capital_stock_hm ] * ( 1 - selected_data [ iso3c== iso & year == y - 1, delta ] ) +
                                                                             selected_data [ iso3c == iso & year == y - 1 , inv ] * selected_data [ iso3c== iso & year == y - 1, gdp_pc_hm * pop_total_hm / 1000 ],
                                                                           selected_data [ iso3c == iso & year == y, capital_stock_hm ] ) ] } )
    
  }
  ) )
  
  # up until the current year, the capital stock is the same in both the high migration and
  # the "high migration plus pension reform" scenarios
  selected_data [ year <= current_year, capital_stock_pr_hm := capital_stock_hm]
  
  
} else {
  
  #if the main and high migration projections diverge after the current year,
  # capital stock is the same until the current year as in the main scenario
  
  selected_data [ year <= current_year, capital_stock_hm := capital_stock ]
  selected_data [ year <= current_year, capital_stock_pr_hm := capital_stock ]
  
}

# calculate historical yearly TFP growth again from the now available data
selected_data  [ , TFP_nr_hm_growth := ( gdp_nr_hm/shift ( gdp_nr_hm ) ) /(((labsh + shift (labsh) ) / 2 ) * emp_hm / shift ( emp_hm ) + ( 1 - ( labsh + shift (labsh) ) / 2 ) * capital_stock_hm / shift ( capital_stock_hm ) ) , by = iso3c]
selected_data  [ , TFP_nr_pr_hm_growth := ( gdp_nr_pr_hm/shift ( gdp_nr_pr_hm ) ) /(((labsh + shift (labsh) ) / 2 ) * emp_pr_hm / shift ( emp_pr_hm ) + ( 1 - ( labsh + shift (labsh) ) / 2 ) * capital_stock_pr_hm / shift ( capital_stock_pr_hm ) ) , by = iso3c]

#relative TFP levels are the same as in the main scenario up to the UN_pop_projections_start_year - 1
selected_data [ year <= UN_pop_projections_start_year - 1, c("TFP_level_rel_nr_hm",
                                                             "TFP_level_rel_nr_pr_hm"
) := .( TFP_level_rel_nr,
        TFP_level_rel_nr )]

# calculate relative TFP levels for the high migration case up to the current year 
# if UN_pop_projections_start_year <= current_year

if (UN_pop_projections_start_year  <= current_year) {
  
  invisible ( lapply ( UN_pop_projections_start_year : current_year , function (y) {
    print(y)
    
    lapply ( selected_data[, unique(iso3c )], function (iso) {
      TFP_level_rel_nr_hm_new = selected_data [ year == y - 1 & iso3c == iso, TFP_level_rel_nr_hm ] * 
        selected_data [ year == y & iso3c == iso, TFP_nr_hm_growth ] /
        selected_data [ year == y & iso3c == "USA", TFP_nr_hm_growth ]
      selected_data [ year == y & iso3c == iso, TFP_level_rel_nr_hm := TFP_level_rel_nr_hm_new ]
      
    }) 
    
  }))
  
  # TFP nr levels are the same up to the current year in both high migration scenarios
  selected_data [ , TFP_level_rel_nr_pr_hm := TFP_level_rel_nr_hm]
  
} else {
  
  #  if the high migration scenario starts after the current year, just use the main scenario up to the current year
  selected_data [ year <= current_year, TFP_level_rel_nr_hm := TFP_level_rel_nr ]
  selected_data [ year <= current_year, TFP_level_rel_nr_pr_hm := TFP_level_rel_nr ]
  
}

### Calculate growth rate for high migration scenario from current year + 1  onward

# select the appropriate column for historical TFP growth deviation
historical_TFP_growth_deviation_column = ifelse ( use_demogr_TFP_model,
                                                  "historical_TFP_nr_demog_growth_deviation",
                                                  "historical_TFP_nr_growth_deviation")

# growth_help_table <- as.data.table(read.fst("growth_help_table.fst"))

# Save data at this stage
write.fst(selected_data, "selected_data_s8.fst")
# selected_data <- setDT ( read.fst ("selected_data_s8.fst" ) )

##### 2. GROWTH CALCULATION LOOP #####

benchmark_iso <- "USA"

# Iterate over each year from (current_year + 1) to max_year
for (y in (current_year + 1):max_year) {
  
  message("Processing Growth Calculation for Year: ", y)
  
  # Iterate over each ISO3C code
  for (iso in growth_help_table$iso3c) {
    
    # Fetch previous year data for the current ISO3C
    prev_data <- selected_data[iso3c == iso & year == (y - 1)]
    
    # Fetch current year data for the current ISO3C
    curr_data <- selected_data[iso3c == iso & year == y]
    
    # Skip iteration if either previous or current year data is missing
    if (nrow(prev_data) == 0 || nrow(curr_data) == 0) {
      warning("Missing data for ISO3C: ", iso, " Year: ", y, ". Skipping.")
      next
    }
    
    ##### Employment Calculations #####
    
    # Predict employment per population ratio (age 15-64) for the current year
    emp_per_pop_age_15_64_hm_predicted_new = min ( emp_per_pop_age_15_64_lt,
                                                   predict(emp_per_pop_age_15_64_model,
                                                           newdata = prev_data[, .(gdp_per_pop_age_15_64 = gdp_per_pop_age_15_64_hm ) ] ) )
    
    # Calculate new employment per population ratio with deviation
    emp_per_pop_age_15_64_hm_new <- emp_per_pop_age_15_64_hm_predicted_new * curr_data$emp_per_pop_age_15_64_hm_dev
    
    # Adjust emp_per_pop_age_15_64_new to long term targets 
    # if emp_per_pop_age_15_64 just crossed the long term limit, adjust it down to the limit
    emp_per_pop_age_15_64_hm_new <- ifelse(prev_data[, emp_per_pop_age_15_64_hm ] <= emp_per_pop_age_15_64_lt &
                                             emp_per_pop_age_15_64_hm_new > emp_per_pop_age_15_64_lt,
                                           emp_per_pop_age_15_64_lt,
                                           emp_per_pop_age_15_64_hm_new)
    
    # if emp_per_pop_age_15_64 was already above the long-term target, make it converge to the target
    emp_per_pop_age_15_64_hm_new <- ifelse(prev_data[, emp_per_pop_age_15_64_hm ] > emp_per_pop_age_15_64_lt,
                                           emp_per_pop_age_15_64_lt + 
                                             (1 - emp_per_pop_age_15_64_conv_rate) *
                                             (prev_data[, emp_per_pop_age_15_64_hm] - emp_per_pop_age_15_64_lt),
                                           emp_per_pop_age_15_64_hm_new)
    
    # Calculate new employment values
    emp_hm_new <- emp_per_pop_age_15_64_hm_new * (curr_data$pop_age_15_64_hm / 1000)
    
    ##### Flexible Working Age Employment Calculations #####
    
    # Predict employment per working age population ratio for the current year
    emp_per_pop_working_age_hm_predicted_new = min (emp_per_pop_working_age_lt,
                                                    predict(emp_per_pop_working_age_model,
                                                            newdata = prev_data[, .(gdp_per_pop_working_age =
                                                                                      gdp_per_pop_working_age_hm)]) )
    
    # Calculate new employment per working age population ratio with deviation
    emp_per_pop_working_age_hm_new <- emp_per_pop_working_age_hm_predicted_new * 
      curr_data$emp_per_pop_working_age_hm_dev

    # if emp_per_working_age_hm would just cross the long term limit, adjust it down to the limit
    emp_per_pop_working_age_hm_new <- ifelse(prev_data$emp_per_pop_working_age_hm <= emp_per_pop_working_age_lt &
                                               emp_per_pop_working_age_hm_new > emp_per_pop_working_age_lt,
                                             emp_per_pop_working_age_lt,
                                             emp_per_pop_working_age_hm_new)
     
    #if emp_per_working_age_hm was already above the long-term target, make it converge to the target
    emp_per_pop_working_age_hm_new <- ifelse(prev_data$emp_per_pop_working_age_hm > emp_per_pop_working_age_lt,
                                             emp_per_pop_working_age_lt +
                                             (1 - emp_per_pop_working_age_conv_rate) *
                                             (prev_data$emp_per_pop_working_age_hm - emp_per_pop_working_age_lt),
                                           emp_per_pop_working_age_hm_new)

    # Calculate new employment values based on updated ratios
    emp_pr_hm_new <- emp_per_pop_working_age_hm_new * (curr_data$pop_working_age_hm / 1000)
    
    ## Calculate employment growth rates
    emp_hm_new_growth <- emp_hm_new / prev_data$emp_hm
    emp_pr_hm_new_growth <- emp_pr_hm_new / prev_data$emp_pr_hm
    
    ##### Capital Stock Calculations #####
    
    # Calculate new capital stock for high migration scenario
    capital_stock_hm_new <- ifelse(
      is.na(curr_data$capital_stock_hm) & !is.na(prev_data$capital_stock_hm),
      prev_data$capital_stock_hm * (1 - prev_data$delta) + prev_data$inv * prev_data$gdp_hm,
      curr_data$capital_stock_hm
    )
    
    # Calculate new capital stock for high migration plus pension reform scenario
    capital_stock_pr_hm_new <- ifelse(
      is.na(curr_data$capital_stock_pr_hm) & !is.na(prev_data$capital_stock_pr_hm),
      prev_data$capital_stock_pr_hm * (1 - prev_data$delta) + prev_data$inv * prev_data$gdp_pr_hm,
      curr_data$capital_stock_pr_hm
    )
    
    # Calculate capital stock growth rates
    capital_stock_hm_new_growth <- capital_stock_hm_new / prev_data$capital_stock_hm
    capital_stock_pr_hm_new_growth <- capital_stock_pr_hm_new / prev_data$capital_stock_pr_hm
    
    ##### TFP Calculations #####
    
    # Predict TFP growth based on the chosen model
    if (use_demogr_TFP_model) {
      tfp_pred_input_hm <- prev_data[, .(TFP_level_rel_nr = TFP_level_rel_nr_hm,
                                         youth_ratio = youth_ratio_hm)]
      predicted_TFP_nr_hm_growth <- 1 + predict(
        object = TFP_demog_model,
        newdata = tfp_pred_input_hm
      )
      
      tfp_pred_input_pr_hm <- prev_data[, .(TFP_level_rel_nr = TFP_level_rel_nr_pr_hm,
                                            youth_ratio = youth_ratio_pr_hm)]
      predicted_TFP_nr_pr_hm_growth <- 1 + predict(
        object = TFP_demog_model,
        newdata = tfp_pred_input_pr_hm
      )
    } else {
      tfp_pred_input_hm <- prev_data[, .(TFP_level_rel_nr = TFP_level_rel_nr_hm,
                                         youth_ratio = youth_ratio_hm)]
      predicted_TFP_nr_hm_growth <- 1 + predict(
        object = TFP_nr_model_multiyear,
        newdata = tfp_pred_input_hm
      )
      
      tfp_pred_input_pr_hm <- prev_data[, .(TFP_level_rel_nr = TFP_level_rel_nr_pr_hm,
                                            youth_ratio = youth_ratio_pr_hm)]
      predicted_TFP_nr_pr_hm_growth <- 1 + predict(
        object = TFP_nr_model_multiyear,
        newdata = tfp_pred_input_pr_hm
      )
    }
    
    # Calculate new TFP growth with historical deviation
    if (y == (current_year + 1)) {
      TFP_nr_hm_growth_new <- weight_of_historical_TFP_growth * 
        growth_help_table[iso3c == iso, get(historical_TFP_growth_deviation_column)] * 
        predicted_TFP_nr_hm_growth +
        (1 - weight_of_historical_TFP_growth) * 
        predicted_TFP_nr_hm_growth
      
      TFP_nr_pr_hm_growth_new <- weight_of_historical_TFP_growth * 
        growth_help_table[iso3c == iso, get(historical_TFP_growth_deviation_column)] * 
        predicted_TFP_nr_pr_hm_growth +
        (1 - weight_of_historical_TFP_growth) * 
        predicted_TFP_nr_pr_hm_growth
    } else {
      TFP_nr_hm_growth_new <- weight_of_historical_TFP_growth * 
        prev_data$TFP_nr_hm_growth + 
        (1 - weight_of_historical_TFP_growth) * 
        predicted_TFP_nr_hm_growth
      
      TFP_nr_pr_hm_growth_new <- weight_of_historical_TFP_growth * 
        prev_data$TFP_nr_pr_hm_growth + 
        (1 - weight_of_historical_TFP_growth) * 
        predicted_TFP_nr_pr_hm_growth
    }
    
    ##### GDP Calculations #####
    
    # Calculate GDP growth based on capital, labor, and TFP
    gdp_nr_hm_growth_new <- (capital_stock_hm_new_growth)^(1 - curr_data$labsh) *
      (emp_hm_new_growth)^(curr_data$labsh) *
      TFP_nr_hm_growth_new
    
    gdp_nr_pr_hm_growth_new <- (capital_stock_pr_hm_new_growth)^(1 - curr_data$labsh) *
      (emp_pr_hm_new_growth)^(curr_data$labsh) *
      TFP_nr_pr_hm_growth_new
    
    # Update nominal GDP for high migration and high migration plus pension reform scenarios
    gdp_nr_hm_new <- prev_data$gdp_nr_hm * gdp_nr_hm_growth_new
    gdp_nr_pr_hm_new <- prev_data$gdp_nr_pr_hm * gdp_nr_pr_hm_growth_new
    
    gdp_hm_new <- gdp_nr_hm_new + curr_data$gdp_rent
    gdp_pr_hm_new <- gdp_nr_pr_hm_new + curr_data$gdp_rent
    
    # Calculate GDP growth rates
    gdp_hm_growth_new <- (gdp_hm_new / prev_data$gdp_hm) - 1
    gdp_pr_hm_growth_new <- (gdp_pr_hm_new / prev_data$gdp_pr_hm) - 1
    
    # Calculate GDP per capita
    gdp_pc_hm_new <- (gdp_hm_new * 1000) / curr_data$pop_total_hm
    gdp_pc_pr_hm_new <- (gdp_pr_hm_new * 1000) / curr_data$pop_total_hm
    
    # Calculate GDP per working age population
    gdp_per_pop_age_15_64_hm_new = gdp_hm_new * 1000 /curr_data[ , pop_age_15_64_hm ]
    gdp_per_pop_working_age_hm_new = gdp_pr_hm_new * 1000 /curr_data[ , pop_working_age_hm ]
    
    ##### Update 'selected_data' with New Calculations #####
    
    selected_data[iso3c == iso & year == y, `:=` (
      emp_per_pop_age_15_64_hm = emp_per_pop_age_15_64_hm_new,
      emp_per_pop_working_age_hm = emp_per_pop_working_age_hm_new,
      emp_per_pop_age_15_64_hm_predicted = emp_per_pop_age_15_64_hm_predicted_new,
      
      emp_hm = emp_hm_new,
      emp_pr_hm = emp_pr_hm_new,
      
      capital_stock_hm = capital_stock_hm_new,
      capital_stock_pr_hm = capital_stock_pr_hm_new,
      
      TFP_nr_hm_growth = TFP_nr_hm_growth_new,
      TFP_nr_pr_hm_growth = TFP_nr_pr_hm_growth_new,
      
      gdp_nr_hm = gdp_nr_hm_new,
      gdp_nr_pr_hm = gdp_nr_pr_hm_new,
      
      gdp_hm = gdp_hm_new,
      gdp_pr_hm = gdp_pr_hm_new,
      
      gdp_hm_growth = gdp_hm_growth_new,
      gdp_pr_hm_growth = gdp_pr_hm_growth_new,
      
      gdp_pc_hm = gdp_pc_hm_new,
      gdp_pc_pr_hm = gdp_pc_pr_hm_new,
      
      gdp_per_pop_age_15_64_hm = gdp_per_pop_age_15_64_hm_new,
      gdp_per_pop_working_age_hm = gdp_per_pop_working_age_hm_new,
      
      predicted_TFP_nr_pr_hm_growth = predicted_TFP_nr_pr_hm_growth
      
    )]
    
  } # End of first ISO3C loop
  
  ##### Second ISO3C Loop: Relative TFP Level Calculations #####
  for (iso in growth_help_table[last_full_year_TFP_nr <= y, iso3c]) {
    
    # collect previous and current data once
    
    prev_data = selected_data[year == y - 1 & iso3c == iso ]
    curr_data = selected_data[year == y & iso3c == iso ]
    USA_data  = selected_data[year == y & iso3c == "USA" ]
    
    # Update TFP levels
    # update the 15-64 working age version
    
    TFP_level_rel_nr_hm_new = prev_data [ , TFP_level_rel_nr_hm ] *
      curr_data [ , TFP_nr_hm_growth ] / USA_data [ , TFP_nr_hm_growth ]
    
    selected_data [ year == y & iso3c == iso, TFP_level_rel_nr_hm := TFP_level_rel_nr_hm_new ]
    
    # update the flexible working age/pension reform (pr) version
    
    TFP_level_rel_nr_pr_hm_new = prev_data [ , TFP_level_rel_nr_pr_hm ] *
      curr_data [ , TFP_nr_pr_hm_growth ] / USA_data [ , TFP_nr_pr_hm_growth ]
    
    selected_data [ year == y & iso3c == iso, TFP_level_rel_nr_pr_hm := TFP_level_rel_nr_pr_hm_new ]
    
  } # End of Second ISO3C Loop
  
} # End of Year loop


#####  FILLING MISSING VALUES AND CALCULATING RATIOS #####

# Define the benchmark ISO code(s) for relative calculations
benchmark_iso <- "USA" # Can be parameterized as needed

# Step 1: Fill in missing GDP and population values with baseline counterparts
selected_data[, `:=`(
  gdp_hm          = fifelse(is.na(gdp_hm), gdp, gdp_hm),
  gdp_pc_hm       = fifelse(is.na(gdp_pc_hm), gdp_pc, gdp_pc_hm),
  pop_age_15_64_hm= fifelse(is.na(pop_age_15_64_hm), pop_age_15_64, pop_age_15_64_hm),
  pop_total_hm    = fifelse(is.na(pop_total_hm), pop_total, pop_total_hm),
  gdp_pr_hm       = fifelse(is.na(gdp_pr_hm), gdp, gdp_pr_hm),
  gdp_pc_pr_hm    = fifelse(is.na(gdp_pc_pr_hm), gdp_pc, gdp_pc_pr_hm)
)]

# Step 2: Calculate capital-employment and GDP-employment ratios
selected_data[, `:=`(
  k_per_emp_hm         = fifelse(emp_hm != 0, capital_stock_hm / emp_hm, NA_real_),
  k_per_emp_pr_hm      = fifelse(emp_pr_hm != 0, capital_stock_pr_hm / emp_pr_hm, NA_real_),
  gdp_per_emp_hm       = fifelse(emp_hm != 0, gdp_hm / emp_hm, NA_real_),
  gdp_per_emp_pr_hm    = fifelse(emp_pr_hm != 0, gdp_pr_hm / emp_pr_hm, NA_real_)
)]

# Step 3: Calculate relative capital-employment and GDP-employment ratios compared to benchmark
selected_data[, `:=`(
  k_per_emp_rel_hm      = fifelse(iso3c != benchmark_iso & !is.na(k_per_emp_hm[iso3c == benchmark_iso]),
                                  k_per_emp_hm / k_per_emp_hm[iso3c == benchmark_iso],
                                  NA_real_),
  k_per_emp_rel_pr_hm   = fifelse(iso3c != benchmark_iso & !is.na(k_per_emp_pr_hm[iso3c == benchmark_iso]),
                                  k_per_emp_pr_hm / k_per_emp_pr_hm[iso3c == benchmark_iso],
                                  NA_real_),
  gdp_per_emp_rel_hm    = fifelse(iso3c != benchmark_iso & !is.na(gdp_per_emp_hm[iso3c == benchmark_iso]),
                                  gdp_per_emp_hm / gdp_per_emp_hm[iso3c == benchmark_iso],
                                  NA_real_),
  gdp_per_emp_rel_pr_hm = fifelse(iso3c != benchmark_iso & !is.na(gdp_per_emp_pr_hm[iso3c == benchmark_iso]),
                                  gdp_per_emp_pr_hm / gdp_per_emp_pr_hm[iso3c == benchmark_iso],
                                  NA_real_)
), by = year]

# Step 4: Calculate relative GDP per capita
selected_data[, `:=`(
  gdp_pc_rel_hm      = fifelse(iso3c != benchmark_iso & !is.na(gdp_pc_hm[iso3c == benchmark_iso]),
                               gdp_pc_hm / gdp_pc_hm[iso3c == benchmark_iso],
                               NA_real_),
  gdp_pc_rel_pr_hm   = fifelse(iso3c != benchmark_iso & !is.na(gdp_pc_pr_hm[iso3c == benchmark_iso]),
                               gdp_pc_pr_hm / gdp_pc_pr_hm[iso3c == benchmark_iso],
                               NA_real_)
), by = year]

# Step 5: Calculate GDP per capita and GDP growth rates
selected_data[, `:=`(
  gdp_pc_hm_growth    = (gdp_pc_hm / shift(gdp_pc_hm)) - 1,
  gdp_pc_pr_hm_growth = (gdp_pc_pr_hm / shift(gdp_pc_pr_hm)) - 1,
  gdp_hm_growth       = (gdp_hm / shift(gdp_hm)) - 1,
  gdp_pr_hm_growth    = (gdp_pr_hm / shift(gdp_pr_hm)) - 1,
  capital_stock_hm_growth  = (capital_stock_hm / shift(capital_stock_hm)) - 1,
  capital_stock_pr_hm_growth = (capital_stock_pr_hm / shift(capital_stock_pr_hm)) - 1,
  emp_hm_growth       = (emp_hm / shift(emp_hm)) - 1,
  emp_pr_hm_growth    = (emp_pr_hm / shift(emp_pr_hm)) - 1
), by = iso3c]

# Step 6: Calculate capital to GDP ratios
selected_data[, `:=`(
  k_per_gdp_hm      = fifelse((gdp_pc_hm * pop_total_hm / 1000) != 0, capital_stock_hm / (gdp_pc_hm * pop_total_hm / 1000), NA_real_),
  k_per_gdp_pr_hm   = fifelse((gdp_pc_pr_hm * pop_total_hm / 1000) != 0, capital_stock_pr_hm / (gdp_pc_pr_hm * pop_total_hm / 1000), NA_real_)
)]

# Step 7: Calculate lagged GDP per capita growth rate over six years
selected_data[, gdp_pc_growth_6y_1y_lagged := (shift(gdp_pc, n = 1) / shift(gdp_pc, n = 6))^(1/5) - 1, by = iso3c]

#####  EU AGGREGATION, HIGH MIGRATION CASE  #####

# aggregate some variables for EU total from individual countries

EU_isos = c('AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST', 'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 'IRL', 'ITA', 'LVA', 'LTU', 'LUX', 'MLT', 'NLD', 'POL', 'PRT', 'ROU', 'SVK', 'SVN', 'ESP', 'SWE')

cols_to_sum <- c( "pop_total",
                  "pop_total_hm",
                  "pop_total_zm",
                  "pop_age_0_14",
                  "pop_age_0_14_hm",
                  "pop_age_25_64",
                  "pop_age_20_64",
                  "pop_age_15_64",
                  "pop_working_age",
                  "pop_working_age_hm",
                  "capital_stock",
                  "emp",
                  "emp_pr",
                  "emp_hm",
                  "emp_pr_hm",
                  "pop_age_15_64_hm",
                  "pop_age_15_64_zm",
                  "gdp",
                  "gdp_pr",
                  "gdp_hm",
                  "gdp_pr_hm",
                  "capital_stock_hm")

# Sum the selected columns for EU countries
EU_data <- selected_data [ iso3c %in% EU_isos & year >= 1950 ][, lapply (.SD, sum) ,by=year, .SDcols = cols_to_sum ]

# Calculate additional columns
EU_data[ ,country_name := "European Union"]
EU_data[ ,iso3c := "EUU"]
EU_data[ is.na (pop_total_zm) , pop_total_zm := pop_total ]
EU_data[ is.na (pop_age_15_64_zm) , pop_age_15_64_zm := pop_age_15_64 ]
EU_data[ , gdp_pc := gdp / pop_total *1000]
EU_data[ , gdp_pc_growth := gdp_pc / shift (gdp_pc ) - 1 ]
EU_data[ ,gdp_growth := gdp / shift (gdp ) - 1 ]
EU_data[ ,gdp_pr_growth := gdp_pr / shift (gdp_pr ) - 1 ]
EU_data[ ,gdp_hm_growth := gdp_hm / shift (gdp_hm ) - 1 ]
EU_data[ ,gdp_pr_hm_growth := gdp_pr_hm / shift (gdp_pr_hm ) - 1 ]
EU_data[ , gdp_pc_hm := gdp_hm / pop_total_hm *1000]
EU_data[ , gdp_pc_hm_growth := gdp_pc_hm / shift (gdp_pc_hm ) - 1 ]
EU_data[ ,gdp_hm_growth := gdp_hm / shift (gdp_hm ) - 1 ]
EU_data[ , gdp_pc_pr := gdp_pr / pop_total *1000]
EU_data[ , gdp_pc_pr_growth := gdp_pc_pr / shift (gdp_pc_pr ) - 1 ]
EU_data[ ,gdp_pr_growth := gdp_pr / shift (gdp_pr ) - 1 ]

##### WRITING DATA  #####
write.csv(selected_data, paste0 ("selected_data_with_fcasts", ifelse(use_demogr_TFP_model, "_demogr_TFP", ""), ".csv" ) )
write.fst(selected_data, paste0 ("selected_data_with_fcasts", ifelse(use_demogr_TFP_model, "_demogr_TFP", ""), ".fst" ) )
#selected_data <- setDT ( read.csv ("selected_data_with_fcasts.csv" ) )
#selected_data <- setDT ( read.fst ("selected_data_with_fcasts.fst" ) )
#selected_data <- setDT ( read.csv ("selected_data_with_fcasts_demogr_TFP.csv" ) )
#selected_data <- setDT ( read.fst ("selected_data_with_fcasts_demogr_TFP.fst" ) )

# write a narrower dataset
write.csv ( selected_data [ year >= 1950 & !is.na ( country_name),.( year,
                                                                     iso3c,
                                                                     country_name,
                                                                     gdp,
                                                                     gdp_hm,
                                                                     gdp_pr,
                                                                     gdp_pr_hm,
                                                                     gdp_growth,
                                                                     gdp_hm_growth,
                                                                     gdp_pr_growth,
                                                                     gdp_pr_hm_growth,
                                                                     gdp_pc,
                                                                     gdp_pc_hm,
                                                                     gdp_pc_pr,
                                                                     gdp_pc_pr_hm,
                                                                     gdp_pc_growth,
                                                                     gdp_pc_hm_growth,
                                                                     gdp_pc_pr_growth,
                                                                     gdp_pc_pr_hm_growth,
                                                                     emp,
                                                                     emp_hm,
                                                                     emp_pr,
                                                                     emp_pr_hm,
                                                                     capital_stock,
                                                                     capital_stock_hm,
                                                                     capital_stock_pr,
                                                                     capital_stock_pr_hm,
                                                                     TFP_level_rel_nr,
                                                                     TFP_level_rel_nr_hm,
                                                                     TFP_level_rel_nr_pr,
                                                                     TFP_level_rel_nr_pr_hm,
                                                                     TFP_nr_growth,
                                                                     TFP_nr_hm_growth,
                                                                     TFP_nr_pr_growth,
                                                                     TFP_nr_pr_hm_growth,
                                                                     pop_total,
                                                                     pop_total_hm,
                                                                     pop_age_15_64,
                                                                     pop_age_15_64_hm,
                                                                     pop_working_age,
                                                                     pop_working_age_hm)],
            "narrow_data.csv" )

write.csv(EU_data, "EU_data.csv")
write.fst(EU_data, "EU_data.fst")
# EU_data <- setDT ( read.csv ( "EU_data.csv" ) )
# EU_data <- setDT ( read.fst ( "EU_data.fst" ) )