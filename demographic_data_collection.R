###############################################
# Demographic Data Collection and Processing  #
# Sources: United Nations and Gapminder       #
###############################################

# -------------------------------
# 1. Load Required Packages
# -------------------------------

# List of required packages
required_packages <- c(
  "data.table",    # Efficient data manipulation
  "countrycode",   # Mapping country codes
  "pwt10",         # Accessing Penn World Tables data
  "WDI",           # Accessing World Development Indicators
  "ggplot2",       # Data visualization
  "fst",           # Fast serialization of data.tables
  "openxlsx",      # Reading Excel files
  "gsheet"         # Reading Google Sheets
)

# Function to install and load required packages
install_and_load <- function(packages) {
  # Install missing packages
  new_packages <- setdiff(packages, rownames(installed.packages()))
  if(length(new_packages) > 0){
    install.packages(new_packages, dependencies = TRUE)
  }
  # Load all packages
  lapply(packages, library, character.only = TRUE)
}

# Install and load the packages
install_and_load(required_packages)

# -------------------------------
# 2. Define Helper Functions
# -------------------------------

#' Download and Read Gzipped CSV Files
#'
#' Downloads a gzipped CSV file from the specified URL and reads it into a data.table.
#'
#' @param url The URL of the gzipped CSV file to download.
#'
#' @return A data.table containing the contents of the CSV file, or NULL if an error occurs.
#'
#' @examples
#' data <- download_and_read_csv_gz("https://example.com/data.csv.gz")

# Function to download and read gzipped CSV files efficiently
download_and_read_csv_gz <- function(url) {
  temp <- tempfile(fileext = ".csv.gz")
  tryCatch({
    download.file(url, temp, mode = "wb", quiet = TRUE)
    data <- fread(temp, showProgress = FALSE)
    unlink(temp)
    return(data)
  }, error = function(e) {
    message("Error downloading or reading the file: ", url)
    unlink(temp)
    return(NULL)
  })
}


# Function to aggregate population across specified age groups
aggregate_age_groups <- function(dt, prefix = "age_", start_age, end_age, new_var) {
  age_cols <- paste0(prefix, start_age:end_age)
  dt[, (new_var) := rowSums(.SD, na.rm = TRUE), .SDcols = age_cols]
}

# Function to calculate ratios
calculate_ratio <- function(dt, numerator, denominator, new_var) {
  dt[, (new_var) := get(numerator) / get(denominator)]
}

# -------------------------------
# 3. Download and Combine UN Demographic Data
# -------------------------------

# URLs for historical and projected UN population data
url_hist <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_PopulationBySingleAgeSex_Medium_1950-2023.csv.gz"
url_proj <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_PopulationBySingleAgeSex_Medium_2024-2100.csv.gz"

# Download and read the data
dem_hist_2024_data <- download_and_read_csv_gz(url_hist)
dem_proj_2024_data <- download_and_read_csv_gz(url_proj)

# Combine historical and projected data
dem_data_age <- rbindlist(
  list(dem_hist_2024_data, dem_proj_2024_data),
  use.names = TRUE,
  fill = TRUE
)

# Clean up temporary variables
rm(dem_hist_2024_data, dem_proj_2024_data)
gc()

# Rename 'Time' column to 'year'
setnames(dem_data_age, 'Time', 'year')

# -------------------------------
# 4. Reshape Data to Wide Format
# -------------------------------

# Modify AgeGrp for variable naming
dem_data_age[, AgeGrp := paste0("age_", AgeGrp)]

# Reshape data: LocID + Location + year ~ AgeGrp
demographic_data <- dcast(
  dem_data_age[, .(LocID, Location, year, AgeGrp, PopTotal)],
  LocID + Location + year ~ AgeGrp,
  value.var = 'PopTotal',
  fill = 0
)

# Clean up
rm(dem_data_age)
gc()

# -------------------------------
# 5. Calculate Total Population
# -------------------------------

# Define age columns
age_cols <- c(paste0('age_', 0:99), 'age_100+')

# Calculate total population
demographic_data[, pop_total := rowSums(.SD, na.rm = TRUE), .SDcols = age_cols]

# -------------------------------
# 6. Aggregate Various Age Groups
# -------------------------------

# Define age aggregations
age_aggregations <- list(
  c(0, 14),
  c(15, 24),
  c(25, 64),
  c(15, 64),
  c(20, 64)
)

# Apply age aggregations
for (agg in age_aggregations) {
  start_age <- agg[1]
  end_age <- agg[2]
  new_var <- paste0('pop_age_', start_age, '_', end_age)
  aggregate_age_groups(demographic_data, prefix = "age_", start_age, end_age, new_var)
}

# Additional aggregations: 0-4, 5-9, ..., 75-79
for (age in seq(0, 75, 5)) {
  start_age <- age
  end_age <- age + 4
  new_var <- paste0('pop_age_', start_age, '_', end_age)
  aggregate_age_groups(demographic_data, prefix = "age_", start_age, end_age, new_var)
}

# -------------------------------
# 7. Calculate Population Aged 65+ and 80+
# -------------------------------

# Calculate 65+ population
demographic_data[, pop_age_65_plus := rowSums(.SD, na.rm = TRUE), .SDcols = c( paste0('age_', 65:99), "age_100+")]

# Calculate 80+ population
demographic_data[, pop_age_80_plus := rowSums(.SD, na.rm = TRUE), .SDcols = c( paste0('age_', 80:99), "age_100+" )]

# -------------------------------
# 8. Calculate Age Group Ratios to Total Population
# -------------------------------

# Define population age groups for ratio calculation
age_ratio_vars <- c(
  'pop_age_0_14', 'pop_age_15_24', 'pop_age_25_64', 
  'pop_age_65_plus', 'pop_age_15_64', 'pop_age_20_64'
)

# Calculate ratios
for (var in age_ratio_vars) {
  new_var <- paste0(var, "_ratio")
  calculate_ratio(demographic_data, var, "pop_total", new_var)
}

# -------------------------------
# 9. Calculate Youth Ratio
# -------------------------------

demographic_data[, youth_ratio := pop_age_15_24 / (pop_age_15_24 + pop_age_25_64 + pop_age_65_plus)]

# -------------------------------
# 10. Calculate Growth Rates
# -------------------------------

# Order data by Location and year for accurate growth rate calculations
setorder(demographic_data, Location, year)

# Define variables for growth calculations
growth_vars <- c('pop_total', 'pop_age_0_14', 'pop_age_15_24', 
                 'pop_age_25_64', 'pop_age_65_plus', 'pop_age_15_64', 'pop_age_20_64')

# Calculate growth rates: (current / previous) - 1
for (var in growth_vars) {
  demographic_data[, paste0(var, "_grth") := (get(var) / shift(get(var)) - 1), by = Location]
}

# Define variables for growth contributions
other_growth_vars <- c(
  'pop_age_0_4', 'pop_age_5_9', 'pop_age_10_14',
  'pop_age_15_19', 'pop_age_20_24', 'pop_age_25_29',
  'pop_age_30_34', 'pop_age_35_39', 'pop_age_40_44',
  'pop_age_45_49', 'pop_age_50_54', 'pop_age_55_59',
  'pop_age_60_64', 'pop_age_65_69', 'pop_age_70_74',
  'pop_age_75_79', 'pop_age_80_plus'
)

# Calculate growth contributions: (current - previous) / previous pop_total
for (var in other_growth_vars) {
  demographic_data[, paste0(var, "_grth_contr") := (get(var) - shift(get(var))) / shift(pop_total), by = Location]
}

# -------------------------------
# 11. Generate ISO3 Country Codes
# -------------------------------

# Convert LocID to ISO3 country codes
demographic_data[, iso3c := countrycode(LocID, "un", "iso3c")]

# Fix country codes for Taiwan and Kosovo
demographic_data[LocID == 158, iso3c := 'TWN']  # Taiwan
demographic_data[LocID == 412, iso3c := 'XKX']  # Kosovo

# Remove rows without ISO3 codes (aggregates)
demographic_data <- demographic_data[!is.na(iso3c)]

# -------------------------------
# 12. Download Additional Demographic Indicators from UN
# -------------------------------

# URL for UN demographic indicators Excel file
url_dem_indicators <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/EXCEL_FILES/1_General/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx"

# Read 'Estimates' and 'Medium variant' sheets starting from row 17
dem_data_est <- read.xlsx(url_dem_indicators, sheet = "Estimates", startRow = 17)
dem_data_mv <- read.xlsx(url_dem_indicators, sheet = "Medium variant", startRow = 17)

# Combine estimates and medium variant data
dem_data_other <- rbindlist(list(dem_data_est, dem_data_mv), use.names = TRUE, fill = TRUE)

# Clean up temporary variables
rm(dem_data_est, dem_data_mv)
gc()

# Rename relevant columns
setnames(
  dem_data_other, 
  old = c("Year", 
          "Net.Number.of.Migrants.(thousands)", 
          "Net.Migration.Rate.(per.1,000.population)"),
  new = c("year", "net_migr_UN_2024", "net_migr_rate_UN_2024"),
  skip_absent = TRUE
)

# Convert specified columns to numeric
numeric_cols <- names(dem_data_other)[10:65]  # Adjust indices based on actual data
dem_data_other[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]

# Generate ISO3 country codes
dem_data_other[, iso3c := countrycode(Location.code, "un", "iso3c")]

# Fix country codes for Taiwan and Kosovo
dem_data_other[Location.code == 158, iso3c := 'TWN']  # Taiwan
dem_data_other[Location.code == 412, iso3c := 'XKX']  # Kosovo

# Remove rows without ISO3 codes (aggregates)
dem_data_other <- dem_data_other[!is.na(iso3c)]

# Rename demographic indicator columns for clarity
setnames(
  dem_data_other, 
  old = c("Life.Expectancy.at.Birth,.both.sexes.(years)",
          "Male.Life.Expectancy.at.Birth.(years)",
          "Female.Life.Expectancy.at.Birth.(years)",
          "Life.Expectancy.at.Age.65,.both.sexes.(years)",
          "Under-Five.Mortality.(deaths.under.age.5.per.1,000.live.births)",
          "Total.Fertility.Rate.(live.births.per.woman)"),
  new = c(
    'lifexp_total',
    'lifexp_male',
    'lifexp_female',
    'lifexp_total_at_65',
    'child_mort_under_5',
    'fertility_rate_total'
  )
)

# -------------------------------
# 13. Merge Additional Indicators with Main Demographic Data
# -------------------------------

demographic_data <- merge(
  demographic_data,
  dem_data_other[, !c('iso2c', "Index", "Variant", "Location", "Notes", "LocID"), with = FALSE],
  by = c('iso3c', 'year'),
  all = TRUE
)

# Clean up
rm(dem_data_other)
gc()

# Rearrange columns: move age-related columns to the end for better readability
demographic_data <- demographic_data[, c(setdiff(names(demographic_data), age_cols), age_cols), with = FALSE]

# -------------------------------
# 14. Download and Merge Gapminder Data
# -------------------------------

# URLs for Gapminder child mortality and fertility rate data via Google Sheets
url_gapminder_u5mr <- "https://docs.google.com/spreadsheets/d/1Av7eps_zEK73-AdbFYEmtTrwFKlfruBYXdrnXAOFVpM/edit?gid=176703676#gid=176703676"
url_gapminder_tfr  <- "https://docs.google.com/spreadsheets/d/1j6zkY_a1vX7RtIGKsC9V6AXyfUgtYZl52_SCezwESZM/edit?gid=176703676#gid=176703676"

# Read Gapminder Under-5 Mortality Rate data
gapminder_u5mr <- gsheet2tbl(url_gapminder_u5mr)
setDT(gapminder_u5mr)
setnames(gapminder_u5mr, old = c("time", "Child mortality"), new = c("year", "child_mort_under_5_gm"))

# Read Gapminder Total Fertility Rate data
gapminder_tfr <- gsheet2tbl(url_gapminder_tfr)
setDT(gapminder_tfr)
setnames(gapminder_tfr, old = c("time", "Babies per woman"), new = c("year", "fertility_rate_total_gm"))

# Merge Gapminder child mortality and fertility rate data
gapminder_data <- merge(
  gapminder_tfr, 
  gapminder_u5mr, 
  by = c('geo', 'year'), 
  all = TRUE
)

# Clean Gapminder data
gapminder_data[, year := as.numeric(as.character(year))]
gapminder_data[, geo := toupper(geo)]
gapminder_data[, country_name := countrycode(geo, "iso3c", "country.name")]
setnames(gapminder_data, "geo", "iso3c")

# Fix country code for Vatican City
gapminder_data[iso3c == "HOS", iso3c := "VAT"]

# Merge Gapminder data with main demographic data
demographic_data <- merge(
  gapminder_data[!is.na(country_name), .(iso3c, year, child_mort_under_5_gm, fertility_rate_total_gm)],
  demographic_data,
  by = c('iso3c', 'year'),
  all = TRUE
)

# Clean up
rm(gapminder_data, gapminder_tfr, gapminder_u5mr)
gc()

# -------------------------------
# 15. Calculate Working Age Population Based on Flexible Upper Age Limit
# -------------------------------

# Function to calculate working age population with flexible upper age limit
calculate_working_age_pop <- function(dt, start_age, offset) {
  # Define the new variable name
  new_var <- paste0('working_age_pop_', start_age, '_lifexp_offset_', offset)
  
  # Calculate end_age and fraction for each row
  dt[, `:=`(
    end_age = floor(lifexp_total_at_65 + 65 - offset),
    fraction = (lifexp_total_at_65 + 65 - offset) - floor(lifexp_total_at_65 + 65 - offset)
  )]
  
  # Initialize the new working age population column with NA_real_
  dt[, (new_var) := NA_real_]
  
  # Identify rows where 'lifexp_total_at_65' is not NA
  valid_rows <- which(!is.na(dt$lifexp_total_at_65))
  
  if (length(valid_rows) == 0) {
    # No valid rows to process
    dt[, c("end_age", "fraction") := NULL]
    return(NULL)
  }
  
  # Define age columns
  age_cols <- paste0('age_', 0:99)  # 'age_100' does not exist, only 'age_100+'
  available_age_cols <- intersect(age_cols, names(dt))
  
  if (length(available_age_cols) == 0) {
    stop("No age columns found in the data.")
  }
  
  # Extract age data for valid rows into a matrix for faster access
  age_matrix <- as.matrix(dt[valid_rows, ..available_age_cols])
  
  # Extract relevant 'end_age' and 'fraction' values for valid rows
  end_ages <- dt$end_age[valid_rows]
  fractions <- dt$fraction[valid_rows]
  
  # Helper function to calculate working age population for a single row
  calc_working_age <- function(row_idx, e_age, frac) {
    # Ensure end_age is within valid bounds
    e_age <- max(e_age, start_age - 1)  # Prevent negative or below start_age
    
    if (e_age < start_age) {
      # If end_age is below start_age, working_age_pop remains 0
      return(0)
    }
    
    # Define age indices (R is 1-based)
    age_start_idx <- start_age + 1
    age_end_idx <- e_age + 1
    
    # Ensure age_end_idx does not exceed the number of columns
    age_end_idx <- min(age_end_idx, ncol(age_matrix))
    
    # Sum populations from start_age to end_age
    sum_pop <- sum(age_matrix[row_idx, age_start_idx:age_end_idx], na.rm = TRUE)
    
    # Add fractional population from the next age group if it exists
    next_age_col <- paste0('age_', e_age + 1)
    if ((e_age + 1) <= 99 && next_age_col %in% available_age_cols) {
      sum_pop <- sum_pop + frac * age_matrix[row_idx, paste0('age_', e_age + 1)]
    }
    
    return(sum_pop)
  }
  
  # Apply the helper function to each valid row using mapply
  working_age_pop_values <- mapply(
    FUN = calc_working_age,
    row_idx = seq_along(valid_rows),
    e_age = end_ages,
    frac = fractions,
    SIMPLIFY = TRUE
  )
  
  # Assign the computed working age population to the new column
  dt[valid_rows, (new_var) := working_age_pop_values]
  
  # Remove temporary columns
  dt[, c("end_age", "fraction") := NULL]
}

# Apply the function to calculate working age population for age 15 with offset 15
calculate_working_age_pop(demographic_data, 15, 15)

# -------------------------------
# 14. Download and Process Zero Migration Data
# -------------------------------

# URL for Zero Migration variant UN population data
url_zero_migration <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_PopulationBySingleAgeSex_Zero%20migration_2024-2100.csv.gz"

# Download and read Zero Migration data
dem_proj_2024_data_zero_migration <- download_and_read_csv_gz(url_zero_migration)

# Rename 'Time' column to 'year'
setnames(dem_proj_2024_data_zero_migration, 'Time', 'year')

# Modify AgeGrp for variable naming
dem_proj_2024_data_zero_migration[, AgeGrp := paste0("zm_age_", AgeGrp)]

# Reshape data: LocID + Location + year ~ AgeGrp
dem_proj_2024_data_zero_migration <- dcast(
  dem_proj_2024_data_zero_migration[, .(LocID, Location, year, AgeGrp, PopTotal)],
  LocID + Location + year ~ AgeGrp,
  value.var = 'PopTotal',
  fill = 0
)

# Generate ISO3 country codes
dem_proj_2024_data_zero_migration[, iso3c := countrycode(LocID, "un", "iso3c")]

# Fix country codes for Taiwan and Kosovo
dem_proj_2024_data_zero_migration[LocID == 158, iso3c := 'TWN']  # Taiwan
dem_proj_2024_data_zero_migration[LocID == 412, iso3c := 'XKX']  # Kosovo

# Remove rows without ISO3 codes (aggregates)
dem_proj_2024_data_zero_migration <- dem_proj_2024_data_zero_migration[!is.na(iso3c)]

# -------------------------------
# 16. Calculate Total Population for Zero Migration
# -------------------------------

# Define age columns for Zero Migration
zm_age_cols <- c(paste0('zm_age_', 0:99), 'zm_age_100+')

# Calculate total population for Zero Migration
dem_proj_2024_data_zero_migration[, pop_total_zm := rowSums(.SD, na.rm = TRUE), .SDcols = zm_age_cols]

# -------------------------------
# 17. Aggregate Various Age Groups for Zero Migration
# -------------------------------

# Define age aggregations for Zero Migration
zm_age_aggregations <- list(
  c(0, 14),
  c(15, 24),
  c(25, 64),
  c(15, 64),
  c(20, 64)
)

# Apply age aggregations for Zero Migration
for (agg in zm_age_aggregations) {
  start_age <- agg[1]
  end_age <- agg[2]
  new_var <- paste0('pop_age_', start_age, '_', end_age, '_zm')
  aggregate_age_groups(
    dt = dem_proj_2024_data_zero_migration,
    prefix = "zm_age_",
    start_age = start_age,
    end_age = end_age,
    new_var = new_var
  )
}

# Additional aggregations: 0-4, 5-9, ..., 75-79 for Zero Migration
for (age in seq(0, 75, 5)) {
  start_age <- age
  end_age <- age + 4
  new_var <- paste0('pop_age_', start_age, '_', end_age, '_zm')
  aggregate_age_groups(
    dt = dem_proj_2024_data_zero_migration,
    prefix = "zm_age_",
    start_age = start_age,
    end_age = end_age,
    new_var = new_var
  )
}

# -------------------------------
# 18. Calculate Population Aged 65+ for Zero Migration
# -------------------------------

dem_proj_2024_data_zero_migration[, pop_age_65_plus_zm := rowSums(.SD, na.rm = TRUE), .SDcols = c( paste0('zm_age_', 65:99), "zm_age_100+")]

# -------------------------------
# 19. Merge Zero Migration Scenario with Main Demographic Data
# -------------------------------

# Merge Zero Migration data with main demographic data
demographic_data <- merge(
  demographic_data,
  dem_proj_2024_data_zero_migration,
  by = c('iso3c', 'year', 'Location'),
  all = TRUE
)

# Clean up
rm(dem_proj_2024_data_zero_migration)
gc()

# -------------------------------
# 20. Analyze Migration Age Structure
# -------------------------------

# Define age columns for migration age structure analysis
migration_age_cols <- c(
  paste0('age_', 0:99), 'age_100+', 'pop_total',
  paste0('zm_age_', 0:99), 'zm_age_100+', 'pop_total_zm'
)

# Calculate migration age structure where net migration is negative
migration_age_structure <- demographic_data[
  (pop_total - pop_total_zm) < 0,
  lapply(.SD, sum, na.rm = TRUE),
  by = year,
  .SDcols = migration_age_cols
]

# Order by year
setorder(migration_age_structure, year)

# Rename 'age_100+' and 'zm_age_100+' for consistency
setnames(
  migration_age_structure,
  old = c("age_100+", "zm_age_100+"),
  new = c("age_100_plus", "zm_age_100_plus")
)

# Define age columns after renaming
age_cols_clean <- c(paste0('age_', 0:99), "age_100_plus")

# Calculate migration ratios for each age group
migration_age_structure[, paste0("migr_ratio_", age_cols_clean) := lapply(age_cols_clean, function(col) {
  (get(col) - get(paste0("zm_", col))) / (pop_total - pop_total_zm)
})]

# -------------------------------
# 21. Calculate Working Age Population for Zero Migration
# -------------------------------

# Function to calculate zero migration working age population with flexible upper age limit
calculate_working_age_pop <- function(dt, start_age, offset) {
  # Define the new variable name
  new_var <- paste0('zm_working_age_pop_', start_age, '_lifexp_offset_', offset)
  
  # Calculate end_age and fraction for each row
  dt[, `:=`(
    end_age = floor(lifexp_total_at_65 + 65 - offset),
    fraction = (lifexp_total_at_65 + 65 - offset) - floor(lifexp_total_at_65 + 65 - offset)
  )]
  
  # Initialize the new working age population column with NA_real_
  dt[, (new_var) := NA_real_]
  
  # Identify rows where 'lifexp_total_at_65' is not NA
  valid_rows <- which(!is.na(dt$lifexp_total_at_65))
  
  if (length(valid_rows) == 0) {
    # No valid rows to process
    dt[, c("end_age", "fraction") := NULL]
    return(NULL)
  }
  
  # Define age columns
  age_cols <- paste0('zm_age_', 0:99)  # 'age_100' does not exist, only 'age_100+'
  available_age_cols <- intersect(age_cols, names(dt))
  
  if (length(available_age_cols) == 0) {
    stop("No age columns found in the data.")
  }
  
  # Extract age data for valid rows into a matrix for faster access
  age_matrix <- as.matrix(dt[valid_rows, ..available_age_cols])
  
  # Extract relevant 'end_age' and 'fraction' values for valid rows
  end_ages <- dt$end_age[valid_rows]
  fractions <- dt$fraction[valid_rows]
  
  # Helper function to calculate working age population for a single row
  calc_working_age <- function(row_idx, e_age, frac) {
    # Ensure end_age is within valid bounds
    e_age <- max(e_age, start_age - 1)  # Prevent negative or below start_age
    
    if (e_age < start_age) {
      # If end_age is below start_age, working_age_pop remains 0
      return(0)
    }
    
    # Define age indices (R is 1-based)
    age_start_idx <- start_age + 1
    age_end_idx <- e_age + 1
    
    # Ensure age_end_idx does not exceed the number of columns
    age_end_idx <- min(age_end_idx, ncol(age_matrix))
    
    # Sum populations from start_age to end_age
    sum_pop <- sum(age_matrix[row_idx, age_start_idx:age_end_idx], na.rm = TRUE)
    
    # Add fractional population from the next age group if it exists
    next_age_col <- paste0('zm_age_', e_age + 1)
    if ((e_age + 1) <= 99 && next_age_col %in% available_age_cols) {
      sum_pop <- sum_pop + frac * age_matrix[row_idx, paste0('zm_age_', e_age + 1)]
    }
    
    return(sum_pop)
  }
  
  # Apply the helper function to each valid row using mapply
  working_age_pop_values <- mapply(
    FUN = calc_working_age,
    row_idx = seq_along(valid_rows),
    e_age = end_ages,
    frac = fractions,
    SIMPLIFY = TRUE
  )
  
  # Assign the computed working age population to the new column
  dt[valid_rows, (new_var) := working_age_pop_values]
  
  # Remove temporary columns
  dt[, c("end_age", "fraction") := NULL]
}

# Apply the function to calculate working age population for age 15 with offset 15
calculate_working_age_pop(demographic_data, 15, 15)


# Apply the function to calculate working age population for age 15 with offset 15
calculate_working_age_pop(demographic_data, 15, 15)

# -------------------------------
# 22. Calculate Working Age Population Ratios Among Net Migrants
# -------------------------------

# Merge migration ratios into the main data table

demographic_data <- merge ( demographic_data ,
                            migration_age_structure [ 
                              , c('year', paste0 ( 'migr_ratio_age_', seq( 0, 99, 1) ), 'migr_ratio_age_100_plus' ) ],
                            by = "year", all = TRUE )


# Function to calculate working age population ratios among net migrants
# Function to calculate zero migration working age population with flexible upper age limit
calculate_working_age_pop <- function(dt, start_age, offset) {
  # Define the new variable name
  new_var <- paste0('migr_working_age_pop_', start_age, '_lifexp_offset_', offset)
  
  # Calculate end_age and fraction for each row
  dt[, `:=`(
    end_age = floor(lifexp_total_at_65 + 65 - offset),
    fraction = (lifexp_total_at_65 + 65 - offset) - floor(lifexp_total_at_65 + 65 - offset)
  )]
  
  # Initialize the new working age population column with NA_real_
  dt[, (new_var) := NA_real_]
  
  # Identify rows where 'lifexp_total_at_65' is not NA
  valid_rows <- which(!is.na(dt$lifexp_total_at_65))
  
  if (length(valid_rows) == 0) {
    # No valid rows to process
    dt[, c("end_age", "fraction") := NULL]
    return(NULL)
  }
  
  # Define age columns
  age_cols <- paste0('migr_ratio_age_', 0:99)  # 'age_100' does not exist, only 'age_100+'
  available_age_cols <- intersect(age_cols, names(dt))
  
  if (length(available_age_cols) == 0) {
    stop("No age columns found in the data.")
  }
  
  # Extract age data for valid rows into a matrix for faster access
  age_matrix <- as.matrix(dt[valid_rows, ..available_age_cols])
  
  # Extract relevant 'end_age' and 'fraction' values for valid rows
  end_ages <- dt$end_age[valid_rows]
  fractions <- dt$fraction[valid_rows]
  
  # Helper function to calculate working age population for a single row
  calc_working_age <- function(row_idx, e_age, frac) {
    # Ensure end_age is within valid bounds
    e_age <- max(e_age, start_age - 1)  # Prevent negative or below start_age
    
    if (e_age < start_age) {
      # If end_age is below start_age, working_age_pop remains 0
      return(0)
    }
    
    # Define age indices (R is 1-based)
    age_start_idx <- start_age + 1
    age_end_idx <- e_age + 1
    
    # Ensure age_end_idx does not exceed the number of columns
    age_end_idx <- min(age_end_idx, ncol(age_matrix))
    
    # Sum populations from start_age to end_age
    sum_pop <- sum(age_matrix[row_idx, age_start_idx:age_end_idx], na.rm = TRUE)
    
    # Add fractional population from the next age group if it exists
    next_age_col <- paste0('migr_ratio_age_', e_age + 1)
    if ((e_age + 1) <= 99 && next_age_col %in% available_age_cols) {
      sum_pop <- sum_pop + frac * age_matrix[row_idx, paste0('migr_ratio_age_', e_age + 1)]
    }
    
    return(sum_pop)
  }
  
  # Apply the helper function to each valid row using mapply
  working_age_pop_values <- mapply(
    FUN = calc_working_age,
    row_idx = seq_along(valid_rows),
    e_age = end_ages,
    frac = fractions,
    SIMPLIFY = TRUE
  )
  
  # Assign the computed working age population to the new column
  dt[valid_rows, (new_var) := working_age_pop_values]
  
  # Remove temporary columns
  dt[, c("end_age", "fraction") := NULL]
}

# Apply the function to calculate working age population for age 15 with offset 15
calculate_working_age_pop(demographic_data, 15, 15)

# remove migration ratio columns from demographic data file
demographic_data[, c( paste0 ( 'migr_ratio_age_', seq( 0, 99, 1) ), 'migr_ratio_age_100_plus' ) := NULL ]

# -------------------------------
# 23. Finalize and Save Demographic Data
# -------------------------------

# Generate country names from ISO3 codes
demographic_data[, country_name := countrycode(iso3c, "iso3c", "country.name")]

# Save the full demographic data to FST format for fast loading
write.fst(demographic_data, "demographic_data_large.fst")

# Save the migration age structure data to FST format
write.fst(migration_age_structure, "migration_age_structure.fst")

# Clean up
gc()

# -------------------------------
# 24. Exclude Age-Related Columns to Create and Save a Narrower Dataset
# -------------------------------

# Define the pattern to match age-related columns with a literal '+' suffix
pattern_to_exclude <- "^age_\\d+\\+?$|^zm_age_\\d+\\+?$"

# Identify columns that match the pattern
exclude_cols <- grep(pattern_to_exclude, names(demographic_data), value = TRUE)

# Check if any columns matched the pattern
if (length(exclude_cols) == 0) {
  warning("No columns matched the exclusion pattern. Verify column names and patterns.")
} else {
  # Create the narrower dataset by excluding the identified columns
  demographic_data_narrow <- demographic_data[, setdiff(names(demographic_data), exclude_cols), with = FALSE]
}

fwrite(demographic_data_narrow, "demographic_data.csv")
write.fst(demographic_data_narrow, "demographic_data.fst")
rm(demographic_data_narrow, migration_age_structure, age_aggregations, zm_age_aggregations)
gc()

# -------------------------------
# 25. Analyze Discrepancies Between Scenarios
# -------------------------------

# Calculate cumulative migration for years >= 2024
demographic_data[year >= 2024, cum_migr := cumsum(net_migr_UN_2024), by = iso3c]

# View discrepancies for age 44 by year
View(
  demographic_data[year >= 2024, .(
    main_scenario_age_44 = sum(age_44, na.rm = TRUE),
    zm_scenario_age_44 = sum(zm_age_44, na.rm = TRUE),
    diff = sum(age_44, na.rm = TRUE) - sum(zm_age_44, na.rm = TRUE),
    net_migr_age_44 = sum(age_44[get("zm_age_44") - age_44 <= 0], na.rm = TRUE) -
      sum(zm_age_44[get("zm_age_44") - age_44 <= 0], na.rm = TRUE)
  ), by = year]
)

# View discrepancies for total population by year
View(
  demographic_data[year >= 2024, .(
    main_scenario_total = sum(pop_total, na.rm = TRUE),
    zm_scenario_total = sum(pop_total_zm, na.rm = TRUE),
    diff = sum(pop_total, na.rm = TRUE) - sum(pop_total_zm, na.rm = TRUE),
    net_migr_total = sum(pop_total[get("pop_total") - pop_total_zm >= 0], na.rm = TRUE) -
      sum(pop_total_zm[get("pop_total") - pop_total_zm >= 0], na.rm = TRUE)
  ), by = year]
)

# View discrepancies for age 44 for a specific year (e.g., 2040) by country
View(
  demographic_data[year == 2040, .(
    age_44,
    zm_age_44,
    diff = age_44 - zm_age_44,
    diff_pct = (age_44 / zm_age_44 * 100) - 100,
    cum_migr
  ), by = .(iso3c, Location)]
)

# -------------------------------
# 12. Generate Plots
# -------------------------------

# Plot 1: Child Mortality vs Fertility Rate by Decade
ggplot(
  demographic_data[year > 1950 & year < 2019 & child_mort_under_5 < 500],
  aes(x = child_mort_under_5, y = fertility_rate_total, color = factor(floor(year / 10) * 10))
) +
  geom_point() +
  geom_smooth(col = "black") +
  labs(
    title = "Child Mortality vs Fertility Rate",
    x = "Under 5 Child Mortality Rate (per thousand)",
    y = "Total Fertility Rate (children per woman)",
    color = "Decade"
  ) +
  theme_minimal()

# Plot 2: Log-Scaled Child Mortality vs Fertility Rate by Decade
ggplot(
  demographic_data[year > 1950 & year < 2019 & child_mort_under_5 < 500],
  aes(x = child_mort_under_5, y = fertility_rate_total, color = factor(floor(year / 10) * 10))
) +
  scale_x_log10() +
  geom_point(alpha = 0.1) +
  geom_smooth(size = 1) +
  labs(
    title = "Child Mortality vs Fertility Rate (Log Scale)",
    x = "Under 5 Child Mortality Rate (log scale)",
    y = "Total Fertility Rate (children per woman)",
    color = "Decade"
  ) +
  theme_minimal()

# Plot 3: Child Mortality vs Fertility Rate with Smooth Lines
ggplot(
  demographic_data[year < 2019],
  aes(x = child_mort_under_5, y = fertility_rate_total, color = factor(floor(year / 10) * 10))
) +
  scale_x_log10() +
  geom_smooth(se = FALSE) +
  labs(
    title = "Child Mortality vs Fertility Rate",
    x = "Under 5 Child Mortality Rate (log scale)",
    y = "Total Fertility Rate (children per woman)",
    color = "Decade"
  ) +
  theme_minimal()

# Plot 4: Gapminder Data - Child Mortality vs Fertility Rate
ggplot(
  demographic_data[year > 1800 & year < 1950 & child_mort_under_5_gm < 500],
  aes(x = child_mort_under_5_gm, y = fertility_rate_total_gm, color = factor(floor(year / 10) * 10))
) +
  geom_point() +
  geom_smooth(col = "black") +
  labs(
    title = "Gapminder: Child Mortality vs Fertility Rate",
    x = "Under 5 Child Mortality Rate (Gapminder)",
    y = "Total Fertility Rate (Gapminder)",
    color = "Decade"
  ) +
  theme_minimal()

# Plot 5: Combined Scenario - Child Mortality vs Fertility Rate with Gapminder Data
ggplot(
  demographic_data[year < 2019],
  aes(x = child_mort_under_5, y = fertility_rate_total, color = factor(floor(year / 10) * 10))
) +
  scale_x_log10() +
  geom_smooth(se = FALSE) +
  geom_smooth(
    data = demographic_data[year > 1800 & year < 1950 & child_mort_under_5_gm < 500],
    aes(x = child_mort_under_5_gm, y = fertility_rate_total_gm),
    color = "black"
  ) +
  labs(
    title = "Child Mortality vs Fertility Rate with Gapminder Data",
    x = "Under 5 Child Mortality Rate",
    y = "Total Fertility Rate",
    color = "Decade"
  ) +
  theme_minimal()

# Plot 6: Child Mortality vs Female Life Expectancy
ggplot(
  demographic_data[year < 2019],
  aes(x = child_mort_under_5, y = lifexp_female, color = factor(floor(year / 10) * 10))
) +
  scale_x_log10() +
  geom_smooth() +
  labs(
    title = "Child Mortality vs Female Life Expectancy",
    x = "Under 5 Child Mortality Rate (log scale)",
    y = "Female Life Expectancy",
    color = "Decade"
  ) +
  theme_minimal()

# Plot 7: Female Life Expectancy vs Fertility Rate
ggplot(
  demographic_data[year < 2019],
  aes(x = lifexp_female, y = fertility_rate_total, color = factor(floor(year / 10) * 10))
) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Female Life Expectancy vs Fertility Rate",
    x = "Female Life Expectancy",
    y = "Total Fertility Rate (children per woman)",
    color = "Decade"
  ) +
  theme_minimal()
