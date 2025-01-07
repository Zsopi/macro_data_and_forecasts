# =============================================================================
# PROJECT: Collecting and Merging Political and Economic Data from Various Sources
# DESCRIPTION: This script gathers GDP per capita, population, and other 
#              economic indicators from IMF, Penn World Table, World Bank, 
#              and Maddison Project databases. It processes and merges the data 
#              into a comprehensive dataset for further analysis.
# =============================================================================

# -------------------------------
# Load Required Packages
# -------------------------------

# Function to install and load required packages
install_and_load <- function(packages) {
  # Identify packages that are not installed
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  
  # Install missing packages
  if(length(new_packages)) {
    install.packages(new_packages, dependencies = TRUE)
  }
  
  # Load all packages
  sapply(packages, require, character.only = TRUE)
}

# List of required packages
required_packages <- c(
  "data.table",   # For efficient data manipulation
  "countrycode",  # For mapping country codes
  "pwt10",        # For accessing Penn World Tables data
  "WDI",          # For accessing World Development Indicators from the World Bank
  "ggplot2",      # For data visualization
  "fst"           # For fast serialization of data.tables
)

# Install and load packages
install_and_load(required_packages)

# -------------------------------
# Define Variable Descriptions
# -------------------------------

# Variable Descriptions:
# - gdp_pc_imf: IMF GDP per capita, PPP (latest available), in USD
# - gdp_pc_pwt: Penn World Table GDP per capita, PPP (latest available), calculated as rgdpo/pop
# - gdp_pc_wb: World Bank GDP per capita, PPP (latest available)
# - gdp_pc_wb_nom: World Bank GDP per capita, nominal (constant 2010 US$), not PPP
# - gdp_pc_mpd: Maddison Project Database GDP per capita, PPP (2011 USD)
# - pop: Population in millions (source-specific suffix)
# - rent: Total resource rent minus forest rent as a percentage of GDP
# - aggregate_dummy: Indicator for aggregate data (e.g., former Yugoslavia)
# - tax_havens_dummy: Indicator for tax haven countries based on Gabriel-Zucman list
# - resource_dummy: Indicator for countries with resource rent minus forest rent > 8% of GDP on average since 2001
# - iso3c: 3-character ISO country code (e.g., XKX for Kosovo)
# - country: Country name
# - year: Calendar year
# - gdp_pc_grth_10y: 10-year future growth rate of composite GDP per capita
# - gdp_pc_grth_6y_1y_lagged: Lagged growth rate (-6 to -1 years)

# -------------------------------
# Economic Data Generation
# -------------------------------

# -------------------------------
# IMF World Economic Outlook Data
# -------------------------------

# Function to get the latest available IMF World Economic Outlook data
get_latest_imf_WEO <- function() {
  
  # Convert current year to numeric once for efficiency
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  # Define potential WEO data URLs
  file0 <- paste0('https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/',
                  current_year - 1, '/October/WEOOct', 
                  current_year - 1, 'all.ashx')
  
  file1 <- paste0('https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/',
                  current_year, '/April/WEOApr', 
                  current_year, 'all.ashx')
  
  file2 <- paste0('https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/',
                  current_year, '/October/WEOOct', 
                  current_year, 'all.ashx')
  
  # Optionally include a hard-coded file if needed
  # Uncomment the following line if you want to include file3
  # file3 <- "https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2023/WEOApr2023all.ashx"
  
  # Possible filenames where the data may exist
  files <- c(file0, file1, file2)  # Add file3 here if required: c(file0, file1, file2, file3)
  
  # Initialize imf_latest as NULL
  imf_latest <- NULL
  
  # Iterate through each file URL
  for (file in files) {
    
    message("Attempting to download and read: ", file)
    
    # Attempt to read the CSV file with specified parameters
    imf_temp <- tryCatch(
      {
        read.csv(
          file = file,
          header = TRUE,
          sep = "\t",
          fill = TRUE,
          stringsAsFactors = FALSE,
          fileEncoding = "UTF-16LE"
        )
      },
      error = function(e) {
        message("Failed to read ", file, ": ", e$message)
        return(NULL)
      }
    )
    
    # Check if imf_temp was successfully read and contains the required column
    if (!is.null(imf_temp) && ncol(imf_temp) > 2 && 'NGDPRPPPPC' %in% imf_temp$WEO.Subject.Code) {
      imf_latest <- imf_temp
      message("Successfully retrieved data from: ", file)
      # Continue looping to allow later files to overwrite if they exist
    } else {
      message("Data not found or criteria not met in: ", file)
    }
  }
  
  # Final validation to check if any data was retrieved
  if (is.null(imf_latest)) {
    warning("No suitable IMF WEO data found in the provided files.")
  } else {
    message("IMF WEO data successfully retrieved.")
  }
  
  return(imf_latest)
}

# Retrieve IMF data
imf_data <- get_latest_imf_WEO()

# Verify the retrieved data
if (!is.null(imf_data)) {
  message("IMF WEO data successfully retrieved.")
  
} else {
  message("Failed to retrieve IMF WEO data.")
}

# Convert to data.table and clean
setDT(imf_data)
imf_data <- imf_data[!is.na(ISO)]

# Identify year columns (start with 'X')
year_cols <- grep("^X\\d{4}$", names(imf_data), value = TRUE)

# Clean numeric columns by removing commas and converting to numeric
imf_data[, (year_cols) := lapply(.SD, function(x) as.numeric(gsub(",", "", x))), .SDcols = year_cols]

# Filter for relevant indicators and select necessary columns
relevant_codes <- c('NGDPRPPPPC', 'LP', 'GGX_NGDP', 'NID_NGDP')

# Select the required columns using .SD and .SDcols
imf_data_wide <- imf_data[`WEO.Subject.Code` %in% relevant_codes, 
                          .SD, 
                          .SDcols = c("WEO.Country.Code", "WEO.Subject.Code", "ISO", "Country", year_cols)]


# Melt the data to long format
imf_data_long <- melt(
  imf_data_wide,
  id.vars = c('WEO.Country.Code', 'WEO.Subject.Code', 'ISO', 'Country'),
  variable.name = 'year',
  value.name = 'value'
)

# Clean year column
imf_data_long[, year := as.integer(sub("^X", "", year))]

# Cast back to wide format with indicators as columns
imf_data <- dcast(
  imf_data_long,
  `WEO.Country.Code` + ISO + Country + year ~ `WEO.Subject.Code`,
  value.var = 'value'
)

# Rename columns for clarity
setnames(imf_data, 
         old = c('LP', 'NGDPRPPPPC', 'GGX_NGDP', 'NID_NGDP'),
         new = c('pop_imf', 'gdp_pc_imf', 'gov_spd_imf', 'inv_imf'))

# Generate ISO2 and ISO3 country codes
imf_data[, `WEO.Country.Code` := as.numeric(`WEO.Country.Code`)]
imf_data[, iso2c := countrycode(`WEO.Country.Code`, "imf", "iso2c")]
imf_data[, iso3c := countrycode(`WEO.Country.Code`, "imf", "iso3c")]

# Correct specific country codes
corrections <- list(
  AND = list(iso2c = "AD", iso3c = "AND"),
  UVK = list(iso2c = "XK", iso3c = "XKX"),
  TUV = list(iso2c = "TV", iso3c = "TUV"),
  PRI = list(iso2c = "PR", iso3c = "PRI")
)

for (code in names(corrections)) {
  imf_data[ISO == code, `:=`(
    iso2c = corrections[[code]]$iso2c,
    iso3c = corrections[[code]]$iso3c
  )]
}

# Order data by country code and year
setkey(imf_data, iso3c, year)
setorder(imf_data, iso3c, year)

# -------------------------------
# Penn World Table Data
# -------------------------------

# Load Penn World Table data
data("pwt10.01")
pwt_data <- copy(pwt10.01)
rm(pwt10.01)

setDT(pwt_data)

# Create GDP per capita variable
pwt_data[, gdp_pc_pwt := rgdpo / pop]

# Select relevant columns
pwt_data <- pwt_data[, .(
  iso3c = isocode,
  year,
  gdp_pc_pwt,
  rgdpna,
  rgdpo,
  rnna,
  labsh,
  delta,
  pop_pwt = pop,
  emp_pwt = emp,
  avh,
  hc,
  rwtfpna,
  rtfpna,
  ctfp
)]

# Generate ISO2 country codes
pwt_data[, iso2c := countrycode(iso3c, "iso3c", "iso2c")]

# Remove unused factor levels
pwt_data <- droplevels(pwt_data)

# -------------------------------
# World Bank Data
# -------------------------------

# Get current year
current_year <- as.numeric(format(Sys.Date(), "%Y"))

# Define World Bank indicators
wb_indicators <- c(
  'NY.GDP.PCAP.KD',   # GDP per capita (constant 2010 US$)
  'NY.GDP.PCAP.PP.KD',# GDP per capita, PPP (constant 2017 international $)
  'NY.GDP.TOTL.RT.ZS',# Total rent (% of GDP)
  'NY.GDP.FRST.RT.ZS',# Forest rent (% of GDP)
  'SP.POP.TOTL',      # Population, total
  'SL.TLF.TOTL.IN',   # Labor force, total
  'SL.UEM.TOTL.ZS'    # Unemployment, total (% of total labor force)
)

# Download World Bank data
wb_data <- WDI(
  indicator = wb_indicators,
  country = "all",
  start = 1960,
  end = current_year,
  extra = TRUE
)

setDT(wb_data)

# Remove regional aggregates
wb_data <- wb_data[region != "Aggregates" | is.na(region)]

# Correct specific countries
wb_data[iso3c == "VNM", region := "XXX"]
wb_data[iso3c == "CZE", region := "XXX"]

# Remove rows with missing region (aggregates)
wb_data <- wb_data[!is.na(region)]

# Calculate "rent" variable (total rent minus forest rent)
wb_data[, rent := NY.GDP.TOTL.RT.ZS - NY.GDP.FRST.RT.ZS]

# Rename columns for clarity
setnames(wb_data, 
         old = c('NY.GDP.PCAP.KD', 'NY.GDP.PCAP.PP.KD', 'SP.POP.TOTL', 
                 'SL.TLF.TOTL.IN', 'SL.UEM.TOTL.ZS'),
         new = c('gdp_pc_wb_nom', 'gdp_pc_wb', 'pop_wb', 
                 'labor_force_wb', 'unempl_wb'))

# Calculate employment from labor force and unemployment
wb_data[, emp_wb := labor_force_wb * (1 - unempl_wb / 100)]

# Select relevant columns
wb_data <- wb_data[, .(
  iso3c, country, year, gdp_pc_wb, gdp_pc_wb_nom, pop_wb, rent, emp_wb, 
  labor_force_wb, unempl_wb
)]

# Remove unused factor levels
wb_data <- droplevels(wb_data)

# Scale population to millions
wb_data[, pop_wb := pop_wb / 1e6]

# -------------------------------
# Maddison Project Data
# -------------------------------

# Load Maddison Project data
mpd_url <- 'https://dataverse.nl/api/access/datafile/421302'
mpd_data <- read.xlsx(mpd_url, sheet = 'Full data')

setDT(mpd_data)

# Rename columns for clarity
setnames(mpd_data, 
         old = c('countrycode', 'gdppc', 'pop'),
         new = c('iso3c', 'gdp_pc_mpd', 'pop_mpd'))

# Scale population to millions
mpd_data[, pop_mpd := pop_mpd / 1e3]

# Remove rows with missing GDP per capita
mpd_data <- mpd_data[!is.na(gdp_pc_mpd)]

# Add country names
mpd_data[, country_name := countrycode(iso3c, "iso3c", "country.name")]

# -------------------------------
# Merge GDP per Capita Data
# -------------------------------

# Merge IMF, PWT, WB, and MPD data
economic_data <- merge(
  imf_data[, .(iso3c, year, gdp_pc_imf, pop_imf, gov_spd_imf, inv_imf)],
  pwt_data,
  by = c('iso3c', 'year'),
  all = TRUE
)

economic_data <- merge(
  economic_data,
  wb_data[, .(iso3c, year, gdp_pc_wb, gdp_pc_wb_nom, pop_wb, rent, emp_wb)],
  by = c('iso3c', 'year'),
  all = TRUE
)

economic_data <- merge(
  economic_data,
  mpd_data[!is.na(gdp_pc_mpd), .(iso3c, year, gdp_pc_mpd, pop_mpd)],
  by = c('iso3c', 'year'),
  all = TRUE
)

# Add country names using World Bank data
economic_data[, country := countrycode(iso3c, 'iso3c', 'country.name')]

# Correct specific country names
economic_data[iso3c == 'XKX', country := 'Kosovo']
economic_data[iso3c == 'CHI', country := 'Channel Islands']

# Add old country names from Maddison Project for missing entries
old_country_codes <- unique(mpd_data[iso3c %in% economic_data[is.na(country), iso3c], .(iso3c, country)])

economic_data <- merge(
  economic_data,
  old_country_codes,
  by = "iso3c",
  all.x = TRUE,
  suffixes = c("", "_old")
)

economic_data[is.na(country) & !is.na(country_old), country := country_old]
economic_data[, country_old := NULL]

# Remove entries with missing ISO3C codes
economic_data <- economic_data[!is.na(iso3c)]

# Indicate aggregate data
aggregate_codes <- c("CSK", "SUN", "YUG")
economic_data[, aggregate_dummy := as.integer(iso3c %in% aggregate_codes | is.na(country))]

# Remove suspect data entries
economic_data[iso3c == "MLT" & year < 1965, gdp_pc_pwt := NA]
economic_data[iso3c == "TCA" & year < 2011, gdp_pc_pwt := NA]

# Order data by country and year
setorder(economic_data, iso3c, year)

# -------------------------------
# Calculate Growth Rates
# -------------------------------

# Calculate GDP per capita growth rates
growth_vars <- c("gdp_pc_imf", "gdp_pc_wb", "gdp_pc_pwt", "gdp_pc_mpd")
for (var in growth_vars) {
  economic_data[, paste0(var, "_grth") := get(var) / shift(get(var), n = 1) - 1, by = iso3c]
}

# -------------------------------
# Calculate Relative Price Levels
# -------------------------------

# Define benchmark year and country for relative price levels
benchmark_year <- 2010
benchmark_country <- "USA"

# Calculate average relative prices compared to IMF data for the USA since 2010
relative_prices <- economic_data[year >= benchmark_year & iso3c == benchmark_country, .(
  pwt_rel = mean(gdp_pc_imf / gdp_pc_pwt, na.rm = TRUE),
  mpd_rel = mean(gdp_pc_imf / gdp_pc_mpd, na.rm = TRUE),
  wb_rel  = mean(gdp_pc_imf / gdp_pc_wb, na.rm = TRUE)
)]

# Merge relative prices back to economic_data
economic_data[, `:=`(
  pwt_rel = relative_prices$pwt_rel,
  mpd_rel = relative_prices$mpd_rel,
  wb_rel  = relative_prices$wb_rel
), by = iso3c]

# -------------------------------
# Create Composite GDP per Capita
# -------------------------------

# Initialize composite GDP per capita with IMF data
economic_data[, `:=`(
  gdp_pc = gdp_pc_imf,
  gdp_source = ifelse(!is.na(gdp_pc_imf), "imf", NA_character_)
)]

# Identify countries with no IMF GDP data whatsoever
no_imf_data <- economic_data[, .(sum_gdp = sum(!is.na(gdp_pc_imf))), by = iso3c][sum_gdp == 0, iso3c]

# Use World Bank data where IMF data is completely unavailable for any year
economic_data[iso3c %in% no_imf_data & !is.na(gdp_pc_wb), `:=`(
  gdp_pc = gdp_pc_wb,
  gdp_source = "wb"
)]

# Identify countries with neither IMF nor World Bank GDP data
no_imf_or_wb_data <- economic_data[, .(sum_gdp = sum(!is.na(gdp_pc))), by = iso3c][sum_gdp == 0, iso3c]

# Use Maddison Project data where IMF and World Bank data are unavailable
economic_data[iso3c %in% no_imf_or_wb_data & !is.na(gdp_pc_mpd), `:=`(
  gdp_pc = gdp_pc_mpd * relative_prices$mpd_rel,
  gdp_source = "mpd"
)]

# Identify countries with no IMF, World Bank, or Maddison data
no_imf_or_wb_or_mpd_data <- economic_data[, .(sum_gdp = sum(!is.na(gdp_pc))), by = iso3c][sum_gdp == 0, iso3c]

# Use Penn World Table data where all above are unavailable
economic_data[iso3c %in% no_imf_or_wb_or_mpd_data & !is.na(gdp_pc_pwt), `:=`(
  gdp_pc = gdp_pc_pwt * relative_prices$pwt_rel,
  gdp_source = "pwt"
)]

# -------------------------------
# Chain GDP per Capita Levels
# -------------------------------

#' Function to chain GDP per Capita Levels Backwards Based on Growth Rates
#'
#' This function fills in missing GDP per capita values by chaining backwards using available growth rates.
#' It calculates the GDP per capita for missing years based on the growth rate from the subsequent year.
#'
#' @param dt A data.table containing GDP per capita and growth rate columns.
#' @param source_suffix A string indicating the data source suffix (e.g., "imf", "wb").
#'
#' @return The input data.table `dt` with updated GDP per capita and GDP source columns.
#'
#' @examples
#' chain_gdp_backward(economic_data, "imf")

chain_gdp_backward <- function(dt, source_suffix) {
  # Define column names based on the source suffix
  growth_col <- paste0("gdp_pc_", source_suffix, "_grth")
  gdp_col <- "gdp_pc"
  
  # Validate that required columns exist
  if (!all(c(gdp_col, growth_col) %in% names(dt))) {
    stop(paste("Missing required columns:", paste(setdiff(c(gdp_col, growth_col), names(dt)), collapse = ", ")))
  }
  
  # Order the data by country code and year to ensure proper sequencing
  setorder(dt, iso3c, year)
  
  # Calculate chained GDP per capita using growth rates
  dt[, paste0(gdp_col, "_chained") := {
    gdp <- get(gdp_col)
    growth <- get(growth_col)
    # Initialize chained GDP as current GDP
    chained_gdp <- gdp
    # Iterate backwards to fill missing GDP values
    for (i in (length(gdp) - 1):1) {
      if (is.na(chained_gdp[i]) && !is.na(growth[i + 1])) {
        chained_gdp[i] <- chained_gdp[i + 1] / (1 + growth[i + 1])
      }
    }
    chained_gdp
  }, by = iso3c]
  
  # Update GDP per capita and source for missing GDP values using the chained values
  dt[is.na(get(gdp_col)) & !is.na(get(paste0(gdp_col, "_chained"))), 
     c(gdp_col, "gdp_source") := .(
       get(paste0(gdp_col, "_chained")),
       paste0(source_suffix, "_grth")
     )
  ]
  
  # Remove the temporary chained GDP column as it's no longer needed
  dt[, paste0(gdp_col, "_chained") := NULL]
  
  # Optional: Validate if any NAs remain in gdp_pc
  if (any(is.na(dt[[gdp_col]]))) {
    warning("Some GDP per capita values remain missing after chaining.")
  }
}

# Apply the function sequentially for various GDP sources
growth_types <- c("wb", "mpd", "pwt")

for (suffix in growth_types) {
  chain_gdp_backward(economic_data, suffix)
}

# -------------------------------
# Calculate Yearly Growth Rate of Chained GDP per Capita
# -------------------------------

economic_data[, gdp_pc_growth := ifelse(
  (year - 1) == shift(year, n = 1),
  gdp_pc / shift(gdp_pc, n = 1) - 1,
  NA_real_
), by = iso3c]

# -------------------------------
# Tax Havens and Resource Dummies
# -------------------------------

# Define tax havens based on Gabriel-Zucman list and additional countries
tax_havens_list <- c(
  "IRL", "LUX", "CHE", "SMR", "AND", "AIA", "ATG", "ABW", "BHS", "BHR", 
  "BRB", "BLZ", "BMU", "BES", "VGB", "CYM", "CUW", "CYP", "JEY", "GRD", 
  "GGY", "GIB", "HKG", "IMN", "LBN", "LIE", "MAC", "MLT", "MHL", "MCO", 
  "SXM", "MUS", "SYC", "SGP", "KNA", "LCA", "VCT", "TCA", "PAN", "PRI"
)

# Create tax havens dummy
economic_data[, tax_havens_dummy := as.integer(iso3c %in% tax_havens_list)]

# Create resource dummy for countries with average rent > 8% or max rent > 10% since 2001
resource_countries <- economic_data[year >= 2001, .(
  mean_rent = mean(rent, na.rm = TRUE),
  max_rent = max(rent, na.rm = TRUE)
), by = .(country, iso3c)][mean_rent >= 8 | max_rent >= 10, iso3c]

economic_data[, resource_dummy := as.integer(iso3c %in% resource_countries)]

# -------------------------------
# Finalize and Save Economic Data
# -------------------------------

# Add country names if missing
economic_data[, country_name := countrycode(iso3c, "iso3c", "country.name")]

# Save economic data to CSV and FST formats
fwrite(economic_data, "economic_data.csv")
write_fst(economic_data, "economic_data.fst")

# Clean up environment
rm(imf_data, pwt_data, wb_data, mpd_data, imf_data_wide, imf_data_long, corrections, relative_prices, old_country_codes)
gc()

# -------------------------------
# Sample Charts
# -------------------------------

# Calculate future 10-year growth rates
economic_data[, gdp_pc_grth_10y_lead := (shift(gdp_pc, type = "lead", n = 10) / gdp_pc)^(1/10) - 1, by = iso3c]
economic_data[, gdp_pc_lead := shift(gdp_pc, type = "lead", n = 10)]

# Plot GDP per capita vs 10-year future growth rate (1800-1900)
ggplot(
  data = economic_data[resource_dummy == 0 & tax_havens_dummy == 0 & year >= 1800 & year < 1900 & gdp_pc <= 50000],
  aes(x = gdp_pc, y = gdp_pc_grth_10y_lead, color = as.factor(floor(year / 10) * 10))
) +
  geom_smooth(se = FALSE) +
  labs(
    title = "GDP per Capita vs 10-Year Future Growth (1800-1900)",
    x = "GDP per Capita (USD)",
    y = "10-Year Future Growth Rate",
    color = "Decade"
  )

# Plot GDP per capita vs 10-year future growth rate (1900 onwards)
ggplot(
  data = economic_data[resource_dummy == 0 & tax_havens_dummy == 0 & year >= 1900 & gdp_pc <= 50000],
  aes(x = gdp_pc, y = gdp_pc_grth_10y_lead, color = as.factor(floor(year / 10) * 10))
) +
  geom_smooth(se = FALSE) +
  labs(
    title = "GDP per Capita vs 10-Year Future Growth (1900 onwards)",
    x = "GDP per Capita (USD)",
    y = "10-Year Future Growth Rate",
    color = "Decade"
  )

# Example: Plot GDP per capita for a specific country (e.g., USA)
iso <- "USA"

ggplot() +
  geom_line(data = economic_data[year %in% 1950:2024 & iso3c == iso], aes(x = year, y = gdp_pc)) +
  geom_line(data = economic_data[year %in% 1950:2024 & iso3c == iso], aes(x = year, y = gdp_pc_imf), color = "red") +
  geom_line(data = economic_data[year %in% 1950:2024 & iso3c == iso], aes(x = year, y = gdp_pc_wb), color = "blue") +
  geom_line(data = economic_data[year %in% 1950:2024 & iso3c == iso], aes(x = year, y = gdp_pc_mpd * mpd_rel), color = "orange") +
  geom_line(data = economic_data[year %in% 1950:2024 & iso3c == iso], aes(x = year, y = gdp_pc_pwt * pwt_rel), color = "green") +
  labs(
    title = paste("GDP per Capita Over Time for", iso),
    x = "Year",
    y = "GDP per Capita (USD)"
  )

# Example: Log-scale plot for a specific country
ggplot() +
  geom_line(data = economic_data[year %in% 1800:2024 & iso3c == iso], aes(x = year, y = gdp_pc)) +
  geom_line(data = economic_data[year %in% 1800:2024 & iso3c == iso], aes(x = year, y = gdp_pc_imf), color = "red") +
  geom_line(data = economic_data[year %in% 1800:2024 & iso3c == iso], aes(x = year, y = gdp_pc_wb), color = "blue") +
  geom_line(data = economic_data[year %in% 1800:2024 & iso3c == iso], aes(x = year, y = gdp_pc_mpd * mpd_rel), color = "orange") +
  geom_line(data = economic_data[year %in% 1800:2024 & iso3c == iso], aes(x = year, y = gdp_pc_pwt * pwt_rel), color = "green") +
  scale_y_log10() +
  geom_smooth(data = economic_data[year %in% 1800:2024 & iso3c == iso], aes(x = year, y = gdp_pc)) +
  labs(
    title = paste("Log-Scaled GDP per Capita Over Time for", iso),
    x = "Year",
    y = "GDP per Capita (Log Scale)"
  )
