###############################################
# Political Data Collection and Processing     #
# Sources: UCDP, Cline Center, V-Dem          #
###############################################

# -------------------------------
# 1. Load Required Packages
# -------------------------------

# List of required packages
required_packages <- c(
  "data.table",    # Efficient data manipulation
  "countrycode",   # Country code conversion
  "fst",            # Fast serialization
  "ggplot2",        # Data visualization
  "devtools"        # Install packages from GitHub
)

# Function to install and load required packages
install_and_load <- function(packages) {
  # Identify packages that are not installed
  new_packages <- setdiff(packages, rownames(installed.packages()))
  
  # Install missing packages
  if(length(new_packages)) {
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

# Function to download and read a zipped CSV file
download_and_read_zip <- function(url, pattern = NULL) {
  temp_zip <- tempfile(fileext = ".zip")
  download.file(url, temp_zip, mode = "wb", quiet = TRUE)
  
  # If a pattern is provided, extract specific files
  if (!is.null(pattern)) {
    zip_contents <- unzip(temp_zip, list = TRUE)
    file_to_extract <- zip_contents$Name[grepl(pattern, zip_contents$Name)]
    
    if(length(file_to_extract) == 0){
      stop("No files matching the pattern were found in the zip archive.")
    }
    
    data <- fread(unzip(temp_zip, files = file_to_extract[1], exdir = tempdir()))
  } else {
    # Extract all files if no pattern is provided
    extracted_files <- unzip(temp_zip, exdir = tempdir())
    csv_files <- extracted_files[grepl("\\.csv$", extracted_files, ignore.case = TRUE)]
    
    if(length(csv_files) == 0){
      stop("No CSV files found in the zip archive.")
    }
    
    # Read the first CSV file found
    data <- fread(csv_files[1])
  }
  
  unlink(temp_zip)  # Remove the temporary zip file
  return(setDT(data))  # Convert to data.table
}

# Function to download and read a CSV file directly
download_and_read_csv <- function(url) {
  temp_csv <- tempfile(fileext = ".csv")
  download.file(url, temp_csv, mode = "wb", quiet = TRUE)
  
  # Read the CSV file
  data <- fread(temp_csv)
  
  unlink(temp_csv)  # Remove the temporary CSV file
  return(setDT(data))  # Convert to data.table
}

# Function to correct ISO3c codes based on specific conditions
correct_iso_codes <- function(dt, iso_var = "iso3c", location_var = "location", year_var = "year") {
  dt[, (iso_var) := fcase(
    get(location_var) == "Hyderabad", "IND",
    get(location_var) == "Yemen (North Yemen)" & get(year_var) <= 1990, "YAR",
    get(location_var) == "Yemen (North Yemen)" & get(year_var) > 1990, "YEM",
    get(location_var) == "South Yemen" & get(year_var) <= 1990, "YMD",
    get(location_var) == "South Yemen" & get(year_var) > 1990, "YEM",
    get(location_var) == "South Vietnam", "RVN",
    default = get(iso_var)
  )]
  return(dt)
}

# Function to correct ISO3c codes in Coup d'État data based on cowcode
correct_iso_codes_coup <- function(dt) {
  dt[, iso3c := fcase(
    cowcode == 393, "RUS",
    cowcode == 340, "SRB",
    cowcode == 315, "CSK",
    cowcode == 265, "DDR",
    cowcode == 260, "DEU",
    cowcode == 817, "RVN",
    cowcode == 678, "YEM",
    cowcode == 680, "YMD",
    default = iso3c
  )]
  return(dt)
}

# Function to correct ISO3c codes in V-Dem data
correct_iso_codes_vdem <- function(dt) {
  dt[year < 1990 & iso3c == "YEM", iso3c := "YAR"]
  dt[iso3c == "VDR", iso3c := "RVN"]
  dt[year %in% 1945:1975 & iso3c == "VNM", iso3c := "VDR"]
  dt[(year %in% 1918:1938 | year %in% 1945:1992) & iso3c == "CZE", iso3c := "CSK"]
  dt[iso3c == "SRB" & year %in% 1918:1991, iso3c := "YUG"]
  return(dt)
}

# Function to summarize Coup d'État data
summarize_coup_data <- function(dt) {
  dt[, .(
    attempted_coup = sum(attempt, na.rm = TRUE),
    realized_coup = sum(realized, na.rm = TRUE),
    coup_conspiracy = sum(conspiracy, na.rm = TRUE)
  ), by = .(iso3c, year)]
}

# Function to summarize UCDP data
summarize_ucdp_data <- function(dt) {
  dt[, .(
    political_violence_intensity = max(intensity_level, na.rm = TRUE),
    ucdp_type_of_conflict = max(type_of_conflict, na.rm = TRUE)
  ), by = .(year, iso3c)]
}

# Function to select relevant V-Dem columns
select_vdem_columns <- function(dt) {
  dt[, .(
    year,
    iso3c,
    country_name_vdem,
    histname,
    v2x_polyarchy,
    v2x_libdem,
    v2x_partipdem,
    v2x_delibdem,
    v2x_egaldem,
    v2x_liberal
  )]
}

# -------------------------------
# 3. Download and Process UCDP Conflict Data
# -------------------------------

# UCDP Conflict Data URL
ucdp_url <- "https://ucdp.uu.se/downloads/ucdpprio/ucdp-prio-acd-241-csv.zip"

# Download and read UCDP data
ucdp_data <- tryCatch({
  download_and_read_zip(ucdp_url, pattern = "UcdpPrioConflict_v24_1.csv")
}, error = function(e) {
  stop("Failed to download or read UCDP data: ", e$message)
})

# Convert side_a_id to numeric and map to ISO3c codes
ucdp_data[, iso_tmp := countrycode(as.numeric(side_a_id), "gwn", "iso3c")]

# Filter for domestic political violence cases (type_of_conflict 3 and 4)
ucdp_data <- ucdp_data[type_of_conflict %in% 3:4]

# Create iso3c variable from location
ucdp_data[, iso3c := countrycode(location, "country.name", "iso3c")]

# Fill missing iso3c codes with specific cases
ucdp_data <- correct_iso_codes(ucdp_data, iso_var = "iso3c", location_var = "location", year_var = "year")

# Determine min and max years in UCDP data
ucdp_min_year <- ucdp_data[, min(year, na.rm = TRUE)]
ucdp_max_year <- ucdp_data[, max(year, na.rm = TRUE)]

# -------------------------------
# 4. Download and Process Coup d'État Data
# -------------------------------

# Coup d'État Data URL
coup_url <- "https://databank.illinois.edu/datafiles/jqt8o/download"

# Download and read Coup d'État data
coup_data <- tryCatch({
  # Since the Coup d'État URL points directly to a CSV, use download_and_read_csv
  download_and_read_csv(coup_url)
}, error = function(e) {
  stop("Failed to download or read Coup d'État data: ", e$message)
})

# Convert cowcode to ISO3c codes
coup_data[, iso3c := countrycode(as.numeric(cowcode), "cown", "iso3c")]

# Correct specific ISO3c codes in Coup d'État data
coup_data <- correct_iso_codes_coup(coup_data)

# Determine min and max years in Coup d'État data
coup_data_min_year <- coup_data[, min(year, na.rm = TRUE)]
coup_data_max_year <- coup_data[, max(year, na.rm = TRUE)]

# -------------------------------
# 5. Download and Process V-Dem Data
# -------------------------------

# Install and load V-Dem data package if not already installed
if(!"vdemdata" %in% rownames(installed.packages())){
  devtools::install_github("vdeminstitute/vdemdata")
}
library(vdemdata)

# Load V-Dem data
vdem_data <- vdem

setDT (vdem_data)

# create iso variable
vdem_data[ , iso3c := country_text_id ]

# Correct ISO3c codes in V-Dem data
vdem_data <- correct_iso_codes_vdem(vdem_data)

# Rename 'country_name' to 'country_name_vdem'
setnames(vdem_data, "country_name", "country_name_vdem")

# Select relevant V-Dem columns
vdem_data <- select_vdem_columns(vdem_data)

# -------------------------------
# 6. Merge V-Dem and Coup d'État Data
# -------------------------------

# Summarize Coup d'État data
coup_summary <- summarize_coup_data(coup_data)

# Merge V-Dem data with Coup d'État summary
political_data <- merge(
  vdem_data,
  coup_summary,
  by = c("year", "iso3c"),
  all = TRUE
)

# -------------------------------
# 7. Pad Coup d'État Data with Zeros Where Missing
# -------------------------------

# Define the range of years for Coup d'État data
coup_year_range <- coup_data_min_year:coup_data_max_year

# Replace NA values with 0 for Coup d'État variables within the year range
political_data[
  year %in% coup_year_range & is.na(attempted_coup),
  attempted_coup := 0
]

political_data[
  year %in% coup_year_range & is.na(realized_coup),
  realized_coup := 0
]

political_data[
  year %in% coup_year_range & is.na(coup_conspiracy),
  coup_conspiracy := 0
]

# -------------------------------
# 8. Merge UCDP Conflict Data into Political Data
# -------------------------------

# Summarize UCDP data
ucdp_summary <- summarize_ucdp_data(ucdp_data)

# Merge UCDP summary into political_data
political_data <- merge(
  political_data,
  ucdp_summary,
  by = c("year", "iso3c"),
  all = TRUE
)

# Define the range of years for UCDP data
ucdp_year_range <- ucdp_min_year:ucdp_max_year

# Replace NA values with 0 for UCDP variables within the year range
political_data[
  year %in% ucdp_year_range & is.na(political_violence_intensity),
  political_violence_intensity := 0
]

political_data[
  year %in% ucdp_year_range & is.na(ucdp_type_of_conflict),
  ucdp_type_of_conflict := 0
]

# -------------------------------
# 9. Create Political Trouble Dummy Variable
# -------------------------------

# Define 'political_trouble_dummy' as 1 if political_violence_intensity == 2 or attempted_coup > 0, else 0
political_data[, political_trouble_dummy := as.integer(political_violence_intensity == 2 | attempted_coup > 0)]

# -------------------------------
# 10. Clean Up and Memory Management 
# -------------------------------

# Remove intermediate datasets to free memory
rm(coup_data, ucdp_data, vdem_data)
gc()

# -------------------------------
# 11. Save the Political Data
# -------------------------------

# Save the full political data to FST and CSV formats
write.fst(political_data, "political_data_gpt.fst")
fwrite(political_data, "political_data_gpt.csv")

# -------------------------------
# 12. Generate Plots
# -------------------------------

# Plot 1: Mean Political Trouble Dummy by Year
ggplot(political_data[ year >=1950,
                       .( mean_political_trouble = mean(political_trouble_dummy, na.rm = T)),
                       by = year], aes(x = year, y = mean_political_trouble, na.rm = TRUE)) +
  geom_line(color = "blue") +
  labs(
    title = "Mean Political Trouble Dummy by Year",
    x = "Year",
    y = "Mean Political Trouble Dummy"
  ) +
  theme_minimal()

# Plot 2: Proportion of Political Trouble Dummy by Year
ggplot(political_data[year >= 1950,.( political_trouble_ratio = sum(political_trouble_dummy, na.rm = T)/
                                       length(political_trouble_dummy [ political_trouble_dummy %in% 0:1])),
                      by = year], aes(x = year, y = political_trouble_ratio ) ) +
  geom_line(color = "green") +
  labs(
    title = "Proportion of Political Trouble Dummy by Year",
    x = "Year",
    y = "Proportion of Political Trouble"
  ) +
  theme_minimal()

# Plot 3: Attempted Coup Proportion by Year with Red Line
ggplot() +
  geom_line(data = political_data[year >= 1950 & !is.na(political_trouble_dummy),
                                  .(political_trouble_ratio = sum(political_trouble_dummy, na.rm = TRUE) / 
                                                   length(political_trouble_dummy )), by = year], 
            aes(x = year, y =  political_trouble_ratio ), 
            color = "blue", size = 1) +
  geom_line(data = political_data[year >= 1950,
                                  .(attempted_coup_ratio = sum(attempted_coup, na.rm = TRUE) / 
                                      length(attempted_coup[attempted_coup %in% 0:1])), by = year], 
            aes(x = year, y = attempted_coup_ratio), 
            color = "red", size = 1) +
  labs(
    title = "Proportion of Political Trouble and Attempted Coups by Year",
    x = "Year",
    y = "Proportion"
  ) +
  scale_color_manual(values = c("green" = "Political Trouble", "red" = "Attempted Coup")) +
  theme_minimal()

# -------------------------------
# End of Script
# -------------------------------
