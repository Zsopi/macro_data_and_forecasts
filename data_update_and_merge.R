#####  CODE DESCRIPTION:  MERGING ECONOMIC, POLITICAL AND DEMOGRAPHIC DATASETS #####

# -------------------------------
# 1. Load Required Packages
# -------------------------------

# List of required packages
required_packages <- c(
  "data.table",   # Efficient data manipulation
  "countrycode",  # Country code conversion
  "fst",          # Fast serialization
  "ggplot2",      # Data visualization
  "ggrepel",      # For labeling points on a chart
  "devtools",     # Install packages from GitHub
  "magrittr"      # forward Pipe Operator
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
# 2. Extract and Sort CSV Files by Modification Time (Optional)
# -------------------------------

# Note: This section is optional and currently not used in the script.
# Uncomment if you need to inspect or utilize file modification times.

# file_info <- file.info(list.files(pattern = "\\.csv$"))
# file_info <- file_info[order(as.POSIXct(file_info$mtime)), ]
# print(file_info[, "mtime"])

# -------------------------------
# 3. User Input for Updating Data
# -------------------------------

# Function to prompt user for updating data
get_user_choice <- function(prompt_text) {
  response <- readline(prompt_text)
  response <- tolower(substring(response, 1, 1))
  return(response == "y")
}

# Prompt user for updates
update_econ <- get_user_choice("Update economic data? (y/n): ")
update_dem  <- get_user_choice("Update demographic data? (y/n): ")
update_pol  <- get_user_choice("Update political data? (y/n): ")

# -------------------------------
# 4. Reading or Updating Data
# -------------------------------

# Function to read or source data based on user input
read_or_update <- function(update_flag, data_name, file_path, script_path) {
  if (update_flag || !file.exists(file_path)) {
    message(paste0("Updating ", data_name, " data..."))
    source(script_path)
  } else {
    message(paste0("Loading existing ", data_name, " data from ", file_path, "..."))
    assign(data_name, setDT(fread(file_path)), envir = .GlobalEnv)
  }
}

# Update or load economic data
read_or_update(update_econ, "economic_data", "economic_data.csv", "economic_data_collection.R")

# Update or load political data
read_or_update(update_pol, "political_data", "political_data.csv", "political_data_collection.R")

# Update or load demographic data
read_or_update(update_dem, "demographic_data", "demographic_data.csv", "demographic_data_collection.R")

# Remove user input variables to free memory
rm(update_econ, update_dem, update_pol)

# -------------------------------
# 5. Rename Columns in Demographic Data
# -------------------------------

# Define old and new column names
old_names_dem <- c("net_migr", "net_migr_rate", "migr_working_age_pop_ratio")
new_names_dem <- c(
  "net_migr_UN_2024",
  "net_migr_rate_UN_2024",
  "migr_working_age_pop_15_lifexp_offset_15"
)

# Rename columns, skipping any that are absent
setnames(demographic_data, old = old_names_dem, new = new_names_dem, skip_absent = TRUE)

# -------------------------------
# 6. Define Columns to Keep
# -------------------------------

# Economic data columns to retain
cols_econ <- c(
  "iso3c", "year", "gdp_pc", "gdp_pc_imf", "inv_imf", 'inv_wb', "tax_havens_dummy",
  "resource_dummy", "emp_wb", "pop_wb", "pop_imf", "pop_mpd", "rent",
  "gov_spd_imf", "rgdpna", "rgdpo", "rnna", "labsh", "delta", "pop_pwt",
  "emp_pwt", "avh", "hc", "rwtfpna", "rtfpna", "ctfp", "gdp_pc_wb_nom",
  "country", "aggregate_dummy", "gdp_source", "gdp_pc_growth"
)

# Political data columns to retain
cols_polit <- c(
  "iso3c", "year", "country_name_vdem", "histname", "v2x_polyarchy",
  "v2x_libdem", "v2x_partipdem", "v2x_delibdem", "v2x_egaldem",
  "v2x_liberal", "attempted_coup", "realized_coup",
  "coup_conspiracy", "political_violence_intensity",
  "ucdp_type_of_conflict", "political_trouble_dummy"
)

# Demographic data columns to retain
cols_demog <- c(
  "iso3c", "year", "pop_total", "youth_ratio",
  "working_age_pop_15_lifexp_offset_15", "zm_working_age_pop_15_lifexp_offset_15",
  "migr_working_age_pop_15_lifexp_offset_15", "lifexp_total", "lifexp_male",
  "lifexp_female", "lifexp_total_at_65", "child_mort_under_5",
  "fertility_rate_total", "child_mort_under_5_gm",
  "fertility_rate_total_gm", "pop_age_0_14", "pop_age_15_24",
  "pop_age_25_64", "pop_age_65_plus", "pop_age_20_64",
  "pop_age_15_64", "net_migr_UN_2024", "net_migr_rate_UN_2024",
  "pop_total_zm", "pop_age_0_14_zm", "pop_age_15_24_zm",
  "pop_age_25_64_zm", "pop_age_65_plus_zm", "pop_age_20_64_zm",
  "pop_age_15_64_zm"
)

# -------------------------------
# 7. Merge the Three Data Files with Selected Columns
# -------------------------------

message("Merging datasets...")

# Select and filter economic data
econ_subset <- economic_data[, ..cols_econ][!is.na(iso3c)]

# Select and filter political data
pol_subset <- political_data[, ..cols_polit][!is.na(iso3c)]

# Select and filter demographic data
dem_subset <- demographic_data[, ..cols_demog][!is.na(iso3c)]

# Merge economic and political data
selected_data <- merge(
  econ_subset,
  pol_subset,
  by = c("iso3c", "year"),
  all = TRUE
)

# Merge demographic data into the combined dataset
selected_data <- merge(
  selected_data,
  dem_subset,
  by = c("iso3c", "year"),
  all = TRUE
)

# -------------------------------
# 8. Add Country Names
# -------------------------------

# Add 'country_name' column based on 'iso3c' codes
selected_data[, country_name := countrycode(iso3c, "iso3c", "country.name")]

# -------------------------------
# 9. Rename Working Age Population Variables
# -------------------------------

# Rename working_age_pop variables to simpler names
setnames(
  selected_data,
  old = c("working_age_pop_15_lifexp_offset_15", "zm_working_age_pop_15_lifexp_offset_15"),
  new = c("pop_working_age", "pop_working_age_zm"),
  skip_absent = TRUE
)

# -------------------------------
# 10. Calculate GDP per Population Metrics
# -------------------------------

# GDP per population aged 15-64
selected_data[, gdp_per_pop_age_15_64 := gdp_pc / (pop_age_15_64 / pop_total)]

# GDP per working-age population
selected_data[, gdp_per_pop_working_age := gdp_pc / (pop_working_age / pop_total)]

# -------------------------------
# 11. Create Lagged Growth Variables
# -------------------------------

# Function to calculate lagged growth
calculate_lagged_growth <- function(dt, var, n_start, n_end, by_var = "iso3c") {
  lagged_growth_var <- paste0(var, "_grth_", n_start, "y_", n_end, "y_lagged")
  dt[, (lagged_growth_var) := (shift(get(var), n = n_start, type = "lag") / 
                                 shift(get(var), n = n_end, type = "lag"))^(1 / (n_end - n_start)) - 1, 
     by = by_var]
}

# Calculate lagged growth for 'gdp_pc' and 'gdp_per_pop_age_15_64'
calculate_lagged_growth(selected_data, "gdp_pc", 1, 6)
calculate_lagged_growth(selected_data, "gdp_per_pop_age_15_64", 1, 6)

# -------------------------------
# 12. Save the Resulting Data File
# -------------------------------

message("Saving merged data...")

# Save to FST format for fast future access
write.fst(selected_data, "selected_data.fst")

# Save to CSV format using fwrite for speed
fwrite(selected_data, "selected_data.csv")

# -------------------------------
# 13. Clean Up and Free Memory
# -------------------------------

# Remove intermediate datasets to free memory
rm(list = c("demographic_data", "economic_data", "political_data"))
gc()

# -------------------------------
# 14. Generate Plots
# -------------------------------


 # Plot 1: Historical GDP Growth vs GDP per Capita
 # Excluding Resource Economies and Tax Havens/financial Centers
 ggplot( data = selected_data[ iso3c %in% selected_data[ pop_total >= 300 & resource_dummy == 0  & tax_havens_dummy == 0, iso3c ],
                              .( gdp_pc_growth_1999_2024 = (gdp_pc[year == 2024]/gdp_pc[year == 1999])**(1/25)-1,
                                 gdp_pc_1999 = gdp_pc[year == 1999]),
                              by = .(iso3c, country_name) ],
        aes(gdp_pc_1999,gdp_pc_growth_1999_2024, label = iso3c)) +
  geom_point() +
  geom_smooth()+
  geom_text_repel() +
  scale_y_continuous(labels = scales::percent) +
  ggtitle( "GDP per capita growth vs GDP per capita level, 1999-2024 \n(excluding financial centers and resource economies)" ) +
  ylab( "GDP per capita growth, 1999-2024 annualized" ) +
  xlab( "GDP per capita in 1999, 2021 USA dollar PPP")


 # Plot 2: Historical GDP Growth vs GDP per Capita
 # Including Resource Economies and Tax Havens/financial Centers
 ggplot( data = selected_data[ iso3c %in% selected_data[ pop_total >= 300, iso3c ],
                              .( gdp_pc_growth_1999_2024 = (gdp_pc[year == 2024]/gdp_pc[year == 1999])**(1/25)-1,
                                 gdp_pc_1999 = gdp_pc[year == 1999],
                                 resource_dummy = resource_dummy [year == 2023],
                                 tax_havens_dummy = tax_havens_dummy[year == 2023]),
                              by = .(iso3c, country_name) ][!is.na(resource_dummy)],
        aes(gdp_pc_1999,gdp_pc_growth_1999_2024, label = iso3c,
            color = as.factor( ifelse ( resource_dummy+tax_havens_dummy == 0,"ordinary countries", "resource/tax haven countries" )))) +
  geom_point() +
  geom_smooth(se = F)+
  geom_text_repel() +
  scale_y_continuous(labels = scales::percent) +
  ggtitle( "GDP per capita growth vs GDP per capita level, 1999-2024 \n(including financial centers and resource economies)" ) +
  ylab( "GDP per capita growth, 1999-2024 annualized" ) +
  xlab( "GDP per capita in 1999, 2021 USA dollar PPP")+ 
  guides(color = guide_legend(title = "resource/tax haven status"))+
    theme_minimal()
  
  # Plot 3: Attempted Coup Proportion by Year with Red Line
  # Prepare data for plotting
  plot_data_trouble <- selected_data[year %in% 1950:2023, .(
    proportion_political_trouble = sum(political_trouble_dummy, na.rm = TRUE) / 
      .N
  ), by = year]
  
  plot_data_coup <- selected_data[year %in% 1950:2023, .(
    proportion_attempted_coup = sum(attempted_coup, na.rm = TRUE) / 
      .N
  ), by = year]
  
  # Merge the two datasets for plotting
  plot_data <- merge(plot_data_trouble, plot_data_coup, by = "year", all = TRUE)
  
  # Create the plot
  ggplot(plot_data, aes(x = year)) +
    geom_line(aes(y = proportion_political_trouble, color = "Political Trouble")) +
    geom_line(aes(y = proportion_attempted_coup, color = "Attempted Coup")) +
    labs(
      title = "Proportion of Countries in Political Trouble and Experiencing Attempted Coups by Year",
      x = "Year",
      y = "Proportion",
      color = "Legend"
    ) +
    scale_color_manual(values = c("Political Trouble" = "green", "Attempted Coup" = "red")) +
    theme_minimal()


# -------------------------------
# End of Data Merge
# -------------------------------
