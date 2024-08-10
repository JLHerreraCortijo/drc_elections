rm(list=ls())

here::i_am("R/002 - models.R")

library(magrittr)

# Load data

load(here::here("results/data.RData"))

# Run model diagnostics?

run_diagnostics <- F

# Function to fix the 'index' values in the ACLED dataset
fix_ACLED_index <- function(df) {
  # Use mutate to create or modify the 'index' column
  df %>%
    # Replace specific 'index' values with new ones based on conditions
    dplyr::mutate(index =
                    # Apply case_when to conditionally change 'index' values
                    dplyr::case_when(
                      # If 'index' matches any of these Kinshasa regions, set it to "kinshasa"
                      index %in% c("kinshasa i lukunga", "kinshasa ii funa", "kinshasa iii mt amba", "kinshasa iv tshangu") ~ "kinshasa",
                      # If 'index' is "beni ville", set it to "beni"
                      index == "beni ville" ~ "beni",
                      # For all other cases, keep the original 'index' value
                      TRUE ~ index
                    )
    )
}



# Selecting relevant columns and reshaping data
# Select columns: index, label, and columns starting with "kabila.percent" or "ramazani.percent"
share <- data %>% 
  dplyr::select(index, label, starts_with("kabila.percent"), starts_with("ramazani.percent")) %>%
  # Convert data from wide to long format, keeping 'index' and 'label' constant, and storing values in 'votes_share'
  tidyr::pivot_longer(cols = -c(index, label), values_to = "votes_share") %>%
  # Separate the 'name' column into 'drop' and 'year', using '_' as a separator
  tidyr::separate(name, c("drop", "year"), sep = "_") %>%
  # Convert 'year' to integer and remove the 'drop' column
  dplyr::mutate(year = as.integer(year)) %>%
  dplyr::select(-drop)

# Processing nightlight data and calculating mean nightlight by election
# Add a small value (0.01) to 'nightlight_mean' to avoid zero values
mean_nightlight <- nightlight_gt30_mean %>% 
  dplyr::mutate(nightlight_mean = nightlight_mean + 0.01) %>%
  # Select relevant columns and rename 'index' to 'index.data'
  dplyr::select(index = index.data, year, nightlight_mean) %>%
  # Add an 'election' column based on 'year' mapping
  dplyr::mutate(election = elections[as.character(year)]) %>%
  # Group by 'index' and 'election', then calculate the mean of 'nightlight_mean' ignoring NA values
  dplyr::group_by(index, election) %>%
  dplyr::summarise(across(nightlight_mean, ~mean(., na.rm = TRUE)), .groups = "drop") %>%
  # Filter out rows where 'election' is NA
  dplyr::filter(!is.na(election))

# Calculating mean nightlight for 2012 and 2013
# Add a small value (0.01) to 'nightlight_mean' to avoid zero values
DMSP_2012_13 <- nightlight_gt30_mean %>%
  dplyr::mutate(nightlight_mean = nightlight_mean + 0.01) %>%
  # Select relevant columns and rename 'index' to 'index.data'
  dplyr::select(index = index.data, year, nightlight_mean) %>%
  # Filter for years 2012 and 2013
  dplyr::filter(year %in% c(2012, 2013)) %>%
  # Group by 'index' and calculate the mean of 'nightlight_mean' ignoring NA values
  dplyr::group_by(index) %>%
  dplyr::summarise(across(nightlight_mean, ~mean(., na.rm = TRUE)), .groups = "drop")

# Calculating percent change in nightlight between 2014 and 2018
# Add a small value (0.01) to 'nightlight_mean' to avoid zero values
VIIR_percent_change <- nightlight_gt30_mean %>%
  dplyr::mutate(nightlight_mean = nightlight_mean + 0.01) %>%
  # Select relevant columns and rename 'index' to 'index.data'
  dplyr::select(index = index.data, year, nightlight_mean) %>%
  # Filter for years 2014 and 2018
  dplyr::filter(year %in% c(2014, 2018)) %>%
  # Reshape data to wide format, creating columns for nightlight_mean in 2014 and 2018
  tidyr::pivot_wider(names_from = year, values_from = nightlight_mean) %>%
  # Calculate percent change in nightlight_mean from 2014 to 2018
  dplyr::mutate(change = (`2018` - `2014`) / (`2014`))

# Correcting nightlight data for 2018 based on percent change
VIIR_corrected <- DMSP_2012_13 %>%
  dplyr::left_join(VIIR_percent_change, by = "index") %>%
  # Apply percent change to calculate corrected nightlight_mean
  dplyr::mutate(nightlight_mean_corrected = nightlight_mean + (nightlight_mean) * change) %>%
  # Select relevant columns and set 'election' to "2018"
  dplyr::select(index, nightlight_mean = nightlight_mean_corrected) %>%
  dplyr::mutate(election = "2018")

# Combine corrected nightlight data with mean_nightlight
mean_nightlight %<>%
  dplyr::bind_rows(VIIR_corrected) %>%
  # Convert 'election' to 'year' and remove the 'election' column
  dplyr::mutate(year = as.integer(election)) %>%
  dplyr::select(-election)

# Join nightlight data with election share data
share %<>% 
  dplyr::left_join(mean_nightlight, by = c("index", "year"))


#### Table 3a ####

##### Model 1 #####

# Aggregating total conflict deaths by index and year
total_conflict_deaths <- conflict.aggregated_by_type %>%
  # Filter out rows with no conflicts
  dplyr::filter(n.conflicts > 0) %>%
  # Select relevant columns and rename 'index' to 'index.data'
  dplyr::select(index = index.data, year, n.deaths) %>%
  # Group by 'index' and 'year', then sum 'n.deaths' ignoring NA values
  dplyr::group_by(index, year) %>%
  dplyr::summarise(across(n.deaths, ~sum(., na.rm = TRUE)), .groups = "drop")

# Merging conflict data with election share data
total_conflict_deaths <- share %>%
  # Remove 'votes_share' column
  dplyr::select(-votes_share) %>%
  # Join with total_conflict_deaths data on 'index' and 'year'
  dplyr::left_join(total_conflict_deaths, by = c("index", "year")) %>%
  # Replace NA values in 'n.deaths' with 0
  dplyr::mutate(n.deaths = tidyr::replace_na(n.deaths, 0)) %>%
  # Rename 'label' to 'region' and remove 'index' column
  dplyr::rename(region = label) %>%
  dplyr::select(-index)

# Reaggregating total conflict deaths (potential duplication, double-check required)
total_conflict_deaths <- conflict.aggregated_by_type %>%
  # Select relevant columns and rename 'index' to 'index.data'
  dplyr::select(index = index.data, year, n.deaths) %>%
  # Group by 'index' and 'year', then sum 'n.deaths' ignoring NA values
  dplyr::group_by(index, year) %>%
  dplyr::summarise(across(n.deaths, ~sum(., na.rm = TRUE)), .groups = "drop")

# Preparing data for modeling
to.model <- share %>%
  # Join with total_conflict_deaths data on 'index' and 'year'
  dplyr::left_join(total_conflict_deaths, by = c("index", "year")) %>%
  # Replace NA values in 'n.deaths' with 0
  dplyr::mutate(n.deaths = tidyr::replace_na(n.deaths, 0)) %>%
  # Rename 'label' to 'region' and remove 'index' column
  dplyr::rename(region = label) %>%
  dplyr::select(-index)

# Convert to panel data frame for plm package
to.model <- plm::pdata.frame(to.model, index = c("region", "year"), drop.index = FALSE)

# Running fixed effects model with two-way effects
model_t3a_1 <- plm::plm(votes_share ~ nightlight_mean + n.deaths, data = to.model, model = "within", effect = "twoways")


##### Model 2 #####

# Calculate the log of total conflict deaths, grouped by index and year
log_total_conflict_deaths <- conflict.aggregated_by_type %>%
  # Filter out rows where there are no conflicts
  dplyr::filter(n.conflicts > 0) %>%
  # Select relevant columns and rename 'index' to 'index.data'
  dplyr::select(index = index.data, year, n.deaths) %>%
  # Group by 'index' and 'year'
  dplyr::group_by(index, year) %>%
  # Calculate the log10 of the sum of deaths, adding 0.1 to avoid log(0)
  dplyr::summarise(across(n.deaths, ~log10(sum(., na.rm = TRUE) + 0.1)), .groups = "drop") %>%
  # Rename the column to 'log_n.deaths'
  dplyr::rename(log_n.deaths = n.deaths)

# Prepare the data for modeling by merging with election data
to.model <- share %>%
  # Merge with log_total_conflict_deaths by 'index' and 'year'
  dplyr::left_join(log_total_conflict_deaths, by = c("index", "year")) %>%
  # Replace NA values in 'log_n.deaths' with 0
  dplyr::mutate(log_n.deaths = tidyr::replace_na(log_n.deaths, 0)) %>%
  # Rename 'label' to 'region' and remove 'index' column
  dplyr::rename(region = label) %>%
  dplyr::select(-index)

# Convert the data to a panel data frame for the plm package
to.model <- plm::pdata.frame(to.model, index = c("region", "year"), drop.index = FALSE)

# Run a fixed effects model with two-way effects (within model)
model_t3a_2 <- plm::plm(votes_share ~ nightlight_mean + log_n.deaths, data = to.model, model = "within", effect = "twoways")

##### Model 3 #####

# Calculate total conflict events, grouped by index and year
total_conflict_events <- conflict.aggregated_by_type %>%
  # Filter out rows where there are no conflicts
  dplyr::filter(n.conflicts > 0) %>%
  # Select relevant columns and rename 'index' to 'index.data'
  dplyr::select(index = index.data, year, n.conflicts) %>%
  # Group by 'index' and 'year'
  dplyr::group_by(index, year) %>%
  # Summarize the total number of conflicts
  dplyr::summarise(across(n.conflicts, ~sum(., na.rm = TRUE)), .groups = "drop")

# Prepare the data for modeling by merging with election data
to.model <- share %>%
  # Merge with total_conflict_events by 'index' and 'year'
  dplyr::left_join(total_conflict_events, by = c("index", "year")) %>%
  # Replace NA values in 'n.conflicts' with 0
  dplyr::mutate(n.conflicts = tidyr::replace_na(n.conflicts, 0)) %>%
  # Rename 'label' to 'region' and remove 'index' column
  dplyr::rename(region = label) %>%
  dplyr::select(-index)

# Convert the data to a panel data frame for the plm package
to.model <- plm::pdata.frame(to.model, index = c("region", "year"), drop.index = FALSE)

# Run a fixed effects model with two-way effects (within model)
model_t3a_3 <- plm::plm(votes_share ~ nightlight_mean + n.conflicts, data = to.model, model = "within", effect = "twoways")

##### Model 4 #####

# Calculate the log of total conflict events, grouped by index and year
log_total_conflict_events <- conflict.aggregated_by_type %>%
  # Filter out rows where there are no conflicts
  dplyr::filter(n.conflicts > 0) %>%
  # Select relevant columns and rename 'index' to 'index.data'
  dplyr::select(index = index.data, year, n.conflicts) %>%
  # Group by 'index' and 'year'
  dplyr::group_by(index, year) %>%
  # Calculate the log10 of the sum of conflicts, adding 0.1 to avoid log(0)
  dplyr::summarise(across(n.conflicts, ~log10(sum(., na.rm = TRUE) + 0.1)), .groups = "drop") %>%
  # Rename the column to 'log_n.conflicts'
  dplyr::rename(log_n.conflicts = n.conflicts)

# Prepare the data for modeling by merging with election data
to.model <- share %>%
  # Merge with log_total_conflict_events by 'index' and 'year'
  dplyr::left_join(log_total_conflict_events, by = c("index", "year")) %>%
  # Replace NA values in 'log_n.conflicts' with 0
  dplyr::mutate(log_n.conflicts = tidyr::replace_na(log_n.conflicts, 0)) %>%
  # Rename 'label' to 'region' and remove 'index' column
  dplyr::rename(region = label) %>%
  dplyr::select(-index)

# Convert the data to a panel data frame for the plm package
to.model <- plm::pdata.frame(to.model, index = c("region", "year"), drop.index = FALSE)

# Run a fixed effects model with two-way effects (within model)
model_t3a_4 <- plm::plm(votes_share ~ nightlight_mean + log_n.conflicts, data = to.model, model = "within", effect = "twoways")

##### Model 5 #####

# Calculate total conflict deaths, grouped by index and year
total_conflict_deaths <- conflict.aggregated_by_type %>%
  # Filter out rows where there are no conflicts
  dplyr::filter(n.conflicts > 0) %>%
  # Select relevant columns and rename 'index' to 'index.data'
  dplyr::select(index = index.data, year, n.deaths) %>%
  # Group by 'index' and 'year'
  dplyr::group_by(index, year) %>%
  # Summarize the total number of deaths
  dplyr::summarise(across(n.deaths, ~sum(., na.rm = TRUE)), .groups = "drop")

# Re-merge and reprocess the data (potential duplication, double-check required)
total_conflict_deaths <- share %>%
  # Remove 'votes_share' column
  dplyr::select(-votes_share) %>%
  # Merge with total_conflict_deaths by 'index' and 'year'
  dplyr::left_join(total_conflict_deaths, by = c("index", "year")) %>%
  # Replace NA values in 'n.deaths' with 0
  dplyr::mutate(n.deaths = tidyr::replace_na(n.deaths, 0)) %>%
  # Rename 'label' to 'region' and remove 'index' column
  dplyr::rename(region = label) %>%
  dplyr::select(-index)

# Recalculate total conflict deaths (this block seems redundant)
total_conflict_deaths <- conflict.aggregated_by_type %>%
  # Filter out rows where there are no conflicts
  dplyr::filter(n.conflicts > 0) %>%
  # Select relevant columns and rename 'index' to 'index.data'
  dplyr::select(index = index.data, year, n.deaths) %>%
  # Group by 'index' and 'year'
  dplyr::group_by(index, year) %>%
  # Summarize the total number of deaths
  dplyr::summarise(across(n.deaths, ~sum(., na.rm = TRUE)), .groups = "drop")

# Prepare the data for modeling by merging with election data
to.model <- share %>%
  # Merge with total_conflict_deaths by 'index' and 'year'
  dplyr::left_join(total_conflict_deaths, by = c("index", "year")) %>%
  # Replace NA values in 'n.deaths' with 0
  dplyr::mutate(n.deaths = tidyr::replace_na(n.deaths, 0)) %>%
  # Rename 'label' to 'region' and remove 'index' column
  dplyr::rename(region = label) %>%
  dplyr::select(-index)

# Convert the data to a panel data frame for the plm package
to.model <- plm::pdata.frame(to.model, index = c("region", "year"), drop.index = FALSE)

# Run a fixed effects model with two-way effects (within model)
model_t3a_5 <- plm::plm(votes_share ~ n.deaths, data = to.model, model = "within", effect = "twoways")

##### Model 6 #####

# Calculate the log of total conflict deaths, grouped by index and year
log_total_conflict_deaths <- conflict.aggregated_by_type %>%
  # Filter out rows where there are no conflicts
  dplyr::filter(n.conflicts > 0) %>%
  # Select relevant columns and rename 'index' to 'index.data'
  dplyr::select(index = index.data, year, n.deaths) %>%
  # Group by 'index' and 'year'
  dplyr::group_by(index, year) %>%
  # Calculate the log10 of the sum of deaths, adding 0.1 to avoid log(0)
  dplyr::summarise(across(n.deaths, ~log10(sum(., na.rm = TRUE) + 0.1)), .groups = "drop") %>%
  # Rename the column to 'log_n.deaths'
  dplyr::rename(log_n.deaths = n.deaths)

# Prepare the data for modeling by merging with election data
to.model <- share %>%
  # Merge with log_total_conflict_deaths by 'index' and 'year'
  dplyr::left_join(log_total_conflict_deaths, by = c("index", "year")) %>%
  # Replace NA values in 'log_n.deaths' with 0
  dplyr::mutate(log_n.deaths = tidyr::replace_na(log_n.deaths, 0)) %>%
  # Rename 'label' to 'region' and remove 'index' column
  dplyr::rename(region = label) %>%
  dplyr::select(-index)

# Convert the data to a panel data frame for the plm package
to.model <- plm::pdata.frame(to.model, index = c("region", "year"), drop.index = FALSE)

# Run a fixed effects model with two-way effects (within model)
model_t3a_6 <- plm::plm(votes_share ~ log_n.deaths, data = to.model, model = "within", effect = "twoways")

##### Model 7 #####

# Calculate total conflict events, grouped by index and year
total_conflict_events <- conflict.aggregated_by_type %>%
  # Filter out rows where there are no conflicts
  dplyr::filter(n.conflicts > 0) %>%
  # Select relevant columns and rename 'index' to 'index.data'
  dplyr::select(index = index.data, year, n.conflicts) %>%
  # Group by 'index' and 'year'
  dplyr::group_by(index, year) %>%
  # Summarize the total number of conflicts
  dplyr::summarise(across(n.conflicts, ~sum(., na.rm = TRUE)), .groups = "drop")

# Prepare the data for modeling by merging with election data
to.model <- share %>%
  # Merge with total_conflict_events by 'index' and 'year'
  dplyr::left_join(total_conflict_events, by = c("index", "year")) %>%
  # Replace NA values in 'n.conflicts' with 0
  dplyr::mutate(n.conflicts = tidyr::replace_na(n.conflicts, 0)) %>%
  # Rename 'label' to 'region' and remove 'index' column
  dplyr::rename(region = label) %>%
  dplyr::select(-index)

# Convert the data to a panel data frame for the plm package
to.model <- plm::pdata.frame(to.model, index = c("region", "year"), drop.index = FALSE)

# Run a fixed effects model with two-way effects (within model)
model_t3a_7 <- plm::plm(votes_share ~ n.conflicts, data = to.model, model = "within", effect = "twoways")

##### Model 8 #####

# Calculate the log of total conflict events, grouped by index and year
log_total_conflict_events <- conflict.aggregated_by_type %>%
  # Filter out rows where there are no conflicts
  dplyr::filter(n.conflicts > 0) %>%
  # Select relevant columns and rename 'index' to 'index.data'
  dplyr::select(index = index.data, year, n.conflicts) %>%
  # Group by 'index' and 'year'
  dplyr::group_by(index, year) %>%
  # Calculate the log10 of the sum of conflicts, adding 0.1 to avoid log(0)
  dplyr::summarise(across(n.conflicts, ~log10(sum(., na.rm = TRUE) + 0.1)), .groups = "drop") %>%
  # Rename the column to 'log_n.conflicts'
  dplyr::rename(log_n.conflicts = n.conflicts)

# Prepare the data for modeling by merging with election data
to.model <- share %>%
  # Merge with log_total_conflict_events by 'index' and 'year'
  dplyr::left_join(log_total_conflict_events, by = c("index", "year")) %>%
  # Replace NA values in 'log_n.conflicts' with 0
  dplyr::mutate(log_n.conflicts = tidyr::replace_na(log_n.conflicts, 0)) %>%
  # Rename 'label' to 'region' and remove 'index' column
  dplyr::rename(region = label) %>%
  dplyr::select(-index)

# Convert the data to a panel data frame for the plm package
to.model <- plm::pdata.frame(to.model, index = c("region", "year"), drop.index = FALSE)

# Run a fixed effects model with two-way effects (within model)
model_t3a_8 <- plm::plm(votes_share ~ log_n.conflicts, data = to.model, model = "within", effect = "twoways")

##### Model 9 #####

# Calculate total conflict deaths from the ACLED data, grouped by index and year
total_conflict_deaths <- ACLED_data_models %>%
  # Filter out rows where there are no conflicts
  dplyr::filter(n.conflicts > 0) %>%
  # Select relevant columns
  dplyr::select(index, year, n.deaths) %>%
  # Group by 'index' and 'year'
  dplyr::group_by(index, year) %>%
  # Summarize the total number of deaths
  dplyr::summarise(across(n.deaths, ~sum(., na.rm = TRUE)), .groups = "drop")

# Prepare the data for modeling by fixing the index and merging with election data
to.model <- share %>%
  # Apply a custom function to fix the ACLED index
  fix_ACLED_index() %>%
  # Merge with total_conflict_deaths by 'index' and 'year'
  dplyr::left_join(total_conflict_deaths, by = c("index", "year")) %>%
  # Replace NA values in 'n.deaths' with 0
  dplyr::mutate(n.deaths = tidyr::replace_na(n.deaths, 0)) %>%
  # Rename 'label' to 'region' and remove 'index' column
  dplyr::rename(region = label) %>%
  dplyr::select(-index)

# Convert the data to a panel data frame for the plm package
to.model <- plm::pdata.frame(to.model, index = c("region", "year"), drop.index = FALSE)

# Run a fixed effects model with two-way effects (within model)
model_t3a_9 <- plm::plm(votes_share ~ nightlight_mean + n.deaths, data = to.model, model = "within", effect = "twoways")

##### Model 10 #####

# Calculate the log of total conflict deaths from the ACLED data, grouped by index and year
log_total_conflict_deaths <- ACLED_data_models %>%
  # Filter out rows where there are no conflicts
  dplyr::filter(n.conflicts > 0) %>%
  # Select relevant columns
  dplyr::select(index, year, n.deaths) %>%
  # Group by 'index' and 'year'
  dplyr::group_by(index, year) %>%
  # Calculate the log10 of the sum of deaths, adding 0.1 to avoid log(0)
  dplyr::summarise(across(n.deaths, ~log10(sum(., na.rm = TRUE) + 0.1)), .groups = "drop") %>%
  # Rename the column to 'log_n.deaths'
  dplyr::rename(log_n.deaths = n.deaths)

# Prepare the data for modeling by fixing the index and merging with election data
to.model <- share %>%
  # Apply a custom function to fix the ACLED index
  fix_ACLED_index() %>%
  # Merge with log_total_conflict_deaths by 'index' and 'year'
  dplyr::left_join(log_total_conflict_deaths, by = c("index", "year")) %>%
  # Replace NA values in 'log_n.deaths' with 0
  dplyr::mutate(log_n.deaths = tidyr::replace_na(log_n.deaths, 0)) %>%
  # Rename 'label' to 'region' and remove 'index' column
  dplyr::rename(region = label) %>%
  dplyr::select(-index)

# Convert the data to a panel data frame for the plm package
to.model <- plm::pdata.frame(to.model, index = c("region", "year"), drop.index = FALSE)

# Run a fixed effects model with two-way effects (within model)
model_t3a_10 <- plm::plm(votes_share ~ nightlight_mean + log_n.deaths, data = to.model, model = "within", effect = "twoways")

##### Model 11 #####

# Calculate total conflict events from the ACLED data, grouped by index and year
total_conflict_events <- ACLED_data_models %>%
  # Filter out rows where there are no conflicts
  dplyr::filter(n.conflicts > 0) %>%
  # Select relevant columns
  dplyr::select(index, year, n.conflicts) %>%
  # Group by 'index' and 'year'
  dplyr::group_by(index, year) %>%
  # Summarize the total number of conflicts
  dplyr::summarise(across(n.conflicts, ~sum(., na.rm = TRUE)), .groups = "drop")

# Prepare the data for modeling by fixing the index and merging with election data
to.model <- share %>%
  # Apply a custom function to fix the ACLED index
  fix_ACLED_index() %>%
  # Merge with total_conflict_events by 'index' and 'year'
  dplyr::left_join(total_conflict_events, by = c("index", "year")) %>%
  # Replace NA values in 'n.conflicts' with 0
  dplyr::mutate(n.conflicts = tidyr::replace_na(n.conflicts, 0)) %>%
  # Rename 'label' to 'region' and remove 'index' column
  dplyr::rename(region = label) %>%
  dplyr::select(-index)

# Convert the data to a panel data frame for the plm package
to.model <- plm::pdata.frame(to.model, index = c("region", "year"), drop.index = FALSE)

# Run a fixed effects model with two-way effects (within model)
model_t3a_11 <- plm::plm(votes_share ~ nightlight_mean + n.conflicts, data = to.model, model = "within", effect = "twoways")

##### Model 12 #####

# Calculate the log of total conflict events from the ACLED data, grouped by index and year
log_total_conflict_events <- ACLED_data_models %>%
  # Filter out rows where there are no conflicts
  dplyr::filter(n.conflicts > 0) %>%
  # Select relevant columns
  dplyr::select(index, year, n.conflicts) %>%
  # Group by 'index' and 'year'
  dplyr::group_by(index, year) %>%
  # Calculate the log10 of the sum of conflicts, adding 0.1 to avoid log(0)
  dplyr::summarise(across(n.conflicts, ~log10(sum(., na.rm = TRUE) + 0.1)), .groups = "drop") %>%
  # Rename the column to 'log_n.conflicts'
  dplyr::rename(log_n.conflicts = n.conflicts)

# Prepare the data for modeling by fixing the index and merging with election data
to.model <- share %>%
  # Apply a custom function to fix the ACLED index
  fix_ACLED_index() %>%
  # Merge with log_total_conflict_events by 'index' and 'year'
  dplyr::left_join(log_total_conflict_events, by = c("index", "year")) %>%
  # Replace NA values in 'log_n.conflicts' with 0
  dplyr::mutate(log_n.conflicts = tidyr::replace_na(log_n.conflicts, 0)) %>%
  # Rename 'label' to 'region' and remove 'index' column
  dplyr::rename(region = label) %>%
  dplyr::select(-index)

# Convert the data to a panel data frame for the plm package
to.model <- plm::pdata.frame(to.model, index = c("region", "year"), drop.index = FALSE)

# Run a fixed effects model with two-way effects (within model)
model_t3a_12 <- plm::plm(votes_share ~ nightlight_mean + log_n.conflicts, data = to.model, model = "within", effect = "twoways")

##### Model 13 #####

# Calculate total conflict deaths from the ACLED data, grouped by index and year
total_conflict_deaths <- ACLED_data_models %>%
  # Filter out rows where there are no conflicts
  dplyr::filter(n.conflicts > 0) %>%
  # Select relevant columns
  dplyr::select(index, year, n.deaths) %>%
  # Group by 'index' and 'year'
  dplyr::group_by(index, year) %>%
  # Summarize the total number of deaths
  dplyr::summarise(across(n.deaths, ~sum(., na.rm = TRUE)), .groups = "drop")

# The next two blocks of code are commented out as they seem redundant

# total_conflict_deaths <- share %>%
#   dplyr::select(-votes_share) %>%
#   dplyr::left_join(total_conflict_deaths, by = c("index", "year")) %>%
#   dplyr::mutate(n.deaths = tidyr::replace_na(n.deaths, 0)) %>%
#   dplyr::rename(region = label) %>%
#   dplyr::select(-index)

# total_conflict_deaths <- conflict.aggregated_by_type %>%
#   dplyr::filter(n.conflicts > 0) %>%
#   dplyr::select(index = index.data, year, n.deaths) %>%
#   dplyr::group_by(index, year) %>%
#   dplyr::summarise(across(n.deaths, ~sum(., na.rm = TRUE)), .groups = "drop")

# Prepare the data for modeling by fixing the index and merging with election data
to.model <- share %>%
  # Apply a custom function to fix the ACLED index
  fix_ACLED_index() %>%
  # Merge with total_conflict_deaths by 'index' and 'year'
  dplyr::left_join(total_conflict_deaths, by = c("index", "year")) %>%
  # Replace NA values in 'n.deaths' with 0
  dplyr::mutate(n.deaths = tidyr::replace_na(n.deaths, 0)) %>%
  # Rename 'label' to 'region' and remove 'index' column
  dplyr::rename(region = label) %>%
  dplyr::select(-index)

# Convert the data to a panel data frame for the plm package
to.model <- plm::pdata.frame(to.model, index = c("region", "year"), drop.index = FALSE)

# Run a fixed effects model with two-way effects (within model)
model_t3a_13 <- plm::plm(votes_share ~ n.deaths, data = to.model, model = "within", effect = "twoways")

##### Model 14 #####

# Calculate the log of total conflict deaths from the ACLED data, grouped by index and year
log_total_conflict_deaths <- ACLED_data_models %>%
  # Filter out rows where there are no conflicts
  dplyr::filter(n.conflicts > 0) %>%
  # Select relevant columns
  dplyr::select(index, year, n.deaths) %>%
  # Group by 'index' and 'year'
  dplyr::group_by(index, year) %>%
  # Calculate the log10 of the sum of deaths, adding 0.1 to avoid log(0)
  dplyr::summarise(across(n.deaths, ~log10(sum(., na.rm = TRUE) + 0.1)), .groups = "drop") %>%
  # Rename the column to 'log_n.deaths'
  dplyr::rename(log_n.deaths = n.deaths)

# Prepare the data for modeling by fixing the index and merging with election data
to.model <- share %>%
  # Apply a custom function to fix the ACLED index
  fix_ACLED_index() %>%
  # Merge with log_total_conflict_deaths by 'index' and 'year'
  dplyr::left_join(log_total_conflict_deaths, by = c("index", "year")) %>%
  # Replace NA values in 'log_n.deaths' with 0
  dplyr::mutate(log_n.deaths = tidyr::replace_na(log_n.deaths, 0)) %>%
  # Rename 'label' to 'region' and remove 'index' column
  dplyr::rename(region = label) %>%
  dplyr::select(-index)

# Convert the data to a panel data frame for the plm package
to.model <- plm::pdata.frame(to.model, index = c("region", "year"), drop.index = FALSE)

# Run a fixed effects model with two-way effects (within model)
model_t3a_14 <- plm::plm(votes_share ~ log_n.deaths, data = to.model, model = "within", effect = "twoways")

##### Model 15 #####

# Calculate total conflict events from the ACLED data, grouped by index and year
total_conflict_events <- ACLED_data_models %>%
  # Filter out rows where there are no conflicts
  dplyr::filter(n.conflicts > 0) %>%
  # Select relevant columns
  dplyr::select(index, year, n.conflicts) %>%
  # Group by 'index' and 'year'
  dplyr::group_by(index, year) %>%
  # Summarize the total number of conflicts
  dplyr::summarise(across(n.conflicts, ~sum(., na.rm = TRUE)), .groups = "drop")

# Prepare the data for modeling by fixing the index and merging with election data
to.model <- share %>%
  # Apply a custom function to fix the ACLED index
  fix_ACLED_index() %>%
  # Merge with total_conflict_events by 'index' and 'year'
  dplyr::left_join(total_conflict_events, by = c("index", "year")) %>%
  # Replace NA values in 'n.conflicts' with 0
  dplyr::mutate(n.conflicts = tidyr::replace_na(n.conflicts, 0)) %>%
  # Rename 'label' to 'region' and remove 'index' column
  dplyr::rename(region = label) %>%
  dplyr::select(-index)

# Convert the data to a panel data frame for the plm package
to.model <- plm::pdata.frame(to.model, index = c("region", "year"), drop.index = FALSE)

# Run a fixed effects model with two-way effects (within model)
model_t3a_15 <- plm::plm(votes_share ~ n.conflicts, data = to.model, model = "within", effect = "twoways")

##### Model 16 #####

# Calculate the log of total conflict events from the ACLED data, grouped by index and year
log_total_conflict_events <- ACLED_data_models %>%
  # Filter out rows where there are no conflicts
  dplyr::filter(n.conflicts > 0) %>%
  # Select relevant columns
  dplyr::select(index, year, n.conflicts) %>%
  # Group by 'index' and 'year'
  dplyr::group_by(index, year) %>%
  # Calculate the log10 of the sum of conflicts, adding 0.1 to avoid log(0)
  dplyr::summarise(across(n.conflicts, ~log10(sum(., na.rm = TRUE) + 0.1)), .groups = "drop") %>%
  # Rename the column to 'log_n.conflicts'
  dplyr::rename(log_n.conflicts = n.conflicts)

# Prepare the data for modeling by fixing the index and merging with election data
to.model <- share %>%
  # Merge with log_total_conflict_events by 'index' and 'year'
  dplyr::left_join(log_total_conflict_events, by = c("index", "year")) %>%
  # Replace NA values in 'log_n.conflicts' with 0
  dplyr::mutate(log_n.conflicts = tidyr::replace_na(log_n.conflicts, 0)) %>%
  # Rename 'label' to 'region' and remove 'index' column
  dplyr::rename(region = label) %>%
  dplyr::select(-index)

# Convert the data to a panel data frame for the plm package
to.model <- plm::pdata.frame(to.model, index = c("region", "year"), drop.index = FALSE)

# Run a fixed effects model with two-way effects (within model)
model_t3a_16 <- plm::plm(votes_share ~ log_n.conflicts, data = to.model, model = "within", effect = "twoways")

##### Save the models #####

models_computed <- ls(pattern = "model_t3a_")

save(list=models_computed, file = here::here("results/Table3a_models.RData"))

rm(list = models_computed)

#### Table 3b ####
##### Model 1 #####

# Create a dataset of conflict deaths by type, with each type of conflict in a separate column
conflict_deaths_by_type <- conflict.aggregated_by_type %>%
  # Filter out rows where there are no conflicts
  dplyr::filter(n.conflicts > 0) %>%
  # Select relevant columns and rename 'index' to 'index.data'
  dplyr::select(index = index.data, year, type, n.deaths) %>%
  # Modify the 'type' column to include '_deaths' suffix
  dplyr::mutate(type = paste0(type, "_deaths")) %>%
  # Pivot the data to a wider format, with each conflict type as a separate column
  tidyr::pivot_wider(names_from = type, values_from = n.deaths)

# Prepare the data for modeling by merging it with the election data
to.model <- share %>%
  # Left join with conflict deaths data by 'index' and 'year'
  dplyr::left_join(conflict_deaths_by_type, by = c("index", "year")) %>%
  # Replace NA values in all columns except 'index', 'year', 'votes_share', and 'label' with 0
  dplyr::mutate(across(-c(index, year, votes_share, label), ~tidyr::replace_na(., 0))) %>%
  # Rename 'label' to 'region' and remove the 'index' column
  dplyr::rename(region = label) %>%
  dplyr::select(-index)

# Convert the data to a panel data frame for the plm package
to.model <- plm::pdata.frame(to.model, index = c("region", "year"), drop.index = FALSE)

# Run a fixed effects model with two-way effects (within model)
model_t3b_1 <- plm::plm(
  votes_share ~ Non.state.vs.non.state_deaths + Foreign.vs.non.state_deaths +
    Non.state.vs.civilians_deaths + DRC.vs.non.state_deaths +
    DRC.vs.civilians_deaths + Foreign.vs.civilians_deaths,
  data = to.model, model = "within", effect = "twoways"
)

##### Model 2 #####

# Create a dataset of conflict events by type, with each type of conflict in a separate column
conflict_events_by_type <- conflict.aggregated_by_type %>%
  # Filter out rows where there are no conflicts
  dplyr::filter(n.conflicts > 0) %>%
  # Select relevant columns and rename 'index' to 'index.data'
  dplyr::select(index = index.data, year, type, n.conflicts) %>%
  # Modify the 'type' column to include '_events' suffix
  dplyr::mutate(type = paste0(type, "_events")) %>%
  # Pivot the data to a wider format, with each conflict type as a separate column
  tidyr::pivot_wider(names_from = type, values_from = n.conflicts)

# Prepare the data for modeling by merging it with the election data
to.model <- share %>%
  # Left join with conflict events data by 'index' and 'year'
  dplyr::left_join(conflict_events_by_type, by = c("index", "year")) %>%
  # Replace NA values in all columns except 'index', 'year', 'votes_share', and 'label' with 0
  dplyr::mutate(across(-c(index, year, votes_share, label), ~tidyr::replace_na(., 0))) %>%
  # Rename 'label' to 'region' and remove the 'index' column
  dplyr::rename(region = label) %>%
  dplyr::select(-index)

# Convert the data to a panel data frame for the plm package
to.model <- plm::pdata.frame(to.model, index = c("region", "year"), drop.index = FALSE)

# Run a fixed effects model with two-way effects (within model)
model_t3b_2 <- plm::plm(
  votes_share ~ Non.state.vs.non.state_events + Foreign.vs.non.state_events +
    Non.state.vs.civilians_events + DRC.vs.non.state_events +
    DRC.vs.civilians_events + Foreign.vs.civilians_events,
  data = to.model, model = "within", effect = "twoways"
)

##### Model 3 #####

# Define a helper function to summarize a variable from a nested dataframe
.summarise_var <- function(.x, .var) {
  if (!is.null(.x)) {
    # Convert to data frame and summarize the specified variable, returning the sum
    .x %>%
      as.data.frame() %>%
      dplyr::summarise(across(dplyr::one_of(.var), ~sum(., na.rm = TRUE))) %>%
      dplyr::pull(.var)
  }
}

# Create a dataset of conflict deaths by casualty type, with detailed breakdowns
conflict_deaths_by_casualty_type <- conflict.aggregated_by_type %>%
  # Filter out rows where there are no conflicts
  dplyr::filter(n.conflicts > 0) %>%
  # Create new columns by applying the summarization function to nested data
  dplyr::mutate(
    n.deaths_a = purrr::map(conflict.data, ~.summarise_var(.x, "deaths_a")),
    n.deaths_b = purrr::map(conflict.data, ~.summarise_var(.x, "deaths_b")),
    n.deaths_civilians = purrr::map(conflict.data, ~.summarise_var(.x, "deaths_civilians")),
    n.deaths_unknow = purrr::map(conflict.data, ~.summarise_var(.x, "deaths_unknown"))
  ) %>%
  # Unnest the newly created columns to bring them to the top level
  tidyr::unnest(c(dplyr::starts_with("n.deaths")))

# Calculate additional casualty types based on existing data
conflict_deaths_by_casualty_type %<>%
  dplyr::mutate(
    n.deaths_DRC_milit = dplyr::case_when(stringr::str_detect(type, "DRC") ~ n.deaths_a, TRUE ~ 0),
    n.deaths_foreign_milit = dplyr::case_when(stringr::str_detect(type, "Foreign") ~ n.deaths_a, TRUE ~ 0),
    n.deaths_non_state = dplyr::case_when(
      type == "Non-state vs non-state" ~ n.deaths_a + n.deaths_b,
      type == "Non-state vs civilians" ~ n.deaths_a,
      stringr::str_ends(type, "non-state") ~ n.deaths_b,
      TRUE ~ 0
    )
  )

# Select and rename relevant columns for further analysis
conflict_deaths_by_casualty_type %<>%
  dplyr::select(
    index = index.data, year, n.deaths, n.deaths_DRC_milit,
    n.deaths_foreign_milit, n.deaths_non_state, n.deaths_civilians, n.deaths_unknow
  )

# Group by 'index' and 'year', and summarize all columns
conflict_deaths_by_casualty_type %<>%
  dplyr::group_by(index, year) %>%
  dplyr::summarise(across(everything(), ~sum(., na.rm = FALSE)), .groups = "drop")

# Prepare the data for modeling by merging it with the election data
to.model <- share %>%
  # Left join with conflict deaths by casualty type data by 'index' and 'year'
  dplyr::left_join(conflict_deaths_by_casualty_type, by = c("index", "year")) %>%
  # Replace NA values in all columns except 'index', 'year', 'votes_share', and 'label' with 0
  dplyr::mutate(across(-c(index, year, votes_share, label), ~tidyr::replace_na(., 0))) %>%
  # Rename 'label' to 'region' and remove the 'index' column
  dplyr::rename(region = label) %>%
  dplyr::select(-index)

# Convert the data to a panel data frame for the plm package
to.model <- plm::pdata.frame(to.model, index = c("region", "year"), drop.index = FALSE)

# Run a fixed effects model with two-way effects (within model)
model_t3b_3 <- plm::plm(
  votes_share ~ n.deaths_DRC_milit + n.deaths_foreign_milit +
    n.deaths_non_state + n.deaths_civilians + n.deaths_unknow,
  data = to.model, model = "within", effect = "twoways"
)

##### Save the models #####

models_computed <- ls(pattern = "model_t3b_")

save(list=models_computed, file = here::here("results/Table3b_models.RData"))

rm(list = models_computed)


#### Table A2 ####

# # Select relevant columns from the data frame for Kabila and Ramazani's vote share
# share <- data %>%
#   dplyr::select(index, label, starts_with("kabila.percent"), starts_with("ramazani.percent")) %>%
#   
#   # Reshape the data from wide to long format, converting the vote percentages into a single 'votes_share' column
#   tidyr::pivot_longer(cols = -c(index, label), values_to = "votes_share") %>%
#   
#   # Separate the 'name' column into 'drop' and 'year' columns based on the underscore separator
#   tidyr::separate(name, c("drop", "year"), sep = "_") %>%
#   
#   # Convert the 'year' column to an integer type
#   dplyr::mutate(year = as.integer(year)) %>%
#   
#   # Drop the 'drop' column as it's no longer needed
#   dplyr::select(-drop)

# Define a vector of variable names related to conflicts and deaths
vars <- c("n.conflicts", "log_n.conflicts", "n.deaths", "log_n.deaths")

# Extract unique years from the conflict data, discard NA values, and sort the years
periods <- conflict.aggregated_by_type %>%
  dplyr::pull("year") %>%
  unique() %>%
  purrr::discard(is.na) %>%
  sort()

# Map over the list of variables to create models for each period
models_tA2 <- vars %>%
  purrr::map(~ purrr::map(periods, function(period, var) {
    
    # Filter the conflict data to include only records with more than 0 conflicts and for the specific year
    to.model <- conflict.aggregated_by_type %>%
      dplyr::filter(n.conflicts > 0) %>%
      dplyr::filter(year == period) %>%
      
      # Group by index and year, summarizing conflict and death counts, and compute log-transformed variables
      dplyr::group_by(index.data, year) %>%
      dplyr::summarise(dplyr::across(c(n.conflicts, n.deaths), sum, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(
        log_n.conflicts = log10(n.conflicts + 0.1),
        log_n.deaths = log10(n.deaths + 0.1)
      ) %>%
      
      # Select the relevant columns, renaming 'index.data' to 'index', and adjust the 'year' column to include the variable name
      dplyr::select(index = index.data, year, dplyr::one_of(var)) %>%
      dplyr::mutate(year = paste(var, year, sep = "_"))
    
    # Join the reshaped vote share data with the conflict data on the 'index' column
    to.model <- share %>%
      dplyr::filter(year == period) %>%
      dplyr::left_join(to.model, by = c("index")) %>%
      
      # Replace NA values in the selected variables with 0 and rename 'label' to 'region'
      dplyr::mutate(dplyr::across(dplyr::one_of(var), ~tidyr::replace_na(., 0))) %>%
      dplyr::rename(region = label) %>%
      
      # Drop the 'index' column as it's no longer needed
      dplyr::select(-index)
    
    # Create a linear model formula based on the variable
    formula <- as.formula(paste0("votes_share ~ ", var))
    
    # Fit the linear model using the formula and the prepared data
    lm(formula, to.model)
    
  }, var = .x)) %>%
  
  # Flatten the nested list of models into a single list
  purrr::flatten()


# Diagnostics section to run additional diagnostics on the models if specified
if (run_diagnostics) {
  
  # Create the directory for diagnostics files if it doesn't already exist
  if (!dir.exists("manuscript/diagnostics/Table_A2/")) {
    dir.create("manuscript/diagnostics/Table_A2/", recursive = TRUE)
  }
  
  # Iterate over each model and corresponding name to render diagnostics as HTML files
  models_tA2 %>% purrr::walk2(model_names, ~ {
    model <- .x
    
    # Render the diagnostics report for the model using RMarkdown
    try(rmarkdown::render("R/lm diagnostics.Rmd", output_file = paste0("manuscript/diagnostics/Table_A2/", .y, ".html")))
  })
}

##### Save the models #####

models_computed <- "models_tA2"

save(periods,list=models_computed, file = here::here("results/TableA2_models.RData"))

rm(list = models_computed)
