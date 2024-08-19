rm(list=ls())

###############################################################################
# Name: 002 - models.R
# Author: John Quattrochi (john.quattrochi@gmail.com)
# Assistant: Juan Luis Herrera Cortijo (juan.luis.herrera.cortijo@gmail.com)
# Purpose: Pre-compute analysis models
# Script UUID: 5b7a4fde-e699-5cce-86d8-b4cc4df12f3a
###############################################################################

here::i_am("R/002 - models.R", uuid = uuid::UUIDfromName("64d742a5-bb7a-0164-0192-f03d7f475bed", "002 - models.R"))

library(magrittr)

source(here::here("R/000 - utils.R"))

#### Settings ####

# Run model diagnostics?

run_diagnostics <- F


#### Load data ####

load(here::here("results/data.RData"))

#### Functions ####

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


run_lm_list_diagnostics <- function(models_list, table_name, model_name_prefix = "model_") {
  
  # Create the path for saving the diagnostic files for this table
  table_diagnostics_path <- glue::glue("results/diagnostics/{table_name}/")
  
  # Check if the diagnostics directory exists, and if not, create it
  if (!dir.exists(table_diagnostics_path)) {
    dir.create(table_diagnostics_path, recursive = TRUE)
  }
  
  # Iterate over each model in the list along with its index
  models_list %>%
    purrr::walk2(seq_along(.), \(model, i) {
      
      # Render the diagnostics report for the current model using RMarkdown
      try(
        rmarkdown::render(
          "R/lm diagnostics.Rmd",
          output_file = glue::glue(
            "{table_diagnostics_path}{model_name_prefix}{sprintf('%02d', i)}.html"
          )
        )
      )
    })
}



#### Compute data shared by multiple models ####


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
  dplyr::summarise(across(nightlight_mean, \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
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
  dplyr::summarise(across(nightlight_mean, \(x) mean(x, na.rm = TRUE)), .groups = "drop")

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


share_viles_merged <- data_villes_merged %>%
  # Select relevant columns, including vote shares for Kabila and Ramazani
  dplyr::select(index, label, starts_with("kabila.percent"), starts_with("ramazani.percent")) %>%
  # Convert data from wide to long format, keeping 'index' and 'label' constant, and storing values in 'votes_share'
  tidyr::pivot_longer(cols = -c(index, label), values_to = "votes_share") %>%
  # Separate the 'name' column into 'drop' and 'year', using '_' as a separator
  tidyr::separate(name, c("drop", "year"), sep = "_") %>%
  # Convert 'year' to integer and remove the 'drop' column
  dplyr::mutate(year = as.integer(year)) %>%
  dplyr::select(-drop)


# Reshape the 'share' data from long to wide format, creating separate columns for each election year
share_wide  <- share %>% dplyr::select(index, label, year, votes_share) %>%
  tidyr::pivot_wider(
    names_from = "year",  # Use 'year' values as the new column names
    values_from = "votes_share",  # The values for the new columns come from 'votes_share'
    names_prefix = "election_"  # Add a prefix to the new column names
  )


# Filter out the year 2018 from the share dataset and reshape the data from long to wide format
share_not_2018 <- share %>% dplyr::select(index, label, year, votes_share) %>% 
  dplyr::filter(year != 2018) %>%  # Exclude data from the year 2018
  tidyr::pivot_wider(
    names_from = "year",  # Use 'year' values as the new column names
    values_from = "votes_share",  # The values for the new columns come from 'votes_share'
    names_prefix = "election_"  # Add a prefix to the new column names
  )

# Filter the 'share' dataset for the year 2006, then reshape it from long to wide format
share_2006 <- share %>% dplyr::select(index, label, year, votes_share) %>%
  dplyr::filter(year == 2006) %>%  # Filter rows where the year is 2006
  tidyr::pivot_wider(              # Reshape data: create a new column for each 'year' value
    names_from = "year",           # The values of 'year' will become new column names
    values_from = "votes_share",   # The values for the new columns will come from 'votes_share'
    names_prefix = "election_"     # Prefix these new column names with 'election_'
  )


# Filter the dataset to only include rows where the number of conflicts is greater than 0
# Then, for each row, create a new variable 'dyad' by concatenating 'side_a' and 'side_b', sorted and joined with an underscore
type_2_model <- actor_type_2_territories %>%
  dplyr::filter(n.conflicts > 0) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(dyad = paste(sort(c(side_a, side_b)), collapse = "_")) %>%
  dplyr::ungroup()

# Group the data by 'dyad', 'year', and 'index.data', then summarize all numeric columns by summing their values
type_2_model %<>%
  dplyr::group_by(dyad, year, index.data) %>%
  dplyr::summarise(
    dplyr::across(tidyselect::where(is.numeric), \(x) sum(x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  # Add new variables 'log_n.conflicts' and 'log_n.deaths' by applying a log transformation
  dplyr::mutate(
    log_n.conflicts = log10(n.conflicts + 0.1),
    log_n.deaths = log10(n.deaths + 0.1)
  )

# Filter the dataset 'actor_type_2_territories' to retain only rows where the number of conflicts is greater than 0
type_2_model_not_2018 <- actor_type_2_territories %>%
  dplyr::filter(n.conflicts > 0) %>%
  # Apply rowwise operation to mutate (create or modify) a new column 'dyad' by combining 'side_a' and 'side_b'
  dplyr::rowwise() %>%
  dplyr::mutate(
    # Create the 'dyad' by sorting and collapsing the two sides ('side_a' and 'side_b') into a single string
    dyad = paste(sort(c(side_a, side_b)), collapse = "_")
  ) %>%
  # Remove rowwise grouping
  dplyr::ungroup()

# Further process 'type_2_model_not_2018' by excluding data from the year 2018
type_2_model_not_2018 %<>%
  dplyr::filter(year != 2018) %>%
  # Group the data by 'dyad' and 'index.data'
  dplyr::group_by(dyad, index.data) %>%
  # Summarize numeric columns by summing their values, removing NA values in the process
  dplyr::summarise(
    dplyr::across(tidyselect::where(is.numeric), \(x) sum(x, na.rm = TRUE)),
    .groups = "drop"  # Drop the grouping after summarization
  ) %>%
  # Create two new columns: 'log_n.conflicts' and 'log_n.deaths', which are the log-transformed values of 'n.conflicts' and 'n.deaths'
  dplyr::mutate(
    log_n.conflicts = log10(n.conflicts + 0.1),
    log_n.deaths = log10(n.deaths + 0.1)
  )
# Ensure that the total number of deaths in the processed dataset matches the original dataset
stopifnot(sum(type_2_model$n.deaths) == sum(conflict.aggregated_by_type$n.deaths))



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
  dplyr::summarise(across(n.deaths, \(x) sum(x, na.rm = TRUE)), .groups = "drop")

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
  dplyr::summarise(across(n.deaths, \(x) sum(x, na.rm = TRUE)), .groups = "drop")

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
  dplyr::summarise(across(n.deaths, \(x) log10(sum(x, na.rm = TRUE) + 0.1)), .groups = "drop") %>%
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
  dplyr::summarise(across(n.conflicts, \(x) sum(x, na.rm = TRUE)), .groups = "drop")

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
  dplyr::summarise(across(n.conflicts, \(x) log10(sum(x, na.rm = TRUE) + 0.1)), .groups = "drop") %>%
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
  dplyr::summarise(across(n.deaths, \(x) sum(x, na.rm = TRUE)), .groups = "drop")

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
  dplyr::summarise(across(n.deaths, \(x) sum(x, na.rm = TRUE)), .groups = "drop")

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
  dplyr::summarise(across(n.deaths, \(x) log10(sum(x, na.rm = TRUE) + 0.1)), .groups = "drop") %>%
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
  dplyr::summarise(across(n.conflicts, \(x) sum(x, na.rm = TRUE)), .groups = "drop")

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
  dplyr::summarise(across(n.conflicts, \(x) log10(sum(x, na.rm = TRUE) + 0.1)), .groups = "drop") %>%
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
  dplyr::summarise(across(n.deaths, \(x) sum(x, na.rm = TRUE)), .groups = "drop")

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
  dplyr::summarise(across(n.deaths, \(x) log10(sum(x, na.rm = TRUE) + 0.1)), .groups = "drop") %>%
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
  dplyr::summarise(across(n.conflicts, \(x) sum(x, na.rm = TRUE)), .groups = "drop")

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
  dplyr::summarise(across(n.conflicts, \(x) log10(sum(x, na.rm = TRUE) + 0.1)), .groups = "drop") %>%
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
  dplyr::summarise(across(n.deaths, \(x) sum(x, na.rm = TRUE)), .groups = "drop")

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
#   dplyr::summarise(across(n.deaths, \(x) sum(x, na.rm = TRUE)), .groups = "drop")

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
  dplyr::summarise(across(n.deaths, \(x) log10(sum(x, na.rm = TRUE) + 0.1)), .groups = "drop") %>%
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
  dplyr::summarise(across(n.conflicts, \(x) sum(x, na.rm = TRUE)), .groups = "drop")

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
  dplyr::summarise(across(n.conflicts, \(x) log10(sum(x, na.rm = TRUE) + 0.1)), .groups = "drop") %>%
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
  dplyr::mutate(across(-c(index, year, votes_share, label), \(x) tidyr::replace_na(x, 0))) %>%
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
  dplyr::mutate(across(-c(index, year, votes_share, label), \(x) tidyr::replace_na(x, 0))) %>%
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
.summarise_var <- function(df, .var) {
  if (!is.null(df)) {
    # Convert to data frame and summarize the specified variable, returning the sum
    
    df %>% 
      dplyr::select(dplyr::one_of(.var)) %>%
      as.data.frame() %>%
      dplyr::summarise(across(dplyr::one_of(.var), \(x) sum(x, na.rm = TRUE))) %>%
      dplyr::pull(.var)
  }
}

# Create a dataset of conflict deaths by casualty type, with detailed breakdowns
conflict_deaths_by_casualty_type <- conflict.aggregated_by_type %>%
  # Filter out rows where there are no conflicts
  dplyr::filter(n.conflicts > 0) %>%
  # Create new columns by applying the summarization function to nested data
  dplyr::mutate(
    n.deaths_a = purrr::map(conflict.data, \(x) .summarise_var(x, "deaths_a")),
    n.deaths_b = purrr::map(conflict.data, \(x) .summarise_var(x, "deaths_b")),
    n.deaths_civilians = purrr::map(conflict.data, \(x) .summarise_var(x, "deaths_civilians")),
    n.deaths_unknow = purrr::map(conflict.data, \(x) .summarise_var(x, "deaths_unknown"))
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
  dplyr::summarise(across(everything(), \(x) sum(x, na.rm = FALSE)), .groups = "drop")

# Prepare the data for modeling by merging it with the election data
to.model <- share %>%
  # Left join with conflict deaths by casualty type data by 'index' and 'year'
  dplyr::left_join(conflict_deaths_by_casualty_type, by = c("index", "year")) %>%
  # Replace NA values in all columns except 'index', 'year', 'votes_share', and 'label' with 0
  dplyr::mutate(dplyr::across(-c(index, year, votes_share, label), \(x) tidyr::replace_na(x, 0))) %>%
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
  purrr::map(\(x) purrr::map(periods, function(period, var) {
    
    # Filter the conflict data to include only records with more than 0 conflicts and for the specific year
    to.model <- conflict.aggregated_by_type %>%
      dplyr::filter(n.conflicts > 0) %>%
      dplyr::filter(year == period) %>%
      
      # Group by index and year, summarizing conflict and death counts, and compute log-transformed variables
      dplyr::group_by(index.data, year) %>%
      dplyr::summarise(dplyr::across(c(n.conflicts, n.deaths), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
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
      dplyr::mutate(dplyr::across(dplyr::one_of(var), \(x) tidyr::replace_na(x, 0))) %>%
      dplyr::rename(region = label) %>%
      
      # Drop the 'index' column as it's no longer needed
      dplyr::select(-index)
    
    # Create a linear model formula based on the variable
    formula <- as.formula(paste0("votes_share ~ ", var))
    
    # Fit the linear model using the formula and the prepared data
    lm(formula, to.model)
    
  }, var = x)) %>%
  
  # Flatten the nested list of models into a single list
  purrr::flatten()

###### Diagnostics ######

# Diagnostics section to run additional diagnostics on the models if specified
if (run_diagnostics) {
  
  run_lm_list_diagnostics( models_list = models_tA2, 
                           table_name =  "Table_A2", 
                           model_name_prefix = "model_tA2_")
  
}

##### Save the models #####

models_computed <- "models_tA2"

save(periods,list=models_computed, file = here::here("results/TableA2_models.RData"))

rm(list = models_computed)




#### Table A2b ####

# Define a vector of variable names related to conflicts and deaths
vars <- c("n.conflicts", "log_n.conflicts", "n.deaths", "log_n.deaths")

# Extract unique years from the ACLED data, discard NA values, and sort the years
periods <- ACLED_data_models %>%
  dplyr::pull("year") %>%
  unique() %>%
  purrr::discard(is.na) %>%
  sort()

# Map over the list of variables to create models for each period in the ACLED data
models_tA2b <- vars %>%
  purrr::map( \(x) purrr::map(periods, function(period, var) {
    
    # Filter the ACLED data to include only records with more than 0 conflicts and for the specific year
    to.model <- ACLED_data_models %>%
      dplyr::filter(n.conflicts > 0) %>%
      dplyr::filter(year == period) %>%
      
      # Group by index and year, summarizing conflict and death counts, and compute log-transformed variables
      dplyr::group_by(index, year) %>%
      dplyr::summarise(dplyr::across(c(n.conflicts, n.deaths), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
      dplyr::mutate(
        log_n.conflicts = log10(n.conflicts + 0.1),
        log_n.deaths = log10(n.deaths + 0.1)
      ) %>%
      
      # Select the relevant columns and adjust the 'year' column to include the variable name
      dplyr::select(index, year, dplyr::one_of(var)) %>%
      dplyr::mutate(year = paste(var, year, sep = "_"))
    
    # Fix the index in the share data and join it with the prepared conflict data on the 'index' column
    to.model <- share %>%
      fix_ACLED_index() %>%
      dplyr::filter(year == period) %>%
      dplyr::left_join(to.model, by = c("index")) %>%
      
      # Replace NA values in the selected variables with 0 and rename 'label' to 'region'
      dplyr::mutate(dplyr::across(dplyr::one_of(var), \(x) tidyr::replace_na(x, 0))) %>%
      dplyr::rename(region = label) %>%
      
      # Drop the 'index' column as it's no longer needed
      dplyr::select(-index)
    
    # Create a linear model formula based on the variable
    formula <- as.formula(paste0("votes_share ~ ", var))
    
    # Fit the linear model using the formula and the prepared data
    lm(formula, to.model)
    
  }, var = x)) %>%
  
  # Flatten the nested list of models into a single list
  purrr::flatten()


###### Diagnostics ######

# Diagnostics section to run additional diagnostics on the models if specified
if (run_diagnostics) {
  
  run_lm_list_diagnostics( models_list = models_tA2b, 
                           table_name =  "Table_A2b", 
                           model_name_prefix = "model_tA2b_")
  
}

##### Save the models #####

models_computed <- "models_tA2b"

save(periods,list=models_computed, file = here::here("results/TableA2b_models.RData"))

rm(list = models_computed)


#### Table A2c ####

# Define a vector of variable names related to conflicts and deaths
vars <- c("n.conflicts", "log_n.conflicts", "n.deaths", "log_n.deaths")

# Extract unique years from the merged conflict data, discard NA values, and sort the years
periods <- conflict.aggregated_by_type %>%
  merge_villes(index = "index.data") %>%  # Merge the data by "index.data"
  dplyr::pull("year") %>%  # Extract the "year" column
  unique() %>%  # Get unique years
  purrr::discard(is.na) %>%  # Discard any NA values
  sort()  # Sort the years

# Map over the list of variables to create models for each period
models_tA2c <- vars %>%
  purrr::map(\(x) purrr::map(periods, function(period, var) {
    
    # Filter the conflict data to include only records with more than 0 conflicts and for the specific year
    to.model <- conflict.aggregated_by_type %>%
      merge_villes(index = "index.data") %>%  # Merge the data by "index.data"
      dplyr::filter(n.conflicts > 0) %>%  # Keep only rows where there are more than 0 conflicts
      dplyr::filter(year == period) %>%  # Filter the data for the specific period
      dplyr::group_by(index.data, year) %>%  # Group by index and year
      dplyr::summarise(  # Summarize the data
        dplyr::across(c(n.conflicts, n.deaths), \(x) sum(x,na.rm = TRUE)),  # Sum conflicts and deaths, removing NAs
        .groups = "drop"  # Drop the grouping after summarizing
      ) %>%
      dplyr::mutate(  # Create log-transformed variables
        log_n.conflicts = log10(n.conflicts + 0.1),  # Log transform the conflict counts
        log_n.deaths = log10(n.deaths + 0.1)  # Log transform the death counts
      ) %>%
      dplyr::select(  # Select relevant columns
        index = index.data, year, dplyr::one_of(var)
      ) %>%
      dplyr::mutate(year = paste(var, year, sep = "_"))  # Modify the "year" column to include the variable name
    
    # Merge the prepared conflict data with the vote share data, handling missing values
    to.model <- share_viles_merged %>%
      dplyr::filter(year == period) %>%  # Filter the share data for the specific period
      dplyr::left_join(to.model, by = c("index")) %>%  # Join the share data with the conflict data by "index"
      dplyr::mutate(dplyr::across(dplyr::one_of(var), \(x) tidyr::replace_na(x, 0))) %>%  # Replace NAs in the selected variables with 0
      dplyr::rename(region = label) %>%  # Rename the "label" column to "region"
      dplyr::select(-index)  # Remove the "index" column, as it's no longer needed
    
    # Create a linear model formula based on the variable
    formula <- as.formula(paste0("votes_share ~ ", var))
    
    # Fit the linear model using the formula and the prepared data
    lm(formula, to.model)
    
  }, var = x)) %>%
  
  # Flatten the nested list of models into a single list
  purrr::flatten()


###### Diagnostics ######

# Diagnostics section to run additional diagnostics on the models if specified
if (run_diagnostics) {
  
  run_lm_list_diagnostics( models_list = models_tA2c, 
                           table_name =  "Table_A2c", 
                           model_name_prefix = "model_tA2c_")
  
}

##### Save the models #####

models_computed <- "models_tA2c"

save(periods,list=models_computed, file = here::here("results/TableA2c_models.RData"))

rm(list = models_computed)

#### Table A2d ####


# Define a vector of variable names related to conflicts and deaths
vars <- c("n.conflicts", "log_n.conflicts", "n.deaths", "log_n.deaths")

# Extract unique years from the conflict data, discard NA values, and sort the years
periods <- conflict.aggregated_by_type %>%
  dplyr::pull("year") %>%  # Extract the 'year' column
  unique() %>%  # Get unique years
  purrr::discard(is.na) %>%  # Discard any NA values
  sort()  # Sort the years

# Create a list of models using different values of 'k' for the k-nearest neighbors analysis
models_tA2d <- 1:10 %>%
  purrr::map( \(k) {
    
    
    # Remove the first row from the borders data 
    borders <- congo.territoire.borders %>% dplyr::slice(-1)  
    
    # Create k-nearest neighbors based on the centroids of the borders
    w1nb <- borders %>%
      sf::st_centroid() %>%  # Calculate the centroid of each border region
      spdep::knearneigh(k = k, longlat = TRUE) %>%  # Compute k-nearest neighbors
      spdep::knn2nb()  # Convert the neighbor object to a list
    
    # Convert the neighbors list to a weights list
    listw1nb <- w1nb %>% spdep::nb2listw(style = "W")
    plot_path <- here::here(paste0("manuscript/knn/nb_", k, ".png"))
    
    if(!file.exists(plot_path)){
      # Convert neighbors to a spatial lines object for plotting
      neighbors_sf <- spdep::nb2lines(w1nb, coords = sp::coordinates(as(borders, "Spatial"))) %>%
        sf::st_as_sf() %>%  # Convert to an 'sf' object
        sf::st_set_crs(sf::st_crs(borders))  # Set the coordinate reference system
      
      # Plot the borders and the neighbors using ggplot2
      ggplot2::ggplot(congo.territoire.borders) + 
        ggplot2::geom_sf(fill = NA) +  # Plot the borders without filling
        ggplot2::theme_void() +  # Use a theme without axes or labels
        ggplot2::geom_sf(data = neighbors_sf, color = "red")  # Overlay the neighbors in red
      
      # Save the plot to a file
      ggplot2::ggsave(
        plot_path,  # File path and name
        width = 10,  # Width of the saved plot
        height = 10  # Height of the saved plot
      )
    }
    # Loop over each variable and period to create the models
    vars %>%
      purrr::map(\(x) purrr::map(periods, function(period, var) {
        
        # Filter the conflict data for the current period and aggregate it by index and year
        to.model <- conflict.aggregated_by_type %>%
          dplyr::filter(n.conflicts > 0) %>%  # Filter for non-zero conflicts
          dplyr::filter(year == period) %>%  # Filter for the specific period
          dplyr::group_by(index.data, year) %>%  # Group by index and year
          dplyr::summarise(
            dplyr::across(c(n.conflicts, n.deaths), \(x) sum(x, na.rm = TRUE)),  # Summarize conflicts and deaths
            .groups = "drop"  # Drop the grouping
          ) %>%
          dplyr::mutate(
            log_n.conflicts = log10(n.conflicts + 0.1),  # Log transform the conflict counts
            log_n.deaths = log10(n.deaths + 0.1)  # Log transform the death counts
          ) %>%
          dplyr::select(
            index = index.data,  # Rename 'index.data' to 'index'
            year, 
            dplyr::one_of(var)  # Select the current variable
          ) %>%
          dplyr::mutate(year = paste(var, year, sep = "_"))  # Modify the 'year' column to include the variable name
        
        # Merge the model data with the vote share data
        to.model <- share %>%
          dplyr::filter(year == period) %>%  # Filter for the specific period
          dplyr::left_join(to.model, by = c("index")) %>%  # Join on 'index'
          dplyr::mutate(dplyr::across(dplyr::one_of(var), \(x) tidyr::replace_na(x, 0))) %>%  # Replace NA values with 0
          dplyr::rename(region = label)  # Rename 'label' to 'region'
        
        # Merge the model data with the borders data
        to.model <- borders %>%
          dplyr::left_join(to.model, by = c(index.data = "index"))  # Join on 'index'
        
        # Create a linear model formula based on the variable
        formula <- as.formula(paste0("votes_share ~ ", var))
        
        # Convert the model data to a data frame
        to.model %<>% as.data.frame()
        
        # Select only the relevant columns for the model
        to.model %<>% dplyr::select(votes_share, dplyr::one_of(var))
        
        # Fit the spatial lag model (SLX) using the specified formula and weights
        m <- spatialreg::lmSLX(formula, to.model, listw = listw1nb, zero.policy = TRUE)
        
        # Return the model object
        m
        
      }, var = x)) %>%  # Pass the current variable to the inner map
      purrr::flatten()  # Flatten the nested list of models into a single list
  })


###### Diagnostics ######

# Diagnostics section to run additional diagnostics on the models if specified
if (run_diagnostics) {
  models_tA2d %>% purrr::map2(seq_along(.),\(x,i){
    run_lm_list_diagnostics( models_list = x, 
                             table_name =  "Table_A2d", 
                             model_name_prefix = glue::glue("model_tA2d_nb_{i}_"))
  })
  
  
}

##### Save the models #####

models_computed <- "models_tA2d"

save(periods,list=models_computed, file = here::here("results/TableA2d_models.RData"))

rm(list = models_computed)





#### Table A3 ####

# Define a vector of variable names related to conflicts and deaths
vars <- c("n.conflicts", "log_n.conflicts", "n.deaths", "log_n.deaths")



# Create a list of linear models using conflict data for years other than 2018
models_tA3 <- vars %>%
  purrr::map(function(var) {
    
    # Filter and aggregate conflict data, and calculate log-transformed variables
    to.model <- conflict.aggregated_by_type %>%
      dplyr::filter(n.conflicts > 0) %>%  # Keep only rows where there are conflicts
      dplyr::filter(year != 2018) %>%  # Exclude data from the year 2018
      dplyr::group_by(index.data) %>%  # Group by 'index.data'
      dplyr::summarise(
        dplyr::across(c(n.conflicts, n.deaths), \(x) sum(x, na.rm = TRUE)),  # Sum conflicts and deaths, ignoring NAs
        .groups = "drop"  # Drop the grouping after summarizing
      ) %>%
      dplyr::mutate(
        log_n.conflicts = log10(n.conflicts + 0.1),  # Log transform the conflict counts
        log_n.deaths = log10(n.deaths + 0.1)  # Log transform the death counts
      ) %>%
      dplyr::select(index = index.data, dplyr::one_of(var))  # Select relevant columns for modeling
    
    # Merge the conflict data with the election vote share data
    to.model <- share_not_2018 %>%
      dplyr::left_join(to.model, by = c("index")) %>%  # Join on 'index'
      dplyr::mutate(dplyr::across(dplyr::one_of(var), \(x) tidyr::replace_na(x, 0))) %>%  # Replace NAs with 0
      dplyr::rename(region = label) %>%  # Rename 'label' to 'region'
      dplyr::select(-index)  # Remove the 'index' column, as it's no longer needed
    
    # Define the linear model formula predicting 2011 election results based on conflicts and 2006 results
    formula <- as.formula(paste0("election_2011 ~ ", var, " + election_2006"))
    
    # Fit the linear model using the formula and the prepared data
    lm(formula, to.model)
    
  })

# Create a similar list of linear models using ACLED data, excluding the year 2018
models_tA3_ACLED <- vars %>%
  purrr::map(function(var) {
    
    # Filter and aggregate ACLED conflict data, and calculate log-transformed variables
    to.model <- ACLED_data_models %>%
      dplyr::filter(n.conflicts > 0) %>%  # Keep only rows where there are conflicts
      dplyr::filter(year != 2018) %>%  # Exclude data from the year 2018
      dplyr::group_by(index) %>%  # Group by 'index'
      dplyr::summarise(
        dplyr::across(c(n.conflicts, n.deaths), \(x) sum(x, na.rm = TRUE)),  # Sum conflicts and deaths, ignoring NAs
        .groups = "drop"  # Drop the grouping after summarizing
      ) %>%
      dplyr::mutate(
        log_n.conflicts = log10(n.conflicts + 0.1),  # Log transform the conflict counts
        log_n.deaths = log10(n.deaths + 0.1)  # Log transform the death counts
      ) %>%
      dplyr::select(index, dplyr::one_of(var))  # Select relevant columns for modeling
    
    # Fix the index in the election vote share data and merge with the conflict data
    to.model <- share_not_2018 %>%
      fix_ACLED_index() %>%  # Apply a function to fix the index
      dplyr::left_join(to.model, by = c("index")) %>%  # Join on 'index'
      dplyr::mutate(dplyr::across(dplyr::one_of(var), ~ tidyr::replace_na(., 0))) %>%  # Replace NAs with 0
      dplyr::rename(region = label) %>%  # Rename 'label' to 'region'
      dplyr::select(-index)  # Remove the 'index' column, as it's no longer needed
    
    # Define the linear model formula predicting 2011 election results based on conflicts and 2006 results
    formula <- as.formula(paste0("election_2011 ~ ", var, " + election_2006"))
    
    # Fit the linear model using the formula and the prepared data
    lm(formula, to.model)
    
  })

# Combine the lists of models from both datasets (conflict.aggregated_by_type and ACLED)
models_tA3 <- c(models_tA3, models_tA3_ACLED)



###### Diagnostics ######

# Diagnostics section to run additional diagnostics on the models if specified
if (run_diagnostics) {
  
  run_lm_list_diagnostics( models_list = models_tA3, 
                           table_name =  "Table_A3", 
                           model_name_prefix = "model_tA3_")
  
}

##### Save the models #####

models_computed <- "models_tA3"

save(list=models_computed, file = here::here("results/TableA3_models.RData"))

rm(list = models_computed)




#### Table A4 ####

# Define a vector of variable names related to conflicts and deaths
vars <- c("n.conflicts", "log_n.conflicts", "n.deaths", "log_n.deaths")


# Create a list of linear models using conflict data, predicting 2018 election results
models_tA4 <- vars %>%
  purrr::map(function(var) {
    
    # Filter and aggregate conflict data, then calculate log-transformed variables
    to.model <- conflict.aggregated_by_type %>%
      dplyr::filter(n.conflicts > 0) %>%  # Keep only rows where there are conflicts
      dplyr::group_by(index.data) %>%  # Group by 'index.data'
      dplyr::summarise(
        dplyr::across(c(n.conflicts, n.deaths), \(x) sum(x, na.rm = TRUE)),  # Sum conflicts and deaths, ignoring NAs
        .groups = "drop"  # Drop the grouping after summarizing
      ) %>%
      dplyr::mutate(
        log_n.conflicts = log10(n.conflicts + 0.1),  # Log transform the conflict counts
        log_n.deaths = log10(n.deaths + 0.1)  # Log transform the death counts
      ) %>%
      dplyr::select(index = index.data, dplyr::one_of(var))  # Select relevant columns for modeling
    
    # Merge the conflict data with the election vote share data
    to.model <- share_wide %>%
      dplyr::left_join(to.model, by = c("index")) %>%  # Join on 'index'
      dplyr::mutate(dplyr::across(dplyr::one_of(var), ~ tidyr::replace_na(., 0))) %>%  # Replace NAs with 0
      dplyr::rename(region = label) %>%  # Rename 'label' to 'region'
      dplyr::select(-index)  # Remove the 'index' column, as it's no longer needed
    
    # Define the linear model formula predicting 2018 election results based on conflicts and 2006 results
    formula <- as.formula(paste0("election_2018 ~ ", var, " + election_2006"))
    
    # Fit the linear model using the formula and the prepared data
    lm(formula, to.model)
    
  })

# Create a similar list of linear models using ACLED data
models_tA4_ACLED <- vars %>%
  purrr::map(function(var) {
    
    # Filter and aggregate ACLED conflict data, then calculate log-transformed variables
    to.model <- ACLED_data_models %>%
      dplyr::filter(n.conflicts > 0) %>%  # Keep only rows where there are conflicts
      dplyr::group_by(index) %>%  # Group by 'index'
      dplyr::summarise(
        dplyr::across(c(n.conflicts, n.deaths), \(x) sum(x, na.rm = TRUE)),  # Sum conflicts and deaths, ignoring NAs
        .groups = "drop"  # Drop the grouping after summarizing
      ) %>%
      dplyr::mutate(
        log_n.conflicts = log10(n.conflicts + 0.1),  # Log transform the conflict counts
        log_n.deaths = log10(n.deaths + 0.1)  # Log transform the death counts
      ) %>%
      dplyr::select(index, dplyr::one_of(var))  # Select relevant columns for modeling
    
    # Fix the index in the election vote share data and merge with the conflict data
    to.model <- share_wide %>%
      fix_ACLED_index() %>%  # Apply a function to fix the index
      dplyr::left_join(to.model, by = c("index")) %>%  # Join on 'index'
      dplyr::mutate(dplyr::across(dplyr::one_of(var), ~ tidyr::replace_na(., 0))) %>%  # Replace NAs with 0
      dplyr::rename(region = label) %>%  # Rename 'label' to 'region'
      dplyr::select(-index)  # Remove the 'index' column, as it's no longer needed
    
    # Define the linear model formula predicting 2018 election results based on conflicts and 2006 results
    formula <- as.formula(paste0("election_2018 ~ ", var, " + election_2006"))
    
    # Fit the linear model using the formula and the prepared data
    lm(formula, to.model)
    
  })

# Combine the lists of models from both datasets (conflict.aggregated_by_type and ACLED)
models_tA4 <- c(models_tA4, models_tA4_ACLED)

###### Diagnostics ######

# Diagnostics section to run additional diagnostics on the models if specified
if (run_diagnostics) {
  
  run_lm_list_diagnostics(
    models_list = models_tA4,  # List of models to run diagnostics on
    table_name = "Table_A4",  # Name of the table for diagnostics
    model_name_prefix = "model_tA4_"  # Prefix for saved diagnostic files
  )
  
}

##### Save the models #####

# Save the computed models to a file for later use
models_computed <- "models_tA4"

save(
  list = models_computed,  # Save the list of models under 'models_computed'
  file = here::here("results/TableA4_models.RData")  # Save to the specified file path
)

# Remove the models from the environment to free up memory
rm(list = models_computed)



#### Table A6i ####

# Define a vector of variable names that will be used in the analysis
vars <- c("n.conflicts", "log_n.conflicts", "n.deaths", "log_n.deaths")



# Create a list of models for each variable in 'vars'
models_tA6i <- vars %>%
  purrr::map(function(var) {  # Apply the following function to each variable in 'vars'
    
    # Filter and summarize the 'conflict.aggregated_by_type' data for modeling
    to.model <- conflict.aggregated_by_type %>%
      dplyr::filter(n.conflicts > 0) %>%  # Keep only rows where 'n.conflicts' is greater than 0
      dplyr::filter(year == 2006) %>%     # Keep only rows where the year is 2006
      dplyr::group_by(index.data, year) %>%  # Group by 'index.data' and 'year'
      dplyr::summarise(                      # Summarize the grouped data
        dplyr::across(c(n.conflicts, n.deaths), \(x) sum(x, na.rm = TRUE)),  # Sum 'n.conflicts' and 'n.deaths' across groups
        .groups = "drop"  # Drop the grouping after summarization
      ) %>%
      dplyr::mutate(  # Add new variables to the dataset
        log_n.conflicts = log10(n.conflicts + 0.1),  # Compute log10 of 'n.conflicts' with a small offset
        log_n.deaths = log10(n.deaths + 0.1)  # Compute log10 of 'n.deaths' with a small offset
      ) %>%
      dplyr::select(index = index.data, year, dplyr::one_of(var)) %>%  # Select relevant columns for the model
      dplyr::mutate(  # Modify the 'year' column to include the variable name
        year = paste(var, year, sep = "_")
      ) %>%
      tidyr::pivot_wider(  # Reshape data: create a new column for each 'year' value
        names_from = "year",  # The modified 'year' column becomes the new column names
        values_from = dplyr::all_of(var)  # The values for the new columns come from the current variable
      )
    
    # Merge the reshaped 'share_2006' data with the 'to.model' data
    to.model <- share_2006 %>%
      dplyr::left_join(to.model, by = c("index")) %>%  # Perform a left join by the 'index' column
      dplyr::mutate(  # Replace NA values in columns starting with the variable name with 0
        dplyr::across(dplyr::starts_with(var), ~tidyr::replace_na(., 0))
      ) %>%
      dplyr::rename(region = label) %>%  # Rename the 'label' column to 'region'
      dplyr::select(-index)  # Remove the 'index' column from the dataset
    
    # Create a formula for the linear model
    formula <- as.formula(paste0("election_2006 ~ ", paste0(var, "_2006")))
    
    # Fit a linear model using the formula and the prepared data
    lm(formula, to.model)
  })

# Create a list of models for each variable in 'vars' using ACLED data
models_tA6i_ACLED <- vars %>%
  purrr::map(function(var) {  # Apply the following function to each variable in 'vars'
    
    # Filter and summarize the 'ACLED_data_models' data for modeling
    to.model <- ACLED_data_models %>%
      dplyr::filter(n.conflicts > 0) %>%  # Keep only rows where 'n.conflicts' is greater than 0
      dplyr::filter(year == 2006) %>%     # Keep only rows where the year is 2006
      dplyr::group_by(index, year) %>%  # Group by 'index' and 'year'
      dplyr::summarise(  # Summarize the grouped data
        dplyr::across(c(n.conflicts, n.deaths), \(x) sum(x, na.rm = TRUE)),  # Sum 'n.conflicts' and 'n.deaths' across groups
        .groups = "drop"  # Drop the grouping after summarization
      ) %>%
      dplyr::mutate(  # Add new variables to the dataset
        log_n.conflicts = log10(n.conflicts + 0.1),  # Compute log10 of 'n.conflicts' with a small offset
        log_n.deaths = log10(n.deaths + 0.1)  # Compute log10 of 'n.deaths' with a small offset
      ) %>%
      dplyr::select(index, year, dplyr::one_of(var)) %>%  # Select relevant columns for the model
      dplyr::mutate(  # Modify the 'year' column to include the variable name
        year = paste(var, year, sep = "_")
      ) %>%
      tidyr::pivot_wider(  # Reshape data: create a new column for each 'year' value
        names_from = "year",  # The modified 'year' column becomes the new column names
        values_from = dplyr::all_of(var)  # The values for the new columns come from the current variable
      )
    
    # Merge the reshaped 'share_2006' data with the 'to.model' data after fixing ACLED index
    to.model <- share_2006 %>%
      fix_ACLED_index() %>%  # Apply a custom function to fix the ACLED index
      dplyr::left_join(to.model, by = c("index")) %>%  # Perform a left join by the 'index' column
      dplyr::mutate(  # Replace NA values in columns starting with the variable name with 0
        dplyr::across(dplyr::starts_with(var), ~tidyr::replace_na(., 0))
      ) %>%
      dplyr::rename(region = label) %>%  # Rename the 'label' column to 'region'
      dplyr::select(-index)  # Remove the 'index' column from the dataset
    
    # Create a formula for the linear model
    formula <- as.formula(paste0("election_2006 ~ ", paste0(var, "_2006")))
    
    # Fit a linear model using the formula and the prepared data
    lm(formula, to.model)
  })

# Combine the two lists of models into one
models_tA6i <- c(models_tA6i, models_tA6i_ACLED)


###### Diagnostics ######

# Diagnostics section to run additional diagnostics on the models if specified
if (run_diagnostics) {
  
  run_lm_list_diagnostics( models_list = models_tA6i, 
                           table_name =  "Table_A6i", 
                           model_name_prefix = "model_tA6i_")
  
}

##### Save the models #####

models_computed <- "models_tA6i"

save(list=models_computed, file = here::here("results/TableA6i_models.RData"))

rm(list = models_computed)


#### Table A6ii ####

# Define a vector of variable names that will be used in the model
vars <- c("n.conflicts", "log_n.conflicts", "n.deaths", "log_n.deaths")

# Create a list of linear models, one for each variable in `vars`, using the `conflict.aggregated_by_type` data
models_tA6ii <- vars %>% 
  # Map over each variable in `vars`
  purrr::map(function(var) {
    # Filter the data to include only rows where `n.conflicts` is greater than 0
    to.model <- conflict.aggregated_by_type %>% 
      dplyr::filter(n.conflicts > 0) %>% 
      # Exclude data from the year 2018
      dplyr::filter(year != 2018) %>% 
      # Group the data by `index.data` and `year`
      dplyr::group_by(index.data, year) %>% 
      # Summarize the data by summing `n.conflicts` and `n.deaths`, removing any missing values
      dplyr::summarise(dplyr::across(c(n.conflicts, n.deaths), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>% 
      # Create new variables with the logarithm (base 10) of `n.conflicts` and `n.deaths`
      dplyr::mutate(log_n.conflicts = log10(n.conflicts + 0.1), log_n.deaths = log10(n.deaths + 0.1)) %>% 
      # Select the relevant columns: `index.data`, `year`, and the variable of interest
      dplyr::select(index = index.data, year, dplyr::one_of(var)) %>% 
      # Modify the `year` column to include the variable name as a prefix
      dplyr::mutate(year = paste(var, year, sep = "_")) %>% 
      # Reshape the data to a wider format, with each year as a separate column
      tidyr::pivot_wider(names_from = "year", values_from = dplyr::all_of(var))
    
    # Join the model data with `share_not_2018` based on the `index` column
    to.model <- share_not_2018 %>% 
      dplyr::left_join(to.model, by = c("index")) %>% 
      # Replace missing values (NA) in columns that start with the variable name with 0
      dplyr::mutate(dplyr::across(dplyr::starts_with(var), \(x) tidyr::replace_na(x, 0))) %>% 
      # Rename the `label` column to `region`
      dplyr::rename(region = label) %>% 
      # Remove the `index` column from the final data
      dplyr::select(-index)
    
    # Define the formula for the linear model, incorporating the variable of interest for 2006 and 2011
    formula <- as.formula(paste0("election_2011 ~ election_2006 + ", paste0(var, "_2006"), "+", paste0(var, "_2011")))
    
    # Fit a linear model using the defined formula and the prepared data
    lm(formula, to.model)
  })

# Create a similar list of linear models using `ACLED_data_models` data
models_tA6ii_ACLED <- vars %>% 
  # Map over each variable in `vars`
  purrr::map(function(var) {
    # Filter the data to include only rows where `n.conflicts` is greater than 0
    to.model <- ACLED_data_models %>% 
      dplyr::filter(n.conflicts > 0) %>% 
      # Exclude data from the year 2018
      dplyr::filter(year != 2018) %>% 
      # Group the data by `index` and `year`
      dplyr::group_by(index, year) %>% 
      # Summarize the data by summing `n.conflicts` and `n.deaths`, removing any missing values
      dplyr::summarise(dplyr::across(c(n.conflicts, n.deaths), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>% 
      # Create new variables with the logarithm (base 10) of `n.conflicts` and `n.deaths`
      dplyr::mutate(log_n.conflicts = log10(n.conflicts + 0.1), log_n.deaths = log10(n.deaths + 0.1)) %>% 
      # Select the relevant columns: `index`, `year`, and the variable of interest
      dplyr::select(index, year, dplyr::one_of(var)) %>% 
      # Modify the `year` column to include the variable name as a prefix
      dplyr::mutate(year = paste(var, year, sep = "_")) %>% 
      # Reshape the data to a wider format, with each year as a separate column
      tidyr::pivot_wider(names_from = "year", values_from = dplyr::all_of(var))
    
    # Fix the `index` values using a custom function `fix_ACLED_index` and join with `share_not_2018`
    to.model <- share_not_2018 %>% 
      fix_ACLED_index() %>% 
      dplyr::left_join(to.model, by = c("index")) %>% 
      # Replace missing values (NA) in columns that start with the variable name with 0
      dplyr::mutate(dplyr::across(dplyr::starts_with(var), \(x) tidyr::replace_na(x, 0))) %>% 
      # Rename the `label` column to `region`
      dplyr::rename(region = label) %>% 
      # Remove the `index` column from the final data
      dplyr::select(-index)
    
    # Define the formula for the linear model, incorporating the variable of interest for 2006 and 2011
    formula <- as.formula(paste0("election_2011 ~ election_2006 + ", paste0(var, "_2006"), "+", paste0(var, "_2011")))
    
    # Fit a linear model using the defined formula and the prepared data
    lm(formula, to.model)
  })

# Combine the lists of models from both `models_tA6ii` and `models_tA6ii_ACLED`
models_tA6ii <- c(models_tA6ii, models_tA6ii_ACLED)


###### Diagnostics ######

# Diagnostics section to run additional diagnostics on the models if specified
if (run_diagnostics) {
  
  run_lm_list_diagnostics( models_list = models_tA6ii, 
                           table_name =  "Table_A6ii", 
                           model_name_prefix = "model_tA6ii_")
  
}

##### Save the models #####

models_computed <- "models_tA6ii"

save(list=models_computed, file = here::here("results/TableA6ii_models.RData"))

rm(list = models_computed)



#### Table A6iii ####


# Define a vector of variable names to be used in the models
vars <- c("n.conflicts", "log_n.conflicts", "n.deaths", "log_n.deaths")

# Apply a function to each element in the `vars` vector using purrr::map()
models_tA6iii <- vars %>% purrr::map(\(var) {
  # Filter the data to only include rows where the number of conflicts is greater than 0
  to.model <- conflict.aggregated_by_type %>%
    dplyr::filter(n.conflicts > 0) %>%
    # Group the data by `index.data` and `year`
    dplyr::group_by(index.data, year) %>%
    # Summarize the data by calculating the sum of `n.conflicts` and `n.deaths`, removing NA values
    dplyr::summarise(
      dplyr::across(c(n.conflicts, n.deaths), \(x) sum(x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    # Create new variables by applying the logarithm to `n.conflicts` and `n.deaths`, adding 0.1 to avoid log(0)
    dplyr::mutate(
      log_n.conflicts = log10(n.conflicts + 0.1),
      log_n.deaths = log10(n.deaths + 0.1)
    ) %>%
    # Select the relevant columns for modeling
    dplyr::select(index = index.data, year, dplyr::one_of(var)) %>%
    # Modify the `year` column to include the variable name as a prefix
    dplyr::mutate(year = paste(var, year, sep = "_")) %>%
    # Reshape the data from long to wide format
    tidyr::pivot_wider(names_from = "year", values_from = dplyr::all_of(var))
  
  # Join the `to.model` data with the `share_wide` data based on the `index` column
  to.model <- share_wide %>%
    dplyr::left_join(to.model, by = c("index")) %>%
    # Replace NA values with 0 for all columns that start with the variable name
    dplyr::mutate(dplyr::across(dplyr::starts_with(var), \(x) { tidyr::replace_na(x, 0) })) %>%
    # Rename the `label` column to `region`
    dplyr::rename(region = label) %>%
    # Drop the `index` column
    dplyr::select(-index)
  
  # Define the formula for the linear model
  formula <- as.formula(
    paste0(
      "election_2018 ~ election_2006 + election_2011 +",
      paste0(var, "_2006"), "+", paste0(var, "_2011"), "+", paste0(var, "_2018")
    )
  )
  
  # Fit a linear model using the defined formula and the `to.model` data
  lm(formula, to.model)
})

# Apply a similar function to each element in the `vars` vector, this time using `ACLED_data_models`
models_tA6iii_ACLED <- vars %>% purrr::map(\(var) {
  # Filter the data to only include rows where the number of conflicts is greater than 0
  to.model <- ACLED_data_models %>%
    dplyr::filter(n.conflicts > 0) %>%
    # Group the data by `index` and `year`
    dplyr::group_by(index, year) %>%
    # Summarize the data by calculating the sum of `n.conflicts` and `n.deaths`, removing NA values
    dplyr::summarise(
      dplyr::across(c(n.conflicts, n.deaths), \(x) sum(x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    # Create new variables by applying the logarithm to `n.conflicts` and `n.deaths`, adding 0.1 to avoid log(0)
    dplyr::mutate(
      log_n.conflicts = log10(n.conflicts + 0.1),
      log_n.deaths = log10(n.deaths + 0.1)
    ) %>%
    # Select the relevant columns for modeling
    dplyr::select(index, year, dplyr::one_of(var)) %>%
    # Modify the `year` column to include the variable name as a prefix
    dplyr::mutate(year = paste(var, year, sep = "_")) %>%
    # Reshape the data from long to wide format
    tidyr::pivot_wider(names_from = "year", values_from = dplyr::all_of(var))
  
  # Fix the `index` column in `share_wide` using the custom `fix_ACLED_index()` function and join with `to.model`
  to.model <- share_wide %>%
    fix_ACLED_index() %>%
    dplyr::left_join(to.model, by = c("index")) %>%
    # Replace NA values with 0 for all columns that start with the variable name
    dplyr::mutate(dplyr::across(dplyr::starts_with(var), \(x) { tidyr::replace_na(x, 0) })) %>%
    # Rename the `label` column to `region`
    dplyr::rename(region = label) %>%
    # Drop the `index` column
    dplyr::select(-index)
  
  # Define the formula for the linear model
  formula <- as.formula(
    paste0(
      "election_2018 ~ election_2006 + election_2011 +",
      paste0(var, "_2006"), "+", paste0(var, "_2011"), "+", paste0(var, "_2018")
    )
  )
  
  # Fit a linear model using the defined formula and the `to.model` data
  lm(formula, to.model)
})

# Combine the results of the two model lists into a single list
models_tA6iii <- c(models_tA6iii, models_tA6iii_ACLED)



###### Diagnostics ######

# Diagnostics section to run additional diagnostics on the models if specified
if (run_diagnostics) {
  
  run_lm_list_diagnostics( models_list = models_tA6iii, 
                           table_name =  "Table_A6iii", 
                           model_name_prefix = "model_tA6iii_")
  
}

##### Save the models #####

models_computed <- "models_tA6iii"

save(list=models_computed, file = here::here("results/TableA6iii_models.RData"))

rm(list = models_computed)


#### Table A8i ####


# Define a vector of variable names to be used in the analysis
vars <- c("n.conflicts", "log_n.conflicts", "n.deaths", "log_n.deaths")


# Repeat the process for the 'ACLED_actor_type_2_territories' dataset
ACLED_type_2_model <- ACLED_actor_type_2_territories %>%
  dplyr::filter(n.conflicts > 0) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(dyad = paste(sort(c(side_a, side_b)), collapse = "_")) %>%
  dplyr::ungroup()

# Filter the data to only include the year 2006
ACLED_type_2_model %<>%
  dplyr::filter(year == 2006) %>%
  dplyr::group_by(dyad, index.data) %>%
  dplyr::summarise(
    dplyr::across(tidyselect::where(is.numeric), \(x) sum(x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  # Add log-transformed variables for conflicts and deaths
  dplyr::mutate(
    log_n.conflicts = log10(n.conflicts + 0.1),
    log_n.deaths = log10(n.deaths + 0.1)
  )

# Generate models for each variable in 'vars'
models_tA8i <- vars %>%
  purrr::map(function(var) {
    # Filter the data for the year 2006 and select relevant columns
    to.model <- type_2_model %>%
      dplyr::filter(year == 2006) %>%
      dplyr::select(index = index.data, dyad, dplyr::one_of(var))
    
    # Reshape the data to wide format
    to.model %<>%
      tidyr::pivot_wider(
        values_from = dplyr::one_of(var),
        names_from = "dyad",
        values_fill = 0
      )
    
    # Join with 'share_2006' data and replace NA values with 0
    to.model <- share_2006 %>%
      dplyr::left_join(to.model, by = c("index")) %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), \(x) tidyr::replace_na(x, 0))) %>%
      dplyr::rename(region = label) %>%
      dplyr::select(-index, -region)
    
    # Create the formula for the linear model
    formula <- as.formula(paste0("election_2006 ~ ."))
    
    # Fit a linear model to the data
    lm(formula, to.model)
  })

# Check non-dyads relevance
# ACLED_type_2_model %>% dplyr::filter(!stringr::str_detect(dyad, "_")) %>%
# dplyr::group_by(dyad) %>% dplyr::summarise(dplyr::across(dplyr::one_of(c("n.deaths", "n.conflicts")), sum))
# # A tibble: 1 × 3
# dyad      n.deaths n.conflicts
# <chr>        <int>       <int>
#   1 civilians        0          19

# Filter out non-dyad entries from the 'ACLED_type_2_model' dataset
ACLED_type_2_model %<>% dplyr::filter(stringr::str_detect(dyad, "_"))

# Generate models for the filtered 'ACLED_type_2_model' data
models_tA8i_ACLED <- vars %>%
  purrr::map(function(var) {
    # Select relevant columns from the data
    to.model <- ACLED_type_2_model %>%
      dplyr::select(index = index.data, dyad, dplyr::one_of(var))
    
    # Reshape the data to wide format
    to.model %<>%
      tidyr::pivot_wider(
        values_from = var,
        names_from = "dyad",
        values_fill = 0
      )
    
    # Join with 'share_2006' data and replace NA values with 0
    to.model <- share_2006 %>%
      dplyr::left_join(to.model, by = c("index")) %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), \(x) tidyr::replace_na(x, 0))) %>%
      dplyr::rename(region = label) %>%
      dplyr::select(-index, -region)
    
    # Create the formula for the linear model
    formula <- as.formula(paste0("election_2006 ~ ."))
    
    # Fit a linear model to the data
    lm(formula, to.model)
  })

# Combine the two sets of models into one list
models_tA8i <- c(models_tA8i, models_tA8i_ACLED)

###### Diagnostics ######

# Diagnostics section to run additional diagnostics on the models if specified
if (run_diagnostics) {
  
  run_lm_list_diagnostics( models_list = models_tA8i, 
                           table_name =  "Table_A8i", 
                           model_name_prefix = "model_tA8i_")
  
}

##### Save the models #####

models_computed <- "models_tA8i"

save(list=models_computed, file = here::here("results/TableA8i_models.RData"))

rm(list = models_computed)


#### Table A8ib ####


# Define a vector of variable names to be used in the models
vars <- c("n.conflicts", "log_n.conflicts", "n.deaths", "log_n.deaths")

# Create models for different values of k using a range from 1 to 10
models_tA8ib <- 1:10 %>% 
  purrr::map(\(k) { # Iterate over each value of k
    
    # Remove the first row from the territorial borders data
    borders <- congo.territoire.borders %>% dplyr::slice(-1)
    
    # Calculate the centroid for each border and find k nearest neighbors
    w1nb <- borders %>% 
      sf::st_centroid(borders) %>% 
      spdep::knearneigh(k = k, longlat = TRUE) %>% 
      spdep::knn2nb()
    
    # Convert neighbors to a spatial weights list
    listw1nb <- w1nb %>% spdep::nb2listw(style = "W")
    
    # Create spatial lines from the neighbor connections and convert to sf format
    neighbors_sf <- as(spdep::nb2lines(w1nb, coords = sp::coordinates(as(borders, "Spatial"))), 'sf')
    neighbors_sf <- sf::st_set_crs(neighbors_sf, sf::st_crs(borders))
    
    # Plot the territorial borders and the neighbor connections using ggplot2
    ggplot2::ggplot(congo.territoire.borders) + 
      ggplot2::geom_sf(fill = NA) +
      ggplot2::theme_void() +
      ggplot2::geom_sf(data = neighbors_sf, color = "red")
    
    # Save the plot as a PNG file
    ggplot2::ggsave(here::here(paste0("manuscript/knn/table_8ib_nb_", k, ".png")), width = 10, height = 10)
    
    # Iterate over each variable in the 'vars' vector to build models
    vars %>% purrr::map(\(var) {
      
      # Filter data for the year 2006 and select relevant columns
      to.model <- type_2_model %>%  
        dplyr::filter(year == 2006) %>% 
        dplyr::select(index = index.data, dyad, dplyr::one_of(var)) %>% 
        dplyr::arrange(dyad)
      
      # Reshape the data to a wide format with dyads as columns
      to.model %<>% tidyr::pivot_wider(values_from = var, names_from = "dyad", values_fill = 0)
      
      # Merge with share_2006 data, replace NAs with 0, and clean up column names
      to.model <- share_2006 %>%  
        dplyr::left_join(to.model, by = c("index")) %>% 
        dplyr::mutate(dplyr::across(tidyselect:::where(is.numeric), \(x) tidyr::replace_na(x, 0))) %>% 
        dplyr::rename(region = label) %>% 
        dplyr::select(-index, -region)
      
      # Define the formula for the regression model
      formula <- as.formula(paste0("election_2006 ~ ."))
      
      # Fit a spatial lag model (SLX) using the prepared data and spatial weights
      m <- spatialreg::lmSLX(formula, to.model, listw = listw1nb, zero.policy = TRUE)
      
      # Return the fitted model
      m
    }) 
  })


###### Diagnostics ######

# Diagnostics section to run additional diagnostics on the models if specified
if (run_diagnostics) {
  
  run_lm_list_diagnostics( models_list = models_tA8ib, 
                           table_name =  "Table_A8ib", 
                           model_name_prefix = "model_tA8ib_")
  
}

##### Save the models #####

models_computed <- "models_tA8ib"

save(list=models_computed, file = here::here("results/TableA8ib_models.RData"))

rm(list = models_computed)



#### Table A8ic ####



# Define a vector of variable names that will be used in the modeling process
vars <- c("n.conflicts", "log_n.conflicts", "n.deaths", "log_n.deaths")

# Generate models for a sequence of k values from 1 to 10
models_tA8ic <- 1:10 %>% purrr::map(\(k) {
  # Store the current value of k
  k <- k
  
  # Remove the first row from the borders data (possibly to avoid an outlier or incorrect entry)
  borders <- congo.territoire.borders %>% dplyr::slice(-1)  
  
  # Compute the centroids of the borders and create k-nearest neighbors for the given k value
  w1nb <- borders %>%
    sf::st_centroid(borders) %>% 
    spdep::knearneigh(k = k, longlat = TRUE) %>% 
    spdep::knn2nb() 
  
  # Convert the neighbor list to a weights list with "W" style (row-standardized)
  listw1nb <- w1nb %>% spdep::nb2listw(style = "W")
  
  # Convert neighbor relationships to spatial lines and set the coordinate reference system
  neighbors_sf <- spdep::nb2lines(w1nb, coords = sp::coordinates(as(borders, "Spatial"))) %>% 
    as('sf') %>% 
    sf::st_set_crs(sf::st_crs(borders))
  
  # Plot the borders and neighbor relationships
  ggplot2::ggplot(congo.territoire.borders) +
    ggplot2::geom_sf(fill = NA) +
    ggplot2::theme_void() +
    ggplot2::geom_sf(data = neighbors_sf, color = "red")
  
  # Save the plot to a PNG file
  ggplot2::ggsave(here::here(paste0("manuscript/knn/table_8ic_nb_", k, ".png")), width = 10, height = 10)
  
  # Loop over each variable in the 'vars' vector to create models
  vars %>% purrr::map(\(var) {
    
    # Filter the data for the year 2011 and select relevant columns
    to.model <- type_2_model %>% 
      dplyr::filter(year == 2011) %>% 
      dplyr::select(index = index.data, dyad, dplyr::one_of(var)) %>% 
      dplyr::arrange(dyad)
    
    # Pivot the data to a wider format, with dyads as columns and fill missing values with 0
    to.model %<>% tidyr::pivot_wider(values_from = dplyr::one_of(var), names_from = "dyad", values_fill = 0)
    
    # Join with another dataset, replace missing numeric values with 0, rename columns, and remove unnecessary columns
    to.model <- share_not_2018 %>% 
      dplyr::left_join(to.model, by = c("index")) %>% 
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ tidyr::replace_na(., 0))) %>% 
      dplyr::rename(region = label) %>% 
      dplyr::select(-index, -region)
    
    # Define the formula for the main model
    formula <- as.formula(paste0("election_2011 ~ ."))
    
    # Define the formula for the lag model, excluding certain variables
    lag_formula <- as.formula(paste0("~ ", paste0(purrr::discard(names(to.model), \(x) x %in% c("election_2011", "election_2006")), collapse = " + ")))
    
    # Fit a spatial lag model using the formulas defined above
    m <- spatialreg::lmSLX(formula, to.model, listw = listw1nb, zero.policy = TRUE, Durbin = lag_formula)
    
    # Return the fitted model
    m
  }) 
})



###### Diagnostics ######

# Diagnostics section to run additional diagnostics on the models if specified
if (run_diagnostics) {
  
  run_lm_list_diagnostics( models_list = models_tA8ic, 
                           table_name =  "Table_A8ic", 
                           model_name_prefix = "model_tA8ic_")
  
}

##### Save the models #####

models_computed <- "models_tA8ic"

save(list=models_computed, file = here::here("results/TableA8ic_models.RData"))

rm(list = models_computed)

#### Table A8id ####

# Define a vector of variable names for the model
vars <- c("n.conflicts", "log_n.conflicts", "n.deaths", "log_n.deaths")


# Generate 10 models using purrr::map
models_tA8id <- 1:10 %>%
  purrr::map(\(k){
    # Set k as the current number of neighbors
    borders <- congo.territoire.borders %>%
      # Remove the first row from the borders data frame
      dplyr::slice(-1)
    
    # Calculate centroids for each territory and create a k-nearest neighbors object
    w1nb <- borders %>%
      sf::st_centroid() %>%
      spdep::knearneigh(k = k, longlat = TRUE) %>%
      spdep::knn2nb()
    
    # Convert the k-nearest neighbors object to a weights list
    listw1nb <- w1nb %>%
      spdep::nb2listw(style = "W")
    
    # Convert neighbor relationships into line segments for visualization
    neighbors_sf <- spdep::nb2lines(w1nb, coords = sp::coordinates(as(borders, "Spatial"))) %>%
      as('sf') %>%
      # Set the coordinate reference system for the neighbors
      sf::st_set_crs(sf::st_crs(borders))
    
    # Plot the territory borders and neighbor connections using ggplot2
    ggplot2::ggplot(congo.territoire.borders) +
      ggplot2::geom_sf(fill = NA) +
      ggplot2::theme_void() +
      ggplot2::geom_sf(data = neighbors_sf, color = "red")
    
    # Save the plot as a PNG file using ggplot2::ggsave
    ggplot2::ggsave(here::here(paste0("manuscript/knn/table_8id_nb_", k, ".png")), width = 10, height = 10)
    
    # Map over each variable in 'vars' to create a model
    vars %>%
      purrr::map(\(var){
        # Filter the data for the year 2018 and select relevant columns
        to.model <- type_2_model %>%
          dplyr::filter(year == 2018) %>%
          dplyr::select(index = index.data, dyad, dplyr::one_of(var)) %>%
          dplyr::arrange(dyad)
        
        # Reshape the data to a wide format
        to.model %<>%
          tidyr::pivot_wider(values_from = var, names_from = "dyad", values_fill = 0)
        
        # Merge with 'share_wide' data and replace NAs with zeros
        to.model <- share_wide %>%
          dplyr::left_join(to.model, by = c("index")) %>%
          dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), \(x){tidyr::replace_na(x, 0)})) %>%
          dplyr::rename(region = label) %>%
          dplyr::select(-index, -region)
        
        # Drop the 'fg_fg' column from the model data
        to.model %<>%
          dplyr::select(-fg_fg)
        
        # Create a formula for the model
        formula <- as.formula(paste0("election_2018 ~ ."))
        
        # Create a formula for the lag terms in the spatial model
        lag_formula <- as.formula(paste0("~ ", paste0(purrr::discard(names(to.model), \(x){x %in% c("election_2011", "election_2006", "election_2018")}), collapse = " + ")))
        
        # Fit a spatial regression model using the spatial lag model with Durbin component
        m <- spatialreg::lmSLX(formula, to.model, listw = listw1nb, zero.policy = TRUE, Durbin = lag_formula)
        
        # Return the fitted model
        m
      })
  })


###### Diagnostics ######

# Diagnostics section to run additional diagnostics on the models if specified
if (run_diagnostics) {
  
  run_lm_list_diagnostics( models_list = models_tA8id, 
                           table_name =  "Table_A8id", 
                           model_name_prefix = "model_tA8id_")
  
}

##### Save the models #####

models_computed <- "models_tA8id"

save(list=models_computed, file = here::here("results/TableA8id_models.RData"))

rm(list = models_computed)

#### Table A8ii ####
# Define a vector of variable names to be used in the model
vars <- c("n.conflicts", "log_n.conflicts", "n.deaths", "log_n.deaths")

# For each variable in 'vars', fit a linear model
models_tA8ii <- vars %>%
  purrr::map(\(var) {
    # Select relevant columns from 'type_2_model_not_2018' for the current variable and rename 'index.data' to 'index'
    to.model <- type_2_model_not_2018 %>%
      dplyr::select(index = index.data, dyad, dplyr::one_of(var))
    
    # Reshape 'to.model' into a wider format, with 'dyad' as column names and 'var' as values
    to.model %<>%
      tidyr::pivot_wider(
        values_from = var,
        names_from = "dyad",
        values_fill = 0  # Replace missing values with 0
      )
    
    # Join the reshaped 'to.model' with 'share_not_2018' on the 'index' column
    to.model <- share_not_2018 %>%
      dplyr::left_join(to.model, by = c("index")) %>%
      # Replace NA values in numeric columns with 0
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), \(x) tidyr::replace_na(x, 0))) %>%
      # Rename 'label' to 'region', and remove 'index' and 'region' columns
      dplyr::rename(region = label) %>%
      dplyr::select(-index, -region)
    
    # Define a linear model formula to predict 'election_2011' using all other variables as predictors
    formula <- as.formula(paste0("election_2011 ~ ."))
    
    # Fit the linear model using the defined formula and 'to.model' data
    lm(formula, to.model)
  })

###### Diagnostics ######

# Diagnostics section to run additional diagnostics on the models if specified
if (run_diagnostics) {
  
  run_lm_list_diagnostics( models_list = models_tA8ii, 
                           table_name =  "Table_A8ii", 
                           model_name_prefix = "model_tA8ii_")
  
}

##### Save the models #####

models_computed <- "models_tA8ii"

save(list=models_computed, file = here::here("results/TableA8ii_models.RData"))

rm(list = models_computed)
