###############################################################################
# Name: 003b - create tables.R
# Author: John Quattrochi (john.quattrochi@gmail.com)
# Assistant: Juan Luis Herrera Cortijo (juan.luis.herrera.cortijo@gmail.com)
# Purpose: Plot manuscript tables. 
# This script is not meant to be run as a stand alone script, but sourced from 
# 003 - update manuscript.R
# Please make sure that you run the script 002 - models.R first.
# Script UUID: ab9f1273-a819-55ec-b7f0-6727060aee3f
###############################################################################

#### TABLES ####

##### TABLE 1 #####

# Convert ged201 to a data frame and group by conflict type, then summarize counts and deaths
conflict.types.table <- ged201 %>%
  as.data.frame() %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(n = dplyr::n(), best = sum(best), .groups = "drop") %>%
  dplyr::rename(`Conflict category` = type)

# Filter data for conflicts involving state actors within a specified date range
conflict.types.table.by.state <- ged201 %>%
  as.data.frame() %>%
  dplyr::filter(date_start >= lubridate::ymd("2001-01-17") & date_end <= lubridate::ymd("2018-12-30")) %>%
  dplyr::filter(stringr::str_starts(type, "State"))

# Group by conflict type and side_a, then summarize counts and deaths
conflict.types.table.by.state %<>%
  dplyr::group_by(type, side_a) %>%
  dplyr::summarise(n = dplyr::n(), best = sum(best), .groups = "drop")

# Rename columns for clarity
conflict.types.table.by.state %<>%
  dplyr::rename(State = side_a) %>%
  dplyr::rename(`Conflict category` = type)

# Calculate total conflicts and deaths
total <- conflict.types.table %>%
  dplyr::summarise(across(where(is.numeric), sum)) %>%
  dplyr::mutate(`Conflict category` = "Total")

# Combine conflict types and state-specific conflicts into a single table
conflicts.table <- conflict.types.table %>%
  dplyr::slice(1:3) %>%
  dplyr::bind_rows(conflict.types.table.by.state %>% dplyr::slice(1:3)) %>%
  dplyr::bind_rows(conflict.types.table %>% dplyr::slice(4)) %>%
  dplyr::bind_rows(conflict.types.table.by.state %>% dplyr::slice(4:8)) %>%
  dplyr::bind_rows(total) %>%
  dplyr::select(`Conflict category`, ` ` = State, `Conflict events` = n, Deaths = best)

# Blank out certain "Conflict category" rows for formatting
conflicts.table$`Conflict category`[c(4:6,8:12)] <- ""

# Create a flextable and apply formatting
Table_1 <- conflicts.table %>%
  flextable::flextable() %>%
  flextable::autofit() %>%
  flextable::hline(i = 12, border = officer::fp_border(width = 2)) %>%
  flextable::hline(i = c(3, 7), border = officer::fp_border(width = 1))



##### TABLE 3a ####

# Load the pre-saved RData file containing model results
load("results/Table3a_models.RData")



# Create a list of models to be printed
models.to.print <- list(
  model_t3a_1, model_t3a_2, model_t3a_3, model_t3a_4,
  model_t3a_5, model_t3a_6, model_t3a_7, model_t3a_8,
  model_t3a_9, model_t3a_10, model_t3a_11, model_t3a_12,
  model_t3a_13, model_t3a_14, model_t3a_15, model_t3a_16
)

# In order to stargazer to recognize the models, we need to fix their calls

models.to.print %<>% purrr::modify(\(model) {model$call[1] <- call("plm"); model})

# Create a vector of model names to be used in the table
model_names <- c(
  "Total Deaths", "Log\nTotal Deaths", "Total Conflicts", "Log\nTotal Conflicts",
  "Total Deaths\nno NL", "Log Total Deaths\nno NL", "Total Conflicts\nno NL", "Log Total Conflicts\nno NL",
  "ACLED\nTotal Deaths", "ACLED\nLog\nTotal Deaths", "ACLED\nTotal Conflicts", "ACLED\nLog\nTotal Conflicts",
  "ACLED\nTotal Deaths\nno NL", "ACLED\nLog Total Deaths\nno NL", "ACLED\nTotal Conflicts\nno NL", "ACLED\nLog Total Conflicts\nno NL"
)

# Calculate standard errors for each model using robust covariance matrix
models.to.print_se <- purrr::map(models.to.print, ~
                                   # Apply coeftest with robust standard errors (HC3 method)
                                   lmtest::coeftest(.x, sandwich::vcovHC(.x, method = "arellano", type = "HC3"))[, "Std. Error"]
)

# Calculate F-statistics and append significance stars
F.stat <- purrr::map(models.to.print, ~{
  # Get the model summary with robust standard errors
  summ <- summary(.x, vcov. = sandwich::vcovHC(.x, method = "arellano", type = "HC3"))
  # Extract the p-value and format the F-statistic with significance stars
  .p <- summ$fstatistic$p.value
  sprintf("%0.3f%s", summ$fstatistic$statistic,
          dplyr::case_when(
            .p < 0.01 ~ "***",
            .p < 0.05 ~ "**",
            .p < 0.1 ~ "*",
            TRUE ~ ""
          ))
}) %>%
  unlist() %>%  # Flatten the list into a vector
  c("F Statistic", .)  # Add label "F Statistic" at the beginning

# Calculate degrees of freedom (df) for each model
df <- purrr::map(models.to.print, ~{
  # Get the model summary with robust standard errors
  summ <- summary(.x, vcov. = sandwich::vcovHC(.x, method = "arellano", type = "HC3"))
  # Extract and combine degrees of freedom
  paste(summ$fstatistic$parameter, collapse = ";")
}) %>%
  unlist() %>%  # Flatten the list into a vector
  c("df", .)  # Add label "df" at the beginning

# Generate the table using stargazer, suppressing warnings
Table_3a <- suppressWarnings(
  # Create the table in HTML format
  stargazer::stargazer(
    models.to.print, type = "html",
    se = models.to.print_se,
    dep.var.caption = "Votes share",  # Set the dependent variable caption
    omit.stat = "f",  # Omit the default F-statistic
    add.lines = list(F.stat, df)  # Add the custom F-statistic and df lines
  )
) %>%
  paste0(collapse = "")  # Combine the HTML output into a single string

# Convert the HTML table to a dataframe
Table_3a <- xml2::read_html(Table_3a) %>%
  rvest::html_table() %>%
  as.data.frame() %>%
  dplyr::slice(-c(1:6))  # Remove the first 6 rows

# Add the model names as column headers
table_names <- c(" ", model_names)
Table_3a <- magrittr::set_names(Table_3a, table_names)

# Identify and remove empty lines in the table
empty_lines <- Table_3a %>%
  dplyr::transmute(across(everything(), ~nchar(.) == 0)) %>%
  dplyr::rowwise() %>%
  dplyr::transmute(all(dplyr::c_across(everything()))) %>%
  dplyr::pull(1) %>%
  which()
Table_3a <- dplyr::slice(Table_3a, -empty_lines)

# Create a flextable and apply formatting
Table_3a %<>%
  flextable::flextable() %>%
  # Merge cells in the last row for all columns except the first
  flextable::merge_at(i = nrow(Table_3a), j = 2:ncol(Table_3a)) %>%
  # Apply text and paragraph styling for all parts of the table
  flextable::style(pr_t = officer::fp_text(font.size = 11), part = "all",
                   pr_p = officer::fp_par(line_spacing = 1, padding = 0, text.align = "center")) %>%
  # Apply specific styling for the header
  flextable::style(pr_t = officer::fp_text(font.size = 11), part = "header",
                   pr_p = officer::fp_par(line_spacing = 1, padding = 0, text.align = "center")) %>%
  # Add a horizontal line at the 10th row
  flextable::hline(i = 10, border = officer::fp_border()) %>%
  # Add vertical lines between the 1st and 9th columns
  flextable::vline(j = c(1, 9), border = officer::fp_border()) %>%
  # Automatically adjust column widths
  flextable::autofit() %>%
  # Fit the table to a width of 10.5 inches
  flextable::fit_to_width(10.5)

rm(models.to.print)

##### TABLE 3b ####

# Load the pre-saved RData file containing model results
load("results/Table3b_models.RData")

# Create a list of models to be printed
models.to.print <- list(model_t3b_1, model_t3b_2, model_t3b_3)

# Modify the model calls so that stargazer recognizes them as 'plm' models
models.to.print %<>%
  purrr::modify(\(model) {
    # Replace the first element of the call with a 'plm' call
    model$call[1] <- call("plm")
    model  # Return the modified model
  })

# Define the names for each model to be displayed in the table
model_names <- c("Deaths by type", "Conflicts by type", "Conflict Deaths by side")

# Calculate Standard Errors

# Calculate robust standard errors for each model using the Arellano method
models.to.print_se <- purrr::map(models.to.print, ~
                                   # Use coeftest with robust standard errors (HC3 method)
                                   lmtest::coeftest(.x, sandwich::vcovHC(.x, method = "arellano", type = "HC3"))[, "Std. Error"]
)

# Calculate F-Statistics

# Calculate F-statistics and format them with significance stars
F.stat <- purrr::map(models.to.print, ~{
  # Get the model summary with robust standard errors
  summ <- summary(.x, vcov. = sandwich::vcovHC(.x, method = "arellano", type = "HC3"))
  # Extract the p-value and format the F-statistic with significance stars
  .p <- summ$fstatistic$p.value
  sprintf("%0.3f%s", summ$fstatistic$statistic,
          dplyr::case_when(
            .p < 0.01 ~ "***",
            .p < 0.05 ~ "**",
            .p < 0.1 ~ "*",
            TRUE ~ ""
          ))
}) %>%
  unlist() %>%  # Flatten the list into a vector
  c("F Statistic", .)  # Add label "F Statistic" at the beginning

# Calculate Degrees of Freedom

# Calculate degrees of freedom (df) for each model
df <- purrr::map(models.to.print, ~{
  # Get the model summary with robust standard errors
  summ <- summary(.x, vcov. = sandwich::vcovHC(.x, method = "arellano", type = "HC3"))
  # Extract and combine degrees of freedom
  paste(summ$fstatistic$parameter, collapse = ";")
}) %>%
  unlist() %>%  # Flatten the list into a vector
  c("df", .)  # Add label "df" at the beginning

# Generate the Table

# Generate the table using stargazer, suppressing warnings
Table_3b <- suppressWarnings(
  stargazer::stargazer(
    models.to.print, type = "html",
    se = models.to.print_se,
    column.labels = model_names,  # Set the column labels
    dep.var.caption = "Votes share",  # Set the dependent variable caption
    omit.stat = "f",  # Omit the default F-statistic
    add.lines = list(F.stat, df)  # Add the custom F-statistic and df lines
  )
) %>%
  paste0(collapse = "")  # Combine the HTML output into a single string

# Convert HTML Table to DataFrame

# Convert the HTML table to a dataframe
Table_3b <- xml2::read_html(Table_3b) %>%
  rvest::html_table() %>%
  as.data.frame() %>%
  dplyr::slice(-c(1:5))  # Remove the first 5 rows (header rows)

# Extract and set the column names from the first row of the table
table_names <- as.vector(Table_3b %>% dplyr::slice(1) %>% unlist())
table_names[1] <- " "  # Replace the first column name with a blank space
Table_3b <- magrittr::set_names(Table_3b, table_names) %>%
  dplyr::slice(-1)  # Remove the first row (now redundant after setting names)

# Remove Empty Lines

# Identify and remove empty rows in the table
empty_lines <- Table_3b %>%
  dplyr::transmute(across(everything(), ~nchar(.) == 0)) %>%
  dplyr::rowwise() %>%
  dplyr::transmute(all(dplyr::c_across(everything()))) %>%
  dplyr::pull(1) %>%
  which()
Table_3b <- dplyr::slice(Table_3b, -empty_lines)

# Format the Table with flextable

# Create a flextable and apply formatting
Table_3b %<>%
  flextable::flextable() %>%
  # Merge cells in the last row for all columns except the first
  flextable::merge_at(i = nrow(Table_3b), j = 2:ncol(Table_3b)) %>%
  # Apply text and paragraph styling for all parts of the table
  flextable::style(pr_t = officer::fp_text(font.size = 7), part = "all",
                   pr_p = officer::fp_par(line_spacing = 1, padding = 0)) %>%
  # Add a horizontal line at the 34th row
  flextable::hline(i = 34, border = officer::fp_border()) %>%
  # Automatically adjust column widths
  flextable::autofit() %>%
  # Fit the table to a width of 6.49 inches
  flextable::fit_to_width(6.49)

rm(models.to.print)

##### TABLE 1v2 #####
# Convert the ged201 object into a data frame for manipulation
conflict.types.period.table <- ged201 %>% 
  as.data.frame() %>% 
  
  # Create a new variable 'period' based on the date ranges that match different election periods
  dplyr::mutate(period = dplyr::case_when(
    # If the event starts and ends between Jan 17, 2001, and Jul 30, 2006, assign it to the "2006 election" period
    date_start >= lubridate::ymd("2001-01-17") & date_end <= lubridate::ymd("2006-07-30") ~ "2006 election",
    
    # If the event starts and ends between Jul 31, 2006, and Nov 28, 2011, assign it to the "2011 election" period
    date_start >= lubridate::ymd("2006-07-31") & date_end <= lubridate::ymd("2011-11-28") ~ "2011 election",
    
    # If the event starts and ends between Nov 29, 2011, and Dec 30, 2018, assign it to the "2018 election" period
    date_start >= lubridate::ymd("2011-11-29") & date_end <= lubridate::ymd("2018-12-30") ~ "2018 election",
    
    # Otherwise, assign NA if it doesn't match any of the above periods
    TRUE ~ NA_character_
  ))

# Group data by 'type' and 'period' to summarize conflicts for each period and type
conflict.types.period.table %<>% 
  dplyr::group_by(type, period) %>%
  
  # Summarize the number of conflicts ('n') and the sum of 'best' casualties for each group
  dplyr::summarise(n = dplyr::n(), best = sum(best), .groups = "drop") %>%
  
  # Rename the 'type' column to 'Conflict category' for clarity
  dplyr::rename(`Conflict category` = type)

# Filter the data to include only conflicts starting with "State" in their type
conflict.types.period.table.by.state <- ged201 %>%
  as.data.frame() %>%
  
  # Create the 'period' variable again as done previously
  dplyr::mutate(period = dplyr::case_when(
    date_start >= lubridate::ymd("2001-01-17") & date_end <= lubridate::ymd("2006-07-30") ~ "2006 election",
    date_start >= lubridate::ymd("2006-07-31") & date_end <= lubridate::ymd("2011-11-28") ~ "2011 election",
    date_start >= lubridate::ymd("2011-11-29") & date_end <= lubridate::ymd("2018-12-30") ~ "2018 election",
    TRUE ~ NA_character_
  )) %>%
  
  # Filter for rows where 'type' starts with "State"
  dplyr::filter(stringr::str_starts(type, "State"))

# Group the state-specific conflict data by 'period', 'type', and 'side_a'
conflict.types.period.table.by.state %<>%
  dplyr::group_by(period, type, side_a) %>%
  
  # Summarize the number of conflicts ('n') and the sum of 'best' casualties for each group
  dplyr::summarise(n = dplyr::n(), best = sum(best), .groups = "drop")

# Rename the 'side_a' column to 'State' and the 'type' column to 'Conflict category'
conflict.types.period.table.by.state %<>% 
  dplyr::rename(State = side_a) %>% 
  dplyr::rename(`Conflict category` = type)

# Nest the conflict data by 'period', apply a transformation to rename columns for each period
conflict.types.period.table %<>% 
  tidyr::nest(data = -period) %>% 
  dplyr::mutate(data = purrr::map2(data, period, ~{
    .period <- .y
    .data <- .x
    
    # Rename columns by appending the period name to each column except 'Conflict category'
    .data %>% dplyr::rename_with(~paste0(.x, "_", .period), .cols = -`Conflict category`)
  })) %>%
  
  # Combine the nested data into a single data frame by full joining on 'Conflict category'
  dplyr::pull("data") %>% 
  purrr::reduce(~{dplyr::full_join(.x, .y, by = "Conflict category")})

# Create a summary row with totals for all numeric columns and add it to the conflict table
total <- conflict.types.period.table %>% 
  dplyr::summarise(dplyr::across(where(is.numeric), sum)) %>%
  
  # Add a 'Conflict category' value of "Total" to the summary row
  dplyr::mutate(`Conflict category` = "Total") %>%
  
  # Convert all columns to character type
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

# Convert all columns in the conflict table to character type
conflict.types.period.table %<>% 
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

# Nest the state-specific conflict data by 'period', apply transformations similar to the general conflict data
conflict.types.period.table.by.state %<>% 
  tidyr::nest(data = -period) %>% 
  dplyr::mutate(data = purrr::map2(data, period, ~{
    .period <- .y
    .data <- .x
    
    # Rename columns by appending the period name, except 'State' and 'Conflict category'
    .data %>% dplyr::rename_with(~paste0(.x, "_", .period), .cols = -c(State, `Conflict category`))
  })) %>% 
  
  # Combine the nested data into a single data frame by full joining on 'Conflict category' and 'State'
  dplyr::pull("data") %>% 
  purrr::reduce(~{dplyr::full_join(.x, .y, by = c("Conflict category", "State"))}) %>%
  
  # Replace NA values in numeric columns with "-" and convert them to character type
  dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~tidyr::replace_na(as.character(.), "-")))

# Combine the general conflict table with the state-specific conflict table and add the total row
conflicts.table <- conflict.types.period.table %>% 
  dplyr::slice(1:3) %>% 
  dplyr::bind_rows(conflict.types.period.table.by.state %>% dplyr::slice(1:3)) %>%
  dplyr::bind_rows(conflict.types.period.table %>% dplyr::slice(4)) %>%
  dplyr::bind_rows(conflict.types.period.table.by.state %>% dplyr::slice(4:8)) %>% 
  dplyr::bind_rows(total) %>% 
  
  # Select and rename columns for the final table
  dplyr::select(`Conflict category`, ` ` = State, dplyr::everything()) %>%
  dplyr::rename_with(~stringr::str_replace(.x, "n_", "Conflict events ") %>% 
                       stringr::str_replace("best_", "Deaths "))

# Replace specific values in the 'Conflict category' column with an empty string
conflicts.table$`Conflict category`[c(4:6, 8:12)] <- ""

# Create a formatted table using the flextable package
Table_1_v2 <- conflicts.table %>%
  
  # Convert the data frame to a flextable object
  flextable::flextable()  %>%
  
  # Add a custom header with merged cells for the election periods
  flextable::add_header(values = list(
    "Conflict events 2006 election" = "2006 election",
    "Deaths 2006 election" = "2006 election",
    "Conflict events 2011 election" = "2011 election",
    "Deaths 2011 election" = "2011 election",
    "Conflict events 2018 election" = "2018 election",
    "Deaths 2018 election" = "2018 election")) %>%
  
  # Set custom labels for the column headers with line breaks
  flextable::set_header_labels(values = list(
    "Conflict events 2006 election" = "Conflict\nevents",
    "Deaths 2006 election" = "Deaths",
    "Conflict events 2011 election" = "Conflict\nevents",
    "Deaths 2011 election" = "Deaths",
    "Conflict events 2018 election" = "Conflict\nevents",
    "Deaths 2018 election" = "Deaths")) %>%
  
  # Merge the header cells by election period
  flextable::merge_h(part = "header") %>%
  
  # Add horizontal lines with varying border widths
  flextable::hline(i = c(2, 6, 12), border = officer::fp_border(width = 2))  %>% 
  flextable::hline(i = c(3, 7), border = officer::fp_border(width = 1))  %>%
  
  # Add vertical lines with a specific border width
  flextable::vline(j = c(2, 4, 6), border = officer::fp_border(width = 2)) %>% 
  
  # Apply text and paragraph styles to the entire table
  flextable::style(
    pr_t = officer::fp_text(font.size = 9),
    part = "all",
    pr_p = officer::fp_par(line_spacing = 1, padding = 1)) %>%
  
  # Automatically adjust the column widths to fit the content
  flextable::autofit() %>% 
  
  # Fit the table to a specific width
  flextable::fit_to_width(6.49)



##### TABLE A2 ######

# Load the pre-saved RData file that contains the model results
load("results/TableA2_models.RData")

# Create a vector of model names corresponding to conflicts, log-transformed conflicts, deaths, and log-transformed deaths
model_names <- c(
  paste("Conflicts", periods),
  paste("Log Conflicts", periods),
  paste("Deaths", periods),
  paste("Log Deaths", periods)
)

# Create a vector of model names formatted with line breaks for printing
model_names.to_print <- c(
  paste0("Conflicts\n", periods),
  paste0("Log Conflicts\n", periods),
  paste0("Deaths\n", periods),
  paste0("Log Deaths\n", periods)
)

# Extract the standard errors from the model coefficients using heteroscedasticity-consistent (HC3) standard errors
models.to.print_se <- purrr::map(models_tA2, ~ {
  lmtest::coeftest(.x, sandwich::vcovHC(.x, method = "arellano", type = "HC3"))[,"Std. Error"]
})

# Compute the F-statistic for each model and format it for inclusion in the table
F.stat <- purrr::map(models_tA2, ~ {
  summ <- summary(.x, vcov. = sandwich::vcovHC(.x, method = "arellano", type = "HC3"))
  
  # Calculate the p-value for the F-statistic and append significance stars based on the p-value
  .p <- pf(
    summ$fstatistic["value"],
    summ$fstatistic["numdf"],
    summ$fstatistic["dendf"],
    lower.tail = FALSE
  )
  
  # Format the F-statistic value with significance stars
  sprintf(
    "%0.3f%s",
    summ$fstatistic["value"],
    dplyr::case_when(
      .p < 0.01 ~ "***",
      .p < 0.05 ~ "**",
      .p < 0.1  ~ "*",
      TRUE ~ ""
    )
  )
}) %>%
  
  # Unlist the results and prepend "F Statistic" as the first element
  unlist() %>% c("F Statistic", .)

# Extract the degrees of freedom (df) from the model summaries
df <- purrr::map(models_tA2, ~ {
  summ <- summary(.x, vcov. = sandwich::vcovHC(.x, method = "arellano", type = "HC3"))
  
  # Combine the numerator and denominator degrees of freedom into a single string
  paste(summ$fstatistic[2:3], collapse = ";")
}) %>%
  
  # Unlist the results and prepend "df" as the first element
  unlist() %>% c("df", .)

# Generate the regression table in HTML format, suppressing warnings
Table_A2 <- suppressWarnings(
  stargazer::stargazer(
    models_tA2, type = "html",
    se = models.to.print_se,
    column.labels = model_names,
    dep.var.caption = "Votes share",
    omit.stat = "f",
    add.lines = list(F.stat, df)
  )
) %>%
  
  # Collapse the HTML output into a single string
  paste0(collapse = "")

# Convert the HTML table into a data frame and remove the first 5 rows (unnecessary headers)
Table_A2 <- xml2::read_html(Table_A2) %>%
  rvest::html_table() %>%
  as.data.frame() %>%
  dplyr::slice(-c(1:5))

# Extract the first row as the table header names, convert it to a vector, and set the first element to a space
table_names <- as.vector(Table_A2 %>% dplyr::slice(1) %>% unlist())
table_names[1] <- " "

# Rename the columns of the table using the formatted model names and remove the first row
Table_A2 <- magrittr::set_names(Table_A2, c(" ", model_names.to_print)) %>%
  dplyr::slice(-1)

# Identify rows where all columns are empty and get their indices
empty_lines <- Table_A2 %>%
  dplyr::transmute(across(dplyr::everything(), ~ nchar(.) == 0)) %>%
  dplyr::rowwise() %>%
  dplyr::transmute(all(dplyr::c_across(dplyr::everything()))) %>%
  dplyr::pull(1) %>%
  which()

# Remove the identified empty rows from the table
Table_A2 %<>% dplyr::slice(-empty_lines)

# Convert the table to a flextable object and apply various styles and formatting
Table_A2 %<>% flextable::flextable() %>%
  flextable::merge_at(i = nrow(Table_A2), j = 2:ncol(Table_A2)) %>%
  flextable::style(
    pr_t = officer::fp_text(font.size = 9),
    part = "all",
    pr_p = officer::fp_par(line_spacing = 1, padding = 0)
  ) %>%
  
  # Add a horizontal line and automatically adjust column widths to fit content
  flextable::hline(i = 10, border = officer::fp_border()) %>%
  flextable::autofit() %>%
  flextable::fit_to_width(9)


rm(models_tA2)

##### TABLE A2b #####

# Load the pre-saved RData file that contains the model results
load("results/TableA2b_models.RData")

# Create a vector of model names for Conflicts, Log Conflicts, Deaths, and Log Deaths
model_names <- c(
  paste("Conflicts", periods),
  paste("Log Conflicts", periods),
  paste("Deaths", periods),
  paste("Log Deaths", periods)
)

# Create a vector of model names with line breaks for printing in the table
model_names.to_print <- c(
  paste0("Conflicts\n", periods),
  paste0("Log Conflicts\n", periods),
  paste0("Deaths\n", periods),
  paste0("Log Deaths\n", periods)
)

# Extract the standard errors for each model using heteroscedasticity-consistent (HC3) standard errors
models.to.print_se <- purrr::map(models_tA2b, ~ {
  lmtest::coeftest(.x, sandwich::vcovHC(.x, method = "arellano", type = "HC3"))[, "Std. Error"]
})

# Compute the F-statistic for each model, format it, and append significance stars based on p-values
F.stat <- purrr::map(models_tA2b, ~ {
  summ <- summary(.x, vcov. = sandwich::vcovHC(.x, method = "arellano", type = "HC3"))
  
  # Compute the p-value for the F-statistic
  .p <- pf(
    summ$fstatistic["value"],
    summ$fstatistic["numdf"],
    summ$fstatistic["dendf"],
    lower.tail = FALSE
  )
  
  # Format the F-statistic value with significance stars
  sprintf(
    "%0.3f%s",
    summ$fstatistic["value"],
    dplyr::case_when(
      .p < 0.01 ~ "***",
      .p < 0.05 ~ "**",
      .p < 0.1  ~ "*",
      TRUE ~ ""
    )
  )
}) %>%
  
  # Unlist the results and prepend "F Statistic" as the first element
  unlist() %>% c("F Statistic", .)

# Extract the degrees of freedom (df) from the model summaries
df <- purrr::map(models_tA2b, ~ {
  summ <- summary(.x, vcov. = sandwich::vcovHC(.x, method = "arellano", type = "HC3"))
  
  # Combine the numerator and denominator degrees of freedom into a single string
  paste(summ$fstatistic[2:3], collapse = ";")
}) %>%
  
  # Unlist the results and prepend "df" as the first element
  unlist() %>% c("df", .)

# Generate the regression table in HTML format, suppressing warnings during generation
Table_A2b <- suppressWarnings(
  stargazer::stargazer(
    models_tA2b, type = "html",
    se = models.to.print_se,
    column.labels = model_names,
    dep.var.caption = "Votes share",
    omit.stat = "f",
    add.lines = list(F.stat, df)
  )
) %>%
  
  # Collapse the HTML output into a single string
  paste0(collapse = "")

# Convert the HTML table into a data frame and remove the first 5 rows (unnecessary headers)
Table_A2b <- xml2::read_html(Table_A2b) %>%
  rvest::html_table() %>%
  as.data.frame() %>%
  dplyr::slice(-c(1:5))

# Extract the first row as the table header names, convert it to a vector, and set the first element to a space
table_names <- as.vector(Table_A2b %>% dplyr::slice(1) %>% unlist())
table_names[1] <- " "

# Rename the columns of the table using the formatted model names and remove the first row
Table_A2b <- magrittr::set_names(Table_A2b, c(" ", model_names.to_print)) %>%
  dplyr::slice(-1)

# Identify rows where all columns are empty and get their indices
empty_lines <- Table_A2b %>%
  dplyr::transmute(dplyr::across(dplyr::everything(), ~ nchar(.) == 0)) %>%
  dplyr::rowwise() %>%
  dplyr::transmute(all(dplyr::c_across(dplyr::everything()))) %>%
  dplyr::pull(1) %>%
  which()

# Remove the identified empty rows from the table
Table_A2b %<>% dplyr::slice(-empty_lines)

# Convert the table to a flextable object and apply various styles and formatting
Table_A2b %<>% flextable::flextable() %>%
  flextable::merge_at(i = nrow(Table_A2b), j = 2:ncol(Table_A2b)) %>%
  flextable::style(
    pr_t = officer::fp_text(font.size = 11),
    part = "all",
    pr_p = officer::fp_par(line_spacing = 1, padding = 0)
  ) %>%
  
  # Add a horizontal line and automatically adjust column widths to fit content
  flextable::hline(i = 10, border = officer::fp_border()) %>%
  flextable::autofit() %>%
  flextable::fit_to_width(9)

rm(models_tA2b)

##### TABLE A2c ######

# Load the pre-saved RData file that contains the model results for Table A2c
load("results/TableA2c_models.RData")

# Create a vector of model names for Conflicts, Log Conflicts, Deaths, and Log Deaths across different periods
model_names <- c(
  paste("Conflicts", periods),
  paste("Log Conflicts", periods),
  paste("Deaths", periods),
  paste("Log Deaths", periods)
)

# Create a vector of model names with line breaks for printing in the table
model_names.to_print <- c(
  paste0("Conflicts\n", periods),
  paste0("Log Conflicts\n", periods),
  paste0("Deaths\n", periods),
  paste0("Log Deaths\n", periods)
)

# Extract the standard errors for each model using heteroscedasticity-consistent (HC3) standard errors
models.to.print_se <- purrr::map(models_tA2c, ~ {
  lmtest::coeftest(.x, sandwich::vcovHC(.x, method = "arellano", type = "HC3"))[, "Std. Error"]
})

# Compute the F-statistic for each model, format it, and append significance stars based on p-values
F.stat <- purrr::map(models_tA2c, ~ {
  summ <- summary(.x, vcov. = sandwich::vcovHC(.x, method = "arellano", type = "HC3"))
  
  # Compute the p-value for the F-statistic
  .p <- pf(
    summ$fstatistic["value"],
    summ$fstatistic["numdf"],
    summ$fstatistic["dendf"],
    lower.tail = FALSE
  )
  
  # Format the F-statistic value with significance stars
  sprintf(
    "%0.3f%s",
    summ$fstatistic["value"],
    dplyr::case_when(
      .p < 0.01 ~ "***",
      .p < 0.05 ~ "**",
      .p < 0.1  ~ "*",
      TRUE ~ ""
    )
  )
}) %>%
  
  # Unlist the results and prepend "F Statistic" as the first element
  unlist() %>% c("F Statistic", .)

# Extract the degrees of freedom (df) from the model summaries
df <- purrr::map(models_tA2c, ~ {
  summ <- summary(.x, vcov. = sandwich::vcovHC(.x, method = "arellano", type = "HC3"))
  
  # Combine the numerator and denominator degrees of freedom into a single string
  paste(summ$fstatistic[2:3], collapse = ";")
}) %>%
  
  # Unlist the results and prepend "df" as the first element
  unlist() %>% c("df", .)

# Generate the regression table in HTML format, suppressing warnings during generation
Table_A2c <- suppressWarnings(
  stargazer::stargazer(
    models_tA2c, type = "html",
    se = models.to.print_se,
    column.labels = model_names,
    dep.var.caption = "Votes share",
    omit.stat = "f",
    add.lines = list(F.stat, df)
  )
) %>%
  
  # Collapse the HTML output into a single string
  paste0(collapse = "")

# Convert the HTML table into a data frame and remove the first 5 rows (unnecessary headers)
Table_A2c <- xml2::read_html(Table_A2c) %>%
  rvest::html_table() %>%
  as.data.frame() %>%
  dplyr::slice(-c(1:5))

# Extract the first row as the table header names, convert it to a vector, and set the first element to a space
table_names <- as.vector(Table_A2c %>% dplyr::slice(1) %>% unlist())
table_names[1] <- " "

# Rename the columns of the table using the formatted model names and remove the first row
Table_A2c <- magrittr::set_names(Table_A2c, c(" ", model_names.to_print)) %>%
  dplyr::slice(-1)

# Identify rows where all columns are empty and get their indices
empty_lines <- Table_A2c %>%
  dplyr::transmute(dplyr::across(dplyr::everything(), ~ nchar(.) == 0)) %>%
  dplyr::rowwise() %>%
  dplyr::transmute(all(dplyr::c_across(dplyr::everything()))) %>%
  dplyr::pull(1) %>%
  which()

# Remove the identified empty rows from the table
Table_A2c %<>% dplyr::slice(-empty_lines)

# Convert the table to a flextable object and apply various styles and formatting
Table_A2c %<>% flextable::flextable() %>%
  flextable::merge_at(i = nrow(Table_A2c), j = 2:ncol(Table_A2c)) %>%
  flextable::style(
    pr_t = officer::fp_text(font.size = 9),
    part = "all",
    pr_p = officer::fp_par(line_spacing = 1, padding = 0)
  ) %>%
  
  # Add a horizontal line and automatically adjust column widths to fit content
  flextable::hline(i = 10, border = officer::fp_border()) %>%
  flextable::autofit() %>%
  flextable::fit_to_width(9)

rm(models_tA2c)

##### TABLE A2d ######


# Load the pre-saved RData file that contains the model results
load("results/TableA2d_models.RData")

# Create a vector of model names for Conflicts, Log Conflicts, Deaths, and Log Deaths across different periods
model_names <- c(
  paste("Conflicts", periods),
  paste("Log Conflicts", periods),
  paste("Deaths", periods),
  paste("Log Deaths", periods)
)

# Calculate the AIC for each model in the list of models (models_tA2d)
AIC_matrix <- models_tA2d %>%
  purrr::map(~ purrr::map(.x, AIC) %>% unlist()) %>%  # Calculate AIC for each model
  unlist() %>%  # Flatten the list to a vector
  matrix(nrow = 10, byrow = TRUE) %>%  # Reshape into a matrix with 10 rows
  as.data.frame() %>%  # Convert to a data frame
  magrittr::set_colnames(model_names) %>%  # Set column names to model names
  dplyr::mutate(k = dplyr::row_number()) %>%  # Add a column for the number of neighbors (k)
  tidyr::pivot_longer(cols = -k, names_to = "model", values_to = "AIC")  # Pivot longer to have model and AIC columns

# Plot AIC values for each model and each k
AIC_matrix %>%
  ggplot2::ggplot(ggplot2::aes(y = AIC, x = k)) +  # Create ggplot with AIC on y-axis and k on x-axis
  ggplot2::geom_point() +  # Add points for each AIC value
  ggplot2::facet_wrap(~ model, ncol = 3, scales = "free_y") +  # Create separate plots for each model
  ggplot2::scale_x_continuous(breaks = 1:10) +  # Set x-axis breaks to be the values of k
  ggplot2::geom_vline(xintercept = 6, color = "red")  # Add a vertical line at k = 6

# Save the AIC plot to a file
ggplot2::ggsave(filename = here::here("manuscript/knn/AIC_detail.png"))

# Select the model corresponding to k = 6
k <- 6
models_tA2d <- models_tA2d %>% purrr::pluck(k)

# Prepare model names for printing in the table
model_names.to_print <- c(
  paste0("Conflicts\n", periods),
  paste0("Log Conflicts\n", periods),
  paste0("Deaths\n", periods),
  paste0("Log Deaths\n", periods)
)

# Extract the standard errors for each model using heteroscedasticity-consistent (HC3) standard errors
models.to.print_se <- purrr::map(models_tA2d, ~ {
  lmtest::coeftest(.x, sandwich::vcovHC(.x, method = "arellano", type = "HC3"))[, "Std. Error"]
})

# Calculate the F-statistic for each model, format it, and append significance stars based on p-values
F.stat <- purrr::map(models_tA2d, ~ {
  summ <- summary(.x, vcov. = sandwich::vcovHC(.x, method = "arellano", type = "HC3"))
  
  # Compute the p-value for the F-statistic
  .p <- pf(
    summ$fstatistic["value"],
    summ$fstatistic["numdf"],
    summ$fstatistic["dendf"],
    lower.tail = FALSE
  )
  
  # Format the F-statistic value with significance stars
  sprintf(
    "%0.3f%s",
    summ$fstatistic["value"],
    dplyr::case_when(
      .p < 0.01 ~ "***",
      .p < 0.05 ~ "**",
      .p < 0.1  ~ "*",
      TRUE ~ ""
    )
  )
}) %>%
  unlist() %>%  # Flatten the list to a vector
  c("F Statistic", .)  # Prepend "F Statistic" to the vector

# Extract the degrees of freedom (df) from the model summaries
df <- purrr::map(models_tA2d, ~ {
  summ <- summary(.x, vcov. = sandwich::vcovHC(.x, method = "arellano", type = "HC3"))
  
  # Combine the numerator and denominator degrees of freedom into a single string
  paste(summ$fstatistic[2:3], collapse = ";")
}) %>%
  unlist() %>%  # Flatten the list to a vector
  c("df", .)  # Prepend "df" to the vector

# Generate the regression table in HTML format, suppressing warnings during generation
Table_A2d <- suppressWarnings(
  stargazer::stargazer(
    models_tA2d, type = "html",
    se = models.to.print_se,
    column.labels = model_names,
    dep.var.caption = "Votes share",
    omit.stat = "f",
    add.lines = list(F.stat, df)
  )
) %>%
  
  # Collapse the HTML output into a single string
  paste0(collapse = "")

# Convert the HTML table into a data frame and remove the first 5 rows (unnecessary headers)
Table_A2d <- xml2::read_html(Table_A2d) %>%
  rvest::html_table() %>%
  as.data.frame() %>%
  dplyr::slice(-c(1:5))

# Extract the first row as the table header names, convert it to a vector, and set the first element to a space
table_names <- as.vector(Table_A2d %>% dplyr::slice(1) %>% unlist())
table_names[1] <- " "

# Rename the columns of the table using the formatted model names and remove the first row
Table_A2d <- magrittr::set_names(Table_A2d, c(" ", model_names.to_print)) %>%
  dplyr::slice(-1)

# Identify rows where all columns are empty and get their indices
empty_lines <- Table_A2d %>%
  dplyr::transmute(dplyr::across(dplyr::everything(), ~ nchar(.) == 0)) %>%
  dplyr::rowwise() %>%
  dplyr::transmute(all(dplyr::c_across(dplyr::everything()))) %>%
  dplyr::pull(1) %>%
  which()

# Remove the identified empty rows from the table
Table_A2d %<>% dplyr::slice(-empty_lines)


# Set scientific notation off for better readability
options(scipen = 999)

# Remove the first row from the borders data (possibly a header or unwanted row)
borders <- congo.territoire.borders %>% dplyr::slice(-1)

# Create k-nearest neighbors based on the centroids of the borders
w1nb <- borders %>%
  sf::st_centroid(borders) %>%  # Calculate the centroid of each border region
  spdep::knearneigh(k = k, longlat = TRUE) %>%  # Compute k-nearest neighbors
  spdep::knn2nb()  # Convert the neighbor object to a list

# Convert the neighbors list to a weights list
listw1nb <- w1nb %>% spdep::nb2listw(style = "W")

# Calculate spatial impacts for each model and integrate the results into the final table
Table_A2d <- models_tA2d %>%
  purrr::map(~ {
    x <- .x %>% spatialreg::impacts(listw = listw1nb) %>% summary(zstats = TRUE)
    
    purrr::map2_chr(
      signif(unlist(x$impacts), 3), 
      p_stars(x$pzmat), 
      ~ paste0(.x, .y)
    ) %>%
      purrr::map2_chr(
        signif(unlist(x$se), 3), 
        ~ paste0(.x, "\n(", .y, ")")
      )
  }) %>%
  
  unlist() %>%  # Flatten the list to a vector
  matrix(ncol = 12, byrow = FALSE) %>%  # Convert to a matrix with 12 columns
  as.data.frame() %>%  # Convert to a data frame
  dplyr::mutate(` ` = c("Direct effect", "Indirect effect", "Total effect"), .before = 1) %>%  # Add labels for effects
  magrittr::set_colnames(names(Table_A2d)) %>%  # Set column names to match the existing table
  dplyr::bind_rows(
    Table_A2d %>% dplyr::slice(1:18),  # Add the new rows in the middle of the existing table
    ., 
    Table_A2d %>% dplyr::slice(19:nrow(Table_A2d))
  )

# Convert the final table to a flextable object and apply various styles and formatting
Table_A2d %<>% flextable::flextable() %>%
  flextable::merge_at(i = nrow(Table_A2d), j = 2:ncol(Table_A2d)) %>%
  flextable::style(
    pr_t = officer::fp_text(font.size = 9),
    part = "all",
    pr_p = officer::fp_par(line_spacing = 1, padding = 0)
  ) %>%
  flextable::hline(i = c(18, 21), border = officer::fp_border()) %>%
  flextable::autofit() %>%
  flextable::fit_to_width(9)

rm(models_tA2d)

##### TABLE A3 #####


# Load the pre-saved RData file that contains the model results
load("results/TableA3_models.RData")

# Create a vector of model names for different types of conflicts and deaths, including ACLED data
model_names <- c(
  paste("Conflicts"),
  paste("Log Conflicts"),
  paste("Deaths"),
  paste("Log Deaths"),
  paste("ACLED\nConflicts"),
  paste("ACLED\nLog Conflicts"),
  paste("ACLED\nDeaths"),
  paste("ACLED\nLog Deaths")
)

# Assign model names for printing in the table
model_names.to_print <- model_names

# Extract the standard errors for each model using heteroscedasticity-consistent (HC3) standard errors
models.to.print_se <- purrr::map(models_tA3, ~ {
  lmtest::coeftest(.x, sandwich::vcovHC(.x, method = "arellano", type = "HC3"))[, "Std. Error"]
})

# Calculate the F-statistic for each model, format it, and append significance stars based on p-values
F.stat <- purrr::map(models_tA3, ~ {
  summ <- summary(.x, vcov. = sandwich::vcovHC(.x, method = "arellano", type = "HC3"))
  
  # Compute the p-value for the F-statistic
  .p <- pf(
    summ$fstatistic["value"],
    summ$fstatistic["numdf"],
    summ$fstatistic["dendf"],
    lower.tail = FALSE
  )
  
  # Format the F-statistic value with significance stars
  sprintf(
    "%0.3f%s",
    summ$fstatistic["value"],
    dplyr::case_when(
      .p < 0.01 ~ "***",
      .p < 0.05 ~ "**",
      .p < 0.1  ~ "*",
      TRUE ~ ""
    )
  )
}) %>%
  unlist() %>%  # Flatten the list to a vector
  c("F Statistic", .)  # Prepend "F Statistic" to the vector

# Extract the degrees of freedom (df) from the model summaries
df <- purrr::map(models_tA3, ~ {
  summ <- summary(.x, vcov. = sandwich::vcovHC(.x, method = "arellano", type = "HC3"))
  
  # Combine the numerator and denominator degrees of freedom into a single string
  paste(summ$fstatistic[2:3], collapse = ";")
}) %>%
  unlist() %>%  # Flatten the list to a vector
  c("df", .)  # Prepend "df" to the vector

# Generate the regression table in HTML format, suppressing warnings during generation
Table_A3 <- suppressWarnings(
  stargazer::stargazer(
    models_tA3, type = "html",
    se = models.to.print_se,
    dep.var.caption = "Votes share 2011",
    omit.stat = "f",
    add.lines = list(F.stat, df)
  )
) %>%
  
  # Collapse the HTML output into a single string
  paste0(collapse = "")

# Convert the HTML table into a data frame and remove the first 5 rows (unnecessary headers)
Table_A3 <- xml2::read_html(Table_A3) %>%
  rvest::html_table() %>%
  as.data.frame() %>%
  dplyr::slice(-c(1:5))

# Set the column names of the table using the prepared model names for printing
Table_A3 <- magrittr::set_names(Table_A3, c(" ", model_names.to_print)) %>%
  dplyr::slice(-1)  # Remove the first row (original column names)

# Identify rows where all columns are empty and get their indices
empty_lines <- Table_A3 %>%
  dplyr::transmute(dplyr::across(dplyr::everything(), ~ nchar(.) == 0)) %>%
  dplyr::rowwise() %>%
  dplyr::transmute(all(dplyr::c_across(dplyr::everything()))) %>%
  dplyr::pull(1) %>%
  which()

# Remove the identified empty rows from the table
Table_A3 %<>% dplyr::slice(-empty_lines)

# Convert the final table to a flextable object and apply various styles and formatting
Table_A3 %<>% flextable::flextable() %>%
  flextable::merge_at(i = nrow(Table_A3), j = 2:ncol(Table_A3)) %>%
  flextable::style(
    pr_t = officer::fp_text(font.size = 10),
    part = "all",
    pr_p = officer::fp_par(line_spacing = 1, padding = 0)
  ) %>%
  flextable::hline(i = 12, border = officer::fp_border()) %>%
  flextable::vline(j = c(1, 5), border = officer::fp_border()) %>%
  flextable::autofit() %>%
  flextable::fit_to_width(10.5)

rm(models_tA3)

##### TABLE A4 #####

# Load the pre-saved RData file that contains the model results
load("results/TableA4_models.RData")

# Create a vector of model names for different types of conflicts and deaths, including ACLED data
model_names <- c(
  paste("Conflicts"),
  paste("Log Conflicts"),
  paste("Deaths"),
  paste("Log Deaths"),
  paste("ACLED\nConflicts"),
  paste("ACLED\nLog Conflicts"),
  paste("ACLED\nDeaths"),
  paste("ACLED\nLog Deaths")
)

# Assign model names for printing in the table
model_names.to_print <- model_names

# Extract the standard errors for each model using heteroscedasticity-consistent (HC3) standard errors
models.to.print_se <- purrr::map(models_tA4, \(x) {
  lmtest::coeftest(x, sandwich::vcovHC(x, method = "arellano", type = "HC3"))[, "Std. Error"]
})

# Calculate the F-statistic for each model, format it, and append significance stars based on p-values
F.stat <- purrr::map(models_tA4, \(x) {
  summ <- summary(x, vcov. = sandwich::vcovHC(x, method = "arellano", type = "HC3"))
  
  # Compute the p-value for the F-statistic
  .p <- pf(
    summ$fstatistic["value"],
    summ$fstatistic["numdf"],
    summ$fstatistic["dendf"],
    lower.tail = FALSE
  )
  
  # Format the F-statistic value with significance stars
  sprintf(
    "%0.3f%s",
    summ$fstatistic["value"],
    dplyr::case_when(
      .p < 0.01 ~ "***",
      .p < 0.05 ~ "**",
      .p < 0.1  ~ "*",
      TRUE ~ ""
    )
  )
}) %>%
  unlist() %>%  # Flatten the list to a vector
  c("F Statistic", .)  # Prepend "F Statistic" to the vector

# Extract the degrees of freedom (df) from the model summaries
df <- purrr::map(models_tA4, \(x) {
  summ <- summary(x, vcov. = sandwich::vcovHC(x, method = "arellano", type = "HC3"))
  
  # Combine the numerator and denominator degrees of freedom into a single string
  paste(summ$fstatistic[2:3], collapse = ";")
}) %>%
  unlist() %>%  # Flatten the list to a vector
  c("df", .)  # Prepend "df" to the vector

# Generate the regression table in HTML format, suppressing warnings during generation
Table_A4 <- suppressWarnings(
  stargazer::stargazer(
    models_tA4, type = "html",
    se = models.to.print_se,
    dep.var.caption = "Votes share 2018",
    omit.stat = "f",
    add.lines = list(F.stat, df)
  )
) %>%
  
  # Collapse the HTML output into a single string
  paste0(collapse = "")

# Convert the HTML table into a data frame and remove the first 5 rows (unnecessary headers)
Table_A4 <- xml2::read_html(Table_A4) %>%
  rvest::html_table() %>%
  as.data.frame() %>%
  dplyr::slice(-c(1:5))

# Set the column names of the table using the prepared model names for printing
Table_A4 <- magrittr::set_names(Table_A4, c(" ", model_names.to_print)) %>%
  dplyr::slice(-1)  # Remove the first row (original column names)

# Identify rows where all columns are empty and get their indices
empty_lines <- Table_A4 %>%
  dplyr::transmute(dplyr::across(dplyr::everything(), ~ nchar(.) == 0)) %>%
  dplyr::rowwise() %>%
  dplyr::transmute(all(dplyr::c_across(dplyr::everything()))) %>%
  dplyr::pull(1) %>%
  which()

# Remove the identified empty rows from the table
Table_A4 %<>% dplyr::slice(-empty_lines)

# Convert the final table to a flextable object and apply various styles and formatting
Table_A4 %<>% flextable::flextable() %>%
  flextable::merge_at(i = nrow(Table_A4), j = 2:ncol(Table_A4)) %>%
  flextable::style(
    pr_t = officer::fp_text(font.size = 10),
    part = "all",
    pr_p = officer::fp_par(line_spacing = 1, padding = 0)
  ) %>%
  flextable::hline(i = 12, border = officer::fp_border()) %>%
  flextable::vline(j = c(1, 5), border = officer::fp_border()) %>%
  flextable::autofit() %>%
  flextable::fit_to_width(10.5)

rm(models_tA4)

##### TABLE A5 #####

# Convert the ged201 object to a data frame for further manipulation
ged201_for_period_and_actors <- ged201 %>% 
  as.data.frame() %>%
  
  # Add a new column 'period' based on the date ranges that correspond to election periods
  dplyr::mutate(period = dplyr::case_when(
    date_start >= lubridate::ymd("2001-01-17") & date_end <= lubridate::ymd("2006-07-30") ~ "2006 election",
    date_start >= lubridate::ymd("2006-07-31") & date_end <= lubridate::ymd("2011-11-28") ~ "2011 election",
    date_start >= lubridate::ymd("2011-11-29") & date_end <= lubridate::ymd("2018-12-30") ~ "2018 election",
    TRUE ~ NA_character_  # Assign NA if the dates do not match any of the election periods
  )) %>%
  
  # Filter the data to keep only rows where side_a and side_b are in the actor_types$groups
  dplyr::filter(side_a %in% actor_types$groups & side_b %in% actor_types$groups)

# Select relevant columns, group by election, side_a, side_b, and index.data
conflict.groups.period.table <- ged201_for_period_and_actors %>%
  dplyr::select(election, side_a, side_b, n.deaths_a = deaths_a, n.deaths_b = deaths_b, n.deaths_civilians = deaths_civilians, index.data) %>%
  
  dplyr::group_by(election, side_a, side_b, index.data) %>%
  
  # Summarize the number of conflicts and sum of deaths
  dplyr::summarise(
    n.conflicts = dplyr::n(),  # Count number of conflicts
    dplyr::across(dplyr::starts_with("n.deaths"), ~sum(.)),  # Sum deaths for side_a, side_b, and civilians
    .groups = "drop"  # Drop the grouping after summarizing
  ) %>%
  
  # Group again by election, side_a, side_b
  dplyr::group_by(election, side_a, side_b) %>%
  
  # Summarize to get total conflicts and deaths, and count territories involved
  dplyr::summarise(
    n.conflicts = sum(n.conflicts),  # Sum the number of conflicts
    dplyr::across(dplyr::starts_with("n.deaths"), ~sum(.)),  # Sum deaths again
    territories = dplyr::n(),  # Count the number of territories
    .groups = "drop"  # Drop the grouping after summarizing
  ) %>%
  
  # Adjust the number of deaths for civilians
  dplyr::mutate(
    n.deaths_a = dplyr::case_when(side_a == "Civilians" ~ n.deaths_a + n.deaths_civilians, TRUE ~ n.deaths_a),
    n.deaths_b = dplyr::case_when(side_b == "Civilians" ~ n.deaths_b + n.deaths_civilians, TRUE ~ n.deaths_b)
  ) %>%
  
  # Remove the column for civilian deaths since it's now accounted for
  dplyr::select(-n.deaths_civilians)

# Merge the data with itself to account for conflicts between the same actors but in reverse order
conflict.groups.period.table %<>% 
  dplyr::left_join(
    conflict.groups.period.table %>% dplyr::filter(side_a != side_b), 
    by = c("election", side_a = "side_b", side_b = "side_a")
  ) %>%
  
  # Create a unique identifier for each conflict using election, side_a, and side_b
  dplyr::rowwise() %>%
  dplyr::mutate(idx = paste(sort(c(election, side_a, side_b)), collapse = ",")) %>%
  
  # Ungroup the data frame to remove rowwise operation
  dplyr::ungroup() %>%
  
  # Filter out duplicate conflicts using the unique identifier
  dplyr::filter(!duplicated(idx)) %>%
  
  # Recalculate the total number of conflicts, deaths, and territories after merging
  dplyr::rowwise() %>%
  dplyr::mutate(
    n.conflicts = sum(n.conflicts.x, n.conflicts.y, na.rm = TRUE),
    n.deaths_a = sum(n.deaths_a.x, n.deaths_b.y, na.rm = TRUE),
    n.deaths_b = sum(n.deaths_b.x, n.deaths_a.y, na.rm = TRUE),
    territories = sum(territories.x, territories.y, na.rm = TRUE)
  ) %>%
  
  # Ungroup the data frame after rowwise operation
  dplyr::ungroup()

# Combine data for side_a and side_b into a single 'group' column
conflict.groups.period.table <- dplyr::bind_rows(
  conflict.groups.period.table %>% dplyr::select(election, group = side_a, n.conflicts, n.deaths = n.deaths_a, territories),
  conflict.groups.period.table %>% dplyr::select(election, group = side_b, n.conflicts, n.deaths = n.deaths_b, territories)
) %>%
  
  # Group by election and group to summarize the total conflicts, deaths, and territories
  dplyr::group_by(election, group) %>%
  dplyr::summarise(
    dplyr::across(c(n.conflicts, n.deaths, territories), ~sum(., na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  
  # Separate the election column into multiple columns and drop the unwanted column
  tidyr::separate(election, sep = "_", into = c("drop", "election")) %>%
  dplyr::select(-drop) %>%
  
  # Reshape the data into a wide format where each election is a separate column
  tidyr::pivot_wider(names_from = "election", values_from = c("n.conflicts", "n.deaths", "territories")) %>%
  
  # Replace NA values with zeros in numeric columns
  dplyr::mutate(
    dplyr::across(tidyselect::where(is.numeric), ~ tidyr::replace_na(., 0))
  )

# Perform checks to ensure the integrity of the results

# Check if the total number of conflicts matches the number of rows in the original filtered data
stopifnot(
  (conflict.groups.period.table %>% 
     dplyr::summarise(
       dplyr::across(c(n.conflicts_2006, n.conflicts_2011, n.conflicts_2018), sum)
     ) %>% 
     unlist %>% 
     sum) == nrow(ged201_for_period_and_actors) * 2
)

# Check if the total number of deaths matches the sum of deaths in the original filtered data
stopifnot(
  (conflict.groups.period.table %>% 
     dplyr::summarise(
       dplyr::across(c(n.deaths_2006, n.deaths_2011, n.deaths_2018), sum)
     ) %>% 
     unlist %>% 
     sum) == (sum(ged201_for_period_and_actors$deaths_a) + sum(ged201_for_period_and_actors$deaths_b) + sum(ged201_for_period_and_actors$deaths_civilians[ged201_for_period_and_actors$side_b == "Civilians"]))
)

# Check if there are no conflicts involving 'Civilians' as side_a
stopifnot(all(ged201_for_period_and_actors$side_a != "Civilians"))

# Check if the groups in the actor_types list match those in the conflict groups table
stopifnot(
  all(
    (dplyr::setdiff(actor_types$groups, conflict.groups.period.table$group) %>% sort) == 
      (dplyr::intersect(
        dplyr::setdiff(actor_types$groups, ged201$side_a),
        dplyr::setdiff(actor_types$groups, ged201$side_b)
      ) %>% sort)
  )
)

# Add a 'Total' line summarizing all groups
conflict.groups.period.table %<>% 
  dplyr::bind_rows(
    conflict.groups.period.table %>% 
      dplyr::summarise(
        dplyr::across(tidyselect::where(is.numeric), sum)
      ) %>% 
      dplyr::mutate(group = "Total", .before = 1)
  )

# Create the final table (Table_A5) and format it using flextable
Table_A5 <- conflict.groups.period.table %>%
  dplyr::select(
    group, 
    dplyr::ends_with("_2006"), 
    dplyr::ends_with("_2011"), 
    dplyr::ends_with("_2018"), 
    -dplyr::starts_with("territories")
  ) %>%
  flextable::flextable() %>%
  flextable::add_header(
    values = list(
      "n.conflicts_2006" = "2006 election",
      "n.deaths_2006" = "2006 election",
      "n.conflicts_2011" = "2011 election",
      "n.deaths_2011" = "2011 election",
      "n.conflicts_2018" = "2018 election",
      "n.deaths_2018" = "2018 election"
    )
  ) %>%
  flextable::set_header_labels(
    values = list(
      "n.conflicts_2006" = "Conflict\nevents",
      "n.deaths_2006" = "Deaths",
      "n.conflicts_2011" = "Conflict\nevents",
      "n.deaths_2011" = "Deaths",
      "n.conflicts_2018" = "Conflict\nevents",
      "n.deaths_2018" = "Deaths"
    )
  ) %>%
  flextable::merge_h(part = "header") %>%
  flextable::hline(i = nrow(conflict.groups.period.table) - 1, border = officer::fp_border(width = 2)) %>%
  flextable::vline(j = c(1, 3, 5), border = officer::fp_border(width = 2)) %>%
  flextable::style(
    pr_t = officer::fp_text(font.size = 9), 
    part = "all", 
    pr_p = officer::fp_par(line_spacing = 1, padding = 1)
  ) %>%
  flextable::autofit() %>% 
  flextable::fit_to_width(6.49)


##### TABLE A6i #####


# Load the pre-saved RData file that contains the model results
load("results/TableA6i_models.RData")

# Create a vector of model names to use for labeling the output tables
model_names <- c(paste("Conflicts"),paste("Log Conflicts"),paste("Deaths"),paste("Log Deaths"),
                 paste("ACLED\nConflicts"),paste("ACLED\nLog Conflicts"),paste("ACLED\nDeaths"),paste("ACLED\nLog Deaths"))

# Duplicate the model names vector for later use in the table output
model_names.to_print <- model_names

# Calculate the standard errors for each model using a robust covariance matrix estimator (HC3) and store them in a list
models.to.print_se <- purrr::map(models_tA6i, \(x) {
  lmtest::coeftest(x, sandwich::vcovHC(x, method="arellano", type="HC3"))[,"Std. Error"]
})

# Calculate the F-statistic for each model and format it with significance stars
F.stat <- purrr::map(models_tA6i, \(x) {
  # Generate a summary of the model with a robust covariance matrix
  summ <- summary(x, vcov. = sandwich::vcovHC(x, method="arellano", type="HC3"))
  # Calculate the p-value of the F-statistic
  .p <- pf(summ$fstatistic["value"], summ$fstatistic["numdf"], summ$fstatistic["dendf"], lower.tail = FALSE)
  # Format the F-statistic value with significance stars based on the p-value
  sprintf("%0.3f%s", summ$fstatistic["value"],
          dplyr::case_when(
            .p < 0.01 ~ "***",
            .p < 0.05 ~ "**",
            .p < 0.1 ~ "*",
            TRUE ~ ""
          ))
}) %>% 
  unlist() %>%  # Flatten the list into a character vector
  c("F Statistic", .)  # Add a label for the F-statistic row

# Extract the degrees of freedom for each model and format them into a string
df <- purrr::map(models_tA6i, \(x) {
  # Generate a summary of the model with a robust covariance matrix
  summ <- summary(x, vcov. = sandwich::vcovHC(x, method="arellano", type="HC3"))
  # Concatenate the numerator and denominator degrees of freedom
  paste(summ$fstatistic[2:3], collapse = ";")
}) %>% 
  unlist() %>%  # Flatten the list into a character vector
  c("df", .)  # Add a label for the degrees of freedom row

# Generate the HTML table for the regression models using stargazer, adding custom lines for F-statistics and degrees of freedom
Table_A6i <- suppressWarnings(
  stargazer::stargazer(models_tA6i, type = "html",
                       se = models.to.print_se,
                       dep.var.caption = "Votes share_2006 2006",
                       omit.stat = "f",
                       add.lines = list(F.stat, df))
) %>% 
  paste0(collapse = "")  # Collapse the list of HTML lines into a single string

# Parse the HTML table into a data frame and remove the first five rows (which contain unwanted metadata)
Table_A6i <- xml2::read_html(Table_A6i) %>% 
  rvest::html_table() %>% 
  as.data.frame() %>% 
  dplyr::slice(-c(1:5))

# Rename the columns of the table using the model names vector and remove the first row (which contains the original column names)
Table_A6i <- 
  magrittr::set_names(Table_A6i, c(" ", model_names.to_print)) %>% 
  dplyr::slice(-1)

# Identify rows that are completely empty (i.e., all cells are blank)
empty_lines <- Table_A6i %>% 
  dplyr::transmute(across(dplyr::everything(), \(x) nchar(x) == 0)) %>% 
  dplyr::rowwise() %>% 
  dplyr::transmute(all(dplyr::c_across(dplyr::everything()))) %>% 
  dplyr::pull(1) %>% 
  which()

# Remove the identified empty rows from the table
Table_A6i %<>% dplyr::slice(-empty_lines)

# Convert the data frame into a flextable and apply various formatting options
Table_A6i %<>% 
  flextable::flextable() %>% 
  flextable::merge_at(i = nrow(Table_A6i), j = 2:ncol(Table_A6i)) %>% 
  flextable::style(pr_t = officer::fp_text(font.size = 10), part = "all", pr_p = officer::fp_par(line_spacing = 1, padding = 0)) %>% 
  flextable::hline(i = 10, border = officer::fp_border()) %>% 
  flextable::vline(j = c(1, 5), border = officer::fp_border()) %>% 
  flextable::autofit() %>% 
  flextable::fit_to_width(10.5)

rm(models_tA6i)

##### TABLE A6ii #####


# Load the pre-saved RData file that contains the model results
load("results/TableA6ii_models.RData")


# Create a vector of model names, each representing different types of conflicts and deaths
model_names <- c(
  paste("Conflicts"),          # Name for Conflicts model
  paste("Log Conflicts"),      # Name for Log Conflicts model
  paste("Deaths"),             # Name for Deaths model
  paste("Log Deaths"),         # Name for Log Deaths model
  paste("ACLED\nConflicts"),   # Name for ACLED Conflicts model
  paste("ACLED\nLog Conflicts"), # Name for ACLED Log Conflicts model
  paste("ACLED\nDeaths"),      # Name for ACLED Deaths model
  paste("ACLED\nLog Deaths")   # Name for ACLED Log Deaths model
)

# Assign the model names to a new variable for printing
model_names.to_print <- model_names

# Extract standard errors from each model using robust standard errors (Arellano method) and save them in a list
models.to.print_se <- purrr::map(models_tA6ii, \(x) {
  lmtest::coeftest(x, sandwich::vcovHC(x, method = "arellano", type = "HC3"))[,"Std. Error"]
})

# Compute F-statistics for each model, format them with significance stars, and combine into a single vector
F.stat <- purrr::map(models_tA6ii, \(x) {
  # Get the summary of the model with robust standard errors
  summ <- summary(x, vcov. = sandwich::vcovHC(x, method = "arellano", type = "HC3"))
  
  # Compute the p-value of the F-statistic
  p_val <- pf(
    summ$fstatistic["value"],
    summ$fstatistic["numdf"],
    summ$fstatistic["dendf"],
    lower.tail = FALSE
  )
  
  # Format the F-statistic and add significance stars based on p-value
  sprintf(
    "%0.3f%s",
    summ$fstatistic["value"],
    dplyr::case_when(
      p_val < 0.01 ~ "***",    # Add three stars for p-value < 0.01
      p_val < 0.05 ~ "**",     # Add two stars for p-value < 0.05
      p_val < 0.1 ~ "*",       # Add one star for p-value < 0.1
      TRUE ~ ""                # No stars for p-value >= 0.1
    )
  )
}) %>%
  unlist() %>%                # Unlist the results into a single vector
  c("F Statistic", .)         # Add a label "F Statistic" at the beginning

# Extract degrees of freedom for each model and format them into a single vector
df <- purrr::map(models_tA6ii, \(x) {
  # Get the summary of the model with robust standard errors
  summ <- summary(x, vcov. = sandwich::vcovHC(x, method = "arellano", type = "HC3"))
  
  # Concatenate numerator and denominator degrees of freedom
  paste(summ$fstatistic[2:3], collapse = ";")
}) %>%
  unlist() %>%                # Unlist the results into a single vector
  c("df", .)                  # Add a label "df" at the beginning

# Create an HTML table using stargazer, suppress warnings during the process
Table_A6ii <- suppressWarnings(
  stargazer::stargazer(
    models_tA6ii,
    type = "html",               # Specify output format as HTML
    se = models.to.print_se,     # Provide standard errors
    dep.var.caption = "Votes share 2011", # Set dependent variable caption
    omit.stat = "f",             # Omit F-statistic from default output
    add.lines = list(F.stat, df) # Add custom lines for F-statistics and degrees of freedom
  )
) %>% paste0(collapse = "")      # Concatenate the HTML code into a single string

# Parse the HTML string into a data frame, extract the table content, and remove unnecessary rows
Table_A6ii <- xml2::read_html(Table_A6ii) %>% 
  rvest::html_table() %>% 
  as.data.frame() %>% 
  dplyr::slice(-c(1:5))          # Remove the first 5 rows (header and irrelevant content)

# Rename the columns of the data frame, using model names for the columns
Table_A6ii <- magrittr::set_names(Table_A6ii, c(" ", model_names.to_print)) %>% 
  dplyr::slice(-1)              # Remove the first row after renaming (redundant content)

# Identify empty rows by checking if all characters in a row are empty, and find their indices
empty_lines <- Table_A6ii %>% 
  dplyr::transmute(across(dplyr::everything(), \(x) { nchar(x) == 0 })) %>% 
  dplyr::rowwise() %>% 
  dplyr::transmute(all(dplyr::c_across(dplyr::everything()))) %>% 
  dplyr::pull(1) %>% 
  which()                       # Get the indices of completely empty rows

# Remove the empty rows from the table
Table_A6ii <- dplyr::slice(Table_A6ii, -empty_lines)

# Convert the cleaned table to a flextable object and apply formatting
Table_A6ii <- Table_A6ii %>% 
  flextable::flextable() %>% 
  flextable::merge_at(i = nrow(Table_A6ii), j = 2:ncol(Table_A6ii)) %>%  # Merge the last row for formatting
  flextable::style(
    pr_t = officer::fp_text(font.size = 9), 
    part = "all", 
    pr_p = officer::fp_par(line_spacing = 1, padding = 0)
  ) %>% 
  flextable::hline(i = 20, border = officer::fp_border()) %>%  # Add a horizontal line at row 20
  flextable::vline(j = c(1, 5), border = officer::fp_border()) %>%  # Add vertical lines at specified columns
  flextable::autofit() %>%       # Adjust column widths to fit content
  flextable::fit_to_width(10.5)  # Set the table width to 10.5 units

rm(models_tA6ii)

##### TABLE A6iii #####


# Load the pre-saved RData file that contains the model results
load("results/TableA6iii_models.RData")

# Create a vector of model names, some with line breaks for display
model_names <- c(
  paste("Conflicts"),
  paste("Log Conflicts"),
  paste("Deaths"),
  paste("Log Deaths"),
  paste("ACLED\nConflicts"),
  paste("ACLED\nLog Conflicts"),
  paste("ACLED\nDeaths"),
  paste("ACLED\nLog Deaths")
)

# Copy model names to a new object for later use in table creation
model_names.to_print <- model_names

# Compute standard errors for each model using the coeftest function with robust covariance matrix estimation
models.to.print_se <- purrr::map(models_tA6iii, \(x) {
  lmtest::coeftest(x, sandwich::vcovHC(x, method="arellano", type="HC3"))[, "Std. Error"]
})

# Calculate F-statistics for each model and format them with significance stars
F.stat <- purrr::map(models_tA6iii, \(x) {
  summ <- summary(x, vcov.=sandwich::vcovHC(x, method="arellano", type="HC3"))
  # Compute the p-value for the F-statistic
  p_value <- pf(summ$fstatistic["value"], summ$fstatistic["numdf"], summ$fstatistic["dendf"], lower.tail = FALSE)
  
  # Format the F-statistic value with significance stars
  sprintf(
    "%0.3f%s",
    summ$fstatistic["value"],
    dplyr::case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.1 ~ "*",
      TRUE ~ ""
    )
  )
}) %>% 
  # Unlist the results into a vector
  unlist() %>% 
  # Add a label "F Statistic" as the first element in the vector
  c("F Statistic", .)

# Extract the degrees of freedom for each model's F-statistic and format them as text
df <- purrr::map(models_tA6iii, \(x) {
  summ <- summary(x, vcov.=sandwich::vcovHC(x, method="arellano", type="HC3"))
  
  # Combine the numerator and denominator degrees of freedom into a single string
  paste(summ$fstatistic[2:3], collapse=";")
}) %>% 
  # Unlist the results into a vector
  unlist() %>% 
  # Add a label "df" as the first element in the vector
  c("df", .)

# Create an HTML regression table with additional statistics using stargazer
Table_A6iii <- suppressWarnings(
  stargazer::stargazer(
    models_tA6iii,
    type="html",
    se=models.to.print_se,
    dep.var.caption = "Votes share 2018",
    omit.stat = "f",
    add.lines = list(F.stat, df)
  )
) %>% 
  # Collapse the output into a single string
  paste0(collapse = "")

# Parse the HTML table into a data frame, removing the first 5 rows (header rows)
Table_A6iii <- xml2::read_html(Table_A6iii) %>% 
  rvest::html_table() %>% 
  as.data.frame() %>% 
  dplyr::slice(-c(1:5))

# Assign new column names to the table, using the vector of model names
Table_A6iii <- magrittr::set_names(Table_A6iii, c(" ", model_names.to_print)) %>% 
  # Remove the first row, which now contains old column names
  dplyr::slice(-1)

# Identify and remove rows where all cells are empty
empty_lines <- Table_A6iii %>% 
  dplyr::transmute(dplyr::across(dplyr::everything(), \(x) {nchar(x) == 0})) %>% 
  dplyr::rowwise() %>% 
  dplyr::transmute(all(dplyr::c_across(dplyr::everything()))) %>% 
  dplyr::pull(1) %>% 
  which()

Table_A6iii <- dplyr::slice(Table_A6iii, -empty_lines)

# Format the table as a flextable, applying various styles and adjustments
Table_A6iii <- Table_A6iii %>% 
  flextable::flextable() %>% 
  # Merge cells in the last row across all columns except the first
  flextable::merge_at(i = nrow(Table_A6iii), j = 2:ncol(Table_A6iii)) %>% 
  # Apply text styling to the entire table
  flextable::style(
    pr_t = officer::fp_text(font.size = 9),
    part = "all",
    pr_p = officer::fp_par(line_spacing = 1, padding = 0)
  ) %>% 
  # Add a horizontal line at row 30
  flextable::hline(i = 30, border = officer::fp_border()) %>% 
  # Add vertical lines between specific columns
  flextable::vline(j = c(1, 5), border = officer::fp_border()) %>% 
  # Automatically adjust column widths to fit the content
  flextable::autofit() %>% 
  # Adjust the table width to a specified value
  flextable::fit_to_width(10.5)

rm(models_tA6iii)


##### TABLE A8i #####


# Load the pre-saved RData file that contains the model results
load("results/TableA8i_models.RData")


# Create a list of model names
model_names <- c(
  paste("Conflicts"),
  paste("Log Conflicts"),
  paste("Deaths"),
  paste("Log Deaths"),
  paste("ACLED\nConflicts"),
  paste("ACLED\nLog Conflicts"),
  paste("ACLED\nDeaths"),
  paste("ACLED\nLog Deaths")
)

# Store the model names for printing
model_names.to_print <- model_names

# Compute standard errors for each model using robust covariance matrices
models.to.print_se <- purrr::map(models_tA8i, \(x) {
  lmtest::coeftest(x, sandwich::vcovHC(x, method = "arellano", type = "HC3"))[, "Std. Error"]
})

# Calculate F-statistics for each model and annotate them with significance levels
F.stat <- purrr::map(models_tA8i, \(x) {
  summ <- summary(x, vcov. = sandwich::vcovHC(x, method = "arellano", type = "HC3"))
  p_value <- pf(summ$fstatistic["value"], summ$fstatistic["numdf"], summ$fstatistic["dendf"], lower.tail = FALSE)
  sprintf("%0.3f%s", summ$fstatistic["value"],
          dplyr::case_when(
            p_value < 0.01 ~ "***",
            p_value < 0.05 ~ "**",
            p_value < 0.1 ~ "*",
            TRUE ~ ""
          ))
}) %>%
  unlist %>%
  c("F Statistic", .)

# Extract degrees of freedom for each model
df <- purrr::map(models_tA8i, \(x) {
  summ <- summary(x, vcov. = sandwich::vcovHC(x, method = "arellano", type = "HC3"))
  paste(summ$fstatistic[2:3], collapse = ";")
}) %>%
  unlist %>%
  c("df", .)

# Generate an HTML table using stargazer and add custom lines for F-statistics and degrees of freedom
Table_A8i <- suppressWarnings(stargazer::stargazer(models_tA8i, type = "html",
                                                   se = models.to.print_se,
                                                   dep.var.caption = "Votes share 2006",
                                                   omit.stat = "f",
                                                   add.lines = list(F.stat, df))) %>%
  paste0(collapse = "")

# Convert the HTML table to a data frame and remove the first 5 rows
Table_A8i <- rvest::read_html(Table_A8i) %>%
  rvest::html_table() %>%
  as.data.frame() %>%
  dplyr::slice(-c(1:5))

# Extract the first row as column names, and adjust the first column name
table_names <- as.vector(Table_A8i %>%
                           dplyr::slice(1) %>%
                           unlist)
table_names[1] <- " "

# Update the labels in the first column of the table
Table_A8i %<>% make_dyad_labels(var = names(Table_A8i)[1])

# Set the column names and remove the first row
Table_A8i <- magrittr::set_names(Table_A8i, c(" ", model_names.to_print)) %>%
  dplyr::slice(-1)

# Identify and remove empty rows from the table
empty_lines <- Table_A8i %>%
  dplyr::transmute(across(dplyr::everything(), \(x) nchar(x) == 0)) %>%
  dplyr::rowwise() %>%
  dplyr::transmute(base::all(dplyr::c_across(dplyr::everything()))) %>%
  dplyr::pull(1) %>%
  which()

Table_A8i %<>% dplyr::slice(-empty_lines)

# Convert the table to a flextable and apply various styles and formatting
Table_A8i %<>%
  flextable::flextable() %>%
  flextable::merge_at(i = nrow(Table_A8i), j = 2:ncol(Table_A8i)) %>%
  flextable::style(pr_t = officer::fp_text(font.size = 10), part = "all",
                   pr_p = officer::fp_par(line_spacing = 1, padding = 0)) %>%
  flextable::hline(i = 28, border = officer::fp_border()) %>%
  flextable::vline(j = c(1, 5), border = officer::fp_border()) %>%
  flextable::autofit() %>%
  flextable::fit_to_width(10.5)



rm(models_tA8i)


##### TABLE A8ib ######



# Load the pre-saved RData file that contains the model results
load("results/TableA8ib_models.RData")

# Define names for the models to use later for plotting
model_names <- c(paste("Conflicts"), paste("Log Conflicts"), paste("Deaths"), paste("Log Deaths"))

# Copy model names for printing
model_names.to_print <- model_names

# Calculate the AIC (Akaike Information Criterion) for each model across k values and plot the results
models_tA8ib %>% 
  purrr::map(\(x) purrr::map(x, AIC) %>% unlist) %>% 
  unlist %>% 
  matrix(nrow = 10, byrow = TRUE) %>% 
  as.data.frame() %>% 
  magrittr::set_colnames(model_names) %>%
  dplyr::mutate(k = dplyr::row_number()) %>%
  tidyr::pivot_longer(cols = -k, names_to = "model", values_to = "AIC") %>%
  ggplot2::ggplot(ggplot2::aes(y = AIC, x = k)) + 
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~model, ncol = 3, scales = "free_y") +
  ggplot2::scale_x_continuous(breaks = 1:10) + 
  ggplot2::geom_vline(xintercept = 6, color = "red")

# Save the AIC plot as a PNG file
ggplot2::ggsave(filename = here::here("manuscript/knn/table_8ib_AIC_detail.png"))

# Select the model corresponding to k = 6 for further analysis
k <- 6
models_tA8ib <- models_tA8ib %>% purrr::pluck(k)

# Prepare the standard errors of the model coefficients for each model
models.to.print_se <- purrr::map(models_tA8ib, \(model) {
  lmtest::coeftest(model, sandwich::vcovHC(model, method = "arellano", type = "HC3"))[, "Std. Error"]
})

# Calculate the F-statistics for each model and format them with significance stars
F.stat <- purrr::map(models_tA8ib, \(model) {
  summ <- summary(model, vcov. = sandwich::vcovHC(model, method = "arellano", type = "HC3"))
  .p <- stats::pf(summ$fstatistic["value"], summ$fstatistic["numdf"], summ$fstatistic["dendf"], lower.tail = FALSE)
  sprintf("%0.3f%s", summ$fstatistic["value"],
          dplyr::case_when(.p < 0.01 ~ "***",
                           .p < 0.05 ~ "**",
                           .p < 0.1 ~ "*",
                           TRUE ~ ""))
}) %>% 
  unlist %>% 
  c("F Statistic", .)

# Calculate the degrees of freedom for each model
df <- purrr::map(models_tA8ib, \(model) {
  summ <- summary(model, vcov. = sandwich::vcovHC(model, method = "arellano", type = "HC3"))
  paste(summ$fstatistic[2:3], collapse = ";")
}) %>% 
  unlist %>% 
  c("df", .)

# Generate an HTML table summarizing the model results using stargazer
Table_A8ib <- suppressWarnings(stargazer::stargazer(models_tA8ib, type = "html",
                                                    se = models.to.print_se,
                                                    column.labels = model_names, dep.var.caption = "Votes share",
                                                    omit.stat = "f",
                                                    add.lines = list(F.stat, df))) %>% 
  paste0(collapse = "")

# Convert the HTML table to a data frame and clean it up
Table_A8ib <- xml2::read_html(Table_A8ib) %>% 
  rvest::html_table() %>% 
  as.data.frame() %>% 
  dplyr::slice(-c(1:5))

# Adjust table column names
table_names <- as.vector(Table_A8ib %>% dplyr::slice(1) %>% unlist)
table_names[1] <- " "
Table_A8ib <- magrittr::set_names(Table_A8ib, c(" ", model_names.to_print)) %>% 
  dplyr::slice(-1)

# Identify and remove empty rows from the table
empty_lines <- Table_A8ib %>% 
  dplyr::transmute(across(dplyr::everything(), \(x) nchar(x) == 0)) %>% 
  dplyr::rowwise() %>% 
  dplyr::transmute(all(dplyr::c_across(dplyr::everything()))) %>% 
  dplyr::pull(1) %>% 
  which()

Table_A8ib %<>% dplyr::slice(-empty_lines)


# Update territorial borders data for further processing
borders <- congo.territoire.borders %>% dplyr::slice(-1)

# Recalculate spatial weights based on the chosen k value
w1nb <- borders %>% 
  sf::st_centroid(borders) %>% 
  spdep::knearneigh(k = k, longlat = TRUE) %>% 
  spdep::knn2nb()

listw1nb <- w1nb %>% spdep::nb2listw(style = "W")

# Calculate the spatial impacts of the models and format them into a table
Table_A8ib <- models_tA8ib %>% 
  purrr::map(\(model) { 
    x <- model %>% spatialreg::impacts(listw = listw1nb) %>% summary(zstats = TRUE)
    
    purrr::map2_chr(signif(unlist(x$impacts), 3), p_stars(as.vector(x$pzmat)), \(val, star) paste0(val, star)) %>% 
      purrr::map2_chr(signif(unlist(x$se), 3), \(val, se) paste0(val, "\n(", se, ")"))
  }) %>% 
  unlist() %>% 
  matrix(., ncol = 4, byrow = FALSE, dimnames = list(names(.)[1:27])) %>% 
  as.data.frame() %>% 
  dplyr::mutate(., ` ` = stringr::str_replace(stringr::str_to_sentence(rownames(.)), "\\.", " "), .before = 1) %>% 
  magrittr::set_colnames(names(Table_A8ib)) %>% 
  dplyr::bind_rows(Table_A8ib %>% dplyr::slice(1:38), ., Table_A8ib %>% dplyr::slice(39:nrow(Table_A8ib))) %>% 
  magrittr::set_rownames(NULL)

# Further clean up the table and prepare it for final output
Table_A8ib <- dplyr::bind_rows(
  Table_A8ib %>% dplyr::slice(1:65) %>% 
    tidyr::separate(1, c("a", "b"), "\\.|\\s") %>% 
    make_dyad_labels(var = "b") %>% 
    make_dyad_labels(var = "a") %>% 
    tidyr::replace_na(replace = list(b = "")) %>% 
    tidyr::unite(` `, a, b, sep = " ", remove = TRUE),
  Table_A8ib %>% dplyr::slice(66:nrow(Table_A8ib))
)

# Remove additional unwanted rows from the table
Table_A8ib %<>% dplyr::slice(-c(39:56))

# Finalize the table formatting and style using flextable
Table_A8ib %<>% 
  flextable::flextable() %>% 
  flextable::merge_at(i = nrow(Table_A8ib), j = 2:ncol(Table_A8ib)) %>%
  flextable::style(pr_t = officer::fp_text(font.size = 9), part = "all", pr_p = officer::fp_par(line_spacing = 1, padding = 0)) %>% 
  flextable::hline(i = c(38, 47), border = officer::fp_border()) %>% 
  flextable::autofit() %>% 
  flextable::fit_to_width(9)



rm(models_tA8ib)



##### TABLE A8ic ######




# Load the pre-saved RData file that contains the model results
load("results/TableA8ic_models.RData")


# Define model names for easier interpretation
model_names <- c(paste("Conflicts"), paste("Log Conflicts"), paste("Deaths"), paste("Log Deaths"))

# Assign model names for display
model_names.to_print <- model_names

# Compute and visualize the AIC values for each model across the range of k values
models_tA8ic %>%
  purrr::map(\(x) purrr::map(x, AIC) %>% unlist) %>%
  unlist %>% 
  matrix(nrow = 10, byrow = TRUE) %>%
  as.data.frame() %>%
  magrittr::set_names(model_names) %>%
  dplyr::mutate(k = dplyr::row_number()) %>%
  tidyr::pivot_longer(cols = -k, names_to = "model", values_to = "AIC") %>%
  ggplot2::ggplot(ggplot2::aes(y = AIC, x = k)) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~model, ncol = 3, scales = "free_y") +
  ggplot2::scale_x_continuous(breaks = 1:10) +
  ggplot2::geom_vline(xintercept = 6, color = "red") 
# Save the AIC plot to a PNG file
ggplot2::ggsave(filename = here::here("manuscript/knn/table_8ic_AIC_detail.png"))

# Select the model corresponding to k = 6
k <- 6
models_tA8ic <- models_tA8ic %>% purrr::pluck(k)
model_names.to_print <- model_names

# Compute F-statistics for each model
F.stat <- models_tA8ic %>% purrr::map(\(x) {
  summ <- summary(x, vcov. = vcovHC(x, method = "arellano", type = "HC3"))
  .p <- pf(summ$fstatistic["value"], summ$fstatistic["numdf"], summ$fstatistic["dendf"], lower.tail = FALSE)
  sprintf("%0.3f%s", summ$fstatistic["value"],
          dplyr::case_when(.p < 0.01 ~ "***",
                           .p < 0.05 ~ "**",
                           .p < 0.1 ~ "*",
                           TRUE ~ ""))
}) %>% unlist %>% c("F Statistic", .)

# Compute degrees of freedom for each model
df <- models_tA8ic %>% purrr::map(\(x) {
  summ <- summary(x, vcov. = vcovHC(x, method = "arellano", type = "HC3"))
  paste(summ$fstatistic[2:3], collapse = ";")
}) %>% unlist %>% c("df", .)

# Create a summary table with stargazer, adding F-statistic and df as additional lines
Table_A8ic <- suppressWarnings(stargazer::stargazer(models_tA8ic, type = "html",
                                                    column.labels = model_names, dep.var.caption = "Votes share",
                                                    omit.stat = "f", add.lines = list(F.stat, df))) %>%
  paste0(collapse = "")

# Convert the stargazer output to a data frame and clean up the table
Table_A8ic <- Table_A8ic %>%
  xml2::read_html() %>%
  rvest::html_table() %>%
  as.data.frame() %>%
  dplyr::slice(-c(1:5))

# Extract and clean the table names
table_names <- as.vector(Table_A8ic %>% dplyr::slice(1) %>% unlist)
table_names[1] <- " "
Table_A8ic <- magrittr::set_names(Table_A8ic, c(" ", model_names.to_print)) %>% dplyr::slice(-1)

# Identify and remove empty lines in the table
empty_lines <- Table_A8ic %>% 
  dplyr::transmute(across(dplyr::everything(), ~ nchar(.) == 0)) %>% 
  dplyr::rowwise() %>% 
  dplyr::transmute(all(dplyr::c_across(dplyr::everything()))) %>% 
  dplyr::pull(1) %>% 
  which()

Table_A8ic %<>% dplyr::slice(-empty_lines)

# Set scientific notation off
options(scipen = 999)

# Recompute the spatial weights for k = 6
borders <- congo.territoire.borders %>% dplyr::slice(-1)  
w1nb <- borders %>%
  sf::st_centroid(borders) %>% 
  spdep::knearneigh(k = k, longlat = TRUE) %>% 
  spdep::knn2nb() 

listw1nb <- w1nb %>% spdep::nb2listw(style = "W")

# Compute and merge the impact estimates for each model into the summary table
Table_A8ic <- models_tA8ic %>% purrr::map(\(x) { 
  impacts_summary <- x %>%
    spatialreg::impacts(listw = listw1nb) %>%
    summary(zstats = TRUE)
 
  purrr::map2_chr(signif(unlist(impacts_summary$impacts), 3), 
                                    p_stars(as.vector(impacts_summary$pzmat)), 
                                    ~ paste0(.x, .y)) %>%
  purrr::map2_chr(signif(unlist(impacts_summary$se), 3), 
                                ~ paste0(.x, "\n(", .y, ")"))
  
  
}) %>%
  unlist %>% 
  matrix(., ncol = 4, byrow = FALSE, dimnames = list(names(.)[1:33])) %>%
  as.data.frame() %>%
  dplyr::mutate(` ` = stringr::str_replace(stringr::str_to_sentence(rownames(.)), "\\.", " "), .before = 1) %>%
  magrittr::set_names(names(Table_A8ic)) %>%
  dplyr::bind_rows(Table_A8ic %>% dplyr::slice(1:44), ., Table_A8ic %>% dplyr::slice(45:nrow(Table_A8ic))) %>%
  magrittr::set_rownames(NULL)

# Further process and finalize the summary table
Table_A8ic <- dplyr::bind_rows(
  Table_A8ic %>%
    dplyr::slice(1:77) %>%
    tidyr::separate(1, c("a", "b"), "\\.|\\s") %>%
    make_dyad_labels(var = "b") %>%
    make_dyad_labels(var = "a") %>%
    tidyr::replace_na(replace = list(b = "")) %>%
    tidyr::unite(` `, a, b, sep = " ", remove = TRUE),
  
  Table_A8ic %>%
    dplyr::slice(78:nrow(Table_A8ic))
)

Table_A8ic %<>% dplyr::slice(-c(45:66))

# Convert the final summary table to a flextable for formatting and output
Table_A8ic %<>% flextable::flextable() %>%
  flextable::merge_at(i = nrow(Table_A8ic), j = 2:ncol(Table_A8ic)) %>%
  flextable::style(pr_t = officer::fp_text(font.size = 9), part = "all", pr_p = officer::fp_par(line_spacing = 1, padding = 0)) %>%
  flextable::hline(i = c(44, 55), border = officer::fp_border()) %>%
  flextable::autofit() %>%
  flextable::fit_to_width(9)

rm(models_tA8ic)


##### TABLE A8id ######


# Load the pre-saved RData file that contains the model results
load("results/TableA8id_models.RData")

# Define a vector of model names for identification
model_names <- c(paste("Conflicts"), paste("Log Conflicts"), paste("Deaths"), paste("Log Deaths"))

# Create a copy of model_names for printing or other purposes
model_names.to_print <- model_names

# Extract AIC values from models, reshape data, and plot
models_tA8id %>%
  # Apply AIC function to each model, then unlist the results
  purrr::map(\(x) { purrr::map(x, AIC) %>% unlist() }) %>%
  unlist() %>%
  # Convert the AIC values into a matrix with 10 rows
  matrix(nrow = 10, byrow = TRUE) %>%
  as.data.frame() %>%
  # Set column names using the model_names vector
  magrittr::set_colnames(model_names) %>%
  # Add a column 'k' that numbers each row
  dplyr::mutate(k = dplyr::row_number()) %>%
  # Reshape data from wide to long format for ggplot
  tidyr::pivot_longer(cols = -k, names_to = "model", values_to = "AIC") %>%
  # Begin a ggplot object with AIC values on the y-axis and 'k' on the x-axis
  ggplot2::ggplot(ggplot2::aes(y = AIC, x = k)) +
  # Add points for each AIC value
  ggplot2::geom_point() +
  # Facet the plot by model name with 3 columns and free y scales
  ggplot2::facet_wrap(~model, ncol = 3, scales = "free_y") +
  # Customize the x-axis to display breaks at each integer from 1 to 10
  ggplot2::scale_x_continuous(breaks = 1:10) +
  # Add a vertical red line at x = 6
  ggplot2::geom_vline(xintercept = 6, color = "red")

# Save the plot to a file in the specified directory
ggplot2::ggsave(filename = here::here("manuscript/knn/table_8id_AIC_detail.png"))

# Select the 6th model from the list of models
k <- 6
models_tA8id <- models_tA8id %>% purrr::pluck(k)
model_names.to_print <- model_names

# Compute F-statistics for each model with Arellano robust standard errors
F.stat <- models_tA8id %>%
  purrr::map(\(x) {
    # Compute summary with robust standard errors
    summ <- summary(x, vcov. = sandwich::vcovHC(x, method = "arellano", type = "HC3"))
    # Calculate the p-value for the F-statistic
    p_val <- pf(summ$fstatistic["value"], summ$fstatistic["numdf"], summ$fstatistic["dendf"], lower.tail = FALSE)
    # Return the formatted F-statistic with significance stars
    sprintf("%0.3f%s", summ$fstatistic["value"],
            dplyr::case_when(
              p_val < 0.01 ~ "***",
              p_val < 0.05 ~ "**",
              p_val < 0.1 ~ "*",
              TRUE ~ ""))
  }) %>%
  unlist() %>%
  # Add a label for the F-statistic row
  c("F Statistic", .)

# Compute degrees of freedom for each model
df <- models_tA8id %>%
  purrr::map(\(x) {
    # Compute summary with robust standard errors
    summ <- summary(x, vcov. = sandwich::vcovHC(x, method = "arellano", type = "HC3"))
    # Concatenate numerator and denominator degrees of freedom
    paste(summ$fstatistic[2:3], collapse = ";")
  }) %>%
  unlist() %>%
  # Add a label for the degrees of freedom row
  c("df", .)

# Generate a table of model summaries in HTML format using stargazer
Table_A8id <- suppressWarnings(
  stargazer::stargazer(
    models_tA8id,
    type = "html",
    column.labels = model_names,
    dep.var.caption = "Votes share",
    omit.stat = "f",
    add.lines = list(F.stat, df)
  )
) %>%
  # Convert the HTML output to a data frame
  paste0(collapse = "") %>%
  xml2::read_html() %>%
  rvest::html_table() %>%
  as.data.frame() %>%
  # Remove the first five rows (metadata)
  dplyr::slice(-c(1:5))

# Extract and clean table column names
table_names <- as.vector(Table_A8id %>% dplyr::slice(1) %>% unlist())
table_names[1] <- " "
# Set the cleaned column names and remove the first row
Table_A8id <- Table_A8id %>%
  magrittr::set_names(c(" ", model_names.to_print)) %>%
  dplyr::slice(-1)

# Identify and remove empty lines from the table
empty_lines <- Table_A8id %>%
  dplyr::transmute(across(dplyr::everything(), \(x) nchar(x) == 0)) %>%
  dplyr::rowwise() %>%
  dplyr::transmute(all(dplyr::c_across(dplyr::everything()))) %>%
  dplyr::pull(1) %>%
  which()
Table_A8id %<>% dplyr::slice(-empty_lines)


# Set options to disable scientific notation
options(scipen = 999)

# Remove the first row from borders and compute centroids, then create neighbors and weights list
borders <- congo.territoire.borders %>% dplyr::slice(-1)
w1nb <- borders %>%
  sf::st_centroid(borders) %>%
  spdep::knearneigh(k = k, longlat = TRUE) %>%
  spdep::knn2nb()
listw1nb <- spdep::nb2listw(w1nb, style = "W")

# Calculate spatial impacts for each model and integrate them into the table
Table_A8id <- models_tA8id %>%
  purrr::map(\(x) {
    # Calculate spatial impacts and extract summary statistics
    impacts <- x %>% spatialreg::impacts(listw = listw1nb) %>%
      summary(zstats = TRUE)
    # Format the impacts and standard errors with significance stars
    purrr::map2_chr(
      signif(unlist(impacts$impacts), 3),
      p_stars(as.vector(impacts$pzmat)),
      \(impact, star) paste0(impact, star)
    ) %>%
      purrr::map2_chr(
        signif(unlist(impacts$se), 3),
        \(impact, se) paste0(impact, "\n(", se, ")")
      )
  }) %>%
  unlist() %>%
  # Convert the results to a data frame and integrate into the existing table
  matrix(ncol = 4, byrow = FALSE, dimnames = list(names(.)[1:39])) %>%
  as.data.frame() %>%
  dplyr::mutate(` ` = stringr::str_replace(stringr::str_to_sentence(rownames(.)), "\\.", " "), .before = 1) %>%
  magrittr::set_colnames(names(Table_A8id)) %>%
  dplyr::bind_rows(Table_A8id %>% dplyr::slice(1:50), ., Table_A8id %>% dplyr::slice(51:nrow(Table_A8id))) %>%
  magrittr::set_rownames(NULL)

# Further clean and format the table
Table_A8id <- dplyr::bind_rows(
  Table_A8id %>%
    dplyr::slice(1:89) %>%
    tidyr::separate(1, c("a", "b"), "\\.|\\s") %>%
    make_dyad_labels(var = "b") %>%
    make_dyad_labels(var = "a") %>%
    tidyr::replace_na(replace = list(b = "")) %>%
    tidyr::unite(` `, a, b, sep = " ", remove = TRUE),
  Table_A8id %>% dplyr::slice(90:nrow(Table_A8id))
)
Table_A8id %<>% dplyr::slice(-c(51:76))

# Convert the final table to a flextable object and apply formatting
Table_A8id %<>%
  flextable::flextable() %>%
  flextable::merge_at(i = nrow(Table_A8id), j = 2:ncol(Table_A8id)) %>%
  flextable::style(pr_t = officer::fp_text(font.size = 9), part = "all", pr_p = officer::fp_par(line_spacing = 1, padding = 0)) %>%
  flextable::hline(i = c(50, 63), border = officer::fp_border()) %>%
  flextable::autofit() %>%
  flextable::fit_to_width(9)

rm(models_tA8id)


##### TABLE A8ii #####

# Load the pre-saved RData file that contains the model results
load("results/TableA8ii_models.RData")

# Define a vector of model names that will be used for labeling the columns in the table
model_names <- c(paste("Conflicts"), paste("Log Conflicts"), paste("Deaths"), paste("Log Deaths"))

# Create a copy of model_names to be printed in the final table
model_names.to_print <- model_names

# Calculate the F-statistic for each model in models_tA8ii, format it as a string with stars for significance levels
F.stat <- purrr::map(models_tA8ii, \(x) {
  # Get the summary of the current model
  summ <- summary(x)
  
  # Calculate the p-value of the F-statistic
  .p <- pf(summ$fstatistic["value"], summ$fstatistic["numdf"], summ$fstatistic["dendf"], lower.tail = FALSE)
  
  # Return the F-statistic with significance stars
  sprintf("%0.3f%s", summ$fstatistic["value"],
          dplyr::case_when(.p < 0.01 ~ "***",
                           .p < 0.05 ~ "**",
                           .p < 0.1 ~ "*",
                           TRUE ~ ""))
}) %>% 
  # Unlist the result into a single vector and add the label "F Statistic" at the beginning
  unlist() %>% c("F Statistic", .)

# Calculate the degrees of freedom (df) for each model in models_tA8ii
df <- purrr::map(models_tA8ii, \(x) {
  # Get the summary of the current model
  summ <- summary(x)
  
  # Concatenate the numerator and denominator degrees of freedom into a single string
  paste(summ$fstatistic[2:3], collapse = ";")
}) %>% 
  # Unlist the result into a single vector and add the label "df" at the beginning
  unlist() %>% c("df", .)

# Generate the HTML output table using stargazer, adding F-statistic and degrees of freedom as additional lines
Table_A8ii <- suppressWarnings(
  stargazer::stargazer(models_tA8ii, type = "html",
                       column.labels = model_names, 
                       dep.var.caption = "Votes share 2018", 
                       omit.stat = "f",
                       add.lines = list(F.stat, df))
) %>% 
  # Convert the table into a single HTML string
  paste0(collapse = "")

# Convert the HTML string to a data frame by parsing the HTML, extracting the table, and converting it to a data frame
Table_A8ii <- xml2::read_html(Table_A8ii) %>% 
  rvest::html_table() %>% 
  as.data.frame() %>% 
  # Remove the first 5 rows of the table (usually containing non-relevant information like headers)
  dplyr::slice(-c(1:5))

# Extract the first row (the header row) and convert it to a vector
table_names <- as.vector(Table_A8ii %>% dplyr::slice(1) %>% unlist())

# Replace the first element of the header vector with a blank space
table_names[1] <- " "

# Apply custom dyad labels to the first column of the table
Table_A8ii %<>% make_dyad_labels(var = names(Table_A8ii)[1])

# Rename the columns of the table to match the model names, starting from the second row
Table_A8ii <- magrittr::set_names(Table_A8ii, c(" ", model_names.to_print)) %>% 
  # Remove the first row, which now contains redundant header information
  dplyr::slice(-1)

# Identify rows that are completely empty and remove them from the table
empty_lines <- Table_A8ii %>% 
  # Check for empty strings in each cell of the table
  dplyr::transmute(across(dplyr::everything(), \(x) nchar(x) == 0)) %>% 
  # Use rowwise to evaluate whether all columns in a row are empty
  dplyr::rowwise() %>% 
  dplyr::transmute(all(dplyr::c_across(dplyr::everything()))) %>% 
  # Extract the row indices of fully empty rows
  dplyr::pull(1) %>% which()

# Remove the empty rows from the table
Table_A8ii %<>% dplyr::slice(-empty_lines)

# Convert the cleaned data frame into a flextable object, and apply formatting
Table_A8ii %<>% 
  flextable::flextable() %>% 
  # Merge the cells in the last row across columns 2 to the end
  flextable::merge_at(i = nrow(Table_A8ii), j = 2:ncol(Table_A8ii)) %>%
  # Apply font size and padding to the entire table
  flextable::style(pr_t = officer::fp_text(font.size = 9), part = "all", pr_p = officer::fp_par(line_spacing = 1, padding = 0)) %>% 
  # Add a horizontal line at the 26th row
  flextable::hline(i = 26, border = officer::fp_border()) %>% 
  # Automatically adjust column widths to fit the content
  flextable::autofit() %>% 
  # Fit the table to a specific width of 6.49 units
  flextable::fit_to_width(6.49)

rm(models_tA8ii)


##### TABLE A8iii #####


# Load the pre-saved RData file that contains the model results
load("results/TableA8iii_models.RData")


# Create a vector of model names for each variable
model_names <- c("Conflicts", "Log Conflicts", "Deaths", "Log Deaths")

# Create a copy of the model names for display
model_names.to_print <- model_names

# Calculate the F-statistics for each model and add significance stars
F.stat <- models_tA8iii %>% purrr::map(\(x){
  # Get the summary of the linear model
  summ <- summary(x)
  
  # Calculate the p-value for the F-statistic
  p_value <- pf(summ$fstatistic["value"], summ$fstatistic["numdf"], summ$fstatistic["dendf"], lower.tail = FALSE)
  
  # Format the F-statistic value with significance stars based on the p-value
  sprintf("%0.3f%s", summ$fstatistic["value"],
          dplyr::case_when(
            p_value < 0.01 ~ "***",
            p_value < 0.05 ~ "**",
            p_value < 0.1  ~ "*",
            TRUE ~ ""
          ))
}) %>% unlist %>% c("F Statistic", .)

# Extract the degrees of freedom for each model
df <- models_tA8iii %>% purrr::map(\(x){
  # Get the summary of the linear model
  summ <- summary(x)
  
  # Combine numerator and denominator degrees of freedom into a single string
  paste(summ$fstatistic[2:3], collapse = ";")
}) %>% unlist %>% c("df", .)

# Generate an HTML table for the models using stargazer, including the F-statistics and degrees of freedom
Table_A8iii <- suppressWarnings(stargazer::stargazer(
  models_tA8iii,
  type = "html",
  column.labels = model_names,
  dep.var.caption = "Votes share 2018",
  omit.stat = "f",
  add.lines = list(F.stat, df)
)) %>% paste0(collapse = "")

# Read the HTML table into a data frame and remove unnecessary rows
Table_A8iii <- rvest::read_html(Table_A8iii) %>%
  rvest::html_table() %>%
  as.data.frame() %>%
  dplyr::slice(-c(1:5))

# Extract the first row (which contains the column names) as a vector
table_names <- as.vector(Table_A8iii %>% dplyr::slice(1) %>% unlist)

# Replace the first column name with an empty string (for better formatting)
table_names[1] <- " "

# Apply custom labels for dyads using a function 'make_dyad_labels'
Table_A8iii %<>% make_dyad_labels(var = names(Table_A8iii)[1])

# Rename the columns using the modified 'model_names.to_print' vector and remove the first row
Table_A8iii %<>% magrittr::set_names(c(" ", model_names.to_print)) %>%
  dplyr::slice(-1)

# Find and remove rows where all columns are empty
empty_lines <- Table_A8iii %>%
  dplyr::transmute(dplyr::across(dplyr::everything(), \(x) { nchar(x) == 0 })) %>%
  dplyr::rowwise() %>%
  dplyr::transmute(base::all(dplyr::c_across(dplyr::everything()))) %>%
  dplyr::pull(1) %>%
  which()

Table_A8iii %<>% dplyr::slice(-empty_lines)

# Format the table as a flextable, merge the last row, apply text styles and adjust width
Table_A8iii %<>%
  flextable::flextable() %>%
  flextable::merge_at(i = nrow(Table_A8iii), j = 2:ncol(Table_A8iii)) %>%
  flextable::style(pr_t = officer::fp_text(font.size = 9), part = "all", pr_p = officer::fp_par(line_spacing = 1, padding = 0)) %>%
  flextable::hline(i = 30, border = officer::fp_border()) %>%
  flextable::autofit() %>%
  flextable::fit_to_width(6.49)

rm(models_tA8iii)

##### TABLE A9i #####



# Load the pre-saved RData file that contains the model results
load("results/TableA9i_models.RData")

# Define a vector of variable names that will be used for the models
vars <- c("n.deaths", "log_n.deaths")

# Define a vector of model names based on the variables for output labeling
model_names <- c(paste("Deaths"), paste("Log Deaths"))

# Copy the model names to another variable that will be used for printing
model_names.to_print <- model_names

# Extract the standard errors from the models using coeftest and heteroskedasticity-robust standard errors (Arellano HC3)
models.to.print_se <- purrr::map(models_tA9i, \(x) {
  lmtest::coeftest(x, sandwich::vcovHC(x, method = "arellano", type = "HC3"))[, "Std. Error"]
})

# Compute the F-statistic for each model and format it as a string with significance stars (***, **, *)
F.stat <- purrr::map(models_tA9i, \(x) {
  # Get the summary of the model using heteroskedasticity-robust standard errors
  summ <- summary(x, vcov. = sandwich::vcovHC(x, method = "arellano", type = "HC3"))
  
  # Calculate the p-value of the F-statistic
  p_value <- pf(summ$fstatistic["value"], summ$fstatistic["numdf"], summ$fstatistic["dendf"], lower.tail = FALSE)
  
  # Format the F-statistic with significance stars based on the p-value
  sprintf("%0.3f%s", summ$fstatistic["value"],
          dplyr::case_when(
            p_value < 0.01 ~ "***",
            p_value < 0.05 ~ "**",
            p_value < 0.1 ~ "*",
            TRUE ~ ""
          ))
}) %>% 
  # Flatten the list of F-statistics into a single vector and prepend "F Statistic" label
  unlist() %>% 
  c("F Statistic", .)

# Extract the degrees of freedom from each model and concatenate them as a single string
df <- purrr::map(models_tA9i, \(x) {
  # Get the summary of the model with heteroskedasticity-robust standard errors
  summ <- summary(x, vcov. = sandwich::vcovHC(x, method = "arellano", type = "HC3"))
  
  # Concatenate the numerator and denominator degrees of freedom
  paste(summ$fstatistic[2:3], collapse = ";")
}) %>% 
  # Flatten the list of degree of freedom strings into a single vector and prepend "df" label
  unlist() %>% 
  c("df", .)

# Generate a regression table with stargazer, suppressing warnings during execution
Table_A9i <- suppressWarnings(
  stargazer::stargazer(models_tA9i, type = "html",
                       se = models.to.print_se,
                       column.labels = model_names, 
                       dep.var.caption = "Votes share 2006",
                       omit.stat = "f",
                       add.lines = list(F.stat, df))
) %>% 
  # Collapse the HTML table into a single string for further processing
  paste0(collapse = "")

# Parse the HTML table into a data frame using rvest
Table_A9i <- rvest::read_html(Table_A9i) %>% 
  rvest::html_table() %>% 
  as.data.frame() %>% 
  dplyr::slice(-c(1:5))  # Remove the first five rows, which are typically non-relevant headers

# Extract the first row as the table's column names, replacing the first column name with a blank space
table_names <- Table_A9i %>% dplyr::slice(1) %>% unlist() %>% as.vector()
table_names[1] <- " "

# Set new column names for the table and remove the first row
Table_A9i <- magrittr::set_names(Table_A9i, c(" ", model_names.to_print)) %>% 
  dplyr::slice(-1)

# Identify rows where all columns are empty (indicated by zero-length character strings)
empty_lines <- Table_A9i %>% 
  dplyr::transmute(across(dplyr::everything(), \(x) nchar(x) == 0)) %>% 
  dplyr::rowwise() %>% 
  dplyr::transmute(all(dplyr::c_across(dplyr::everything()))) %>% 
  dplyr::pull(1) %>% 
  which()

# Remove rows that are identified as empty
Table_A9i <- dplyr::slice(Table_A9i, -empty_lines)

# Convert the data frame to a flextable object and apply formatting
Table_A9i <- Table_A9i %>% 
  flextable::flextable() %>% 
  # Merge cells in the last row across columns 2 to the end
  flextable::merge_at(i = nrow(Table_A9i), j = 2:ncol(Table_A9i)) %>% 
  # Apply style formatting (font size, line spacing, and padding)
  flextable::style(pr_t = officer::fp_text(font.size = 9), part = "all",
                   pr_p = officer::fp_par(line_spacing = 1, padding = 0)) %>% 
  # Add a horizontal line at row 10 using a custom border
  flextable::hline(i = 10, border = officer::fp_border()) %>% 
  # Automatically adjust the width of the columns to fit content
  flextable::autofit() %>% 
  # Ensure the table fits within a specified width (6.49 inches)
  flextable::fit_to_width(6.49)


rm(models_tA9i)

##### TABLE A9ii #####

# Load the pre-saved RData file that contains the model results
load("results/TableA9ii_models.RData")

# Create a vector of model names for identification
model_names <- c(paste("Deaths"), paste("Log Deaths"))

# Store the model names to print later in the table
model_names.to_print <- model_names

# Extract the standard errors from the models using a heteroskedasticity-consistent covariance matrix (Arellano method, HC3)
models.to.print_se <- purrr::map(models_tA9ii, \(x) {
  # Apply coeftest with robust standard errors and extract the "Std. Error" column
  lmtest::coeftest(x, sandwich::vcovHC(x, method = "arellano", type = "HC3"))[, "Std. Error"]
})

# Compute the F-statistics for each model, formatted with stars for significance levels
F.stat <- purrr::map(models_tA9ii, \(x) {
  # Get summary of the model with robust standard errors
  summ <- summary(x, vcov. = sandwich::vcovHC(x, method = "arellano", type = "HC3"))
  
  # Calculate the p-value for the F-statistic
  p_value <- stats::pf(summ$fstatistic["value"], summ$fstatistic["numdf"], summ$fstatistic["dendf"], lower.tail = FALSE)
  
  # Format the F-statistic and add stars for significance
  sprintf("%0.3f%s", summ$fstatistic["value"],
          dplyr::case_when(p_value < 0.01 ~ "***",
                           p_value < 0.05 ~ "**",
                           p_value < 0.1  ~ "*",
                           TRUE           ~ ""))
}) %>%
  # Flatten the list into a vector and prepend "F Statistic" as the label
  unlist() %>%
  c("F Statistic", .)

# Extract the degrees of freedom (df) for each model and format as strings
df <- purrr::map(models_tA9ii, \(x) {
  # Get the summary of the model
  summ <- summary(x, vcov. = sandwich::vcovHC(x, method = "arellano", type = "HC3"))
  
  # Concatenate numerator and denominator df with a semicolon
  paste(summ$fstatistic[2:3], collapse = ";")
}) %>%
  # Flatten the list into a vector and prepend "df" as the label
  unlist() %>%
  c("df", .)

# Generate the HTML table using stargazer, with robust standard errors and custom labels
Table_A9ii <- suppressWarnings(
  stargazer::stargazer(models_tA9ii, type = "html",
                       se = models.to.print_se,
                       column.labels = model_names,
                       dep.var.caption = "Votes share 2011",
                       omit.stat = "f",
                       add.lines = list(F.stat, df)
  )
) %>%
  # Collapse the HTML table into a single string
  paste0(collapse = "")

# Read the HTML string as a table and convert it to a data frame, removing the first 5 rows
Table_A9ii <- rvest::read_html(Table_A9ii) %>%
  rvest::html_table() %>%
  as.data.frame() %>%
  dplyr::slice(-c(1:5))

# Extract the first row as column names, and replace the first name with a space
table_names <- as.vector(dplyr::slice(Table_A9ii, 1) %>% unlist())
table_names[1] <- " "

# Rename the columns of the table and remove the first row (now column names)
Table_A9ii <- magrittr::set_names(Table_A9ii, c(" ", model_names.to_print)) %>%
  dplyr::slice(-1)

# Find the rows that are entirely empty by checking if all characters in a row are of length zero
empty_lines <- Table_A9ii %>%
  dplyr::transmute(across(dplyr::everything(), \(x) nchar(x) == 0)) %>%
  dplyr::rowwise() %>%
  dplyr::transmute(all(dplyr::c_across(dplyr::everything()))) %>%
  dplyr::pull(1) %>%
  which()

# Remove the rows that are empty
Table_A9ii %<>% dplyr::slice(-empty_lines)

# Convert the cleaned table to a flextable, merge the last row for column alignment, and apply styling
Table_A9ii %<>%
  flextable::flextable() %>%
  flextable::merge_at(i = nrow(Table_A9ii), j = 2:ncol(Table_A9ii)) %>%
  flextable::style(
    pr_t = officer::fp_text(font.size = 9),
    part = "all",
    pr_p = officer::fp_par(line_spacing = 1, padding = 0)
  ) %>%
  # Add a horizontal line to row 18 and autofit the table width
  flextable::hline(i = 18, border = officer::fp_border()) %>%
  flextable::autofit() %>%
  flextable::fit_to_width(6.49)

# Remove the model list from memory
rm(models_tA9ii)


##### TABLE A9iii #####

# Load the pre-saved RData file that contains the model results
load("results/TableA9iii_models.RData")


# Create a vector of model names to be used in the table
model_names <- c(paste("Deaths"), paste("Log Deaths"))

# Assign the model names to a variable for printing purposes
model_names.to_print <- model_names

# Extract the standard errors from the models using coeftest and vcovHC
models.to.print_se <- purrr::map(models_tA9iii, \(x) {
  lmtest::coeftest(x, sandwich::vcovHC(x, method = "arellano", type = "HC3"))[,"Std. Error"]
})

# Calculate the F-statistic for each model and format it with significance stars
F.stat <- purrr::map(models_tA9iii, \(x) {
  # Get the model summary using robust standard errors
  summ <- summary(x, vcov. = sandwich::vcovHC(x, method = "arellano", type = "HC3"))
  
  # Calculate the p-value for the F-statistic
  p_val <- pf(summ$fstatistic["value"], summ$fstatistic["numdf"], summ$fstatistic["dendf"], lower.tail = FALSE)
  
  # Format the F-statistic value with significance stars based on p-value
  sprintf("%0.3f%s", summ$fstatistic["value"],
          dplyr::case_when(p_val < 0.01 ~ "***",
                           p_val < 0.05 ~ "**",
                           p_val < 0.1  ~ "*",
                           TRUE ~ ""))
}) %>% 
  # Unlist the results to get a single vector and prepend the label "F Statistic"
  unlist() %>% 
  c("F Statistic", .)

# Extract the degrees of freedom (numdf; dendf) for each model
df <- purrr::map(models_tA9iii, \(x) {
  # Get the model summary using robust standard errors
  summ <- summary(x, vcov. = sandwich::vcovHC(x, method = "arellano", type = "HC3"))
  
  # Concatenate the numerator and denominator degrees of freedom
  paste(summ$fstatistic[2:3], collapse = ";")
}) %>% 
  # Unlist the results to get a single vector and prepend the label "df"
  unlist() %>% 
  c("df", .)

# Create a stargazer table for the models, suppress warnings
Table_A9iii <- suppressWarnings(stargazer::stargazer(models_tA9iii, 
                                                     type = "html",
                                                     se = models.to.print_se,
                                                     column.labels = model_names,
                                                     dep.var.caption = "Votes share 2018",
                                                     omit.stat = "f",
                                                     add.lines = list(F.stat, df))) %>% 
  # Convert the stargazer output to a single string by collapsing the list elements
  paste0(collapse = "")

# Parse the HTML table from the stargazer output and convert it to a data frame
Table_A9iii <- rvest::read_html(Table_A9iii) %>% 
  rvest::html_table() %>% 
  as.data.frame() %>% 
  dplyr::slice(-c(1:5))  # Remove the first 5 rows (headers and unwanted info)

# Extract the first row as table column names and clean the first name
table_names <- as.vector(Table_A9iii %>% dplyr::slice(1) %>% unlist())
table_names[1] <- " "

# Set the cleaned column names and remove the first row from the table
Table_A9iii <- magrittr::set_names(Table_A9iii, c(" ", model_names.to_print)) %>% 
  dplyr::slice(-1)

# Identify rows where all columns are empty (e.g., from stargazer artifacts)
empty_lines <- Table_A9iii %>% 
  dplyr::transmute(dplyr::across(dplyr::everything(), \(x) nchar(x) == 0)) %>% 
  dplyr::rowwise() %>% 
  dplyr::transmute(base::all(dplyr::c_across(dplyr::everything()))) %>% 
  dplyr::pull(1) %>% 
  which()

# Remove the empty rows from the table
Table_A9iii %<>% dplyr::slice(-empty_lines)

# Convert the table to a flextable object for formatting and styling
Table_A9iii %<>% flextable::flextable() %>% 
  flextable::merge_at(i = nrow(Table_A9iii), j = 2:ncol(Table_A9iii)) %>%
  flextable::style(pr_t = officer::fp_text(font.size = 9), part = "all", pr_p = officer::fp_par(line_spacing = 1, padding = 0)) %>%
  flextable::hline(i = 30, border = officer::fp_border()) %>% 
  flextable::autofit() %>% 
  flextable::fit_to_width(6.49)

# Remove the list of models from the environment to free up memory
rm(models_tA9iii)


##### TABLE A10 #####

# Combine rows from two selections of the ACLED_data: selecting event_date, actor1 (renamed to actor), 
# fatalities, and inter1 in one, and event_date, actor2 (renamed to actor), fatalities, and inter2 in the other
to.plot <- dplyr::bind_rows(
  ACLED_data %>%
    dplyr::select(event_date, actor = actor1, fatalities, inter = inter1),
  ACLED_data %>%
    dplyr::select(event_date, actor = actor2, fatalities, inter = inter2)
) %>%
  
  # Group the data by actor and inter variables
  dplyr::group_by(actor, inter) %>%
  
  # Summarize the data: count number of events, sum the fatalities, get the first and last event dates
  dplyr::summarise(
    n.events = dplyr::n(),
    fatalities = sum(fatalities),
    first_event_date = min(event_date),
    last_event_date = max(event_date),
    .groups = "drop"
  )

# Mutate the data to replace empty actor names with "(Empty)", 
# and then convert the summarized data frame into a flextable object
Table_A10 <- to.plot %>%
  
  # Replace any empty strings in the actor column with "(Empty)"
  dplyr::mutate(actor = dplyr::case_when(actor == "" ~ "(Empty)", TRUE ~ actor)) %>%
  
  # Create a flextable from the summarized data
  flextable::flextable() %>%
  
  # Add vertical lines in columns 1 and 4 with a thickness of 2
  flextable::vline(j = c(1, 4), border = officer::fp_border(width = 2)) %>%
  
  # Apply styling to all parts of the table: set font size and line spacing
  flextable::style(
    pr_t = officer::fp_text(font.size = 8),
    part = "all",
    pr_p = officer::fp_par(line_spacing = 1, padding = 1)
  ) %>%
  
  # Automatically adjust column widths to fit content
  flextable::autofit() %>%
  
  # Fit the table to a specific width (6.49 inches)
  flextable::fit_to_width(6.49)

##### TABLE A11 #####

# Group the ACLED_data by time_period, event_type, and sub_event_type,
# then summarize the data by counting the number of events and summing the fatalities
to.plot <- ACLED_data %>%
  dplyr::group_by(time_period, event_type, sub_event_type) %>%
  dplyr::summarise(
    n.events = dplyr::n(),
    fatalities = sum(fatalities),
    .groups = "drop"
  )

# Convert the summarized data into a flextable object for display and formatting
Table_A11 <- to.plot %>%
  
  # Create the flextable from the summarized data
  flextable::flextable() %>%
  
  # Merge vertical cells in columns 1 and 2
  flextable::merge_v(j = c(1, 2)) %>%
  
  # Apply styling to the body of the table, setting font size, line spacing, and bottom cell border
  flextable::style(
    pr_t = officer::fp_text(font.size = 8),
    part = "body",
    pr_p = officer::fp_par(line_spacing = 1, padding = 1),
    pr_c = officer::fp_cell(border.bottom = officer::fp_border())
  ) %>%
  
  # Apply styling to the header of the table, setting font size and line spacing
  flextable::style(
    pr_t = officer::fp_text(font.size = 8),
    part = "header",
    pr_p = officer::fp_par(line_spacing = 1, padding = 1)
  ) %>%
  
  # Add vertical lines in columns 1 and 2 with a thickness of 2
  flextable::vline(j = c(1, 2), border = officer::fp_border(width = 2)) %>%
  
  # Automatically adjust column widths to fit content
  flextable::autofit() %>%
  
  # Fit the table to a specific width (6.49 inches)
  flextable::fit_to_width(6.49)

##### TABLE A12 #####

# Select relevant columns from the dataset 'actor_types2_table' and exclude irrelevant columns
to.plot <- actor_types2_table %>% 
  dplyr::select(side_a, side_b, n.conflicts_2006, n.deaths_2006, n.conflicts_2011, n.deaths_2011, n.conflicts_2018, n.deaths_2018) %>%
  
  # Filter out rows where 'side_a' equals "Total"
  dplyr::filter(side_a != "Total") %>%
  
  # Create a new 'Actors' column by concatenating 'side_a' and 'side_b'
  dplyr::mutate(Actors = paste0(side_a, " vs. ", side_b), .before = 1) %>%
  
  # Remove the 'side_a' and 'side_b' columns after combining them
  dplyr::select(-c(2:3)) %>%
  
  # Add a new row with the total sums for all numeric columns and set 'Actors' as "Total"
  dplyr::bind_rows(
    .,
    dplyr::summarise(
      .,
      dplyr::across(tidyselect:::where(is.numeric), sum)
    ) %>% dplyr::mutate(Actors = "Total")
  ) %>%
  
  # Calculate total conflicts and deaths for each row
  dplyr::rowwise() %>%
  dplyr::mutate(
    n.conflicts_total = sum(dplyr::c_across(dplyr::starts_with("n.conflicts_"))),
    n.deaths_total = sum(dplyr::c_across(dplyr::starts_with("n.deaths_")))
  ) %>%
  
  # Ungroup the data after rowwise operation
  dplyr::ungroup()

# Summarize the total deaths and conflicts in the original dataset
original_deaths_sum <- sum(ACLED_actor_type_2_territories$n.deaths, na.rm = TRUE)
original_conflicts_sum <- sum(ACLED_actor_type_2_territories$n.conflicts, na.rm = TRUE)

# Remove rows with missing values in 'side_b' and check that no NA values remain
ACLED_to.plot <- ACLED_actor_type_2_territories %>%
  dplyr::filter(!is.na(side_b))
stopifnot(!any(is.na(ACLED_to.plot$side_b)))

# Check that the sums of deaths and conflicts after filtering do not exceed the original sums
filtered_deaths_sum <- sum(ACLED_to.plot$n.deaths, na.rm = TRUE)
filtered_conflicts_sum <- sum(ACLED_to.plot$n.conflicts, na.rm = TRUE)
stopifnot(filtered_deaths_sum <= original_deaths_sum)
stopifnot(filtered_conflicts_sum <= original_conflicts_sum)

# Reshape the data to long format, combining 'var' and 'year' into a single column
ACLED_to.plot %<>% 
  tidyr::pivot_longer(cols = dplyr::all_of(c("n.deaths", "n.conflicts")), names_to = "var", values_to = "value") %>%
  tidyr::unite(col = "var", var, year)
stopifnot("var" %in% names(ACLED_to.plot))

# Verify that the sum of deaths and conflicts remains unchanged after pivoting
pivot_deaths_sum <- sum(ACLED_to.plot$value[grepl("n.deaths", ACLED_to.plot$var)], na.rm = TRUE)
pivot_conflicts_sum <- sum(ACLED_to.plot$value[grepl("n.conflicts", ACLED_to.plot$var)], na.rm = TRUE)
stopifnot(all.equal(filtered_deaths_sum, pivot_deaths_sum))
stopifnot(all.equal(filtered_conflicts_sum, pivot_conflicts_sum))

# Group data by 'side_a', 'side_b', and 'var', then sum the 'value' column for each group
ACLED_to.plot %<>% 
  dplyr::group_by(side_a, side_b, var) %>%
  dplyr::summarise(value = sum(value), .groups = "drop")
stopifnot("value" %in% names(ACLED_to.plot))

# Verify that the sum of deaths and conflicts remains unchanged after grouping
grouped_deaths_sum <- sum(ACLED_to.plot$value[grepl("n.deaths", ACLED_to.plot$var)], na.rm = TRUE)
grouped_conflicts_sum <- sum(ACLED_to.plot$value[grepl("n.conflicts", ACLED_to.plot$var)], na.rm = TRUE)
stopifnot(all.equal(pivot_deaths_sum, grouped_deaths_sum))
stopifnot(all.equal(pivot_conflicts_sum, grouped_conflicts_sum))

# Create a new column 'Actors' by concatenating 'side_a' and 'side_b', then remove the original columns
ACLED_to.plot %<>% 
  dplyr::mutate(Actors = paste0(side_a, "_", side_b), .before = 1) %>%
  dplyr::select(-side_a, -side_b)
stopifnot("Actors" %in% names(ACLED_to.plot))

# Reshape the data back to wide format and rename the columns accordingly
ACLED_to.plot %<>% 
  tidyr::pivot_wider(id_cols = "Actors", names_from = "var", values_from = "value", values_fill = 0) %>%
  dplyr::select(
    Actors,
    ACLED_n.conflicts_2006 = n.conflicts_2006,
    ACLED_n.deaths_2006 = n.deaths_2006,
    ACLED_n.conflicts_2011 = n.conflicts_2011,
    ACLED_n.deaths_2011 = n.deaths_2011,
    ACLED_n.conflicts_2018 = n.conflicts_2018,
    ACLED_n.deaths_2018 = n.deaths_2018
  )
stopifnot(all(c("Actors", "ACLED_n.conflicts_2006", "ACLED_n.deaths_2006", "ACLED_n.conflicts_2011", "ACLED_n.deaths_2011", "ACLED_n.conflicts_2018", "ACLED_n.deaths_2018") %in% names(ACLED_to.plot)))

# Calculate the total conflicts and deaths for each row and add them as new columns
ACLED_to.plot %<>% 
  dplyr::rowwise() %>%
  dplyr::mutate(
    ACLED_n.conflicts_total = sum(dplyr::c_across(cols = dplyr::starts_with("ACLED_n.conflicts"))),
    ACLED_n.deaths_total = sum(dplyr::c_across(cols = dplyr::starts_with("ACLED_n.deaths")))
  ) %>%
  dplyr::ungroup()
stopifnot(all(c("ACLED_n.conflicts_total", "ACLED_n.deaths_total") %in% names(ACLED_to.plot)))

# Ensure the totals calculated match the previously summed values before transformations
stopifnot(round(sum(ACLED_to.plot$ACLED_n.conflicts_total, na.rm = TRUE), 0) == round(filtered_conflicts_sum, 0))
stopifnot(round(sum(ACLED_to.plot$ACLED_n.deaths_total, na.rm = TRUE), 0) == round(filtered_deaths_sum, 0))

# Add a total row to the dataset by summing all numeric columns and setting 'Actors' as "Total"
ACLED_to.plot %<>% 
  dplyr::bind_rows(
    .,
    dplyr::summarise(., dplyr::across(tidyselect:::where(is.numeric), sum)) %>%
      dplyr::mutate(Actors = "Total")
  )
stopifnot(ACLED_to.plot[nrow(ACLED_to.plot), "Actors"] == "Total")

# Verify the sums in the total row are correct
stopifnot(round(ACLED_to.plot[nrow(ACLED_to.plot), "ACLED_n.conflicts_total"], 0) == round(sum(ACLED_to.plot$ACLED_n.conflicts_total[1:(nrow(ACLED_to.plot) - 1)], na.rm = TRUE), 0))
stopifnot(round(ACLED_to.plot[nrow(ACLED_to.plot), "ACLED_n.deaths_total"], 0) == round(sum(ACLED_to.plot$ACLED_n.deaths_total[1:(nrow(ACLED_to.plot) - 1)], na.rm = TRUE), 0))

# Combine the two datasets, apply transformations and create a formatted table
Table_A12 <- to.plot %>%
  
  # Replace " vs. " with "_" in the 'Actors' column
  dplyr::mutate(Actors = stringr::str_replace(Actors, "\\svs.\\s", "_")) %>%
  
  # Merge with 'ACLED_to.plot' by 'Actors'
  dplyr::left_join(ACLED_to.plot, by = "Actors") %>%
  
  # Filter rows where at least one numeric value is not zero
  dplyr::filter(dplyr::if_any(tidyselect:::where(is.numeric), .fns = \(x){ x != 0 })) %>%
  
  # Apply custom dyad labels
  make_dyad_labels(var = "Actors") %>%
  
  # Sort by 'Actors'
  dplyr::arrange(Actors) %>%
  
  # Create a flextable with specified formatting and headers
  flextable::flextable() %>%
  flextable::add_header(values = list(
    "n.conflicts_2006" = "2006 election",
    "n.deaths_2006" = "2006 election",
    "n.conflicts_2011" = "2011 election",
    "n.deaths_2011" = "2011 election",
    "n.conflicts_2018" = "2018 election",
    "n.deaths_2018" = "2018 election",
    "n.conflicts_total" = "Total",
    "n.deaths_total" = "Total",
    "ACLED_n.conflicts_2006" = "2006 election",
    "ACLED_n.deaths_2006" = "2006 election",
    "ACLED_n.conflicts_2011" = "2011 election",
    "ACLED_n.deaths_2011" = "2011 election",
    "ACLED_n.conflicts_2018" = "2018 election",
    "ACLED_n.deaths_2018" = "2018 election",
    "ACLED_n.conflicts_total" = "Total",
    "ACLED_n.deaths_total" = "Total"
  )) %>%
  
  # Add a second header with source information
  flextable::add_header(values = list(
    "n.conflicts_2006" = "UDCP",
    "n.deaths_2006" = "UDCP",
    "n.conflicts_2011" = "UDCP",
    "n.deaths_2011" = "UDCP",
    "n.conflicts_2018" = "UDCP",
    "n.deaths_2018" = "UDCP",
    "n.conflicts_total" = "UDCP",
    "n.deaths_total" = "UDCP",
    "ACLED_n.conflicts_2006" = "ACLED",
    "ACLED_n.deaths_2006" = "ACLED",
    "ACLED_n.conflicts_2011" = "ACLED",
    "ACLED_n.deaths_2011" = "ACLED",
    "ACLED_n.conflicts_2018" = "ACLED",
    "ACLED_n.deaths_2018" = "ACLED",
    "ACLED_n.conflicts_total" = "ACLED",
    "ACLED_n.deaths_total" = "ACLED"
  )) %>%
  
  # Set specific labels for the headers
  flextable::set_header_labels(values = list(
    "n.conflicts_2006" = "Conflict\nevents",
    "n.deaths_2006" = "Deaths",
    "n.conflicts_2011" = "Conflict\nevents",
    "n.deaths_2011" = "Deaths",
    "n.conflicts_2018" = "Conflict\nevents",
    "n.deaths_2018" = "Deaths",
    "n.conflicts_total" = "Conflict\nevents",
    "n.deaths_total" = "Deaths",
    "ACLED_n.conflicts_2006" = "Conflict\nevents",
    "ACLED_n.deaths_2006" = "Deaths",
    "ACLED_n.conflicts_2011" = "Conflict\nevents",
    "ACLED_n.deaths_2011" = "Deaths",
    "ACLED_n.conflicts_2018" = "Conflict\nevents",
    "ACLED_n.deaths_2018" = "Deaths",
    "ACLED_n.conflicts_total" = "Conflict\nevents",
    "ACLED_n.deaths_total" = "Deaths"
  )) %>%
  
  # Merge headers and apply vertical lines for formatting
  flextable::merge_h(part = "header") %>%
  flextable::vline(j = c(1, 3, 5, 7, 9, 11, 13, 15), border = officer::fp_border(width = 2)) %>%
  flextable::vline(j = c(1, 9), border = officer::fp_border(width = 2.5)) %>%
  flextable::hline(i = c(14), border = officer::fp_border(width = 2.5)) %>%
  
  # Apply styling to the table
  flextable::style(pr_t = officer::fp_text(font.size = 9), part = "all", pr_p = officer::fp_par(line_spacing = 1, padding = 1)) %>%
  flextable::style(i = 13, pr_t = officer::fp_text(font.size = 9, bold = TRUE), part = "body", pr_p = officer::fp_par(line_spacing = 1, padding = 1)) %>%
  flextable::style(pr_t = officer::fp_text(font.size = 9, bold = TRUE), part = "header", pr_p = officer::fp_par(line_spacing = 1, padding = 1)) %>%
  flextable::style(j = c(1, 8, 9, 16, 17), pr_t = officer::fp_text(font.size = 9, bold = TRUE), part = "all", pr_p = officer::fp_par(line_spacing = 1, padding = 1)) %>%
  
  # Align columns and autofit the table to the page width
  flextable::align(j = 2:9, part = "body", align = "right") %>%
  flextable::align(i = 1, j = 2:17, part = "header", align = "center") %>%
  flextable::autofit() %>%
  flextable::fit_to_width(10.5)

##### TABLE A13 #####

# Load the pre-saved RData file that contains the model results
load("results/TableA13_models.RData")

# Create a vector of model names to label the results
model_names <- c(paste("Conflicts"), paste("Log Conflicts"), paste("Deaths"), paste("Log Deaths"),
                 paste("ACLED\nConflicts"), paste("ACLED\nLog Conflicts"), paste("ACLED\nDeaths"), paste("ACLED\nLog Deaths"))

# Assign the model names to be printed as-is
model_names.to_print <- model_names

# Extract F-statistics for each model in 'models_tA13'
F.stat <- purrr::map(models_tA13, \(x) {
  # Get summary statistics for each model
  summ <- summary(x)
  
  # Calculate the p-value for the F-statistic
  .p <- pf(summ$fstatistic["value"], summ$fstatistic["numdf"], summ$fstatistic["dendf"], lower.tail = FALSE)
  
  # Format the F-statistic with significance stars based on the p-value
  sprintf("%0.3f%s", summ$fstatistic["value"],
          dplyr::case_when(
            .p < 0.01 ~ "***", # Three stars for p < 0.01
            .p < 0.05 ~ "**",  # Two stars for p < 0.05
            .p < 0.1 ~ "*",    # One star for p < 0.1
            TRUE ~ ""          # No stars otherwise
          ))
}) %>% unlist %>% c("F Statistic", .)

# Extract the degrees of freedom (df) for each model
df <- purrr::map(models_tA13, \(x) {
  # Get summary statistics for each model
  summ <- summary(x)
  
  # Concatenate the numerator and denominator degrees of freedom as a string
  paste(summ$fstatistic[2:3], collapse = ";")
}) %>% unlist %>% c("df", .)

# Generate a formatted table using stargazer
Table_A13 <- suppressWarnings(
  stargazer::stargazer(models_tA13, type = "html",
                       
                       # Set the caption for the dependent variable
                       dep.var.caption = "Votes share 2018",
                       
                       # Omit the F-statistic row (added separately)
                       omit.stat = "f",
                       
                       # Add custom lines for F-statistics and degrees of freedom
                       add.lines = list(F.stat, df)
  )
) %>% paste0(collapse = "")

# Parse the generated HTML table into a data frame for manipulation
Table_A13 <- xml2::read_html(Table_A13) %>% rvest::html_table() %>% as.data.frame() %>% dplyr::slice(-c(1:5))

# Extract the first row as column names and update the first column's name
table_names <- as.vector(dplyr::slice(Table_A13, 1) %>% unlist())
table_names[1] <- " "

# Create dyad labels for the first column of the table
Table_A13 <- make_dyad_labels(Table_A13, var = names(Table_A13)[1])

# Set new column names using 'model_names.to_print', excluding the first row
Table_A13 <- magrittr::set_names(Table_A13, c(" ", model_names.to_print)) %>% dplyr::slice(-1)

# Identify empty rows (all columns are empty strings) and remove them
empty_lines <- Table_A13 %>%
  dplyr::transmute(across(dplyr::everything(), \(x) nchar(x) == 0)) %>%
  dplyr::rowwise() %>%
  dplyr::transmute(all(dplyr::c_across(dplyr::everything()))) %>%
  dplyr::pull(1) %>%
  which()

Table_A13 <- dplyr::slice(Table_A13, -empty_lines)

# Create a formatted flextable object from the data frame
Table_A13 <- flextable::flextable(Table_A13) %>%
  
  # Merge cells for the last row from column 2 onwards
  flextable::merge_at(i = nrow(Table_A13), j = 2:ncol(Table_A13)) %>%
  
  # Apply text and paragraph styling to all parts of the table
  flextable::style(
    pr_t = officer::fp_text(font.size = 10), # Set font size
    part = "all", # Apply to all parts of the table
    pr_p = officer::fp_par(line_spacing = 1, padding = 0) # Set line spacing and padding
  ) %>%
  
  # Add a horizontal line at row 34
  flextable::hline(i = 34, border = officer::fp_border()) %>%
  
  # Add vertical lines at columns 1 and 5
  flextable::vline(j = c(1, 5), border = officer::fp_border()) %>%
  
  # Auto-fit the table to content width and set the total width to 10.5 units
  flextable::autofit() %>%
  flextable::fit_to_width(10.5)

# Remove the 'models_tA13' object to free memory
rm(models_tA13)


##### TABLE A14  #####

# Load the pre-saved RData file that contains the model results for Table A14
load("results/TableA14_models.RData")

# Create a vector of model names for labeling the results
model_names <- c(paste("Conflicts"), paste("Log Conflicts"), paste("Deaths"), paste("Log Deaths"),
                 paste("ACLED\nConflicts"), paste("ACLED\nLog Conflicts"), paste("ACLED\nDeaths"), paste("ACLED\nLog Deaths"))

# Assign the model names to be used for printing
model_names.to_print <- model_names

# Extract F-statistics for each model in 'models_tA14'
F.stat <- purrr::map(models_tA14, \(x) {
  # Get the summary statistics for each model
  summ <- summary(x)
  
  # Calculate the p-value for the F-statistic
  .p <- pf(summ$fstatistic["value"], summ$fstatistic["numdf"], summ$fstatistic["dendf"], lower.tail = FALSE)
  
  # Format the F-statistic with significance stars based on the p-value
  sprintf("%0.3f%s", summ$fstatistic["value"],
          dplyr::case_when(
            .p < 0.01 ~ "***",  # Three stars for p < 0.01
            .p < 0.05 ~ "**",   # Two stars for p < 0.05
            .p < 0.1 ~ "*",     # One star for p < 0.1
            TRUE ~ ""           # No stars otherwise
          ))
}) %>% unlist %>% c("F Statistic", .)

# Extract the degrees of freedom (df) for each model
df <- purrr::map(models_tA14, \(x) {
  # Get the summary statistics for each model
  summ <- summary(x)
  
  # Concatenate the numerator and denominator degrees of freedom as a string
  paste(summ$fstatistic[2:3], collapse = ";")
}) %>% unlist %>% c("df", .)

# Generate a formatted table using stargazer
Table_A14 <- suppressWarnings(
  stargazer::stargazer(models_tA14, type = "html",
                       
                       # Set the caption for the dependent variable
                       dep.var.caption = "Votes share 2018",
                       
                       # Omit the F-statistic row (added separately)
                       omit.stat = "f",
                       
                       # Add custom lines for F-statistics and degrees of freedom
                       add.lines = list(F.stat, df)
  )
) %>% paste0(collapse = "")

# Parse the generated HTML table into a data frame for further manipulation
Table_A14 <- xml2::read_html(Table_A14) %>% rvest::html_table() %>% as.data.frame() %>% dplyr::slice(-c(1:5))

# Extract the first row as column names and update the first column's name
table_names <- as.vector(dplyr::slice(Table_A14, 1) %>% unlist())
table_names[1] <- " "

# Create dyad labels for the first column of the table
Table_A14 <- make_dyad_labels(Table_A14, var = names(Table_A14)[1])

# Set new column names using 'model_names.to_print' and remove the first row
Table_A14 <- magrittr::set_names(Table_A14, c(" ", model_names.to_print)) %>% dplyr::slice(-1)

# Identify and remove empty rows (where all columns contain empty strings)
empty_lines <- Table_A14 %>%
  dplyr::transmute(across(dplyr::everything(), \(x) nchar(x) == 0)) %>%
  dplyr::rowwise() %>%
  dplyr::transmute(all(dplyr::c_across(dplyr::everything()))) %>%
  dplyr::pull(1) %>%
  which()

Table_A14 <- dplyr::slice(Table_A14, -empty_lines)

# Create a formatted flextable object from the data frame
Table_A14 <- flextable::flextable(Table_A14) %>%
  
  # Merge cells in the last row from column 2 onwards
  flextable::merge_at(i = nrow(Table_A14), j = 2:ncol(Table_A14)) %>%
  
  # Apply text and paragraph styling to all parts of the table
  flextable::style(
    pr_t = officer::fp_text(font.size = 10),  # Set font size
    part = "all",  # Apply styling to all parts of the table
    pr_p = officer::fp_par(line_spacing = 1, padding = 0)  # Set line spacing and padding
  ) %>%
  
  # Add a horizontal line at row 41
  flextable::hline(i = 35, border = officer::fp_border()) %>%
  
  # Add vertical lines at columns 1 and 5
  flextable::vline(j = c(1, 5), border = officer::fp_border()) %>%
  
  # Auto-fit the table to content width and set the total width to 10.5 units
  flextable::autofit() %>%
  flextable::fit_to_width(10.5)

# Remove the 'models_tA14' object from the environment to free memory
rm(models_tA14)


##### TABLE A15 #####

ged201_for_period_and_actors <- ged201 %>% as.data.frame() %>% 
  dplyr::mutate(period=dplyr::case_when(date_start >= lubridate::ymd("2001-01-17") & date_end <= lubridate::ymd("2006-07-30") ~"2006 election",
                                        date_start >= lubridate::ymd("2006-07-31") & date_end <= lubridate::ymd("2011-11-28") ~"2011 election",
                                        date_start >= lubridate::ymd("2011-11-29") & date_end <= lubridate::ymd("2018-12-30") ~"2018 election",
                                        TRUE~NA_character_))%>% 
  dplyr::filter(side_a %in% actor_types$groups & side_b %in% actor_types$groups)

# ged201_for_period_and_actors %>% dplyr::filter(side_a=="CNDD-FDD" | side_b=="CNDD-FDD") %>% View()

conflict.groups.period.table <- ged201_for_period_and_actors%>% dplyr::select(election,side_a,side_b,n.deaths=deaths_civilians,index.data) %>% 
  dplyr::group_by(election,side_a,side_b) %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with("n.deaths"),~sum(.)),.groups="drop") %>%
  tidyr::pivot_longer(cols=c(side_a,side_b),values_to = "group") %>%
  dplyr::group_by(election,group) %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with("n.deaths"),~sum(.)),.groups="drop") %>%
  dplyr::filter(n.deaths>0 & group!="Civilians") 







conflict.groups.period.table %<>%
  tidyr::separate(election,sep="_",into = c("drop","election")) %>%
  dplyr::select(-drop) %>%
  tidyr::pivot_wider(names_from = "election",values_from = "n.deaths") %>%
  dplyr::mutate(dplyr::across(tidyselect:::where(is.numeric),~tidyr::replace_na(.,0))) 





# Totals line
conflict.groups.period.table %<>% dplyr::bind_rows(
  conflict.groups.period.table %>% dplyr::summarise(dplyr::across(tidyselect:::where(is.numeric),sum)) %>% dplyr::mutate(group="Total",.before=1)
)


Table_A15 <- conflict.groups.period.table%>% 
  dplyr::select(group,dplyr::ends_with("2006"),dplyr::ends_with("2011"),dplyr::ends_with("2018"))%>%
  flextable::flextable()  %>%
  flextable::set_header_labels(values=list(
    "2006"="2006 election",
    "2011"="2011 election",
    "2018"="2018 election")) %>%
  
  flextable::hline(i = nrow(conflict.groups.period.table)-1,border = officer::fp_border(width = 2))  %>% 
  flextable::vline(j=c(1),border=officer::fp_border(width = 2))%>% 
  flextable::style(pr_t=fp_text(font.size = 9),part = "all",pr_p = fp_par(line_spacing = 1,padding = 1)) %>%
  flextable::autofit() %>% flextable::fit_to_width(6.49)




##### TABLE A16 #####

# Select relevant columns from the dataset for analysis
df <- actor_type_2_territories %>% 
  dplyr::select(side_a, side_b, year, index.data, n.conflicts) %>%
  # Filter the data to include only rows from the year 2006
  dplyr::filter(year == 2006) %>%
  # Create a new column "Actors" by concatenating 'side_a' and 'side_b', placed at the start of the dataset
  dplyr::mutate(Actors = paste0(side_a, "_", side_b), .before = 1) %>%
  # Remove columns 2 through 4, keeping only "Actors" and "n.conflicts"
  dplyr::select(-c(2:4)) %>%
  # Reshape the data from long to wide format, filling in missing values with 0
  tidyr::pivot_wider(names_from = "Actors", values_from = "n.conflicts", values_fill = 0) %>%
  # Remove the 'index.data' column from the dataset
  dplyr::select(-index.data)

# Compute correlation matrix using the "spearman" method, setting diagonal values to 1
corr_matrix <- df %>%
  corrr::correlate(use = "pairwise", method = "spearman", quiet = TRUE, diagonal = 1.0)

# Calculate Spearman correlation matrix and p-values using Hmisc::rcorr()
r <- Hmisc::rcorr(as.matrix(df), type = "spearman")  

# Convert the correlation matrix to a data frame, remove diagonal elements for clarity
correlations <- corr_matrix %>%
  corrr::shave() %>%
  corrr::fashion(na_print = "-", decimals = 2, leading_zeros = FALSE) %>%
  data.frame()

# Remove upper triangle of the matrix by setting those elements to an empty string
for(i in 1:nrow(correlations)) {
  for(j in 2:ncol(correlations)) {
    if(i < j) {
      correlations[i, j] <- ""
    }
  }
}

# Convert p-values matrix to data frame and divide p-values by 2 for one-tailed tests
p.values <- r$P %>%
  as.data.frame() %>%
  divide_by(2)

# Set upper triangle of the p-values matrix to NA since it's redundant
p.values[upper.tri(p.values)] <- NA

# Initialize an empty matrix for p-value significance levels
p.stars <- matrix("", ncol = ncol(p.values), nrow = nrow(p.values))

# Assign symbols based on significance thresholds: "." for p <= 0.1, "*" for p <= 0.05, etc.
p.stars[p.values <= 0.1 & p.values > 0.05] <- "."
p.stars[p.values <= 0.05 & p.values > 0.01] <- "*"
p.stars[p.values <= 0.01 & p.values > 0.001] <- "**"
p.stars[p.values <= 0.001] <- "***"

# Combine correlations with significance stars, convert to data frame and set column names
correlations <- matrix(paste0(
  correlations %>%
    dplyr::select(-term) %>%
    as.matrix(), p.stars), 
  nrow = nrow(p.stars), ncol = ncol(p.stars)) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  magrittr::set_names(correlations$term) %>%
  dplyr::mutate(rowname = correlations$term) %>%
  dplyr::select(rowname, dplyr::everything())

# Add a column indicating the statistic type ("Spearman Correlation")
correlations %<>%
  dplyr::mutate(stat = "Spearman Correlation") %>%
  dplyr::rename(vars = rowname) %>%
  dplyr::select(stat, vars, dplyr::everything())

# Format the p-values data frame, replacing numeric values with appropriate strings
p.values %<>%
  dplyr::mutate(stat = "Sig. (1-tailed)", vars = rownames(.)) %>%
  dplyr::select(stat, vars, dplyr::everything()) %>%
  dplyr::mutate_if(is.numeric, \(x) dplyr::case_when(
    is.na(x) ~ "", 
    x < 0.001 ~ "<0.001", 
    TRUE ~ sprintf("%0.3f", x)
  ))

# Bind rows to combine the correlations and p-values data frames into one
to.plot <- suppressWarnings(dplyr::bind_rows(correlations))

# Convert all columns to character data type
to.plot %<>%
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

# Extract variable names analyzed for further operations
vars.analyzed <- to.plot %>%
  dplyr::pull("vars")

# Filter out rows where the 'vars' column starts with "afdl_"
to.plot %<>%
  dplyr::rowwise() %>%
  dplyr::filter(!stringr::str_starts(vars, "afdl_")) %>%
  dplyr::ungroup() %>%
  dplyr::select(., dplyr::one_of(c(names(to.plot[1:2]), .$vars))) %>%
  dplyr::select(-1)

# Apply custom labeling to the 'vars' column to create "dyad labels"
to.plot %<>%
  make_dyad_labels(var = "vars")

# Update column names with custom "dyad labels"
to.plot %<>%
  magrittr::set_colnames(data.frame(dyad = names(to.plot)) %>%
                           make_dyad_labels(var = "dyad") %>%
                           dplyr::pull("dyad"))

# Remove the last column from the data frame, which is not needed
to.plot %<>%
  dplyr::select(-ncol(to.plot))

# Create a formatted table using flextable, adding a footer and formatting options
Table_A16 <- flextable::flextable(to.plot) %>%
  flextable::add_footer(vars = "p-values: 0.1 . 0.05 * 0.01 ** 0.001 ***") %>%
  flextable::merge_at(j = 1:ncol(to.plot), part = "footer") %>%
  flextable::merge_v(j = 1) %>%
  flextable::set_header_labels(stat = "", vars = "") %>%
  flextable::align(j = 1:ncol(to.plot), align = "left") %>%
  flextable::hline(i = ncol(to.plot), border = officer::fp_border(width = 2)) %>%
  flextable::vline(j = 2:ncol(to.plot), border = officer::fp_border(width = 2, color = "gray80"), part = "body") %>%
  flextable::style(pr_t = officer::fp_text(font.size = 8), part = "all") %>%
  flextable::width(1, 1.5) %>%
  flextable::fit_to_width(11.5)

##### TABLE A17 #####

# Load the pre-saved RData file that contains the model results for Table A17
load("results/TableA17_models.RData")

# Create a vector of model names for labeling the table
model_names <- c(paste("Conflicts"), paste("Log Conflicts"), paste("Deaths"), paste("Log Deaths"),
                 paste("ACLED\nConflicts"), paste("ACLED\nLog Conflicts"), paste("ACLED\nDeaths"), paste("ACLED\nLog Deaths"))

# Assign model names for display purposes
model_names.to_print <- model_names

# Extract the standard errors for each model using heteroskedasticity-consistent standard errors (Arellano HC3 method)
models.to.print_se <- purrr::map(models_tA17, \(x) {
  lmtest::coeftest(x, sandwich::vcovHC(x, method = "arellano", type = "HC3"))[, "Std. Error"]
})

# Extract F-statistics for each model and format them with significance stars
F.stat <- purrr::map(models_tA17, \(x) {
  # Obtain summary statistics using heteroskedasticity-consistent standard errors
  summ <- summary(x, vcov. = sandwich::vcovHC(x, method = "arellano", type = "HC3"))
  
  # Calculate the p-value for the F-statistic
  .p <- pf(summ$fstatistic["value"], summ$fstatistic["numdf"], summ$fstatistic["dendf"], lower.tail = FALSE)
  
  # Format the F-statistic and append significance stars based on the p-value
  sprintf("%0.3f%s", summ$fstatistic["value"],
          dplyr::case_when(
            .p < 0.01 ~ "***", # Three stars for p < 0.01
            .p < 0.05 ~ "**",  # Two stars for p < 0.05
            .p < 0.1 ~ "*",    # One star for p < 0.1
            TRUE ~ ""          # No stars otherwise
          ))
}) %>% unlist %>% c("F Statistic", .)

# Extract degrees of freedom for each model and format them
df <- purrr::map(models_tA17, \(x) {
  # Obtain summary statistics using heteroskedasticity-consistent standard errors
  summ <- summary(x, vcov. = sandwich::vcovHC(x, method = "arellano", type = "HC3"))
  
  # Concatenate the numerator and denominator degrees of freedom
  paste(summ$fstatistic[2:3], collapse = ";")
}) %>% unlist %>% c("df", .)

# Create an HTML table using stargazer, with custom standard errors, and additional F-statistics and df lines
Table_A17 <- suppressWarnings(
  stargazer::stargazer(models_tA17, type = "html",
                       
                       # Insert custom standard errors into the table
                       se = models.to.print_se,
                       
                       # Set caption for the dependent variable
                       dep.var.caption = "Change in vote share 2006-2011",
                       
                       # Omit the F-statistic (added separately)
                       omit.stat = "f",
                       
                       # Add custom lines for F-statistics and degrees of freedom
                       add.lines = list(F.stat, df)
  )
) %>% paste0(collapse = "")

# Parse the generated HTML table into a data frame and remove the first 5 rows (header rows)
Table_A17 <- xml2::read_html(Table_A17) %>% rvest::html_table() %>% as.data.frame() %>% dplyr::slice(-c(1:5))

# Assign new column names to the table, using 'model_names.to_print', and remove the first row (headers)
Table_A17 <- magrittr::set_names(Table_A17, c(" ", model_names.to_print)) %>% dplyr::slice(-1)

# Identify and remove empty rows (all cells are empty) from the table
empty_lines <- Table_A17 %>%
  dplyr::transmute(across(dplyr::everything(), \(x) nchar(x) == 0)) %>%
  dplyr::rowwise() %>%
  dplyr::transmute(all(dplyr::c_across(dplyr::everything()))) %>%
  dplyr::pull(1) %>%
  which()

Table_A17 <- dplyr::slice(Table_A17, -empty_lines)

# Create a formatted flextable object from the data frame
Table_A17 <- flextable::flextable(Table_A17) %>%
  
  # Merge cells in the last row from column 2 onwards
  flextable::merge_at(i = nrow(Table_A17), j = 2:ncol(Table_A17)) %>%
  
  # Apply text and paragraph styling to all parts of the table
  flextable::style(
    pr_t = officer::fp_text(font.size = 10), # Set font size for the text
    part = "all", # Apply to all parts of the table
    pr_p = officer::fp_par(line_spacing = 1, padding = 0) # Set line spacing and padding
  ) %>%
  
  # Add a horizontal line at row 18
  flextable::hline(i = 18, border = officer::fp_border()) %>%
  
  # Add vertical lines at columns 1 and 5
  flextable::vline(j = c(1, 5), border = officer::fp_border()) %>%
  
  # Auto-fit the table to the content width and set the total width to 10.5 units
  flextable::autofit() %>%
  flextable::fit_to_width(10.5)

# Remove the 'models_tA17' object to free memory
rm(models_tA17)


##### TABLE A18 #####

# Load the pre-saved RData file that contains the model results for analysis
load("results/TableA18_models.RData")

# Create a vector of model names to use as column headers in the final table
model_names <- c(paste("Conflicts"), paste("Log Conflicts"), paste("Deaths"), paste("Log Deaths"),
                 paste("ACLED\nConflicts"), paste("ACLED\nLog Conflicts"), paste("ACLED\nDeaths"), paste("ACLED\nLog Deaths"))

# Set the model names to be printed as-is
model_names.to_print <- model_names

# Extract the standard errors for the coefficients of each model using heteroskedasticity-consistent covariance matrix
models.to.print_se <- purrr::map(models_tA18, \(x) {
  lmtest::coeftest(x, sandwich::vcovHC(x, method = "arellano", type = "HC3"))[, "Std. Error"]
})

# Extract F-statistics for each model in 'models_tA18'
F.stat <- purrr::map(models_tA18, \(x) {
  # Generate summary statistics for each model with heteroskedasticity-consistent covariance matrix
  summ <- summary(x, vcov. = sandwich::vcovHC(x, method = "arellano", type = "HC3"))
  
  # Calculate the p-value for the F-statistic
  .p <- pf(summ$fstatistic["value"], summ$fstatistic["numdf"], summ$fstatistic["dendf"], lower.tail = FALSE)
  
  # Format the F-statistic with significance stars based on the p-value
  sprintf("%0.3f%s", summ$fstatistic["value"],
          dplyr::case_when(
            .p < 0.01 ~ "***", # Three stars for p < 0.01
            .p < 0.05 ~ "**",  # Two stars for p < 0.05
            .p < 0.1 ~ "*",    # One star for p < 0.1
            TRUE ~ ""          # No stars otherwise
          ))
}) %>% unlist %>% c("F Statistic", .)

# Extract degrees of freedom (df) for each model
df <- purrr::map(models_tA18, \(x) {
  # Generate summary statistics for each model with heteroskedasticity-consistent covariance matrix
  summ <- summary(x, vcov. = sandwich::vcovHC(x, method = "arellano", type = "HC3"))
  
  # Concatenate the numerator and denominator degrees of freedom as a string
  paste(summ$fstatistic[2:3], collapse = ";")
}) %>% unlist %>% c("df", .)

# Generate a formatted table using stargazer, including standard errors, F-statistics, and degrees of freedom
Table_A18 <- suppressWarnings(
  stargazer::stargazer(models_tA18, type = "html",
                       
                       # Use the pre-calculated standard errors
                       se = models.to.print_se,
                       
                       # Set the caption for the dependent variable
                       dep.var.caption = "Change in vote share 2011-2018",
                       
                       # Omit the F-statistic row (added separately)
                       omit.stat = "f",
                       
                       # Add custom lines for F-statistics and degrees of freedom
                       add.lines = list(F.stat, df)
  )
) %>% paste0(collapse = "")

# Parse the generated HTML table into a data frame for further manipulation
Table_A18 <- xml2::read_html(Table_A18) %>%
  rvest::html_table() %>%
  as.data.frame() %>%
  dplyr::slice(-c(1:5))  # Remove the first five rows (header and unwanted content)

# Assign new column names, replacing the first column's name with a space
Table_A18 <- magrittr::set_names(Table_A18, c(" ", model_names.to_print)) %>% dplyr::slice(-1)

# Identify rows where all columns contain empty strings and remove these rows
empty_lines <- Table_A18 %>%
  dplyr::transmute(across(dplyr::everything(), \(x) nchar(x) == 0)) %>%
  dplyr::rowwise() %>%
  dplyr::transmute(all(dplyr::c_across(dplyr::everything()))) %>%
  dplyr::pull(1) %>%
  which()

Table_A18 <- dplyr::slice(Table_A18, -empty_lines)

# Create a formatted flextable object from the data frame
Table_A18 <- flextable::flextable(Table_A18) %>%
  
  # Merge cells in the last row from column 2 onwards
  flextable::merge_at(i = nrow(Table_A18), j = 2:ncol(Table_A18)) %>%
  
  # Apply text and paragraph formatting to all parts of the table
  flextable::style(
    pr_t = officer::fp_text(font.size = 10), # Set font size
    part = "all", # Apply to all parts of the table
    pr_p = officer::fp_par(line_spacing = 1, padding = 0) # Set line spacing and padding
  ) %>%
  
  # Add a horizontal line at row 28
  flextable::hline(i = 28, border = officer::fp_border()) %>%
  
  # Add vertical lines at columns 1 and 5
  flextable::vline(j = c(1, 5), border = officer::fp_border()) %>%
  
  # Auto-fit the table to content width and set the total width to 8.5 units
  flextable::autofit() %>%
  flextable::fit_to_width(8.5)

# Remove the 'models_tA18' object to free up memory
rm(models_tA18)


##### TABLE A19i #####
# Load the pre-saved RData file that contains the model results for Table A19i
load("results/TableA19i_models.RData")

# Create a vector of model names to label the output in the table
model_names <- c(paste("Conflicts"), paste("Log Conflicts"), paste("Deaths"), paste("Log Deaths"),
                 paste("ACLED\nConflicts"), paste("ACLED\nLog Conflicts"), paste("ACLED\nDeaths"), paste("ACLED\nLog Deaths"))

# Assign the model names to be printed as-is
model_names.to_print <- model_names

# Extract standard errors for each model using coeftest with robust standard errors (Arellano, HC3)
models.to.print_se <- purrr::map(models_tA19i, \(x) {
  lmtest::coeftest(x, sandwich::vcovHC(x, method = "arellano", type = "HC3"))[, "Std. Error"]
})

# Extract F-statistics for each model in 'models_tA19i'
F.stat <- purrr::map(models_tA19i, \(x) {
  # Get summary statistics with robust standard errors
  summ <- summary(x, vcov. = sandwich::vcovHC(x, method = "arellano", type = "HC3"))
  
  # Calculate the p-value for the F-statistic
  .p <- pf(summ$fstatistic["value"], summ$fstatistic["numdf"], summ$fstatistic["dendf"], lower.tail = FALSE)
  
  # Format the F-statistic with significance stars based on the p-value
  sprintf("%0.3f%s", summ$fstatistic["value"],
          dplyr::case_when(
            .p < 0.01 ~ "***",  # Three stars for p < 0.01
            .p < 0.05 ~ "**",   # Two stars for p < 0.05
            .p < 0.1 ~ "*",     # One star for p < 0.1
            TRUE ~ ""           # No stars otherwise
          ))
}) %>% unlist() %>% c("F Statistic", .)

# Extract degrees of freedom (df) for each model
df <- purrr::map(models_tA19i, \(x) {
  # Get summary statistics with robust standard errors
  summ <- summary(x, vcov. = sandwich::vcovHC(x, method = "arellano", type = "HC3"))
  
  # Concatenate the numerator and denominator degrees of freedom as a string
  paste(summ$fstatistic[2:3], collapse = ";")
}) %>% unlist() %>% c("df", .)

# Generate a formatted table using stargazer
Table_A19i <- suppressWarnings(
  stargazer::stargazer(models_tA19i, type = "html",
                       
                       # Include the standard errors extracted above
                       se = models.to.print_se,
                       
                       # Set the caption for the dependent variable
                       dep.var.caption = "Turnout 2006",
                       
                       # Omit the F-statistic row (added separately)
                       omit.stat = "f",
                       
                       # Add custom lines for F-statistics and degrees of freedom
                       add.lines = list(F.stat, df)
  )
) %>% paste0(collapse = "")

# Parse the generated HTML table into a data frame for manipulation
Table_A19i <- xml2::read_html(Table_A19i) %>% rvest::html_table() %>% as.data.frame() %>% dplyr::slice(-c(1:5))

# Set new column names using 'model_names.to_print', excluding the first row
Table_A19i <- magrittr::set_names(Table_A19i, c(" ", model_names.to_print)) %>% dplyr::slice(-1)

# Identify empty rows (all columns are empty strings) and remove them
empty_lines <- Table_A19i %>%
  dplyr::transmute(across(dplyr::everything(), \(x) nchar(x) == 0)) %>%
  dplyr::rowwise() %>%
  dplyr::transmute(all(dplyr::c_across(dplyr::everything()))) %>%
  dplyr::pull(1) %>%
  which()

Table_A19i <- dplyr::slice(Table_A19i, -empty_lines)

# Create a formatted flextable object from the data frame
Table_A19i <- flextable::flextable(Table_A19i) %>%
  
  # Merge cells for the last row from column 2 onwards
  flextable::merge_at(i = nrow(Table_A19i), j = 2:ncol(Table_A19i)) %>%
  
  # Apply text and paragraph styling to all parts of the table
  flextable::style(
    pr_t = officer::fp_text(font.size = 10), # Set font size
    part = "all", # Apply to all parts of the table
    pr_p = officer::fp_par(line_spacing = 1, padding = 0) # Set line spacing and padding
  ) %>%
  
  # Add a horizontal line at row 10
  flextable::hline(i = 10, border = officer::fp_border()) %>%
  
  # Add vertical lines at columns 1 and 5
  flextable::vline(j = c(1, 5), border = officer::fp_border()) %>%
  
  # Auto-fit the table to content width and set the total width to 10.5 units
  flextable::autofit() %>%
  flextable::fit_to_width(10.5)

# Remove the 'models_tA19i' object to free memory
rm(models_tA19i)


##### TABLE A19ii #####
# Load the pre-saved RData file that contains the model results for Table A19ii
load("results/TableA19ii_models.RData")

# Create a vector of model names for labeling the table columns
model_names <- c(paste("Conflicts"), paste("Log Conflicts"), paste("Deaths"), paste("Log Deaths"),
                 paste("ACLED\nConflicts"), paste("ACLED\nLog Conflicts"), paste("ACLED\nDeaths"), paste("ACLED\nLog Deaths"))

# Assign model names to a separate object for printing
model_names.to_print <- model_names

# Compute the standard errors for each model using the Arellano robust covariance matrix (HC3 method)
models.to.print_se <- purrr::map(models_tA19ii, \(x) {
  lmtest::coeftest(x, sandwich::vcovHC(x, method = "arellano", type = "HC3"))[,"Std. Error"]
})

# Extract F-statistics for each model and format them with significance stars
F.stat <- purrr::map(models_tA19ii, \(x) {
  # Get the summary of the model using robust standard errors
  summ <- summary(x, vcov. = sandwich::vcovHC(x, method = "arellano", type = "HC3"))
  
  # Compute the p-value for the F-statistic
  .p <- pf(summ$fstatistic["value"], summ$fstatistic["numdf"], summ$fstatistic["dendf"], lower.tail = FALSE)
  
  # Format the F-statistic and append significance stars
  sprintf("%0.3f%s", summ$fstatistic["value"],
          dplyr::case_when(
            .p < 0.01 ~ "***", # Three stars for p < 0.01
            .p < 0.05 ~ "**",  # Two stars for p < 0.05
            .p < 0.1 ~ "*",    # One star for p < 0.1
            TRUE ~ ""          # No stars otherwise
          ))
}) %>% unlist() %>% c("F Statistic", .)

# Extract degrees of freedom (df) for each model
df <- purrr::map(models_tA19ii, \(x) {
  # Get the summary of the model using robust standard errors
  summ <- summary(x, vcov. = sandwich::vcovHC(x, method = "arellano", type = "HC3"))
  
  # Concatenate numerator and denominator degrees of freedom as a string
  paste(summ$fstatistic[2:3], collapse = ";")
}) %>% unlist() %>% c("df", .)

# Generate an HTML table using stargazer with robust standard errors and additional F-stat and df rows
Table_A19ii <- suppressWarnings(
  stargazer::stargazer(models_tA19ii, type = "html",
                       
                       # Include standard errors for models
                       se = models.to.print_se,
                       
                       # Set the caption for the dependent variable
                       dep.var.caption = "Votes turnout 2011",
                       
                       # Omit the automatic F-statistic row (added separately)
                       omit.stat = "f",
                       
                       # Add custom lines for F-statistics and degrees of freedom
                       add.lines = list(F.stat, df)
  )
) %>% paste0(collapse = "")

# Parse the HTML table into a data frame and remove the first 5 rows (header rows generated by stargazer)
Table_A19ii <- xml2::read_html(Table_A19ii) %>% rvest::html_table() %>% as.data.frame() %>% dplyr::slice(-c(1:5))

# Set the column names using 'model_names.to_print', removing the first row from the data frame
Table_A19ii <- magrittr::set_names(Table_A19ii, c(" ", model_names.to_print)) %>% dplyr::slice(-1)

# Identify rows that are completely empty (all columns contain empty strings) and remove them
empty_lines <- Table_A19ii %>%
  dplyr::transmute(across(dplyr::everything(), \(x) nchar(x) == 0)) %>%
  dplyr::rowwise() %>%
  dplyr::transmute(all(dplyr::c_across(dplyr::everything()))) %>%
  dplyr::pull(1) %>%
  which()

Table_A19ii <- dplyr::slice(Table_A19ii, -empty_lines)

# Convert the cleaned data frame into a flextable object for formatting and visualization
Table_A19ii <- flextable::flextable(Table_A19ii) %>%
  
  # Merge cells in the last row from column 2 onwards
  flextable::merge_at(i = nrow(Table_A19ii), j = 2:ncol(Table_A19ii)) %>%
  
  # Apply text and paragraph styling to all parts of the table
  flextable::style(
    pr_t = officer::fp_text(font.size = 9), # Set font size to 9
    part = "all", # Apply to all parts of the table
    pr_p = officer::fp_par(line_spacing = 1, padding = 0) # Set line spacing and padding
  ) %>%
  
  # Add a horizontal line at row 18
  flextable::hline(i = 18, border = officer::fp_border()) %>%
  
  # Add vertical lines at columns 1 and 5
  flextable::vline(j = c(1, 5), border = officer::fp_border()) %>%
  
  # Auto-fit the table to content width and set the total width to 10.5 units
  flextable::autofit() %>%
  flextable::fit_to_width(10.5)

# Remove the 'models_tA19ii' object to free up memory
rm(models_tA19ii)


##### TABLE A19iii #####
# Load the pre-saved RData file that contains the model results
load("results/TableA19iii_models.RData")

# Create a vector of model names to label the results
model_names <- c(paste("Conflicts"), paste("Log Conflicts"), paste("Deaths"), paste("Log Deaths"),
                 paste("ACLED\nConflicts"), paste("ACLED\nLog Conflicts"), paste("ACLED\nDeaths"), paste("ACLED\nLog Deaths"))

# Assign the model names to be printed as-is
model_names.to_print <- model_names

# Extract standard errors for each model in 'models_tA19iii' using heteroskedasticity-consistent covariance matrix estimation
models.to.print_se <- purrr::map(models_tA19iii, \(x) {
  lmtest::coeftest(x, sandwich::vcovHC(x, method = "arellano", type = "HC3"))[,"Std. Error"]
})

# Extract F-statistics for each model in 'models_tA19iii'
F.stat <- purrr::map(models_tA19iii, \(x) {
  # Get summary statistics for each model using heteroskedasticity-consistent covariance matrix
  summ <- summary(x, vcov. = sandwich::vcovHC(x, method = "arellano", type = "HC3"))
  
  # Calculate the p-value for the F-statistic
  .p <- pf(summ$fstatistic["value"], summ$fstatistic["numdf"], summ$fstatistic["dendf"], lower.tail = FALSE)
  
  # Format the F-statistic with significance stars based on the p-value
  sprintf("%0.3f%s", summ$fstatistic["value"],
          dplyr::case_when(
            .p < 0.01 ~ "***", # Three stars for p < 0.01
            .p < 0.05 ~ "**",  # Two stars for p < 0.05
            .p < 0.1 ~ "*",    # One star for p < 0.1
            TRUE ~ ""          # No stars otherwise
          ))
}) %>% unlist %>% c("F Statistic", .)

# Extract the degrees of freedom (df) for each model
df <- purrr::map(models_tA19iii, \(x) {
  # Get summary statistics for each model using heteroskedasticity-consistent covariance matrix
  summ <- summary(x, vcov. = sandwich::vcovHC(x, method = "arellano", type = "HC3"))
  
  # Concatenate the numerator and denominator degrees of freedom as a string
  paste(summ$fstatistic[2:3], collapse = ";")
}) %>% unlist %>% c("df", .)

# Generate a formatted table using stargazer, including custom standard errors, F-statistics, and degrees of freedom
Table_A19iii <- suppressWarnings(
  stargazer::stargazer(models_tA19iii, type = "html",
                       
                       # Include custom standard errors
                       se = models.to.print_se,
                       
                       # Set the caption for the dependent variable
                       dep.var.caption = "Turnout 2018",
                       
                       # Omit the F-statistic row (added separately)
                       omit.stat = "f",
                       
                       # Add custom lines for F-statistics and degrees of freedom
                       add.lines = list(F.stat, df)
  )
) %>% paste0(collapse = "")

# Parse the generated HTML table into a data frame for manipulation
Table_A19iii <- xml2::read_html(Table_A19iii) %>% rvest::html_table() %>% as.data.frame() %>% dplyr::slice(-c(1:5))

# Set new column names using 'model_names.to_print', excluding the first row
Table_A19iii <- magrittr::set_names(Table_A19iii, c(" ", model_names.to_print)) %>% dplyr::slice(-1)

# Identify empty rows (all columns are empty strings) and remove them
empty_lines <- Table_A19iii %>%
  dplyr::transmute(across(dplyr::everything(), \(x) nchar(x) == 0)) %>%
  dplyr::rowwise() %>%
  dplyr::transmute(all(dplyr::c_across(dplyr::everything()))) %>%
  dplyr::pull(1) %>%
  which()

Table_A19iii <- dplyr::slice(Table_A19iii, -empty_lines)

# Create a formatted flextable object from the data frame
Table_A19iii <- flextable::flextable(Table_A19iii) %>%
  
  # Merge cells for the last row from column 2 onwards
  flextable::merge_at(i = nrow(Table_A19iii), j = 2:ncol(Table_A19iii)) %>%
  
  # Apply text and paragraph styling to all parts of the table
  flextable::style(
    pr_t = officer::fp_text(font.size = 9), # Set font size
    part = "all", # Apply to all parts of the table
    pr_p = officer::fp_par(line_spacing = 1, padding = 0) # Set line spacing and padding
  ) %>%
  
  # Add a horizontal line at row 26
  flextable::hline(i = 26, border = officer::fp_border()) %>%
  
  # Add vertical lines at columns 1 and 5
  flextable::vline(j = c(1, 5), border = officer::fp_border()) %>%
  
  # Auto-fit the table to content width and set the total width to 10.5 units
  flextable::autofit() %>%
  flextable::fit_to_width(10.5)

# Remove the 'models_tA19iii' object to free memory
rm(models_tA19iii)
