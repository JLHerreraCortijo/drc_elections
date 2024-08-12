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


# Load the pre-saved RData file that contains the model results for Table A2c
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

# Define a function to add significance stars to p-values
p_stars <- function(p) dplyr::case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.1 ~ "*", TRUE ~ "")

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


# Load the pre-saved RData file that contains the model results for Table A2c
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

# Load the pre-saved RData file that contains the model results for Table A2c
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


# Load the pre-saved RData file that contains the model results for Table A2c
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


# Load the pre-saved RData file that contains the model results for Table A2c
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


# Load the pre-saved RData file that contains the model results for Table A2c
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
