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


##### TABLE A2d ######



vars <- c("n.conflicts","log_n.conflicts","n.deaths","log_n.deaths")

periods <- conflict.aggregated_by_type  %>% dplyr::pull("year") %>% unique() %>% purrr::discard(is.na) %>% sort()



models.to.print <- 1:10 %>% purrr::map(~{
  k <- .x
  borders <- congo.territoire.borders %>% dplyr::slice(-1)  
  
  w1nb<- borders %>% sf::st_centroid(borders)  %>% spdep::knearneigh(k=k, longlat = T) %>% spdep::knn2nb() 
  
  listw1nb <- w1nb %>%spdep::nb2listw(style="W")
  neighbors_sf <- as(spdep::nb2lines(w1nb, coords = coordinates(as(borders,"Spatial"))), 'sf')
  neighbors_sf <- sf::st_set_crs(neighbors_sf, sf::st_crs(borders))
  
  ggplot2::ggplot(congo.territoire.borders) + 
    ggplot2::geom_sf(fill=NA) +
    ggplot2::theme_void()+
    ggplot2::geom_sf(data=neighbors_sf,color="red")
  ggplot2::ggsave(here::here(paste0("document/knn/nb_",k,".png")),width = 10,height = 10)
  
  vars  %>% purrr::map(~purrr::map(periods,function(period,var){
    # var <- vars[1]
    # period <- periods[1]
    to.model <- conflict.aggregated_by_type %>%  dplyr::filter(n.conflicts >0)%>%  dplyr::filter(year==period) %>% dplyr::group_by(index.data,year) %>% dplyr::summarise(dplyr::across(c(n.conflicts,n.deaths),sum,na.rm=T),.groups = "drop") %>% dplyr::mutate(log_n.conflicts=log10(n.conflicts+0.1),log_n.deaths=log10(n.deaths+0.1))  %>% select(index=index.data,year,dplyr::one_of(var))  %>% dplyr::mutate(year=paste(var,year,sep = "_"))
    #%>% tidyr::pivot_wider(names_from = "year",values_from = dplyr::all_of(var))
    
    
    # to.model <- conflict.aggregated_by_type %>% group_by(index.data,year) %>% dplyr::mutate(log_n.conflicts=log10(sum(n.conflicts,na.rm = TRUE)+0.1),log_n.deaths=log10(sum(n.deaths,na.rm = TRUE)+0.1)) %>% dplyr::ungroup()%>% dplyr::filter(year==period) %>% select(index=index.data,dplyr::one_of(var)) %>% group_by(index) %>% summarise(across(dplyr::one_of(var),~sum(.,na.rm = TRUE)),.groups = "drop")
    
    
    to.model <- share %>% dplyr::filter(year==period) %>%  left_join(to.model,by=c("index")) %>% mutate(dplyr::across(dplyr::one_of(var),~replace_na(.,0))) %>% rename(region=label) 
    
    
    to.model <- borders %>% dplyr::left_join(to.model,by=c(index.data="index")) 
    
    
    formula <- as.formula(paste0("votes_share ~ ",var))
    
    to.model %<>% as.data.frame
    to.model %<>% dplyr::select(votes_share,dplyr::one_of(var))
    
    
    m <- spatialreg::lmSLX(formula,to.model,listw = listw1nb,zero.policy = TRUE)
    
    # summary(m)
    # spatialreg::impacts(m)
    # summary(spatialreg::impacts(m),zstats=T)
  },var=.x )) %>% purrr::flatten() 
})


model_names <- c(paste("Conflicts",periods),paste("Log Conflicts",periods),paste("Deaths",periods),paste("Log Deaths",periods))

# https://duckduckgo.com/?q=spatial+econometric+models+model+selection&atb=v168-1&ia=web
models.to.print %>%purrr::map( ~purrr::map(.x,AIC) %>% unlist) %>% unlist %>% matrix(nrow=10,byrow = T) %>% as.data.frame() %>% magrittr::set_colnames(model_names) %>%
  dplyr::mutate(k=dplyr::row_number()) %>%
  tidyr::pivot_longer(cols=-k, names_to = "model",values_to = "AIC") %>%
  ggplot2::ggplot(ggplot2::aes(y=AIC,x=k)) + ggplot2::geom_point() +
  ggplot2::facet_wrap(~model,ncol = 3,scales="free_y") +
  ggplot2::scale_x_continuous(breaks = 1:10) + 
  ggplot2::geom_vline(xintercept = 6,color="red") 
ggplot2::ggsave(filename=here::here("document/knn/AIC_detail.png"))


models.to.print <- models.to.print %>% purrr::pluck(6)
model_names.to_print <- c(paste0("Conflicts\n",periods),paste0("Log Conflicts\n",periods),paste0("Deaths\n",periods),paste0("Log Deaths\n",periods))



models.to.print_se <- map(models.to.print,~coeftest(.x, vcovHC(.x, method="arellano", type="HC3"))[,"Std. Error"])



F.stat <-map(models.to.print,~{
  summ <- summary(.x,vcov.=vcovHC(.x, method="arellano", type="HC3"))
  .p <- pf(summ$fstatistic["value"],summ$fstatistic["numdf"],summ$fstatistic["dendf"],lower.tail = F)
  sprintf("%0.3f%s",summ$fstatistic["value"],
          case_when(.p<0.01 ~"***",
                    .p<0.05 ~"**",
                    .p<0.1~"*",
                    TRUE ~""))
  
  
})  %>% unlist %>% c("F Statistic",.)

df <-map(models.to.print,~{
  summ <- summary(.x,vcov.=vcovHC(.x, method="arellano", type="HC3"))
  
  paste(summ$fstatistic[2:3],collapse=";")
  
  
})  %>% unlist %>% c("df",.)


Table_A2d <- suppressWarnings(stargazer::stargazer(models.to.print,type="html",
                                                   se=models.to.print_se,
                                                   
                                                   column.labels=model_names,dep.var.caption = "Votes share",
                                                   omit.stat = "f",
                                                   add.lines = list(F.stat,
                                                                    df))) %>% paste0(collapse = "")

Table_A2d <- read_html(Table_A2d) %>% html_table() %>% as.data.frame() %>% slice(-c(1:5))
table_names <- as.vector(Table_A2d %>% slice(1) %>% unlist)
table_names[1] <- " "
Table_A2d <- set_names(Table_A2d,c(" ",model_names.to_print)) %>% slice(-1)

empty_lines <- Table_A2d %>% transmute(across(everything(),~nchar(.)==0)) %>% rowwise() %>% transmute(all(c_across(everything()))) %>% pull(1) %>% which()

Table_A2d %<>% slice(-empty_lines)

p_stars <- function(p) dplyr::case_when(p<0.01 ~ "***",p <0.05 ~"**", p < 0.1 ~ "*",TRUE~"")


options(scipen=999)

k <- 6
borders <- congo.territoire.borders %>% dplyr::slice(-1)  

w1nb<- borders %>% sf::st_centroid(borders)  %>% spdep::knearneigh(k=k, longlat = T) %>% spdep::knn2nb() 

listw1nb <- w1nb %>%spdep::nb2listw(style="W")

Table_A2d <- models.to.print %>% purrr::map(~{ 
  x <- .x %>%spatialreg::impacts(listw=listw1nb) %>% summary(zstats=T)
  
  purrr::map2_chr(signif(unlist(x$impacts),3), p_stars(x$pzmat),~paste0(.x,.y)) %>% purrr::map2_chr(signif(unlist(x$se),3),~paste0(.x,"\n(",.y,")"))
  
  
}) %>% unlist %>% matrix(ncol=12,byrow = F) %>% as.data.frame() %>% dplyr::mutate(` `=c("Direct effect","Indirect effect","Total effect"),.before=1) %>% magrittr::set_colnames(names(Table_A2d)) %>% 
  dplyr::bind_rows(Table_A2d %>% dplyr::slice(1:18),.,Table_A2d %>% dplyr::slice(19:nrow(Table_A2d)))

Table_A2d %<>% flextable() %>% merge_at(i=nrow(Table_A2d),j=2:ncol(Table_A2d)) %>%
  style(pr_t=fp_text(font.size = 9),part = "all",pr_p = fp_par(line_spacing = 1,padding = 0))  %>% hline(i = c(18,21),border = officer::fp_border())%>% autofit() %>% flextable::fit_to_width(9)

# Diagnostics    
if(run_diagnostics){
  if(!dir.exists("document/diagnostics/Table_A2d/")){
    dir.create("document/diagnostics/Table_A2d/",recursive = T)
  }
  
  models.to.print %>% purrr::walk2(model_names,~{
    model <- .x
    try(rmarkdown::render("R/lm diagnostics.Rmd",output_file = paste0("document/diagnostics/Table_A2d/",.y,".html")))
  }) 
  
}

