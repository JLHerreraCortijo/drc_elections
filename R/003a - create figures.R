###############################################################################
# Name: 003a - create figures.R
# Author: John Quattrochi (john.quattrochi@gmail.com)
# Assistant: Juan Luis Herrera Cortijo (juan.luis.herrera.cortijo@gmail.com)
# Purpose: Plot manuscript figures. 
# This script is not meant to be run as a stand alone script, but sourced from 
# 003 - update manuscript.R
# Script UUID: 0da234d9-175a-54be-a11a-402b0c2c3243
###############################################################################



#### FIGURES ####

# Check that we have all the necessary subfolders

if(!dir.exists(figures_path)) dir.create(figures_path)



##### FIGURE 1 #####

# This section processes election data to calculate the percentage of votes received 
# by key candidates in 2006, 2011, and 2018. 
# It reshapes the data to facilitate comparison across these years and integrates 
# it with geographical information for mapping. The script handles missing data for 
# administrative units such as lakes by marking them appropriately. 
# Finally, it creates and saves a series of maps that visualize the vote percentages 
# across different regions for each election year, with a clear distinction for 
# areas without available data.


# Select relevant columns from the data frame for further analysis
percentages <- data %>% 
  dplyr::select(index, total.votes_2006, total.votes_2011, total.votes_2018, kabila.votes_2006, kabila.votes_2011, ramazani.votes_2018)

# Compute the percentages of votes for each year and select the relevant columns
percentages %<>% 
  dplyr::mutate(
    percentage_2006 = kabila.votes_2006 / total.votes_2006,  # Calculate percentage for 2006
    percentage_2011 = kabila.votes_2011 / total.votes_2011,  # Calculate percentage for 2011
    percentage_2018 = ramazani.votes_2018 / total.votes_2018 # Calculate percentage for 2018
  ) %>% 
  dplyr::select(index, starts_with("perc")) %>%  # Select columns that start with 'perc'
  dplyr::ungroup()

# Pivot the data frame to have one column for the year and another for the percentage
percentages %<>% 
  tidyr::pivot_longer(cols = -c(index), names_to = "var", values_to = "percent")

# Separate the 'var' column into two columns: one to drop and another for the year
percentages %<>% 
  tidyr::separate(var, c("drop", "year"), sep = "_")

# Convert percentages to a scale of 0 to 100
percentages %<>% 
  dplyr::mutate(percent = percent * 100)

# Add rows for administrative units that are not available, indicating lakes
percentages %<>% 
  dplyr::bind_rows(data.frame(index = "administrative unit not available", year = c("2006", "2011", "2018")))

# Join the percentages data with the shapefile data for mapping
percentages.map <- congo.territoire.borders %>% 
  dplyr::full_join(percentages, by = c("index.data" = "index"))

# Extract lakes for plotting them in a different color
lakes <- percentages.map %>% 
  dplyr::filter(index.data == "administrative unit not available")

# Filter out the lakes from the main map data
percentages.map %<>% 
  dplyr::filter(index.data != "administrative unit not available")

# Plot and save the figure using ggplot2
Figure_1 <- ggplot2::ggplot(percentages.map, ggplot2::aes(fill = percent, color = "")) +
  ggplot2::geom_sf() +  # Plot the main map
  ggplot2::geom_sf(data = lakes, fill = "white") +  # Overlay lakes with white color
  ggplot2::scale_color_manual(values = "gray30") +  # Set color for the 'No data' category
  ggplot2::scale_fill_gradient(name = "% Votes", guide = "colourbar", na.value = "gray50") +  # Define fill gradient for percentage votes
  ggplot2::guides(colour = ggplot2::guide_legend("No data", override.aes = list(color = "gray50", fill = "gray50"))) +  # Customize legend for 'No data'
  ggplot2::facet_wrap(~year, ncol = 2) +  # Create a separate map for each year
  ggplot2::theme_void() +  # Use a minimalistic theme with no background
  ggplot2::theme(
    legend.position = "inside", # Position legend inside the plot
    legend.position.inside = c(0.75, 0.25), # Coordinates for the legend position
    strip.text = ggplot2::element_text(size = 18),  # Customize strip text size
    legend.text = ggplot2::element_text(size = 12),  # Customize legend text size
    legend.title = ggplot2::element_text(size = 14)  # Customize legend title size
  )

# Save the figure if the update_Fig1 flag is set to TRUE
if (update_Fig1) {
  ggplot2::ggsave(
    here::here("manuscript/figures/Figure1.png"),  # Specify the file path
    Figure_1,  # The plot to save
    width = fig1_width,  # Width of the saved figure
    height = fig1_height,  # Height of the saved figure
    units = "in",  # Units for width and height
    dpi = 300  # Resolution of the saved figure
  )
}


##### FIGURE 2 #####

# This section processes election data to visualize the changes in voting percentages over time. 
# First, it selects the relevant columns and reshapes the data to compare election results 
# between 2006, 2011, and 2018. It calculates the differences in voting percentages between 
# these years and prepares the data for visualization. Additionally, it ensures that 
# non-administrative areas (lakes) are accounted for in the analysis. Finally, it creates 
# a plot that illustrates these changes in voting patterns over the specified periods and 
# saves the resulting figure as a PNG file.

# Select the relevant columns and convert to data frame, excluding geometry
percentage.differences <- percentages.map %>% 
  dplyr::select(index.data, year, percent) %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry) %>% 
  # Pivot the data to have one column per year
  tidyr::pivot_wider(names_from = "year", values_from = "percent") %>% 
  # Calculate the differences between years
  dplyr::mutate(
    `2006-2011` = `2011` - `2006`,
    `2011-2018` = `2018` - `2011`
  ) %>% 
  # Select only the difference columns
  dplyr::select(-c(`2006`, `2011`, `2018`)) %>% 
  # Pivot the data back to long format
  tidyr::pivot_longer(cols = -index.data, names_to = "period", values_to = "difference")

# Adding rows for lakes to be shown as NAs since lakes are not administrative units
percentage.differences %<>% 
  dplyr::bind_rows(
    data.frame(index.data = "administrative unit not available", period = c("2006-2011", "2011-2018"))
  )

# Join the percentage differences data with the shapefile data
percentage.differences.map <- congo.territoire.borders %>% 
  dplyr::full_join(percentage.differences, by = "index.data")

# Extract rows for lakes to plot them separately
lakes <- percentage.differences.map %>% 
  dplyr::filter(index.data == "administrative unit not available")

# Filter out rows corresponding to lakes from the main data
percentage.differences.map %<>% 
  dplyr::filter(index.data != "administrative unit not available")

# Plot and save the figure
Figure_2 <- ggplot2::ggplot(percentage.differences.map, ggplot2::aes(fill = difference, color = "")) +
  # Plot the main shapefile data with fill representing the difference in percentages
  ggplot2::geom_sf() + 
  # Plot the lakes data separately with white fill
  ggplot2::geom_sf(data = lakes, fill = "white") +
  # Manually set the color scale for the plot, using gray for unspecified data
  ggplot2::scale_color_manual(values = "gray30") +
  # Set the gradient scale for the fill representing the change in percentage votes
  ggplot2::scale_fill_gradient2(
    name = "Change", 
    guide = "colourbar", 
    na.value = "gray50"
  ) +
  # Customize the legend to show "No data" for unspecified data
  ggplot2::guides(
    colour = ggplot2::guide_legend(
      "No data", 
      override.aes = list(color = "gray50", fill = "gray50")
    )
  ) +
  # Facet the plot by the period of change (2006-2011 and 2011-2018)
  ggplot2::facet_wrap(~period, ncol = 2) + 
  # Apply a void theme, which removes background and axes
  ggplot2::theme_void() +
  # Customize the theme for the plot
  ggplot2::theme(
    legend.position = "bottom",  # Position the legend at the bottom
    strip.text = ggplot2::element_text(size = 18),  # Set the size of the facet strip text
    legend.text = ggplot2::element_text(size = 12),  # Set the size of the legend text
    legend.title = ggplot2::element_text(size = 14)  # Set the size of the legend title
  )

# Save the figure if the condition is met
if (update_Fig2) {
  ggplot2::ggsave(here::here("manuscript/figures/Figure2.png"), Figure_2, width = fig2_width, height = fig2_height)
}



##### FIGURE 3 #####

# This section creates a histogram to visualize the distribution of changes in vote share 
# between different election periods (2006-2011 and 2011-2018). 
# The script calculates the number of bins for the histogram using Sturges' formula to 
# ensure an appropriate level of detail. It then generates the histogram with distinct 
# panels for each period, labeling the x-axis accordingly. 
# If the condition to update the figure is met, it saves the histogram as a PNG file.


# Create a histogram plot of the percentage differences
Figure_3 <- ggplot2::ggplot(percentage.differences, ggplot2::aes(x = difference)) +
  
  # Add a histogram with dynamically calculated number of bins
  ggplot2::geom_histogram(
    bins = round(1 + 3.322 * log10(nrow(percentage.differences))),  # Calculate number of bins using Sturges' formula
    color = "black",  # Set border color of the bars
    fill = "red"  # Set fill color of the bars
  ) +
  
  # Create separate panels for each period
  ggplot2::facet_wrap(~period, ncol = 2) +
  
  # Set the x-axis label
  ggplot2::xlab("Change in vote share")

# Save the figure if the condition is met
if (update_Fig3) {
  ggplot2::ggsave(here::here("manuscript/figures/Figure3.png"), Figure_3, width = fig3_width, height = fig3_height, units = "in")
}


##### FIGURE 4 #####

# This section processes data to visualize changes in nightlight intensity related 
# to election periods. 
# First, it selects and reshapes the data to include vote shares and separates 
# election years. 
# It defines election periods and calculates mean nightlight values for each period,
#  adjusting to avoid zero values.
# It also calculates the percent change in nightlight between specific years and 
# corrects nightlight values accordingly.
# The final dataset merges vote share data with mean nightlight values, enabling 
# analysis of differences in nightlight 
# between election periods. Lastly, it creates a histogram to illustrate the 
# distribution of these differences, 
# adjusting the y-axis to a logarithmic scale, and saves the figure if required.

# Select relevant columns from the data frame
share <- data %>% 
  dplyr::select(index, label, starts_with("kabila.percent"), starts_with("ramazani.percent")) %>%
  
  # Pivot data to longer format
  tidyr::pivot_longer(
    cols = -c(index, label),
    values_to = "votes_share"
  ) %>% 
  
  # Separate the year from the variable name
  tidyr::separate(name, c("drop", "year"), sep = "_") %>% 
  
  # Convert the year to an integer
  dplyr::mutate(year = as.integer(year)) %>% 
  
  # Remove the 'drop' column
  dplyr::select(-drop) 



# Calculate the mean nightlight values, adjusted slightly to avoid zero values
mean_nightlight <- nightlight_gt30_mean %>% 
  dplyr::mutate(nightlight_mean = nightlight_mean + 0.01) %>% 
  dplyr::select(index = index.data, year, nightlight_mean) %>% 
  
  # Map years to election periods
  dplyr::mutate(election = elections[as.character(year)]) %>% 
  
  # Group by index and election period, then calculate the mean nightlight
  dplyr::group_by(index, election) %>% 
  dplyr::summarise(
    across(nightlight_mean, ~mean(., na.rm = TRUE)),
    .groups = "drop"
  ) %>% 
  
  # Filter out rows without election period information
  dplyr::filter(!is.na(election))

# Calculate the mean nightlight for 2012-2013
DMSP_2012_13 <- nightlight_gt30_mean %>% 
  dplyr::mutate(nightlight_mean = nightlight_mean + 0.01) %>% 
  dplyr::select(index = index.data, year, nightlight_mean) %>% 
  
  # Filter for years 2012 and 2013, then group by index
  dplyr::filter(year %in% c(2012, 2013)) %>% 
  dplyr::group_by(index) %>% 
  
  # Calculate the mean nightlight for these years
  dplyr::summarise(
    across(nightlight_mean, ~mean(., na.rm = TRUE)),
    .groups = "drop"
  ) 

# Calculate the percent change in nightlight between 2014 and 2018
VIIR_percent_change <- nightlight_gt30_mean %>% 
  dplyr::mutate(nightlight_mean = nightlight_mean + 0.01) %>% 
  dplyr::select(index = index.data, year, nightlight_mean) %>% 
  
  # Filter for years 2014 and 2018
  dplyr::filter(year %in% c(2014, 2018)) %>% 
  
  # Pivot data to wider format
  tidyr::pivot_wider(
    names_from = year,
    values_from = nightlight_mean
  ) %>% 
  
  # Calculate the percent change in nightlight
  dplyr::mutate(change = (`2018` - `2014`) / `2014`)

# Correct nightlight values using the calculated percent change
VIIR_corrected <- DMSP_2012_13 %>% 
  dplyr::left_join(VIIR_percent_change, by = "index") %>% 
  dplyr::mutate(
    nightlight_mean_corrected = nightlight_mean + (nightlight_mean) * change
  ) %>% 
  dplyr::select(index, nightlight_mean = nightlight_mean_corrected) %>% 
  dplyr::mutate(election = "2018")

# Combine the corrected nightlight values with the original data
mean_nightlight %<>% 
  dplyr::bind_rows(VIIR_corrected) %>% 
  dplyr::mutate(year = as.integer(election)) %>% 
  dplyr::select(-election)

# Merge the vote share data with the mean nightlight data
to.model <- share %>% 
  dplyr::left_join(mean_nightlight, by = c("index", "year")) %>% 
  dplyr::rename(region = label) %>% 
  dplyr::select(-index)

# Calculate differences in nightlight between election periods
differences <- to.model %>% 
  dplyr::select(region, year, nightlight_mean) %>% 
  as.data.frame() %>% 
  
  # Pivot data to wider format
  tidyr::pivot_wider(
    names_from = "year",
    values_from = "nightlight_mean"
  ) %>% 
  
  # Calculate the differences in nightlight between periods
  dplyr::mutate(
    `2006-2011` = `2011` - `2006`,
    `2011-2018` = `2018` - `2011`
  ) %>% 
  
  # Remove the original year columns
  dplyr::select(-c(`2006`, `2011`, `2018`)) %>% 
  
  # Pivot data back to longer format
  tidyr::pivot_longer(
    cols = -region,
    names_to = "period",
    values_to = "difference"
  )

# Create a histogram of the changes in nightlight
Figure_4 <- ggplot2::ggplot(differences, ggplot2::aes(x = difference)) +
  
  # Add a histogram with dynamically calculated number of bins
  ggplot2::geom_histogram(
    bins = floor(1 + 3.322 * log10(nrow(to.model))),  # Calculate number of bins using Sturges' formula
    color = "black",  # Set border color of the bars
    fill = "red"  # Set fill color of the bars
  ) +
  
  # Create separate panels for each period
  ggplot2::facet_wrap(~period, ncol = 2) +
  
  # Set the x-axis label
  ggplot2::xlab("Change in mean nightlight") +
  
  # Set the y-axis to log scale and label it
  ggplot2::scale_y_continuous(trans = "log10") +
  ggplot2::ylab("log10(count)")

# Save the figure if the condition is met
if (update_Fig4) {
  ggplot2::ggsave(here::here("manuscript/figures/Figure4.png"), Figure_4, width = fig4_width, height = fig4_height, units = "in")
}


##### FIGURE 5 #####

# This section analyzes and visualizes the number of conflict-related deaths 
# across different regions and years. 
# It aggregates the total number of deaths per region and year, merges this data 
# with voting share information, and prepares it for panel data analysis. 
# The script then creates a histogram to show the distribution of conflict deaths 
# for regions with more than zero deaths, with separate panels for each year. 
# If required, the resulting figure is saved as a PNG file.

# Aggregate conflict deaths by index and year, summing the number of deaths
total_conflict_deaths <- conflict.aggregated_by_type %>% 
  dplyr::select(index = index.data, year, n.deaths) %>% 
  dplyr::group_by(index, year) %>% 
  dplyr::summarise(across(n.deaths, ~sum(., na.rm = TRUE)), .groups = "drop")

# Merge the conflict deaths data with the share data, replacing NA values in n.deaths with 0
to.model <- share %>% 
  dplyr::left_join(total_conflict_deaths, by = c("index", "year")) %>% 
  dplyr::mutate(n.deaths = tidyr::replace_na(n.deaths, 0)) %>% 
  dplyr::rename(region = label) %>% 
  dplyr::select(-index)

# Convert the data frame to a panel data frame for further analysis
to.model <- plm::pdata.frame(to.model, index = c("region", "year"), drop.index = FALSE)

# Create a histogram plot of the number of conflict deaths for regions with more than 0 deaths
Figure_5 <- to.model %>% 
  data.frame() %>% 
  dplyr::filter(n.deaths > 0) %>% 
  dplyr::select(n.deaths, year) %>%
  dplyr::mutate(n.deaths = as.integer(n.deaths), year = factor(as.character(year))) %>%
  ggplot2::ggplot() + 
  
  # Add histogram of conflict deaths with dynamically calculated number of bins
  ggplot2::geom_histogram(
    ggplot2::aes(x = n.deaths), 
    bins = floor(1 + 3.322 * log10(nrow(to.model  %>% data.frame() %>% dplyr::filter(n.deaths > 0)) / 3)),  # Sturges' formula for number of bins
    color = "black",  # Border color of the bars
    fill = "red"  # Fill color of the bars
  ) + 
  
  # Create separate panels for each year
  ggplot2::facet_wrap(~year, ncol = 3) + 
  
  # Set the title of the plot
  ggplot2::ggtitle("# deaths > 0")

# Save the figure if the condition is met
if (update_Fig5) {
  ggplot2::ggsave(here::here("manuscript/figures/Figure5.png"), Figure_5, width = fig5_width, height = fig5_height, units = "in")
}

##### FIGURE 6 #####

# This section processes and visualizes changes in the number of conflict-related 
# deaths between election periods. It first converts the data to a wide format 
# with separate columns for each year and calculates the changes in deaths between
# 2006-2011 and 2011-2018. The data is then reshaped to long format for visualization.
# The script creates a histogram to show the distribution of these changes, 
# with separate panels for each period and the y-axis on a logarithmic scale. 
# If required, the resulting figure is saved as a PNG file.

# Convert the to.model data to a data frame and remove the votes_share column
.to_plot <- to.model %>% 
  as.data.frame() %>% 
  dplyr::select(-votes_share) %>% 
  
  # Pivot the data to have separate columns for each year
  tidyr::pivot_wider(id_cols = region, names_from = "year", values_from = "n.deaths") %>%
  
  # Calculate the changes in number of deaths between the specified years
  dplyr::mutate(
    change_2006_2011 = `2011` - `2006`,
    change_2011_2018 = `2018` - `2011`
  ) %>%
  
  # Select only the region and change columns
  dplyr::select(region, starts_with("change_"))

# Pivot the data back to long format for plotting
.to_plot %<>% 
  tidyr::pivot_longer(
    cols = -region, 
    values_to = "change"
  )

# Create a histogram plot of the changes in number of deaths
Figure_6 <- .to_plot %>% 
  ggplot2::ggplot() + 
  
  # Add histogram of changes with log10 count on y-axis
  ggplot2::geom_histogram(
    ggplot2::aes(x = change, y = log10(..count..)), 
    bins = floor(1 + 3.322 * log10(nrow(.to_plot) / 2)),  # Sturges' formula for number of bins
    color = "black",  # Border color of the bars
    fill = "red"  # Fill color of the bars
  ) + 
  
  # Create separate panels for each change period
  ggplot2::facet_wrap(~name, ncol = 2)

# Save the figure if the condition is met
if (update_Fig6) {
  ggplot2::ggsave(here::here("manuscript/figures/Figure6.png"), Figure_6, width = fig6_width, height = fig6_height, units = "in")
}


##### FIGURE 7 #####

# This section analyzes and visualizes the number of conflict events across 
# different regions and years. It aggregates the total number of conflict events 
# per region and year, merges this data with voting share information, 
# and prepares it for panel data analysis. 
# The script then creates a histogram to show the distribution of conflict 
# events for regions with more than zero events, with separate panels for each year. 
# If required, the resulting figure is saved as a PNG file.

# Aggregate conflict events by index and year, summing the number of conflicts
total_conflict_events <- conflict.aggregated_by_type %>% 
  dplyr::select(index = index.data, year, n.conflicts) %>% 
  dplyr::group_by(index, year) %>% 
  dplyr::summarise(across(n.conflicts, ~sum(., na.rm = TRUE)), .groups = "drop")

# Merge the conflict events data with the share data, replacing NA values in n.conflicts with 0
to.model <- share %>% 
  dplyr::left_join(total_conflict_events, by = c("index", "year")) %>% 
  dplyr::mutate(n.conflicts = tidyr::replace_na(n.conflicts, 0)) %>% 
  dplyr::rename(region = label) %>% 
  dplyr::select(-index)

# Convert the data frame to a panel data frame for further analysis
to.model <- plm::pdata.frame(to.model, index = c("region", "year"), drop.index = FALSE)

# Create a histogram plot of the number of conflict events for regions with more than 0 conflicts
Figure_7 <- to.model %>% 
  as.data.frame() %>% 
  dplyr::filter(n.conflicts > 0) %>% 
  ggplot2::ggplot() + 
  
  # Add histogram of conflict events with dynamically calculated number of bins
  ggplot2::geom_histogram(
    ggplot2::aes(x = n.conflicts), 
    bins = floor(1 + 3.322 * log10(nrow(to.model %>% dplyr::filter(n.conflicts > 0)) / 3)),  # Sturges' formula for number of bins
    color = "black",  # Border color of the bars
    fill = "red"  # Fill color of the bars
  ) + 
  
  # Create separate panels for each year
  ggplot2::facet_wrap(~year, ncol = 3) + 
  
  # Set the title of the plot
  ggplot2::ggtitle("# conflicts > 0")

# Save the figure if the condition is met
if (update_Fig7) {
  ggplot2::ggsave(here::here("manuscript/figures/Figure7.png"), Figure_7, width = fig7_width, height = fig7_height, units = "in")
}

##### FIGURE 8 ####

# This section analyzes and visualizes changes in the number of conflict events 
# between election periods. First, it converts the data to a wide format with 
# separate columns for each year and calculates the changes in conflicts between 
# 2006-2011 and 2011-2018. The data is then reshaped to a long format for visualization.
# The script creates a histogram to show the distribution of these changes, 
# with the y-axis on a logarithmic scale, and separate panels for each period.
# If required, the resulting figure is saved as a PNG file.

# Convert the to.model data to a data frame and remove the votes_share column
.to_plot <- to.model %>% 
  as.data.frame() %>% 
  dplyr::select(-votes_share) %>% 
  
  # Pivot the data to have separate columns for each year
  tidyr::pivot_wider(id_cols = region, names_from = "year", values_from = "n.conflicts") %>%
  
  # Calculate the changes in number of conflicts between the specified years
  dplyr::mutate(
    change_2006_2011 = `2011` - `2006`,
    change_2011_2018 = `2018` - `2011`
  ) %>%
  
  # Select only the region and change columns
  dplyr::select(region, starts_with("change_"))

# Pivot the data back to long format for plotting
.to_plot %<>% 
  tidyr::pivot_longer(
    cols = -region, 
    values_to = "change"
  )

# Create a histogram plot of the changes in number of conflicts
Figure_8 <- .to_plot %>% 
  ggplot2::ggplot() + 
  
  # Add histogram of changes with log10 count on y-axis
  ggplot2::geom_histogram(
    ggplot2::aes(x = change, y = log10(..count..)), 
    bins = floor(1 + 3.322 * log10(nrow(.to_plot) / 2)),  # Sturges' formula for number of bins
    color = "black",  # Border color of the bars
    fill = "red"  # Fill color of the bars
  ) + 
  
  # Create separate panels for each change period
  ggplot2::facet_wrap(~name, ncol = 2)

# Save the figure if the condition is met
if (update_Fig8) {
  ggplot2::ggsave(here::here("manuscript/figures/Figure8.png"), Figure_8, width = fig8_width, height = fig8_height, units = "in")
}


##### FIGURE A1 #####

# This section analyzes and visualizes voter turnout data across different years.
# First, it selects columns related to voter turnout and pivots the data to a 
# long format for easier comparison. The data is then transformed to show turnout 
# percentages and merged with geographical shapefile data. Non-administrative units, 
# like lakes, are accounted for separately in the visualization.
# Finally, it creates a map to display voter turnout percentages across regions 
# for the years 2006, 2011, and 2018, and saves the figure as a PNG file if required.

# Select columns related to voter turnout from the data
percentages <- data %>% 
  dplyr::select(index, starts_with("turnout_"))

# Pivot the data to have one row per index per year
percentages %<>% 
  tidyr::pivot_longer(
    cols = -c(index), 
    names_to = "var", 
    values_to = "percent"
  )

# Separate the year from the variable name
percentages %<>% 
  tidyr::separate(var, c("drop", "year"), sep = "_")

# Convert the percentage values to actual percentages (multiplying by 100)
percentages %<>% 
  dplyr::mutate(percent = percent * 100)

# Adding rows for lakes to be shown as NAs since lakes are not administrative units
percentages %<>% 
  dplyr::bind_rows(
    data.frame(index = "administrative unit not available", year = c("2006", "2011", "2018"))
  )

# Join the percentages data with the shapefile data
percentages.map <- congo.territoire.borders %>% 
  dplyr::full_join(percentages, by = c("index.data" = "index"))

# Extract rows for lakes to plot them separately
lakes <- percentages.map %>% 
  dplyr::filter(index.data == "administrative unit not available")

# Filter out rows corresponding to lakes from the main data
percentages.map %<>% 
  dplyr::filter(index.data != "administrative unit not available")

# Plot and save the figure
Figure_A1 <- ggplot2::ggplot(percentages.map, ggplot2::aes(fill = percent, color = "")) +
  ggplot2::geom_sf() +  # Add spatial polygons
  ggplot2::geom_sf(data = lakes, fill = "white") +  # Add lakes with white fill
  ggplot2::scale_color_manual(values = "gray30") +  # Set manual color scale for the border
  ggplot2::scale_fill_gradient(name = "Turnout (%)", guide = "colourbar", na.value = "gray50") +  # Set fill gradient for turnout percentage
  ggplot2::guides(colour = ggplot2::guide_legend("No data", override.aes = list(color = "gray50", fill = "gray50"))) +  # Customize the legend for "No data"
  ggplot2::facet_wrap(~year, ncol = 2) +  # Create separate panels for each year
  ggplot2::theme_void() +  # Use a theme without axes or background
  ggplot2::theme(
    legend.position = "inside", # Position legend inside the plot
    legend.position.inside = c(0.75, 0.3), # Coordinates for the legend position
    strip.text = ggplot2::element_text(size = 18),  # Set the size of the facet strip text
    legend.text = ggplot2::element_text(size = 12),  # Set the size of the legend text
    legend.title = ggplot2::element_text(size = 14)  # Set the size of the legend title
  )

# Save the figure if the condition is met
if (update_FigA1) {
  ggplot2::ggsave(here::here("manuscript/figures/FigureA1.png"), Figure_A1, width = figA1_width, height = figA1_height, units = "in")
}

##### FIGURE A1b #####

# This section analyzes and visualizes changes in voter turnout between election 
# periods. First, it converts the data to a wide format with separate columns for 
# each year and calculates the changes in turnout between 2006-2011 and 2011-2018.
# The data is then reshaped to a long format for easier visualization.
# The script creates a map to show the distribution of these turnout changes, 
# with separate panels for each period and the color scale on a logarithmic scale.
# If required, the resulting figure is saved as a PNG file.

# Select relevant columns from percentages.map and convert to data frame, excluding geometry column
percentage.differences <- percentages.map %>% 
  dplyr::select(index.data, year, percent) %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry) %>% 
  
  # Pivot the data to have separate columns for each year
  tidyr::pivot_wider(names_from = "year", values_from = "percent") %>%
  
  # Calculate percentage differences between election years
  dplyr::mutate(
    `2006-2011` = `2011` - `2006`,
    `2011-2018` = `2018` - `2011`
  ) %>% 
  
  # Remove the original year columns
  dplyr::select(-c(`2006`, `2011`, `2018`)) %>% 
  
  # Pivot the data back to long format for plotting
  tidyr::pivot_longer(
    cols = -index.data,
    names_to = "period",
    values_to = "difference"
  )

# Add rows for lakes to be shown as NAs since lakes are not administrative units
percentage.differences %<>% 
  dplyr::bind_rows(
    data.frame(index.data = "administrative unit not available", period = c("2006-2011", "2011-2018"))
  )

# Join the percentage differences data with the shapefile data
percentage.differences.map <- congo.territoire.borders %>% 
  dplyr::full_join(percentage.differences, by = "index.data")

# Extract rows for lakes to plot them separately
lakes <- percentage.differences.map %>% 
  dplyr::filter(index.data == "administrative unit not available")

# Filter out rows corresponding to lakes from the main data
percentage.differences.map %<>% 
  dplyr::filter(index.data != "administrative unit not available")

# Create the plot for the differences in voter turnout
Figure_A1b <- ggplot2::ggplot(percentage.differences.map, ggplot2::aes(fill = difference, color = "")) +
  ggplot2::geom_sf() +  # Add spatial polygons
  ggplot2::geom_sf(data = lakes, fill = "white") +  # Add lakes with white fill
  ggplot2::scale_color_manual(values = "gray30") +  # Set manual color scale for the border
  ggplot2::scale_fill_gradient2(name = "Turnout Change", guide = "colourbar", na.value = "gray50") +  # Set fill gradient for turnout change
  ggplot2::guides(colour = ggplot2::guide_legend("No data", override.aes = list(color = "gray50", fill = "gray50"))) +  # Customize the legend for "No data"
  ggplot2::facet_wrap(~period, ncol = 2) +  # Create separate panels for each period
  ggplot2::theme_void() +  # Use a theme without axes or background
  ggplot2::theme(
    legend.position = "bottom",  # Position the legend at the bottom
    strip.text = ggplot2::element_text(size = 18),  # Set the size of the facet strip text
    legend.text = ggplot2::element_text(size = 12),  # Set the size of the legend text
    legend.title = ggplot2::element_text(size = 14)  # Set the size of the legend title
  )

# Save the figure if the condition is met
if (update_FigA1b) {
  ggplot2::ggsave(here::here("manuscript/figures/FigureA1b.png"), Figure_A1b, width = figA1b_width, height = figA1b_height, units = "in")
}


##### FIGURE A2 #####

# This section analyzes and visualizes the relationship between vote share and 
# the number of conflict events. It first selects relevant columns related to 
# vote share from the data and pivots the data to a longer format for easier comparison.
# It then and aggregates conflict events by index and lection period, summing the 
# number of conflicts. The conflict events data is merged with the share data, 
# replacing NA values in n.conflicts with 0, and categorizes the number of conflicts 
# into "0 conflicts" and "1+ conflicts". A jitter plot is created to show the vote 
# share vs. number of conflicts with separate panels for each year. If required, 
# the resulting figure is saved as a PNG file.

# Select relevant columns related to vote share from the data
share <- data %>% 
  dplyr::select(index, label, starts_with("kabila.percent"), starts_with("ramazani.percent")) %>%
  
  # Pivot the data to longer format for easier comparison
  tidyr::pivot_longer(
    cols = -c(index, label), 
    values_to = "votes_share"
  ) %>% 
  
  # Separate the year from the variable name
  tidyr::separate(name, c("drop", "year"), sep = "_") %>% 
  
  # Convert the year to an integer
  dplyr::mutate(year = as.integer(year)) %>% 
  
  # Remove the 'drop' column
  dplyr::select(-drop)


# Aggregate conflict events by index and year, summing the number of conflicts
total_conflict_events <- conflict.aggregated_by_type %>% 
  dplyr::select(index = index.data, year, n.conflicts) %>% 
  dplyr::group_by(index, year) %>% 
  dplyr::summarise(across(n.conflicts, ~sum(., na.rm = TRUE)), .groups = "drop")

# Merge the conflict events data with the share data, replacing NA values in n.conflicts with 0
to.plot <- share %>% 
  dplyr::left_join(total_conflict_events, by = c("index", "year")) %>% 
  dplyr::mutate(n.conflicts = tidyr::replace_na(n.conflicts, 0)) %>% 
  dplyr::rename(region = label) %>% 
  dplyr::select(-index)

# Categorize the number of conflicts into "0 conflicts" and "1+ conflicts"
to.plot %<>% 
  dplyr::mutate(n.conflicts = dplyr::case_when(
    n.conflicts == 0 ~ "0 conflicts", 
    TRUE ~ "1+ conflicts"
  ))

# Set seed for reproducibility of jitter plot
set.seed(200)

# Create a jitter plot of vote share vs. number of conflicts
Figure_A2 <- to.plot %>% 
  ggplot2::ggplot(ggplot2::aes(x = n.conflicts, y = votes_share)) + 
  ggplot2::geom_jitter(alpha = 0.5, width = 0.1, size = 1) +  # Add jitter points with transparency
  ggplot2::facet_wrap(~year, ncol = 1) +  # Create separate panels for each year
  ggplot2::xlab(NULL) +  # Remove x-axis label
  ggplot2::ylab("Votes share") +  # Set y-axis label
  ggplot2::scale_y_continuous(labels = scales::percent)  # Format y-axis labels as percentages

# Save the figure if the condition is met
if (update_FigA2) {
  ggplot2::ggsave(here::here("manuscript/figures/FigureA2.png"), Figure_A2, width = figA2_width, height = figA2_height, units = "in")
}


##### FIGURE A3 #####

# This section analyzes and visualizes the relationship between voter turnout and
# the number of conflict events. First, it selects columns related to voter 
# turnout from the data and pivots the data to a longer format for easier comparison. 
# It then aggregates conflict events by index and election, summing the number of conflicts. 
# The conflict events data is merged with the turnout data, replacing NA values 
# in n.conflicts with 0, and categorizes the number of conflicts into "0 conflicts" 
# and "1+ conflicts". A jitter plot is created to show the voter turnout vs. 
# number of conflicts with separate panels for each year. If required, the resulting 
# figure is saved as a PNG file.

# Select columns related to voter turnout from the data
turnout <- data %>% 
  dplyr::select(index, starts_with("turnout_"))

# Pivot the data to have one row per index per year
turnout %<>% 
  tidyr::pivot_longer(
    cols = -c(index), 
    names_to = "var", 
    values_to = "percent"
  )

# Separate the year from the variable name
turnout %<>% 
  tidyr::separate(var, c("drop", "year"), sep = "_")

# Convert the year to an integer
turnout %<>% 
  dplyr::mutate(year = as.integer(year))

# Aggregate conflict events by index and year, summing the number of conflicts
total_conflict_events <- conflict.aggregated_by_type %>% 
  dplyr::select(index = index.data, year, n.conflicts) %>% 
  dplyr::group_by(index, year) %>% 
  dplyr::summarise(across(n.conflicts, ~sum(., na.rm = TRUE)), .groups = "drop")

# Merge the conflict events data with the turnout data, replacing NA values in n.conflicts with 0
to.plot <- turnout %>% 
  dplyr::left_join(total_conflict_events, by = c("index", "year")) %>% 
  dplyr::mutate(n.conflicts = tidyr::replace_na(n.conflicts, 0)) %>% 
  dplyr::select(-index)

# Categorize the number of conflicts into "0 conflicts" and "1+ conflicts"
to.plot %<>% 
  dplyr::mutate(n.conflicts = dplyr::case_when(
    n.conflicts == 0 ~ "0 conflicts", 
    TRUE ~ "1+ conflicts"
  ))

# Set seed for reproducibility of jitter plot
set.seed(200)

# Create a jitter plot of turnout percentage vs. number of conflicts
Figure_A3 <- to.plot %>% 
  ggplot2::ggplot(ggplot2::aes(x = n.conflicts, y = percent)) + 
  ggplot2::geom_jitter(alpha = 0.5, width = 0.1, size = 1) +  # Add jitter points with transparency
  ggplot2::facet_wrap(~year, ncol = 1) +  # Create separate panels for each year
  ggplot2::xlab(NULL) +  # Remove x-axis label
  ggplot2::ylab("Turnout") +  # Set y-axis label
  ggplot2::scale_y_continuous(labels = scales::percent)  # Format y-axis labels as percentages

# Save the figure if the condition is met
if (update_FigA3) {
  ggplot2::ggsave(here::here("manuscript/figures/FigureA3.png"), Figure_A3, width = figA3_width, height = figA3_height, units = "in")
}

##### FIGURE A4 #####

# This section analyzes and visualizes the relationship between vote share for 
# Kabila and the number of conflict events before the election. It selects relevant
#  columns related to vote share from the data and pivots it to a longer format. 
#  The code then aggregates conflict events by index and election, and merges the 
#  conflict data with the vote share data, replacing NA values with 0. It 
#  categorizes the number of conflicts into "0 conflicts" and "1+ conflicts" and 
#  creates a scatter plot with a smoothing line to show the 
#  relationship between vote share and the number of conflicts, with separate 
#  panels for each year. The figure is saved as a PNG file if required.

# Select relevant columns related to vote share from the data
share <- data %>%
  dplyr::select(index, label, starts_with("kabila.percent"), starts_with("ramazani.percent")) %>% 
  # Pivot the data to longer format for easier comparison
  tidyr::pivot_longer(cols = -c(index, label), values_to = "votes_share") %>% 
  # Separate the year from the variable name
  tidyr::separate(name, c("drop", "year"), sep = "_") %>% 
  # Convert the year to an integer
  dplyr::mutate(year = as.integer(year)) %>% 
  # Remove the 'drop' column
  dplyr::select(-drop)


# Aggregate conflict events by index and year, summing the number of conflicts
total_conflict_events <- conflict.aggregated_by_type %>%
  dplyr::select(index = index.data, year, n.conflicts) %>% 
  dplyr::group_by(index, year) %>% 
  dplyr::summarise(across(n.conflicts, ~sum(., na.rm = TRUE)), .groups = "drop")

# Merge the conflict events data with the share data, replacing NA values in n.conflicts with 0
to.plot <- share %>%
  dplyr::left_join(total_conflict_events, by = c("index", "year")) %>% 
  dplyr::mutate(n.conflicts = tidyr::replace_na(n.conflicts, 0)) %>% 
  dplyr::rename(region = label) %>% 
  dplyr::select(-index)

# Create a scatter plot with a smoothing line of vote share vs. number of conflicts
Figure_A4 <- to.plot %>%
  ggplot2::ggplot(ggplot2::aes(x = n.conflicts, y = votes_share)) + 
  ggplot2::geom_point(alpha = 0.2, size = 0.5) +  # Add scatter points with transparency
  ggplot2::geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, formula = y ~ x) +  # Add a smooth line using GLM
  ggplot2::xlab("# Conflicts before election") +  # Set x-axis label
  ggplot2::ylab("% Votes for Kabila") +  # Set y-axis label
  ggplot2::scale_y_continuous(labels = scales::percent) +  # Format y-axis labels as percentages
  ggplot2::facet_wrap(~year, ncol = 2) +  # Create separate panels for each year
  ggplot2::theme(
    axis.title = ggplot2::element_text(size = 16),  # Set size of axis titles
    axis.text = ggplot2::element_text(size = 12),  # Set size of axis text
    strip.text = ggplot2::element_text(size = 18)  # Set size of facet strip text
  )

# Save the figure if the condition is met
if (update_FigA4) {
  ggplot2::ggsave(here::here("manuscript/figures/FigureA4.png"), Figure_A4, width = figA4_width, height = figA4_height, units = "in")
}


##### FIGURE A5 #####

# This section analyzes and visualizes the relationship between vote share for 
# Kabila and the number of conflict-related deaths before the election. It selects 
# relevant columns related to vote share from the data and pivots it to a longer 
# format. The code then aggregates conflict-related deaths by index and election 
# period, and merges the conflict data with the vote share data, replacing NA 
# values with 0. A scatter plot with a smoothing line is created to 
# show the relationship between vote share and the number of deaths, with 
# separate panels for each year. The figure is saved as a PNG file if required.

# Select relevant columns related to vote share from the data
share <- data %>% 
  dplyr::select(index, label, starts_with("kabila.percent"), starts_with("ramazani.percent")) %>% 
  
  # Pivot the data to longer format for easier comparison
  tidyr::pivot_longer(cols = -c(index, label), values_to = "votes_share") %>% 
  
  # Separate the year from the variable name
  tidyr::separate(name, c("drop", "year"), sep = "_") %>% 
  
  # Convert the year to an integer
  dplyr::mutate(year = as.integer(year)) %>% 
  
  # Remove the 'drop' column
  dplyr::select(-drop)



# Aggregate total deaths by index and year, summing the number of deaths
total_deaths <- conflict.aggregated_by_type %>% 
  dplyr::select(index = index.data, year, n.deaths) %>% 
  dplyr::group_by(index, year) %>% 
  dplyr::summarise(across(n.deaths, ~sum(., na.rm = TRUE)), .groups = "drop")

# Merge the total deaths data with the share data, replacing NA values in n.deaths with 0
to.plot <- share %>% 
  dplyr::left_join(total_deaths, by = c("index", "year")) %>% 
  dplyr::mutate(n.deaths = tidyr::replace_na(n.deaths, 0)) %>% 
  dplyr::rename(region = label) %>% 
  dplyr::select(-index)

# Create a scatter plot with a smoothing line of vote share vs. number of deaths
Figure_A5 <- to.plot %>% 
  ggplot2::ggplot(ggplot2::aes(x = n.deaths, y = votes_share)) + 
  
  # Add scatter points with transparency
  ggplot2::geom_point(alpha = 0.2, size = 0.5) + 
  
  # Add a smooth line using GLM
  ggplot2::geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, formula = y ~ x) + 
  
  # Set x-axis label
  ggplot2::xlab("# Deaths before election") + 
  
  # Set y-axis label
  ggplot2::ylab("% Votes for Kabila") + 
  
  # Format y-axis labels as percentages
  ggplot2::scale_y_continuous(labels = scales::percent) + 
  
  # Create separate panels for each year
  ggplot2::facet_wrap(~year, ncol = 2) + 
  
  # Customize theme elements
  ggplot2::theme(
    axis.title = ggplot2::element_text(size = 16),  # Set size of axis titles
    axis.text = ggplot2::element_text(size = 12),  # Set size of axis text
    strip.text = ggplot2::element_text(size = 18)  # Set size of facet strip text
  )

# Save the figure if the condition is met
if (update_FigA5) {
  ggplot2::ggsave(here::here("manuscript/figures/FigureA5.png"), Figure_A5, width = figA5_width, height = figA5_height, units = "in")
}



##### FIGURE A6 #####

# This section analyzes and visualizes the relationship between vote share for 
# Kabila and the number of conflict events before the election. It selects relevant
#  columns related to vote share from the data and pivots it to a longer format. 
#  The code then aggregates conflict events by index and election period, and 
#  merges the conflict data with the vote share data, replacing NA values 
#  with 0. A scatter plot with a smoothing line is created to show the relationship 
#  between vote share and the number of conflicts, with separate panels for each year. 
#  The figure is saved as a PNG file if required.

# Select the columns 'index', 'label', and any columns starting with 'kabila.percent' or 'ramazani.percent'
share <- data %>% 
  dplyr::select(index, label, starts_with("kabila.percent"), starts_with("ramazani.percent")) %>%
  # Transform the dataset to a longer format, keeping 'index' and 'label' columns fixed
  tidyr::pivot_longer(cols = -c(index, label), values_to = "votes_share") %>%
  # Split the 'name' column into 'drop' and 'year' based on the underscore separator
  tidyr::separate(name, c("drop", "year"), sep = "_") %>%
  # Convert the 'year' column to integer type
  dplyr::mutate(year = as.integer(year)) %>%
  # Remove the 'drop' column as it is no longer needed
  dplyr::select(-drop)



# Aggregate conflict data by region and year
total_conflict_events <- conflict.aggregated_by_type %>%
  dplyr::select(index = index.data, year, n.conflicts) %>%
  # Group data by 'index' and 'year'
  dplyr::group_by(index, year) %>%
  # Summarize the number of conflicts, summing up and removing NAs
  dplyr::summarise(across(n.conflicts, ~sum(., na.rm = TRUE)), .groups = "drop")

# Merge the vote share data with the conflict data
to.plot <- share %>%
  # Join with 'total_conflict_events' by 'index' and 'year'
  dplyr::left_join(total_conflict_events, by = c("index", "year")) %>%
  # Replace any NA values in 'n.conflicts' with 0
  dplyr::mutate(n.conflicts = tidyr::replace_na(n.conflicts, 0)) %>%
  # Rename the 'label' column to 'region'
  dplyr::rename(region = label) %>%
  # Remove the 'index' column as it is no longer needed
  dplyr::select(-index)

# Create the plot for Figure A6
Figure_A6 <- to.plot %>%
  # Set up the ggplot with the 'x' and 'y' aesthetics
  ggplot2::ggplot(ggplot2::aes(x = log10(n.conflicts), y = votes_share)) + 
  # Add points to the plot with transparency and size
  ggplot2::geom_point(alpha = 0.2, size = 0.5) + 
  # Add a smooth curve using a generalized linear model with a binomial family
  ggplot2::geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, formula = y ~ x) + 
  # Set the x-axis label
  ggplot2::xlab("log10 of # Conflicts before election") +
  # Set the y-axis label
  ggplot2::ylab("% Votes for Kabila") +
  # Format the y-axis labels as percentages
  ggplot2::scale_y_continuous(labels = scales::percent) +
  # Create facets for each year, arranging them in 2 columns
  ggplot2::facet_wrap(~ year, ncol = 2) + 
  # Customize the theme for axis titles, text, and strip text
  ggplot2::theme(
    axis.title = ggplot2::element_text(size = 16),
    axis.text = ggplot2::element_text(size = 12),
    strip.text = ggplot2::element_text(size = 18)
  )

# Save the plot if 'update_FigA6' is TRUE
if(update_FigA6) {  
  ggplot2::ggsave(here::here("manuscript/figures/FigureA6.png"), Figure_A6, width = figA6_width, height = figA6_height, units = "in")
}  


##### FIGURE A7 #####

# Select relevant columns from data and pivot to longer format
share <- data %>%
  dplyr::select(index, label, dplyr::starts_with("kabila.percent"), dplyr::starts_with("ramazani.percent")) %>%
  tidyr::pivot_longer(cols = -c(index, label), values_to = "votes_share") %>%
  tidyr::separate(name, c("drop", "year"), sep = "_") %>%
  dplyr::mutate(year = as.integer(year)) %>%
  dplyr::select(-drop)



# Aggregate conflict data by type and year
total_deaths <- conflict.aggregated_by_type %>%
  dplyr::select(index = index.data, year, n.deaths) %>%
  dplyr::group_by(index, year) %>%
  dplyr::summarise(across(n.deaths, ~sum(., na.rm = TRUE)), .groups = "drop")

# Merge share data with total deaths data, handle NA values, and rename columns
to.plot <- share %>%
  dplyr::left_join(total_deaths, by = c("index", "year")) %>%
  dplyr::mutate(n.deaths = tidyr::replace_na(n.deaths, 0)) %>%
  dplyr::rename(region = label) %>%
  dplyr::select(-index)

# Create ggplot for Figure A7
Figure_A7 <- to.plot %>%
  ggplot2::ggplot(ggplot2::aes(x = log10(n.deaths), y = votes_share)) +
  ggplot2::geom_point(alpha = 0.2, size = 0.5) +
  ggplot2::geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, formula = y ~ x) +
  ggplot2::xlab("log10 of # Deaths before election") +
  ggplot2::ylab("% Votes for Kabila") +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::facet_wrap(~year, ncol = 2) +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 16),
                 axis.text = ggplot2::element_text(size = 12),
                 strip.text = ggplot2::element_text(size = 18))

# Save the plot if update_FigA7 is TRUE
if (update_FigA7) {
  ggplot2::ggsave(here::here("manuscript/figures/FigureA7.png"), Figure_A7, width = figA7_width, height = figA7_height, units = "in")
}

##### FIGURE A8 #####

# Select relevant columns from conflict.aggregated and pivot to longer format
conflicts <- conflict.aggregated %>%
  dplyr::select(index.data, dplyr::starts_with("n.conflicts")) %>%
  tidyr::pivot_longer(-index.data, names_to = "var", values_to = "n") %>%
  tidyr::separate(var, c("drop", "year"), sep = "_") %>%
  dplyr::select(-drop)

# Lakes are not administrative units, we will show them as NAs
# Bind rows to include "administrative unit not available" for specific years
conflicts %<>% dplyr::bind_rows(data.frame(index.data = "administrative unit not available", year = c("2006", "2011", "2018")))

# Convert index.data to a factor with levels ordered by congo.territoire.borders
conflicts %<>% dplyr::mutate(index.data = factor(index.data, levels = unique(congo.territoire.borders$index.data)))

# Complete the conflicts dataset to include all combinations of index.data and year
conflicts %<>% tidyr::complete(index.data, year)

# Replace NA values in n with 0
conflicts %<>% dplyr::mutate(n = tidyr::replace_na(n, 0))

# Join percentages and shapefile data
conflicts.map <- congo.territoire.borders %>%
  dplyr::full_join(conflicts, by = "index.data")

# Extract lakes because we are plotting them in a different color
lakes <- conflicts.map %>%
  dplyr::filter(index.data == "administrative unit not available")

# Filter out the lakes from conflicts.map
conflicts.map %<>% dplyr::filter(index.data != "administrative unit not available")

# Plot and save the figure
Figure_A8 <- ggplot2::ggplot(conflicts.map, ggplot2::aes(fill = n + 0.1)) +
  ggplot2::geom_sf() +
  ggplot2::geom_sf(data = lakes, fill = "white") +
  ggplot2::scale_fill_gradientn(colours = c("white", "orange", "red", "red3", "red4"), values = scales::rescale(log10(c(0.1, 1, 10, 100, 1000))), name = "# Conflicts", guide = "colorbar", breaks = c(0.1, 1, 10, 100, 1000), na.value = "gray50", trans = "log10", labels = c("0", "1", "10", "100", "1000")) +
  ggplot2::facet_wrap(~year, ncol = 2) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = "inside", # Position legend inside the plot
                 legend.position.inside = c(0.75, 0.25), # Coordinates for the legend position
                 strip.text = ggplot2::element_text(size = 18),
                 legend.text = ggplot2::element_text(size = 12),
                 legend.title = ggplot2::element_text(size = 14))

# Save the plot if update_FigA8 is TRUE
if (update_FigA8) {
  ggplot2::ggsave(here::here("manuscript/figures/FigureA8.png"), Figure_A8, width = figA8_width, height = figA8_height, units = "in")
}


##### FIGURE A9 #####

# Select relevant columns from conflict.aggregated and pivot to longer format
deaths <- conflict.aggregated %>%
  dplyr::select(index.data, dplyr::starts_with("n.deaths")) %>%
  tidyr::pivot_longer(-index.data, names_to = "var", values_to = "n") %>%
  tidyr::separate(var, c("drop", "year"), sep = "_") %>%
  dplyr::select(-drop)

# Lakes are not administrative units, we will show them as NAs
# Bind rows to include "administrative unit not available" for specific years
deaths %<>% dplyr::bind_rows(data.frame(index.data = "administrative unit not available", year = c("2006", "2011", "2018")))

# Convert index.data to a factor with levels ordered by congo.territoire.borders
deaths %<>% dplyr::mutate(index.data = factor(index.data, levels = unique(congo.territoire.borders$index.data)))

# Complete the deaths dataset to include all combinations of index.data and year
deaths %<>% tidyr::complete(index.data, year)

# Replace NA values in n with 0
deaths %<>% dplyr::mutate(n = tidyr::replace_na(n, 0))

# Join percentages and shapefile data
deaths.map <- congo.territoire.borders %>%
  dplyr::full_join(deaths, by = "index.data")

# Extract lakes because we are plotting them in a different color
lakes <- deaths.map %>%
  dplyr::filter(index.data == "administrative unit not available")

# Filter out the lakes from deaths.map
deaths.map %<>% dplyr::filter(index.data != "administrative unit not available")

# Plot and save the figure
Figure_A9 <- ggplot2::ggplot(deaths.map, ggplot2::aes(fill = n + 0.1)) +
  ggplot2::geom_sf() +
  ggplot2::geom_sf(data = lakes, fill = "white") +
  ggplot2::scale_fill_gradientn(colours = c("white", "orange", "red", "red3", "red4"), values = scales::rescale(log10(c(0.1, 1, 10, 100, 1000))), name = "# Deaths", guide = "colorbar", breaks = c(0.1, 1, 10, 100, 1000), na.value = "gray50", trans = "log10", labels = c("0", "1", "10", "100", "1000")) +
  ggplot2::facet_wrap(~year, ncol = 2) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = "inside", # Position legend inside the plot
                 legend.position.inside = c(0.75, 0.25), # Coordinates for the legend position
                 strip.text = ggplot2::element_text(size = 18),
                 legend.text = ggplot2::element_text(size = 12),
                 legend.title = ggplot2::element_text(size = 14))

# Save the plot if update_FigA9 is TRUE
if (update_FigA9) {
  ggplot2::ggsave(here::here("manuscript/figures/FigureA9.png"), Figure_A9, width = figA9_width, height = figA9_height, units = "in")
}

##### FIGURE A10 #####

# Select relevant columns for conflicts data from different years
to.plot <- conflict.aggregated %>%
  dplyr::select(conflicts.data_2006, conflicts.data_2011, conflicts.data_2018)

# Combine data from different years into one dataframe, removing NULLs
to.plot <- dplyr::bind_rows(
  purrr::compact(to.plot$conflicts.data_2006),
  purrr::compact(to.plot$conflicts.data_2011),
  purrr::compact(to.plot$conflicts.data_2018)
)

# Process the combined data
to.plot %<>%
  # Select date_start and best columns
  dplyr::select(date_start, best) %>%
  # Create a month column by flooring the date_start to the nearest month
  dplyr::mutate(month = lubridate::floor_date(date_start, unit = "month")) %>%
  # Group by month and summarize the number of conflicts and total deaths
  dplyr::group_by(month) %>%
  dplyr::summarise(n.conflicts = dplyr::n(), n.deaths = sum(best, na.rm = TRUE))

# Create the plot for Figure A10
Figure_A10 <- to.plot %>%
  # Scale deaths to match the plot scale
  dplyr::mutate(n.deaths = n.deaths / 100 * 6) %>%
  ggplot2::ggplot(ggplot2::aes(x = month)) +
  # Plot linerange for number of conflicts
  ggplot2::geom_linerange(ggplot2::aes(ymax = n.conflicts), ymin = 0, color = "#0111FA", linewidth = 0.2) +
  # Plot linerange for number of deaths with a slight nudge on the x-axis
  ggplot2::geom_linerange(ggplot2::aes(ymax = n.deaths), ymin = 0, color = "#FA4C00", position = ggplot2::position_nudge(x = 10 * 24 * 3600), linewidth = 0.2) +
  # Add vertical lines for election dates
  ggplot2::geom_vline(data = data.frame(month = as.POSIXct(as.Date(c("2006-07-30", "2011-11-28", "2018-12-30")))), ggplot2::aes(xintercept = month), color = "black") +
  # Add text labels for election dates
  ggplot2::geom_text(data = data.frame(month = as.POSIXct(as.Date(c("2006-07-30", "2011-11-28", "2018-12-30"))), label = c("2006 election", "2011 election", "2018 election")), ggplot2::aes(label = label), y = 75, color = "black", nudge_x = -3 * 30 * 3600 * 24, angle = 90) +
  # Scale the x-axis with a small expansion
  ggplot2::scale_x_datetime(expand = c(0.01, 0.01)) +
  # Remove x-axis label
  ggplot2::xlab(NULL) +
  ggplot2::ylab("n.conflicts") +
  # Add a secondary y-axis for number of deaths
  ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(trans = ~.*100/6, guide = ggplot2::guide_axis(title = "n.deaths"))) +
  # Customize the theme for axis text colors and sizes
  ggplot2::theme(axis.text.y.left = ggplot2::element_text(color = "#0111FA", size = 10),
                 axis.text.y.right = ggplot2::element_text(color = "#FA4C00", size = 10))

# Save the plot if update_FigA10 is TRUE
if (update_FigA10) {
  ggplot2::ggsave(here::here("manuscript/figures/FigureA10.png"), Figure_A10, width = figA10_width, height = figA10_height, units = "in")
}


##### FIGURE A11 ####


# Load the pre-saved RData file that contains the model results for Table A2c
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

# Prepare data for forest plot for the year 2006
forest_plot_data_2006 <- models_tA8i[1:4] %>%
  purrr::map(broom::tidy) %>%
  purrr::map2(model_names.to_print[1:4], \(x, name) x %>% dplyr::mutate(model = name)) %>%
  purrr::map2_dfr(models.to.print_se[1:4], \(x, se) {
    as.data.frame(se) %>%
      magrittr::set_names("se.robust") %>%
      dplyr::mutate(term = rownames(.)) %>%
      dplyr::right_join(x, by = "term")
  }) %>%
  dplyr::mutate(year = 2006)

rm(models_tA8i)


# Load the pre-saved RData file that contains the model results
load("results/TableA8ii_models.RData")


# Define a vector of model names that will be used for labeling the columns in the table
model_names <- c(paste("Conflicts"), paste("Log Conflicts"), paste("Deaths"), paste("Log Deaths"))

# Create a copy of model_names to be printed in the final table
model_names.to_print <- model_names

# Create a forest plot data set for 2011 by extracting coefficients and standard errors from each model
forest_plot_data_2011 <- models_tA8ii %>% 
  # Convert each model into a tidy data frame of coefficients and statistics using broom::tidy
  purrr::map(broom::tidy) %>% 
  # For each tidy model, add the model name as a column and initialize a column for robust standard errors
  purrr::map2(model_names.to_print[1:4], \(data, model_name) {
    data %>% dplyr::mutate(model = model_name, se.robust = NA_real_)
  }) %>% 
  # Combine all the tidy data frames into a single data frame
  dplyr::bind_rows() %>% 
  # Add a column for the year (2011) to the data set
  dplyr::mutate(year = 2011)

rm(models_tA8ii)

# Load the pre-saved RData file that contains the model results
load("results/TableA8iii_models.RData")


# Create a vector of model names for each variable
model_names <- c("Conflicts", "Log Conflicts", "Deaths", "Log Deaths")

# Create a copy of the model names for display
model_names.to_print <- model_names

# Prepare the data for a forest plot by extracting the coefficients from the models,
# adding model names, and setting the year to 2018
forest_plot_data_2018 <- models_tA8iii %>%
  purrr::map(broom::tidy) %>%
  purrr::map2(model_names.to_print[1:4], \(x, y) { x %>% dplyr::mutate(model = y, se.robust = NA_real_) }) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(year = 2018)

rm(models_tA8iii)


# Combine data from three different datasets (2006, 2011, 2018) into a single data frame.
to.plot <- dplyr::bind_rows(forest_plot_data_2006, forest_plot_data_2011, forest_plot_data_2018) %>%
  
  # Remove any leading hyphen from the 'term' column values.
  dplyr::mutate(term = stringr::str_remove(term, "^-")) %>%
  
  # Filter out rows where the 'term' is either "fg_govt", "fg_fg", "(Intercept)" 
  # or where the term starts with "election".
  dplyr::filter(!term %in% c("fg_govt", "fg_fg", "(Intercept)") & 
                  !stringr::str_starts(term, "election")) %>%
  
  # Replace missing standard errors (se) with robust standard errors (se.robust) if available; 
  # otherwise, use the standard error (std.error).
  dplyr::mutate(se = dplyr::case_when(!is.na(se.robust) ~ se.robust, TRUE ~ std.error)) %>%
  
  # Generate human-readable dyad labels for the 'term' variable (custom function).
  make_dyad_labels(var = "term") %>%
  
  # Replace any occurrences of " v " in the 'term' column with " v\n" 
  # to insert a line break for better plot readability.
  dplyr::mutate(term = stringr::str_replace(term, "\\sv\\s", " v\n"))

# Filter the dataset to only include rows where the 'model' is "Log Deaths".
# Adjust the 'estimate' and 'se' columns by multiplying them by log10(1.10).
Figure_A11 <- to.plot %>%
  dplyr::filter(model == "Log Deaths") %>%
  dplyr::mutate(estimate = estimate * log10(1.10), se = se * log10(1.10)) %>%
  
  # Create a forest plot using the 'term', 'estimate', and 'se' columns as inputs.
  ggforestplot::forestplot(name = term, estimate = estimate, se = se, 
                           xlab = "Difference in vote share (pp)", ylab = NULL) +
  
  # Facet the plot by 'year' with independent x-scales for each facet.
  ggplot2::facet_wrap(~ year, scales = "free_x") +
  
  # Customize the x-axis to display labels as percentages without a suffix.
  ggplot2::scale_x_continuous(labels = \(x) scales::percent(x, suffix = "")) +
  
  # Adjust theme settings: set panel spacing and customize axis text sizes.
  ggplot2::theme(
    panel.spacing.x = ggplot2::unit(0.3, "in"),
    axis.text.x = ggplot2::element_text(size = 8),
    axis.text.y = ggplot2::element_text(size = 8)
  )

# Save the plot as a PNG file if the 'update_FigA11' flag is TRUE.
if (update_FigA11) {
  
  # Save the plot 'Figure_A11' to the specified path with defined dimensions and white background.
  ggplot2::ggsave(
    here::here("manuscript/figures/FigureA11.png"),
    Figure_A11,
    width = figA11_width,
    height = figA11_height,
    units = "in",
    bg = "white"
  )
}
