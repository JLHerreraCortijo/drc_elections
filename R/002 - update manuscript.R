
rm(list=ls())

here::i_am("R/002 - update manuscript.R")

###############################################################################
#' Name: update document
#' Author: John Quattrochi (john.quattrochi@gmail.com)
#' Assistant: Juan Luis Herrera Cortijo (juan.luis.herrera.cortijo@gmail.com)
#' Purpose: Updates word document with tables and figures
#' Notes:
#' 
#' Figures are stored in the manuscript/figures folder as png files. Figure files are updated only
#' if the corresponding update_FigX un the user section is TRUE.
#' 
#' Figures width and height in inches can be set in the user section.
#' 
#' 
#' Figures and tables produced: 
#' 
#' Figure 1
#' Figure 2
#' Figure 3
#' 
#' 
#' The script assumes the following folder structure:
#' Scripts are stored in "[project folder]/R"
#' Data are stored in "[project folder]/data"
#' Document is stored in "[project folder]/manuscript"
###############################################################################

library(magrittr)
library(sf)
#### FUNCTIONS ####


compute.province.table <- function(p.table,col.2006,col.2011,col.2018,aggr.fn){
  
  province <- p.table$province %>% unique
  
  p.table %<>%  select(label,province,`2006`=all_of(col.2006),`2011`=all_of(col.2011),`2018`=all_of(col.2018))
  
  p.table %<>% select(-province)
  province.data <- p.table %>% summarise(label=province,across(c(`2006`,`2011`,`2018`),~aggr.fn(.)))  %>% set_colnames(c("A","B","C","D"))
  
  
  col_A <- p.table[1:ceiling(nrow(p.table)/2),] %>% set_colnames(c("A","B","C","D"))
  col_B <- p.table[(ceiling(nrow(p.table)/2)+1):nrow(p.table),] %>% set_colnames(c("E","F","G","H"))
  if(nrow(col_B)<nrow(col_A)){
    col_B %<>% add_row()
  }
  col_A %<>% bind_rows(province.data)
  col_B %<>% add_row()
  bind_cols(col_A,col_B)
}

format.table <- function(tbl,format.fn){
  tbl %<>% mutate(across(c(B,C,D,F,G,H),~case_when(!is.na(.)~format.fn(.),TRUE~" ")))
  
  
  tbl %>% flextable() %>% set_table_properties(width=1,layout="autofit") %>% 
    set_header_labels(A=" ",B="2006",C="2011", D="2018",E=" ",F="2006",G="2011", H="2018") %>% 
    align(j=c(2:4,6:8),align = "right",part="all") %>%
    bold(i=nrow(tbl),j=1:4) %>% 
    hline(i=nrow(tbl)-1,j=1:4,border = fp_border()) %>% 
    bold(i=1,part = "header")
}

compute.overall.table <- function(x,col.2006,col.2011,col.2018,aggr.fn) {
  
  x %>% reduce(bind_rows) %>% select(`2006`=all_of(col.2006),`2011`=all_of(col.2011),`2018`=all_of(col.2018)) %>% 
    summarise(label="DRC",across(c(`2006`,`2011`,`2018`),~aggr.fn(.))) %>% set_colnames(c("A","B","C","D"))
  
}

format.overall.table <- function(x,format.fn)  x %>% mutate(across(c(B,C,D),~case_when(!is.na(.)~format.fn(.),TRUE~" "))) %>%
  flextable() %>% set_table_properties(width=1,layout="autofit") %>% 
  set_header_labels(A=" ",B="2006",C="2011", D="2018") %>% 
  align(j=c(2:4),align = "right",part="all") %>%
  bold(i=1,j=1:4) %>% 
  bold(i=1,part = "header")

province.tables <- function(x,format.fn,col.2006,col.2011,col.2018,aggr.fn){
  
  overall <-  compute.overall.table(x,col.2006,col.2011,col.2018,aggr.fn) %>%
    format.overall.table(format.fn)
  
  
  prov <- x %>%
    map(
      ~{  
        
        
        .x %>% compute.province.table(col.2006,col.2011,col.2018,aggr.fn) %>%
          format.table(format.fn)
        
      }) 
  c(list(overall),prov) %>% set_names(c("DRC",names(prov)))
}

print.province.tables.to.doc <- function(province.tables,title,file,tables.per.page=4){
  
  
  doc <- read_docx()
  
  # styles_info(doc)
  doc %<>% body_add_par(title,style="heading 1")
  
  pages <- province.tables %>% split(floor((1:length(province.tables))/tables.per.page+1))
  for(page in pages){
    
    for(province in names(page)){
      
      doc %<>% body_add_par(province,style="table title")
      doc %<>% body_add_flextable(province.tables[[province]],align = "center")
      
    }
    doc %<>% body_add_break()
  }
  print(doc,target=file)
}

fix_ACLED_index <- function(df) df%>% 
  dplyr::mutate(index=
                  dplyr::case_when(
                    index %in% c("kinshasa i lukunga","kinshasa ii funa","kinshasa iii mt amba","kinshasa iv tshangu") ~ "kinshasa",
                    index == "beni ville" ~ "beni",
                    TRUE ~ index
                  ))

make_dyad_labels <- function(df,var) df %>% 
  dplyr::mutate(dplyr::across(dplyr::one_of(var),
                              ~dplyr::case_when(.%in% names(dyad_labels)~ dyad_labels[.],TRUE~.)))



#### USER SECTION ####

document_path <- here::here("manuscript/main_CONFLICT.docx")

# Figures are stored in the manuscript/figures folder as png files. Figure files are updated only
# if the corresponding update_FigX is TRUE

figures_path <- here::here("manuscript/figures")

update_Fig1 <- F
fig1_width <- 6.5 # inches
fig1_height <- 6.5 # inches

update_Fig2 <- F
fig2_width <- 6.5 #inches
fig2_height <-6.5 #inches

update_Fig3 <- F
fig3_width <- 6.5 #inches
fig3_height <- 3 #inches

update_Fig4 <- F
fig4_width <- 6.5 #inches
fig4_height <- 3 #inches

update_Fig5 <- F
fig5_width <- 6.5 #inches
fig5_height <- 3 #inches

update_Fig6 <- F
fig6_width <- 6.5 #inches
fig6_height <- 3 #inches


update_Fig7 <- F
fig7_width <- 6.5 #inches
fig7_height <- 3 #inches

update_Fig8 <- F
fig8_width <- 6.5 #inches
fig8_height <- 3 #inches

update_FigA1 <- F
figA1_width <- 4.3 #inches
figA1_height <- 4.3 #inches

update_FigA1b <- F
figA1b_width <- 4.3 #inches
figA1b_height <- 4.3 #inches


update_FigA2 <- F
figA2_width <- 4.3 #inches
figA2_height <- 4.3 #inches

update_FigA3 <- F
figA3_width <- 4.3 #inches
figA3_height <- 4.3 #inches

update_FigA4 <- F
figA4_width <- 6 #inches
figA4_height <- 4.3 #inches

update_FigA5 <- F
figA5_width <- 6 #inches
figA5_height <- 4.3 #inches

update_FigA6 <- F
figA6_width <- 6 #inches
figA6_height <- 4.3 #inches

update_FigA7 <- F
figA7_width <- 6 #inches
figA7_height <- 4.3 #inches

update_FigA8 <- F
figA8_width <- 6.5 #inches
figA8_height <- 6.5 #inches

update_FigA9 <- F
figA9_width <- 6.5 #inches
figA9_height <- 6.5 #inches

update_FigA10 <- F
figA10_width <- 6.5 #inches
figA10_height <- 4 #inches


update_FigA11 <- F
figA11_width <- 6.5 #inches
figA11_height <- 4 #inches


update_FigA12 <- F
figA12_width <- 6.5 #inches
figA12_height <- 4 #inches



# Run model diagnostics?

run_diagnostics <- F


# Actor types labels

actor_type_labels <- c(cag="DRC non state",
                       civilians="Civilians",
                       fg="Foreign non state",
                       fsa="Foreign state",
                       govt="DRC state")

# Dyad labels

dyad_labels <- c("cag_cag"="DRC non state v DRC non state",
                 "cag_civilians"="DRC non state v Civilians",
                 "cag_fsa"="DRC non state v Foreign state",
                 "cag_govt"="DRC non state v DRC state",
                 "govt_govt"="DRC state v DRC State",
                 "civilians_fsa"="Foreign state v Civilians",
                 "civilians_govt"="DRC state v Civilians",
                 "civilians_fg"="Foreign non-state v Civilians",
                 "civilians_civilians"="Civilians v Civilians",
                 "fg_fsa"="Foreign state v Foreign non state",
                 "fg_govt"="DRC state v Foreign non state",
                 "fsa_fsa"="Foreign state v Foreign state",
                 "fsa_govt"="Foreign state v DRC state",
                 "cag_fg"="DRC non state v Foreign non state",
                 "fg_fg"="Foreign non state v Foreign non state"
)

dyad_labels <- names(dyad_labels) %>% purrr::map(~stringr::str_split(.x,"_") %>% unlist %>% rev %>% paste0(collapse = "_") %>% magrittr::set_names(dyad_labels[.x],.)) %>% unlist %>% c(dyad_labels)

dyad_labels <-  dyad_labels[unique(names(dyad_labels))]

# Load data

load(here::here("results/data.RData"))

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

# Define election periods for grouping
elections <- list(
  "2006" = c(2001:2006),
  "2011" = c(2007:2011)
) %>% 
  
  # Create a named vector for mapping years to election periods
  purrr::map2(
    names(.), 
    ~rep(.y, length(.x)) %>% set_names(.x)
  ) %>% 
  
  # Combine the list into a single vector
  purrr::reduce(c)

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
# It then defines election periods for grouping and aggregates conflict events 
# by index and year, summing the number of conflicts. The conflict events data 
# is merged with the share data, replacing NA values in n.conflicts with 0, and 
# categorizes the number of conflicts into "0 conflicts" and "1+ conflicts".
# A jitter plot is created to show the vote share vs. number of conflicts with 
# separate panels for each year. If required, the resulting figure is saved as a PNG file.

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

# Define election periods for grouping
elections <- list(
  "2006" = c(2001:2006),
  "2011" = c(2007:2011)
) %>% 
  purrr::map2(names(.), ~rep(.y, length(.x)) %>% set_names(.x)) %>% 
  purrr::reduce(c)

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
# It then defines election periods for grouping and aggregates conflict events 
# by index and year, summing the number of conflicts. The conflict events data is 
# merged with the turnout data, replacing NA values in n.conflicts with 0, and 
# categorizes the number of conflicts into "0 conflicts" and "1+ conflicts".
# A jitter plot is created to show the voter turnout vs. number of conflicts with 
# separate panels for each year. If required, the resulting figure is saved as a PNG file.

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

# Define election periods for grouping
elections <- list(
  "2006" = c(2001:2006),
  "2011" = c(2007:2011)
) %>% 
  purrr::map2(
    names(.), 
    ~rep(.y, length(.x)) %>% set_names(.x)
  ) %>% 
  purrr::reduce(c)

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
