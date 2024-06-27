
rm(list=ls())

here::i_am("R/002 - update manuscript.R")

###############################################################################
#' Name: update document
#' Author: John Quattrochi (john.quattrochi@gmail.com)
#' Assistant: Juan Luis Herrera Cortijo (juan.luis.herrera.cortijo@gmail.com)
#' Purpose: Updates word document with tables and figures
#' Notes:
#' 
#' Figures are stored in the document/figures folder as png files. Figure files are updated only
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
#' Document is stored in "[project folder]/document"
###############################################################################


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


#### FIGURES ####

# Check that we have all the necessary subfolders

if(!dir.exists(figures_path)) dir.create(figures_path)

# Load data

load(here::here("results/data.RData"))

##### FIGURE 1 #####

# This section processes election data to visualize the percentage of votes received 
# by candidates Kabila and Ramazani in the years 2006, 2011, and 2018. 
# The data is first filtered to include only relevant columns. 
# Percentages of votes for each candidate in each year are then calculated. 
# The data is restructured to have a single column for percentages and another 
# for the corresponding year.
# Percentages are scaled from 0 to 100, and non-administrative units like lakes 
# are marked as not available.
# The processed data is then merged with geographical shapefile data of the 
# Congo territories.
# Lakes are separated out for special coloring in the visualization.
# Finally, the section generates and saves a plot showing the percentage of votes 
# by territory for each year, with appropriate color scales and legends.


# Select relevant columns from the data for analysis
percentages <- dplyr::select(data, index, total.votes_2006, total.votes_2011, total.votes_2018, kabila.votes_2006, kabila.votes_2011, ramazani.votes_2018)

# Now, compute the percentages
# Calculate the percentage of votes for each year and select relevant columns
percentages %<>% dplyr::mutate(
  # Calculate the percentage of Kabila votes in 2006
  percentage_2006 = kabila.votes_2006 / total.votes_2006,
  # Calculate the percentage of Kabila votes in 2011
  percentage_2011 = kabila.votes_2011 / total.votes_2011,
  # Calculate the percentage of Ramazani votes in 2018
  percentage_2018 = ramazani.votes_2018 / total.votes_2018
  # Select the index and all columns starting with "perc"
) %>% dplyr::select(index, dplyr::starts_with("perc")) %>% dplyr::ungroup()

# Pivot the data to have one map per year
percentages %<>% tidyr::pivot_longer(cols = -c(index), names_to = "var", values_to = "percent")

# Separate the year from the variable name
percentages %<>% tidyr::separate(var, c("drop", "year"), sep = "_")

# Convert the percentages to a scale from 0 to 100
percentages %<>% dplyr::mutate(percent = percent * 100)

# Lakes are not administrative units, so show them as NAs
percentages %<>% dplyr::bind_rows(data.frame(index = "administrative unit not available", year = c("2006", "2011", "2018")))

# Join percentages with shapefile data for mapping
percentages.map <- dplyr::full_join(congo.territoire.borders, percentages, by = c("index.data" = "index"))

# Extract lakes to plot them in a different color
lakes <- dplyr::filter(percentages.map, index.data == "administrative unit not available")

# Remove lakes from the main map data
percentages.map %<>% dplyr::filter(index.data != "administrative unit not available")

# Plot and save the figure
Figure_1 <- ggplot2::ggplot(percentages.map, ggplot2::aes(fill = percent, color = "")) +
  ggplot2::geom_sf() + # Plot the main map data
  ggplot2::geom_sf(data = lakes, fill = "white") + # Plot lakes in white color
  ggplot2::scale_color_manual(values = "gray30") + # Set the color scale for map outlines
  ggplot2::scale_fill_gradient(name = "% Votes", guide = "colourbar", na.value = "gray50") + # Set the fill gradient for percentage votes
  ggplot2::guides(colour = ggplot2::guide_legend("No data", override.aes = list(color = "gray50", fill = "gray50"))) + # Customize legend for missing data
  ggplot2::facet_wrap(~year, ncol = 2) + # Create separate panels for each year
  ggplot2::theme_void() + # Remove background and axis
  ggplot2::theme(
    legend.position = "inside", # Position legend inside the plot
    legend.position.inside = c(0.75, 0.25), # Coordinates for the legend position
    strip.text = ggplot2::element_text(size = 18), # Customize the size of the strip text
    legend.text = ggplot2::element_text(size = 12), # Customize the size of the legend text
    legend.title = ggplot2::element_text(size = 14) # Customize the size of the legend title
  )

# Save the figure if update_Fig1 is TRUE
if (update_Fig1) {
  ggplot2::ggsave(file.path(figures_path, "Figure1.png"), Figure_1, width = fig1_width, height = fig1_height, units = "in", dpi = 300)
}

##### FIGURE 2#####

# This section prepares data for Figure 2 by performing the following steps:

# - Selects specific columns and converts the data into a data frame.
# - Reshapes the data from long to wide format to compare percentages across different years.
# - Calculates the percentage differences between years and removes the original year columns.
# - Reshapes the data back to long format.
# - Adds rows for administrative units with unavailable data.
# - Merges this data with geographic borders.
# - Separates out and handles lake data where administrative units are not available.
# - Creates a plot using ggplot2, with customized aesthetics and themes, and saves it if required.

# Select relevant columns and convert to data frame
percentage.differences <- percentages.map %>%
  dplyr::select(index.data, year, percent) %>%  # Select the columns 'index.data', 'year', and 'percent'
  as.data.frame() %>%  # Convert to a data frame
  dplyr::select(-geometry) %>%  # Remove the 'geometry' column
  
  # Reshape data from long to wide format
  tidyr::pivot_wider(names_from = "year", values_from = "percent") %>%  # Spread 'year' column into multiple columns, with values from 'percent'
  
  # Calculate differences between years
  dplyr::mutate(`2006-2011` = `2011` - `2006`, `2011-2018` = `2018` - `2011`) %>%  # Create new columns for year differences
  
  # Remove the original year columns
  dplyr::select(-c(`2006`, `2011`, `2018`)) %>%  # Remove the original '2006', '2011', and '2018' columns
  
  # Reshape data back to long format
  tidyr::pivot_longer(cols = -index.data, names_to = "period", values_to = "difference")  # Gather the year difference columns into 'period' and 'difference' columns

# Add a row for administrative units not available
percentage.differences %<>% dplyr::bind_rows(data.frame(index.data = "administrative unit not available", period = c("2006-2011", "2011-2018")))  # Append rows for missing administrative units

# Join the differences with the geographic data
percentage.differences.map <- congo.territoire.borders %>%
  dplyr::full_join(percentage.differences, by = "index.data")  # Perform a full join to merge geographic borders with the percentage differences

# Extract lake data (administrative units not available)
lakes <- percentage.differences.map %>%
  dplyr::filter(index.data == "administrative unit not available")  # Filter rows where 'index.data' is "administrative unit not available"

# Filter out administrative units not available from the main map data
percentage.differences.map %<>%
  dplyr::filter(index.data != "administrative unit not available")  # Keep rows where 'index.data' is not "administrative unit not available"

# Create the plot using ggplot2
Figure_2 <- ggplot2::ggplot(percentage.differences.map, ggplot2::aes(fill = difference, color = "")) +  # Initialize ggplot with the map data and aesthetic mappings
  ggplot2::geom_sf() +  # Add the main map layer
  ggplot2::geom_sf(data = lakes, fill = "white") +  # Add lakes layer with white fill
  ggplot2::scale_color_manual(values = "gray30") +  # Manually set the color scale
  ggplot2::scale_fill_gradient2(name = "Change", guide = "colourbar", na.value = "gray50") +  # Set the fill gradient for the differences
  ggplot2::guides(colour = ggplot2::guide_legend("No data", override.aes = list(color = "gray50", fill = "gray50"))) +  # Add a legend for missing data
  ggplot2::facet_wrap(~period, ncol = 2) +  # Create separate plots for each period
  ggplot2::theme_void() +  # Use a theme with no background or axes
  ggplot2::theme(legend.position = "bottom",
                 strip.text = ggplot2::element_text(size = 18),
                 legend.text = ggplot2::element_text(size = 12),
                 legend.title = ggplot2::element_text(size = 14))  # Customize theme elements

# Save the plot if update_Fig2 is TRUE
if (update_Fig2) {
  ggplot2::ggsave(file.path(figures_path,"Figure2.png"), Figure_2, width = fig2_width, height = fig2_height)  # Save the plot to a file
}

