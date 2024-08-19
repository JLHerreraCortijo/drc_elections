###############################################################################
#' Name: 000 - utils.R
#' Author: John Quattrochi (john.quattrochi@gmail.com)
#' Assistant: Juan Luis Herrera Cortijo (juan.luis.herrera.cortijo@gmail.com)
#' Purpose: Useful functions used across the analysis scripts
#' Script UUID: a3de7b29-e1c1-5af5-afa6-f9cd49c1028a
###############################################################################



# Define a function to merge villes into their surrounding territories
merge_villes <- function(df, labels = NA, index = "index") {
  df %<>% dplyr::rename_with(.fn = ~ paste0(".index"), .cols = dplyr::one_of(index))
  if (!is.na(labels)) {
    df %<>% dplyr::mutate(dplyr::across(dplyr::one_of(labels), ~ dplyr::case_when(.index %in% names(villes_labels) ~ villes_labels[.index], TRUE ~ .)))
  }
  df %>%
    dplyr::mutate(.index = dplyr::case_when(.index %in% names(villes_to_merge) ~ villes_to_merge[.index], TRUE ~ .index)) %>%
    dplyr::rename_with(.fn = ~ paste0(index), .cols = dplyr::one_of(".index"))
}



make_dyad_labels <- function(df,var) df %>% 
  dplyr::mutate(dplyr::across(dplyr::one_of(var),
                              ~dplyr::case_when(.%in% names(dyad_labels)~ dyad_labels[.],TRUE~.)))


# Define a function to add significance stars to p-values
p_stars <- function(p) dplyr::case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.1 ~ "*", TRUE ~ "")