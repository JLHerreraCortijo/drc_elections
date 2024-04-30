rm(list=ls())

library(magrittr)

###############################################################################
#' Name: 001 - data processing.R
#' Author: John Quattrochi (john.quattrochi@gmail.com)
#' Assistant: Juan Luis Herrera Cortijo (juan.luis.herrera.cortijo@gmail.com)
#' Purpose: Reads clean dara and produces data frame for analysis
#' UUID: e44ebf95-8ba3-4dad-aacf-7356584fab22
#' Notes:
#' We merge the data from the 2006, 2011 and 2018 elections in one dataframe. Since
#' data granularity is different across elections, we try to keep that granularity by
#' nesting the data and merging using a hierarchy of indexes.
#' The script assumes the following folder structure:
#' Scripts are stored in "[project folder]/R"
#' Data are stored in "[project folder]/data"
#' Results are stored in "[project folder]/results"
###############################################################################

here::i_am("R/001 - data processing.R", uuid = "e44ebf95-8ba3-4dad-aacf-7356584fab22")

# R and packages versions defined in renv.lock 


##### 1-READ DATA #####

# This section processes and combines election data from different years to prepare 
# it for further analysis. Specifically, it handles data from the years 2006, 2011, 
# and 2018, sourced from multiple Excel files.
# 
# 2006 Data: The script reads two sets of data for 2006. The first dataset is filtered 
# to remove rows representing provincial totals and select columns are excluded. 
# The second dataset, labeled as clean, undergoes a similar cleaning process. 
# These datasets are then merged based on a common column related to geographical divisions.
# 2011 Data: Data from the 2011 elections is read and rows containing aggregated 
# totals are excluded to ensure the dataset only contains individual record entries.
# 2018 Data: The script first reads the primary dataset for 2018. It then reads a 
# separate file containing candidate names and IDs, which are necessary to match 
# candidate details correctly across different datasets. Due to discrepancies in 
# candidate IDs between different sources, the script includes a manual mapping of IDs 
# to align them correctly based on a comparison with the original data source (a PDF file). 
# Once the IDs are aligned, the candidate details are merged into the main 2018 dataset.

# Read 2006 data

data.2006 <- readxl::read_xlsx("data/2006first_round.xlsx") %>% 
  dplyr::filter(is.na(Province_total)) %>% dplyr::select(-Province_total,-check_row_100) # Filter out province totals

data.2006.2nd.round <- readxl::read_xlsx("data/2006clean.xlsx") %>% 
  dplyr::filter(is.na(Province_total)) %>% dplyr::select(-Province_total) # Filter out province totals

data.2006 <- data.2006.2nd.round %>% 
  dplyr::select(Province,Subprovince,`Territoire/ville`) %>% 
  dplyr::right_join(data.2006, by = dplyr::join_by(`Territoire/ville`))

# Read 2011 data

data.2011 <- readxl::read_xlsx("data/2011drc_election_all_clcr_cleaned_stata.xlsx")  %>% 
  dplyr::filter(`Second Name`!="TOTAL") # Drop totals

# Read 2018 data

data.2018 <- readxl::read_xlsx("data/prs_edited.xlsx")

# Join candidate names with 2018 data
candidates.2018 <- readxl::read_xlsx("data/RESULTAT-PRESIDENTIEL-1.xlsx") %>% dplyr::select(nCANDIDAT,nom,prenom,postnom)

# Candidate ids are different in both files, but are the same in the original pdf. We can recover the original 
# ids in data.2018 by direct comparison of the data and the original pdf. These are the correspondences.
candidate_id <- c("1001_44_10"="20","1001_48_14"="4","1001_84_158"="13")

# Replace ids
data.2018 %<>% dplyr::mutate(candidat_id=as.numeric(candidate_id[candidat_id]))

# Join candidates table
data.2018 %<>% dplyr::left_join(candidates.2018,by=c(candidat_id="nCANDIDAT"))


