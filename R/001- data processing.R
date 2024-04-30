rm(list=ls())

###############################################################################
#' Name: 001 - data processing.R
#' Author: John Quattrochi (john.quattrochi@gmail.com)
#' Assistant: Juan Luis Herrera Cortijo (juan.luis.herrera.cortijo@gmail.com)
#' Purpose: Reads clean dara and produces data frame for analysis
#' Notes:
#' We merge the data from the 2006, 2011 and 2018 elections in one dataframe. Since
#' data granularity is different across elections, we try to keep that granularity by
#' nesting the data and merging using a hierarchy of indexes.
#' The script assumes the following folder structure:
#' Scripts are stored in "[project folder]/R"
#' Data are stored in "[project folder]/data"
#' Results are stored in "[project folder]/results"
###############################################################################

here::i_am("R/001 - data processing.R")

# R and packages versions defined in renv.lock 


##### 1-READ DATA #####

# Read 2006 data

data.2006 <- readxl::read_xlsx("data/2006first_round.xlsx") %>% 
  filter(is.na(Province_total)) %>% dplyr::select(-Province_total,-check_row_100) # Filter out province totals

data.2006.2nd.round <- readxl::read_xlsx("data/2006clean.xlsx") %>% 
  filter(is.na(Province_total)) %>% dplyr::select(-Province_total) # Filter out province totals

data.2006 <- data.2006.2nd.round %>% dplyr::select(Province,Subprovince,`Territoire/ville`) %>% dplyr::right_join(data.2006)

# Read 2011 data

data.2011 <- read_xlsx("data/2011drc_election_all_clcr_cleaned_stata.xlsx")  %>% 
  filter(`Second Name`!="TOTAL") # Drop totals

# Read 2018 data

data.2018 <- read_xlsx("data/prs_edited.xlsx")
# Join candidate names with 2018 data
candidates.2018 <- read_xlsx("data/RESULTAT-PRESIDENTIEL-1.xlsx") %>% select(nCANDIDAT,nom,prenom,postnom)

# Candidate ids are differents in both files, but are the same in the original pdf. We can recover the original 
# ids in data.2018 by direct comparison of the data and the original pdf. These are the correspondences.
candidate_id <- c("1001_44_10"="20","1001_48_14"="4","1001_84_158"="13")

# Replace ids
data.2018 %<>% mutate(candidat_id=as.numeric(candidate_id[candidat_id]))

# Join candidates table
data.2018 %<>% left_join(candidates.2018,by=c(candidat_id="nCANDIDAT"))
