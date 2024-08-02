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

if(!dir.exists(here::here("results"))){
  dir.create(here::here("results"))
  
}

#### 1-READ DATA ####

# This section processes and combines election data from different years to prepare 
# it for further analysis. Specifically, it handles data from the years 2006, 2011, 
# and 2018, sourced from multiple Excel files.
# 
# 2006 Data: The script reads two sets of data for 2006. The first dataset is filtered 
# to remove rows representing provincial totals and select columns are excluded. 
# The second dataset, labeled as second round, undergoes a similar cleaning process. 
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

data.2006 <- readxl::read_xlsx(here::here("data/2006first_round.xlsx")) %>% 
  dplyr::filter(is.na(Province_total)) %>% dplyr::select(-Province_total,-check_row_100) # Filter out province totals

data.2006.2nd.round <- readxl::read_xlsx(here::here("data/2006clean.xlsx")) %>% 
  dplyr::filter(is.na(Province_total)) %>% dplyr::select(-Province_total) # Filter out province totals

data.2006 <- data.2006.2nd.round %>% 
  dplyr::select(Province,Subprovince,`Territoire/ville`) %>% 
  dplyr::right_join(data.2006, by = dplyr::join_by(`Territoire/ville`))

# Read 2011 data

data.2011 <- readxl::read_xlsx(here::here("data/2011drc_election_all_clcr_cleaned_stata.xlsx"))  %>% 
  dplyr::filter(`Second Name`!="TOTAL") # Drop totals

# Read 2018 data

data.2018 <- readxl::read_xlsx(here::here("data/prs_edited.xlsx"))

# Join candidate names with 2018 data
candidates.2018 <- readxl::read_xlsx(here::here("data/RESULTAT-PRESIDENTIEL-1.xlsx")) %>% dplyr::select(nCANDIDAT,nom,prenom,postnom)

# Candidate ids are different in both files, but are the same in the original pdf. We can recover the original 
# ids in data.2018 by direct comparison of the data and the original pdf. These are the correspondences.
candidate_id <- c("1001_44_10"="20","1001_48_14"="4","1001_84_158"="13")

# Replace ids
data.2018 %<>% dplyr::mutate(candidat_id=as.numeric(candidate_id[candidat_id]))

# Join candidates table
data.2018 %<>% dplyr::left_join(candidates.2018,by=c(candidat_id="nCANDIDAT"))

#### 2-RENAME COLUMNS AND NEST VOTES FOR EACH CANDIDATE ####

# This section processes election data from 2006, 2011, and 2018 to structure and 
# organize key information regarding electoral votes and participants. Specifically, 
# it performs the following operations:
#   
# 1. For the year 2006, it restructures the data to list each candidate along with 
# the percentage and calculated number of votes they received. This transformation 
# involves pivoting the dataset so that candidate names and their corresponding 
# vote percentages are collated into a nested data frame, which includes both the 
# candidate names and the calculated votes based on valid votes and participation percentages.
# 
# 2. For the years 2011 and 2018, the script formats the data by nesting details 
# about each candidate’s votes into a similar structured format. This includes 
# renaming certain columns for consistency and clarity, such as the candidate’s 
# name and the number of votes they received.
# 
# 3. Additionally, the script standardizes other important electoral information 
# across the datasets for these years, such as the number of registered voters, 
# actual voters, ballot boxes, and the count of processed ballot boxes. This renaming 
# ensures uniformity across different election years for easier comparison and analysis.
# 
# Overall, this section enhances the accessibility and usability of election data 
# by organizing it into a consistent format across different election years, 
# allowing for streamlined analysis and reporting.

# In 2011 and 2018, we have:
# - Votes for each candidate
# - Registered voters
# - Voters
# - Number of ballot boxes
# - Number of ballot boxes counted

# In 2006 we have:
# - Votes for each candidate
# - Total votes

# nest the number of votes for each candidate in a data.frame with columns:
# - First name= candidate
# - number of votes=votes

data.2006 %<>% tidyr::pivot_longer(cols = -dplyr::one_of("Votes valables","Percent_participation","Territoire/ville","Province","Subprovince"),names_to = "candidate",values_to="percent" ) %>%
  dplyr::mutate(votes=`Votes valables`*percent/100) %>% 
  tidyr::nest(votes.data=c(candidate,votes,percent))


data.2011 %<>% dplyr::mutate(candidate=`First Name`) %>%
  dplyr::rename(votes=`Number of Votes`) %>%
  tidyr::nest(votes.data=c(candidate,votes,`Candidate Number`,`First Name`,`Second Name`,`Third Name`,`Fourth Name`,`Fifth Name`,Percent))


data.2018 %<>% dplyr::mutate(candidate=nom) %>%
  dplyr::rename(votes=voix) %>%
  tidyr::nest(votes.data=c(candidate,votes,candidat_id, nom,prenom,postnom))

# Other columns will be renamed as:
# - registered.voters
# - voters
# - ballot.boxes
# - ballot.boxes_counted

data.2011 %<>% dplyr::rename(registered.voters=`Number of registered voters`,
                             voters=`Number of Voters`,
                             ballot.boxes=`Number of Ballot boxed`,
                             ballot.boxes_counted=`Ballot boxes counted`)

data.2018 %<>% dplyr::rename(registered.voters=electeurs_attendus,
                             voters=votants,
                             ballot.boxes=bv_prevus,
                             ballot.boxes_counted=bv_traites)

#### 3-NEST DATA BY LOCATION AND STANDARDIZE INDEXES ####

# This section focuses on organizing and indexing election data from 2006, 2011, 
# and 2018 by geographic and administrative divisions such as provinces, cities, 
# and electoral constituencies. It aims to facilitate easier data comparison and 
# analysis across different election years:
#   
#   1. **Nesting and Index Creation**: The script nests voting site data within 
#   each constituency for the 2018 dataset. For all years, it establishes indexes 
#   based on geographic identifiers like city names and constituency names, which 
#   are standardized to lower case and stripped of extra spaces for uniformity.
# 
# 2. **Labeling and Manual Adjustments**: The script also assigns labels to each 
# entry for clearer identification and resolves inconsistencies in geographic names 
# between datasets. In Kinshasa, for example, it adjusts city names to subprovince 
# levels to match data granularity in other years.
# 
# 3. **Data Matching and Structuring**: Efforts are made to align the datasets by 
# manually correcting discrepancies in geographic names across the election years. 
# This includes both simplifying and matching names and ensuring that the names 
# used reflect the administrative changes or differences noted in different datasets.
# 
# 4. **Nested Structuring**: Finally, the data is restructured into nested formats 
# based on updated indexes and labels. This allows for detailed yet manageable 
# data subsets, which can be used for in-depth regional analysis or aggregated to 
# provide broader electoral insights.
# 
# This meticulous organization of data by location enhances the analytical framework, 
# making it easier to track electoral trends and patterns across different regions 
# and election cycles.

# Nest 2018 voting sites within each circonscription

data.2018 %<>% tidyr::nest(voting.sites=-c(province_id,province,clcr_id,clcr,circonscription_id,circonscription,siege))


# Create indexes. We can match most of the data using Ville in 2006, Ville in 2011 and circonscription in 2018. So,
# we create the indexes using those
data.2006 %<>% dplyr::mutate(index.2006=`Territoire/ville` %>% tolower() %>% stringr::str_squish(),
                             label.2006=`Territoire/ville`,
                             province.2006=stringr::str_to_title(tolower(Province)))
data.2011 %<>% dplyr::mutate(index.2011=Ville %>% tolower() %>% stringr::str_squish(),
                             label.2011=Ville,
                             province.2011=Province)
data.2018 %<>% dplyr::mutate(index.2018=circonscription %>% tolower() %>%stringr::str_squish(),
                             label.2018=stringr::str_to_title(circonscription),
                             province.2018=province)

# Labels 

#' 2011 data at Kinshasa is given only at the subprovince level
#' So, in 2006 and 2018, we must use the data by subprovince. Here we replace the ville names with
#' their corresponding subprovince

Kinshasa.2006 <- data.2006 %>% dplyr::filter(Province=="KINSHASA")
manual.matches <- c("kin 1 lukunga"="kinshasa i lukunga",
                    "kin 2 funa"="kinshasa ii funa",
                    "kin 3 mt amba"="kinshasa iii mt amba",
                    "kin 4 tshangu"="kinshasa iv tshangu"
)
kinshasa.subprov <- Kinshasa.2006$Subprovince %>% tolower()

kinshasa.subprov  %<>% stringr::str_replace_all(manual.matches) %>% magrittr::set_names(Kinshasa.2006$index.2006)

kinshasa.subprov.labels <- Kinshasa.2006$Subprovince %>% magrittr::set_names(Kinshasa.2006$index.2006)

# Nest the data
data.2006 %<>% dplyr::mutate(index=index.2006,label=label.2006) %>% 
  tidyr::nest(circonscription=-c(index.2006,label.2006,province.2006))
data.2011 %<>%  dplyr::mutate(index=index.2011,label=label.2011) %>% 
  tidyr::nest(circonscription=-c(index.2011,label.2011,province.2011))
data.2018 %<>%  dplyr::mutate(index=index.2018,label=label.2018) %>% 
  tidyr::nest(ville.territoire=-c(index.2018,label.2018,province.2018))

# Replace Villages with Subprovinces where necessary


data.2006 %<>% dplyr:: mutate(index=index.2006,
                              label=label.2006,
                              label.2006=dplyr::case_when(index.2006 %in% names(kinshasa.subprov.labels) ~ kinshasa.subprov.labels[index.2006],
                                                          TRUE ~ label.2006),
                              index.2006=dplyr::case_when(index.2006 %in% names(kinshasa.subprov) ~ kinshasa.subprov[index.2006],
                                                          TRUE ~ index.2006))


# Some names are written differently across the elections, fix that in 2006

manual.matches <- c("mo bay i-m bongo"="mobayi-mbongo",
                    "kabeya-kamwang"="kabeya-kamwanga",
                    "beni territoire"="beni"
)

# Apply equivalences and nest the data
data.2006 %<>% dplyr::mutate(index.2006=dplyr::case_when(index.2006 %in% names(manual.matches) ~ manual.matches[index.2006],
                                                         TRUE ~ index.2006))
data.2006 %<>% tidyr::nest(data_2006=-c(index.2006,label.2006,province.2006))


# Create indexes and nest data for 2011

data.2011 %<>% dplyr::mutate(index=index.2011,label=label.2011) %>% 
  tidyr::nest(data_2011=-c(index.2011,label.2011,province.2011))


# Some names are written differently across the elections, fix that in 2018
manual.matches <- c(
  "luilu (mwene-ditu)"="luilu",
  "tshikapa"="kamonia",
  "tshikapa ville"="tshikapa"
)

# Apply the equivalences

data.2018 %<>% dplyr::mutate(
  label.2018=dplyr::case_when(index.2018 %in% names(manual.matches) ~ stringr::str_to_title(manual.matches[index.2018]),
                              TRUE ~ label.2018),
  index.2018=dplyr::case_when(index.2018 %in% names(manual.matches) ~ manual.matches[index.2018],
                              TRUE ~ index.2018))


# In 2018, in some provinces we have a distinction between their capital and the remaining territoire. In 2006 and 2011 we don't
# have such distinction. We will match both the capital and the remaining territorie with the aggregated 2006, 2011 data, but
# nesting them to keep the granularity.

manual.matches <- c("kenge ville"="kenge",
                    "inongo ville"="inongo",
                    "gemena ville"="gemena",
                    "lisala ville"="lisala",
                    "boende ville"="boende",
                    "buta ville"="buta",
                    "kamina ville"="kamina",
                    "kalemie ville"="kalemie",
                    "kabinda ville"="kabinda",
                    "lusambo ville"="lusambo",
                    "isiro ville"="rungu",
                    "bunia ville"="irumu"
)




# Apply the equivalences and nest the data

data.2018 %<>% dplyr::mutate(index=index.2018,
                             label=label.2018,
                             label.2018=dplyr::case_when(index.2018 %in% names(manual.matches) ~ stringr::str_to_title(manual.matches[index.2018]),
                                                         TRUE ~ label.2018),
                             index.2018=dplyr::case_when(index.2018 %in% names(manual.matches) ~ manual.matches[index.2018],
                                                         TRUE ~ index.2018)
)
data.2018 %<>% tidyr::nest(circonscription=-c(index.2018,label.2018,province.2018))

# Nest by subprovinces
data.2018 %<>% dplyr::mutate(index=index.2018,
                             label=label.2018,
                             label.2018=dplyr::case_when(index.2018 %in% names(kinshasa.subprov.labels) ~ kinshasa.subprov.labels[index.2018],
                                                         TRUE ~ label.2018),
                             index.2018=dplyr::case_when(index.2018 %in% names(kinshasa.subprov) ~ kinshasa.subprov[index.2018],TRUE ~ index.2018))

data.2018 %<>% tidyr::nest(data_2018=-c(index.2018,label.2018,province.2018))


#### 4-MATCH DATA ACROSS ELECTIONS ####

# This section is dedicated to matching election data across three different election 
# years: 2006, 2011, and 2018. The primary challenge addressed here is the discrepancies 
# in geographical names across different datasets, particularly because some regions 
# like Kinshasa lack detailed geographic identifiers in some years. To tackle this, 
# a multi-step matching strategy is implemented:
# 
# 1. **Initial Extraction and Naming**: The script extracts names of cities or 
# constituencies (referred to as villes/circonscriptions) from each year's dataset 
# and assigns them as names to character vectors. This unique approach uses the 
# names of these vectors for matching, ensuring that the actual content remains unchanged.
# 
# 2. **Iterative Matching Process**: The matching process involves several iterative 
# steps where the names in the vectors are slightly modified in each iteration to 
# accommodate differences in naming conventions between datasets. For example, 
# the term "ville" is removed and excess whitespace is cleaned to improve matching accuracy.
# 
# 3. **Compilation of Matched and Unmatched Names**: After each matching attempt, 
# matched names are compiled, and unmatched names undergo further processing to 
# refine their format and attempt another match. This stepwise refinement continues 
# until no further matches can be found.
# 
# 4. **Final Data Structuring**: The results are then structured into a comprehensive 
# list that captures both matched and unmatched names, ensuring that data from different 
# years can be compared accurately despite initial discrepancies.
# 
# The process is meticulous and aims to ensure that electoral data from different 
# years can be aligned and analyzed consistently, addressing challenges posed by 
# changes in geographic names and administrative boundaries over time.

#' There are some missing Circonscriptions in 2011. At Kinshasa, because we don't have enough detail in 2011 data, 
#' we are limited to use the subprovince level.
#' The strategy followed is to extract the names of the villes/circonscriptions into a named character vector. Matching is done
#' in several steps because there are differences across elections in the way the villes are named.
#' First, the names of the vector are set equal to the vector contents. 
#' We will use the names and not the contents to match the villes across elections. 
#' In each step trying to match the data, we modify the names of the vector, leaving the vector contents untouched.
#' That way, in the end, we have an index of villes matches across elections, even if the actual names are different.

# Get the villes/circonscriptions
ville.2006 <- data.2006  %>% dplyr::pull("index.2006")  %>% magrittr::set_names(.,.)
ville.2011 <- data.2011 %>% dplyr::pull("index.2011") %>% magrittr::set_names(.,.)
circonscription.2018 <- data.2018 %>% dplyr::pull("index.2018") %>% magrittr::set_names(.,.)


# Match 2006 and 2011 

# Find matches
matches.2006.2011 <- match(names(ville.2006),names(ville.2011))
matches.2011.2006 <- match(names(ville.2011),names(ville.2006))

# Extract matched names
villes.2006.2011 <- ville.2006[!is.na(matches.2006.2011)]
villes.2011.2006 <- ville.2011[!is.na(matches.2011.2006)]

# Get names not matched
villes.unique.2011 <- ville.2011[is.na(matches.2011.2006)]
villes.unique.2006 <- ville.2006[is.na(matches.2006.2011)]

# In some cases the difference is because there is the name ville included in the name
names(villes.unique.2011) %<>% stringr::str_remove("ville") %>% stringr::str_squish()
names(villes.unique.2006) %<>% stringr::str_remove("ville") %>% stringr::str_squish()

# Match again
matches.2006.2011 <- match(names(villes.unique.2006),names(villes.unique.2011))
matches.2011.2006 <- match(names(villes.unique.2011),names(villes.unique.2006))

# Get matched names and append to previous ones
villes.2006.2011 %<>% c(villes.unique.2006[!is.na(matches.2006.2011)])
villes.2011.2006 %<>% c(villes.unique.2011[!is.na(matches.2011.2006)])

# Get remaining names not matched
villes.unique.2011 <- villes.unique.2011[is.na(matches.2011.2006)]
villes.unique.2006 <- villes.unique.2006[is.na(matches.2006.2011)]



# Check names not matched
# villes.unique.2011
# named character(0)
# 
# villes.unique.2006
# kiri      watsa   niangara      wamba    mambasa 
# "kiri"    "watsa" "niangara"    "wamba"  "mambasa" 

# Now match the names already matched and create a data frame of paired Villes. Then add the villes not matched

matches.2006.2011 <- match(names(villes.2006.2011),names(villes.2011.2006))

matches.2011.2006 <- match(names(villes.2011.2006),names(villes.2006.2011))

villes.equ <- dplyr::bind_rows(
  data.frame(index.2006=villes.2006.2011,index.2011=villes.2011.2006[matches.2006.2011]),
  data.frame(index.2006=villes.2006.2011[matches.2011.2006],index.2011=villes.2011.2006),
  data.frame(index.2006=villes.unique.2006),
  data.frame(index.2011=villes.unique.2011)
) %>% dplyr::distinct()

villes.equ %<>% dplyr::arrange(ville.2006)



# Match 2006 and 2018

# Match names
matches.2006.2018 <- match(names(ville.2006),names(circonscription.2018))
matches.2018.2006 <- match(names(circonscription.2018),names(ville.2006))

# Get matched names
villes.2006.2018 <- c(ville.2006[!is.na(matches.2006.2018)])
villes.2018.2006 <- c(circonscription.2018[!is.na(matches.2018.2006)])

# Get names not matched
villes.unique.2018 <- circonscription.2018[is.na(matches.2018.2006)]
villes.unique.2006 <- ville.2006[is.na(matches.2006.2018)]


# Now we can remove the word ville as in2006-2011
names(villes.unique.2018) %<>% stringr::str_remove("ville") %>% stringr::str_squish()
names(villes.unique.2006) %<>% stringr::str_remove("ville") %>% stringr::str_squish()

# Match names
matches.2006.2018 <- match(names(villes.unique.2006),names(villes.unique.2018))
matches.2018.2006 <- match(names(villes.unique.2018),names(villes.unique.2006))

# Append matched names to previous data
villes.2006.2018 %<>% c(villes.unique.2006[!is.na(matches.2006.2018)])
villes.2018.2006 %<>% c(villes.unique.2018[!is.na(matches.2011.2006)])

# Get names not matched
villes.unique.2018 <- villes.unique.2018[is.na(matches.2018.2006)]
villes.unique.2006 <- villes.unique.2006[is.na(matches.2006.2018)]



# Check unmatched names
# villes.unique.2006
# named character(0)

# 
# villes.unique.2018
# named character(0)


# Now match the names already matched and create a data frame of paired Villes. Then add the villes not matched

matches.2006.2018 <- match(names(villes.2006.2018),names(villes.2018.2006))
matches.2018.2006 <- match(names(villes.2018.2006),names(villes.2006.2018))

villes.equ.2006.2018 <- dplyr::bind_rows(
  data.frame(index.2006=villes.2006.2018,index.2018=villes.2018.2006[matches.2006.2018]),
  data.frame(index.2006=villes.2006.2018[matches.2018.2006],index.2018=villes.2018.2006),
  data.frame(index.2006=villes.unique.2006),
  data.frame(index.2018=villes.unique.2018)) %>% dplyr::distinct()

# Now, merge with previous 2006-2011 matched names
villes.equ %<>% dplyr::full_join(villes.equ.2006.2018,by="index.2006") %>% dplyr::arrange(index.2006)

# Clean vars no longer needed
rm(list=setdiff(ls(),c("data.2006","data.2011","data.2018","villes.equ","kinshasa.subprov")))

#### 5-MERGE DATA ACROSS ELECTIONS ####

# This section consolidates election data from different years (2006, 2011, and 2018) 
# using previously created matching indexes to ensure consistency and continuity 
# across datasets. The primary activities in this process include:
#
# 1. **Data Preparation**: Indexes that have been aligned in previous steps are 
# used to merge data across the election years. This ensures that each entry from 
# different years corresponds to the same geographic location or administrative 
# division, even when direct matches in names are not apparent.
#
# 2. **Merging Process**: The script performs several full joins to combine the 
# datasets from 2006, 2011, and 2018 based on these indexes. This method allows 
# for the inclusion of all available data, whether or not a direct match exists 
# in all three years, thus preserving the maximum possible data granularity.
#
# 3. **Final Structuring and Cleaning**: Once merged, the data is restructured to 
# create a unified view that includes labels and province names standardized across 
# years. This structuring is crucial for analyses that require consistent geographic 
# identifiers across multiple election cycles.
#
# 4. **Variable Cleanup and Data Saving**: Unnecessary variables are removed to 
# tidy up the workspace, and the final structured dataset is arranged and saved 
# for further analysis or reporting.
#
# By integrating data from multiple election years, this section facilitates 
# comprehensive longitudinal electoral analyses, helping to identify trends and 
# changes over time within the same geographic locales.

#' In order to keep the original data granularity, we will nest the data and then we will join using the indexes created in
#' villes.equ.

# Merge 2006-2018
villes.eq.2006_2018 <- villes.equ %>% dplyr::select(index.2006,index.2018) %>% 
  dplyr::filter(!is.na(index.2006) | !is.na(index.2018))

merged.2006_2018 <- data.2006  %>% dplyr::full_join(villes.eq.2006_2018,by="index.2006")

merged.2018_2006 <- data.2018  %>% dplyr::full_join(villes.eq.2006_2018,by="index.2018")

merged <- merged.2006_2018 %>% dplyr::full_join(merged.2018_2006,by=c("index.2006","index.2018"))

# Merge 2011
villes.eq.2006_2011 <- villes.equ %>% dplyr::select(index.2006,index.2011) %>% 
  dplyr::filter(!is.na(index.2006) | !is.na(index.2011))

merged.2011_2006 <- data.2011 %>% dplyr::full_join(villes.eq.2006_2011,by="index.2011")

merged %<>% dplyr::full_join(merged.2011_2006,by="index.2006")


# Clean vars no longer needed
rm(list=setdiff(ls(),c("merged","kinshasa.subprov")))

# Arrange the data

data <- merged %>% dplyr::mutate(
  province=dplyr::case_when(!is.na(index.2018)~province.2018, # Create common index across years
                            TRUE ~dplyr::case_when(!is.na(index.2011)~province.2011,
                                                   TRUE~province.2006)),
  label=dplyr::case_when(!is.na(index.2006)~label.2006, # Create common index across years
                         TRUE ~dplyr::case_when(!is.na(index.2011)~label.2011,
                                                TRUE~label.2018)),
  index=dplyr::case_when(!is.na(index.2006)~index.2006, # Create common index across years
                         TRUE ~dplyr::case_when(!is.na(index.2011)~index.2011,
                                                TRUE~index.2018))) %>% 
  dplyr::select(index,label,province,index.2006,data_2006,index.2011,data_2011,index.2018,data_2018) %>% 
  dplyr::arrange(province,index)


rm(list=setdiff(ls(),c("data","kinshasa.subprov")))

#### 6-DERIVED QUANTITIES #####

##### 6.1-REGISTERED VOTERS IN EACH LEVEL #####


# This section focuses on calculating the number of registered voters at various 
# administrative levels for each election year. The calculations are performed through 
# a series of nested data manipulations, leveraging the flexibility of functional 
# programming within R:
#
# 1. **Data Transformation**: For each election dataset, the script navigates through 
# multiple nested structures—ranging from regions down to individual voting sites—to 
# calculate the number of registered voters based on the available data, such as 
# valid votes and participation percentages.
#
# 2. **Voter Estimation**: Using the data on valid votes and the percentage of 
# participation, the script estimates the total number of registered voters at 
# different levels (e.g., circonscription, ville territoire) by reversing the 
# calculation of valid votes from participation rates.
#
# 3. **Aggregation**: After calculating the registered voters at the lowest levels, 
# these figures are summed up through the nested structures to provide total counts 
# at higher administrative levels, ensuring that each region’s total reflects all 
# underlying data.
#
# 4. **Data Integration**: The computed totals of registered voters for each level 
# and year are then integrated back into the main dataset, providing enriched data 
# points that support more detailed electoral analysis.
#
# By systematically estimating and aggregating registered voter counts across 
# different levels, this section enhances the dataset's value for analyzing voter 
# turnout and electoral engagement across regions and election cycles.

# Update 'data' by mutating and adding a new column 'data_2006' with transformed data
data %<>%  dplyr::mutate(data_2006=data_2006 %>% purrr::map(~{
  
  # Check if the current element (.x) is not NULL to avoid errors in empty data
  if(!is.null(.x)){
    
    # Perform nested operations on each 'circonscription' within 'data_2006'
    .x %>% dplyr::mutate(registered.voters=circonscription %>% purrr::map(~{
      # Calculate 'registered.voters' based on the valid votes ('Votes valables') and participation percentage
      .x %<>% dplyr::mutate(registered.voters=`Votes valables`/(Percent_participation/100))
      # Sum up all registered voters in the current circonscription, ignoring NA values
      sum(.x$registered.voters, na.rm = TRUE)
    })) %>% 
      # Flatten the nested list of registered voters to a single column
      tidyr::unnest(registered.voters)
  }else{
    # Return NULL if the data element was initially NULL
    NULL
  }
})) %>% 
  # Create a new column 'registered.voters_2006' by summing up registered voters in all data points
  dplyr::mutate(registered.voters_2006=purrr::map(data_2006,~sum(.x$registered.voters, na.rm = TRUE))) %>% 
  # Flatten the 'registered.voters_2006' to integrate into the main dataframe
  tidyr::unnest(registered.voters_2006)

# Similar to 2006, mutate 'data' to include transformations specific to 2011
data %<>%  dplyr::mutate(data_2011=data_2011 %>% purrr::map(~{
  
  # Ensure the data element is not NULL
  if(!is.null(.x)){
    
    # Calculate registered voters for each 'circonscription' and unnest the results
    .x %>% dplyr::mutate(registered.voters=circonscription %>% purrr::map(~sum(.x$registered.voters, na.rm = TRUE))) %>% 
      tidyr::unnest(registered.voters)
  }else{
    # Return NULL for empty elements
    NULL
  }
}),
# Sum the registered voters for the year 2011 and unnest this into the main data frame
registered.voters_2011=purrr::map(data_2011,~sum(.x$registered.voters, na.rm = TRUE))) %>% 
  tidyr::unnest(registered.voters_2011)


# Update 'data' for the year 2018 with nested calculations for registered voters
data %<>%  dplyr::mutate(data_2018=data_2018 %>% purrr::map(~{
  
  # Check for non-NULL data elements
  if(!is.null(.x)){
    
    # Perform nested operations down to 'voting.sites' level to calculate registered voters
    .x %>% dplyr::mutate(circonscription=circonscription %>% purrr::map(~{
      
      .x %>% dplyr::mutate(ville.territoire=ville.territoire %>% purrr::map(~{
        
        .x %>% dplyr::mutate(registered.voters=voting.sites %>% purrr::map(~sum(.x$registered.voters, na.rm = TRUE))) %>% 
          tidyr::unnest(registered.voters)
      }),
      # Sum up registered voters for each 'ville.territoire' and unnest
      registered.voters=purrr::map(ville.territoire,~sum(.x$registered.voters, na.rm = TRUE))) %>% 
        tidyr::unnest(registered.voters)
    }),
    # Sum up all registered voters for each 'circonscription' and unnest to the main data structure
    registered.voters=purrr::map(circonscription,~sum(.x$registered.voters, na.rm = TRUE))) %>% 
      tidyr::unnest(registered.voters)
  }else{
    # Handle NULL elements
    NULL
  }
}),
# Sum up all registered voters for 2018 and unnest this into the main dataframe
registered.voters_2018=purrr::map(data_2018,~sum(.x$registered.voters, na.rm = TRUE))) %>% 
  tidyr::unnest(registered.voters_2018)

##### 6.2-VOTERS IN EACH LEVEL #####

# This section is devoted to calculating the total number of voters 
# from election data spanning three different years: 2006, 2011, and 2018. Here's 
# an overview of how the data is processed:
#
# 1. **Data Transformation**: For each election year, the data undergoes a series 
# of transformations to calculate the total voters within each administrative level, 
# such as circonscriptions and voting sites.
#
# 2. **Nested Calculations**: Utilizing the `purrr::map` function, the script navigates 
# through nested structures (circonscription to voting sites) to aggregate 
# voters at various levels.
#
# 3. **Summation and Aggregation**: After extracting the voters for smaller 
# units, the script sums these voters to compute total figures for larger geographic 
# or administrative areas. This aggregation helps in understanding the total voter 
# turnout and the distribution across different regions.
#
# 4. **Data Integration and Unnesting**: The sums of voters are then integrated 
# back into the main dataset, ensuring that each administrative unit's total voters 
# are reflected in the final dataset. The use of `tidyr::unnest` helps in flattening 
# nested lists into simpler dataframe structures.
#

# Update 'data' dataframe by mutating it to include a transformation of the 'data_2006'
data %<>%  
  dplyr::mutate(
    data_2006=data_2006 %>% 
      purrr::map(~{
        # Check if the element is not NULL to proceed with processing
        if(!is.null(.x)){
          # Calculate the sum of voters ('Votes valables') within each circonscription
          .x %>% dplyr::mutate(
            voters=circonscription %>% 
              purrr::map(~{
                .x %<>% dplyr::mutate(voters=`Votes valables`)
                sum(.x$voters, na.rm = TRUE)  # Sum voters while ignoring NA values
              })) %>% 
            tidyr::unnest(voters)  # Unnest the list to flatten the structure
        }else{
          NULL  # Return NULL if the original data element is NULL
        }
      })) %>% 
  # Aggregate the voters from all circonscriptions and add as a new column
  dplyr::mutate(
    voters_2006=purrr::map(data_2006,~sum(.x$voters, na.rm = TRUE))
  ) %>% 
  tidyr::unnest(voters_2006)  # Unnest the final sum of voters to integrate into 'data'


data %<>%  
  dplyr::mutate(
    # Process 'data_2011' in a similar fashion to 'data_2006'
    data_2011=data_2011 %>% 
      purrr::map(
        ~{
          if(!is.null(.x)){
            .x %>% dplyr::mutate(
              voters=circonscription %>% 
                purrr::map(~sum(.x$voters, na.rm = TRUE))  # Directly summing voters in the map
            ) %>% 
              tidyr::unnest(voters)  # Flatten list of voters
          }else{
            NULL
          }
        }),
    # Compute the total voters for 2011 across all entries and add to 'data'
    voters_2011=purrr::map(
      data_2011,
      ~sum(.x$voters, na.rm = TRUE)
    )
  ) %>% 
  tidyr::unnest(voters_2011)  # Flatten the structure to integrate voters into 'data'


data %<>%  
  dplyr::mutate(
    # Nested structure processing for 'data_2018'
    data_2018=data_2018 %>% 
      purrr::map(~{
        if(!is.null(.x)){
          .x %>% dplyr::mutate(
            # Navigate deeper into 'ville.territoire' level for vote calculation
            circonscription=circonscription %>% 
              purrr::map(~{
                .x %>% dplyr::mutate(
                  ville.territoire=ville.territoire %>% 
                    purrr::map(~{
                      .x %>% 
                        dplyr::mutate(
                          # Sum votes within each voting site
                          voters=voting.sites %>% purrr::map(
                            ~sum(.x$voters, na.rm = TRUE))
                        ) %>% 
                        tidyr::unnest(voters)  # Unnest at 'ville.territoire' level
                    }),
                  # Sum voters for each 'ville.territoire'
                  voters=purrr::map(
                    ville.territoire,
                    ~sum(.x$voters, na.rm = TRUE)
                  )
                ) %>% tidyr::unnest(voters)  # Unnest at 'circonscription' level
              }),
            # Aggregate voters across all 'circonscription'
            voters=purrr::map(
              circonscription,
              ~sum(.x$voters, na.rm = TRUE)
            )
          ) %>% tidyr::unnest(voters)
        }else{
          NULL
        }
      }),
    # Sum all voters from 2018 data and unnest for integration into 'data'
    voters_2018=purrr::map(data_2018,~sum(.x$voters, na.rm = TRUE))
  ) %>% 
  tidyr::unnest(voters_2018)  # Final unnest to flatten and integrate voters into 'data'

##### 6.3-TOTAL VOTES IN EACH LEVEL #####


# This section of the script is dedicated to calculating the total number of valid votes 
# (referred to as "total votes") across different administrative levels for the 
# election years 2006, 2011, and 2018. Here’s a concise breakdown of the process:
#
# 1. **Data Preparation**: For each year, the data is initially checked for null 
# values to ensure only valid data is processed. This step is crucial for maintaining 
# data integrity.
#
# 2. **Vote Calculation**:
#    - For 2006, the script directly assigns the number of valid votes ('Votes valables') 
#    from the data to a new column called 'total.votes' for each circonscription. 
#    It then sums these votes to get a total count for each higher administrative level.
#    - In 2011, the process involves extracting and summing votes from nested data 
#    structures within each circonscription. This is slightly more complex as it 
#    involves pulling and summing nested vote counts.
#    - For 2018, the calculation is even more granular, extending down to voting 
#    sites within each ville and territoire. This involves multiple layers of 
#    mapping and summing to aggregate votes all the way up from the most detailed levels.
#
# 3. **Data Aggregation**: After calculating the total votes at the lowest necessary 
# levels, the script aggregates these totals to provide comprehensive vote counts 
# for larger geographic or administrative areas.
#
# 4. **Integration and Unnesting**: The aggregated total votes are then integrated 
# back into the main dataset. The use of `tidyr::unnest()` simplifies the data 
# structure, making it more accessible for further analysis.


# Updating the 'data' dataframe to include transformations for 2006 election data
data %<>%  
  dplyr::mutate(
    data_2006=data_2006 %>% 
      purrr::map(~{
        # Check if the current data element is not NULL
        if(!is.null(.x)){
          # Perform transformations within each circonscription
          .x %>% 
            dplyr::mutate(
              circonscription=circonscription %>% 
                purrr::map(~{
                  # Directly assign 'Votes valables' to a new column 'total.votes'
                  .x %>% dplyr::mutate(total.votes=`Votes valables`)
                }),
              # Sum up 'total.votes' across all circonscriptions
              total.votes= circonscription %>% 
                purrr::map(~sum(.x$total.votes, na.rm = TRUE))
            ) %>% 
            # Flatten the list to a single vector of total votes
            tidyr::unnest(total.votes)
        } else {
          NULL  # Return NULL if the initial data was NULL
        }
      }),
    # Sum all total votes from 2006 and add as a new column
    total.votes_2006=data_2006 %>% 
      purrr::map(~sum(.x$total.votes, na.rm = TRUE))
  ) %>% 
  # Flatten the summed total votes for easier analysis
  tidyr::unnest(total.votes_2006)


# Processing 2011 election data similarly to 2006
data %<>%  dplyr::mutate(
  data_2011=data_2011 %>% 
    purrr::map(~{
      if(!is.null(.x)){
        .x %>% dplyr::mutate(
          circonscription=circonscription %>% 
            purrr::map(~{
              .x %>% dplyr::mutate(
                # Calculate total votes within each voting site
                total.votes=votes.data %>% 
                  purrr::map(~{
                    .x  %>% dplyr::pull("votes") %>% sum(na.rm = TRUE)
                  })) %>% 
                # Unnest the total votes calculated from nested voting sites
                tidyr::unnest(total.votes)
              
            }),
          # Sum up all the total votes across circonscriptions
          total.votes=circonscription %>% 
            purrr::map(~sum(.x$total.votes, na.rm = TRUE))
        ) %>% 
          # Flatten the structure to include total votes in the main dataframe
          tidyr::unnest(total.votes)
      } else {
        NULL
      }
    }),
  # Sum all the total votes from 2011 and integrate into the main dataframe
  total.votes_2011=data_2011 %>% 
    purrr::map(~sum(.x$total.votes, na.rm = TRUE))
) %>% 
  # Flatten the summed total votes for 2011
  tidyr::unnest(total.votes_2011)



# Similar process for 2018 election data with more nested levels
data %<>%  dplyr::mutate(
  data_2018=data_2018 %>% 
    purrr::map(~{
      if(!is.null(.x)){
        .x %>% dplyr::mutate(
          circonscription=circonscription %>% 
            purrr::map(~{
              .x %>% dplyr::mutate(
                ville.territoire=ville.territoire %>% 
                  purrr::map(~{
                    .x %>% dplyr::mutate(
                      voting.sites=voting.sites %>% 
                        purrr::map(~{
                          .x %>% dplyr::mutate(total.votes=voters)  # Map total votes directly from voters
                        }),
                      # Sum votes from all voting sites
                      total.votes = voting.sites %>% 
                        purrr::map(~sum(.x$total.votes, na.rm = TRUE))
                    ) %>% 
                      # Unnest total votes from each ville.territoire
                      tidyr::unnest(total.votes)
                  }),
                # Sum all votes from ville.territoire and unnest
                total.votes = ville.territoire %>% 
                  purrr::map(~sum(.x$total.votes, na.rm = TRUE))) %>% 
                tidyr::unnest(total.votes)
            }),
          # Sum up total votes from each circonscription and unnest for the final structure
          total.votes = circonscription %>% 
            purrr::map(~sum(.x$total.votes, na.rm = TRUE))) %>% 
          tidyr::unnest(total.votes)
      } else {
        NULL
      }
    }),
  # Sum and unnest total votes for 2018 in the main dataframe
  total.votes_2018=data_2018 %>% 
    purrr::map(~sum(.x$total.votes, na.rm = TRUE))
) %>% 
  tidyr::unnest(total.votes_2018)

##### 6.4-VOTES AND PERCENTAGE FOR KABILA IN EACH LEVEL #####

# This section of the script is dedicated to calculating the total votes and the 
# percentage of votes received by the candidate Kabila (and Ramazani in 2018) at 
# various administrative levels across the election years 2006, 2011, and 2018. 
# The method involves several steps:
# 
# 1. **Data Filtering and Extraction**:
#   - For each year, the data undergoes a check to ensure it is not null, which 
#   is crucial for maintaining accuracy.
# - The votes specifically for Kabila (or Ramazani in 2018) are extracted from 
# nested data structures by filtering for the candidate's name within each voting 
# site or circonscription.
# 
# 2. **Vote Calculation**:
#    - The valid votes for Kabila are aggregated first within the smallest units 
#    (voting sites or circonscriptions) and then summed up to provide totals for 
#    larger administrative areas.
#    - This process involves mapping through nested data structures and applying 
#    filters and summation functions to accurately compile the votes.
# 
# 3. **Percentage Calculation**:
#    - The percentage of votes received by Kabila or Ramazani is calculated by 
#    dividing their total votes by the total votes of all candidates at each 
#    administrative level. This step provides insight into the candidate's 
#    performance relative to others.
# - The calculation is done post-aggregation to ensure it reflects the comprehensive 
# vote share.
# 
# 4. **Data Aggregation and Unnesting**:
#   - After calculating the total votes and percentages, these metrics are nested 
#   back into the main dataset. Using `tidyr::unnest()`, the nested lists are 
#   simplified into standard columns for ease of analysis.
# 
# 5. **Result Integration**:
#   - The final step involves integrating the calculated votes and percentages 
#   into the main dataset, enhancing the dataset with key electoral metrics for 
#   Kabila and Ramazani, which are pivotal for electoral analysis.
# 



# Update the 'data' dataframe by adding Kabila's votes and percentages for 2006
data %<>%  dplyr::mutate(
  # Process each entry in 'data_2006' using a map function
  data_2006=data_2006 %>% 
    purrr::map(~{
      # Check if the data entry is not null
      if(!is.null(.x)){
        # For each circonscription, calculate Kabila's votes
        .x %>% dplyr::mutate(
          circonscription=circonscription %>% 
            purrr::map(~{
              # Within each circonscription, extract and sum votes for Kabila
              .x %>% dplyr::mutate(
                kabila.votes=votes.data %>% 
                  purrr::map(~{
                    .x %>% 
                      dplyr::filter(tolower(candidate)=="kabila") %>% 
                      dplyr::pull("votes") %>% 
                      sum(na.rm = TRUE)
                  })
              ) %>% 
                tidyr::unnest(kabila.votes) %>% 
                dplyr::mutate(kabila.percent=kabila.votes/total.votes)
            }),
          # Sum Kabila's votes across all circonscriptions
          kabila.votes=circonscription %>% 
            purrr::map(~sum(.x$kabila.votes,na.rm = TRUE))
        ) %>% 
          tidyr::unnest(kabila.votes) %>% 
          dplyr::mutate(kabila.percent=kabila.votes/total.votes)
      } else {
        NULL
      }
    }),
  # Aggregate Kabila's votes and calculate the overall percentage for 2006
  kabila.votes_2006 = data_2006 %>%
    purrr::map(~sum(.x$kabila.votes,na.rm = TRUE))
) %>% 
  tidyr::unnest(kabila.votes_2006) %>% 
  dplyr::mutate(kabila.percent_2006=kabila.votes_2006/total.votes_2006)




# Similar processing for 2011 data with Kabila's votes and percentages
data %<>%  dplyr::mutate(
  data_2011=data_2011 %>% 
    purrr::map(~{
      if(!is.null(.x)){
        .x %>% dplyr::mutate(
          circonscription=circonscription %>% 
            purrr::map(~{
              .x %>% dplyr::mutate(
                kabila.votes = votes.data %>% 
                  purrr::map(~{
                    .x %>% dplyr::filter(tolower(candidate)=="kabila") %>% 
                      dplyr::pull("votes") %>% 
                      sum(na.rm = TRUE)
                  })
              ) %>% 
                tidyr::unnest(kabila.votes) %>% 
                dplyr::mutate(kabila.percent=kabila.votes/total.votes)
            }),
          kabila.votes=circonscription %>% 
            purrr::map(~sum(.x$kabila.votes,na.rm = TRUE))
        ) %>% 
          tidyr::unnest(kabila.votes)%>% 
          dplyr::mutate(kabila.percent=kabila.votes/total.votes)
      } else {
        NULL
      }
    }),
  kabila.votes_2011=data_2011 %>% 
    purrr::map(~sum(.x$kabila.votes,na.rm = TRUE))
) %>% 
  tidyr::unnest(kabila.votes_2011) %>% 
  dplyr::mutate(kabila.percent_2011=kabila.votes_2011/total.votes_2011)




# Processing 2018 data for another candidate, Ramazani
data %<>%  dplyr::mutate(
  data_2018=data_2018 %>% 
    purrr::map(~{
      if(!is.null(.x)){
        .x %>% dplyr::mutate(
          circonscription=circonscription %>% 
            purrr::map(~{
              .x %>% dplyr::mutate(
                ville.territoire=ville.territoire %>% 
                  purrr::map(~{
                    .x %>% dplyr::mutate(
                      voting.sites=voting.sites %>% 
                        purrr::map(~{
                          .x %>% dplyr::mutate(
                            ramazani.votes=votes.data %>% 
                              purrr::map(~{
                                .x %>% 
                                  dplyr::filter(tolower(candidate)=="ramazani") %>% 
                                  dplyr::pull("votes") %>% 
                                  sum(na.rm = TRUE)
                              })
                          ) %>% 
                            tidyr::unnest(ramazani.votes) %>% 
                            dplyr::mutate(ramazani.percent=ramazani.votes/total.votes)
                        }),
                      ramazani.votes=voting.sites %>% 
                        purrr::map(~sum(.x$ramazani.votes,na.rm = TRUE))
                    ) %>% 
                      tidyr::unnest(ramazani.votes) %>% 
                      dplyr::mutate(ramazani.percent=ramazani.votes/total.votes)
                  }),
                ramazani.votes=ville.territoire %>% 
                  purrr::map(~sum(.x$ramazani.votes,na.rm = TRUE))
              ) %>% 
                tidyr::unnest(ramazani.votes) %>% 
                dplyr::mutate(ramazani.percent=ramazani.votes/total.votes)
            }),
          ramazani.votes=circonscription %>% 
            purrr::map(~sum(.x$ramazani.votes,na.rm = TRUE))
        ) %>% 
          tidyr::unnest(ramazani.votes) %>% 
          dplyr::mutate(ramazani.percent=ramazani.votes/total.votes)
      } else {
        NULL
      }
    }),
  ramazani.votes_2018=data_2018 %>% 
    purrr::map(~sum(.x$ramazani.votes,na.rm = TRUE))
) %>% 
  tidyr::unnest(ramazani.votes_2018) %>% 
  dplyr::mutate(ramazani.percent_2018=ramazani.votes_2018/total.votes_2018)



##### 6.5-BALLOT BOXES IN EACH LEVEL (2011,2018 ONLY) #####

# This section of the script processes and calculates the total number of ballot 
# boxes counted at various administrative levels during the 2011 and 2018 elections. 
# The approach involves multiple steps to ensure accurate aggregation of data:
# 
# 1. **Data Validation**: Each dataset is checked to confirm that it is not null, 
# ensuring that only valid data is processed. This step is crucial to avoid 
# errors during data manipulation.
# 
# 2. **Mapping and Summation**:
#   - For each entry in the dataset, the script uses `purrr::map()` to apply 
#   functions at different nesting levels, calculating the sum of ballot boxes 
#   counted within each administrative unit, such as circonscriptions or voting sites.
# - This involves iterating over each circonscription and, for the 2018 data, 
# each ville.territoire within the circonscriptions, to aggregate the counts 
# from the smallest units upwards.
# 
# 3. **Data Aggregation**:
#   - The aggregated counts of ballot boxes from the smaller units (voting sites 
#   or circonscriptions) are further summed to provide totals for larger areas.
# - This is performed using nested `purrr::map()` functions that apply the summation 
# across the nested data structures.
# 
# 4. **Flattening and Integration**:
#   - The nested results are flattened using `tidyr::unnest()` to integrate the 
#   summed ballot box counts back into the main dataframe. This transformation 
#   simplifies the nested list into a standard column format within the main dataset.
# 
# 5. **Final Aggregation and Calculation**:
#   - After processing individual entries, a final summation is performed across 
#   all entries for each election year to calculate the total number of ballot 
#   boxes counted across all circonscriptions.
# - The results are added to the dataframe, providing a clear overview of ballot 
# box distribution and availability during the elections.


# Update the 'data' dataframe to include the count of ballot boxes for 2011
data %<>%  dplyr::mutate(
  # Process each entry in 'data_2011'
  data_2011=data_2011 %>% 
    purrr::map(~{
      # Check if the data entry is not null to ensure valid data processing
      if(!is.null(.x)){
        # Calculate the sum of ballot boxes counted within each circonscription
        .x %>% dplyr::mutate(
          ballot.boxes_counted=circonscription %>% 
            purrr::map(~sum(.x$ballot.boxes_counted, na.rm = TRUE))
        ) %>% 
          # Flatten the list to integrate ballot box counts into the main dataframe
          tidyr::unnest(ballot.boxes_counted)
      }else{
        NULL  # Return NULL if the initial data was NULL
      }
    }),
  # Sum the total ballot boxes counted across all circonscriptions for 2011
  ballot.boxes_counted_2011=data_2011 %>% 
    purrr::map(~sum(.x$ballot.boxes_counted, na.rm = TRUE))
) %>% 
  # Flatten the summed total of ballot boxes for easier analysis and integration
  tidyr::unnest(ballot.boxes_counted_2011)
# Similar processing is done for 2018 data
data %<>%  dplyr::mutate(
  # Process each entry in 'data_2018'
  data_2018=data_2018 %>% 
    purrr::map(~{
      if(!is.null(.x)){
        # Navigate deeper into nested structures for 2018, reaching down to voting sites
        .x %>% dplyr::mutate(
          circonscription=circonscription %>% 
            purrr::map(~{
              .x %>% dplyr::mutate(
                ville.territoire=ville.territoire %>% 
                  purrr::map(~{
                    .x %>% dplyr::mutate(
                      # Aggregate ballot boxes counted within each voting site
                      ballot.boxes_counted=voting.sites %>% 
                        purrr::map(~sum(.x$ballot.boxes_counted, na.rm = TRUE))
                    ) %>% 
                      # Unnest the results to collapse nested structures
                      tidyr::unnest(ballot.boxes_counted)
                  }),
                # Sum ballot boxes counted at the 'ville.territoire' level
                ballot.boxes_counted=ville.territoire %>% 
                  purrr::map(~sum(.x$ballot.boxes_counted, na.rm = TRUE))
              ) %>% 
                tidyr::unnest(ballot.boxes_counted)
            }),
          # Sum ballot boxes counted at the 'circonscription' level
          ballot.boxes_counted=circonscription %>% 
            purrr::map(~sum(.x$ballot.boxes_counted, na.rm = TRUE))
        ) %>% 
          tidyr::unnest(ballot.boxes_counted)
      }else{
        NULL
      }
    }),
  # Sum the total ballot boxes counted across all circonscriptions for 2018
  ballot.boxes_counted_2018=data_2018 %>% 
    purrr::map(~sum(.x$ballot.boxes_counted, na.rm = TRUE))
) %>% 
  # Flatten the summed total of ballot boxes to finalize integration
  tidyr::unnest(ballot.boxes_counted_2018)


##### 6.6-VOTING SITES WITH ZERO VOTERS (2018 ONLY) #####

# This section identifies and quantifies voting sites with zero voters from the 
# 2018 election data. It performs the following operations:
#   
# 1. **Filtering Voting Sites**: The script navigates through multiple nested 
# data structures, filtering out voting sites where the number of voters is not 
# available.
# 2. **Counting Voting Sites**: For each administrative level (e.g., circonscription, 
# ville.territoire), it counts the number of voting sites that reported zero voters.
# 3. **Aggregating Data**: These counts are aggregated at higher administrative 
# levels, providing a comprehensive total of voting sites with zero voters for each level.
# 4. **Data Integration**: The results are integrated back into the main dataset, 
# adding the total counts of zero-voter sites for further analysis and reporting.
# 


# Update 'data' dataframe to include the count of voting sites with non-zero voters for 2018
data %<>%  dplyr::mutate(
  # Process each entry in 'data_2018'
  data_2018 = data_2018 %>% 
    purrr::map(~{
      # Check if the data entry is not null to ensure valid data processing
      if (!is.null(.x)) {
        # Calculate the number of non-zero voter sites within each circonscription
        .x %>% dplyr::mutate(
          circonscription = circonscription %>% 
            purrr::map(~{
              .x %>% dplyr::mutate(
                ville.territoire = ville.territoire %>% 
                  purrr::map(~{
                    .x %>% dplyr::mutate(
                      # Count the number of voting sites with non-zero voters
                      n.voting.sites = voting.sites %>% 
                        purrr::map(
                          ~.x %>% 
                            dplyr::filter(!is.na(voters)) %>% 
                            nrow
                        )
                    ) %>% 
                      # Unnest the results to integrate into the main dataframe
                      tidyr::unnest(n.voting.sites)
                  }),
                # Sum the counts of non-zero voter sites at the ville.territoire level
                n.voting.sites = ville.territoire %>% 
                  purrr::map(~sum(.x$n.voting.sites, na.rm = TRUE))
              ) %>% 
                tidyr::unnest(n.voting.sites)
            }),
          # Sum the counts at the circonscription level
          n.voting.sites = circonscription %>% 
            purrr::map(~sum(.x$n.voting.sites, na.rm = TRUE))
        ) %>% 
          tidyr::unnest(n.voting.sites)
      } else {
        NULL  # Return NULL if the initial data was NULL
      }
    }),
  # Sum the total number of non-zero voter sites across all circonscriptions for 2018
  n.voting.sites_2018 = data_2018 %>% 
    purrr::map(~sum(.x$n.voting.sites, na.rm = TRUE))
) %>% 
  # Unnest the summed total to integrate into the main dataframe
  tidyr::unnest(n.voting.sites_2018)



# Update 'data' dataframe to include the count of voting sites with zero voters for 2018
data %<>%  dplyr::mutate(
  # Process each entry in 'data_2018'
  data_2018 = data_2018 %>% 
    purrr::map(~{
      # Check if the data entry is not null to ensure valid data processing
      if (!is.null(.x)) {
        # Calculate the number of zero voter sites within each circonscription
        .x %>% dplyr::mutate(
          circonscription = circonscription %>% 
            purrr::map(~{
              .x %>% dplyr::mutate(
                ville.territoire = ville.territoire %>% 
                  purrr::map(~{
                    .x %>% dplyr::mutate(
                      # Count the number of voting sites with zero voters
                      zero.voters.sites = voting.sites %>% 
                        purrr::map(~sum(.x$voters == 0, na.rm = TRUE))
                    ) %>% 
                      # Unnest the results to integrate into the main dataframe
                      tidyr::unnest(zero.voters.sites)
                  }),
                # Sum the counts of zero voter sites at the ville.territoire level
                zero.voters.sites = ville.territoire %>% 
                  purrr::map(~sum(.x$zero.voters.sites, na.rm = TRUE))
              ) %>% 
                tidyr::unnest(zero.voters.sites)
            }),
          # Sum the counts at the circonscription level
          zero.voters.sites = circonscription %>% 
            purrr::map(~sum(.x$zero.voters.sites, na.rm = TRUE))
        ) %>% 
          tidyr::unnest(zero.voters.sites)
      } else {
        NULL  # Return NULL if the initial data was NULL
      }
    }),
  # Sum the total number of zero voter sites across all circonscriptions for 2018
  zero.voters.sites_2018 = data_2018 %>% 
    purrr::map(~sum(.x$zero.voters.sites, na.rm = TRUE))
) %>% 
  # Unnest the summed total to integrate into the main dataframe
  tidyr::unnest(zero.voters.sites_2018)


##### 6.7-VOTES AND PERCENTAGES FOR OTHER CANDIDATES IN 2018 #####

# This section calculates the total votes and percentages for the candidates 
# Fayulu and Tshisekedi in the 2018 election. The script performs the following 
# tasks:
#   
#   1. **Fayulu's Votes and Percentages**:
#    - For each administrative level (circonscription, ville.territoire, voting sites), 
#    the script identifies and sums the votes cast for Fayulu.
#    - It then calculates the percentage of votes Fayulu received out of the total 
#    votes at each level.
#    - These results are aggregated and integrated back into the main dataset, 
#    providing Fayulu's total votes and percentage for 2018.
# 
# 2. **Tshisekedi's Votes and Percentages**:
#    - Similar to the process for Fayulu, the script identifies and sums the 
#    votes cast for Tshisekedi at each administrative level.
#    - It calculates Tshisekedi's percentage of total votes at each level.
# - The aggregated results are then integrated back into the main dataset, 
# detailing Tshisekedi's total votes and percentage for 2018.


# Faluyu
data %<>%  dplyr::mutate(
  data_2018 = data_2018 %>%
    purrr::map(~{
      
      # Check if the current element is not NULL
      if(!is.null(.x)){
        
        .x %>% dplyr::mutate(
          # Map over circonscription
          circonscription = circonscription %>% 
            purrr::map(~{
              
              .x %>% dplyr::mutate(
                # Map over ville.territoire
                ville.territoire = ville.territoire %>% 
                  purrr::map(~{
                    
                    .x %>% dplyr::mutate(
                      # Map over voting.sites
                      voting.sites = voting.sites %>% 
                        purrr::map(~{
                          
                          .x %>% dplyr::mutate(
                            # Calculate fayulu.votes for each voting site
                            fayulu.votes = votes.data %>% 
                              purrr::map(~{
                                
                                # Filter votes for Fayulu and sum them
                                .x %>% dplyr::filter(tolower(candidate) == "fayulu") %>% 
                                  dplyr::pull("votes") %>% 
                                  sum(na.rm = TRUE)
                              })
                          ) %>% 
                            tidyr::unnest(fayulu.votes) %>% 
                            # Calculate the percentage of Fayulu's votes
                            dplyr::mutate(fayulu.percent = fayulu.votes / total.votes)
                        }),
                      # Sum Fayulu's votes at the ville.territoire level
                      fayulu.votes = voting.sites %>% 
                        purrr::map(~sum(.x$fayulu.votes, na.rm = TRUE))
                    ) %>% 
                      tidyr::unnest(fayulu.votes) %>% 
                      # Calculate the percentage of Fayulu's votes at the ville.territoire level
                      dplyr::mutate(fayulu.percent = fayulu.votes / total.votes)
                  }),
                # Sum Fayulu's votes at the circonscription level
                fayulu.votes = ville.territoire %>% 
                  purrr::map(~sum(.x$fayulu.votes, na.rm = TRUE))
              ) %>% 
                tidyr::unnest(fayulu.votes) %>%
                # Calculate the percentage of Fayulu's votes at the circonscription level
                dplyr::mutate(fayulu.percent = fayulu.votes / total.votes)
            }),
          # Sum Fayulu's votes at the data_2018 level
          fayulu.votes = circonscription %>% 
            purrr::map(~sum(.x$fayulu.votes, na.rm = TRUE))
        ) %>% 
          tidyr::unnest(fayulu.votes) %>% 
          # Calculate the percentage of Fayulu's votes at the data_2018 level
          dplyr::mutate(fayulu.percent = fayulu.votes / total.votes)
      } else {
        NULL
      }
    }),
  # Sum Fayulu's votes for all data_2018
  fayulu.votes_2018 = data_2018 %>% 
    purrr::map(~sum(.x$fayulu.votes, na.rm = TRUE))
) %>% 
  tidyr::unnest(fayulu.votes_2018) %>% 
  # Calculate the percentage of Fayulu's votes for 2018
  dplyr::mutate(fayulu.percent_2018 = fayulu.votes_2018 / total.votes_2018)

# Tshisekedi
data %<>%  dplyr::mutate(
  data_2018 = data_2018 %>%
    purrr::map(~{
      
      # Check if the current element is not NULL
      if(!is.null(.x)){
        
        .x %>% dplyr::mutate(
          # Map over circonscription
          circonscription = circonscription %>% 
            purrr::map(~{
              
              .x %>% dplyr::mutate(
                # Map over ville.territoire
                ville.territoire = ville.territoire %>% 
                  purrr::map(~{
                    
                    .x %>% dplyr::mutate(
                      # Map over voting.sites
                      voting.sites = voting.sites %>% 
                        purrr::map(~{
                          
                          .x %>% dplyr::mutate(
                            # Calculate tshisekedi.votes for each voting site
                            tshisekedi.votes = votes.data %>% 
                              purrr::map(~{
                                
                                # Filter votes for Tshisekedi and sum them
                                .x %>% 
                                  dplyr::filter(tolower(candidate) == "tshisekedi") %>% 
                                  dplyr::pull("votes") %>% 
                                  sum(na.rm = TRUE)
                              })) %>% 
                            tidyr::unnest(tshisekedi.votes) %>% 
                            # Calculate the percentage of Tshisekedi's votes
                            dplyr::mutate(tshisekedi.percent = tshisekedi.votes / total.votes)
                        }),
                      # Sum Tshisekedi's votes at the ville.territoire level
                      tshisekedi.votes = voting.sites %>% 
                        purrr::map(~sum(.x$tshisekedi.votes, na.rm = TRUE))
                    ) %>% 
                      tidyr::unnest(tshisekedi.votes) %>% 
                      # Calculate the percentage of Tshisekedi's votes at the ville.territoire level
                      dplyr::mutate(tshisekedi.percent = tshisekedi.votes / total.votes)
                  }),
                # Sum Tshisekedi's votes at the circonscription level
                tshisekedi.votes = ville.territoire %>% 
                  purrr::map(~sum(.x$tshisekedi.votes, na.rm = TRUE))
              ) %>% 
                tidyr::unnest(tshisekedi.votes) %>% 
                # Calculate the percentage of Tshisekedi's votes at the circonscription level
                dplyr::mutate(tshisekedi.percent = tshisekedi.votes / total.votes)
            }),
          # Sum Tshisekedi's votes at the data_2018 level
          tshisekedi.votes = circonscription %>% 
            purrr::map(~sum(.x$tshisekedi.votes, na.rm = TRUE))
        ) %>% 
          tidyr::unnest(tshisekedi.votes) %>% 
          # Calculate the percentage of Tshisekedi's votes at the data_2018 level
          dplyr::mutate(tshisekedi.percent = tshisekedi.votes / total.votes)
      } else {
        NULL
      }
    }),
  # Sum Tshisekedi's votes for all data_2018
  tshisekedi.votes_2018 = data_2018 %>% 
    purrr::map(~sum(.x$tshisekedi.votes, na.rm = TRUE))
) %>% 
  tidyr::unnest(tshisekedi.votes_2018) %>% 
  # Calculate the percentage of Tshisekedi's votes for 2018
  dplyr::mutate(tshisekedi.percent_2018 = tshisekedi.votes_2018 / total.votes_2018)


##### 6.8-TURNOUT #####
# This section calculates voter turnout rates for the election years 2006, 2011, 
# and 2018. The script performs the following tasks:
#   
# 1. **Calculate Turnout Rates**: It computes the turnout rate for each year by 
# dividing the number of voters by the number of registered voters for 2006, 
# 2011, and 2018.
# 2. **Generate Summary Statistics**: Although commented out, the script includes 
# code to generate summary statistics for the calculated turnout rates, providing 
# insights into the distribution of turnout across the datasets.

# Mutate the data to calculate turnout rates for the years 2006, 2011, and 2018
data %<>% dplyr::mutate(
  # Calculate the turnout rate for 2006 by dividing the number of voters by the 
  # number of registered voters
  turnout_2006 = voters_2006 / registered.voters_2006,
  # Calculate the turnout rate for 2011 by dividing the number of voters by the 
  # number of registered voters
  turnout_2011 = voters_2011 / registered.voters_2011,
  # Calculate the turnout rate for 2018 by dividing the number of voters by the 
  # number of registered voters
  turnout_2018 = voters_2018 / registered.voters_2018
)

# The following commented-out code generates a summary of the turnout rates
# Uncomment this line to see a summary of the calculated turnout rates for each year
# data %>% dplyr::select(starts_with("turnout_")) %>% summary

# Example summary output for turnout rates:
# turnout_2006     turnout_2011      turnout_2018   
# Min.   :0.1230   Min.   :0.05822   Min.   :0.0000  
# 1st Qu.:0.6480   1st Qu.:0.51215   1st Qu.:0.3095  
# Median :0.7410   Median :0.57121   Median :0.3940  
# Mean   :0.7034   Mean   :0.57619   Mean   :0.3821  
# 3rd Qu.:0.8150   3rd Qu.:0.63532   3rd Qu.:0.4656  
# Max.   :0.9990   Max.   :1.00137   Max.   :0.7334  
# NA's   :5

# The following commented-out code generates a detailed summary using the dfSummary 
# function from the summarytools package
# Uncomment this line to see a detailed summary of the turnout rates
# data %>% dplyr::select(starts_with("turnout_")) %>% summarytools::dfSummary() %>% 
# summarytools::stview()


#### 7-HARMONIZE MAP AND DATA LOCATIONS ####

# This section aligns geographic data with the electoral dataset to ensure consistency 
# in analysis. It performs the following tasks:
#   
#   1. **Read Detailed Shapefiles**: Loads detailed shapefiles for Congo's 
#   territories to get precise geographic boundaries.
# 2. **Filter and Standardize Kinshasa Regions**: Filters the shapefile data to 
# include only Kinshasa regions and standardizes names by replacing hyphens 
# with spaces.
# 3. **Update and Summarize Geometry Data**: Updates the Kinshasa shapefile data 
# with standardized names, selects relevant columns, groups by names, and 
# summarizes geometries.
# 4. **Read Main Shapefiles**: Loads the main shapefile for Congo's territories 
# and creates an index by standardizing territory names.
# 5. **Exclude Kinshasa**: Excludes Kinshasa from the main shapefile data.
# 6. **Create Indices for Matching**: Extracts and standardizes names from both 
# the map and data for matching purposes.
# 7. **Manual Name Matching**: Applies manual name corrections for regions and 
# cities to ensure consistency between map and data.
# 8. **Match Indices**: Matches data names with map names and identifies 
# unmatched names.
# 9. **Create Index DataFrame**: Creates a DataFrame to link data and map 
# indices, ensuring unique matches.
# 10. **Join and Summarize Map Data**: Joins the map data with matched indices, 
# updates index names, and groups by index to summarize geometries.
# 11. **Transform Projections**: Ensures the geographic projections of Kinshasa 
# borders match the main dataset.
# 12. **Combine Borders**: Combines Kinshasa borders with the main territory 
# borders.
# 13. **Clean Environment**: Removes unnecessary variables, keeping only the 
# relevant ones for further analysis.
# 
# This section harmonizes geographic and electoral data, facilitating accurate 
# mapping and spatial analysis of election results.

# Read the detailed shapefile for Congo's territories
detailed.congo.territoire.border <- sf::st_read(here::here("data/Les territoires de Congo (territories of Congo)/data/commondata/data0/Territoire.shp"))

# Filter the shapefile data to include only Kinshasa regions
congo.territoire.borders.kinsasha <- detailed.congo.territoire.border %>%
  dplyr::filter(stringr::str_replace(tolower(NOM), "-", " ") %in% stringr::str_replace(names(kinshasa.subprov), "-", " "))

# Create a copy of the Kinshasa subprovince names
.kinsasha.subp <- kinshasa.subprov
# Standardize the names by replacing hyphens with spaces
names(.kinsasha.subp) <- stringr::str_replace(names(kinshasa.subprov), "-", " ")

# Update the Kinshasa shapefile data with the standardized names
congo.territoire.borders.kinsasha %<>% dplyr::mutate(
  NOM = .kinsasha.subp[stringr::str_replace(tolower(congo.territoire.borders.kinsasha$NOM), "-", " ")]
)

# Select the updated names and geometry columns
congo.territoire.borders.kinsasha %<>% dplyr::select(index.data = NOM, geometry)

# Group by the updated names and summarize to combine geometries
congo.territoire.borders.kinsasha %<>% dplyr::group_by(index.data) %>% dplyr::summarise()

# Read the main shapefile for Congo's territories
congo.territoire.borders <- sf::st_read(here::here("data/cod_adm2_un/cod_adm2_un.shp"))

# Create an index for the map data by standardizing the territory names
congo.territoire.borders %<>% dplyr::mutate(index.map = adm2_name %>% tolower() %>% stringr::str_squish())

# Exclude Kinshasa from the main shapefile data
congo.territoire.borders %<>% dplyr::filter(index.map != "kinshasa")

# Create indices for matching map names with data names

# Extract the map names
map.index <- congo.territoire.borders %>% dplyr::pull("index.map")
map.index %<>% magrittr::set_names(map.index)

# Extract the data names
data.index <- data %>% dplyr::pull(index)
data.index %<>% magrittr::set_names(data.index)

# Define manual matches for regions with different names in the map and data
manual.matches <- c(
  "mankanza" = "makanza",
  "beni" = "oicha",
  "beni ville" = "beni"
)

# Update the data index names with manual matches
names(data.index) <- dplyr::case_when(names(data.index) %in% names(manual.matches) ~ manual.matches[data.index], TRUE ~ names(data.index))

# Define manual matches for cities merged into their territories
manual.matches <- c(
  "isiro" = "rungu",
  "bunia" = "irumu",
  "nioki" = "kutu",
  "inkisi" = "madimba",
  "mangai" = "idiofa",
  "dibaya-lubwe" = "idiofa",
  "yangambi" = "isangi",
  "dingila" = "bambesa",
  "ariwara" = "aru",
  "baraka" = "fizi",
  "kamituga" = "mwenga",
  "ingbokolo" = "aru",
  "mongwalu" = "djugu",
  "kalima" = "kailo",
  "namoya" = "kabambare",
  "kasaji" = "dilolo",
  "lukalaba" = "tshilenge",
  "tshimbulu" = "dibaya",
  "bena-dibele" = "kole",
  "kaoze" = "moba",
  "aba" = "faradje",
  "tshumbe" = "kole",
  "bangu" = "songololo"
)

# Update the map index names with manual matches
names(map.index) <- dplyr::case_when(names(map.index) %in% names(manual.matches) ~ manual.matches[map.index], TRUE ~ names(map.index))

# Remove the word "city" from map index names
names(map.index) %<>% stringr::str_remove("city") %>% stringr::str_squish()

# Match data and map indices
matches.map.data <- match(names(map.index), names(data.index))
matches.data.map <- match(names(data.index), names(map.index))

# Extract matched names for map and data
villes.map.data <- map.index[!is.na(matches.map.data)]
villes.data.map <- data.index[!is.na(matches.data.map)]

# Get names not matched for map and data
villes.unique.map <- map.index[is.na(matches.map.data)]
villes.unique.data <- data.index[is.na(matches.data.map)]

# Create index data.frame for matching data and map indices
matches.data.map <- match(names(villes.data.map), names(villes.map.data))
matches.map.data <- match(names(villes.map.data), names(villes.data.map))
data.map.index <- dplyr::bind_rows(
  data.frame(index.data = villes.data.map, index.map = villes.map.data[matches.data.map]),
  data.frame(index.data = villes.data.map[matches.map.data], index.map = villes.map.data)
) %>% dplyr::distinct()

# Join the map data with the matched indices and update the index.data column
congo.territoire.borders %<>% dplyr::full_join(data.map.index, by = "index.map") %>%
  dplyr::mutate(index.data = dplyr::case_when(is.na(index.data) ~ index.map, TRUE ~ index.data)) %>%
  dplyr::select(index.data, geometry) %>%
  dplyr::group_by(index.data) %>%
  dplyr::summarise()

# Get the data projection
lon_lat_projection <- sf::st_crs(congo.territoire.borders)

# Transform the Kinshasa borders to match the data projection
congo.territoire.borders.kinsasha <- sf::st_transform(congo.territoire.borders.kinsasha, lon_lat_projection)

# Combine the main borders with the Kinshasa borders
congo.territoire.borders <- dplyr::bind_rows(congo.territoire.borders, congo.territoire.borders.kinsasha)

# Clean up the environment, keeping only relevant variables
rm(list = setdiff(ls(), c("data", "kinshasa.subprov", "congo.territoire.borders", "data.map.index")))

#### 8-CONFLICT DATA ####

# This section integrates conflict data with geographic boundaries for detailed 
# spatial analysis. It performs the following tasks:
#   
# 1. **Load Conflict Data**: Loads the UCDP Georeferenced Event Dataset (GED) 
#   for organized violence events.
# 2. **Set Map Projection**: Obtains the coordinate reference system (CRS) from 
# the Congo territory borders to ensure consistency in spatial analysis.
# 3. **Georeference Conflict Data**: Converts the conflict data into a spatial 
# format using longitude and latitude coordinates.
# 4. **Filter Conflicts in DRC**: Filters the conflict events to include only 
# those within the geographic boundaries of the Democratic Republic of Congo (DRC).
# 5. **Categorize Types of Violence**: Classifies the conflict events into 
# categories based on the type of violence and the involved parties.
# 6. **Clean Environment**: Retains only relevant variables for further analysis, 
# ensuring a clean workspace.
# 
# By integrating and categorizing conflict data within the geographic context of 
# the DRC, this section enhances the dataset's ability to support spatial 
# analysis of organized violence events.

# UCDP Georeferenced Event Dataset (GED) Global version 20.1
# 
# This dataset is UCDP's most disaggregated dataset, covering individual events of organized violence (phenomena of lethal violence occurring at a given time and place). These events are sufficiently fine-grained to be geo-coded down to the level of individual villages, with temporal durations disaggregated to single, individual days.
# 
# Available as:
# 
# CSV  EXCEL  RDATA  STATA  CODEBOOK
# 
# Please cite:
# 
# • Pettersson, Therese & Magnus Öberg (2020) Organized violence, 1989-2019. Journal of Peace Research 57(4).
# 
# • Sundberg, Ralph and Erik Melander (2013) Introducing the UCDP Georeferenced Event Dataset. Journal of Peace Research 50(4).
#
# Downloaded from https://ucdp.uu.se/downloads/index.html#ged_global on 2021/01/30

# Load the conflict data from the RData file
load(here::here("data/ged201.RData"))

# Get the coordinate reference system (CRS) of the Congo territory borders
map.projection <- sf::st_crs(congo.territoire.borders)

# Georeference the conflict data
ged201 %<>% sf::st_as_sf(coords = c("longitude", "latitude"), crs = map.projection)

# Filter conflicts in the Democratic Republic of Congo (DRC) by cropping the spatial data to the Congo borders
ged201 %<>% sf::st_crop(sf::st_bbox(congo.territoire.borders))

# Intersect the conflict data with the Congo territory borders to keep only relevant conflicts
ged201 %<>% sf::st_intersection(congo.territoire.borders)

# Categorize the types of violence in the conflict data
ged201 %<>% dplyr::mutate(type = dplyr::case_when(
  type_of_violence == 2 & side_b != "Civilians" ~ "Non-state vs non-state",
  type_of_violence == 1 & side_b != "Civilians" ~ "State vs non-state",
  type_of_violence == 3 & stringr::str_detect(side_a, "Government") & side_b == "Civilians" ~ "State vs civilians",
  type_of_violence == 3 & !stringr::str_detect(side_a, "Government") & side_b == "Civilians" ~ "Non-state vs civilians",
  TRUE ~ NA_character_
))

# Clean up the environment, keeping only relevant variables
rm(list = setdiff(ls(), c("data", "kinshasa.subprov", "congo.territoire.borders", "ged201", "data.map.index")))


##### 8.1-AGGREGATE CONFLICT DATA FOR EACH ELECTION AND MAP REGION #####

# This section aggregates conflict data for each election period and map region, 
# focusing on conflicts from the five years preceding each election. The script 
# performs the following tasks:
#   
# 1. **Assign Election Periods**: Categorizes conflict events into election periods 
# based on their dates.
# 2. **Filter Relevant Conflicts**: Retains only conflict events that fall within 
# the specified election periods.
# 3. **Group and Nest Data**: Groups conflicts by data area and election period, 
# nesting the data within each group for further analysis.
# 4. **Summarize Conflict Data**: Counts the number of conflicts and related 
# deaths for each election period.
# 5. **Aggregate by Conflict Type**: Groups and counts conflicts by type of 
# violence, further categorizing them based on involved parties.
# 6. **Clean Environment**: Keeps only the relevant variables for further 
# analysis, ensuring a clean workspace.
# 
# By organizing and summarizing conflict data in this manner, this section 
# facilitates detailed analysis of conflict patterns in relation to election 
# periods and geographic regions.

# Assign election periods to conflict events based on their dates
ged201 %<>% dplyr::mutate(election = dplyr::case_when(
  date_start >= lubridate::ymd("2001-01-17") & date_end <= lubridate::ymd("2006-07-30") ~ "conflicts.data_2006",
  date_start >= lubridate::ymd("2006-07-31") & date_end <= lubridate::ymd("2011-11-28") ~ "conflicts.data_2011",
  date_start >= lubridate::ymd("2011-11-29") & date_end <= lubridate::ymd("2018-12-30") ~ "conflicts.data_2018",
  TRUE ~ NA_character_
))

# Filter out conflicts that don't fall within the specified election periods
ged201 %<>% dplyr::filter(!is.na(election)) %>% 
  dplyr::filter(date_start >= lubridate::ymd("2001-01-17") & date_end <= lubridate::ymd("2018-12-30"))

# Group conflicts by data area and election, and nest the data within each group
conflict.aggregated <- ged201 %>% as.data.frame() %>%
  dplyr::filter(!is.na(election)) %>% 
  tidyr::nest(conflict.data = -c(index.data, election)) %>% 
  dplyr::filter(purrr::map(conflict.data, ~nrow(.x) > 0) %>% unlist) %>%
  tidyr::pivot_wider(id_cols = c(index.data), names_from = election, values_from = conflict.data)

# Define a function to summarize deaths in a given dataset
.summarise_deaths <- function(.x) {
  if (!is.null(.x)) {
    .x %>% as.data.frame() %>% dplyr::summarise(r = sum(best, na.rm = TRUE)) %>% dplyr::pull("r")
  }
}

# Count the number of conflicts and deaths in each election period
conflict.aggregated %<>% dplyr::mutate(
  n.conflicts_2006 = purrr::map(conflicts.data_2006, nrow),
  n.conflicts_2011 = purrr::map(conflicts.data_2011, nrow),
  n.conflicts_2018 = purrr::map(conflicts.data_2018, nrow),
  n.deaths_2006 = purrr::map(conflicts.data_2006, .summarise_deaths),
  n.deaths_2011 = purrr::map(conflicts.data_2011, .summarise_deaths),
  n.deaths_2018 = purrr::map(conflicts.data_2018, .summarise_deaths)
) %>%
  tidyr::unnest(c(dplyr::starts_with("n.conflicts"), dplyr::starts_with("n.deaths")))

# Aggregate conflicts by type of violence
conflict.aggregated_by_type <- ged201 %>% dplyr::mutate(type = dplyr::case_when(
  stringr::str_detect(side_a, "Government of DR Congo") & stringr::str_starts(type, "State") ~ stringr::str_replace(type, "State", "DRC"),
  !stringr::str_detect(side_a, "Government of DR Congo") & stringr::str_starts(type, "State") ~ stringr::str_replace(type, "State", "Foreign"),
  TRUE ~ type
)) %>% as.data.frame() %>%
  dplyr::filter(!is.na(election)) %>%
  tidyr::nest(conflict.data = -c(index.data, election, type))

# Count the number of conflicts and deaths by type
conflict.aggregated_by_type %<>% dplyr::mutate(
  n.conflicts = purrr::map(conflict.data, nrow),
  n.deaths = purrr::map(conflict.data, .summarise_deaths)
) %>%
  tidyr::unnest(c(dplyr::starts_with("n.conflicts"), dplyr::starts_with("n.deaths")))

# Separate the election column into two parts and convert the year part to integer
conflict.aggregated_by_type %<>% tidyr::separate(col = election, into = c("drop", "year"), sep = "_") %>% 
  dplyr::select(-drop) %>% dplyr::mutate(year = as.integer(year))

# Filter out rows with zero conflicts
conflict.aggregated_by_type %<>% dplyr::filter(n.conflicts > 0)

# Clean up the environment, keeping only relevant variables
rm(list = setdiff(ls(), c("data", "kinshasa.subprov", "congo.territoire.borders", "ged201", "conflict.aggregated", "conflict.aggregated_by_type", "data.map.index")))

##### 8.2-RECODING CONFLICT ACTORS #####

# This section processes conflict data to categorize and summarize conflicts by 
# actor types, ensuring all actors are accounted for and correctly classified. The steps include:
#   
# 1. **Read Actor Types**: Loads data on conflict actors and their classifications 
# from an Excel file.
# 2. **Filter Conflict Data**: Ensures that the conflict dataset only includes 
# actors present in the actor types list.
# 3. **Add Actor Type Information**: Merges actor type classifications into the 
# conflict data.
# 4. **Summarize Conflicts by Actor Type 1 and 2**: Aggregates conflicts and death 
# counts by actor type and election period, ensuring data consistency and completeness.
# For actor type 2, data is also summarized by territory.
# 5. **Merge Reversed Actor Types**: Combines conflict data where actor roles 
# (side_a and side_b) are reversed.
# 6. **Generate Actor Type Combinations**: Creates all possible combinations of 
# actor types for comprehensive analysis.
# 7. **Calculate Total Conflicts and Deaths**: Aggregates total counts across all 
# years and ensures consistency with the original data.
# 8. **Export Results**: Summarizes the data and exports it to an Excel file for 
# further analysis.
# 
# This process standardizes conflict actor data and provides a structured summary 
# for analyzing the impact of different actor types in conflicts.

# Read actor types data from an Excel file
actor_types <- readxl::read_excel(here::here("data/DRC armed groups in UCDP dataset Rwanda and Uganda.xlsx"))

# Create a copy of the conflict data for the relevant period
ged201_for_period <- ged201

# Check for actors in conflict data that are not in the actor types list and sort them
dplyr::setdiff(unique(c(ged201_for_period$side_a, ged201_for_period$side_b)), actor_types$groups) %>% sort

# Ensure all actors in conflict data are present in the actor types list
stopifnot(all(ged201_for_period$side_a %in% actor_types$groups) & all(ged201_for_period$side_b %in% actor_types$groups))

# Filter the conflict data to include only actors present in the actor types list
ged201_for_period %<>% dplyr::filter(side_a %in% actor_types$groups & side_b %in% actor_types$groups)

# Create lists for actor types
actor_types1 <- as.list(actor_types$type1) %>% magrittr::set_names(actor_types$groups)
actor_types2 <- as.list(actor_types$type2) %>% magrittr::set_names(actor_types$groups)

# Add actor type information to the conflict data
ged201_for_period %<>% as.data.frame() %>%
  dplyr::mutate(
    side_a_type1 = dplyr::case_when(side_a %in% names(actor_types1) ~ unlist(actor_types1[side_a]), TRUE ~ NA_character_),
    side_b_type1 = dplyr::case_when(side_b %in% names(actor_types1) ~ unlist(actor_types1[side_b]), TRUE ~ NA_character_),
    side_a_type2 = dplyr::case_when(side_a %in% names(actor_types2) ~ unlist(actor_types2[side_a]), TRUE ~ NA_character_),
    side_b_type2 = dplyr::case_when(side_b %in% names(actor_types2) ~ unlist(actor_types2[side_b]), TRUE ~ NA_character_)
  )

###### Actor type 1 ######

# Summarize conflicts and deaths by actor type 1 and election period
actor_types1_table <- ged201_for_period %>% dplyr::filter(!is.na(election)) %>%
  dplyr::select(election, side_a_type1, side_b_type1, n.deaths_a = deaths_a, n.deaths_b = deaths_b, n.deaths_civilians = deaths_civilians, n.deaths_unknown = deaths_unknown, n.deaths = best, index.data) %>% 
  dplyr::group_by(election, side_a_type1, side_b_type1, index.data) %>%
  dplyr::summarise(n.conflicts = dplyr::n(), dplyr::across(dplyr::starts_with("n.deaths"), ~sum(.)), .groups = "drop") %>% 
  dplyr::group_by(election, side_a_type1, side_b_type1) %>%
  dplyr::summarise(n.conflicts = sum(n.conflicts), dplyr::across(dplyr::starts_with("n.deaths"), ~sum(.)), territories = dplyr::n(), .groups = "drop")

# Ensure the total number of conflicts matches the original data
stopifnot(sum(actor_types1_table$n.conflicts) == nrow(ged201_for_period))

# Ensure the sum of deaths matches the total deaths
with(actor_types1_table, stopifnot(all(n.deaths == n.deaths_a + n.deaths_b + n.deaths_civilians + n.deaths_unknown)))

# Merge conflict data where the actor types are reversed (side_a and side_b switched)
actor_types1_table %<>% dplyr::left_join(
  actor_types1_table %>% dplyr::filter(side_a_type1 != side_b_type1), 
  by = c("election", side_a_type1 = "side_b_type1", side_b_type1 = "side_a_type1")
) %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(idx = paste(sort(c(election, side_a_type1, side_b_type1)), collapse = ",")) %>% 
  dplyr::ungroup() %>%
  dplyr::filter(!duplicated(idx)) %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(
    n.conflicts = sum(n.conflicts.x, n.conflicts.y, na.rm = TRUE),
    n.deaths_a = sum(n.deaths_a.x, n.deaths_b.y, na.rm = TRUE),
    n.deaths_b = sum(n.deaths_b.x, n.deaths_a.y, na.rm = TRUE),
    n.deaths_civilians = sum(n.deaths_civilians.x, n.deaths_civilians.y, na.rm = TRUE),
    n.deaths_unknown = sum(n.deaths_unknown.x, n.deaths_unknown.y, na.rm = TRUE),
    n.deaths = sum(n.deaths.x, n.deaths.y, na.rm = TRUE),
    territories = sum(territories.x, territories.y, na.rm = TRUE)
  ) %>%
  dplyr::ungroup()

# Ensure the total number of conflicts matches the original data
stopifnot(sum(actor_types1_table$n.conflicts) == nrow(ged201_for_period))

# Generate all possible combinations of actor types for completeness
all_types1 <- actor_types1 %>% unlist() %>% unique %>% combn(2) %>% t %>% as.data.frame() %>% magrittr::set_colnames(c("side_a", "side_b")) %>%
  dplyr::slice(rep(1:dplyr::n(), each = 3))

# Add election periods to the combinations
all_types1 %<>% dplyr::mutate(election = actor_types1_table$election %>% unique %>% rep(nrow(all_types1) / 3))

# Join the complete combinations with the summarized conflict data
actor_types1_table <- dplyr::bind_rows(
  all_types1 %>% dplyr::right_join(actor_types1_table, by = c("side_a" = "side_a_type1", "side_b" = "side_b_type1", "election")),
  all_types1 %>% dplyr::right_join(actor_types1_table, by = c("side_a" = "side_b_type1", "side_b" = "side_a_type1", "election"))
) %>% dplyr::full_join(all_types1, by = c("side_a", "side_b", "election")) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(idx = paste(sort(c(election, side_a, side_b)), collapse = ",")) %>% 
  dplyr::ungroup() %>%
  dplyr::filter(!duplicated(idx)) %>% 
  dplyr::select(side_a, side_b, election, n.conflicts, n.deaths, n.deaths_a, n.deaths_b, n.deaths_civilians, n.deaths_unknown, territories) %>%
  dplyr::mutate(dplyr::across(tidyselect:::where(is.numeric), ~tidyr::replace_na(., 0))) %>%
  dplyr::arrange(side_a, side_b, election) %>%
  tidyr::separate(election, c("drop", "year"), sep = "_") %>% dplyr::select(-drop) %>%
  tidyr::pivot_wider(id_cols = c(side_a, side_b), names_from = "year", values_from = c("n.conflicts", "n.deaths_a", "n.deaths_b", "n.deaths_civilians", "n.deaths_unknown", "n.deaths", "territories")) %>%
  dplyr::mutate(dplyr::across(tidyselect:::where(is.numeric), ~tidyr::replace_na(., 0)))

# Calculate total conflicts and deaths across all years
actor_types1_table %<>% dplyr::rowwise() %>% dplyr::mutate(
  n.conflicts_total = sum(dplyr::c_across(dplyr::starts_with("n.conflicts_"))),
  n.deaths_a_total = sum(dplyr::c_across(dplyr::matches("n.deaths_a_\\d{4}"))),
  n.deaths_b_total = sum(dplyr::c_across(dplyr::matches("n.deaths_b_\\d{4}"))),
  n.deaths_civilians_total = sum(dplyr::c_across(dplyr::matches("n.deaths_civilians_\\d{4}"))),
  n.deaths_unknown_total = sum(dplyr::c_across(dplyr::matches("n.deaths_unknown_\\d{4}"))),
  n.deaths_total = sum(dplyr::c_across(dplyr::matches("n.deaths_\\d{4}"))),
  territories_total = sum(dplyr::c_across(dplyr::starts_with("territories_")))
) %>% dplyr::ungroup()

# Ensure the total number of conflicts matches the original data
stopifnot(sum(actor_types1_table$n.conflicts_total) == nrow(ged201_for_period))

# Reorder and select relevant columns
actor_types1_table %<>% dplyr::select(side_a, side_b, dplyr::matches("_total$"), side_b, dplyr::matches("_2006$"), side_b, dplyr::matches("_2011$"), side_b, dplyr::matches("_2018$"))

# Add a total row summing all conflicts and deaths
actor_types1_table %<>% dplyr::bind_rows(
  actor_types1_table %>% dplyr::summarise(side_a = "Total", dplyr::across(dplyr::matches(c("^n.conflicts", "^n.deaths_(total|\\d{4}$)", "^territories")), sum))
)

# Load the workbook and get the first sheet
wb <- xlsx::loadWorkbook(here::here("data/Conflict by actor prototype table.xlsx"))
sheets <- xlsx::getSheets(wb)
sheet <- sheets[[1]]

# Add the summarized data to the Excel sheet starting at row 4, column 1
xlsx::addDataFrame(as.data.frame(actor_types1_table), sheet, col.names = FALSE, row.names = FALSE, startRow = 4, startColumn = 1)

# Save the workbook
xlsx::saveWorkbook(wb, here::here("results/Conflict by actor table type 1.xlsx"))

###### Actor type 2 ######

# Export conflict data to Excel
ged201_for_period %>% 
  dplyr::filter(!is.na(election)) %>% 
  dplyr::mutate(dyad = paste0(side_a_type2, "_", side_b_type2), election = stringr::str_extract(election, "\\d{4}")) %>%
  dplyr::select(territory = index.data, dyad, actor1 = side_a, actor2 = side_b, election, n.deaths = best, date_start) %>% 
  dplyr::arrange(territory, dyad, actor1, actor2, date_start) %>% 
  writexl::write_xlsx(here::here("results/UCDP_detailed.xlsx"))

# Summarize conflicts and deaths by actor type 2 and election period
actor_types2_table <- ged201_for_period %>% 
  dplyr::filter(!is.na(election)) %>%
  dplyr::select(election, side_a_type2, side_b_type2, n.deaths_a = deaths_a, n.deaths_b = deaths_b, n.deaths_civilians = deaths_civilians, n.deaths_unknown = deaths_unknown, n.deaths = best, index.data) %>% 
  dplyr::group_by(election, side_a_type2, side_b_type2, index.data) %>%
  dplyr::summarise(n.conflicts = dplyr::n(), dplyr::across(dplyr::starts_with("n.deaths"), ~sum(.)), .groups = "drop") %>% 
  dplyr::group_by(election, side_a_type2, side_b_type2) %>%
  dplyr::summarise(n.conflicts = sum(n.conflicts), dplyr::across(dplyr::starts_with("n.deaths"), ~sum(.)), territories = dplyr::n(), .groups = "drop")

# Ensure the total number of conflicts matches the original data
stopifnot(sum(actor_types2_table$n.conflicts) == nrow(ged201_for_period))

# Ensure the sum of deaths matches the total deaths
with(actor_types2_table, stopifnot(all(n.deaths == n.deaths_a + n.deaths_b + n.deaths_civilians + n.deaths_unknown)))

# Merge conflict data where the actor types are reversed (side_a and side_b switched)
actor_types2_table %<>% 
  dplyr::left_join(actor_types2_table %>% dplyr::filter(side_a_type2 != side_b_type2), by = c("election", side_a_type2 = "side_b_type2", side_b_type2 = "side_a_type2")) %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(idx = paste(sort(c(election, side_a_type2, side_b_type2)), collapse = ",")) %>% 
  dplyr::ungroup() %>%
  dplyr::filter(!duplicated(idx)) %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(
    n.conflicts = sum(n.conflicts.x, n.conflicts.y, na.rm = TRUE),
    n.deaths_a = sum(n.deaths_a.x, n.deaths_b.y, na.rm = TRUE),
    n.deaths_b = sum(n.deaths_b.x, n.deaths_a.y, na.rm = TRUE),
    n.deaths_civilians = sum(n.deaths_civilians.x, n.deaths_civilians.y, na.rm = TRUE),
    n.deaths_unknown = sum(n.deaths_unknown.x, n.deaths_unknown.y, na.rm = TRUE),
    n.deaths = sum(n.deaths.x, n.deaths.y, na.rm = TRUE),
    territories = sum(territories.x, territories.y, na.rm = TRUE)
  ) %>%
  dplyr::ungroup()

# Ensure the total number of conflicts matches the original data
stopifnot(sum(actor_types2_table$n.conflicts) == nrow(ged201_for_period))

# Generate all possible combinations of actor types for completeness
all_types2 <- actor_types2 %>% unlist() %>% unique %>% combn(2) %>% t %>% as.data.frame() %>% magrittr::set_colnames(c("side_a", "side_b")) %>%
  dplyr::slice(rep(1:dplyr::n(), each = 3))

# Add election periods to the combinations
all_types2 %<>% dplyr::mutate(election = actor_types2_table$election %>% unique %>% rep(nrow(all_types2) / 3))

# Join the complete combinations with the summarized conflict data
actor_types2_table <- dplyr::bind_rows(
  all_types2 %>% dplyr::right_join(actor_types2_table, by = c("side_a" = "side_a_type2", "side_b" = "side_b_type2", "election")),
  all_types2 %>% dplyr::right_join(actor_types2_table, by = c("side_a" = "side_b_type2", "side_b" = "side_a_type2", "election"))
) %>% dplyr::full_join(all_types2, by = c("side_a", "side_b", "election")) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(idx = paste(sort(c(election, side_a, side_b)), collapse = ",")) %>% 
  dplyr::ungroup() %>%
  dplyr::filter(!duplicated(idx)) %>% 
  dplyr::select(side_a, side_b, election, n.conflicts, n.deaths, n.deaths_a, n.deaths_b, n.deaths_civilians, n.deaths_unknown, territories) %>%
  dplyr::mutate(dplyr::across(tidyselect:::where(is.numeric), ~tidyr::replace_na(., 0))) %>%
  dplyr::arrange(side_a, side_b, election) %>%
  tidyr::separate(election, c("drop", "year"), sep = "_") %>% dplyr::select(-drop) %>%
  tidyr::pivot_wider(id_cols = c(side_a, side_b), names_from = "year", values_from = c("n.conflicts", "n.deaths_a", "n.deaths_b", "n.deaths_civilians", "n.deaths_unknown", "n.deaths", "territories")) %>%
  dplyr::mutate(dplyr::across(tidyselect:::where(is.numeric), ~tidyr::replace_na(., 0)))

# Calculate total conflicts and deaths across all years
actor_types2_table %<>% dplyr::rowwise() %>% dplyr::mutate(
  n.conflicts_total = sum(dplyr::c_across(dplyr::starts_with("n.conflicts_"))),
  n.deaths_a_total = sum(dplyr::c_across(dplyr::matches("n.deaths_a_\\d{4}"))),
  n.deaths_b_total = sum(dplyr::c_across(dplyr::matches("n.deaths_b_\\d{4}"))),
  n.deaths_civilians_total = sum(dplyr::c_across(dplyr::matches("n.deaths_civilians_\\d{4}"))),
  n.deaths_unknown_total = sum(dplyr::c_across(dplyr::matches("n.deaths_unknown_\\d{4}"))),
  n.deaths_total = sum(dplyr::c_across(dplyr::matches("n.deaths_\\d{4}"))),
  territories_total = sum(dplyr::c_across(dplyr::starts_with("territories_")))
) %>% dplyr::ungroup()

# Ensure the total number of conflicts matches the original data
stopifnot(sum(actor_types2_table$n.conflicts_total) == nrow(ged201_for_period))

# Reorder and select relevant columns
actor_types2_table %<>% dplyr::select(side_a, side_b, dplyr::matches("_total$"), side_b, dplyr::matches("_2006$"), side_b, dplyr::matches("_2011$"), side_b, dplyr::matches("_2018$"))

# Add a total row summing all conflicts and deaths
actor_types2_table %<>% dplyr::bind_rows(
  actor_types2_table %>% dplyr::summarise(side_a = "Total", dplyr::across(dplyr::matches(c("^n.conflicts", "^n.deaths_(total|\\d{4}$)", "^territories")), sum))
)

# Load the workbook and get the first sheet
wb <- xlsx::loadWorkbook(here::here("data/Conflict by actor prototype table.xlsx"))
sheets <- xlsx::getSheets(wb)
sheet <- sheets[[1]]

# Add the summarized data to the Excel sheet starting at row 4, column 1
xlsx::addDataFrame(as.data.frame(actor_types2_table), sheet, col.names = FALSE, row.names = FALSE, startRow = 4, startColumn = 1)

# Save the workbook
xlsx::saveWorkbook(wb, here::here("results/Conflict by actor table type 2.xlsx"))

###### Actor type 2 by territories #####

# Summarize conflicts and deaths by actor type 2, election period, and territory
actor_type_2_territories <- ged201_for_period %>% 
  dplyr::filter(!is.na(election)) %>%
  dplyr::select(election, side_a_type2, side_b_type2, n.deaths_a = deaths_a, n.deaths_b = deaths_b, n.deaths_civilians = deaths_civilians, n.deaths_unknown = deaths_unknown, n.deaths = best, index.data) %>% 
  dplyr::group_by(election, side_a_type2, side_b_type2, index.data) %>%
  dplyr::summarise(n.conflicts = dplyr::n(), dplyr::across(dplyr::starts_with("n.deaths"), ~sum(.)), .groups = "drop")

# Merge conflict data where the actor types are reversed (side_a and side_b switched) and within the same territory
actor_type_2_territories %<>% 
  dplyr::left_join(actor_type_2_territories %>% dplyr::filter(side_a_type2 != side_b_type2), by = c("election", "index.data", side_a_type2 = "side_b_type2", side_b_type2 = "side_a_type2")) %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(idx = paste(sort(c(index.data, election, side_a_type2, side_b_type2)), collapse = ",")) %>% 
  dplyr::ungroup() %>%
  dplyr::filter(!duplicated(idx)) %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(
    n.conflicts = sum(n.conflicts.x, n.conflicts.y, na.rm = TRUE),
    n.deaths_a = sum(n.deaths_a.x, n.deaths_b.y, na.rm = TRUE),
    n.deaths_b = sum(n.deaths_b.x, n.deaths_a.y, na.rm = TRUE),
    n.deaths_civilians = sum(n.deaths_civilians.x, n.deaths_civilians.y, na.rm = TRUE),
    n.deaths_unknown = sum(n.deaths_unknown.x, n.deaths_unknown.y, na.rm = TRUE),
    n.deaths = sum(n.deaths.x, n.deaths.y, na.rm = TRUE)
  ) %>% dplyr::ungroup()

# Generate all possible combinations of actor types for completeness within territories
all_types2 <- actor_types2 %>% unlist() %>% unique %>% combn(2) %>% t %>% as.data.frame() %>% magrittr::set_colnames(c("side_a", "side_b")) %>%
  dplyr::slice(rep(1:dplyr::n(), each = 3 * length(unique(actor_type_2_territories$index.data))))

# Add election periods and territories to the combinations
all_types2 %<>% dplyr::mutate(
  election = actor_type_2_territories$election %>% unique %>% rep(nrow(all_types2) / (3)),
  index.data = actor_type_2_territories$index.data %>% unique %>% rep(nrow(all_types2) / (length(unique(actor_type_2_territories$index.data))))
)

# Join the complete combinations with the summarized conflict data within territories
actor_type_2_territories <- dplyr::bind_rows(
  all_types2 %>% dplyr::right_join(actor_type_2_territories, by = c(side_a = "side_a_type2", side_b = "side_b_type2", "election", "index.data")),
  all_types2 %>% dplyr::right_join(actor_type_2_territories, by = c(side_a = "side_b_type2", side_b = "side_a_type2", "election", "index.data"))
) %>% dplyr::full_join(all_types2, by = c("side_a", "side_b", "election", "index.data")) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(idx = paste(sort(c(index.data, election, side_a, side_b)), collapse = ",")) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(!duplicated(idx)) %>%
  dplyr::select(side_a, side_b, election, index.data, n.conflicts, n.deaths, n.deaths_a, n.deaths_b, n.deaths_civilians, n.deaths_unknown) %>%
  dplyr::mutate(dplyr::across(tidyselect:::where(is.numeric), ~tidyr::replace_na(., 0))) %>%
  dplyr::arrange(side_a, side_b, election, index.data) %>% 
  tidyr::separate(election, c("drop", "year"), sep = "_") %>% dplyr::select(-drop) %>%
  dplyr::mutate(year = as.integer(year))

# This fragment ensures that each unique side pair is consistently 
# represented, sums numeric conflict data within each group, and verifies 
# that the transformation does not alter the number of rows in the dataset.


# Store the original number of rows for later validation
nrow_orig <- nrow(actor_type_2_territories)

# Sort the data by side_a and side_b
actor_type_2_territories %<>% dplyr::arrange(side_a, side_b) %>% 
  # Create a unique identifier for each side pair by sorting and concatenating side_a and side_b
  dplyr::mutate(side_pair = apply(actor_type_2_territories[c("side_a", "side_b")], 1, function(x) paste(sort(x), collapse = "-"))) %>%
  # Group by year, index.data, and the side_pair identifier
  dplyr::group_by(year, index.data, side_pair) %>%
  # Summarize the numeric columns by summing them within each group
  dplyr::summarise(dplyr::across(tidyselect:::where(is.numeric), sum), .groups = 'drop') %>%
  # Row-wise operation to split the side_pair back into side_a and side_b
  dplyr::rowwise() %>%
  dplyr::mutate(
    side_a = stringr::str_split(side_pair, "-")[[1]][1],
    side_b = stringr::str_split(side_pair, "-")[[1]][2]
  ) %>%
  # Select relevant columns and arrange by side_a, side_b, year, and index.data
  dplyr::select(side_a, side_b, year, index.data, tidyselect:::where(is.numeric)) %>% 
  dplyr::arrange(side_a, side_b, year, index.data)

# Validate that the number of rows remains the same as the original
stopifnot(nrow(actor_type_2_territories) == nrow_orig)




#### 9-NIGHTLIGHT DATA ####

##### 9.1. READ DATA #####

# Raw nightlight data obtained from Li, X., Zhou, Y., Zhao, M. et al. A harmonized 
# global nighttime light dataset 1992–2018. Sci Data 7, 168 (2020).
#  https://doi.org/10.1038/s41597-020-0510-y
# 
# Raw globe-wide nightlight data (Harmonized_DN_NTL_[year]_calDMSP.tif and 
# Harmonized_DN_NTL_[year]_simVIIRS.tif files) is not redistributed with this paper, 
# but can be downloaded from the paper above at https://doi.org/10.6084/m9.figshare.9828827.v2. 
# Then the DRC data can be extracted for the 2001-2008 period using the following code.
# 
# The DRC extracted data is provided as an RData file, which is a 175 Mb file 
# stored using Git Large File Storage on GitHub. Extra steps may be required
# for retrieval, please check this repository README file.

# Please modify the next line with your current nightlight data folder path
nightlight_data_folder <- here::here("data/nighttime_lights/9828827/")

# Check if the nightlight RData file already exists
if(!file.exists(here::here("data/nightlight.RData"))){
  # Define a temporary folder for processing files
  temp_files_folder <- here::here("data/nightlight/")
  
  # Create the temporary folder if it doesn't exist
  if(!dir.exists(temp_files_folder)){
    dir.create(temp_files_folder)
  }
  
  # List all files in the nightlight data folder matching the pattern "Harmonized*.tif"
  files <- list.files(nightlight_data_folder, "Harmonized.*\\.tif$", recursive = TRUE, full.names = TRUE)
  
  # Process each file in the list
  nightlight <- files %>% purrr::map(~{
    # Update the file to the current file being processed
    file <- .x
    nightlight <- NULL
    # Extract the year from the file name and convert it to a numeric value
    year <- stringr::str_match(basename(file), "(\\d{4})")[,2] %>% as.numeric
    # Define the output file path
    outfile <- here::here(sprintf("%snightlight_%d.RData", temp_files_folder, year))
    
    # If the output file doesn't exist, process the data
    if(!file.exists(outfile)){
      # Check if the year is between 2001 and 2018
      if(dplyr::between(year, 2001, 2018)){
        print(year)  
        # Load the raster file
        nightlight <- raster::raster(file)
        
        # Crop the raster to the Congo borders
        nightlight %<>% raster::crop(congo.territoire.borders)
        
        # Further crop the raster to each territory border and convert to points
        nightlight <- 1:nrow(congo.territoire.borders) %>% 
          purrr::map(\(i) {
            nightlight %>% raster::crop(congo.territoire.borders %>% dplyr::slice(i))
            
          }) %>% 
          
          purrr::map(\(x) { 
            x %>% raster::rasterToPoints(spatial = TRUE) %>% 
              dplyr::rename(nightlight = 1) %>% 
              dplyr::mutate(year = year)
            
          })
        
        # Convert points to sf objects
        nightlight %<>% purrr::map(sf::st_as_sf)
        
        # Intersect the sf objects with the Congo territory borders
        nightlight %<>% purrr::map2(1:length(.), ~ .x %>% sf::st_intersection(congo.territoire.borders %>% dplyr::slice(.y)))
        
        # Combine all the processed data into a single data frame
        nightlight %<>% dplyr::bind_rows()
        # Save the processed data to the output file
        save(nightlight, file = outfile)  
      }
    } else {
      # Load the existing output file if it exists
      load(outfile)
    }
    nightlight
  }) %>% purrr::compact
  
  # Save the combined nightlight data to the main RData file
  save(nightlight, file = here::here("data/nightlight.RData"))
} else {
  # Load the main RData file if it exists
  load(here::here("data/nightlight.RData"))
}


##### 9.2. MEAN #####



# This section calculates the mean nightlight values and processes the data to analyze trends. 
# Specifically, it performs the following steps:
# 
# 1. **Calculate Mean Nightlight Values**: For each geographic index and year, it calculates 
# the mean nightlight intensity, ignoring missing values. This helps in understanding the 
# average nightlight levels over time.
# 
# 2. **Threshold Adjustment for Urban Areas**: It creates a modified dataset where nightlight values less 
# than 30 are set to zero. This threshold is suggested by Li et al. (2020) to identify urban areas.
# 
# 3. **Calculate Mean for Adjusted Data**: Similar to step 1, it calculates the mean nightlight 
# values for the adjusted dataset to analyze trends in significant nightlight intensities.

# Calculate the mean nightlight values
nightlight_mean <- nightlight %>% purrr::map(\(.x){
  
  # Convert the current data frame to a standard data frame
  .x %>% as.data.frame %>% 
    # Select only the relevant columns: index.data, year, and nightlight
    dplyr::select(index.data, year, nightlight) %>% 
    # Group the data by index.data and year
    dplyr::group_by(index.data, year) %>% 
    # Calculate the mean nightlight value, removing NA values, and drop the grouping
    dplyr::summarise(nightlight_mean = mean(nightlight, na.rm = TRUE), .groups = "drop")
}) %>% dplyr::bind_rows()

# Create a new dataset where nightlight values less than 30 are set to 0
nightlight_gt30 <- nightlight %>% purrr::map(~ 
                                               .x %>% dplyr::mutate(nightlight = dplyr::case_when(nightlight < 30 ~ 0, TRUE ~ nightlight))
)
# Remove the original nightlight data to free up memory
rm(nightlight)

# Calculate the mean nightlight values for the modified dataset (values >= 30)
nightlight_gt30_mean <- nightlight_gt30 %>% purrr::map(~{
  # Convert the current data frame to a standard data frame
  .x %>% as.data.frame %>% 
    # Select only the relevant columns: index.data, year, and nightlight
    dplyr::select(index.data, year, nightlight) %>% 
    # Group the data by index.data and year
    dplyr::group_by(index.data, year) %>% 
    # Calculate the mean nightlight value, removing NA values, and drop the grouping
    dplyr::summarise(nightlight_mean = mean(nightlight, na.rm = TRUE), .groups = "drop")
}) %>% dplyr::bind_rows()

# Remove the modified nightlight data to free up memory
rm(nightlight_gt30)


##### 9.3. TRENDS #####


# This section calculates trends in nightlight data over specific periods using linear regression models. 
# It separates the data into two eras: the DMSP era (up to 2011) and the VIIRS era (from 2014 onwards). 
# For each era, it fits separate linear models to determine the trends in nightlight intensity. 
# The calculated trends are then combined into a single dataset, providing insights into changes in 
# nightlight intensity over time. Similar calculations are performed for both the original nightlight 
# data and the modified dataset where nightlight values below 30 are set to zero. 

# Calculate trends for nightlight_mean data
trends_nightlight_mean <- nightlight_mean %>% 
  # Nest data by index.data
  tidyr::nest(data = -c(index.data)) %>% 
  # Apply function to each nested data frame
  dplyr::mutate(data = purrr::map(data, ~{
    
    # Filter data for years <= 2011 (DMSP era)
    x_DMSP <- .x %>% as.data.frame() %>% dplyr::filter(year <= 2011)
    
    # Fit linear model for DMSP era with a breakpoint at 2006
    m_DMSP <- lm(nightlight_mean ~ I((year - 2006) * (year <= 2006)) + I((year - 2006) * (year >= 2006)), data = x_DMSP)
    
    # Filter data for years >= 2014 (VIIRS era)
    x_VIIRS <- .x %>% as.data.frame() %>% dplyr::filter(year >= 2014)
    
    # Fit linear model for VIIRS era
    m_VIIRS <- lm(nightlight_mean ~ year, data = x_VIIRS)
    
    # Extract coefficients and create output data frame
    .out <- coefficients(m_DMSP) %>% t %>% as.data.frame() %>% 
      magrittr::set_colnames(c("Intercept", "NL.trend.2001_2006", "NL.trend.2006_2011")) %>% 
      dplyr::select(-Intercept) %>% 
      dplyr::mutate(NL.trend.2014_2018 = coefficients(m_VIIRS)["year"])
    
    .out
    
  })) %>% tidyr::unnest(data)

# Calculate trends for nightlight_gt30_mean data
trends_nightlight_gt30_mean <- nightlight_gt30_mean %>% 
  # Nest data by index.data
  tidyr::nest(data = -c(index.data)) %>% 
  # Apply function to each nested data frame
  dplyr::mutate(data = purrr::map(data, ~{
    
    # Filter data for years <= 2011 (DMSP era)
    x_DMSP <- .x %>% as.data.frame() %>% dplyr::filter(year <= 2011)
    
    # Fit linear model for DMSP era with a breakpoint at 2006
    m_DMSP <- lm(nightlight_mean ~ I((year - 2006) * (year <= 2006)) + I((year - 2006) * (year >= 2006)), data = x_DMSP)
    
    # Filter data for years >= 2014 (VIIRS era)
    x_VIIRS <- .x %>% as.data.frame() %>% dplyr::filter(year >= 2014)
    
    # Fit linear model for VIIRS era
    m_VIIRS <- lm(nightlight_mean ~ year, data = x_VIIRS)
    
    # Extract coefficients and create output data frame
    .out <- coefficients(m_DMSP) %>% t %>% as.data.frame() %>% 
      magrittr::set_colnames(c("Intercept", "NL.trend.2001_2006", "NL.trend.2006_2011")) %>% 
      dplyr::select(-Intercept) %>% 
      dplyr::mutate(NL.trend.2014_2018 = coefficients(m_VIIRS)["year"])
    
    .out
    
  })) %>% tidyr::unnest(data)

#### 10-ACLED data ####

# This section processes the ACLED conflict event data for the Democratic Republic 
# of Congo from 2001 to 2018. 
# It performs several key tasks:
# 
# 1. **Data Loading and Filtering**: The ACLED data is loaded from a CSV file, 
# dates are converted to a standard format, and events within the specified date 
# range are retained.
# 2. **Categorizing Time Periods**: Events are categorized into three time periods: 
# 2001-2006, 2006-2011, and 2011-2018.
# 3. **Standardizing Location Names**: The admin2 column is cleaned and standardized 
# to create a consistent index for geographic locations.
# 4. **Assigning Actor Types**: Actor type data is read from an external Excel 
# file and matched with event actors to categorize them into predefined groups.
# 5. **Summarizing Events**: The data is filtered for specific event types and 
# summarized to calculate the number of deaths and conflicts for each period, location, and event type.
# 6. **Data Validation**: Checks are performed to ensure data integrity and 
# correctness after transformations.
# 7. **Exporting Data**: The processed and filtered data is exported to an Excel 
# file for further analysis and reporting.
# 
# Raleigh, C., Kishi, R. & Linke, A. Political instability patterns are obscured by conflict
# dataset scope conditions, sources, and coding choices. Humanit Soc Sci Commun 10,
# 74 (2023). https://doi.org/10.1057/s41599-023-01559-4
# 
# From https://acleddata.com/download/35181/ :
# If using ACLED data in a visual, graphic, or map of your own, please attribute the source
# data clearly and prominently on the visual itself or within the key/legend and include a link
# to ACLED’s website. This can be in small print on the bottom of the image. Please note your
# date of data access. These citations should be included for both standalone infographics as
# well as for tables/figures within a larger report. If unable to include a link on a static visual
# file, please note “acleddata.com” as the source URL. When sharing such an image on social
# media, please (1) be sure that the citation is not cut off, and (2) please tag ACLED (Twitter;
#                                                                                       Facebook; LinkedIn).

# Load ACLED data from a CSV file
ACLED_data <- data.table::fread(here::here("data/ACLED data/1900-01-01-2022-12-20-Democratic_Republic_of_Congo.csv"))

# Convert the event_date column to Date format
ACLED_data %<>% dplyr::mutate(event_date = lubridate::dmy(event_date))

# Filter the data for events between January 17, 2001, and December 30, 2018
ACLED_data %<>% dplyr::filter(event_date >= lubridate::ymd("2001-01-17") & event_date <= lubridate::ymd("2018-12-30"))

# Add a time period column based on event_date
ACLED_data %<>% dplyr::mutate(time_period = dplyr::case_when(
  dplyr::between(event_date, lubridate::ymd("2001-01-17"), lubridate::ymd("2006-07-30")) ~ "01-06",
  dplyr::between(event_date, lubridate::ymd("2006-07-31"), lubridate::ymd("2011-11-28")) ~ "06-11",
  dplyr::between(event_date, lubridate::ymd("2011-11-29"), lubridate::ymd("2018-12-30")) ~ "11-18",
  TRUE ~ NA_character_
))


# ACLED_data %$% table(time_period,useNA = "ifany")
# time_period
# 01-06 06-11 11-18 
# 1696  2579  7655 

# event_type

# ACLED_data %$% table(event_type)
# event_type
# Battles Explosions/Remote violence                   Protests 
# 4990                        108                        865 
# Riots     Strategic developments Violence against civilians 
# 714                       1269                       3984 

# sub_event_type

# ACLED_data %$% table(sub_event_type)
# sub_event_type
# Abduction/forced disappearance                           Agreement 
# 730                                 141 
# Air/drone strike                         Armed clash 
# 21                                4121 
# Arrests                              Attack 
# 60                                2847 
# Change to group/activity               Disrupted weapons use 
# 339                                  10 
# Excessive force against protesters        Government regains territory 
# 80                                 552 
# Grenade    Headquarters or base established 
# 26                                 102 
# Looting/property destruction                        Mob violence 
# 317                                 220 
# Non-state actor overtakes territory   Non-violent transfer of territory 
# 317                                 237 
# Other                    Peaceful protest 
# 63                                 590 
# Protest with intervention       Remote explosive/landmine/IED 
# 195                                  20 
# Sexual violence   Shelling/artillery/missile attack 
# 407                                  40 
# Suicide bomb               Violent demonstration 
# 1                                 494                                1                                 494 

# fatalities

# ACLED_data$fatalities %>% summary
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000    0.000    0.000    3.162    1.000 1037.000 

# Clean the admin2 column to create a standardized index
ACLED_data %<>% dplyr::mutate(index = stringr::str_to_lower(admin2)) %>%
  dplyr::mutate(index = dplyr::case_when(
    index == "bunia" ~ "irumu",
    index == "aba" ~ "faradje",
    index == "isiro" ~ "rungu",
    index == "bangu" ~ "songololo",
    index == "kalima" ~ "kailo",
    index == "kaoze" ~ "moba",
    index == "namoya" ~ "kabambare",
    index == "tshimbulu" ~ "dibaya",
    index == "makanza" ~ "mankanza",
    index == "" ~ "kinshasa",
    TRUE ~ index
  ))

# Read actor type data from an Excel file
actor_type2 <- readxl::read_excel("data/DRC armed groups in ACLED dataset.xlsx", 2) %>% dplyr::select(actor, type2)
# Create a named vector for actor types
actor_type2 <- actor_type2$type2 %>% magrittr::set_names(actor_type2$actor)

# Ensure all actor1 and actor2 values are in the actor_type2 vector
stopifnot(all(unique(ACLED_data$actor1) %in% names(actor_type2)))
stopifnot(all(unique(ACLED_data$actor2) %in% c("", names(actor_type2))))

# Add actor type columns to the data
ACLED_data %<>% dplyr::mutate(actor1_type2 = actor_type2[actor1])
ACLED_data %<>% dplyr::mutate(actor2_type2 = actor_type2[actor2])

# ACLED_data %>% dplyr::pull("event_type") %>% unique
# [1] "Riots"                      "Strategic developments"     "Protests"                  
# [4] "Violence against civilians" "Battles"                    "Explosions/Remote violence"

# Filter data for specific event types and summarize the results
ACLED_data_models <- ACLED_data %>% dplyr::filter(event_type %in% c(
  "Riots", "Violence against civilians", "Battles", "Explosions/Remote violence"
)) %>% dplyr::group_by(time_period, index, event_type) %>%
  dplyr::summarise(n.deaths = sum(fatalities, na.rm = TRUE), n.conflicts = dplyr::n(), .groups = "drop") %>%
  dplyr::mutate(year = c("01-06" = 2006, "06-11" = 2011, "11-18" = 2018)[time_period])

# Filter for specific event types and ensure the data is correct
ACLED_actor_type_2_territories <- ACLED_data %>% dplyr::filter(event_type %in% c(
  "Riots", "Violence against civilians", "Battles", "Explosions/Remote violence"
))

# Test that the filtered data contains the correct event types
stopifnot(all(unique(ACLED_actor_type_2_territories$event_type) %in% c(
  "Riots", "Violence against civilians", "Battles", "Explosions/Remote violence"
)))

# Add a new column 'year' based on 'time_period'
ACLED_actor_type_2_territories %<>% dplyr::mutate(year = c("01-06" = 2006, "06-11" = 2011, "11-18" = 2018)[time_period])

# Test that the new column 'year' has been added and has correct values
stopifnot(all(names(ACLED_actor_type_2_territories) == c(
  'data_id', 'iso', 'event_id_cnty', 'event_id_no_cnty', 'event_date', 'year', 'time_precision', 
  'event_type', 'sub_event_type', 'actor1', 'assoc_actor_1', 'inter1', 'actor2', 'assoc_actor_2', 
  'inter2', 'interaction', 'region', 'country', 'admin1', 'admin2', 'admin3', 'location', 'latitude', 
  'longitude', 'geo_precision', 'source', 'source_scale', 'notes', 'fatalities', 'timestamp', 'iso3', 
  'time_period', 'index', 'actor1_type2', 'actor2_type2'
)))
stopifnot(all(unique(ACLED_actor_type_2_territories$year) %in% c(2006, 2011, 2018)))

# Select required columns for analysis
ACLED_actor_type_2_territories %<>% dplyr::select(
  index.data = index, side_a = actor1_type2, side_b = actor2_type2, year, fatalities
)

# Test that only the required columns remain
stopifnot(all(names(ACLED_actor_type_2_territories) == c('index.data', 'side_a', 'side_b', 'year', 'fatalities')))

# Save initial total deaths and count before group_by and summarise
initial_total_deaths <- sum(ACLED_actor_type_2_territories$fatalities, na.rm = TRUE)
initial_total_count <- nrow(ACLED_actor_type_2_territories)

# Group by side_a, side_b, year, and index.data, then summarise the data
ACLED_actor_type_2_territories %<>% dplyr::group_by(side_a, side_b, year, index.data) %>% 
  dplyr::summarise(n.deaths = sum(fatalities, na.rm = TRUE), n.conflicts = dplyr::n(), .groups = "drop")

# Check that total deaths and count are still the same after group_by and summarise
final_total_deaths <- sum(ACLED_actor_type_2_territories$n.deaths)
final_total_count <- sum(ACLED_actor_type_2_territories$n.conflicts)

stopifnot(initial_total_deaths == final_total_deaths)
stopifnot(initial_total_count == final_total_count)

# Test that the data is grouped correctly and summarisation has been done
stopifnot(length(unique(paste(
  ACLED_actor_type_2_territories$side_a, 
  ACLED_actor_type_2_territories$side_b, 
  ACLED_actor_type_2_territories$year, 
  ACLED_actor_type_2_territories$index.data
))) == nrow(ACLED_actor_type_2_territories))

# Export detailed ACLED data to an Excel file
ACLED_data %>% dplyr::filter(event_type %in% c(
  "Riots", "Violence against civilians", "Battles", "Explosions/Remote violence"
) & !is.na(actor2)) %>%
  dplyr::mutate(election = c("01-06" = 2006, "06-11" = 2011, "11-18" = 2018)[time_period], dyad = paste0(actor1_type2, "_", actor2_type2)) %>%
  dplyr::select(territory = index, dyad, actor1, actor2, election, notes, fatalities, event_date) %>%
  dplyr::arrange(territory, dyad, actor1, actor2, event_date) %>% writexl::write_xlsx("results/ACLED_detailed.xlsx")

#### 11-MERGE VILLES INTO THEIR TERRITORIES ####

# This section identifies and merges smaller administrative regions (villes) into 
# their larger surrounding territories for more accurate data analysis. The process involves:
# 
# 1. **Identifying Intersections**: Checking which villes should be merged by 
# analyzing geographic intersections of territory borders.
# 2. **Creating Plots for Manual Checking**: Generating plots of intersection 
# geometries for manual verification.
# 3. **Defining Merges**: Specifying which villes should be merged into which territories.
# 4. **Updating Labels**: Adjusting the labels for the villes being merged.
# 5. **Merging Data**: Using a custom function to merge the data of specified villes 
# into their corresponding territories.
# 6. **Summarizing Results**: Aggregating various metrics such as voter counts 
# and election results for the merged territories, and calculating additional 
# percentages and turnout rates.

# Check which villes should be merged into their surrounding territories. Uncomment and run once

# Calculate intersections of Congo territory borders and filter those with exactly 2 overlaps
# territory_intersections <- sf::st_intersection(congo.territoire.borders) %>% dplyr::filter(n.overlaps == 2)

# Filter out invalid geometries from the intersections
# territory_intersections %<>% dplyr::filter(sf::st_is_valid(territory_intersections))

# Extract geometries of the intersections
# intersections_geometries <- territory_intersections %>% dplyr::pull("origins") %>% purrr::map(~{
#   congo.territoire.borders %>% dplyr::slice(.x)
# })

# Create directory for saving intersection plots if it doesn't exist
# if (!dir.exists(here::here("manuscript/intersections/"))) dir.create(here::here("manuscript/intersections/"))

# Plot and save each intersection geometry for manual checking
# intersections_geometries %>% purrr::walk2(1:length(.), ~{
#   ggplot2::ggplot(.x) + ggplot2::geom_sf()
#   ggplot2::ggsave(here::here(paste0("manuscript/intersections/", .y, ".png")))
# })

# Define villes that should be merged into their surrounding territories
villes_to_merge <- c(
  "beni ville" = "beni",
  "kikwit" = "bulungu",
  "kindu" = "kailo",
  "likasi" = "kambove",
  "mbuji-mayi" = "lupatapata",
  "kolwezi" = "mutshatsha",
  "mwene-ditu" = "luilu",
  "tshela" = "luozi"
)

# Create a vector of labels for the villes to be merged
villes_labels <- data %>% dplyr::filter(index %in% c(names(villes_to_merge), villes_to_merge)) %>%
  dplyr::mutate(out = label %>% magrittr::set_names(index)) %>% dplyr::pull("out")

# Update labels for villes to be merged
villes_labels[names(villes_to_merge)] <- villes_labels[villes_to_merge]

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

# Apply the merge_villes function to the data and summarize the results
data_villes_merged <- data %>% merge_villes("label") %>% dplyr::group_by(index) %>% dplyr::summarise(
  label = unique(label),
  province = unique(province),
  dplyr::across(dplyr::starts_with("registered.voters_"), sum),
  dplyr::across(dplyr::starts_with("voters_"), sum),
  dplyr::across(dplyr::starts_with("total.votes_"), sum),
  dplyr::across(dplyr::starts_with("kabila.votes_"), sum),
  dplyr::across(dplyr::starts_with("ramazani.votes_"), sum),
  dplyr::across(dplyr::starts_with("ballot.boxes_counted_"), sum),
  dplyr::across(dplyr::starts_with("n.voting.sites_"), sum),
  dplyr::across(dplyr::starts_with("zero.voters.sites_"), sum),
  dplyr::across(dplyr::starts_with("fayulu.votes_"), sum),
  dplyr::across(dplyr::starts_with("tshisekedi.votes_"), sum),
  .groups = "drop"
) %>% dplyr::mutate(
  kabila.percent_2006 = kabila.votes_2006 / total.votes_2006,
  kabila.percent_2011 = kabila.votes_2011 / total.votes_2011,
  ramazani.percent_2018 = ramazani.votes_2018 / total.votes_2018,
  fayulu.percent_2018 = fayulu.votes_2018 / total.votes_2018,
  tshisekedi.percent_2018 = tshisekedi.votes_2018 / total.votes_2018,
  turnout_2006 = total.votes_2006 / registered.voters_2006,
  turnout_2011 = total.votes_2011 / registered.voters_2011,
  turnout_2018 = total.votes_2018 / registered.voters_2018
)

rm(merge_villes, villes_to_merge, villes_labels)

#### 12-DISTANCES ####

##### 12.1-DISTANCES TO RWANDA AND UGANDA BORDERS #####

# This section involves extracting and processing geographical data to analyze the 
# borders of the Democratic Republic of the Congo (DRC) and its neighboring countries. 
# The data is obtained from a shapefile sourced from the Humanitarian Data Exchange 
# (https://data.humdata.org/dataset/limites-de-la-rdc-et-de-20-pays-avoisinants-drc-and-neighbouring-countries), 
# which includes boundaries for 20 countries based on OpenStreetMap. The section 
# focuses on filtering and isolating the borders for Uganda, Rwanda, and the DRC. 
# To ensure accurate spatial operations, a small buffer is applied to these borders. 
# The intersections between the buffered borders of the DRC with Rwanda and Uganda 
# are calculated. Additionally, the script computes the centroids of various 
# territories within the DRC and measures the minimum distances from these centroids 
# to the identified borders with Rwanda and Uganda.

###### Get borders geometry ######

# Read the shapefile containing borders information for the Democratic Republic of the Congo and neighboring countries
borders <- sf::st_read("data/borders/OSM_RDC_PaysVoisins_211012.shp")

# Filter the borders data to get the geometry for Uganda
uga_border <- borders %>% 
  dplyr::filter(name_en == "Uganda") %>% 
  sf::st_geometry()

# Filter the borders data to get the geometry for Rwanda
rwa_border <- borders %>% 
  dplyr::filter(name_en == "Rwanda") %>% 
  sf::st_geometry()

# Filter the borders data to get the geometry for the Democratic Republic of the Congo
drc_border <- borders %>% 
  dplyr::filter(name_en == "Democratic Republic of the Congo") %>% 
  sf::st_geometry()

# Define a small buffer size to adjust border precision
buffer_size <- 0.00001  

# Apply the buffer to the Democratic Republic of the Congo borders to ensure proper spatial operations
buffered_drc <- sf::st_buffer(drc_border, dist = buffer_size)

# Apply the buffer to the Rwanda borders to ensure proper spatial operations
buffered_rwa <- sf::st_buffer(rwa_border, dist = buffer_size)

# Apply the buffer to the Uganda borders to ensure proper spatial operations
buffered_uga <- sf::st_buffer(uga_border, dist = buffer_size)

# Calculate the intersection of the buffered borders between the Democratic Republic of the Congo and Rwanda
drc_rwa_border <- sf::st_intersection(buffered_drc, buffered_rwa) 

# Calculate the intersection of the buffered borders between the Democratic Republic of the Congo and Uganda
drc_uga_border <- sf::st_intersection(buffered_drc, buffered_uga) 

# Ensure the resulting geometries are valid
drc_rwa_border <- sf::st_make_valid(drc_rwa_border)
drc_uga_border <- sf::st_make_valid(drc_uga_border)

###### Compute distances in m from territories to each border ######

# Calculate the centroid of each territory
territories_centroids <- congo.territoire.borders %>%
  sf::st_centroid()

# Calculate the minimum distances from each centroid to the Rwanda border
distance_to_rwa_border <- apply(
  sf::st_distance(territories_centroids$geometry, drc_rwa_border),
  1,
  min
)

# Calculate the minimum distances from each centroid to the Uganda border
distance_to_uga_border <- apply(
  sf::st_distance(territories_centroids$geometry, drc_uga_border),
  1,
  min
)

# Add the distance calculations to the original data frame of territories
congo.territoire.borders %<>%
  dplyr::mutate(
    distance_to_rwa_border = distance_to_rwa_border,
    distance_to_uga_border = distance_to_uga_border
  )

rm(drc_rwa_border, drc_uga_border, borders, uga_border, rwa_border, drc_border, distance_to_rwa_border, distance_to_uga_border, buffered_uga, buffered_drc, buffered_rwa)


##### 12.3-COMPUTE Distance_Bemba_home #####

# Distance in m from each territory centroid to the centroid of Mbandaka ville

mbandaka_centroid <- territories_centroids %>% dplyr::filter(index.data == "mbandaka")

congo.territoire.borders %<>% dplyr::mutate(Distance_Bemba_home =  sf::st_distance(territories_centroids$geometry,mbandaka_centroid))

rm(mbandaka_centroid)  

##### 12.3-COMPUTE Distance_Bemba_home #####

# Distance in m from each territory centroid to the centroid of Mbandaka ville

mbandaka_centroid <- territories_centroids %>% dplyr::filter(index.data == "mbandaka")

congo.territoire.borders %<>% dplyr::mutate(Distance_Bemba_home =  sf::st_distance(territories_centroids$geometry,mbandaka_centroid))

rm(mbandaka_centroid)
                
#### 13-SAVE DATA ####


save(data,kinshasa.subprov,congo.territoire.borders,ged201,data.map.index,conflict.aggregated,conflict.aggregated_by_type,nightlight_mean,nightlight_gt30_mean,trends_nightlight_mean,trends_nightlight_gt30_mean,actor_types,actor_types1_table,actor_types2_table,actor_type_2_territories, ACLED_data, ACLED_data_models, ACLED_actor_type_2_territories,data_villes_merged,file=here::here("results/data.RData"))

