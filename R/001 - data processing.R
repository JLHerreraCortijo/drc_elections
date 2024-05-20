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

# Arrange the data and save

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
# available or is zero.
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
  # Calculate the turnout rate for 2006 by dividing the number of voters by the number of registered voters
  turnout_2006 = voters_2006 / registered.voters_2006,
  # Calculate the turnout rate for 2011 by dividing the number of voters by the number of registered voters
  turnout_2011 = voters_2011 / registered.voters_2011,
  # Calculate the turnout rate for 2018 by dividing the number of voters by the number of registered voters
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

# The following commented-out code generates a detailed summary using the dfSummary function from the summarytools package
# Uncomment this line to see a detailed summary of the turnout rates
# data %>% dplyr::select(starts_with("turnout_")) %>% summarytools::dfSummary() %>% summarytools::stview()




