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

##### 2-RENAME COLUMNS AND NEST VOTES FOR EACH CANDIDATE #####

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

##### 3-NEST DATA BY LOCATION AND STANDARDIZE INDEXES #####

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


##### 4-MATCH DATA ACROSS ELECTIONS #####

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

##### 5-MERGE DATA ACROSS ELECTIONS #####

##### 5-MERGE DATA ACROSS ELECTIONS #####

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

