## Large File Storage Support

This project uses git large file storage to store the nightlight data. Please, make sure that support for git large file storage is installed before cloning it. Please check for more information at https://docs.github.com/en/repositories/working-with-files/managing-large-files

## Preprocessing

### 1-READ DATA

 This section processes and combines election data from different years to prepare 
 it for further analysis. Specifically, it handles data from the years 2006, 2011, 
 and 2018, sourced from multiple Excel files.
 
 2006 Data: The script reads two sets of data for 2006. The first dataset is filtered 
 to remove rows representing provincial totals and select columns are excluded. 
 The second dataset, labeled as second round, undergoes a similar cleaning process. 
 These datasets are then merged based on a common column related to geographical divisions.
 2011 Data: Data from the 2011 elections is read and rows containing aggregated 
 totals are excluded to ensure the dataset only contains individual record entries.
 2018 Data: The script first reads the primary dataset for 2018. It then reads a 
 separate file containing candidate names and IDs, which are necessary to match 
 candidate details correctly across different datasets. Due to discrepancies in 
 candidate IDs between different sources, the script includes a manual mapping of IDs 
 to align them correctly based on a comparison with the original data source (a PDF file). 
 Once the IDs are aligned, the candidate details are merged into the main 2018 dataset.

### 2-RENAME COLUMNS AND NEST VOTES FOR EACH CANDIDATE

 This section processes election data from 2006, 2011, and 2018 to structure and 
 organize key information regarding electoral votes and participants. Specifically, 
 it performs the following operations:
   
 1. For the year 2006, it restructures the data to list each candidate along with 
 the percentage and calculated number of votes they received. This transformation 
 involves pivoting the dataset so that candidate names and their corresponding 
 vote percentages are collated into a nested data frame, which includes both the 
 candidate names and the calculated votes based on valid votes and participation percentages.
 
 2. For the years 2011 and 2018, the script formats the data by nesting details 
 about each candidate’s votes into a similar structured format. This includes 
 renaming certain columns for consistency and clarity, such as the candidate’s 
 name and the number of votes they received.
 
 3. Additionally, the script standardizes other important electoral information 
 across the datasets for these years, such as the number of registered voters, 
 actual voters, ballot boxes, and the count of processed ballot boxes. This renaming 
 ensures uniformity across different election years for easier comparison and analysis.
 
 Overall, this section enhances the accessibility and usability of election data 
 by organizing it into a consistent format across different election years, 
 allowing for streamlined analysis and reporting.
 
### 3-NEST DATA BY LOCATION AND STANDARDIZE INDEXES

 This section focuses on organizing and indexing election data from 2006, 2011, 
 and 2018 by geographic and administrative divisions such as provinces, cities, 
 and electoral constituencies. It aims to facilitate easier data comparison and 
 analysis across different election years:
   
   1. **Nesting and Index Creation**: The script nests voting site data within 
   each constituency for the 2018 dataset. For all years, it establishes indexes 
   based on geographic identifiers like city names and constituency names, which 
   are standardized to lower case and stripped of extra spaces for uniformity.
 
 2. **Labeling and Manual Adjustments**: The script also assigns labels to each 
 entry for clearer identification and resolves inconsistencies in geographic names 
 between datasets. In Kinshasa, for example, it adjusts city names to subprovince 
 levels to match data granularity in other years.
 
 3. **Data Matching and Structuring**: Efforts are made to align the datasets by 
 manually correcting discrepancies in geographic names across the election years. 
 This includes both simplifying and matching names and ensuring that the names 
 used reflect the administrative changes or differences noted in different datasets.
 
 4. **Nested Structuring**: Finally, the data is restructured into nested formats 
 based on updated indexes and labels. This allows for detailed yet manageable 
 data subsets, which can be used for in-depth regional analysis or aggregated to 
 provide broader electoral insights.
 
 This meticulous organization of data by location enhances the analytical framework, 
 making it easier to track electoral trends and patterns across different regions 
 and election cycles.
 
### 4-MATCH DATA ACROSS ELECTIONS

 This section is dedicated to matching election data across three different election 
 years: 2006, 2011, and 2018. The primary challenge addressed here is the discrepancies 
 in geographical names across different datasets, particularly because some regions 
 like Kinshasa lack detailed geographic identifiers in some years. To tackle this, 
 a multi-step matching strategy is implemented:
 
 1. **Initial Extraction and Naming**: The script extracts names of cities or 
 constituencies (referred to as villes/circonscriptions) from each year's dataset 
 and assigns them as names to character vectors. This unique approach uses the 
 names of these vectors for matching, ensuring that the actual content remains unchanged.
 
 2. **Iterative Matching Process**: The matching process involves several iterative 
 steps where the names in the vectors are slightly modified in each iteration to 
 accommodate differences in naming conventions between datasets. For example, 
 the term "ville" is removed and excess whitespace is cleaned to improve matching accuracy.
 
 3. **Compilation of Matched and Unmatched Names**: After each matching attempt, 
 matched names are compiled, and unmatched names undergo further processing to 
 refine their format and attempt another match. This stepwise refinement continues 
 until no further matches can be found.
 
 4. **Final Data Structuring**: The results are then structured into a comprehensive 
 list that captures both matched and unmatched names, ensuring that data from different 
 years can be compared accurately despite initial discrepancies.
 
 The process is meticulous and aims to ensure that electoral data from different 
 years can be aligned and analyzed consistently, addressing challenges posed by 
 changes in geographic names and administrative boundaries over time.

There are some missing Circonscriptions in 2011. At Kinshasa, because we don't have enough detail in 2011 data, 
we are limited to use the subprovince level.
The strategy followed is to extract the names of the villes/circonscriptions into a named character vector. Matching is done
in several steps because there are differences across elections in the way the villes are named.
First, the names of the vector are set equal to the vector contents. 
We will use the names and not the contents to match the villes across elections. 
In each step trying to match the data, we modify the names of the vector, leaving the vector contents untouched.
That way, in the end, we have an index of villes matches across elections, even if the actual names are different.


### 5-MERGE DATA ACROSS ELECTIONS

 This section consolidates election data from different years (2006, 2011, and 2018) 
 using previously created matching indexes to ensure consistency and continuity 
 across datasets. The primary activities in this process include:

 1. **Data Preparation**: Indexes that have been aligned in previous steps are 
 used to merge data across the election years. This ensures that each entry from 
 different years corresponds to the same geographic location or administrative 
 division, even when direct matches in names are not apparent.

 2. **Merging Process**: The script performs several full joins to combine the 
 datasets from 2006, 2011, and 2018 based on these indexes. This method allows 
 for the inclusion of all available data, whether or not a direct match exists 
 in all three years, thus preserving the maximum possible data granularity.

 3. **Final Structuring and Cleaning**: Once merged, the data is restructured to 
 create a unified view that includes labels and province names standardized across 
 years. This structuring is crucial for analyses that require consistent geographic 
 identifiers across multiple election cycles.

 4. **Variable Cleanup and Data Saving**: Unnecessary variables are removed to 
 tidy up the workspace, and the final structured dataset is arranged and saved 
 for further analysis or reporting.

 By integrating data from multiple election years, this section facilitates 
 comprehensive longitudinal electoral analyses, helping to identify trends and 
 changes over time within the same geographic locales.
 
### 6-DERIVED QUANTITIES

#### 6.1-REGISTERED VOTERS IN EACH LEVEL


 This section focuses on calculating the number of registered voters at various 
 administrative levels for each election year. The calculations are performed through 
 a series of nested data manipulations, leveraging the flexibility of functional 
 programming within R:

 1. **Data Transformation**: For each election dataset, the script navigates through 
 multiple nested structures—ranging from regions down to individual voting sites—to 
 calculate the number of registered voters based on the available data, such as 
 valid votes and participation percentages.

 2. **Voter Estimation**: Using the data on valid votes and the percentage of 
 participation, the script estimates the total number of registered voters at 
 different levels (e.g., circonscription, ville territoire) by reversing the 
 calculation of valid votes from participation rates.

 3. **Aggregation**: After calculating the registered voters at the lowest levels, 
 these figures are summed up through the nested structures to provide total counts 
 at higher administrative levels, ensuring that each region’s total reflects all 
 underlying data.

 4. **Data Integration**: The computed totals of registered voters for each level 
 and year are then integrated back into the main dataset, providing enriched data 
 points that support more detailed electoral analysis.

 By systematically estimating and aggregating registered voter counts across 
 different levels, this section enhances the dataset's value for analyzing voter 
 turnout and electoral engagement across regions and election cycles.
 
#### 6.2-VOTERS IN EACH LEVEL

 This section is devoted to calculating the total number of voters 
 from election data spanning three different years: 2006, 2011, and 2018. Here's 
 an overview of how the data is processed:

 1. **Data Transformation**: For each election year, the data undergoes a series 
 of transformations to calculate the total voters within each administrative level, 
 such as circonscriptions and voting sites.

 2. **Nested Calculations**: Utilizing the `purrr::map` function, the script navigates 
 through nested structures (circonscription to voting sites) to aggregate 
 voters at various levels.

 3. **Summation and Aggregation**: After extracting the voters for smaller 
 units, the script sums these voters to compute total figures for larger geographic 
 or administrative areas. This aggregation helps in understanding the total voter 
 turnout and the distribution across different regions.

 4. **Data Integration and Unnesting**: The sums of voters are then integrated 
 back into the main dataset, ensuring that each administrative unit's total voters 
 are reflected in the final dataset. The use of `tidyr::unnest` helps in flattening 
 nested lists into simpler dataframe structures.

#### 6.3-TOTAL VOTES IN EACH LEVEL


 This section of the script is dedicated to calculating the total number of valid votes 
 (referred to as "total votes") across different administrative levels for the 
 election years 2006, 2011, and 2018. Here’s a concise breakdown of the process:

 1. **Data Preparation**: For each year, the data is initially checked for null 
 values to ensure only valid data is processed. This step is crucial for maintaining 
 data integrity.

 2. **Vote Calculation**:
    - For 2006, the script directly assigns the number of valid votes ('Votes valables') 
    from the data to a new column called 'total.votes' for each circonscription. 
    It then sums these votes to get a total count for each higher administrative level.
    - In 2011, the process involves extracting and summing votes from nested data 
    structures within each circonscription. This is slightly more complex as it 
    involves pulling and summing nested vote counts.
    - For 2018, the calculation is even more granular, extending down to voting 
    sites within each ville and territoire. This involves multiple layers of 
    mapping and summing to aggregate votes all the way up from the most detailed levels.

 3. **Data Aggregation**: After calculating the total votes at the lowest necessary 
 levels, the script aggregates these totals to provide comprehensive vote counts 
 for larger geographic or administrative areas.

 4. **Integration and Unnesting**: The aggregated total votes are then integrated 
 back into the main dataset. The use of `tidyr::unnest()` simplifies the data 
 structure, making it more accessible for further analysis.
 
 
#### 6.4-VOTES AND PERCENTAGE FOR KABILA IN EACH LEVEL

 This section of the script is dedicated to calculating the total votes and the 
 percentage of votes received by the candidate Kabila (and Ramazani in 2018) at 
 various administrative levels across the election years 2006, 2011, and 2018. 
 The method involves several steps:
 
 1. **Data Filtering and Extraction**:
   - For each year, the data undergoes a check to ensure it is not null, which 
   is crucial for maintaining accuracy.
 - The votes specifically for Kabila (or Ramazani in 2018) are extracted from 
 nested data structures by filtering for the candidate's name within each voting 
 site or circonscription.
 
 2. **Vote Calculation**:
    - The valid votes for Kabila are aggregated first within the smallest units 
    (voting sites or circonscriptions) and then summed up to provide totals for 
    larger administrative areas.
    - This process involves mapping through nested data structures and applying 
    filters and summation functions to accurately compile the votes.
 
 3. **Percentage Calculation**:
    - The percentage of votes received by Kabila or Ramazani is calculated by 
    dividing their total votes by the total votes of all candidates at each 
    administrative level. This step provides insight into the candidate's 
    performance relative to others.
 - The calculation is done post-aggregation to ensure it reflects the comprehensive 
 vote share.
 
 4. **Data Aggregation and Unnesting**:
   - After calculating the total votes and percentages, these metrics are nested 
   back into the main dataset. Using `tidyr::unnest()`, the nested lists are 
   simplified into standard columns for ease of analysis.
 
 5. **Result Integration**:
   - The final step involves integrating the calculated votes and percentages 
   into the main dataset, enhancing the dataset with key electoral metrics for 
   Kabila and Ramazani, which are pivotal for electoral analysis.
 
#### 6.5-BALLOT BOXES IN EACH LEVEL (2011,2018 ONLY)

 This section of the script processes and calculates the total number of ballot 
 boxes counted at various administrative levels during the 2011 and 2018 elections. 
 The approach involves multiple steps to ensure accurate aggregation of data:
 
 1. **Data Validation**: Each dataset is checked to confirm that it is not null, 
 ensuring that only valid data is processed. This step is crucial to avoid 
 errors during data manipulation.
 
 2. **Mapping and Summation**:
   - For each entry in the dataset, the script uses `purrr::map()` to apply 
   functions at different nesting levels, calculating the sum of ballot boxes 
   counted within each administrative unit, such as circonscriptions or voting sites.
 - This involves iterating over each circonscription and, for the 2018 data, 
 each ville.territoire within the circonscriptions, to aggregate the counts 
 from the smallest units upwards.
 
 3. **Data Aggregation**:
   - The aggregated counts of ballot boxes from the smaller units (voting sites 
   or circonscriptions) are further summed to provide totals for larger areas.
 - This is performed using nested `purrr::map()` functions that apply the summation 
 across the nested data structures.
 
 4. **Flattening and Integration**:
   - The nested results are flattened using `tidyr::unnest()` to integrate the 
   summed ballot box counts back into the main dataframe. This transformation 
   simplifies the nested list into a standard column format within the main dataset.
 
 5. **Final Aggregation and Calculation**:
   - After processing individual entries, a final summation is performed across 
   all entries for each election year to calculate the total number of ballot 
   boxes counted across all circonscriptions.
 - The results are added to the dataframe, providing a clear overview of ballot 
 box distribution and availability during the elections.
 
#### 6.6-VOTING SITES WITH ZERO VOTERS (2018 ONLY)

 This section identifies and quantifies voting sites with zero voters from the 
 2018 election data. It performs the following operations:
   
 1. **Filtering Voting Sites**: The script navigates through multiple nested 
 data structures, filtering out voting sites where the number of voters is not 
 available or is zero.
 2. **Counting Voting Sites**: For each administrative level (e.g., circonscription, 
 ville.territoire), it counts the number of voting sites that reported zero voters.
 3. **Aggregating Data**: These counts are aggregated at higher administrative 
 levels, providing a comprehensive total of voting sites with zero voters for each level.
 4. **Data Integration**: The results are integrated back into the main dataset, 
 adding the total counts of zero-voter sites for further analysis and reporting.
 
#### 6.7-VOTES AND PERCENTAGES FOR OTHER CANDIDATES IN 2018

 This section calculates the total votes and percentages for the candidates 
 Fayulu and Tshisekedi in the 2018 election. The script performs the following 
 tasks:
   
   1. **Fayulu's Votes and Percentages**:
    - For each administrative level (circonscription, ville.territoire, voting sites), 
    the script identifies and sums the votes cast for Fayulu.
    - It then calculates the percentage of votes Fayulu received out of the total 
    votes at each level.
    - These results are aggregated and integrated back into the main dataset, 
    providing Fayulu's total votes and percentage for 2018.
 
 2. **Tshisekedi's Votes and Percentages**:
    - Similar to the process for Fayulu, the script identifies and sums the 
    votes cast for Tshisekedi at each administrative level.
    - It calculates Tshisekedi's percentage of total votes at each level.
 - The aggregated results are then integrated back into the main dataset, 
 detailing Tshisekedi's total votes and percentage for 2018.
 
#### 6.8-TURNOUT

 This section calculates voter turnout rates for the election years 2006, 2011, 
 and 2018. The script performs the following tasks:
   
 1. **Calculate Turnout Rates**: It computes the turnout rate for each year by 
 dividing the number of voters by the number of registered voters for 2006, 
 2011, and 2018.
 2. **Generate Summary Statistics**: Although commented out, the script includes 
 code to generate summary statistics for the calculated turnout rates, providing 
 insights into the distribution of turnout across the datasets.
 
 
### 7-HARMONIZE MAP AND DATA LOCATIONS

 This section aligns geographic data with the electoral dataset to ensure consistency 
 in analysis. It performs the following tasks:
   
   1. **Read Detailed Shapefiles**: Loads detailed shapefiles for Congo's 
   territories to get precise geographic boundaries. [ref shapefiles](data/Les territoires de Congo (rerritories of Congo)/CONTENIDOS.txt)
 2. **Filter and Standardize Kinshasa Regions**: Filters the shapefile data to 
 include only Kinshasa regions and standardizes names by replacing hyphens 
 with spaces.
 3. **Update and Summarize Geometry Data**: Updates the Kinshasa shapefile data 
 with standardized names, selects relevant columns, groups by names, and 
 summarizes geometries.
 4. **Read Main Shapefiles**: Loads the main shapefile for Congo's territories 
 and creates an index by standardizing territory names.
 5. **Exclude Kinshasa**: Excludes Kinshasa from the main shapefile data.
 6. **Create Indices for Matching**: Extracts and standardizes names from both 
 the map and data for matching purposes.
 7. **Manual Name Matching**: Applies manual name corrections for regions and 
 cities to ensure consistency between map and data.
 8. **Match Indices**: Matches data names with map names and identifies 
 unmatched names.
 9. **Create Index DataFrame**: Creates a DataFrame to link data and map 
 indices, ensuring unique matches.
 10. **Join and Summarize Map Data**: Joins the map data with matched indices, 
 updates index names, and groups by index to summarize geometries.
 11. **Transform Projections**: Ensures the geographic projections of Kinshasa 
 borders match the main dataset.
 12. **Combine Borders**: Combines Kinshasa borders with the main territory 
 borders.
 13. **Clean Environment**: Removes unnecessary variables, keeping only the 
 relevant ones for further analysis.
 
 This section harmonizes geographic and electoral data, facilitating accurate 
 mapping and spatial analysis of election results.