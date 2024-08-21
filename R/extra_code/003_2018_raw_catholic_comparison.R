rm(list=ls())

###############################################################################
#' Name: 003_2018_raw_catholic_comparison.R
#' Author: John Quattrochi (john.quattrochi@gmail.com)
#' Assistant: Juan Luis Herrera Cortijo (juan.luis.herrera.cortijo@gmail.com)
#' Purpose: Produces a table comparing 2018 raw and catholic data
#' The script assumes the following folder structure:
#' Scripts are stored in "[project folder]/R"
#' Data are stored in "[project folder]/data"
#' Results are stored in "[project folder]/results"
#' Tables are stored in "[project folder]/tables"
###############################################################################


# R version and load packages and install if necessary

# version
# 
# _                           
# platform       x86_64-apple-darwin17.0     
# arch           x86_64                      
# os             darwin17.0                  
# system         x86_64, darwin17.0          
# status                                     
# major          4                           
# minor          0.2                         
# year           2020                        
# month          06                          
# day            22                          
# svn rev        78730                       
# language       R                           
# version.string R version 4.0.2 (2020-06-22)
# nickname       Taking Off Again   

if(!require(tabulizer)){
  install.packages("tabulizer",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(tabulizer)
packageVersion("tabulizer")
# 0.2.2

if(!require(magrittr)){
  install.packages("magrittr",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(magrittr)
packageVersion("magrittr")
# 1.5


if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(tidyverse)
packageVersion("tidyverse")
#── Attaching packages ────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──
# ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
# ✓ tibble  3.0.4     ✓ dplyr   1.0.2
# ✓ tidyr   1.1.2     ✓ stringr 1.4.0
# ✓ readr   1.4.0     ✓ forcats 0.5.0

##### FUNCTIONS #####
source("R/function.library.R")

##### EXECUTED STATEMENTS

load("results/data.RData")

# Read the Catholic report from the PDF

file <- "data/Annexe_1_Resulat-42Pourcent-1.pdf"


# Authomatic extraction does not fully work on page 2. We need to define the table areas
# and then do some postprocessing.

# page 2 area
# p2.area <- locate_areas(file,pages = 2)

p2.area <- list(c( 66.59104,  46.92537, 303.00896, 557.19403 ))
page2.1 <- extract_tables(file, pages=rep(2,length(p2.area)),area=p2.area)


p2.area <- list(c(433.03881,  46.92537, 744.32239, 553.25373 ))
page2.2 <- extract_tables(file, pages=rep(2,length(p2.area)),area=p2.area)

page2 <- list(rbind(page2.1[[1]][-nrow(page2.1[[1]]),],
                    page2.2[[1]][-1,]))

# Extract the remaining tables and put all together.

catholic.tables <- extract_tables(file)

catholic.tables <- c(catholic.tables[1],page2,catholic.tables[2:length(catholic.tables)])


# Get the tables header and drop it from the first page
table.names <- catholic.tables[[1]][1,]

catholic.tables[[1]] <- catholic.tables[[1]][-1,]

# Separate grand total and final percentage table from the last table in page 7
catholic.tables[[8]] <-catholic.tables[[7]][nrow(catholic.tables[[7]])-1,]
catholic.tables[[9]] <-catholic.tables[[7]][nrow(catholic.tables[[7]]),]
catholic.tables[[7]] <- catholic.tables[[7]][1:(nrow(catholic.tables[[7]])-2),]


# All the tables in one page are together, we need to split them
pages <- c(catholic.tables[1:7] %>% map(~{
  page <- .x %>% as.data.frame()
  
  # Find the beginning an the end of each table
  inits <- page[,2] %>% str_squish() %>% equals("") %>% which %>% c(1) %>% unique %>% sort
  ends <- inits[-1] %>% add(-1) %>% c(nrow(page))
  
  # Extract the tables
  tables <- map2(inits,ends,~page %>% slice(.x:.y))
  
  # Get the table names
  provinces <- tables %>% map(~.x[1,1]) %>% unlist
  
  
  tables %<>% map(~.x %>% set_colnames(table.names) %>% # Set table header
                    mutate(`Row Labels`=str_to_title(`Row Labels`), # Format the locations strings
                           across(-1,~str_remove(.," "))) ) # Remove spaces in the numbers
  
  tables %<>% set_names(provinces)
  
}),

# Format the grand total and percentage tables as above
catholic.tables[8:9] %>% map(~{
  tables <- .x %>% t %>% as.data.frame()
  
  
  provinces <- tables[1,1]
  
  tables %<>% set_colnames(table.names) %>% mutate(`Row Labels`=str_to_title(`Row Labels`),across(-1,~str_remove(.," "))) %>% list
  
  tables %<>% set_names(provinces)
  
})

)


# Some tables are split between pages, check when that happens and merge if necessary.
# Return a list of tables.
catholic.provinces <- pages[-1] %>% reduce(.init=pages[[1]],~{
  
  last.table <- last(.x)
  
  
  if(!str_detect(last.table[nrow(last.table),1],"Total")){
    .x[[length(.x)]] <- bind_rows(last.table,
                                  first(.y))
    .y <- .y[-1]
  }
  
  c(.x,.y)
  
})


# Check that all the figures are correct.
catholic.provinces[-c((length(catholic.provinces)-1):length(catholic.provinces))] %>% walk2(names(.),~{
  print(.y)
  
  totals <- .x %>% slice(nrow(.)) %>% select(-1) %>% mutate(across(everything(),as.numeric))
  
  sums <- .x %>% slice(-nrow(.)) %>% select(-1) %>% mutate(across(everything(),as.numeric)) %>% summarise(across(everything(),~sum(.,na.rm = TRUE)))
  test <- all(totals==sums)
  
  stopifnot(test)
  
})

# We need to match the catholic data and our data. Let's do that

catholic.provinces %<>% map(~.x %>% mutate(index.catholic=tolower(`Row Labels`) %>% str_squish()))

rm(list=setdiff(ls(),c("data","catholic.provinces","kinshasa.subprov")))

# Match locations written in a different way and cities to be merged into their territories.
catholic.replacements <- c(kinshasa.subprov,
                           "luilu (mwene-ditu)"="luilu",
                           "isiro ville"="rungu",
                           "bunia ville"="irumu",
                           "kenge ville"="kenge",
                           "inongo ville"="inongo",
                           "gemena ville"="gemena",
                           "lisala ville"="lisala",
                           "boende ville"="boende",
                           "buta ville"="buta",
                           "kamina ville"="kamina",
                           "kalemie ville"="kalemie",
                           "kabinda ville"="kabinda",
                           "lusambo ville"="lusambo",
                           "tshikapa"="kamonia",
                           "tshikapa ville"="tshikapa")

# Create the indexes for the raw and catholic data.
raw.index <- data$index.2018 %>% unique %>% set_names(.)

catholic.index <- catholic.provinces %>% map(~.x %>% slice(-c(1,nrow(.x))) %>% pull("index.catholic")) %>% reduce(c) %>% unique %>% set_names(.)

names(catholic.index) %<>% str_remove("commune de") %>% str_squish()

names(catholic.index) <- case_when(names(catholic.index) %in% names(catholic.replacements) ~ catholic.replacements[names(catholic.index)],TRUE ~ names(catholic.index))

# Find matches
matches.raw.catholic <- match(names(raw.index),names(catholic.index))
matches.catholic.raw <- match(names(catholic.index),names(raw.index))

# Extract matched names
villes.raw.catholic <- raw.index[!is.na(matches.raw.catholic)]
villes.catholic.raw <- catholic.index[!is.na(matches.catholic.raw)]

# Get names not matched
villes.unique.raw <- raw.index[is.na(matches.raw.catholic)]
villes.unique.catholic <- catholic.index[is.na(matches.catholic.raw)]

# Create index data.frame

matches.raw.catholic <- match(names(villes.raw.catholic),names(villes.catholic.raw))
matches.catholic.raw <- match(names(villes.catholic.raw),names(villes.raw.catholic))
villes.equ <- bind_rows(
  data.frame(index.raw=villes.raw.catholic,index.catholic=villes.catholic.raw[matches.raw.catholic]),
  data.frame(index.raw=villes.raw.catholic[matches.catholic.raw],index.catholic=villes.catholic.raw),
  data.frame(index.raw=villes.unique.raw),
  data.frame(index.catholic=villes.unique.catholic)
) %>% distinct()


# Join the raw and datholic data
raw.data <- data %>% select(index.2018,total.votes_2018,ramazani.votes_2018,fayulu.votes_2018,tshisekedi.votes_2018)

tables <- catholic.provinces[-c(length(catholic.provinces),length(catholic.provinces)-1)] %>% map(~{

  # Join and merge catholic data  
  joined <- .x %>% slice(-1,-nrow(.))%>% 
    left_join(villes.equ,by="index.catholic") %>% 
    left_join(raw.data,by=c(index.raw="index.2018")) %>%
    group_by(index.raw) %>% 
    summarise(Label=str_to_title(unique(index.raw)),
              across(c(`Nombre de BVD`,`Suffrage valable`,Fayulu,Ramazani,Tshisekedi),~sum(as.numeric(.))), # Merge catholic data to match our data grannularity
              across(ends_with("_2018"),unique),.groups="drop") %>% # Drop repeated values in raw data resuting from the join
    select(-index.raw)
  
  # Compute province total
  total <- joined %>% summarise(Label="Total",across(where(is.numeric),~sum(.,na.rm = TRUE)))
  
  # Compute percentages
  percent <- total %>% mutate(Label="Percentage",
                              `Nombre de BVD`=as.character(NA),
                              across(c(Fayulu,Ramazani,Tshisekedi),~percent(./`Suffrage valable`)),
                              `Suffrage valable`=as.character(NA),
                              across(ends_with("_2018"),~percent(./total.votes_2018)),
                              total.votes_2018=NA)
  # Format and put all together in a table.
  total %<>% mutate(across(where(is.numeric),~dollar(.,prefix = "")))
  
  joined %<>% mutate(across(where(is.numeric),~dollar(.,prefix = "")))
  
  joined%<>% bind_rows(total,percent)
  
  
})

# Grand total table. Join and merge catholic data as above. Then sum all the votes
total.table <- catholic.provinces[-c(length(catholic.provinces),length(catholic.provinces)-1)] %>% map(~{
  
  joined <- .x %>% slice(-1,-nrow(.))%>% left_join(villes.equ,by="index.catholic") %>% left_join(raw.data,by=c(index.raw="index.2018")) %>%
    group_by(index.raw) %>% summarise(Label=str_to_title(unique(index.raw)),across(c(`Nombre de BVD`,`Suffrage valable`,Fayulu,Ramazani,Tshisekedi),~sum(as.numeric(.))), across(ends_with("_2018"),unique),.groups="drop") %>%
    select(-index.raw) %>% summarise(Label="Total",across(where(is.numeric),~sum(.,na.rm = TRUE)))
  
}) %>% reduce(bind_rows) %>% summarise(Label="Grand Total",across(where(is.numeric),~sum(.,na.rm=TRUE)))

# Compute percentages from the grand total
percent.table <- total.table %>% mutate(Label="Percentage",
                            `Nombre de BVD`=as.character(NA),
                            across(c(Fayulu,Ramazani,Tshisekedi),~percent(./`Suffrage valable`)),
                            `Suffrage valable`=as.character(NA),
                            across(ends_with("_2018"),~percent(./total.votes_2018)),
                            total.votes_2018=NA)

# Format grand total table
total.table %<>% mutate(across(where(is.numeric),~dollar(.,prefix = "")))

# Join the provinces, grand total and percentages table 
tables <- c(tables,list(total.table,percent.table) %>% set_names(c("Grand Total","Percentage")))

# Create flextables from the data.
tables.formatted <- tables %>% map(~{

  ft <- .x %>% select(1,3:7,9,8,everything(),-`Nombre de BVD`) %>% set_colnames(LETTERS[c(1,3:10)]) %>% flextable() %>% set_table_properties(width=1,layout="autofit") %>% 
    set_header_labels(A=" ",C="Valid Votes",E="Ramazani",D="Fayulu",F="Tshisekedi",G="Voters",I="Ramazani",H="Fayulu",J="Tshisekedi") %>% 
    align(j=c(2:9),align = "right",part="all") %>%
    bold(j=1) %>%
    add_header(values=c(A="",C="Catholic",D="Catholic",E="Catholic",F="Catholic",
                        G="Raw",H="Raw",I="Raw",J="Raw")) %>%
    merge_h(part="header") %>%
    bold(part = "header") %>%
    vline(j=5,border = fp_border()) %>% 
      fontsize(size=9,part="all")
  
  # Format for the total and percentage rows in provinces only. 
  if(nrow(.x)>1){
    ft %<>%hline(i=nrow(.x)-2,border=fp_border(width = 2)) %>%
    hline(i=nrow(.x)-1,border=fp_border())
  }
  ft
})

# Save to docx
title <- "2018 Raw and Catholic report data comparison"
file <- "tables/Raw_catholic.docx"


print.province.tables.to.doc(tables.formatted,title,file,tables.per.page=2)
