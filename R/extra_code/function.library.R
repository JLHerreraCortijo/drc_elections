
###############################################################################
#' Name: 001_preprocess.R
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

if(!require(sp)){
  install.packages("sp",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(sp)
packageVersion("sp")
# 1.4.5


if(!require(sf)){
  install.packages("sf",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(sf)
packageVersion("sf")
# 0.9.7


if(!require(scales)){
  install.packages("scales",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(scales)
packageVersion("scales")
# 1.1.1

if(!require(officer)){
  install.packages("officer",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(officer)
packageVersion("officer")
# 0.3.16


if(!require(flextable)){
  install.packages("flextable",dependencies = TRUE,repos='http://cran.us.r-project.org')
}


require(flextable)
packageVersion("flextable")
# 0.6.1


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
