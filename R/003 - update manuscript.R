
rm(list=ls())

###############################################################################
# Name: 003 - update manuscript.R
# Author: John Quattrochi (john.quattrochi@gmail.com)
# Assistant: Juan Luis Herrera Cortijo (juan.luis.herrera.cortijo@gmail.com)
# Purpose: Generate Figures and Tables for the manuscript and update the
# manuscript document.
# Script UUID: "6c0bba01-1af0-5a6e-abeb-f00aa4c08b28"
###############################################################################

here::i_am("R/003 - update manuscript.R", uuid = uuid::UUIDfromName("64d742a5-bb7a-0164-0192-f03d7f475bed", "003 - update manuscript.R"))

library(magrittr)
library(sf)
library(officer)
#### FUNCTIONS ####


source(here::here("R/000 - utils.R"))

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






#### USER SECTION ####

document_path <- here::here("manuscript/main_CONFLICT.docx")

# Figures are stored in the manuscript/figures folder as png files. Figure files are updated only
# if the corresponding update_FigX is TRUE

figures_path <- here::here("manuscript/figures")

update_document <- TRUE

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

# Load data

load(here::here("results/data.RData"))

##### Figures #####

# Check if any variables matching the pattern "update_Fig*" exist and are TRUE
update_any_figure <- ls(pattern = "update_Fig*") %>%
  # Apply get() to retrieve the value of each variable and check if it is TRUE
  purrr::map_lgl(get) %>%
  # Check if any of the variables are TRUE
  any()

# If any of the "update_Fig*" variables are TRUE, then update the figures
if (update_any_figure) {
  # Source the script to create figures
  source(here::here("R/003a - create figures.R"))
}



# Tables

source(here::here("R/003b - create tables.R"))


#### UPDATE DOCUMENT #####

if(update_document){
  source(here::here("R/003c - update document.R"))
}


