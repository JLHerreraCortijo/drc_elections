
rm(list=ls())

here::i_am("R/002 - update manuscript.R")

###############################################################################
#' Name: update document
#' Author: John Quattrochi (john.quattrochi@gmail.com)
#' Assistant: Juan Luis Herrera Cortijo (juan.luis.herrera.cortijo@gmail.com)
#' Purpose: Updates word document with tables and figures
#' Notes:
#' 
#' Figures are stored in the document/figures folder as png files. Figure files are updated only
#' if the corresponding update_FigX un the user section is TRUE.
#' 
#' Figures width and height in inches can be set in the user section.
#' 
#' 
#' Figures and tables produced: 
#' 
#' Figure 1
#' Figure 2
#' Figure 3
#' 
#' 
#' The script assumes the following folder structure:
#' Scripts are stored in "[project folder]/R"
#' Data are stored in "[project folder]/data"
#' Document is stored in "[project folder]/document"
###############################################################################


#### FUNCTIONS ####


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

fix_ACLED_index <- function(df) df%>% 
  dplyr::mutate(index=
                  dplyr::case_when(
                    index %in% c("kinshasa i lukunga","kinshasa ii funa","kinshasa iii mt amba","kinshasa iv tshangu") ~ "kinshasa",
                    index == "beni ville" ~ "beni",
                    TRUE ~ index
                  ))

make_dyad_labels <- function(df,var) df %>% 
  dplyr::mutate(dplyr::across(dplyr::one_of(var),
                              ~dplyr::case_when(.%in% names(dyad_labels)~ dyad_labels[.],TRUE~.)))



#### USER SECTION ####

document_path <- here::here("manuscript/main_CONFLICT.docx")

# Figures are stored in the document/figures folder as png files. Figure files are updated only
# if the corresponding update_FigX is TRUE

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



# Run model diagnostics?

run_diagnostics <- F


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


#### MAIN BODY ####

# Load data

load(here::here("data/data.RData"))



stop("move to preprocess")
# Check which  territories should be merged

# x <- sf::st_intersection(congo.territoire.borders) %>% dplyr::filter(n.overlaps ==2)
# 
# x %<>% dplyr::filter(sf::st_is_valid(x))
# 
# y <- x %>% dplyr::pull("origins") %>% purrr::map(~{
#   congo.territoire.borders %>% dplyr::slice(.x)
# })
# 
# y %>%  purrr::walk2(1:length(.),~{
#   ggplot2::ggplot(.x) + geom_sf()
#   ggsave(here::here(paste0("document/intersections/",.y,".png")))
#   })
# 
# y[c(8,116,118,146,241,271,281,358,417)] %>% purrr::map(~.x %>% dplyr::mutate(area=st_area(.x)))

villes_to_merge <- c(
  "beni ville"="beni",
  "kikwit"="bulungu",
  "kindu"="kailo",
  "likasi"="kambove",
  "mbuji-mayi"="lupatapata",
  "kolwezi"="mutshatsha",
  "mwene-ditu"="luilu",
  "tshela"="luozi"
)

villes_labels <- data %>% dplyr::filter(index %in% c(names(villes_to_merge),villes_to_merge)) %>%
  dplyr::mutate(out=label %>% magrittr::set_names(index)) %>% dplyr::pull("out")

villes_labels[names(villes_to_merge)] <- villes_labels[villes_to_merge]

merge_villes <- function(df,labels=NA,index="index"){
  df %<>% dplyr::rename_with(.fn = ~ paste0(".index"),.cols = dplyr::one_of(index))
  if(!is.na(labels)){
    
    df %<>% dplyr::mutate(dplyr::across(dplyr::one_of(labels),~dplyr::case_when(.index %in% names(villes_labels) ~ villes_labels[.index], TRUE ~ .)))
  }
  df %>% 
    dplyr::mutate(.index=dplyr::case_when(.index %in% names(villes_to_merge) ~ villes_to_merge[.index], TRUE ~ .index)) %>% 
    dplyr::rename_with(.fn = ~ paste0(index),.cols = dplyr::one_of(".index"))
} 



data_villes_merged <- data %>% merge_villes("label") %>% dplyr::group_by(index) %>% dplyr::summarise(
  label=unique(label),
  province=unique(province),
  # dplyr::across(dplyr::starts_with("data_"),~purrr::map(.,~dplyr::bind_rows(.x))),
  dplyr::across(dplyr::starts_with("registered.voters_"),sum),
  dplyr::across(dplyr::starts_with("voters_"),sum),
  dplyr::across(dplyr::starts_with("total.votes_"),sum),
  dplyr::across(dplyr::starts_with("kabila.votes_"),sum),
  dplyr::across(dplyr::starts_with("ramazani.votes_"),sum),
  dplyr::across(dplyr::starts_with("ballot.boxes_counted_"),sum),
  dplyr::across(dplyr::starts_with("n.voting.sites_"),sum),
  dplyr::across(dplyr::starts_with("zero.voters.sites_"),sum),
  dplyr::across(dplyr::starts_with("fayulu.votes_"),sum),
  dplyr::across(dplyr::starts_with("tshisekedi.votes_"),sum),
  .groups="drop"
  
) %>% dplyr::mutate(
  kabila.percent_2006=kabila.votes_2006/total.votes_2006,
  kabila.percent_2011=kabila.votes_2011/total.votes_2011,
  ramazani.percent_2018=ramazani.votes_2018/total.votes_2018,
  fayulu.percent_2018=fayulu.votes_2018/total.votes_2018,
  tshisekedi.percent_2018=tshisekedi.votes_2018/total.votes_2018,
  turnout_2006=total.votes_2006/registered.voters_2006,
  turnout_2011=total.votes_2011/registered.voters_2011,
  turnout_2018=total.votes_2018/registered.voters_2018
)

