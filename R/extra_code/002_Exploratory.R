rm(list=ls())

here::i_am("R/002_Exploratory.R")

###############################################################################
#' Name: 002_Exploratory.R
#' Author: John Quattrochi (john.quattrochi@gmail.com)
#' Assistant: Juan Luis Herrera Cortijo (juan.luis.herrera.cortijo@gmail.com)
#' Purpose: Produces exploratory tables and plots
#' Notes:
#' 
#' Tables produced: 
#' - Kabla and Ramazani percentages
#' - Total votes for the main candidates
#' - Total voters
#' - Registered voters
#' - Kabila and Ramazani votes
#' - Percentage of voting sites with zero voters in 2018.
#' 
#' Figures produced:
#' - Per year Kabila and Ramazani percentages of votes maps.
#' 
#' The script assumes the following folder structure:
#' Scripts are stored in "[project folder]/R"
#' Data are stored in "[project folder]/data"
#' Results are stored in "[project folder]/results"
#' Figures are stored in "[project folder]/figures"
#' Tables are stored in "[project folder]/tables"
###############################################################################


require(sp)

require(sf)

require(scales)

require(officer)

require(flextable)

require(magrittr)

require(tidyverse)

require(spdplyr)


##### FUNCTIONS #####
source(here::here("R/function.library.R"))
##### LOAD THE DATA #####

load(here::here("results/data.RData"))

# 1.TABLES PER PROVINCE

# We will use 2018 provinces. Replace them

provinces.2018 <- c("Kasai Occidental"="Kasai")

provinces <- data %>% mutate(province=case_when(province %in% names(provinces.2018) ~provinces.2018[province], TRUE~province),
                             empty=NA)

provinces  %<>%
  group_by(province) %>%
  group_split()
province.names <- provinces %>% map(~.x$province %>% unique) %>% unlist
provinces %<>% set_names(province.names)

##### 1.1 PERCENTAGE TABLES #####




title <- "Percentages of votes for Kabila (2006,2011) and for Ramazani (2018)"
file <- here::here("tables/Kabila_percent.docx")

format.funct <- function(x) sprintf("%0.1f%%",x*100)
prov.aggr.function <- function(x)  sum(x,na.rm = TRUE)
col.name.2006<-"kabila.votes_2006"
col.name.2011<-"kabila.votes_2011"
col.name.2018<-"ramazani.votes_2018"

kabila.votes.overall<- compute.overall.table(provinces,col.name.2006,col.name.2011,col.name.2018,prov.aggr.function)
kabila.votes.provinces <- provinces %>% map(~compute.province.table(.x,col.name.2006,col.name.2011,col.name.2018,prov.aggr.function))

col.name.2006<-"total.votes_2006"
col.name.2011<-"total.votes_2011"
col.name.2018<-"total.votes_2018"

total.votes.overall<- compute.overall.table(provinces,col.name.2006,col.name.2011,col.name.2018,prov.aggr.function)
total.votes.provinces <- provinces %>% map(~compute.province.table(.x,col.name.2006,col.name.2011,col.name.2018,prov.aggr.function))

percentages.provinces <- map2(kabila.votes.provinces,total.votes.provinces,~{
  (.x[,c("B","C","D","F","G","H")]/.y[,c("B","C","D","F","G","H")]) %>% mutate(A=.x$A,E=.x$E) %>% select(A,B,C,D,E,F,G,H) %>% format.table(format.funct)
})
percentages.overall <- (kabila.votes.overall[,c("B","C","D")]/total.votes.overall[,c("B","C","D")]) %>% mutate(A="DRC") %>% select(A,B,C,D) %>% format.overall.table(format.funct)

percentages <- c(list(percentages.overall),percentages.provinces) %>% set_names("DRC",names(percentages.provinces))

print.province.tables.to.doc(percentages,title,file)


##### 1.2 MAIN CANDIDATES VOTES TABLES #####




title <- "Total valid votes (In 2018=Voters)"
file <- here::here("tables/Valid_votes.docx")

format.funct<- function(x) scales::dollar(x,prefix="")
prov.aggr.function <- function(x) sum(x,na.rm = TRUE)

col.name.2006<-"total.votes_2006"
col.name.2011<-"total.votes_2011"
col.name.2018<-"total.votes_2018"

total.votes <- province.tables(provinces,format.funct,col.name.2006,col.name.2011,col.name.2018,prov.aggr.function)

print.province.tables.to.doc(total.votes,title,file)


##### 1.3 TOTAL VOTERS TABLES #####




title <- "Voters"
file <- here::here("tables/Voters.docx")

format.funct <- function(x) scales::dollar(x,prefix="")
prov.aggr.function <- function(x) sum(x,na.rm = TRUE)

col.name.2006<-"voters_2006"
col.name.2011<-"voters_2011"
col.name.2018<-"voters_2018"

total.votes <- province.tables(provinces,format.funct,col.name.2006,col.name.2011,col.name.2018,prov.aggr.function)

print.province.tables.to.doc(total.votes,title,file)


##### 1.4 REGISTERED VOTERS TABLES #####




title <- "Registered Voters"
file <- here::here("tables/Registered_voters.docx")

format.funct <- function(x) scales::dollar(x,prefix="")
prov.aggr.function <- function(x) sum(x,na.rm = TRUE)

col.name.2006<-"registered.voters_2006"
col.name.2011<-"registered.voters_2011"
col.name.2018<-"registered.voters_2018"

registered.voters <- province.tables(provinces,format.funct,col.name.2006,col.name.2011,col.name.2018,prov.aggr.function)

print.province.tables.to.doc(registered.voters,title,file)

##### 1.5 KABILA, RAMAZANI VOTES TABLES #####




title <- "Votes for Kabila (2006,2011) and for Ramazani (2018)"
file <- here::here("tables/Kabila_votes.docx")
format.funct <- function(x) scales::dollar(x,prefix="")
prov.aggr.function <- function(x) sum(x,na.rm = TRUE)

col.name.2006<-"kabila.votes_2006"
col.name.2011<-"kabila.votes_2011"
col.name.2018<-"ramazani.votes_2018"

total.votes <- province.tables(provinces,format.funct,col.name.2006,col.name.2011,col.name.2018,prov.aggr.function)

print.province.tables.to.doc(total.votes,title,file)

##### 1.6 PERCENTAGE ZERO VOTING SITES TABLES #####




title <- "Percentages of zero voters sites (2018 only)"
file <- here::here("tables/Zero_voters_percent.docx")

format.funct <- function(x) sprintf("%0.1f%%",x*100)
prov.aggr.function <- function(x)  sum(x,na.rm = TRUE)
col.name.2006<-"empty"
col.name.2011<-"empty"
col.name.2018<-"zero.voters.sites_2018"

zero.voters.overall<- compute.overall.table(provinces,col.name.2006,col.name.2011,col.name.2018,prov.aggr.function)
zero.voters.provinces <- provinces %>% map(~compute.province.table(.x,col.name.2006,col.name.2011,col.name.2018,prov.aggr.function))

col.name.2006<-"empty"
col.name.2011<-"empty"
col.name.2018<-"n.voting.sites_2018"

voting.sites.overall<- compute.overall.table(provinces,col.name.2006,col.name.2011,col.name.2018,prov.aggr.function)
voting.sites.provinces <- provinces %>% map(~compute.province.table(.x,col.name.2006,col.name.2011,col.name.2018,prov.aggr.function))

percentages.provinces <- map2(zero.voters.provinces,voting.sites.provinces,~{
  (.x[,c("B","C","D","F","G","H")]/.y[,c("B","C","D","F","G","H")]) %>% mutate(A=.x$A,E=.x$E) %>% select(A,B,C,D,E,F,G,H) %>% format.table(format.funct)
})
percentages.overall <- (zero.voters.overall[,c("B","C","D")]/voting.sites.overall[,c("B","C","D")]) %>% mutate(A="DRC") %>% select(A,B,C,D) %>% format.overall.table(format.funct)

percentages <- c(list(percentages.overall),percentages.provinces) %>% set_names("DRC",names(percentages.provinces))

print.province.tables.to.doc(percentages,title,file)

#### 1.7 TURNOUT TABLES ####


title <- "Turnout"
file <- here::here("tables/Turnout.docx")


format.funct <- function(x) sprintf("%0.1f%%",x*100)
prov.aggr.function <- function(x)  sum(x,na.rm = TRUE)
col.name.2006<-"voters_2006"
col.name.2011<-"voters_2011"
col.name.2018<-"voters_2018"

voters.overall<- compute.overall.table(provinces,col.name.2006,col.name.2011,col.name.2018,prov.aggr.function)
voters.provinces <- provinces %>% map(~compute.province.table(.x,col.name.2006,col.name.2011,col.name.2018,prov.aggr.function))

col.name.2006<-"registered.voters_2006"
col.name.2011<-"registered.voters_2011"
col.name.2018<-"registered.voters_2018"

registered.voters.overall<- compute.overall.table(provinces,col.name.2006,col.name.2011,col.name.2018,prov.aggr.function)
registered.voters.provinces <- provinces %>% map(~compute.province.table(.x,col.name.2006,col.name.2011,col.name.2018,prov.aggr.function))

percentages.provinces <- map2(voters.provinces,registered.voters.provinces,~{
  (.x[,c("B","C","D","F","G","H")]/.y[,c("B","C","D","F","G","H")]) %>% mutate(A=.x$A,E=.x$E) %>% select(A,B,C,D,E,F,G,H) %>% format.table(format.funct)
})
percentages.overall <- (voters.overall[,c("B","C","D")]/registered.voters.overall[,c("B","C","D")]) %>% mutate(A="DRC") %>% select(A,B,C,D) %>% format.overall.table(format.funct)

percentages <- c(list(percentages.overall),percentages.provinces) %>% set_names("DRC",names(percentages.provinces))

print.province.tables.to.doc(percentages,title,file)




##### 2. KABILA AND RAMAZANI PERCENTAGES #####

rm(list=ls())

load(here::here("results/data.RData"))

##### 2.1 MAP #####

# As we have merged some regions when matching the map (for example Kinshasa) we need to aggregate the data
# On the merged regions before computing the percentages.

# total.votes <- data %>% left_join(data.map.index,by=c(index="index.data")) %>% select(index.map,total.votes_2006,total.votes_2011,total.votes_2018)  %>% 
#   group_by(index.map) %>% summarise(across(starts_with("total.votes"),~sum(.,na.rm=TRUE)),.groups="drop")
# 
# kabila.votes <- data %>% left_join(data.map.index,by=c(index="index.data")) %>% select(index.map,kabila.votes_2006,kabila.votes_2011,ramazani.votes_2018) %>% 
#   group_by(index.map) %>% summarise(across(matches("votes"),~sum(.,na.rm=TRUE)),.groups="drop")

# percentages <- kabila.votes %>% full_join(total.votes,by="index.map")

percentages <- data %>% select(index,total.votes_2006,total.votes_2011,total.votes_2018,kabila.votes_2006,kabila.votes_2011,ramazani.votes_2018)

# Now, compute the percentages

percentages %<>% mutate(percentage_2006=kabila.votes_2006/total.votes_2006,
                        percentage_2011=kabila.votes_2011/total.votes_2011,
                        percentage_2018=ramazani.votes_2018/total.votes_2018
) %>% select(index,starts_with("perc")) %>% ungroup

# Pivot the data to plot one map per year
percentages %<>%  pivot_longer(cols=-c(index),names_to="var",values_to="percent")

percentages %<>% separate(var,c("drop","year"),sep="_")

percentages %<>% mutate(percent=percent*100)

# Lakes are not administrative units, we will show them as NAs

percentages %<>% bind_rows(data.frame(index="administrative unit not available",year=c("2006","2011","2018")))

# Join percentages and shapefile data



percentages.map <- congo.territoire.borders %>% full_join(percentages,by=c("index.data"="index"))

# percentages.map %<>% filter(year==2011)

# Extract lakes because we are plotting them in a different color

lakes <- percentages.map %>% filter(index.data =="administrative unit not available")

percentages.map %<>% filter(index.data !="administrative unit not available")



# Plot and save the figure

if(!file.exists("figures/Kabila_percentage_maps.png")){
  g <- ggplot(percentages.map,aes(fill=percent,color="")) +
    geom_sf() + 
    geom_sf(data = lakes,fill="white") +
    scale_color_manual(values="gray30") +
    scale_fill_gradient(name="% Votes",guide="colourbar",na.value="gray50")+
    guides(colour=guide_legend("No data", override.aes=list(color="gray50",fill="gray50")))+
    facet_wrap(~year,ncol=2) + 
    theme_void()+
    theme(legend.position = c(0.75,0.25),
          strip.text = element_text(size=18),
          legend.text = element_text(size=12),
          legend.title = element_text(size=14)) 
  
  ggsave(here::here("figures/Kabila_percentage_maps.png"),g)
  
}

##### 2.2 DIFFERENCE MAPS ######

percentage.differences <- percentages.map %>% select(index.data,year,percent) %>% as.data.frame() %>% select(-geometry) %>% pivot_wider(names_from = "year",values_from = "percent") %>%
  mutate(`2006-2011`=`2011`-`2006`,`2011-2018`=`2018`-`2011`) %>% select(-c(`2006`,`2011`,`2018`)) %>% pivot_longer(cols = -index.data,names_to = "period",values_to = "difference")

percentage.differences %<>% bind_rows(data.frame(index.data="administrative unit not available",period=c("2006-2011","2011-2018")))

percentage.differences.map <- congo.territoire.borders %>% full_join(percentage.differences,by="index.data")
lakes <- percentage.differences.map %>% filter(index.data =="administrative unit not available")

percentage.differences.map %<>% filter(index.data !="administrative unit not available")

if(!file.exists("figures/Kabila_percentage_diff_maps.png")){
  g <- ggplot(percentage.differences.map,aes(fill=difference,color="")) +
    geom_sf() + 
    geom_sf(data = lakes,fill="white") +
    scale_color_manual(values="gray30") +
    scale_fill_gradient2(name="Change",guide="colourbar",na.value="gray50",)+
    guides(colour=guide_legend("No data", override.aes=list(color="gray50",fill="gray50")))+
    facet_wrap(~period,ncol=2) + 
    theme_void()+
    theme(legend.position = "bottom",
          strip.text = element_text(size=18),
          legend.text = element_text(size=12),
          legend.title = element_text(size=14)) 
  
  ggsave(here::here("figures/Kabila_percentage_diff_maps.png"),g)
  
}


g <- ggplot(percentage.differences,aes(x=difference)) +geom_histogram(bins = 1+3.322*log10(nrow(percentage.differences)),color="black",fill="red") +
  facet_wrap(~period,ncol=2) + xlab("Change in vote share")

ggsave(here::here("figures/Kabila_percentage_diff_histograms.png"),g,width = 5,height = 3)

##### 2.3 MISSING PERCENTAGE DATA TABLE #####

missing.data.table <- percentages.map %>% 
  filter(is.na(percent)) %>% 
  data.frame() %>% select(year,index.data)  %>% 
  select(region=index.data,year) %>% 
  mutate(region=str_to_title(region)) %>% 
  arrange(year,region) %>% distinct %>% flextable() %>%
  merge_v(j=1) %>% hline(i=5,border = fp_border()) %>% autofit()

doc <- read_docx()



doc %<>% body_add_par("Missing data",style="table title")
doc %<>% body_add_flextable(missing.data.table,align = "center")


print(doc,target=here::here("tables/Missing.percentages.docx"))


##### 3 CONFLICT #####

rm(list=ls())

load(here::here("results/data.RData"))


###### 3.1 - CONFLICTS LOCATION #####

if(!file.exists(here::here("figures/Conflict_locations_maps.png"))){
  
  lakes <- congo.territoire.borders %>% filter(index.data =="administrative unit not available")
  
  
  
  g <- ged201 %>% ggplot() +
    geom_sf(data = congo.territoire.borders,color="gray30")+
    geom_sf(data = lakes,fill="white")+
    geom_sf(color="red",size=0.1,alpha=0.5) + 
    facet_wrap(~year,ncol=7)+
    theme_void()+
    theme(legend.position = c(0.75,0.25),
          strip.text = element_text(size=18),
          legend.text = element_text(size=12),
          legend.title = element_text(size=14))
  
  ggsave(here::here("figures/Conflict_locations_maps.png"),g,width=11,height=8)
}


###### 3.1b - CONFLICTS LOCATION NO ADMINISTRATIVE UNIT #####

if(!file.exists(here::here("figures/Conflict_locations_no_adm_unit_maps.png"))){
  
  lakes <- congo.territoire.borders %>% filter(index.data =="administrative unit not available")
  
  map.projection <- st_crs(congo.territoire.borders)
  
  # Georeference conflict data
  
  
  
  g <- ged201_for_period %>% dplyr::filter(index.data=="administrative unit not available") %>% ggplot(aes(geometry=geometry)) +
    geom_sf(data = congo.territoire.borders,color="gray30")+
    geom_sf(data = lakes,fill="white")+
    geom_sf(color="red",size=0.3,alpha=0.5) + 
    theme_void()+
    theme(legend.position = c(0.75,0.25),
          strip.text = element_text(size=18),
          legend.text = element_text(size=12),
          legend.title = element_text(size=14))
  g
  ggsave(here::here("figures/Conflict_locations_no_adm_unit_maps.png"),g,width=11,height=8)
}


###### 3.2 - CONFLICTS BEFORE EACH ELECTION MAP #####




conflicts <- conflict.aggregated %>% 
  select(index.data,starts_with("n.conflicts")) %>% 
  pivot_longer(-index.data,names_to = "var",values_to = "n") %>% 
  separate(var,c("drop","year"),sep="_") %>%
  select(-drop)



# Lakes are not administrative units, we will show them as NAs

conflicts %<>% bind_rows(data.frame(index.data="administrative unit not available",year=c("2006","2011","2018")))

conflicts %<>% mutate(index.data=factor(index.data,levels = unique(congo.territoire.borders$index.data)))

conflicts %<>% complete(index.data,year)

conflicts %<>% mutate(n=replace_na(n,0))

# Join percentages and shapefile data



conflicts.map <- congo.territoire.borders %>% full_join(conflicts,by="index.data")



# Extract lakes because we are plotting them in a different color

lakes <- conflicts.map %>% filter(index.data =="administrative unit not available")

conflicts.map %<>% filter(index.data !="administrative unit not available")



# Plot and save the figure

if(!file.exists(here::here("figures/Conflicts_elections_maps.png"))){
  g <- ggplot(conflicts.map,aes(fill=n)) +
    geom_sf() + 
    geom_sf(data = lakes,fill="white") +
    scale_fill_gradient(low="white",high="red",name="# Conflicts",guide="colourbar",na.value="gray50")+
    facet_wrap(~year,ncol=2) + 
    theme_void()+
    theme(legend.position = c(0.75,0.25),
          strip.text = element_text(size=18),
          legend.text = element_text(size=12),
          legend.title = element_text(size=14)) 
  
  ggsave(here::here("figures/Conflicts_elections_maps.png"),g)
  
}



###### 3.3 - PERCENTAGES VS CONFLICTS #####


# As we have merged some regions when matching the map (for example Kinshasa) we need to aggregate the data
# On the merged regions before computing the percentages.
# 
# total.votes <- data %>% left_join(data.map.index,by=c(index="index.data")) %>% select(index.map,total.votes_2006,total.votes_2011,total.votes_2018)  %>% 
#   group_by(index.map) %>% summarise(across(starts_with("total.votes"),~sum(.,na.rm=TRUE)),.groups="drop")
# 
# kabila.votes <- data %>% left_join(data.map.index,by=c(index="index.data")) %>% select(index.map,kabila.votes_2006,kabila.votes_2011,ramazani.votes_2018) %>% 
#   group_by(index.map) %>% summarise(across(matches("votes"),~sum(.,na.rm=TRUE)),.groups="drop")
# 
# percentages <- kabila.votes %>% full_join(total.votes,by="index.map")

percentages <- data %>% select(index,total.votes_2006,total.votes_2011,total.votes_2018,kabila.votes_2006,kabila.votes_2011,ramazani.votes_2018)
# Now, compute the percentages

percentages %<>% mutate(percentage_2006=kabila.votes_2006/total.votes_2006,
                        percentage_2011=kabila.votes_2011/total.votes_2011,
                        percentage_2018=ramazani.votes_2018/total.votes_2018
) %>% select(index,starts_with("perc")) %>% ungroup

# Pivot the data to plot one map per year
percentages %<>%  pivot_longer(cols=-c(index),names_to="var",values_to="percent")

percentages %<>% separate(var,c("drop","year"),sep="_")

# percentages %<>% mutate(percent=percent*100) %>% select(-drop)

to.plot <- conflicts %>% full_join(percentages,by = c("index.data"="index", "year"))


g <- to.plot %>% ggplot(aes(x=n,y=percent)) + 
  geom_point(alpha=0.2,size=0.5) + 
  geom_smooth(method="glm",method.args=list(family="binomial"),se=FALSE, formula=y~x) + 
  xlab("# Conflicts before election")+
  ylab("% Votes for Kabila")+
  scale_y_continuous(labels=scales::percent)+
  facet_wrap(~year,ncol=2) + 
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size=12),
        strip.text = element_text(size=18))

ggsave(here::here("figures/Percent_votes_vs_conflicts_scatterplot.png"),g,width=7,height = 5)



###### 3.4 - CONFLICTS TYPES TABLE ######

conflict.types.table <- ged201 %>% as.data.frame() %>% filter(date_start >= lubridate::ymd("2001-07-30") & date_end <= lubridate::ymd("2018-12-30")) %>%
   group_by(type) %>% summarise(n=n(),best=sum(best),.groups = "drop") %>% rename(`Conflict category`=type) %>%
  flextable() %>% autofit()

doc <- read_docx()



doc %<>% body_add_par("Conflict categories",style="table title")
doc %<>% body_add_flextable(conflict.types.table,align = "center")


print(doc,target=here::here("tables/Conflict.categories.docx"))

# By state

conflict.types.table.by.state <- ged201 %>% as.data.frame() %>% filter(date_start >= lubridate::ymd("2001-07-30") & date_end <= lubridate::ymd("2018-12-30")) %>%
 filter(str_starts(type,"State"))
  
  
conflict.types.table.by.state %<>% group_by(type,side_a) %>% summarise(n=n(),best=sum(best),.groups = "drop") 

.line_pos <- conflict.types.table.by.state %>% select(1) %>% count(type) %>% slice(1) %>% pull(n)
conflict.types.table.by.state %<>% rename(State=side_a) %>% rename(`Conflict category`=type)  %>% 
  flextable() %>% autofit() %>% hline(i = .line_pos,border = officer::fp_border())

doc <- read_docx()



doc %<>% body_add_par("Conflict categories by State involved",style="table title")
doc %<>% body_add_flextable(conflict.types.table.by.state,align = "center")


print(doc,target=here::here("tables/Conflict.categories.by.state.docx"))


# ged201 %>% as.data.frame() %>% filter(date_start >= lubridate::ymd("2001-07-30") & date_end <= lubridate::ymd("2018-12-30")) %>%
#  select(type_of_violence,side_a,side_b,best) %>% filter(str_detect(side_a,"Government")) %>% pull("side_a") %>% unique


###### 3.5 - TURNOUT VS CONFLICTS #####


# As we have merged some regions when matching the map (for example Kinshasa) we need to aggregate the data
# On the merged regions before computing the percentages.
# 
# total.votes <- data %>% left_join(data.map.index,by=c(index="index.data")) %>% select(index.map,total.votes_2006,total.votes_2011,total.votes_2018)  %>% 
#   group_by(index.map) %>% summarise(across(starts_with("total.votes"),~sum(.,na.rm=TRUE)),.groups="drop")
# 
# kabila.votes <- data %>% left_join(data.map.index,by=c(index="index.data")) %>% select(index.map,kabila.votes_2006,kabila.votes_2011,ramazani.votes_2018) %>% 
#   group_by(index.map) %>% summarise(across(matches("votes"),~sum(.,na.rm=TRUE)),.groups="drop")
# 
# percentages <- kabila.votes %>% full_join(total.votes,by="index.map")

percentages <- data %>% select(index,starts_with("turnout_"))
# Now, compute the percentages

# percentages %<>% mutate(percentage_2006=kabila.votes_2006/total.votes_2006,
#                         percentage_2011=kabila.votes_2011/total.votes_2011,
#                         percentage_2018=ramazani.votes_2018/total.votes_2018
# ) %>% select(index,starts_with("perc")) %>% ungroup

# Pivot the data to plot one map per year
percentages %<>%  pivot_longer(cols=-c(index),names_to="var",values_to="percent")

percentages %<>% separate(var,c("drop","year"),sep="_")

# percentages %<>% mutate(percent=percent*100) %>% select(-drop)

to.plot <- conflicts %>% full_join(percentages,by = c("index.data"="index", "year"))


g <- to.plot %>% rowwise() %>% mutate(percent=min(percent,1)) %>% ungroup %>% ggplot(aes(x=n,y=percent)) + 
  geom_point(alpha=0.2,size=0.5) + 
  geom_smooth(method="glm",method.args=list(family="binomial"),se=FALSE, formula=y~x) + 
  xlab("# Conflicts before election")+
  ylab("Turnout (%)")+
  scale_y_continuous(labels=scales::percent)+
  facet_wrap(~year,ncol=2) + 
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size=12),
        strip.text = element_text(size=18))

ggsave(here::here("figures/Turnout_vs_conflicts_scatterplot.png"),g,width=7,height = 5)



##### 4. NIGHTLIGHT ####


rm(list=ls())

load(here::here("results/data.RData"))


###### 4.1 - RAW MAPS #####

load(here::here("results/nightlight.RData"))

lakes <- congo.territoire.borders %>% filter(index.data =="administrative unit not available")


to.plot <- nightlight %>% map(~{
  
  .x %<>% filter(nightlight > 30)
  
  
  
  # 
  # .x %<>% st_as_sf()
  # 
  # .x %<>% st_intersection(congo.territoire.borders)
  .x
  
})





data.plot <-  bind_rows(to.plot)

data.geom <- st_geometry(data.plot) # Extract the geometry
# 
# 
# 
# # Transform into a matrix
# data.geom <- do.rbind(data.geom)

# Merge all the rows into one 2D matrix

data.geom <- do.call(rbind, data.geom)

# Convert to table

colnames(data.geom) <- c("x","y")

data.geom <- as_tibble(data.geom)                     


data.geom <- as.data.frame(data.geom)

# Add the x and y columns to the map table



to.p <- bind_cols(data.plot,data.geom)

to.p <- as.data.frame(to.p)

to.p %<>% filter(as.numeric(year)>=2001)

tile_width <- median(diff(sort(unique(to.p$x))) %>% keep(~.x >1e-5))

tile_height <- median(diff(sort(unique(to.p$y)))%>% keep(~.x >1e-5))

g <- to.p %>%  ggplot() +
  geom_sf(data = congo.territoire.borders,color="gray60",fill="black",size=0.1)+
  geom_sf(data = lakes,fill="white",color="gray60",size=0.1)+
  geom_tile(aes(color=nightlight,x=x,y=y),width=tile_width,height=tile_height) +
  scale_color_gradient(low = "white",high = "red") +
  facet_wrap(~year ,ncol=4)+
  theme_void()+
  theme(legend.position = "bottom",
        strip.text = element_text(size=18),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14))

ggsave(here::here("figures/nightlight_maps_harmonized_gt30.png"),g,width=20,height=20)

###### 4.2 - MEAN NIGHTLIGHT ######




nightlight <- nightlight_mean



# Lakes are not administrative units, we will show them as NAs

# nightlight %<>% bind_rows(data.frame(index.map="administrative unit not available",year=unique(nightlight$year)))

# nightlight %<>% mutate(index.map=factor(index.map,levels = unique(congo.territoire.borders$index.map)))
# 
# nightlight %<>% complete(index.map,year)
# 
# conflicts %<>% mutate(n=replace_na(n,0))

# Join percentages and shapefile data



nightlight.map <- congo.territoire.borders %>% full_join(nightlight,by="index.data")



# Extract lakes because we are plotting them in a different color

lakes <- nightlight.map %>% filter(index.data =="administrative unit not available")

nightlight.map %<>% filter(index.data !="administrative unit not available")



# Plot and save the figure

if(!file.exists(here::here("figures/Mean_nightlight_maps.png"))){
  g <- ggplot(nightlight.map,aes(fill=log10(nightlight_mean+0.01))) +
    geom_sf() + 
    scale_fill_gradient(low="white",high="red",name="log10 Nightlight",guide="colourbar",na.value="gray50")+
    facet_wrap(~year,ncol=4) + 
    theme_void()+
    theme(legend.position = "bottom",
          strip.text = element_text(size=18),
          legend.text = element_text(size=12),
          legend.title = element_text(size=14)) 
  
  
  
  ggsave(here::here("figures/Mean_nightlight_maps.png"),g,width=20,height=20)
  
}


nightlight <- nightlight_gt30_mean



# Lakes are not administrative units, we will show them as NAs

# nightlight %<>% bind_rows(data.frame(index.map="administrative unit not available",year=unique(nightlight$year)))

# nightlight %<>% mutate(index.map=factor(index.map,levels = unique(congo.territoire.borders$index.map)))
# 
# nightlight %<>% complete(index.map,year)
# 
# conflicts %<>% mutate(n=replace_na(n,0))

# Join percentages and shapefile data



nightlight.map <- congo.territoire.borders %>% full_join(nightlight,by="index.data")



# Extract lakes because we are plotting them in a different color

lakes <- nightlight.map %>% filter(index.data =="administrative unit not available")

nightlight.map %<>% filter(index.data !="administrative unit not available")



if(!file.exists(here::here("figures/Mean_nightlight_gt30_maps.png"))){
  g <- ggplot(nightlight.map,aes(fill=log10(nightlight_mean+0.01))) +
    geom_sf() + 
    scale_fill_gradient(low="white",high="red",name="log10 Nightlight",guide="colourbar",na.value="gray50")+
    facet_wrap(~year,ncol=4) + 
    theme_void()+
    theme(legend.position = "bottom",
          strip.text = element_text(size=18),
          legend.text = element_text(size=12),
          legend.title = element_text(size=14)) 
  
  
  
  ggsave(here::here("figures/Mean_nightlight_gt30_maps.png"),g,width=20,height=20)
  
}

###### 4.3 - TRENDS ######

to.plot <- nightlight_mean






g <- to.plot %>% ggplot(aes(x=year,y=nightlight_mean,group=index.data)) + 
  geom_line(alpha=0.2) + 
  geom_line(stat="smooth",method="lm",se=FALSE,alpha=0.5,color="blue",formula=y ~I((x-2006)*(x <=2006))+I((x-2006)*(x >=2006 & x <=2011))+I((x-2006)*(x >=2011))) +
  theme_bw()

ggsave(here::here("figures/Mean_nightlight_trends.png"),g,width=7,height=5)



to.plot <- nightlight_gt30_mean


g <- to.plot %>% ggplot(aes(x=year,y=nightlight_mean,group=index.data)) + 
  geom_line(alpha=0.2) + 
  geom_line(stat="smooth",se=FALSE,alpha=0.5,color="blue") + 
  theme_bw()

ggsave(here::here("figures/Mean_nightlight_gt30_trends.png"),g,width=7,height=5)


# Piecewise 

to.plot <- nightlight_mean

to.plot %<>% nest(data=-c(index.data)) %>% mutate(data=map(data,~{
  
  x_DMSP <- .x %>% as.data.frame() %>% filter( year <=2011 )
  
  m_DMSP <- lm(nightlight_mean ~I((year-2006)*(year <=2006))+I((year-2006)*(year >=2006)),data=x_DMSP)
  
  x_DMSP %<>% mutate(modeled=predict(m_DMSP))
  
  x_VIIRS <- .x %>% as.data.frame() %>% filter( year >=2014 )
  
  m_VIIRS <- lm(nightlight_mean ~year,data=x_VIIRS)
  x_VIIRS %<>% mutate(modeled=predict(m_VIIRS))
  
  .x %<>% left_join(bind_rows(x_DMSP,x_VIIRS) %>% select(-nightlight_mean),by="year")
  
  .x  
  
}))

to.plot %<>% unnest(data)


# predict(m) %>% plot
# 
# sctrl <- segmented::seg.control(it.max = 1)
# s <- segmented::segmented(m_DMSP,seg.Z = ~year,fixed.psi = c(2006))
# s
# 
# plot(predict(s))
# plot(s)
# summary(s)
# segmented::slope(s)
# s$coefficients
# ?segmented::seg.control


g <- to.plot %>% ggplot(aes(x=year,y=nightlight_mean,group=index.data)) + 
  geom_line(alpha=0.2) + 
  geom_line(aes(y=modeled),color="blue",alpha=0.5)+
  # geom_line(stat="smooth",method="lm",se=FALSE,alpha=0.5,color="blue",formula=y ~I((x-2006)*(x <=2006))+I((x-2006)*(x >=2006 & x <=2011))+I((x-2006)*(x >=2011))) + 
  theme_bw()

ggsave(here::here("figures/Mean_nightlight_trends_piecewise.png"),g,width=7,height=5)



to.plot <- nightlight_gt30_mean


to.plot %<>% nest(data=-c(index.data)) %>% mutate(data=map(data,~{
  
  x_DMSP <- .x %>% as.data.frame() %>% filter( year <=2011 )
  
  m_DMSP <- lm(nightlight_mean ~I((year-2006)*(year <=2006))+I((year-2006)*(year >=2006)),data=x_DMSP)
  
  x_DMSP %<>% mutate(modeled=predict(m_DMSP))
  
  x_VIIRS <- .x %>% as.data.frame() %>% filter( year >=2014 )
  
  m_VIIRS <- lm(nightlight_mean ~year,data=x_VIIRS)
  x_VIIRS %<>% mutate(modeled=predict(m_VIIRS))
  
  .x %<>% left_join(bind_rows(x_DMSP,x_VIIRS) %>% select(-nightlight_mean),by="year")
  
  .x  
  
}))

to.plot %<>% unnest(data)

g <- to.plot %>% ggplot(aes(x=year,y=nightlight_mean,group=index.data)) + 
  geom_line(alpha=0.2) + 
  geom_line(aes(y=modeled),color="blue",alpha=0.5)+
  theme_bw()

ggsave(here::here("figures/Mean_nightlight_gt30_trends_piecewise.png"),g,width=7,height=5)


# Slope maps


to.plot <- nightlight_mean

to.plot %<>% nest(data=-c(index.data)) %>% mutate(data=map(data,~{
  
  x_DMSP <- .x %>% as.data.frame() %>% filter( year <=2011 )
  
  m_DMSP <- lm(nightlight_mean ~I((year-2006)*(year <=2006))+I((year-2006)*(year >=2006)),data=x_DMSP)
  
  
  x_VIIRS <- .x %>% as.data.frame() %>% filter( year >=2014 )
  
  m_VIIRS <- lm(nightlight_mean ~year,data=x_VIIRS)
  
  .out <- coefficients(m_DMSP) %>% t %>% as.data.frame() %>% set_colnames(c("Intercept","2001-2006","2006-2011")) %>% select(-Intercept) %>% mutate(`2014-2018`=coefficients(m_VIIRS)["year"])
  
  
  .out
  
}))

to.plot %<>% unnest(data)

to.plot %<>% pivot_longer(cols=-index.data,names_to="year",values_to="trend")



to.plot <- congo.territoire.borders %>% full_join(to.plot,by="index.data")



# Extract lakes because we are plotting them in a different color

lakes <- to.plot %>% filter(index.data =="administrative unit not available")

to.plot %<>% filter(index.data !="administrative unit not available")



# Plot and save the figure

if(!file.exists(here::here("figures/Mean_nightlight_trend_maps.png"))){
  g <- ggplot(to.plot,aes(fill=trend)) +
    geom_sf() + 
    scale_fill_gradient2(low="red",mid = "white",high="blue",name="Nightlight trend",guide="colourbar",na.value="gray50")+
    facet_wrap(~year,ncol=4) + 
    theme_void()+
    theme(legend.position = "bottom",
          strip.text = element_text(size=18),
          legend.text = element_text(size=12),
          legend.title = element_text(size=14)) 
  
  
  
  ggsave(here::here("figures/Mean_nightlight_trend_maps.png"),g,width=20,height=20)
  
}


if(!file.exists(here::here("figures/Mean_nightlight_trend_maps_compressed.png"))){
  g <- ggplot(to.plot %>% mutate(trend=sign(trend)*sqrt(abs(trend))),aes(fill=trend)) +
    geom_sf() + 
    scale_fill_gradient2(low="red",mid = "white",high="blue",name="Nightlight trend",guide="colourbar",na.value="gray50")+
    facet_wrap(~year,ncol=4) + 
    theme_void()+
    theme(legend.position = "bottom",
          strip.text = element_text(size=18),
          legend.text = element_text(size=12),
          legend.title = element_text(size=14)) 
  
  
  
  ggsave(here::here("figures/Mean_nightlight_trend_maps_compressed.png"),g,width=20,height=20)
  
}

to.plot <- nightlight_gt30_mean



to.plot %<>% nest(data=-c(index.data)) %>% mutate(data=map(data,~{
  
  x_DMSP <- .x %>% as.data.frame() %>% filter( year <=2011 )
  
  m_DMSP <- lm(nightlight_mean ~I((year-2006)*(year <=2006))+I((year-2006)*(year >=2006)),data=x_DMSP)
  
  
  x_VIIRS <- .x %>% as.data.frame() %>% filter( year >=2014 )
  
  m_VIIRS <- lm(nightlight_mean ~year,data=x_VIIRS)
  
  .out <- coefficients(m_DMSP) %>% t %>% as.data.frame() %>% set_colnames(c("Intercept","2001-2006","2006-2011")) %>% select(-Intercept) %>% mutate(`2014-2018`=coefficients(m_VIIRS)["year"])
  
  
  .out
  
}))

to.plot %<>% unnest(data)

to.plot %<>% pivot_longer(cols=-index.data,names_to="year",values_to="trend")

to.plot <- congo.territoire.borders %>% full_join(to.plot,by="index.data")



# Extract lakes because we are plotting them in a different color

lakes <- to.plot %>% filter(index.data =="administrative unit not available")

to.plot %<>% filter(index.data !="administrative unit not available")



# Plot and save the figure

if(!file.exists(here::here("figures/Mean_nightlight_gt30_trend_maps.png"))){
  g <- ggplot(to.plot,aes(fill=trend)) +
    geom_sf() + 
    scale_fill_gradient2(low="red",mid = "white",high="blue",name="Nightlight trend",guide="colourbar",na.value="gray50")+
    facet_wrap(~year,ncol=4) + 
    theme_void()+
    theme(legend.position = "bottom",
          strip.text = element_text(size=18),
          legend.text = element_text(size=12),
          legend.title = element_text(size=14)) 
  
  
  
  ggsave(here::here("figures/Mean_nightlight_gt30_trend_maps.png"),g,width=20,height=20)
  
}

if(!file.exists(here::here("figures/Mean_nightlight_gt30_trend_maps_compressed.png"))){
  g <- ggplot(to.plot%>% mutate(trend=sign(trend)*sqrt(abs(trend))),aes(fill=trend)) +
    geom_sf() + 
    scale_fill_gradient2(low="red",mid = "white",high="blue",name="Nightlight trend",guide="colourbar",na.value="gray50")+
    facet_wrap(~year,ncol=4) + 
    theme_void()+
    theme(legend.position = "bottom",
          strip.text = element_text(size=18),
          legend.text = element_text(size=12),
          legend.title = element_text(size=14)) 
  
  
  
  ggsave(here::here("figures/Mean_nightlight_gt30_trend_maps_compressed.png"),g,width=20,height=20)
  
}

###### 4.4 - PERCENTAGES VS NIGTHLIGHT #####


NL_trends <- trends_nightlight_mean  %>% 
  select(index.data,starts_with("NL.trend")) %>% 
  pivot_longer(-index.data,names_to = "var",values_to = "n") %>% 
  separate(var,c("drop","year"),sep="_") %>%
  select(-drop)

percentages <- data %>% select(index,total.votes_2006,total.votes_2011,total.votes_2018,kabila.votes_2006,kabila.votes_2011,ramazani.votes_2018)
# Now, compute the percentages

percentages %<>% mutate(percentage_2006=kabila.votes_2006/total.votes_2006,
                        percentage_2011=kabila.votes_2011/total.votes_2011,
                        percentage_2018=ramazani.votes_2018/total.votes_2018
) %>% select(index,starts_with("perc")) %>% ungroup

# Pivot the data to plot one map per year
percentages %<>%  pivot_longer(cols=-c(index),names_to="var",values_to="percent")

percentages %<>% separate(var,c("drop","year"),sep="_")

# percentages %<>% mutate(percent=percent*100) %>% select(-drop)

to.plot <- NL_trends %>% full_join(percentages,by = c("index.data"="index", "year"))


g <- to.plot %>% ggplot(aes(x=n,y=percent)) + 
  geom_point(alpha=0.2,size=0.5) + 
  geom_smooth(method="glm",method.args=list(family="binomial"),se=FALSE, formula=y~x) + 
  xlab("# Nighlight trends before election")+
  ylab("% Votes for Kabila")+
  scale_y_continuous(labels=scales::percent)+
  facet_wrap(~year,ncol=2) + 
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size=12),
        strip.text = element_text(size=18))

ggsave(here::here("figures/Percent_votes_vs_nightlight_scatterplot.png"),g,width=7,height = 5)



NL_trends <- trends_nightlight_gt30_mean  %>% 
  select(index.data,starts_with("NL.trend")) %>% 
  pivot_longer(-index.data,names_to = "var",values_to = "n") %>% 
  separate(var,c("drop","year"),sep="_") %>%
  select(-drop)

percentages <- data %>% select(index,total.votes_2006,total.votes_2011,total.votes_2018,kabila.votes_2006,kabila.votes_2011,ramazani.votes_2018)
# Now, compute the percentages

percentages %<>% mutate(percentage_2006=kabila.votes_2006/total.votes_2006,
                        percentage_2011=kabila.votes_2011/total.votes_2011,
                        percentage_2018=ramazani.votes_2018/total.votes_2018
) %>% select(index,starts_with("perc")) %>% ungroup

# Pivot the data to plot one map per year
percentages %<>%  pivot_longer(cols=-c(index),names_to="var",values_to="percent")

percentages %<>% separate(var,c("drop","year"),sep="_")

# percentages %<>% mutate(percent=percent*100) %>% select(-drop)

to.plot <- NL_trends %>% full_join(percentages,by = c("index.data"="index", "year"))


g <- to.plot %>% ggplot(aes(x=n,y=percent)) + 
  geom_point(alpha=0.2,size=0.5) + 
  geom_smooth(method="glm",method.args=list(family="binomial"),se=FALSE, formula=y~x) + 
  xlab("# Nighlight gt 30 trends before election")+
  ylab("% Votes for Kabila")+
  scale_y_continuous(labels=scales::percent)+
  facet_wrap(~year,ncol=2) + 
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size=12),
        strip.text = element_text(size=18))

ggsave(here::here("figures/Percent_votes_vs_nightlight_gt30_scatterplot.png"),g,width=7,height = 5)




###### 4.5 - TURNOUT VS NIGTHLIGHT #####


NL_trends <- trends_nightlight_mean  %>% 
  select(index.data,starts_with("NL.trend")) %>% 
  pivot_longer(-index.data,names_to = "var",values_to = "n") %>% 
  separate(var,c("drop","year"),sep="_") %>%
  select(-drop)

percentages <- data %>% select(index,starts_with("turnout_"))
# Now, compute the percentages

# percentages %<>% mutate(percentage_2006=kabila.votes_2006/total.votes_2006,
#                         percentage_2011=kabila.votes_2011/total.votes_2011,
#                         percentage_2018=ramazani.votes_2018/total.votes_2018
# ) %>% select(index,starts_with("perc")) %>% ungroup

# Pivot the data to plot one map per year
percentages %<>%  pivot_longer(cols=-c(index),names_to="var",values_to="percent")

percentages %<>% separate(var,c("drop","year"),sep="_")

# percentages %<>% mutate(percent=percent*100) %>% select(-drop)

to.plot <- NL_trends %>% full_join(percentages,by = c("index.data"="index", "year"))


g <- to.plot %>% rowwise() %>% mutate(percent=min(percent,1)) %>% ungroup %>% ggplot(aes(x=n,y=percent)) + 
  geom_point(alpha=0.2,size=0.5) + 
  geom_smooth(method="glm",method.args=list(family="binomial"),se=FALSE, formula=y~x) + 
  xlab("# Nighlight trends before election")+
  ylab("Turnout (%)")+
  scale_y_continuous(labels=scales::percent)+
  facet_wrap(~year,ncol=2) + 
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size=12),
        strip.text = element_text(size=18))

ggsave(here::here("figures/Turnout_vs_nightlight_scatterplot.png"),g,width=7,height = 5)



NL_trends <- trends_nightlight_gt30_mean  %>% 
  select(index.data,starts_with("NL.trend")) %>% 
  pivot_longer(-index.data,names_to = "var",values_to = "n") %>% 
  separate(var,c("drop","year"),sep="_") %>%
  select(-drop)

percentages <- data %>% select(index,starts_with("turnout_"))
# Now, compute the percentages

# percentages %<>% mutate(percentage_2006=kabila.votes_2006/total.votes_2006,
#                         percentage_2011=kabila.votes_2011/total.votes_2011,
#                         percentage_2018=ramazani.votes_2018/total.votes_2018
# ) %>% select(index,starts_with("perc")) %>% ungroup

# Pivot the data to plot one map per year
percentages %<>%  pivot_longer(cols=-c(index),names_to="var",values_to="percent")

percentages %<>% separate(var,c("drop","year"),sep="_")

# percentages %<>% mutate(percent=percent*100) %>% select(-drop)

to.plot <- NL_trends %>% full_join(percentages,by = c("index.data"="index", "year"))


g <- to.plot  %>% rowwise() %>% mutate(percent=min(percent,1)) %>% ungroup %>% ggplot(aes(x=n,y=percent)) + 
  geom_point(alpha=0.2,size=0.5) + 
  geom_smooth(method="glm",method.args=list(family="binomial"),se=FALSE, formula=y~x) + 
  xlab("# Nighlight gt 30 trends before election")+
  ylab("Turnout (%)")+
  scale_y_continuous(labels=scales::percent)+
  facet_wrap(~year,ncol=2) + 
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size=12),
        strip.text = element_text(size=18))

ggsave(here::here("figures/Turnout_vs_nightlight_gt30_scatterplot.png"),g,width=7,height = 5)



##### 5. TURNOUT  #####

rm(list=ls())

load(here::here("results/data.RData"))


##### 5.1 MAP #####

# As we have merged some regions when matching the map (for example Kinshasa) we need to aggregate the data
# On the merged regions before computing the percentages.

# total.votes <- data %>% left_join(data.map.index,by=c(index="index.data")) %>% select(index.map,total.votes_2006,total.votes_2011,total.votes_2018)  %>% 
#   group_by(index.map) %>% summarise(across(starts_with("total.votes"),~sum(.,na.rm=TRUE)),.groups="drop")
# 
# kabila.votes <- data %>% left_join(data.map.index,by=c(index="index.data")) %>% select(index.map,kabila.votes_2006,kabila.votes_2011,ramazani.votes_2018) %>% 
#   group_by(index.map) %>% summarise(across(matches("votes"),~sum(.,na.rm=TRUE)),.groups="drop")

# percentages <- kabila.votes %>% full_join(total.votes,by="index.map")

percentages <- data %>% select(index,starts_with("turnout_"))

# Now, compute the percentages

# percentages %<>% mutate(percentage_2006=kabila.votes_2006/total.votes_2006,
#                         percentage_2011=kabila.votes_2011/total.votes_2011,
#                         percentage_2018=ramazani.votes_2018/total.votes_2018
# ) %>% select(index,starts_with("perc")) %>% ungroup

# Pivot the data to plot one map per year
percentages %<>%  pivot_longer(cols=-c(index),names_to="var",values_to="percent")

percentages %<>% separate(var,c("drop","year"),sep="_")

percentages %<>% mutate(percent=percent*100)

# Lakes are not administrative units, we will show them as NAs

percentages %<>% bind_rows(data.frame(index="administrative unit not available",year=c("2006","2011","2018")))

# Join percentages and shapefile data



percentages.map <- congo.territoire.borders %>% full_join(percentages,by=c("index.data"="index"))

# percentages.map %<>% filter(year==2011)

# Extract lakes because we are plotting them in a different color

lakes <- percentages.map %>% filter(index.data =="administrative unit not available")

percentages.map %<>% filter(index.data !="administrative unit not available")



# Plot and save the figure

if(!file.exists("figures/Turnout_maps.png")){
  g <- ggplot(percentages.map,aes(fill=percent,color="")) +
    geom_sf() + 
    geom_sf(data = lakes,fill="white") +
    scale_color_manual(values="gray30") +
    scale_fill_gradient(name="Turnout (%)",guide="colourbar",na.value="gray50")+
    guides(colour=guide_legend("No data", override.aes=list(color="gray50",fill="gray50")))+
    facet_wrap(~year,ncol=2) + 
    theme_void()+
    theme(legend.position = c(0.75,0.25),
          strip.text = element_text(size=18),
          legend.text = element_text(size=12),
          legend.title = element_text(size=14)) 
  
  ggsave(here::here("figures/Turnout_maps.png"),g)
  
}

##### 5.2 DIFFERENCE MAPS ######

percentage.differences <- percentages.map %>% select(index.data,year,percent) %>% as.data.frame() %>% select(-geometry) %>% pivot_wider(names_from = "year",values_from = "percent") %>%
  mutate(`2006-2011`=`2011`-`2006`,`2011-2018`=`2018`-`2011`) %>% select(-c(`2006`,`2011`,`2018`)) %>% pivot_longer(cols = -index.data,names_to = "period",values_to = "difference")

percentage.differences %<>% bind_rows(data.frame(index.data="administrative unit not available",period=c("2006-2011","2011-2018")))

percentage.differences.map <- congo.territoire.borders %>% full_join(percentage.differences,by="index.data")
lakes <- percentage.differences.map %>% filter(index.data =="administrative unit not available")

percentage.differences.map %<>% filter(index.data !="administrative unit not available")

if(!file.exists("figures/Turnout_diff_maps.png")){
  g <- ggplot(percentage.differences.map,aes(fill=difference,color="")) +
    geom_sf() + 
    geom_sf(data = lakes,fill="white") +
    scale_color_manual(values="gray30") +
    scale_fill_gradient2(name="Turnout Change",guide="colourbar",na.value="gray50",)+
    guides(colour=guide_legend("No data", override.aes=list(color="gray50",fill="gray50")))+
    facet_wrap(~period,ncol=2) + 
    theme_void()+
    theme(legend.position = "bottom",
          strip.text = element_text(size=18),
          legend.text = element_text(size=12),
          legend.title = element_text(size=14)) 
  
  ggsave(here::here("figures/Turnout_diff_maps.png"),g)
  
}


g <- ggplot(percentage.differences,aes(x=difference)) +geom_histogram(bins = 1+3.322*log10(nrow(percentage.differences)),color="black",fill="red") +
  facet_wrap(~period,ncol=2) + xlab("Change in turnout")

ggsave(here::here("figures/Turnout_diff_histograms.png"),g,width = 5,height = 3)

##### 5.3 MISSING PERCENTAGE DATA TABLE #####

missing.data.table <- percentages.map %>% 
  filter(is.na(percent)) %>% 
  data.frame() %>% select(year,index.data)  %>% 
  select(region=index.data,year) %>% 
  mutate(region=str_to_title(region)) %>% 
  arrange(year,region) %>% distinct %>% flextable() %>%
  merge_v(j=1) %>% hline(i=5,border = fp_border()) %>% autofit()

doc <- read_docx()



doc %<>% body_add_par("Missing data",style="table title")
doc %<>% body_add_flextable(missing.data.table,align = "center")


print(doc,target=here::here("tables/Missing.turnout.docx"))

