#### IMPORTANT NOTE #####
# This script is not meant to be run as a stand alone script, but sourced from 
# 003 - update manuscript.R
# Please make sure that you run the script 002 - models.R first.

#### TABLES ####

##### TABLE 1 #####

# Convert ged201 to a data frame and group by conflict type, then summarize counts and deaths
conflict.types.table <- ged201 %>%
  as.data.frame() %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(n = dplyr::n(), best = sum(best), .groups = "drop") %>%
  dplyr::rename(`Conflict category` = type)

# Filter data for conflicts involving state actors within a specified date range
conflict.types.table.by.state <- ged201 %>%
  as.data.frame() %>%
  dplyr::filter(date_start >= lubridate::ymd("2001-01-17") & date_end <= lubridate::ymd("2018-12-30")) %>%
  dplyr::filter(stringr::str_starts(type, "State"))

# Group by conflict type and side_a, then summarize counts and deaths
conflict.types.table.by.state %<>%
  dplyr::group_by(type, side_a) %>%
  dplyr::summarise(n = dplyr::n(), best = sum(best), .groups = "drop")

# Rename columns for clarity
conflict.types.table.by.state %<>%
  dplyr::rename(State = side_a) %>%
  dplyr::rename(`Conflict category` = type)

# Calculate total conflicts and deaths
total <- conflict.types.table %>%
  dplyr::summarise(across(where(is.numeric), sum)) %>%
  dplyr::mutate(`Conflict category` = "Total")

# Combine conflict types and state-specific conflicts into a single table
conflicts.table <- conflict.types.table %>%
  dplyr::slice(1:3) %>%
  dplyr::bind_rows(conflict.types.table.by.state %>% dplyr::slice(1:3)) %>%
  dplyr::bind_rows(conflict.types.table %>% dplyr::slice(4)) %>%
  dplyr::bind_rows(conflict.types.table.by.state %>% dplyr::slice(4:8)) %>%
  dplyr::bind_rows(total) %>%
  dplyr::select(`Conflict category`, ` ` = State, `Conflict events` = n, Deaths = best)

# Blank out certain "Conflict category" rows for formatting
conflicts.table$`Conflict category`[c(4:6,8:12)] <- ""

# Create a flextable and apply formatting
Table_1 <- conflicts.table %>%
  flextable::flextable() %>%
  flextable::autofit() %>%
  flextable::hline(i = 12, border = officer::fp_border(width = 2)) %>%
  flextable::hline(i = c(3, 7), border = officer::fp_border(width = 1))



##### TABLE 3a ####

# Load the pre-saved RData file containing model results
load("results/Table3a_models.RData")



# Create a list of models to be printed
models.to.print <- list(
  model_t3a_1_1, model_t3a_1_2, model_t3a_1_3, model_t3a_1_4,
  model_t3a_1_5, model_t3a_1_6, model_t3a_1_7, model_t3a_1_8,
  model_t3a_1_9, model_t3a_1_10, model_t3a_1_11, model_t3a_1_12,
  model_t3a_1_13, model_t3a_1_14, model_t3a_1_15, model_t3a_1_16
)

# In order to stargazer to recognize the models, we need to fix their calls

models.to.print %<>% purrr::modify(\(model) {model$call[1] <- call("plm"); model})

# Create a vector of model names to be used in the table
model_names <- c(
  "Total Deaths", "Log\nTotal Deaths", "Total Conflicts", "Log\nTotal Conflicts",
  "Total Deaths\nno NL", "Log Total Deaths\nno NL", "Total Conflicts\nno NL", "Log Total Conflicts\nno NL",
  "ACLED\nTotal Deaths", "ACLED\nLog\nTotal Deaths", "ACLED\nTotal Conflicts", "ACLED\nLog\nTotal Conflicts",
  "ACLED\nTotal Deaths\nno NL", "ACLED\nLog Total Deaths\nno NL", "ACLED\nTotal Conflicts\nno NL", "ACLED\nLog Total Conflicts\nno NL"
)

# Calculate standard errors for each model using robust covariance matrix
models.to.print_se <- purrr::map(models.to.print, ~
                                   # Apply coeftest with robust standard errors (HC3 method)
                                   lmtest::coeftest(.x, sandwich::vcovHC(.x, method = "arellano", type = "HC3"))[, "Std. Error"]
)

# Calculate F-statistics and append significance stars
F.stat <- purrr::map(models.to.print, ~{
  # Get the model summary with robust standard errors
  summ <- summary(.x, vcov. = sandwich::vcovHC(.x, method = "arellano", type = "HC3"))
  # Extract the p-value and format the F-statistic with significance stars
  .p <- summ$fstatistic$p.value
  sprintf("%0.3f%s", summ$fstatistic$statistic,
          dplyr::case_when(
            .p < 0.01 ~ "***",
            .p < 0.05 ~ "**",
            .p < 0.1 ~ "*",
            TRUE ~ ""
          ))
}) %>%
  unlist() %>%  # Flatten the list into a vector
  c("F Statistic", .)  # Add label "F Statistic" at the beginning

# Calculate degrees of freedom (df) for each model
df <- purrr::map(models.to.print, ~{
  # Get the model summary with robust standard errors
  summ <- summary(.x, vcov. = sandwich::vcovHC(.x, method = "arellano", type = "HC3"))
  # Extract and combine degrees of freedom
  paste(summ$fstatistic$parameter, collapse = ";")
}) %>%
  unlist() %>%  # Flatten the list into a vector
  c("df", .)  # Add label "df" at the beginning

# Generate the table using stargazer, suppressing warnings
Table_3a <- suppressWarnings(
  # Create the table in HTML format
  stargazer::stargazer(
    models.to.print, type = "html",
    se = models.to.print_se,
    dep.var.caption = "Votes share",  # Set the dependent variable caption
    omit.stat = "f",  # Omit the default F-statistic
    add.lines = list(F.stat, df)  # Add the custom F-statistic and df lines
  )
) %>%
  paste0(collapse = "")  # Combine the HTML output into a single string

# Convert the HTML table to a dataframe
Table_3a <- xml2::read_html(Table_3a) %>%
  rvest::html_table() %>%
  as.data.frame() %>%
  dplyr::slice(-c(1:6))  # Remove the first 6 rows

# Add the model names as column headers
table_names <- c(" ", model_names)
Table_3a <- magrittr::set_names(Table_3a, table_names)

# Identify and remove empty lines in the table
empty_lines <- Table_3a %>%
  dplyr::transmute(across(everything(), ~nchar(.) == 0)) %>%
  dplyr::rowwise() %>%
  dplyr::transmute(all(dplyr::c_across(everything()))) %>%
  dplyr::pull(1) %>%
  which()
Table_3a <- dplyr::slice(Table_3a, -empty_lines)

# Create a flextable and apply formatting
Table_3a %<>%
  flextable::flextable() %>%
  # Merge cells in the last row for all columns except the first
  flextable::merge_at(i = nrow(Table_3a), j = 2:ncol(Table_3a)) %>%
  # Apply text and paragraph styling for all parts of the table
  flextable::style(pr_t = officer::fp_text(font.size = 11), part = "all",
                   pr_p = officer::fp_par(line_spacing = 1, padding = 0, text.align = "center")) %>%
  # Apply specific styling for the header
  flextable::style(pr_t = officer::fp_text(font.size = 11), part = "header",
                   pr_p = officer::fp_par(line_spacing = 1, padding = 0, text.align = "center")) %>%
  # Add a horizontal line at the 10th row
  flextable::hline(i = 10, border = officer::fp_border()) %>%
  # Add vertical lines between the 1st and 9th columns
  flextable::vline(j = c(1, 9), border = officer::fp_border()) %>%
  # Automatically adjust column widths
  flextable::autofit() %>%
  # Fit the table to a width of 10.5 inches
  flextable::fit_to_width(10.5)



##### TABLE 3b ####

share <- data %>% select(index,label,starts_with("kabila.percent"),starts_with("ramazani.percent")) %>%
  pivot_longer(cols = -c(index,label),values_to = "votes_share") %>% separate(name,c("drop","year"),sep="_") %>% mutate(year=as.integer(year)) %>% select(-drop)



mean_nightlight <- nightlight_gt30_mean %>% mutate(nightlight_mean=nightlight_mean+0.01) %>% select(index=index.data,year,nightlight_mean) %>% 
  mutate(election=elections[as.character(year)]) %>%  group_by(index,election) %>% summarise(across(nightlight_mean,~mean(.,na.rm = TRUE)),.groups = "drop") %>% filter(!is.na(election))


DMSP_2012_13 <- nightlight_gt30_mean %>% mutate(nightlight_mean=nightlight_mean+0.01) %>% select(index=index.data,year,nightlight_mean) %>% filter(year %in% c(2012,2013)) %>% group_by(index) %>% summarise(across(nightlight_mean,~mean(.,na.rm = TRUE)),.groups = "drop") 

VIIR_percent_change <- nightlight_gt30_mean %>% mutate(nightlight_mean=nightlight_mean+0.01) %>% select(index=index.data,year,nightlight_mean) %>% filter(year %in% c(2014,2018)) %>% pivot_wider(names_from = year,values_from = nightlight_mean) %>% mutate(change=(`2018`-`2014`)/(`2014`))

VIIR_corrected <- DMSP_2012_13 %>% left_join(VIIR_percent_change,by="index") %>% mutate(nightlight_mean_corrected=nightlight_mean+(nightlight_mean)*change) %>% select(index,nightlight_mean=nightlight_mean_corrected) %>% mutate(election="2018")

mean_nightlight %<>% bind_rows(VIIR_corrected) %>% mutate(year=as.integer(election)) %>% select(-election)

share %<>% left_join(mean_nightlight,by=c("index","year"))



# Model 1

conflict_deaths_by_type <- conflict.aggregated_by_type %>% dplyr::filter(n.conflicts >0) %>% select(index=index.data,year,type,n.deaths) %>% mutate(type=paste0(type,"_deaths")) %>% pivot_wider(names_from = type,values_from = n.deaths)


to.model <- share %>% left_join(conflict_deaths_by_type,by=c("index","year")) %>% mutate(across(-c(index,year,votes_share,label),~replace_na(.,0))) %>% rename(region=label) %>% select(-index)

to.model <- plm::pdata.frame(to.model,index=c("region","year"),drop.index = FALSE)

model1_1 <- plm(votes_share ~ Non.state.vs.non.state_deaths + Foreign.vs.non.state_deaths + Non.state.vs.civilians_deaths + DRC.vs.non.state_deaths + DRC.vs.civilians_deaths +Foreign.vs.civilians_deaths,data=to.model,model="within",effect = "twoways")


# Model 2

conflict_events_by_type <- conflict.aggregated_by_type%>% dplyr::filter(n.conflicts >0) %>% select(index=index.data,year,type,n.conflicts) %>% mutate(type=paste0(type,"_events")) %>% pivot_wider(names_from = type,values_from = n.conflicts)


to.model <- share %>% left_join(conflict_events_by_type,by=c("index","year")) %>% mutate(across(-c(index,year,votes_share,label),~replace_na(.,0))) %>% rename(region=label) %>% select(-index)

to.model <- plm::pdata.frame(to.model,index=c("region","year"),drop.index = FALSE)

model1_2 <- plm(votes_share ~ Non.state.vs.non.state_events + Foreign.vs.non.state_events + Non.state.vs.civilians_events + DRC.vs.non.state_events + DRC.vs.civilians_events +Foreign.vs.civilians_events,data=to.model,model="within",effect = "twoways")


# Model 3

.summarise_var<- function(.x,.var){
  if(!is.null(.x)){
    
    .x %>% as.data.frame() %>%summarise(across(one_of(.var),~sum(.,na.rm = TRUE))) %>% pull(.var)
  }
}

conflict_deaths_by_casualty_type <- conflict.aggregated_by_type %>% dplyr::filter(n.conflicts >0) %>% mutate(n.deaths_a=map(conflict.data,~.summarise_var(.x,"deaths_a")),
                                                                                                             n.deaths_b=map(conflict.data,~.summarise_var(.x,"deaths_b")),
                                                                                                             n.deaths_civilians=map(conflict.data,~.summarise_var(.x,"deaths_civilians")),
                                                                                                             n.deaths_unknow=map(conflict.data,~.summarise_var(.x,"deaths_unknown"))
) %>%
  unnest(c(starts_with("n.deaths")))


conflict_deaths_by_casualty_type %<>% mutate(n.deaths_DRC_milit=case_when(str_detect(type,"DRC")~n.deaths_a,TRUE~0),
                                             n.deaths_foreign_milit=case_when(str_detect(type,"Foreign")~n.deaths_a,TRUE~0),
                                             n.deaths_non_state=case_when(type=="Non-state vs non-state"~n.deaths_a+n.deaths_b,
                                                                          type=="Non-state vs civilians"~n.deaths_a,
                                                                          str_ends(type,"non-state")~n.deaths_b,
                                                                          TRUE~0))



conflict_deaths_by_casualty_type %<>% select(index=index.data,year,n.deaths,n.deaths_DRC_milit,n.deaths_foreign_milit,n.deaths_non_state,n.deaths_civilians,n.deaths_unknow)

conflict_deaths_by_casualty_type %<>% group_by(index,year) %>% summarise(across(everything(),~sum(.,na.rm = FALSE)),.groups="drop")



to.model <- share %>% left_join(conflict_deaths_by_casualty_type,by=c("index","year")) %>% mutate(across(-c(index,year,votes_share,label),~replace_na(.,0))) %>% rename(region=label) %>% select(-index)

to.model <- plm::pdata.frame(to.model,index=c("region","year"),drop.index = FALSE)


model1_3 <- plm(votes_share ~ n.deaths_DRC_milit + n.deaths_foreign_milit +  n.deaths_non_state + n.deaths_civilians + n.deaths_unknow,data=to.model,model="within",effect = "twoways")

# Table

models.to.print <- list(model1_1,model1_2,model1_3)

model_names <- c("Deaths by type","Conflicts by type","Conflict Deaths by side")

models.to.print_se <- map(models.to.print,~coeftest(.x, vcovHC(.x, method="arellano", type="HC3"))[,"Std. Error"])



F.stat <-map(models.to.print,~{
  summ <- summary(.x,vcov.=vcovHC(.x, method="arellano", type="HC3"))
  
  .p <- summ$fstatistic$p.value
  sprintf("%0.3f%s",summ$fstatistic$statistic,
          case_when(.p<0.01 ~"***",
                    .p<0.05 ~"**",
                    .p<0.1~"*",
                    TRUE ~""))
  
  
})  %>% unlist %>% c("F Statistic",.)

df <-map(models.to.print,~{
  summ <- summary(.x,vcov.=vcovHC(.x, method="arellano", type="HC3"))
  
  paste(summ$fstatistic$parameter,collapse=";")
  
  
})  %>% unlist %>% c("df",.)


Table_3b <- suppressWarnings(stargazer::stargazer(models.to.print,type="html",
                                                  se=models.to.print_se,
                                                  
                                                  column.labels=model_names,dep.var.caption = "Votes share",
                                                  omit.stat = "f",
                                                  add.lines = list(F.stat,
                                                                   df))) %>% paste0(collapse = "")

Table_3b <- read_html(Table_3b) %>% html_table() %>% as.data.frame() %>% slice(-c(1:5))
table_names <- as.vector(Table_3b %>% slice(1) %>% unlist)
table_names[1] <- " "
Table_3b <- set_names(Table_3b,table_names) %>% slice(-1)

empty_lines <- Table_3b %>% transmute(across(everything(),~nchar(.)==0)) %>% rowwise() %>% transmute(all(c_across(everything()))) %>% pull(1) %>% which()

Table_3b %<>% slice(-empty_lines)

Table_3b %<>% flextable() %>% merge_at(i=nrow(Table_3b),j=2:ncol(Table_3b)) %>%
  style(pr_t=fp_text(font.size = 7),part = "all",pr_p = fp_par(line_spacing = 1,padding = 0))  %>% hline(i = 34,border = officer::fp_border())%>% autofit() %>% flextable::fit_to_width(6.49)




