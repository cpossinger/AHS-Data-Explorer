library(foreach)
library(tibble)
library(ggplot2)
library(shiny)
library(dplyr)
library(magrittr)
library("DT")
library(tidyr)
library(readxl)
library(purrr)
library(readr)
library(fs)
library(stringr)
library(RCurl)
library(xtable)
library(broom)
library(treemapify)
library(reshape2)
#library(devtools)
library(plyr)
library(plotly)
#### Paths ####

#base_dir <- "S:/SHARED/RESEARCH/Projects/Volunteering"
#base_dir <- "C:\\Users\\The Boys\\Documents\\VolR"
#base_dir <- "C:\\Users\\19097\\OneDrive\\Documents\\Vol R\\fneep 5"
base_dir <- getwd()
data_dir <- file.path(base_dir)
setwd(base_dir)
food_group_paths <- list( nonoverlapping = file.path(data_dir, "All Food Groups\\Food Groups - Non-overlapping"),
                          overlapping = file.path(data_dir, "All Food Groups\\Food Groups"))
#### Load datasets ####

### : Main data ####
vol_ffqrecipe <- readRDS(file.path(data_dir, "vol_ffqrecipe.RDS"))
vol_allrecipes <- readRDS(file.path(data_dir, "vol_allrecipes.RDS"))
vol_nutr      <- readRDS(file.path(data_dir, "vol_nutr.RDS"))
vol_dietary   <- readRDS(file.path(data_dir, "vol_dietary_a.RDS"))
vol_dr_ingr   <- readRDS(file.path(data_dir,"vol_dr_ingr.RDS"))
vol_dr_subjects <- readRDS(file.path(data_dir,"vol_dr_subjects.RDS"))
write_in_patches <- read_csv(file.path(data_dir,"write-in-remapping-FISHOTH-OTHSWT.csv")) %>% mutate(across(everything(), tolower))
soy_resolutions <- read_csv(file.path(data_dir,"Resolutions for Soy beverages-20161207.csv")) %>% mutate(across(everything(), tolower))

###: tolower ###
vol_allrecipes %<>% mutate(across(one_of(c("recp_id","recp_name")), tolower))
vol_dietary %<>% mutate(across(where(is.character), tolower))
vol_ffqrecipe %<>% mutate(across(one_of(c("VNAME")), tolower))

# PATCH FOR WRITEIN META-RECIPES
meta_col <- "fruitcl"
patch_meta_col <- function(dietary, meta_col, allrecipes){
  dietary[[meta_col]] %<>% str_replace("hotdrink_r", "hotdrink_")
  recp_idx <- match(dietary[[meta_col]], allrecipes$recp_id)
  dietary[[meta_col]] <- if_else(is.na(recp_idx), dietary[[meta_col]], allrecipes$recp_name[recp_idx], NA_character_)
  return(dietary)
}

meta_cols <- c("fruitcl", "othhot1l", "fruitgl" )
for(varname in meta_cols){
  vol_dietary %<>% patch_meta_col(varname, vol_allrecipes)
}


recp_remap <- function(x, mapping){
  match_idx <- match(x, mapping$orig_recp_name)  
  x <- ifelse(is.na(match_idx), x, mapping$new_recp_name[match_idx])
  return(x)
}

vol_dietary %<>% 
  mutate(across(one_of("othsoy1l", "othsoy2l"), ~recp_remap(., soy_resolutions)),
         across(one_of("othswt1l", "othswt2l", "fishothl"), ~recp_remap(., write_in_patches)))

### : Food Group Data ####

load_food_groups <- function(fg_path){
  fg_files <- fs::dir_ls(path = fg_path, glob="*.csv")
  fg_names <- fg_files %>% basename %>% str_remove(".csv")
  fg_data  <- fg_files %>% map(read.csv)
  fg_data %<>% lapply(function(d){names(d) %<>% tolower; return(d)})
  names(fg_data) <- fg_names
  return(fg_data)
}

fg_data <- food_group_paths %>% lapply(load_food_groups)


### : Metadata ####
metadata      <- readr::read_csv(file.path(data_dir, "table-metadata-DIETARY-20180201-test.csv"))

res_cat_dict  <- readxl::read_xlsx(file.path(data_dir, "ResponseCategoryDictionary.xlsx"))

#### : Data Prep ####
metadata %<>% mutate(across(one_of(c("QName", "AmountCol","FrequencyCol","QLongName", "WriteInName")), tolower))

writein_metadata <- metadata %>% filter(Writein == 1)

# Create SectionAndPage column
section_format <- function(r){ paste0(r[1], r[-1] %>% as.vector %>% na.omit %>% unique) %>% paste0(collapse="/") %>% str_remove_all(" ")}
metadata$SectionAndPage <- metadata %>% select(SectionLetter, v1SectionPage, v2SectionPage, v3SectionPage) %>% apply(1, section_format)
metadata %<>% mutate(v1SectionAndPage = paste0(SectionLetter, v1SectionPage) %>% str_remove(".NA"),
                     v2SectionAndPage = paste0(SectionLetter, v2SectionPage) %>% str_remove(".NA"),
                     v3SectionAndPage = paste0(SectionLetter, v3SectionPage) %>% str_remove(".NA")) 

# List of pages for Pages Tab
page.list <- metadata %>% select(matches("v.SectionAndPage")) %>% unlist %>% unique %>% str_sort(numeric = T) %>% setdiff("")

# Clean up metadata table
metadata %<>%
  select(QName, ResponseCategory, AmountCol,FrequencyCol,SectionLetter, WriteInName, Writein, Frequency, Amount, SectionAndPage,Subsection,QLongName)


vol_ffqrecipe <- vol_ffqrecipe %>% as.data.frame


# || Freq/Response Table ####
# Table for conversion of response value to frequency value
#freq_conv_table <- res_cat_dict %>% select(ResponseCategory, starts_with("N"))
freq_conv_table <- plyr::alply(res_cat_dict,1, function(d){d %>% select(starts_with("N")) %>% as.matrix %>% as.vector %>% na.omit})

# Helper functions #### 

get_freq <- function(resp_cat, resp, impute = NA) {
  
  if(!is.na(resp_cat) && !is.na(resp) ){
    if(resp == 0){value = freq_conv_table[[resp_cat]][2]}     
    else{         value = freq_conv_table[[resp_cat]][as.integer(resp)]} 
  }else{
    value = impute
  }
  return(value)
} 

get_freq_vec <- Vectorize(get_freq, vectorize.args = c("resp_cat", "resp"))



writein_col <- metadata %>% filter(Writein == 1) %$% QName  %>% subset( . %in% names(vol_dietary))

ignored_recp_names <- c("", "blank", "Blank", "No Write In", "no write in", "ignore", "no write in", "No Write In Test", "No Write-in") #%>% tolower
ignored_recp_names <- c(ignored_recp_names, ignored_recp_names %>% tolower) %>% unique

merge_writein_nutr <- function(writein_col, dietary, allrecipes){
  dietary %>%
    select(vid, one_of(writein_col)) %>%
    `names<-`(c("vid", "recp_name")) %>%
    left_join(allrecipes) %>%
    mutate(writeincol = writein_col) %>%
    filter(!is.na(recp_name) & !(recp_name %in% ignored_recp_names)) %>%
    left_join(metadata %>% select(SectionLetter,writeincol = QName,FrequencyCol,AmountCol,Subsection)) %>%
    left_join(metadata %>% select(ResponseCategory, QName), by=c("FrequencyCol" = "QName"))
  
  
  
}



nutrient.list <- names(vol_nutr) %>% setdiff("vid") 

nutrient.list.g <- union(names(vol_ffqrecipe["gram"]),nutrient.list)

diet.list <- names(vol_dietary)
vol_recipe <- vol_ffqrecipe %>% group_by(VNAME,fcmb_name) %>% dplyr::summarise(across(one_of(nutrient.list), sum)) %>% ungroup


fcmb_select <- vol_ffqrecipe %>% group_by(VNAME,fcmb_name) %>% dplyr::summarise(across(one_of(nutrient.list.g), sum)) %>% ungroup

fcmb_select %<>% left_join(metadata[,c("SectionAndPage","QName")],by = c("VNAME" = "QName"))
fcmb_select %<>% select(SectionAndPage, everything()) 

recp_select <- vol_ffqrecipe %>% group_by(VNAME,recp_id, recp_name) %>% dplyr::summarise(across(one_of(nutrient.list.g), sum)) %>% ungroup



all_recp_select <- vol_allrecipes %>% group_by(recp_id, recp_name) %>% dplyr::summarise(across(one_of(nutrient.list.g), sum)) %>% ungroup

writein_list <- lapply(writein_col,merge_writein_nutr, dietary=vol_dietary, allrecipes=all_recp_select)

writein_data <- do.call(rbind, writein_list) %>% arrange(vid)
writein_data$FrequencyCol %<>% tolower
writein_data$AmountCol %<>% tolower

writein_sum <- writein_data %>% group_by(vid,SectionLetter) %>% dplyr::summarize(across(one_of(nutrient.list.g),sum,na.rm = TRUE))

writein_sum_select <- writein_sum %>% group_by(vid) %>% dplyr::summarize(across(one_of(nutrient.list.g), sum, na.rm = TRUE))



writein_amt <- vol_dietary %>% select(na.exclude(writein_data$AmountCol),vid) 
writein_freq <- vol_dietary %>% select(na.exclude(writein_data$FrequencyCol),vid) 

writein_amt_freq <- vol_dietary %>% select(na.exclude(writein_data$AmountCol),na.exclude(writein_data$FrequencyCol),vid) 


# : Callbacks Data Prep ####

#rc_subject_table <- vol_dr_ingr %>% group_by(vid) %>% dplyr::summarise(across(one_of(nutrient.list.g),sum,na.rm = TRUE)) %>% ungroup
rc_recall_table  <- vol_dr_ingr %>% group_by(vid, recall_set, dayofweek) %>% dplyr::summarise(across(one_of(nutrient.list.g), sum, na.rm = TRUE)) %>% ungroup
rc_recipe_table  <- vol_dr_ingr %>% group_by(vid, dayofweek, recall_set, mealname, recp_id, recp_name) %>% dplyr::summarise(across(one_of(nutrient.list.g),sum,na.rm = TRUE)) %>% ungroup

rc_sum_table <- rc_recall_table %>%
  mutate(across(one_of(nutrient.list.g), ~ifelse(dayofweek == "W",.x*5,.x))) %>% 
  group_by(vid, recall_set) %>% dplyr::summarize(across(one_of(nutrient.list.g), sum, na.rm = TRUE)) %>%
  mutate(across(one_of(nutrient.list.g), ~.x / 7))  %>% data.frame

rc_avg_table <- rc_sum_table %>% 
  group_by(vid) %>% 
  dplyr::summarize(across(one_of(nutrient.list.g), mean, na.rm=T)) %>%
  mutate(recall_set = "Avg") %>% 
  select(vid, recall_set, one_of(nutrient.list.g)) %>% data.frame

rc_sum_table %>%
  mutate(recall_set = as.character(recall_set)) %>%
  rbind(rc_avg_table) %>% 
  select(vid, `Week`=recall_set, one_of("kcal")) %>% 
  pivot_wider(id_cols = "vid", names_from = "Week", values_from="kcal")
