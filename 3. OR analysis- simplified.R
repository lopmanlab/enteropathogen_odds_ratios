#######################################################################
#
# WHO Burden of Diseases OR analysis- Lit Review Data
# Data analysis
# Programer: Julia Baker
# Last updated: Dec 14, 2020
#   
#######################################################################

.libPaths()
.Library

#########################################
####            DATA PREP            ####
#########################################

# load libraries
  library(tidyverse)
  library(data.table)
  library(dplyr)
  library(meta)
  library(metafor)
  library(foreach)
  library(devtools)
  library(magicfor)
  library(readxl)
  library(ggplot2)
  

#########################################################################
####        SUBSETTING DATA INTO PATHOGEN-SPECIFIC DATASETS          ####
#########################################################################

# load dataframe for analysis
  load("case_control_clean.Rda")
  
  # dataframe for each pathogen (not further stratified)
    pathogen_data_list <- unique(case_control_clean$path)
      for (v in pathogen_data_list) {
        assign(paste0("data_", as.character(v)), case_control_clean %>% filter (path == v), envir = .GlobalEnv)
      }  

 
    
########################################################
####        OUTLIERS/INFLUENCE DIAGNOSTICS          ####
########################################################
# resource: http://www.metafor-project.org/doku.php/plots:plot_of_influence_diagnostics
  
# random effects meta-analysis loop
  meta_analysis_loop_unadj <- function(data){
    rma(yi = log_ee, vi = var,
        method = "DL", 
        data = data, 
        slab=paste(author_year_repeat))
  }   
  
  
  # data for loop
    data_list_unadj <- c('data_salm', 'data_shig', 'data_campy', 'data_chol', 'data_etec', 'data_epec', 'data_rota', 'data_noro',
                         'data_sapo', 'data_astro', 'data_adeno', 'data_giard', 'data_crypto', 'data_ehist', 'data_aero')
  # running m-a loop for each pathogen
    ma_path_unadj <- list()
      for(i in seq_along(data_list_unadj)){ 
        ma_path_unadj[[i]] <- meta_analysis_loop_unadj(get(data_list_unadj[i]))
        }
  # calculate and plot influence diagnostics
    for (i in 1:15){
      plot(influence(ma_path_unadj[[i]]))
    }  
  
  # examining each and identifying studies to drop
    options(max.print=10000)
    # salm
      print(influence(ma_path_unadj[[1]]))
      # drop: Hasan-2006-134-44
      # drop2: Chang-2017-12-5; Cardemil-2017-27-3   
    # shig
      print(influence(ma_path_unadj[[2]]))
      # drop: Hasan-2006-134-2; Hasan-2006-134-38; Hasan-2006-134-36 
      # drop2: GEMS-NA-NA-206; GEMS-NA-NA-205 
    # campy
      print(influence(ma_path_unadj[[3]]))
      # drop: Schiaffino-2019-66-2; Schiaffino-2019-66-1; Qadri-2007-112-1; Hasan-2006-134-39 
      # drop2: none
    # chol
      print(influence(ma_path_unadj[[4]])) 
      # drop: Bodhidatta-2019-30-3
      # drop2: Bodhidatta-2019-30-3
    # etec
      print(influence(ma_path_unadj[[5]])) 
      # drop: Hasan-2006-134-26
      # drop2: none
    # epec
      print(influence(ma_path_unadj[[6]])) 
      # drop: Santos-2019-179-1 
      # drop2: Santos-2019-179-1 
    # rota
      print(influence(ma_path_unadj[[7]])) 
      # drop: Nhampossa-2015-131-8; Iturriza-Gómara-2019-108-1; Randremanana-2012-116-8; Krumkamp-2015-183-40
      #       Krumkamp-2015-183-35; Krumkamp-2015-183-38; Li-2015-68-1; Georges-Courbot-1990-122-6 
      # drop2: none
    # noro
      print(influence(ma_path_unadj[[8]])) 
      # drop: My-2013-91-1; Lopman-2014-67-2; Lopman-2014-67-3; Lopman-2014-67-4
      # drop2: My-2013-91-1
    # sapo
      print(influence(ma_path_unadj[[9]])) 
      # drop:  GEMS-NA-NA-181
      # drop2: GEMS-NA-NA-180
    # astro
      print(influence(ma_path_unadj[[10]])) 
      # drop: Platts-Mills-2014-125-2
      # drop2: Platts-Mills-2014-125-2
    # adeno
      print(influence(ma_path_unadj[[11]])) 
      # drop: Iturriza-Gómara-2019-108-2
      # drop2: Iturriza-Gómara-2019-108-2
    # giard
      print(influence(ma_path_unadj[[12]])) 
      # drop: Bodhidatta-2010-31-10; Bodhidatta-2010-31-35; Bhandari-1999-25-1; Krumkamp-2015-183-30;
      #       Haque-2009-96-14; Haque-2009-96-15; Haque-2009-96-18
      # drop2: Bodhidatta-2010-31-35
    # crypto
      print(influence(ma_path_unadj[[13]])) 
      # drop: MALED-NA-NA-91 
      # drop2: MALED-NA-NA-91 
    # ehist
      print(influence(ma_path_unadj[[14]])) 
      # drop: Krumkamp-2015-183-7
      # drop2: Krumkamp-2015-183-6; Krumkamp-2015-183-7; Mitra-2016-180-2 
    # aero
      print(influence(ma_path_unadj[[15]])) 
      # drop: none
      # drop2: none
      
# variable for influential observation (1 = influential; 0 = not)
  case_control_clean$infl <-  ifelse((case_control_clean$path == "salm" & case_control_clean$author_year_repeat == "Hasan-2006-134-44"), 1,
                              ifelse((case_control_clean$path == "shig" & case_control_clean$author_year_repeat == "Hasan-2006-134-2"), 1,
                              ifelse((case_control_clean$path == "shig" & case_control_clean$author_year_repeat == "Hasan-2006-134-38"), 1,
                              ifelse((case_control_clean$path == "shig" & case_control_clean$author_year_repeat == "Hasan-2006-134-36"), 1,
                              ifelse((case_control_clean$path == "campy" & case_control_clean$author_year_repeat == "Schiaffino-2019-66-2"), 1,
                              ifelse((case_control_clean$path == "campy" & case_control_clean$author_year_repeat == "Schiaffino-2019-66-1"), 1,
                              ifelse((case_control_clean$path == "campy" & case_control_clean$author_year_repeat == "Qadri-2007-112-1"), 1,
                              ifelse((case_control_clean$path == "campy" & case_control_clean$author_year_repeat == "Hasan-2006-134-39"), 1,
                              ifelse((case_control_clean$path == "chol" & case_control_clean$author_year_repeat == "Bodhidatta-2019-30-3"), 1,
                              ifelse((case_control_clean$path == "etec" & case_control_clean$author_year_repeat == "Hasan-2006-134-26"), 1,
                              ifelse((case_control_clean$path == "epec" & case_control_clean$author_year_repeat == "Santos-2019-179-1"), 1,
                              ifelse((case_control_clean$path == "rota" & case_control_clean$author_year_repeat == "Nhampossa-2015-131-8"), 1,
                              ifelse((case_control_clean$path == "rota" & case_control_clean$author_year_repeat == "Iturriza-Gómara-2019-108-1"), 1,
                              ifelse((case_control_clean$path == "rota" & case_control_clean$author_year_repeat == "Krumkamp-2015-183-40"), 1,
                              ifelse((case_control_clean$path == "rota" & case_control_clean$author_year_repeat == "Krumkamp-2015-183-35"), 1,
                              ifelse((case_control_clean$path == "rota" & case_control_clean$author_year_repeat == "Krumkamp-2015-183-38"), 1,
                              ifelse((case_control_clean$path == "rota" & case_control_clean$author_year_repeat == "Li-2015-68-1"), 1,
                              ifelse((case_control_clean$path == "rota" & case_control_clean$author_year_repeat == "Georges-Courbot-1990-122-6"), 1,
                              ifelse((case_control_clean$path == "rota" & case_control_clean$author_year_repeat == "Randremanana-2012-116-8"), 1,
                              ifelse((case_control_clean$path == "noro" & case_control_clean$author_year_repeat == "My-2013-91-1"), 1,
                              ifelse((case_control_clean$path == "noro" & case_control_clean$author_year_repeat == "Lopman-2014-67-2"), 1,
                              ifelse((case_control_clean$path == "noro" & case_control_clean$author_year_repeat == "Lopman-2014-67-3"), 1,
                              ifelse((case_control_clean$path == "noro" & case_control_clean$author_year_repeat == "Lopman-2014-67-4"), 1,
                              ifelse((case_control_clean$path == "sapo" & case_control_clean$author_year_repeat == "GEMS-NA-NA-181"), 1,
                              ifelse((case_control_clean$path == "astro" & case_control_clean$author_year_repeat == "Platts-Mills-2014-125-2"), 1,
                              ifelse((case_control_clean$path == "adeno" & case_control_clean$author_year_repeat == "Iturriza-Gómara-2019-108-2"), 1,
                              ifelse((case_control_clean$path == "giard" & case_control_clean$author_year_repeat == "Bodhidatta-2010-31-10"), 1,    
                              ifelse((case_control_clean$path == "giard" & case_control_clean$author_year_repeat == "Bodhidatta-2010-31-35"), 1,  
                              ifelse((case_control_clean$path == "giard" & case_control_clean$author_year_repeat == "Bhandari-1999-25-1"), 1, 
                              ifelse((case_control_clean$path == "giard" & case_control_clean$author_year_repeat == "Krumkamp-2015-183-30"), 1,
                              ifelse((case_control_clean$path == "giard" & case_control_clean$author_year_repeat == "Haque-2009-96-14"), 1,     
                              ifelse((case_control_clean$path == "giard" & case_control_clean$author_year_repeat == "Haque-2009-96-15"), 1,     
                              ifelse((case_control_clean$path == "giard" & case_control_clean$author_year_repeat == "Haque-2009-96-18"), 1,    
                              ifelse((case_control_clean$path == "crypto" & case_control_clean$author_year_repeat == "MALED-NA-NA-91 "), 1,
                              ifelse((case_control_clean$path == "ehist" & case_control_clean$author_year_repeat == "Krumkamp-2015-183-7"), 1, 0)))))))))))))))))))))))))))))))))))     
                                         
  # variable for influential observation (2nd time around) (1 = influential; 0 = not)
  case_control_clean$infl2 <-  ifelse((case_control_clean$path == "salm" & case_control_clean$author_year_repeat == "Chang-2017-12-5"), 1,
                               ifelse((case_control_clean$path == "salm" & case_control_clean$author_year_repeat == "Cardemil-2017-27-3"), 1,
                               ifelse((case_control_clean$path == "shig" & case_control_clean$author_year_repeat == "GEMS-NA-NA-206"), 1,
                               ifelse((case_control_clean$path == "shig" & case_control_clean$author_year_repeat == "GEMS-NA-NA-205 "), 1,
                               ifelse((case_control_clean$path == "chol" & case_control_clean$author_year_repeat == "Bodhidatta-2019-30-3"), 1,
                               ifelse((case_control_clean$path == "epec" & case_control_clean$author_year_repeat == "Santos-2019-179-1"), 1,
                               ifelse((case_control_clean$path == "noro" & case_control_clean$author_year_repeat == "My-2013-91-1"), 1,
                               ifelse((case_control_clean$path == "sapo" & case_control_clean$author_year_repeat == "GEMS-NA-NA-180"), 1,
                               ifelse((case_control_clean$path == "astro" & case_control_clean$author_year_repeat == "Platts-Mills-2014-125-2"), 1,
                               ifelse((case_control_clean$path == "adeno" & case_control_clean$author_year_repeat == "Iturriza-Gómara-2019-108-2"), 1,
                               ifelse((case_control_clean$path == "giard" & case_control_clean$author_year_repeat == "Bodhidatta-2010-31-35"), 1,     
                               ifelse((case_control_clean$path == "crypto" & case_control_clean$author_year_repeat == "MALED-NA-NA-91 "), 1,
                               ifelse((case_control_clean$path == "ehist" & case_control_clean$author_year_repeat == "Krumkamp-2015-183-6"), 1, 
                               ifelse((case_control_clean$path == "ehist" & case_control_clean$author_year_repeat == "Krumkamp-2015-183-7"), 1,
                               ifelse((case_control_clean$path == "ehist" & case_control_clean$author_year_repeat == "Mitra-2016-180-2"), 1, 0)))))))))))))))
  
# re-save dataframe with this new variable
  save(case_control_clean,file="case_control_clean.Rdata")
 
 
################################################
####           FURTHER SUBSETTING           ####
################################################
  
# load dataframe for analysis
  load("case_control_clean.Rdata")
  
# dataframe with all observations (keeping outliers/influential) but dropping those with missing info
  # dropping observations with...
    # NA for variance
      case_control_full <- case_control_clean[!is.na(case_control_clean$var), ]
      # ~500 dropped here b/c of NA resulting from 0s in 2x2 table, typically among controls
    # NA for path
      case_control_full <- case_control_full[!is.na(case_control_full$path), ]
    # "other" for path
      case_control_full <- case_control_full[ which(case_control_full$path != "other"), ]
    # now down to 1225 observations
      write.csv(case_control_full,file="/Users/juliabaker/Box Sync/WHO BoD/OR Analysis/OR Deliverables/Julia/Data/case_control_full.csv")
      
  # dataframe without the outliers/influential observations
    case_control_limited <- case_control_full[ which(case_control_full$infl2 == 0), ]
    #case_control_limited <- case_control_full[ which(case_control_full$study_design == "case control"), ]
    # now down to 1251 observations
 
      
# each pathogen & strain
    pathogen_data_list <- unique(case_control_full$path)
      for (v in pathogen_data_list) {
        assign(paste0("data_", as.character(v)), case_control_full %>% filter (path == v), envir = .GlobalEnv)
        } 
    data_etec_st   <- case_control_full[ which(case_control_full$path_strain == "etec_st"), ]
    data_etec_lt   <- case_control_full[ which(case_control_full$path_strain == "etec_lt"), ]
    data_noro_gi   <- case_control_full[ which(case_control_full$path_strain == "noro_gi"), ]
    data_noro_gii  <- case_control_full[ which(case_control_full$path_strain == "noro_gii"), ]
    data_rota_pre  <- case_control_full[ which(case_control_full$path_strain == "rota_pre"),]
    data_rota_post <- case_control_full[ which(case_control_full$path_strain == "rota_post"),]
  
 # dataframe for each stratification (age group and child mortality level)
    
    # age group (young, mixed, old)
      age_group_list <- unique(case_control_full$age_group)
        for (v in age_group_list) {
          assign(paste0("data_", as.character(v)), case_control_full %>% filter (age_group == v), envir = .GlobalEnv)
          }
    
    # child_mort (very low, low, high, very low/low combined)
      data_vlow     <- case_control_full[ which(case_control_full$child_mort == "very low"), ]
      data_low      <- case_control_full[ which(case_control_full$child_mort == "low"), ]
      data_high     <- case_control_full[ which(case_control_full$child_mort == "high"), ]
      data_vlow_low <- case_control_full[ which((case_control_full$child_mort == "very low" | case_control_full$child_mort == "low")), ]
    
    # young age by child mortality
      data_young_vlow     <- data_young[ which(data_young$child_mort == "very low"), ]
      data_young_low      <- data_young[ which(data_young$child_mort == "low"), ]
      data_young_high     <- data_young[ which(data_young$child_mort == "high"), ]
      data_young_vlow_low <- data_young[ which((data_young$child_mort == "very low" | data_young$child_mort == "low")), ]
      
      # each pathogen within the age/child mort dataframes
        pathogen_data_list <- unique(case_control_full$path)
        pathogen_strain_data_list <- unique(case_control_full$path_strain)
        # young/very low
          for (v in pathogen_data_list) {
            assign(paste0("data_young_vlow_", as.character(v)), data_young_vlow %>% filter (path == v), envir = .GlobalEnv)
            }
          for (u in pathogen_strain_data_list) {
            assign(paste0("data_young_vlow_", as.character(u)), data_young_vlow %>% filter (path_strain == u), envir = .GlobalEnv)
            } 
        # young/low
          for (v in pathogen_data_list) {
            assign(paste0("data_young_low_", as.character(v)), data_young_low %>% filter (path == v), envir = .GlobalEnv)
            } 
          for (u in pathogen_strain_data_list) {
            assign(paste0("data_young_low_", as.character(u)), data_young_low %>% filter (path_strain == u), envir = .GlobalEnv)
            } 
        # young/high
          for (v in pathogen_data_list) {
            assign(paste0("data_young_high_", as.character(v)), data_young_high %>% filter (path == v), envir = .GlobalEnv)
            }
          for (u in pathogen_strain_data_list) {
            assign(paste0("data_young_high_", as.character(u)), data_young_high %>% filter (path_strain == u), envir = .GlobalEnv)
          } 
        # young/very low/low
          for (v in pathogen_data_list) {
            assign(paste0("data_young_vlow_low_", as.character(v)), data_young_vlow_low %>% filter (path == v), envir = .GlobalEnv)
            }
          for (u in pathogen_strain_data_list) {
            assign(paste0("data_young_vlow_low_", as.character(u)), data_young_vlow_low %>% filter (path_strain == u), envir = .GlobalEnv)
            } 
        # old/all child mortality levels
          for (v in pathogen_data_list) {
            assign(paste0("data_old_", as.character(v)), data_old %>% filter (path == v), envir = .GlobalEnv)
            }
          for (u in pathogen_strain_data_list) {
            assign(paste0("data_old_", as.character(u)), data_old %>% filter (path_strain == u), envir = .GlobalEnv)
            } 
 
            
            
#############################################################
####      PRELIMINARY ANALYSIS & DESCRIPTIVE STATS       ####
#############################################################


# number of unique studies  
  cc_unique_study_raw <- case_control[ which(case_control$redcap_repeat_instance == 1), ]
  cc_unique_study <- case_control_clean[ which(case_control_clean$redcap_repeat_instance == 1 & case_control_clean$dup_num == 1), ]  
 
        
  length(unique(case_control$ID_uniq_label))
  
  table(cc_unique_study_raw$design)
  table(cc_unique_study$design)
  
# year of publication, year study began/ended 
  summary(cc_unique_study$year_pub)
  summary(cc_unique_study$year_began) 
  summary(cc_unique_study$year_ended) 
    # median start date is 2003 (mean 2002)
    # will use 2003 child mortality data for countries
  
### Using "full" dataset ###
  # 1,285 observations
  
  # number of unique studies
    cc_full_unique_study <- case_control_full[ which(case_control_full$redcap_repeat_instance == 1), ]
    length(unique(case_control$author_year_title))
    length(unique(case_control_clean$author_year_title))
    length(unique(case_control_full$author_year_title))
    length(unique(case_control_full$redcap_repeat_instance))
    # 145
  
  # observations by pathogen
    obs_by_path <- data.frame(case_control_full %>% group_by(path) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
    # getting n (%) set up
      obs_by_path$n_perc_part1 <- paste(obs_by_path$n,format(round(obs_by_path$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
      obs_by_path$n_perc_part2 <- ")"
      obs_by_path$n_perc <- paste(obs_by_path$n_perc_part1, obs_by_path$n_perc_part2, sep="")
    # keeping only needed columns
      obs_by_path <- obs_by_path %>% select(path, n_perc)
    # flip dataset
      obs_by_path_wide <- obs_by_path %>% pivot_wider(names_from = c(path), values_from = c(n_perc))
      obs_by_path_wide$variable <- "total"
    
  # age group
    table(case_control_full$age_group, exclude=NULL)
    path_age <- data.frame(case_control_full %>% group_by(path, age_group) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
      # getting n (%) set up
      path_age$n_perc_part1 <- paste(path_age$n,format(round(path_age$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
      path_age$n_perc_part2 <- ")"
      path_age$n_perc <- paste(path_age$n_perc_part1, path_age$n_perc_part2, sep="")
      # keeping only needed columns
      path_age <- path_age %>% select(path, age_group, n_perc)
      # flip dataset
      path_age_wide <- path_age %>% pivot_wider(names_from = c(path), values_from = c(n_perc))
      names(path_age_wide)[names(path_age_wide) == "age_group"] <- "variable"
       
  # U5MR
    table(case_control_full$child_mort, exclude=NULL)
    path_child_mort <- data.frame(case_control_full %>% group_by(path, child_mort) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
    # getting n (%) set up
      path_child_mort$n_perc_part1 <- paste(path_child_mort$n,format(round(path_child_mort$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
      path_child_mort$n_perc_part2 <- ")"
      path_child_mort$n_perc <- paste(path_child_mort$n_perc_part1, path_child_mort$n_perc_part2, sep="")
    # keeping only needed columns
      path_child_mort <- path_child_mort %>% select(path, child_mort, n_perc)
    # flip dataset
      path_child_mort_wide <- path_child_mort %>% pivot_wider(names_from = c(path), values_from = c(n_perc)) 
      names(path_child_mort_wide)[names(path_child_mort_wide) == "child_mort"] <- "variable"
      
  # pathogen detection method
    table(case_control_full$det_meth_desc, exclude=NULL)
    path_det_meth <- data.frame(case_control_full %>% group_by(path, det_meth_desc) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
    # getting n (%) set up
      path_det_meth$n_perc_part1 <- paste(path_det_meth$n,format(round(path_det_meth$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
      path_det_meth$n_perc_part2 <- ")"
      path_det_meth$n_perc <- paste(path_det_meth$n_perc_part1, path_det_meth$n_perc_part2, sep="")
    # keeping only needed columns
      path_det_meth <- path_det_meth %>% select(path, det_meth_desc, n_perc)
    # flip dataset
      path_det_meth_wide <- path_det_meth %>% pivot_wider(names_from = c(path), values_from = c(n_perc)) 
      names(path_det_meth_wide)[names(path_det_meth_wide) == "det_meth_desc"] <- "variable"
    
  # study design
    table(case_control_full$study_design, exclude=NULL)
    path_study_design <- data.frame(case_control_full %>% group_by(path, study_design) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
    # getting n (%) set up
    path_study_design$n_perc_part1 <- paste(path_study_design$n,format(round(path_study_design$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
    path_study_design$n_perc_part2 <- ")"
    path_study_design$n_perc <- paste(path_study_design$n_perc_part1, path_study_design$n_perc_part2, sep="")
    # keeping only needed columns
    path_study_design <- path_study_design %>% select(path, study_design, n_perc)
    # flip dataset
    path_study_design_wide <- path_study_design %>% pivot_wider(names_from = c(path), values_from = c(n_perc)) 
    names(path_study_design_wide)[names(path_study_design_wide) == "study_design"] <- "variable"
   
  # merging data for pathogen table
    path_table <- rbind(obs_by_path_wide, path_age_wide, path_child_mort_wide, path_det_meth_wide, path_study_design_wide)
    path_table <- path_table[, c(16,1,3,11,12,14,2,4,5,8,9,13,15,6,7,10)]   
    
    
  # effect estimates table
    best_meas_design_only <- data.frame(case_control_full %>% group_by(best_meas) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
    
    best_meas_design <- data.frame(case_control_full %>% group_by(best_meas, design) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
    # getting n (%) set up
      best_meas_design$n_perc_part1 <- paste(best_meas_design$n,format(round(best_meas_design$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
      best_meas_design$n_perc_part2 <- ")"
      best_meas_design$n_perc <- paste(best_meas_design$n_perc_part1, best_meas_design$n_perc_part2, sep="")
    # keeping only needed columns
      best_meas_design <- best_meas_design %>% select(best_meas, design, n_perc)
    # flip dataset
      best_meas_design_wide <- best_meas_design %>% pivot_wider(names_from = c(best_meas), values_from = c(n_perc))
      best_meas_design_wide <- best_meas_design_wide[, c(1,6,5,4,3,2)]
    
    best_meas_matching1 <- data.frame(case_control_full %>% group_by(best_meas, matching___1) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
    # getting n (%) set up
      best_meas_matching1$n_perc_part1 <- paste(best_meas_matching1$n,format(round(best_meas_matching1$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
      best_meas_matching1$n_perc_part2 <- ")"
      best_meas_matching1$n_perc <- paste(best_meas_matching1$n_perc_part1, best_meas_matching1$n_perc_part2, sep="")
    # keeping only needed columns
      best_meas_matching1 <- best_meas_matching1 %>% select(best_meas, matching___1, n_perc)
    # flip dataset
      best_meas_matching1_wide <- best_meas_matching1 %>% pivot_wider(names_from = c(best_meas), values_from = c(n_perc))
      best_meas_matching1_wide <- best_meas_matching1_wide[, c(1,6,5,4,3,2)]
    
    best_meas_matching2 <- data.frame(case_control_full %>% group_by(best_meas, matching___2) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
    # getting n (%) set up
      best_meas_matching2$n_perc_part1 <- paste(best_meas_matching2$n,format(round(best_meas_matching2$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
      best_meas_matching2$n_perc_part2 <- ")"
      best_meas_matching2$n_perc <- paste(best_meas_matching2$n_perc_part1, best_meas_matching2$n_perc_part2, sep="")
    # keeping only needed columns
      best_meas_matching2 <- best_meas_matching2 %>% select(best_meas, matching___2, n_perc)
    # flip dataset
      best_meas_matching2_wide <- best_meas_matching2 %>% pivot_wider(names_from = c(best_meas), values_from = c(n_perc))
      best_meas_matching2_wide <- best_meas_matching2_wide[, c(1,6,5,4,3,2)]
    
    best_meas_matching3 <- data.frame(case_control_full %>% group_by(best_meas, matching___3) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
    # getting n (%) set up
      best_meas_matching3$n_perc_part1 <- paste(best_meas_matching3$n,format(round(best_meas_matching3$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
      best_meas_matching3$n_perc_part2 <- ")"
      best_meas_matching3$n_perc <- paste(best_meas_matching3$n_perc_part1, best_meas_matching3$n_perc_part2, sep="")
    # keeping only needed columns
      best_meas_matching3 <- best_meas_matching3 %>% select(best_meas, matching___3, n_perc)
    # flip dataset
      best_meas_matching3_wide <- best_meas_matching3 %>% pivot_wider(names_from = c(best_meas), values_from = c(n_perc))
      best_meas_matching3_wide <- best_meas_matching3_wide[, c(1,6,5,4,3,2)]
    
    best_meas_matching88 <- data.frame(case_control_full %>% group_by(best_meas, matching___88) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
    # getting n (%) set up
      best_meas_matching88$n_perc_part1 <- paste(best_meas_matching88$n,format(round(best_meas_matching88$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
      best_meas_matching88$n_perc_part2 <- ")"
      best_meas_matching88$n_perc <- paste(best_meas_matching88$n_perc_part1, best_meas_matching88$n_perc_part2, sep="")
    # keeping only needed columns
      best_meas_matching88 <- best_meas_matching88 %>% select(best_meas, matching___88, n_perc)
    # flip dataset
      best_meas_matching88_wide <- best_meas_matching88 %>% pivot_wider(names_from = c(best_meas), values_from = c(n_perc))
      best_meas_matching88_wide <- best_meas_matching88_wide[, c(1,6,5,4,3,2)]
    
    
  # best measure x validity
    best_meas_validity <- data.frame(case_control_full %>% group_by(best_meas, validity) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
    # getting n (%) set up
      best_meas_validity$n_perc_part1 <- paste(best_meas_validity$n,format(round(best_meas_validity$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
      best_meas_validity$n_perc_part2 <- ")"
      best_meas_validity$n_perc <- paste(best_meas_validity$n_perc_part1, best_meas_validity$n_perc_part2, sep="")
    # keeping only needed columns
      best_meas_validity <- best_meas_validity %>% select(best_meas,  validity, n_perc)
    # flip dataset
      best_meas_validity_wide <- best_meas_validity %>% pivot_wider(names_from = c(best_meas), values_from = c(n_perc))
      best_meas_validity_wide <- best_meas_validity_wide[, c(1,6,5,4,3,2)]
      
      
    ma_new2_combined_results_wide <- ma_new2_combined_results_wide %>% pivot_wider(names_from = c(group), values_from = c(ee_ci, pcr_ci))
    

  # number of case/control individuals/samples
    # first extracting one record per study
      case_control_full_unique <- case_control_full %>% 
                                  distinct(author_year_title, .keep_all = T)
      # dropping GEMS/MAL-ED
        case_control_full_unique <- case_control_full_unique %>%
                                    subset(author != "GEMS" & author != "MALED")
      # 140 studies
      # note: this does NOT include MAL-ED and GEMS (cases/controls for these studies are not study-specific but rather stratum-specific)
      sum(case_control_full_unique$cases, na.rm=TRUE) # 50671
      sum(case_control_full_unique$controls, na.rm=TRUE) # 50109
      sum(case_control_full_unique$outcome_cohort, na.rm=TRUE) # 31917
      sum(case_control_full_unique$no_outcome_cohort, na.rm=TRUE) # 81342
      # for MAL-ED (from James)
        # 5646 cases and 1:1 matched controls (from cc subset)
      # for GEMS (from James)
        # 5304 cases and 1:1 matched controls

    # excluding "mixed" age group
      case_control_full_unique_no_mix <- case_control_full_unique %>%
                                         subset(age_group != "mixed")
      # 108 studies remain
      
    table(case_control_full_unique$validity, exclude=NULL)  
    table(case_control_full$path, case_control_full$age_group, case_control_full$det_meth_3, exclude=NULL)  
    summary(case_control_full_unique$year_began)
    
  
  # rotavirus vax introduction status
    table(case_control_clean$rota_vax_comb, exclude=NULL)
    table(case_control_full$rota_vax_comb, exclude=NULL)

    
  # making table summarizing studies
    # pulling n 
      case_control_full_unique$case_diar    <- ifelse(case_control_full_unique$study_design == "case control", case_control_full_unique$cases, case_control_full_unique$outcome_cohort)
      case_control_full_unique$cont_no_diar <- ifelse(case_control_full_unique$study_design == "case control", case_control_full_unique$controls, case_control_full_unique$no_outcome_cohort)
      
    
    study_summary <- case_control_full_unique %>%
                      select(author, year_pub, title, design, validity, case_diar, cont_no_diar)
     


#############################################################    
#############################################################
###                                                       ###
###                       MODELING                        ###
###                                                       ###
#############################################################
############################################################# 
  
  
  
###################################################
####   TESTING/PRACTICE META-ANALYSIS & LOOPS  ####
###################################################
  
# random effect meta-analysis-- developing/checking code with one pathogen 
  # rota model (no predictors)-- young
    meta_young_high_rota <- rma(yi = log_ee, vi = var, 
                       method = "DL", 
                       data = data_young_high_rota, 
                       slab=paste(author, year_pub, sep=", "))
    # exponentiating results
      predict(meta_young_high_rota, transf=exp, digits=2)
  
  # rota model (no predictors)-- old
    meta_old_rota <- rma(yi = log_ee, vi = var, 
                         method = "DL", 
                         data = data_old_rota, 
                         slab=paste(author, year_pub, sep=", "))
    # exponentiating results
      predict(meta_old_rota, transf=exp, digits=2)
      
    # rota model (no predictors)-- mixed
      meta_mixed_rota <- rma(yi = log_ee, vi = var, 
                           method = "DL", 
                           data = data_mixed_rota, 
                           slab=paste(author, year_pub, sep=", "))
      # exponentiating results
      predict(meta_mixed_rota, transf=exp, digits=2)

    # plotting forest plot
      forest(meta_rota,
         order = "obs",
         atransf=exp,
         #alim = c(-1, 10),
         refline = 0,
         xlim=c(-6,10),
         cex.axis = .9,
         cex.lab = .9, 
         xlab = "Effect estimate") 
    
  # rota model- predictor = age group
    meta_rota_age <- rma(yi = log_ee, vi = var, mods = ~ age_group -1,
                         method = "DL", 
                         data = data_rota, 
                         slab=paste(author, year_pub, sep=", "))   
    # exponentiating results
      #predict(meta_rota_age, transf=exp, digits=2)
      predict(meta_rota_age, newmods = rbind(c(0,0,1),c(1,0,0),c(0,1,0)), transf = exp, digits = 2)
      

      
##################################################################
####       RUNNING MODELS- **NEW METHOD** ALL PREDICTORS      ####
####    NEW STRATEGY-- LOOPING THROUGH PATHOGEN DATAFRAMES    ####
####                     MAY 01, 2020                         ####
##################################################################   

### unstratified ###
    
# meta-analysis loop function
  meta_analysis_loop_unadj <- function(data){
    rma(yi = log_ee, vi = var,
      method = "DL", 
      data = data, 
      slab=paste(author, year_pub, sep=", "))
      }       
  
  # data list to loop through  
    data_list_unadj <- c('data_adeno', 'data_astro', 'data_noro', 'data_rota', 'data_sapo',
                         'data_aero', 'data_campy', 'data_chol', 'data_epec',  'data_etec', 'data_salm', 'data_shig', 
                         'data_crypto', 'data_ehist', 'data_giard', 
                         'data_etec_st', 'data_etec_lt', 'data_noro_gi', 'data_noro_gii', 'data_rota_pre', 'data_rota_post')
  # labels for results
    data_labels_unadj <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                           'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                           'crypto', 'ehist', 'giard', 
                           'etec_st', 'etec_lt', 'noro_gi', 'noro_gii', 'rota_pre', 'rota_post')
  # model
    ma_path_unadj <- list()
    for(i in seq_along(data_list_unadj)){ 
      ma_path_unadj[[i]] <- meta_analysis_loop_unadj(get(data_list_unadj[i]))
      }  
  # examine results  
    for (i in 1:21){
      print(ma_path_unadj[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
      for (i in 1:21) {
        coeff <- exp(coef(summary(ma_path_unadj[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
        lci   <- exp(coef(summary(ma_path_unadj[[i]]))$ci.lb[1])
        uci   <- exp(coef(summary(ma_path_unadj[[i]]))$ci.ub[1])    
        group <- "unadj"
        put(coeff, lci, uci, group)
      }
    ma_path_unadj_results <-magic_result_as_dataframe()
  # results in a dataframe
    names <- data_labels_unadj
    ma_path_unadj_results <- data.frame(names, ma_path_unadj_results)   
    ma_path_unadj_results$i <- NULL
    magic_free()
  # adding in NA for pcr results
    ma_path_unadj_results$pcr_coeff <- NA
    ma_path_unadj_results$pcr_lci <- NA
    ma_path_unadj_results$pcr_uci <- NA
    

      
### controlling for pathogen detection method and study design ###
    
# meta-analysis loop function
  meta_analysis_loop_all <- function(data){
    rma(yi = log_ee, vi = var, mods = ~ det_meth_3 + study_design,
        method = "DL", 
        data = data, 
        slab=paste(author, year_pub, sep=", "))
        }     
    
  ### young/high ### 
      
    # data list to loop through  
      data_list_young_high_all <- c('data_young_high_adeno', 'data_young_high_astro', 'data_young_high_noro', 'data_young_high_rota', 'data_young_high_sapo',
                                    'data_young_high_aero', 'data_young_high_campy', 'data_young_high_epec',  'data_young_high_etec', 'data_young_high_salm', 'data_young_high_shig', 
                                    'data_young_high_crypto', 'data_young_high_ehist', 'data_young_high_giard',
                                    'data_young_high_etec_st', 'data_young_high_etec_lt', 'data_young_high_noro_gi', 'data_young_high_noro_gii', 'data_young_high_rota_pre')
      # chol has too few obs
    # labels for results
      data_labels_young_high_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                      'aero', 'campy', 'epec',  'etec', 'salm', 'shig', 
                                      'crypto', 'ehist', 'giard',
                                      'etec_st', 'etec_lt', 'noro_gi', 'noro_gii', 'rota_pre')
    # model
      ma_path_young_high_all <- list()
      for(i in seq_along(data_list_young_high_all)){ 
        ma_path_young_high_all[[i]] <- meta_analysis_loop_all(get(data_list_young_high_all[i]))
      }  
    # examine results  
      for (i in 1:19){
        print(ma_path_young_high_all[[i]], digits=2)
      }   
    # extract estimate and combine results  
      magic_for(silent = TRUE)
      for (i in 1:19) {
        coeff <- exp(coef(summary(ma_path_young_high_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
        lci   <- exp(coef(summary(ma_path_young_high_all[[i]]))$ci.lb[1])
        uci   <- exp(coef(summary(ma_path_young_high_all[[i]]))$ci.ub[1])    
        group <- "young, high mortality"
        pcr_coeff <- exp(coef(summary(ma_path_young_high_all[[i]]))$estimate[2]) # extract coeff for "pcr" only (second coeff in list, not always pcr!)
        pcr_lci   <- exp(coef(summary(ma_path_young_high_all[[i]]))$ci.lb[2])
        pcr_uci   <- exp(coef(summary(ma_path_young_high_all[[i]]))$ci.ub[2])    
        put(coeff, lci, uci, group, pcr_coeff, pcr_lci, pcr_uci)
      }
      ma_path_young_high_all_results <-magic_result_as_dataframe()
    # results in a dataframe
      names <- data_labels_young_high_all
      ma_path_young_high_all_results <- data.frame(names, ma_path_young_high_all_results)   
      ma_path_young_high_all_results$i <- NULL
      magic_free()        

      
  ### young/vlow & low ### 
      
    # data list to loop through  
      data_list_young_vlow_low_all <- c('data_young_vlow_low_adeno', 'data_young_vlow_low_astro', 'data_young_vlow_low_noro', 'data_young_vlow_low_rota', 'data_young_vlow_low_sapo',
                                        'data_young_vlow_low_aero', 'data_young_vlow_low_campy', 'data_young_vlow_low_epec',  'data_young_vlow_low_etec', 'data_young_vlow_low_salm', 'data_young_vlow_low_shig', 
                                        'data_young_vlow_low_crypto', 'data_young_vlow_low_ehist', 'data_young_vlow_low_giard',
                                        'data_young_vlow_low_etec_st', 'data_young_vlow_low_etec_lt', 'data_young_vlow_low_noro_gi', 'data_young_vlow_low_noro_gii', 'data_young_vlow_low_rota_pre', 'data_young_vlow_low_rota_post')
        # chol hs too few obs
    # labels for results
      data_labels_young_vlow_low_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                          'aero', 'campy', 'epec',  'etec', 'salm', 'shig', 
                                          'crypto', 'ehist', 'giard',
                                          'etec_st', 'etec_lt', 'noro_gi', 'noro_gii', 'rota_pre', 'rota_post')
    # model
      ma_path_young_vlow_low_all <- list()
        for(i in seq_along(data_list_young_vlow_low_all)){ 
          ma_path_young_vlow_low_all[[i]] <- meta_analysis_loop_all(get(data_list_young_vlow_low_all[i]))
          }  
    # examine results  
      for (i in 1:20){
        print(ma_path_young_vlow_low_all[[i]], digits=2)
        }   
    # extract estimate and combine results  
      magic_for(silent = TRUE)
        for (i in 1:20) {
          coeff <- exp(coef(summary(ma_path_young_vlow_low_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
          lci   <- exp(coef(summary(ma_path_young_vlow_low_all[[i]]))$ci.lb[1])
          uci   <- exp(coef(summary(ma_path_young_vlow_low_all[[i]]))$ci.ub[1])    
          group <- "young, vlow & low mortality"
          pcr_coeff <- exp(coef(summary(ma_path_young_vlow_low_all[[i]]))$estimate[2]) # extract coeff for "pcr" only (second coeff in list, not always pcr!)
          pcr_lci   <- exp(coef(summary(ma_path_young_vlow_low_all[[i]]))$ci.lb[2])
          pcr_uci   <- exp(coef(summary(ma_path_young_vlow_low_all[[i]]))$ci.ub[2]) 
          put(coeff, lci, uci, group, pcr_coeff, pcr_lci, pcr_uci)
          }
      ma_path_young_vlow_low_all_results <-magic_result_as_dataframe()
    # results in a dataframe
      names <- data_labels_young_vlow_low_all
      ma_path_young_vlow_low_all_results <- data.frame(names, ma_path_young_vlow_low_all_results)   
      ma_path_young_vlow_low_all_results$i <- NULL
      magic_free()          
      
      
  ### old/all child mort levels ### 
      
    # data list to loop through  
      data_list_old_all <- c('data_old_noro', 'data_old_rota', 
                             'data_old_campy', 'data_old_epec',  'data_old_etec', 'data_old_salm', 'data_old_shig',
                             'data_old_crypto', 'data_old_ehist', 'data_old_giard',
                             'data_old_etec_st', 'data_old_etec_lt', 'data_old_noro_gi', 'data_old_noro_gii', 'data_old_rota_pre')
      # too few obs for adeno, aero, astro, chol, sapo
    # labels for results
      data_labels_old_all <- c('noro', 'rota',
                               'campy', 'epec',  'etec', 'salm', 'shig',
                               'crypto', 'ehist', 'giard',
                               'etec_st', 'etec_lt', 'noro_gi', 'noro_gii', 'rota_pre')
    # model
      ma_path_old_all <- list()
      for(i in seq_along(data_list_old_all)){ 
        ma_path_old_all[[i]] <- meta_analysis_loop_all(get(data_list_old_all[i]))
      }  
    # examine results  
      for (i in 1:15){
        print(ma_path_old_all[[i]], digits=2)
      }   
    # extract estimate and combine results  
      magic_for(silent = TRUE)
      for (i in 1:15) {
        coeff <- exp(coef(summary(ma_path_old_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
        lci   <- exp(coef(summary(ma_path_old_all[[i]]))$ci.lb[1])
        uci   <- exp(coef(summary(ma_path_old_all[[i]]))$ci.ub[1])    
        group <- "old, all mortality levels"
        pcr_coeff <- exp(coef(summary(ma_path_old_all[[i]]))$estimate[2]) # extract coeff for "pcr" only (second coeff in list, not always pcr!) 
        pcr_lci   <- exp(coef(summary(ma_path_old_all[[i]]))$ci.lb[2])
        pcr_uci   <- exp(coef(summary(ma_path_old_all[[i]]))$ci.ub[2])   
        put(coeff, lci, uci, group, pcr_coeff, pcr_lci, pcr_uci)
      }
      ma_path_old_all_results <-magic_result_as_dataframe()
    # results in a dataframe
      names <- data_labels_old_all
      ma_path_old_all_results <- data.frame(names, ma_path_old_all_results)   
      ma_path_old_all_results$i <- NULL
      magic_free()      
    # dropping "pcr" info when none available
      ma_path_old_all_results$pcr_coeff <- ifelse((ma_path_old_all_results$names == "noro" | ma_path_old_all_results$names == "shig" | ma_path_old_all_results$names == "ehist"), 
                                                   ma_path_old_all_results$pcr_coeff == NA, ma_path_old_all_results$pcr_coeff)
      ma_path_old_all_results$pcr_lci <- ifelse((ma_path_old_all_results$names == "noro" | ma_path_old_all_results$names == "shig" | ma_path_old_all_results$names == "ehist"), 
                                                 ma_path_old_all_results$pcr_lci == NA, ma_path_old_all_results$pcr_lci)
      ma_path_old_all_results$pcr_uci <- ifelse((ma_path_old_all_results$names == "noro" | ma_path_old_all_results$names == "shig" | ma_path_old_all_results$names == "ehist"), 
                                                 ma_path_old_all_results$pcr_uci == NA, ma_path_old_all_results$pcr_uci)
      
    

# combining all datasets
  ma_new2_combined_results <- rbind(ma_path_unadj_results, 
                                    #ma_path_young_vlow_all_results, ma_path_young_low_all_results, 
                                    ma_path_young_high_all_results, 
                                    ma_path_young_vlow_low_all_results,
                                    ma_path_old_all_results)
  # formatting
    ma_new2_combined_results$coeff  <- round(ma_new2_combined_results$coeff, 1)
    ma_new2_combined_results$lci    <- round(ma_new2_combined_results$lci, 1)
    ma_new2_combined_results$uci    <- round(ma_new2_combined_results$uci, 1)
    ma_new2_combined_results$pcr_coeff  <- round(ma_new2_combined_results$pcr_coeff, 1)
    ma_new2_combined_results$pcr_lci    <- round(ma_new2_combined_results$pcr_lci, 1)
    ma_new2_combined_results$pcr_uci    <- round(ma_new2_combined_results$pcr_uci, 1)
  # creating variable with effect estimate and CI for pathogen
    ma_new2_combined_results$part1  <- paste(ma_new2_combined_results$coeff, ma_new2_combined_results$lci, sep=" (")
    ma_new2_combined_results$part2  <- paste(ma_new2_combined_results$part1, ma_new2_combined_results$uci, sep=", ")
    ma_new2_combined_results$part3  <- ")"
    ma_new2_combined_results$ee_ci  <- paste(ma_new2_combined_results$part2, ma_new2_combined_results$part3, sep="")
    # cleaning/dropping unnecessary variables
      ma_new2_combined_results$part1 <- NULL
      ma_new2_combined_results$part2 <- NULL
      ma_new2_combined_results$part3 <- NULL
  # creating variable with effect estimate and CI for pcr
    ma_new2_combined_results$part1  <- paste(ma_new2_combined_results$pcr_coeff, ma_new2_combined_results$pcr_lci, sep=" (")
    ma_new2_combined_results$part2  <- paste(ma_new2_combined_results$part1, ma_new2_combined_results$pcr_uci, sep=", ")
    ma_new2_combined_results$part3  <- ")"
    ma_new2_combined_results$pcr_ci  <- paste(ma_new2_combined_results$part2, ma_new2_combined_results$part3, sep="")
    # cleaning/dropping unnecessary variables
      ma_new2_combined_results$part1 <- NULL
      ma_new2_combined_results$part2 <- NULL
      ma_new2_combined_results$part3 <- NULL
  # creating larger/broader group variable (for plotting later)
    ma_new2_combined_results$strat <- ifelse(ma_new2_combined_results$group == 'unadj', 'unadjusted',
                                      ifelse((ma_new2_combined_results$group == 'young, very low mortality' |
                                              ma_new2_combined_results$group == 'young, low mortality' |
                                              ma_new2_combined_results$group == 'young, high mortality' |
                                              ma_new2_combined_results$group == 'young, vlow & low mortality'), '0-5 years',
                                      ifelse(ma_new2_combined_results$group == 'old, all mortality levels', '>5 years', NA)))
    
  # switching from long to wide structure
    # keep only needed variables
      ma_new2_combined_results_wide <- ma_new2_combined_results %>% select(names, group, ee_ci, pcr_ci)
      ma_new2_combined_results_wide <- ma_new2_combined_results_wide %>% pivot_wider(names_from = c(group), values_from = c(ee_ci, pcr_ci))
      # format and export
        ordered_path <- c('adeno', 'astro', 'noro', 'noro_gi', 'noro_gii', 'rota', 'rota_pre', 'rota_post', 'sapo',
                          'aero', 'campy', 'chol', 'epec',  'etec', 'etec_st', 'etec_lt', 'salm', 'shig', 
                          'crypto', 'ehist', 'giard')

      
        ma_new2_combined_results_wide <- ma_new2_combined_results_wide[, c(1,2,4,3,5)]
        ma_new2_combined_results_wide <- ma_new2_combined_results_wide %>%
                                         mutate(names =  factor(names, levels = ordered_path)) %>%
                                         arrange(names)
      
  # writing to csv
    write.csv(ma_new2_combined_results_wide, file = "/Users/juliabaker/Box Sync/WHO BoD/OR Analysis/OR Deliverables/Julia/Results/ma_new2_combined_results_wide.csv")
      
      
    summary(ma_new2_combined_results$coeff)
    
    
    
    
##################################################################
####       RUNNING MODELS- **NEW METHOD** ALL PREDICTORS      ####
####    NEW STRATEGY-- LOOPING THROUGH PATHOGEN DATAFRAMES    ####
###       SENSITIVITY ANALYSIS-- USING ONLY ORS (NO RR)       ####    
####                     JUL 11, 2020                         ####
##################################################################   
    
# dataframe without RRs
  case_control_or_only <- case_control_full[ which(case_control_full$best_meas == "OR adj" |
                                                   case_control_full$best_meas == "OR unadj" |
                                                   case_control_full$best_meas == "OR unadj- calc"), ]
    # now down to 1171 obs
    
    
### prep dataframes ###
    
  # each pathogen & strain
    pathogen_data_or_list <- unique(case_control_or_only$path)
    for (v in pathogen_data_or_list) {
      assign(paste0("data_or_", as.character(v)), case_control_or_only %>% filter (path == v), envir = .GlobalEnv)
    } 
    data_or_etec_st   <- case_control_or_only[ which(case_control_or_only$path_strain == "etec_st"), ]
    data_or_etec_lt   <- case_control_or_only[ which(case_control_or_only$path_strain == "etec_lt"), ]
    data_or_noro_gi   <- case_control_or_only[ which(case_control_or_only$path_strain == "noro_gi"), ]
    data_or_noro_gii  <- case_control_or_only[ which(case_control_or_only$path_strain == "noro_gii"), ]
    data_or_rota_pre  <- case_control_or_only[ which(case_control_or_only$path_strain == "rota_pre"), ]
    data_or_rota_post <- case_control_or_only[ which(case_control_or_only$path_strain == "rota_post"), ]
    
  # dataframe for each stratification (age group and child mortality level)
    
  # age group (young, mixed, old)
    age_group_list <- unique(case_control_or_only$age_group)
    for (v in age_group_list) {
      assign(paste0("data_or_", as.character(v)), case_control_or_only %>% filter (age_group == v), envir = .GlobalEnv)
    }
    
  # child_mort (very low, low, high, very low/low combined)
    data_or_high     <- case_control_or_only[ which(case_control_or_only$child_mort == "high"), ]
    data_or_vlow_low <- case_control_or_only[ which((case_control_or_only$child_mort == "very low" | case_control_or_only$child_mort == "low")), ]
    
  # young age by child mortality
    data_or_young_high     <- data_or_young[ which(data_or_young$child_mort == "high"), ]
    data_or_young_vlow_low <- data_or_young[ which((data_or_young$child_mort == "very low" | data_or_young$child_mort == "low")), ]
    
  # each pathogen within the age/child mort dataframes
    pathogen_data_or_list <- unique(case_control_or_only$path)
    pathogen_strain_data_or_list <- unique(case_control_or_only$path_strain)
    # young/high
      for (v in pathogen_data_or_list) {
        assign(paste0("data_or_young_high_", as.character(v)), data_or_young_high %>% filter (path == v), envir = .GlobalEnv)
        }
      for (u in pathogen_strain_data_or_list) {
        assign(paste0("data_or_young_high_", as.character(u)), data_or_young_high %>% filter (path_strain == u), envir = .GlobalEnv)
        } 
      # young/very low/low
      for (v in pathogen_data_or_list) {
        assign(paste0("data_or_young_vlow_low_", as.character(v)), data_or_young_vlow_low %>% filter (path == v), envir = .GlobalEnv)
        }
      for (u in pathogen_strain_data_or_list) {
        assign(paste0("data_or_young_vlow_low_", as.character(u)), data_or_young_vlow_low %>% filter (path_strain == u), envir = .GlobalEnv)
        } 
      # old/all child mortality levels
      for (v in pathogen_data_or_list) {
        assign(paste0("data_or_old_", as.character(v)), data_or_old %>% filter (path == v), envir = .GlobalEnv)
        }
      for (u in pathogen_strain_data_or_list) {
        assign(paste0("data_or_old_", as.character(u)), data_or_old %>% filter (path_strain == u), envir = .GlobalEnv)
        } 
      
    
    
### unstratified ###
    
  # meta-analysis loop function
    meta_analysis_loop_unadj <- function(data){
      rma(yi = log_ee, vi = var,
          method = "DL", 
          data = data, 
          slab=paste(author, year_pub, sep=", "))
    }       
    
  # data list to loop through  
    data_or_list_unadj <- c('data_or_adeno', 'data_or_astro', 'data_or_noro', 'data_or_rota', 'data_or_sapo',
                            'data_or_aero', 'data_or_campy', 'data_or_chol', 'data_or_epec',  'data_or_etec', 'data_or_salm', 'data_or_shig', 
                            'data_or_crypto', 'data_or_ehist', 'data_or_giard', 
                            'data_or_etec_st', 'data_or_etec_lt', 'data_or_noro_gi', 'data_or_noro_gii', 'data_or_rota_pre', 'data_or_rota_post')
  # labels for results
    data_or_labels_unadj <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                              'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                              'crypto', 'ehist', 'giard', 
                              'etec_st', 'etec_lt', 'noro_gi', 'noro_gii', 'rota_pre', 'rota_post')
  # model
    ma_path_or_unadj <- list()
    for(i in seq_along(data_or_list_unadj)){ 
      ma_path_or_unadj[[i]] <- meta_analysis_loop_unadj(get(data_or_list_unadj[i]))
    }  
  # examine results  
    for (i in 1:21){
      print(ma_path_or_unadj[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:21) {
      coeff <- exp(coef(summary(ma_path_or_unadj[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
      lci   <- exp(coef(summary(ma_path_or_unadj[[i]]))$ci.lb[1])
      uci   <- exp(coef(summary(ma_path_or_unadj[[i]]))$ci.ub[1])    
      group <- "unadj"
      put(coeff, lci, uci, group)
    }
    ma_path_or_unadj_results <-magic_result_as_dataframe()
  # results in a dataframe
    names <- data_or_labels_unadj
    ma_path_or_unadj_results <- data.frame(names, ma_path_or_unadj_results)   
    ma_path_or_unadj_results$i <- NULL
    magic_free()
  # adding in NA for pcr results
    ma_path_or_unadj_results$pcr_coeff <- NA
    ma_path_or_unadj_results$pcr_lci <- NA
    ma_path_or_unadj_results$pcr_uci <- NA
    
    
    
### controlling for pathogen detection method and study design ###
    
  # meta-analysis loop function
    meta_analysis_loop_all <- function(data){
      rma(yi = log_ee, vi = var, mods = ~ det_meth_3 + study_design,
          method = "DL", 
          data = data, 
          slab=paste(author, year_pub, sep=", "))
    }     
    

### young/high ### 
    
  # data list to loop through  
    data_or_list_young_high_all <- c('data_or_young_high_adeno', 'data_or_young_high_astro', 'data_or_young_high_noro', 'data_or_young_high_rota', 'data_or_young_high_sapo',
                                     'data_or_young_high_aero', 'data_or_young_high_campy', 'data_or_young_high_epec',  'data_or_young_high_etec', 'data_or_young_high_salm', 'data_or_young_high_shig', 
                                     'data_or_young_high_crypto', 'data_or_young_high_ehist', 'data_or_young_high_giard',
                                     'data_or_young_high_etec_st', 'data_or_young_high_etec_lt', 'data_or_young_high_noro_gi', 'data_or_young_high_noro_gii', 'data_or_young_high_rota_pre', 'data_or_young_high_rota_post')
    # chol has too few obs
  # labels for results
    data_or_labels_young_high_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                       'aero', 'campy', 'epec',  'etec', 'salm', 'shig', 
                                       'crypto', 'ehist', 'giard',
                                       'etec_st', 'etec_lt', 'noro_gi', 'noro_gii', 'rota_pre', 'rota_post')
  # model
    ma_path_or_young_high_all <- list()
    for(i in seq_along(data_or_list_young_high_all)){ 
      ma_path_or_young_high_all[[i]] <- meta_analysis_loop_all(get(data_or_list_young_high_all[i]))
    }  
  # examine results  
    for (i in 1:20){
      print(ma_path_or_young_high_all[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:20) {
      coeff <- exp(coef(summary(ma_path_or_young_high_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
      lci   <- exp(coef(summary(ma_path_or_young_high_all[[i]]))$ci.lb[1])
      uci   <- exp(coef(summary(ma_path_or_young_high_all[[i]]))$ci.ub[1])    
      group <- "young, high mortality"
      pcr_coeff <- exp(coef(summary(ma_path_or_young_high_all[[i]]))$estimate[2]) # extract coeff for "pcr" only (second coeff in list, not always pcr!)
      pcr_lci   <- exp(coef(summary(ma_path_or_young_high_all[[i]]))$ci.lb[2])
      pcr_uci   <- exp(coef(summary(ma_path_or_young_high_all[[i]]))$ci.ub[2])    
      put(coeff, lci, uci, group, pcr_coeff, pcr_lci, pcr_uci)
    }
    ma_path_or_young_high_all_results <-magic_result_as_dataframe()
  # results in a dataframe
    names <- data_or_labels_young_high_all
    ma_path_or_young_high_all_results <- data.frame(names, ma_path_or_young_high_all_results)   
    ma_path_or_young_high_all_results$i <- NULL
    magic_free()        
    
    
### young/vlow & low ### 
    
  # data list to loop through  
    data_or_list_young_vlow_low_all <- c('data_or_young_vlow_low_adeno', 'data_or_young_vlow_low_astro', 'data_or_young_vlow_low_noro', 'data_or_young_vlow_low_rota', 'data_or_young_vlow_low_sapo',
                                      'data_or_young_vlow_low_aero', 'data_or_young_vlow_low_campy', 'data_or_young_vlow_low_epec',  'data_or_young_vlow_low_etec', 'data_or_young_vlow_low_salm', 'data_or_young_vlow_low_shig', 
                                      'data_or_young_vlow_low_crypto', 'data_or_young_vlow_low_ehist', 'data_or_young_vlow_low_giard',
                                      'data_or_young_vlow_low_etec_st', 'data_or_young_vlow_low_etec_lt', 'data_or_young_vlow_low_noro_gi', 'data_or_young_vlow_low_noro_gii', 'data_or_young_vlow_low_rota_pre', 'data_or_young_vlow_low_rota_post')
    # chol hs too few obs
  # labels for results
    data_or_labels_young_vlow_low_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                        'aero', 'campy', 'epec',  'etec', 'salm', 'shig', 
                                        'crypto', 'ehist', 'giard',
                                        'etec_st', 'etec_lt', 'noro_gi', 'noro_gii', 'rota_pre', 'rota_post')
  # model
    ma_path_or_young_vlow_low_all <- list()
    for(i in seq_along(data_or_list_young_vlow_low_all)){ 
      ma_path_or_young_vlow_low_all[[i]] <- meta_analysis_loop_all(get(data_or_list_young_vlow_low_all[i]))
    }  
  # examine results  
    for (i in 1:20){
      print(ma_path_or_young_vlow_low_all[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:20) {
      coeff <- exp(coef(summary(ma_path_or_young_vlow_low_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
      lci   <- exp(coef(summary(ma_path_or_young_vlow_low_all[[i]]))$ci.lb[1])
      uci   <- exp(coef(summary(ma_path_or_young_vlow_low_all[[i]]))$ci.ub[1])    
      group <- "young, vlow & low mortality"
      pcr_coeff <- exp(coef(summary(ma_path_or_young_vlow_low_all[[i]]))$estimate[2]) # extract coeff for "pcr" only (second coeff in list, not always pcr!)
      pcr_lci   <- exp(coef(summary(ma_path_or_young_vlow_low_all[[i]]))$ci.lb[2])
      pcr_uci   <- exp(coef(summary(ma_path_or_young_vlow_low_all[[i]]))$ci.ub[2]) 
      put(coeff, lci, uci, group, pcr_coeff, pcr_lci, pcr_uci)
    }
    ma_path_or_young_vlow_low_all_results <-magic_result_as_dataframe()
  # results in a dataframe
    names <- data_or_labels_young_vlow_low_all
    ma_path_or_young_vlow_low_all_results <- data.frame(names, ma_path_or_young_vlow_low_all_results)   
    ma_path_or_young_vlow_low_all_results$i <- NULL
    magic_free()          
    
    
### old/all child mort levels ### 
    
  # data list to loop through  
    data_or_list_old_all <- c('data_or_old_noro', 'data_or_old_rota', 
                           'data_or_old_campy', 'data_or_old_epec',  'data_or_old_etec', 'data_or_old_salm', 'data_or_old_shig',
                           'data_or_old_crypto', 'data_or_old_ehist', 'data_or_old_giard',
                           'data_or_old_etec_st', 'data_or_old_etec_lt', 'data_or_old_noro_gi', 'data_or_old_noro_gii', 'data_or_old_rota_pre')
    # too few obs for adeno, aero, astro, chol, sapo
  # labels for results
    data_or_labels_old_all <- c('noro', 'rota',
                             'campy', 'epec',  'etec', 'salm', 'shig',
                             'crypto', 'ehist', 'giard',
                             'etec_st', 'etec_lt', 'noro_gi', 'noro_gii', 'rota_pre')
  # model
    ma_path_or_old_all <- list()
    for(i in seq_along(data_or_list_old_all)){ 
      ma_path_or_old_all[[i]] <- meta_analysis_loop_all(get(data_or_list_old_all[i]))
    }  
  # examine results  
    for (i in 1:15){
      print(ma_path_or_old_all[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:15) {
      coeff <- exp(coef(summary(ma_path_or_old_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
      lci   <- exp(coef(summary(ma_path_or_old_all[[i]]))$ci.lb[1])
      uci   <- exp(coef(summary(ma_path_or_old_all[[i]]))$ci.ub[1])    
      group <- "old, all mortality levels"
      pcr_coeff <- exp(coef(summary(ma_path_or_old_all[[i]]))$estimate[2]) # extract coeff for "pcr" only (second coeff in list, not always pcr!) 
      pcr_lci   <- exp(coef(summary(ma_path_or_old_all[[i]]))$ci.lb[2])
      pcr_uci   <- exp(coef(summary(ma_path_or_old_all[[i]]))$ci.ub[2])   
      put(coeff, lci, uci, group, pcr_coeff, pcr_lci, pcr_uci)
    }
    ma_path_or_old_all_results <-magic_result_as_dataframe()
  # results in a dataframe
    names <- data_or_labels_old_all
    ma_path_or_old_all_results <- data.frame(names, ma_path_or_old_all_results)   
    ma_path_or_old_all_results$i <- NULL
    magic_free()      
  # dropping "pcr" info when none available
    ma_path_or_old_all_results$pcr_coeff <- ifelse((ma_path_or_old_all_results$names == "noro" | ma_path_or_old_all_results$names == "shig" | ma_path_or_old_all_results$names == "ehist"), 
                                                ma_path_or_old_all_results$pcr_coeff == NA, ma_path_or_old_all_results$pcr_coeff)
    ma_path_or_old_all_results$pcr_lci <- ifelse((ma_path_or_old_all_results$names == "noro" | ma_path_or_old_all_results$names == "shig" | ma_path_or_old_all_results$names == "ehist"), 
                                              ma_path_or_old_all_results$pcr_lci == NA, ma_path_or_old_all_results$pcr_lci)
    ma_path_or_old_all_results$pcr_uci <- ifelse((ma_path_or_old_all_results$names == "noro" | ma_path_or_old_all_results$names == "shig" | ma_path_or_old_all_results$names == "ehist"), 
                                              ma_path_or_old_all_results$pcr_uci == NA, ma_path_or_old_all_results$pcr_uci)
    
    
    
  # combinding all datasets
    ma_or_combined_results <- rbind(ma_path_or_unadj_results, 
                                    ma_path_or_young_high_all_results, ma_path_or_young_vlow_low_all_results,
                                    ma_path_or_old_all_results)
  # formatting
    ma_or_combined_results$coeff  <- format(round(ma_or_combined_results$coeff, 1), nsmall=1, trim=TRUE)
    ma_or_combined_results$lci    <- format(round(ma_or_combined_results$lci, 1), nsmall=1, trim=TRUE)
    ma_or_combined_results$uci    <- format(round(ma_or_combined_results$uci, 1), nsmall=1, trim=TRUE)
    ma_or_combined_results$pcr_coeff  <- format(round(ma_or_combined_results$pcr_coeff, 1), nsmall=1, trim=TRUE)
    ma_or_combined_results$pcr_lci    <- format(round(ma_or_combined_results$pcr_lci, 1), nsmall=1, trim=TRUE)
    ma_or_combined_results$pcr_uci    <- format(round(ma_or_combined_results$pcr_uci, 1), nsmall=1, trim=TRUE)
  # creating variable with effect estimate and CI for pathogen
    ma_or_combined_results$part1  <- paste(ma_or_combined_results$coeff, ma_or_combined_results$lci, sep=" (")
    ma_or_combined_results$part2  <- paste(ma_or_combined_results$part1, ma_or_combined_results$uci, sep=", ")
    ma_or_combined_results$part3  <- ")"
    ma_or_combined_results$ee_ci  <- paste(ma_or_combined_results$part2, ma_or_combined_results$part3, sep="")
    # cleaning/dropping unnecessary variables
      ma_or_combined_results$part1 <- NULL
      ma_or_combined_results$part2 <- NULL
      ma_or_combined_results$part3 <- NULL
  # creating variable with effect estimate and CI for pcr
    ma_or_combined_results$part1  <- paste(ma_or_combined_results$pcr_coeff, ma_or_combined_results$pcr_lci, sep=" (")
    ma_or_combined_results$part2  <- paste(ma_or_combined_results$part1, ma_or_combined_results$pcr_uci, sep=", ")
    ma_or_combined_results$part3  <- ")"
    ma_or_combined_results$pcr_ci  <- paste(ma_or_combined_results$part2, ma_or_combined_results$part3, sep="")
    # cleaning/dropping unnecessary variables
      ma_or_combined_results$part1 <- NULL
      ma_or_combined_results$part2 <- NULL
      ma_or_combined_results$part3 <- NULL
   # creating larger/broader group variable (for plotting later)
      ma_or_combined_results$strat <- ifelse(ma_or_combined_results$group == 'unadj', 'unadjusted',
                                      ifelse((ma_or_combined_results$group == 'young, high mortality' |
                                              ma_or_combined_results$group == 'young, vlow & low mortality'), '0-5 years',
                                      ifelse(ma_or_combined_results$group == 'old, all mortality levels', '>5 years', NA)))
  # switching from long to wide structure
    # keep only needed variables
      ma_or_combined_results_wide <- ma_or_combined_results %>% select(names, group, ee_ci, pcr_ci)
      ma_or_combined_results_wide <- ma_or_combined_results_wide %>% pivot_wider(names_from = c(group), values_from = c(ee_ci, pcr_ci))
    # format and export
      ma_or_combined_results_wide <- ma_or_combined_results_wide[, c(1,2,4,3,5)]
      ma_or_combined_results_wide <- ma_or_combined_results_wide %>%
                                     mutate(names =  factor(names, levels = ordered_path)) %>%
                                     arrange(names)
    # writing to csv
      write.csv(ma_or_combined_results_wide, file = "/Users/juliabaker/Box Sync/WHO BoD/OR Analysis/OR Deliverables/Julia/Results/ma_or_combined_results_wide.csv")
    

    

##################################################################
####       RUNNING MODELS- **NEW METHOD** ALL PREDICTORS      ####
####    NEW STRATEGY-- LOOPING THROUGH PATHOGEN DATAFRAMES    ####
###       SENSITIVITY ANALYSIS-- EXCLUDING COINFECTIONS       ####    
####                     JUL 13, 2020                         ####
##################################################################   
    
# dataframe without co-infections
    case_control_coinf <- case_control_full[ which(case_control_full$path_num == 1), ]
    # now down to 971 obs
    
    
### prep dataframes ###
    
  # each pathogen & strain
    pathogen_data_coinf_list <- unique(case_control_coinf$path)
    for (v in pathogen_data_coinf_list) {
      assign(paste0("data_coinf_", as.character(v)), case_control_coinf %>% filter (path == v), envir = .GlobalEnv)
    } 
    data_coinf_etec_st   <- case_control_coinf[ which(case_control_coinf$path_strain == "etec_st"), ]
    data_coinf_etec_lt   <- case_control_coinf[ which(case_control_coinf$path_strain == "etec_lt"), ]
    data_coinf_noro_gi   <- case_control_coinf[ which(case_control_coinf$path_strain == "noro_gi"), ]
    data_coinf_noro_gii  <- case_control_coinf[ which(case_control_coinf$path_strain == "noro_gii"), ]
    data_coinf_rota_pre  <- case_control_coinf[ which(case_control_coinf$path_strain == "rota_pre"), ]
    data_coinf_rota_post <- case_control_coinf[ which(case_control_coinf$path_strain == "rota_post"), ]
    
  # dataframe for each stratification (age group and child mortality level)
    
    # age group (young, mixed, old)
      age_group_list <- unique(case_control_coinf$age_group)
      for (v in age_group_list) {
        assign(paste0("data_coinf_", as.character(v)), case_control_coinf %>% filter (age_group == v), envir = .GlobalEnv)
      }
    
    # child_mort (very low, low, high, very low/low combined)
      data_coinf_high     <- case_control_coinf[ which(case_control_coinf$child_mort == "high"), ]
      data_coinf_vlow_low <- case_control_coinf[ which((case_control_coinf$child_mort == "very low" | case_control_coinf$child_mort == "low")), ]
      
    # young age by child mortality
      data_coinf_young_high     <- data_coinf_young[ which(data_coinf_young$child_mort == "high"), ]
      data_coinf_young_vlow_low <- data_coinf_young[ which((data_coinf_young$child_mort == "very low" | data_coinf_young$child_mort == "low")), ]
      
    # each pathogen within the age/child mort dataframes
      pathogen_data_coinf_list <- unique(case_control_coinf$path)
      pathogen_strain_data_coinf_list <- unique(case_control_coinf$path_strain)
    # young/high
      for (v in pathogen_data_coinf_list) {
        assign(paste0("data_coinf_young_high_", as.character(v)), data_coinf_young_high %>% filter (path == v), envir = .GlobalEnv)
      }
      for (u in pathogen_strain_data_coinf_list) {
        assign(paste0("data_coinf_young_high_", as.character(u)), data_coinf_young_high %>% filter (path_strain == u), envir = .GlobalEnv)
      } 
    # young/very low/low
      for (v in pathogen_data_coinf_list) {
        assign(paste0("data_coinf_young_vlow_low_", as.character(v)), data_coinf_young_vlow_low %>% filter (path == v), envir = .GlobalEnv)
      }
      for (u in pathogen_strain_data_coinf_list) {
        assign(paste0("data_coinf_young_vlow_low_", as.character(u)), data_coinf_young_vlow_low %>% filter (path_strain == u), envir = .GlobalEnv)
      } 
    # old/all child mortality levels
      for (v in pathogen_data_coinf_list) {
        assign(paste0("data_coinf_old_", as.character(v)), data_coinf_old %>% filter (path == v), envir = .GlobalEnv)
      }
      for (u in pathogen_strain_data_coinf_list) {
        assign(paste0("data_coinf_old_", as.character(u)), data_coinf_old %>% filter (path_strain == u), envir = .GlobalEnv)
      } 
    
    
    
### unstratified ###
    
  # meta-analysis loop function
    meta_analysis_loop_unadj <- function(data){
      rma(yi = log_ee, vi = var,
          method = "DL", 
          data = data, 
          slab=paste(author, year_pub, sep=", "))
    }       
    
  # data list to loop through  
    data_coinf_list_unadj <- c('data_coinf_adeno', 'data_coinf_astro', 'data_coinf_noro', 'data_coinf_rota', 'data_coinf_sapo',
                               'data_coinf_aero', 'data_coinf_campy', 'data_coinf_chol', 'data_coinf_epec',  'data_coinf_etec', 'data_coinf_salm', 'data_coinf_shig', 
                               'data_coinf_crypto', 'data_coinf_ehist', 'data_coinf_giard', 
                               'data_coinf_etec_st', 'data_coinf_etec_lt', 'data_coinf_noro_gi', 'data_coinf_noro_gii', 'data_coinf_rota_pre', 'data_coinf_rota_post')
  # labels for results
    data_coinf_labels_unadj <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                 'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                                 'crypto', 'ehist', 'giard', 
                                 'etec_st', 'etec_lt', 'noro_gi', 'noro_gii', 'rota_pre', 'rota_post')
  # model
    ma_path_coinf_unadj <- list()
    for(i in seq_along(data_coinf_list_unadj)){ 
      ma_path_coinf_unadj[[i]] <- meta_analysis_loop_unadj(get(data_coinf_list_unadj[i]))
    }  
  # examine results  
    for (i in 1:21){
      print(ma_path_coinf_unadj[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:21) {
      coeff <- exp(coef(summary(ma_path_coinf_unadj[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
      lci   <- exp(coef(summary(ma_path_coinf_unadj[[i]]))$ci.lb[1])
      uci   <- exp(coef(summary(ma_path_coinf_unadj[[i]]))$ci.ub[1])    
      group <- "unadj"
      put(coeff, lci, uci, group)
    }
    ma_path_coinf_unadj_results <-magic_result_as_dataframe()
  # results in a dataframe
    names <- data_coinf_labels_unadj
    ma_path_coinf_unadj_results <- data.frame(names, ma_path_coinf_unadj_results)   
    ma_path_coinf_unadj_results$i <- NULL
    magic_free()
  # adding in NA for pcr results
    ma_path_coinf_unadj_results$pcr_coeff <- NA
    ma_path_coinf_unadj_results$pcr_lci <- NA
    ma_path_coinf_unadj_results$pcr_uci <- NA
    
    
    
### controlling for pathogen detection method and study design ###
    
  # meta-analysis loop function
    meta_analysis_loop_all <- function(data){
      rma(yi = log_ee, vi = var, mods = ~ det_meth_3 + study_design,
          method = "DL", 
          data = data, 
          slab=paste(author, year_pub, sep=", "))
    }     
    
    
### young/high ### 
    
  # data list to loop through  
    data_coinf_list_young_high_all <- c('data_coinf_young_high_adeno', 'data_coinf_young_high_astro', 'data_coinf_young_high_noro', 'data_coinf_young_high_rota', 'data_coinf_young_high_sapo',
                                        'data_coinf_young_high_aero', 'data_coinf_young_high_campy', 'data_coinf_young_high_epec',  'data_coinf_young_high_etec', 'data_coinf_young_high_salm', 'data_coinf_young_high_shig', 
                                        'data_coinf_young_high_crypto', 'data_coinf_young_high_ehist', 'data_coinf_young_high_giard',
                                        'data_coinf_young_high_etec_st', 'data_coinf_young_high_etec_lt', 'data_coinf_young_high_noro_gi', 'data_coinf_young_high_noro_gii', 'data_coinf_young_high_rota_pre', 'data_coinf_young_high_rota_post')
    # chol has too few obs
  # labels for results
    data_coinf_labels_young_high_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                          'aero', 'campy', 'epec',  'etec', 'salm', 'shig', 
                                          'crypto', 'ehist', 'giard',
                                          'etec_st', 'etec_lt', 'noro_gi', 'noro_gii', 'rota_pre', 'rota_post')
  # model
    ma_path_coinf_young_high_all <- list()
    for(i in seq_along(data_coinf_list_young_high_all)){ 
      ma_path_coinf_young_high_all[[i]] <- meta_analysis_loop_all(get(data_coinf_list_young_high_all[i]))
    }  
  # examine results  
    for (i in 1:20){
      print(ma_path_coinf_young_high_all[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:20) {
      coeff <- exp(coef(summary(ma_path_coinf_young_high_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
      lci   <- exp(coef(summary(ma_path_coinf_young_high_all[[i]]))$ci.lb[1])
      uci   <- exp(coef(summary(ma_path_coinf_young_high_all[[i]]))$ci.ub[1])    
      group <- "young, high mortality"
      pcr_coeff <- exp(coef(summary(ma_path_coinf_young_high_all[[i]]))$estimate[2]) # extract coeff for "pcr" only (second coeff in list, not always pcr!)
      pcr_lci   <- exp(coef(summary(ma_path_coinf_young_high_all[[i]]))$ci.lb[2])
      pcr_uci   <- exp(coef(summary(ma_path_coinf_young_high_all[[i]]))$ci.ub[2])    
      put(coeff, lci, uci, group, pcr_coeff, pcr_lci, pcr_uci)
    }
    ma_path_coinf_young_high_all_results <-magic_result_as_dataframe()
  # results in a dataframe
    names <- data_coinf_labels_young_high_all
    ma_path_coinf_young_high_all_results <- data.frame(names, ma_path_coinf_young_high_all_results)   
    ma_path_coinf_young_high_all_results$i <- NULL
    magic_free()    
  # dropping pcr data when there are none
    ma_path_coinf_young_high_all_results$pcr_coeff <- ifelse(ma_path_coinf_young_high_all_results$names == "sapo", NA, ma_path_coinf_young_high_all_results$pcr_coeff)
    ma_path_coinf_young_high_all_results$pcr_lci <- ifelse(ma_path_coinf_young_high_all_results$names == "sapo", NA, ma_path_coinf_young_high_all_results$pcr_lci)
    ma_path_coinf_young_high_all_results$pcr_uci <- ifelse(ma_path_coinf_young_high_all_results$names == "sapo", NA, ma_path_coinf_young_high_all_results$pcr_uci)
    
    
### young/vlow & low ### 
    
  # data list to loop through  
    data_coinf_list_young_vlow_low_all <- c('data_coinf_young_vlow_low_adeno', 'data_coinf_young_vlow_low_astro', 'data_coinf_young_vlow_low_noro', 'data_coinf_young_vlow_low_rota', 'data_coinf_young_vlow_low_sapo',
                                            'data_coinf_young_vlow_low_campy', 'data_coinf_young_vlow_low_epec',  'data_coinf_young_vlow_low_etec', 'data_coinf_young_vlow_low_salm', 'data_coinf_young_vlow_low_shig', 
                                            'data_coinf_young_vlow_low_crypto', 'data_coinf_young_vlow_low_ehist', 'data_coinf_young_vlow_low_giard',
                                            'data_coinf_young_vlow_low_etec_st', 'data_coinf_young_vlow_low_etec_lt', 'data_coinf_young_vlow_low_noro_gi', 'data_coinf_young_vlow_low_noro_gii', 'data_coinf_young_vlow_low_rota_pre')
    # chol and aero has too few obs
  # labels for results
    data_coinf_labels_young_vlow_low_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                              'campy', 'epec',  'etec', 'salm', 'shig', 
                                              'crypto', 'ehist', 'giard',
                                              'etec_st', 'etec_lt', 'noro_gi', 'noro_gii', 'rota_pre')
  # model
    ma_path_coinf_young_vlow_low_all <- list()
    for(i in seq_along(data_coinf_list_young_vlow_low_all)){ 
      ma_path_coinf_young_vlow_low_all[[i]] <- meta_analysis_loop_all(get(data_coinf_list_young_vlow_low_all[i]))
    }  
  # examine results  
    for (i in 1:18){
      print(ma_path_coinf_young_vlow_low_all[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:18) {
      coeff <- exp(coef(summary(ma_path_coinf_young_vlow_low_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
      lci   <- exp(coef(summary(ma_path_coinf_young_vlow_low_all[[i]]))$ci.lb[1])
      uci   <- exp(coef(summary(ma_path_coinf_young_vlow_low_all[[i]]))$ci.ub[1])    
      group <- "young, vlow & low mortality"
      pcr_coeff <- exp(coef(summary(ma_path_coinf_young_vlow_low_all[[i]]))$estimate[2]) # extract coeff for "pcr" only (second coeff in list, not always pcr!)
      pcr_lci   <- exp(coef(summary(ma_path_coinf_young_vlow_low_all[[i]]))$ci.lb[2])
      pcr_uci   <- exp(coef(summary(ma_path_coinf_young_vlow_low_all[[i]]))$ci.ub[2]) 
      put(coeff, lci, uci, group, pcr_coeff, pcr_lci, pcr_uci)
    }
    ma_path_coinf_young_vlow_low_all_results <-magic_result_as_dataframe()
  # results in a dataframe
    names <- data_coinf_labels_young_vlow_low_all
    ma_path_coinf_young_vlow_low_all_results <- data.frame(names, ma_path_coinf_young_vlow_low_all_results)   
    ma_path_coinf_young_vlow_low_all_results$i <- NULL
    magic_free()    
  # dropping pcr data when there are none
    ma_path_coinf_young_vlow_low_all_results$pcr_coeff <- ifelse(ma_path_coinf_young_vlow_low_all_results$names == "sapo", NA, ma_path_coinf_young_vlow_low_all_results$pcr_coeff)
    ma_path_coinf_young_vlow_low_all_results$pcr_lci <- ifelse(ma_path_coinf_young_vlow_low_all_results$names == "sapo", NA, ma_path_coinf_young_vlow_low_all_results$pcr_lci)
    ma_path_coinf_young_vlow_low_all_results$pcr_uci <- ifelse(ma_path_coinf_young_vlow_low_all_results$names == "sapo", NA, ma_path_coinf_young_vlow_low_all_results$pcr_uci)
    
    
### old/all child mort levels ### 
    
  # data list to loop through  
    data_coinf_list_old_all <- c('data_coinf_old_noro', 'data_coinf_old_rota', 
                                 'data_coinf_old_campy', 'data_coinf_old_epec',  'data_coinf_old_etec', 'data_coinf_old_salm', 'data_coinf_old_shig',
                                 'data_coinf_old_crypto', 'data_coinf_old_ehist', 'data_coinf_old_giard',
                                 'data_coinf_old_etec_st', 'data_coinf_old_etec_lt', 'data_coinf_old_noro_gi', 'data_coinf_old_noro_gii', 'data_coinf_old_rota_pre')
    # too few obs for adeno, aero, astro, chol, sapo
  # labels for results
    data_coinf_labels_old_all <- c('noro', 'rota',
                                   'campy', 'epec',  'etec', 'salm', 'shig',
                                   'crypto', 'ehist', 'giard',
                                   'etec_st', 'etec_lt', 'noro_gi', 'noro_gii', 'rota_pre')
  # model
    ma_path_coinf_old_all <- list()
    for(i in seq_along(data_coinf_list_old_all)){ 
      ma_path_coinf_old_all[[i]] <- meta_analysis_loop_all(get(data_coinf_list_old_all[i]))
    }  
  # examine results  
    for (i in 1:15){
      print(ma_path_coinf_old_all[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:15) {
      coeff <- exp(coef(summary(ma_path_coinf_old_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
      lci   <- exp(coef(summary(ma_path_coinf_old_all[[i]]))$ci.lb[1])
      uci   <- exp(coef(summary(ma_path_coinf_old_all[[i]]))$ci.ub[1])    
      group <- "old, all mortality levels"
      pcr_coeff <- exp(coef(summary(ma_path_coinf_old_all[[i]]))$estimate[2]) # extract coeff for "pcr" only (second coeff in list, not always pcr!) 
      pcr_lci   <- exp(coef(summary(ma_path_coinf_old_all[[i]]))$ci.lb[2])
      pcr_uci   <- exp(coef(summary(ma_path_coinf_old_all[[i]]))$ci.ub[2])   
      put(coeff, lci, uci, group, pcr_coeff, pcr_lci, pcr_uci)
    }
    ma_path_coinf_old_all_results <-magic_result_as_dataframe()
  # results in a dataframe
    names <- data_coinf_labels_old_all
    ma_path_coinf_old_all_results <- data.frame(names, ma_path_coinf_old_all_results)   
    ma_path_coinf_old_all_results$i <- NULL
    magic_free()      
  # dropping "pcr" info when none available
    ma_path_coinf_old_all_results$pcr_coeff <- ifelse((ma_path_coinf_old_all_results$names == "noro" | ma_path_coinf_old_all_results$names == "shig" | ma_path_coinf_old_all_results$names == "ehist"), 
                                                   NA, ma_path_coinf_old_all_results$pcr_coeff)
    ma_path_coinf_old_all_results$pcr_lci <- ifelse((ma_path_coinf_old_all_results$names == "noro" | ma_path_coinf_old_all_results$names == "shig" | ma_path_coinf_old_all_results$names == "ehist"), 
                                                 NA, ma_path_coinf_old_all_results$pcr_lci)
    ma_path_coinf_old_all_results$pcr_uci <- ifelse((ma_path_coinf_old_all_results$names == "noro" | ma_path_coinf_old_all_results$names == "shig" | ma_path_coinf_old_all_results$names == "ehist"), 
                                                 NA, ma_path_coinf_old_all_results$pcr_uci)
    
    
    
# combinding all datasets
    ma_coinf_combined_results <- rbind(ma_path_coinf_unadj_results, 
                                       ma_path_coinf_young_high_all_results, ma_path_coinf_young_vlow_low_all_results,
                                       ma_path_coinf_old_all_results)
  # formatting
    ma_coinf_combined_results$coeff  <- format(round(ma_coinf_combined_results$coeff, 1), nsmall=1, trim=TRUE)
    ma_coinf_combined_results$lci    <- format(round(ma_coinf_combined_results$lci, 1), nsmall=1, trim=TRUE)
    ma_coinf_combined_results$uci    <- format(round(ma_coinf_combined_results$uci, 1), nsmall=1, trim=TRUE)
    ma_coinf_combined_results$pcr_coeff  <- format(round(ma_coinf_combined_results$pcr_coeff, 1), nsmall=1, trim=TRUE)
    ma_coinf_combined_results$pcr_lci    <- format(round(ma_coinf_combined_results$pcr_lci, 1), nsmall=1, trim=TRUE)
    ma_coinf_combined_results$pcr_uci    <- format(round(ma_coinf_combined_results$pcr_uci, 1), nsmall=1, trim=TRUE)
  # creating variable with effect estimate and CI for pathogen
    ma_coinf_combined_results$part1  <- paste(ma_coinf_combined_results$coeff, ma_coinf_combined_results$lci, sep=" (")
    ma_coinf_combined_results$part2  <- paste(ma_coinf_combined_results$part1, ma_coinf_combined_results$uci, sep=", ")
    ma_coinf_combined_results$part3  <- ")"
    ma_coinf_combined_results$ee_ci  <- paste(ma_coinf_combined_results$part2, ma_coinf_combined_results$part3, sep="")
  # cleaning/dropping unnecessary variables
    ma_coinf_combined_results$part1 <- NULL
    ma_coinf_combined_results$part2 <- NULL
    ma_coinf_combined_results$part3 <- NULL
  # creating variable with effect estimate and CI for pcr
    ma_coinf_combined_results$part1  <- paste(ma_coinf_combined_results$pcr_coeff, ma_coinf_combined_results$pcr_lci, sep=" (")
    ma_coinf_combined_results$part2  <- paste(ma_coinf_combined_results$part1, ma_coinf_combined_results$pcr_uci, sep=", ")
    ma_coinf_combined_results$part3  <- ")"
    ma_coinf_combined_results$pcr_ci  <- paste(ma_coinf_combined_results$part2, ma_coinf_combined_results$part3, sep="")
    # cleaning/dropping unnecessary variables
      ma_coinf_combined_results$part1 <- NULL
      ma_coinf_combined_results$part2 <- NULL
      ma_coinf_combined_results$part3 <- NULL
  # creating larger/broader group variable (for plotting later)
    ma_coinf_combined_results$strat <- ifelse(ma_coinf_combined_results$group == 'unadj', 'unadjusted',
                                       ifelse((ma_coinf_combined_results$group == 'young, high mortality' |
                                               ma_coinf_combined_results$group == 'young, vlow & low mortality'), '0-5 years',
                                       ifelse(ma_coinf_combined_results$group == 'old, all mortality levels', '>5 years', NA)))
    
  # switching from long to wide structure
    # keep only needed variables
      ma_coinf_combined_results_wide <- ma_coinf_combined_results %>% select(names, group, ee_ci, pcr_ci)
      ma_coinf_combined_results_wide <- ma_coinf_combined_results_wide %>% pivot_wider(names_from = c(group), values_from = c(ee_ci, pcr_ci))
    # switching structure
      ma_coinf_combined_results_wide <- ma_coinf_combined_results_wide[, c(1,2,4,3,5)]
      ma_coinf_combined_results_wide <- ma_coinf_combined_results_wide %>%
                                        mutate(names =  factor(names, levels = ordered_path)) %>%
                                        arrange(names)
    
    
  # writing to csv
    write.csv(ma_coinf_combined_results_wide, file = "/Users/juliabaker/Box Sync/WHO BoD/OR Analysis/OR Deliverables/Julia/Results/ma_coinf_combined_results_wide.csv")
    
            

##################################################################
####       RUNNING MODELS- **NEW METHOD** ALL PREDICTORS      ####
####          STRATIFICATION BY DETECTION METHOD ONLY         ####
####            LOOPING THROUGH PATHOGEN DATAFRAMES           ####
####                     Dec 04, 2020                         ####
##################################################################         
    
# dataframes with only those tested by PCR and only those not tested by pcr
  case_control_pcr <- case_control_full[ which(case_control_full$det_meth_2 == "PCR"), ]
  # now down to 624 obs
  case_control_no_pcr <- case_control_full[ which(case_control_full$det_meth_2!= "PCR"), ]
  # now down to 601 obs 
    
  ### prep dataframes ###
    
  # each pathogen & strain- PCR only
    pathogen_data_pcr_list <- unique(case_control_pcr$path)
    for (v in pathogen_data_pcr_list) {
      assign(paste0("data_pcr_", as.character(v)), case_control_pcr %>% filter (path == v), envir = .GlobalEnv)
    } 
    data_pcr_etec_st   <- case_control_pcr[ which(case_control_pcr$path_strain == "etec_st"), ]
    data_pcr_etec_lt   <- case_control_pcr[ which(case_control_pcr$path_strain == "etec_lt"), ]
    data_pcr_noro_gi   <- case_control_pcr[ which(case_control_pcr$path_strain == "noro_gi"), ]
    data_pcr_noro_gii  <- case_control_pcr[ which(case_control_pcr$path_strain == "noro_gii"), ]
    data_pcr_rota_pre  <- case_control_pcr[ which(case_control_pcr$path_strain == "rota_pre"), ]
    data_pcr_rota_post <- case_control_pcr[ which(case_control_pcr$path_strain == "rota_post"), ]
    
  # each pathogen & strain- no PCR
    pathogen_data_no_pcr_list <- unique(case_control_no_pcr$path)
    for (v in pathogen_data_no_pcr_list) {
      assign(paste0("data_no_pcr_", as.character(v)), case_control_no_pcr %>% filter (path == v), envir = .GlobalEnv)
    } 
    data_no_pcr_etec_st   <- case_control_no_pcr[ which(case_control_no_pcr$path_strain == "etec_st"), ]
    data_no_pcr_etec_lt   <- case_control_no_pcr[ which(case_control_no_pcr$path_strain == "etec_lt"), ]
    data_no_pcr_noro_gi   <- case_control_no_pcr[ which(case_control_no_pcr$path_strain == "noro_gi"), ]
    data_no_pcr_noro_gii  <- case_control_no_pcr[ which(case_control_no_pcr$path_strain == "noro_gii"), ]
    data_no_pcr_rota_pre  <- case_control_no_pcr[ which(case_control_no_pcr$path_strain == "rota_pre"), ]
    data_no_pcr_rota_post <- case_control_no_pcr[ which(case_control_no_pcr$path_strain == "rota_post"), ]
    

### unstratified ###
    
  # meta-analysis loop function
    meta_analysis_loop_unadj <- function(data){
      rma(yi = log_ee, vi = var,
          method = "DL", 
          data = data, 
          slab=paste(author, year_pub, sep=", "))
    }       
    
  ### PCR only ###
    
  # data list to loop through  
    data_pcr_list_unadj <- c('data_pcr_adeno', 'data_pcr_astro', 'data_pcr_noro', 'data_pcr_rota', 'data_pcr_sapo',
                             'data_pcr_aero', 'data_pcr_campy', 'data_pcr_chol', 'data_pcr_epec',  'data_pcr_etec', 'data_pcr_salm', 'data_pcr_shig', 
                             'data_pcr_crypto', 'data_pcr_ehist', 'data_pcr_giard', 
                             'data_pcr_etec_st', 'data_pcr_etec_lt', 'data_pcr_noro_gi', 'data_pcr_noro_gii', 'data_pcr_rota_pre', 'data_pcr_rota_post')
  # labels for results
    data_pcr_labels_unadj <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                               'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                               'crypto', 'ehist', 'giard', 
                               'etec_st', 'etec_lt', 'noro_gi', 'noro_gii', 'rota_pre', 'rota_post')
  # model
    ma_path_pcr_unadj <- list()
    for(i in seq_along(data_pcr_list_unadj)){ 
      ma_path_pcr_unadj[[i]] <- meta_analysis_loop_unadj(get(data_pcr_list_unadj[i]))
    }  
  # examine results  
    for (i in 1:21){
      print(ma_path_pcr_unadj[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:21) {
      coeff <- exp(coef(summary(ma_path_pcr_unadj[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
      lci   <- exp(coef(summary(ma_path_pcr_unadj[[i]]))$ci.lb[1])
      uci   <- exp(coef(summary(ma_path_pcr_unadj[[i]]))$ci.ub[1])    
      group <- "pcr"
      put(coeff, lci, uci, group)
    }
    ma_path_pcr_unadj_results <-magic_result_as_dataframe()
  # results in a dataframe
    names <- data_pcr_labels_unadj
    ma_path_pcr_unadj_results <- data.frame(names, ma_path_pcr_unadj_results)   
    ma_path_pcr_unadj_results$i <- NULL
    magic_free()

  ### No PCR only ###
    
  # data list to loop through  
    data_no_pcr_list_unadj <- c('data_no_pcr_adeno', 'data_no_pcr_astro', 'data_no_pcr_noro', 'data_no_pcr_rota', 'data_no_pcr_sapo',
                                'data_no_pcr_aero', 'data_no_pcr_campy', 'data_no_pcr_chol', 'data_no_pcr_epec',  'data_no_pcr_etec', 'data_no_pcr_salm', 'data_no_pcr_shig', 
                                'data_no_pcr_crypto', 'data_no_pcr_ehist', 'data_no_pcr_giard', 
                                'data_no_pcr_etec_st', 'data_no_pcr_etec_lt', 'data_no_pcr_noro_gi', 'data_no_pcr_noro_gii', 'data_no_pcr_rota_pre', 'data_no_pcr_rota_post')
  # labels for results
    data_no_pcr_labels_unadj <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                  'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                                  'crypto', 'ehist', 'giard', 
                                  'etec_st', 'etec_lt', 'noro_gi', 'noro_gii', 'rota_pre', 'rota_post')
  # model
    ma_path_no_pcr_unadj <- list()
    for(i in seq_along(data_no_pcr_list_unadj)){ 
      ma_path_no_pcr_unadj[[i]] <- meta_analysis_loop_unadj(get(data_no_pcr_list_unadj[i]))
    }  
  # examine results  
    for (i in 1:21){
      print(ma_path_no_pcr_unadj[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:21) {
      coeff <- exp(coef(summary(ma_path_no_pcr_unadj[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
      lci   <- exp(coef(summary(ma_path_no_pcr_unadj[[i]]))$ci.lb[1])
      uci   <- exp(coef(summary(ma_path_no_pcr_unadj[[i]]))$ci.ub[1])    
      group <- "no pcr"
      put(coeff, lci, uci, group)
    }
    ma_path_no_pcr_unadj_results <-magic_result_as_dataframe()
  # results in a dataframe
    names <- data_no_pcr_labels_unadj
    ma_path_no_pcr_unadj_results <- data.frame(names, ma_path_no_pcr_unadj_results)   
    ma_path_no_pcr_unadj_results$i <- NULL
    magic_free()  
    
    
  # combinding all datasets
    ma_pcr_combined_results <- rbind(ma_path_pcr_unadj_results, ma_path_no_pcr_unadj_results)
    # formatting
      ma_pcr_combined_results$coeff  <- format(round(ma_pcr_combined_results$coeff, 1), nsmall=1, trim = TRUE)
      ma_pcr_combined_results$lci    <- format(round(ma_pcr_combined_results$lci, 1), nsmall=1, trim = TRUE)
      ma_pcr_combined_results$uci    <- format(round(ma_pcr_combined_results$uci, 1), nsmall=1, trim = TRUE)
      # creating variable with effect estimate and CI for pathogen
        ma_pcr_combined_results$part1  <- paste(ma_pcr_combined_results$coeff, ma_pcr_combined_results$lci, sep=" (")
        ma_pcr_combined_results$part2  <- paste(ma_pcr_combined_results$part1, ma_pcr_combined_results$uci, sep=", ")
        ma_pcr_combined_results$part3  <- ")"
        ma_pcr_combined_results$ee_ci  <- paste(ma_pcr_combined_results$part2, ma_pcr_combined_results$part3, sep="")
        # cleaning/dropping unnecessary variables
          ma_pcr_combined_results$part1 <- NULL
          ma_pcr_combined_results$part2 <- NULL
          ma_pcr_combined_results$part3 <- NULL
 
  # switching from long to wide structure
    # keep only needed variables
      ma_pcr_combined_results_wide <- ma_pcr_combined_results %>% select(names, group, ee_ci)
      ma_pcr_combined_results_wide <- ma_pcr_combined_results_wide %>% pivot_wider(names_from = c(group), values_from = c(ee_ci))
    # switching structure
      ma_pcr_combined_results_wide <- ma_pcr_combined_results_wide %>%
                                      mutate(names =  factor(names, levels = ordered_path)) %>%
                                      arrange(names)
  
  # writing to csv
    write.csv(ma_pcr_combined_results_wide, file = "/Users/juliabaker/Box Sync/WHO BoD/OR Analysis/OR Deliverables/Julia/Results/ma_pcr_combined_results.csv")
    
    
        
  
#########################################
####          MODEL PLOTS            ####
#########################################    
  
  
# MAIN PLOT FOR MANUSCRIPT- NOV 17, 2020
  
  
  # midpoint decimals (for Lancet formatting)
    options(OutDec="·")
    options(digits=1)
  # renaming dataset for Lancet
    ma_new2_combined_results_lancet <- ma_new2_combined_results
  
    options(OutDec=".")
    
    # formatting
      ma_new2_combined_results_lancet$coeff2  <- format(round(ma_new2_combined_results_lancet$coeff, 1), nsmall=1, trim=TRUE)
      ma_new2_combined_results_lancet$lci2    <- format(round(ma_new2_combined_results_lancet$lci, 1), nsmall=1, trim=TRUE)
      ma_new2_combined_results_lancet$uci2    <- format(round(ma_new2_combined_results_lancet$uci, 1), nsmall=1, trim=TRUE)
      ma_new2_combined_results_lancet$pcr_coeff2  <- format(round(ma_new2_combined_results_lancet$pcr_coeff, 1), nsmall=1, trim=TRUE)
      ma_new2_combined_results_lancet$pcr_lci2    <- format(round(ma_new2_combined_results_lancet$pcr_lci, 1), nsmall=1, trim=TRUE)
      ma_new2_combined_results_lancet$pcr_uci2    <- format(round(ma_new2_combined_results_lancet$pcr_uci, 1), nsmall=1, trim=TRUE)
      # creating variable with effect estimate and CI for pathogen
        ma_new2_combined_results_lancet$part1  <- paste(ma_new2_combined_results_lancet$coeff2, ma_new2_combined_results_lancet$lci2, sep=" (")
        ma_new2_combined_results_lancet$part2  <- paste(ma_new2_combined_results_lancet$part1, ma_new2_combined_results_lancet$uci2, sep=", ")
        ma_new2_combined_results_lancet$part3  <- ")"
        ma_new2_combined_results_lancet$ee_ci  <- paste(ma_new2_combined_results_lancet$part2, ma_new2_combined_results_lancet$part3, sep="")
      # cleaning/dropping unnecessary variables
        ma_new2_combined_results_lancet$part1 <- NULL
        ma_new2_combined_results_lancet$part2 <- NULL
        ma_new2_combined_results_lancet$part3 <- NULL
      # creating variable with effect estimate and CI for pcr
        ma_new2_combined_results_lancet$part1  <- paste(ma_new2_combined_results_lancet$pcr_coeff2, ma_new2_combined_results_lancet$pcr_lci2, sep=" (")
        ma_new2_combined_results_lancet$part2  <- paste(ma_new2_combined_results_lancet$part1, ma_new2_combined_results_lancet$pcr_uci2, sep=", ")
        ma_new2_combined_results_lancet$part3  <- ")"
        ma_new2_combined_results_lancet$pcr_ci  <- paste(ma_new2_combined_results_lancet$part2, ma_new2_combined_results_lancet$part3, sep="")
      # cleaning/dropping unnecessary variables
        ma_new2_combined_results_lancet$part1 <- NULL
        ma_new2_combined_results_lancet$part2 <- NULL
        ma_new2_combined_results_lancet$part3 <- NULL
    
    
  # format order of groups
    ma_new2_combined_results_lancet$group_ordered <- factor(ma_new2_combined_results_lancet$group, 
                                                     levels = c("unadj", "young, vlow & low mortality", "young, high mortality", "old, all mortality levels"))
  
    ma_new2_combined_results_lancet$path_ordered <- factor(ma_new2_combined_results_lancet$names, 
                                                    levels =c('adeno', 'astro', 'noro', 'noro_gi', 'noro_gii', 'rota', 'rota_pre', 'rota_post', 'sapo',
                                                              'aero', 'campy', 'chol', 'epec',  'etec',  'etec_st', 'etec_lt', 'salm', 'shig', 
                                                              'crypto', 'ehist', 'giard'))
  
    pathogen_names <- list(
      'adeno'='Adenovirus 40/41', 
      'astro'='Astrovirus', 
      'noro' ='Norovirus (all)', 
      'noro_gi'='Norovirus GI', 
      'noro_gii'='Norovirus GII', 
      'rota'='Rotavirus (all)', 
      'rota_pre'='Rotavirus (pre-vaccine)', 
      'rota_post'='Rotavirus (post-vaccine)', 
      'sapo'='Sapovirus',
      'aero'='Aeromonas', 
      'campy'='Campylobacter', 
      'chol'='V. cholerae', 
      'epec'='EPEC',  
      'etec'='ETEC (all)',  
      'etec_st'='ST ETEC', 
      'etec_lt'='LT ETEC', 
      'salm'='Salmonella', 
      'shig'='Shigella', 
      'crypto'='Cryptosporidium', 
      'ehist'='E. histolytica', 
      'giard'='Giardia lamblia')
    
    pathogen_labeller <- function(variable,value){
      return(pathogen_names[value])
    }
 
  
  pdf("path_ORs_wrap_2.pdf", width=8, height=11)
  print()
      ggplot(ma_new2_combined_results_lancet) +
        geom_pointrange(aes(x=group_ordered, y=coeff, ymin=lci, ymax=uci, color=group_ordered),
                        position = position_dodge(width = 1), size=.5) +
        scale_y_log10() +
        scale_color_discrete(name="Model", labels = c("Unadjusted and unstratified", "0-5 years, very low/low child mortality settings", "0-5 years, high child mortality settings", ">5 years, all child mortality settings")) +
        labs(x = " ", y = "Effect estimate (summary OR)", colour = "Model") +
        theme_bw() +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.background = element_rect(fill="white")) +
        theme(plot.margin = unit(c(1,5,1,1), "lines")) +
        coord_flip(ylim = c(.1, 70), clip='off') +
        geom_text(aes(x=group_ordered, y=110, label = ee_ci),size=2, hjust=0) +
        #geom_vline(yintercept=0) +
        facet_wrap(vars(path_ordered),labeller=pathogen_labeller, ncol=3) +
        theme(panel.spacing.x = unit(3, "lines"), legend.direction = "vertical", legend.position = "bottom") +
        guides(color = guide_legend(reverse = TRUE))
      dev.off()
    

# NEW FIGURE MAY 01-- UNADJUSTED, STRATIFIED RESULTS
  
  # ordering groups and strata
    ma_new2_combined_results$group <- factor(ma_new2_combined_results$group, levels=c('unadj', 'young, very low mortality', 'young, low mortality', 'young, high mortality', 'old, all mortality levels'))
    ma_new2_combined_results$strat <- factor(ma_new2_combined_results$strat, levels=c('unadjusted', '0-5 years', '>5 years'))
  
  # figure  
    pdf("/Users/juliabaker/Box Sync/WHO BoD/OR Analysis/OR Deliverables/Julia/Figures/all_pathogens_new2.pdf")
  # make figure for each pathogen
    for (k in unique(ma_new2_combined_results$name)){
      path_new2 <- subset(ma_new2_combined_results, names == k)
    
    print(ggplot(path_new2, aes(x=group, y=coeff, group = strat, color = strat)) +
            geom_pointrange(aes(ymin=lci, ymax=uci)) +
            coord_cartesian(ylim = c(0.5, 10)) +
            scale_y_log10() +
            geom_hline(yintercept=1, color = "grey") +
            labs(x = "Predictor", y = "Effect estimate") +
            scale_colour_manual(values = c("red", "royalblue1", "green3")) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                  axis.text.y = element_text(size = 12), 
                  axis.title.x = element_text(size = 14),
                  axis.title.y = element_text(size = 14),
                  axis.line = element_line(colour = "grey"),
                  panel.background = element_rect(fill="white"),
                  legend.key = element_rect(fill = "white"),
                  legend.text=element_text(size = 12)) +
            ggtitle(as.character(k)))
    }
    dev.off()  
  
  
                    
#########################################
####          FOREST PLOTS           ####
#########################################

    data_labels_unadj <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                           'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                           'crypto', 'ehist', 'giard', 
                           'etec_st', 'etec_lt', 'noro_gi', 'noro_gii', 'rota_pre', 'rota_post')
   
 
 pdf(file='Forest plots- all-test.pdf', width=8.5, height=11)
 for (i in 1:21){
  forest(ma_path_unadj[[i]],
        order = "obs",
        atransf=exp,
        refline = 0,
        xlim=c(-8,10),
        #cex.axis = .9,
        #cex.lab = .9, 
        digits=1L,
        top= 1,
        psize=1,
        xlab = "Effect estimate",
        header="Author, year, subset #")
 }
 dev.off() 
 
 
 
 pdf(file='Forest plots- all-test2.pdf', width=8.5, height=9)
 for (i in 1:21){
   forest(ma_path_unadj[[i]],
          order = "obs",
          atransf=exp,
          refline = 0,
          xlim=c(-8,10),
          #cex.axis = .9,
          #cex.lab = .9, 
          digits=1L,
          top= 1,
          psize=1,
          xlab = "Effect estimate",
          header="Author, year, subset #")
 }
 dev.off() 
 
 
 
