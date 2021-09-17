#######################################################################
#
# WHO Burden of Diseases OR analysis- Lit Review Data
# Data analysis
# Aug 2021
#   
#######################################################################


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


  
# load dataframe for analysis
  load("case_control_clean.Rdata")

  
# dataframe with all observations (keeping outliers/influential) but dropping those with missing info
  # dropping observations with...
    # NA for variance
      case_control_full <- case_control_clean[!is.na(case_control_clean$var), ]
      # 405 obs dropped here b/c of NA resulting from 0s in 2x2 table, typically among controls
        length(unique(case_control_full$author_year_title))
        # now down to 1240 observations & 130 studies
      write.csv(case_control_full,file="case_control_full.csv")
     
# sensitivity analyses       
  # dataframe without the outliers/influential observations
    case_control_limited <- case_control_full[ which(case_control_full$infl != 1), ]
    # 27 outliers dropped
      length(unique(case_control_limited$author_year_title))
      # now down to 1213 obs and 129 studies
  # dataframe with only case-control studies (excludes other study types) but includes outliers
    case_control_limited2 <- case_control_full[ which(case_control_full$study_design == "case control"), ]
      length(unique(case_control_limited2$author_year_title))
    # now down to 999 obs and 93 studies

      
# each pathogen & strain & rota pre/post (using 1 year lag on WHO vax intro year) & high child mortality settings in WPRO and AFRO
  # pathogens
    pathogen_data_list <- unique(case_control_full$path)
      for (v in pathogen_data_list) {
        assign(paste0("data_", as.character(v)), case_control_full %>% filter (path == v), envir = .GlobalEnv)
      } 
  # pathogen strains
    strain_data_list <- unique(case_control_full$path_strain)
      for (s in strain_data_list) {
        assign(paste0("data_", as.character(s)), case_control_full %>% filter (path_strain == s), envir = .GlobalEnv)
      } 
  # rota pre/post
    data_rota_pre  <- case_control_full[ which(case_control_full$path == "rota" & case_control_full$rota_vax_lag == 0),]
    data_rota_post <- case_control_full[ which(case_control_full$path == "rota" & case_control_full$rota_vax_lag == 1),]
  # young/high child mortality and region
    data_young_afro_high <- case_control_full[ which(case_control_full$age_group == "young" & 
                                                     case_control_full$child_mort == "high" & 
                                                     case_control_full$who_region == "AFRO"),]
    data_young_asia_high <- case_control_full[ which(case_control_full$age_group == "young" &
                                                     case_control_full$child_mort == "high" &
                                                     (case_control_full$who_region == "SEARO" | case_control_full$who_region == "WPRO")), ]
    
    
 # dataframe for each stratification (age group and child mortality level)
    
    # age group (age_group: young, mixed, old; age_group_2: youngest, middle)
      data_young <- case_control_full[ which(case_control_full$age_group == "young"), ]
      data_old   <- case_control_full[ which(case_control_full$age_group == "old"), ]
      data_youngest   <- case_control_full[ which(case_control_full$age_group_2 == "youngest"), ]
      data_middle     <- case_control_full[ which(case_control_full$age_group_2 == "middle"), ]
      
    # young age by child mortality
      data_young_high     <- data_young[ which(data_young$child_mort == "high"), ]
      data_young_vlow_low <- data_young[ which((data_young$child_mort == "very low" | data_young$child_mort == "low")), ]
      data_youngest_high     <- data_youngest[ which(data_youngest$child_mort == "high"), ]
      data_youngest_vlow_low <- data_youngest[ which((data_youngest$child_mort == "very low" | data_youngest$child_mort == "low")), ]
      data_middle_high     <- data_middle[ which(data_middle$child_mort == "high"), ]
      data_middle_vlow_low <- data_middle[ which((data_middle$child_mort == "very low" | data_middle$child_mort == "low")), ]
      
      # each pathogen and strain within the age/child mort dataframes
        # young/high
          for (v in pathogen_data_list) {
            assign(paste0("data_young_high_", as.character(v)), data_young_high %>% filter (path == v), envir = .GlobalEnv)
            }
          for (u in strain_data_list) {
            assign(paste0("data_young_high_", as.character(u)), data_young_high %>% filter (path_strain == u), envir = .GlobalEnv)
            } 
          data_young_high_rota_pre  <- data_young_high[ which(data_young_high$path == "rota" & data_young_high$rota_vax_lag == 0),]
          data_young_high_rota_post <- data_young_high[ which(data_young_high$path == "rota" & data_young_high$rota_vax_lag == 1),]
      
          
        # young/very low/low
          for (v in pathogen_data_list) {
            assign(paste0("data_young_vlow_low_", as.character(v)), data_young_vlow_low %>% filter (path == v), envir = .GlobalEnv)
            }
          for (u in strain_data_list) {
            assign(paste0("data_young_vlow_low_", as.character(u)), data_young_vlow_low %>% filter (path_strain == u), envir = .GlobalEnv)
            } 
          data_young_vlow_low_rota_pre  <- data_young_vlow_low[ which(data_young_vlow_low$path == "rota" & data_young_vlow_low$rota_vax_lag == 0),]
          data_young_vlow_low_rota_post <- data_young_vlow_low[ which(data_young_vlow_low$path == "rota" & data_young_vlow_low$rota_vax_lag == 1),]
          
          
        # old/all child mortality levels
          for (v in pathogen_data_list) {
            assign(paste0("data_old_", as.character(v)), data_old %>% filter (path == v), envir = .GlobalEnv)
            }
          for (u in strain_data_list) {
            assign(paste0("data_old_", as.character(u)), data_old %>% filter (path_strain == u), envir = .GlobalEnv)
            } 
          data_old_rota_pre  <- data_old[ which(data_old$path == "rota" & data_old$rota_vax_lag == 0),]
          data_old_rota_post <- data_old[ which(data_old$path == "rota" & data_old$rota_vax_lag == 1),]
          
          
        # youngest/all child mortality levels
          for (v in pathogen_data_list) {
            assign(paste0("data_youngest_", as.character(v)), data_youngest %>% filter (path == v), envir = .GlobalEnv)
          }
          for (u in strain_data_list) {
            assign(paste0("data_youngest_", as.character(u)), data_youngest %>% filter (path_strain == u), envir = .GlobalEnv)
          } 
          data_youngest_rota_pre  <- data_youngest[ which(data_youngest$path == "rota" & data_youngest$rota_vax_lag == 0),]
          data_youngest_rota_post <- data_youngest[ which(data_youngest$path == "rota" & data_youngest$rota_vax_lag == 1),]  
          
        # middle/all child mortality levels
          for (v in pathogen_data_list) {
            assign(paste0("data_middle_", as.character(v)), data_middle %>% filter (path == v), envir = .GlobalEnv)
          }
          for (u in strain_data_list) {
            assign(paste0("data_middle_", as.character(u)), data_middle %>% filter (path_strain == u), envir = .GlobalEnv)
          } 
          data_middle_rota_pre  <- data_middle[ which(data_middle$path == "rota" & data_middle$rota_vax_lag == 0),]
          data_middle_rota_post <- data_middle[ which(data_middle$path == "rota" & data_middle$rota_vax_lag == 1),]    
          
          
        # youngest/high
          for (v in pathogen_data_list) {
            assign(paste0("data_youngest_high_", as.character(v)), data_youngest_high %>% filter (path == v), envir = .GlobalEnv)
          }
          for (u in strain_data_list) {
            assign(paste0("data_youngest_high_", as.character(u)), data_youngest_high %>% filter (path_strain == u), envir = .GlobalEnv)
          } 
          data_youngest_high_rota_pre  <- data_youngest_high[ which(data_youngest_high$path == "rota" & data_youngest_high$rota_vax_lag == 0),]
          data_youngest_high_rota_post <- data_youngest_high[ which(data_youngest_high$path == "rota" & data_youngest_high$rota_vax_lag == 1),]
          
         # youngest/very low/low
          for (v in pathogen_data_list) {
            assign(paste0("data_youngest_vlow_low_", as.character(v)), data_youngest_vlow_low %>% filter (path == v), envir = .GlobalEnv)
          }
          for (u in strain_data_list) {
            assign(paste0("data_youngest_vlow_low_", as.character(u)), data_youngest_vlow_low %>% filter (path_strain == u), envir = .GlobalEnv)
          } 
          data_youngest_vlow_low_rota_pre  <- data_youngest_vlow_low[ which(data_youngest_vlow_low$path == "rota" & data_youngest_vlow_low$rota_vax_lag == 0),]
          data_youngest_vlow_low_rota_post <- data_youngest_vlow_low[ which(data_youngest_vlow_low$path == "rota" & data_youngest_vlow_low$rota_vax_lag == 1),]
            
          
        # middle/high
          for (v in pathogen_data_list) {
            assign(paste0("data_middle_high_", as.character(v)), data_middle_high %>% filter (path == v), envir = .GlobalEnv)
          }
          for (u in strain_data_list) {
            assign(paste0("data_middle_high_", as.character(u)), data_middle_high %>% filter (path_strain == u), envir = .GlobalEnv)
          } 
          data_middle_high_rota_pre  <- data_middle_high[ which(data_middle_high$path == "rota" & data_middle_high$rota_vax_lag == 0),]
          data_middle_high_rota_post <- data_middle_high[ which(data_middle_high$path == "rota" & data_middle_high$rota_vax_lag == 1),]
          
          # middle/very low/low
          for (v in pathogen_data_list) {
            assign(paste0("data_middle_vlow_low_", as.character(v)), data_middle_vlow_low %>% filter (path == v), envir = .GlobalEnv)
          }
          for (u in strain_data_list) {
            assign(paste0("data_middle_vlow_low_", as.character(u)), data_middle_vlow_low %>% filter (path_strain == u), envir = .GlobalEnv)
          } 
          data_middle_vlow_low_rota_pre  <- data_middle_vlow_low[ which(data_middle_vlow_low$path == "rota" & data_middle_vlow_low$rota_vax_lag == 0),]
          data_middle_vlow_low_rota_post <- data_middle_vlow_low[ which(data_middle_vlow_low$path == "rota" & data_middle_vlow_low$rota_vax_lag == 1),]
            
          
        # AFRO/young/high child mortality
          for (v in pathogen_data_list) {
            assign(paste0("data_young_afro_high_", as.character(v)), data_young_afro_high %>% filter (path == v), envir = .GlobalEnv)
          }
          for (u in strain_data_list) {
            assign(paste0("data_young_afro_high_", as.character(u)), data_young_afro_high %>% filter (path_strain == u), envir = .GlobalEnv)
          }
          data_young_afro_high_rota_pre  <- data_young_afro_high[ which(data_young_afro_high$path == "rota" & data_young_afro_high$rota_vax_lag == 0),]
          data_young_afro_high_rota_post <- data_young_afro_high[ which(data_young_afro_high$path == "rota" & data_young_afro_high$rota_vax_lag == 1),]
          
        # Asia/young/high child mortality
          for (v in pathogen_data_list) {
            assign(paste0("data_young_asia_high_", as.character(v)), data_young_asia_high %>% filter (path == v), envir = .GlobalEnv)
          }
          for (u in strain_data_list) {
            assign(paste0("data_young_asia_high_", as.character(u)), data_young_asia_high %>% filter (path_strain == u), envir = .GlobalEnv)
          }
          data_young_asia_high_rota_pre  <- data_young_asia_high[ which(data_young_asia_high$path == "rota" & data_young_asia_high$rota_vax_lag == 0),]
          data_young_asia_high_rota_post <- data_young_asia_high[ which(data_young_asia_high$path == "rota" & data_young_asia_high$rota_vax_lag == 1),]
          
          
          
          
          
#############################################################
####                DESCRIPTIVE STATS                    ####
#############################################################
          
        
### Using "full" dataset ###      

# recategorize "path" to also include the specific strains of interest (epec, etec)
  case_control_full$path_spec <- ifelse((case_control_full$path == 'etec' | case_control_full$path == 'epec'), case_control_full$path_strain, as.character(case_control_full$path))
  table(case_control_full$path_spec, exclude=NULL)                        
  
        
# number of studies and observations (all "included" studies, including mixed age group but excluding those without variance)
  length(unique(case_control_full$author_year_title))
  # 130 studies (including GEMS/MAL-ED), 1240 observations    
        
# number of case/control individuals/samples
  # first extracting one record per study & country
    case_control_full_unique <- case_control_full %>% distinct(author_year_title, country, region, .keep_all = T)
    # dropping GEMS/MAL-ED
      case_control_full_unique_no_maledgems <- case_control_full_unique %>% subset(title != "GEMS" & title != "MALED")
        length(unique(case_control_full_unique_no_maledgems$author_year_title))
        # 128 studies
    # note: this does NOT include MAL-ED and GEMS (cases/controls for these studies are not study-specific but rather stratum-specific)
      n_cases     <- sum(case_control_full_unique_no_maledgems$cases, na.rm=TRUE) 
      n_controls  <- sum(case_control_full_unique_no_maledgems$controls, na.rm=TRUE) 
      n_diar      <- sum(case_control_full_unique_no_maledgems$outcome_cohort, na.rm=TRUE) 
      n_no_diar   <- sum(case_control_full_unique_no_maledgems$no_outcome_cohort, na.rm=TRUE) 
      # for MAL-ED (from James)
      # 5646 cases and 1:1 matched controls (from cc subset)
      # for GEMS (from James)
      # 5304 cases and 1:1 matched controls
    # total cases/diarrhea samples
      n_cases + n_diar + 5646 + 5304
    # total controls/non-diarrhea samples
      n_controls + n_no_diar + 5646 + 5304
      
        
# number of outliers
   table(case_control_full$infl, exclude=NULL)
   
        
        
# table summarizing studies
  # pull n for cases/diar samples and controls/non-diar samples
    case_control_full_unique$case_diar    <- ifelse(case_control_full_unique$study_design == "case control", case_control_full_unique$cases, case_control_full_unique$outcome_cohort)
    case_control_full_unique$cont_no_diar <- ifelse(case_control_full_unique$study_design == "case control", case_control_full_unique$controls, case_control_full_unique$no_outcome_cohort)
    case_control_full_unique$design_desc  <- ifelse(case_control_full_unique$design == 1, "Cross-sectional",
                                             ifelse(case_control_full_unique$design == 2, "Prospective cohort",
                                             ifelse(case_control_full_unique$design == 3, "Retrospective cohort",
                                             ifelse(case_control_full_unique$design == 4, "Randomized controlled trial",
                                             ifelse(case_control_full_unique$design == 6, "Case-control",
                                             ifelse(case_control_full_unique$design == 7, "Nested case-control",
                                             ifelse(case_control_full_unique$design == 88, "Other", "Missing")))))))
  # order alphabetically by author
    case_control_full_unique <- case_control_full_unique[order(case_control_full_unique$author),]
  # create table      
    study_summary <- case_control_full_unique %>% select(author, year_pub, title, design_desc, validity, case_diar, cont_no_diar, country, child_mort_2)
        

  # observations by pathogen
    obs_by_path <- data.frame(case_control_full %>% group_by(path_spec) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
    # add data for all obs combined
      df<-data.frame("total", sum(obs_by_path$n), 100)
      names(df)<- c("path_spec","n","prop")
      obs_by_path <- rbind(obs_by_path, df)
    # getting n (%) set up
      obs_by_path$n_perc_part1 <- paste(obs_by_path$n,format(round(obs_by_path$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
      obs_by_path$n_perc_part2 <- ")"
      obs_by_path$n_perc <- paste(obs_by_path$n_perc_part1, obs_by_path$n_perc_part2, sep="")
    # keeping only needed columns
      obs_by_path <- obs_by_path %>% select(path_spec, n_perc)
    # flip dataset
      obs_by_path_wide <- obs_by_path %>% pivot_wider(names_from = c(path_spec), values_from = c(n_perc))
      obs_by_path_wide$variable <- "total"
    
  # age group 
    path_age <- data.frame(case_control_full %>% group_by(path_spec, age_group_2) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
    # add data for all obs combined
      w <- c("total", "youngest", with(path_age, sum(path_age[age_group_2=="youngest", "n"])), 100)
      x <- c("total", "middle", with(path_age, sum(path_age[age_group_2=="middle", "n"])), 100)
      y <- c("total", "mixed", with(path_age, sum(path_age[age_group_2=="mixed", "n"])), 100)
      z <- c("total", "oldest", with(path_age, sum(path_age[age_group_2=="oldest", "n"])), 100)
      df <- data.frame(t(data.frame(w,x,y,z)))
      rownames(df)<-NULL
      df$X3 <- as.numeric(df$X3)
      df$X4 <- as.numeric(df$X4)
      colnames(df) <- c("path_spec", "age_group_2", "n", "prop")
      path_age <- rbind(path_age, df)
    # getting n (%) set up
      path_age$n_perc_part1 <- paste(path_age$n,format(round(path_age$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
      path_age$n_perc_part2 <- ")"
      path_age$n_perc <- paste(path_age$n_perc_part1, path_age$n_perc_part2, sep="")
    # keeping only needed columns
      path_age <- path_age %>% select(path_spec, age_group_2, n_perc)
    # flip dataset
      path_age_wide <- path_age %>% pivot_wider(names_from = c(path_spec), values_from = c(n_perc))
      names(path_age_wide)[names(path_age_wide) == "age_group_2"] <- "variable"
      
    # age group 1
      path_age_1 <- data.frame(case_control_full %>% group_by(path_spec, age_group) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
      # add data for all obs combined
        x <- c("total", "young", with(path_age_1, sum(path_age_1[age_group=="young", "n"])), 100)
        y <- c("total", "mixed", with(path_age_1, sum(path_age_1[age_group=="mixed", "n"])), 100)
        z <- c("total", "old", with(path_age_1, sum(path_age_1[age_group=="old", "n"])), 100)
        df <- data.frame(t(data.frame(x,y,z)))
        rownames(df)<-NULL
        df$X3 <- as.numeric(df$X3)
        df$X4 <- as.numeric(df$X4)
        colnames(df) <- c("path_spec", "age_group", "n", "prop")
        path_age_1 <- rbind(path_age_1, df)
      # getting n (%) set up
        path_age_1$n_perc_part1 <- paste(path_age_1$n,format(round(path_age_1$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
        path_age_1$n_perc_part2 <- ")"
        path_age_1$n_perc <- paste(path_age_1$n_perc_part1, path_age_1$n_perc_part2, sep="")
      # keeping only needed columns
        path_age_1 <- path_age_1 %>% select(path_spec, age_group, n_perc)
      # flip dataset
        path_age_1_wide <- path_age_1 %>% pivot_wider(names_from = c(path_spec), values_from = c(n_perc))
        names(path_age_1_wide)[names(path_age_1_wide) == "age_group"] <- "variable"
        
       
  # U5MR
    table(case_control_full$child_mort, exclude=NULL)
    path_child_mort <- data.frame(case_control_full %>% group_by(path_spec, child_mort) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
    # add data for all obs combined
      x <- c("total", "high", with(path_child_mort, sum(path_child_mort[child_mort=="high", "n"])), 100)
      y <- c("total", "very low", with(path_child_mort, sum(path_child_mort[child_mort=="very low", "n"])), 100)
      z <- c("total", "low", with(path_child_mort, sum(path_child_mort[child_mort=="low", "n"])), 100)
      df <- data.frame(t(data.frame(x,y,z)))
      rownames(df)<-NULL
      df$X3 <- as.numeric(df$X3)
      df$X4 <- as.numeric(df$X4)
      colnames(df) <- c("path_spec", "child_mort", "n", "prop")
      path_child_mort <- rbind(path_child_mort, df)
    # getting n (%) set up
      path_child_mort$n_perc_part1 <- paste(path_child_mort$n,format(round(path_child_mort$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
      path_child_mort$n_perc_part2 <- ")"
      path_child_mort$n_perc <- paste(path_child_mort$n_perc_part1, path_child_mort$n_perc_part2, sep="")
    # keeping only needed columns
      path_child_mort <- path_child_mort %>% select(path_spec, child_mort, n_perc)
    # flip dataset
      path_child_mort_wide <- path_child_mort %>% pivot_wider(names_from = c(path_spec), values_from = c(n_perc)) 
      names(path_child_mort_wide)[names(path_child_mort_wide) == "child_mort"] <- "variable"
      
  # pathogen detection method
    path_det_meth <- data.frame(case_control_full %>% group_by(path_spec, det_meth_desc) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
      # change NAs to missing
        path_det_meth$det_meth_desc <- ifelse(is.na(path_det_meth$det_meth_desc), "Missing", as.character(path_det_meth$det_meth_desc))
    # add data for all obs combined
      u <- c("total", "EIA", with(path_det_meth, sum(path_det_meth[det_meth_desc=="EIA", "n"])), 100)
      v <- c("total", "Culture", with(path_det_meth, sum(path_det_meth[det_meth_desc=="Culture", "n"])), 100)
      w <- c("total", "Microscopy", with(path_det_meth, sum(path_det_meth[det_meth_desc=="Microscopy", "n"])), 100)
      x <- c("total", "Other/Unspec", with(path_det_meth, sum(path_det_meth[det_meth_desc=="Other/Unspec", "n"])), 100)
      y <- c("total", "PCR", with(path_det_meth, sum(path_det_meth[det_meth_desc=="PCR", "n"])), 100)
      z <- c("total", "Missing", with(path_det_meth, sum(path_det_meth[det_meth_desc=="Missing", "n"])), 100)
      df <- data.frame(t(data.frame(u,v,w,x,y,z)))
      rownames(df)<-NULL
      df$X3 <- as.numeric(df$X3)
      df$X4 <- as.numeric(df$X4)
      colnames(df) <- c("path_spec", "det_meth_desc", "n", "prop")
      path_det_meth <- rbind(path_det_meth, df)
    # getting n (%) set up
      path_det_meth$n_perc_part1 <- paste(path_det_meth$n,format(round(path_det_meth$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
      path_det_meth$n_perc_part2 <- ")"
      path_det_meth$n_perc <- paste(path_det_meth$n_perc_part1, path_det_meth$n_perc_part2, sep="")
    # keeping only needed columns
      path_det_meth <- path_det_meth %>% select(path_spec, det_meth_desc, n_perc)
    # flip dataset
      path_det_meth_wide <- path_det_meth %>% pivot_wider(names_from = c(path_spec), values_from = c(n_perc)) 
      names(path_det_meth_wide)[names(path_det_meth_wide) == "det_meth_desc"] <- "variable"
    
  # study design
    table(case_control_full$study_design, exclude=NULL)
    path_study_design <- data.frame(case_control_full %>% group_by(path_spec, study_design) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
    # add data for all obs combined
      y <- c("total", "case control", with(path_study_design, sum(path_study_design[study_design=="case control", "n"])), 100)
      z <- c("total", "cohort", with(path_study_design, sum(path_study_design[study_design=="cohort", "n"])), 100)
      df <- data.frame(t(data.frame(y,z)))
      rownames(df)<-NULL
      df$X3 <- as.numeric(df$X3)
      df$X4 <- as.numeric(df$X4)
      colnames(df) <- c("path_spec", "study_design", "n", "prop")
      path_study_design <- rbind(path_study_design, df)
    # getting n (%) set up
      path_study_design$n_perc_part1 <- paste(path_study_design$n,format(round(path_study_design$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
      path_study_design$n_perc_part2 <- ")"
      path_study_design$n_perc <- paste(path_study_design$n_perc_part1, path_study_design$n_perc_part2, sep="")
    # keeping only needed columns
      path_study_design <- path_study_design %>% select(path_spec, study_design, n_perc)
    # flip dataset
      path_study_design_wide <- path_study_design %>% pivot_wider(names_from = c(path_spec), values_from = c(n_perc)) 
      names(path_study_design_wide)[names(path_study_design_wide) == "study_design"] <- "variable"
   
      
  # study setting
    table(case_control_full$setting, exclude=NULL)
      case_control_full$setting <- ifelse(is.na(case_control_full$setting), 99, case_control_full$setting)
    path_study_setting <- data.frame(case_control_full %>% group_by(path_spec, setting) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
    # format setting names
      path_study_setting$setting <- ifelse(path_study_setting$setting == 1, 'community',
                                    ifelse(path_study_setting$setting == 2, 'facility',
                                    ifelse(path_study_setting$setting == 99, 'unknown', NA)))
    # add data for all obs combined
      x <- c("total", "community", with(path_study_setting, sum(path_study_setting[setting=='community', "n"])), 100)  
      y <- c("total", "facility", with(path_study_setting, sum(path_study_setting[setting=='facility', "n"])), 100)
      z <- c("total", "unknown", with(path_study_setting, sum(path_study_setting[setting=='unknown', "n"])), 100)
      df <- data.frame(t(data.frame(x,y,z)))
      rownames(df)<-NULL
      df$X3 <- as.numeric(df$X3)
      df$X4 <- as.numeric(df$X4)
      colnames(df) <- c("path_spec", "setting", "n", "prop")
      path_study_setting <- rbind(path_study_setting, df)
    # getting n (%) set up
      path_study_setting$n_perc_part1 <- paste(path_study_setting$n,format(round(path_study_setting$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
      path_study_setting$n_perc_part2 <- ")"
      path_study_setting$n_perc <- paste(path_study_setting$n_perc_part1, path_study_setting$n_perc_part2, sep="")
    # keeping only needed columns
      path_study_setting <- path_study_setting %>% select(path_spec, setting, n_perc)
    # flip dataset
      path_study_setting_wide <- path_study_setting %>% pivot_wider(names_from = c(path_spec), values_from = c(n_perc)) 
      names(path_study_setting_wide)[names(path_study_setting_wide) == "setting"] <- "variable"    
      
      
  # presentation
    # create combined variable
      case_control_full$presentation___1 <- ifelse(is.na(case_control_full$presentation___1), 0, case_control_full$presentation___1)
      case_control_full$presentation___2 <- ifelse(is.na(case_control_full$presentation___2), 0, case_control_full$presentation___2)
      case_control_full$presentation___3 <- ifelse(is.na(case_control_full$presentation___3), 0, case_control_full$presentation___3)
      case_control_full$presentation_comb <- ifelse(case_control_full$presentation___1==1 & case_control_full$presentation___2==0 & case_control_full$presentation___3==0, "persistant",
                                             ifelse(case_control_full$presentation___1==0 & case_control_full$presentation___2==1 & case_control_full$presentation___3==0, "dysenteric (bloody)",
                                             ifelse(case_control_full$presentation___1==0 & case_control_full$presentation___2==0 & case_control_full$presentation___3==1, "acute, watery",
                                             ifelse(case_control_full$presentation___1==1 & case_control_full$presentation___2==1 & case_control_full$presentation___3==0, "persistant & dysenteric (bloody)",
                                             ifelse(case_control_full$presentation___1==1 & case_control_full$presentation___2==0 & case_control_full$presentation___3==1, "persistant & acute, watery",
                                             ifelse(case_control_full$presentation___1==0 & case_control_full$presentation___2==1 & case_control_full$presentation___3==1, "dysenteric (bloody) & acute, watery",
                                             ifelse(case_control_full$presentation___1==1 & case_control_full$presentation___2==1 & case_control_full$presentation___3==1, "all", "unknown")))))))
    # summarize
      path_study_presentation_comb <- data.frame(case_control_full %>% group_by(path_spec, presentation_comb) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
   # add data for all obs combined
      t <- c("total", "acute, watery", with(path_study_presentation_comb, sum(path_study_presentation_comb[presentation_comb=='acute, watery', "n"])), 100)
      u <- c("total", "all", with(path_study_presentation_comb, sum(path_study_presentation_comb[presentation_comb=='all', "n"])), 100)  
      v <- c("total", "dysenteric (bloody)", with(path_study_presentation_comb, sum(path_study_presentation_comb[presentation_comb=='dysenteric (bloody)', "n"])), 100)
      w <- c("total", "dysenteric (bloody) & acute, watery", with(path_study_presentation_comb, sum(path_study_presentation_comb[presentation_comb=='dysenteric (bloody) & acute, watery', "n"])), 100)
      x <- c("total", "persistant", with(path_study_presentation_comb, sum(path_study_presentation_comb[presentation_comb=='persistant', "n"])), 100)  
      y <- c("total", "persistant & acute, watery", with(path_study_presentation_comb, sum(path_study_presentation_comb[presentation_comb=='persistant & acute, watery', "n"])), 100)
      z <- c("total", "unknown", with(path_study_presentation_comb, sum(path_study_presentation_comb[presentation_comb=='unknown', "n"])), 100)
      df <- data.frame(t(data.frame(t,u,v,w,x,y,z)))
      rownames(df)<-NULL
      df$X3 <- as.numeric(df$X3)
      df$X4 <- as.numeric(df$X4)
      colnames(df) <- c("path_spec", "presentation_comb", "n", "prop")
      path_study_presentation_comb <- rbind(path_study_presentation_comb, df)
    # getting n (%) set up
      path_study_presentation_comb$n_perc_part1 <- paste(path_study_presentation_comb$n,format(round(path_study_presentation_comb$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
      path_study_presentation_comb$n_perc_part2 <- ")"
      path_study_presentation_comb$n_perc <- paste(path_study_presentation_comb$n_perc_part1, path_study_presentation_comb$n_perc_part2, sep="")
    # keeping only needed columns
      path_study_presentation_comb <- path_study_presentation_comb %>% select(path_spec, presentation_comb, n_perc)
    # flip dataset
      path_study_presentation_comb_wide <- path_study_presentation_comb %>% pivot_wider(names_from = c(path_spec), values_from = c(n_perc)) 
      names(path_study_presentation_comb_wide)[names(path_study_presentation_comb_wide) == "presentation_comb"] <- "variable"         
      
      
      
  # merging data for pathogen table
    path_table <- rbind(obs_by_path_wide, path_age_wide, path_age_1_wide, path_child_mort_wide, path_det_meth_wide, path_study_design_wide, path_study_setting_wide, path_study_presentation_comb_wide)
    path_table <- path_table[, c(21,20,1,3,15,16,18,2,4,5,8,9,10,12,11,13,17,19,6,7,14)]   
    # fill NAs with "0 (0.0)"
      path_table[is.na(path_table)] <- "0 (0.0)"
      
    

# effect estimates table
      
  # best measure 
    best_meas_only <- data.frame(case_control_full %>% group_by(best_meas) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
    # add data for all obs combined
      df<-data.frame("total", sum(best_meas_only$n), 100)
      names(df)<- c("best_meas","n","prop")
      best_meas_only <- rbind(best_meas_only, df)
    # getting n (%) set up
      best_meas_only$n_perc_part1 <- paste(best_meas_only$n,format(round(best_meas_only$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
      best_meas_only$n_perc_part2 <- ")"
      best_meas_only$n_perc <- paste(best_meas_only$n_perc_part1, best_meas_only$n_perc_part2, sep="")
    # keeping only needed columns
      best_meas_only <- best_meas_only %>% select(best_meas, n_perc)
    # flip dataset
      best_meas_only_wide <- best_meas_only %>% pivot_wider(names_from = c(best_meas), values_from = c(n_perc))
      best_meas_only_wide$variable <- "total"
  
  # by study design
    best_meas_design <- data.frame(case_control_full %>% group_by(best_meas, design) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
    # add data for all obs combined
      v <- c("total", 1, with(best_meas_design, sum(best_meas_design[design==1, "n"])), 100)
      w <- c("total", 2, with(best_meas_design, sum(best_meas_design[design==2, "n"])), 100)
      x <- c("total", 4, with(best_meas_design, sum(best_meas_design[design==4, "n"])), 100)
      y <- c("total", 6, with(best_meas_design, sum(best_meas_design[design==6, "n"])), 100)
      z <- c("total", 7, with(best_meas_design, sum(best_meas_design[design==7, "n"])), 100)
      df <- data.frame(t(data.frame(v,w,x,y,z)))
      rownames(df)<-NULL
      df$X3 <- as.numeric(df$X3)
      df$X4 <- as.numeric(df$X4)
      colnames(df) <- c("best_meas", "design", "n", "prop")
      best_meas_design <- rbind(best_meas_design, df)
    # getting n (%) set up
      best_meas_design$n_perc_part1 <- paste(best_meas_design$n,format(round(best_meas_design$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
      best_meas_design$n_perc_part2 <- ")"
      best_meas_design$n_perc <- paste(best_meas_design$n_perc_part1, best_meas_design$n_perc_part2, sep="")
    # keeping only needed columns
      best_meas_design <- best_meas_design %>% select(best_meas, design, n_perc)
    # flip dataset
      best_meas_design_wide <- best_meas_design %>% pivot_wider(names_from = c(best_meas), values_from = c(n_perc))
    # label design types and variable
      best_meas_design_wide$design <- recode(best_meas_design_wide$design, "1"="Cross-sectional", "2"="Prospective cohort", "4"="Randomized control trial", 
                                                                           "6"="Case-control", "7"="Nested case-control")
      best_meas_design_wide <- best_meas_design_wide %>% rename("variable" = "design")
    
      
# by matching variables
  # age
    best_meas_matching1 <- data.frame(case_control_full %>% filter(matching___1==1) %>% group_by(best_meas, matching___1) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
    # add data for all obs combined
      df<-data.frame("total", 1, sum(best_meas_matching1$n), 100)
      names(df)<- c("best_meas","matching___1","n","prop")
      best_meas_matching1 <- rbind(best_meas_matching1, df)
    # getting n (%) set up
      best_meas_matching1$n_perc_part1 <- paste(best_meas_matching1$n,format(round(best_meas_matching1$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
      best_meas_matching1$n_perc_part2 <- ")"
      best_meas_matching1$n_perc <- paste(best_meas_matching1$n_perc_part1, best_meas_matching1$n_perc_part2, sep="")
    # keeping only needed columns
      best_meas_matching1 <- best_meas_matching1 %>% select(best_meas, matching___1, n_perc)
    # flip dataset
      best_meas_matching1_wide <- best_meas_matching1 %>% pivot_wider(names_from = c(best_meas), values_from = c(n_perc))
    # indicate this is matching by age
      best_meas_matching1_wide <- best_meas_matching1_wide %>% rename("variable"="matching___1")
      best_meas_matching1_wide$variable <- "Age"
 
  
  # sex
    best_meas_matching2 <- data.frame(case_control_full %>% filter(matching___2==1) %>% group_by(best_meas, matching___2) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
    # add data for all obs combined
      df<-data.frame("total", 1, sum(best_meas_matching2$n), 100)
      names(df)<- c("best_meas","matching___2","n","prop")
      best_meas_matching2 <- rbind(best_meas_matching2, df)
    # getting n (%) set up
      best_meas_matching2$n_perc_part1 <- paste(best_meas_matching2$n,format(round(best_meas_matching2$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
      best_meas_matching2$n_perc_part2 <- ")"
      best_meas_matching2$n_perc <- paste(best_meas_matching2$n_perc_part1, best_meas_matching2$n_perc_part2, sep="")
    # keeping only needed columns
      best_meas_matching2 <- best_meas_matching2 %>% select(best_meas, matching___2, n_perc)
    # flip dataset
      best_meas_matching2_wide <- best_meas_matching2 %>% pivot_wider(names_from = c(best_meas), values_from = c(n_perc))
    # indicate this is matching by sex
      best_meas_matching2_wide <- best_meas_matching2_wide %>% rename("variable"="matching___2")
      best_meas_matching2_wide$variable <- "Sex"
    
  # location
    best_meas_matching3 <- data.frame(case_control_full %>% filter(matching___3==1) %>% group_by(best_meas, matching___3) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
    # add data for all obs combined
      df<-data.frame("total", 1, sum(best_meas_matching3$n), 100)
      names(df)<- c("best_meas","matching___3","n","prop")
      best_meas_matching3 <- rbind(best_meas_matching3, df)
    # getting n (%) set up
      best_meas_matching3$n_perc_part1 <- paste(best_meas_matching3$n,format(round(best_meas_matching3$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
      best_meas_matching3$n_perc_part2 <- ")"
      best_meas_matching3$n_perc <- paste(best_meas_matching3$n_perc_part1, best_meas_matching3$n_perc_part2, sep="")
    # keeping only needed columns
      best_meas_matching3 <- best_meas_matching3 %>% select(best_meas, matching___3, n_perc)
    # flip dataset
      best_meas_matching3_wide <- best_meas_matching3 %>% pivot_wider(names_from = c(best_meas), values_from = c(n_perc))
    # indicate this is matching by location
      best_meas_matching3_wide <- best_meas_matching3_wide %>% rename("variable"="matching___3")
      best_meas_matching3_wide$variable <- "Location"
    
  # other matching criteria
    best_meas_matching88 <- data.frame(case_control_full %>% filter(matching___88==1) %>% group_by(best_meas, matching___88) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
    # add data for all obs combined
      df<-data.frame("total", 1, sum(best_meas_matching88$n), 100)
      names(df)<- c("best_meas","matching___88","n","prop")
      best_meas_matching88 <- rbind(best_meas_matching88, df)
    # getting n (%) set up
      best_meas_matching88$n_perc_part1 <- paste(best_meas_matching88$n,format(round(best_meas_matching88$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
      best_meas_matching88$n_perc_part2 <- ")"
      best_meas_matching88$n_perc <- paste(best_meas_matching88$n_perc_part1, best_meas_matching88$n_perc_part2, sep="")
    # keeping only needed columns
      best_meas_matching88 <- best_meas_matching88 %>% select(best_meas, matching___88, n_perc)
    # flip dataset
      best_meas_matching88_wide <- best_meas_matching88 %>% pivot_wider(names_from = c(best_meas), values_from = c(n_perc))
    # indicate this is matching by other criteria
      best_meas_matching88_wide <- best_meas_matching88_wide %>% rename("variable"="matching___88")
      best_meas_matching88_wide$variable <- "Other"
    
  # no matching or matching not indicated
    best_meas_matching99 <- data.frame(case_control_full %>% filter(matching___1!=1 & matching___2!=1 & matching___3!=1 & matching___88!=1) %>% 
                                       group_by(best_meas) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
    # add data for all obs combined
      df<-data.frame("total", sum(best_meas_matching99$n), 100)
      names(df)<- c("best_meas","n","prop")
      best_meas_matching99 <- rbind(best_meas_matching99, df)
      best_meas_matching99$n <- as.numeric(best_meas_matching99$n)
      best_meas_matching99$prop <- as.numeric(best_meas_matching99$prop)
    # getting n (%) set up
      best_meas_matching99$n_perc_part1 <- paste(best_meas_matching99$n,format(round(best_meas_matching99$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
      best_meas_matching99$n_perc_part2 <- ")"
      best_meas_matching99$n_perc <- paste(best_meas_matching99$n_perc_part1, best_meas_matching99$n_perc_part2, sep="")
    # keeping only needed columns
      best_meas_matching99 <- best_meas_matching99 %>% select(best_meas, n_perc)
    # flip dataset
      best_meas_matching99_wide <- best_meas_matching99 %>% pivot_wider(names_from = c(best_meas), values_from = c(n_perc))
      best_meas_matching99_wide$variable <- "Not matched/not incidated"

    
  # best measure x validity
    best_meas_validity <- data.frame(case_control_full %>% group_by(best_meas, validity) %>% summarise(n=n()) %>% mutate(prop=((n/sum(n))*100)))
      best_meas_validity$validity <- ifelse(is.na(best_meas_validity$validity), 0, best_meas_validity$validity)
    # add data for all obs combined
      v <- c("total", 1, with(best_meas_validity, sum(best_meas_validity[validity=="1", "n"])), 100)
      w <- c("total", 2, with(best_meas_validity, sum(best_meas_validity[validity=="2", "n"])), 100)
      x <- c("total", 3, with(best_meas_validity, sum(best_meas_validity[validity=="3", "n"])), 100)
      y <- c("total", 4, with(best_meas_validity, sum(best_meas_validity[validity=="4", "n"])), 100)
      z <- c("total", 5, with(best_meas_validity, sum(best_meas_validity[validity=="5", "n"])), 100)
      df <- data.frame(t(data.frame(v,w,x,y,z)))
      rownames(df)<-NULL
      df$X3 <- as.numeric(df$X3)
      df$X4 <- as.numeric(df$X4)
      colnames(df) <- c("best_meas", "validity", "n", "prop")
      best_meas_validity <- rbind(best_meas_validity, df)
    # getting n (%) set up
      best_meas_validity$n_perc_part1 <- paste(best_meas_validity$n,format(round(best_meas_validity$prop, digits=1), nsmall=1, trim = TRUE), sep=" (")
      best_meas_validity$n_perc_part2 <- ")"
      best_meas_validity$n_perc <- paste(best_meas_validity$n_perc_part1, best_meas_validity$n_perc_part2, sep="")
    # keeping only needed columns
      best_meas_validity <- best_meas_validity %>% select(best_meas,  validity, n_perc)
    # flip dataset
      best_meas_validity_wide <- best_meas_validity %>% pivot_wider(names_from = c(best_meas), values_from = c(n_perc))
    # rename "validity column in prep for merging with other descriptive data
      best_meas_validity_wide <- best_meas_validity_wide %>% rename("variable"="validity")
    # recode 0 to missing
      best_meas_validity_wide$variable <- recode(best_meas_validity_wide$variable, "0"="Not applicable")
 
      
      
# merging data for pathogen table
  best_meas_table <- bind_rows(best_meas_only_wide, best_meas_design_wide, best_meas_matching1_wide, best_meas_matching2_wide, best_meas_matching3_wide, 
                           best_meas_matching88_wide, best_meas_matching99_wide, best_meas_validity_wide)
  best_meas_table <- best_meas_table[, c(7,6,5,4,3,2,1)]   
  # fill NAs with "0 (0.0)"
  best_meas_table[is.na(best_meas_table)] <- "0 (0.0)"
    

    
# identifying studies that used multiple detection methods
  count_det_meth <- case_control_full %>% group_by(author_year_title, det_meth_desc) %>% select(author_year_title, det_meth_desc, path, cases_detected) %>% summarise_at(vars(cases_detected), sum)
    count_det_meth$ones <- 1
    count_det_meth <- count_det_meth %>% select(author_year_title, det_meth_desc, ones)
  # flip dataset
    count_det_meth_wide <- count_det_meth %>% pivot_wider(names_from = c(det_meth_desc), values_from = c(ones))
    count_det_meth_wide <- count_det_meth_wide %>% rowwise() %>% mutate(sum = sum(EIA, Culture, PCR, Microscopy, na.rm=TRUE))
  # summarize
    table(count_det_meth_wide$sum)
    
    
    
    
  
#############################################################    
#############################################################
###                                                       ###
###                       MODELING                        ###
###                                                       ###
#############################################################
############################################################# 
  
  
      
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
                         'data_rota_pre', 'data_rota_post',
                         'data_noro_gi', 'data_noro_gii', 'data_rota_a', 
                         'data_aero_hydrophila', 'data_campy_coli', 'data_campy_jejuni', 'data_campy_lari', 'data_chol_o1', 'data_chol_o139',
                         'data_epec_a', 'data_epec_t', 'data_epec_unkn', 'data_etec_st', 'data_etec_lt', 'data_etec_unkn', 
                         'data_salm_nts', 'data_salm_paratyphi', 'data_shig_a', 'data_shig_c', 'data_shig_d',
                         'data_crypto_parvum', 'data_ehist_coli', 'data_ehist_dispar', 'data_ehist_histolytica', 'data_giard_l_i_d')
    
  # labels for results
    data_labels_unadj <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                           'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                           'crypto', 'ehist', 'giard', 
                           'rota_pre', 'rota_post',
                           'noro_gi', 'noro_gii', 'rota_a', 
                           'aero_hydrophila', 'campy_coli', 'campy_jejuni', 'campy_lari', 'chol_o1', 'chol_o139',
                           'epec_a', 'epec_t', 'epec_unkn', 'etec_st', 'etec_lt', 'etec_unkn', 
                           'salm_nts', 'salm_paratyphi', 'shig_a', 'shig_c', 'shig_d',
                           'crypto_parvum', 'ehist_coli', 'ehist_dispar', 'ehist_histolytica', 'giard_l_i_d')
  # model
    ma_path_unadj <- list()
    for(i in seq_along(data_list_unadj)){ 
      ma_path_unadj[[i]] <- meta_analysis_loop_unadj(get(data_list_unadj[i]))
      }  
  # examine results  
    for (i in 1:42){
      print(ma_path_unadj[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
      for (i in 1:42) {
        coeff <- exp(coef(summary(ma_path_unadj[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
        lci   <- exp(coef(summary(ma_path_unadj[[i]]))$ci.lb[1])
        uci   <- exp(coef(summary(ma_path_unadj[[i]]))$ci.ub[1]) 
        i2    <- ma_path_unadj[[i]]$I2
        group <- "unadj"
        put(coeff, lci, uci, i2, group)
      }
    ma_path_unadj_results <-magic_result_as_dataframe()
  # results in a dataframe
    names <- data_labels_unadj
    ma_path_unadj_results <- data.frame(names, ma_path_unadj_results)   
    ma_path_unadj_results$i <- NULL
    magic_free()
  # delete I2 data
    ma_path_unadj_results$i2 <- NULL

      
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
                                    'data_young_high_aero', 'data_young_high_campy', 'data_young_high_chol', 'data_young_high_epec',  'data_young_high_etec', 'data_young_high_salm', 'data_young_high_shig', 
                                    'data_young_high_crypto', 'data_young_high_ehist', 'data_young_high_giard',
                                    'data_young_high_rota_pre', 
                                    'data_young_high_noro_gi', 'data_young_high_noro_gii', 'data_young_high_aero_hydrophila', 
                                    'data_young_high_campy_coli', 'data_young_high_campy_jejuni', 
                                    'data_young_high_chol_o1', 'data_young_high_chol_o139',
                                    'data_young_high_etec_st', 'data_young_high_etec_lt', 'data_young_high_etec_unkn',
                                    'data_young_high_epec_a', 'data_young_high_epec_t', 'data_young_high_epec_unkn',
                                    'data_young_high_salm_nts', 'data_young_high_salm_paratyphi',
                                    'data_young_high_ehist_coli', 'data_young_high_ehist_histolytica', 'data_young_high_giard_l_i_d')
    # labels for results
      data_labels_young_high_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                      'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                                      'crypto', 'ehist', 'giard',
                                      'rota_pre', 
                                      'noro_gi', 'noro_gii', 'aero_hydrophila',
                                      'campy_coli', 'campy_jejuni', 
                                      'chol_o1', 'chol_o139',
                                      'etec_st', 'etec_lt', 'etec_unkn',
                                      'epec_a', 'epec_t', 'epec_unkn',
                                      'salm_nts', 'salm_paratyphi',
                                      'ehist_coli', 'ehist_histolytica', 'giard_l_i_d')
    
    # model
      ma_path_young_high_all <- list()
      for(i in seq_along(data_list_young_high_all)){ 
        ma_path_young_high_all[[i]] <- meta_analysis_loop_all(get(data_list_young_high_all[i]))
      }  
    # examine results  
      for (i in 1:34){
        print(ma_path_young_high_all[[i]], digits=2)
      }   
    # extract estimate and combine results  
      magic_for(silent = TRUE)
      for (i in 1:34) {
        coeff <- exp(coef(summary(ma_path_young_high_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
        lci   <- exp(coef(summary(ma_path_young_high_all[[i]]))$ci.lb[1])
        uci   <- exp(coef(summary(ma_path_young_high_all[[i]]))$ci.ub[1])    
        group <- "young, high mortality"
        put(coeff, lci, uci, group)
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
                                        'data_young_vlow_low_campy',
                                        'data_young_vlow_low_chol', 
                                        'data_young_vlow_low_epec',  'data_young_vlow_low_etec', 'data_young_vlow_low_salm', 'data_young_vlow_low_shig', 
                                        'data_young_vlow_low_crypto', 'data_young_vlow_low_ehist', 'data_young_vlow_low_giard',
                                        'data_young_vlow_low_rota_pre', 'data_young_vlow_low_rota_post',
                                        'data_young_vlow_low_noro_gi', 'data_young_vlow_low_noro_gii', 
                                        'data_young_vlow_low_campy_coli', 'data_young_vlow_low_campy_jejuni', 
                                        'data_young_vlow_low_epec_a', 'data_young_vlow_low_epec_t', 'data_young_vlow_low_epec_unkn',
                                        'data_young_vlow_low_etec_st', 'data_young_vlow_low_etec_lt', 'data_young_vlow_low_etec_unkn',
                                        'data_young_vlow_low_salm_nts', 'data_young_vlow_low_salm_paratyphi',
                                        'data_young_vlow_low_shig_a', 'data_young_vlow_low_shig_c',
                                        'data_young_vlow_low_crypto_parvum', 'data_young_vlow_low_ehist_histolytica',
                                        'data_young_vlow_low_giard_l_i_d')
                                        
    # labels for results
      data_labels_young_vlow_low_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                          'campy', 
                                          'chol',  
                                          'epec',  'etec', 'salm', 'shig', 
                                          'crypto', 'ehist', 'giard',
                                          'rota_pre', 'rota_post',
                                          'noro_gi', 'noro_gii', 
                                          'campy_coli', 'campy_jejuni', 
                                          'epec_a', 'epec_t', 'epec_unkn',
                                          'etec_st', 'etec_lt', 'etec_unkn',
                                          'salm_nts', 'salm_paratyphi',
                                          'shig_a', 'shig_c', 
                                          'crypto_parvum', 'ehist_histolytica',
                                          'giard_l_i_d')

    # model
      ma_path_young_vlow_low_all <- list()
        for(i in seq_along(data_list_young_vlow_low_all)){ 
          ma_path_young_vlow_low_all[[i]] <- meta_analysis_loop_all(get(data_list_young_vlow_low_all[i]))
          }  
    # examine results  
      for (i in 1:33){
        print(ma_path_young_vlow_low_all[[i]], digits=2)
        }   
    # extract estimate and combine results  
      magic_for(silent = TRUE)
        for (i in 1:33) {
          coeff <- exp(coef(summary(ma_path_young_vlow_low_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
          lci   <- exp(coef(summary(ma_path_young_vlow_low_all[[i]]))$ci.lb[1])
          uci   <- exp(coef(summary(ma_path_young_vlow_low_all[[i]]))$ci.ub[1])    
          group <- "young, vlow & low mortality"
          put(coeff, lci, uci, group)
          }
      ma_path_young_vlow_low_all_results <-magic_result_as_dataframe()
    # results in a dataframe
      names <- data_labels_young_vlow_low_all
      ma_path_young_vlow_low_all_results <- data.frame(names, ma_path_young_vlow_low_all_results)   
      ma_path_young_vlow_low_all_results$i <- NULL
      magic_free()          
      
      
  ### old/all child mort levels ### 
      
    # data list to loop through  
      data_list_old_all <- c('data_old_adeno', 'data_old_astro', 'data_old_noro', 'data_old_rota', 
                             'data_old_aero', 'data_old_campy', 
                             'data_old_chol', 
                             'data_old_epec',  'data_old_etec', 'data_old_salm', 'data_old_shig',
                             'data_old_crypto', 'data_old_ehist', 'data_old_giard',
                             'data_old_rota_pre', 
                             'data_old_noro_gi', 'data_old_noro_gii', 
                             'data_old_campy_coli', 'data_old_campy_jejuni',
                             'data_old_epec_a', 'data_old_epec_t', 'data_old_epec_unkn',
                             'data_old_etec_st', 'data_old_etec_lt', 'data_old_etec_unkn',
                             'data_old_salm_nts', 'data_old_shig_d',
                             'data_old_crypto_parvum', 'data_old_ehist_histolytica', 'data_old_giard_l_i_d')
    # labels for results
      data_labels_old_all <- c('adeno', 'astro', 'noro', 'rota',
                               'aero', 'campy', 
                               'chol', 
                               'epec',  'etec', 'salm', 'shig',
                               'crypto', 'ehist', 'giard',
                               'rota_pre', 
                               'noro_gi', 'noro_gii',
                               'campy_coli', 'campy_jejuni',
                               'epec_a', 'epec_t', 'epec_unkn',
                               'etec_st', 'etec_lt', 'etec_unkn',
                               'salm_nts', 'shig_d', 
                               'crypto_parvum', 'ehist_histolytica', 'giard_l_i_d')
    # model
      ma_path_old_all <- list()
      for(i in seq_along(data_list_old_all)){ 
        ma_path_old_all[[i]] <- meta_analysis_loop_all(get(data_list_old_all[i]))
      }  
    # examine results  
      for (i in 1:30){
        print(ma_path_old_all[[i]], digits=2)
      }   
    # extract estimate and combine results  
      magic_for(silent = TRUE)
      for (i in 1:30) {
        coeff <- exp(coef(summary(ma_path_old_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
        lci   <- exp(coef(summary(ma_path_old_all[[i]]))$ci.lb[1])
        uci   <- exp(coef(summary(ma_path_old_all[[i]]))$ci.ub[1])    
        group <- "old, all mortality levels"
        put(coeff, lci, uci, group)
      }
      ma_path_old_all_results <-magic_result_as_dataframe()
    # results in a dataframe
      names <- data_labels_old_all
      ma_path_old_all_results <- data.frame(names, ma_path_old_all_results)   
      ma_path_old_all_results$i <- NULL
      magic_free()      
       

  
  ### youngest/all child mort levels ### 
      
    # data list to loop through  
      data_list_youngest_all <- c('data_youngest_adeno', 'data_youngest_astro', 'data_youngest_noro', 'data_youngest_rota', 'data_youngest_sapo',
                                  'data_youngest_aero', 'data_youngest_campy', 'data_youngest_chol', 'data_youngest_epec',  'data_youngest_etec', 'data_youngest_salm', 'data_youngest_shig', 
                                  'data_youngest_crypto', 'data_youngest_ehist', 'data_youngest_giard', 
                                  'data_youngest_rota_pre', 'data_youngest_rota_post',
                                  'data_youngest_epec_a', 'data_youngest_epec_t', 'data_youngest_epec_unkn',
                                  'data_youngest_etec_st', 'data_youngest_etec_lt', 'data_youngest_etec_unkn')
      
    # labels for results
      data_labels_youngest_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                    'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                                    'crypto', 'ehist', 'giard', 
                                    'rota_pre', 'rota_post',
                                    'epec_a', 'epec_t', 'epec_unkn',
                                    'etec_st', 'etec_lt', 'etec_unkn')
      
    # model
      ma_path_youngest_all <- list()
      for(i in seq_along(data_list_youngest_all)){ 
        ma_path_youngest_all[[i]] <- meta_analysis_loop_all(get(data_list_youngest_all[i]))
      }  
    # examine results  
      for (i in 1:23){
        print(ma_path_youngest_all[[i]], digits=2)
      }   
    # extract estimate and combine results  
      magic_for(silent = TRUE)
      for (i in 1:23) {
        coeff <- exp(coef(summary(ma_path_youngest_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
        lci   <- exp(coef(summary(ma_path_youngest_all[[i]]))$ci.lb[1])
        uci   <- exp(coef(summary(ma_path_youngest_all[[i]]))$ci.ub[1])    
        group <- "youngest, all mortality levels"
        put(coeff, lci, uci, group)
      }
      ma_path_youngest_all_results <-magic_result_as_dataframe()
    # results in a dataframe
      names <- data_labels_youngest_all
      ma_path_youngest_all_results <- data.frame(names, ma_path_youngest_all_results)   
      ma_path_youngest_all_results$i <- NULL
      magic_free()           
      
      
      
  ### middle/all child mort levels ### 
      
    # data list to loop through  
      data_list_middle_all <- c('data_middle_adeno', 'data_middle_astro', 'data_middle_noro', 'data_middle_rota', 'data_middle_sapo',
                                'data_middle_aero', 'data_middle_campy','data_middle_epec',  'data_middle_etec', 'data_middle_shig', 
                                'data_middle_crypto', 'data_middle_giard', 
                                'data_middle_rota_pre', 'data_rota_post',
                                'data_middle_epec_unkn',
                                'data_middle_etec_st', 'data_middle_etec_lt', 'data_middle_etec_unkn')
      
    # labels for results
      data_labels_middle_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                  'aero', 'campy', 'epec',  'etec', 'shig', 
                                  'crypto', 'giard', 
                                  'rota_pre', 'rota_post',
                                  'epec_unkn',
                                  'etec_st', 'etec_lt', 'etec_unkn')
      
    # model
      ma_path_middle_all <- list()
      for(i in seq_along(data_list_middle_all)){ 
        ma_path_middle_all[[i]] <- meta_analysis_loop_all(get(data_list_middle_all[i]))
        }  
    # examine results  
      for (i in 1:18){
        print(ma_path_middle_all[[i]], digits=2)
        }   
    # extract estimate and combine results  
      magic_for(silent = TRUE)
      for (i in 1:18) {
        coeff <- exp(coef(summary(ma_path_middle_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
        lci   <- exp(coef(summary(ma_path_middle_all[[i]]))$ci.lb[1])
        uci   <- exp(coef(summary(ma_path_middle_all[[i]]))$ci.ub[1])    
        group <- "middle, all mortality levels"
        put(coeff, lci, uci, group)
        }
      ma_path_middle_all_results <-magic_result_as_dataframe()
    # results in a dataframe
      names <- data_labels_middle_all
      ma_path_middle_all_results <- data.frame(names, ma_path_middle_all_results)   
      ma_path_middle_all_results$i <- NULL
      magic_free()                
      
      
    
    
  ### youngest/high child mort levels ### 
    
    # data list to loop through  
      data_list_youngest_high_all <- c('data_youngest_high_adeno', 'data_youngest_high_astro', 'data_youngest_high_noro', 'data_youngest_high_rota', 'data_youngest_high_sapo',
                                       'data_youngest_high_aero', 'data_youngest_high_campy', 'data_youngest_high_chol', 'data_youngest_high_epec',  'data_youngest_high_etec', 'data_youngest_high_salm', 'data_youngest_high_shig', 
                                       'data_youngest_high_crypto', 'data_youngest_high_ehist', 'data_youngest_high_giard', 
                                       'data_youngest_high_rota_pre',  
                                       'data_youngest_high_epec_a', 'data_youngest_high_epec_t', 'data_youngest_high_epec_unkn', 
                                       'data_youngest_high_etec_st', 'data_youngest_high_etec_lt', 'data_youngest_high_etec_unkn')
    
    # labels for results
      data_labels_youngest_high_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                         'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                                         'crypto', 'ehist', 'giard', 
                                         'rota_pre',
                                         'epec_a', 'epec_t', 'epec_unkn',
                                         'etec_st', 'etec_lt', 'etec_unkn')
    
    # model
      ma_path_youngest_high_all <- list()
      for(i in seq_along(data_list_youngest_high_all)){ 
        ma_path_youngest_high_all[[i]] <- meta_analysis_loop_all(get(data_list_youngest_high_all[i]))
      }  
    # examine results  
      for (i in 1:22){
        print(ma_path_youngest_high_all[[i]], digits=2)
      }   
    # extract estimate and combine results  
      magic_for(silent = TRUE)
      for (i in 1:22) {
        coeff <- exp(coef(summary(ma_path_youngest_high_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
        lci   <- exp(coef(summary(ma_path_youngest_high_all[[i]]))$ci.lb[1])
        uci   <- exp(coef(summary(ma_path_youngest_high_all[[i]]))$ci.ub[1])    
        group <- "youngest, high mortality"
        put(coeff, lci, uci, group)
      }
      ma_path_youngest_high_all_results <-magic_result_as_dataframe()
    # results in a dataframe
      names <- data_labels_youngest_high_all
      ma_path_youngest_high_all_results <- data.frame(names, ma_path_youngest_high_all_results)   
      ma_path_youngest_high_all_results$i <- NULL
      magic_free()           
    
      summary(ma_path_youngest_high_all[[21]])
    
  ### middle/high child mort levels ### 
      
    # data list to loop through  
      data_list_middle_high_all <- c('data_middle_high_adeno', 'data_middle_high_astro', 'data_middle_high_noro', 'data_middle_high_rota', 'data_middle_high_sapo',
                                     'data_middle_high_aero', 'data_middle_high_campy', 'data_middle_high_epec',  'data_middle_high_etec',  'data_middle_high_shig', 
                                     'data_middle_high_crypto',
                                     'data_middle_high_rota_pre', 
                                     'data_middle_high_epec_a', 'data_middle_high_epec_t','data_middle_high_epec_unkn',
                                     'data_middle_high_etec_lt','data_middle_high_etec_st', 'data_middle_high_etec_unkn')
      
    # labels for results
      data_labels_middle_high_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                       'aero', 'campy', 'epec',  'etec', 'shig', 
                                       'crypto', 
                                       'rota_pre',
                                       'epec_a', 'epec_t', 'epec_unkn',
                                       'etec_lt', 'etec_st', 'etec_unkn')
      
    # model
      ma_path_middle_high_all <- list()
      for(i in seq_along(data_list_middle_high_all)){ 
        ma_path_middle_high_all[[i]] <- meta_analysis_loop_all(get(data_list_middle_high_all[i]))
        }  
    # examine results  
      for (i in 1:18){
        print(ma_path_middle_high_all[[i]], digits=2)
        }   
    # extract estimate and combine results  
      magic_for(silent = TRUE)
      for (i in 1:18) {
        coeff <- exp(coef(summary(ma_path_middle_high_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
        lci   <- exp(coef(summary(ma_path_middle_high_all[[i]]))$ci.lb[1])
        uci   <- exp(coef(summary(ma_path_middle_high_all[[i]]))$ci.ub[1])    
        group <- "middle, high mortality"
        put(coeff, lci, uci, group)
        }
      ma_path_middle_high_all_results <-magic_result_as_dataframe()
    # results in a dataframe
      names <- data_labels_middle_high_all
      ma_path_middle_high_all_results <- data.frame(names, ma_path_middle_high_all_results)   
      ma_path_middle_high_all_results$i <- NULL
      magic_free()           
      
      
      
  ### youngest/v.low and low child mort levels ### 
      
    # data list to loop through  
      data_list_youngest_vlow_low_all <- c('data_youngest_vlow_low_adeno', 'data_youngest_vlow_low_astro', 'data_youngest_vlow_low_noro', 'data_youngest_vlow_low_rota', 'data_youngest_vlow_low_sapo',
                                           'data_youngest_vlow_low_campy', 'data_youngest_vlow_low_epec',  'data_youngest_vlow_low_etec', 'data_youngest_vlow_low_salm', 'data_youngest_vlow_low_shig', 
                                           'data_youngest_vlow_low_crypto', 'data_youngest_vlow_low_giard',
                                           'data_youngest_vlow_low_rota_pre', 'data_youngest_vlow_low_rota_post', 
                                           'data_youngest_vlow_low_epec_a', 'data_youngest_vlow_low_epec_t', 'data_youngest_vlow_low_epec_unkn',
                                           'data_youngest_vlow_low_etec_st', 'data_youngest_vlow_low_etec_lt', 'data_youngest_vlow_low_etec_unkn')
      
    # labels for results
      data_labels_youngest_vlow_low_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                             'campy', 'epec',  'etec', 'salm', 'shig', 
                                             'crypto', 'giard', 
                                             'rota_pre', 'rota_post',
                                             'epec_a', 'epec_t', 'epec_unkn',
                                             'etec_st', 'etec_lt', 'etec_unkn')
      
    # model
      ma_path_youngest_vlow_low_all <- list()
      for(i in seq_along(data_list_youngest_vlow_low_all)){ 
        ma_path_youngest_vlow_low_all[[i]] <- meta_analysis_loop_all(get(data_list_youngest_vlow_low_all[i]))
        }  
    # examine results  
      for (i in 1:20){
        print(ma_path_youngest_vlow_low_all[[i]], digits=2)
        }   
    # extract estimate and combine results  
      magic_for(silent = TRUE)
      for (i in 1:20) {
        coeff <- exp(coef(summary(ma_path_youngest_vlow_low_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
        lci   <- exp(coef(summary(ma_path_youngest_vlow_low_all[[i]]))$ci.lb[1])
        uci   <- exp(coef(summary(ma_path_youngest_vlow_low_all[[i]]))$ci.ub[1])    
        group <- "youngest, vlow & low mortality"
        put(coeff, lci, uci, group)
        }
      ma_path_youngest_vlow_low_all_results <-magic_result_as_dataframe()
    # results in a dataframe
      names <- data_labels_youngest_vlow_low_all
      ma_path_youngest_vlow_low_all_results <- data.frame(names, ma_path_youngest_vlow_low_all_results)   
      ma_path_youngest_vlow_low_all_results$i <- NULL
      magic_free()           
      
      
      
  ### middle/v.low and low child mort levels ### 
      
    # data list to loop through  
      data_list_middle_vlow_low_all <- c('data_middle_vlow_low_epec',  'data_middle_vlow_low_etec',  'data_middle_vlow_low_giard', 
                                         'data_middle_vlow_low_epec_unkn',
                                         'data_middle_vlow_low_etec_st', 'data_middle_vlow_low_etec_lt', 'data_middle_vlow_low_etec_unkn')
      
    # labels for results
      data_labels_middle_vlow_low_all <- c('epec',  'etec', 'giard',
                                           'epec_unkn',
                                           'etec_st', 'etec_lt', 'etec_unkn')
      
    # model
      ma_path_middle_vlow_low_all <- list()
      for(i in seq_along(data_list_middle_vlow_low_all)){ 
        ma_path_middle_vlow_low_all[[i]] <- meta_analysis_loop_all(get(data_list_middle_vlow_low_all[i]))
        }  
      # examine results  
      for (i in 1:7){
        print(ma_path_middle_vlow_low_all[[i]], digits=2)
        }   
    # extract estimate and combine results  
      magic_for(silent = TRUE)
      for (i in 1:7) {
        coeff <- exp(coef(summary(ma_path_middle_vlow_low_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
        lci   <- exp(coef(summary(ma_path_middle_vlow_low_all[[i]]))$ci.lb[1])
        uci   <- exp(coef(summary(ma_path_middle_vlow_low_all[[i]]))$ci.ub[1])    
        group <- "middle, vlow & low mortality"
        put(coeff, lci, uci, group)
        }
      ma_path_middle_vlow_low_all_results <-magic_result_as_dataframe()
    # results in a dataframe
      names <- data_labels_middle_vlow_low_all
      ma_path_middle_vlow_low_all_results <- data.frame(names, ma_path_middle_vlow_low_all_results)   
      ma_path_middle_vlow_low_all_results$i <- NULL
      magic_free()      
      
      
      
      
      
  ### AFRO/high ### 
      
    # data list to loop through  
      data_list_young_afro_high_all <- c('data_young_afro_high_adeno', 'data_young_afro_high_astro', 'data_young_afro_high_noro', 'data_young_afro_high_rota', 'data_young_afro_high_sapo',
                                         'data_young_afro_high_aero', 'data_young_afro_high_campy', 'data_young_afro_high_epec',  'data_young_afro_high_etec', 'data_young_afro_high_salm', 'data_young_afro_high_shig', 
                                         'data_young_afro_high_crypto', 'data_young_afro_high_ehist', 'data_young_afro_high_giard',
                                         'data_young_afro_high_rota_pre', 
                                         'data_young_afro_high_epec_a', 'data_young_afro_high_epec_t', 'data_young_afro_high_epec_unkn',
                                         'data_young_afro_high_etec_st', 'data_young_afro_high_etec_lt', 'data_young_afro_high_etec_unkn')
      
    # labels for results
      data_young_labels_afro_high_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                           'aero', 'campy', 'epec',  'etec', 'salm', 'shig', 
                                           'crypto', 'ehist', 'giard',
                                           'rota_pre',
                                           'epec_a', 'epec_t', 'epec_unkn',
                                           'etec_st', 'etec_lt', 'etec_unkn')
      
    # model
      ma_path_young_afro_high_all <- list()
      for(i in seq_along(data_list_young_afro_high_all)){ 
        ma_path_young_afro_high_all[[i]] <- meta_analysis_loop_all(get(data_list_young_afro_high_all[i]))
        }  
    # examine results  
      for (i in 1:21){
        print(ma_path_young_afro_high_all[[i]], digits=2)
      }   
    # extract estimate and combine results  
      magic_for(silent = TRUE)
      for (i in 1:21) {
        coeff <- exp(coef(summary(ma_path_young_afro_high_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
        lci   <- exp(coef(summary(ma_path_young_afro_high_all[[i]]))$ci.lb[1])
        uci   <- exp(coef(summary(ma_path_young_afro_high_all[[i]]))$ci.ub[1])    
        group <- "young, afro, high mortality"
        put(coeff, lci, uci, group)
        }
      ma_path_young_afro_high_all_results <-magic_result_as_dataframe()
    # results in a dataframe
      names <- data_young_labels_afro_high_all
      ma_path_young_afro_high_all_results <- data.frame(names, ma_path_young_afro_high_all_results)   
      ma_path_young_afro_high_all_results$i <- NULL
      magic_free()            
      
      
  ### Asia/high ### 
      
    # data list to loop through  
      data_list_young_asia_high_all <- c('data_young_asia_high_adeno', 'data_young_asia_high_astro', 'data_young_asia_high_noro', 'data_young_asia_high_rota', 'data_young_asia_high_sapo',
                                         'data_young_asia_high_aero', 'data_young_asia_high_campy', 'data_young_asia_high_chol', 'data_young_asia_high_epec',  'data_young_asia_high_etec', 'data_young_asia_high_shig', 
                                         'data_young_asia_high_crypto', 'data_young_asia_high_ehist', 'data_young_asia_high_giard',
                                         'data_young_asia_high_rota_pre', 
                                         'data_young_asia_high_epec_a', 'data_young_asia_high_epec_t', 'data_young_asia_high_epec_unkn', 
                                         'data_young_asia_high_etec_st', 'data_young_asia_high_etec_lt', 'data_young_asia_high_etec_unkn')
      
    # labels for results
      data_young_labels_asia_high_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                           'aero', 'campy', 'chol', 'epec',  'etec', 'shig',
                                           'crypto', 'ehist', 'giard',
                                           'rota_pre',
                                           'epec_a', 'epec_t', 'epec_unkn',
                                           'etec_st', 'etec_lt', 'etec_unkn')
      
    # model
      ma_path_young_asia_high_all <- list()
      for(i in seq_along(data_list_young_asia_high_all)){ 
        ma_path_young_asia_high_all[[i]] <- meta_analysis_loop_all(get(data_list_young_asia_high_all[i]))
      }  
    # examine results  
      for (i in 1:21){
        print(ma_path_young_asia_high_all[[i]], digits=2)
      }   
    # extract estimate and combine results  
      magic_for(silent = TRUE)
      for (i in 1:21) {
        coeff <- exp(coef(summary(ma_path_young_asia_high_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
        lci   <- exp(coef(summary(ma_path_young_asia_high_all[[i]]))$ci.lb[1])
        uci   <- exp(coef(summary(ma_path_young_asia_high_all[[i]]))$ci.ub[1])    
        group <- "young, asia, high mortality"
        put(coeff, lci, uci, group)
      }
      ma_path_young_asia_high_all_results <-magic_result_as_dataframe()
    # results in a dataframe
      names <- data_young_labels_asia_high_all
      ma_path_young_asia_high_all_results <- data.frame(names, ma_path_young_asia_high_all_results)   
      ma_path_young_asia_high_all_results$i <- NULL
      magic_free()                 
      
      
      
### combining all datasets
    ma_new2_combined_results <- rbind(ma_path_unadj_results, 
                                        #ma_path_young_vlow_all_results, ma_path_young_low_all_results, 
                                        ma_path_young_high_all_results,ma_path_young_vlow_low_all_results,
                                        ma_path_old_all_results,
                                        ma_path_youngest_all_results, ma_path_middle_all_results,
                                        ma_path_youngest_high_all_results, ma_path_youngest_vlow_low_all_results,
                                        ma_path_middle_high_all_results, ma_path_middle_vlow_low_all_results,
                                        ma_path_young_afro_high_all_results, ma_path_young_asia_high_all_results)
    # formatting
      ma_new2_combined_results$coeff  <- round(ma_new2_combined_results$coeff, 1)
      ma_new2_combined_results$lci    <- round(ma_new2_combined_results$lci, 1)
      ma_new2_combined_results$uci    <- round(ma_new2_combined_results$uci, 1)
    # creating variable with effect estimate and CI for pathogen
      ma_new2_combined_results$part1  <- paste(ma_new2_combined_results$coeff, ma_new2_combined_results$lci, sep=" (")
      ma_new2_combined_results$part2  <- paste(ma_new2_combined_results$part1, ma_new2_combined_results$uci, sep=", ")
      ma_new2_combined_results$part3  <- ")"
      ma_new2_combined_results$ee_ci  <- paste(ma_new2_combined_results$part2, ma_new2_combined_results$part3, sep="")
    # cleaning/dropping unnecessary variables
      ma_new2_combined_results$part1 <- NULL
      ma_new2_combined_results$part2 <- NULL
      ma_new2_combined_results$part3 <- NULL
    # creating larger/broader group variable (for plotting later)
      ma_new2_combined_results$strat <- ifelse(ma_new2_combined_results$group == 'unadj', 'unadjusted',
                                        ifelse((ma_new2_combined_results$group == 'youngest, vlow & low mortality' |
                                                ma_new2_combined_results$group == 'youngest, high mortality' |
                                                ma_new2_combined_results$group == 'youngest, all mortality levels' ), '0-2 years',
                                        ifelse((ma_new2_combined_results$group == 'middle, vlow & low mortality' |
                                                ma_new2_combined_results$group == 'middle, high mortality' |
                                                ma_new2_combined_results$group == 'middle, all mortality levels'), '3-5 years',
                                        ifelse((ma_new2_combined_results$group == 'young, very low mortality' |
                                                ma_new2_combined_results$group == 'young, low mortality' |
                                                ma_new2_combined_results$group == 'young, high mortality' |
                                                ma_new2_combined_results$group == 'young, vlow & low mortality' |
                                                ma_new2_combined_results$group == 'young, afro, high mortality' |
                                                ma_new2_combined_results$group == 'young, asia, high mortality'), '0-5 years',
                                        ifelse(ma_new2_combined_results$group == 'old, all mortality levels', '>5 years', NA)))))
      
    # switching from long to wide structure
      # keep only needed variables
      ma_new2_combined_results_wide <- ma_new2_combined_results %>% select(names, group, ee_ci)
      ma_new2_combined_results_wide <- ma_new2_combined_results_wide %>% pivot_wider(names_from = c(group), values_from = c(ee_ci))
    # format and export
      ordered_path <- c('adeno', 'astro', 'noro', 'rota', 'rota_pre', 'rota_post', 'sapo',
                        'aero','campy', 'chol', 
                        'epec', 'epec_a', 'epec_t', 'epec_unkn',
                        'etec', 'etec_st', 'etec_lt',  'etec_unkn',
                        'salm', 
                        'shig',
                        'crypto',  'ehist', 'giard', 
                        'noro_gi', 'noro_gii', 'rota_a', 
                        'aero_hydrophila', 'campy_coli', 'campy_jejuni', 'campy_lari',  'chol_o1', 'chol_o139', 
                        'salm_nts', 'salm_paratyphi', 
                        'shig_a', 'shig_c', 'shig_d',
                        'crypto_parvum', 'ehist_coli', 'ehist_dispar', 'ehist_histolytica', 'giard_l_i_d')
      
      
      ma_new2_combined_results_wide <- ma_new2_combined_results_wide[, c(1,2,9,8,6,11,10,7,4,3,5,12,13)]
      ma_new2_combined_results_wide <- ma_new2_combined_results_wide %>%
        mutate(names =  factor(names, levels = ordered_path)) %>%
        arrange(names)
      
      
      
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
    
    
### prep dataframes ###
    
  # each pathogen & strain & rota pre/post
    # pathogens
      pathogen_data_or_list <- unique(case_control_or_only$path)
        for (v in pathogen_data_or_list) {
        assign(paste0("data_or_", as.character(v)), case_control_or_only %>% filter (path == v), envir = .GlobalEnv)
        } 
    # pathogen strains
      strain_data_or_list <- unique(case_control_or_only$path_strain)
        for (s in strain_data_or_list) {
        assign(paste0("data_or_", as.character(s)), case_control_or_only %>% filter (path_strain == s), envir = .GlobalEnv)
        } 
    # rota pre/post
      data_or_rota_pre  <- case_control_or_only[ which(case_control_or_only$path == "rota" & case_control_or_only$rota_vax_lag == 0),]
      data_or_rota_post <- case_control_or_only[ which(case_control_or_only$path == "rota" & case_control_or_only$rota_vax_lag == 1),]
    
    
    # dataframe for each stratification (age group and child mortality level)
    
    # age group (young, mixed, old)
      data_or_young <- case_control_or_only[ which(case_control_or_only$age_group == "young"), ]
      data_or_old   <- case_control_or_only[ which(case_control_or_only$age_group == "old"), ]
    
    # young age by child mortality
      data_or_young_high     <- data_or_young[ which(data_or_young$child_mort == "high"), ]
      data_or_young_vlow_low <- data_or_young[ which((data_or_young$child_mort == "very low" | data_or_young$child_mort == "low")), ]
      
    # each pathogen and strain within the age/child mort dataframes
    # young/high
      for (v in pathogen_data_or_list) {
        assign(paste0("data_or_young_high_", as.character(v)), data_or_young_high %>% filter (path == v), envir = .GlobalEnv)
        }
      for (u in strain_data_or_list) {
        assign(paste0("data_or_young_high_", as.character(u)), data_or_young_high %>% filter (path_strain == u), envir = .GlobalEnv)
        } 
      data_or_young_high_rota_pre  <- data_or_young_high[ which(data_or_young_high$path == "rota" & data_or_young_high$rota_vax_lag == 0),]
      data_or_young_high_rota_post <- data_or_young_high[ which(data_or_young_high$path == "rota" & data_or_young_high$rota_vax_lag == 1),]
      
    
    # young/very low/low
      for (v in pathogen_data_or_list) {
        assign(paste0("data_or_young_vlow_low_", as.character(v)), data_or_young_vlow_low %>% filter (path == v), envir = .GlobalEnv)
        }
      for (u in strain_data_or_list) {
        assign(paste0("data_or_young_vlow_low_", as.character(u)), data_or_young_vlow_low %>% filter (path_strain == u), envir = .GlobalEnv)
        } 
      data_or_young_vlow_low_rota_pre  <- data_or_young_vlow_low[ which(data_or_young_vlow_low$path == "rota" & data_or_young_vlow_low$rota_vax_lag == 0),]
      data_or_young_vlow_low_rota_post <- data_or_young_vlow_low[ which(data_or_young_vlow_low$path == "rota" & data_or_young_vlow_low$rota_vax_lag == 1),]
      
      
    # old/all child mortality levels
      for (v in pathogen_data_or_list) {
        assign(paste0("data_or_old_", as.character(v)), data_or_old %>% filter (path == v), envir = .GlobalEnv)
        }
      for (u in strain_data_or_list) {
        assign(paste0("data_or_old_", as.character(u)), data_or_old %>% filter (path_strain == u), envir = .GlobalEnv)
        } 
      data_or_old_rota_pre  <- data_or_old[ which(data_or_old$path == "rota" & data_or_old$rota_vax_lag == 0),]
      data_or_old_rota_post <- data_or_old[ which(data_or_old$path == "rota" & data_or_old$rota_vax_lag == 1),]
       
      
      
    # age group 2 (youngest, middle)
      data_or_youngest <- case_control_or_only[ which(case_control_or_only$age_group_2 == "youngest"), ]
      data_or_middle   <- case_control_or_only[ which(case_control_or_only$age_group_2 == "middle"), ]
      
    # youngest age by child mortality
      data_or_youngest_high     <- data_or_youngest[ which(data_or_youngest$child_mort == "high"), ]
      data_or_youngest_vlow_low <- data_or_youngest[ which((data_or_youngest$child_mort == "very low" | data_or_youngest$child_mort == "low")), ]
    # middle age by child mortality
      data_or_middle_high     <- data_or_middle[ which(data_or_middle$child_mort == "high"), ]
      data_or_middle_vlow_low <- data_or_middle[ which((data_or_middle$child_mort == "very low" | data_or_middle$child_mort == "low")), ]
      
      
    # each pathogen and strain within the age/child mort dataframes
      # youngest/high
      for (v in pathogen_data_or_list) {
        assign(paste0("data_or_youngest_high_", as.character(v)), data_or_youngest_high %>% filter (path == v), envir = .GlobalEnv)
      }
      for (u in strain_data_or_list) {
        assign(paste0("data_or_youngest_high_", as.character(u)), data_or_youngest_high %>% filter (path_strain == u), envir = .GlobalEnv)
      } 
      data_or_youngest_high_rota_pre  <- data_or_youngest_high[ which(data_or_youngest_high$path == "rota" & data_or_youngest_high$rota_vax_lag == 0),]
      data_or_youngest_high_rota_post <- data_or_youngest_high[ which(data_or_youngest_high$path == "rota" & data_or_youngest_high$rota_vax_lag == 1),]
      
      
    # youngest/very low/low
      for (v in pathogen_data_or_list) {
        assign(paste0("data_or_youngest_vlow_low_", as.character(v)), data_or_youngest_vlow_low %>% filter (path == v), envir = .GlobalEnv)
      }
      for (u in strain_data_or_list) {
        assign(paste0("data_or_youngest_vlow_low_", as.character(u)), data_or_youngest_vlow_low %>% filter (path_strain == u), envir = .GlobalEnv)
      } 
      data_or_youngest_vlow_low_rota_pre  <- data_or_youngest_vlow_low[ which(data_or_youngest_vlow_low$path == "rota" & data_or_youngest_vlow_low$rota_vax_lag == 0),]
      data_or_youngest_vlow_low_rota_post <- data_or_youngest_vlow_low[ which(data_or_youngest_vlow_low$path == "rota" & data_or_youngest_vlow_low$rota_vax_lag == 1),]
      
      
    # middle/high
      for (v in pathogen_data_or_list) {
        assign(paste0("data_or_middle_high_", as.character(v)), data_or_middle_high %>% filter (path == v), envir = .GlobalEnv)
      }
      for (u in strain_data_or_list) {
        assign(paste0("data_or_middle_high_", as.character(u)), data_or_middle_high %>% filter (path_strain == u), envir = .GlobalEnv)
      } 
      data_or_middle_high_rota_pre  <- data_or_middle_high[ which(data_or_middle_high$path == "rota" & data_or_middle_high$rota_vax_lag == 0),]
      data_or_middle_high_rota_post <- data_or_middle_high[ which(data_or_middle_high$path == "rota" & data_or_middle_high$rota_vax_lag == 1),]
      
      
    # middle/very low/low
      for (v in pathogen_data_or_list) {
        assign(paste0("data_or_middle_vlow_low_", as.character(v)), data_or_middle_vlow_low %>% filter (path == v), envir = .GlobalEnv)
      }
      for (u in strain_data_or_list) {
        assign(paste0("data_or_middle_vlow_low_", as.character(u)), data_or_middle_vlow_low %>% filter (path_strain == u), envir = .GlobalEnv)
      } 
      data_or_middle_vlow_low_rota_pre  <- data_or_middle_vlow_low[ which(data_or_middle_vlow_low$path == "rota" & data_or_middle_vlow_low$rota_vax_lag == 0),]
      data_or_middle_vlow_low_rota_post <- data_or_middle_vlow_low[ which(data_or_middle_vlow_low$path == "rota" & data_or_middle_vlow_low$rota_vax_lag == 1),]
      
      
      
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
                            'data_or_rota_pre', 'data_or_rota_post',
                            'data_or_noro_gi', 'data_or_noro_gii', 'data_or_rota_a', 
                            'data_or_campy_coli', 'data_or_campy_jejuni', 'data_or_campy_lari', 'data_or_chol_o1', 'data_or_chol_o139',
                            'data_or_epec_a', 'data_or_epec_t', 'data_or_epec_unkn', 
                            'data_or_etec_st', 'data_or_etec_lt', 'data_or_etec_unkn',
                            'data_or_salm_nts', 'data_or_shig_a', 'data_or_shig_c', 'data_or_shig_d',
                            'data_or_crypto_parvum', 'data_or_ehist_coli', 'data_or_ehist_dispar', 'data_or_ehist_histolytica', 'data_or_giard_l_i_d')
 
  # labels for results
    data_or_labels_unadj <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                              'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                              'crypto', 'ehist', 'giard', 
                              'rota_pre', 'rota_post',
                              'noro_gi', 'noro_gii', 'rota_a', 
                              'campy_coli', 'campy_jejuni', 'campy_lari', 'chol_o1', 'chol_o139',
                              'epec_a', 'epec_t', 'epec_unkn', 
                              'etec_st', 'etec_lt', 'etec_unkn',
                              'salm_nts', 'shig_a', 'shig_c', 'shig_d',
                              'crypto_parvum', 'ehist_coli', 'ehist_dispar', 'ehist_histolytica', 'giard_l_i_d')
    
  # model
    ma_path_or_unadj <- list()
    for(i in seq_along(data_or_list_unadj)){ 
      ma_path_or_unadj[[i]] <- meta_analysis_loop_unadj(get(data_or_list_unadj[i]))
    }  
  # examine results  
    for (i in 1:40){
      print(ma_path_or_unadj[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:40) {
      coeff <- exp(coef(summary(ma_path_or_unadj[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
      lci   <- exp(coef(summary(ma_path_or_unadj[[i]]))$ci.lb[1])
      uci   <- exp(coef(summary(ma_path_or_unadj[[i]]))$ci.ub[1])  
      i2    <- ma_path_or_unadj[[i]]$I2
      group <- "unadj"
      put(coeff, lci, uci, i2, group)
    }
    ma_path_or_unadj_results <-magic_result_as_dataframe()
  # results in a dataframe
    names <- data_or_labels_unadj
    ma_path_or_unadj_results <- data.frame(names, ma_path_or_unadj_results)   
    ma_path_or_unadj_results$i <- NULL
    magic_free()
  # drop I2 values in prep for merging
    ma_path_or_unadj_results$i2 <- NULL
    
    
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
                                     'data_or_young_high_aero', 'data_or_young_high_campy', 'data_or_young_high_chol','data_or_young_high_epec',  'data_or_young_high_etec', 'data_or_young_high_salm', 'data_or_young_high_shig', 
                                     'data_or_young_high_crypto', 'data_or_young_high_ehist', 'data_or_young_high_giard',
                                     'data_or_young_high_rota_pre',
                                     'data_or_young_high_noro_gii', 
                                     'data_or_young_high_campy_jejuni', 
                                     'data_or_young_high_epec_a', 'data_or_young_high_epec_t', 'data_or_young_high_epec_unkn',
                                     'data_or_young_high_etec_st', 'data_or_young_high_etec_lt', 'data_or_young_high_etec_unkn',
                                     'data_or_young_high_ehist_histolytica', 'data_or_young_high_giard_l_i_d')
    
  # labels for results
    data_or_labels_young_high_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                       'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                                       'crypto', 'ehist', 'giard',
                                       'rota_pre',
                                       'noro_gii', 
                                       'campy_jejuni', 
                                       'epec_a', 'epec_t', 'epec_unkn',
                                       'etec_st', 'etec_lt', 'etec_unkn',
                                       'ehist_histolytica', 'giard_l_i_d')
  # model
    ma_path_or_young_high_all <- list()
    for(i in seq_along(data_or_list_young_high_all)){ 
      ma_path_or_young_high_all[[i]] <- meta_analysis_loop_all(get(data_or_list_young_high_all[i]))
    }  
  # examine results  
    for (i in 1:26){
      print(ma_path_or_young_high_all[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:26) {
      coeff <- exp(coef(summary(ma_path_or_young_high_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
      lci   <- exp(coef(summary(ma_path_or_young_high_all[[i]]))$ci.lb[1])
      uci   <- exp(coef(summary(ma_path_or_young_high_all[[i]]))$ci.ub[1])    
      group <- "young, high mortality"
      put(coeff, lci, uci, group)
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
                                         'data_or_young_vlow_low_campy', 'data_or_young_vlow_low_chol', 'data_or_young_vlow_low_epec',  'data_or_young_vlow_low_etec', 'data_or_young_vlow_low_salm', 'data_or_young_vlow_low_shig', 
                                         'data_or_young_vlow_low_crypto', 'data_or_young_vlow_low_ehist', 'data_or_young_vlow_low_giard',
                                         'data_or_young_vlow_low_rota_pre', 'data_or_young_vlow_low_rota_post',
                                         'data_or_young_vlow_low_noro_gii',
                                         'data_or_young_vlow_low_campy_jejuni', 
                                         'data_or_young_vlow_low_epec_a', 'data_or_young_vlow_low_epec_t', 'data_or_young_vlow_low_epec_unkn',
                                         'data_or_young_vlow_low_etec_st', 'data_or_young_vlow_low_etec_lt', 'data_or_young_vlow_low_etec_unkn',
                                         'data_or_young_vlow_low_giard_l_i_d')
      
  # labels for results
    data_or_labels_young_vlow_low_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                           'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                                           'crypto', 'ehist', 'giard',
                                           'rota_pre', 'rota_post',
                                           'noro_gii',
                                           'campy_jejuni', 
                                           'epec_a', 'epec_t', 'epec_unkn',
                                           'etec_st', 'etec_lt', 'etec_unkn',
                                           'giard_l_i_d')
  # model
    ma_path_or_young_vlow_low_all <- list()
    for(i in seq_along(data_or_list_young_vlow_low_all)){ 
      ma_path_or_young_vlow_low_all[[i]] <- meta_analysis_loop_all(get(data_or_list_young_vlow_low_all[i]))
    }  
  # examine results  
    for (i in 1:25){
      print(ma_path_or_young_vlow_low_all[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:25) {
      coeff <- exp(coef(summary(ma_path_or_young_vlow_low_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
      lci   <- exp(coef(summary(ma_path_or_young_vlow_low_all[[i]]))$ci.lb[1])
      uci   <- exp(coef(summary(ma_path_or_young_vlow_low_all[[i]]))$ci.ub[1])    
      group <- "young, vlow & low mortality"
      put(coeff, lci, uci, group)
    }
    ma_path_or_young_vlow_low_all_results <-magic_result_as_dataframe()
  # results in a dataframe
    names <- data_or_labels_young_vlow_low_all
    ma_path_or_young_vlow_low_all_results <- data.frame(names, ma_path_or_young_vlow_low_all_results)   
    ma_path_or_young_vlow_low_all_results$i <- NULL
    magic_free()          
    
    
### old/all child mort levels ### 
    
  # data list to loop through  
    data_or_list_old_all <- c('data_or_old_adeno', 'data_or_old_astro', 'data_or_old_noro', 'data_or_old_rota', 
                              'data_or_old_aero', 'data_or_old_campy', 'data_or_old_chol', 'data_or_old_epec',  'data_or_old_etec', 'data_or_old_salm', 'data_or_old_shig',
                              'data_or_old_crypto', 'data_or_old_ehist', 'data_or_old_giard',
                              'data_or_old_rota_pre', 
                              'data_or_old_campy_jejuni', 
                              'data_or_old_epec_a', 'data_or_old_epec_t', 'data_or_old_epec_unkn',
                              'data_or_old_etec_st', 'data_or_old_etec_lt', 'data_or_old_etec_unkn',
                              'data_or_old_ehist_histolytica', 'data_or_old_giard_l_i_d')
      
  # labels for results
    data_or_labels_old_all <- c('adeno', 'astro', 'noro', 'rota',
                                'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig',
                                'crypto', 'ehist', 'giard',
                                'rota_pre', 
                                'campy_jejuni',
                                'epec_a', 'epec_t', 'epec_unkn',
                                'etec_st', 'etec_lt', 'etec_unkn',
                                'ehist_histolytica', 'giard_l_i_d')
  # model
    ma_path_or_old_all <- list()
    for(i in seq_along(data_or_list_old_all)){ 
      ma_path_or_old_all[[i]] <- meta_analysis_loop_all(get(data_or_list_old_all[i]))
    }  
  # examine results  
    for (i in 1:24){
      print(ma_path_or_old_all[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:24) {
      coeff <- exp(coef(summary(ma_path_or_old_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
      lci   <- exp(coef(summary(ma_path_or_old_all[[i]]))$ci.lb[1])
      uci   <- exp(coef(summary(ma_path_or_old_all[[i]]))$ci.ub[1])    
      group <- "old, all mortality levels"
      put(coeff, lci, uci, group)
    }
    ma_path_or_old_all_results <-magic_result_as_dataframe()
  # results in a dataframe
    names <- data_or_labels_old_all
    ma_path_or_old_all_results <- data.frame(names, ma_path_or_old_all_results)   
    ma_path_or_old_all_results$i <- NULL
    magic_free()      
  
    
    
    
### youngest/high child mort levels ### 
    
  # data list to loop through  
    data_or_list_youngest_high_all <- c('data_or_youngest_high_adeno', 'data_or_youngest_high_astro', 'data_or_youngest_high_noro', 'data_or_youngest_high_rota', 'data_or_youngest_high_sapo',
                                        'data_or_youngest_high_aero', 'data_or_youngest_high_campy', 'data_or_youngest_high_chol', 'data_or_youngest_high_epec',  'data_or_youngest_high_etec', 'data_or_youngest_high_salm', 'data_or_youngest_high_shig', 
                                        'data_or_youngest_high_crypto', 'data_or_youngest_high_ehist', 'data_or_youngest_high_giard', 
                                        'data_or_youngest_high_rota_pre',
                                        'data_or_youngest_high_epec_a', 'data_or_youngest_high_epec_t','data_or_youngest_high_epec_unkn',
                                        'data_or_youngest_high_etec_st', 'data_or_youngest_high_etec_lt', 'data_or_youngest_high_etec_unkn')
        
  # labels for results
    data_or_labels_youngest_high_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                          'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                                          'crypto', 'ehist', 'giard', 
                                          'rota_pre',
                                          'epec_a', 'epec_t', 'epec_unkn',
                                          'etec_st', 'etec_lt', 'etec_unkn')
      
  # model
    ma_path_or_youngest_high_all <- list()
    for(i in seq_along(data_or_list_youngest_high_all)){ 
      ma_path_or_youngest_high_all[[i]] <- meta_analysis_loop_all(get(data_or_list_youngest_high_all[i]))
    }  
  # examine results  
    for (i in 1:22){
      print(ma_path_or_youngest_high_all[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:22) {
      coeff <- exp(coef(summary(ma_path_or_youngest_high_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
      lci   <- exp(coef(summary(ma_path_or_youngest_high_all[[i]]))$ci.lb[1])
      uci   <- exp(coef(summary(ma_path_or_youngest_high_all[[i]]))$ci.ub[1])    
      group <- "youngest, high mortality"
      put(coeff, lci, uci, group)
    }
    ma_path_or_youngest_high_all_results <-magic_result_as_dataframe()
  # results in a dataframe
    names <- data_or_labels_youngest_high_all
    ma_path_or_youngest_high_all_results <- data.frame(names, ma_path_or_youngest_high_all_results)   
    ma_path_or_youngest_high_all_results$i <- NULL
    magic_free()           
    
    
    
### middle/high child mort levels ### 
    
  # data list to loop through  
    data_or_list_middle_high_all <- c('data_or_middle_high_adeno', 'data_or_middle_high_astro', 'data_or_middle_high_noro', 'data_or_middle_high_rota', 'data_or_middle_high_sapo',
                                      'data_or_middle_high_aero', 'data_or_middle_high_campy', 'data_or_middle_high_epec',  'data_or_middle_high_etec', 'data_or_middle_high_shig', 
                                      'data_or_middle_high_crypto',
                                      'data_or_middle_high_rota_pre',
                                      'data_or_middle_high_epec_a', 'data_or_middle_high_epec_t', 'data_or_middle_high_epec_unkn',
                                      'data_or_middle_high_etec_st', 'data_or_middle_high_etec_lt', 'data_or_middle_high_etec_unkn')
    
  # labels for results
    data_or_labels_middle_high_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                        'aero', 'campy', 'epec',  'etec', 'shig', 
                                        'crypto', 
                                        'rota_pre',
                                        'epec_a', 'epec_t', 'epec_unkn',
                                        'etec_st', 'etec_lt', 'etec_unkn')
    
  # model
    ma_path_or_middle_high_all <- list()
    for(i in seq_along(data_or_list_middle_high_all)){ 
      ma_path_or_middle_high_all[[i]] <- meta_analysis_loop_all(get(data_or_list_middle_high_all[i]))
    }  
  # examine results  
    for (i in 1:18){
      print(ma_path_or_middle_high_all[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:18) {
      coeff <- exp(coef(summary(ma_path_or_middle_high_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
      lci   <- exp(coef(summary(ma_path_or_middle_high_all[[i]]))$ci.lb[1])
      uci   <- exp(coef(summary(ma_path_or_middle_high_all[[i]]))$ci.ub[1])    
      group <- "middle, high mortality"
      put(coeff, lci, uci, group)
    }
    ma_path_or_middle_high_all_results <-magic_result_as_dataframe()
  # results in a dataframe
    names <- data_or_labels_middle_high_all
    ma_path_or_middle_high_all_results <- data.frame(names, ma_path_or_middle_high_all_results)   
    ma_path_or_middle_high_all_results$i <- NULL
    magic_free()           
    
    
    
### youngest/v.low and low child mort levels ### 
    
  # data list to loop through  
    data_or_list_youngest_vlow_low_all <- c('data_or_youngest_vlow_low_adeno', 'data_or_youngest_vlow_low_astro', 'data_or_youngest_vlow_low_noro', 'data_or_youngest_vlow_low_rota', 'data_or_youngest_vlow_low_sapo',
                                            'data_or_youngest_vlow_low_campy', 'data_or_youngest_vlow_low_epec',  'data_or_youngest_vlow_low_etec', 'data_or_youngest_vlow_low_salm', 'data_or_youngest_vlow_low_shig', 
                                            'data_or_youngest_vlow_low_crypto', 'data_or_youngest_vlow_low_giard', 
                                            'data_or_youngest_vlow_low_rota_pre', 'data_or_youngest_vlow_low_rota_post',
                                            'data_or_youngest_vlow_low_epec_a', 'data_or_youngest_vlow_low_epec_t', 'data_or_youngest_vlow_low_epec_unkn',
                                            'data_or_youngest_vlow_low_etec_st', 'data_or_youngest_vlow_low_etec_lt', 'data_or_youngest_vlow_low_etec_unkn')
      
  # labels for results
    data_or_labels_youngest_vlow_low_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                              'campy', 'epec',  'etec', 'salm', 'shig', 
                                              'crypto', 'giard', 
                                              'rota_pre', 'rota_post',
                                              'epec_a', 'epec_t', 'epec_unkn',
                                              'etec_st', 'etec_lt', 'etec_unkn')
    
  # model
    ma_path_or_youngest_vlow_low_all <- list()
    for(i in seq_along(data_or_list_youngest_vlow_low_all)){ 
      ma_path_or_youngest_vlow_low_all[[i]] <- meta_analysis_loop_all(get(data_or_list_youngest_vlow_low_all[i]))
    }  
  # examine results  
    for (i in 1:20){
      print(ma_path_or_youngest_vlow_low_all[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:20) {
      coeff <- exp(coef(summary(ma_path_or_youngest_vlow_low_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
      lci   <- exp(coef(summary(ma_path_or_youngest_vlow_low_all[[i]]))$ci.lb[1])
      uci   <- exp(coef(summary(ma_path_or_youngest_vlow_low_all[[i]]))$ci.ub[1])    
      group <- "youngest, vlow & low mortality"
      put(coeff, lci, uci, group)
    }
    ma_path_or_youngest_vlow_low_all_results <-magic_result_as_dataframe()
  # results in a dataframe
    names <- data_or_labels_youngest_vlow_low_all
    ma_path_or_youngest_vlow_low_all_results <- data.frame(names, ma_path_or_youngest_vlow_low_all_results)   
    ma_path_or_youngest_vlow_low_all_results$i <- NULL
    magic_free()           
    
    
    
### middle/v.low and low child mort levels ### 
    
  # data list to loop through  
    data_or_list_middle_vlow_low_all <- c('data_or_middle_vlow_low_etec_st', 'data_or_middle_vlow_low_etec_lt', 'data_or_middle_vlow_low_etec_unkn',
                                          'data_or_middle_vlow_low_giard')
      
  # labels for results
    data_or_labels_middle_vlow_low_all <- c('etec_st', 'etec_lt', 'etec_unkn',
                                            'giard')
    
  # model
    ma_path_or_middle_vlow_low_all <- list()
    for(i in seq_along(data_or_list_middle_vlow_low_all)){ 
      ma_path_or_middle_vlow_low_all[[i]] <- meta_analysis_loop_all(get(data_or_list_middle_vlow_low_all[i]))
    }  
  # examine results  
    for (i in 1:4){
      print(ma_path_or_middle_vlow_low_all[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:4) {
      coeff <- exp(coef(summary(ma_path_or_middle_vlow_low_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
      lci   <- exp(coef(summary(ma_path_or_middle_vlow_low_all[[i]]))$ci.lb[1])
      uci   <- exp(coef(summary(ma_path_or_middle_vlow_low_all[[i]]))$ci.ub[1])    
      group <- "middle, vlow & low mortality"
      put(coeff, lci, uci, group)
    }
    ma_path_or_middle_vlow_low_all_results <-magic_result_as_dataframe()
  # results in a dataframe
    names <- data_or_labels_middle_vlow_low_all
    ma_path_or_middle_vlow_low_all_results <- data.frame(names, ma_path_or_middle_vlow_low_all_results)   
    ma_path_or_middle_vlow_low_all_results$i <- NULL
    magic_free()      
    
    
    
    
  # combinding all datasets
    options(scipen = 50)
    ma_or_combined_results <- rbind(ma_path_or_unadj_results, 
                                    ma_path_or_young_high_all_results, ma_path_or_young_vlow_low_all_results,
                                    ma_path_or_old_all_results,
                                    ma_path_or_youngest_high_all_results, ma_path_or_youngest_vlow_low_all_results,
                                    ma_path_or_middle_high_all_results, ma_path_or_middle_vlow_low_all_results)
  # formatting
    ma_or_combined_results$coeff  <- format(round(ma_or_combined_results$coeff, 1), nsmall=1, trim=TRUE)
    ma_or_combined_results$lci    <- format(round(ma_or_combined_results$lci, 1), nsmall=1, trim=TRUE)
    ma_or_combined_results$uci    <- format(round(ma_or_combined_results$uci, 1), nsmall=1, trim=TRUE)
  # creating variable with effect estimate and CI for pathogen
    ma_or_combined_results$part1  <- paste(ma_or_combined_results$coeff, ma_or_combined_results$lci, sep=" (")
    ma_or_combined_results$part2  <- paste(ma_or_combined_results$part1, ma_or_combined_results$uci, sep=", ")
    ma_or_combined_results$part3  <- ")"
    ma_or_combined_results$ee_ci  <- paste(ma_or_combined_results$part2, ma_or_combined_results$part3, sep="")
    # cleaning/dropping unnecessary variables
      ma_or_combined_results$part1 <- NULL
      ma_or_combined_results$part2 <- NULL
      ma_or_combined_results$part3 <- NULL
   # creating larger/broader group variable (for plotting later)
      ma_or_combined_results$strat <- ifelse(ma_or_combined_results$group == 'unadj', 'unadjusted',
                                      ifelse((ma_or_combined_results$group == 'young, high mortality' |
                                              ma_or_combined_results$group == 'young, vlow & low mortality'), '0-5 years',
                                      ifelse(ma_or_combined_results$group == 'old, all mortality levels', '>5 years', 
                                      ifelse((ma_or_combined_results$group == 'youngest, high mortality' |
                                              ma_or_combined_results$group == 'youngest, vlow & low mortality'), '0-2 years', 
                                      ifelse((ma_or_combined_results$group == 'middle, high mortality' |
                                              ma_or_combined_results$group == 'middle, vlow & low mortality'), '3-5 years',NA)))))
  # switching from long to wide structure
    # keep only needed variables
      ma_or_combined_results_wide <- ma_or_combined_results %>% select(names, group, ee_ci)
      ma_or_combined_results_wide <- ma_or_combined_results_wide %>% pivot_wider(names_from = c(group), values_from = c(ee_ci))
    # format and export
      ma_or_combined_results_wide <- ma_or_combined_results_wide[, c(1,2,7,6,9,8,4,3,5)]
      ma_or_combined_results_wide <- ma_or_combined_results_wide %>%
                                     mutate(names =  factor(names, levels = ordered_path)) %>%
                                     arrange(names)
     

##################################################################
####       RUNNING MODELS- **NEW METHOD** ALL PREDICTORS      ####
####    NEW STRATEGY-- LOOPING THROUGH PATHOGEN DATAFRAMES    ####
###       SENSITIVITY ANALYSIS-- EXCLUDING COINFECTIONS       ####    
####                     JUL 13, 2020                         ####
##################################################################   
    
# dataframe without co-infections
    case_control_coinf <- case_control_full[ which(case_control_full$path_num == 1), ]
    
    
### prep dataframes ###
    
  # each pathogen & strain & rota pre/post (using 1 year lag on WHO vax intro year)
    # pathogens
      pathogen_data_coinf_list <- unique(case_control_coinf$path)
        for (v in pathogen_data_coinf_list) {
          assign(paste0("data_coinf_", as.character(v)), case_control_coinf %>% filter (path == v), envir = .GlobalEnv)
          } 
    # pathogen strains
      strain_data_coinf_list <- unique(case_control_coinf$path_strain)
        for (s in strain_data_coinf_list) {
          assign(paste0("data_coinf_", as.character(s)), case_control_coinf %>% filter (path_strain == s), envir = .GlobalEnv)
          } 
    # rota pre/post
      data_coinf_rota_pre  <- case_control_coinf[ which(case_control_coinf$path == "rota" & case_control_coinf$rota_vax_lag == 0),]
      data_coinf_rota_post <- case_control_coinf[ which(case_control_coinf$path == "rota" & case_control_coinf$rota_vax_lag == 1),]
      
      
  # dataframe for each stratification (age group and child mortality level)
      
    # age group (young, mixed, old)
      data_coinf_young <- case_control_coinf[ which(case_control_coinf$age_group == "young"), ]
      data_coinf_old   <- case_control_coinf[ which(case_control_coinf$age_group == "old"), ]
      
    # young age by child mortality
      data_coinf_young_high     <- data_coinf_young[ which(data_coinf_young$child_mort == "high"), ]
      data_coinf_young_vlow_low <- data_coinf_young[ which((data_coinf_young$child_mort == "very low" | data_coinf_young$child_mort == "low")), ]
      
  # each pathogen and strain within the age/child mort dataframes
    # young/high
      for (v in pathogen_data_coinf_list) {
        assign(paste0("data_coinf_young_high_", as.character(v)), data_coinf_young_high %>% filter (path == v), envir = .GlobalEnv)
        }
      for (u in strain_data_coinf_list) {
        assign(paste0("data_coinf_young_high_", as.character(u)), data_coinf_young_high %>% filter (path_strain == u), envir = .GlobalEnv)
        } 
      data_coinf_young_high_rota_pre  <- data_coinf_young_high[ which(data_coinf_young_high$path == "rota" & data_coinf_young_high$rota_vax_lag == 0),]
      data_coinf_young_high_rota_post <- data_coinf_young_high[ which(data_coinf_young_high$path == "rota" & data_coinf_young_high$rota_vax_lag == 1),]
      
      
    # young/very low/low
      for (v in pathogen_data_coinf_list) {
        assign(paste0("data_coinf_young_vlow_low_", as.character(v)), data_coinf_young_vlow_low %>% filter (path == v), envir = .GlobalEnv)
        }
      for (u in strain_data_coinf_list) {
        assign(paste0("data_coinf_young_vlow_low_", as.character(u)), data_coinf_young_vlow_low %>% filter (path_strain == u), envir = .GlobalEnv)
        } 
      data_coinf_young_vlow_low_rota_pre  <- data_coinf_young_vlow_low[ which(data_coinf_young_vlow_low$path == "rota" & data_coinf_young_vlow_low$rota_vax_lag == 0),]
      data_coinf_young_vlow_low_rota_post <- data_coinf_young_vlow_low[ which(data_coinf_young_vlow_low$path == "rota" & data_coinf_young_vlow_low$rota_vax_lag == 1),]
      
      
    # old/all child mortality levels
      for (v in pathogen_data_coinf_list) {
        assign(paste0("data_coinf_old_", as.character(v)), data_coinf_old %>% filter (path == v), envir = .GlobalEnv)
        }
      for (u in strain_data_coinf_list) {
        assign(paste0("data_coinf_old_", as.character(u)), data_coinf_old %>% filter (path_strain == u), envir = .GlobalEnv)
        } 
      data_coinf_old_rota_pre  <- data_coinf_old[ which(data_coinf_old$path == "rota" & data_coinf_old$rota_vax_lag == 0),]
      data_coinf_old_rota_post <- data_coinf_old[ which(data_coinf_old$path == "rota" & data_coinf_old$rota_vax_lag == 1),]
    
      
      
    # age group 2 (youngest, middle)
      data_coinf_youngest <- case_control_coinf[ which(case_control_coinf$age_group_2 == "youngest"), ]
      data_coinf_middle   <- case_control_coinf[ which(case_control_coinf$age_group_2 == "middle"), ]
      
    # youngest age by child mortality
      data_coinf_youngest_high     <- data_coinf_youngest[ which(data_coinf_youngest$child_mort == "high"), ]
      data_coinf_youngest_vlow_low <- data_coinf_youngest[ which((data_coinf_youngest$child_mort == "very low" | data_coinf_youngest$child_mort == "low")), ]
     
    # middle age by child mortality
      data_coinf_middle_high     <- data_coinf_middle[ which(data_coinf_middle$child_mort == "high"), ]
      data_coinf_middle_vlow_low <- data_coinf_middle[ which((data_coinf_middle$child_mort == "very low" | data_coinf_middle$child_mort == "low")), ]
      
       
    # each pathogen and strain within the age/child mort dataframes
      # youngest/high
        for (v in pathogen_data_coinf_list) {
          assign(paste0("data_coinf_youngest_high_", as.character(v)), data_coinf_youngest_high %>% filter (path == v), envir = .GlobalEnv)
        }
        for (u in strain_data_coinf_list) {
          assign(paste0("data_coinf_youngest_high_", as.character(u)), data_coinf_youngest_high %>% filter (path_strain == u), envir = .GlobalEnv)
        } 
        data_coinf_youngest_high_rota_pre  <- data_coinf_youngest_high[ which(data_coinf_youngest_high$path == "rota" & data_coinf_youngest_high$rota_vax_lag == 0),]
        data_coinf_youngest_high_rota_post <- data_coinf_youngest_high[ which(data_coinf_youngest_high$path == "rota" & data_coinf_youngest_high$rota_vax_lag == 1),]
        
      
      # youngest/very low/low
        for (v in pathogen_data_coinf_list) {
          assign(paste0("data_coinf_youngest_vlow_low_", as.character(v)), data_coinf_youngest_vlow_low %>% filter (path == v), envir = .GlobalEnv)
        }
        for (u in strain_data_coinf_list) {
          assign(paste0("data_coinf_youngest_vlow_low_", as.character(u)), data_coinf_youngest_vlow_low %>% filter (path_strain == u), envir = .GlobalEnv)
        } 
        data_coinf_youngest_vlow_low_rota_pre  <- data_coinf_youngest_vlow_low[ which(data_coinf_youngest_vlow_low$path == "rota" & data_coinf_youngest_vlow_low$rota_vax_lag == 0),]
        data_coinf_youngest_vlow_low_rota_post <- data_coinf_youngest_vlow_low[ which(data_coinf_youngest_vlow_low$path == "rota" & data_coinf_youngest_vlow_low$rota_vax_lag == 1),]
        
        

      # middle/high
        for (v in pathogen_data_coinf_list) {
          assign(paste0("data_coinf_middle_high_", as.character(v)), data_coinf_middle_high %>% filter (path == v), envir = .GlobalEnv)
        }
        for (u in strain_data_coinf_list) {
          assign(paste0("data_coinf_middle_high_", as.character(u)), data_coinf_middle_high %>% filter (path_strain == u), envir = .GlobalEnv)
        } 
        data_coinf_middle_high_rota_pre  <- data_coinf_middle_high[ which(data_coinf_middle_high$path == "rota" & data_coinf_middle_high$rota_vax_lag == 0),]
        data_coinf_middle_high_rota_post <- data_coinf_middle_high[ which(data_coinf_middle_high$path == "rota" & data_coinf_middle_high$rota_vax_lag == 1),]
        
        
      # middle/very low/low
        for (v in pathogen_data_coinf_list) {
          assign(paste0("data_coinf_middle_vlow_low_", as.character(v)), data_coinf_middle_vlow_low %>% filter (path == v), envir = .GlobalEnv)
        }
        for (u in strain_data_coinf_list) {
          assign(paste0("data_coinf_middle_vlow_low_", as.character(u)), data_coinf_middle_vlow_low %>% filter (path_strain == u), envir = .GlobalEnv)
        } 
        data_coinf_middle_vlow_low_rota_pre  <- data_coinf_middle_vlow_low[ which(data_coinf_middle_vlow_low$path == "rota" & data_coinf_middle_vlow_low$rota_vax_lag == 0),]
        data_coinf_middle_vlow_low_rota_post <- data_coinf_middle_vlow_low[ which(data_coinf_middle_vlow_low$path == "rota" & data_coinf_middle_vlow_low$rota_vax_lag == 1),]
        
      
    
    
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
                               'data_coinf_rota_pre', 'data_coinf_rota_post',
                               'data_coinf_noro_gi', 'data_coinf_noro_gii', 
                               'data_coinf_campy_coli', 'data_coinf_campy_jejuni',
                               'data_coinf_epec_a', 'data_coinf_epec_t', 'data_coinf_epec_unkn',
                               'data_coinf_etec_st', 'data_coinf_etec_lt', 'data_coinf_etec_unkn',
                               'data_coinf_ehist_histolytica', 'data_coinf_giard_l_i_d')
    
  # labels for results
    data_coinf_labels_unadj <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                 'aero', 'campy', 'chol', 'epec', 'etec', 'salm', 'shig', 
                                 'crypto', 'ehist', 'giard', 
                                 'rota_pre', 'rota_post',
                                 'noro_gi', 'noro_gii', 
                                 'campy_coli', 'campy_jejuni',
                                 'epec_a', 'epec_t', 'epec_unkn',
                                 'etec_st', 'etec_lt', 'etec_unkn',
                                 'ehist_histolytica', 'giard_l_i_d')
  # model
    ma_path_coinf_unadj <- list()
    for(i in seq_along(data_coinf_list_unadj)){ 
      ma_path_coinf_unadj[[i]] <- meta_analysis_loop_unadj(get(data_coinf_list_unadj[i]))
    }  
  # examine results  
    for (i in 1:29){
      print(ma_path_coinf_unadj[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:29) {
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
    data_coinf_list_young_high_all <- c('data_coinf_young_high_adeno', 'data_coinf_young_high_astro', 'data_coinf_young_high_noro', 'data_coinf_young_high_rota',
                                        'data_coinf_young_high_aero', 'data_coinf_young_high_campy', 'data_coinf_young_high_chol', 'data_coinf_young_high_epec',  'data_coinf_young_high_etec', 'data_coinf_young_high_salm', 'data_coinf_young_high_shig', 
                                        'data_coinf_young_high_crypto', 'data_coinf_young_high_ehist', 'data_coinf_young_high_giard',
                                        'data_coinf_young_high_rota_pre', 
                                        'data_coinf_young_high_campy_jejuni',
                                        'data_coinf_young_high_epec_a', 'data_coinf_young_high_epec_t', 'data_coinf_young_high_epec_unkn',
                                        'data_coinf_young_high_etec_st', 'data_coinf_young_high_etec_lt', 'data_coinf_young_high_etec_unkn',
                                        'data_coinf_young_high_ehist_histolytica', 'data_coinf_young_high_giard_l_i_d')
  # labels for results
    data_coinf_labels_young_high_all <- c('adeno', 'astro', 'noro', 'rota', 
                                          'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                                          'crypto', 'ehist', 'giard',
                                          'rota_pre', 
                                          'campy_jejuni',
                                          'epec_a', 'epec_t', 'epec_unkn',
                                          'etec_st', 'etec_lt', 'etec_unkn',
                                          'ehist_histolytica', 'giard_l_i_d')
  # model
    ma_path_coinf_young_high_all <- list()
    for(i in seq_along(data_coinf_list_young_high_all)){ 
      ma_path_coinf_young_high_all[[i]] <- meta_analysis_loop_all(get(data_coinf_list_young_high_all[i]))
    }  
  # examine results  
    for (i in 1:24){
      print(ma_path_coinf_young_high_all[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:24) {
      coeff <- exp(coef(summary(ma_path_coinf_young_high_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
      lci   <- exp(coef(summary(ma_path_coinf_young_high_all[[i]]))$ci.lb[1])
      uci   <- exp(coef(summary(ma_path_coinf_young_high_all[[i]]))$ci.ub[1])    
      group <- "young, high mortality"
      put(coeff, lci, uci, group)
    }
    ma_path_coinf_young_high_all_results <-magic_result_as_dataframe()
  # results in a dataframe
    names <- data_coinf_labels_young_high_all
    ma_path_coinf_young_high_all_results <- data.frame(names, ma_path_coinf_young_high_all_results)   
    ma_path_coinf_young_high_all_results$i <- NULL
    magic_free()    
 
    
### young/vlow & low ### 
    
  # data list to loop through  
    data_coinf_list_young_vlow_low_all <- c('data_coinf_young_vlow_low_adeno', 'data_coinf_young_vlow_low_astro', 'data_coinf_young_vlow_low_noro', 'data_coinf_young_vlow_low_rota', 'data_coinf_young_vlow_low_sapo',
                                            'data_coinf_young_vlow_low_campy', 'data_coinf_young_vlow_low_chol', 'data_coinf_young_vlow_low_epec',  'data_coinf_young_vlow_low_etec', 'data_coinf_young_vlow_low_salm', 'data_coinf_young_vlow_low_shig', 
                                            'data_coinf_young_vlow_low_crypto', 'data_coinf_young_vlow_low_ehist', 'data_coinf_young_vlow_low_giard',
                                            'data_coinf_young_vlow_low_rota_pre',
                                            'data_coinf_young_vlow_low_noro_gii', 
                                            'data_coinf_young_vlow_low_campy_jejuni',
                                            'data_coinf_young_vlow_low_epec_a', 'data_coinf_young_vlow_low_epec_t', 'data_coinf_young_vlow_low_epec_unkn',
                                            'data_coinf_young_vlow_low_etec_st', 'data_coinf_young_vlow_low_etec_lt', 'data_coinf_young_vlow_low_etec_unkn',
                                            'data_coinf_young_vlow_low_giard_l_i_d')
  # labels for results
    data_coinf_labels_young_vlow_low_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                              'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                                              'crypto', 'ehist', 'giard',
                                              'rota_pre', 
                                              'noro_gii', 
                                              'campy_jejuni',
                                              'epec_a', 'epec_t', 'epec_unkn',
                                              'etec_st', 'etec_lt', 'etec_unkn',
                                              'giard_l_i_d')
  # model
    ma_path_coinf_young_vlow_low_all <- list()
    for(i in seq_along(data_coinf_list_young_vlow_low_all)){ 
      ma_path_coinf_young_vlow_low_all[[i]] <- meta_analysis_loop_all(get(data_coinf_list_young_vlow_low_all[i]))
    }  
  # examine results  
    for (i in 1:24){
      print(ma_path_coinf_young_vlow_low_all[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:24) {
      coeff <- exp(coef(summary(ma_path_coinf_young_vlow_low_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
      lci   <- exp(coef(summary(ma_path_coinf_young_vlow_low_all[[i]]))$ci.lb[1])
      uci   <- exp(coef(summary(ma_path_coinf_young_vlow_low_all[[i]]))$ci.ub[1])    
      group <- "young, vlow & low mortality"
      put(coeff, lci, uci, group)
    }
    ma_path_coinf_young_vlow_low_all_results <-magic_result_as_dataframe()
  # results in a dataframe
    names <- data_coinf_labels_young_vlow_low_all
    ma_path_coinf_young_vlow_low_all_results <- data.frame(names, ma_path_coinf_young_vlow_low_all_results)   
    ma_path_coinf_young_vlow_low_all_results$i <- NULL
    magic_free()    
  
    
### old/all child mort levels ### 
    
  # data list to loop through  
    data_coinf_list_old_all <- c('data_coinf_old_adeno', 'data_coinf_old_astro', 'data_coinf_old_noro', 'data_coinf_old_rota', 
                                 'data_coinf_old_aero', 'data_coinf_old_campy', 'data_coinf_old_chol', 'data_coinf_old_epec',  'data_coinf_old_etec', 'data_coinf_old_salm', 'data_coinf_old_shig',
                                 'data_coinf_old_crypto', 'data_coinf_old_ehist', 'data_coinf_old_giard',
                                 'data_coinf_old_rota_pre', 
                                 'data_coinf_old_campy_jejuni',
                                 'data_coinf_old_epec_a', 'data_coinf_old_epec_t', 'data_coinf_old_epec_unkn',
                                 'data_coinf_old_etec_st', 'data_coinf_old_etec_lt', 'data_coinf_old_etec_unkn',
                                 'data_coinf_old_ehist_histolytica', 'data_coinf_old_giard_l_i_d')
  # labels for results
    data_coinf_labels_old_all <- c('adeno', 'astro', 'noro', 'rota',
                                   'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig',
                                   'crypto', 'ehist', 'giard',
                                   'rota_pre', 
                                   'campy_jejuni',
                                   'epec_a', 'epec_t', 'epec_unkn',
                                   'etec_st', 'etec_lt', 'etec_unkn',
                                   'ehist_histolytica', 'giard_l_i_d')
  # model
    ma_path_coinf_old_all <- list()
    for(i in seq_along(data_coinf_list_old_all)){ 
      ma_path_coinf_old_all[[i]] <- meta_analysis_loop_all(get(data_coinf_list_old_all[i]))
    }  
  # examine results  
    for (i in 1:24){
      print(ma_path_coinf_old_all[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:24) {
      coeff <- exp(coef(summary(ma_path_coinf_old_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
      lci   <- exp(coef(summary(ma_path_coinf_old_all[[i]]))$ci.lb[1])
      uci   <- exp(coef(summary(ma_path_coinf_old_all[[i]]))$ci.ub[1])    
      group <- "old, all mortality levels"  
      put(coeff, lci, uci, group)
    }
    ma_path_coinf_old_all_results <-magic_result_as_dataframe()
  # results in a dataframe
    names <- data_coinf_labels_old_all
    ma_path_coinf_old_all_results <- data.frame(names, ma_path_coinf_old_all_results)   
    ma_path_coinf_old_all_results$i <- NULL
    magic_free()      
  
    
    
    
### youngest/high child mort levels ### 
    
  # data list to loop through  
    data_coinf_list_youngest_high_all <- c('data_coinf_youngest_high_adeno', 'data_coinf_youngest_high_astro', 'data_coinf_youngest_high_noro', 'data_coinf_youngest_high_rota', 'data_coinf_youngest_high_sapo',
                                           'data_coinf_youngest_high_aero', 'data_coinf_youngest_high_campy', 'data_coinf_youngest_high_chol', 'data_coinf_youngest_high_epec',  'data_coinf_youngest_high_etec', 'data_coinf_youngest_high_salm', 'data_coinf_youngest_high_shig', 
                                           'data_coinf_youngest_high_crypto', 'data_coinf_youngest_high_ehist', 'data_coinf_youngest_high_giard', 
                                           'data_coinf_youngest_high_rota_pre',
                                           'data_coinf_youngest_high_epec_unkn',
                                           'data_coinf_youngest_high_etec_st', 'data_coinf_youngest_high_etec_lt', 'data_coinf_youngest_high_etec_unkn')
      
  # labels for results
    data_coinf_labels_youngest_high_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                             'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                                             'crypto', 'ehist', 'giard', 
                                             'rota_pre',
                                             'epec_unkn',
                                             'etec_st', 'etec_lt', 'etec_unkn')
      
  # model
    ma_path_coinf_youngest_high_all <- list()
    for(i in seq_along(data_coinf_list_youngest_high_all)){ 
      ma_path_coinf_youngest_high_all[[i]] <- meta_analysis_loop_all(get(data_coinf_list_youngest_high_all[i]))
    }  
  # examine results  
    for (i in 1:20){
      print(ma_path_coinf_youngest_high_all[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:20) {
      coeff <- exp(coef(summary(ma_path_coinf_youngest_high_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
      lci   <- exp(coef(summary(ma_path_coinf_youngest_high_all[[i]]))$ci.lb[1])
      uci   <- exp(coef(summary(ma_path_coinf_youngest_high_all[[i]]))$ci.ub[1])    
      group <- "youngest, high mortality"
      put(coeff, lci, uci, group)
    }
    ma_path_coinf_youngest_high_all_results <-magic_result_as_dataframe()
  # results in a dataframe
    names <- data_coinf_labels_youngest_high_all
    ma_path_coinf_youngest_high_all_results <- data.frame(names, ma_path_coinf_youngest_high_all_results)   
    ma_path_coinf_youngest_high_all_results$i <- NULL
    magic_free()           
    
    
    
### middle/high child mort levels ### 
    
  # data list to loop through  
    data_coinf_list_middle_high_all <- c('data_coinf_middle_high_rota', 
                                         'data_coinf_middle_high_aero', 'data_coinf_middle_high_campy', 
                                         'data_coinf_middle_high_epec',  
                                         'data_coinf_middle_high_etec', 
                                         #'data_coinf_middle_high_shig', 
                                         'data_coinf_middle_high_rota_pre',
                                         'data_coinf_middle_high_epec_unkn',
                                         'data_coinf_middle_high_etec_unkn')
      
  # labels for results
    data_coinf_labels_middle_high_all <- c('rota', 
                                           'aero', 'campy', 
                                           'epec',  
                                           'etec', 
                                           #'shig', 
                                           'rota_pre',
                                           'epec_unkn',
                                           'etec_unkn')
    
  # model
    ma_path_coinf_middle_high_all <- list()
    for(i in seq_along(data_coinf_list_middle_high_all)){ 
      ma_path_coinf_middle_high_all[[i]] <- meta_analysis_loop_all(get(data_coinf_list_middle_high_all[i]))
    }  
  # examine results  
    for (i in 1:8){
      print(ma_path_coinf_middle_high_all[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:8) {
      coeff <- exp(coef(summary(ma_path_coinf_middle_high_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
      lci   <- exp(coef(summary(ma_path_coinf_middle_high_all[[i]]))$ci.lb[1])
      uci   <- exp(coef(summary(ma_path_coinf_middle_high_all[[i]]))$ci.ub[1])    
      group <- "middle, high mortality"
      put(coeff, lci, uci, group)
    }
    ma_path_coinf_middle_high_all_results <-magic_result_as_dataframe()
  # results in a dataframe
    names <- data_coinf_labels_middle_high_all
    ma_path_coinf_middle_high_all_results <- data.frame(names, ma_path_coinf_middle_high_all_results)   
    ma_path_coinf_middle_high_all_results$i <- NULL
    magic_free()           
    
    
    
### youngest/v.low and low child mort levels ### 
    
  # data list to loop through  
    data_coinf_list_youngest_vlow_low_all <- c('data_coinf_youngest_vlow_low_adeno', 'data_coinf_youngest_vlow_low_astro', 'data_coinf_youngest_vlow_low_noro', 'data_coinf_youngest_vlow_low_rota', 'data_coinf_youngest_vlow_low_sapo',
                                               'data_coinf_youngest_vlow_low_campy', 'data_coinf_youngest_vlow_low_epec',  'data_coinf_youngest_vlow_low_etec', 'data_coinf_youngest_vlow_low_salm', 'data_coinf_youngest_vlow_low_shig', 
                                               'data_coinf_youngest_vlow_low_crypto', 'data_coinf_youngest_vlow_low_giard', 
                                               'data_coinf_youngest_vlow_low_rota_pre', 
                                               'data_coinf_youngest_vlow_low_epec_a', 'data_coinf_youngest_vlow_low_epec_t', 'data_coinf_youngest_vlow_low_epec_unkn',
                                               'data_coinf_youngest_vlow_low_etec_st', 'data_coinf_youngest_vlow_low_etec_lt', 'data_coinf_youngest_vlow_low_etec_unkn')
      
  # labels for results
    data_coinf_labels_youngest_vlow_low_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                                 'campy', 'epec',  'etec', 'salm', 'shig', 
                                                 'crypto', 'giard', 
                                                 'rota_pre', 
                                                 'epec_a', 'epec_t', 'epec_unkn',
                                                 'etec_st', 'etec_lt', 'etec_unkn')
    
  # model
    ma_path_coinf_youngest_vlow_low_all <- list()
    for(i in seq_along(data_coinf_list_youngest_vlow_low_all)){ 
      ma_path_coinf_youngest_vlow_low_all[[i]] <- meta_analysis_loop_all(get(data_coinf_list_youngest_vlow_low_all[i]))
    }  
  # examine results  
    for (i in 1:19){
      print(ma_path_coinf_youngest_vlow_low_all[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:19) {
      coeff <- exp(coef(summary(ma_path_coinf_youngest_vlow_low_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
      lci   <- exp(coef(summary(ma_path_coinf_youngest_vlow_low_all[[i]]))$ci.lb[1])
      uci   <- exp(coef(summary(ma_path_coinf_youngest_vlow_low_all[[i]]))$ci.ub[1])    
      group <- "youngest, vlow & low mortality"
      put(coeff, lci, uci, group)
    }
    ma_path_coinf_youngest_vlow_low_all_results <-magic_result_as_dataframe()
  # results in a dataframe
    names <- data_coinf_labels_youngest_vlow_low_all
    ma_path_coinf_youngest_vlow_low_all_results <- data.frame(names, ma_path_coinf_youngest_vlow_low_all_results)   
    ma_path_coinf_youngest_vlow_low_all_results$i <- NULL
    magic_free()           
    
    
    
### middle/v.low and low child mort levels ### 
    
  # data list to loop through  
    data_coinf_list_middle_vlow_low_all <- c('data_coinf_middle_vlow_low_epec',  'data_coinf_middle_vlow_low_etec', 
                                             'data_coinf_middle_vlow_low_epec_unkn', 'data_coinf_middle_vlow_low_giard',
                                             'data_coinf_middle_vlow_low_etec_st', 'data_coinf_middle_vlow_low_etec_lt', 'data_coinf_middle_vlow_low_etec_unkn')
      
  # labels for results
    data_coinf_labels_middle_vlow_low_all <- c('epec',  'etec', 
                                               'epec_unkn', 'giard',
                                               'etec_st', 'etec_lt', 'etec_unkn')
    
  # model
    ma_path_coinf_middle_vlow_low_all <- list()
    for(i in seq_along(data_coinf_list_middle_vlow_low_all)){ 
      ma_path_coinf_middle_vlow_low_all[[i]] <- meta_analysis_loop_all(get(data_coinf_list_middle_vlow_low_all[i]))
    }  
  # examine results  
    for (i in 1:7){
      print(ma_path_coinf_middle_vlow_low_all[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:7) {
      coeff <- exp(coef(summary(ma_path_coinf_middle_vlow_low_all[[i]]))$estimate[1]) # extract & exponentiate coeff for "intercept" only 
      lci   <- exp(coef(summary(ma_path_coinf_middle_vlow_low_all[[i]]))$ci.lb[1])
      uci   <- exp(coef(summary(ma_path_coinf_middle_vlow_low_all[[i]]))$ci.ub[1])    
      group <- "middle, vlow & low mortality"
      put(coeff, lci, uci, group)
    }
    ma_path_coinf_middle_vlow_low_all_results <-magic_result_as_dataframe()
  # results in a dataframe
    names <- data_coinf_labels_middle_vlow_low_all
    ma_path_coinf_middle_vlow_low_all_results <- data.frame(names, ma_path_coinf_middle_vlow_low_all_results)   
    ma_path_coinf_middle_vlow_low_all_results$i <- NULL
    magic_free()          
    
    
    
# combinding all datasets
    ma_coinf_combined_results <- rbind(ma_path_coinf_unadj_results, 
                                       ma_path_coinf_young_high_all_results, ma_path_coinf_young_vlow_low_all_results,
                                       ma_path_coinf_old_all_results,
                                       ma_path_coinf_youngest_high_all_results, ma_path_coinf_youngest_vlow_low_all_results,
                                       ma_path_coinf_middle_high_all_results, ma_path_coinf_middle_vlow_low_all_results)
  # formatting
    ma_coinf_combined_results$coeff  <- format(round(ma_coinf_combined_results$coeff, 1), nsmall=1, trim=TRUE)
    ma_coinf_combined_results$lci    <- format(round(ma_coinf_combined_results$lci, 1), nsmall=1, trim=TRUE)
    ma_coinf_combined_results$uci    <- format(round(ma_coinf_combined_results$uci, 1), nsmall=1, trim=TRUE)
  # creating variable with effect estimate and CI for pathogen
    ma_coinf_combined_results$part1  <- paste(ma_coinf_combined_results$coeff, ma_coinf_combined_results$lci, sep=" (")
    ma_coinf_combined_results$part2  <- paste(ma_coinf_combined_results$part1, ma_coinf_combined_results$uci, sep=", ")
    ma_coinf_combined_results$part3  <- ")"
    ma_coinf_combined_results$ee_ci  <- paste(ma_coinf_combined_results$part2, ma_coinf_combined_results$part3, sep="")
  # cleaning/dropping unnecessary variables
    ma_coinf_combined_results$part1 <- NULL
    ma_coinf_combined_results$part2 <- NULL
    ma_coinf_combined_results$part3 <- NULL
  # creating larger/broader group variable (for plotting later)
    ma_coinf_combined_results$strat <- ifelse(ma_coinf_combined_results$group == 'unadj', 'unadjusted',
                                       ifelse((ma_coinf_combined_results$group == 'young, high mortality' |
                                               ma_coinf_combined_results$group == 'young, vlow & low mortality'), '0-5 years',
                                       ifelse(ma_coinf_combined_results$group == 'old, all mortality levels', '>5 years', 
                                       ifelse((ma_coinf_combined_results$group == 'youngest, high mortality' |
                                               ma_coinf_combined_results$group == 'youngest, vlow & low mortality'), '0-2 years',
                                       ifelse((ma_coinf_combined_results$group == 'middle, high mortality' |
                                               ma_coinf_combined_results$group == 'middle, vlow & low mortality'), '3-5 years',NA)))))
    
  # switching from long to wide structure
    # keep only needed variables
      ma_coinf_combined_results_wide <- ma_coinf_combined_results %>% select(names, group, ee_ci)
      ma_coinf_combined_results_wide <- ma_coinf_combined_results_wide %>% pivot_wider(names_from = c(group), values_from = c(ee_ci))
    # switching structure
      ma_coinf_combined_results_wide <- ma_coinf_combined_results_wide[, c(1,2,7,6,9,8,4,3,5)]
      ma_coinf_combined_results_wide <- ma_coinf_combined_results_wide %>%
                                        mutate(names =  factor(names, levels = ordered_path)) %>%
                                        arrange(names)
    
  
    
    
##################################################################
####       RUNNING MODELS- **NEW METHOD** ALL PREDICTORS      ####
####          STRATIFICATION BY DETECTION METHOD ONLY         ####
####            LOOPING THROUGH PATHOGEN DATAFRAMES           ####
####                     Dec 04, 2020                         ####
##################################################################         
    
# dataframes with only those tested by PCR and only those not tested by pcr
  case_control_pcr <- case_control_full[ which(case_control_full$det_meth_2 == "PCR"), ]
  case_control_no_pcr <- case_control_full[ which(case_control_full$det_meth_2!= "PCR"), ]
  
  
  ### prep dataframes ###
    
  # each pathogen & strain- PCR only
    # pathogens
      pathogen_data_pcr_list <- unique(case_control_pcr$path)
        for (v in pathogen_data_pcr_list) {
        assign(paste0("data_pcr_", as.character(v)), case_control_pcr %>% filter (path == v), envir = .GlobalEnv)
        } 
    # pathogen strains
      strain_data_pcr_list <- unique(case_control_pcr$path_strain)
        for (s in strain_data_pcr_list) {
        assign(paste0("data_pcr_", as.character(s)), case_control_pcr %>% filter (path_strain == s), envir = .GlobalEnv)
        } 
    # rota pre/post
      data_pcr_rota_pre  <- case_control_pcr[ which(case_control_pcr$path == "rota" & case_control_pcr$rota_vax_lag == 0),]
      data_pcr_rota_post <- case_control_pcr[ which(case_control_pcr$path == "rota" & case_control_pcr$rota_vax_lag == 1),]
    
        
  # each pathogen & strain- no PCR only
    # pathogens
      pathogen_data_no_pcr_list <- unique(case_control_no_pcr$path)
        for (v in pathogen_data_no_pcr_list) {
        assign(paste0("data_no_pcr_", as.character(v)), case_control_no_pcr %>% filter (path == v), envir = .GlobalEnv)
        } 
    # pathogen strains
      strain_data_no_pcr_list <- unique(case_control_no_pcr$path_strain)
        for (s in strain_data_no_pcr_list) {
        assign(paste0("data_no_pcr_", as.character(s)), case_control_no_pcr %>% filter (path_strain == s), envir = .GlobalEnv)
        } 
    # rota pre/post
      data_no_pcr_rota_pre  <- case_control_no_pcr[ which(case_control_no_pcr$path == "rota" & case_control_no_pcr$rota_vax_lag == 0),]
      data_no_pcr_rota_post <- case_control_no_pcr[ which(case_control_no_pcr$path == "rota" & case_control_no_pcr$rota_vax_lag == 1),]

      
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
                             'data_pcr_rota_pre', 'data_pcr_rota_post',
                             'data_pcr_noro_gi', 'data_pcr_noro_gii', 'data_pcr_rota_a', 
                             'data_pcr_campy_coli', 'data_pcr_campy_jejuni',
                             'data_pcr_epec_a', 'data_pcr_epec_t', 'data_pcr_epec_unkn',
                             'data_pcr_etec_st', 'data_pcr_etec_lt', 'data_pcr_etec_unkn',
                             'data_pcr_salm_nts', 
                             'data_pcr_crypto_parvum', 'data_pcr_ehist_coli', 'data_pcr_ehist_dispar', 'data_pcr_ehist_histolytica', 'data_pcr_giard_l_i_d')
    
  # labels for results
    data_pcr_labels_unadj <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                               'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                               'crypto', 'ehist', 'giard', 
                               'rota_pre', 'rota_post',
                               'noro_gi', 'noro_gii', 'rota_a', 
                               'campy_coli', 'campy_jejuni', 
                               'epec_a', 'epec_t', 'epec_unkn',
                               'etec_st', 'etec_lt', 'etec_unkn',
                               'salm_nts', 
                               'crypto_parvum', 'ehist_coli', 'ehist_dispar', 'ehist_histolytica', 'giard_l_i_d')
    
  # model
    ma_path_pcr_unadj <- list()
    for(i in seq_along(data_pcr_list_unadj)){ 
      ma_path_pcr_unadj[[i]] <- meta_analysis_loop_unadj(get(data_pcr_list_unadj[i]))
    }  
  # examine results  
    for (i in 1:34){
      print(ma_path_pcr_unadj[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:34) {
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
    data_no_pcr_list_unadj <- c('data_no_pcr_adeno', 'data_no_pcr_astro', 'data_no_pcr_noro', 'data_no_pcr_rota', 
                                'data_no_pcr_aero', 'data_no_pcr_campy', 'data_no_pcr_chol', 'data_no_pcr_epec',  'data_no_pcr_etec', 'data_no_pcr_salm', 'data_no_pcr_shig', 
                                'data_no_pcr_crypto', 'data_no_pcr_ehist', 'data_no_pcr_giard', 
                                'data_no_pcr_rota_pre', 'data_no_pcr_rota_post',
                                'data_no_pcr_noro_gi', 'data_no_pcr_noro_gii', 
                                'data_no_pcr_aero_hydrophila', 'data_no_pcr_campy_coli', 'data_no_pcr_campy_jejuni', 'data_no_pcr_campy_lari',
                                'data_no_pcr_chol_o1', 'data_no_pcr_chol_o139', 
                                'data_no_pcr_epec_unkn', 'data_no_pcr_etec_st', 'data_no_pcr_etec_lt', 'data_no_pcr_etec_unkn',
                                'data_no_pcr_salm_nts', 'data_no_pcr_salm_paratyphi',
                                'data_no_pcr_shig_a', 'data_no_pcr_shig_c', 'data_no_pcr_shig_d', 'data_no_pcr_crypto_parvum',
                                'data_no_pcr_ehist_coli', 'data_no_pcr_ehist_dispar', 'data_no_pcr_ehist_histolytica', 'data_no_pcr_giard_l_i_d')
  # labels for results
    data_no_pcr_labels_unadj <- c('adeno', 'astro', 'noro', 'rota', 
                                  'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                                  'crypto', 'ehist', 'giard', 
                                  'rota_pre', 'rota_post',
                                  'noro_gi', 'noro_gii', 
                                  'aero_hydrophila', 'campy_coli', 'campy_jejuni', 'campy_lari',
                                  'chol_o1', 'chol_o139', 'epec_unkn', 'etec_st', 'etec_lt', 'etec_unkn',
                                  'salm_nts', 'salm_paratyphi',
                                  'shig_a', 'shig_c', 'shig_d', 'crypto_parvum',
                                  'ehist_coli', 'ehist_dispar', 'ehist_histolytica', 'giard_l_i_d')
  # model
    ma_path_no_pcr_unadj <- list()
    for(i in seq_along(data_no_pcr_list_unadj)){ 
      ma_path_no_pcr_unadj[[i]] <- meta_analysis_loop_unadj(get(data_no_pcr_list_unadj[i]))
    }  
  # examine results  
    for (i in 1:38){
      print(ma_path_no_pcr_unadj[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:38) {
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
  
    
  

  
#########################################
####          MODEL PLOTS            ####
#########################################    
  
  
  # midpoint decimals (for Lancet formatting)
    options(OutDec="·")
    options(digits=1)
  # renaming dataset for Lancet and filtering so we only have main pathogens (not strains) and main results of interest
    ma_new2_combined_results_lancet <- ma_new2_combined_results %>% filter(names == 'adeno' | names == 'astro' |  names == 'noro' |
                                                                           names == 'rota' | names == 'sapo' | names == 'aero' |
                                                                           names == 'campy' | names == 'chol' | 
                                                                           names == 'epec_a' | names == 'epec_t' | names == 'epec_unkn' |
                                                                           names == 'etec_st' | names == 'etec_lt' | names == 'etec_unkn' |
                                                                           names == 'salm' | names == 'shig' |
                                                                           names == 'crypto' | names == 'ehist' | names == 'giard' |
                                                                           names == 'rota_pre' | names == 'rota_post')
    ma_new2_combined_results_lancet <- ma_new2_combined_results_lancet %>% filter(group == 'unadj' |
                                                                                  group == 'youngest, high mortality' |
                                                                                  group == 'youngest, vlow & low mortality' |
                                                                                  group == 'middle, high mortality' |
                                                                                  group == 'middle, vlow & low mortality' |
                                                                                  group == 'old, all mortality levels')
  
    options(OutDec=".")
    
  # formatting
    ma_new2_combined_results_lancet$coeff2  <- format(round(ma_new2_combined_results_lancet$coeff, 1), nsmall=1, trim=TRUE)
    ma_new2_combined_results_lancet$lci2    <- format(round(ma_new2_combined_results_lancet$lci, 1), nsmall=1, trim=TRUE)
    ma_new2_combined_results_lancet$uci2    <- format(round(ma_new2_combined_results_lancet$uci, 1), nsmall=1, trim=TRUE)
  # creating variable with effect estimate and CI for pathogen
    ma_new2_combined_results_lancet$part1  <- paste(ma_new2_combined_results_lancet$coeff2, ma_new2_combined_results_lancet$lci2, sep=" (")
    ma_new2_combined_results_lancet$part2  <- paste(ma_new2_combined_results_lancet$part1, ma_new2_combined_results_lancet$uci2, sep=", ")
    ma_new2_combined_results_lancet$part3  <- ")"
    ma_new2_combined_results_lancet$ee_ci  <- paste(ma_new2_combined_results_lancet$part2, ma_new2_combined_results_lancet$part3, sep="")
  # cleaning/dropping unnecessary variables
    ma_new2_combined_results_lancet$part1 <- NULL
    ma_new2_combined_results_lancet$part2 <- NULL
    ma_new2_combined_results_lancet$part3 <- NULL
  
    
  # format order of groups
    ma_new2_combined_results_lancet$group_ordered <- factor(ma_new2_combined_results_lancet$group, 
                                                     levels = c("unadj", 
                                                                "youngest, vlow & low mortality", "youngest, high mortality", 
                                                                "middle, vlow & low mortality", "middle, high mortality",
                                                                "old, all mortality levels"))
  
    ma_new2_combined_results_lancet$path_ordered <- factor(ma_new2_combined_results_lancet$names, 
                                                    levels =c('adeno', 'astro', 'noro',  'rota', 'rota_pre', 'rota_post', 'sapo',
                                                              'aero', 'campy', 'chol', 
                                                              'epec_a', 'epec_t', 'epec_unkn',
                                                              'etec_st', 'etec_lt', 'etec_unkn', 
                                                              'salm', 'shig', 
                                                              'crypto', 'ehist', 'giard'))
    
    pathogen_names <- list(
      'adeno'='Adenovirus 40/41', 
      'astro'='Astrovirus', 
      'noro' ='Norovirus', 
      'rota'='Rotavirus', 
      'rota_pre'='Rotavirus (pre-vaccine)', 
      'rota_post'='Rotavirus (post-vaccine)', 
      'sapo'='Sapovirus',
      'aero'='Aeromonas', 
      'campy'='Campylobacter', 
      'chol'='V. cholerae', 
      'epec_a'='aEPEC', 
      'epec_t'='tEPEC', 
      'epec_unkn'='EPEC (unknown subtype)', 
      'etec_st'='ST ETEC',  
      'etec_lt'='LT ETEC', 
      'etec_unkn'='ETEC (unknown subtype)', 
      'salm'='Salmonella', 
      'shig'='Shigella', 
      'crypto'='Cryptosporidium', 
      'ehist'='E. histolytica', 
      'giard'='Giardia lamblia')
    
    pathogen_labeller <- function(variable,value){
      return(pathogen_names[value])
    }
 
    
  # dropping the one observation for cholera that has huge CIs (messes up figure)
    ma_new2_combined_results_lancet$coeff <- ifelse((ma_new2_combined_results_lancet$names=='chol' & 
                                                     ma_new2_combined_results_lancet$group=='old, all mortality levels'), NA, ma_new2_combined_results_lancet$coeff)
    ma_new2_combined_results_lancet$lci   <- ifelse((ma_new2_combined_results_lancet$names=='chol' & 
                                                     ma_new2_combined_results_lancet$group=='old, all mortality levels'), NA, ma_new2_combined_results_lancet$lci)
    ma_new2_combined_results_lancet$uci   <- ifelse((ma_new2_combined_results_lancet$names=='chol' & 
                                                     ma_new2_combined_results_lancet$group=='old, all mortality levels'), NA, ma_new2_combined_results_lancet$uci)
    ma_new2_combined_results_lancet$ee_ci <- ifelse((ma_new2_combined_results_lancet$names=='chol' & 
                                                     ma_new2_combined_results_lancet$group=='old, all mortality levels'), NA, ma_new2_combined_results_lancet$ee_ci)
    
    view(ma_new2_combined_results_lancet)
    
    
  print()
    ggplot(ma_new2_combined_results_lancet) +
        geom_pointrange(aes(x=group_ordered, y=coeff, ymin=lci, ymax=uci, color=group_ordered),
                        position = position_dodge(width = 1), size=.5) +
        scale_y_log10() +
        scale_color_discrete(name="Model", 
                             labels = c("Unadjusted and unstratified", 
                                        "0-1 year, very low/low child mortality settings", 
                                        "0-1 year, high child mortality settings", 
                                        "2-4 years, very low/low child mortality settings", 
                                        "2-4 years, high child mortality settings", 
                                        ">=5 years, all child mortality settings")) +
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
    
      ggsave("path_ORs_wrap_6.eps", width = 8.5, height = 11, units = "in")

      
                    
#########################################
####          FOREST PLOTS           ####
#########################################

    install.packages("extrafont")
    library(extrafont)
    font_import()

   
    data_labels_unadj <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                           'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                           'crypto', 'ehist', 'giard', 
                           'rota_pre', 'rota_post',
                           'noro_gi', 'noro_gii', 'rota_a', 
                           'aero_hydrophila', 'campy_coli', 'campy_jejuni', 'campy_lari', 'chol_o1', 'chol_o139',
                           'epec_a', 'epec_t', 'epec_unkn', 'etec_st', 'etec_lt', 'etec_unkn', 
                           'salm_nts', 'salm_paratyphi', 'shig_a', 'shig_c', 'shig_d',
                           'crypto_parvum', 'ehist_coli', 'ehist_dispar', 'ehist_histolytica', 'giard_l_i_d')
    
 
 pdf(file='Forest plots- all-test2.pdf', width=8.5, height=11)
 for (i in 1:32){
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
 
 