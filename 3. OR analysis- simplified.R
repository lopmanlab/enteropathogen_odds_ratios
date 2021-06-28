#######################################################################
#
# WHO Burden of Diseases OR analysis- Lit Review Data
# Data analysis
# Programer: Julia Baker
# Updated: Jun 24, 2021 for Lancet GH
#   
#######################################################################

.libPaths()
.Library


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




#########################################
####            DATA PREP            ####
#########################################

  
# set working directory
  # getwd()
  setwd("/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/Julia")

# load case-control lit review data (using original data file)
  case_control_original <- read_csv("/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/2_Database_R/Data_original/EntericPathogensCase_DATA_2019-12-16_0727.csv")
  #1814 observations, 243 variables
  
  # extract article details and strata details in preparation for merging together
    article_details <- case_control_original %>% filter(is.na(redcap_repeat_instrument)) %>% select(1,4:78)
    strata_details  <- case_control_original %>% filter(redcap_repeat_instrument == "pathogen_strata") %>% select(1,3,79:243)
  # merge article and strata details by ID
    case_control <- merge(article_details, strata_details, by="id", all.x=TRUE, all.y=TRUE)
  
  # check number of unique studies
    case_control$author_year_title <- paste(case_control$author, case_control$year_pub, case_control$title, sep='-')
      length(unique(case_control$author_year_title, exclude=NULL))
      # 172 studies (but includes some errors-- see cleaning below)

      
      
#########################################
####      CLEANING/FORMATTING        ####
#########################################


# clean country names
  case_control$country[case_control$country == "Netherlands"] <- "The Netherlands"
  case_control$country[case_control$country == "U.S."] <- "United States"
  case_control$country[case_control$country == "USA"] <- "United States"  
  
# clean author names
  case_control$author[case_control$author == "Bueris V."] <- "Bueris"
  case_control$author[case_control$author == "Meyer, Caroline T."] <- "Meyer"
  case_control$author[case_control$author == "Zhao Feng Ming"] <- "Ming"
  case_control$author[case_control$author == "Breurec, Sébastien"] <- "Breurec"
  case_control$author[case_control$author == "Afset, Jane"] <- "Afset"
  case_control$author[case_control$author == "Oralak Serichantalergs"] <- "Serichantalergs"
  case_control$author[case_control$author == "Mansour, AM"] <- "Mansour"
  case_control$author[case_control$author == "Adel M. Mansour"] <- "Mansour"
  case_control$author[case_control$author == "Yori, PP"] <- "Yori"
  case_control$author[case_control$author == "Yvonne Maldonado"] <- "Maldonado"
  case_control$author[case_control$author == "Britney Neitenbach"] <- "Neitenbach"
  case_control$author[case_control$author == "Gwenyth Lee"] <- "Lee"
  case_control$author[case_control$author == "Carl J. Mason"] <- "Mason"
  case_control$author[case_control$author == "Teshome Firdu"] <- "Firdu"
  case_control$author[case_control$author == "Nadia Vieira"] <- "Vieira"
  case_control$author[case_control$author == "Lima, A"] <- "Lima"
  case_control$author[case_control$author == "Levine, MM"] <- "Levine"
  case_control$author[case_control$author == "GASCON"] <- "Gascon"
  case_control$author[case_control$author == "ESPINOZA"] <- "Espinoza"
  case_control$author[case_control$author == "ISENBARGER"] <- "Isenbarger"
  case_control$author[case_control$author == "Farfan-Garcia, AE"] <- "Farfan-Garcia"
  case_control$author[case_control$author == "Iturriza-Gómara, M"] <- "Iturriza-Gómara"
  case_control$author[case_control$author == "Firdausi Qadri"] <- "Qadri"
  case_control$author[case_control$author == "G. Pazzaglia"] <- "Pazzaglia"
  case_control$author[case_control$author == "Randremanana, RV"] <- "Randremanana"
  case_control$author[case_control$author == "Prativa Pandey"] <- "Pandey"
  case_control$author[case_control$author == "Randremanana, R"] <- "Randremanana"
  case_control$author[case_control$author == "Rebecca Oketcho"] <- "Oketcho"
  case_control$author[case_control$author == "Satarupa Mullick"] <- "Mullick"
  case_control$author[case_control$author == "O'Ryan, ML"] <- "O'Ryan"
  case_control$author[case_control$author == "Newman, RD"] <- "Newman"
  case_control$author[case_control$author == "M.C. Georges-Courbot"] <- "Georges-Courbot"
  case_control$author[case_control$author == "Thea K.  Fischer"] <- "Fischer"
  case_control$author[case_control$author == "Xin-xin Shen"] <- "Shen"
  case_control$author[case_control$author == "Lori R. Holtz"] <- "Holtz"
  case_control$author[case_control$author == "Guodong D. Fang"] <- "Fang"
  case_control$author[case_control$author == "CHRISTIANE FORESTIER"] <- "Forestier"
  case_control$author[case_control$author == "Bui Thi Thu Hien"] <- "Hien"
  case_control$author[case_control$author == "SCHORLING"] <- "Schorling"
  case_control$author[case_control$author == "SCALETSKY"] <- "Scaletsky"
  case_control$author[case_control$author == "PORAT, NURITH PHD"] <- "Porat"
  case_control$author[case_control$author == "Juliette Pavie"] <- "Pavie"
  case_control$author[case_control$author == "SALLON"] <- "Sallon"
  case_control$author[case_control$author == "Brett E. Swierczewski"] <- "Swierczewski"
  case_control$author[case_control$author == "Muhsen, K"] <- "Muhsen"
  case_control$author[case_control$author == "Guerrero, L"] <- "Guerrero"
  case_control$author[case_control$author == "Gomez-Duarte, OG"] <- "Gomez-Duarte"
  case_control$author[case_control$author == "Fraser, D"] <- "Fraser"
  case_control$author[case_control$author == "MOHAMMAD MEHDI SOLTAN DALLAL"] <- "Soltan Dallal"
  case_control$author[case_control$author == "Daniel Reyes"] <- "Reyes"
  case_control$author[case_control$author == "El-Hakim, MA"] <- "El-Hakim"
  case_control$author[case_control$author == "Eibach, D"] <- "Eibach"
  case_control$author[case_control$author == "Sen Gupta, PG"] <- "Sen Gupta"
  case_control$author[case_control$author == "Scaletsky, ICA"] <- "Scaletsky"
  case_control$author[case_control$author == "Samuel, S"] <- "Samuel"
  case_control$author[case_control$author == "Guodong D. Fang"] <- "Fang"
  case_control$author[case_control$author == "Hans Steinsland"] <- "Steinsland"
  case_control$author[case_control$author == "Hendricks, MK"] <- "Hendricks"
  case_control$author[case_control$author == "Santos, AK"] <- "Santos"
  case_control$author[case_control$author == "Vasco, G"] <- "Vasco"
  case_control$author[case_control$author == "SCHULTSZ"] <- "Schultsz"
  case_control$author[case_control$author == "Tumwine, J"] <- "Tumwine"
  case_control$author[case_control$author == "Samba O. Sow"] <- "Sow"
  case_control$author[case_control$author == "Tellevik, MG"] <- "Tellevik"
  case_control$author[case_control$author == "Taniuchi, M"] <- "Taniuchi"
  case_control$author[case_control$author == "Kattula, D"] <- "Kattula"
  case_control$author[case_control$author == "Lindsay, B"] <- "Lindsay"
  
  
# clean study titles
  case_control$title[case_control$title == "Role of Protozoa as Risk Factors for Persistent Diarrhea"] <- 
                                           "Role of protozoa as risk factors for persistent diarrhea"
  case_control$title[case_control$title == "ASSOCIACAO DE PADROES DE ADESAO DE Escherichia coli As CELULAS HEp-2 COM DIARREIA  AGUDA E PERSISTENTE"] <-
                                           "ASSOCIACAO DE PADROES DE ADESAO DE Escherichia coli As CELULAS HEp-2 COM DIARREIA AGUDA E PERSISTENTE"
                       
  
# re-create unique ID using (corrected) author, pub year and (corrected) title
  case_control$author_year_title <- paste(case_control$author, case_control$year_pub, case_control$title, sep='-')
    length(unique(case_control$author_year_title, exclude=NULL))
  # 170 unique studies

# create unique ID number for each study and strata
  case_control$ID_uniq_n <- group_indices(case_control, author_year_title)    
    
    
# variable indicating whether or not to include study in analysis
  case_control$include <- 1
   
  # exclude 1 study with incomplete author/article information
    case_control$include <- ifelse(case_control$id == 53, 0, case_control$include)
      # check number of unique studies
        include <- case_control %>% filter(include==1)
        length(unique(include$author_year_title, exclude=NULL))
        # 169 studies
        
  # check GEMS/MAL-ED variable 
    gems_maled_list <- data.frame(case_control %>% filter(gems_maled == 1 | gems_maled == 2) %>% dplyr::select(id, author, year_pub, country, title, gems_maled, author_year_title))
    # correcting GEMS/MAL-ED code in main dataset for those that are not actually part of either study (first assuming NAs are not part of either)
      case_control$gems_maled <- ifelse(is.na(case_control$gems_maled), 0, case_control$gems_maled)
      case_control$gems_maled <- ifelse((case_control$author == "Becker" & case_control$year_pub == 2015 & case_control$country == "Côte d'Ivoire"), 0,
                                 ifelse((case_control$author == "Zaidi" & case_control$year_pub == 2012 & case_control$country == "Mexico"), 0,
                                 ifelse((case_control$author == "Schultsz" & case_control$year_pub == 2000 & case_control$country == "The Netherlands"), 0, 
                                        case_control$gems_maled)))
    # adding study that actually is part of MAL-ED but was previously marked as not
      case_control$gems_maled <- ifelse((case_control$author == "Francois" & case_control$year_pub == 2018 & case_control$country == "Peru"), 1, case_control$gems_maled)
    
      
    # exclude GEMS & MAL-ED observations b/c we are pulling ORs from James' data BUT include the 2 studies below that are an extension/modified version of GEMS/MAL-ED
      # Mullick 2014 is an extension of MAL-ED data (includes hospitalized cases in addition to MAL-ED cohort so including in the study)
      # Nelson 2018 is a subset of MAL-ED data and looked at strain-specific noro
      # Francois 2018 is part of MAL-ED but specifically looks at dysentary so keeping in
      case_control$include <- ifelse((case_control$author == "Mullick" & case_control$year_pub == 2014 & case_control$country == "India"), 1,
                              ifelse((case_control$author == "Nelson" & case_control$year_pub == 2018 & case_control$country == "Bangladesh"), 1,     
                              ifelse((case_control$author == "Francois" & case_control$year_pub == 2018 & case_control$country == "Peru"), 1,  
                              ifelse((case_control$gems_maled == 1 | case_control$gems_maled == 2), 0, case_control$include))))
      # 11 studies excluded b/c part of GEMS/MAL-ED
      # check number of unique studies
        include <- case_control %>% filter(include==1)
        length(unique(include$author_year_title, exclude=NULL))
        # 158 studies

        
        
    # exclude studies with identified overlap with other data in dataset (see "case_control_full_unique- checking overlap" spreadsheet for detailed notes)       
      # Drop Qui 2018, Jensen 2007, Eibach 2015, Steinsland 2002 (only drop LT-etec observation), Porat 1998, Ochoa 2009 ("Age related susceptibility..."), 
      # Mayo 2014, de Wit 2001 ("Gastroenteritis in sentinel...")
      case_control$include <- ifelse((case_control$author == "Qiu" & case_control$year_pub == 2018 & case_control$country == "China"), 0,
                              ifelse((case_control$author == "Jensen" & case_control$year_pub == 2007 & case_control$country == "Denmark"), 0, 
                              ifelse((case_control$author == "Eibach" & case_control$year_pub == 2015 & case_control$country == "Ghana"), 0, 
                              ifelse((case_control$author == "Steinsland" & case_control$year_pub == 2002 & case_control$country == "Guinea-Bissau" & 
                                      case_control$etec_strain___1 == 1 & case_control$etec_strain___2 == 0), 0,
                              ifelse((case_control$author == "Porat" & case_control$year_pub == 1998 & case_control$country == "Israel"), 0,
                              ifelse((case_control$author == "Ochoa" & case_control$year_pub == 2009 & case_control$country == "Peru" &
                                      case_control$title == "Age-Related Susceptibility to Infection with Diarrheagenic Escherichia coli among Infants from Periurban Areas in Lima, Peru"), 0,
                              ifelse((case_control$author == "Mayo" & case_control$year_pub == 2014 & case_control$country == "Tanzania"), 0,
                              ifelse((case_control$author == "de Wit" & case_control$year_pub == 2001 & case_control$country == "The Netherlands" &
                                      case_control$title == "Gastroenteritis in Sentinel General Practices, the Netherlands"), 0, case_control$include))))))))
      # check number of unique studies
        include <- case_control %>% filter(include==1)
        length(unique(include$author_year_title, exclude=NULL))
        # 151 studies

        
## rename variables for ease/consistency (new name <- old name)
  
  # pathogen(s) detected in stratum (can have multiple pathogens indicated in one stratum)
    case_control <- case_control %>% 
                    dplyr::rename(salm   = pathogen___1,
                                  shig   = pathogen___2,
                                  campy  = pathogen___3,
                                  chol   = pathogen___4,
                                  etec   = pathogen___5,
                                  epec   = pathogen___6,
                                  rota   = pathogen___7,
                                  noro   = pathogen___8,
                                  sapo   = pathogen___9,
                                  astro  = pathogen___10,
                                  adeno  = pathogen___11,
                                  giard  = pathogen___12,
                                  crypto = pathogen___13,
                                  ehist  = pathogen___14,
                                  aero   = pathogen___15,
                                  other  = pathogen___88,
                                  unkn   = pathogen___99)

    pathogen <- c('salm', 'shig', 'campy', 'chol', 'etec', 'epec', 'rota', 'noro',
                  'sapo', 'astro', 'adeno', 'giard', 'crypto', 'ehist', 'aero', 'other')

    
    # some pathogen stratum (rows) have multiple pathogens listed. 
    # identifying rows with multiple pathogens listed (counting number of pathogens present)
      case_control$path_num <- case_control$salm + case_control$shig + case_control$campy + case_control$chol + case_control$etec +
                               case_control$epec + case_control$rota + case_control$noro + case_control$sapo + case_control$astro + 
                               case_control$adeno + case_control$giard + case_control$crypto + case_control$ehist + case_control$aero + 
                               case_control$other + case_control$unkn
      table(case_control$path_num, exclude = NULL)
      # 1530 w/ only 1 pathogen; 71 w/ 2 pathogens; 7 w/ 3 pathogens
      # 9 w/ 0 pathogens; 7 NA
      
      # exclude rows w/ 0 or NA for pathogen (set NAs to 0 first)
        case_control$path_num <- ifelse(is.na(case_control$path_num), 0, case_control$path_num)
        case_control$include <- ifelse(case_control$path_num == 0, 0, case_control$include)
        # 5 studies dropped b/c no data on pathogens detected (plus some subsets of other studies)
        # check number of unique studies
          include <- case_control %>% filter(include==1)
          length(unique(include$author_year_title, exclude=NULL))
          # 146

     
      # duplicating rows with multiple pathogens so that I can include an effect estimate for each pathogen included (will be the same for each pathogen in that stratum)
        # duplicate rows will be marked with the "dup_num" variable-- value of 2 or 3 (1 is original row)
        case_control <- case_control %>% uncount(path_num, .id = "dup_num") 
        # create variable indicating number of pathogens again
          case_control$path_num <- case_control$salm + case_control$shig + case_control$campy + case_control$chol + case_control$etec +
                                   case_control$epec + case_control$rota + case_control$noro + case_control$sapo + case_control$astro + 
                                   case_control$adeno + case_control$giard + case_control$crypto + case_control$ehist + case_control$aero + 
                                   case_control$other + case_control$unkn
        
      
  # measures of association (new name <- old name)
    case_control <- case_control %>% 
    dplyr::rename(or_unadj_yn  = odds_ratio,
                  or_unadj     = odds_ratio_value_unadj,
                  or_unadj_lci = or_unadj_ci_lower,
                  or_unadj_uci = or_unadj_ci_upper,
                  or_adj_yn    = odds_ratio_adj_yn,
                  or_adj       = odds_ratio_value_adj,
                  or_adj_lci   = or_adj_ci_lower,
                  or_adj_uci   = or_adj_ci_upper,
                  rr_unadj_yn  = relative_risk,
                  rr_unadj     = relative_risk_value,
                  rr_unadj_lci = rr_unadj_ci_lower,
                  rr_unadj_uci = rr_unadj_ci_upper,
                  rr_adj_yn    = relative_risk_adj_yn,
                  rr_adj       = relative_risk_value_adj,
                  rr_adj_lci   = rr_adj_ci_lower,
                  rr_adj_uci   = rr_adj_ci_upper,
                  ir           = incidence_rate,
                  ir_lci       = ci_lower_ir,
                  ir_uci       = ci_upper_ir)
    
      
### creating helpful variables ###

  # for only/first record (dup_num = 1) creating variable indicating pathogen identified for this stratum
    case_control$path_1 <- ifelse((case_control$dup_num == 1 & case_control$salm == 1), "salm",
                           ifelse((case_control$dup_num == 1 & case_control$shig == 1), "shig",
                           ifelse((case_control$dup_num == 1 & case_control$campy == 1), "campy",
                           ifelse((case_control$dup_num == 1 & case_control$chol == 1), "chol",
                           ifelse((case_control$dup_num == 1 & case_control$etec == 1), "etec",
                           ifelse((case_control$dup_num == 1 & case_control$epec == 1), "epec",
                           ifelse((case_control$dup_num == 1 & case_control$rota == 1), "rota",
                           ifelse((case_control$dup_num == 1 & case_control$noro == 1), "noro",
                           ifelse((case_control$dup_num == 1 & case_control$sapo == 1), "sapo",
                           ifelse((case_control$dup_num == 1 & case_control$astro == 1), "astro",
                           ifelse((case_control$dup_num == 1 & case_control$adeno == 1), "adeno",
                           ifelse((case_control$dup_num == 1 & case_control$giard == 1), "giard",
                           ifelse((case_control$dup_num == 1 & case_control$crypto == 1), "crypto",
                           ifelse((case_control$dup_num == 1 & case_control$ehist == 1), "ehist",
                           ifelse((case_control$dup_num == 1 & case_control$aero == 1), "aero",
                           ifelse((case_control$dup_num == 1 & case_control$other == 1), "other", NA))))))))))))))))
    
    # for those with 1 duplicate record (dup_num = 2), selecting the last (2nd of 2 or 3rd of 3) pathogen detected in that row
      # basically flipping the above ifelse statements so it picks backwards
      case_control$path_2 <- ifelse((case_control$dup_num == 2 & case_control$other == 1), "other",
                             ifelse((case_control$dup_num == 2 & case_control$aero == 1), "aero",
                             ifelse((case_control$dup_num == 2 & case_control$ehist == 1), "ehist",
                             ifelse((case_control$dup_num == 2 & case_control$crypto == 1), "crypto",
                             ifelse((case_control$dup_num == 2 & case_control$giard == 1), "giard",
                             ifelse((case_control$dup_num == 2 & case_control$adeno == 1), "adeno",
                             ifelse((case_control$dup_num == 2 & case_control$astro == 1), "astro",
                             ifelse((case_control$dup_num == 2 & case_control$sapo == 1), "sapo",
                             ifelse((case_control$dup_num == 2 & case_control$noro == 1), "noro",
                             ifelse((case_control$dup_num == 2 & case_control$rota == 1), "rota",
                             ifelse((case_control$dup_num == 2 & case_control$epec == 1), "epec",
                             ifelse((case_control$dup_num == 2 & case_control$etec == 1), "etec",
                             ifelse((case_control$dup_num == 2 & case_control$chol == 1), "chol",
                             ifelse((case_control$dup_num == 2 & case_control$campy == 1), "campy",
                             ifelse((case_control$dup_num == 2 & case_control$shig == 1), "shig",
                             ifelse((case_control$dup_num == 2 & case_control$salm == 1), "salm", NA))))))))))))))))

    # for those with 2 duplicate records (dup_num = 3), selecting the middle pathogen detected in that row manually
      case_control$path_3 <- ifelse((case_control$dup_num == 3 & case_control$redcap_repeat_instance == 12), "rota",
                             ifelse((case_control$dup_num == 3 & case_control$redcap_repeat_instance == 13), "rota",
                             ifelse((case_control$dup_num == 3 & case_control$redcap_repeat_instance == 18), "rota",
                             ifelse((case_control$dup_num == 3 & case_control$redcap_repeat_instance == 19 & case_control$ID_uniq_n == 172), "giard",
                             ifelse((case_control$dup_num == 3 & case_control$redcap_repeat_instance == 22), "ehist",
                             ifelse((case_control$dup_num == 3 & case_control$redcap_repeat_instance == 15), "giard",
                             ifelse((case_control$dup_num == 3 & case_control$redcap_repeat_instance == 19 & case_control$ID_uniq_n == 173), "giard", NA)))))))
     
    # combining path 1,2,3 into one variable
      case_control$path <- ifelse(case_control$dup_num == 1, case_control$path_1,
                           ifelse(case_control$dup_num == 2, case_control$path_2,  
                           ifelse(case_control$dup_num == 3, case_control$path_3, NA)))
      case_control$path <- factor(case_control$path)
      table(case_control$path, exclude = NULL)
      # 30 "NA" which are "unknown" pathogens
      # deleting unneeded variables
        case_control$path_1 <- NULL
        case_control$path_2 <- NULL
        case_control$path_3 <- NULL
          
    # exclude studies where only data are for "other" pathogen only or "unknown" (NA) 
      case_control$path <- ifelse((is.na(case_control$path)), "unknown", as.character(case_control$path))
      case_control$include <- ifelse((case_control$path == "other" | case_control$path == "unknown"), 0, case_control$include) 
      # check number of unique studies
        include <- case_control %>% filter(include==1)
        length(unique(include$author_year_title))
        # 146 studies (none dropped entirely, just some observations/strata within studies)
        
        
  # y/n variable for IR
    case_control$ir_yn <- ifelse(case_control$ir >= 0, 1, 0) #all with IR have IRs > 1

  
### merging with additional data ###
  
  # MAL-ED/GEMS data 
    load("/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/Julia/Data/gems_maled_wide.rdata")
    # format
      gems_maled_wide$title <- gems_maled_wide$author
      gems_maled_wide$author <- "Platts-Mills"
      gems_maled_wide$include <- 1
      gems_maled_wide$author_year_title <- paste(gems_maled_wide$author, "NA", gems_maled_wide$title, sep="-")
    # merge MAL-ED/GEMS data with main dataset
      case_control <- bind_rows(case_control, gems_maled_wide)
      # make sure "path" is still a factor
        case_control$path <- factor(case_control$path)
      # check number of unique studies
        include <- case_control %>% filter(include==1)
        length(unique(include$author_year_title, exclude=NULL))
        # 148 studies
    
        
  # U5MR data
    load("/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/Julia/Data/u5mr_prepped.rdata")
    case_control <- merge(case_control, u5mr_prepped, all.x=TRUE)
  
    # Côte d'Ivoire and People's Republic of China won't match to U5MR dataset so adding data manually
      case_control$u5mr[case_control$country == "Côte d'Ivoire"] <- 134.551614  
      case_control$u5mr[case_control$country == "People's Republic of China"] <- 28.860378

  # rota vax intro status (from WHO)
    rota_vax_intro <- read_excel("/Users/juliabaker/OneDrive - Emory University/Julia Baker/Rota Post-Vax Review/WHO RVSN Data Request/Data from WHO/ref_vaccine_introduction.xls",
                                 sheet = "Rotavirus")
    # limit to needed variables and format
      rota_vax_intro <- rota_vax_intro %>% select("Country", "Year of introduction in entire country", "WHO Region")
      names(rota_vax_intro)[names(rota_vax_intro) == "Year of introduction in entire country"] <- "rota_vax_yr"
      names(rota_vax_intro)[names(rota_vax_intro) == "Country"] <- "country"
      names(rota_vax_intro)[names(rota_vax_intro) == "WHO Region"] <- "who_region"
    # merge
      case_control <- merge(case_control, rota_vax_intro, all.x=TRUE)
    # check
      table(case_control$rota_vax_yr, exclude=NULL)
      case_control_missing_rota <- case_control %>% filter(path=="rota" & is.na(rota_vax_yr))
    # manually add missing vax intro year (b/c of differences in country name)
      case_control$rota_vax_yr[case_control$country == "Central African Republic"] <- 2020
      case_control$rota_vax_yr[case_control$country == "Tanzania"] <- 2013
      case_control$rota_vax_yr[case_control$country == "The Gambia"] <- 2013
      case_control$rota_vax_yr[case_control$country == "United States"] <- 2006
      case_control$rota_vax_yr[case_control$country == "Vietnam"] <- 2021
      case_control$rota_vax_yr[case_control$country == "People's Republic of China"] <- 2040
      case_control$rota_vax_yr[case_control$country == "The Netherlands"] <- 2040
    # setting "n/a" to some future date b/c "n/a" means it hasn't been introduced nationally yet
      case_control$rota_vax_yr <- ifelse(case_control$rota_vax_yr == "n/a", 2040, case_control$rota_vax_yr)
      # make numeric
        case_control$rota_vax_yr <- as.numeric(case_control$rota_vax_yr)
    # manually add missing regions (b/c of differences in country name)
        case_control$who_region[case_control$country == "Central African Republic"] <- 'AFRO'
        case_control$who_region[case_control$country == "Tanzania"] <- 'AFRO'
        case_control$who_region[case_control$country == "The Gambia"] <- 'AFRO'
        case_control$who_region[case_control$country == "United States"] <- 'AMRO'
        case_control$who_region[case_control$country == "Vietnam"] <- 'WPRO'
        case_control$who_region[case_control$country == "People's Republic of China"] <- 'WPRO'
        case_control$who_region[case_control$country == "The Netherlands"] <- 'EURO'   
        case_control$who_region[case_control$country == "Iran"] <- 'EMRO' 
        case_control$who_region[case_control$country == "Palestinian territory"] <- 'EMRO' 
        
        
### assigning child mortality levels
  # breaks at 8.74, 20.33, 37.96, 94.15 based on quintiles from ALL countries in 2003 
  
  # create child mortality categorization variable: lowest quintile = "very low"; next two quintiles = "low", two highest quintiles = "high" as per WHO
    case_control$child_mort <- ifelse(case_control$u5mr <= 8.74, "very low",
                               ifelse(case_control$u5mr > 8.74 & case_control$u5mr <= 37.96, "low",
                               ifelse(case_control$u5mr > 37.96, "high", NA)))
    # make into an ordered factor (ref = high)
      case_control$child_mort <- factor(case_control$child_mort, levels = c("high", "low", "very low"))
    # create condensed, 2-level child mortality variable
      case_control$child_mort_2 <- ifelse((case_control$child_mort == "very low" | case_control$child_mort == "low"), "vlow/low",
                                   ifelse(case_control$child_mort == "high", "high", NA))
      # make into an ordered factor (ref = high)
        case_control$child_mort_2 <- factor(case_control$child_mort_2, levels = c("high", "vlow/low"))
    
      
      
      
### determine pre/post rota status based on added WHO data
  # compare year of study and year of vax intro (according to WHO)
    case_control$rota_intro_who <- ifelse((case_control$year_began > case_control$rota_vax_yr), 1,
                                   ifelse((case_control$year_began == case_control$rota_vax_yr), 2,
                                   ifelse((case_control$year_began < case_control$rota_vax_yr), 0, NA))) 
  # create new variable that adds WHO data where START data is missing
    case_control$rota_vax_comb <- ifelse((is.na(case_control$rotavirus_vacc) | case_control$rotavirus_vacc ==99), case_control$rota_intro_who, case_control$rotavirus_vacc)
    rota_vax_comparison <- case_control %>% filter(path=="rota") %>% select("country", "year_began", "rotavirus_vacc", "rota_vax_yr", "rota_intro_who", "rota_vax_comb")
  # create lagged variable (post only if >1 year after vax intro) using WHO data
    case_control$rota_vax_lag <- ifelse((case_control$year_began > (case_control$rota_vax_yr+1)), 1,
                                 ifelse(((case_control$year_began == case_control$rota_vax_yr) | (case_control$year_began == (case_control$rota_vax_yr+1))), 2,
                                 ifelse((case_control$year_began < case_control$rota_vax_yr), 0, NA))) 
    
    #table(case_control$rota_vax_lag, case_control$child_mort_2)
    #table(case_control$rota_vax_comb, case_control$child_mort_2)
      
    
### pull out noro and etec strains, and others ###
  # etec- LT or ST, noro- g1 or g2 (MALED and GEMS were all ST etec and noro GII), specifying rota pre/post vax intro
    case_control$path_strain <- ifelse((case_control$path == "etec" & case_control$title == "GEMS"), "etec_st",
                                ifelse((case_control$path == "etec" & case_control$title == "MALED"), "etec_st",
                                ifelse((case_control$path == "etec" & case_control$etec_strain___2 == 1), "etec_st", # st regardless of lt status
                                ifelse((case_control$path == "etec" & case_control$etec_strain___1 == 1), "etec_lt", # lt only, not also st
                                ifelse(case_control$path == "etec", 'etec_unkn', 
                                ifelse((case_control$path == "noro" & case_control$title == "GEMS"), "noro_gii",
                                ifelse((case_control$path == "noro" & case_control$title == "MALED"), "noro_gii", 
                                ifelse((case_control$path == "noro" & case_control$noro_strain___1 == 1 & case_control$noro_strain___2 == 0), "noro_gi",
                                ifelse((case_control$path == "noro" & case_control$noro_strain___1 == 0 & case_control$noro_strain___2 == 1), "noro_gii", 
                                ifelse((case_control$path == "aero" & case_control$aero_strain___1 == 1 & case_control$aero_strain___2 == 0 & 
                                                                      case_control$aero_strain___3 == 0), "aero_hydrophila", 
                                ifelse((case_control$path == "campy" & case_control$campy_strain___1 == 1 & case_control$campy_strain___2 == 0 &
                                                                       case_control$campy_strain___3 == 0 & case_control$campy_strain___4 == 0), "campy_jejuni",
                                ifelse((case_control$path == "campy" & case_control$campy_strain___1 == 0 & case_control$campy_strain___2 == 1 &
                                                                       case_control$campy_strain___3 == 0 & case_control$campy_strain___4 == 0), "campy_coli",
                                ifelse((case_control$path == "campy" & case_control$campy_strain___1 == 0 & case_control$campy_strain___2 == 0 &
                                                                       case_control$campy_strain___3 == 1 & case_control$campy_strain___4 == 0), "campy_lari",
                                ifelse((case_control$path == "campy" & case_control$campy_strain___1 == 0 & case_control$campy_strain___2 == 0 & 
                                                                       case_control$campy_strain___3 == 0 & case_control$campy_strain___4 == 1), "campy_upsaliensis", 
                                ifelse((case_control$path == "chol" & case_control$chol_strain___1 == 1 & case_control$chol_strain___2 == 0), "chol_o1",
                                ifelse((case_control$path == "chol" & case_control$chol_strain___1 == 0 & case_control$chol_strain___2 == 1), "chol_o139",
                                ifelse((case_control$path == "crypto" & case_control$crypto_strain___1 == 1 & case_control$crypto_strain___2 == 0), "crypto_hominis",
                                ifelse((case_control$path == "crypto" & case_control$crypto_strain___1 == 0 & case_control$crypto_strain___2 == 1), "crypto_parvum",
                                ifelse((case_control$path == "ehist" & case_control$enta_strain___1 == 1 & case_control$enta_strain___2 == 0 & case_control$enta_strain___5 == 0), "ehist_histolytica", 
                                ifelse((case_control$path == "ehist" & case_control$enta_strain___1 == 0 & case_control$enta_strain___2 == 1 & case_control$enta_strain___5 == 0), "ehist_dispar",
                                ifelse((case_control$path == "ehist" & case_control$enta_strain___1 == 0 & case_control$enta_strain___2 == 0 & case_control$enta_strain___5 == 1), "ehist_coli",
                                ifelse((case_control$path == "rota" & case_control$rota_strain___1 == 1), "rota_a",
                                ifelse((case_control$path == "epec" & case_control$epec_strain___1 == 1 & case_control$epec_strain___2 == 0), "epec_t",
                                ifelse((case_control$path == "epec" & case_control$epec_strain___1 == 0 & case_control$epec_strain___2 == 1), "epec_a",
                                ifelse(case_control$path == "epec", "epec_unkn",
                                ifelse((case_control$path == "giard" & case_control$giar_strain == 1), "giard_l_i_d",
                                ifelse((case_control$path == "salm" & case_control$salm_strain___2 == 1 & case_control$salm_strain___3 == 0), "salm_paratyphi",
                                ifelse((case_control$path == "salm" & case_control$salm_strain___2 == 0 & case_control$salm_strain___3 == 1), "salm_nts",
                                ifelse((case_control$path == "shig" & case_control$shig_strain___1 == 1 & case_control$shig_strain___2 == 0 &
                                                                      case_control$shig_strain___3 == 0 & case_control$shig_strain___4 == 0), "shig_a",   
                                ifelse((case_control$path == "shig" & case_control$shig_strain___1 == 0 & case_control$shig_strain___2 == 1 &
                                                                      case_control$shig_strain___3 == 0 & case_control$shig_strain___4 == 0), "shig_b",  
                                ifelse((case_control$path == "shig" & case_control$shig_strain___1 == 0 & case_control$shig_strain___2 == 0 &
                                                                      case_control$shig_strain___3 == 1 & case_control$shig_strain___4 == 0), "shig_c",  
                                ifelse((case_control$path == "shig" & case_control$shig_strain___1 == 0 & case_control$shig_strain___2 == 0 &
                                                                      case_control$shig_strain___3 == 0 & case_control$shig_strain___4 == 1), "shig_d", NA))))))))))))))))))))))))))))))))

 
  # pulling out pre/post rota vax info
    case_control$rota_pre_post <-  ifelse((case_control$path == "rota" & case_control$rota_vax_comb == 0), "rota_pre",
                                   ifelse((case_control$path == "rota" & case_control$rota_vax_comb == 1), "rota_post", NA))
    
    
 
### identifying pathogen detection method ###
  
  # pulling detection method from each pathogen    
    case_control$det_meth  <- ifelse(case_control$path == "salm", case_control$salm_detect,
                              ifelse(case_control$path == "shig", case_control$shig_detect,
                              ifelse(case_control$path == "campy", case_control$campy_detect,
                              ifelse(case_control$path == "chol", case_control$chol_detect,
                              ifelse(case_control$path == "etec", case_control$etec_detect,
                              ifelse(case_control$path == "epec", case_control$epec_detect,
                              ifelse(case_control$path == "rota", case_control$rota_detect,
                              ifelse(case_control$path == "noro", case_control$noro_detect,
                              ifelse(case_control$path == "sapo", case_control$sapo_detect,
                              ifelse(case_control$path == "astro", case_control$astro_detect,
                              ifelse(case_control$path == "adeno", case_control$adeno_detect,
                              ifelse(case_control$path == "giard", case_control$giar_detect,
                              ifelse(case_control$path == "crypto", case_control$crypto_detect,
                              ifelse(case_control$path == "ehist", case_control$enta_detect,
                              ifelse(case_control$path == "aero", case_control$aero_detect, NA))))))))))))))) 
    case_control$det_meth <- ifelse((case_control$title == "GEMS" | case_control$title == "MALED"), 2, case_control$det_meth)
  
  # recode
    case_control$det_meth_desc <- ifelse(case_control$det_meth == 1, "EIA",
                                  ifelse(case_control$det_meth == 2, "PCR",
                                  ifelse(case_control$det_meth == 3, "Microscopy",
                                  ifelse(case_control$det_meth == 4, "Insurance claims data",
                                  ifelse(case_control$det_meth == 5, "Clinical diagnosis",
                                  ifelse(case_control$det_meth == 6, "National databases with ICD codes",
                                  ifelse(case_control$det_meth == 7, "Other source with ICD codes",
                                  ifelse(case_control$det_meth == 8, "Culture",
                                  ifelse((case_control$det_meth == 88 | case_control$det_meth == 99), "Other/Unspec", NA)))))))))
    # setting reference level to be "EIA"
      case_control$det_meth_desc <- relevel(as.factor(case_control$det_meth_desc), ref = "EIA")
      
      
  # recode a different way (combine EIA, microscopy, culture/isolation into "conventional")
    case_control$det_meth_3 <- ifelse((case_control$det_meth == 1 | case_control$det_meth ==  3 | case_control$det_meth == 8), "Conventional", 
                               ifelse(case_control$det_meth == 2, "PCR", 
                               ifelse((case_control$det_meth == 88 | case_control$det_meth == 99), "Other/Unspec", NA)))
                               case_control$det_meth_3[is.na(case_control$det_meth_3)] <- "Other/Unspec"
    # setting reference level to be "conventional"
      case_control$det_meth_3 <- factor(case_control$det_meth_3, levels = c("Conventional", "PCR", "Other/Unspec"))
      # flipping reference level to PCR 
        case_control$det_meth_3_flip <- factor(case_control$det_meth_3, levels = c("PCR", "Conventional", "Other/Unspec"))    

    
  # recode as PCR vs no PCR
    case_control$det_meth_2 <- ifelse((case_control$det_meth == 1 | case_control$det_meth ==  3 | case_control$det_meth == 8), "Other/Unspec", 
                               ifelse(case_control$det_meth == 2, "PCR", 
                               ifelse((case_control$det_meth == 88 | case_control$det_meth == 99), "Other/Unspec", NA)))
    case_control$det_meth_2[is.na(case_control$det_meth_2)] <- "Other/Unspec"
    case_control$det_meth_2 <- relevel(as.factor(case_control$det_meth_2), ref = "Other/Unspec")
    
    
### formatting age data ###
  
  # examine stratum-specific age data available
    # minimum age for strata
      summary(case_control$age_min_strata, exclude=NULL)
      table(case_control$age_min, exclude=NULL)
      table(case_control$age_min_strata, exclude=NULL)
      # 41 with age min of 5
    # maximum age for strata  
      table(case_control$age_max, exclude=NULL)
      table(case_control$age_max_strata, exclude=NULL)
      # 165 have age max of 5 --> include these in the youngest age group (ie 5 and under instead of under 5)
      # 309 have age max of 2 --> include in the 0-2 age group (ie 2 and unders instead of under 2)
  
  # create age group variable  
    # in some instances, stratum specific age data are missing but overall age data for the study are available. 
    # using overall study data for the individual strata if necessary and possible (eg. if study is limited to ages 0-2 years, we know all strata are for kids under 5)
    # grouping into 0-5 (many had max at 5 so creating cutoff here), >5, and mixed/unknown
      case_control$age_max <- ifelse(is.na(case_control$age_max), 999, case_control$age_max)
      case_control$age_min <- ifelse(is.na(case_control$age_min), 999, case_control$age_min)
      
      case_control$age_group <- ifelse(case_control$age_max_strata <= 5, "young",
                                ifelse((case_control$age_max_strata == 999 & case_control$age_max <= 5), "young",
                                ifelse((case_control$age_min_strata > 5 & case_control$age_min_strata < 999), "old",
                                ifelse((case_control$age_min_strata == 999 & case_control$age_min > 5 & case_control$age_min < 999), "old", "mixed"))))
      # setting reference level to be "young"
        case_control$age_group <- relevel(as.factor(case_control$age_group), ref = "young")
  
    # splitting U5 into 0-2 and 3-5       
      case_control$age_group_2 <- ifelse(case_control$age_max_strata <= 2, "youngest",
                                  ifelse((case_control$age_max_strata == 999 & case_control$age_max <= 2), "youngest",
                                  ifelse((case_control$age_min_strata > 2 & case_control$age_max_strata <= 5), "middle",
                                  ifelse(((case_control$age_min_strata == 999 & case_control$age_min >2) & (case_control$age_max_strata <= 5)), "middle",
                                  ifelse(((case_control$age_min_strata == 999 & case_control$age_min >2) & (case_control$age_max_strata == 999 & case_control$age_max <= 5)), "middle",
                                  ifelse((case_control$age_min_strata >2 & (case_control$age_max_strata == 999 & case_control$age_max <= 5)), "middle",
                                  ifelse((case_control$age_min_strata > 5 & case_control$age_min_strata < 999), "oldest",
                                  ifelse((case_control$age_min_strata == 999 & case_control$age_min > 5 & case_control$age_min < 999), "oldest", "mixed"))))))))
      # setting reference level to be "youngest"
        case_control$age_group_2 <- relevel(as.factor(case_control$age_group_2), ref = "youngest")

    # splitting U5 into 0-1 and 2-4       
      case_control$age_group_3 <- ifelse(case_control$age_max_strata < 2, "youngest",
                                  ifelse((case_control$age_max_strata == 999 & case_control$age_max < 2), "youngest",
                                  ifelse((case_control$age_min_strata >= 2 & case_control$age_max_strata < 5), "middle",
                                  ifelse(((case_control$age_min_strata == 999 & case_control$age_min >= 2) & (case_control$age_max_strata < 5)), "middle",
                                  ifelse(((case_control$age_min_strata == 999 & case_control$age_min >= 2) & (case_control$age_max_strata == 999 & case_control$age_max < 5)), "middle",
                                  ifelse((case_control$age_min_strata >= 2 & (case_control$age_max_strata == 999 & case_control$age_max < 5)), "middle",
                                  ifelse((case_control$age_min_strata >= 5 & case_control$age_min_strata < 999), "oldest",
                                  ifelse((case_control$age_min_strata == 999 & case_control$age_min >= 5 & case_control$age_min < 999), "oldest", "mixed"))))))))
      # setting reference level to be "youngest"
        case_control$age_group_3 <- relevel(as.factor(case_control$age_group_3), ref = "youngest")
      
    # seeing how the age groups differ in terms of n
      table(case_control$age_group, exclude=NULL)
      table(case_control$age_group_2, exclude=NULL)
      table(case_control$age_group_3, exclude=NULL)
        
        
### formatting study type data ###

  # combining study designs from lit review
    case_control$study_design <- ifelse((case_control$design == 1 | case_control$design == 2 | case_control$design == 3 | case_control$design == 4), "cohort",
                                 ifelse((case_control$design == 6 | case_control$design == 7), "case control", NA))
    # setting reference level to be "case control"
      case_control$study_design <- relevel(as.factor(case_control$study_design), ref = "case control")
      
 
      
### calculate a,b,c,d of 2x2 table for OR and CI where not already provided for study ###
      
  # using columns:
    # cases_detected = # of cases/diarrhea samples (cohort) with pathogen detected
    # cases_detected_percent = % of cases/diarrhea samples (cohort) with pathogen detected
    # controls_detected = # of controls/non-diarrhea samples (cohort) with pathogen detected
    # controls_detected_percent = % of controls/non-diarrhea samples (cohort) with pathogen detected

  # correcting obvious error where percent of controls with pathogen detected is over 100
    case_control <- case_control %>% mutate(controls_detected_percent = replace(controls_detected_percent, (author=="Bodhidatta" & year_pub==2010 & path=="crypto"), 5.1))
  # correcting identified error in MAyo and MOyo
    case_control <- case_control %>% mutate(cases_detected = replace(cases_detected, (author=="Moyo"), 24))
  
   
  # Yori 2009 has percent of cases/diarrhea samples or non-cases/non-diarrhea samples with pathogen detected but
  # missing number of cases/diarrhea and non-cases/non-diarrhea samples indicated for strata, using the study-specific 
  # number of cases/diarrhea and non-cases/non-diarrhea (have confirmed the numbers apply to the strata)
    case_control$cases_detected     <- ifelse(case_control$ID_uniq_n == 168, case_control$cases*(case_control$cases_detected_percent*.01), case_control$cases_detected)
    case_control$controls_detected  <- ifelse(case_control$ID_uniq_n == 168, case_control$controls*(case_control$controls_detected_percent*.01), case_control$controls_detected)

      
  # calculate cells of the 2x2 table-- will be used to calculate OR and CIs where data are not already provided
    case_control$a <- case_control$cases_detected
    case_control$b <- case_control$controls_detected
    case_control$c <- case_control$cases_detected/(case_control$cases_detected_percent*.01) - case_control$a
    case_control$d <- case_control$controls_detected/(case_control$controls_detected_percent*.01) - case_control$b

  # calculate prevalence in cases & controls
    case_control$prev_case    <- case_control$a/(case_control$a + case_control$c)
    case_control$prev_control <- case_control$b/(case_control$b + case_control$d)

### determining the best available measure of association ###
  # heirarchy: adj OR provided > unadj OR prvided > adj RR provided > unadj RR provided > unadj OR-calculated (not provided in article)
  
  # changing NAs in y/n columns for OR, RR and IR to 0
    case_control$or_unadj_yn[is.na(case_control$or_unadj_yn)] <- 0
    case_control$or_adj_yn[is.na(case_control$or_adj_yn)] <- 0
    case_control$rr_unadj_yn[is.na(case_control$rr_unadj_yn)] <- 0
    case_control$rr_adj_yn[is.na(case_control$rr_adj_yn)] <- 0
    case_control$ir_yn[is.na(case_control$ir_yn)] <- 0 
  
  # variable indicating best available measure of association
    case_control$best_meas <- ifelse(case_control$or_adj_yn == 1, "OR adj",
                              ifelse(case_control$or_unadj_yn == 1, "OR unadj",
                              ifelse(case_control$rr_adj_yn == 1, "RR adj",
                              ifelse(case_control$rr_unadj_yn == 1, "RR unadj", "OR unadj- calc"))))
    case_control$best_meas <- factor(case_control$best_meas, levels = c("OR unadj- calc", "RR unadj", "RR adj", "OR unadj", "OR adj"))

  # variable with best available measure of association for each observation
    # effect estimate
      case_control$ee_comb <-   ifelse(case_control$best_meas == "OR adj", case_control$or_adj,
                                ifelse(case_control$best_meas == "OR unadj", case_control$or_unadj,
                                ifelse(case_control$best_meas == "RR adj", case_control$rr_adj,
                                ifelse(case_control$best_meas == "RR unadj", case_control$rr_unadj, ((case_control$a/case_control$c)/(case_control$b/case_control$d))))))
    # lower CI
      case_control$ee_comb_lci <- ifelse(case_control$best_meas == "OR adj", case_control$or_adj_lci,
                                  ifelse(case_control$best_meas == "OR unadj", case_control$or_unadj_lci,
                                  ifelse(case_control$best_meas == "RR adj", case_control$rr_adj_lci,
                                  ifelse(case_control$best_meas == "RR unadj", case_control$rr_unadj_lci, exp(log(case_control$ee_comb)-1.96*sqrt(1/case_control$a + 1/case_control$b + 1/case_control$c + 1/case_control$d))))))
    # upper CI
      case_control$ee_comb_uci <- ifelse(case_control$best_meas == "OR adj", case_control$or_adj_uci,
                                  ifelse(case_control$best_meas == "OR unadj", case_control$or_unadj_uci,
                                  ifelse(case_control$best_meas == "RR adj", case_control$rr_adj_uci,
                                  ifelse(case_control$best_meas == "RR unadj", case_control$rr_unadj_uci, exp(log(case_control$ee_comb)+1.96*sqrt(1/case_control$a + 1/case_control$b + 1/case_control$c + 1/case_control$d))))))
    
    # cleaning/formatting 
      case_control$ee_comb <- ifelse(case_control$ee_comb == "Inf", NA, case_control$ee_comb)  

                   
### formatting data for use with "meta" package-- need log OR or log RR and sampling variances ###
      
  # log OR- using "combined" variable (variable using best available measure of association) and taking log
    case_control$log_ee <- NA
    case_control$log_ee <- replmiss(case_control$log_ee, log(case_control$ee_comb))
  
  # standard errors
    case_control$log_ee_se <- NA
    case_control$log_ee_se <- replmiss(case_control$log_ee_se, with(case_control, (log(ee_comb_uci) - log(ee_comb_lci))/(2*1.96)))
  
  # sampling variance from standard errors
    case_control$var <- NA
    case_control$var <- replmiss(case_control$var, case_control$log_ee_se^2)
    
  # cleaning
    case_control$log_ee <- ifelse(case_control$log_ee == "Inf", NA, case_control$log_ee)
    case_control$log_ee <- ifelse(case_control$log_ee == "-Inf", NA, case_control$log_ee)
    case_control$log_ee[is.nan(case_control$log_ee)] <- NA
    case_control$var <- ifelse(case_control$var == "Inf", NA, case_control$var)
    case_control$var[is.nan(case_control$var)] <- NA
    case_control$var <- ifelse(case_control$var == 0, NA, case_control$var)
    case_control$log_ee_se <- NULL
    

# unique ID for each observation (based on unique author/year/title and new number for each strata/pathogen)
  case_control$study_obs_ID <- ave(case_control$author_year_title, case_control$author_year_title, FUN=seq_along)
  case_control$author_year_obs <- paste(case_control$author, case_control$year_pub, case_control$study_obs_ID, sep = "-")

# author/year simplified for forest plot
  case_control$author_year_simp <- paste(case_control$author, case_control$year_pub, sep = "-")
  
  
  # mark KNOWN incorrect data
    case_control$include <- ifelse((case_control$author_year_simp == "Lopman-2014" |
                                    case_control$author_year_simp == "Hasan-2006" |
                                    case_control$author_year_simp == "Vubil-2018"), 0, case_control$include)
    # checking number of unique studies
      include <- case_control %>% filter(include==1)
      length(unique(include$author_year_title))
      # 145 studies
      
      
      
      

########################################################
####            SAVE PREPPED DATAFRAME              ####
########################################################
      
   
# save prepped dataframe
  save(case_control,file="/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/Julia/Data/case_control_edited.Rdata")  
  write.csv(case_control,file="/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/Julia/Data/case_control_edited.csv") 
   
  
# save dataframe of INCLUDED studies only as "case_control_clean" 
  case_control_clean <- case_control %>% filter(include==1)
    save(case_control_clean,file="/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/Julia/Data/case_control_clean.Rdata")
    write.csv(case_control_clean,file="/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/Julia/Data/case_control_clean.csv")
    
  

 
    
########################################################
####        OUTLIERS/INFLUENCE DIAGNOSTICS          ####
########################################################
# resource: http://www.metafor-project.org/doku.php/plots:plot_of_influence_diagnostics
  
    
# load dataframe for analysis
  load("/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/Julia/Data/case_control_clean.Rdata")
  length(unique(case_control_clean$author_year_title))
  # dataframe for each pathogen (not further stratified)
    pathogen_data_list <- unique(case_control_clean$path)
    for (v in pathogen_data_list) {
      assign(paste0("data_", as.character(v)), case_control_clean %>% filter (path == v), envir = .GlobalEnv)
    }  
  # dataframes for specific pathogen strains
    data_etec_st   <- case_control_clean %>% filter(path_strain == 'etec_st')
    data_etec_lt   <- case_control_clean %>% filter(path_strain == 'etec_lt')
    data_etec_unkn <- case_control_clean %>% filter(path_strain == 'etec_unkn') 
    data_epec_t    <- case_control_clean %>% filter(path_strain == 'epec_t') 
    data_epec_a    <- case_control_clean %>% filter(path_strain == 'epec_a') 
    data_epec_unkn <- case_control_clean %>% filter(path_strain == 'epec_unkn')  
    
    
# random effects meta-analysis loop
  meta_analysis_loop_unadj <- function(data){
    rma(yi = log_ee, vi = var,
        method = "DL", 
        data = data, 
        slab=paste(author_year_obs))
  }   
  
  
  # data for loop
    data_list_unadj <- c('data_salm', 'data_shig', 'data_campy', 'data_chol', 'data_etec', 'data_epec', 'data_rota', 'data_noro',
                         'data_sapo', 'data_astro', 'data_adeno', 'data_giard', 'data_crypto', 'data_ehist', 'data_aero',
                         'data_etec_st', 'data_etec_lt', 'data_etec_unkn', 'data_epec_t', 'data_epec_a', 'data_epec_unkn')
  # running m-a loop for each pathogen
    ma_path_unadj <- list()
      for(i in seq_along(data_list_unadj)){ 
        ma_path_unadj[[i]] <- meta_analysis_loop_unadj(get(data_list_unadj[i]))
        }
  # calculate and plot influence diagnostics
    for (i in 1:21){
      plot(influence(ma_path_unadj[[i]]))
    }  
  
  # examining each and identifying studies to drop
    options(max.print=10000)
    # salm
      print(influence(ma_path_unadj[[1]]))
      # drop: Chang-2017-4 ; Cardemil-2017-7   
    # shig
      print(influence(ma_path_unadj[[2]]))
      # drop: Platts-Mills-NA-9; Platts-Mills-NA-10
    # campy
      print(influence(ma_path_unadj[[3]]))
      # drop: Francois-2018-1; de Wit-2001-28
    # chol
      print(influence(ma_path_unadj[[4]])) 
      # drop: Lima-2019-13; Bodhidatta-2019-5
    # etec
      print(influence(ma_path_unadj[[5]])) 
      # drop: none
    # epec
      print(influence(ma_path_unadj[[6]])) 
      # drop: Albert-1999-23 
    # rota
      print(influence(ma_path_unadj[[7]])) 
      # drop: none
    # noro
      print(influence(ma_path_unadj[[8]])) 
      # drop: My-2013-2 
    # sapo
      print(influence(ma_path_unadj[[9]])) 
      # drop: Platts-Mills-NA-183
    # astro
      print(influence(ma_path_unadj[[10]])) 
      # drop: Platts-Mills-NA-5; Maldonado-1998-2; Platts-Mills-NA-37; Platts-Mills-NA-46
    # adeno
      print(influence(ma_path_unadj[[11]])) 
      # drop: Iturriza-Gómara-2019-13
    # giard
      print(influence(ma_path_unadj[[12]])) 
      # drop: none
    # crypto
      print(influence(ma_path_unadj[[13]])) 
      # drop: Platts-Mills-NA-8 
    # ehist
      print(influence(ma_path_unadj[[14]])) 
      # drop: Krumkamp-2015-30
    # aero
      print(influence(ma_path_unadj[[15]])) 
      # drop: none
    # etec_st
      print(influence(ma_path_unadj[[16]])) 
      # drop: none  
    # etec_lt
      print(influence(ma_path_unadj[[17]])) 
      # drop: Mansour-2014-5 
    # etec_unkn
      print(influence(ma_path_unadj[[18]])) 
      # drop: none
    # epec_t
      print(influence(ma_path_unadj[[19]])) 
      # drop: none
    # epec_a
      print(influence(ma_path_unadj[[20]])) 
      # drop: none
    # epec_unkn
      print(influence(ma_path_unadj[[21]])) 
      # drop: Albert-1999-23
   
      
# variable for influential observation (1 = influential; 0 = not)
  case_control_clean$infl  <-  ifelse((case_control_clean$path == "salm" & case_control_clean$author_year_obs == "Chang-2017-4"), 1,
                               ifelse((case_control_clean$path == "salm" & case_control_clean$author_year_obs == "Cardemil-2017-7"), 1,
                               ifelse((case_control_clean$path == "shig" & case_control_clean$author_year_obs == "Platts-Mills-NA-9"), 1,
                               ifelse((case_control_clean$path == "shig" & case_control_clean$author_year_obs == "Platts-Mills-NA-10"), 1,
                               ifelse((case_control_clean$path == "campy" & case_control_clean$author_year_obs == "Francois-2018-1"), 1,
                               ifelse((case_control_clean$path == "campy" & case_control_clean$author_year_obs == "de Wit-2001-28"), 1,
                               ifelse((case_control_clean$path == "chol" & case_control_clean$author_year_obs == "Lima-2019-13"), 1,
                               ifelse((case_control_clean$path == "chol" & case_control_clean$author_year_obs == "Bodhidatta-2019-5"), 1,
                               ifelse((case_control_clean$path_strain == "etec_lt" & case_control_clean$author_year_obs == "Mansour-2014-5"), 1,
                               ifelse((case_control_clean$path_strain == "epec_unkn" & case_control_clean$author_year_obs == "Albert-1999-23"), 1,
                               ifelse((case_control_clean$path == "noro" & case_control_clean$author_year_obs == "My-2013-2"), 1,
                               ifelse((case_control_clean$path == "sapo" & case_control_clean$author_year_obs == "Platts-Mills-NA-183"), 1,
                               ifelse((case_control_clean$path == "astro" & case_control_clean$author_year_obs == "Platts-Mills-NA-5"), 1,
                               ifelse((case_control_clean$path == "astro" & case_control_clean$author_year_obs == "Maldonado-1998-2"), 1,
                               ifelse((case_control_clean$path == "astro" & case_control_clean$author_year_obs == "Platts-Mills-NA-37"), 1,
                               ifelse((case_control_clean$path == "astro" & case_control_clean$author_year_obs == "Platts-Mills-NA-46"), 1,
                               ifelse((case_control_clean$path == "adeno" & case_control_clean$author_year_obs == "Iturriza-Gómara-2019-13"), 1,   
                               ifelse((case_control_clean$path == "crypto" & case_control_clean$author_year_obs == "Platts-Mills-NA-8"), 1,
                               ifelse((case_control_clean$path == "ehist" & case_control_clean$author_year_obs == "Krumkamp-2015-30"), 1, 0)))))))))))))))))))
  
# re-save dataframe with this new variable
  save(case_control_clean,file="/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/Julia/Data/case_control_clean.Rdata")
  write.csv(case_control_clean,file="/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/Julia/Data/case_control_clean.csv")
  
 
  
################################################
####           FURTHER SUBSETTING           ####
################################################
  
  
# load dataframe for analysis
  load("/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/Julia/Data/case_control_clean.Rdata")

  
# dataframe with all observations (keeping outliers/influential) but dropping those with missing info
  # dropping observations with...
    # NA for variance
      case_control_full <- case_control_clean[!is.na(case_control_clean$var), ]
      # 446 obs dropped here b/c of NA resulting from 0s in 2x2 table, typically among controls
        length(unique(case_control_full$author_year_title))
        # now down to 1122 observations & 130 studies
      write.csv(case_control_full,file="/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/Julia/Data/case_control_full.csv")
     
# sensitivity analyses       
  # dataframe without the outliers/influential observations
    case_control_limited <- case_control_full[ which(case_control_full$infl == 0), ]
    # 19 outliers dropped
      length(unique(case_control_limited$author_year_title))
      # now down to 1103 obs and 129 studies
  # dataframe with only case-control studies (excludes other study types) but includes outliers
    case_control_limited2 <- case_control_full[ which(case_control_full$study_design == "case control"), ]
      length(unique(case_control_limited2$author_year_title))
    # now down to 881 obs and 93 studies

      
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

# 0-5, 6+          
table(case_control_full$age_group, exclude=NULL)   
# 0-2, 3-5, 6+
table(case_control_full$age_group_2, exclude=NULL) 
# 0-1, 2-4, 5+
table(case_control_full$age_group_3, exclude=NULL) 
          
          
        
### Using "full" dataset ###      

# recategorize "path" to also include the specific strains of interest (epec, etec)
  case_control_full$path_spec <- ifelse((case_control_full$path == 'etec' | case_control_full$path == 'epec'), case_control_full$path_strain, as.character(case_control_full$path))
  table(case_control_full$path_spec, exclude=NULL)                        
  
        
# number of studies and observations (all "included" studies, including mixed age group but excluding those without variance)
  length(unique(case_control_full$author_year_title))
  # 130 studies (including GEMS/MAL-ED), 1122 observations    
        
# number of case/control individuals/samples
  # first extracting one record per study & country
    case_control_full_unique <- case_control_full %>% distinct(author_year_title, country, .keep_all = T)
    write.csv(case_control_full_unique, "/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/Julia/Data/case_control_full_unique.csv")
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
    write.csv(study_summary, "/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/Julia/Results/study_summary.csv")
        
        

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
    # save  
      write.csv(path_table, "/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/Julia/Results/descr_stats_by_path.csv")
      
    

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
  # save  
    write.csv(best_meas_table, "/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/Julia/Results/best_meas_table.csv")     
      
    

    
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
  # save (so I can extract I2 data)
    write.csv(ma_path_unadj_results, "/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/Julia/Results/ma_path_unadj_results.csv")
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
                                        'data_young_vlow_low_campy', 'data_young_vlow_low_chol', 'data_young_vlow_low_epec',  'data_young_vlow_low_etec', 'data_young_vlow_low_salm', 'data_young_vlow_low_shig', 
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
                                          'campy', 'chol',  'epec',  'etec', 'salm', 'shig', 
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
                             'data_old_aero', 'data_old_campy', 'data_old_chol', 'data_old_epec',  'data_old_etec', 'data_old_salm', 'data_old_shig',
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
                               'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig',
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
                                'data_middle_aero', 'data_middle_campy', 'data_middle_chol', 'data_middle_epec',  'data_middle_etec', 'data_middle_salm', 'data_middle_shig', 
                                'data_middle_crypto', 'data_middle_ehist', 'data_middle_giard', 
                                'data_middle_rota_pre', 'data_rota_post',
                                'data_middle_epec_a', 'data_middle_epec_t', 'data_middle_epec_unkn',
                                'data_middle_etec_st', 'data_middle_etec_lt', 'data_middle_etec_unkn')
      
    # labels for results
      data_labels_middle_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                  'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                                  'crypto', 'ehist', 'giard', 
                                  'rota_pre', 'rota_post',
                                  'epec_a', 'epec_t', 'epec_unkn',
                                  'etec_st', 'etec_lt', 'etec_unkn')
      
    # model
      ma_path_middle_all <- list()
      for(i in seq_along(data_list_middle_all)){ 
        ma_path_middle_all[[i]] <- meta_analysis_loop_all(get(data_list_middle_all[i]))
        }  
    # examine results  
      for (i in 1:23){
        print(ma_path_middle_all[[i]], digits=2)
        }   
    # extract estimate and combine results  
      magic_for(silent = TRUE)
      for (i in 1:23) {
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
                                       'data_youngest_high_epec_unkn',
                                       'data_youngest_high_etec_st', 'data_youngest_high_etec_lt', 'data_youngest_high_etec_unkn')
    
    # labels for results
      data_labels_youngest_high_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                         'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                                         'crypto', 'ehist', 'giard', 
                                         'rota_pre',
                                         'epec_unkn',
                                         'etec_st', 'etec_lt', 'etec_unkn')
    
    # model
      ma_path_youngest_high_all <- list()
      for(i in seq_along(data_list_youngest_high_all)){ 
        ma_path_youngest_high_all[[i]] <- meta_analysis_loop_all(get(data_list_youngest_high_all[i]))
      }  
    # examine results  
      for (i in 1:20){
        print(ma_path_youngest_high_all[[i]], digits=2)
      }   
    # extract estimate and combine results  
      magic_for(silent = TRUE)
      for (i in 1:20) {
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
    
    
    
  ### middle/high child mort levels ### 
      
    # data list to loop through  
      data_list_middle_high_all <- c('data_middle_high_adeno', 'data_middle_high_astro', 'data_middle_high_noro', 'data_middle_high_rota', 'data_middle_high_sapo',
                                     'data_middle_high_aero', 'data_middle_high_campy', 'data_middle_high_epec',  'data_middle_high_etec', 'data_middle_high_salm', 'data_middle_high_shig', 
                                     'data_middle_high_crypto', 'data_middle_high_ehist', 'data_middle_high_giard', 
                                     'data_middle_high_rota_pre',
                                     'data_middle_high_epec_a', 'data_middle_high_epec_t', 'data_middle_high_epec_unkn',
                                     'data_middle_high_etec_st', 'data_middle_high_etec_unkn')
      
    # labels for results
      data_labels_middle_high_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                       'aero', 'campy', 'epec',  'etec', 'salm', 'shig', 
                                       'crypto', 'ehist', 'giard', 
                                       'rota_pre',
                                       'epec_a', 'epec_t', 'epec_unkn',
                                       'etec_st', 'etec_unkn')
      
    # model
      ma_path_middle_high_all <- list()
      for(i in seq_along(data_list_middle_high_all)){ 
        ma_path_middle_high_all[[i]] <- meta_analysis_loop_all(get(data_list_middle_high_all[i]))
        }  
    # examine results  
      for (i in 1:20){
        print(ma_path_middle_high_all[[i]], digits=2)
        }   
    # extract estimate and combine results  
      magic_for(silent = TRUE)
      for (i in 1:20) {
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
      data_list_middle_vlow_low_all <- c('data_middle_vlow_low_noro', 'data_middle_vlow_low_rota', 
                                         'data_middle_vlow_low_chol',
                                         'data_middle_vlow_low_epec',  'data_middle_vlow_low_etec', 'data_middle_vlow_low_shig',
                                         'data_middle_vlow_low_crypto', 
                                         'data_middle_vlow_low_rota_pre', 'data_middle_vlow_low_rota_post',
                                         'data_middle_vlow_low_epec_t', 'data_middle_vlow_low_epec_unkn',
                                         'data_middle_vlow_low_etec_st', 'data_middle_vlow_low_etec_lt', 'data_middle_vlow_low_etec_unkn')
      
    # labels for results
      data_labels_middle_vlow_low_all <- c('noro', 'rota', 
                                           'chol',
                                           'epec',  'etec', 'shig',
                                           'crypto', 
                                           'rota_pre', 'rota_post',
                                           'epec_t', 'epec_unkn',
                                           'etec_st', 'etec_lt', 'etec_unkn')
      
    # model
      ma_path_middle_vlow_low_all <- list()
      for(i in seq_along(data_list_middle_vlow_low_all)){ 
        ma_path_middle_vlow_low_all[[i]] <- meta_analysis_loop_all(get(data_list_middle_vlow_low_all[i]))
        }  
      # examine results  
      for (i in 1:14){
        print(ma_path_middle_vlow_low_all[[i]], digits=2)
        }   
    # extract estimate and combine results  
      magic_for(silent = TRUE)
      for (i in 1:14) {
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
                                         'data_young_asia_high_epec_unkn', 
                                         'data_young_asia_high_etec_st', 'data_young_asia_high_etec_lt', 'data_young_asia_high_etec_unkn')
      
    # labels for results
      data_young_labels_asia_high_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                           'aero', 'campy', 'chol', 'epec',  'etec', 'shig',
                                           'crypto', 'ehist', 'giard',
                                           'rota_pre',
                                           'epec_unkn',
                                           'etec_st', 'etec_lt', 'etec_unkn')
      
    # model
      ma_path_young_asia_high_all <- list()
      for(i in seq_along(data_list_young_asia_high_all)){ 
        ma_path_young_asia_high_all[[i]] <- meta_analysis_loop_all(get(data_list_young_asia_high_all[i]))
      }  
    # examine results  
      for (i in 1:19){
        print(ma_path_young_asia_high_all[[i]], digits=2)
      }   
    # extract estimate and combine results  
      magic_for(silent = TRUE)
      for (i in 1:19) {
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
                                        #ma_path_youngest_all_results, ma_path_middle_all_results,
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
      
    # writing to csv
      write.csv(ma_new2_combined_results_wide, file = "/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/Julia/Results/ma_new2_combined_results_wide.csv")
      
      
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
  # now down to 1,068 obs
    
    
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
  # save so I can pull out I2 values
    write.csv(ma_path_or_unadj_results, "/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/Julia/Results/ma_path_or_unadj_results.csv")
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
                                        'data_or_youngest_high_epec_unkn',
                                        'data_or_youngest_high_etec_st', 'data_or_youngest_high_etec_lt', 'data_or_youngest_high_etec_unkn')
        
  # labels for results
    data_or_labels_youngest_high_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                          'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                                          'crypto', 'ehist', 'giard', 
                                          'rota_pre',
                                          'epec_unkn',
                                          'etec_st', 'etec_lt', 'etec_unkn')
      
  # model
    ma_path_or_youngest_high_all <- list()
    for(i in seq_along(data_or_list_youngest_high_all)){ 
      ma_path_or_youngest_high_all[[i]] <- meta_analysis_loop_all(get(data_or_list_youngest_high_all[i]))
    }  
  # examine results  
    for (i in 1:20){
      print(ma_path_or_youngest_high_all[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:20) {
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
                                      'data_or_middle_high_aero', 'data_or_middle_high_campy', 'data_or_middle_high_epec',  'data_or_middle_high_etec', 'data_or_middle_high_salm', 'data_or_middle_high_shig', 
                                      'data_or_middle_high_crypto', 'data_or_middle_high_ehist', 'data_or_middle_high_giard', 
                                      'data_or_middle_high_rota_pre',
                                      'data_or_middle_high_epec_a', 'data_or_middle_high_epec_t', 'data_or_middle_high_epec_unkn',
                                      'data_or_middle_high_etec_st', 'data_or_middle_high_etec_unkn')
    
  # labels for results
    data_or_labels_middle_high_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                        'aero', 'campy', 'epec',  'etec', 'salm', 'shig', 
                                        'crypto', 'ehist', 'giard', 
                                        'rota_pre',
                                        'epec_a', 'epec_t', 'epec_unkn',
                                        'etec_st', 'etec_unkn')
    
  # model
    ma_path_or_middle_high_all <- list()
    for(i in seq_along(data_or_list_middle_high_all)){ 
      ma_path_or_middle_high_all[[i]] <- meta_analysis_loop_all(get(data_or_list_middle_high_all[i]))
    }  
  # examine results  
    for (i in 1:20){
      print(ma_path_or_middle_high_all[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:20) {
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
    data_or_list_middle_vlow_low_all <- c('data_or_middle_vlow_low_noro', 'data_or_middle_vlow_low_rota', 
                                          'data_or_middle_vlow_low_chol',
                                          'data_or_middle_vlow_low_epec',  'data_or_middle_vlow_low_etec', 'data_or_middle_vlow_low_shig',
                                          'data_or_middle_vlow_low_crypto', 
                                          'data_or_middle_vlow_low_rota_pre', 'data_or_middle_vlow_low_rota_post',
                                          'data_or_middle_vlow_low_epec_t', 'data_or_middle_vlow_low_epec_unkn',
                                          'data_or_middle_vlow_low_etec_st', 'data_or_middle_vlow_low_etec_lt', 'data_or_middle_vlow_low_etec_unkn')
      
  # labels for results
    data_or_labels_middle_vlow_low_all <- c('noro', 'rota', 
                                            'chol',
                                            'epec',  'etec', 'shig',
                                            'crypto', 
                                            'rota_pre', 'rota_post',
                                            'epec_t', 'epec_unkn',
                                            'etec_st', 'etec_lt', 'etec_unkn')
    
  # model
    ma_path_or_middle_vlow_low_all <- list()
    for(i in seq_along(data_or_list_middle_vlow_low_all)){ 
      ma_path_or_middle_vlow_low_all[[i]] <- meta_analysis_loop_all(get(data_or_list_middle_vlow_low_all[i]))
    }  
  # examine results  
    for (i in 1:14){
      print(ma_path_or_middle_vlow_low_all[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:14) {
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
    # writing to csv
      write.csv(ma_or_combined_results_wide, file = "/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/Julia/Results/ma_or_combined_results_wide.csv")
      
      view(ma_or_combined_results_wide)
    
      
      

##################################################################
####       RUNNING MODELS- **NEW METHOD** ALL PREDICTORS      ####
####    NEW STRATEGY-- LOOPING THROUGH PATHOGEN DATAFRAMES    ####
###       SENSITIVITY ANALYSIS-- EXCLUDING COINFECTIONS       ####    
####                     JUL 13, 2020                         ####
##################################################################   
    
# dataframe without co-infections
    case_control_coinf <- case_control_full[ which(case_control_full$path_num == 1), ]
    # now down to 835 obs
    
    
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
    data_coinf_list_middle_high_all <- c('data_coinf_middle_high_adeno', 'data_coinf_middle_high_astro', 'data_coinf_middle_high_noro', 'data_coinf_middle_high_rota', 'data_coinf_middle_high_sapo',
                                         'data_coinf_middle_high_aero', 'data_coinf_middle_high_campy', 'data_coinf_middle_high_epec',  'data_coinf_middle_high_etec', 'data_coinf_middle_high_salm', 'data_coinf_middle_high_shig', 
                                         'data_coinf_middle_high_crypto', 'data_coinf_middle_high_ehist', 'data_coinf_middle_high_giard', 
                                         'data_coinf_middle_high_rota_pre',
                                         'data_coinf_middle_high_epec_a', 'data_coinf_middle_high_epec_t', 'data_coinf_middle_high_epec_unkn',
                                         'data_coinf_middle_high_etec_st', 'data_coinf_middle_high_etec_unkn')
      
  # labels for results
    data_coinf_labels_middle_high_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                           'aero', 'campy', 'epec',  'etec', 'salm', 'shig', 
                                           'crypto', 'ehist', 'giard', 
                                           'rota_pre',
                                           'epec_a', 'epec_t', 'epec_unkn',
                                           'etec_st', 'etec_unkn')
    
  # model
    ma_path_coinf_middle_high_all <- list()
    for(i in seq_along(data_coinf_list_middle_high_all)){ 
      ma_path_coinf_middle_high_all[[i]] <- meta_analysis_loop_all(get(data_coinf_list_middle_high_all[i]))
    }  
  # examine results  
    for (i in 1:20){
      print(ma_path_coinf_middle_high_all[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:20) {
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
    data_coinf_list_middle_vlow_low_all <- c('data_coinf_middle_vlow_low_noro', 'data_coinf_middle_vlow_low_rota', 
                                             'data_coinf_middle_vlow_low_chol',
                                             'data_coinf_middle_vlow_low_epec',  'data_coinf_middle_vlow_low_etec', 'data_coinf_middle_vlow_low_shig',
                                             'data_coinf_middle_vlow_low_crypto', 
                                             'data_coinf_middle_vlow_low_rota_pre', 'data_coinf_middle_vlow_low_rota_post',
                                             'data_coinf_middle_vlow_low_epec_t', 'data_coinf_middle_vlow_low_epec_unkn',
                                             'data_coinf_middle_vlow_low_etec_st', 'data_coinf_middle_vlow_low_etec_lt', 'data_coinf_middle_vlow_low_etec_unkn')
      
  # labels for results
    data_coinf_labels_middle_vlow_low_all <- c('noro', 'rota', 
                                               'chol',
                                               'epec',  'etec', 'shig',
                                               'crypto', 
                                               'rota_pre', 'rota_post',
                                               'epec_t', 'epec_unkn',
                                               'etec_st', 'etec_lt', 'etec_unkn')
    
  # model
    ma_path_coinf_middle_vlow_low_all <- list()
    for(i in seq_along(data_coinf_list_middle_vlow_low_all)){ 
      ma_path_coinf_middle_vlow_low_all[[i]] <- meta_analysis_loop_all(get(data_coinf_list_middle_vlow_low_all[i]))
    }  
  # examine results  
    for (i in 1:14){
      print(ma_path_coinf_middle_vlow_low_all[[i]], digits=2)
    }   
  # extract estimate and combine results  
    magic_for(silent = TRUE)
    for (i in 1:14) {
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
    
    
  # writing to csv
    write.csv(ma_coinf_combined_results_wide, file = "/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/Julia/Results/ma_coinf_combined_results_wide.csv")
    
    view(ma_coinf_combined_results_wide)    

    
    
##################################################################
####       RUNNING MODELS- **NEW METHOD** ALL PREDICTORS      ####
####          STRATIFICATION BY DETECTION METHOD ONLY         ####
####            LOOPING THROUGH PATHOGEN DATAFRAMES           ####
####                     Dec 04, 2020                         ####
##################################################################         
    
# dataframes with only those tested by PCR and only those not tested by pcr
  case_control_pcr <- case_control_full[ which(case_control_full$det_meth_2 == "PCR"), ]
  # now down to 571 obs
  case_control_no_pcr <- case_control_full[ which(case_control_full$det_meth_2!= "PCR"), ]
  # now down to 551 obs 
  
  
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
  
  # writing to csv
    write.csv(ma_pcr_combined_results_wide, file = "/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/Julia/Results/ma_pcr_combined_results.csv")
    
    
  

  
#########################################
####          MODEL PLOTS            ####
#########################################    
  
  
# MAIN PLOT FOR MANUSCRIPT- NOV 17, 2020,
# UPDATED AGAIN JUN 22, 2021
  
  
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
    
    
  pdf("/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/Julia/Figures/path_ORs_wrap_5.pdf", width=8, height=11)
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
    

      
                    
#########################################
####          FOREST PLOTS           ####
#########################################
# RESOURCES:
# some basics: https://stackoverflow.com/questions/38062650/forest-plot-for-a-beginner-simple-example-using-ggplot2-edited
# fine tuning plots: https://stackoverflow.com/questions/20766666/finetuning-a-forest-plot-with-ggplot2
# looping through databases and plotting:  https://stackoverflow.com/questions/46686599/loop-with-a-defined-ggplot-function-over-multiple-dataframes
# saving plots from loop into pdf: https://stackoverflow.com/questions/7534606/save-multiple-graphs-one-after-another-in-one-pdf-file
# https://stackoverflow.com/questions/30464484/custom-forest-plot-with-with-ggplot2-cant-have-multiple-groups-cis-cross-the

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
    
 
 pdf(file='/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/Julia/Forest Plots/Forest plots- all-test2.pdf', width=8.5, height=11)
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
 
 
 
##################################################################
####       RUNNING MODELS- **NEW METHOD** ALL PREDICTORS      ####
####    NEW STRATEGY-- LOOPING THROUGH PATHOGEN DATAFRAMES    ####
####              EXTRA ANALYSIS FOR LANCET GH 
####      SENSITIVITY ANALYSIS W/O AGE GROUP ASSUMPTIONS      ####
####                     JUNE 23, 2021                        ####
##################################################################   
 
# recreate datasets using new age groups (age group 3)-- OVERWRITING PREVIOUS DATAFRAMES!
 # dataframe for each stratification (age group and child mortality level)
 
 # age group (age_group: young, mixed, old; age_group_3: youngest, middle)
   data_young <- case_control_full[ which(case_control_full$age_group_3 == "youngest" | case_control_full$age_group_3 == 'middle'), ]
   data_old   <- case_control_full[ which(case_control_full$age_group_3 == "oldest"), ]
   data_youngest   <- case_control_full[ which(case_control_full$age_group_3 == "youngest"), ]
   data_middle     <- case_control_full[ which(case_control_full$age_group_3 == "middle"), ]
 
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
   

     
#### RUNNING MODELS- **NEW METHOD** ALL PREDICTORS #### 
     
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
    # save (so I can extract I2 data)
      write.csv(ma_path_unadj_results, "/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/Julia/Results/ma_path_unadj_results.csv")
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
                                   'data_young_high_crypto', 
                                   #'data_young_high_ehist', 
                                   'data_young_high_giard',
                                   'data_young_high_rota_pre', 
                                   'data_young_high_etec_st', 'data_young_high_etec_lt', 'data_young_high_etec_unkn',
                                   #'data_young_high_epec_a', 'data_young_high_epec_t', 
                                   'data_young_high_epec_unkn')
   # labels for results
     data_labels_young_high_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                     'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                                     'crypto', 
                                     #'ehist', 
                                     'giard',
                                     'rota_pre', 
                                     'etec_st', 'etec_lt', 'etec_unkn',
                                     #'epec_a', 'epec_t', 
                                     'epec_unkn')
     
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
                                       'data_young_vlow_low_campy', 'data_young_vlow_low_chol', 
                                       'data_young_vlow_low_epec',  'data_young_vlow_low_etec', 'data_young_vlow_low_salm', 'data_young_vlow_low_shig', 
                                       'data_young_vlow_low_crypto', 
                                       #'data_young_vlow_low_ehist', 
                                       'data_young_vlow_low_giard',
                                       'data_young_vlow_low_rota_pre', 'data_young_vlow_low_rota_post',
                                       'data_young_vlow_low_epec_a', 'data_young_vlow_low_epec_t', 'data_young_vlow_low_epec_unkn',
                                       'data_young_vlow_low_etec_st', 'data_young_vlow_low_etec_lt', 'data_young_vlow_low_etec_unkn')
     
   # labels for results
     data_labels_young_vlow_low_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                         'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                                         'crypto', 
                                         #'ehist', 
                                         'giard',
                                         'rota_pre', 'rota_post',
                                         'epec_a', 'epec_t', 'epec_unkn',
                                         'etec_st', 'etec_lt', 'etec_unkn')
     
   # model
     ma_path_young_vlow_low_all <- list()
     for(i in seq_along(data_list_young_vlow_low_all)){ 
       ma_path_young_vlow_low_all[[i]] <- meta_analysis_loop_all(get(data_list_young_vlow_low_all[i]))
     }  
   # examine results  
     for (i in 1:21){
       print(ma_path_young_vlow_low_all[[i]], digits=2)
     }   
     # extract estimate and combine results  
       magic_for(silent = TRUE)
       for (i in 1:21) {
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
                            'data_old_aero', 'data_old_campy', 'data_old_chol', 'data_old_epec',  'data_old_etec', 'data_old_salm', 'data_old_shig',
                            'data_old_crypto', 'data_old_ehist', 'data_old_giard',
                            'data_old_rota_pre', 
                            'data_old_epec_a', 'data_old_epec_t', 'data_old_epec_unkn',
                            'data_old_etec_st', 'data_old_etec_lt', 'data_old_etec_unkn')
   # labels for results
     data_labels_old_all <- c('adeno', 'astro', 'noro', 'rota',
                              'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig',
                              'crypto', 'ehist', 'giard',
                              'rota_pre', 
                              'epec_a', 'epec_t', 'epec_unkn',
                              'etec_st', 'etec_lt', 'etec_unkn')
   # model
     ma_path_old_all <- list()
     for(i in seq_along(data_list_old_all)){ 
       ma_path_old_all[[i]] <- meta_analysis_loop_all(get(data_list_old_all[i]))
     }  
   # examine results  
     for (i in 1:21){
       print(ma_path_old_all[[i]], digits=2)
     }   
   # extract estimate and combine results  
     magic_for(silent = TRUE)
     for (i in 1:21) {
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
     
     
     
### youngest/high child mort levels ### 
     
   # data list to loop through  
     data_list_youngest_high_all <- c('data_youngest_high_adeno', 'data_youngest_high_astro', 'data_youngest_high_noro', 'data_youngest_high_rota', 'data_youngest_high_sapo',
                                      'data_youngest_high_aero', 'data_youngest_high_campy', 'data_youngest_high_chol', 'data_youngest_high_epec', 'data_youngest_high_etec', 'data_youngest_high_salm', 'data_youngest_high_shig', 
                                      'data_youngest_high_crypto', 
                                      #'data_youngest_high_ehist', 
                                      'data_youngest_high_giard', 
                                      'data_youngest_high_rota_pre',
                                      'data_youngest_high_epec_unkn',
                                      'data_youngest_high_etec_st', 'data_youngest_high_etec_lt', 'data_youngest_high_etec_unkn')
     
   # labels for results
     data_labels_youngest_high_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                        'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                                        'crypto', 
                                        #'ehist', 
                                        'giard', 
                                        'rota_pre',
                                        'epec_unkn',
                                        'etec_st', 'etec_lt', 'etec_unkn')
     
   # model
     ma_path_youngest_high_all <- list()
     for(i in seq_along(data_list_youngest_high_all)){ 
       ma_path_youngest_high_all[[i]] <- meta_analysis_loop_all(get(data_list_youngest_high_all[i]))
     }  
   # examine results  
     for (i in 1:19){
       print(ma_path_youngest_high_all[[i]], digits=2)
     }   
   # extract estimate and combine results  
     magic_for(silent = TRUE)
     for (i in 1:19) {
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
     
     
     
### middle/high child mort levels ### 
     
   # data list to loop through  
     data_list_middle_high_all <- c('data_middle_high_adeno', 'data_middle_high_astro', 'data_middle_high_noro', 'data_middle_high_rota', 'data_middle_high_sapo',
                                    'data_middle_high_aero', 'data_middle_high_campy', 'data_middle_high_chol', 'data_middle_high_epec', 'data_middle_high_etec', 'data_middle_high_salm', 'data_middle_high_shig', 
                                    'data_middle_high_crypto', 
                                    #'data_middle_high_ehist', 'data_middle_high_giard', 
                                    'data_middle_high_rota_pre',
                                    #'data_middle_high_epec_a', 'data_middle_high_epec_t', 
                                    'data_middle_high_epec_unkn',
                                    'data_middle_high_etec_st', 'data_middle_high_etec_lt', 'data_middle_high_etec_unkn')
     
   # labels for results
     data_labels_middle_high_all <- c('adeno', 'astro', 'noro', 'rota', 'sapo',
                                      'aero', 'campy', 'chol', 'epec',  'etec', 'salm', 'shig', 
                                      'crypto', 
                                      #'ehist', 'giard', 
                                      'rota_pre',
                                      #'epec_a', 'epec_t', 
                                      'epec_unkn',
                                      'etec_st', 'etec_lt', 'etec_unkn')
     
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
     data_list_youngest_vlow_low_all <- c('data_youngest_vlow_low_adeno', 
                                          #'data_youngest_vlow_low_astro', 
                                          'data_youngest_vlow_low_noro', 'data_youngest_vlow_low_rota', 'data_youngest_vlow_low_sapo',
                                          'data_youngest_vlow_low_campy', 'data_youngest_vlow_low_epec',  'data_youngest_vlow_low_etec', 
                                          'data_youngest_vlow_low_salm', 
                                          #'data_youngest_vlow_low_shig', 
                                          'data_youngest_vlow_low_crypto', 'data_youngest_vlow_low_giard', 
                                          'data_youngest_vlow_low_rota_pre', 'data_youngest_vlow_low_rota_post',
                                          'data_youngest_vlow_low_epec_a', 'data_youngest_vlow_low_epec_t', 'data_youngest_vlow_low_epec_unkn',
                                          'data_youngest_vlow_low_etec_st', 'data_youngest_vlow_low_etec_lt') 
                                          #'data_youngest_vlow_low_etec_unkn')
     
   # labels for results
     data_labels_youngest_vlow_low_all <- c('adeno', 
                                            #'astro', 
                                            'noro', 'rota', 'sapo',
                                            'campy', 'epec',  'etec', 
                                            'salm', 
                                            #'shig', 
                                            'crypto', 'giard', 
                                            'rota_pre', 'rota_post',
                                            'epec_a', 'epec_t', 'epec_unkn',
                                            'etec_st', 'etec_lt')
                                            #'etec_unkn')
     
   # model
     ma_path_youngest_vlow_low_all <- list()
     for(i in seq_along(data_list_youngest_vlow_low_all)){ 
       ma_path_youngest_vlow_low_all[[i]] <- meta_analysis_loop_all(get(data_list_youngest_vlow_low_all[i]))
     }  
   # examine results  
     for (i in 1:17){
       print(ma_path_youngest_vlow_low_all[[i]], digits=2)
     }   
   # extract estimate and combine results  
     magic_for(silent = TRUE)
     for (i in 1:17) {
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
     data_list_middle_vlow_low_all <- c(#'data_middle_vlow_low_noro', 
       'data_middle_vlow_low_rota', 
       'data_middle_vlow_low_campy', 'data_middle_vlow_low_chol', 
       'data_middle_vlow_low_epec',  'data_middle_vlow_low_etec', 'data_middle_vlow_low_shig',
       'data_middle_vlow_low_crypto', 
       'data_middle_vlow_low_rota_pre', 'data_middle_vlow_low_rota_post',
       'data_middle_vlow_low_epec_t', 'data_middle_vlow_low_epec_unkn',
       'data_middle_vlow_low_etec_st', 'data_middle_vlow_low_etec_lt', 'data_middle_vlow_low_etec_unkn')
     
   # labels for results
     data_labels_middle_vlow_low_all <- c(#'noro', 
       'rota', 
       'campy', 'chol',
       'epec',  'etec', 'shig',
       'crypto', 
       'rota_pre', 'rota_post',
       'epec_t', 'epec_unkn',
       'etec_st', 'etec_lt', 'etec_unkn')
     
   # model
     ma_path_middle_vlow_low_all <- list()
     for(i in seq_along(data_list_middle_vlow_low_all)){ 
       ma_path_middle_vlow_low_all[[i]] <- meta_analysis_loop_all(get(data_list_middle_vlow_low_all[i]))
     }  
   # examine results  
     for (i in 1:14){
       print(ma_path_middle_vlow_low_all[[i]], digits=2)
     }   
   # extract estimate and combine results  
     magic_for(silent = TRUE)
     for (i in 1:14) {
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
     

     
### combining all datasets
    ma_new2_combined_results <- rbind(ma_path_unadj_results, 
                                      ma_path_young_high_all_results,ma_path_young_vlow_low_all_results,
                                      ma_path_old_all_results,
                                      ma_path_youngest_high_all_results, ma_path_youngest_vlow_low_all_results,
                                      ma_path_middle_high_all_results, ma_path_middle_vlow_low_all_results)
  
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
     
     
     ma_new2_combined_results_wide <- ma_new2_combined_results_wide[, c(1,2,7,6,9,8,4,3,5)]
     ma_new2_combined_results_wide <- ma_new2_combined_results_wide %>%
       mutate(names =  factor(names, levels = ordered_path)) %>%
       arrange(names)
     
   # writing to csv
     write.csv(ma_new2_combined_results_wide, file = "/Users/juliabaker/OneDrive - Emory University/WHO BoD/OR Analysis/OR Deliverables/Julia/Results/ma_new2_combined_results_wide- age group 3.csv")
     
