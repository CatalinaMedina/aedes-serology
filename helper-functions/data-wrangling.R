library(lubridate)
library(janitor)
library(readxl)
library(tidyverse)


process_padagodas_survey_data <- function(
    padagodas_survey_path,
    padagodas_repeat_ELISA_path,
    padagodas_coordinates_path,
    padagodas_river_distance_path,
    padagodas_repeat_risk_factors_path,
    padagodas_land_type_path,
    modis_home_500m_path,
    modis_home_250m_path,
    modis_school_250m_path,
    near_river_cut_off = 300,
    modis_lag = 32
){
  
  saliva_ELISA_log_shift <- 0.15
  
  sched_visit_names <- c(
    "VISIT 1 DENGUE INDEX",
    "VISIT 2  DENGUE INDEX",
    "VISIT 3 DENGUE INDEX",
    "VISIT 4 INDEX"
  )
  
  low_count_schools <- c( # schools with <= 5 study participants with saliva measurement
    "Amnor Antarak Cheat School",
    "Ampe Phnom",
    "Aphivodth", 
    "Christian School Khum Skus",
    "Kdey Amnor", 
    "Kindergarten Asian",
    "Kindergarten Kampuchea Asian",
    "Kindergarten Wattbor",
    "Kindergarten​ Salon",
    "Meatophoum School",
    "No Name",
    "Panha Bot School",
    "Phoumi Thmei",
    "Sinsisamuth"
  ) 
  
  
  pagodas_survey_og <- read_excel(padagodas_survey_path)
  
  
  pagodas_survey_cleaned <- pagodas_survey_og %>% 
    clean_names() %>% 
    mutate(wet_season = ifelse(
      visit %in% c("VISIT 1 DENGUE INDEX", "VISIT 3 DENGUE INDEX"),
      yes = "yes",
      no = "no"
    )) %>% 
    mutate(visit_year = case_when(
      visit == "VISIT 1 DENGUE INDEX" ~ 0,
      visit %in% c("VISIT 2  DENGUE INDEX", "VISIT 3 DENGUE INDEX") ~ 1,
      visit == "VISIT 4 INDEX" ~ 2,
      TRUE ~ -99
    )) %>% 
    mutate(age = as.numeric(age)) %>% 
    mutate(age_cented3 = age - 3) %>%
    mutate(age_cented7 = age - 7) %>%
    mutate(age_cented = age - 6) %>%
    mutate(gender = factor(gender)) %>% 
    mutate(school_attended = str_to_title(school_attended)) %>% 
    mutate(specify_if_other = str_to_title(specify_if_other)) %>% 
    mutate(school_attended_all = ifelse(
      school_attended == "Other, Specify:",
      yes = specify_if_other,
      no = school_attended
    )) %>% 
    mutate(school_attended_all = factor(
      case_when(
        school_attended_all == "New Town" ~ "Kindergarten New Town",
        school_attended_all %in% c("Sin Si Sa Muth", "Sinsisamuth") ~ "Kindergarten Sin Si Sa Muth",
        school_attended_all == "Panha Bot" ~ "Panha Bot School",
        school_attended_all == "No Name" ~ "NA",
        TRUE ~ school_attended_all
      ))) %>% 
    mutate(school_attended_all = as.character(school_attended_all)) %>%
    mutate(in_school = factor(ifelse(
      school_attended_all != "Subject Not In School",
      yes = "yes",
      no = "no"
    ))) %>% 
    mutate(school_attended_red = factor(
      case_when(
        school_attended_all %in% low_count_schools ~ "other",
        TRUE ~ school_attended_all
      ),
      levels = c(
        "Subject Not In School",
        "Ang Seri",              
        "Anouvotta",             
        "Mroum Cheung",          
        "Mroum Tbaung",          
        "New Town",
        "Rkathom",
        "Santiphap",
        "Kindergarten Sin Si Sa Muth",
        "other"
      )
    )) %>% 
    select(-c("specify_if_other")) %>% 
    mutate(housing_type = factor(housing_type)) %>% 
    mutate(socioeconomic_class = factor(
      socioeconomic_class,
      levels = c("Very Poor", "Lower", "Middle", "Upper")
    )) %>% 
    mutate(visit_date = as.Date(visit_date)) %>% 
    mutate(sched_visit = ifelse(visit %in% sched_visit_names, yes = "yes", no = "no")) %>% 
    mutate(num_domestic_water_containers_in_home = as.numeric(num_domestic_water_containers_in_home)) %>% 
    mutate(num_toilets_in_home = as.numeric(num_toilets_in_home)) %>% 
    mutate(how_often_use_bed_net = factor(
      how_often_use_bed_net,
      levels = c(
        "Never", 
        "Rarely", 
        "Regularly/ Most of the time", 
        "All of the time"
      ),
      labels = c(
        "Never", 
        "Rarely", 
        "Most of the time", 
        "All of the time"
      )
    )) %>% 
    mutate(insecticide_spray = factor(insecticide_spray)) %>% 
    mutate(larvicide_applied_to_water = factor(larvicide_applied_to_water)) %>% 
    mutate(how_often_mosquito_coils_burned = factor(
      how_often_mosquito_coils_burned,
      levels = c(
        "Never", 
        "Rarely (about 1x/month)", 
        "Sometimes (about 1x/week)",
        "Often (about 3x/week)",
        "Daily"
      ),
      labels = c(
        "Never", 
        "~ 1x/month", 
        "~ 1x/week",
        "~ 3x/week",
        "Daily"
      )
    )) %>% 
    mutate(visit_date_abr = factor(zoo::as.yearmon(visit_date))) %>% 
    mutate(log_saliva_elisa = log(saliva_elisa_value + saliva_ELISA_log_shift)) 
  
  
  pagodas_visit5 <- read_csv(padagodas_repeat_ELISA_path, show_col_types = FALSE) %>% 
    select(study_id = ID, saliva_elisa_value = "Visit 5") %>% 
    mutate(saliva_elisa_value = as.numeric(ifelse(
      saliva_elisa_value == ".", 
      yes = NA, 
      no = saliva_elisa_value
    ))) %>% 
    mutate(log_saliva_elisa = log(saliva_elisa_value + saliva_ELISA_log_shift)) 
  
  
  pagodas_survey_cleaned_v1_v5 <- pagodas_survey_cleaned[!duplicated(pagodas_survey_cleaned$study_id), ] %>% 
    mutate(visit = "Visit 5") %>% 
    mutate(visit_date = as.Date("2020-07-01")) %>% 
    mutate(visit_date_abr = NA) %>% 
    mutate(wet_season = "yes") %>% 
    mutate(visit_year = 2) %>% 
    select(-c(saliva_elisa_value, log_saliva_elisa)) %>% 
    full_join(pagodas_visit5) %>% 
    select(colnames(pagodas_survey_cleaned)) %>% 
    bind_rows(pagodas_survey_cleaned) %>% 
    arrange(study_id, visit_date)
  
  
  pagodas_coord_og <- read_csv(padagodas_coordinates_path, show_col_types = FALSE)
  
  pagodas_coord_cleaned <- pagodas_coord_og %>%
    clean_names()
  
  pagodas_full <- full_join(pagodas_survey_cleaned_v1_v5, pagodas_coord_cleaned) %>% 
    filter(sched_visit == "yes") %>% 
    mutate(visit = case_when(
      visit == "VISIT 1 DENGUE INDEX" ~ "visit1",
      visit == "VISIT 2  DENGUE INDEX" ~ "visit2",
      visit == "VISIT 3 DENGUE INDEX" ~ "visit3",
      visit == "VISIT 4 INDEX" ~ "visit4",
      visit == "Visit 5" ~ "visit5",
      TRUE ~ "NA"
    ))
  
  pagodas_survey_with_river <- read_csv(padagodas_river_distance_path, show_col_types = FALSE) %>% 
    select(study_id = Study.ID, distance_to_river = distance) %>%
    mutate(distance_to_river_scaled = (distance_to_river - mean(distance_to_river)) / sd(distance_to_river)) %>% 
    mutate(close_to_river = factor(
      ifelse(distance_to_river <= near_river_cut_off, "yes", "no"), 
      levels = c("no", "yes")
    )) %>% 
    right_join(pagodas_full)
  
  
  pagodas_red <- pagodas_survey_with_river %>% 
    filter(!is.na(saliva_elisa_value)) %>% 
    filter(!is.na(visit_date)) %>% 
    filter(school_attended_all != "NA") %>% 
    mutate(num_domestic_water_containers_in_home = ifelse(
      visit != "visit1", 
      yes = NA, 
      no = num_domestic_water_containers_in_home
    )) %>% 
    mutate(num_toilets_in_home = ifelse(
      visit != "visit1", 
      yes = NA, 
      no = num_toilets_in_home
    )) %>% 
    mutate(how_often_use_bed_net = ifelse(
      visit != "visit1", 
      yes = NA, 
      no = as.character(how_often_use_bed_net)
    )) %>% 
    mutate(insecticide_spray = ifelse(
      visit != "visit1", 
      yes = NA, 
      no = as.character(insecticide_spray)
    )) %>%
    mutate(larvicide_applied_to_water = ifelse(
      visit != "visit1", 
      yes = NA, 
      no = as.character(larvicide_applied_to_water)
    )) %>%
    mutate(how_often_mosquito_coils_burned = ifelse(
      visit != "visit1", 
      yes = NA, 
      no = as.character(how_often_mosquito_coils_burned)
    ))
  
  
  time_varying <- read_xlsx(padagodas_repeat_risk_factors_path) %>% 
    select(
      study_id = "Study ID", 
      visit = "Event Name",
      num_domestic_water_containers_in_home = "1. How many domestic water containers are in the subject's home?",
      num_toilets_in_home = "2. How many toilets are in the subject's home?",
      how_often_use_bed_net = "3. How often does the subject use a bed net?",
      insecticide_spray = "4. Is the subject's home sprayed with insecticide?",
      larvicide_applied_to_water = "5. Is larvicide applied to water containers at the subject's home?",
      how_often_mosquito_coils_burned = "6. How often are mosquito coils burned in the subject's house?",
    ) %>% 
    mutate(visit = gsub(" ", "", str_to_lower(visit), fixed = TRUE)) %>%
    filter(visit != "visit6") %>% 
    filter(!(study_id == "100-0468" & visit == "visit4")) %>% # Missing data
    mutate(num_domestic_water_containers_in_home = as.numeric(num_domestic_water_containers_in_home)) %>% 
    mutate(num_toilets_in_home = as.numeric(num_toilets_in_home)) %>% 
    mutate(how_often_use_bed_net = as.character(factor(
      how_often_use_bed_net,
      levels = c(
        "Never", 
        "Rarely", 
        "Regularly/ Most of the time", 
        "All the time"
      ),
      labels = c(
        "Never", 
        "Rarely", 
        "Most of the time", 
        "All of the time"
      )
    ))) %>% 
    mutate(how_often_mosquito_coils_burned = as.character(factor(
      how_often_mosquito_coils_burned,
      levels = c(
        "Never", 
        "Rarely (about 1x/month)", 
        "Sometimes (about 1x/week)",
        "Often (about 3x/week)",
        "Daily"
      ),
      labels = c(
        "Never", 
        "~ 1x/month", 
        "~ 1x/week",
        "~ 3x/week",
        "Daily"
      )
    )))
  
  time_varying_vars <- c("num_domestic_water_containers_in_home", "num_toilets_in_home", "how_often_use_bed_net", "insecticide_spray", "larvicide_applied_to_water", "how_often_mosquito_coils_burned")
  
  
  pagodas_red <- pagodas_red %>% 
    filter(visit != "visit1") %>% 
    select(-time_varying_vars) %>% 
    left_join(time_varying) %>% 
    select(colnames(pagodas_red)) %>% 
    bind_rows(pagodas_red %>% filter(visit == "visit1")) %>% 
    mutate(visit = factor(visit)) %>% 
    mutate(visit5 = factor(ifelse(visit != "visit5", "no", "yes"))) %>% 
    mutate(wet_season = factor(wet_season, levels = c("no", "yes"))) %>% 
    filter(!is.na(num_domestic_water_containers_in_home)) %>% 
    mutate(how_often_use_bed_net = factor(
      how_often_use_bed_net,
      levels = c(
        "Never", 
        "Rarely", 
        "Most of the time", 
        "All of the time"
      )
    )) %>% 
    mutate(insecticide_spray = factor(insecticide_spray)) %>% 
    mutate(larvicide_applied_to_water = factor(larvicide_applied_to_water)) %>% 
    mutate(how_often_mosquito_coils_burned = factor(
      how_often_mosquito_coils_burned,
      levels = c(
        "Never", 
        "~ 1x/month", 
        "~ 1x/week",
        "~ 3x/week",
        "Daily"
      )
    ))
  
  
  pagodas_red <- read_csv(padagodas_land_type_path) %>% 
    select(-c(X, Y, lon, lat, elevation)) %>% 
    clean_names() %>% 
    mutate(modal_land_type = factor(
      str_to_lower(modal_land_type), 
      levels = c("urban", "rice paddy", "cropland")
    )) %>% 
    right_join(pagodas_red, by = "study_id")
  
  
  #### Incorporate satellite data
  max_date_diff <- 60
  date_lag <- modis_lag
  
  
  sat_data500 <- process_modis_data(
    df_og = read_csv(modis_home_500m_path, show_col_types = FALSE), 
    sat_var_names = c("EVI", "NVI", "NFI"), # keep order that it appear in df
    sat_var_col_start = 7
  ) %>% 
    arrange(Study.ID, var_type, date)
  
  
  sat_df <- sat_data500
  df_og <- pagodas_red
  df_new <- cbind(
    df_og, 
    "EVI" = rep(NA, nrow(df_og)),
    "NFI" = rep(NA, nrow(df_og)),
    "NVI" = rep(NA, nrow(df_og))
  )
  
  num_vars <- 3
  var_types <- levels(factor(sat_df$var_type))
  
  
  for(i in 1:nrow(df_og)) {
    df_id <- sat_df %>% 
      filter(Study.ID == as.character(df_og[i, "study_id"]))
    
    for(j in 1:num_vars) {
      df_id_var <- df_id %>% 
        filter(var_type == var_types[j])
      
      if (sum(!is.na(df_id_var$sat_var_value)) > 0) {
        date_diff <- (df_og$visit_date[i] - days(date_lag)) - df_id_var$date
        curr <- df_id_var[which.min(ifelse(date_diff < 0, 1000, date_diff)), ]
        
        if (abs(curr$date - df_og$visit_date[i]) > max_date_diff) {
          print(paste0(
            "Warning: Study ID ", df_og[i, "study_id"], " does not have ", 
            var_types[j], " value available within ", max_date_diff, 
            " days [max_date_diff] of ", date_lag, " days [date_lag] prior to visit date!"
          ))
        } else {
          df_new[i, var_types[j]] <- curr$sat_var_value
        }
      }
    }
  }
  
  pagodas_red <- df_new %>% 
    rename(EVI500_home = EVI, NVI500_home = NVI, NFI500_home = NFI)
  
  
  #### Incorporate satellite data 
  sat_data250 <- process_modis_data(
    df_og = read_csv(modis_home_250m_path, show_col_types = FALSE), 
    sat_var_names = c("EVI", "NVI", "NFI"), # keep order that it appear in df
    sat_var_col_start = 7
  )
  
  
  sat_df <- sat_data250
  df_og <- pagodas_red
  df_new <- cbind(
    df_og, 
    "EVI" = rep(NA, nrow(df_og)),
    "NFI" = rep(NA, nrow(df_og)),
    "NVI" = rep(NA, nrow(df_og))
  )
  
  num_vars <- 3
  var_types <- levels(factor(sat_df$var_type))
  
  
  for(i in 1:nrow(df_og)) {
    df_id <- sat_df %>% 
      filter(Study.ID == as.character(df_og[i, "study_id"]))
    
    for(j in 1:num_vars) {
      df_id_var <- df_id %>% 
        filter(var_type == var_types[j])
      
      if (sum(!is.na(df_id_var$sat_var_value)) > 0) {
        date_diff <- (df_og$visit_date[i] - days(date_lag)) - df_id_var$date
        curr <- df_id_var[which.min(ifelse(date_diff < 0, 1000, date_diff)), ]
        
        if (abs(curr$date - df_og$visit_date[i]) > max_date_diff) {
          print(paste0(
            "Warning: Study ID ", df_og[i, "study_id"], " does not have ", 
            var_types[j], " value available within ", max_date_diff, 
            " days [max_date_diff] of ", date_lag, " days [date_lag] prior to visit date!"
          ))
        } else {
          df_new[i, var_types[j]] <- curr$sat_var_value
        }
      }
    }
  }
  
  pagodas_red <- df_new %>% 
    rename(EVI250_home = EVI, NVI250_home = NVI, NFI250_home = NFI)
  
  
  #### Incorporate satellite data 16 days
  sat_df <- sat_data250
  df_og <- pagodas_red
  df_new <- cbind(
    df_og, 
    "EVI" = rep(NA, nrow(df_og)),
    "NFI" = rep(NA, nrow(df_og)),
    "NVI" = rep(NA, nrow(df_og))
  )
  
  num_vars <- 3
  var_types <- levels(factor(sat_df$var_type))
  
  
  for(i in 1:nrow(df_og)) {
    df_id <- sat_df %>% 
      filter(Study.ID == as.character(df_og[i, "study_id"]))
    
    for(j in 1:num_vars) {
      df_id_var <- df_id %>% 
        filter(var_type == var_types[j])
      
      if (sum(!is.na(df_id_var$sat_var_value)) > 0) {
        date_diff <- (df_og$visit_date[i] - days(16)) - df_id_var$date
        curr <- df_id_var[which.min(ifelse(date_diff < 0, 1000, date_diff)), ]
        
        if (abs(curr$date - df_og$visit_date[i]) > max_date_diff) {
          print(paste0(
            "Warning: Study ID ", df_og[i, "study_id"], " does not have ", 
            var_types[j], " value available within ", max_date_diff, 
            " days [max_date_diff] of ", 16, " days [16] prior to visit date!"
          ))
        } else {
          df_new[i, var_types[j]] <- curr$sat_var_value
        }
      }
    }
  }
  
  pagodas_red <- df_new %>% 
    rename(EVI250_home16 = EVI, NVI250_home16 = NVI, NFI250_home16 = NFI)
  
  
  #### Incorporate satellite data 48 days

  sat_df <- sat_data250
  df_og <- pagodas_red
  df_new <- cbind(
    df_og, 
    "EVI" = rep(NA, nrow(df_og)),
    "NFI" = rep(NA, nrow(df_og)),
    "NVI" = rep(NA, nrow(df_og))
  )
  
  num_vars <- 3
  var_types <- levels(factor(sat_df$var_type))
  
  
  for(i in 1:nrow(df_og)) {
    df_id <- sat_df %>% 
      filter(Study.ID == as.character(df_og[i, "study_id"]))
    
    for(j in 1:num_vars) {
      df_id_var <- df_id %>% 
        filter(var_type == var_types[j])
      
      if (sum(!is.na(df_id_var$sat_var_value)) > 0) {
        date_diff <- (df_og$visit_date[i] - days(48)) - df_id_var$date
        curr <- df_id_var[which.min(ifelse(date_diff < 0, 1000, date_diff)), ]
        
        if (abs(curr$date - df_og$visit_date[i]) > max_date_diff) {
          print(paste0(
            "Warning: Study ID ", df_og[i, "study_id"], " does not have ", 
            var_types[j], " value available within ", max_date_diff, 
            " days [max_date_diff] of ", 48, " days [48] prior to visit date!"
          ))
        } else {
          df_new[i, var_types[j]] <- curr$sat_var_value
        }
      }
    }
  }
  
  pagodas_red <- df_new %>% 
    rename(EVI250_home48 = EVI, NVI250_home48 = NVI, NFI250_home48 = NFI)
  
  
  #### Incorporate school satellite data 
  sat_data250 <- process_modis_data(
    df_og = read_csv(modis_school_250m_path, show_col_types = FALSE), 
    sat_var_names = c("EVI", "NVI", "NFI"), # keep order that it appear in df
    sat_var_col_start = 6
  ) %>% 
    mutate(names = ifelse(
      names == "Kindergarten<U+200B> Salon", 
      yes = "Kindergarten​ Salon", 
      no = names
    ))
  
  
  sat_df <- sat_data250
  df_og <- pagodas_red
  df_new <- cbind(
    df_og, 
    "EVI" = rep(NA, nrow(df_og)),
    "NFI" = rep(NA, nrow(df_og)),
    "NVI" = rep(NA, nrow(df_og))
  )
  
  num_vars <- 3
  var_types <- levels(factor(sat_df$var_type))
  
  
  for(i in 1:nrow(df_og)) {
    if(df_og[i, "school_attended_all"] != "Subject Not In School") {
      df_id <- sat_df %>% 
        filter(names == as.character(df_og[i, "school_attended_all"]))
      
      for(j in 1:num_vars) {
        df_id_var <- df_id %>% 
          filter(var_type == var_types[j])
        
        if (sum(!is.na(df_id_var$sat_var_value)) > 0) {
          date_diff <- (df_og$visit_date[i] - days(date_lag)) - df_id_var$date
          curr <- df_id_var[which.min(ifelse(date_diff < 0, 1000, date_diff)), ]
          
          if (abs(curr$date - df_og$visit_date[i]) > max_date_diff) {
            print(paste0(
              "Warning: Study ID ", df_og[i, "study_id"], " does not have ", 
              var_types[j], " value available within ", max_date_diff, 
              " days [max_date_diff] of ", date_lag, " days [date_lag] prior to visit date!"
            ))
          } else {
            df_new[i, var_types[j]] <- curr$sat_var_value
          }
        }
      }
    }
  }
  
  pagodas_red <- df_new %>% 
    rename(EVI250_sch = EVI, NVI250_sch = NVI, NFI250_sch = NFI) %>% 
    mutate(EVI250_sch = ifelse(is.na(EVI250_sch), yes = 0, no = EVI250_sch)) %>% 
    mutate(NVI250_sch = ifelse(is.na(NVI250_sch), yes = 0, no = NVI250_sch)) %>% 
    mutate(NFI250_sch = ifelse(is.na(NFI250_sch), yes = 0, no = NFI250_sch))                 
  
  pagodas_red <- pagodas_red[!is.na(pagodas_red$NVI250_home), ]
  
  
  #### Incorporate school satellite data 16
  sat_data250 <- process_modis_data(
    df_og = read_csv(modis_school_250m_path, show_col_types = FALSE), 
    sat_var_names = c("EVI", "NVI", "NFI"), # keep order that it appear in df
    sat_var_col_start = 6
  ) %>% 
    mutate(names = ifelse(
      names == "Kindergarten<U+200B> Salon", 
      yes = "Kindergarten​ Salon", 
      no = names
    ))
  
  
  sat_df <- sat_data250
  df_og <- pagodas_red
  df_new <- cbind(
    df_og, 
    "EVI" = rep(NA, nrow(df_og)),
    "NFI" = rep(NA, nrow(df_og)),
    "NVI" = rep(NA, nrow(df_og))
  )
  
  num_vars <- 3
  var_types <- levels(factor(sat_df$var_type))
  
  
  for(i in 1:nrow(df_og)) {
    if(df_og[i, "school_attended_all"] != "Subject Not In School") {
      df_id <- sat_df %>% 
        filter(names == as.character(df_og[i, "school_attended_all"]))
      
      for(j in 1:num_vars) {
        df_id_var <- df_id %>% 
          filter(var_type == var_types[j])
        
        if (sum(!is.na(df_id_var$sat_var_value)) > 0) {
          date_diff <- (df_og$visit_date[i] - days(16)) - df_id_var$date
          curr <- df_id_var[which.min(ifelse(date_diff < 0, 1000, date_diff)), ]
          
          if (abs(curr$date - df_og$visit_date[i]) > max_date_diff) {
            print(paste0(
              "Warning: Study ID ", df_og[i, "study_id"], " does not have ", 
              var_types[j], " value available within ", max_date_diff, 
              " days [max_date_diff] of ", 16, " days [16] prior to visit date!"
            ))
          } else {
            df_new[i, var_types[j]] <- curr$sat_var_value
          }
        }
      }
    }
  }
  
  pagodas_red <- df_new %>% 
    rename(EVI250_sch16 = EVI, NVI250_sch16 = NVI, NFI250_sch16 = NFI) %>% 
    mutate(EVI250_sch16 = ifelse(is.na(EVI250_sch16), yes = 0, no = EVI250_sch16)) %>% 
    mutate(NVI250_sch16 = ifelse(is.na(NVI250_sch16), yes = 0, no = NVI250_sch16)) %>% 
    mutate(NFI250_sch16 = ifelse(is.na(NFI250_sch16), yes = 0, no = NFI250_sch16))                 
  
  
  pagodas_red %>% 
    mutate(lat_s = (lat - mean(lat)) / sd(lat)) %>% 
    mutate(lon_s = (lon - mean(lon)) / sd(lon)) %>% 
    mutate(time = as.numeric(case_when(
      visit == "visit1" ~ 1,
      visit == "visit2" ~ 2,
      visit == "visit3" ~ 3,
      visit == "visit4" ~ 4,
      visit == "visit5" ~ 5
    ))) %>% 
    mutate(visit_year_cat = factor(as.character(visit_year), levels = c("0", "1", "2")))
}

process_larval_house_survey_data <- function(
    cleaned_pagodas_data,
    house_coordinates_path,
    house_survey_path
){
  
  # Load house survey location data
  larval_lat_lo <- read_csv(house_coordinates_path, show_col_types = FALSE) %>% 
    select(
      larval_X = "X...1",
      larval_Y = "Y...2",
      larval_lon = "X...3",
      larval_lat = "Y...4",
      subj_id = "SUBJID"
    ) %>% 
    mutate(subj_id = as.character(as.numeric(subj_id))) %>% 
    filter(!duplicated(subj_id)) #subjects 389 and 716 had repeat measurements; 1st kept
  
  # Load house survey water container and larvae presence data and merge with location
  larval_hs <- read_xlsx(house_survey_path, na = c("", "*")) %>%
    select(
      subj_id = SUBJID,
      visit_year = Year,
      visit_month = Month,
      water_cont = WATERCON,
      larv_water_cont = WATERLARVAE,
      pci = PCI
    ) %>% 
    filter(!(is.na(visit_year) | is.na(visit_month))) %>%  # drops 43 rows
    mutate(subj_id = as.character(subj_id)) %>% 
    mutate(visit_date = ym(paste(visit_year, visit_month))) %>% 
    mutate(visit = factor(case_when(
      visit_year == 2018 & visit_month >= 5 & visit_month <= 9 ~ "visit1",
      visit_year == 2019 & !(visit_month >= 5 & visit_month <= 9) ~ "visit2",
      visit_year == 2019 & visit_month >= 5 & visit_month <= 9 ~ "visit3",
      visit_year == 2020 & !(visit_month >= 5 & visit_month <= 9) ~ "visit4"
    ))) %>% 
    mutate(larv_in_house = ifelse(water_cont == 0, yes = 0, no = 1)) %>%  # Presence of any larvae in any water containers in a house
    select(
      subj_id, visit, visit_date, larv_in_house, water_cont, larv_water_cont, pci
    ) %>% 
    left_join(y = larval_lat_lo) 
  
  # Prep temporary df with new columns
  pagodas_w_larval <- cleaned_pagodas_data %>% 
    mutate(ave_larv_cont = NA) %>% 
    mutate(per_larv_houses = NA) %>% 
    mutate(per_larv_cont = NA) %>% 
    mutate(ave_pci = NA)
  
  # Prep new cleaned_pagodas_data df
  pagodas_w_larv <- NA
  
  # Match house survey data to padagodas participants by visit and 250m buffer around house
  for(v in levels(pagodas_w_larval$visit)) {
    larval_hs_visit <- filter(larval_hs, visit == v)
    
    pagodas_visit <- filter(pagodas_w_larval, visit == v)
    
    for(i in 1:nrow(pagodas_visit)){
      dist_from_house <- sqrt(
        (larval_hs_visit$larval_X - pagodas_visit$x[i])^2 + 
          (larval_hs_visit$larval_Y - pagodas_visit$y[i])^2
      )
      
      hs_houses_in_buffer <- dist_from_house <= 250
      
      if(sum(hs_houses_in_buffer) > 0) {
        close_hs <- larval_hs_visit[hs_houses_in_buffer, ]
        
        pagodas_visit$per_larv_houses[i] <- 100 * sum(close_hs$larv_in_house) / nrow(close_hs)
        
        pagodas_visit$ave_larv_cont[i] <- mean(close_hs$water_cont)
        
        if (sum(close_hs$water_cont) == 0) {
          pagodas_visit$per_larv_cont[i] <- 0
        } else {
          pagodas_visit$per_larv_cont[i] <- 100 * sum(close_hs$larv_water_cont) / sum(close_hs$water_cont)
        }
        
        if (!all(is.na(close_hs$pci))) {
          pagodas_visit$ave_pci[i] <- mean(close_hs$pci, na.rm = TRUE)
        }
        
      } 
    }
    
    if(v == "visit1") {
      pagodas_w_larv <- pagodas_visit
    } else {
      pagodas_w_larv <- bind_rows(pagodas_w_larv, pagodas_visit)
    }
    
  }
  
  pagodas_w_larv %>% 
    filter(!is.na(ave_pci)) %>%  # 377 did not have a match in hs within 250m buffer
    arrange(study_id, visit)
}

process_adult_mosquito_trap_data <- function(
    cleaned_pagodas_data,
    adult_mosquito_trap_path
){
  
  pagodas_w_traps <- cleaned_pagodas_data %>% 
    filter(visit == "visit1")
  
  # Incorporate adult Aedes mosquito data
  adult_trapped <- read_csv(adult_mosquito_trap_path, show_col_types = FALSE) %>% 
    clean_names() %>% 
    select(
      adult_x = x, adult_y = y, adult_lon = e, adult_lat = n,
      date_placed, 
      total_aed, w1_aeg, w2_aed, w3_aed, w4_aed, w5_aed, w6_aed, w7_aed, w8_aed
    )
  
  ## Match nearest adult count to pagodas data
  pagodas_w_traps$ave_adult_count <- NA
  
  for(i in 1:nrow(pagodas_w_traps)){
    dist_from_house <- sqrt(
      (pagodas_w_traps$x[i] - adult_trapped$adult_x)^2 + 
        (pagodas_w_traps$y[i] - adult_trapped$adult_y)^2
    )
    
    traps_in_buffer <- dist_from_house <= 250
    
    if(sum(traps_in_buffer) > 0) {
      pagodas_w_traps$ave_adult_count[i] <- mean(
        adult_trapped$total_aed[traps_in_buffer], 
        na.rm = TRUE
      )
      
    } 
    
  }
  
  pagodas_w_traps %>% 
    filter(!is.na(ave_adult_count)) %>% 
    arrange(study_id)
}