
# This comes from ~/preprocessing/chpre01_metacabg cross sectional data.R
# Recommend running the below line if last run was long ago
source("preprocessing/chpre01_metacabg cross sectional data.R")
rm(post1,post2,screening,metacabg,surgery)
surgery_timestamps <- readRDS(paste0(path_metacabg_paper,"/working/data/surgery_cs.RDS")) %>% 
  dplyr::select(record_id,surgery_start_time,surgery_end_time)

blood_draws_timestamps <- readxl::read_excel(paste0(path_sh_folder,"/raw/METABO CABG - Visits dates and times_JV_LGA.xlsx")) %>% 
  rename(record_id = 'Participant ID',
         visit1 = 'Visit 1',
         visit2 = 'Visit 2',
         visit3 = 'Visit 3',
         visit4 = 'Visit 4',
         visit5 = 'Visit 5') %>% 
  mutate(across(starts_with("visit"), .f = function(x) case_when(is.na(x) ~ "Missing",
                                                                 hour(x) == 0 & minute(x) == 0 ~ "Invalid",
                                                                 TRUE ~ "Valid"),
                .names = "valid_{col}")) %>% 
  left_join(surgery_timestamps,
            by = "record_id") %>% 
  mutate(diff_surgerystartmv1 = case_when(valid_visit1 == "Valid"  ~ as.numeric(difftime(surgery_start_time,visit1,units="mins")),
                                TRUE ~ NA_real_),
         diff_v2msurgerystart = case_when(valid_visit2 == "Valid" ~ as.numeric(difftime(visit2,surgery_start_time,units="mins")),
                                TRUE ~ NA_real_),
         diff_v3msurgeryend = case_when(valid_visit3 == "Valid" ~ as.numeric(difftime(visit3,surgery_end_time,units="mins")),
                                TRUE ~ NA_real_),
         diff_v4msurgeryend = case_when(valid_visit4 == "Valid"  ~ as.numeric(difftime(visit4,surgery_end_time,units="mins")),
                                TRUE ~ NA_real_),
         diff_v5msurgeryend = case_when(valid_visit5 == "Valid"  ~ as.numeric(difftime(visit5,surgery_end_time,units="days")),
                                        TRUE ~ NA_real_)
         ) %>% 
  mutate(flags_v1 = case_when(diff_surgerystartmv1 > 24*60 ~ "More than 24 hours",
                                 is.na(diff_surgerystartmv1) ~ "Missing data",
                                 TRUE ~ "Looks good"),
         flags_v2 = case_when(diff_v2msurgerystart > 2*60 ~ "More than 2 hours",
                                 is.na(diff_v2msurgerystart) ~ "Missing data",
                                 TRUE ~ "Looks good"),
         
         flags_v3 = case_when(diff_v3msurgeryend > 48*60 ~ "More than 48 hours",
                              diff_v3msurgeryend <24*60 ~ "Less than 24 hours",
                                 is.na(diff_v3msurgeryend) ~ "Missing data",
                                 TRUE ~ "Looks good"),
         
         flags_v4 = case_when(diff_v4msurgeryend > 96*60 ~ "More than 96 hours",
                              diff_v4msurgeryend < 72*60 ~ "Less than 72 hours",
                                 is.na(diff_v4msurgeryend) ~ "Missing data",
                                 TRUE ~ "Looks good"),
         
         flags_v5 = case_when(diff_v5msurgeryend > 30 ~ "More than 30 days",
                              is.na(diff_v5msurgeryend) ~ "Missing data",
                              TRUE ~ "Looks good")
         )


blood_draws_timestamps %>% 
  writexl::write_xlsx(path = paste0(path_sh_folder,"/working/chd04_QC of METABO CABG - Visits dates and times.xlsx"))

# Do not impute valid_visitX where value is "Missing" since no blood was drawn!
cleaned_blood_draws_timestamps <- blood_draws_timestamps %>% 
  mutate(visit1_imputed = case_when(valid_visit1 %in% c("Invalid") ~ surgery_start_time - minutes(round(median(diff_surgerystartmv1,na.rm=TRUE))),
                            TRUE ~ visit1),
         visit2_imputed = case_when(valid_visit2 %in% c("Invalid") ~ surgery_start_time + minutes(round(median(diff_v2msurgerystart,na.rm=TRUE))),
                                    TRUE ~ visit2),
         
         visit3_imputed = case_when(valid_visit3 %in% c("Invalid") ~ surgery_end_time + minutes(round(median(diff_v3msurgeryend,na.rm=TRUE))),
                                    TRUE ~ visit3),
         visit4_imputed = case_when(valid_visit4 %in% c("Invalid") ~ surgery_end_time + minutes(round(median(diff_v4msurgeryend,na.rm=TRUE))),
                                    TRUE ~ visit4),
         visit5_imputed = case_when(valid_visit5 %in% c("Invalid") ~ surgery_end_time + minutes(round(median(diff_v5msurgeryend*24*60,na.rm=TRUE))),
                                    TRUE ~ visit5)
         
         )

saveRDS(cleaned_blood_draws_timestamps,paste0(path_metacabg_paper,"/working/data/blood draws timestamps.RDS"))
write_csv(cleaned_blood_draws_timestamps,paste0(path_metacabg_paper,"/working/data/blood draws timestamps.csv"))
