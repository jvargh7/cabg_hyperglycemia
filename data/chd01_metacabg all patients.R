rm(list=ls());gc();source(".Rprofile")


library(janitor)

variables_metabocabg <- readxl::read_excel("data/CABG Hyperglycemia Variable List.xlsx",sheet="METABOCABG 20230831") %>% 
  dplyr::filter(!is.na(new_var))

metabocabg <- read_csv(paste0(path_sh_folder,"/raw/METABOCABG-CGMAnalysisForJithin_DATA_LABELS_2023-08-31_1425.csv")) %>% 
  janitor::clean_names(.) %>% 
  dplyr::select(one_of(variables_metabocabg$variable)) %>% 
  rename_with(~ variables_metabocabg$new_var[which(variables_metabocabg$variable == .x)], 
              .cols = variables_metabocabg$variable) %>% 
  mutate(event_name = case_when(event_name == "Screening/Randomization Visit" ~ "screening",
                                event_name == "CABG Day (OR-ICU arrival)" ~ "surgery",
                                event_name == "CABG Post-Operative Day 1" ~ "post1",
                                event_name == "CABG Post-Operative Day 2" ~ "post2",
                                event_name == "CABG Post-Operative Day 3" ~ "post3",
                                event_name == "CABG Post-Operative Day 4" ~ "post4",
                                event_name == "CABG Post-Operative Day 5" ~ "post5",
                                event_name == "CABG Post-Operative Day 6" ~ "post6",
                                event_name == "CABG Post-Operative Day 7" ~ "post7",
                                event_name == "CABG Post-Operative Day 8" ~ "post8",
                                event_name == "CABG Post-Operative Day 9" ~ "post9",
                                event_name == "CABG Post-Operative Day 10" ~ "post10",
                                event_name == "CABG Post-Operative Day 11" ~ "post11",
                                event_name == "CABG Post-Operative Day 12" ~ "post12",
                                event_name == "CABG Post-Operative Day 13" ~ "post13",
                                event_name == "CABG Post-Operative Day 14" ~ "post14",
                                event_name == "CABG Post-Operative Day 15" ~ "post15",
                                event_name == "CABG Post-Operative Day 16" ~ "post16",
                                event_name == "CABG Post-Operative Day 17" ~ "post17",
                                event_name == "CABG Post-Operative Day 18" ~ "post18",
                                event_name == "CABG Post-Operative Day 19" ~ "post19",
                                event_name == "Discharge" ~ "discharge",
                                event_name == "Follow up post-discharge" ~ "followup_postdischarge",
                                event_name == "Close-Out" ~ "closeout",
                                
                                )) %>% 
  mutate(surgery_start_time = case_when(record_id == "MCM045" & event_name == "surgery" ~ ymd_hms("2020-11-17 15:12:00"),
                                        record_id == "MCM061" & event_name == "surgery" ~ ymd_hms("2021-12-13 08:04:00"),
                                        # FZT (2023-08-03): Surgery date: 7/25/2022—Start time: 09:08 --- Stop time: 15:04 --- I updated the database accordingly.
                                        record_id == "MCM065" & event_name == "surgery" ~ ymd_hms("2022-07-25 09:08:00"),
                                        TRUE ~ surgery_start_time),
         surgery_end_time = case_when(record_id == "MCM045" & event_name == "surgery" ~ ymd_hms("2020-11-17 19:15:00"),
                                      record_id == "MCM061" & event_name == "surgery" ~ ymd_hms("2021-12-13 13:37:00"),
                                      record_id == "MCM065" & event_name == "surgery" ~ ymd_hms("2022-07-25 15:04:00"),
                                        TRUE ~ surgery_end_time))

saveRDS(metabocabg,paste0(path_metacabg_paper,"/working/raw/metabocabg_20230831.RDS"))


