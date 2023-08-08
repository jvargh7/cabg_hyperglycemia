rm(list=ls());gc();source(".Rprofile")


library(janitor)

variables_metacabg <- readxl::read_excel("data/CABG Hyperglycemia Variable List.xlsx",sheet="METACABG 20230706")

metacabg <- readxl::read_excel(paste0(path_sh_folder,"/raw/METACABG days 1 and 2 ALL patients_7.6.2023.xlsx")) %>% 
  janitor::clean_names(.) %>% 
  rename_with(~ variables_metacabg$new_var[which(variables_metacabg$variable == .x)], 
              .cols = variables_metacabg$variable) %>% 
  mutate(event_name = case_when(event_name == "Screening/Randomization Visit" ~ "screening",
                                event_name == "CABG Day (OR-ICU arrival)" ~ "surgery",
                                event_name == "CABG Post-Operative Day 1" ~ "post1",
                                event_name == "CABG Post-Operative Day 2" ~ "post2")) %>% 
  mutate(surgery_start_time = case_when(record_id == "MCM045" & event_name == "surgery" ~ ymd_hms("2020-11-17 15:12:00"),
                                        record_id == "MCM061" & event_name == "surgery" ~ ymd_hms("2021-12-13 08:04:00"),
                                        # FZT (2023-08-03): Surgery date: 7/25/2022—Start time: 09:08 --- Stop time: 15:04 --- I updated the database accordingly.
                                        record_id == "MCM065" & event_name == "surgery" ~ ymd_hms("2022-07-25 09:08:00"),
                                        TRUE ~ surgery_start_time),
         surgery_end_time = case_when(record_id == "MCM045" & event_name == "surgery" ~ ymd_hms("2020-11-17 19:15:00"),
                                      record_id == "MCM061" & event_name == "surgery" ~ ymd_hms("2021-12-13 13:37:00"),
                                      record_id == "MCM065" & event_name == "surgery" ~ ymd_hms("2022-07-25 15:04:00"),
                                        TRUE ~ surgery_end_time))

saveRDS(metacabg,paste0(path_metacabg_paper,"/working/raw/metacabg_20230706.RDS"))


