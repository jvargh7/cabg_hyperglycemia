rm(list=ls());gc();source(".Rprofile")


library(janitor)

variables_metacabg <- readxl::read_excel("data/Stress Hyperglycemia Variable List.xlsx",sheet="METACABG 20230706")

metacabg <- readxl::read_excel(paste0(path_sh_folder,"/raw/METACABG days 1 and 2 ALL patients_7.6.2023.xlsx")) %>% 
  janitor::clean_names(.) %>% 
  rename_with(~ variables_metacabg$new_var[which(variables_metacabg$variable == .x)], 
              .cols = variables_metacabg$variable) %>% 
  mutate(event_name = case_when(event_name == "Screening/Randomization Visit" ~ "screening",
                                event_name == "CABG Day (OR-ICU arrival)" ~ "surgery",
                                event_name == "CABG Post-Operative Day 1" ~ "post1",
                                event_name == "CABG Post-Operative Day 2" ~ "post2"))

saveRDS(metacabg,paste0(path_metacabg_paper,"/working/metacabg_20230706.RDS"))


