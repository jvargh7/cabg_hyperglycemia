rm(list=ls());gc();source(".Rprofile")

bg_presurgery <- readRDS(paste0(path_metacabg_paper,"/working/raw/metabocabg_20230831.RDS"))  %>% 
  dplyr::filter(event_name == "surgery") %>% 
  dplyr::select(record_id,bg_admission,bg_pre_op) %>% 
  mutate(bg_admission_timestamp = "",
         bg_pre_op_timestamp = "")


writexl::write_xlsx(bg_presurgery,paste0(path_sh_folder,"/working/chd06_TEMPLATE Blood Glucose at Admission and Preop.xlsx"))
