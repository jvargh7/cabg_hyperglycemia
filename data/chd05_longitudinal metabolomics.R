rm(list=ls());gc();source(".Rprofile")


c18_feature_2 <- read_table(paste0(path_metabolomics_folder,"/c18/feature_2.txt")) %>% 
  pivot_longer(cols=-one_of("mz","time"),names_to=c("record_id","visit"),names_sep = "_",values_to="value") %>% 
  pivot_wider(names_from="visit",values_from="value") %>% 
  mutate(record_id = str_replace(record_id,"MCE011","MCG001"))  %>% 
  dplyr::select(record_id,everything())



hilic_feature_2 <- read_table(paste0(path_metabolomics_folder,"/hil/feature_2.txt")) %>% 
  pivot_longer(cols=-one_of("mz","time"),names_to=c("record_id","visit"),names_sep = "_",values_to="value") %>% 
  pivot_wider(names_from="visit",values_from="value") %>% 
  mutate(record_id = str_replace(record_id,"MCE011","MCG001")) %>% 
  dplyr::select(record_id,everything())

names(c18_feature_2)

saveRDS(c18_feature_2,paste0(path_metacabg_paper,"/working/data/c18_feature_2.RDS"))
saveRDS(hilic_feature_2,paste0(path_metacabg_paper,"/working/data/hilic_feature_2.RDS"))


write_csv(c18_feature_2,paste0(path_metacabg_paper,"/working/data/c18_feature_2.csv"))
write_csv(hilic_feature_2,paste0(path_metacabg_paper,"/working/data/hilic_feature_2.csv"))

# c18_class_2 <- read_table(paste0(path_metabolomics_folder,"/c18/class_2.txt")) %>% 
#   mutate(Sample = str_replace(Sample,"MCE011","MCG001")) %>% 
#   mutate(record_id = str_replace(Sample,"_V[1-5]",""),
#          visit_id = str_replace(Sample,"[A-Z0-9]+_",""))
# 
# hilic_class_2 <- read_table(paste0(path_metabolomics_folder,"/hil/class_2.txt")) %>% 
#   mutate(Sample = str_replace(Sample,"MCE011","MCG001")) %>% 
#   mutate(record_id = str_replace(Sample,"_V[1-5]",""),
#          visit_id = str_replace(Sample,"[A-Z0-9]+_",""))


