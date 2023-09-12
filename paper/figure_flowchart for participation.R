rm(list=ls());gc();source(".Rprofile")

# Screening data --------
screening <- readRDS(paste0(path_metacabg_paper,"/working/data/screening_cs.RDS"))

screening %>% 
  dplyr::filter(!record_id %in% excluded_metabocabg_record_ids) %>% 
  group_by(type_of_participation,blinded_group) %>% 
  tally() 

with(screening,table(type_of_participation, blinded_group,useNA="always"))

# POCT glucose data ---------

bg_longitudinal <- readRDS(paste0(path_metacabg_paper,"/working/data/bg_longitudinal.RDS")) 

screening %>% 
  dplyr::filter(!record_id %in% excluded_metabocabg_record_ids) %>% 
  dplyr::filter(record_id %in% bg_longitudinal$record_id) %>% 
  group_by(type_of_participation,blinded_group) %>% 
  tally() 

# Metabolomics data ----------
c18_class_2 <- read_table(paste0(path_metabolomics_folder,"/c18/class_2.txt")) %>% 
  mutate(record_id = str_replace(Sample,"_V[1-5]",""))

screening %>% 
  dplyr::filter(!record_id %in% excluded_metabocabg_record_ids) %>% 
  dplyr::filter(record_id %in% c18_class_2$record_id) %>% 
  group_by(type_of_participation,blinded_group) %>% 
  tally() 



# CGM data ---------
metacabg_cgm <- readxl::read_excel("data/CABG Hyperglycemia Variable List.xlsx",sheet="METACABG CGM") %>% 
  dplyr::filter(is.na(cgm1_status) | !cgm1_status %in% c("not usable","screen failure")) %>% 
  dplyr::filter(!is.na(cgm1_id) | !is.na(cgm2_id)) %>% 
  left_join(screening %>% 
              dplyr::select(record_id,blinded_group),
            by = "record_id") 

metacabg_cgm %>%
  dplyr::filter(!record_id %in% excluded_metabocabg_record_ids) %>% 
  dplyr::filter(!is.na(cgm1_id)) %>% 
  group_by(type_of_participation,blinded_group) %>% 
  tally()

metacabg_cgm %>%
  dplyr::filter(!record_id %in% excluded_metabocabg_record_ids) %>% 
  dplyr::filter(!is.na(cgm2_id)) %>% 
  group_by(type_of_participation,blinded_group) %>% 
  tally()

metacabg_cgm %>%
  dplyr::filter(!record_id %in% excluded_metabocabg_record_ids) %>% 
  dplyr::filter(!is.na(cgm2_id),!is.na(cgm2_id)) %>% 
  group_by(type_of_participation,blinded_group) %>% 
  tally()
