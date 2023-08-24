
rm(list=ls());gc();source(".Rprofile")

source("functions/agp_indicators.R")

id_vars = c("file","record_id","cgm_id","cgm_type","cgm1_status")

cgm_summary <- agp_indicators(readRDS(paste0(path_metacabg_paper,"/working/data/cgm harmonization.RDS")),
                           id_vars= id_vars) %>% 
  mutate_at(vars(-one_of(c(id_vars))),~round(as.numeric(.),2)) %>% 
  write_csv(.,paste0(path_metacabg_paper,"/working/data/cgm summary.csv"))
