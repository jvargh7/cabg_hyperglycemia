path_cgm_repo <- "C:/code/nutritional_epidemiology/cgm"

source("functions/cgmanalysis_sh.R")




# df <- readxl::read_excel(paste0(path_sh_folder,"/CGM data for metabolomics/MCE008.xlsx"),skip = 5)
# df <- read_csv(paste0(path_sh_folder,"/CGM data for metabolomics/MCM056.csv"),skip = 0)

inputdirectory = paste0(path_sh_folder,"/Glucose and Insulin Data/raw/CGM/")
outputdirectory = paste0(path_sh_folder,"/Glucose and Insulin Data/working/")
outputname = paste0("cgm_summary_",Sys.Date())

log_file = paste0("preprocessing/shpre03_cgm data.log")
cat("\n","Date: ",as.character(Sys.Date()),file=log_file,append=TRUE)

cgm_summary <- cgmanalysis_sh(inputdirectory,
                                        outputdirectory,
                                        outputname,log_file=log_file,
                                        magedef = "1sd",
                                        aboveexcursionlength = 15,
                                        belowexcursionlength = 15)

rownames(cgm_summary) <- NULL
saveRDS(cgm_summary,paste0(path_sh_folder,"/Glucose and Insulin Data/working/cgm_summary.RDS"))

cgm_summary %>% 
  mutate_at(vars(contains("percent")),~as.numeric(.)) %>% 
  mutate(percent_time_180_250 = percent_time_over_180 - percent_time_over_250,
         percent_time_54_70 = percent_time_under_70 - percent_time_under_54) %>% 
  dplyr::select(subject_id,date_cgm_placement,num_days_good_data,percent_cgm_wear,
                average_sensor,cv,gmi,percent_time_70_180,
                percent_time_over_250,percent_time_180_250,
                percent_time_under_54,percent_time_54_70) %>% 
  mutate_at(vars(-one_of(c("subject_id","date_cgm_placement"))),~round(as.numeric(.),2)) %>% 
  write_csv(.,paste0(path_sh_folder,"/Glucose and Insulin Data/working/cgm_summary for 10 AGP indicators.csv"))
