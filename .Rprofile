library(tidyverse)
library(lubridate)
library(hms)


path_cabg_sharepoint_folder <- "C:/Cloud/Emory University/Smith, Ryan - Emory_Pasquel_F_CABG_Hyperglycemia" 
path_sh_folder <- paste0(path_cabg_sharepoint_folder,"/Glucose and Insulin Data")
path_metabolomics_folder <- paste0(path_cabg_sharepoint_folder,"/New 2023 Analysis/Column")
path_sh_paper <- "C:/Cloud/OneDrive - Emory University/Papers/CABG Stress Hyperglycemia"
path_metacabg_paper <- "C:/Cloud/OneDrive - Emory University/Papers/CABG CGM Metabolomics"

tir_low = 70
tir_high = 140

low_glucose_cutoff = 40

excluded_cgm_record_ids = c("MCM013","MCM011","MCM043")
excluded_metabocabg_record_ids = c("MCG002","MCM020","MCM025","MCM043","MCM053","MCM057","MCM058")
# Screen fail: MCG002, MCM020, MCM043, MCM053, MCM057, MCM058
# Withdrew consent: MCM025
