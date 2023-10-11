# Refer Metabolomics Data Introduction_2023-08-30.pptx for description of annotation procedure (e.g. Slide: HIL_Mummichog Metabolite Match)

readxl::read_excel(paste0(path_cabg_sharepoint_folder,"/New 2023 Analysis/Signficant tables/HIL_Mummichog Metabolite Match.xlsx")) %>% 
  rename(mz = input_mz) %>% 
  write_csv(.,
          paste0(path_metacabg_paper,"/working/omics results/HIL_Mummichog Metabolite Match Visit 1 vs Visit 2.csv")
          )

readxl::read_excel(paste0(path_cabg_sharepoint_folder,"/New 2023 Analysis/Signficant tables/C18_Mummichog Metabolite Match.xlsx")) %>% 
  rename(mz = input_mz) %>% 
  write_csv(.,
            paste0(path_metacabg_paper,"/working/omics results/C18_Mummichog Metabolite Match Visit 1 vs Visit 2.csv")
  )
