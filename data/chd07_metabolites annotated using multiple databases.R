# Pathway level information can inform us on the question on which pathways are involved. 

# From: Smith, Ryan <xxxx@emory.edu> 
# Sent: Saturday, October 14, 2023 6:25 PM
# Taking the top features and then annotating the top 10-100 features can identify metabolites 
# that may not fit in any common pathways, or identify novel metabolites that are unknown.

# Doing pathway analysis first allows us to get a better snapshot of what is occurring within 
# central metabolism, and can start informing us on where to focus our attention. 
# Doing individual annotation first helps to identify central metabolites, but not 
# necessarily if they’re involved with metabolism. 

# To question B, the simple answer is yes, but the complicated answer is more complex. 
# So I used 4 databases to query the dataset as a whole for both columns, KEGG, 
# the human metabolome database (HMDB), Lipid Maps, and the Toxin and Toxin Target Database (T3DB). 
# Depending on your interest, many other databases exist or can be created, such as the 
# Food Database as one example, and I've designed several databases regarding specific 
# metabolites of drugs for other studies.

 # In my shared folder, i have a file that says full annotation which are the 
# annotations using the 4 above databases, with each database separate and a file 
# that I made to merge the results. You all should have access to these files

# Mummichog, the software used for pathway analysis, was designed to use only 
# KEGG and the edinborough database which is nested within the software, 
# though that may change in future updates.


Sig_Annotation_negative_AllVisits <- readxl::read_excel(paste0(path_cabg_sharepoint_folder,
                                                               "/New 2023 Analysis/Full Annotator/Sig_Annotation_negative_AllVisits.xlsx")) %>% 
  dplyr::select(chemical_ID, Annotation.confidence.score, 
                one_of("mz...3","time...4","theoretical.mz",
                       "delta_ppm","Name","Formula","MonoisotopicMass","Adduct",
                       "P.value","adjusted.P.value","max.fold.change.log2")) %>% 
  rename(annotation_confidence_score = Annotation.confidence.score,
         mz = "mz...3",
         time = "time...4",
         theoretical_mz = theoretical.mz,
         name = Name,
         formula = Formula,
         mono_isotopic_mass = MonoisotopicMass,
         adduct = Adduct,
         p_value = P.value,
         adjusted_p_value = adjusted.P.value,
         max_fold_change_log2 = max.fold.change.log2)

Sig_Annotation_positive_AllVisits <- readxl::read_excel(paste0(path_cabg_sharepoint_folder,
                                                               "/New 2023 Analysis/Full Annotator/Sig_Annotation_positive_AllVisits.xlsx")) %>% 
  dplyr::select(chemical_ID, Annotation.confidence.score, 
                one_of("mz...3","time...4","theoretical.mz",
                       "delta_ppm","Name","Formula","MonoisotopicMass","Adduct",
                       "P.value","adjusted.P.value","max.fold.change.log2")) %>% 
  rename(annotation_confidence_score = Annotation.confidence.score,
         mz = "mz...3",
         time = "time...4",
         # theoretical_mz = theoretical.mz,
         name = Name,
         formula = Formula,
         mono_isotopic_mass = MonoisotopicMass,
         adduct = Adduct,
         p_value = P.value,
         adjusted_p_value = adjusted.P.value,
         max_fold_change_log2 = max.fold.change.log2)


write_csv(Sig_Annotation_negative_AllVisits,paste0(path_metacabg_paper,"/working/omics results/Sig_Annotation_negative_AllVisits.csv"))
write_csv(Sig_Annotation_positive_AllVisits,paste0(path_metacabg_paper,"/working/omics results/Sig_Annotation_positive_AllVisits.csv"))
