# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





set_assessment_tool<-function(w) {
 #print(".......................................... [fisheryFun.r] --> reload_fleetsegment_info()", quote=F)
    index_to_load = -1
  selected <- gtkComboBoxGetActiveText(combo_assessment_tool)
  index_to_update <- which(ASSESSMENT_TOOLS == selected)
   if (index_to_update != 5) {
   gtkNotebookSetCurrentPage(BMTnotebook_assessment, (index_to_update-1)) 
  
   for (ntools in 0:(length(ASSESSMENT_TOOLS)-2)) {
       if (ntools != (index_to_update-1)) {
           gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook_assessment, ntools), F)
       } else {
           gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook_assessment, ntools), T)
       }
   }   
   
    } else {
      gtkNotebookSetCurrentPage(BMTnotebook_assessment, 0) 
      gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook_assessment, 0), F)
         gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook_assessment, 1), F)
            gtkWidgetSetSensitive( gtkNotebookGetNthPage(BMTnotebook_assessment, 2), F)
               gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook_assessment, 3), F)
    }
    
    
 selected_spe <- gtkComboBoxGetActiveText(combo_species)
    index_to_update_spe <- which(BMT_SPECIES == selected_spe) 
    
  temp_asse <- mat_cfg_assessment_tools[[index_to_update_spe]]
   
    if (!is.null(temp_asse) ) {
    
    if (as.character(temp_asse[2,2]) == selected ) {
    
    if (selected == "VIT") {
    gtkEntrySetText(entry_assessment_minAge_VIT,  temp_asse[2,6] )
    gtkEntrySetText(entry_assessment_maxAge_VIT,  temp_asse[2,7])
    
     if (as.logical(temp_asse[2,3])) {
            gtkToggleButtonSetActive(bmt_chk_assessment_sex_VIT, T)
       } else {
            gtkToggleButtonSetActive(bmt_chk_assessment_sex_VIT, F)
       } 
       
   if (as.logical(temp_asse[2,5])) {
            gtkToggleButtonSetActive(bmt_chk_assessment_discard_VIT, T)
       } else {
            gtkToggleButtonSetActive(bmt_chk_assessment_discard_VIT, F)
       }   
    
    gtkComboBoxSetActive(combo_years_VIT, 0 )
    
    
      temp_VIT <- mat_cfg_VIT_list[[index_to_update_spe]]
  
   matrix_VITpath <<- data.frame(t(temp_VIT[3:6, -1]) )
	  matrix_VITpath <<- matrix_VITpath[matrix_VITpath[,1] != "", ]
	 matrix_VITpath[,1] <<- BMT_YEARS_SIMULATION
	  colnames(matrix_VITpath) <<-  c("Year", "Combined", "Males", "Females")
    
    
     if ( gtkToggleButtonGetActive(radio_combined) ) {   
          reload_VITpaths_combined_table()
     } else if (gtkToggleButtonGetActive(radio_males) ) {
          reload_VITpaths_males_table()
     } else if (gtkToggleButtonGetActive(radio_females) ) {
          reload_VITpaths_females_table()
     }
    
    } else  if (selected == "XSA") {
        gtkEntrySetText(entry_assessment_numberAgeClasses_XSA, temp_asse[2,6])
         gtkEntrySetText(entry_assessment_XSA_results, temp_asse[4,2]) 
         gtkEntrySetText(entry_assessment_assess_Catchfleet, temp_asse[4,3]) 
         gtkEntrySetText(entry_assessment_assess_Ffleet, temp_asse[4,4]) 
         gtkEntrySetText(entry_assessment_assess_RPs, temp_asse[4,5])  
        
    } else  if (selected == "SURBA") {
         gtkEntrySetText(entry_assessment_numberAgeClasses_SURBA, temp_asse[2,3])
         gtkEntrySetText(entry_assessment_SURBAresults, temp_asse[4,2]) 
    
    } else  if (selected == "from Report") {
          gtkEntrySetText(entry_assessment_numberAgeClasses_Report, temp_asse[2,6])
         gtkEntrySetText(entry_assessment_Reportresults, temp_asse[4,2]) 
         
    }
    } else {
          clear_bio_assessment_items()
    
    }
    
    }
  
   

} 
