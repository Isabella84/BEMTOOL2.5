# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


  clear_bio_assessment_items <- function() {
 
       gtkToggleButtonSetActive(bmt_chk_assessment_sex_VIT, F)
            gtkToggleButtonSetActive(bmt_chk_assessment_discard_VIT, F)

    gtkEntrySetText(entry_assessment_minAge_VIT, "") 
      gtkEntrySetText(entry_assessment_maxAge_VIT, "")
       
       gtkEntrySetText(entry_assessment_VITresults, "")  
  
  matrix_VITpath <<- NULL
VITpaths_maless <<- NULL
VITpaths_femaless <<- NULL
VITpaths_combineds <<- NULL

reload_VITpaths_combined_table()
reload_VITpaths_males_table()
reload_VITpaths_females_table()

 # gtkComboBoxSetActive(combo_assessment_tool, 0 ) 
gtkComboBoxSetActive(combo_years_VIT, 0 ) 

    gtkEntrySetText(entry_assessment_numberAgeClasses_XSA, "") 
        gtkEntrySetText(entry_assessment_XSA_results, "") 
            gtkEntrySetText(entry_assessment_assess_Catchfleet, "") 
                gtkEntrySetText(entry_assessment_assess_Ffleet, "") 
                       gtkEntrySetText(entry_assessment_assess_RPs, "") 
                       
            gtkEntrySetText(entry_assessment_numberAgeClasses_SURBA, "") 
        gtkEntrySetText(entry_assessment_SURBAresults, "") 
                                                     
        gtkEntrySetText(entry_assessment_numberAgeClasses_Report, "") 
       gtkEntrySetText(entry_assessment_Reportresults, "") 
       
}
 
  