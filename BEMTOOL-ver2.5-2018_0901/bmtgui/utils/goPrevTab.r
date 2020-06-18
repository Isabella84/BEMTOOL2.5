# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



#
#
#
#
#
#
#
#
#
#
goPrevTab <- function(w) {
  
  #gtkButtonSetLabel(btn_prev, "      << Prev    ")
  gtkButtonSetLabel(btn_next, "      Next >>    ")
gtkButtonSetLabel(btn_prev, "      << Prev    ")
  gtkWidgetSetSensitive(btn_prev, T)  
      gtkWidgetSetSensitive(btn_next, T)      
  
  # --------------------------------------------------------------------------------------------------------------------------------    
   # current: Biological Assessment settings  -->  go to:  Case Study configuration 

if (gtkNotebookGetCurrentPage(BMTnotebook) == 1) {
     gtkLabelSetText(lbl_current_step, " Step 1 of 9 ")  
     gtkNotebookSetCurrentPage(BMTnotebook, 0)   
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 0), T)  
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 1), F)          
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 2), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 3), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 4), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 5), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 6), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 7), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 8), F)
gtkWidgetSetSensitive(btn_runSIMULATION_bmt, F)
gtkWidgetSetSensitive(btn_runFORECAST_bmt, F)
  gtkWidgetSetSensitive(btn_prev, F)
#gtkComboBoxSetActive(bmt_combo_fleetsegments, 0 )
 
  # --------------------------------------------------------------------------------------------------------------------------------    
  # current: Effort Landing data  --> go to: Biological Assessment settings
   } else if (gtkNotebookGetCurrentPage(BMTnotebook) == 2) {
     gtkLabelSetText(lbl_current_step, " Step 2 of 9 ")  
     gtkNotebookSetCurrentPage(BMTnotebook, 1)   
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 0), F)  
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 1), T)          
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 2), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 3), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 4), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 5), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 6), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 7), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 8), F)
gtkWidgetSetSensitive(btn_runSIMULATION_bmt, F)
gtkWidgetSetSensitive(btn_runFORECAST_bmt, F)
  # --------------------------------------------------------------------------------------------------------------------------------    
  # current: Economic time series --> go to: Effort Landing data 
   } else if (gtkNotebookGetCurrentPage(BMTnotebook) == 3) {
   # gtkComboBoxSetActive(bmt_diagnosis_species, 0 )  
    gtkLabelSetText(lbl_current_step, " Step 3 of 9 ")  
     gtkNotebookSetCurrentPage(BMTnotebook, 2)   
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 0), F)  
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 1), F)          
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 2), T)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 3), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 4), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 5), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 6), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 7), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 8), F)
gtkWidgetSetSensitive(btn_runSIMULATION_bmt, F)
gtkWidgetSetSensitive(btn_runFORECAST_bmt, F)
  # --------------------------------------------------------------------------------------------------------------------------------    
 # current: Diagnosis results  --> go to: Economic time series   
   } else if (gtkNotebookGetCurrentPage(BMTnotebook) == 4) {  
#    gtkComboBoxSetActive(bmt_diagnosis_species, 0 )
    gtkButtonSetLabel(btn_next, "      See EXISTING DIAGNOSIS    ")
    
    gtkLabelSetText(lbl_current_step, " Step 4 of 9 ")  
     gtkNotebookSetCurrentPage(BMTnotebook, 3)   
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 0), F)  
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 1), F)          
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 2), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 3), T)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 4), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 5), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 6), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 7), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 8), F)
gtkWidgetSetSensitive(btn_runSIMULATION_bmt, T)
gtkWidgetSetSensitive(btn_runFORECAST_bmt, F)   
# --------------------------------------------------------------------------------------------------------------------------------    
      # current: Economic parameters for scenario --> go to: Diagnosis results 
   } else if (gtkNotebookGetCurrentPage(BMTnotebook) == 5) {    
#    gtkComboBoxSetActive(bmt_diagnosis_species, 0 )
    gtkLabelSetText(lbl_current_step, " Step 5 of 9 ")  
     gtkNotebookSetCurrentPage(BMTnotebook, 4)   
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 0), F)  
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 1), F)          
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 2), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 3), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 4), T)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 5), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 6), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 7), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 8), F)
gtkWidgetSetSensitive(btn_runSIMULATION_bmt, T)
gtkWidgetSetSensitive(btn_runFORECAST_bmt, F) 
# --------------------------------------------------------------------------------------------------------------------------------    
      # current: Management rules for the scenario --> go to: Economic parameters for scenario 
   } else if (gtkNotebookGetCurrentPage(BMTnotebook) == 6) {
#    gtkComboBoxSetActive(bmt_diagnosis_species, 0 )  

casestudy_path <- str_replace_all(casestudy_path, "\\\\", "/" )

scenarios_dirs <<- list.dirs(path = casestudy_path, recursive=F,  full.names = FALSE)
scenarios_dirs <<- scenarios_dirs[str_detect(scenarios_dirs[], "HR")  ]

executed_scenarios_INFOs <<- file.info(list.dirs(path = casestudy_path, recursive=F,  full.names = T)  )
executed_scenarios_INFOs <- executed_scenarios_INFOs[which(str_detect(rownames(executed_scenarios_INFOs), "HR")),]

SCENARIO_TO_LOAD_FROM_MENU <<- 0
 for (choice in 1:length(executed_scenarios_INFOs) ) { 
      gtkComboBoxRemoveText(bmt_forecast_executed_scenarios, (choice-1) )
    }

    gtkLabelSetText(lbl_current_step, " Step 6 of 9 ")  
     gtkNotebookSetCurrentPage(BMTnotebook, 5)   
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 0), F)  
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 1), F)          
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 2), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 3), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 4), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 5), T)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 6), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 7), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 8), F)
gtkWidgetSetSensitive(btn_runSIMULATION_bmt, F)
gtkWidgetSetSensitive(btn_runFORECAST_bmt, F) 
gtkWidgetSetSensitive(btn_load_LOADSCENARIO, T)  
# --------------------------------------------------------------------------------------------------------------------------------    
 # current: Forecast results   --> go to: Management rules for the scenario 
   } else if (gtkNotebookGetCurrentPage(BMTnotebook) == 7) { 
   
    gtkButtonSetLabel(btn_next, "      See EXISTING SCENARIO    ")
     
#    gtkComboBoxSetActive(bmt_diagnosis_species, 0 )
    gtkLabelSetText(lbl_current_step, " Step 7 of 6 ")  
     gtkNotebookSetCurrentPage(BMTnotebook, 6)   
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 0), F)  
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 1), F)          
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 2), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 3), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 4), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 5), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 6), T)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 7), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 8), F)
gtkWidgetSetSensitive(btn_runSIMULATION_bmt, F)
gtkWidgetSetSensitive(btn_runFORECAST_bmt, T) 
gtkWidgetSetSensitive(btn_load_LOADSCENARIO, F) 
# --------------------------------------------------------------------------------------------------------------------------------    
 # current: MCDA   --> go to: Forecast results
   } else if (gtkNotebookGetCurrentPage(BMTnotebook) == 8) {    
#    gtkComboBoxSetActive(bmt_diagnosis_species, 0 )
    gtkLabelSetText(lbl_current_step, " Step 8 of 9 ")  
     gtkNotebookSetCurrentPage(BMTnotebook, 7)   
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 0), F)  
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 1), F)          
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 2), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 3), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 4), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 5), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 6), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 7), T)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 8), F)
gtkWidgetSetSensitive(btn_runSIMULATION_bmt, F)
gtkWidgetSetSensitive(btn_runFORECAST_bmt, T)   
   }
    
    
    
}

