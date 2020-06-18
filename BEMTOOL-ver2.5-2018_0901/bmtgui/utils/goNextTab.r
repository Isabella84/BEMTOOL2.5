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
goNextTab <- function(w) {

gtkButtonSetLabel(btn_next, "      Next >>    ")
gtkButtonSetLabel(btn_prev, "      << Prev    ")
    gtkWidgetSetSensitive(btn_next, T) 
        gtkWidgetSetSensitive(btn_prev, T)  
  # --------------------------------------------------------------------------------------------------------------------------------    
   # current: Case Study configuration   -->  go to: Biological Assessment settings
    if (gtkNotebookGetCurrentPage(BMTnotebook) == 0) {
suppressWarnings(source(paste(getwd(), "/bmtgui/utils/goTab2_action.r", sep="")))					
    if (go_quest) { 
     gtkNotebookSetCurrentPage(BMTnotebook, 1) 
    gtkLabelSetText(lbl_current_step, " Step 2 of 9 ")    
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
gtkWidgetSetSensitive(btn_load_CS, F) 
gtkWidgetSetSensitive(btn_load_LOADSCENARIO, F) 
     }
  # --------------------------------------------------------------------------------------------------------------------------------
  # current: Biological Assessment settings --> go to: Effort Landing data        
    } else if (gtkNotebookGetCurrentPage(BMTnotebook) == 1) {
    suppressWarnings(source(paste(getwd(), "/bmtgui/utils/goTab3_EffortData.r", sep="")))					
    if (go_quest) { 
     gtkLabelSetText(lbl_current_step, " Step 3 of 9")  
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
gtkWidgetSetSensitive(btn_load_CS, F) 
gtkWidgetSetSensitive(btn_load_LOADSCENARIO, F) 
gtkComboBoxSetActive(bmt_combo_fleetsegments, 0 )
}
  # --------------------------------------------------------------------------------------------------------------------------------
    # current: Effort Landing data --> go to: Economic time series        
   } else if (gtkNotebookGetCurrentPage(BMTnotebook) == 2) {
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
gtkWidgetSetSensitive(btn_load_CS, F)  
gtkWidgetSetSensitive(btn_load_LOADSCENARIO, F)  
gtkComboBoxSetActive(bmt_economicdata_fleet_combo, 0 )
  # --------------------------------------------------------------------------------------------------------------------------------
      # current: Economic time series  --> go to: Diagnosis results     
   } else if (gtkNotebookGetCurrentPage(BMTnotebook) == 3) {

   if (LOADED_CASESTUDY & phase == "SIMULATION" & !EX_DIAGNOSIS) {
           showQuestionYN_diagnosis()      
     } else if (LOADED_CASESTUDY & phase == "SIMULATION" & EX_DIAGNOSIS) {
# print("dentro goNextTab!")
#print(BMTnotebook)
   ts_ok <- check_bmt_simulation_economic_ts()
       if (ts_ok)  { 
suppressWarnings(source(paste(getwd(), "/bmtgui/diagnosis/reload_diagnosis.r", sep="")))		
     }
   }  
  # --------------------------------------------------------------------------------------------------------------------------------
  # current: Diagnosis results  --> go to:  Economic parameters for scenario 
   } else if (gtkNotebookGetCurrentPage(BMTnotebook) == 4) {
 
  FINAL_BMTCFG <- saveBMTCFG()
# print(FINAL_BMTCFG)
 write.table(FINAL_BMTCFG, paste(getwd(), "/bmtconfig.csv", sep=""), sep=";", row.names=F, col.names=F)

cfg <<- try(read.csv(file=paste(getwd(), "/bmtconfig.csv", sep=""), sep=";", na.strings = "NA", header=FALSE) )

if (class(cfg) != "try-error") {
nm <- as.character(cfg[,1])
cfg_rownames <<- nm
empty_indices <- which(nm!="")
nm <- nm[empty_indices]
cfg <<- cfg[,2:ncol(cfg)]
rownames(cfg)[empty_indices] <- nm
casestudy.endsimulation <<- as.numeric(as.character(cfg[rownames(cfg) == "casestudy.endsimulation",1])) 
casestudy.startsimulation <<- as.numeric(as.character(cfg[rownames(cfg) == "casestudy.startsimulation",1])) 
years <<- c(casestudy.startsimulation:casestudy.endsimulation )
simperiod <<- casestudy.endsimulation - casestudy.startsimulation + 1

casestudy.endforecast <<- as.numeric(as.character(cfg[rownames(cfg) == "casestudy.endforecast",1])) 
casestudy.startforecast <<- as.numeric(as.character(cfg[rownames(cfg) == "casestudy.startforecast",1])) 
years.forecast <<- c(casestudy.startforecast:casestudy.endforecast)
foreperiod <<-  casestudy.endforecast - casestudy.startforecast  +  1
 }
   
# if (LOADED_SCENARIO) {
#suppressWarnings(source(paste(getwd(), "/bmtgui/scenarios/load_scenario.r", sep="")))	 
#      } else {
#      
#      }  

suppressWarnings(source(paste(getwd(), "/bmtgui/utils/buildecomat.r", sep="")))		
                    
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
gtkWidgetSetSensitive(btn_load_CS, F) 
gtkWidgetSetSensitive(btn_load_LOADSCENARIO, T)

#print("------------------------------------------- mat_cfg_price (after goNextTab)")
#print(mat_cfg_price)

    # --------------------------------------------------------------------------------------------------------------------------------
      # current: Economic parameters for scenario --> go to: Management rules for the scenario
   } else if (gtkNotebookGetCurrentPage(BMTnotebook) == 5) {
   
      gtkButtonSetLabel(btn_next, "      See EXISTING SCENARIO    ")
   
     econom_params_all_zeros <- c()
   for (tbl in BMTCFG_SUBTABLES_ECONOMIC_PARAMS) {
               obj <- get(tbl) 
               if (!(tbl %in% c("bmt_fleet.labour_fuel" ,"bmt_fleet.labour_commercial" ,"bmt_fleet.labour_others"))) {
                 obj <- obj[,-1]
               }
         # if ( all(ifelse(obj[] == 0, T, F)) | all(!obj[])) {
        if ( all(ifelse(obj[] == 0, T, F)) ) {
              econom_params_all_zeros <- c(econom_params_all_zeros, T )
              } else {
              #print(paste(tbl, ": non ZERO !"))
              #print(obj)
              econom_params_all_zeros <-  c(econom_params_all_zeros, F)
        }
        
   }
   
   econom_params_all_zeros <- all(econom_params_all_zeros)
 
 # set status quo in the effort variables
 
EFFORT_NUMBER_list_fore <<-  vector(mode = "list", length = length(BMT_FLEETSEGMENTS))
EFFORT_NUMBER_list_fore <<-  vector(mode = "list", length = length(BMT_FLEETSEGMENTS))

   
    NUMBER_r4_matrix <- data.frame(matrix(0, nrow=length(BMT_YEARS_FORECAST), ncol=13))
   colnames(NUMBER_r4_matrix) <- c("year",MONTHS)
     NUMBER_r4_matrix$year <- BMT_YEARS_FORECAST  
   
    for (nfl in 1:length(BMT_FLEETSEGMENTS)) {     
   last_years_vess <- EFFORT_NUMBER_list[[nfl]]
    
    for (ri in 1:nrow(NUMBER_r4_matrix)) {
     NUMBER_r4_matrix[ri,2:13] <-  as.numeric(as.character(last_years_vess[nrow(last_years_vess),2:13]))                   
    } 
 
  EFFORT_NUMBER_list_fore[[nfl]] <<-  NUMBER_r4_matrix 
    bmt_fleet.NUMBER_r4 <<- NUMBER_r4_matrix 
   }
   
       bmt_fleet.NUMBER_r4 <<- EFFORT_NUMBER_list_fore[[1]] 
  
    DAY_r4_matrix <- data.frame(matrix(0, nrow=length(BMT_YEARS_FORECAST), ncol=13))
   colnames(DAY_r4_matrix) <- c("year",MONTHS)
     DAY_r4_matrix$year <- BMT_YEARS_FORECAST  
 
     for (nfl in 1:length(BMT_FLEETSEGMENTS)) {         
      last_years_days <- EFFORT_DAY_list[[nfl]]
        for (ri in 1:nrow(DAY_r4_matrix)) {     
        DAY_r4_matrix[ri,2:13] <-  as.numeric(as.character( last_years_days[nrow(last_years_days),2:13]))     
        }
    EFFORT_DAY_list_fore[[nfl]] <<-  DAY_r4_matrix  
            bmt_fleet.DAY_r4 <<- DAY_r4_matrix      
    }
    
        
   
   if (!econom_params_all_zeros) {
    gtkLabelSetText(lbl_current_step, " Step 7 of 9 ")  
     gtkNotebookSetCurrentPage(BMTnotebook, 6)   
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 0), F)  
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 1), F)          
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 2), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 3), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 4), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 5), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 6), T)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 7), T)       # cambiareeeeeeeeeeeeeeeeeee
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 8), F)
gtkWidgetSetSensitive(btn_runSIMULATION_bmt, F)
gtkWidgetSetSensitive(btn_runFORECAST_bmt, T) 
gtkWidgetSetSensitive(btn_load_CS, F) 
gtkWidgetSetSensitive(btn_load_LOADSCENARIO, F) 
  
        for (choice in BMT_FLEETSEGMENTS) { 
         gtkComboBoxRemoveText(bmt_combo_fleetsegments_effort_r4, 0)
    }
      
   for (choice in BMT_FLEETSEGMENTS) { 
        bmt_combo_fleetsegments_effort_r4$appendText(choice)        
    }
 
     for (choice in BMT_SPECIES) { 
        bmt_combo_TAC_species_r7$appendText(choice)        
    }

     for (choice in BMT_FLEETSEGMENTS) { 
         gtkComboBoxRemoveText(bmt_combo_fleetsegments_MEY_r8, 0)
    }
    gtkComboBoxRemoveText(bmt_combo_fleetsegments_MEY_r8, 0)
      
   for (choice in BMT_FLEETSEGMENTS) { 
        bmt_combo_fleetsegments_MEY_r8$appendText(choice)        
    }
        bmt_combo_fleetsegments_MEY_r8$appendText("ALL")        
    
       # suppressWarnings(source(paste(getwd(), "/bmtgui/scenarios/bmt_hr_change_effort.r", sep="")))		 
         bmt_reload_DAY_r4_table()
         bmt_reload_NUMBER_r4_table()
         reload_input_table_r5()
         reload_input_table_r6()
         reload_input_table_r6_species_settings()
         reload_input_table_r7()
         reload_input_table_r7_opt3_indices()
         reload_input_table_r7_opt3_tac()
       
         if (!LOADED_SCENARIO) {
            gtkComboBoxSetActive(bmt_combo_fleetsegments_MEY_r8, 0)  
            gtkComboBoxSetActive(bmt_combo_TAC_species_r7, 0)
            gtkComboBoxSetActive(bmt_combo_fleetsegments_effort_r4, 0)
       } else {
            loadRulesintoGUI()
       }
       
    } else {
         showError("Economic parameterization not completed!\nInsert the parameters or load an existing Scenario.")
    }
  # --------------------------------------------------------------------------------------------------------------------------------
        # current: Management rules for the scenario  --> go to: Forecast results
 # }  else if (LOAD_SCENARIO_ACTION) {
#	        	print("CHIAMO SCRIPT LOAD SCENARIO!")	        	
#						suppressWarnings(source(paste(getwd(), "/bmtgui/forecast/loadScenario_script.r", sep="")))   
#	

  
   casestudy_path <- str_replace_all(casestudy_path, "\\\\", "/" )

scenarios_dirs <<- list.dirs(path = casestudy_path, recursive=F,  full.names = FALSE)
scenarios_dirs <<- scenarios_dirs[str_detect(scenarios_dirs[], "HR")  ]

executed_scenarios_INFOs <<- file.info(list.dirs(path = casestudy_path, recursive=F,  full.names = F)  )
executed_scenarios_INFOs <- executed_scenarios_INFOs[which(str_detect(rownames(executed_scenarios_INFOs), "HR"))  ,]

executed_scenarios_INFOs <- executed_scenarios_INFOs[order(timeDate(executed_scenarios_INFOs$mtime)), ]

#print(paste("esiste scenario names?", exists("scenario_names")))

if (exists("scenario_names") ) {
print(scenario_names)
} else {
if (!exists("scenario_names")) {
scenario_names <<- c()
if (nrow(executed_scenarios_INFOs) >0) {
for (len in 1:nrow(executed_scenarios_INFOs)) {
vect_ <- str_split(as.character(rownames(executed_scenarios_INFOs)[len]), "/")
vect_ <- vect_[[1]]
scenario_names <<- c(scenario_names, vect_[length(vect_)]) 
}
} 
already_added <- scenario_names 
} 
}




#print(paste("esiste already_added?", exists("already_added")))
#
#if (exists("already_added") ) {
#print(already_added)
#}

#if (!exists("already_added")) {
#if (length(scenario_names) > 0 ) {
        for (choice in scenario_names) { 
    bmt_forecast_executed_scenarios$appendText(choice) 
		print(paste("adding:", choice))   
    }
#already_added <- scenario_names 
#}
#
#} else {
#
#scenario_to_add_in_list <- scenario_names[!(scenario_names %in% already_added) ]
#   for (choice in scenario_to_add_in_list) { 
#    bmt_forecast_executed_scenarios$appendText(choice) 
#		print(paste("adding:", choice))   
#    }
#}  

 # --------------------------------------------------------------------------------------------------------------------------------
      # current: Management rules for the scenario --> go to: Forecast results
      
 } else if (gtkNotebookGetCurrentPage(BMTnotebook) == 6) {
 
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
gtkWidgetSetSensitive(btn_runFORECAST_bmt, F) 
gtkWidgetSetSensitive(btn_load_CS, F) 
gtkWidgetSetSensitive(btn_load_LOADSCENARIO, F) 
 gtkWidgetSetSensitive(btn_next, F) 		    

phase <<- "FORECAST"

#	print("RIEMPIO IL MENU A TENDINA")

casestudy_path <- str_replace_all(casestudy_path, "\\\\", "/" )

scenarios_dirs <<- list.dirs(path = casestudy_path, recursive=F,  full.names = FALSE)
scenarios_dirs <<- scenarios_dirs[str_detect(scenarios_dirs[], "HR")  ]

executed_scenarios_INFOs <<- file.info(list.dirs(path = casestudy_path, recursive=F,  full.names = T)  )
executed_scenarios_INFOs <- executed_scenarios_INFOs[which(str_detect(rownames(executed_scenarios_INFOs), "HR"))  ,]
                              
executed_scenarios_INFOs <- executed_scenarios_INFOs[order(timeDate(executed_scenarios_INFOs$mtime)), ]

#gtkComboBoxSetActive(bmt_forecast_executed_scenarios, (which(scenario_names == casestudy_name) - 1))
#gtkComboBoxSetActive(.GlobalEnv$bmt_forecast_executed_scenarios, (nrow(executed_scenarios_INFOs)-1))

#name_this_scenario <- paste("HR", as.character(mat_cfg_scenario_settings_fore[2,2]), "-", as.character(mat_cfg_scenario_settings_fore[2,3]), sep="")

if (nrow(executed_scenarios_INFOs) > 0) {
    # gtkComboBoxSetActive(bmt_forecast_executed_scenarios, (which(scenario_names == casestudy_name) - 1)) 
 # rdata_path_fore <- paste(mat_cfg_general[1,3],  "/", name_this_scenario, "/", mat_cfg_general[1,2], " - BEMTOOL forecast ", name_this_scenario,".Rdata", sep="")
  
res_split <- str_split(rownames(executed_scenarios_INFOs)[nrow(executed_scenarios_INFOs)], "/")[[1]]
rdata_path_fore <- paste(rownames(executed_scenarios_INFOs)[nrow(executed_scenarios_INFOs)], "/", mat_cfg_general[1,2], " - BEMTOOL forecast ",   res_split[length(res_split)] ,".Rdata", sep="")
 
 
 
  if (SCENARIO_TO_LOAD_FROM_MENU == "") {
   SCENARIO_TO_LOAD_FROM_MENU <<- res_split[length(res_split)]
 }  
   
   #gtkComboBoxSetActive(bmt_forecast_executed_scenarios, (which(scenario_names == SCENARIO_TO_LOAD_FROM_MENU) - 1))
 # goNextTab() 
suppressWarnings(source(paste(getwd(), "/bmtgui/scenarios/loadScenario_script.r", sep="")) )		

  }  

#}


#else {
#if (exists("hbox_forecast")) { hbox_forecast$destroy() }
#  hbox_forecast <<- gtkHBox(homogeneous = FALSE, 5)  
# hbox_forecast$packStart(gtkLabel(" Any BEMTOOL scenario has been found for the loaded case study! "), expand = T, fill = T, padding = 10)      
#}
#}

  # suppressWarnings(source(paste(getwd(), "/bmtgui/scenarios/loadScenario_script.r", sep="")))					
#}

 # name_this_scenario <<-   paste("HR", as.character(mat_cfg_scenario_settings_fore[2,2]), "-", as.character(mat_cfg_scenario_settings_fore[2,3]), sep="")

 
#}
  # --------------------------------------------------------------------------------------------------------------------------------
   # current: Forecast results   --> go to: MCDA
   }  else if (gtkNotebookGetCurrentPage(BMTnotebook) == 7) {
 
# SCENARIO_TO_LOAD_FROM_MENU <<-  gtkComboBoxGetActiveText(bmt_forecast_executed_scenarios)
 
 if (SCENARIO_TO_LOAD_FROM_MENU != "" ) {
 
       suppressWarnings(source(paste(getwd(), "/bmtgui/scenarios/loadScenario_script.r", sep="")) )		
 }
 
gtkWidgetSetSensitive(btn_next, F)
  
  
 
 if (FALSE) {  
    gtkLabelSetText(lbl_current_step, " Step 9 of 9 ")  
     gtkNotebookSetCurrentPage(BMTnotebook, 8)   
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 0), F)  
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 1), F)          
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 2), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 3), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 4), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 5), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 6), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 7), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 8), T)
gtkWidgetSetSensitive(btn_runSIMULATION_bmt, F)
gtkWidgetSetSensitive(btn_runFORECAST_bmt, F) 
gtkWidgetSetSensitive(btn_load_CS, F) 
gtkWidgetSetSensitive(btn_load_LOADSCENARIO, F) 


}


# carico i risultati del forecast  
}
    
    
    
}

