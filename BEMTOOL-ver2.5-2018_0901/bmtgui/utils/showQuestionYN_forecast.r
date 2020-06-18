# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




showQuestionYN_forecast<-function() {


error_window <<- gtkWindow(show=FALSE)       
error_window["title"] <- "BEMTOOL 2.0"  
gtkWindowSetModal(error_window, TRUE)    
gtkWindowSetResizable(error_window, FALSE)
gtkWindowSetDeletable(error_window, T)                
gtkWindowSetDefaultSize(error_window, 300, 20)
gtkWindowSetPosition(error_window, 3)   
  
no_button <- gtkButton("         No         ")  
no_button$AddCallback("clicked", QuestionYN_forecast_NO)

yes_button <- gtkButton("         Yes         ") 
yes_button$AddCallback("clicked", QuestionYN_forecast_YES)

vbox <- gtkVBox()
hbox1 <- gtkHBox()
hbox1$PackStart(gtkLabel("Want to load a FORECAST found for this case study?\n\nPress YES to search existing Forecast, NO to run a new Scenario."), expand = TRUE, fill = T, padding = 40)
hbox2 <- gtkHBox(homogeneous = F)
hbox2$PackStart(yes_button, expand = T, fill = F, padding = 10)  
hbox2$PackStart(no_button, expand = T, fill = F, padding = 10) 
hbox_buttons_fore <- gtkHBox(homogeneous = F)
hbox_buttons_fore$PackStart(hbox2, expand =T, fill = F, padding = 10) 
vbox$PackStart(hbox1, expand = F, fill = FALSE, padding = 20)
vbox$PackStart(hbox_buttons_fore, expand = F, fill = FALSE, padding = 20)
error_window$add(vbox)
error_window$show()

return(error_window)

}


QuestionYN_forecast_NO<-function(w) {
     error_window$destroy()
}



QuestionYN_forecast_YES<-function(w) {
error_window$destroy()

if (FALSE) {
casestudy_path <- str_replace_all(casestudy_path, "\\\\", "/" )

scenarios_dirs <<- list.dirs(path = casestudy_path, recursive=F,  full.names = FALSE)
scenarios_dirs <<- scenarios_dirs[str_detect(scenarios_dirs[], "HR")  ]

scenario_names <<- c()
for (len in 1:length(scenarios_dirs)) {
vect_ <- str_split(as.character(scenarios_dirs[len]), "/")
vect_ <- vect_[[1]]
scenario_names <<- c(scenario_names, vect_[length(vect_)])
}

name_this_scenario <-   paste("HR", as.character(mat_cfg_scenario_settings_fore[2,2]), "-", as.character(mat_cfg_scenario_settings_fore[2,3]), sep="")

if (name_this_scenario %in% scenario_names)  {
     gtkComboBoxSetActive(.GlobalEnv$bmt_forecast_executed_scenarios, (which(scenario_names == name_this_scenario)-1))
} else {
    gtkComboBoxSetActive(.GlobalEnv$bmt_forecast_executed_scenarios, 0)
}
         
EX_SCENARIO <<- TRUE
}
 goNextTab()
 
 if (FALSE)  {
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
   }
# return(w)  
}


