# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


 
# if (FALSE) {
#selected_SCENARIO_TO_LOAD <<- gtkComboBoxGetActiveText(bmt_forecast_executed_scenarios)
#index_to_select <- gtkComboBoxGetActive(bmt_forecast_executed_scenarios)
# 
# if ( is.null(selected_SCENARIO_TO_LOAD) ) {
#    selected_SCENARIO_TO_LOAD <- harvest_rule_id
# }   
#}

bmt_wnd_scenario <- showMessage(paste("Loading results of ",SCENARIO_TO_LOAD_FROM_MENU," scenario...")) 
gtkWidgetSetSensitive(BMTmain_window, F)
    
# print(paste("Loading...", SCENARIO_TO_LOAD_FROM_MENU), quote=F)    
rdata_path_fore <- paste(mat_cfg_general[1,3],  "/", SCENARIO_TO_LOAD_FROM_MENU, "/working files/", mat_cfg_general[1,2], " - BEMTOOL forecast ", SCENARIO_TO_LOAD_FROM_MENU,"-rev.Rdata", sep="")

res_fore <- suppressWarnings(try(load(rdata_path_fore) ) )


print(paste("Loading...", rdata_path_fore), quote=F)

if ( class(res_fore) != "try-error") {
# LOADED_RESULTS <<- T
#bmt_wnd_scenario <- showMessage("Loading SCENARIO results...") 
#gtkWidgetSetSensitive(BMTmain_window, F)

if (exists("hbox_forecast")) {
#rm(hbox_forecast)
hbox_forecast$destroy()
hbox_forecast <- gtkHBox(homogeneous = FALSE, 5)  
}

# if (exists("BMTnotebook_forecast")) {
# BMTnotebook_forecast$destroy()
# }


BMTnotebook_forecast <<- gtkNotebook()
BMTnotebook_forecast$setTabPos("top")
 


                            
suppressWarnings(source(paste(getwd(), "/bmtgui/forecast/forecast_tab_statestocks.r", sep="")))					
BMTnotebook_forecast$appendPage(vbox_Forecast_StateStocks, gtkLabel(str=" State of the stocks "))

suppressWarnings(source(paste(getwd(), "/bmtgui/forecast/forecast_tab_impactpressure.r", sep="")))					
BMTnotebook_forecast$appendPage(vbox_Forecast_ImpactPressure, gtkLabel(str=" Impact/Pressure "))

suppressWarnings(source(paste(getwd(), "/bmtgui/forecast/forecast_tab_statefleets.r", sep="")))					
BMTnotebook_forecast$appendPage(vbox_Forecast_StateFleets, gtkLabel(str=" State of the fleets "))

 hbox_forecast$packStart(BMTnotebook_forecast, expand = T, fill = T, padding = 5)

#    for (choice in BMT_SPECIES) { 
#    bmt_forecast_species$appendText(choice)    
#    }
      gtkComboBoxSetActive(bmt_forecast_species, 0 )   

#  bmt_wnd_forecast$destroy()
gtkWidgetSetSensitive(BMTmain_window, T)  
bmt_wnd_scenario$destroy()
showMessageOK("SCENARIO successfully loaded!")    

} else {
if (gtkNotebookGetCurrentPage(BMTnotebook) == 7 ) {
bmt_wnd_scenario$destroy()
showError("Forecast results not found for the selected scenario! ")
}
LOADED_RESULTS <<-F                 
}

vbox_container_fore$packStart(hbox_forecast, expand = T, fill = T, padding = 10)  
