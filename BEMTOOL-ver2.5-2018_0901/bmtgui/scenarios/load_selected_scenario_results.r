# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


load_selected_scenario_results <- function(w) {

SCENARIO_TO_LOAD_FROM_MENU <<-  gtkComboBoxGetActiveText(bmt_forecast_executed_scenarios)

#print(paste("recepisco scenario:" , SCENARIO_TO_LOAD_FROM_MENU) )

goNextTab(w)

#selected_SCENARIO_TO_LOAD <<- gtkComboBoxGetActiveText(bmt_forecast_executed_scenarios)
#index_to_select <- gtkComboBoxGetActive(bmt_forecast_executed_scenarios)
# 
# if ( is.null(selected_SCENARIO_TO_LOAD) ) {
#    selected_SCENARIO_TO_LOAD <- harvest_rule_id
# }   
#    
#print(selected_SCENARIO_TO_LOAD)    
#rdata_path_fore <- paste(mat_cfg_general[1,3],  "/", selected_SCENARIO_TO_LOAD, "/", mat_cfg_general[1,2], " - BEMTOOL forecast ", selected_SCENARIO_TO_LOAD,".Rdata", sep="")
#
#

#   if (!removing)  {
#        print("CARICA SCENARIO AZIONE!!")
#   LOAD_SCENARIO_ACTION <<- T
#  suppressWarnings(source(paste(getwd(), "/bmtgui/scenarios/loadScenario_script.r", sep="")) )		
#	 }

}
