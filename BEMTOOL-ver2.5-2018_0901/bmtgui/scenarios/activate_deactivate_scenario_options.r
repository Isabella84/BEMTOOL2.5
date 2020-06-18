# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



activate_deactivate_scenario_options <- function(w) {

        
gtkWidgetSetSensitive(vbox_hr_change_effort_options, F)
gtkWidgetSetSensitive(vbox_hr_change_f_by_fleet_options, F)
gtkWidgetSetSensitive(vbox_hr_change_total_f_options, F)
gtkWidgetSetSensitive(vbox_hr_set_tac_options, F)
gtkWidgetSetSensitive(vbox_hr_MEY_options, F)
gtkWidgetSetSensitive(vbox_hr_effort_F_options, F)	
 gtkWidgetSetSensitive(hbox_scenario_name, T) 	

if (gtkToggleButtonGetActive(bmt_chk_hr_change_effort) ) {
   gtkWidgetSetSensitive(vbox_hr_change_effort_options, T)
#   EFFORT_NUMBER_list_fore <<-  vector(mode = "list", length = length(BMT_FLEETSEGMENTS))
#   EFFORT_NUMBER_list_fore <<-  vector(mode = "list", length = length(BMT_FLEETSEGMENTS))
     
    bmt_reload_NUMBER_r4_table()
    bmt_reload_DAY_r4_table()
    gtkWidgetSetSensitive(vbox_hr_effort_F_options, T)
     gtkWidgetSetSensitive(hbox_scenario_name, T) 
}

if (gtkToggleButtonGetActive(bmt_chk_hr_change_f_by_fleet) ) {
	 gtkWidgetSetSensitive(vbox_hr_change_f_by_fleet_options, T)
	  gtkWidgetSetSensitive(hbox_scenario_name, T) 
}

if (gtkToggleButtonGetActive(bmt_chk_hr_change_total_f) ) {
	 gtkWidgetSetSensitive(vbox_hr_change_total_f_options, T)
	  gtkWidgetSetSensitive(hbox_scenario_name, T) 
}

if (gtkToggleButtonGetActive(bmt_chk_hr_set_tac) ) {
	 gtkWidgetSetSensitive(vbox_hr_set_tac_options, T)
    gtkWidgetSetSensitive(vbox_hr_effort_F_options, T)
     gtkWidgetSetSensitive(hbox_scenario_name, T) 
}

if (gtkToggleButtonGetActive(bmt_chk_hr_MEY) ) {
 gtkWidgetSetSensitive(vbox_hr_MEY_options, T)
     gtkWidgetSetSensitive(vbox_hr_effort_F_options, T)
     gtkWidgetSetSensitive(hbox_scenario_name, F) 
     gtkEntrySetText(bmt_entry_scenario_name, "")
}

if (gtkToggleButtonGetActive(bmt_chk_hr_behavioural_module) ) {
     gtkWidgetSetSensitive(vbox_hr_effort_F_options, T)
          gtkWidgetSetSensitive(hbox_scenario_name, T) 	
}



}