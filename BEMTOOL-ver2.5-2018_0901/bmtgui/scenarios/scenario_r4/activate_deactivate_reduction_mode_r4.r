# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


activate_deactivate_reduction_mode_r4<- function(w) {

gtkWidgetSetSensitive(hbox_reduction_mode_r4, F)
gtkWidgetSetSensitive(bmt_button_load_effortdata_r4, F)

#bmt_resetAutomaticReduction(w)

#EFFORT_NUMBER_list_fore <<-  vector(mode = "list", length = length(BMT_FLEETSEGMENTS))
#EFFORT_DAY_list_fore <<-  vector(mode = "list", length = length(BMT_FLEETSEGMENTS))
 
index_to_update = -1
selected <- gtkComboBoxGetActiveText(bmt_combo_fleetsegments_effort_r4)
index_to_update <- which(BMT_FLEETSEGMENTS == selected)       
                  
bmt_fleet.NUMBER_r4 <<-   EFFORT_NUMBER_list_fore[[index_to_update]]
bmt_fleet.DAY_r4 <<-    EFFORT_DAY_list_fore[[index_to_update]]
 
 bmt_reload_NUMBER_r4_table()
 bmt_reload_DAY_r4_table()

if (gtkToggleButtonGetActive(bmt_chk_hr_reduction_mode_r4) ) {
   gtkWidgetSetSensitive(hbox_reduction_mode_r4, T)
   gtkWidgetSetSensitive(bmt_button_load_effortdata_r4, F)
} else {
   gtkWidgetSetSensitive(hbox_reduction_mode_r4, F)
   gtkWidgetSetSensitive(bmt_button_load_effortdata_r4, T)
   
#EFFORT_NUMBER_list_fore <<-  vector(mode = "list", length = length(BMT_FLEETSEGMENTS))
#EFFORT_DAY_list_fore <<-  vector(mode = "list", length = length(BMT_FLEETSEGMENTS))

#bmt_resetAutomaticReduction(w)
 
index_to_update = -1
selected <- gtkComboBoxGetActiveText(bmt_combo_fleetsegments_effort_r4)
index_to_update <- which(BMT_FLEETSEGMENTS == selected)       
                  
bmt_fleet.NUMBER_r4 <<-   EFFORT_NUMBER_list_fore[[index_to_update]]
bmt_fleet.DAY_r4 <<-    EFFORT_DAY_list_fore[[index_to_update]]
 
 bmt_reload_NUMBER_r4_table()
 bmt_reload_DAY_r4_table()
}




}