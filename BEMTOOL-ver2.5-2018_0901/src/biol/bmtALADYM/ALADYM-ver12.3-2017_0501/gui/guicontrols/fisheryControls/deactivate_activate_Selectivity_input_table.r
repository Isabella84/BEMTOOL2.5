# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



deactivate_activate_Selectivity_input_table <-function(w) {
  
  gtkWidgetSetSensitive(frame_1_selparams, TRUE)
  gtkWidgetSetSensitive(frame_2_selAge, TRUE)
 gtkWidgetSetSensitive(frame_3_selLength, TRUE)

   gtkWidgetSetSensitive(button_load_selectivity, TRUE)
    gtkWidgetSetSensitive(button_load_selectivity_vect_age, TRUE)
  gtkWidgetSetSensitive(button_load_selectivity_vect_len, TRUE)  

      gtkWidgetSetSensitive(button_saveall_selectivity, TRUE)
       gtkWidgetSetSensitive(button_saveall_selectivity_vect_age, TRUE)
           gtkWidgetSetSensitive(button_saveall_selectivity_vect_len, TRUE) 

selected <- gtkComboBoxGetActiveText(combo_escape_survival_rate)
selected_fleet <- gtkComboBoxGetActiveText(combo_fleetsegments)
select_index_fleet <- which(FLEETSEGMENTS_names == selected_fleet)

if ( gtkToggleButtonGetActive(radio_selectivity_params) ) {

  if (length(select_index_fleet) != 0) {
       FleetList_simulation[[select_index_fleet]]@selectivity.mode <<- "params"
      }


  gtkWidgetSetSensitive(frame_1_selparams, TRUE)
  gtkWidgetSetSensitive(frame_2_selAge, F)
 gtkWidgetSetSensitive(frame_3_selLength, F)

   gtkWidgetSetSensitive(button_load_selectivity, TRUE)
    gtkWidgetSetSensitive(button_load_selectivity_vect_age, F)
 gtkWidgetSetSensitive(button_load_selectivity_vect_len, F)
     
      gtkWidgetSetSensitive(button_saveall_selectivity, TRUE)
       gtkWidgetSetSensitive(button_saveall_selectivity_vect_age, F)
   gtkWidgetSetSensitive(button_saveall_selectivity_vect_len, F)
 
 
} else if ( gtkToggleButtonGetActive(radio_selectivity_vector_age) ) {

  if (length(select_index_fleet) != 0) {
       FleetList_simulation[[select_index_fleet]]@selectivity.mode <<- "age"
      }

  gtkWidgetSetSensitive(frame_1_selparams, F)
  gtkWidgetSetSensitive(frame_2_selAge, TRUE)
    gtkWidgetSetSensitive(frame_3_selLength, F)

   gtkWidgetSetSensitive(button_load_selectivity, F)
    gtkWidgetSetSensitive(button_load_selectivity_vect_age, TRUE)
  gtkWidgetSetSensitive(button_load_selectivity_vect_len, F)
     
      gtkWidgetSetSensitive(button_saveall_selectivity, F)
       gtkWidgetSetSensitive(button_saveall_selectivity_vect_age, TRUE)
        gtkWidgetSetSensitive(button_saveall_selectivity_vect_len, F)

    
} else if ( gtkToggleButtonGetActive(radio_selectivity_vector_length) ) {

  if (length(select_index_fleet) != 0) {
       FleetList_simulation[[select_index_fleet]]@selectivity.mode <<- "length"
      }

  gtkWidgetSetSensitive(frame_1_selparams, F)
  gtkWidgetSetSensitive(frame_2_selAge, F)
    gtkWidgetSetSensitive(frame_3_selLength, T)

   gtkWidgetSetSensitive(button_load_selectivity, F)
    gtkWidgetSetSensitive(button_load_selectivity_vect_age, F)
  gtkWidgetSetSensitive(button_load_selectivity_vect_len, T)
     
      gtkWidgetSetSensitive(button_saveall_selectivity, F)
       gtkWidgetSetSensitive(button_saveall_selectivity_vect_age, F)
        gtkWidgetSetSensitive(button_saveall_selectivity_vect_len, T)
      
}


}