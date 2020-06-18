# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



deactivate_activate_Selectivity_input_table_fore <-function(w) {
  select_index = -1
  
  selected <- gtkComboBoxGetActiveText(combo_fleetsegments_fore)
  
  if (!is.null(selected)) {
select_index <- which(FLEETSEGMENTS_names == selected )
  
  gtkWidgetSetSensitive(frame_1_selparams_fore, TRUE)
  gtkWidgetSetSensitive(frame_2_selAge_fore, TRUE)
 gtkWidgetSetSensitive(frame_3_selLength_fore, TRUE)

   gtkWidgetSetSensitive(button_load_selectivity_fore, TRUE)
    gtkWidgetSetSensitive(button_load_selectivity_vect_age_fore, TRUE)
  gtkWidgetSetSensitive(button_load_selectivity_vect_len_fore, TRUE)  

      gtkWidgetSetSensitive(button_saveall_selectivity_fore, TRUE)
       gtkWidgetSetSensitive(button_saveall_selectivity_vect_age_fore, TRUE)
           gtkWidgetSetSensitive(button_saveall_selectivity_vect_len_fore, TRUE) 

 if (length(FleetList_forecast[[select_index]]@selectivity.mode) != 0) {

if ( FleetList_forecast[[select_index]]@selectivity.mode == "params") {

  gtkWidgetSetSensitive(frame_1_selparams_fore, TRUE)
  gtkWidgetSetSensitive(frame_2_selAge_fore, F)
 gtkWidgetSetSensitive(frame_3_selLength_fore, F)

   gtkWidgetSetSensitive(button_load_selectivity_fore, TRUE)
    gtkWidgetSetSensitive(button_load_selectivity_vect_age_fore, F)
 gtkWidgetSetSensitive(button_load_selectivity_vect_len_fore, F)
     
      gtkWidgetSetSensitive(button_saveall_selectivity_fore, TRUE)
       gtkWidgetSetSensitive(button_saveall_selectivity_vect_age_fore, F)
   gtkWidgetSetSensitive(button_saveall_selectivity_vect_len_fore, F)
 
 
} else if ( FleetList_forecast[[select_index]]@selectivity.mode == "age" ) {

  gtkWidgetSetSensitive(frame_1_selparams_fore, F)
  gtkWidgetSetSensitive(frame_2_selAge_fore, TRUE)
    gtkWidgetSetSensitive(frame_3_selLength_fore, F)

   gtkWidgetSetSensitive(button_load_selectivity_fore, F)
    gtkWidgetSetSensitive(button_load_selectivity_vect_age_fore, TRUE)
  gtkWidgetSetSensitive(button_load_selectivity_vect_len_fore, F)
     
      gtkWidgetSetSensitive(button_saveall_selectivity_fore, F)
       gtkWidgetSetSensitive(button_saveall_selectivity_vect_age_fore, TRUE)
        gtkWidgetSetSensitive(button_saveall_selectivity_vect_len_fore, F)

    
} else if ( FleetList_forecast[[select_index]]@selectivity.mode == "length" ) {

  gtkWidgetSetSensitive(frame_1_selparams_fore, F)
  gtkWidgetSetSensitive(frame_2_selAge_fore, F)
    gtkWidgetSetSensitive(frame_3_selLength_fore, T)

   gtkWidgetSetSensitive(button_load_selectivity_fore, F)
    gtkWidgetSetSensitive(button_load_selectivity_vect_age_fore, F)
  gtkWidgetSetSensitive(button_load_selectivity_vect_len_fore, T)
     
      gtkWidgetSetSensitive(button_saveall_selectivity_fore, F)
       gtkWidgetSetSensitive(button_saveall_selectivity_vect_age_fore, F)
        gtkWidgetSetSensitive(button_saveall_selectivity_vect_len_fore, T)
      
}

}

}

}