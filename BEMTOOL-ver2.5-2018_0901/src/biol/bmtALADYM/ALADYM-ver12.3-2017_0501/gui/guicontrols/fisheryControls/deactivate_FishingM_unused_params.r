# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




deactivate_FishingM_unused_params <-function(w) {
  gtkWidgetSetSensitive(hboxEnteringFishingMortality_overall, F)
   
 gtkWidgetSetSensitive(button_load_F_by_fleet, F)
 gtkWidgetSetSensitive(button_load_F_overall, F)
 gtkWidgetSetSensitive(button_saveall_Fmortalities_overall, F)
   
if ( gtkToggleButtonGetActive(radio_f_by_fleet) ) {

  gtkWidgetSetSensitive(hboxEnteringFishingMortality_overall, F)
   
 gtkWidgetSetSensitive(button_load_F_by_fleet, T)
 gtkWidgetSetSensitive(button_load_F_overall, F)
 gtkWidgetSetSensitive(button_saveall_Fmortalities_overall, F)
 
  gtkWidgetSetSensitive(button_load_catch_by_age_for_splitting, F)
  gtkWidgetSetSensitive(button_save_catch_by_age_for_splitting, F)
    gtkWidgetSetSensitive(radio_production_splitting, F)
      gtkWidgetSetSensitive(radio_catch_by_age_splitting, F)

} else if ( gtkToggleButtonGetActive(radio_f_overall) ) {

  gtkWidgetSetSensitive(hboxEnteringFishingMortality_overall, T)
   
 gtkWidgetSetSensitive(button_load_F_by_fleet, F)
 gtkWidgetSetSensitive(button_load_F_overall, T)
 gtkWidgetSetSensitive(button_saveall_Fmortalities_overall, T)
 
   gtkWidgetSetSensitive(button_load_catch_by_age_for_splitting, T)
  gtkWidgetSetSensitive(button_save_catch_by_age_for_splitting, T)
    gtkWidgetSetSensitive(radio_production_splitting, T)
      gtkWidgetSetSensitive(radio_catch_by_age_splitting, T)
      
      deactivate_splittingF_unused_params()
    
} 

}





deactivate_splittingF_unused_params <-function(w) {

     gtkWidgetSetSensitive(button_load_catch_by_age_for_splitting, F)
  gtkWidgetSetSensitive(button_save_catch_by_age_for_splitting, F)
     gtkWidgetSetSensitive(vboxcatchAtAge_females, F)
   gtkWidgetSetSensitive(vboxcatchAtAge_males, F)
   
if ( gtkToggleButtonGetActive(radio_production_splitting) ) {

# ------------------------------------------------------ SPLITTING BY PRODUCTION DATA

     gtkWidgetSetSensitive(button_load_catch_by_age_for_splitting, F)
  gtkWidgetSetSensitive(button_save_catch_by_age_for_splitting, F)
     gtkWidgetSetSensitive(vboxcatchAtAge_females, F)
   gtkWidgetSetSensitive(vboxcatchAtAge_males, F)

   new_aldSimulation@Fsplittingtype <<- "P"

} else if ( gtkToggleButtonGetActive(radio_catch_by_age_splitting) ) {

# ------------------------------------------------------ SPLITTING BY CATCH AT AGE

     gtkWidgetSetSensitive(button_load_catch_by_age_for_splitting,T)
  gtkWidgetSetSensitive(button_save_catch_by_age_for_splitting, T)
     gtkWidgetSetSensitive(vboxcatchAtAge_females, T)
   gtkWidgetSetSensitive(vboxcatchAtAge_males, T)

      new_aldSimulation@Fsplittingtype <<- "CAA"

}


}
    
