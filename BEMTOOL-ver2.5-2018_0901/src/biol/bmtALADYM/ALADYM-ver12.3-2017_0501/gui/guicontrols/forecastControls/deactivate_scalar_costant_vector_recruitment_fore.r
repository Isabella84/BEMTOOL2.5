# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




deactivate_scalar_costant_vector_recruitment_fore <-function(w) {

  gtkWidgetSetSensitive(btn_load_recruitment_fromVector_fore, T)
    gtkWidgetSetSensitive(entry_costant_recr_forecast, T)
      gtkWidgetSetSensitive(recruitments_fore_from_vector.sw, T)
    

if (gtkToggleButtonGetActive(radio_forecast_recruits_costant_vect_C)) {
gtkWidgetSetSensitive(btn_load_recruitment_fromVector_fore, F)
gtkWidgetSetSensitive(entry_costant_recr_forecast, T)
      gtkWidgetSetSensitive(recruitments_fore_from_vector.sw, F)
}  else { 
gtkWidgetSetSensitive(btn_load_recruitment_fromVector_fore, T)
gtkWidgetSetSensitive(entry_costant_recr_forecast, F)
      gtkWidgetSetSensitive(recruitments_fore_from_vector.sw, T)
}

}