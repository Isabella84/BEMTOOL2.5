# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




deactivate_costant_relatinship_recr_fore <-function(w) {

gtkWidgetSetSensitive(hboxSRType_fore, T)
 gtkWidgetSetSensitive(hboxSR_params_fore, T)
  gtkWidgetSetSensitive(hbox_entry_costant_recr_forecast, T)
 

#  gtkWidgetSetSensitive(hboxSRType_fore, T)
# gtkWidgetSetSensitive(hboxSR_params_fore, T)
# gtkWidgetSetSensitive(hbox_entry_costant_recr_forecast, T)
# gtkWidgetSetSensitive(hbox_entry_costant_recr_forecast_radios, T)
# gtkWidgetSetSensitive( recruitments_fore_from_vector.sw, T)

# print(paste("Selected element: ", selected, "[",select_index,"]", sep=""))

if (gtkToggleButtonGetActive(radio_forecast_recruits_costant)) {

gtkWidgetSetSensitive(hboxSRType_fore, F)
 gtkWidgetSetSensitive(hboxSR_params_fore, F)
  gtkWidgetSetSensitive(hbox_entry_costant_recr_forecast, T)
 
# gtkWidgetSetSensitive(hbox_entry_costant_recr_forecast, T)
# gtkWidgetSetSensitive(hbox_entry_costant_recr_forecast_radios, F)
#  gtkWidgetSetSensitive( recruitments_fore_from_vector.sw, T)
 
}  else { 

gtkWidgetSetSensitive(hboxSRType_fore, T)
 gtkWidgetSetSensitive(hboxSR_params_fore, T)
  gtkWidgetSetSensitive(hbox_entry_costant_recr_forecast, F)
  
}

}


