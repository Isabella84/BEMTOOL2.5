# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




deactivate_FishingEffort_unused_params_fore <-function(w) {
 gtkWidgetSetSensitive(VESSELS_fore.treeview, TRUE)
  gtkWidgetSetSensitive(DAYS_fore.treeview, TRUE)
  gtkWidgetSetSensitive(GT_fore.treeview, TRUE)
  gtkWidgetSetSensitive(FISHINGEFFORT_fore.treeview, TRUE)


if ( gtkToggleButtonGetActive(radio_effortdata) ) {
gtkWidgetSetSensitive(VESSELS_fore.treeview, TRUE)
  gtkWidgetSetSensitive(DAYS_fore.treeview, TRUE)
  gtkWidgetSetSensitive(GT_fore.treeview, TRUE)
  gtkWidgetSetSensitive(FISHINGEFFORT_fore.treeview, FALSE)
} else if ( gtkToggleButtonGetActive(radio_fishingcoeff) ) {
  gtkWidgetSetSensitive(VESSELS_fore.treeview, FALSE)
  gtkWidgetSetSensitive(DAYS_fore.treeview, FALSE)
  gtkWidgetSetSensitive(GT_fore.treeview, FALSE)
  gtkWidgetSetSensitive(FISHINGEFFORT_fore.treeview, TRUE)
} 

}
