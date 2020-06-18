# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




lockFisheryValues_fore<-function() {

#gtkWidgetSetSensitive(radio_classicalogive, FALSE)
#gtkWidgetSetSensitive(radio_ogivedes, FALSE)
#gtkWidgetSetSensitive(radio_normal, FALSE)
#gtkWidgetSetSensitive(radio_lognormal, FALSE)
#gtkWidgetSetSensitive(radio_binormal, FALSE)
#gtkWidgetSetSensitive(radio_twosided, FALSE)

gtkWidgetSetSensitive(selectivities.treeview, FALSE)
 gtkWidgetSetSensitive(discards.treeview, FALSE)
  gtkWidgetSetSensitive(combo_discard, FALSE)
gtkWidgetSetSensitive(button_load_discard, FALSE) 
gtkWidgetSetSensitive(button_load_selectivity, FALSE)

gtkWidgetSetSensitive(button_load_effortdata_fore, FALSE)
gtkWidgetSetSensitive(button_load_fc_fore, FALSE)

gtkWidgetSetSensitive(FISHINGEFFORT_fore.treeview, FALSE)
gtkWidgetSetSensitive(VESSELS_fore.treeview, FALSE)
gtkWidgetSetSensitive(DAYS_fore.treeview, FALSE)
gtkWidgetSetSensitive(GT_fore.treeview, FALSE)

   gtkWidgetSetSensitive(chkFMSY, FALSE) 
   
if (BMT_SCENARIO %in% c(BMT_HR_CHANGE_FISHEFFORT, BMT_HR_STATUS_QUO, BMT_HR_CHANGE_FISHMORTALITY, BMT_HR_STATUS_QUO_BEHAVIOURAL, BMT_HR_TAC_VARIATION, BMT_HR_CHANGE_FISHEFFORT_BEHAVIOURAL) ) {
   gtkWidgetSetSensitive(selectivities_fore.treeview, FALSE)
# gtkWidgetSetSensitive(discards_fore.treeview, FALSE)
#gtkWidgetSetSensitive(button_load_discard_fore, FALSE) 
gtkWidgetSetSensitive(button_load_selectivity_fore, FALSE)
gtkWidgetSetSensitive(button_savechanges_fleet_fore, FALSE)
                                                                        
} else if (BMT_SCENARIO %in% c(BMT_HR_CHANGE_SELECTIVITY, BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT, BMT_HR_CHANGE_SELECTIVITY_BEHAVIOURAL, BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT_BEHAVIOURAL) ) {     
 gtkWidgetSetSensitive(discards_fore.treeview, FALSE)
#gtkWidgetSetSensitive(button_load_discard_fore, FALSE) 
gtkWidgetSetSensitive(button_savechanges_fleet_fore, FALSE)

}



}