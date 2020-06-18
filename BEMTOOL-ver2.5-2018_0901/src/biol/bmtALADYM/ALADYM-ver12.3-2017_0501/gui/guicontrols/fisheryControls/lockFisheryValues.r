# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




lockFisheryValues<-function() {

gtkEntrySetEditable(entryGearName, FALSE)
gtkWidgetSetSensitive(button_save_fleet, FALSE)
gtkWidgetSetSensitive(button_savechanges_fleet, FALSE)
gtkWidgetSetSensitive(button_remove_fleet, FALSE)

gtkWidgetSetSensitive(button_load_production, FALSE)
gtkWidgetSetSensitive(button_load_effortdata, FALSE)
gtkWidgetSetSensitive(button_load_p_production, FALSE)
gtkWidgetSetSensitive(button_load_fishingcoeff, FALSE)

gtkWidgetSetSensitive(radio_effortdata, FALSE)
gtkWidgetSetSensitive(radio_fishingcoeff, FALSE)
gtkWidgetSetSensitive(radio_data, FALSE)
gtkWidgetSetSensitive(radio_pp, FALSE)


gtkWidgetSetSensitive(DAYS.treeview, FALSE)
gtkWidgetSetSensitive(FISHINGEFFORT.treeview, FALSE)
gtkWidgetSetSensitive(VESSELS.treeview, FALSE)
gtkWidgetSetSensitive(GT.treeview, FALSE)
gtkWidgetSetSensitive(pproductions.treeview, FALSE)
gtkWidgetSetSensitive(productions.treeview, FALSE)

gtkWidgetSetSensitive(entry_DAYS_seedvalue, FALSE)
gtkWidgetSetSensitive(entry_FISHINGEFFORT_seedvalue, FALSE)
gtkWidgetSetSensitive(entry_VESSELS_seedvalue, FALSE)
gtkWidgetSetSensitive(entry_GT_seedvalue, FALSE)
gtkWidgetSetSensitive(entry_Production_seedvalue, FALSE)
gtkWidgetSetSensitive(entry_pProduction_seedvalue, FALSE)

gtkWidgetSetSensitive(btn_load_seed_DAYS, FALSE)
gtkWidgetSetSensitive(btn_load_seed_FISHINGEFFORT, FALSE)
gtkWidgetSetSensitive(btn_load_seed_VESSELS, FALSE)
gtkWidgetSetSensitive(btn_load_seed_GT, FALSE)
gtkWidgetSetSensitive(btn_load_seed_production, FALSE)


}
