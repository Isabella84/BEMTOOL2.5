# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.






deactivate_Survivability_unused_params_fore <-function(w) {
 select_index = -1
 
gtkWidgetSetSensitive(lbl_option_survivability_fore, TRUE)
gtkWidgetSetSensitive(radio_survivability_ogive_fore, TRUE)
gtkWidgetSetSensitive(radio_survivability_constant_fore, TRUE)
gtkWidgetSetSensitive(lbl_option_survivability_fore_param1_males, TRUE)
gtkWidgetSetSensitive(entry_survivability_param1_males_fore, TRUE)
gtkWidgetSetSensitive(lbl_option_survivability_fore_param2_females, TRUE)
gtkWidgetSetSensitive(entry_survivability_param2_females_fore, TRUE)


selected <- gtkComboBoxGetActiveText(combo_discard_survival_rate_fore)

selected_fleet <- gtkComboBoxGetActiveText(combo_fleetsegments_fore)
select_index_fleet <- which(FLEETSEGMENTS_names == selected_fleet)


if (selected == "YES" ) {

      if (length(select_index_fleet) != 0) {
       FleetList_forecast[[select_index_fleet]]@discard.survivability.calculation <<- "Y"
      }

gtkWidgetSetSensitive(lbl_option_survivability_fore, T)
gtkWidgetSetSensitive(radio_survivability_ogive_fore, T)
gtkWidgetSetSensitive(radio_survivability_constant_fore, T)
gtkWidgetSetSensitive(lbl_option_survivability_fore_param1_males, T)
gtkWidgetSetSensitive(entry_survivability_param1_males_fore, T)
gtkWidgetSetSensitive(lbl_option_survivability_fore_param2_females, T)
gtkWidgetSetSensitive(entry_survivability_param2_females_fore, T)

 selected <- gtkComboBoxGetActiveText(combo_discard_survival_rate_fore)

if (selected == "NO") { 
   
   gtkWidgetSetSensitive(lbl_option_survivability_fore, F)
gtkWidgetSetSensitive(radio_survivability_ogive_fore, F)
gtkWidgetSetSensitive(radio_survivability_constant_fore, F)
gtkWidgetSetSensitive(lbl_option_survivability_fore_param1_males, F)
gtkWidgetSetSensitive(entry_survivability_param1_males_fore, F)
gtkWidgetSetSensitive(lbl_option_survivability_fore_param2_females, F)
gtkWidgetSetSensitive(entry_survivability_param2_females_fore, F)

} else {

    if (  gtkToggleButtonGetActive(radio_survivability_ogive_fore) ) {

gtkLabelSetText(lbl_option_survivability_fore_param1_males, "L50% [mm]")
gtkLabelSetText(lbl_option_survivability_fore_param2_females, "L75%L25% [mm]")

    } else {
gtkLabelSetText(lbl_option_survivability_fore_param1_males, "MALES")
gtkLabelSetText(lbl_option_survivability_fore_param2_females, "FEMALES")

    }
  }

} else {
   
         if (length(select_index_fleet) != 0) {
       FleetList_forecast[[select_index_fleet]]@discard.survivability.calculation <<- "N"
      }
   
   gtkWidgetSetSensitive(lbl_option_survivability_fore, F)
gtkWidgetSetSensitive(radio_survivability_ogive_fore, F)
gtkWidgetSetSensitive(radio_survivability_constant_fore, F)
gtkWidgetSetSensitive(lbl_option_survivability_fore_param1_males, F)
gtkWidgetSetSensitive(entry_survivability_param1_males_fore, F)
gtkWidgetSetSensitive(lbl_option_survivability_fore_param2_females, F)
gtkWidgetSetSensitive(entry_survivability_param2_females_fore, F)

}


deactive_survivability_O_C_fore()


}






deactive_survivability_O_C_fore <- function(w) {

selected_fleet <- gtkComboBoxGetActiveText(combo_fleetsegments_fore)
select_index_fleet <- which(FLEETSEGMENTS_names == selected_fleet)


selected <- gtkComboBoxGetActiveText(combo_discard_survival_rate_fore)

if (selected == "NO") { 
   
gtkWidgetSetSensitive(lbl_option_survivability_fore, F)
gtkWidgetSetSensitive(radio_survivability_ogive_fore, F)
gtkWidgetSetSensitive(radio_survivability_constant_fore, F)
gtkWidgetSetSensitive(lbl_option_survivability_fore_param1_males, F)
gtkWidgetSetSensitive(entry_survivability_param1_males_fore, F)
gtkWidgetSetSensitive(lbl_option_survivability_fore_param2_females, F)
gtkWidgetSetSensitive(entry_survivability_param2_females_fore, F)

} else {

    if (  gtkToggleButtonGetActive(radio_survivability_ogive_fore) ) {

gtkLabelSetText(lbl_option_survivability_fore_param1_males, "L50% [mm]")
gtkLabelSetText(lbl_option_survivability_fore_param2_females, "L75%L25% [mm]")

   if (length(select_index_fleet) > 0) {
    FleetList_forecast[[select_index_fleet]]@discard.survivability.datatype <<- "DOS" 
     }

    } else {
gtkLabelSetText(lbl_option_survivability_fore_param1_males, "MALES")
gtkLabelSetText(lbl_option_survivability_fore_param2_females, "FEMALES")

   if (length(select_index_fleet) > 0) {
   FleetList_forecast[[select_index_fleet]]@discard.survivability.datatype <<- "C"
   }
    }
  }
}