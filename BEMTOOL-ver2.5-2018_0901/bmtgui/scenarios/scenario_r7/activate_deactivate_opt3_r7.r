# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


activate_deactivate_opt3_r7 <- function(w) {

gtkWidgetSetSensitive(hbox_scenarios_r7_table_option3, F)

index_to_update = -1
selected <- gtkComboBoxGetActiveText(bmt_combo_optionTAC_r7)

if (selected == "Option 3" ) {
gtkWidgetSetSensitive(hbox_scenarios_r7_table_option3, T)
} else {
gtkWidgetSetSensitive(hbox_scenarios_r7_table_option3, F)
}

}