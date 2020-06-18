# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




deactivate_selectivity_uncertainty <-function(w) {

gtkWidgetSetSensitive(vbox_global_selectivity_uncert, T)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook_forecast_fleets, 1), TRUE)

if (gtkToggleButtonGetActive(chkConfidenceIntervals_fore_Selectivity)) {
gtkWidgetSetSensitive(vbox_global_selectivity_uncert, T)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook_forecast_fleets, 1), FALSE)
}  else { 
gtkWidgetSetSensitive(vbox_global_selectivity_uncert, F) 
 if (new_aldSimulation@enteringMortality == "Z" ) {
     gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook_forecast_fleets, 1), TRUE)
 } else {
     gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook_forecast_fleets, 1), FALSE)
 }

}

}



 deactivate_selectivity_uncertainty_distr_extfile <-function(w) {

gtkWidgetSetSensitive(btn_load_selectivity_uncert_distribution_from_file, T)
gtkWidgetSetSensitive(selectivity_uncert_distribution_from_file_sw, T)
gtkWidgetSetSensitive(btn_load_selectivity_uncert_vector_from_file, T)
gtkWidgetSetSensitive(selectivity_uncert_vector_from_file_sw, T)
 
if (gtkToggleButtonGetActive(radio_selectivity_uncert_from_distribution)) {

gtkWidgetSetSensitive(btn_load_selectivity_uncert_distribution_from_file, T)
gtkWidgetSetSensitive(selectivity_uncert_distribution_from_file_sw, T)
gtkWidgetSetSensitive(btn_load_selectivity_uncert_vector_from_file, F)
gtkWidgetSetSensitive(selectivity_uncert_vector_from_file_sw, F)

}  else { 

gtkWidgetSetSensitive(btn_load_selectivity_uncert_distribution_from_file, F)
gtkWidgetSetSensitive(selectivity_uncert_distribution_from_file_sw, F)
gtkWidgetSetSensitive(btn_load_selectivity_uncert_vector_from_file, T)
gtkWidgetSetSensitive(selectivity_uncert_vector_from_file_sw, T)
}

}

