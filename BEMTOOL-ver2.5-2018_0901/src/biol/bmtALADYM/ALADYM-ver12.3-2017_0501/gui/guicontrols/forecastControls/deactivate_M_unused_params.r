# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




deactivate_M_unused_params_m <-function(w) {

 gtkWidgetSetSensitive(lbl_Mtmax_m, TRUE)
 gtkWidgetSetSensitive(entry_Mtmax_m, TRUE)
 
selected <- gtkComboBoxGetActiveText(combo_M_model_m)

if ( selected == "ProdbiomUniqueSolution" ) {
 gtkWidgetSetSensitive(lbl_Mtmax_m, TRUE)
 gtkWidgetSetSensitive(entry_Mtmax_m, TRUE)
}  else  { 
 gtkWidgetSetSensitive(lbl_Mtmax_m, FALSE)
 gtkWidgetSetSensitive(entry_Mtmax_m, FALSE)
}

}




deactivate_M_unused_params_f <-function(w) {

 gtkWidgetSetSensitive(lbl_Mtmax_f, TRUE)
 gtkWidgetSetSensitive(entry_Mtmax_f, TRUE)
 
selected <- gtkComboBoxGetActiveText(combo_M_model_f)
# print(paste("Selected element: ", selected, "[",select_index,"]", sep=""))

if ( selected == "ProdbiomUniqueSolution" ) {
 gtkWidgetSetSensitive(lbl_Mtmax_f, TRUE)
 gtkWidgetSetSensitive(entry_Mtmax_f, TRUE)
}  else  { 
 gtkWidgetSetSensitive(lbl_Mtmax_f, FALSE)
 gtkWidgetSetSensitive(entry_Mtmax_f, FALSE)
}

}




activ_deact_uncertainty_on_M <-function(w) {
 gtkWidgetSetSensitive(frame_M_model, TRUE)
if ( gtkToggleButtonGetActive(chkConfidenceIntervals_fore_M) ) {
 gtkWidgetSetSensitive(frame_M_model, TRUE)
}  else  { 
 gtkWidgetSetSensitive(frame_M_model, FALSE)
}
}