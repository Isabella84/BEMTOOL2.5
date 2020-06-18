# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




deactivate_SR_unused_params_fore <-function(w) {
 select_index = -1
  gtkWidgetSetSensitive(lbl_SRparameters_fore, TRUE)
 gtkWidgetSetSensitive(lbl_SR_params_a_fore, TRUE)
 gtkWidgetSetSensitive(entrySR_params_a_fore, TRUE)
 gtkWidgetSetSensitive(lbl_SR_params_b_fore, TRUE)
 gtkWidgetSetSensitive(entrySR_params_b_fore, TRUE)
 gtkWidgetSetSensitive(lbl_SR_params_c_fore, TRUE)
 gtkWidgetSetSensitive(entrySR_params_c_fore, TRUE) 
  
selected <- gtkComboBoxGetActiveText(combo_SRtype_fore)
select_index <- which(SR_TYPE == selected )
# print(paste("Selected element: ", selected, "[",select_index,"]", sep=""))

if (select_index == 1 | select_index == 2 | select_index == 5) {
   
    gtkWidgetSetSensitive(lbl_SR_params_c_fore, FALSE)
    gtkWidgetSetSensitive(entrySR_params_c_fore, FALSE)
}  else if (select_index == 4) { 
  gtkWidgetSetSensitive(lbl_SRparameters_fore, FALSE)
    gtkWidgetSetSensitive(lbl_SR_params_a_fore, FALSE)
    gtkWidgetSetSensitive(entrySR_params_a_fore, FALSE)
    gtkWidgetSetSensitive(lbl_SR_params_b_fore, FALSE)
    gtkWidgetSetSensitive(entrySR_params_b_fore, FALSE)
    gtkWidgetSetSensitive(lbl_SR_params_c_fore, FALSE)
    gtkWidgetSetSensitive(entrySR_params_c_fore, FALSE)
}

}





deactivate_SSR_error_unused_params <-function(w) {
 select_index = -1
 gtkWidgetSetSensitive(lbl_SR_params_a_fore_UN, TRUE)
 gtkWidgetSetSensitive(entryMean_aSRR, TRUE)
  gtkWidgetSetSensitive(entryStDev_aSRR, TRUE)
 gtkWidgetSetSensitive(lbl_SR_params_b_fore_UN, TRUE)
 gtkWidgetSetSensitive(entryMean_bSRR, TRUE)
  gtkWidgetSetSensitive(entryStDev_bSRR, TRUE)
 gtkWidgetSetSensitive(lbl_SR_params_c_fore_UN, TRUE)
 gtkWidgetSetSensitive(entryMean_cSRR, TRUE) 
  gtkWidgetSetSensitive(entryStDev_cSRR, TRUE)
  
selected <- gtkComboBoxGetActiveText(combo_SRtype_fore_UN)
select_index <- which(SR_TYPE == selected )
# print(paste("Selected element: ", selected, "[",select_index,"]", sep=""))

if (select_index == 1 | select_index == 2 | select_index == 5) {

 gtkWidgetSetSensitive(lbl_SR_params_c_fore_UN, FALSE)
 gtkWidgetSetSensitive(entryMean_cSRR, FALSE) 
  gtkWidgetSetSensitive(entryStDev_cSRR, FALSE)

}  

}