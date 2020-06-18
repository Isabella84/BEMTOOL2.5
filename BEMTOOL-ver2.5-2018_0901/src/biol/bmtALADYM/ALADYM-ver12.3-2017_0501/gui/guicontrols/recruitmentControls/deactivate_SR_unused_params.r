# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




deactivate_SR_unused_params <-function(w) {
 select_index = -1
  gtkWidgetSetSensitive(lbl_SRparameters, TRUE)
 gtkWidgetSetSensitive(lbl_SR_params_a, TRUE)
 gtkWidgetSetSensitive(entrySR_params_a, TRUE)
 gtkWidgetSetSensitive(lbl_SR_params_b, TRUE)
 gtkWidgetSetSensitive(entrySR_params_b, TRUE)
 gtkWidgetSetSensitive(lbl_SR_params_c, TRUE)
 gtkWidgetSetSensitive(entrySR_params_c, TRUE)
 #gtkWidgetSetSensitive(lbl_SR_Rfile, TRUE)
 gtkWidgetSetSensitive(btn_browse_Rfile, TRUE)
 #gtkWidgetSetSensitive(lbl_SRvectorFile, TRUE)
 gtkWidgetSetSensitive(recruitments.treeview, TRUE)
 gtkWidgetSetSensitive(lbl_OFFSPRING_seed_value, TRUE)
 gtkWidgetSetSensitive(entry_OFFSPRING_seedvalue, TRUE)
 gtkWidgetSetSensitive(btn_load_seed_recruitment, TRUE)
 gtkWidgetSetSensitive(btn_browse_save_recruitment, TRUE)    
  
selected <- gtkComboBoxGetActiveText(combo_SRtype)
select_index <- which(SR_TYPE == selected )
# print(paste("Selected element: ", selected, "[",select_index,"]", sep=""))

if (select_index == 1 | select_index == 2 | select_index == 5) {
    #gtkWidgetSetSensitive(lbl_SR_Rfile, FALSE)
    gtkWidgetSetSensitive(btn_browse_Rfile, FALSE)
    #gtkWidgetSetSensitive(lbl_SRvectorFile, FALSE)
    gtkWidgetSetSensitive(lbl_SR_params_c, FALSE)
    gtkWidgetSetSensitive(entrySR_params_c, FALSE)
    gtkWidgetSetSensitive(recruitments.treeview, FALSE)
    gtkWidgetSetSensitive(lbl_OFFSPRING_seed_value, FALSE)
    gtkWidgetSetSensitive(entry_OFFSPRING_seedvalue, FALSE)
    gtkWidgetSetSensitive(btn_load_seed_recruitment, FALSE) 
    gtkWidgetSetSensitive(btn_browse_save_recruitment, FALSE)   
} else if (select_index == 3 | select_index == 6) {
    #gtkWidgetSetSensitive(lbl_SR_Rfile, FALSE)
    gtkWidgetSetSensitive(btn_browse_Rfile, FALSE)
    #gtkWidgetSetSensitive(lbl_SRvectorFile, FALSE)
    gtkWidgetSetSensitive(recruitments.treeview, FALSE)
    gtkWidgetSetSensitive(lbl_OFFSPRING_seed_value, FALSE)
    gtkWidgetSetSensitive(entry_OFFSPRING_seedvalue, FALSE)
    gtkWidgetSetSensitive(btn_load_seed_recruitment, FALSE)
    gtkWidgetSetSensitive(btn_browse_save_recruitment, FALSE)    
} else if (select_index == 4) { 
  gtkWidgetSetSensitive(lbl_SRparameters, FALSE)
    gtkWidgetSetSensitive(lbl_SR_params_a, FALSE)
    gtkWidgetSetSensitive(entrySR_params_a, FALSE)
    gtkWidgetSetSensitive(lbl_SR_params_b, FALSE)
    gtkWidgetSetSensitive(entrySR_params_b, FALSE)
    gtkWidgetSetSensitive(lbl_SR_params_c, FALSE)
    gtkWidgetSetSensitive(entrySR_params_c, FALSE)
}

}