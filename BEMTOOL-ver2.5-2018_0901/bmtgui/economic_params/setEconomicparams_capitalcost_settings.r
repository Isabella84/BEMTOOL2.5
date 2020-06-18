# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같
setEconomicparams_capitalcost_settings <- function(w) {

 bmt_fleet.cost_capital  <<- NULL

index_to_update = -1
selected <- gtkComboBoxGetActiveText(bmt_combo_capitalcost_models)
#wnd <- showMessage(paste("        Saving changes to ", selected, "...        ", sep=""))

#activate_deactivate_fixedcost_tables(selected)
if (!is.null(selected) ) {

mat_cfg_capCosts[2,2] <<- which(CAPITALCOST_MODELS$option_name == selected)   
index_to_update <- which(CAPITALCOST_MODELS$option_name == selected)    

print(paste("Option selected for capital cost calculation:", mat_cfg_capCosts[2,2] ), quote=F) 

if (CAPITALCOST_MODELS$model_name[index_to_update] == 1) {
gtkLabelSetText(lblCapCostFunction, "DC = a * GT\nOC = b * GT")

} else if (CAPITALCOST_MODELS$model_name[index_to_update] == 2) {
gtkLabelSetText(lblCapCostFunction, "DC = a * K\nOC = b * K")

} else if (CAPITALCOST_MODELS$model_name[index_to_update] == 3) {
gtkLabelSetText(lblCapCostFunction, "CC = a * N")

} 


if (index_to_update == 1) {
  CAPITALCOSTS_head <<- COEFFICIENT_CAPITALCOST_BIRDMOD_names
} else if (index_to_update == 2)  {
    CAPITALCOSTS_head <<- COEFFICIENT_CAPITALCOST_MEFISTO_names
} else if (index_to_update == 3)  {
    CAPITALCOSTS_head <<- COEFFICIENT_CAPITALCOST_FISHRENT_names
} 


   # ------------------------------------------------------------ BIRDMOD
matrix_temp <-   mat_cfg_capCosts[4:(3+length(BMT_FLEETSEGMENTS)), 1:3]
 capcost_variable_matrix <- data.frame(matrix(0, nrow=length(CAPITALCOSTS_head), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(capcost_variable_matrix) <-  c(" ",BMT_FLEETSEGMENTS)
     capcost_variable_matrix[,1] <- CAPITALCOSTS_head

    if (!is.na(mat_cfg_capCosts[2,2])) {
#if (CAPITALCOST_MODELS$model_name[index_to_update] == mat_cfg_capCosts[2,2]) {
for (flee in 1:length(BMT_FLEETSEGMENTS)) {
for (coeff in 1:length(CAPITALCOSTS_head)) {
       capcost_variable_matrix[coeff, flee+1] <- as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.capCostFun.F", flee, sep="") ,(coeff+1)] ))
}      
}

#}
}

# mefisto e fishrent
  
 bmt_fleet.cost_capital  <<- capcost_variable_matrix
 bmt_reload_cost_capital_table()
     
}

}