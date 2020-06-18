# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같
setEconomicparams_fixedcost_settings <- function(w) {

bmt_fleet.cost_fixed  <<- NULL

index_to_update = -1
selected <- gtkComboBoxGetActiveText(bmt_combo_fixedcost_models)
#wnd <- showMessage(paste("        Saving changes to ", selected, "...        ", sep=""))
index_to_update <- which(FIXEDCOST_MODELS$option_name == selected)


if (!is.null(selected) ) {

if (FIXEDCOST_MODELS$model_name[index_to_update] == 1) {
gtkLabelSetText(lblFixCostFunction, "MC = a * GT\nOFC = b * GT")

} else if (FIXEDCOST_MODELS$model_name[index_to_update] == 2) {
gtkLabelSetText(lblFixCostFunction, "EC = b * N\nUMC = a * N\nAMC = min(c * N ; max( c * N + P[t-1]; 0))")

} else if (FIXEDCOST_MODELS$model_name[index_to_update] == 3) {
gtkLabelSetText(lblFixCostFunction, "FC = a * N")

} 

mat_cfg_fixCosts[2,2] <<- which(FIXEDCOST_MODELS$option_name == selected)
print(paste("Option selected for fixed costs calculation:", mat_cfg_fixCosts[2,2] ), quote=F)


if (index_to_update == 1) {
  FIXEDCOSTS_head <<- COEFFICIENT_FIXEDCOST_BIRDMOD_names
} else if (index_to_update == 2)  {
    FIXEDCOSTS_head <<- COEFFICIENT_FIXEDCOST_MEFISTO_names
} else if (index_to_update == 3)  {
    FIXEDCOSTS_head <<- COEFFICIENT_FIXEDCOST_FISHRENT_names
} 

     if (!is.na(mat_cfg_fixCosts[2,2])) {
   # ------------------------------------------------------------ BIRDMOD
matrix_temp <-   mat_cfg_fixCosts[4:(3+length(BMT_FLEETSEGMENTS)), 1:4]

 fixcost_variable_matrix <- data.frame(matrix(0, nrow=length(FIXEDCOSTS_head), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(fixcost_variable_matrix) <-  c(" ",BMT_FLEETSEGMENTS)
     fixcost_variable_matrix[,1] <- FIXEDCOSTS_head

#if (FIXEDCOST_MODELS$model_name[index_to_update] == mat_cfg_fixCosts[2,2] ) {
for (flee in 1:length(BMT_FLEETSEGMENTS)) {
for (coeff in 1:length(FIXEDCOSTS_head)) {
       fixcost_variable_matrix[coeff, flee+1] <- as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.fixCostFun.F", flee, sep="") ,(coeff+1)] ))
}      
}

#}

}

# mefisto e fishrent
  
 bmt_fleet.cost_fixed  <<- fixcost_variable_matrix
 bmt_reload_cost_fixed_table()
     
}

}