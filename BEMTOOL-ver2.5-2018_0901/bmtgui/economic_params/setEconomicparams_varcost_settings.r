# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


   




# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같
setEconomicparams_varcost_settings <- function(w) {

 bmt_fleet.cost_variable <<- NULL
 bmt_fleet.cost_fuelprice  <<- NULL

index_to_update = -1
selected <- gtkComboBoxGetActiveText(bmt_combo_varcost_models)
#wnd <- showMessage(paste("        Saving changes to ", selected, "...        ", sep=""))


#for (fl in 1:length(BMT_FLEETSEGMENTS) ) {   
#         mat_cfg_varCosts[fl+3,1] <<- paste("casestudy.varCostFun.F" ,fl , sep="")
#}
#mat_cfg_varCosts[2,1]  <<- "casestudy.varCostFun" 


if (!is.null(selected) ) {
index_to_update <- which(VARCOST_MODELS$option_name == selected)   

if (VARCOST_MODELS$model_name[index_to_update] == 1) {
gtkLabelSetText(lblVarCostFunction, "FuC = a * E\nCoC = b * L\nOVC = c * E")

} else if (VARCOST_MODELS$model_name[index_to_update] == 2) {
gtkLabelSetText(lblVarCostFunction, "FuC = fp * a * E\nCoC = b * R\nOVC = c * E  +  d * E")

} else if (VARCOST_MODELS$model_name[index_to_update] == 3) {
gtkLabelSetText(lblVarCostFunction, "FuC = fp * a * E\nOVC = b * R")

} else if (VARCOST_MODELS$model_name[index_to_update] == 4) {
gtkLabelSetText(lblVarCostFunction, " VC  =  a * E")

} 

 
activate_deactivate_varcost_tables(VARCOST_MODELS$model_name[index_to_update])

mat_cfg_varCosts[2,2] <<- which(VARCOST_MODELS$option_name == selected)
print(paste("Option selected for variable costs calculation:", mat_cfg_varCosts[2,2] ), quote=F)    

if (index_to_update == 1) {
  VARCOSTS_head <<- COEFFICIENT_VARCOST_BIRDMOD_names
} else if (index_to_update == 2)  {
    VARCOSTS_head <<- COEFFICIENT_VARCOST_MEFISTO_names
} else if (index_to_update == 3)  {
    VARCOSTS_head <<- COEFFICIENT_VARCOST_FISHRENT_names
} else if (index_to_update == 4)  {
     VARCOSTS_head <<- COEFFICIENT_VARCOST_BEMMFISH_names
}

   # ------------------------------------------------------------ BIRDMOD
if (!is.na(mat_cfg_varCosts[2,2]) ) {

matrix_temp <-   mat_cfg_varCosts[4:(3+length(BMT_FLEETSEGMENTS)), 1:(length(varcost_cfg_names)+1)]

 cost_variable_matrix <- data.frame(matrix(0, nrow=length(VARCOSTS_head), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(cost_variable_matrix) <-  c(" ",BMT_FLEETSEGMENTS)
     cost_variable_matrix[,1] <- VARCOSTS_head

#if (VARCOST_MODELS$model_name[index_to_update]  == mat_cfg_varCosts[2,2]) {
for (flee in 1:length(BMT_FLEETSEGMENTS)) {
for (coeff in 1:length(VARCOSTS_head)) {
#if (coeff== 2 &  mat_cfg_varCosts[2,2] == 3) {
#       cost_variable_matrix[coeff, flee+1] <- as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.varCostFun.F", flee, sep="") ,(coeff+2)] ))
#} else {
       cost_variable_matrix[coeff, flee+1] <- as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.varCostFun.F", flee, sep="") ,(coeff+1)] ))
#}
}      
}

#}

# mefisto e fishrent


 bmt_fleet.cost_fuelprice  <<-  mat_cfg_varCosts[which(as.character(mat_cfg_varCosts[,1]) == "casestudy.fuelprice"), 1:(length(BMT_YEARS_FORECAST)+1)]
 if (all(is.na(as.numeric(as.character(bmt_fleet.cost_fuelprice [,2:ncol(bmt_fleet.cost_fuelprice )])))) ) {
   bmt_fleet.cost_fuelprice <<- NULL
 }

 
 bmt_reload_cost_fuelprice_table()
  
 bmt_fleet.cost_variable  <<- cost_variable_matrix
 bmt_reload_cost_variable_table()
     
}

}

} 
