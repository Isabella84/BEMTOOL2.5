# ALADYM  Age lencost_variableh based dynamic model
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2013
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

#
#
#
#
#
#
#
#
#
#
# ------------------------------------------------------------------------------
# Function for the editing of the cells
# ------------------------------------------------------------------------------
#
bmt_cost_variable.cell_edited <- function(cell, path.string, new.text, data) {
  #checkPtrType(data, "cost_variablekListStore")
  if (is.na(as.double(new.text) )) {
      showError("Value of coefficient for variable costs functions must be numbers!")
 } else {
 bmt_cost_variable.model <- data 
  path <- gtkTreePathNewFromString(path.string)
  print(paste("cost_variable Edited row:", (as.numeric(path.string)+1)))
  column <- as.integer(cell$getData("column"))
  print(paste("cost_variable Edited column:", column))
  iter <- bmt_cost_variable.model$getIter(path)$iter
   #print(paste("new text:", new.text))
  	i <- path$getIndices()[[1]]+1
  	#print(paste("indice i:", i))
  #	print(pproductions[[i]])
  	bmt_cost_variable_list[[i]][column+1] <<- as.double(new.text)           # [column+1]
  #	print(paste("indice column:", column+1))
  #	print(pproductions[[i]][column+1])
  	bmt_cost_variable.model$set(iter, column, bmt_cost_variable_list[[i]][column+1])

         for (r in 1:length(bmt_cost_variable_list)) {
  bmt_fleet.cost_variable[r,] <<- as.double(as.character(bmt_cost_variable_list[[r]]) )
  }
  
for (nco in 2:ncol( bmt_fleet.cost_variable)) {
mat_cfg_varCosts[nco+2,2:(length(bmt_fleet.cost_variable[,nco])+1)] <<- bmt_fleet.cost_variable[,nco]
}

selected <- gtkComboBoxGetActiveText(bmt_combo_varcost_models)
mat_cfg_varCosts[2,2] <<- which(VARCOST_MODELS$option_name == selected)

print(mat_cfg_varCosts)
 

}
}
