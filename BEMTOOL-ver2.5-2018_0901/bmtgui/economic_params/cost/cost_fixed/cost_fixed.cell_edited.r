# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
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
bmt_cost_fixed.cell_edited <- function(cell, path.string, new.text, data) {
  #checkPtrType(data, "cost_fixedkListStore")
  if (is.na(as.double(new.text) )) {
      showError("Value of coefficient for fixed costs functions must be numbers!")
 } else {
 bmt_cost_fixed.model <- data 
  path <- gtkTreePathNewFromString(path.string)
  print(paste("cost_fixed Edited row:", (as.numeric(path.string)+1)))
  column <- as.integer(cell$getData("column"))
  print(paste("cost_fixed Edited column:", column))
  iter <- bmt_cost_fixed.model$getIter(path)$iter
   #print(paste("new text:", new.text))
  	i <- path$getIndices()[[1]]+1
  	#print(paste("indice i:", i))
  #	print(pproductions[[i]])
  	bmt_cost_fixed_list[[i]][column+1] <<- as.double(new.text)           # [column+1]
  #	print(paste("indice column:", column+1))
  #	print(pproductions[[i]][column+1])
  	bmt_cost_fixed.model$set(iter, column, bmt_cost_fixed_list[[i]][column+1])

         for (r in 1:length(bmt_cost_fixed_list)) {
  bmt_fleet.cost_fixed[r,] <<- as.numeric(as.character(bmt_cost_fixed_list[[r]]))
  }
  
  for (nco in 2:ncol( bmt_fleet.cost_fixed)) {
mat_cfg_fixCosts[nco+2,2:(length(bmt_fleet.cost_fixed[,nco])+1)] <<- bmt_fleet.cost_fixed[,nco]
}

selected <- gtkComboBoxGetActiveText(bmt_combo_fixedcost_models)
mat_cfg_fixCosts[2,2] <<- which(FIXEDCOST_MODELS$option_name == selected)

print(mat_cfg_fixCosts)

}
}
