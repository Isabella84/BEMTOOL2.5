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
bmt_cost_equipment.cell_edited <- function(cell, path.string, new.text, data) {
  #checkPtrType(data, "indic_taxeskListStore")
  if (is.na(as.double(new.text) )) {
      showError("Value for cost equipment must be a number!")
 } else {
 bmt_cost_equipment.model <- data 
  path <- gtkTreePathNewFromString(path.string)
  print(paste("cost_equipment Edited row:", (as.numeric(path.string)+1)))
  column <- as.integer(cell$getData("column"))
  print(paste("cost_equipment Edited column:", column))
  iter <- bmt_cost_equipment.model$getIter(path)$iter
   #print(paste("new text:", new.text))
  	i <- path$getIndices()[[1]]+1
  	#print(paste("indice i:", i))
  #	print(pproductions[[i]])
  	bmt_cost_equipment_list[[i]][column+1] <<- as.double(new.text)           # [column+1]
  #	print(paste("indice column:", column+1))
  #	print(pproductions[[i]][column+1])
  	bmt_cost_equipment.model$set(iter, column, bmt_cost_equipment_list[[i]][column+1])

         for (r in 1:length(bmt_cost_equipment_list)) {
  bmt_fleet.cost_equipment[r,] <<- as.numeric(as.character(bmt_cost_equipment_list[[r]]))
  }
  
for (nro in 1:nrow( bmt_fleet.cost_equipment)) {
mat_cfg_EconomicIndicator[length(BMT_FLEETSEGMENTS)*3+nro+6,2:(length(BMT_YEARS_FORECAST)+1)] <<- as.character(bmt_fleet.cost_equipment[nro, 2:ncol(bmt_fleet.cost_equipment)])
}

print(mat_cfg_EconomicIndicator)

}
}
