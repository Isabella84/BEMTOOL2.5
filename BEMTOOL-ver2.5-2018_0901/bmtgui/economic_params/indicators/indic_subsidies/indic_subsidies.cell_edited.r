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
# ------------------------------------------------------------------------------
# Function for the editing of the cells
# ------------------------------------------------------------------------------
#
bmt_indic_subsidies.cell_edited <- function(cell, path.string, new.text, data) {
  #checkPtrType(data, "indic_subsidieskListStore")
  if (is.na(as.double(new.text) )) {
      showError("Value for additional taxes must be a number!")
 } else {
 bmt_indic_subsidies.model <- data 
  path <- gtkTreePathNewFromString(path.string)
  print(paste("indic_subsidies Edited row:", (as.numeric(path.string)+1)))
  column <- as.integer(cell$getData("column"))
  print(paste("indic_subsidies Edited column:", column))
  iter <- bmt_indic_subsidies.model$getIter(path)$iter
   #print(paste("new text:", new.text))
  	i <- path$getIndices()[[1]]+1
  	#print(paste("indice i:", i))
  #	print(pproductions[[i]])
  	bmt_indic_subsidies_list[[i]][column+1] <<- as.double(new.text)           # [column+1]
  #	print(paste("indice column:", column+1))
  #	print(pproductions[[i]][column+1])
  	bmt_indic_subsidies.model$set(iter, column, bmt_indic_subsidies_list[[i]][column+1])

         for (r in 1:length(bmt_indic_subsidies_list)) {
  bmt_fleet.indic_subsidies[r,] <<- as.numeric(as.character(bmt_indic_subsidies_list[[r]]))
  }

      for (nro in 1:nrow( bmt_fleet.indic_subsidies)) {
mat_cfg_EconomicIndicator[length(BMT_FLEETSEGMENTS)+nro+4,2:(length(BMT_YEARS_FORECAST)+1)] <<- as.character(bmt_fleet.indic_subsidies[nro,2:ncol(bmt_fleet.indic_subsidies)])
}

print(mat_cfg_EconomicIndicator)

}
}
