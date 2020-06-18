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
bmt_ecoind.cell_edited <- function(cell, path.string, new.text, data) {
  #checkPtrType(data, "ecoindkListStore")
  if (is.na(as.double(new.text) )) {
      showError("Coefficients for Indicators params must be numbers!")
 } else {
 bmt_ecoind.model <- data 
  path <- gtkTreePathNewFromString(path.string)
  print(paste("ecoind Edited row:", (as.numeric(path.string)+1)))
  column <- as.integer(cell$getData("column"))
  print(paste("ecoind Edited column:", column))
  iter <- bmt_ecoind.model$getIter(path)$iter
   #print(paste("new text:", new.text))
  	i <- path$getIndices()[[1]]+1
  	#print(paste("indice i:", i))
  #	print(pproductions[[i]])
  	bmt_ecoind_list[[i]][column+1] <<- as.double(new.text)           # [column+1]
  #	print(paste("indice column:", column+1))
  #	print(pproductions[[i]][column+1])
  	bmt_ecoind.model$set(iter, column, bmt_ecoind_list[[i]][column+1])

         for (r in 1:length(bmt_ecoind_list)) {
  bmt_fleet.ecoind[r,] <<- as.numeric(as.character(bmt_ecoind_list[[r]]))
  }
  
    for (nco in 2:ncol( bmt_fleet.ecoind)) {
mat_cfg_EconomicIndicator[nco+2,2:(length(BMT_YEARS_FORECAST)+1)] <<- as.character(bmt_fleet.ecoind[,nco])
}

        
print(mat_cfg_EconomicIndicator)

}
}
