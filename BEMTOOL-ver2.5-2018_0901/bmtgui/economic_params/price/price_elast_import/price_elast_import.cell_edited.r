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
bmt_price_elast_import.cell_edited <- function(cell, path.string, new.text, data) {
  #checkPtrType(data, "price_elast_importkListStore")
  if (is.na(as.double(new.text) )) {
      showError("Value of coefficients for price functions must be numbers!")
 } else {
 bmt_price_elast_import.model <- data 
  path <- gtkTreePathNewFromString(path.string)
  print(paste("price_elast_import Edited row:", (as.numeric(path.string)+1)))
  column <- as.integer(cell$getData("column"))
  print(paste("price_elast_import Edited column:", column))
  iter <- bmt_price_elast_import.model$getIter(path)$iter
   #print(paste("new text:", new.text))
  	i <- path$getIndices()[[1]]+1
  	#print(paste("indice i:", i))
  #	print(pproductions[[i]])
  	bmt_price_elast_import_list[[i]][column+1] <<- as.double(new.text)           # [column+1]
  #	print(paste("indice column:", column+1))
  #	print(pproductions[[i]][column+1])
  	bmt_price_elast_import.model$set(iter, column, bmt_price_elast_import_list[[i]][column+1])

         for (r in 1:length(bmt_price_elast_import_list)) {
  bmt_fleet.price_elast_import[r,] <<- as.double(as.character(bmt_price_elast_import_list[[r]]))
  }

  elasti <- c()
for (nco in 2:ncol( bmt_fleet.price_elast_import)) {
    elasti <- c(elasti, bmt_fleet.price_elast_import[,nco])
}

mat_cfg_price[4:(length(BMT_SPECIES)*length(BMT_FLEETSEGMENTS)+3),3]  <<-  as.numeric(as.character(elasti))

selected <- gtkComboBoxGetActiveText(bmt_combo_price_models)
mat_cfg_price[2,2] <<- which(PRICE_MODELS$option_name == selected)

print(mat_cfg_price)
  

}
}
