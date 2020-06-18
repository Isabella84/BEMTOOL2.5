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
# ------------------------------------------------------------------------------
# Function for the editing of the cells
# ------------------------------------------------------------------------------
#
price_elast_discard_byfleet.cell_edited <- function(cell, path.string, new.text, data) {
  #checkPtrType(data, "price_elast_landing_byfleetkListStore")
  if (is.na(as.double(new.text) )) {
      showError("Value of coefficients for discard price functions must be numbers!")
 } else {
price_elast_discard_byfleet.model <- data 
  path <- gtkTreePathNewFromString(path.string)
  print(paste("price_elast_discard_byfleet Edited row:", (as.numeric(path.string)+1)))
  column <- as.integer(cell$getData("column"))
  print(paste("price_elast_discard_byfleet Edited column:", column))
  iter <- price_elast_discard_byfleet.model$getIter(path)$iter
   #print(paste("new text:", new.text))
  	i <- path$getIndices()[[1]]+1
  	#print(paste("indice i:", i))
  #	print(pproductions[[i]])
  	price_elast_discard_byfleet_list[[i]][column+1] <<- as.double(new.text)           # [column+1]
  #	print(paste("indice column:", column+1))
  #	print(pproductions[[i]][column+1])
  	price_elast_discard_byfleet.model$set(iter, column, price_elast_discard_byfleet_list[[i]][column+1])

  for (r in 1:length(price_elast_discard_byfleet_list)) {
  price_elast_discard_byfleet_MATRIX[r,] <<- as.character(price_elast_discard_byfleet_list[[r]])
  }
  
elasti <- c()
for (nco in 2:ncol( price_elast_discard_byfleet_MATRIX)) {
    elasti <- c(elasti, price_elast_discard_byfleet_MATRIX[,nco])
}

mat_cfg_price[4:(length(BMT_SPECIES)*length(BMT_FLEETSEGMENTS)+3),5]  <<-  as.numeric(as.character(elasti))

selected <- gtkComboBoxGetActiveText(bmt_combo_price_models_discard)
#sistemareeeeeeeeeeeeeeeeeeeeeeeeeeeeee
mat_cfg_price[2,3] <<- which(PRICE_MODELS_DISCARD$option_name == selected)

print(mat_cfg_price)

}
}
