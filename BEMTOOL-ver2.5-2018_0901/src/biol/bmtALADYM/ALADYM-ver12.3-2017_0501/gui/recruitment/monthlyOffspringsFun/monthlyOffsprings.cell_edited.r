# BEMTOOL - Bio-Economic Model TOOLs - version 2.0
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





# ---------------------------------- edit event for the cell of monthly sex ratio table 
monthlyOffsprings.cell_edited <- function(cell, path.string, new.text, data) {
  #checkPtrType(data, "GtkListStore")
  monthlyOffsprings.model <- data 
  path <- gtkTreePathNewFromString(path.string)
  print(paste("Monthly recruitment Edited row:", (as.numeric(path.string)+1)))
  column <- as.integer(cell$getData("column"))
  print(paste("Monthly recruitment Edited column:", column))
  iter <- monthlyOffsprings.model$getIter(path)$iter
  # print(paste("new text:", new.text))
  	i <- path$getIndices()[[1]]+1
  #	print(paste("indice i:", i))
  #	print(monthlyOffsprings[[i]])
    	# monthlyOffsprings[[i]][column+1] <<- as.double(new.text)           # [column+1]     era così
  	monthlyOffsprings[[column+1]] <<- as.double(new.text)           # [column+1]
  #	print(paste("indice column:", column+1))
  #	print(monthlyOffsprings[[i]][column+1])
  
  # 	monthlyOffsprings.model$set(iter, column, monthlyOffsprings[[i]][column+1])     era così
  	monthlyOffsprings.model$set(iter, column, monthlyOffsprings[[column+1]])
}
