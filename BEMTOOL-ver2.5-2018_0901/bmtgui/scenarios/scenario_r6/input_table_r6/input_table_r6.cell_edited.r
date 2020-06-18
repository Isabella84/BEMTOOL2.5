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
input_table_r6.cell_edited <- function(cell, path.string, new.text, data) {
    column <- as.integer(cell$getData("column"))
  if (is.na(as.double(new.text) )) {
  if (column != 1) {
  print(column)
	  showError("Value for input table for Change of Total F must be a number!")
	  }
 } else {
 input_table_r6.model <- data 
  path <- gtkTreePathNewFromString(path.string)
  print(paste("Input_table_r6 Edited row:", (as.numeric(path.string)+1)))
  print(paste("Input_table_r6 Edited column:", column))
  
  iter <- input_table_r6.model$getIter(path)$iter
   #print(paste("new text:", new.text))
  	i <- path$getIndices()[[1]]+1
  	#print(paste("indice i:", i))
  #	print(pproductions[[i]])
  	input_table_r6_list[[i]][column+1] <<- as.double(new.text)           # [column+1]
  #	print(paste("indice column:", column+1))
  #	print(pproductions[[i]][column+1])
  	input_table_r6.model$set(iter, column, input_table_r6_list[[i]][column+1])

         for (r in 1:length(input_table_r6_list)) {
  input_table_r6[r,] <<- as.numeric(as.character(input_table_r6_list[[r]]))
  }

}
}
