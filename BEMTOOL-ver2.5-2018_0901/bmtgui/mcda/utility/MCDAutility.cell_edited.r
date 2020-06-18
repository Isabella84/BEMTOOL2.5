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
MCDAutility.cell_edited <- function(cell, path.string, new.text, data) {
  #checkPtrType(data, "behav_actkListStore")
  if (is.na(as.double(new.text) )) {
  # non da errore nel caso della penultima riga
      showError("MCDA utility must be a number!")
 } else {
 MCDAutility.model <- data 
  path <- gtkTreePathNewFromString(path.string)
  print(paste("MCDAutility Edited row:", (as.numeric(path.string)+1)))
  column <- as.integer(cell$getData("column"))
  print(paste("MCDAutility Edited column:", column))
  iter <- MCDAutility.model$getIter(path)$iter
   #print(paste("new text:", new.text))
  	i <- path$getIndices()[[1]]+1
  	#print(paste("indice i:", i))
  #	print(pproductions[[i]])
  	MCDAutility_list[[i]][column+1] <<- as.numeric(as.character(new.text ))         # [column+1]
  #	print(paste("indice column:", column+1))
  #	print(pproductions[[i]][column+1])
  	MCDAutility.model$set(iter, column, MCDAutility_list[[i]][column+1])

         for (r in 1:length(MCDAutility_list)) {
  MCDAutility_table[r,] <<- as.numeric(as.character(MCDAutility_list[[r]]))
  }

}
}
