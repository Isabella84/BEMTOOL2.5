# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
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
# ------------------------------------------------------------------------------
# Function for the editing of the cells
# ------------------------------------------------------------------------------
#
FISHINGEFFORT_fore.cell_edited <- function(cell, path.string, new.text, data) {
  
  #checkPtrType(data, "GtkListStore")
 FISHINGEFFORT_fore.model <- data 
  path <- gtkTreePathNewFromString(path.string)
  print(paste("forecast Edited row:", (as.numeric(path.string)+1)))
  column <- as.integer(cell$getData("column"))
  print(paste("forecast Edited column:", column))
  iter <- FISHINGEFFORT_fore.model$getIter(path)$iter
   #print(paste("new text:", new.text))
  	i <- path$getIndices()[[1]]+1
  #	print(paste("indice i:", i))
  #	print(activities_fore[[i]])
  	FISHINGEFFORT_fore[[i]][column+1] <<- as.double(new.text)           # [column+1]
  #	print(paste("indice column:", column+1))
  #	print(activities_fore[[i]][column+1])
  	FISHINGEFFORT_fore.model$set(iter, column, FISHINGEFFORT_fore[[i]][column+1])
}
