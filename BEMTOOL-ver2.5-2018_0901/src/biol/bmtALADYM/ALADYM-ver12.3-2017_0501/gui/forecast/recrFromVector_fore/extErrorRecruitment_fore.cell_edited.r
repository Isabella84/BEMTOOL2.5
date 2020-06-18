# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



# ---------------------------------- edit event for the cell of monthly sex ratio table 
extErrorRecruitment_fore.cell_edited <- function(cell, path.string, new.text, data) {
  #checkPtrType(data, "GtkListStore")
  extErrorRecruitment_fore.model <- data 
  path <- gtkTreePathNewFromString(path.string)
  print(paste("External error recruitment Edited row:", (as.numeric(path.string)+1)))
  column <- as.integer(cell$getData("column"))
  print(paste("External error recruitment Edited column:", column))
  iter <- extErrorRecruitment_fore.model$getIter(path)$iter
  # print(paste("new text:", new.text))
  	i <- path$getIndices()[[1]]+1
  #	print(paste("indice i:", i))
  #	print(extErrorRecruitment_fore[[i]])
    	# extErrorRecruitment_fore[[i]][column+1] <<- as.double(new.text)           # [column+1]     era così
  	extErrorRecruitment_fore_list[[column+1]] <<- as.double(new.text)           # [column+1]
  #	print(paste("indice column:", column+1))
  #	print(extErrorRecruitment_fore[[i]][column+1])
  
  # 	extErrorRecruitment_fore.model$set(iter, column, extErrorRecruitment_fore_list[[i]][column+1])     era così
  	extErrorRecruitment_fore.model$set(iter, column, extErrorRecruitment_fore_list[[column+1]])
}
