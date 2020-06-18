# BEMTOOL - Bio-Economic Model TOOLs - version 2.0
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.






# ---------------------------------- edit event for the cell of monthly sex ratio table 
extErrorRecruitment.cell_edited <- function(cell, path.string, new.text, data) {
  #checkPtrType(data, "GtkListStore")
  extErrorRecruitment.model <- data 
  path <- gtkTreePathNewFromString(path.string)
  print(paste("External error recruitment Edited row:", (as.numeric(path.string)+1)))
  column <- as.integer(cell$getData("column"))
  print(paste("External error recruitment Edited column:", column))
  iter <- extErrorRecruitment.model$getIter(path)$iter
  # print(paste("new text:", new.text))
  	i <- path$getIndices()[[1]]+1
  #	print(paste("indice i:", i))
  #	print(extErrorRecruitment[[i]])
    	# extErrorRecruitment[[i]][column+1] <<- as.double(new.text)           # [column+1]     era così
  	extErrorRecruitment_list[[column+1]] <<- as.double(new.text)           # [column+1]
  #	print(paste("indice column:", column+1))
  #	print(extErrorRecruitment[[i]][column+1])
  
  # 	extErrorRecruitment.model$set(iter, column, extErrorRecruitment_list[[i]][column+1])     era così
  	extErrorRecruitment.model$set(iter, column, extErrorRecruitment_list[[column+1]])
}
