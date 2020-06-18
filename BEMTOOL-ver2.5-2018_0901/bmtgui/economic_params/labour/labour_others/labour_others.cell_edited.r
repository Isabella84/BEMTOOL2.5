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
# ------------------------------------------------------------------------------
# Function for the editing of the cells
# ------------------------------------------------------------------------------
#
bmt_labour_others.cell_edited <- function(cell, path.string, data) {
  #checkPtrType(data, "labour_otherskListStore")

   #  print(paste("IS ACTIVE ??????????????????????? ", gtkCellRendererToggleGetActive(cell)))

 bmt_labour_others.model <- data 
  path <- gtkTreePathNewFromString(path.string)
  print(paste("labour_others Edited row:", (as.numeric(path.string)+1)))
  column <- as.integer(cell$getData("column"))
  print(paste("labour_others Edited column:", column))
  iter <- bmt_labour_others.model$getIter(path)$iter
  	i <- path$getIndices()[[1]]+1

#    value <-  bmt_labour_others_list[[i]][column+1][[1]]
    print(  paste("PRIMA",	bmt_labour_others_list[[i]][column+1][[1]] ))
       
    bmt_labour_others_list[[i]][column+1][[1]] <<- ifelse(bmt_labour_others_list[[i]][column+1][[1]], FALSE, TRUE)
        
       gtkCellRendererToggleSetActive(cell,  bmt_labour_others_list[[i]][column+1][[1]])

  print(    paste("DOPO",	bmt_labour_others_list[[i]][column+1][[1]] ))
  bmt_labour_others.model$set(iter, column, bmt_labour_others_list[[i]][column+1][[1]] )   # , 

         for (r in 1:length(bmt_labour_others_list)) {
  bmt_fleet.labour_others[r,] <<- as.character(bmt_labour_others_list[[r]])
  }
  
  for (nc in 1:ncol(bmt_fleet.labour_others)) {
      if (as.logical(bmt_fleet.labour_others[1,nc])) {
            mat_cfg_labCosts[3+nc,5] <<- "1"
      } else {
            mat_cfg_labCosts[3+nc,5] <<- "0"
      }
  }

print(mat_cfg_labCosts)

}
