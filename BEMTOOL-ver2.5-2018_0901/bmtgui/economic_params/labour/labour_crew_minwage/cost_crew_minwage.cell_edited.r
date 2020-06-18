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
bmt_cost_crew_minwage.cell_edited <- function(cell, path.string, new.text, data) {
  #checkPtrType(data, "cost_crew_minwagekListStore")
  if (is.na(as.double(new.text) )) {
      showError("Value of coefficients for labour functions must be numbers!")
 } else {
 bmt_cost_crew_minwage.model <- data 
  path <- gtkTreePathNewFromString(path.string)
  print(paste("cost_crew_minwage Edited row:", (as.numeric(path.string)+1)))
  column <- as.integer(cell$getData("column"))
  print(paste("cost_crew_minwage Edited column:", column))
  iter <- bmt_cost_crew_minwage.model$getIter(path)$iter
   #print(paste("new text:", new.text))
  	i <- path$getIndices()[[1]]+1
  	#print(paste("indice i:", i))
  #	print(pproductions[[i]])
  	bmt_cost_crew_minwage_list[[i]][column+1] <<- as.double(new.text)           # [column+1]
  #	print(paste("indice column:", column+1))
  #	print(pproductions[[i]][column+1])
  	bmt_cost_crew_minwage.model$set(iter, column, bmt_cost_crew_minwage_list[[i]][column+1])

         for (r in 1:length(bmt_cost_crew_minwage_list)) {
  bmt_fleet.cost_crew_minwage[r,] <<- as.numeric(as.character(bmt_cost_crew_minwage_list[[r]]))
  }
  
   
mat_cfg_labCosts[4:nrow(mat_cfg_labCosts),2] <<- as.character(bmt_fleet.cost_crew_minwage[1,2:(length(BMT_FLEETSEGMENTS)+1)])
mat_cfg_labCosts[4:nrow(mat_cfg_labCosts),6] <<- as.character(bmt_fleet.cost_crew_minwage[2,2:(length(BMT_FLEETSEGMENTS)+1)])

print(mat_cfg_labCosts)
  

}
}
