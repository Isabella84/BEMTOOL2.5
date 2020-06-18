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
#
# ------------------------------------------------------------------------------
# add elements to the list of the p production values
# ------------------------------------------------------------------------------
#
bmt_add.cost_variable <- function() {
#print("Adding elements to the list...")   
  if (!is.null(bmt_fleet.cost_variable)) {
  for (r in 1:nrow(bmt_fleet.cost_variable)) {
  cost_variable_temp <- as.list(bmt_fleet.cost_variable[r,]) 
  names(cost_variable_temp) <-  c(" ",BMT_FLEETSEGMENTS)
  bmt_cost_variable_list <<- c(bmt_cost_variable_list, list(cost_variable_temp)) 
  }
   } else {
   cost_variable_matrix <- data.frame(matrix(0, nrow=length(VARCOSTS_head), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(cost_variable_matrix) <-  c(" ",BMT_FLEETSEGMENTS)
     cost_variable_matrix[,1] <- VARCOSTS_head
     if (length(VARCOSTS_head) != 0) {
   for (r in 1:nrow(cost_variable_matrix)) { 
  cost_variable_temp <- as.list(cost_variable_matrix[r,]) 
  bmt_cost_variable_list <<- c(bmt_cost_variable_list, list(cost_variable_temp)) 
  }
  }
 }
#print("cost_variable (simulation) list successfully updated!", quote=F)
}