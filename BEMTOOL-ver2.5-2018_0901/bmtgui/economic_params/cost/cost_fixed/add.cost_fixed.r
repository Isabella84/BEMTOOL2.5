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
bmt_add.cost_fixed <- function() {
#print("Adding elements to the list...")   
  if (!is.null(bmt_fleet.cost_fixed)) {
  for (r in 1:nrow(bmt_fleet.cost_fixed)) {
  cost_fixed_temp <- as.list(bmt_fleet.cost_fixed[r,]) 
  names(cost_fixed_temp) <-  c(" ",BMT_FLEETSEGMENTS)
  bmt_cost_fixed_list <<- c(bmt_cost_fixed_list, list(cost_fixed_temp)) 
  }
   } else {
   cost_fixed_matrix <- data.frame(matrix(0, nrow=length(FIXEDCOSTS_head), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(cost_fixed_matrix) <-  c(" ",BMT_FLEETSEGMENTS)
     cost_fixed_matrix[,1] <- FIXEDCOSTS_head
     if (length(FIXEDCOSTS_head) != 0) {
   for (r in 1:nrow(cost_fixed_matrix)) { 
  cost_fixed_temp <- as.list(cost_fixed_matrix[r,]) 
  bmt_cost_fixed_list <<- c(bmt_cost_fixed_list, list(cost_fixed_temp)) 
  }
  }
 }
#print("cost_fixed (simulation) list successfully updated!", quote=F)
}