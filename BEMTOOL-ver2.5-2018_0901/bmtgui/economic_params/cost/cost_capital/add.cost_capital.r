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
bmt_add.cost_capital <- function() {
#print("Adding elements to the list...")   
  if (!is.null(bmt_fleet.cost_capital)) {
  for (r in 1:nrow(bmt_fleet.cost_capital)) {
  cost_capital_temp <- as.list(bmt_fleet.cost_capital[r,]) 
  names(cost_capital_temp) <-  c(" ",BMT_FLEETSEGMENTS)
  bmt_cost_capital_list <<- c(bmt_cost_capital_list, list(cost_capital_temp)) 
  }
   } else {
   cost_capital_matrix <- data.frame(matrix(0, nrow=length(CAPITALCOSTS_head), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(cost_capital_matrix) <-  c(" ",BMT_FLEETSEGMENTS)
     cost_capital_matrix[,1] <- CAPITALCOSTS_head
     if (length(CAPITALCOSTS_head) != 0) {
   for (r in 1:nrow(cost_capital_matrix)) { 
  cost_capital_temp <- as.list(cost_capital_matrix[r,]) 
  bmt_cost_capital_list <<- c(bmt_cost_capital_list, list(cost_capital_temp)) 
  }
  }
 }
#print("cost_capital (simulation) list successfully updated!", quote=F)
}