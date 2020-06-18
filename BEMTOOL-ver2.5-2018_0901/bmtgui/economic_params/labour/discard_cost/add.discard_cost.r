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
bmt_add.discard_cost <- function() {
#print("Adding elements to the list...")   
  if (!is.null(bmt_fleet.discard_cost)) {
  for (r in 1:nrow(bmt_fleet.discard_cost)) {
  discard_cost_temp <- as.list(bmt_fleet.discard_cost[r,]) 
  names(discard_cost_temp) <-  c(" ",BMT_YEARS_FORECAST)
  bmt_discard_cost_list <<- c(bmt_discard_cost_list, list(discard_cost_temp)) 
  }
   } else {
   discard_cost_matrix <- data.frame(matrix(0, nrow=1, ncol=(length(BMT_YEARS_FORECAST)+1)))
   colnames(discard_cost_matrix) <-  c("FleetSegment",BMT_YEARS_FORECAST)
     discard_cost_matrix$FleetSegment <- paste(" ", BMT_FLEETSEGMENTS, " ", sep="")
     if (length(BMT_SPECIES) != 0) {
   for (r in 1:nrow(discard_cost_matrix)) { 
  discard_cost_temp <- as.list(discard_cost_matrix[r,]) 
  bmt_discard_cost_list <<- c(bmt_discard_cost_list, list(discard_cost_temp)) 
  }
  }
 }
#print("discard_cost (simulation) list successfully updated!", quote=F)
}