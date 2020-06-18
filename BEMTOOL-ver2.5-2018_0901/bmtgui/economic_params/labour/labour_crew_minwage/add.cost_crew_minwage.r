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
# add elements to the list of the p production values
# ------------------------------------------------------------------------------
#
bmt_add.cost_crew_minwage <- function() {
#print("Adding elements to the list...")   
  if (!is.null(bmt_fleet.cost_crew_minwage)) {
  for (r in 1:nrow(bmt_fleet.cost_crew_minwage)) {
  cost_crew_minwage_temp <- as.list(bmt_fleet.cost_crew_minwage[r,]) 
  names(cost_crew_minwage_temp) <-  c(" ",BMT_FLEETSEGMENTS)
  bmt_cost_crew_minwage_list <<- c(bmt_cost_crew_minwage_list, list(cost_crew_minwage_temp)) 
  }
   } else {
   cost_crew_minwage_matrix <- data.frame(matrix(0, nrow=length(LABOURCOST_head), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(cost_crew_minwage_matrix) <-  c(" ",BMT_FLEETSEGMENTS)
     cost_crew_minwage_matrix[,1] <- LABOURCOST_head
     if (length(BMT_FLEETSEGMENTS) != 0) {
   for (r in 1:nrow(cost_crew_minwage_matrix)) { 
  cost_crew_minwage_temp <- as.list(cost_crew_minwage_matrix[r,]) 
  bmt_cost_crew_minwage_list <<- c(bmt_cost_crew_minwage_list, list(cost_crew_minwage_temp)) 
  }
  }
 }
#print("cost_crew_minwage (simulation) list successfully updated!", quote=F)
}