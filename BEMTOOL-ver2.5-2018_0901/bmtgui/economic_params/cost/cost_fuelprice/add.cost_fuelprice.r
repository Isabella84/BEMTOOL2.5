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
bmt_add.cost_fuelprice <- function() {
#print("Adding elements to the list...")   
  if (!is.null(bmt_fleet.cost_fuelprice)) {
  for (r in 1:nrow(bmt_fleet.cost_fuelprice)) {
  cost_fuelprice_temp <- as.list(bmt_fleet.cost_fuelprice[r,]) 
  names(cost_fuelprice_temp) <-  c(" ",BMT_YEARS_FORECAST)
  bmt_cost_fuelprice_list <<- c(bmt_cost_fuelprice_list, list(cost_fuelprice_temp)) 
  }
   } else {
   cost_fuelprice_matrix <- data.frame(matrix(0, nrow=1, ncol=(length(BMT_YEARS_FORECAST)+1)))
   colnames(cost_fuelprice_matrix) <-  c(" ",BMT_YEARS_FORECAST)
     cost_fuelprice_matrix[,1] <- " fuel price "
     if (length(BMT_SPECIES) != 0) {
   for (r in 1:nrow(cost_fuelprice_matrix)) { 
  cost_fuelprice_temp <- as.list(cost_fuelprice_matrix[r,]) 
  bmt_cost_fuelprice_list <<- c(bmt_cost_fuelprice_list, list(cost_fuelprice_temp)) 
  }
  }
 }
#print("cost_fuelprice (simulation) list successfully updated!", quote=F)
}