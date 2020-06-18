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
# add elements to the list of the p production values
# ------------------------------------------------------------------------------
#
bmt_add.COSTS <- function() {
#print("Adding elements to the list...")   
  if (!is.null(bmt_economic.COSTS)) {
  for (r in 1:nrow(bmt_economic.COSTS)) {
  COSTS_temp <- as.list(bmt_economic.COSTS[r,]) 
  names(COSTS_temp) <- c("year",COSTS_vector)
  bmt_COSTS_list <<- c(bmt_COSTS_list, list(COSTS_temp)) 
  }
   } else {
   COSTS_matrix <- data.frame(matrix(0, nrow=length(years), ncol=(length(COSTS_vector)+1)))
   colnames(COSTS_matrix) <- c("year",COSTS_vector)
     COSTS_matrix$year <- years
   for (r in 1:nrow(COSTS_matrix)) { 
  COSTS_temp <- as.list(COSTS_matrix[r,]) 
  bmt_COSTS_list <<- c(bmt_COSTS_list, list(COSTS_temp)) 
  }
 }
#print("COSTS (simulation) list successfully updated!", quote=F)
}