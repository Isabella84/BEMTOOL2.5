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
bmt_add.DAY <- function() {
#print("Adding elements to the list...")   
  if (!is.null(bmt_fleet.DAY)) {
  for (r in 1:nrow(bmt_fleet.DAY)) {
  DAY_temp <- as.list(bmt_fleet.DAY[r,]) 
  names(DAY_temp) <- c("year",MONTHS)
  bmt_DAY_list <<- c(bmt_DAY_list, list(DAY_temp)) 
  }
   } else {
   DAY_matrix <- data.frame(matrix(0, nrow=length(years), ncol=13))
   colnames(DAY_matrix) <- c("year",MONTHS)
     DAY_matrix$year <- years
   for (r in 1:nrow(DAY_matrix)) { 
  DAY_temp <- as.list(DAY_matrix[r,]) 
  bmt_DAY_list <<- c(bmt_DAY_list, list(DAY_temp)) 
  }
 }
#print("DAY (simulation) list successfully updated!", quote=F)
}