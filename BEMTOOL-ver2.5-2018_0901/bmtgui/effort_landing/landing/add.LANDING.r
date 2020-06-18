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
bmt_add.LANDING <- function() {
#print("Adding elements to the list...")   
  if (!is.null(bmt_fleet.LANDING)) {
  for (r in 1:nrow(bmt_fleet.LANDING)) {
  LANDING_temp <- as.list(bmt_fleet.LANDING[r,]) 
  names(LANDING_temp) <- c("year",MONTHS)
  bmt_LANDING_list <<- c(bmt_LANDING_list, list(LANDING_temp)) 
  }
   } else {
   LANDING_matrix <- data.frame(matrix(0, nrow=length(years), ncol=13))
   colnames(LANDING_matrix) <- c("year",MONTHS)
     LANDING_matrix$year <- years
   for (r in 1:nrow(LANDING_matrix)) { 
  LANDING_temp <- as.list(LANDING_matrix[r,]) 
  bmt_LANDING_list <<- c(bmt_LANDING_list, list(LANDING_temp)) 
  }
 }
#print("LANDING (simulation) list successfully updated!", quote=F)
}