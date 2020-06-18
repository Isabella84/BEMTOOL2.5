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
bmt_add.NUMBER_r4 <- function() {
#print("Adding elements to the list...")   
  if (!is.null(bmt_fleet.NUMBER_r4)) {
  for (r in 1:nrow(bmt_fleet.NUMBER_r4)) {
  NUMBER_r4_temp <- as.list(bmt_fleet.NUMBER_r4[r,]) 
  names(NUMBER_r4_temp) <- c("year",MONTHS)
  bmt_NUMBER_r4_list <<- c(bmt_NUMBER_r4_list, list(NUMBER_r4_temp)) 
  }
   } else {
   NUMBER_r4_matrix <- data.frame(matrix(0, nrow=length(BMT_YEARS_FORECAST), ncol=13))
   colnames(NUMBER_r4_matrix) <- c("year",MONTHS)
     NUMBER_r4_matrix$year <- BMT_YEARS_FORECAST
   for (r in 1:nrow(NUMBER_r4_matrix)) { 
  NUMBER_r4_temp <- as.list(NUMBER_r4_matrix[r,]) 
  bmt_NUMBER_r4_list <<- c(bmt_NUMBER_r4_list, list(NUMBER_r4_temp)) 
  }
 }
#print("NUMBER_r4 (simulation) list successfully updated!", quote=F)
}