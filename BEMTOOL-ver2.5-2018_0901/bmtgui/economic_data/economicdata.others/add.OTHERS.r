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
bmt_add.OTHERS <- function() {
#print("Adding elements to the list...")   
  if (!is.null(bmt_economic.OTHERS)) {
  for (r in 1:nrow(bmt_economic.OTHERS)) {
  OTHERS_temp <- as.list(bmt_economic.OTHERS[r,]) 
  names(OTHERS_temp) <- c("year",OTHERS_vector)
  bmt_OTHERS_list <<- c(bmt_OTHERS_list, list(OTHERS_temp)) 
  }
   } else {
   OTHERS_matrix <- data.frame(matrix(0, nrow=length(years), ncol=(length(OTHERS_vector)+1)))
   colnames(OTHERS_matrix) <- c("year",OTHERS_vector)
     OTHERS_matrix$year <- years
   for (r in 1:nrow(OTHERS_matrix)) { 
  OTHERS_temp <- as.list(OTHERS_matrix[r,]) 
  bmt_OTHERS_list <<- c(bmt_OTHERS_list, list(OTHERS_temp)) 
  }
 }
#print("OTHERS (simulation) list successfully updated!", quote=F)
}