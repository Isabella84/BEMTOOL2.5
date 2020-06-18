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
bmt_add.labour_commercial <- function() {
#print("Adding elements to the list...")   
  if (!is.null(bmt_fleet.labour_commercial)) {
  for (r in 1:nrow(bmt_fleet.labour_commercial)) {
  labour_commercial_temp <- as.list(bmt_fleet.labour_commercial[r,]) 
  names(labour_commercial_temp) <-  c(BMT_FLEETSEGMENTS)
  bmt_labour_commercial_list <<- c(bmt_labour_commercial_list, list(labour_commercial_temp)) 
  }
   } else {
   labour_commercial_matrix <- data.frame(matrix(0, nrow=1, ncol=length(BMT_FLEETSEGMENTS)))
   colnames(labour_commercial_matrix) <-  c(BMT_FLEETSEGMENTS)
#     labour_commercial_matrix[1,1] <- c(" fuel costs in labour costs dynamic ")
     if (length(BMT_FLEETSEGMENTS) != 0) {
   for (r in 1:nrow(labour_commercial_matrix)) { 
  labour_commercial_temp <- as.list(labour_commercial_matrix[r,]) 
  bmt_labour_commercial_list <<- c(bmt_labour_commercial_list, list(labour_commercial_temp)) 
  }
  }
 }
#print("labour_commercial (simulation) list successfully updated!", quote=F)
}