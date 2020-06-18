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
bmt_add.labour_sorting <- function() {
#print("Adding elements to the list...")   
  if (!is.null(bmt_fleet.labour_sorting)) {
  for (r in 1:nrow(bmt_fleet.labour_sorting)) {
  labour_sorting_temp <- as.list(bmt_fleet.labour_sorting[r,]) 
  names(labour_sorting_temp) <-  c(" ",BMT_FLEETSEGMENTS)
  bmt_labour_sorting_list <<- c(bmt_labour_sorting_list, list(labour_sorting_temp)) 
  }
   } else {
   labour_sorting_matrix <- data.frame(matrix(0, nrow=1, ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(labour_sorting_matrix) <-  c(" ",BMT_FLEETSEGMENTS)
     labour_sorting_matrix[1,1] <- " sorting coefficient "
     if (length(BMT_FLEETSEGMENTS) != 0) {
   for (r in 1:nrow(labour_sorting_matrix)) { 
  labour_sorting_temp <- as.list(labour_sorting_matrix[r,]) 
  bmt_labour_sorting_list <<- c(bmt_labour_sorting_list, list(labour_sorting_temp)) 
  }
  }
 }
#print("labour_sorting (simulation) list successfully updated!", quote=F)
}