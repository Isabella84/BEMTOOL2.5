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
bmt_add.behav_dyn <- function() {
#print("Adding elements to the list...")   
  if (!is.null(bmt_fleet.behav_dyn)) {
  for (r in 1:nrow(bmt_fleet.behav_dyn)) {
  behav_dyn_temp <- as.list(bmt_fleet.behav_dyn[r,]) 
  names(behav_dyn_temp) <-  c(" ",BMT_FLEETSEGMENTS)
  bmt_behav_dyn_list <<- c(bmt_behav_dyn_list, list(behav_dyn_temp)) 
  }
   } else {
   behav_dyn_matrix <- data.frame(matrix(0, nrow=length(BEHAV_DYN_head), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(behav_dyn_matrix) <-  c(" ",BMT_FLEETSEGMENTS)
     behav_dyn_matrix[,1] <- BEHAV_DYN_head
     if (length(BMT_FLEETSEGMENTS) != 0) {
   for (r in 1:nrow(behav_dyn_matrix)) { 
  behav_dyn_temp <- as.list(behav_dyn_matrix[r,]) 
  bmt_behav_dyn_list <<- c(bmt_behav_dyn_list, list(behav_dyn_temp)) 
  }
  }
 }
#print("behav_dyn (simulation) list successfully updated!", quote=F)
}