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
bmt_add.behav_progr <- function() {
#print("Adding elements to the list...")   
  if (!is.null(bmt_fleet.behav_progr)) {
  for (r in 1:nrow(bmt_fleet.behav_progr)) {
  behav_progr_temp <- as.list(bmt_fleet.behav_progr[r,]) 
  names(behav_progr_temp) <-  c(" ",BMT_FLEETSEGMENTS)
  bmt_behav_progr_list <<- c(bmt_behav_progr_list, list(behav_progr_temp)) 
  }
   } else {
   behav_progr_matrix <- data.frame(matrix(0, nrow=length(BEHAV_PROGR_head), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(behav_progr_matrix) <-  c(" ",BMT_FLEETSEGMENTS)
     behav_progr_matrix[,1] <- BEHAV_PROGR_head
     if (length(BMT_FLEETSEGMENTS) != 0) {
   for (r in 1:nrow(behav_progr_matrix)) { 
  behav_progr_temp <- as.list(behav_progr_matrix[r,]) 
  bmt_behav_progr_list <<- c(bmt_behav_progr_list, list(behav_progr_temp)) 
  }
  }
 }
#print("behav_progr (simulation) list successfully updated!", quote=F)
}