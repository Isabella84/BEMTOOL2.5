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
bmt_add.behav_act <- function() {
#print("Adding elements to the list...")   
  if (!is.null(bmt_fleet.behav_act)) {
  for (r in 1:nrow(bmt_fleet.behav_act)) {
  behav_act_temp <- as.list(bmt_fleet.behav_act[r,]) 
  names(behav_act_temp) <-  c(" ",BMT_FLEETSEGMENTS)
  bmt_behav_act_list <<- c(bmt_behav_act_list, list(behav_act_temp)) 
  }
   } else {
   behav_act_matrix <- data.frame(matrix(0, nrow=length(BEHAV_ACT_head), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(behav_act_matrix) <-  c(" ",BMT_FLEETSEGMENTS)
     behav_act_matrix[,1] <- BEHAV_ACT_head
     if (length(BMT_FLEETSEGMENTS) != 0) {
   for (r in 1:nrow(behav_act_matrix)) { 
  behav_act_temp <- as.list(behav_act_matrix[r,]) 
  bmt_behav_act_list <<- c(bmt_behav_act_list, list(behav_act_temp)) 
  }
  }
 }
#print("behav_act (simulation) list successfully updated!", quote=F)
}