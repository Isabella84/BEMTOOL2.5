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
add.MCDAweight <- function() {
#print("Adding elements to the list...")   
  if (!is.null(MCDAweight_table)) {
  for (r in 1:nrow(MCDAweight_table)) {
  behav_act_temp <- as.list(MCDAweight_table[r,]) 
  names(behav_act_temp) <-  c("SuperDimension", "Dimension", "Name", "Value")
  MCDAweight_list <<- c(MCDAweight_list, list(behav_act_temp)) 
  }
   } else {
   behav_act_matrix <- data.frame(matrix(0, nrow=8, ncol=4))
   colnames(behav_act_matrix) <-  c("SuperDimension", "Dimension", "Name", "Value")
     behav_act_matrix[,1] <- c("Socioeconomic", "Socioeconomic", "Socioeconomic", "Socioeconomic", "Biological", "Biological", "Biological", "Biological")
	 	   behav_act_matrix[,2] <- c("Economic", "Economic", "Social", "Social", "Biological Conservation", "Biological Conservation" , "Biological Production" , "Biological Production")
	    behav_act_matrix[,3] <- c("k_GVA_ROI", "k_RBER", "k_WAGE", "k_EMPL", "k_SSB", "k_F", "k_Y", "k_D")
   for (r in 1:nrow(behav_act_matrix)) { 
  behav_act_temp <- as.list(behav_act_matrix[r,]) 
  MCDAweight_list <<- c(MCDAweight_list, list(behav_act_temp)) 
  }
 }
#print("behav_act (simulation) list successfully updated!", quote=F)
}