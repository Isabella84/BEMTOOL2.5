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
add.MCDAutility <- function() {
#print("Adding elements to the list...")   
  if (!is.null(MCDAutility_table)) {
  for (r in 1:nrow(MCDAutility_table)) {
  behav_act_temp <- as.list(MCDAutility_table[r,]) 
  names(behav_act_temp) <-  c("Utility_params", "Value")
  MCDAutility_list <<- c(MCDAutility_list, list(behav_act_temp)) 
  }
   } else {
   behav_act_matrix <- data.frame(matrix(0, nrow=17, ncol=2))
   colnames(behav_act_matrix) <-  c("Utility_params", "Value")
     behav_act_matrix[,1] <- c("u_gva_mey", "u_gva_0.5mey", "u_rber_1", "u_rber_1.5", "u_empl_ce", "u_empl_0.5ce", "u_wage_mnw", "u_ssb_0.2", "u_ssb_msy", "u_f_msy", "u_f_2msy", "u_y_msy", "u_y_0.5msy", "u_d_0.25", "u_d_0.5", "GVA_or_ROI_or_PROFITS", "last_values")
   for (r in 1:nrow(behav_act_matrix)) { 
  behav_act_temp <- as.list(behav_act_matrix[r,]) 
  MCDAutility_list <<- c(MCDAutility_list, list(behav_act_temp)) 
  }
 }
#print("behav_act (simulation) list successfully updated!", quote=F)
}