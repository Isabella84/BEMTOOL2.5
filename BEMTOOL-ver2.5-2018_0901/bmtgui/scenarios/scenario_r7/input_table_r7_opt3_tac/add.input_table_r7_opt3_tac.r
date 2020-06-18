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
add.input_table_r7_opt3_tac <- function() {
#print("Adding elements to the list...")   
  if (!is.null(input_table_r7_opt3_tac)) {
  for (r in 1:nrow(input_table_r7_opt3_tac)) {
  input_table_r7_opt3_tac_temp <- as.list(input_table_r7_opt3_tac[r,]) 
  names(input_table_r7_opt3_tac_temp) <- c("year", "tac")
  input_table_r7_opt3_tac_list <<- c(input_table_r7_opt3_tac_list, list(input_table_r7_opt3_tac_temp)) 
  }
   } else {
   input_table_r7_opt3_tac_matrix <- data.frame(matrix(0, nrow=length(BMT_YEARS_SIMULATION), ncol=2))
   colnames(input_table_r7_opt3_tac_matrix) <- c("year", "tac")
     input_table_r7_opt3_tac_matrix$year <- BMT_FLEETSEGMENTS
   for (r in 1:nrow(input_table_r7_opt3_tac_matrix)) { 
  input_table_r7_opt3_tac_temp <- as.list(input_table_r7_opt3_tac_matrix[r,]) 
  input_table_r7_opt3_tac_list <<- c(input_table_r7_opt3_tac_list, list(input_table_r7_opt3_tac_temp)) 
  }
 }
#print("input_table_r7_opt3_tac (simulation) list successfully updated!", quote=F)
}