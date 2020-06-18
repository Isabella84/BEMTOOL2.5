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
add.input_table_r7_opt3_indices <- function() {
#print("Adding elements to the list...")   
  if (!is.null(input_table_r7_opt3_indices)) {
  for (r in 1:nrow(input_table_r7_opt3_indices)) {
  input_table_r7_opt3_indices_temp <- as.list(input_table_r7_opt3_indices[r,]) 
  names(input_table_r7_opt3_indices_temp) <- c("year", "SSB_index")
  input_table_r7_opt3_indices_list <<- c(input_table_r7_opt3_indices_list, list(input_table_r7_opt3_indices_temp)) 
  }
   } else {
   input_table_r7_opt3_indices_matrix <- data.frame(matrix(0, nrow=length(BMT_YEARS_SIMULATION), ncol=2))
   colnames(input_table_r7_opt3_indices_matrix) <- c("Year", "SSB index")
     input_table_r7_opt3_indices_matrix$year <- BMT_YEARS_SIMULATION
   for (r in 1:nrow(input_table_r7_opt3_indices_matrix)) { 
  input_table_r7_opt3_indices_temp <- as.list(input_table_r7_opt3_indices_matrix[r,]) 
  input_table_r7_opt3_indices_list <<- c(input_table_r7_opt3_indices_list, list(input_table_r7_opt3_indices_temp)) 
  }
 }
#print("input_table_r7_opt3_indices (simulation) list successfully updated!", quote=F)
}