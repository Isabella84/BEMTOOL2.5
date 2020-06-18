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
add.input_table_r5 <- function() {
#print("Adding elements to the list...")   
  if (!is.null(input_table_r5)) {
  for (r in 1:nrow(input_table_r5)) {
  input_table_r5_temp <- as.list(input_table_r5[r,]) 
  names(input_table_r5_temp) <- c("fleet_segment", "Reduction scenario", "% vessel red.", "% days reduction")
  input_table_r5_list <<- c(input_table_r5_list, list(input_table_r5_temp)) 
  }
   } else {
   input_table_r5_matrix <- data.frame(matrix(0, nrow=length(BMT_FLEETSEGMENTS), ncol=4))
   colnames(input_table_r5_matrix) <- c("Fleet segment", "Reduction scenario", "% vessel red.", "% days reduction")
     input_table_r5_matrix$fleet_segment <- BMT_FLEETSEGMENTS
	 input_table_r5_matrix[,2] <- 1
   for (r in 1:nrow(input_table_r5_matrix)) { 
  input_table_r5_temp <- as.list(input_table_r5_matrix[r,]) 
  input_table_r5_list <<- c(input_table_r5_list, list(input_table_r5_temp)) 
  }
 }
#print("input_table_r5 (simulation) list successfully updated!", quote=F)
}