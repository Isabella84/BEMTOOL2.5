# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
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
# add elements to the list of stock-recruitment_fore_from_vector values
# ------------------------------------------------------------------------------
#
add.recruitments_fore_from_vector_UN <- function() {
#print("Adding elements to the list...")   
  if (!is.null(table_recruitments_fore_from_vector_UN)) {
  for (r in 1:nrow(table_recruitments_fore_from_vector_UN)) {
  sr_temp <- as.list(table_recruitments_fore_from_vector_UN[r,]) 
  names(sr_temp) <- c("run_N", "a", "b", "c") 
  recruitments_fore_from_vector_UN <<- c(recruitments_fore_from_vector_UN, list(sr_temp)) 
  }
   } else {
    zero_matrix <- data.frame(matrix(-1, nrow=CI_NB_RUNS_FORE, ncol=4 ))
     colnames(zero_matrix) <- c("run_N", "a", "b", "c") 
   for (r in 1:nrow(zero_matrix)) { 
  sr_temp <- as.list(zero_matrix[r,]) 
  recruitments_fore_from_vector_UN <<- c(recruitments_fore_from_vector_UN, list(sr_temp)) 
  }
   
 }
# print("recruitments_fore_from_vector list successfully updated!", quote=F)
}

