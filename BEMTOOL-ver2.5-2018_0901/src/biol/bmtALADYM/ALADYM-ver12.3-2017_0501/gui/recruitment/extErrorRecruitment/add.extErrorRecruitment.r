# BEMTOOL - Bio-Economic Model TOOLs - version 2.0
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





add.extErrorRecruitment <- function() {
# print("Adding empty elements to the list...") 

 if (!is.null(CI_external_matrix)) {
   # sr_matrix <- data.frame(offspring_prop_df, nrow=1, ncol=12)
  for (r in 1:nrow(CI_external_matrix)) {
     sr_matrix <- as.list(as.numeric(as.character(CI_external_matrix[r,]))) 
  names(sr_matrix) <- "error"
  extErrorRecruitment_list <<- c(extErrorRecruitment_list, sr_matrix) 
  } 
 } else {
    zero_matrix <- data.frame(matrix(-1, nrow=CI_NB_RUNS, ncol=1))
  colnames(zero_matrix) <- "error"
CI_external_matrix  <<- zero_matrix
 for (r in 1:nrow(zero_matrix)) {
     sr_matrix <- as.list(as.numeric(as.character(zero_matrix[r,]))) 
  names(sr_matrix) <- "error"
  extErrorRecruitment_list <<- c(extErrorRecruitment_list, sr_matrix) 
  } 
 }  

   #print(extErrorRecruitment)
}
