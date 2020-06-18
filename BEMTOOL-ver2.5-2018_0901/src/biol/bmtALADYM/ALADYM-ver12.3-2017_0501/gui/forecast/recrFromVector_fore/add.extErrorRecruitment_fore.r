# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




add.extErrorRecruitment_fore <- function() {
# print("Adding empty elements to the list...") 

 if (!is.null(CI_external_matrix_fore)) {
   # sr_matrix <- data.frame(offspring_prop_df, nrow=1, ncol=12)
  for (r in 1:nrow(CI_external_matrix_fore)) {
     sr_matrix <- as.list(as.numeric(as.character(CI_external_matrix_fore[r,]))) 
  names(sr_matrix) <- c("run_N", years_forecast)
  extErrorRecruitment_fore_list <<- c(extErrorRecruitment_fore_list, list(sr_matrix)) 
  } 
 } else {
    zero_matrix <- data.frame(matrix(-1, nrow=CI_NB_RUNS_FORE, ncol=(length(years_forecast)+1) ))
  colnames(zero_matrix) <- c("run_N", years_forecast) 
CI_external_matrix_fore  <<- zero_matrix
 for (r in 1:nrow(zero_matrix)) {
     sr_matrix <- as.list(as.numeric(as.character(zero_matrix[r,]))) 
  names(sr_matrix) <- c("run_N", years_forecast) 
  extErrorRecruitment_fore_list <<- c(extErrorRecruitment_fore_list, list(sr_matrix)) 
  } 
 }  

   #print(extErrorRecruitment_fore)
}
