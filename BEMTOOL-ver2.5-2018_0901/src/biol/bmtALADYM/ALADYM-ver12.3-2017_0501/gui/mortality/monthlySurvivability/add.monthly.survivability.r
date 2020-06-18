# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




add.monthly.survivability <- function() {
# print("Adding empty elements to the list...") 

 if (!is.null(monthly.survivability_df)) {
   # sr_matrix <- data.frame(monthly.survivability_df, nrow=1, ncol=12)
  for (cl in 1:ncol(monthly.survivability_df)) {
     sr_matrix <- as.list(as.numeric(as.character(monthly.survivability_df[,cl]))) 
  names(sr_matrix) <- c(MONTHS[cl])
  monthly.survivability <<- c(monthly.survivability, sr_matrix) 
  } 
 } else {
    sr_matrix <- data.frame(matrix(as.double(0.08), nrow=1, ncol=12))
   colnames(sr_matrix) <- c(MONTHS)
   monthly.survivability <<- as.list(sr_matrix) 
 }  

   #print(monthly.survivability)
}
