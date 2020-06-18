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
# ------------------------------------------------------------------------------
# add elements to the list of the p production values
# ------------------------------------------------------------------------------
#
add.productions <- function() {
#print("Adding elements to the list...")   
  if (!is.null(fleet.production)) {
  for (r in 1:nrow(fleet.production)) {
  prod_temp <- as.list(fleet.production[r,]) 
  names(prod_temp) <- c("year",MONTHS)
  productions <<- c(productions, list(prod_temp)) 
  }
   } else {
   prod_matrix <- data.frame(matrix(0, nrow=length(years), ncol=13))
   colnames(prod_matrix) <- c("year",MONTHS)
     prod_matrix$year <- years
   for (r in 1:nrow(prod_matrix)) { 
  prod_temp <- as.list(prod_matrix[r,]) 
  productions <<- c(productions, list(prod_temp)) 
  }
 }
# print("PRODUCTION (simulation) list successfully updated!", quote=F)
}
