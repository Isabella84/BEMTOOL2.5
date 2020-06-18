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
# ------------------------------------------------------------------------------
# add elements to the list of the p production values
# ------------------------------------------------------------------------------
#
add.monthlyDiscard <- function() {
#print("Adding elements to the list...")   
  if (!is.null(fleet.monthlyDiscard)) {
  for (r in 1:nrow(fleet.monthlyDiscard)) {
  monthlyDiscard_temp <- as.list(fleet.monthlyDiscard[r,]) 
  names(monthlyDiscard_temp) <- c("year",MONTHS)
  monthlyDiscard_list <<- c(monthlyDiscard_list, list(monthlyDiscard_temp)) 
  }
   } else {
   monthlyDiscard_matrix <- data.frame(matrix(0, nrow=length(years), ncol=13))
   colnames(monthlyDiscard_matrix) <- c("year",MONTHS)
     monthlyDiscard_matrix$year <- years
   for (r in 1:nrow(monthlyDiscard_matrix)) { 
  monthlyDiscard_temp <- as.list(monthlyDiscard_matrix[r,]) 
  monthlyDiscard_list <<- c(monthlyDiscard_list, list(monthlyDiscard_temp)) 
  }
 }
# print("PRODUCTION (simulation) list successfully updated!", quote=F)
}
