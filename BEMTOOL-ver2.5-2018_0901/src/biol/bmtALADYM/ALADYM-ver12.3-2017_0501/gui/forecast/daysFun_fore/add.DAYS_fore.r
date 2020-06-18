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
# add elements to the list of the selectivity values
# ------------------------------------------------------------------------------
#
add.DAYS_fore <- function() {
#print("Adding elements to the list...")   
  if (!is.null(fleet.DAYS_fore)) {
  for (r in 1:nrow(fleet.DAYS_fore)) {
  act_temp <- as.list(fleet.DAYS_fore[r,]) 
  names(act_temp) <- c("year",MONTHS)
  DAYS_fore <<- c(DAYS_fore, list(act_temp)) 
  }
   } else {
   act_matrix <- data.frame(matrix(0, nrow=length(years_forecast), ncol=13))
   colnames(act_matrix) <- c("year",MONTHS)
     act_matrix$year <- years_forecast
   for (r in 1:nrow(act_matrix)) { 
  act_temp <- as.list(act_matrix[r,]) 
  DAYS_fore <<- c(DAYS_fore, list(act_temp)) 
  }
 }
# print("DAYS (forecast) list successfully updated!", quote=F)
}
