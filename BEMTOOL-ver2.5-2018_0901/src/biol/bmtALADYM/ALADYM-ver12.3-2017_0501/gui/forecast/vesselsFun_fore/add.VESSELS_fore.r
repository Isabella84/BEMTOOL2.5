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
# add elements to the list of the selectivity values
# ------------------------------------------------------------------------------
#
add.VESSELS_fore <- function() {
#print("Adding elements to the list...")   
  if (!is.null(fleet.VESSELS_fore)) {
  for (r in 1:nrow(fleet.VESSELS_fore)) {
  vess_temp <- as.list(fleet.VESSELS_fore[r,]) 
  names(vess_temp) <- c("year",MONTHS)
  VESSELS_fore <<- c(VESSELS_fore, list(vess_temp)) 
  }
   } else {
   vess_matrix <- data.frame(matrix(0, nrow=length(years_forecast), ncol=13))
   colnames(vess_matrix) <- c("year",MONTHS)
     vess_matrix$year <- years_forecast
   for (r in 1:nrow(vess_matrix)) { 
  vess_temp <- as.list(vess_matrix[r,]) 
  VESSELS_fore <<- c(VESSELS_fore, list(vess_temp)) 
  }
 }
 #print("VESSELS (forecast) list successfully updated!", quote=F)
}