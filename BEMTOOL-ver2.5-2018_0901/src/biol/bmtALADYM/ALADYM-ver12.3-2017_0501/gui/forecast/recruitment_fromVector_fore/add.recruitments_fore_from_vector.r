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
add.recruitments_fore_from_vector <- function() {
#print("Adding elements to the list...")   
  if (!is.null(recruitments_fore_from_vector.vector)) {
  for (r in 1:nrow(recruitments_fore_from_vector.vector)) {
  sr_temp <- as.list(recruitments_fore_from_vector.vector[r,]) 
  names(sr_temp) <- c("year",MONTHS)
  recruitments_fore_from_vector <<- c(recruitments_fore_from_vector, list(sr_temp)) 
  }
   } else {
   sr_matrix <- data.frame(matrix(0, nrow=length(years_forecast), ncol=13))
   colnames(sr_matrix) <- c("year",MONTHS)
     sr_matrix$year <- years_forecast
   for (r in 1:nrow(sr_matrix)) { 
  sr_temp <- as.list(sr_matrix[r,]) 
  recruitments_fore_from_vector <<- c(recruitments_fore_from_vector, list(sr_temp)) 
  }
 }
# print("recruitments_fore_from_vector list successfully updated!", quote=F)
}

