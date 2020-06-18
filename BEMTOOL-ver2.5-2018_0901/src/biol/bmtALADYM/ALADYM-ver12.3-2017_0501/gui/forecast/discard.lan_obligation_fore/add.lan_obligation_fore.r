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
# ------------------------------------------------------------------------------
# add elements to the list of the p production values
# ------------------------------------------------------------------------------
#
add.lan_obligation_fore <- function() {
#print("Adding elements to the list...")   
  if (!is.null(fleet.lan_obligation_fore)) {
  for (r in 1:nrow(fleet.lan_obligation_fore)) {
  lan_obligation_fore_temp <- as.list(fleet.lan_obligation_fore[r,]) 
  names(lan_obligation_fore_temp) <- c("year",MONTHS)
  lan_obligation_fore <<- c(lan_obligation_fore, list(lan_obligation_fore_temp)) 
  }
   } else {
   lan_obligation_fore_matrix <- data.frame(matrix("Y", nrow=length(years_forecast), ncol=13), stringsAsFactors =F)
   colnames(lan_obligation_fore_matrix) <- c("year",MONTHS)
     lan_obligation_fore_matrix$year <- years_forecast
   for (r in 1:nrow(lan_obligation_fore_matrix)) { 
  lan_obligation_fore_temp <- as.list(lan_obligation_fore_matrix[r,]) 
  lan_obligation_fore <<- c(lan_obligation_fore, list(lan_obligation_fore_temp)) 
  }
 }
 #print("lan_obligation_fore (simulation) list successfully updated!", quote=F)
}
