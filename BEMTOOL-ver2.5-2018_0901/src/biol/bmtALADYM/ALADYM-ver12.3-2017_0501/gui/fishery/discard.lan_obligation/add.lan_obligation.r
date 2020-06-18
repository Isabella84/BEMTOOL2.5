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
# add elements to the list of the p production values
# ------------------------------------------------------------------------------
#
add.lan_obligation <- function() {
#print("Adding elements to the list...")   
  if (!is.null(fleet.lan_obligation)) {
  for (r in 1:nrow(fleet.lan_obligation)) {
  lan_obligation_temp <- as.list(fleet.lan_obligation[r,]) 
  names(lan_obligation_temp) <- c("year",MONTHS)
  lan_obligation <<- c(lan_obligation, list(lan_obligation_temp)) 
  }
   } else {
   lan_obligation_matrix <- data.frame(matrix("Y", nrow=length(years), ncol=13), stringsAsFactors =F)
   colnames(lan_obligation_matrix) <- c("year",MONTHS)
     lan_obligation_matrix$year <- years
   for (r in 1:nrow(lan_obligation_matrix)) { 
  lan_obligation_temp <- as.list(lan_obligation_matrix[r,]) 
  lan_obligation <<- c(lan_obligation, list(lan_obligation_temp)) 
  }
 }
 #print("lan_obligation (simulation) list successfully updated!", quote=F)
}
