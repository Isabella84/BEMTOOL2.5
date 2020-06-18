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
add.pproductions <- function() {
#print("Adding elements to the list...")   
  if (!is.null(fleet.pproduction)) {
  for (r in 1:nrow(fleet.pproduction)) {
  pprod_temp <- as.list(fleet.pproduction[r,]) 
  names(pprod_temp) <- c("year",MONTHS)
  pproductions <<- c(pproductions, list(pprod_temp)) 
  }
   } else {
   pprod_matrix <- data.frame(matrix(0, nrow=length(years), ncol=13))
   colnames(pprod_matrix) <- c("year",MONTHS)
     pprod_matrix$year <- years
   for (r in 1:nrow(pprod_matrix)) { 
  pprod_temp <- as.list(pprod_matrix[r,]) 
  pproductions <<- c(pproductions, list(pprod_temp)) 
  }
 }
 #print("P PRODUCTION (simulation) list successfully updated!", quote=F)
}