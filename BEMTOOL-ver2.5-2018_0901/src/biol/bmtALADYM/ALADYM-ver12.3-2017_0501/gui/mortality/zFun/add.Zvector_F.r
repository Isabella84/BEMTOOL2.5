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
# add elements to the list of total mortality values (FEMALES)
# ------------------------------------------------------------------------------
#
add.Zvector_F <- function() {
#print("Adding elements to the list...")   
  if (!is.null(mortality.Zvector.females)) {
  for (r in 1:nrow(mortality.Zvector.females)) {
  ZF_temp <- as.list(mortality.Zvector.females[r,]) 
  names(ZF_temp) <- c("year",MONTHS)
  Zvector_F <<- c(Zvector_F, list(ZF_temp)) 
  }
   } else {
  ZF_matrix <- data.frame(matrix(0, nrow=length(years), ncol=13))
   colnames(ZF_matrix) <- c("year",MONTHS)
     ZF_matrix$year <- years
   for (r in 1:nrow(ZF_matrix)) { 
  ZF_temp <- as.list(ZF_matrix[r,]) 
  Zvector_F <<- c(Zvector_F, list(ZF_temp)) 
  }
 }
# print("Total mortality (FEMALES) list successfully updated!", quote=F)
}
