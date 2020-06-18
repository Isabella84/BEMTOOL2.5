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
# add elements to the list of total mortality values (MALES)
# ------------------------------------------------------------------------------
#
add.Zvector_M <- function() {
#print("Adding elements to the list...")   
  if (!is.null(mortality.Zvector.males)) {
  for (r in 1:nrow(mortality.Zvector.males)) {
  ZM_temp <- as.list(mortality.Zvector.males[r,]) 
  names(ZM_temp) <- c("year",MONTHS)
  Zvector_M <<- c(Zvector_M, list(ZM_temp)) 
  }
   } else {
   ZM_matrix <- data.frame(matrix(0, nrow=length(years), ncol=13))
   colnames(ZM_matrix) <- c("year",MONTHS)
     ZM_matrix$year <- years
   for (r in 1:nrow(ZM_matrix)) { 
  ZM_temp <- as.list(ZM_matrix[r,]) 
  Zvector_M <<- c(Zvector_M, list(ZM_temp)) 
  }
 }
# print("Total mortality (MALES) successfully added to the list!", quote=F)
}