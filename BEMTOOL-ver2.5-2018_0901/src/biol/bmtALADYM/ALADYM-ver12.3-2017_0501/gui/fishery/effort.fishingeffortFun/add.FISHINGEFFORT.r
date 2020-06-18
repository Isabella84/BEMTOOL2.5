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
add.FISHINGEFFORT <- function() {
#print("Adding elements to the list...")   
  if (!is.null(fleet.FISHINGEFFORT)) {
if (nrow(fleet.FISHINGEFFORT) != 0) {
  for (r in 1:nrow(fleet.FISHINGEFFORT)) {
  FISHINGEFFORT_temp <- as.list(fleet.FISHINGEFFORT[r,]) 
  names(FISHINGEFFORT_temp) <- c("year",MONTHS)
  FISHINGEFFORT <<- c(FISHINGEFFORT, list(FISHINGEFFORT_temp)) 
  }
  } else {
    FISHINGEFFORT_matrix <- data.frame(matrix(1, nrow=length(years), ncol=13))
   colnames(FISHINGEFFORT_matrix) <- c("year",MONTHS)
     FISHINGEFFORT_matrix$year <- years
   for (r in 1:nrow(FISHINGEFFORT_matrix)) { 
  FISHINGEFFORT_temp <- as.list(FISHINGEFFORT_matrix[r,]) 
  FISHINGEFFORT <<- c(FISHINGEFFORT, list(FISHINGEFFORT_temp)) 
  } 
  
  }
   } else {
   FISHINGEFFORT_matrix <- data.frame(matrix(0, nrow=length(years), ncol=13))
   colnames(FISHINGEFFORT_matrix) <- c("year",MONTHS)
     FISHINGEFFORT_matrix$year <- years
   for (r in 1:nrow(FISHINGEFFORT_matrix)) { 
  FISHINGEFFORT_temp <- as.list(FISHINGEFFORT_matrix[r,]) 
  FISHINGEFFORT <<- c(FISHINGEFFORT, list(FISHINGEFFORT_temp)) 
  }
 }
# print("FISHINGEFFORT (simulation) list successfully updated!", quote=F)
}
