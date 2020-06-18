# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
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
bmt_add.KW <- function() {
#print("Adding elements to the list...")   
  if (!is.null(bmt_fleet.KW)) {
  for (r in 1:nrow(bmt_fleet.KW)) {
  KW_temp <- as.list(bmt_fleet.KW[r,]) 
  names(KW_temp) <- c("year",MONTHS)
  bmt_KW_list <<- c(bmt_KW_list, list(KW_temp)) 
  }
   } else {
   KW_matrix <- data.frame(matrix(0, nrow=length(years), ncol=13))
   colnames(KW_matrix) <- c("year",MONTHS)
     KW_matrix$year <- years
   for (r in 1:nrow(KW_matrix)) { 
  KW_temp <- as.list(KW_matrix[r,]) 
  bmt_KW_list <<- c(bmt_KW_list, list(KW_temp)) 
  }
 }
#print("KW (simulation) list successfully updated!", quote=F)
}