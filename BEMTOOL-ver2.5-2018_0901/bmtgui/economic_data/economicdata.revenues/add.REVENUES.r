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
#
# ------------------------------------------------------------------------------
# add elements to the list of the p production values
# ------------------------------------------------------------------------------
#
bmt_add.REVENUES <- function() {
#print("Adding elements to the list...")   
  if (!is.null(bmt_economic.REVENUES)) {
  for (r in 1:nrow(bmt_economic.REVENUES)) {
  REVENUES_temp <- as.list(bmt_economic.REVENUES[r,]) 
  names(REVENUES_temp) <- c("year",paste("casestudy.revenues.S", 1:length(BMT_SPECIES), sep=""))
  bmt_REVENUES_list <<- c(bmt_REVENUES_list, list(REVENUES_temp)) 
  }
   } else {
   REVENUES_matrix <- data.frame(matrix(0, nrow=length(years), ncol=(length(BMT_SPECIES)+1)))
   colnames(REVENUES_matrix) <- c("year")
     REVENUES_matrix$year <- years
   for (r in 1:nrow(REVENUES_matrix)) { 
  REVENUES_temp <- as.list(REVENUES_matrix[r,]) 
  bmt_REVENUES_list <<- c(bmt_REVENUES_list, list(REVENUES_temp)) 
  }
 }
#print("REVENUES (simulation) list successfully updated!", quote=F)
}