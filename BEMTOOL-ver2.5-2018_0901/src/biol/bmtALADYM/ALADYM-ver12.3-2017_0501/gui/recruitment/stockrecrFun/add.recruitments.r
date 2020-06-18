# BEMTOOL - Bio-Economic Model TOOLs - version 2.0
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
# add elements to the list of stock-recruitment values
# ------------------------------------------------------------------------------
#
add.recruitments <- function() {
#print("Adding elements to the list...")   
  if (!is.null(stockrecruitment.SRvector)) {
  for (r in 1:nrow(stockrecruitment.SRvector)) {
  sr_temp <- as.list(stockrecruitment.SRvector[r,]) 
  names(sr_temp) <- c("year",MONTHS)
  recruitments <<- c(recruitments, list(sr_temp)) 
  }
   } else {
   sr_matrix <- data.frame(matrix(0, nrow=length(years), ncol=13))
   colnames(sr_matrix) <- c("year",MONTHS)
     sr_matrix$year <- years
   for (r in 1:nrow(sr_matrix)) { 
  sr_temp <- as.list(sr_matrix[r,]) 
  recruitments <<- c(recruitments, list(sr_temp)) 
  }
 }
# print("Recruitments list successfully updated!", quote=F)
}

