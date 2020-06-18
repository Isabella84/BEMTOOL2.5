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
# ------------------------------------------------------------------------------
# add elements to the list of the p production values
# ------------------------------------------------------------------------------
#
bmt_add.effort_F <- function() {
#print("Adding elements to the list...")   
  if (!is.null(bmt_effort_F)) {
  for (r in 1:nrow(bmt_effort_F)) {
  bmt_effort_F_temp <- as.list(bmt_effort_F[r,]) 
  names(bmt_effort_F_temp) <-  c(" ",BMT_FLEETSEGMENTS)
  bmt_effort_F_list <<- c(bmt_effort_F_list, list(bmt_effort_F_temp)) 
  }
   } else {
   effort_F_matrix <- data.frame(matrix(0, nrow=3, ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(effort_F_matrix) <-  c(" ",BMT_FLEETSEGMENTS)
     effort_F_matrix[,1] <- c("Type of relationship", "coeff a", "coeff b")
     if (length(BMT_FLEETSEGMENTS) != 0) {
   for (r in 1:nrow(effort_F_matrix)) { 
  bmt_effort_F_temp <- as.list(effort_F_matrix[r,]) 
  bmt_effort_F_list <<- c(bmt_effort_F_list, list(bmt_effort_F_temp)) 
  }
  }
 }
#print("ecoind (simulation) list successfully updated!", quote=F)
}