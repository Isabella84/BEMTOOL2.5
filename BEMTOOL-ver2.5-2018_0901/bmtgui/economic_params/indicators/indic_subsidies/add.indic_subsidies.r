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
bmt_add.indic_subsidies <- function() {
#print("Adding elements to the list...")   
  if (!is.null(bmt_fleet.indic_subsidies)) {
  for (r in 1:nrow(bmt_fleet.indic_subsidies)) {
  indic_subsidies_temp <- as.list(bmt_fleet.indic_subsidies[r,]) 
  names(indic_subsidies_temp) <-  c(" ",BMT_YEARS_FORECAST)
  bmt_indic_subsidies_list <<- c(bmt_indic_subsidies_list, list(indic_subsidies_temp)) 
  }
   } else {
   indic_subsidies_matrix <- data.frame(matrix(0, nrow=1, ncol=(length(BMT_YEARS_FORECAST)+1)))
   colnames(indic_subsidies_matrix) <-  c("FleetSegment",BMT_YEARS_FORECAST)
     indic_subsidies_matrix$FleetSegment <- paste(" ", BMT_FLEETSEGMENTS, " ", sep="")
     if (length(BMT_SPECIES) != 0) {
   for (r in 1:nrow(indic_subsidies_matrix)) { 
  indic_subsidies_temp <- as.list(indic_subsidies_matrix[r,]) 
  bmt_indic_subsidies_list <<- c(bmt_indic_subsidies_list, list(indic_subsidies_temp)) 
  }
  }
 }
#print("indic_subsidies (simulation) list successfully updated!", quote=F)
}