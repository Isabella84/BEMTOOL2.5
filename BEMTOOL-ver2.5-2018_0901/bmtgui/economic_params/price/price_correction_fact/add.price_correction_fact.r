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
bmt_add.price_correction_fact <- function() {
#print("Adding elements to the list...")   
  if (!is.null(bmt_fleet.price_correction_fact)) {
  for (r in 1:nrow(bmt_fleet.price_correction_fact)) {
  price_correction_fact_temp <- as.list(bmt_fleet.price_correction_fact[r,]) 
  names(price_correction_fact_temp) <-  c("Species",BMT_FLEETSEGMENTS)
  bmt_price_correction_fact_list <<- c(bmt_price_correction_fact_list, list(price_correction_fact_temp)) 
  }
   } else {
   price_correction_fact_matrix <- data.frame(matrix(0, nrow=length(BMT_SPECIES), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(price_correction_fact_matrix) <-  c("Species",BMT_FLEETSEGMENTS)
     price_correction_fact_matrix$Species <- BMT_SPECIES
     if (length(BMT_SPECIES) != 0) {
   for (r in 1:nrow(price_correction_fact_matrix)) { 
  price_correction_fact_temp <- as.list(price_correction_fact_matrix[r,]) 
  bmt_price_correction_fact_list <<- c(bmt_price_correction_fact_list, list(price_correction_fact_temp)) 
  }
  }
 }
#print("price_correction_fact (simulation) list successfully updated!", quote=F)
}