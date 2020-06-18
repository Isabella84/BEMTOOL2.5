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
add.price_elast_discard_byfleet <- function() {
#print("Adding elements to the list...")   
  if (!is.null(price_elast_discard_byfleet_MATRIX)) {
  for (r in 1:nrow(price_elast_discard_byfleet_MATRIX)) {
  price_elast_discard_byfleet_temp <- as.list(price_elast_discard_byfleet_MATRIX[r,]) 
  names(price_elast_discard_byfleet_temp) <-  c("Species",BMT_FLEETSEGMENTS)
  price_elast_discard_byfleet_list <<- c(price_elast_discard_byfleet_list, list(price_elast_discard_byfleet_temp)) 
  }
   } else {
   price_elast_discard_byfleet_matrix <- data.frame(matrix(0, nrow=length(BMT_SPECIES), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(price_elast_discard_byfleet_matrix) <-  c("Species",BMT_FLEETSEGMENTS)
     price_elast_discard_byfleet_matrix$Species <- BMT_SPECIES
     if (length(BMT_SPECIES) != 0) {
   for (r in 1:nrow(price_elast_discard_byfleet_matrix)) { 
  price_elast_discard_byfleet_temp <- as.list(price_elast_discard_byfleet_matrix[r,]) 
  price_elast_discard_byfleet_list <<- c(price_elast_discard_byfleet_list, list(price_elast_discard_byfleet_temp)) 
  }
  }
 }
#print("price_elast_landing_byfleet (simulation) list successfully updated!", quote=F)
}