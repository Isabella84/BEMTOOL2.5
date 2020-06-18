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
bmt_add.price_elast_MW <- function() {
#print("Adding elements to the list...")   
  if (!is.null(bmt_fleet.price_elast_MW)) {
  for (r in 1:nrow(bmt_fleet.price_elast_MW)) {
  price_elast_MW_temp <- as.list(bmt_fleet.price_elast_MW[r,]) 
  names(price_elast_MW_temp) <-  c("Species",BMT_FLEETSEGMENTS)
  bmt_price_elast_MW_list <<- c(bmt_price_elast_MW_list, list(price_elast_MW_temp)) 
  }
   } else {
   price_elast_MW_matrix <- data.frame(matrix(0, nrow=length(BMT_SPECIES), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(price_elast_MW_matrix) <-  c("Species",BMT_FLEETSEGMENTS)
     price_elast_MW_matrix$Species <- BMT_SPECIES
     if (length(BMT_SPECIES) != 0) {
   for (r in 1:nrow(price_elast_MW_matrix)) { 
  price_elast_MW_temp <- as.list(price_elast_MW_matrix[r,]) 
  bmt_price_elast_MW_list <<- c(bmt_price_elast_MW_list, list(price_elast_MW_temp)) 
  }
  }
 }
#print("price_elast_MW (simulation) list successfully updated!", quote=F)
}