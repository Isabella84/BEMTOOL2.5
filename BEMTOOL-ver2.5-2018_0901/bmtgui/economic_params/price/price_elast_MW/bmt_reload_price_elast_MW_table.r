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
# Function to reload the values for the production according to the 
# seed value
# ------------------------------------------------------------------------------
#
bmt_reload_price_elast_MW_table<- function(w) {

  bmt_price_elast_MW_list <<- list()
  bmt_price_elast_MWIndex <<- 0

    if (is.null( bmt_fleet.price_elast_MW )) { 
   price_elast_MW_matrix <- data.frame(matrix(0, nrow=length(BMT_SPECIES), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(price_elast_MW_matrix) <- c("Species",BMT_FLEETSEGMENTS)
     price_elast_MW_matrix$Species <- BMT_SPECIES
 bmt_fleet.price_elast_MW <<- price_elast_MW_matrix
 } else {
     price_elast_MW_matrix <<- bmt_fleet.price_elast_MW 
 }
 
   for (r in 1:nrow(price_elast_MW_matrix)) { 
  price_elast_MW_temp <- as.list(price_elast_MW_matrix[r,]) 
  bmt_price_elast_MW_list <<- c(bmt_price_elast_MW_list, list(price_elast_MW_temp)) 
  }

bmt_price_elast_MW.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(BMT_FLEETSEGMENTS)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_price_elast_MW_list)) {
    iter <- bmt_price_elast_MW.model$append()$iter
    bmt_price_elast_MW.model$set(iter,0, bmt_price_elast_MW_list[[i]]$Species)
    for (e in 1:length(BMT_FLEETSEGMENTS)) {
         bmt_price_elast_MW.model$set(iter, e, as.double(bmt_price_elast_MW_list[[i]][e+1]))
    }
       bmt_price_elast_MW.model$set(iter, (length(BMT_FLEETSEGMENTS)+1),TRUE)
  } 
 
bmt_price_elast_MW.treeview$destroy()
  
  bmt_price_elast_MW.treeview <<- gtkTreeViewNewWithModel( bmt_price_elast_MW.model)
 bmt_price_elast_MW.treeview$setRulesHint(TRUE)
 bmt_price_elast_MW.treeview$getSelection()$setMode("single")
bmt_price_elast_MW.add_columns( bmt_price_elast_MW.treeview)
bmt_price_elast_MW.sw$add(bmt_price_elast_MW.treeview)
    
}
