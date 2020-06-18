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
bmt_reload_price_importweight_table<- function(w) {

  bmt_price_importweight_list <<- list()
  bmt_price_importweightIndex <<- 0

    if (is.null( bmt_fleet.price_importweight )) { 
   price_importweight_matrix <- data.frame(matrix(0, nrow=length(BMT_SPECIES), ncol=(length(BMT_YEARS_FORECAST)+1)))
   colnames(price_importweight_matrix) <- c("Species",BMT_YEARS_FORECAST)
     price_importweight_matrix$Species <- BMT_SPECIES
 bmt_fleet.price_importweight <<- price_importweight_matrix
 } else {
     price_importweight_matrix <<- bmt_fleet.price_importweight 
 }
 
   for (r in 1:nrow(price_importweight_matrix)) { 
  price_importweight_temp <- as.list(price_importweight_matrix[r,]) 
  bmt_price_importweight_list <<- c(bmt_price_importweight_list, list(price_importweight_temp)) 
  }

bmt_price_importweight.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(BMT_YEARS_FORECAST)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_price_importweight_list)) {
    iter <- bmt_price_importweight.model$append()$iter
    bmt_price_importweight.model$set(iter,0, bmt_price_importweight_list[[i]]$Species)
    for (e in 1:length(BMT_YEARS_FORECAST)) {
         bmt_price_importweight.model$set(iter, e, as.double(bmt_price_importweight_list[[i]][e+1]))
    }
       bmt_price_importweight.model$set(iter, (length(BMT_YEARS_FORECAST)+1),TRUE)
  } 
 
bmt_price_importweight.treeview$destroy()
  
  bmt_price_importweight.treeview <<- gtkTreeViewNewWithModel( bmt_price_importweight.model)
 bmt_price_importweight.treeview$setRulesHint(TRUE)
 bmt_price_importweight.treeview$getSelection()$setMode("single")
bmt_price_importweight.add_columns( bmt_price_importweight.treeview)
bmt_price_importweight.sw$add(bmt_price_importweight.treeview)
    
}
