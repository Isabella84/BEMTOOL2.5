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
bmt_reload_price_elast_landing_byfleet_table<- function(w) {

  bmt_price_elast_landing_byfleet_list <<- list()
  bmt_price_elast_landing_byfleetIndex <<- 0

    if (is.null( bmt_fleet.price_elast_landing_byfleet )) { 
   price_elast_landing_byfleet_matrix <- data.frame(matrix(0, nrow=length(BMT_SPECIES), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(price_elast_landing_byfleet_matrix) <- c("Species",BMT_FLEETSEGMENTS)
     price_elast_landing_byfleet_matrix$Species <- BMT_SPECIES
 bmt_fleet.price_elast_landing_byfleet <<- price_elast_landing_byfleet_matrix
 } else {
     price_elast_landing_byfleet_matrix <<- bmt_fleet.price_elast_landing_byfleet 
 }
 
   for (r in 1:nrow(price_elast_landing_byfleet_matrix)) { 
  price_elast_landing_byfleet_temp <- as.list(price_elast_landing_byfleet_matrix[r,]) 
  bmt_price_elast_landing_byfleet_list <<- c(bmt_price_elast_landing_byfleet_list, list(price_elast_landing_byfleet_temp)) 
  }

bmt_price_elast_landing_byfleet.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(BMT_FLEETSEGMENTS)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_price_elast_landing_byfleet_list)) {
    iter <- bmt_price_elast_landing_byfleet.model$append()$iter
    bmt_price_elast_landing_byfleet.model$set(iter,0, bmt_price_elast_landing_byfleet_list[[i]]$Species)
    for (e in 1:length(BMT_FLEETSEGMENTS)) {
         bmt_price_elast_landing_byfleet.model$set(iter, e, as.double(bmt_price_elast_landing_byfleet_list[[i]][e+1]))
    }
       bmt_price_elast_landing_byfleet.model$set(iter, (length(BMT_FLEETSEGMENTS)+1),TRUE)
  } 
 
bmt_price_elast_landing_byfleet.treeview$destroy()
  
  bmt_price_elast_landing_byfleet.treeview <<- gtkTreeViewNewWithModel( bmt_price_elast_landing_byfleet.model)
 bmt_price_elast_landing_byfleet.treeview$setRulesHint(TRUE)
 bmt_price_elast_landing_byfleet.treeview$getSelection()$setMode("single")
bmt_price_elast_landing_byfleet.add_columns( bmt_price_elast_landing_byfleet.treeview)
bmt_price_elast_landing_byfleet.sw$add(bmt_price_elast_landing_byfleet.treeview)
    
}
