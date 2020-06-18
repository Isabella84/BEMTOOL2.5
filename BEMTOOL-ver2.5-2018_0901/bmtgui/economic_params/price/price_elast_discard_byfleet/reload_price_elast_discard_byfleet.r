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
reload_price_elast_discard_byfleet<- function(w) {

  price_elast_discard_byfleet_list <<- list()
  price_elast_discard_byfleetIndex <<- 0

    if (is.null( price_elast_discard_byfleet_MATRIX )) { 
   price_elast_discard_byfleet_matrix <- data.frame(matrix(0, nrow=length(BMT_SPECIES), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(price_elast_discard_byfleet_matrix) <- c("Species",BMT_FLEETSEGMENTS)
     price_elast_discard_byfleet_matrix$Species <- BMT_SPECIES
 price_elast_discard_byfleet_MATRIX <<- price_elast_discard_byfleet_matrix
 } else {
     price_elast_discard_byfleet_matrix <<- price_elast_discard_byfleet_MATRIX 
 }
 
   for (r in 1:nrow(price_elast_discard_byfleet_matrix)) { 
  price_elast_discard_byfleet_temp <- as.list(price_elast_discard_byfleet_matrix[r,]) 
  price_elast_discard_byfleet_list <<- c(price_elast_discard_byfleet_list, list(price_elast_discard_byfleet_temp)) 
  }

price_elast_discard_byfleet.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(BMT_FLEETSEGMENTS)), "gboolean")  
  # add items 
  for (i in 1:length(price_elast_discard_byfleet_list)) {
    iter <- price_elast_discard_byfleet.model$append()$iter
    price_elast_discard_byfleet.model$set(iter,0, price_elast_discard_byfleet_list[[i]]$Species)
    for (e in 1:length(BMT_FLEETSEGMENTS)) {
         price_elast_discard_byfleet.model$set(iter, e, as.double(price_elast_discard_byfleet_list[[i]][e+1]))
    }
       price_elast_discard_byfleet.model$set(iter, (length(BMT_FLEETSEGMENTS)+1),TRUE)
  } 
 
price_elast_discard_byfleet.treeview$destroy()
  
price_elast_discard_byfleet.treeview <<- gtkTreeViewNewWithModel( price_elast_discard_byfleet.model)
price_elast_discard_byfleet.treeview$setRulesHint(TRUE)
 price_elast_discard_byfleet.treeview$getSelection()$setMode("single")
price_elast_discard_byfleet.add_columns( price_elast_discard_byfleet.treeview)
price_elast_discard_byfleet.sw$add(price_elast_discard_byfleet.treeview)
    
}
