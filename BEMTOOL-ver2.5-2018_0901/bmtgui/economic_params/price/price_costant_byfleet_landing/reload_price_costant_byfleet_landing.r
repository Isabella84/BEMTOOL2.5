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
reload_price_costant_byfleet_landing<- function(w) {

  price_costant_byfleet_landing_list <<- list()
  price_costant_byfleet_landingIndex <<- 0

    if (is.null(price_costant_byfleet_landing_MATRIX )) { 
   price_costant_byfleet_landing_matrix <- data.frame(matrix(0, nrow=length(BMT_SPECIES), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(price_costant_byfleet_landing_matrix) <- c("Species",BMT_FLEETSEGMENTS)
     price_costant_byfleet_landing_matrix$Species <- BMT_SPECIES
 price_costant_byfleet_landing_MATRIX <<- price_costant_byfleet_landing_matrix
 } else {
     price_costant_byfleet_landing_matrix <<- price_costant_byfleet_landing_MATRIX
 }
 
   for (r in 1:nrow(price_costant_byfleet_landing_matrix)) { 
  price_costant_byfleet_landing_temp <- as.list(price_costant_byfleet_landing_matrix[r,]) 
  price_costant_byfleet_landing_list <<- c(price_costant_byfleet_landing_list, list(price_costant_byfleet_landing_temp)) 
  }

price_costant_byfleet_landing.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(BMT_FLEETSEGMENTS)), "gboolean")  
  # add items 
  for (i in 1:length(price_costant_byfleet_landing_list)) {
    iter <- price_costant_byfleet_landing.model$append()$iter
    price_costant_byfleet_landing.model$set(iter,0, price_costant_byfleet_landing_list[[i]]$Species)
    for (e in 1:length(BMT_FLEETSEGMENTS)) {
         price_costant_byfleet_landing.model$set(iter, e, as.double(price_costant_byfleet_landing_list[[i]][e+1]))
    }
       price_costant_byfleet_landing.model$set(iter, (length(BMT_FLEETSEGMENTS)+1),TRUE)
  } 
 
price_costant_byfleet_landing.treeview$destroy()
  
  price_costant_byfleet_landing.treeview <<- gtkTreeViewNewWithModel( price_costant_byfleet_landing.model)
 price_costant_byfleet_landing.treeview$setRulesHint(TRUE)
 price_costant_byfleet_landing.treeview$getSelection()$setMode("single")
price_costant_byfleet_landing.add_columns( price_costant_byfleet_landing.treeview)
price_costant_byfleet_landing.sw$add(price_costant_byfleet_landing.treeview)
    
}
