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
bmt_reload_price_correction_fact_table<- function(w) {

  bmt_price_correction_fact_list <<- list()
  bmt_price_correction_factIndex <<- 0

    if (is.null( bmt_fleet.price_correction_fact )) { 
   price_correction_fact_matrix <- data.frame(matrix(0, nrow=length(BMT_SPECIES), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(price_correction_fact_matrix) <- c("Species",BMT_FLEETSEGMENTS)
     price_correction_fact_matrix$Species <- BMT_SPECIES
 bmt_fleet.price_correction_fact <<- price_correction_fact_matrix
 } else {
     price_correction_fact_matrix <<- bmt_fleet.price_correction_fact 
 }
 
   for (r in 1:nrow(price_correction_fact_matrix)) { 
  price_correction_fact_temp <- as.list(price_correction_fact_matrix[r,]) 
  bmt_price_correction_fact_list <<- c(bmt_price_correction_fact_list, list(price_correction_fact_temp)) 
  }

bmt_price_correction_fact.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(BMT_FLEETSEGMENTS)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_price_correction_fact_list)) {
    iter <- bmt_price_correction_fact.model$append()$iter
    bmt_price_correction_fact.model$set(iter,0, bmt_price_correction_fact_list[[i]]$Species)
    for (e in 1:length(BMT_FLEETSEGMENTS)) {
         bmt_price_correction_fact.model$set(iter, e, as.double(bmt_price_correction_fact_list[[i]][e+1]))
    }
       bmt_price_correction_fact.model$set(iter, (length(BMT_FLEETSEGMENTS)+1),TRUE)
  } 
 
bmt_price_correction_fact.treeview$destroy()
  
  bmt_price_correction_fact.treeview <<- gtkTreeViewNewWithModel( bmt_price_correction_fact.model)
 bmt_price_correction_fact.treeview$setRulesHint(TRUE)
 bmt_price_correction_fact.treeview$getSelection()$setMode("single")
bmt_price_correction_fact.add_columns( bmt_price_correction_fact.treeview)
bmt_price_correction_fact.sw$add(bmt_price_correction_fact.treeview)
    
}
