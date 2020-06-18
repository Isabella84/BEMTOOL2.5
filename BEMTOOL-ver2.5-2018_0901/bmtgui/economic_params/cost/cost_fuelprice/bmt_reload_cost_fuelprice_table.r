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
# Function to reload the values for the production according to the 
# seed value
# ------------------------------------------------------------------------------
#
bmt_reload_cost_fuelprice_table<- function(w) {

  bmt_cost_fuelprice_list <<- list()
  bmt_cost_fuelpriceIndex <<- 0

    if (is.null( bmt_fleet.cost_fuelprice )) { 
   cost_fuelprice_matrix <- data.frame(matrix(0, nrow=1, ncol=(length(BMT_YEARS_FORECAST)+1)))
   colnames(cost_fuelprice_matrix) <- c(" fuel price ",BMT_YEARS_FORECAST)
     #cost_fuelprice_matrix[,1] <- " fuel price "
 bmt_fleet.cost_fuelprice <<- cost_fuelprice_matrix
 } else {
     cost_fuelprice_matrix <<- bmt_fleet.cost_fuelprice 
 }
 
   for (r in 1:nrow(cost_fuelprice_matrix)) { 
  cost_fuelprice_temp <- as.list(cost_fuelprice_matrix[r,]) 
  bmt_cost_fuelprice_list <<- c(bmt_cost_fuelprice_list, list(cost_fuelprice_temp)) 
  }

bmt_cost_fuelprice.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(BMT_YEARS_FORECAST)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_cost_fuelprice_list)) {
    iter <- bmt_cost_fuelprice.model$append()$iter
    bmt_cost_fuelprice.model$set(iter,0, bmt_cost_fuelprice_list[[i]][1])
    for (e in 1:length(BMT_YEARS_FORECAST)) {
         bmt_cost_fuelprice.model$set(iter, e, as.double(bmt_cost_fuelprice_list[[i]][e+1]))
    }
       bmt_cost_fuelprice.model$set(iter, (length(BMT_YEARS_FORECAST)+1),TRUE)
  } 
 
bmt_cost_fuelprice.treeview$destroy()
  
  bmt_cost_fuelprice.treeview <<- gtkTreeViewNewWithModel( bmt_cost_fuelprice.model)
 bmt_cost_fuelprice.treeview$setRulesHint(TRUE)
 bmt_cost_fuelprice.treeview$getSelection()$setMode("single")
bmt_cost_fuelprice.add_columns( bmt_cost_fuelprice.treeview)
bmt_cost_fuelprice.sw$add(bmt_cost_fuelprice.treeview)
    
}
