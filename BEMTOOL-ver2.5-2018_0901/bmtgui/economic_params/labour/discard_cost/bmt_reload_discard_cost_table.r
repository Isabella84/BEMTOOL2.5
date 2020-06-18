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
bmt_reload_discard_cost_table<- function(w) {

  bmt_discard_cost_list <<- list()
  bmt_discard_costIndex <<- 0

    if (is.null( bmt_fleet.discard_cost )) { 
   discard_cost_matrix <- data.frame(matrix(0, nrow=length(BMT_FLEETSEGMENTS), ncol=(length(BMT_YEARS_FORECAST)+1)))
   colnames(discard_cost_matrix) <- c("FleetSegment",BMT_YEARS_FORECAST)
     discard_cost_matrix$FleetSegment <- paste(" ", BMT_FLEETSEGMENTS, " ", sep="")
 bmt_fleet.discard_cost <<- discard_cost_matrix
 } else {
     discard_cost_matrix <<- bmt_fleet.discard_cost 
 }
 
   for (r in 1:nrow(discard_cost_matrix)) { 
  discard_cost_temp <- as.list(discard_cost_matrix[r,]) 
  bmt_discard_cost_list <<- c(bmt_discard_cost_list, list(discard_cost_temp)) 
  }

bmt_discard_cost.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(BMT_YEARS_FORECAST)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_discard_cost_list)) {
    iter <- bmt_discard_cost.model$append()$iter
    bmt_discard_cost.model$set(iter,0, bmt_discard_cost_list[[i]]$FleetSegment)
    for (e in 1:length(BMT_YEARS_FORECAST)) {
         bmt_discard_cost.model$set(iter, e, as.double(bmt_discard_cost_list[[i]][e+1]))
    }
       bmt_discard_cost.model$set(iter, (length(BMT_YEARS_FORECAST)+1),TRUE)
  } 
 
bmt_discard_cost.treeview$destroy()
  
  bmt_discard_cost.treeview <<- gtkTreeViewNewWithModel( bmt_discard_cost.model)
 bmt_discard_cost.treeview$setRulesHint(TRUE)
 bmt_discard_cost.treeview$getSelection()$setMode("single")
bmt_discard_cost.add_columns( bmt_discard_cost.treeview)
bmt_discard_cost.sw$add(bmt_discard_cost.treeview)
    
}
