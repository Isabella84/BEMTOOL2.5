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
bmt_reload_cost_fixed_table<- function(w) {

  bmt_cost_fixed_list <<- list()
  bmt_cost_fixedIndex <<- 0

    if (is.null( bmt_fleet.cost_fixed )) { 
   cost_fixed_matrix <- data.frame(matrix(0, nrow=length(FIXEDCOSTS_head), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(cost_fixed_matrix) <- c("",BMT_FLEETSEGMENTS)
     cost_fixed_matrix[,1] <- FIXEDCOSTS_head
 bmt_fleet.cost_fixed <<- cost_fixed_matrix
 } else {
     cost_fixed_matrix <<- bmt_fleet.cost_fixed 
 }
 
   for (r in 1:nrow(cost_fixed_matrix)) { 
  cost_fixed_temp <- as.list(cost_fixed_matrix[r,]) 
  bmt_cost_fixed_list <<- c(bmt_cost_fixed_list, list(cost_fixed_temp)) 
  }

bmt_cost_fixed.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(BMT_FLEETSEGMENTS)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_cost_fixed_list)) {
    iter <- bmt_cost_fixed.model$append()$iter
    bmt_cost_fixed.model$set(iter,0, bmt_cost_fixed_list[[i]][1])
    for (e in 1:length(BMT_FLEETSEGMENTS)) {
         bmt_cost_fixed.model$set(iter, e, as.double(bmt_cost_fixed_list[[i]][e+1]))
    }
       bmt_cost_fixed.model$set(iter, (length(BMT_FLEETSEGMENTS)+1),TRUE)
  } 
 
bmt_cost_fixed.treeview$destroy()
  
  bmt_cost_fixed.treeview <<- gtkTreeViewNewWithModel( bmt_cost_fixed.model)
 bmt_cost_fixed.treeview$setRulesHint(TRUE)
 bmt_cost_fixed.treeview$getSelection()$setMode("single")
bmt_cost_fixed.add_columns( bmt_cost_fixed.treeview)
bmt_cost_fixed.sw$add(bmt_cost_fixed.treeview)
    
}
