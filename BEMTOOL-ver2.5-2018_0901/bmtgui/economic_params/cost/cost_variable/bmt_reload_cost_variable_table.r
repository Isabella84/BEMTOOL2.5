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
bmt_reload_cost_variable_table<- function(w) {

  bmt_cost_variable_list <<- list()
  bmt_cost_variableIndex <<- 0

    if (is.null( bmt_fleet.cost_variable )) { 
   cost_variable_matrix <- data.frame(matrix(0, nrow=length(VARCOSTS_head), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(cost_variable_matrix) <- c("",BMT_FLEETSEGMENTS)
     cost_variable_matrix[,1] <- VARCOSTS_head
 bmt_fleet.cost_variable <<- cost_variable_matrix
 } else {
     cost_variable_matrix <<- bmt_fleet.cost_variable 
 }
 
   for (r in 1:nrow(cost_variable_matrix)) { 
  cost_variable_temp <- as.list(cost_variable_matrix[r,]) 
  bmt_cost_variable_list <<- c(bmt_cost_variable_list, list(cost_variable_temp)) 
  }

bmt_cost_variable.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(BMT_FLEETSEGMENTS)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_cost_variable_list)) {
    iter <- bmt_cost_variable.model$append()$iter
    bmt_cost_variable.model$set(iter,0, bmt_cost_variable_list[[i]][1])
    for (e in 1:length(BMT_FLEETSEGMENTS)) {
         bmt_cost_variable.model$set(iter, e, as.double(bmt_cost_variable_list[[i]][e+1]))
    }
       bmt_cost_variable.model$set(iter, (length(BMT_FLEETSEGMENTS)+1),TRUE)
  } 
 
bmt_cost_variable.treeview$destroy()
  
  bmt_cost_variable.treeview <<- gtkTreeViewNewWithModel( bmt_cost_variable.model)
 bmt_cost_variable.treeview$setRulesHint(TRUE)
 bmt_cost_variable.treeview$getSelection()$setMode("single")
bmt_cost_variable.add_columns( bmt_cost_variable.treeview)
bmt_cost_variable.sw$add(bmt_cost_variable.treeview)
    
}
