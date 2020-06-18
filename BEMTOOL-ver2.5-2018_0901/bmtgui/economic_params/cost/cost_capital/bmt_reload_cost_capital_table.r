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
bmt_reload_cost_capital_table<- function(w) {

  bmt_cost_capital_list <<- list()
  bmt_cost_capitalIndex <<- 0

    if (is.null( bmt_fleet.cost_capital )) { 
   cost_capital_matrix <- data.frame(matrix(0, nrow=length(CAPITALCOSTS_head), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(cost_capital_matrix) <- c("",BMT_FLEETSEGMENTS)
     cost_capital_matrix[,1] <- CAPITALCOSTS_head
 bmt_fleet.cost_capital <<- cost_capital_matrix
 } else {
     cost_capital_matrix <<- bmt_fleet.cost_capital 
 }
 
   for (r in 1:nrow(cost_capital_matrix)) { 
  cost_capital_temp <- as.list(cost_capital_matrix[r,]) 
  bmt_cost_capital_list <<- c(bmt_cost_capital_list, list(cost_capital_temp)) 
  }

bmt_cost_capital.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(BMT_FLEETSEGMENTS)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_cost_capital_list)) {
    iter <- bmt_cost_capital.model$append()$iter
    bmt_cost_capital.model$set(iter,0, bmt_cost_capital_list[[i]][1])
    for (e in 1:length(BMT_FLEETSEGMENTS)) {
         bmt_cost_capital.model$set(iter, e, as.double(bmt_cost_capital_list[[i]][e+1]))
    }
       bmt_cost_capital.model$set(iter, (length(BMT_FLEETSEGMENTS)+1),TRUE)
  } 
 
bmt_cost_capital.treeview$destroy()
  
  bmt_cost_capital.treeview <<- gtkTreeViewNewWithModel( bmt_cost_capital.model)
 bmt_cost_capital.treeview$setRulesHint(TRUE)
 bmt_cost_capital.treeview$getSelection()$setMode("single")
bmt_cost_capital.add_columns( bmt_cost_capital.treeview)
bmt_cost_capital.sw$add(bmt_cost_capital.treeview)
    
}
