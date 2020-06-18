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
bmt_reload_cost_crew_minwage_table<- function(w) {

  bmt_cost_crew_minwage_list <<- list()
  bmt_cost_crew_minwageIndex <<- 0

    if (is.null( bmt_fleet.cost_crew_minwage )) { 
   cost_crew_minwage_matrix <- data.frame(matrix(0, nrow=length(LABOURCOST_head), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(cost_crew_minwage_matrix) <- c(" ",BMT_FLEETSEGMENTS)
     cost_crew_minwage_matrix[,1] <- LABOURCOST_head
 bmt_fleet.cost_crew_minwage <<- cost_crew_minwage_matrix
 } else {
     cost_crew_minwage_matrix <<- bmt_fleet.cost_crew_minwage 
 }
 
   for (r in 1:nrow(cost_crew_minwage_matrix)) { 
  cost_crew_minwage_temp <- as.list(cost_crew_minwage_matrix[r,]) 
  bmt_cost_crew_minwage_list <<- c(bmt_cost_crew_minwage_list, list(cost_crew_minwage_temp)) 
  }

bmt_cost_crew_minwage.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(BMT_FLEETSEGMENTS)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_cost_crew_minwage_list)) {
    iter <- bmt_cost_crew_minwage.model$append()$iter
    bmt_cost_crew_minwage.model$set(iter,0, bmt_cost_crew_minwage_list[[i]][1])
    for (e in 1:length(BMT_FLEETSEGMENTS)) {
         bmt_cost_crew_minwage.model$set(iter, e, as.double(bmt_cost_crew_minwage_list[[i]][e+1]))
    }
       bmt_cost_crew_minwage.model$set(iter, (length(BMT_FLEETSEGMENTS)+1),TRUE)
  } 
 
bmt_cost_crew_minwage.treeview$destroy()
  
  bmt_cost_crew_minwage.treeview <<- gtkTreeViewNewWithModel( bmt_cost_crew_minwage.model)
 bmt_cost_crew_minwage.treeview$setRulesHint(TRUE)
 bmt_cost_crew_minwage.treeview$getSelection()$setMode("single")
bmt_cost_crew_minwage.add_columns( bmt_cost_crew_minwage.treeview)
bmt_cost_crew_minwage.sw$add(bmt_cost_crew_minwage.treeview)
 
 
 
 
    
}
