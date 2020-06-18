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
bmt_reload_behav_dyn_table<- function(w) {

  bmt_behav_dyn_list <<- list()
  bmt_behav_dynIndex <<- 0

    if (is.null( bmt_fleet.behav_dyn )) { 
   behav_dyn_matrix <- data.frame(matrix(0, nrow=length(BEHAV_DYN_head), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(behav_dyn_matrix) <- c("",BMT_FLEETSEGMENTS)
     behav_dyn_matrix[,1] <- BEHAV_DYN_head
 bmt_fleet.behav_dyn <<- behav_dyn_matrix
 } else {
     behav_dyn_matrix <<- bmt_fleet.behav_dyn 
 }
 
   for (r in 1:nrow(behav_dyn_matrix)) { 
  behav_dyn_temp <- as.list(behav_dyn_matrix[r,]) 
  bmt_behav_dyn_list <<- c(bmt_behav_dyn_list, list(behav_dyn_temp)) 
  }

bmt_behav_dyn.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(BMT_FLEETSEGMENTS)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_behav_dyn_list)) {
    iter <- bmt_behav_dyn.model$append()$iter
    bmt_behav_dyn.model$set(iter,0, bmt_behav_dyn_list[[i]][1])
    for (e in 1:length(BMT_FLEETSEGMENTS)) {
         bmt_behav_dyn.model$set(iter, e, as.double(bmt_behav_dyn_list[[i]][e+1]))
    }
       bmt_behav_dyn.model$set(iter, (length(BMT_FLEETSEGMENTS)+1),TRUE)
  } 
 
bmt_behav_dyn.treeview$destroy()
  
  bmt_behav_dyn.treeview <<- gtkTreeViewNewWithModel( bmt_behav_dyn.model)
 bmt_behav_dyn.treeview$setRulesHint(TRUE)
 bmt_behav_dyn.treeview$getSelection()$setMode("single")
bmt_behav_dyn.add_columns( bmt_behav_dyn.treeview)
bmt_behav_dyn.sw$add(bmt_behav_dyn.treeview)
    
}
