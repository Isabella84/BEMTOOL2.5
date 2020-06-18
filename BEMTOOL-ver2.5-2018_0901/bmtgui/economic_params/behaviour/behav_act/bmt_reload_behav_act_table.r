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
bmt_reload_behav_act_table<- function(w) {

  bmt_behav_act_list <<- list()
  bmt_behav_actIndex <<- 0

    if (is.null( bmt_fleet.behav_act )) { 
   behav_act_matrix <- data.frame(matrix(0, nrow=length(BEHAV_ACT_head), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(behav_act_matrix) <- c("",BMT_FLEETSEGMENTS)
     behav_act_matrix[,1] <- BEHAV_ACT_head
 bmt_fleet.behav_act <<- behav_act_matrix
 } else {
     behav_act_matrix <<- bmt_fleet.behav_act 
 }
 
   for (r in 1:nrow(behav_act_matrix)) { 
  behav_act_temp <- as.list(behav_act_matrix[r,]) 
  bmt_behav_act_list <<- c(bmt_behav_act_list, list(behav_act_temp)) 
  }

bmt_behav_act.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(BMT_FLEETSEGMENTS)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_behav_act_list)) {
    iter <- bmt_behav_act.model$append()$iter
    bmt_behav_act.model$set(iter,0, bmt_behav_act_list[[i]][1])
    for (e in 1:length(BMT_FLEETSEGMENTS)) {
         bmt_behav_act.model$set(iter, e, as.double(bmt_behav_act_list[[i]][e+1]))
    }
       bmt_behav_act.model$set(iter, (length(BMT_FLEETSEGMENTS)+1),TRUE)
  } 
 
bmt_behav_act.treeview$destroy()
  
  bmt_behav_act.treeview <<- gtkTreeViewNewWithModel( bmt_behav_act.model)
 bmt_behav_act.treeview$setRulesHint(TRUE)
 bmt_behav_act.treeview$getSelection()$setMode("single")
bmt_behav_act.add_columns( bmt_behav_act.treeview)
bmt_behav_act.sw$add(bmt_behav_act.treeview)
    
}
