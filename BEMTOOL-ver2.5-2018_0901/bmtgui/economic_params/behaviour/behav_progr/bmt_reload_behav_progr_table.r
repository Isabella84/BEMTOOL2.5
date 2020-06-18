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
bmt_reload_behav_progr_table<- function(w) {

  bmt_behav_progr_list <<- list()
  bmt_behav_progrIndex <<- 0

    if (is.null( bmt_fleet.behav_progr )) { 
   behav_progr_matrix <- data.frame(matrix(0, nrow=length(BEHAV_PROGR_head), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(behav_progr_matrix) <- c("",BMT_FLEETSEGMENTS)
     behav_progr_matrix[,1] <- BEHAV_PROGR_head
 bmt_fleet.behav_progr <<- behav_progr_matrix
 } else {
     behav_progr_matrix <<- bmt_fleet.behav_progr 
 }
 
   for (r in 1:nrow(behav_progr_matrix)) { 
  behav_progr_temp <- as.list(behav_progr_matrix[r,]) 
  bmt_behav_progr_list <<- c(bmt_behav_progr_list, list(behav_progr_temp)) 
  }

bmt_behav_progr.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(BMT_FLEETSEGMENTS)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_behav_progr_list)) {
    iter <- bmt_behav_progr.model$append()$iter
    bmt_behav_progr.model$set(iter,0, bmt_behav_progr_list[[i]][1])
    for (e in 1:length(BMT_FLEETSEGMENTS)) {
         bmt_behav_progr.model$set(iter, e, as.double(bmt_behav_progr_list[[i]][e+1]))
    }
       bmt_behav_progr.model$set(iter, (length(BMT_FLEETSEGMENTS)+1),TRUE)
  } 
 
bmt_behav_progr.treeview$destroy()
  
  bmt_behav_progr.treeview <<- gtkTreeViewNewWithModel( bmt_behav_progr.model)
 bmt_behav_progr.treeview$setRulesHint(TRUE)
 bmt_behav_progr.treeview$getSelection()$setMode("single")
bmt_behav_progr.add_columns( bmt_behav_progr.treeview)
bmt_behav_progr.sw$add(bmt_behav_progr.treeview)
    
}
