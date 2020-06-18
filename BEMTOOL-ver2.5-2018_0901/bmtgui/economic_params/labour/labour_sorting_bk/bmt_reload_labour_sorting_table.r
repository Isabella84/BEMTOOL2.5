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
bmt_reload_labour_sorting_table<- function(w) {

  bmt_labour_sorting_list <<- list()
  bmt_labour_sortingIndex <<- 0

    if (is.null( bmt_fleet.labour_sorting )) { 
   labour_sorting_matrix <- data.frame(matrix(0, nrow=1, ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(labour_sorting_matrix) <- c(" ",BMT_FLEETSEGMENTS)
     labour_sorting_matrix[1,1] <- " sorting coefficient "
 bmt_fleet.labour_sorting <<- labour_sorting_matrix
 } else {
     labour_sorting_matrix <<- bmt_fleet.labour_sorting 
 }
 
   for (r in 1:nrow(labour_sorting_matrix)) { 
  labour_sorting_temp <- as.list(labour_sorting_matrix[r,]) 
  bmt_labour_sorting_list <<- c(bmt_labour_sorting_list, list(labour_sorting_temp)) 
  }

bmt_labour_sorting.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(BMT_FLEETSEGMENTS)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_labour_sorting_list)) {
    iter <- bmt_labour_sorting.model$append()$iter
    bmt_labour_sorting.model$set(iter,0, bmt_labour_sorting_list[[i]][1])
    for (e in 1:length(BMT_FLEETSEGMENTS)) {
         bmt_labour_sorting.model$set(iter, e, as.double(bmt_labour_sorting_list[[i]][e+1]))
    }
       bmt_labour_sorting.model$set(iter, (length(BMT_FLEETSEGMENTS)+1),TRUE)
  } 
 
bmt_labour_sorting.treeview$destroy()
  
  bmt_labour_sorting.treeview <<- gtkTreeViewNewWithModel( bmt_labour_sorting.model)
 bmt_labour_sorting.treeview$setRulesHint(TRUE)
 bmt_labour_sorting.treeview$getSelection()$setMode("single")
bmt_labour_sorting.add_columns( bmt_labour_sorting.treeview)
bmt_labour_sorting.sw$add(bmt_labour_sorting.treeview)
    
}
