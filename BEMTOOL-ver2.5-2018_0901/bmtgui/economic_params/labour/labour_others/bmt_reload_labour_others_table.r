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
bmt_reload_labour_others_table<- function(w) {

  bmt_labour_others_list <<- list()
  bmt_labour_othersIndex <<- 0

    if (is.null( bmt_fleet.labour_others )) { 
   labour_others_matrix <- data.frame(matrix(FALSE, nrow=1, ncol=length(BMT_FLEETSEGMENTS)))
   colnames(labour_others_matrix) <- c(BMT_FLEETSEGMENTS)
#     labour_others_matrix[1,1] <- c(" fuel costs in labour costs dynamic ")
 bmt_fleet.labour_others <<- labour_others_matrix
 } else {
     labour_others_matrix <<- bmt_fleet.labour_others 
 }
 
   for (r in 1:nrow(labour_others_matrix)) { 
  labour_others_temp <- as.list(labour_others_matrix[r,]) 
  bmt_labour_others_list <<- c(bmt_labour_others_list, list(labour_others_temp)) 
  }

bmt_labour_others.model <<- gtkListStoreNew( rep("gboolean", length(BMT_FLEETSEGMENTS)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_labour_others_list)) {
    iter <- bmt_labour_others.model$append()$iter
#    bmt_labour_others.model$set(iter, 0, bmt_labour_others_list[[i]][1])
    for (e in 1:length(BMT_FLEETSEGMENTS)) {
     value <- bmt_labour_others_list[[i]][e][[1]]
         if (value ) {
              bmt_labour_others.model$set(iter, e-1, TRUE) 
         } else {
              bmt_labour_others.model$set(iter, e-1, FALSE) 
         } 
#         bmt_labour_others.model$set(iter, e, as.logical(bmt_labour_others_list[[i]][e+1]))      # 
    }
       bmt_labour_others.model$set(iter, length(BMT_FLEETSEGMENTS),TRUE)
  } 
 
bmt_labour_others.treeview$destroy()
  
  bmt_labour_others.treeview <<- gtkTreeViewNewWithModel( bmt_labour_others.model)
 bmt_labour_others.treeview$setRulesHint(TRUE)
 bmt_labour_others.treeview$getSelection()$setMode("single")
bmt_labour_others.add_columns( bmt_labour_others.treeview)
bmt_labour_others.sw$add(bmt_labour_others.treeview)
    
}
