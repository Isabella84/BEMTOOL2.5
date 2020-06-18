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
bmt_reload_labour_commercial_table<- function(w) {

  bmt_labour_commercial_list <<- list()
  bmt_labour_commercialIndex <<- 0

    if (is.null( bmt_fleet.labour_commercial )) { 
   labour_commercial_matrix <- data.frame(matrix(FALSE, nrow=1, ncol=length(BMT_FLEETSEGMENTS)))
   colnames(labour_commercial_matrix) <- c(BMT_FLEETSEGMENTS)
#     labour_commercial_matrix[1,1] <- c(" fuel costs in labour costs dynamic ")
 bmt_fleet.labour_commercial <<- labour_commercial_matrix
 } else {
     labour_commercial_matrix <<- bmt_fleet.labour_commercial 
 }
 
   for (r in 1:nrow(labour_commercial_matrix)) { 
  labour_commercial_temp <- as.list(labour_commercial_matrix[r,]) 
  bmt_labour_commercial_list <<- c(bmt_labour_commercial_list, list(labour_commercial_temp)) 
  }

bmt_labour_commercial.model <<- gtkListStoreNew(  rep("gboolean", length(BMT_FLEETSEGMENTS)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_labour_commercial_list)) {
    iter <- bmt_labour_commercial.model$append()$iter
#    bmt_labour_commercial.model$set(iter, 0, bmt_labour_commercial_list[[i]][1])
    for (e in 1:length(BMT_FLEETSEGMENTS)) {
     value <- bmt_labour_commercial_list[[i]][e][[1]]
         if (value ) {
              bmt_labour_commercial.model$set(iter, e-1, TRUE) 
         } else {
              bmt_labour_commercial.model$set(iter, e-1, FALSE) 
         } 
#         bmt_labour_commercial.model$set(iter, e, as.logical(bmt_labour_commercial_list[[i]][e+1]))      # 
    }
       bmt_labour_commercial.model$set(iter, length(BMT_FLEETSEGMENTS),TRUE)
  } 
 
bmt_labour_commercial.treeview$destroy()
  
  bmt_labour_commercial.treeview <<- gtkTreeViewNewWithModel( bmt_labour_commercial.model)
 bmt_labour_commercial.treeview$setRulesHint(TRUE)
 bmt_labour_commercial.treeview$getSelection()$setMode("single")
bmt_labour_commercial.add_columns( bmt_labour_commercial.treeview)
bmt_labour_commercial.sw$add(bmt_labour_commercial.treeview)
    
}
