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
bmt_reload_ecoind_table<- function(w) {

  bmt_ecoind_list <<- list()
  bmt_ecoindIndex <<- 0

    if (is.null( bmt_fleet.ecoind )) { 
   ecoind_matrix <- data.frame(matrix(0, nrow=length(ECOIND_head), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(ecoind_matrix) <- c("",BMT_FLEETSEGMENTS)
     ecoind_matrix[,1] <- ECOIND_head
 bmt_fleet.ecoind <<- ecoind_matrix
 } else {
     ecoind_matrix <<- bmt_fleet.ecoind 
 }
 
   for (r in 1:nrow(ecoind_matrix)) { 
  ecoind_temp <- as.list(ecoind_matrix[r,]) 
  bmt_ecoind_list <<- c(bmt_ecoind_list, list(ecoind_temp)) 
  }

bmt_ecoind.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(BMT_FLEETSEGMENTS)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_ecoind_list)) {
    iter <- bmt_ecoind.model$append()$iter
    bmt_ecoind.model$set(iter,0, bmt_ecoind_list[[i]][1])
    for (e in 1:length(BMT_FLEETSEGMENTS)) {
         bmt_ecoind.model$set(iter, e, as.double(bmt_ecoind_list[[i]][e+1]))
    }
       bmt_ecoind.model$set(iter, (length(BMT_FLEETSEGMENTS)+1),TRUE)
  } 
 
bmt_ecoind.treeview$destroy()
  
  bmt_ecoind.treeview <<- gtkTreeViewNewWithModel( bmt_ecoind.model)
 bmt_ecoind.treeview$setRulesHint(TRUE)
 bmt_ecoind.treeview$getSelection()$setMode("single")
bmt_ecoind.add_columns( bmt_ecoind.treeview)
bmt_ecoind.sw$add(bmt_ecoind.treeview)
    
}
