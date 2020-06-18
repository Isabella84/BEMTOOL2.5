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
reload_effort_F<- function(w) {

  bmt_effort_F_list <<- list()
  bmt_effort_FIndex <<- 0

    if (is.null( bmt_effort_F )) { 
   effort_F_matrix <- data.frame(matrix(0, nrow=3, ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(effort_F_matrix) <- c("",BMT_FLEETSEGMENTS)
     effort_F_matrix[,1] <- c("Type of relationship", "coeff a", "coeff b")
 bmt_effort_F <<- effort_F_matrix
 } else {
    effort_F_matrix <<- bmt_effort_F 
 }
 
   for (r in 1:nrow(effort_F_matrix)) { 
  effort_F_temp <- as.list(effort_F_matrix[r,]) 
  bmt_effort_F_list <<- c(bmt_effort_F_list, list(effort_F_temp)) 
  }

bmt_effort_F.model <<- gtkListStoreNew(  rep("gchararray", (length(BMT_FLEETSEGMENTS)+1) ), "gboolean")  
  # add items 
  for (i in 1:length(bmt_effort_F_list)) {
    iter <- bmt_effort_F.model$append()$iter
    bmt_effort_F.model$set(iter,0, bmt_effort_F_list[[i]][1])
    for (e in 1:length(BMT_FLEETSEGMENTS)) {
         bmt_effort_F.model$set(iter, e, bmt_effort_F_list[[i]][e+1])
    }
       bmt_effort_F.model$set(iter, (length(BMT_FLEETSEGMENTS)+1),TRUE)
  } 
 
bmt_effort_F.treeview$destroy()
  
  bmt_effort_F.treeview <<- gtkTreeViewNewWithModel( bmt_effort_F.model)
 bmt_effort_F.treeview$setRulesHint(TRUE)
 bmt_effort_F.treeview$getSelection()$setMode("single")
bmt_effort_F.add_columns( bmt_effort_F.treeview)
bmt_effort_F.sw$add(bmt_effort_F.treeview)
    
}
