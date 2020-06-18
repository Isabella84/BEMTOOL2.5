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
bmt_reload_GT_table<- function(w) {

  bmt_GT_list <<- list()
  bmt_GTIndex <<- 0

    if (is.null( bmt_fleet.GT )) { 
   GT_matrix <- data.frame(matrix(0, nrow=length(years), ncol=13))
   colnames(GT_matrix) <- c("year",MONTHS)
     GT_matrix$year <- years
	     bmt_fleet.GT <<- GT_matrix
	 } else {
	    GT_matrix <<- bmt_fleet.GT  
	 }
	 
   for (r in 1:nrow(GT_matrix)) { 
  GT_temp <- as.list(GT_matrix[r,]) 
  bmt_GT_list <<- c(bmt_GT_list, list(GT_temp)) 
  }
  
bmt_GT.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_GT_list)) {
    iter <- bmt_GT.model$append()$iter
    bmt_GT.model$set(iter,0, bmt_GT_list[[i]]$year)
    for (e in 1:length(MONTHS)) {
         bmt_GT.model$set(iter, e, as.numeric(bmt_GT_list[[i]][e+1]))
    }
       bmt_GT.model$set(iter, 13,TRUE)
  } 
 
 if (exists("bmt_GT.treeview")) { 
bmt_GT.treeview$destroy()
}
  
  bmt_GT.treeview <<- gtkTreeViewNewWithModel( bmt_GT.model)
 bmt_GT.treeview$setRulesHint(TRUE)
 bmt_GT.treeview$getSelection()$setMode("single")
bmt_GT.add_columns( bmt_GT.treeview)
bmt_GT.sw$add(bmt_GT.treeview)
    
}
