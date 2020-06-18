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
# ------------------------------------------------------------------------------
# Function to reload the values for the production according to the 
# seed value
# ------------------------------------------------------------------------------
#
bmt_reload_COSTS_table<- function(w) {

  bmt_COSTS_list <<- list()
  bmt_COSTSIndex <<- 0

  if (is.null( bmt_economic.COSTS )) { 
  COSTS_matrix <- data.frame(matrix(0, nrow=length(years), ncol=(length(COSTS_vector)+1)))
   colnames(COSTS_matrix) <- c("year",COSTS_vector)
     COSTS_matrix$year <- years   
  bmt_economic.COSTS <<- COSTS_matrix   
  } else {
    COSTS_matrix <<- bmt_economic.COSTS 
  }

   for (r in 1:nrow(COSTS_matrix)) { 
  COSTS_temp <- as.list(COSTS_matrix[r,]) 
  bmt_COSTS_list <<- c(bmt_COSTS_list, list(COSTS_temp)) 
  }
  
bmt_COSTS.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(COSTS_vector)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_COSTS_list)) {
    iter <- bmt_COSTS.model$append()$iter
    bmt_COSTS.model$set(iter,0, bmt_COSTS_list[[i]]$year)
    for (e in 1:length(COSTS_vector)) {
         bmt_COSTS.model$set(iter, e, as.numeric(bmt_COSTS_list[[i]][e+1]))
    }
       bmt_COSTS.model$set(iter, (length(COSTS_vector)+1),TRUE)
  } 
 
 if (exists("bmt_COSTS.treeview")) { 
bmt_COSTS.treeview$destroy()
}
  
  bmt_COSTS.treeview <<- gtkTreeViewNewWithModel( bmt_COSTS.model)
 bmt_COSTS.treeview$setRulesHint(TRUE)
 bmt_COSTS.treeview$getSelection()$setMode("single")
bmt_COSTS.add_columns( bmt_COSTS.treeview)
bmt_COSTS.sw$add(bmt_COSTS.treeview)
    
}
