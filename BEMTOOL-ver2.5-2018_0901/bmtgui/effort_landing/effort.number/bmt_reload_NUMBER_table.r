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
bmt_reload_NUMBER_table<- function(w) {

  bmt_NUMBER_list <<- list()
  bmt_NUMBERIndex <<- 0

  if (is.null( bmt_fleet.NUMBER )) { 
  NUMBER_matrix <- data.frame(matrix(0, nrow=length(years), ncol=13))
   colnames(NUMBER_matrix) <- c("year",MONTHS)
     NUMBER_matrix$year <- years   
  bmt_fleet.NUMBER <<- NUMBER_matrix   
  } else {
    NUMBER_matrix <<- bmt_fleet.NUMBER 
  }

   for (r in 1:nrow(NUMBER_matrix)) { 
  NUMBER_temp <- as.list(NUMBER_matrix[r,]) 
  bmt_NUMBER_list <<- c(bmt_NUMBER_list, list(NUMBER_temp)) 
  }
  
bmt_NUMBER.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_NUMBER_list)) {
    iter <- bmt_NUMBER.model$append()$iter
    bmt_NUMBER.model$set(iter,0, bmt_NUMBER_list[[i]]$year)
    for (e in 1:length(MONTHS)) {
         bmt_NUMBER.model$set(iter, e, as.numeric(bmt_NUMBER_list[[i]][e+1]))
    }
       bmt_NUMBER.model$set(iter, 13,TRUE)
  } 
 
 if (exists("bmt_NUMBER.treeview")) { 
bmt_NUMBER.treeview$destroy()
}
  
  bmt_NUMBER.treeview <<- gtkTreeViewNewWithModel( bmt_NUMBER.model)
 bmt_NUMBER.treeview$setRulesHint(TRUE)
 bmt_NUMBER.treeview$getSelection()$setMode("single")
bmt_NUMBER.add_columns( bmt_NUMBER.treeview)
bmt_NUMBER.sw$add(bmt_NUMBER.treeview)
    
}
