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
bmt_reload_OTHERS_table<- function(w) {

  bmt_OTHERS_list <<- list()
  bmt_OTHERSIndex <<- 0

  if (is.null( bmt_economic.OTHERS )) { 
  OTHERS_matrix <- data.frame(matrix(0, nrow=length(years), ncol=(LENGTH(OTHERS_vector)+1)))
   colnames(OTHERS_matrix) <- c("year",OTHERS_vector)
     OTHERS_matrix$year <- years   
  bmt_economic.OTHERS <<- OTHERS_matrix   
  } else {
    OTHERS_matrix <<- bmt_economic.OTHERS 
  }

   for (r in 1:nrow(OTHERS_matrix)) { 
  OTHERS_temp <- as.list(OTHERS_matrix[r,]) 
  bmt_OTHERS_list <<- c(bmt_OTHERS_list, list(OTHERS_temp)) 
  }
  
bmt_OTHERS.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(OTHERS_vector)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_OTHERS_list)) {
    iter <- bmt_OTHERS.model$append()$iter
    bmt_OTHERS.model$set(iter,0, bmt_OTHERS_list[[i]]$year)
    for (e in 1:length(OTHERS_vector)) {
         bmt_OTHERS.model$set(iter, e, as.numeric(bmt_OTHERS_list[[i]][e+1]))
    }
       bmt_OTHERS.model$set(iter, (length(OTHERS_vector)+1),TRUE)
  } 
 
 if (exists("bmt_OTHERS.treeview")) { 
bmt_OTHERS.treeview$destroy()
}
  
  bmt_OTHERS.treeview <<- gtkTreeViewNewWithModel( bmt_OTHERS.model)
 bmt_OTHERS.treeview$setRulesHint(TRUE)
 bmt_OTHERS.treeview$getSelection()$setMode("single")
bmt_OTHERS.add_columns( bmt_OTHERS.treeview)
bmt_OTHERS.sw$add(bmt_OTHERS.treeview)
    
}
