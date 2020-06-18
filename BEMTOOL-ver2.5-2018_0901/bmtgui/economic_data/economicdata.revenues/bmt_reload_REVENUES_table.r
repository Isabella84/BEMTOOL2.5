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
bmt_reload_REVENUES_table<- function(w) {

  bmt_REVENUES_list <<- list()
  bmt_REVENUESIndex <<- 0

  if (is.null( bmt_economic.REVENUES )) { 
  REVENUES_matrix <- data.frame(matrix(0, nrow=length(years), ncol=(length(BMT_SPECIES)+1)))
   colnames(REVENUES_matrix) <- c("year",paste("casestudy.revenues.S", 1:length(BMT_SPECIES), sep=""))
     REVENUES_matrix$year <- years   
  bmt_economic.REVENUES <<- REVENUES_matrix   
  } else {
    REVENUES_matrix <<- bmt_economic.REVENUES 
  }

   for (r in 1:nrow(REVENUES_matrix)) { 
  REVENUES_temp <- as.list(REVENUES_matrix[r,]) 
  bmt_REVENUES_list <<- c(bmt_REVENUES_list, list(REVENUES_temp)) 
  }
  
bmt_REVENUES.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(BMT_SPECIES)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_REVENUES_list)) {
    iter <- bmt_REVENUES.model$append()$iter
    bmt_REVENUES.model$set(iter,0, bmt_REVENUES_list[[i]]$year)
    for (e in 1:length(BMT_SPECIES)) {
         bmt_REVENUES.model$set(iter, e, as.numeric(bmt_REVENUES_list[[i]][e+1]))
    }
       bmt_REVENUES.model$set(iter, (length(BMT_SPECIES)+1),TRUE)
  } 

if (exists("bmt_REVENUES.treeview")) {  
bmt_REVENUES.treeview$destroy()
}
  
  bmt_REVENUES.treeview <<- gtkTreeViewNewWithModel( bmt_REVENUES.model)
 bmt_REVENUES.treeview$setRulesHint(TRUE)
 bmt_REVENUES.treeview$getSelection()$setMode("single")
bmt_REVENUES.add_columns( bmt_REVENUES.treeview)
bmt_REVENUES.sw$add(bmt_REVENUES.treeview)
    
}
