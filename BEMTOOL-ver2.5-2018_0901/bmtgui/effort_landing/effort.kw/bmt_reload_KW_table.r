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
bmt_reload_KW_table<- function(w) {

  bmt_KW_list <<- list()
  bmt_KWIndex <<- 0

    if (is.null( bmt_fleet.KW )) { 
   KW_matrix <- data.frame(matrix(0, nrow=length(years), ncol=13))
   colnames(KW_matrix) <- c("year",MONTHS)
     KW_matrix$year <- years
	    bmt_fleet.KW <<- KW_matrix
 } else {
     KW_matrix <<- bmt_fleet.KW 
 }
 
   for (r in 1:nrow(KW_matrix)) { 
  KW_temp <- as.list(KW_matrix[r,]) 
  bmt_KW_list <<- c(bmt_KW_list, list(KW_temp)) 
  }
  
  
bmt_KW.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_KW_list)) {
    iter <- bmt_KW.model$append()$iter
    bmt_KW.model$set(iter,0, bmt_KW_list[[i]]$year)
    for (e in 1:length(MONTHS)) {
         bmt_KW.model$set(iter, e, as.numeric(bmt_KW_list[[i]][e+1]))
    }
       bmt_KW.model$set(iter, 13,TRUE)
  } 

if (exists("bmt_KW.treeview")) {  
bmt_KW.treeview$destroy()
}
  
  bmt_KW.treeview <<- gtkTreeViewNewWithModel( bmt_KW.model)
 bmt_KW.treeview$setRulesHint(TRUE)
 bmt_KW.treeview$getSelection()$setMode("single")
bmt_KW.add_columns( bmt_KW.treeview)
bmt_KW.sw$add(bmt_KW.treeview)
    
}
