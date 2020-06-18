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
bmt_reload_DAY_table<- function(w) {

  bmt_DAY_list <<- list()
  bmt_DAYIndex <<- 0

    if (is.null( bmt_fleet.DAY )) { 
   DAY_matrix <- data.frame(matrix(0, nrow=length(years), ncol=13))
   colnames(DAY_matrix) <- c("year",MONTHS)
     DAY_matrix$year <- years
 bmt_fleet.DAY <<- DAY_matrix
 } else {
     DAY_matrix <<- bmt_fleet.DAY 
 }
 
   for (r in 1:nrow(DAY_matrix)) { 
  DAY_temp <- as.list(DAY_matrix[r,]) 
  bmt_DAY_list <<- c(bmt_DAY_list, list(DAY_temp)) 
  }

bmt_DAY.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_DAY_list)) {
    iter <- bmt_DAY.model$append()$iter
    bmt_DAY.model$set(iter,0, bmt_DAY_list[[i]]$year)
    for (e in 1:length(MONTHS)) {
         bmt_DAY.model$set(iter, e, as.numeric(bmt_DAY_list[[i]][e+1]))
    }
       bmt_DAY.model$set(iter, 13,TRUE)
  } 

if (exists("bmt_DAY.treeview")) { 
bmt_DAY.treeview$destroy()
}
  
  bmt_DAY.treeview <<- gtkTreeViewNewWithModel( bmt_DAY.model)
 bmt_DAY.treeview$setRulesHint(TRUE)
 bmt_DAY.treeview$getSelection()$setMode("single")
bmt_DAY.add_columns( bmt_DAY.treeview)
bmt_DAY.sw$add(bmt_DAY.treeview)
    
}
