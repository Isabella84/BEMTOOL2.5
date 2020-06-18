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
bmt_reload_LANDING_table<- function(w) {

  bmt_LANDING_list <<- list()
  bmt_LANDINGIndex <<- 0


  if (is.null( bmt_fleet.LANDING )) { 
  LANDING_matrix <- data.frame(matrix(0, nrow=length(years), ncol=13))
   colnames(LANDING_matrix) <- c("year",MONTHS)
     LANDING_matrix$year <- years
  bmt_fleet.LANDING <<- LANDING_matrix   
  } else {
    LANDING_matrix <<- bmt_fleet.LANDING
  }

   for (r in 1:nrow(LANDING_matrix)) { 
  LANDING_temp <- as.list(LANDING_matrix[r,]) 
  bmt_LANDING_list <<- c(bmt_LANDING_list, list(LANDING_temp)) 
  }
  
bmt_LANDING.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_LANDING_list)) {
    iter <- bmt_LANDING.model$append()$iter
    bmt_LANDING.model$set(iter,0, bmt_LANDING_list[[i]]$year)
    for (e in 1:length(MONTHS)) {
         bmt_LANDING.model$set(iter, e, as.numeric(bmt_LANDING_list[[i]][e+1]))
    }
       bmt_LANDING.model$set(iter, 13,TRUE)
  } 

  if (exists("bmt_LANDING.treeview")) { 
bmt_LANDING.treeview$destroy()
}
  
  bmt_LANDING.treeview <<- gtkTreeViewNewWithModel( bmt_LANDING.model)
 bmt_LANDING.treeview$setRulesHint(TRUE)
 bmt_LANDING.treeview$getSelection()$setMode("single")
bmt_LANDING.add_columns( bmt_LANDING.treeview)
bmt_LANDING.sw$add(bmt_LANDING.treeview)
    
}
