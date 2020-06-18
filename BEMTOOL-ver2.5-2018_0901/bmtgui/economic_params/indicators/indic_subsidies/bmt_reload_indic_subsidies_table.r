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
bmt_reload_indic_subsidies_table<- function(w) {

  bmt_indic_subsidies_list <<- list()
  bmt_indic_subsidiesIndex <<- 0

    if (is.null( bmt_fleet.indic_subsidies )) { 
   indic_subsidies_matrix <- data.frame(matrix(0, nrow=length(BMT_FLEETSEGMENTS), ncol=(length(BMT_YEARS_FORECAST)+1)))
   colnames(indic_subsidies_matrix) <- c("FleetSegment",BMT_YEARS_FORECAST)
     indic_subsidies_matrix$FleetSegment <- paste(" ", BMT_FLEETSEGMENTS, " ", sep="")
 bmt_fleet.indic_subsidies <<- indic_subsidies_matrix
 } else {
     indic_subsidies_matrix <<- bmt_fleet.indic_subsidies 
 }
 
   for (r in 1:nrow(indic_subsidies_matrix)) { 
  indic_subsidies_temp <- as.list(indic_subsidies_matrix[r,]) 
  bmt_indic_subsidies_list <<- c(bmt_indic_subsidies_list, list(indic_subsidies_temp)) 
  }

bmt_indic_subsidies.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(BMT_YEARS_FORECAST)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_indic_subsidies_list)) {
    iter <- bmt_indic_subsidies.model$append()$iter
    bmt_indic_subsidies.model$set(iter,0, bmt_indic_subsidies_list[[i]]$FleetSegment)
    for (e in 1:length(BMT_YEARS_FORECAST)) {
         bmt_indic_subsidies.model$set(iter, e, as.double(bmt_indic_subsidies_list[[i]][e+1]))
    }
       bmt_indic_subsidies.model$set(iter, (length(BMT_YEARS_FORECAST)+1),TRUE)
  } 
 
bmt_indic_subsidies.treeview$destroy()
  
  bmt_indic_subsidies.treeview <<- gtkTreeViewNewWithModel( bmt_indic_subsidies.model)
 bmt_indic_subsidies.treeview$setRulesHint(TRUE)
 bmt_indic_subsidies.treeview$getSelection()$setMode("single")
bmt_indic_subsidies.add_columns( bmt_indic_subsidies.treeview)
bmt_indic_subsidies.sw$add(bmt_indic_subsidies.treeview)
    
}
