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
bmt_reload_cost_equipment<- function(w) {

  bmt_cost_equipment_list <<- list()
  bmt_cost_equipmentIndex <<- 0

    if (is.null( bmt_fleet.cost_equipment )) { 
   cost_equipment_matrix <- data.frame(matrix(0, nrow=length(BMT_FLEETSEGMENTS), ncol=(length(BMT_YEARS_FORECAST)+1)))
   colnames(cost_equipment_matrix) <- c("FleetSegment",BMT_YEARS_FORECAST)
     cost_equipment_matrix$FleetSegment <- paste(" ", BMT_FLEETSEGMENTS, " ", sep="")
 bmt_fleet.cost_equipment <<- cost_equipment_matrix
 } else {
     cost_equipment_matrix <<- bmt_fleet.cost_equipment 
 }
 
   for (r in 1:nrow(cost_equipment_matrix)) { 
  cost_equipment_temp <- as.list(cost_equipment_matrix[r,]) 
  bmt_cost_equipment_list <<- c(bmt_cost_equipment_list, list(cost_equipment_temp)) 
  }

bmt_cost_equipment.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(BMT_YEARS_FORECAST)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_cost_equipment_list)) {
    iter <- bmt_cost_equipment.model$append()$iter
    bmt_cost_equipment.model$set(iter,0, bmt_cost_equipment_list[[i]]$FleetSegment)
    for (e in 1:length(BMT_YEARS_FORECAST)) {
         bmt_cost_equipment.model$set(iter, e, as.double(bmt_cost_equipment_list[[i]][e+1]))
    }
       bmt_cost_equipment.model$set(iter, (length(BMT_YEARS_FORECAST)+1),TRUE)
  } 
 
bmt_cost_equipment.treeview$destroy()
  
  bmt_cost_equipment.treeview <<- gtkTreeViewNewWithModel( bmt_cost_equipment.model)
 bmt_cost_equipment.treeview$setRulesHint(TRUE)
 bmt_cost_equipment.treeview$getSelection()$setMode("single")
bmt_cost_equipment.add_columns( bmt_cost_equipment.treeview)
bmt_cost_equipment.sw$add(bmt_cost_equipment.treeview)
    
    
}
