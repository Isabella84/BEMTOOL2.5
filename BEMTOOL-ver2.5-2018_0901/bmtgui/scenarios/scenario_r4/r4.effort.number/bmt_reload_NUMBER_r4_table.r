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
bmt_reload_NUMBER_r4_table<- function(w) {

  bmt_NUMBER_r4_list <<- list()
  bmt_NUMBER_r4Index <<- 0

  if (is.null( bmt_fleet.NUMBER_r4 )) {
    NUMBER_r4_matrix <- data.frame(matrix(0, nrow=length(BMT_YEARS_FORECAST), ncol=13))
   colnames(NUMBER_r4_matrix) <- c("year",MONTHS)
     NUMBER_r4_matrix$year <- BMT_YEARS_FORECAST  
     
  if (length(EFFORT_NUMBER_list) == 0) {   
  bmt_fleet.NUMBER_r4 <<- NUMBER_r4_matrix  
  } else {
  
  selected_fleet <- gtkComboBoxGetActive(bmt_combo_fleetsegments_effort_r4)
            selected_fleet <- ifelse(selected_fleet >= 0, selected_fleet, 0)
      last_years_vess <- EFFORT_NUMBER_list_fore[[selected_fleet+1]]
    
    for (ri in 1:nrow(NUMBER_r4_matrix)) {
     NUMBER_r4_matrix[ri,2:13] <-  as.numeric(as.character(last_years_vess[nrow(last_years_vess),2:13]))                   
    } 
 
  EFFORT_NUMBER_list_fore[[selected_fleet+1]] <<-  NUMBER_r4_matrix 
    bmt_fleet.NUMBER_r4 <<- NUMBER_r4_matrix      

  } 
  } else {
    NUMBER_r4_matrix <<- bmt_fleet.NUMBER_r4 
  }

   for (r in 1:nrow(NUMBER_r4_matrix)) { 
  NUMBER_r4_temp <- as.list(NUMBER_r4_matrix[r,]) 
  bmt_NUMBER_r4_list <<- c(bmt_NUMBER_r4_list, list(NUMBER_r4_temp)) 
  }
  
bmt_NUMBER_r4.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_NUMBER_r4_list)) {
    iter <- bmt_NUMBER_r4.model$append()$iter
    bmt_NUMBER_r4.model$set(iter,0, bmt_NUMBER_r4_list[[i]]$year)
    for (e in 1:length(MONTHS)) {
         bmt_NUMBER_r4.model$set(iter, e, as.numeric(bmt_NUMBER_r4_list[[i]][e+1]))
    }
       bmt_NUMBER_r4.model$set(iter, 13,TRUE)
  } 
 
 if (exists("bmt_NUMBER_r4.treeview")) { 
bmt_NUMBER_r4.treeview$destroy()
}
  
  bmt_NUMBER_r4.treeview <<- gtkTreeViewNewWithModel( bmt_NUMBER_r4.model)
 bmt_NUMBER_r4.treeview$setRulesHint(TRUE)
 bmt_NUMBER_r4.treeview$getSelection()$setMode("single")
bmt_NUMBER_r4.add_columns( bmt_NUMBER_r4.treeview)
bmt_NUMBER_r4.sw$add(bmt_NUMBER_r4.treeview)
  
}
