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
bmt_reload_DAY_r4_table<- function(w) {

  bmt_DAY_r4_list <<- list()
  bmt_DAY_r4Index <<- 0

    if (is.null( bmt_fleet.DAY_r4 )) {
    DAY_r4_matrix <- data.frame(matrix(0, nrow=length(BMT_YEARS_FORECAST), ncol=13))
   colnames(DAY_r4_matrix) <- c("year",MONTHS)
     DAY_r4_matrix$year <- BMT_YEARS_FORECAST  
     
      if (length(EFFORT_DAY_list) == 0) {    
 bmt_fleet.DAY_r4 <<- DAY_r4_matrix
 } else {
        selected_fleet <- gtkComboBoxGetActive(bmt_combo_fleetsegments_effort_r4)
        selected_fleet <- ifelse(selected_fleet >= 0, selected_fleet, 0)
         last_years_days <- EFFORT_DAY_list[[selected_fleet+1]]
        for (ri in 1:nrow(DAY_r4_matrix)) {     
        DAY_r4_matrix[ri,2:13] <-  as.numeric(as.character( last_years_days[nrow(last_years_days),2:13]))     
        }
    EFFORT_DAY_list_fore[[fl]] <<-  DAY_r4_matrix      
        bmt_fleet.DAY_r4 <<- DAY_r4_matrix      
 }
 } else {
     DAY_r4_matrix <<- bmt_fleet.DAY_r4 
 }
 
   for (r in 1:nrow(DAY_r4_matrix)) { 
  DAY_r4_temp <- as.list(DAY_r4_matrix[r,]) 
  bmt_DAY_r4_list <<- c(bmt_DAY_r4_list, list(DAY_r4_temp)) 
  }

bmt_DAY_r4.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length(bmt_DAY_r4_list)) {
    iter <- bmt_DAY_r4.model$append()$iter
    bmt_DAY_r4.model$set(iter,0, bmt_DAY_r4_list[[i]]$year)
    for (e in 1:length(MONTHS)) {
         bmt_DAY_r4.model$set(iter, e, as.numeric(bmt_DAY_r4_list[[i]][e+1]))
    }
       bmt_DAY_r4.model$set(iter, 13,TRUE)
  } 

if (exists("bmt_DAY_r4.treeview")) { 
bmt_DAY_r4.treeview$destroy()
}
  
  bmt_DAY_r4.treeview <<- gtkTreeViewNewWithModel( bmt_DAY_r4.model)
 bmt_DAY_r4.treeview$setRulesHint(TRUE)
 bmt_DAY_r4.treeview$getSelection()$setMode("single")
bmt_DAY_r4.add_columns( bmt_DAY_r4.treeview)
bmt_DAY_r4.sw$add(bmt_DAY_r4.treeview)
    
}
