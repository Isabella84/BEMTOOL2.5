# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
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
# Function to reload the values for the recruitment_fore_from_vector according to the 
# seed value
# ------------------------------------------------------------------------------
#
reload_EMPTY_growth_uncert_k_from_file_table <- function(w) {
  growth_uncert_k_from_file_list <<- list()
growth_uncert_k_from_file_index <<- 0

   zero_matrix <- data.frame(matrix(-1, nrow=CI_NB_RUNS_FORE, ncol=3 ))
     colnames(zero_matrix) <- c("run_N", "MALES", "FEMALES") 
   for (r in 1:nrow(zero_matrix)) { 
  sr_temp <- as.list(zero_matrix[r,]) 
  growth_uncert_k_from_file_list <<- c(growth_uncert_k_from_file_list, list(sr_temp)) 
  }
  
  growth_uncert_k_from_file_matrix <<- zero_matrix
 growth_uncert_k_from_file_model <<- gtkListStoreNew("gchararray", "gdouble","gdouble", "gboolean")
 
  for (i in 1:length(growth_uncert_k_from_file_list)) {
    iter <-  growth_uncert_k_from_file_model$append()$iter
     growth_uncert_k_from_file_model$set(iter,0, growth_uncert_k_from_file_list[[i]]$run_N) 
     growth_uncert_k_from_file_model$set(iter, 1, as.numeric(as.character(growth_uncert_k_from_file_list[[i]]$MALES)) )
	    growth_uncert_k_from_file_model$set(iter, 2, as.numeric(as.character(growth_uncert_k_from_file_list[[i]]$FEMALES)) )
     growth_uncert_k_from_file_model$set(iter, 3,TRUE)
  } 

  growth_uncert_k_from_file_treeview$destroy()
 growth_uncert_k_from_file_treeview <<- gtkTreeViewNewWithModel( growth_uncert_k_from_file_model)
 growth_uncert_k_from_file_treeview$setRulesHint(TRUE)
 growth_uncert_k_from_file_treeview$getSelection()$setMode("single")
 
 growth_uncert_k_from_file_add_columns(  growth_uncert_k_from_file_treeview)  
 growth_uncert_k_from_file_sw$add( growth_uncert_k_from_file_treeview)
   
}



reload_growth_uncert_k_from_file_table <- function(w) {
  growth_uncert_k_from_file_list <<- list()
 growth_uncert_k_from_file_index <<- 0

       this_matrix <-  growth_uncert_k_from_file_matrix  
   for (r in 1:nrow(this_matrix)) { 
  sr_temp <- as.list(this_matrix[r,]) 
  growth_uncert_k_from_file_list <<- c(growth_uncert_k_from_file_list, list(sr_temp)) 
  }
  
   growth_uncert_k_from_file_model <<- gtkListStoreNew("gchararray", "gdouble" ,"gdouble" ,"gboolean")
 
  for (i in 1:length(growth_uncert_k_from_file_list)) {
    iter <-  growth_uncert_k_from_file_model$append()$iter
     growth_uncert_k_from_file_model$set(iter,0, growth_uncert_k_from_file_list[[i]]$run_N) 
     growth_uncert_k_from_file_model$set(iter, 1, as.numeric(as.character(growth_uncert_k_from_file_list[[i]]$MALES)) )
	 growth_uncert_k_from_file_model$set(iter, 2, as.numeric(as.character(growth_uncert_k_from_file_list[[i]]$FEMALES)) )
     growth_uncert_k_from_file_model$set(iter, 3,TRUE)
  } 

  growth_uncert_k_from_file_treeview$destroy()
 growth_uncert_k_from_file_treeview <<- gtkTreeViewNewWithModel( growth_uncert_k_from_file_model)
 growth_uncert_k_from_file_treeview$setRulesHint(TRUE)
 growth_uncert_k_from_file_treeview$getSelection()$setMode("single")
 growth_uncert_k_from_file_add_columns( growth_uncert_k_from_file_treeview)  
 growth_uncert_k_from_file_sw$add(growth_uncert_k_from_file_treeview)

    
}
