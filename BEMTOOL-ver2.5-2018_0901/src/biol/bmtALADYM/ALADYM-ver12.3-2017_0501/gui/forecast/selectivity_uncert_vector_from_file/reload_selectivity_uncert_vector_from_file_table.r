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
reload_EMPTY_selectivity_uncert_vector_from_file_table <- function(w) {
  selectivity_uncert_vector_from_file_list <<- list()
selectivity_uncert_vector_from_file_index <<- 0

   zero_matrix <- data.frame(matrix(-1, nrow=CI_NB_RUNS_FORE, ncol=3 ))
     colnames(zero_matrix) <- c("run_N",	"param1",	"param2",	"param3",	"param4",	"param5",	"sel_type",	"fleet") 
   for (r in 1:nrow(zero_matrix)) { 
  sr_temp <- as.list(zero_matrix[r,]) 
  selectivity_uncert_vector_from_file_list <<- c(selectivity_uncert_vector_from_file_list, list(sr_temp)) 
  }
  
  selectivity_uncert_vector_from_file_matrix <<- zero_matrix
 selectivity_uncert_vector_from_file_model <<- gtkListStoreNew("gchararray", rep("gdouble", 6), "gchararray", "gboolean")
 
  for (i in 1:length(selectivity_uncert_vector_from_file_list)) {
    iter <-  selectivity_uncert_vector_from_file_model$append()$iter
    selectivity_uncert_vector_from_file_model$set(iter,0, selectivity_uncert_vector_from_file_list[[i]]$run_N)
     selectivity_uncert_vector_from_file_model$set(iter, 1, as.numeric(as.character(selectivity_uncert_vector_from_file_list[[i]]$param1)) )
	  selectivity_uncert_vector_from_file_model$set(iter, 2, as.numeric(as.character(selectivity_uncert_vector_from_file_list[[i]]$param2)) )
	   selectivity_uncert_vector_from_file_model$set(iter, 3, as.numeric(as.character(selectivity_uncert_vector_from_file_list[[i]]$param3)) )
	  selectivity_uncert_vector_from_file_model$set(iter, 4, as.numeric(as.character(selectivity_uncert_vector_from_file_list[[i]]$param4)) )
	   selectivity_uncert_vector_from_file_model$set(iter, 5, as.numeric(as.character(selectivity_uncert_vector_from_file_list[[i]]$param5)) )
	  selectivity_uncert_vector_from_file_model$set(iter, 6, as.numeric(as.character(selectivity_uncert_vector_from_file_list[[i]]$sel_type)) )
	  	  selectivity_uncert_vector_from_file_model$set(iter, 7, as.character(selectivity_uncert_vector_from_file_list[[i]]$fleet) )
    selectivity_uncert_vector_from_file_model$set(iter, 8,TRUE)
  } 

  selectivity_uncert_vector_from_file_treeview$destroy()
 selectivity_uncert_vector_from_file_treeview <<- gtkTreeViewNewWithModel( selectivity_uncert_vector_from_file_model)
 selectivity_uncert_vector_from_file_treeview$setRulesHint(TRUE)
 selectivity_uncert_vector_from_file_treeview$getSelection()$setMode("single")
 
 selectivity_uncert_vector_from_file_add_columns(  selectivity_uncert_vector_from_file_treeview)  
 selectivity_uncert_vector_from_file_sw$add( selectivity_uncert_vector_from_file_treeview)
   
}



reload_selectivity_uncert_vector_from_file_table <- function(w) {
  selectivity_uncert_vector_from_file_list <<- list()
 selectivity_uncert_vector_from_file_index <<- 0

       this_matrix <-  selectivity_uncert_vector_from_file_matrix  
   for (r in 1:nrow(this_matrix)) { 
  sr_temp <- as.list(this_matrix[r,]) 
  selectivity_uncert_vector_from_file_list <<- c(selectivity_uncert_vector_from_file_list, list(sr_temp)) 
  }
  
   selectivity_uncert_vector_from_file_model <<- gtkListStoreNew("gchararray", rep("gdouble", 6), "gchararray", "gboolean")
 
  for (i in 1:length(selectivity_uncert_vector_from_file_list)) {
    iter <-  selectivity_uncert_vector_from_file_model$append()$iter
    selectivity_uncert_vector_from_file_model$set(iter,0, selectivity_uncert_vector_from_file_list[[i]]$run_N)
     selectivity_uncert_vector_from_file_model$set(iter, 1, as.numeric(as.character(selectivity_uncert_vector_from_file_list[[i]]$param1)) )
	  selectivity_uncert_vector_from_file_model$set(iter, 2, as.numeric(as.character(selectivity_uncert_vector_from_file_list[[i]]$param2)) )
	   selectivity_uncert_vector_from_file_model$set(iter, 3, as.numeric(as.character(selectivity_uncert_vector_from_file_list[[i]]$param3)) )
	  selectivity_uncert_vector_from_file_model$set(iter, 4, as.numeric(as.character(selectivity_uncert_vector_from_file_list[[i]]$param4)) )
	   selectivity_uncert_vector_from_file_model$set(iter, 5, as.numeric(as.character(selectivity_uncert_vector_from_file_list[[i]]$param5)) )
	  selectivity_uncert_vector_from_file_model$set(iter, 6, as.numeric(as.character(selectivity_uncert_vector_from_file_list[[i]]$sel_type)) )
	  	  selectivity_uncert_vector_from_file_model$set(iter, 7, as.character(selectivity_uncert_vector_from_file_list[[i]]$fleet) )
    selectivity_uncert_vector_from_file_model$set(iter, 8,TRUE)
  } 

  selectivity_uncert_vector_from_file_treeview$destroy()
 selectivity_uncert_vector_from_file_treeview <<- gtkTreeViewNewWithModel( selectivity_uncert_vector_from_file_model)
 selectivity_uncert_vector_from_file_treeview$setRulesHint(TRUE)
 selectivity_uncert_vector_from_file_treeview$getSelection()$setMode("single")
 selectivity_uncert_vector_from_file_add_columns( selectivity_uncert_vector_from_file_treeview)  
 selectivity_uncert_vector_from_file_sw$add(selectivity_uncert_vector_from_file_treeview)

    
}
