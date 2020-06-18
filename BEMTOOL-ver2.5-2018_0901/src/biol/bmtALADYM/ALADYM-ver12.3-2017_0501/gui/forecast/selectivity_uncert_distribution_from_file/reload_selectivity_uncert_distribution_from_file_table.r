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
reload_EMPTY_selectivity_uncert_distribution_from_file_table <- function(w) {
  selectivity_uncert_distribution_from_file_list <<- list()
selectivity_uncert_distribution_from_file_index <<- 0

   zero_matrix <- data.frame(matrix(-1, nrow=CI_NB_RUNS_FORE, ncol=13 ))
     colnames(zero_matrix) <- c("fleet", "sel_type", "distribution", "param1_a", "param2_a", "param3_a", "param4_a", "param5_a", "param1_b", "param2_b", "param3_b", "param4_b", "param5_b") 
   for (r in 1:nrow(zero_matrix)) { 
  sr_temp <- as.list(zero_matrix[r,]) 
  selectivity_uncert_distribution_from_file_list <<- c(selectivity_uncert_distribution_from_file_list, list(sr_temp)) 
  }
  
  selectivity_uncert_distribution_from_file_matrix <<- zero_matrix
 selectivity_uncert_distribution_from_file_model <<- gtkListStoreNew("gchararray", "gdouble", "gchararray", rep("gdouble", 10), "gboolean")
 
  for (i in 1:length(selectivity_uncert_distribution_from_file_list)) {
    iter <-  selectivity_uncert_distribution_from_file_model$append()$iter
     selectivity_uncert_distribution_from_file_model$set(iter,0, selectivity_uncert_distribution_from_file_list[[i]]$fleet) 
     selectivity_uncert_distribution_from_file_model$set(iter, 1, as.numeric(selectivity_uncert_distribution_from_file_list[[i]]$sel_type) )
	    selectivity_uncert_distribution_from_file_model$set(iter, 2, as.character(selectivity_uncert_distribution_from_file_list[[i]]$distribution) )
		 selectivity_uncert_distribution_from_file_model$set(iter, 3, as.numeric(selectivity_uncert_distribution_from_file_list[[i]]$param1_a) )
		  selectivity_uncert_distribution_from_file_model$set(iter, 4, as.numeric(selectivity_uncert_distribution_from_file_list[[i]]$param2_a) )
		   selectivity_uncert_distribution_from_file_model$set(iter, 5, as.numeric(selectivity_uncert_distribution_from_file_list[[i]]$param3_a) )
		    selectivity_uncert_distribution_from_file_model$set(iter, 6, as.numeric(selectivity_uncert_distribution_from_file_list[[i]]$param4_a) )
			 selectivity_uncert_distribution_from_file_model$set(iter, 7, as.numeric(selectivity_uncert_distribution_from_file_list[[i]]$param5_a) )
			  selectivity_uncert_distribution_from_file_model$set(iter, 8, as.numeric(selectivity_uncert_distribution_from_file_list[[i]]$param1_b) )
			   selectivity_uncert_distribution_from_file_model$set(iter, 9, as.numeric(selectivity_uncert_distribution_from_file_list[[i]]$param2_b) )
			    selectivity_uncert_distribution_from_file_model$set(iter, 10, as.numeric(selectivity_uncert_distribution_from_file_list[[i]]$param3_b) )
				 selectivity_uncert_distribution_from_file_model$set(iter, 11, as.numeric(selectivity_uncert_distribution_from_file_list[[i]]$param4_b) )
				  selectivity_uncert_distribution_from_file_model$set(iter, 12, as.numeric(selectivity_uncert_distribution_from_file_list[[i]]$param5_b) )
     selectivity_uncert_distribution_from_file_model$set(iter, 13,TRUE)
  } 

  selectivity_uncert_distribution_from_file_treeview$destroy()
 selectivity_uncert_distribution_from_file_treeview <<- gtkTreeViewNewWithModel( selectivity_uncert_distribution_from_file_model)
 selectivity_uncert_distribution_from_file_treeview$setRulesHint(TRUE)
 selectivity_uncert_distribution_from_file_treeview$getSelection()$setMode("single")
 
 selectivity_uncert_distribution_from_file_add_columns(  selectivity_uncert_distribution_from_file_treeview)  
 selectivity_uncert_distribution_from_file_sw$add( selectivity_uncert_distribution_from_file_treeview)
   
}



reload_selectivity_uncert_distribution_from_file_table <- function(w) {
  selectivity_uncert_distribution_from_file_list <<- list()
 selectivity_uncert_distribution_from_file_index <<- 0

       this_matrix <-  selectivity_uncert_distribution_from_file_matrix  
   for (r in 1:nrow(this_matrix)) { 
  sr_temp <- as.list(this_matrix[r,]) 
  selectivity_uncert_distribution_from_file_list <<- c(selectivity_uncert_distribution_from_file_list, list(sr_temp)) 
  }
  
   selectivity_uncert_distribution_from_file_model <<- gtkListStoreNew("gchararray", "gdouble", "gchararray", rep("gdouble", 10), "gboolean")
 
  for (i in 1:length(selectivity_uncert_distribution_from_file_list)) {
    iter <-  selectivity_uncert_distribution_from_file_model$append()$iter
     selectivity_uncert_distribution_from_file_model$set(iter,0, selectivity_uncert_distribution_from_file_list[[i]]$fleet) 
     selectivity_uncert_distribution_from_file_model$set(iter, 1, as.numeric(selectivity_uncert_distribution_from_file_list[[i]]$sel_type) )
	    selectivity_uncert_distribution_from_file_model$set(iter, 2, as.character(selectivity_uncert_distribution_from_file_list[[i]]$distribution) )
		 selectivity_uncert_distribution_from_file_model$set(iter, 3, as.numeric(selectivity_uncert_distribution_from_file_list[[i]]$param1_a) )
		  selectivity_uncert_distribution_from_file_model$set(iter, 4, as.numeric(selectivity_uncert_distribution_from_file_list[[i]]$param2_a) )
		   selectivity_uncert_distribution_from_file_model$set(iter, 5, as.numeric(selectivity_uncert_distribution_from_file_list[[i]]$param3_a) )
		    selectivity_uncert_distribution_from_file_model$set(iter, 6, as.numeric(selectivity_uncert_distribution_from_file_list[[i]]$param4_a) )
			 selectivity_uncert_distribution_from_file_model$set(iter, 7, as.numeric(selectivity_uncert_distribution_from_file_list[[i]]$param5_a) )
			  selectivity_uncert_distribution_from_file_model$set(iter, 8, as.numeric(selectivity_uncert_distribution_from_file_list[[i]]$param1_b) )
			   selectivity_uncert_distribution_from_file_model$set(iter, 9, as.numeric(selectivity_uncert_distribution_from_file_list[[i]]$param2_b) )
			    selectivity_uncert_distribution_from_file_model$set(iter, 10, as.numeric(selectivity_uncert_distribution_from_file_list[[i]]$param3_b) )
				 selectivity_uncert_distribution_from_file_model$set(iter, 11, as.numeric(selectivity_uncert_distribution_from_file_list[[i]]$param4_b) )
				  selectivity_uncert_distribution_from_file_model$set(iter, 12, as.numeric(selectivity_uncert_distribution_from_file_list[[i]]$param5_b) )
     selectivity_uncert_distribution_from_file_model$set(iter, 13,TRUE)
  } 

  selectivity_uncert_distribution_from_file_treeview$destroy()
 selectivity_uncert_distribution_from_file_treeview <<- gtkTreeViewNewWithModel( selectivity_uncert_distribution_from_file_model)
 selectivity_uncert_distribution_from_file_treeview$setRulesHint(TRUE)
 selectivity_uncert_distribution_from_file_treeview$getSelection()$setMode("single")
 selectivity_uncert_distribution_from_file_add_columns( selectivity_uncert_distribution_from_file_treeview)  
 selectivity_uncert_distribution_from_file_sw$add(selectivity_uncert_distribution_from_file_treeview)

    
}
