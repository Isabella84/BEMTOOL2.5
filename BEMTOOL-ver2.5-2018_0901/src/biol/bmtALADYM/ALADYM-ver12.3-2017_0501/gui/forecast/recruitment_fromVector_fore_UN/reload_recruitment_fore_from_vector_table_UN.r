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
reload_EMPTY_recruitment_fore_from_vector_table_UN <- function(w) {
  recruitments_fore_from_vector_UN <<- list()
recruitments_fore_from_vector_index_UN <<- 0

   zero_matrix <- data.frame(matrix(-1, nrow=CI_NB_RUNS_FORE, ncol=4 ))
     colnames(zero_matrix) <- c("run_N", "a", "b", "c") 
   for (r in 1:nrow(zero_matrix)) { 
  sr_temp <- as.list(zero_matrix[r,]) 
  recruitments_fore_from_vector_UN <<- c(recruitments_fore_from_vector_UN, list(sr_temp)) 
  }
  
  recruitments_fore_from_vector.vector_UN <<- zero_matrix
 recruitments_fore_from_vector.model_UN <<- gtkListStoreNew("gchararray", rep("gdouble", 3), "gboolean")
 
  for (i in 1:length(recruitments_fore_from_vector_UN)) {
    iter <-  recruitments_fore_from_vector.model_UN$append()$iter
     recruitments_fore_from_vector.model_UN$set(iter,0, recruitments_fore_from_vector_UN[[i]]$run_N)
	 
     recruitments_fore_from_vector.model_UN$set(iter, 1, as.numeric(as.character(recruitments_fore_from_vector_UN[[i]]$a)) )
     recruitments_fore_from_vector.model_UN$set(iter, 2, as.numeric(as.character(recruitments_fore_from_vector_UN[[i]]$b)) )
     recruitments_fore_from_vector.model_UN$set(iter, 3, as.numeric(as.character(recruitments_fore_from_vector_UN[[i]]$c)) )
     recruitments_fore_from_vector.model_UN$set(iter, 4,TRUE)
  } 

  recruitments_fore_from_vector.treeview_UN$destroy()
 recruitments_fore_from_vector.treeview_UN <<- gtkTreeViewNewWithModel( recruitments_fore_from_vector.model_UN)
 recruitments_fore_from_vector.treeview_UN$setRulesHint(TRUE)
 recruitments_fore_from_vector.treeview_UN$getSelection()$setMode("single")
recruitments_fore_from_vector.add_columns_UN( recruitments_fore_from_vector.treeview_UN)  
recruitments_fore_from_vector.sw_UN$add(recruitments_fore_from_vector.treeview_UN)

    
}



reload_recruitment_fore_from_vector_table_UN <- function(w) {
  recruitments_fore_from_vector_UN <<- list()
recruitments_fore_from_vector_index_UN <<- 0

       this_matrix <- table_recruitments_fore_from_vector_UN  
   for (r in 1:nrow(this_matrix)) { 
  sr_temp <- as.list(this_matrix[r,]) 
  recruitments_fore_from_vector_UN <<- c(recruitments_fore_from_vector_UN, list(sr_temp)) 
  }
  
   recruitments_fore_from_vector.model_UN <<- gtkListStoreNew("gchararray", rep("gdouble", 3), "gboolean")
 
  for (i in 1:length(recruitments_fore_from_vector_UN)) {
    iter <-  recruitments_fore_from_vector.model_UN$append()$iter
     recruitments_fore_from_vector.model_UN$set(iter,0, recruitments_fore_from_vector_UN[[i]]$run_N)
	 
     recruitments_fore_from_vector.model_UN$set(iter, 1, as.numeric(as.character(recruitments_fore_from_vector_UN[[i]]$a)) )
     recruitments_fore_from_vector.model_UN$set(iter, 2, as.numeric(as.character(recruitments_fore_from_vector_UN[[i]]$b)) )
     recruitments_fore_from_vector.model_UN$set(iter, 3, as.numeric(as.character(recruitments_fore_from_vector_UN[[i]]$c)) )
     recruitments_fore_from_vector.model_UN$set(iter, 4,TRUE)
  } 

  recruitments_fore_from_vector.treeview_UN$destroy()
 recruitments_fore_from_vector.treeview_UN <<- gtkTreeViewNewWithModel( recruitments_fore_from_vector.model_UN)
 recruitments_fore_from_vector.treeview_UN$setRulesHint(TRUE)
 recruitments_fore_from_vector.treeview_UN$getSelection()$setMode("single")
recruitments_fore_from_vector.add_columns_UN( recruitments_fore_from_vector.treeview_UN)  
recruitments_fore_from_vector.sw_UN$add(recruitments_fore_from_vector.treeview_UN)

    
}
