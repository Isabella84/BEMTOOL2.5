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
reload_EMPTY_maturity_uncert_females_from_file_table <- function(w) {
  maturity_uncert_females_from_file_list <<- list()
maturity_uncert_females_from_file_index <<- 0

   zero_matrix <- data.frame(matrix(-1, nrow=CI_NB_RUNS_FORE, ncol=3 ))
     colnames(zero_matrix) <- c("run_N", "L50", "L75L25") 
   for (r in 1:nrow(zero_matrix)) { 
  sr_temp <- as.list(zero_matrix[r,]) 
  maturity_uncert_females_from_file_list <<- c(maturity_uncert_females_from_file_list, list(sr_temp)) 
  }
  
  maturity_uncert_females_from_file_matrix <<- zero_matrix
 maturity_uncert_females_from_file_model <<- gtkListStoreNew("gchararray", "gdouble","gdouble", "gboolean")
 
  for (i in 1:length(maturity_uncert_females_from_file_list)) {
    iter <-  maturity_uncert_females_from_file_model$append()$iter
     maturity_uncert_females_from_file_model$set(iter,0, maturity_uncert_females_from_file_list[[i]]$run_N) 
     maturity_uncert_females_from_file_model$set(iter, 1, as.numeric(as.character(maturity_uncert_females_from_file_list[[i]]$L50)) )
	    maturity_uncert_females_from_file_model$set(iter, 2, as.numeric(as.character(maturity_uncert_females_from_file_list[[i]]$L75L25)) )
     maturity_uncert_females_from_file_model$set(iter, 3,TRUE)
  } 

  maturity_uncert_females_from_file_treeview$destroy()
 maturity_uncert_females_from_file_treeview <<- gtkTreeViewNewWithModel( maturity_uncert_females_from_file_model)
 maturity_uncert_females_from_file_treeview$setRulesHint(TRUE)
 maturity_uncert_females_from_file_treeview$getSelection()$setMode("single")
 
 maturity_uncert_females_from_file_add_columns(  maturity_uncert_females_from_file_treeview)  
 maturity_uncert_females_from_file_sw$add( maturity_uncert_females_from_file_treeview)
   
}



reload_maturity_uncert_females_from_file_table <- function(w) {
  maturity_uncert_females_from_file_list <<- list()
 maturity_uncert_females_from_file_index <<- 0

       this_matrix <-  maturity_uncert_females_from_file_matrix  
   for (r in 1:nrow(this_matrix)) { 
  sr_temp <- as.list(this_matrix[r,]) 
  maturity_uncert_females_from_file_list <<- c(maturity_uncert_females_from_file_list, list(sr_temp)) 
  }
  
   maturity_uncert_females_from_file_model <<- gtkListStoreNew("gchararray", "gdouble" ,"gdouble" ,"gboolean")
 
  for (i in 1:length(maturity_uncert_females_from_file_list)) {
    iter <-  maturity_uncert_females_from_file_model$append()$iter
     maturity_uncert_females_from_file_model$set(iter,0, maturity_uncert_females_from_file_list[[i]]$run_N) 
     maturity_uncert_females_from_file_model$set(iter, 1, as.numeric(as.character(maturity_uncert_females_from_file_list[[i]]$L50)) )
	 maturity_uncert_females_from_file_model$set(iter, 2, as.numeric(as.character(maturity_uncert_females_from_file_list[[i]]$L75L25)) )
     maturity_uncert_females_from_file_model$set(iter, 3,TRUE)
  } 

  maturity_uncert_females_from_file_treeview$destroy()
 maturity_uncert_females_from_file_treeview <<- gtkTreeViewNewWithModel( maturity_uncert_females_from_file_model)
 maturity_uncert_females_from_file_treeview$setRulesHint(TRUE)
 maturity_uncert_females_from_file_treeview$getSelection()$setMode("single")
 maturity_uncert_females_from_file_add_columns( maturity_uncert_females_from_file_treeview)  
 maturity_uncert_females_from_file_sw$add(maturity_uncert_females_from_file_treeview)

    
}
