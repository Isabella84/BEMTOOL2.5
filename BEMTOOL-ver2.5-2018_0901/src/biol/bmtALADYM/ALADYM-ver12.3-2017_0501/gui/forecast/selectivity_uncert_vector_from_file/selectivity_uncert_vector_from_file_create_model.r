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
# create model for the tree of stock-recruitment_fore_from_vector
# ------------------------------------------------------------------------------
#
selectivity_uncert_vector_from_file_create_model <- function() {
#print("Creating model...")   
  # create list store   # gtkListStoreNew("gchararray", rep("gdouble", 3), "gboolean")  
  selectivity_uncert_vector_from_file_model <<- gtkListStoreNew("gchararray", rep("gdouble", 6), "gchararray", "gboolean")
  add_selectivity_uncert_vector_from_file()
  # add items 
  for (i in 1:length(selectivity_uncert_vector_from_file_list)) {
    iter <- selectivity_uncert_vector_from_file_model$append()$iter
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
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
  #print("Recruitment Model successfully created!")  
}

