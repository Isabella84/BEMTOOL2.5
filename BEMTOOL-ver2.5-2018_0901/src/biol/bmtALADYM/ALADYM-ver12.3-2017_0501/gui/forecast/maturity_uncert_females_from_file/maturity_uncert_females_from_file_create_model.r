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
maturity_uncert_females_from_file_create_model <- function() {
#print("Creating model...")   
  # create list store   # gtkListStoreNew("gchararray", rep("gdouble", 3), "gboolean")  
  maturity_uncert_females_from_file_model <<- gtkListStoreNew("gchararray", "gdouble", "gdouble", "gboolean")
  add_maturity_uncert_females_from_file()
  # add items 
  for (i in 1:length(maturity_uncert_females_from_file_list)) {
    iter <- maturity_uncert_females_from_file_model$append()$iter
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    maturity_uncert_females_from_file_model$set(iter,0, maturity_uncert_females_from_file_list[[i]]$run_N)
     maturity_uncert_females_from_file_model$set(iter, 1, as.numeric(as.character(maturity_uncert_females_from_file_list[[i]]$L50)) )
	  maturity_uncert_females_from_file_model$set(iter, 2, as.numeric(as.character(maturity_uncert_females_from_file_list[[i]]$L75L25)) )
    maturity_uncert_females_from_file_model$set(iter, 3,TRUE)
  } 
  #print("Recruitment Model successfully created!")  
}

