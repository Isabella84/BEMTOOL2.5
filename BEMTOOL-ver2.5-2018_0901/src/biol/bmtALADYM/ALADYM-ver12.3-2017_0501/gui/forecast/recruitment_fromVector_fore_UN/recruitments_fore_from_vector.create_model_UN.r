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
recruitments_fore_from_vector.create_model_UN <- function() {
#print("Creating model...")   
  # create list store   # gtkListStoreNew("gchararray", rep("gdouble", 3), "gboolean")  
  recruitments_fore_from_vector.model_UN <<- gtkListStoreNew("gchararray", rep("gdouble", 3), "gboolean")
  add.recruitments_fore_from_vector_UN()
  # add items 
  for (i in 1:length(recruitments_fore_from_vector_UN)) {
    iter <- recruitments_fore_from_vector.model_UN$append()$iter
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    recruitments_fore_from_vector.model_UN$set(iter,0, recruitments_fore_from_vector_UN[[i]]$run_N)
     recruitments_fore_from_vector.model_UN$set(iter, 1, as.numeric(as.character(recruitments_fore_from_vector_UN[[i]]$a)) )
     recruitments_fore_from_vector.model_UN$set(iter, 2, as.numeric(as.character(recruitments_fore_from_vector_UN[[i]]$b)) )
     recruitments_fore_from_vector.model_UN$set(iter, 3, as.numeric(as.character(recruitments_fore_from_vector_UN[[i]]$c)) )
     recruitments_fore_from_vector.model_UN$set(iter, 4,TRUE)
  } 
  #print("Recruitment Model successfully created!")  
}

