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
# create model for the tree of natural mortality (MALES)
# ------------------------------------------------------------------------------
#
Mvector_M.create_model <- function() {
#print("Creating model...")   
  # create list store
  Mvector_M.model <<- gtkListStoreNew("gchararray", "gdouble", "gboolean")  
  add.Mvector_M()
  # add items 
 for (i in 1:length(Mvector_M)) {
    iter <-  Mvector_M.model$append()$iter
    Mvector_M.model$set(iter, 0, Mvector_M[[i]]$age_month)            
    Mvector_M.model$set(iter, 1, as.double(Mvector_M[[i]]$M))
     Mvector_M.model$set(iter,2,TRUE)
  } 
 # print("Natural mortality (MALES) model successfully created!")  
}