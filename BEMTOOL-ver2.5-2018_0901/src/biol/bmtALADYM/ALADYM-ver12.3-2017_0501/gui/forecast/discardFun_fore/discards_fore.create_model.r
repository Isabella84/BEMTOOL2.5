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
# ------------------------------------------------------------------------------
# create model for the tree of discard
# ------------------------------------------------------------------------------
#
discards_fore.create_model <- function() {
#print("Creating model...")   
  # create list store
  discards_fore.model <<- gtkListStoreNew("gchararray", "gchararray", rep("gdouble", 2), "gboolean")  
  add.discards_fore()
  # add items 
 for (i in 1:length(discards_fore)) {
    iter <-  discards_fore.model$append()$iter
    discards_fore.model$set(iter,0, discards_fore[[i]]$year)
    discards_fore.model$set(iter, 1, discards_fore[[i]]$month)           
    for (np in 1:2) {
         discards_fore.model$set(iter, np+1, as.double(discards_fore[[i]][np+2]))
    }
     discards_fore.model$set(iter,4,TRUE)
  } 
  #print("Discard forecast Model successfully created!")  
}
