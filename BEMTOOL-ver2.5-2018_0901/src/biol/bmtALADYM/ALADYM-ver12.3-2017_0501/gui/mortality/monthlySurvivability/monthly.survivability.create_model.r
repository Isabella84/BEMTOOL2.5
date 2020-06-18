# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




monthly.survivability.create_model <- function() {
# print("Creating model...")   
  # create list store
 # monthly.survivability <<- list()
# monthly.survivabilityIndex <<- 0

  monthly.survivability.model <<- gtkListStoreNew(rep("gdouble", 12), "gboolean")  
  add.monthly.survivability()
  # add items 
  iter <- monthly.survivability.model$append()$iter
  for (i in c(1:length(MONTHS))) {
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    monthly.survivability.model$set(iter,(i-1), as.double(monthly.survivability[[i]][1]))
    #print(i)
      }   
   monthly.survivability.model$set(iter, i,TRUE)
}
