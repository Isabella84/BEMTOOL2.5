# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.






#
# ------------------------------------------------------------------------------
# create model for the tree of total mortality
# ------------------------------------------------------------------------------
#
SelectivityAge_M_fore.create_model <- function() {
#print("Creating model...")

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
 n_ages <- as.numeric(gtkEntryGetText(entryVBF_M_lifespan))  
} else {
   n_ages <- as.numeric(new_aldPopulation@lifespan[1,1])      
} 

first_age_mal <- 0

    n_ages <- n_ages - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)
 
  # create list store
  SelectivityAge_M_fore.model <<- gtkListStoreNew( rep("gdouble", n_ages+1), "gboolean") 
  add.SelectivityAge_M_fore()
  # add items 
  for (i in 1:length(SelectivityAge_M_fore_list)) {
    iter <- SelectivityAge_M_fore.model$append()$iter
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
   SelectivityAge_M_fore.model$set(iter,0, SelectivityAge_M_fore_list[[i]]$Year)
    for (e in 1:(n_ages)) {
   # print(paste("in model:", years[nc]) )
         SelectivityAge_M_fore.model$set(iter, e, as.numeric(SelectivityAge_M_fore_list[[i]][e+1]))
    }
       SelectivityAge_M_fore.model$set(iter, (n_ages+1),TRUE)
  } 
 # print("Total mortality (MALES) Model successfully created!")  
}

