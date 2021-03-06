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
FishingMvector_F_overall.create_model <- function() {

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_F_lifespan))  
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[2,1])      
} 
first_age_fem <- 0

    n_ages <- n_ages - trunc(Tr/12)
    first_age_fem <- trunc(Tr/12)

#print("Creating model...")   
  # create list store
  FishingMvector_F_overall.model <<- gtkListStoreNew("gchararray",  rep("gdouble", n_ages), "gboolean")  
  add.FishingMvector_F_overall()
  # add items 
  for (i in 1:length(FishingMvector_F_overall_list)) {
    iter <- FishingMvector_F_overall.model$append()$iter
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    FishingMvector_F_overall.model$set(iter,0, FishingMvector_F_overall_list[[i]]$year)
    for (e in 1:(n_ages) ) {
   # print(paste("in model:", years[nc]) )
         FishingMvector_F_overall.model$set(iter, e, as.numeric(FishingMvector_F_overall_list[[i]][e+1]))
    }
       FishingMvector_F_overall.model$set(iter, (n_ages+1),TRUE)
  } 
 # print("Total mortality (MALES) Model successfully created!")  
}

