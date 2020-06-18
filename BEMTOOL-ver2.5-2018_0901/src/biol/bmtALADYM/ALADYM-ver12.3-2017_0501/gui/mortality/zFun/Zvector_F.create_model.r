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
# create model for the tree of total mortality (FEMALES)
# ------------------------------------------------------------------------------
#
Zvector_F.create_model <- function() {
#print("Creating model...")   
  # create list store
  Zvector_F.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  add.Zvector_F()
  # add items 
  for (i in 1:length(Zvector_F)) {
    iter <- Zvector_F.model$append()$iter
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    Zvector_F.model$set(iter,0, Zvector_F[[i]]$year)
    for (e in 1:length(MONTHS)) {
   # print(paste("in model:", years[nc]) )
         Zvector_F.model$set(iter, e, as.numeric(Zvector_F[[i]][e+1]))
    }
       Zvector_F.model$set(iter, 13,TRUE)
  } 
 # print("Total mortality (FEMALES) Model successfully created!")  
}