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
recruitments_fore_from_vector.create_model <- function() {
#print("Creating model...")   
  # create list store
  recruitments_fore_from_vector.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  add.recruitments_fore_from_vector()
  # add items 
  for (i in 1:length(recruitments_fore_from_vector)) {
    iter <- recruitments_fore_from_vector.model$append()$iter
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    recruitments_fore_from_vector.model$set(iter,0, recruitments_fore_from_vector[[i]]$year)
    for (e in 1:length(MONTHS)) {
   # print(paste("in model:", years[nc]) )
         recruitments_fore_from_vector.model$set(iter, e, as.numeric(recruitments_fore_from_vector[[i]][e+1]))
    }
       recruitments_fore_from_vector.model$set(iter, 13,TRUE)
  } 
  #print("Recruitment Model successfully created!")  
}

