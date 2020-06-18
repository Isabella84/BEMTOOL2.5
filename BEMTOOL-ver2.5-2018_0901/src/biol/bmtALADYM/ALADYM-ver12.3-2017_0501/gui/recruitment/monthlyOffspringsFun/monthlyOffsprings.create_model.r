# BEMTOOL - Bio-Economic Model TOOLs - version 2.0
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





monthlyOffsprings.create_model <- function() {
# print("Creating model...")   
  # create list store
 # monthlyOffsprings <<- list()
# monthlyOffspringsIndex <<- 0

  monthlyOffsprings.model <<- gtkListStoreNew(rep("gdouble", 12), "gboolean")  
  add.monthlyOffsprings()
  # add items 
  iter <- monthlyOffsprings.model$append()$iter
  for (i in c(1:length(MONTHS))) {
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    monthlyOffsprings.model$set(iter,(i-1), as.double(monthlyOffsprings[[i]][1]))
    #print(i)
      }   
   monthlyOffsprings.model$set(iter, i,TRUE)
}
