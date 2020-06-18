# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
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
# create model for the tree of p production
# ------------------------------------------------------------------------------
#
MCDAutility.create_model <- function() {
#print("Creating model...")   
  # create list store
  MCDAutility.model <<- gtkListStoreNew("gchararray", "gdouble", "gboolean")  
  add.MCDAutility()
  # add items
  if (length(MCDAutility_list) != 0) { 
  for (i in 1:length(MCDAutility_list)) {
    iter <- MCDAutility.model$append()$iter
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    MCDAutility.model$set(iter,0, MCDAutility_list[[i]][1])
         MCDAutility.model$set(iter, 1, as.numeric(as.character(MCDAutility_list[[i]][2])))
       MCDAutility.model$set(iter, 2,TRUE)
  } 
  }
  #print("behav_act Model successfully created!")  
}
