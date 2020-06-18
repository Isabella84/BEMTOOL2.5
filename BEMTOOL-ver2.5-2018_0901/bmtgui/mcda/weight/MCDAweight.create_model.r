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
# ------------------------------------------------------------------------------
# create model for the tree of p production
# ------------------------------------------------------------------------------
#
MCDAweight.create_model <- function() {
#print("Creating model...")   
  # create list store
  MCDAweight.model <<- gtkListStoreNew("gchararray", "gchararray", "gchararray", "gdouble", "gboolean")  
  add.MCDAweight()
  # add items
  if (length(MCDAweight_list) != 0) { 
  for (i in 1:length(MCDAweight_list)) {
    iter <- MCDAweight.model$append()$iter
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    MCDAweight.model$set(iter,0, MCDAweight_list[[i]][1])
         MCDAweight.model$set(iter, 1, MCDAweight_list[[i]][2])
		     MCDAweight.model$set(iter,2, MCDAweight_list[[i]][3])
         MCDAweight.model$set(iter, 3, as.numeric(MCDAweight_list[[i]][4]))
       MCDAweight.model$set(iter, 4,TRUE)
  } 
  }
  #print("behav_act Model successfully created!")  
}
