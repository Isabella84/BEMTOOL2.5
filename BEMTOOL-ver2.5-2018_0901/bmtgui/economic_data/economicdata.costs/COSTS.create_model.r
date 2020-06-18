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
#
# ------------------------------------------------------------------------------
# create model for the tree of p production
# ------------------------------------------------------------------------------
#
bmt_COSTS.create_model <- function() {
#print("Creating model...")   
  # create list store
  bmt_COSTS.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(COSTS_vector)), "gboolean")  
  bmt_add.COSTS()
  # add items 
  for (i in 1:length(bmt_COSTS_list)) {
    iter <- bmt_COSTS.model$append()$iter
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    bmt_COSTS.model$set(iter,0, bmt_COSTS_list[[i]]$year)
    for (e in 1:length(COSTS_vector)) {
   # print(paste("in model:", years[nc]) )
         bmt_COSTS.model$set(iter, e, as.numeric(bmt_COSTS_list[[i]][e+1]))
    }
       bmt_COSTS.model$set(iter, (length(COSTS_vector)+1),TRUE)
  } 
  #print("COSTS Model successfully created!")  
}
