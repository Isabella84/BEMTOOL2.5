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
bmt_behav_dyn.create_model <- function() {
#print("Creating model...")   
  # create list store
  bmt_behav_dyn.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(BMT_FLEETSEGMENTS)), "gboolean")  
  bmt_add.behav_dyn()
  # add items
  if (length(bmt_behav_dyn_list) != 0) { 
  for (i in 1:length(bmt_behav_dyn_list)) {
    iter <- bmt_behav_dyn.model$append()$iter
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    bmt_behav_dyn.model$set(iter,0, bmt_behav_dyn_list[[i]][1])
    for (e in 1:length(BMT_FLEETSEGMENTS)) {
   # print(paste("in model:", years[nc]) )
         bmt_behav_dyn.model$set(iter, e, as.numeric(bmt_behav_dyn_list[[i]][e+1]))
    }
       bmt_behav_dyn.model$set(iter, (length(BMT_FLEETSEGMENTS)+1),TRUE)
  } 
  }
  #print("behav_dyn Model successfully created!")  
}
