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
bmt_labour_sorting.create_model <- function() {
#print("Creating model...")   
  # create list store
  bmt_labour_sorting.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(BMT_FLEETSEGMENTS)), "gboolean")  
  bmt_add.labour_sorting()
  # add items
  if (length(bmt_labour_sorting_list) != 0) { 
  for (i in 1:length(bmt_labour_sorting_list)) {
    iter <- bmt_labour_sorting.model$append()$iter
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    bmt_labour_sorting.model$set(iter,0, bmt_labour_sorting_list[[i]][1])
    for (e in 1:length(BMT_FLEETSEGMENTS)) {
   # print(paste("in model:", years[nc]) )
         bmt_labour_sorting.model$set(iter, e, as.numeric(bmt_labour_sorting_list[[i]][e+1]))
    }
       bmt_labour_sorting.model$set(iter, (length(BMT_FLEETSEGMENTS)+1),TRUE)
  } 
  }
  #print("labour_sorting Model successfully created!")  
}
