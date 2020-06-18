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
bmt_REVENUES.create_model <- function() {
#print("Creating model...")   
  # create list store
  bmt_REVENUES.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(BMT_SPECIES)), "gboolean")  
  bmt_add.REVENUES()
  # add items 
  if (length(BMT_SPECIES) != 0) {
  for (i in 1:length(bmt_REVENUES_list)) {
    iter <- bmt_REVENUES.model$append()$iter
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    bmt_REVENUES.model$set(iter,0, bmt_REVENUES_list[[i]]$year)
    for (e in 1:length(BMT_SPECIES)) {
   # print(paste("in model:", years[nc]) )
         bmt_REVENUES.model$set(iter, e, as.numeric(bmt_REVENUES_list[[i]][e+1]))
    }
       bmt_REVENUES.model$set(iter, (length(BMT_SPECIES)+1),TRUE)
  } 
  }
  #print("REVENUES Model successfully created!")  
}
