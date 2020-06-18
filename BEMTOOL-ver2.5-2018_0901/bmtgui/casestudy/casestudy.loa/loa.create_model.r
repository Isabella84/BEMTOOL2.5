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
#
# ------------------------------------------------------------------------------
# create model for the tree of selectivity
# ------------------------------------------------------------------------------
#
loa.create_model <- function() {
#print("Creating model...")   
  # create list store
  loa.model <<- gtkListStoreNew("gchararray", "gboolean")  
  add.loa()
  # add items 
 for (i in 1:length(loa_list)) {
    iter <-  loa.model$append()$iter
    loa.model$set(iter,0, loa_list[[i]])
   #discards.model$set(iter,1, discards_list[[i]]$month)           
        # loa.model$set(iter, 2, loa_list[[i]])
     loa.model$set(iter,1,TRUE)
  } 
 # print("Discard Model successfully created!")  
}
