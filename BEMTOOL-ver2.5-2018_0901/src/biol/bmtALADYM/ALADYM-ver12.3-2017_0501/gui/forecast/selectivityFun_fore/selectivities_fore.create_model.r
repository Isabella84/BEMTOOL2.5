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
# create model for the tree of selectivity_fore
# ------------------------------------------------------------------------------
#
selectivities_fore.create_model <- function() {
#print("Creating model...")   
  # create list store
  selectivities_fore.model <<- gtkListStoreNew("gchararray", "gchararray", rep("gdouble", 6), "gboolean")  
  add.selectivities_fore()
  # add items 
 for (i in 1:length(selectivities_fore)) {
    iter <-  selectivities_fore.model$append()$iter
    selectivities_fore.model$set(iter,0, selectivities_fore[[i]]$year)
    selectivities_fore.model$set(iter,1, selectivities_fore[[i]]$month) 
        selectivities_fore.model$set(iter,2, selectivities_fore[[i]]$param1)  
            selectivities_fore.model$set(iter,3, selectivities_fore[[i]]$param2)  
                selectivities_fore.model$set(iter,4, selectivities_fore[[i]]$param3)  
                    selectivities_fore.model$set(iter,5, selectivities_fore[[i]]$param4)  
                        selectivities_fore.model$set(iter,6, selectivities_fore[[i]]$param5)  
                            selectivities_fore.model$set(iter,7, selectivities_fore[[i]]$sel_type)                             
     selectivities_fore.model$set(iter,8,TRUE)
  } 
  #print("Selectivities Model successfully created!")  
}