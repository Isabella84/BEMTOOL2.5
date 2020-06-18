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
# create model for the tree of selectivity
# ------------------------------------------------------------------------------
#
selectivities.create_model <- function() {
#print("Creating model...")   
  # create list store
  selectivities.model <<- gtkListStoreNew("gchararray", "gchararray", rep("gdouble", 6), "gboolean")  
  add.selectivities()
  # add items 
 for (i in 1:length(selectivities)) {
    iter <-  selectivities.model$append()$iter
    selectivities.model$set(iter,0, selectivities[[i]]$year)
    selectivities.model$set(iter,1, selectivities[[i]]$month) 
        selectivities.model$set(iter,2, ifelse(!is.na(selectivities[[i]]$param1), selectivities[[i]]$param1, -1))  
            selectivities.model$set(iter,3, ifelse(!is.na(selectivities[[i]]$param2), selectivities[[i]]$param2, -1))  
                selectivities.model$set(iter,4, ifelse(!is.na(selectivities[[i]]$param3), selectivities[[i]]$param3, -1))  
                    selectivities.model$set(iter,5, ifelse(!is.na(selectivities[[i]]$param4), selectivities[[i]]$param4, -1))  
                        selectivities.model$set(iter,6, ifelse(!is.na(selectivities[[i]]$param5), selectivities[[i]]$param5, -1))  
                            selectivities.model$set(iter,7, selectivities[[i]]$sel_type)                             
     selectivities.model$set(iter,8,TRUE)
  } 
  #print("Selectivities Model successfully created!")  
}