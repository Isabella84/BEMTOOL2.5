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
interaction.create_model <- function() {
#print("Creating model...")   
  # create list store
  interaction.model <<- gtkListStoreNew("gchararray", "gchararray", "gboolean")  
  add.interaction()
  # add items 
  if (!is.null(interactions) ) {
   if (nrow(interactions) != 0) {
 for (i in 1:length(interaction_list)) {
    iter <-  interaction.model$append()$iter
    interaction.model$set(iter,0, interaction_list[[i]]$Species)
    interaction.model$set(iter,1, interaction_list[[i]]$Fleet_Segment)           
        # interaction.model$set(iter, 2, interaction_list[[i]])     
     interaction.model$set(iter,2,TRUE)
  } 
}  
  
  }
 # print("Discard Model successfully created!")  
}
