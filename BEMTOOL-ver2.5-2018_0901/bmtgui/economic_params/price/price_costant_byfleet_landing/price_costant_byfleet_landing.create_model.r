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
price_costant_byfleet_landing.create_model <- function() {
#print("Creating model...")   
  # create list store
  price_costant_byfleet_landing.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(BMT_FLEETSEGMENTS)), "gboolean")  
  add.price_costant_byfleet_landing()
  # add items
  if (length(price_costant_byfleet_landing_list) != 0) { 
  for (i in 1:length(price_costant_byfleet_landing_list)) {
    iter <- price_costant_byfleet_landing.model$append()$iter
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    price_costant_byfleet_landing.model$set(iter,0, price_costant_byfleet_landing_list[[i]]$Species)
    for (e in 1:length(BMT_FLEETSEGMENTS)) {
   # print(paste("in model:", years[nc]) )
         price_costant_byfleet_landing.model$set(iter, e, as.numeric(price_costant_byfleet_landing_list[[i]][e+1]))
    }
       price_costant_byfleet_landing.model$set(iter, (length(BMT_FLEETSEGMENTS)+1),TRUE)
  } 
  }
  #print("price_elast_landing_byfleet Model successfully created!")  
}
