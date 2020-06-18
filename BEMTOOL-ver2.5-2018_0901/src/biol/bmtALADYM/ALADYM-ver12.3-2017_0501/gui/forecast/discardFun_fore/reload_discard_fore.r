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
# Function to reload the values for the production according to the 
# seed value
# ------------------------------------------------------------------------------
#
reload_EMPTY_discard_fore<- function(w) {
                            # fleet.GT_fore
discards_fore <<- list()
discards_foreIndex <<- 0

     dis_matrix <- data.frame(matrix(0, nrow=((length(years_forecast)*12)), ncol=4))

   heading <- c("year","month",   "L50%",  "L75%-L25%")

   colnames(dis_matrix) <- heading
   years_rep <- rep(years_forecast, 12)
   years_rep <- years_rep[order(years_rep)]
   #years_rep <- c("", years_rep)
   months_rep <- rep(MONTHS, length(years_forecast))
   #months_rep <- c("seed", months_rep)
   dis_matrix$year <- years_rep
   dis_matrix$month <- months_rep
  # print(dis_matrix)
   for (r in 1:nrow(dis_matrix)) { 
  dis_temp <- as.list(dis_matrix[r,]) 
  discards_fore <<- c(discards_fore, list(dis_temp)) 
  }

    fleet.discard_fore <<- dis_matrix    
  
 discards_fore.model <<- gtkListStoreNew("gchararray", "gchararray", rep("gdouble", 2), "gboolean")  
  for (i in 1:length(discards_fore)) {
    iter <-  discards_fore.model$append()$iter
     discards_fore.model$set(iter,0, discards_fore[[i]]$year)
    discards_fore.model$set(iter, 1, discards_fore[[i]]$month)           
   for (e in 1:2) {
        discards_fore.model$set(iter, e+1, as.double(discards_fore[[i]][e+2]))   
    }
     discards_fore.model$set(iter,4,TRUE)
  } 

  discards_fore.treeview$destroy()
 discards_fore.treeview <<- gtkTreeViewNewWithModel( discards_fore.model)
 discards_fore.treeview$setRulesHint(TRUE)
 discards_fore.treeview$getSelection()$setMode("single")
discards_fore.add_columns( discards_fore.treeview)
discards_fore.sw$add(discards_fore.treeview)
    
}




reload_discard_fore<- function(w) {
                            # fleet.GT_fore
discards_fore <<- list()
discards_foreIndex <<- 0

     dis_matrix <-   fleet.discard_fore
#		 data.frame(matrix(0, nrow=((length(years_forecast)*12)), ncol=4))
#
#   heading <- c("year","month",   "L50%",  "L75%-L25%")
#
#   colnames(dis_matrix) <- heading
#   years_rep <- rep(years_forecast, 12)
#   years_rep <- years_rep[order(years_rep)]
#   #years_rep <- c("", years_rep)
#   months_rep <- rep(MONTHS, length(years_forecast))
#   #months_rep <- c("seed", months_rep)
#   dis_matrix$year <- years_rep
#   dis_matrix$month <- months_rep
  # print(dis_matrix)
   for (r in 1:nrow(dis_matrix)) { 
  dis_temp <- as.list(dis_matrix[r,]) 
  discards_fore <<- c(discards_fore, list(dis_temp)) 
  }

    #fleet.discard_fore <<- dis_matrix    
  
 discards_fore.model <<- gtkListStoreNew("gchararray", "gchararray", rep("gdouble", 2), "gboolean")  
  for (i in 1:length(discards_fore)) {
    iter <-  discards_fore.model$append()$iter
     discards_fore.model$set(iter,0, discards_fore[[i]]$year)
    discards_fore.model$set(iter, 1, discards_fore[[i]]$month)           
   for (e in 1:2) {
        discards_fore.model$set(iter, e+1, as.double(discards_fore[[i]][e+2]))   
    }
     discards_fore.model$set(iter,4,TRUE)
  } 

  discards_fore.treeview$destroy()
 discards_fore.treeview <<- gtkTreeViewNewWithModel( discards_fore.model)
 discards_fore.treeview$setRulesHint(TRUE)
 discards_fore.treeview$getSelection()$setMode("single")
discards_fore.add_columns( discards_fore.treeview)
discards_fore.sw$add(discards_fore.treeview)
    
}
