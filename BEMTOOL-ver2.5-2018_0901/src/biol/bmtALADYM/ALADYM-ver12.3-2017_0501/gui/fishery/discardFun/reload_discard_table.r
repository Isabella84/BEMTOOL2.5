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
# Function to reload the values for the selectivity according to the 
# selection of the selectivity function
# ------------------------------------------------------------------------------
#
reload_EMPTY_discard_table <- function(w) {

discards_list <<- list()
discardIndex <<- 0

  dis_matrix <- data.frame(matrix(0, nrow=((length(years)*12) +1), ncol=4))
   
   heading <- c("year","month",   "L50%",  "L75%-L25%")
 
   colnames(dis_matrix) <- heading
   years_rep <- rep(years, 12)
   years_rep <- years_rep[order(years_rep)]
   years_rep <- c("", years_rep)
   months_rep <- rep(MONTHS, length(years))
   months_rep <- c("seed", months_rep)
   dis_matrix$year <- years_rep
   dis_matrix$month <- months_rep
   
    for (r in 1:nrow(dis_matrix)) { 
  dis_temp <- as.list(dis_matrix[r,]) 
  discards_list <<- c(discards_list, list(dis_temp)) 
  }

  fleet.discard <<- dis_matrix

  discards.model <<- gtkListStoreNew("gchararray", "gchararray", rep("gdouble", 2), "gboolean")  
  
 for (i in 1:length(discards_list)) {
    iter <-  discards.model$append()$iter
    discards.model$set(iter,0, discards_list[[i]]$year)
    discards.model$set(iter, 1, discards_list[[i]]$month)           
    for (np in 1:2) {
         discards.model$set(iter, np+1, as.double(discards_list[[i]][np+2]))
    }
     discards.model$set(iter,4,TRUE)
  } 

   discards.treeview$destroy()

 discards.treeview <<- gtkTreeViewNewWithModel( discards.model)
 discards.treeview$setRulesHint(TRUE)
 discards.treeview$getSelection()$setMode("single")
discards.add_columns( discards.treeview)
discards.sw$add(discards.treeview)

 
}





reload_discard_table <- function(w) {

   discards_list <<- list()
discardIndex <<- 0

  dis_matrix <- fleet.discard
  
      for (r in 1:nrow(dis_matrix)) { 
  dis_temp <- as.list(dis_matrix[r,]) 
  discards_list <<- c(discards_list, list(dis_temp)) 
  }
  

  discards.model <<- gtkListStoreNew("gchararray", "gchararray", rep("gdouble", 2), "gboolean")  
  
 for (i in 1:length(discards_list)) {
    iter <-  discards.model$append()$iter
    discards.model$set(iter,0, discards_list[[i]]$year)
    discards.model$set(iter, 1, discards_list[[i]]$month)           
    for (np in 1:2) {
         discards.model$set(iter, np+1, as.double(discards_list[[i]][np+2]))
    }
     discards.model$set(iter,4,TRUE)
  } 

   discards.treeview$destroy()

 discards.treeview <<- gtkTreeViewNewWithModel( discards.model)
 discards.treeview$setRulesHint(TRUE)
 discards.treeview$getSelection()$setMode("single")
discards.add_columns( discards.treeview)
discards.sw$add(discards.treeview)

 
}