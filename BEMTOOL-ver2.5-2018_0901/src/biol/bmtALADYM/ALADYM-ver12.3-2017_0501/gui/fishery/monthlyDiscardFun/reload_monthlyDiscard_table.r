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
# ------------------------------------------------------------------------------
# Function to reload the values for the production according to the 
# seed value
# ------------------------------------------------------------------------------
#
reload_EMPTY_monthlyDiscard_table<- function(w) {

  monthlyDiscard_list <<- list()
  monthlyDiscardIndex <<- 0

   monthlyDiscard_matrix <- data.frame(matrix(0, nrow=length(years), ncol=13))
   colnames(monthlyDiscard_matrix) <- c("year",MONTHS)
     monthlyDiscard_matrix$year <- years
   for (r in 1:nrow( monthlyDiscard_matrix)) { 
   monthlyDiscard_temp <- as.list( monthlyDiscard_matrix[r,]) 
  monthlyDiscard_list <<- c(monthlyDiscard_list, list( monthlyDiscard_temp)) 
  }
  
    fleet.monthlyDiscard <<-  monthlyDiscard_matrix
  
 monthlyDiscard.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length( monthlyDiscard_list)) {
    iter <-  monthlyDiscard.model$append()$iter
     monthlyDiscard.model$set(iter,0,  monthlyDiscard_list[[i]]$year)
    for (e in 1:length(MONTHS)) {
          monthlyDiscard.model$set(iter, e, as.numeric( monthlyDiscard_list[[i]][e+1]))
    }
        monthlyDiscard.model$set(iter, 13,TRUE)
  } 
 
     monthlyDiscard.treeview$destroy()
    
   monthlyDiscard.treeview <<- gtkTreeViewNewWithModel(  monthlyDiscard.model)
  monthlyDiscard.treeview$setRulesHint(TRUE)
  monthlyDiscard.treeview$getSelection()$setMode("single")
 monthlyDiscard.add_columns(  monthlyDiscard.treeview)
 monthlyDiscard.sw$add( monthlyDiscard.treeview)
   
}






reload_monthlyDiscard_table<- function(w) {

  monthlyDiscard_list <<- list()
  monthlyDiscardIndex <<- 0

   monthlyDiscard_matrix <-  fleet.monthlyDiscard
   colnames(monthlyDiscard_matrix) <- c("year",MONTHS)
     monthlyDiscard_matrix$year <- years
   for (r in 1:nrow( monthlyDiscard_matrix)) { 
   monthlyDiscard_temp <- as.list( monthlyDiscard_matrix[r,]) 
  monthlyDiscard_list <<- c(monthlyDiscard_list, list( monthlyDiscard_temp)) 
  }
  
 monthlyDiscard.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length( monthlyDiscard_list)) {
    iter <-  monthlyDiscard.model$append()$iter
     monthlyDiscard.model$set(iter,0,  monthlyDiscard_list[[i]]$year)
    for (e in 1:length(MONTHS)) {
          monthlyDiscard.model$set(iter, e, as.numeric( monthlyDiscard_list[[i]][e+1]))
    }
        monthlyDiscard.model$set(iter, 13,TRUE)
  } 
 
     monthlyDiscard.treeview$destroy()
    
   monthlyDiscard.treeview <<- gtkTreeViewNewWithModel(  monthlyDiscard.model)
  monthlyDiscard.treeview$setRulesHint(TRUE)
  monthlyDiscard.treeview$getSelection()$setMode("single")
 monthlyDiscard.add_columns(  monthlyDiscard.treeview)
 monthlyDiscard.sw$add( monthlyDiscard.treeview)
 
}