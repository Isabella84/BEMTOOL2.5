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
# ------------------------------------------------------------------------------
# Function to reload the values for the production according to the 
# seed value
# ------------------------------------------------------------------------------
#
reload_EMPTY_production_table<- function(w) {

  productions <<- list()
  productionsIndex <<- 0

   productions_matrix <- data.frame(matrix(0, nrow=length(years), ncol=13))
   colnames(productions_matrix) <- c("year",MONTHS)
     productions_matrix$year <- years
   for (r in 1:nrow(productions_matrix)) { 
  productions_temp <- as.list(productions_matrix[r,]) 
  productions <<- c(productions, list(productions_temp)) 
  }
  
    fleet.production <<- productions_matrix
  
productions.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length(productions)) {
    iter <- productions.model$append()$iter
    productions.model$set(iter,0, productions[[i]]$year)
    for (e in 1:length(MONTHS)) {
         productions.model$set(iter, e, as.numeric(productions[[i]][e+1]))
    }
       productions.model$set(iter, 13,TRUE)
  } 
 
    productions.treeview$destroy()
    
  productions.treeview <<- gtkTreeViewNewWithModel( productions.model)
 productions.treeview$setRulesHint(TRUE)
 productions.treeview$getSelection()$setMode("single")
productions.add_columns( productions.treeview)
productions.sw$add(productions.treeview)
   
}






reload_production_table<- function(w) {

  productions <<- list()
  productionsIndex <<- 0

   productions_matrix <- fleet.production
   
   for (r in 1:nrow(productions_matrix)) { 
  productions_temp <- as.list(productions_matrix[r,]) 
  productions <<- c(productions, list(productions_temp)) 
  }
  
productions.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length(productions)) {
    iter <- productions.model$append()$iter
    productions.model$set(iter,0, productions[[i]]$year)
    for (e in 1:length(MONTHS)) {
         productions.model$set(iter, e, as.numeric(productions[[i]][e+1]))
    }
       productions.model$set(iter, 13,TRUE)
  } 
 
    productions.treeview$destroy()
    
  productions.treeview <<- gtkTreeViewNewWithModel( productions.model)
 productions.treeview$setRulesHint(TRUE)
 productions.treeview$getSelection()$setMode("single")
productions.add_columns( productions.treeview)
productions.sw$add(productions.treeview)
   
}