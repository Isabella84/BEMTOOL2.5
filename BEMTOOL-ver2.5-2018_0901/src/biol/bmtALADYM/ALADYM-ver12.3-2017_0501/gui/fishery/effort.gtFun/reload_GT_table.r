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
# ------------------------------------------------------------------------------
# Function to reload the values for the production according to the 
# seed value
# ------------------------------------------------------------------------------
#
reload_EMPTY_GT_table<- function(w) {

  GT <<- list()
  GTIndex <<- 0

   GT_matrix <- data.frame(matrix(0, nrow=length(years), ncol=13))
   colnames(GT_matrix) <- c("year",MONTHS)
     GT_matrix$year <- years
   for (r in 1:nrow(GT_matrix)) { 
  GT_temp <- as.list(GT_matrix[r,]) 
  GT <<- c(GT, list(GT_temp)) 
  }
  
    fleet.GT <<- GT_matrix
  
GT.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length(GT)) {
    iter <- GT.model$append()$iter
    GT.model$set(iter,0, GT[[i]]$year)
    for (e in 1:length(MONTHS)) {
         GT.model$set(iter, e, as.numeric(GT[[i]][e+1]))
    }
       GT.model$set(iter, 13,TRUE)
  } 
 
  GT.treeview$destroy()
  
  GT.treeview <<- gtkTreeViewNewWithModel( GT.model)
 GT.treeview$setRulesHint(TRUE)
 GT.treeview$getSelection()$setMode("single")
GT.add_columns( GT.treeview) 
GT.sw$add(GT.treeview)
    
}




reload_GT_table<- function(w) {

  GT <<- list()
  GTIndex <<- 0

   GT_matrix <- fleet.GT 
   for (r in 1:nrow(GT_matrix)) { 
  GT_temp <- as.list(GT_matrix[r,]) 
  GT <<- c(GT, list(GT_temp)) 
  }
  
GT.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length(GT)) {
    iter <- GT.model$append()$iter
    GT.model$set(iter,0, GT[[i]]$year)
    for (e in 1:length(MONTHS)) {
         GT.model$set(iter, e, as.numeric(GT[[i]][e+1]))
    }
       GT.model$set(iter, 13,TRUE)
  } 
 
  GT.treeview$destroy()
  
  GT.treeview <<- gtkTreeViewNewWithModel( GT.model)
 GT.treeview$setRulesHint(TRUE)
 GT.treeview$getSelection()$setMode("single")
GT.add_columns( GT.treeview) 
GT.sw$add(GT.treeview)
    
}