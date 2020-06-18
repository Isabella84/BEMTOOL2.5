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
reload_EMPTY_GT_fore_table<- function(w) {
                            # fleet.GT_fore
  GT_fore <<- list()
  GT_foreIndex <<- 0

    GT_fore.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 12), "gboolean")  

   GT_fore_matrix <- data.frame(matrix(0, nrow=length(years_forecast), ncol=13))
   colnames(GT_fore_matrix) <- c("year",MONTHS)
     GT_fore_matrix$year <- years_forecast
   for (r in 1:nrow(GT_fore_matrix)) { 
  gt_fore_temp <- as.list(GT_fore_matrix[r,]) 
  GT_fore <<- c(GT_fore, list(gt_fore_temp)) 
  }
     fleet.GT_fore <<- GT_fore_matrix
  
  for (i in 1:length(GT_fore)) {
    iter <-  GT_fore.model$append()$iter
     GT_fore.model$set(iter,0, GT_fore[[i]]$year)
    for (e in 1:length(MONTHS)) {
        GT_fore.model$set(iter, e, as.double(GT_fore[[i]][e+1]))      
    }
    GT_fore.model$set(iter,13,TRUE)
  } 
  
  GT_fore.treeview$destroy() 
     GT_fore.treeview <<- gtkTreeViewNewWithModel( GT_fore.model)
GT_fore.treeview$setRulesHint(TRUE)
 GT_fore.treeview$getSelection()$setMode("single")
GT_fore.add_columns( GT_fore.treeview)
GT_fore.sw$add(GT_fore.treeview)

}




reload_GT_fore_table<- function(w) {
                            # fleet.GT_fore
  GT_fore <<- list()
  GT_foreIndex <<- 0

    GT_fore.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 12), "gboolean")  

   GT_fore_matrix <-  fleet.GT_fore
   for (r in 1:nrow(GT_fore_matrix)) { 
  gt_fore_temp <- as.list(GT_fore_matrix[r,]) 
  GT_fore <<- c(GT_fore, list(gt_fore_temp)) 
  }
  
  for (i in 1:length(GT_fore)) {
    iter <-  GT_fore.model$append()$iter
     GT_fore.model$set(iter,0, GT_fore[[i]]$year)
    for (e in 1:length(MONTHS)) {
        GT_fore.model$set(iter, e, as.double(GT_fore[[i]][e+1]))      
    }
    GT_fore.model$set(iter,13,TRUE)
  } 
  
  GT_fore.treeview$destroy() 
     GT_fore.treeview <<- gtkTreeViewNewWithModel( GT_fore.model)
GT_fore.treeview$setRulesHint(TRUE)
 GT_fore.treeview$getSelection()$setMode("single")
GT_fore.add_columns( GT_fore.treeview)
GT_fore.sw$add(GT_fore.treeview)

}
