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
reload_EMPTY_DAYS_fore_table<- function(w) {
                            # fleet.GT_fore
  DAYS_fore <<- list()
  DAYS_foreIndex <<- 0

   DAYS_fore_matrix <- data.frame(matrix(0, nrow=length(years_forecast), ncol=13))
   colnames(DAYS_fore_matrix) <- c("year",MONTHS)
     DAYS_fore_matrix$year <- years_forecast
   for (r in 1:nrow(DAYS_fore_matrix)) { 
  DAYS_fore_temp <- as.list(DAYS_fore_matrix[r,]) 
  DAYS_fore <<- c(DAYS_fore, list(DAYS_fore_temp)) 
  }
  
    fleet.DAYS_fore <<- DAYS_fore_matrix    
  
 DAYS_fore.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 12), "gboolean")  
  for (i in 1:length(DAYS_fore)) {
    iter <-  DAYS_fore.model$append()$iter
     DAYS_fore.model$set(iter,0, DAYS_fore[[i]]$year)
    for (e in 1:length(MONTHS)) {
        DAYS_fore.model$set(iter, e, as.double(DAYS_fore[[i]][e+1]))      
    }
    DAYS_fore.model$set(iter,13,TRUE)
  } 

  DAYS_fore.treeview$destroy()
 DAYS_fore.treeview <<- gtkTreeViewNewWithModel( DAYS_fore.model)
DAYS_fore.treeview$setRulesHint(TRUE)
 DAYS_fore.treeview$getSelection()$setMode("single")
DAYS_fore.add_columns( DAYS_fore.treeview)
DAYS_fore.sw$add(DAYS_fore.treeview)
    
}





reload_DAYS_fore_table<- function(w) {
                            # fleet.GT_fore
  DAYS_fore <<- list()
  DAYS_foreIndex <<- 0

     DAYS_fore_matrix <-  fleet.DAYS_fore

   for (r in 1:nrow(DAYS_fore_matrix)) { 
  DAYS_fore_temp <- as.list(DAYS_fore_matrix[r,]) 
  DAYS_fore <<- c(DAYS_fore, list(DAYS_fore_temp)) 
  }
   
 DAYS_fore.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 12), "gboolean")  
  for (i in 1:length(DAYS_fore)) {
    iter <-  DAYS_fore.model$append()$iter
     DAYS_fore.model$set(iter,0, DAYS_fore[[i]]$year)
    for (e in 1:length(MONTHS)) {
        DAYS_fore.model$set(iter, e, as.double(DAYS_fore[[i]][e+1]))      
    }
    DAYS_fore.model$set(iter,13,TRUE)
  } 

  DAYS_fore.treeview$destroy()
 DAYS_fore.treeview <<- gtkTreeViewNewWithModel( DAYS_fore.model)
DAYS_fore.treeview$setRulesHint(TRUE)
 DAYS_fore.treeview$getSelection()$setMode("single")
DAYS_fore.add_columns( DAYS_fore.treeview)
DAYS_fore.sw$add(DAYS_fore.treeview)
    
}

