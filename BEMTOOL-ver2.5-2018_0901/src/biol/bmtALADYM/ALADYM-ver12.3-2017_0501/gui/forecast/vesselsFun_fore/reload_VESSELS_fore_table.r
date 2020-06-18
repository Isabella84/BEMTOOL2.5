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
reload_EMPTY_VESSELS_fore_table<- function(w) {
                            # fleet.GT_fore
VESSELS_fore <<- list()
VESSELS_foreIndex <<- 0

  VESSELS_fore.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 12), "gboolean")  


   vess_matrix <- data.frame(matrix(0, nrow=length(years_forecast), ncol=13))
   colnames(vess_matrix) <- c("year",MONTHS)
     vess_matrix$year <- years_forecast
   for (r in 1:nrow(vess_matrix)) { 
  vess_temp <- as.list(vess_matrix[r,]) 
  VESSELS_fore <<- c(VESSELS_fore, list(vess_temp)) 
  }
  
    fleet.VESSELS_fore <<- VESSELS_fore
  
  for (i in 1:length(VESSELS_fore)) {
    iter <-  VESSELS_fore.model$append()$iter
     VESSELS_fore.model$set(iter,0, VESSELS_fore[[i]]$year)
    for (e in 1:length(MONTHS)) {
        VESSELS_fore.model$set(iter, e, as.double(VESSELS_fore[[i]][e+1]))      
    }
    VESSELS_fore.model$set(iter,13,TRUE)
  } 

  VESSELS_fore.treeview$destroy()
VESSELS_fore.treeview <<- gtkTreeViewNewWithModel(VESSELS_fore.model)
VESSELS_fore.treeview$setRulesHint(TRUE)
VESSELS_fore.treeview$getSelection()$setMode("single")
VESSELS_fore.add_columns(VESSELS_fore.treeview)
VESSELS_fore.sw$add(VESSELS_fore.treeview)

}






reload_VESSELS_fore_table<- function(w) {
                            # fleet.GT_fore
VESSELS_fore <<- list()
VESSELS_foreIndex <<- 0

  VESSELS_fore.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 12), "gboolean")  

   vess_matrix <-   fleet.VESSELS_fore 
   for (r in 1:nrow(vess_matrix)) { 
  vess_temp <- as.list(vess_matrix[r,]) 
  VESSELS_fore <<- c(VESSELS_fore, list(vess_temp)) 
  }
  
  for (i in 1:length(VESSELS_fore)) {
    iter <-  VESSELS_fore.model$append()$iter
     VESSELS_fore.model$set(iter,0, VESSELS_fore[[i]]$year)
    for (e in 1:length(MONTHS)) {
        VESSELS_fore.model$set(iter, e, as.double(VESSELS_fore[[i]][e+1]))      
    }
    VESSELS_fore.model$set(iter,13,TRUE)
  } 

  VESSELS_fore.treeview$destroy()
VESSELS_fore.treeview <<- gtkTreeViewNewWithModel(VESSELS_fore.model)
VESSELS_fore.treeview$setRulesHint(TRUE)
VESSELS_fore.treeview$getSelection()$setMode("single")
VESSELS_fore.add_columns(VESSELS_fore.treeview)
VESSELS_fore.sw$add(VESSELS_fore.treeview)

}
