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
reload_EMPTY_FISHINGEFFORT_fore_table<- function(w) {
                            # fleet.GT_fore
 FISHINGEFFORT_fore <<- list()
FISHINGEFFORT_foreIndex <<- 0


   FE_matrix <- data.frame(matrix(0, nrow=length(years_forecast), ncol=13))
   colnames(FE_matrix) <- c("year",MONTHS)
     FE_matrix$year <- years_forecast
   for (r in 1:nrow(FE_matrix)) { 
  FE_temp <- as.list(FE_matrix[r,]) 
  FISHINGEFFORT_fore <<- c(FISHINGEFFORT_fore, list(FE_temp)) 
  }
  
   fleet.FISHINGEFFORT_fore <<- FISHINGEFFORT_fore    
  
 FISHINGEFFORT_fore.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 12), "gboolean")  
  for (i in 1:length(FISHINGEFFORT_fore)) {
    iter <-  FISHINGEFFORT_fore.model$append()$iter
     FISHINGEFFORT_fore.model$set(iter,0, FISHINGEFFORT_fore[[i]]$year)
    for (e in 1:length(MONTHS)) {
        FISHINGEFFORT_fore.model$set(iter, e, as.double(FISHINGEFFORT_fore[[i]][e+1]))      
    }
    FISHINGEFFORT_fore.model$set(iter,13,TRUE)
  } 

   FISHINGEFFORT_fore.treeview$destroy()
 FISHINGEFFORT_fore.treeview <<- gtkTreeViewNewWithModel( FISHINGEFFORT_fore.model)
FISHINGEFFORT_fore.treeview$setRulesHint(TRUE)
 FISHINGEFFORT_fore.treeview$getSelection()$setMode("single")
FISHINGEFFORT_fore.add_columns( FISHINGEFFORT_fore.treeview)
FISHINGEFFORT_fore.sw$add(FISHINGEFFORT_fore.treeview)

}




reload_FISHINGEFFORT_fore_table<- function(w) {
                            # fleet.GT_fore
 FISHINGEFFORT_fore <<- list()
FISHINGEFFORT_foreIndex <<- 0


   FE_matrix <-  fleet.FISHINGEFFORT_fore 
   for (r in 1:nrow(FE_matrix)) { 
  FE_temp <- as.list(FE_matrix[r,]) 
  FISHINGEFFORT_fore <<- c(FISHINGEFFORT_fore, list(FE_temp)) 
  }

  
 FISHINGEFFORT_fore.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 12), "gboolean")  
  for (i in 1:length(FISHINGEFFORT_fore)) {
    iter <-  FISHINGEFFORT_fore.model$append()$iter
     FISHINGEFFORT_fore.model$set(iter,0, FISHINGEFFORT_fore[[i]]$year)
    for (e in 1:length(MONTHS)) {
        FISHINGEFFORT_fore.model$set(iter, e, as.double(FISHINGEFFORT_fore[[i]][e+1]))      
    }
    FISHINGEFFORT_fore.model$set(iter,13,TRUE)
  } 

   FISHINGEFFORT_fore.treeview$destroy()
 FISHINGEFFORT_fore.treeview <<- gtkTreeViewNewWithModel( FISHINGEFFORT_fore.model)
FISHINGEFFORT_fore.treeview$setRulesHint(TRUE)
 FISHINGEFFORT_fore.treeview$getSelection()$setMode("single")
FISHINGEFFORT_fore.add_columns( FISHINGEFFORT_fore.treeview)
FISHINGEFFORT_fore.sw$add(FISHINGEFFORT_fore.treeview)

}
