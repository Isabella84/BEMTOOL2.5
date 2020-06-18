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
reload_EMPTY_VESSELS_table<- function(w) {

  VESSELS <<- list()
  VESSELSIndex <<- 0

   VESSELS_matrix <- data.frame(matrix(0, nrow=length(years), ncol=13))
   colnames(VESSELS_matrix) <- c("year",MONTHS)
     VESSELS_matrix$year <- years
   for (r in 1:nrow(VESSELS_matrix)) { 
  VESSELS_temp <- as.list(VESSELS_matrix[r,]) 
  VESSELS <<- c(VESSELS, list(VESSELS_temp)) 
  }
  
    fleet.VESSELS <<- VESSELS_matrix
  
VESSELS.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length(VESSELS)) {
    iter <- VESSELS.model$append()$iter
    VESSELS.model$set(iter,0, VESSELS[[i]]$year)
    for (e in 1:length(MONTHS)) {
         VESSELS.model$set(iter, e, as.numeric(VESSELS[[i]][e+1]))
    }
       VESSELS.model$set(iter, 13,TRUE)
  } 

  VESSELS.treeview$destroy()
  VESSELS.treeview <<- gtkTreeViewNewWithModel( VESSELS.model)
 VESSELS.treeview$setRulesHint(TRUE)
 VESSELS.treeview$getSelection()$setMode("single")
VESSELS.add_columns( VESSELS.treeview) 
VESSELS.sw$add(VESSELS.treeview)
  
}




reload_VESSELS_table<- function(w) {

  VESSELS <<- list()
  VESSELSIndex <<- 0

   VESSELS_matrix <- fleet.VESSELS
   for (r in 1:nrow(VESSELS_matrix)) { 
  VESSELS_temp <- as.list(VESSELS_matrix[r,]) 
  VESSELS <<- c(VESSELS, list(VESSELS_temp)) 
  }
  
VESSELS.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length(VESSELS)) {
    iter <- VESSELS.model$append()$iter
    VESSELS.model$set(iter,0, VESSELS[[i]]$year)
    for (e in 1:length(MONTHS)) {
         VESSELS.model$set(iter, e, as.numeric(VESSELS[[i]][e+1]))
    }
       VESSELS.model$set(iter, 13,TRUE)
  } 

  VESSELS.treeview$destroy()
  VESSELS.treeview <<- gtkTreeViewNewWithModel( VESSELS.model)
 VESSELS.treeview$setRulesHint(TRUE)
 VESSELS.treeview$getSelection()$setMode("single")
VESSELS.add_columns( VESSELS.treeview) 
VESSELS.sw$add(VESSELS.treeview)
  
}
