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
reload_EMPTY_FISHINGEFFORT_table<- function(w) {

  fleet.FISHINGEFFORT.seed <- gtkEntryGetText(entry_FISHINGEFFORT_seedvalue)
  FISHINGEFFORT <<- list()
  FISHINGEFFORTIndex <<- 0

   FISHINGEFFORT_matrix <- data.frame(matrix(-1, nrow=length(years), ncol=13))
   colnames(FISHINGEFFORT_matrix) <- c("year",MONTHS)
     FISHINGEFFORT_matrix$year <- years
   for (r in 1:nrow(FISHINGEFFORT_matrix)) { 
  FISHINGEFFORT_temp <- as.list(FISHINGEFFORT_matrix[r,]) 
  FISHINGEFFORT <<- c(FISHINGEFFORT, list(FISHINGEFFORT_temp)) 
  }
  
    fleet.FISHINGEFFORT <<- FISHINGEFFORT_matrix
  
FISHINGEFFORT.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length(FISHINGEFFORT)) {
    iter <- FISHINGEFFORT.model$append()$iter
    FISHINGEFFORT.model$set(iter,0, FISHINGEFFORT[[i]]$year)
    for (e in 1:length(MONTHS)) {
         FISHINGEFFORT.model$set(iter, e, as.numeric(FISHINGEFFORT[[i]][e+1]))
    }
       FISHINGEFFORT.model$set(iter, 13,TRUE)
  } 

FISHINGEFFORT.treeview$destroy()
  
FISHINGEFFORT.treeview <<- gtkTreeViewNewWithModel( FISHINGEFFORT.model)
FISHINGEFFORT.treeview$setRulesHint(TRUE)
FISHINGEFFORT.treeview$getSelection()$setMode("single")
FISHINGEFFORT.add_columns( FISHINGEFFORT.treeview) 
FISHINGEFFORT.sw$add(FISHINGEFFORT.treeview)
}



reload_FISHINGEFFORT_table<- function(w) {

  FISHINGEFFORT <<- list()
  FISHINGEFFORTIndex <<- 0

   FISHINGEFFORT_matrix <- fleet.FISHINGEFFORT
   for (r in 1:nrow(FISHINGEFFORT_matrix)) { 
  FISHINGEFFORT_temp <- as.list(FISHINGEFFORT_matrix[r,]) 
  FISHINGEFFORT <<- c(FISHINGEFFORT, list(FISHINGEFFORT_temp)) 
  }
  
FISHINGEFFORT.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length(FISHINGEFFORT)) {
    iter <- FISHINGEFFORT.model$append()$iter
    FISHINGEFFORT.model$set(iter,0, FISHINGEFFORT[[i]]$year)
    for (e in 1:length(MONTHS)) {
         FISHINGEFFORT.model$set(iter, e, as.numeric(FISHINGEFFORT[[i]][e+1]))
    }
       FISHINGEFFORT.model$set(iter, 13,TRUE)
  } 

FISHINGEFFORT.treeview$destroy()
  
FISHINGEFFORT.treeview <<- gtkTreeViewNewWithModel( FISHINGEFFORT.model)
FISHINGEFFORT.treeview$setRulesHint(TRUE)
FISHINGEFFORT.treeview$getSelection()$setMode("single")
FISHINGEFFORT.add_columns( FISHINGEFFORT.treeview) 
FISHINGEFFORT.sw$add(FISHINGEFFORT.treeview)
}
