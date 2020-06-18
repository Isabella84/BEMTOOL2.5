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
reload_EMPTY_DAYS_table<- function(w) {


  DAYS <<- list()
  DAYSIndex <<- 0

   DAYS_matrix <- data.frame(matrix(0, nrow=length(years), ncol=13))
   colnames(DAYS_matrix) <- c("year",MONTHS)
     DAYS_matrix$year <- years
   for (r in 1:nrow(DAYS_matrix)) { 
  DAYS_temp <- as.list(DAYS_matrix[r,]) 
  DAYS <<- c(DAYS, list(DAYS_temp)) 
  }
  
    fleet.DAYS <<- DAYS_matrix
  
DAYS.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length(DAYS)) {
    iter <- DAYS.model$append()$iter
    DAYS.model$set(iter,0, DAYS[[i]]$year)
    for (e in 1:length(MONTHS)) {
         DAYS.model$set(iter, e, as.numeric(DAYS[[i]][e+1]))
    }
       DAYS.model$set(iter, 13,TRUE)
  } 
  
  DAYS.treeview$destroy()
  
  DAYS.treeview <<- gtkTreeViewNewWithModel( DAYS.model)
 DAYS.treeview$setRulesHint(TRUE)
 DAYS.treeview$getSelection()$setMode("single")
DAYS.add_columns( DAYS.treeview) 
DAYS.sw$add(DAYS.treeview)
  
}




reload_DAYS_table<- function(w) {

  DAYS <<- list()
  DAYSIndex <<- 0

   DAYS_matrix <- fleet.DAYS

   for (r in 1:nrow(DAYS_matrix)) { 
  DAYS_temp <- as.list(DAYS_matrix[r,]) 
  DAYS <<- c(DAYS, list(DAYS_temp)) 
  }
  
DAYS.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length(DAYS)) {
    iter <- DAYS.model$append()$iter
    DAYS.model$set(iter,0, DAYS[[i]]$year)
    for (e in 1:length(MONTHS)) {
         DAYS.model$set(iter, e, as.numeric(DAYS[[i]][e+1]))
    }
       DAYS.model$set(iter, 13,TRUE)
  } 
  
  DAYS.treeview$destroy()
  
  DAYS.treeview <<- gtkTreeViewNewWithModel( DAYS.model)
 DAYS.treeview$setRulesHint(TRUE)
 DAYS.treeview$getSelection()$setMode("single")
DAYS.add_columns( DAYS.treeview) 
DAYS.sw$add(DAYS.treeview)
  
}