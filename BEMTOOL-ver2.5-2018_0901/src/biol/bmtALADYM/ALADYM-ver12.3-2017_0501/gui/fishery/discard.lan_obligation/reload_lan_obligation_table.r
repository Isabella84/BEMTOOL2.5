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
reload_EMPTY_lan_obligation_table<- function(w) {

  lan_obligation <<- list()
  lan_obligationIndex <<- 0

   lan_obligation_matrix <- data.frame(matrix("N", nrow=length(years), ncol=13), stringsAsFactors  = F)
   colnames(lan_obligation_matrix) <- c("year",MONTHS)
     lan_obligation_matrix$year <- years
   for (r in 1:nrow(lan_obligation_matrix)) { 
  lan_obligation_temp <- as.list(lan_obligation_matrix[r,]) 
  lan_obligation <<- c(lan_obligation, list(lan_obligation_temp)) 
  }
  
    fleet.lan_obligation <<- lan_obligation_matrix
  
lan_obligation.model <<- gtkListStoreNew("gchararray",  rep("gchararray", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length(lan_obligation)) {
    iter <- lan_obligation.model$append()$iter
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    lan_obligation.model$set(iter,0, lan_obligation[[i]]$year)
    for (e in 1:length(MONTHS)) {
   # print(paste("in model:", years[nc]) )
         lan_obligation.model$set(iter, e, as.character(lan_obligation[[i]][e+1][[1]]) )
    }
       lan_obligation.model$set(iter, 13,TRUE)
  } 
  
  lan_obligation.treeview$destroy()
  
  lan_obligation.treeview <<- gtkTreeViewNewWithModel( lan_obligation.model)
 lan_obligation.treeview$setRulesHint(TRUE)
 lan_obligation.treeview$getSelection()$setMode("single")
lan_obligation.add_columns( lan_obligation.treeview) 
lan_obligation.sw$add(lan_obligation.treeview)
  
}



reload_lan_obligation_table<- function(w) {

  lan_obligation <<- list()
  lan_obligationIndex <<- 0
  
    for (r in 1:nrow(fleet.lan_obligation)) {
  lan_obligation_temp <- as.list(fleet.lan_obligation[r,]) 
  names(lan_obligation_temp) <- c("year",MONTHS)
  lan_obligation <<- c(lan_obligation, list(lan_obligation_temp)) 
  }
  
  
lan_obligation.model <<- gtkListStoreNew("gchararray",  rep("gchararray", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length(lan_obligation)) {
    iter <- lan_obligation.model$append()$iter
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    lan_obligation.model$set(iter,0, lan_obligation[[i]]$year)
    for (e in 1:length(MONTHS)) {
   # print(paste("in model:", years[nc]) )
         lan_obligation.model$set(iter, e, as.character(lan_obligation[[i]][e+1][[1]]) )
    }
       lan_obligation.model$set(iter, 13,TRUE)
  } 
  
  lan_obligation.treeview$destroy()
  
  lan_obligation.treeview <<- gtkTreeViewNewWithModel( lan_obligation.model)
 lan_obligation.treeview$setRulesHint(TRUE)
 lan_obligation.treeview$getSelection()$setMode("single")
lan_obligation.add_columns( lan_obligation.treeview) 
lan_obligation.sw$add(lan_obligation.treeview)
  
}
