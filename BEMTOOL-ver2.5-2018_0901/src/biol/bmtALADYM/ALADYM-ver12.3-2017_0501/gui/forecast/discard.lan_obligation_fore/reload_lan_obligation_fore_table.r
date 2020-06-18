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
# ------------------------------------------------------------------------------
# Function to reload the values for the production according to the 
# seed value
# ------------------------------------------------------------------------------
#
reload_EMPTY_lan_obligation_fore_table <- function(w) {

 # fleet.lan_obligation_fore.seed <- gtkEntryGetText(entry_lan_obligation_fore_seedvalue)
  lan_obligation_fore <<- list()
  lan_obligation_foreIndex <<- 0

   lan_obligation_fore_matrix <- data.frame(matrix("N", nrow=length(years_forecast), ncol=13), stringsAsFactors  = F)
   colnames(lan_obligation_fore_matrix) <- c("year",MONTHS)
     lan_obligation_fore_matrix$year <- years_forecast
   for (r in 1:nrow(lan_obligation_fore_matrix)) { 
  lan_obligation_fore_temp <- as.list(lan_obligation_fore_matrix[r,]) 
  lan_obligation_fore <<- c(lan_obligation_fore, list(lan_obligation_fore_temp)) 
  }
  
    fleet.lan_obligation_fore <<- lan_obligation_fore_matrix
  
lan_obligation_fore.model <<- gtkListStoreNew("gchararray",  rep("gchararray", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length(lan_obligation_fore)) {
    iter <- lan_obligation_fore.model$append()$iter
    lan_obligation_fore.model$set(iter,0, lan_obligation_fore[[i]]$year)
    for (e in 1:length(MONTHS)) {
         lan_obligation_fore.model$set(iter, e, as.character(lan_obligation_fore[[i]][e+1][[1]]))
    }
       lan_obligation_fore.model$set(iter, 13,TRUE)
  } 
  
  lan_obligation_fore.treeview$destroy()
  
  lan_obligation_fore.treeview <<- gtkTreeViewNewWithModel( lan_obligation_fore.model)
 lan_obligation_fore.treeview$setRulesHint(TRUE)
 lan_obligation_fore.treeview$getSelection()$setMode("single")
lan_obligation_fore.add_columns( lan_obligation_fore.treeview) 
lan_obligation_fore.sw$add(lan_obligation_fore.treeview)

#
    
}





reload_lan_obligation_fore_table <- function(w) {

 # fleet.lan_obligation_fore.seed <- gtkEntryGetText(entry_lan_obligation_fore_seedvalue)
  lan_obligation_fore <<- list()
  lan_obligation_foreIndex <<- 0

   lan_obligation_fore_matrix <- fleet.lan_obligation_fore

   for (r in 1:nrow(lan_obligation_fore_matrix)) { 
  lan_obligation_fore_temp <- as.list(lan_obligation_fore_matrix[r,]) 
  lan_obligation_fore <<- c(lan_obligation_fore, list(lan_obligation_fore_temp)) 
  }
  
lan_obligation_fore.model <<- gtkListStoreNew("gchararray",  rep("gchararray", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length(lan_obligation_fore)) {
    iter <- lan_obligation_fore.model$append()$iter
    lan_obligation_fore.model$set(iter,0, lan_obligation_fore[[i]]$year)
    for (e in 1:length(MONTHS)) {
         lan_obligation_fore.model$set(iter, e, as.character(lan_obligation_fore[[i]][e+1][[1]]))
    }
       lan_obligation_fore.model$set(iter, 13,TRUE)
  } 
  
  lan_obligation_fore.treeview$destroy()
  
  lan_obligation_fore.treeview <<- gtkTreeViewNewWithModel( lan_obligation_fore.model)
 lan_obligation_fore.treeview$setRulesHint(TRUE)
 lan_obligation_fore.treeview$getSelection()$setMode("single")
lan_obligation_fore.add_columns( lan_obligation_fore.treeview) 
lan_obligation_fore.sw$add(lan_obligation_fore.treeview)
    
}