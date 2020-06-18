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
# Function to reload the values for the total mortality according to the 
# seed value (MALES)
# ------------------------------------------------------------------------------
#
reload_ZvectorM_table<- function(w) {

  mortality.Zvector.males.seed <- gtkEntryGetText(entry_Zseedvalue_M)
  Zvector_M <<- list()
Zvector_MIndex <<- 0

   ZM_matrix <- data.frame(matrix(as.double(mortality.Zvector.males.seed), nrow=length(years), ncol=13))
   colnames(ZM_matrix) <- c("year",MONTHS)
     ZM_matrix$year <- years
   for (r in 1:nrow(ZM_matrix)) { 
  ZM_temp <- as.list(ZM_matrix[r,]) 
  Zvector_M <<- c(Zvector_M, list(ZM_temp)) 
  }
  
  mortality.Zvector.males <<- ZM_matrix
  
Zvector_M.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length(Zvector_M)) {
    iter <- Zvector_M.model$append()$iter
    Zvector_M.model$set(iter,0, Zvector_M[[i]]$year)
    for (e in 1:length(MONTHS)) {
         Zvector_M.model$set(iter, e, as.numeric(Zvector_M[[i]][e+1]))
    }
       Zvector_M.model$set(iter, 13,TRUE)
  } 
  
   Zvector_M.treeview$destroy()
   
  Zvector_M.treeview <<- gtkTreeViewNewWithModel( Zvector_M.model)
 Zvector_M.treeview$setRulesHint(TRUE)
 Zvector_M.treeview$getSelection()$setMode("single")
Zvector_M.add_columns( Zvector_M.treeview)

Zvector_M.sw$add(Zvector_M.treeview)

    
}
