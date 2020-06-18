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
# seed value (FEMALES)
# ------------------------------------------------------------------------------
#
reload_ZvectorF_table<- function(w) {

  mortality.Zvector.females.seed <- gtkEntryGetText(entry_Zseedvalue_F)
  Zvector_F <<- list()
Zvector_FIndex <<- 0

   ZF_matrix <- data.frame(matrix(as.double(mortality.Zvector.females.seed), nrow=length(years), ncol=13))
   colnames(ZF_matrix) <- c("year",MONTHS)
     ZF_matrix$year <- years
   for (r in 1:nrow(ZF_matrix)) { 
  ZF_temp <- as.list(ZF_matrix[r,]) 
  Zvector_F <<- c(Zvector_F, list(ZF_temp)) 
  }
  
  mortality.Zvector.females <<- ZF_matrix
  
Zvector_F.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length(Zvector_F)) {
    iter <- Zvector_F.model$append()$iter
    Zvector_F.model$set(iter,0, Zvector_F[[i]]$year)
    for (e in 1:length(MONTHS)) {
         Zvector_F.model$set(iter, e, as.numeric(Zvector_F[[i]][e+1]))
    }
       Zvector_F.model$set(iter, 13,TRUE)
  } 
  
   Zvector_F.treeview$destroy()
  Zvector_F.treeview <<- gtkTreeViewNewWithModel( Zvector_F.model)
 Zvector_F.treeview$setRulesHint(TRUE)
 Zvector_F.treeview$getSelection()$setMode("single")
Zvector_F.add_columns( Zvector_F.treeview)
 
Zvector_F.sw$add(Zvector_F.treeview)

    
}
