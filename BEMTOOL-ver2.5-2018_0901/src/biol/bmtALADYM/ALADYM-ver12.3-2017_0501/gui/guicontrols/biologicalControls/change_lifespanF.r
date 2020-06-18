# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




change_lifespanF<- function(w) {


biological.lifeSpanF  <<- as.numeric(as.character(gtkEntryGetText(entryVBF_F_lifespan))) 
  
 # print(paste("changed TR!", Tr))
 months_vec_F <<- c(Tr:(biological.lifeSpanF*12))
biological.months_MF <<- length(months_vec_F)


#------------------------------------------ load the file
if (FALSE) {
Mvector_F <<- list()
Mvector_FIndex <<- 0
add.Mvector_F()
  Mvector_F.model <<- gtkListStoreNew("gchararray", "gdouble", "gboolean")  
  for (i in 1:length(Mvector_F)) {
    iter <-  Mvector_F.model$append()$iter
     Mvector_F.model$set(iter,0, as.character(Mvector_F[[i]]$age_month))
     Mvector_F.model$set(iter, 1, as.double(Mvector_F[[i]]$M))          # as.double(sexratios[[ind]][nc_i+1]) 
     Mvector_F.model$set(iter,2,TRUE)
  } 

   Mvector_F.treeview$destroy()
 Mvector_F.treeview <<- gtkTreeViewNewWithModel( Mvector_F.model)
 Mvector_F.treeview$setRulesHint(TRUE)
 Mvector_F.treeview$getSelection()$setMode("single")
Mvector_F.add_columns( Mvector_F.treeview)
Mvector_F.sw$add(Mvector_F.treeview)

deactivate_M_unused_params_F()
	}
	
}