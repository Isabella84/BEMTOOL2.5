# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





change_lifespanM<- function(w) {

biological.lifeSpanM  <<- as.numeric(as.character(gtkEntryGetText(entryVBF_M_lifespan))) 
  
 # print(paste("changed TR!", Tr))
months_vec_M <<- c(Tr:(biological.lifeSpanM*12))
biological.months_MM <<- length(months_vec_M)


#------------------------------------------ load the file
if (FALSE) {
Mvector_M <<- list()
Mvector_MIndex <<- 0
add.Mvector_M()
  Mvector_M.model <<- gtkListStoreNew("gchararray", "gdouble", "gboolean")  
  for (i in 1:length(Mvector_M)) {
    iter <-  Mvector_M.model$append()$iter
     Mvector_M.model$set(iter, 0, as.character(Mvector_M[[i]]$age_month))
     Mvector_M.model$set(iter, 1, as.double(Mvector_M[[i]]$M))          # as.double(sexratios[[ind]][nc_i+1]) 
     Mvector_M.model$set(iter, 2,TRUE)
  } 

  Mvector_M.treeview$destroy()
 Mvector_M.treeview <<- gtkTreeViewNewWithModel( Mvector_M.model)
 Mvector_M.treeview$setRulesHint(TRUE)
 Mvector_M.treeview$getSelection()$setMode("single")
Mvector_M.add_columns( Mvector_M.treeview)

Mvector_M.sw$add(Mvector_M.treeview)

deactivate_M_unused_params_M()
 }
 
}