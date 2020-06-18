# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





#------------------------------------------ load the file  (MALE)
escape_survival_extvector_M_list_fore <<- list()
escape_survival_extvector_MIndex_fore <<- 0
add.escape_survival_extvector_M_fore()
 # try( print(FleetList_simulation[[1]]@fishingmortality.F.vector) )
if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_M_lifespan))  
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[1,1])      
} 
#
first_age_mal<- 0

    n_ages <- n_ages - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)

	
escape_survival_extvector_M_fore.model <<- gtkListStoreNew(rep("gdouble", n_ages), "gboolean")  
  # add items 
  for (i in 1:length(escape_survival_extvector_M_list_fore)) {
    iter <- escape_survival_extvector_M_fore.model$append()$iter
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    #escape_survival_extvector_M_fore.model$set(iter,0, escape_survival_extvector_M_list[[i]]$year)
    for (e in 1:(n_ages) ) {
   # print(paste("in model:", years[nc]) )
         escape_survival_extvector_M_fore.model$set(iter, e-1, as.numeric(escape_survival_extvector_M_list_fore[[i]][e]))
    }
       escape_survival_extvector_M_fore.model$set(iter, (n_ages),TRUE)
  } 	
	
 
  #  print("DOPO AVER CREATO LA TABELLA DELLA f (FEMMINE)!", quote=F)
#try( print(FleetList_simulation[[1]]@fishingmortality.F.vector)  )

escape_survival_extvector_M_fore.treeview$destroy()
escape_survival_extvector_M_fore.treeview <<- gtkTreeViewNewWithModel( escape_survival_extvector_M_fore.model)
escape_survival_extvector_M_fore.treeview$setRulesHint(TRUE)
escape_survival_extvector_M_fore.treeview$getSelection()$setMode("single")
escape_survival_extvector_M_fore.add_columns( escape_survival_extvector_M_fore.treeview)
escape_survival_extvector_M_fore.sw$add(escape_survival_extvector_M_fore.treeview)

