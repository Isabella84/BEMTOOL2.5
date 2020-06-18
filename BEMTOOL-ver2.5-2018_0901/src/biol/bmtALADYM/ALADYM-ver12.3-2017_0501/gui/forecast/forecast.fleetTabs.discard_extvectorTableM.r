# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





#------------------------------------------ load the file  (FEMALE)
discards_extvector_M_list_fore <<- list()
discards_extvector_MIndex_fore <<- 0
add.discards_extvector_M_fore()
 # try( print(FleetList_simulation[[1]]@fishingmortality.F.vector) )
if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_M_lifespan))  
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[2,1])      
} 
#
first_age_mal<- 0

    n_ages <- n_ages - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)

  discards_extvector_M_fore.model <<- gtkListStoreNew("gchararray",  rep("gdouble", n_ages), "gboolean")  
  for (i in 1:length(discards_extvector_M_list_fore)) {
    iter <-  discards_extvector_M_fore.model$append()$iter
     discards_extvector_M_fore.model$set(iter,0, discards_extvector_M_list_fore[[i]]$year)
    #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:n_ages) {
        discards_extvector_M_fore.model$set(iter, e, as.double(discards_extvector_M_list_fore[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     discards_extvector_M_fore.model$set(iter,(n_ages+1),TRUE)
  } 
 
  #  print("DOPO AVER CREATO LA TABELLA DELLA f (FEMMINE)!", quote=F)
#try( print(FleetList_simulation[[1]]@fishingmortality.F.vector)  )

discards_extvector_M_fore.treeview$destroy()
discards_extvector_M_fore.treeview <<- gtkTreeViewNewWithModel( discards_extvector_M_fore.model)
discards_extvector_M_fore.treeview$setRulesHint(TRUE)
discards_extvector_M_fore.treeview$getSelection()$setMode("single")
discards_extvector_M_fore.add_columns( discards_extvector_M_fore.treeview)
discards_extvector_M_fore.sw$add(discards_extvector_M_fore.treeview)

