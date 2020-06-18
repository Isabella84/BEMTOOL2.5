# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




#------------------------------------------ load the file  (MALES)
FishingMvector_M <<- list()
FishingMvector_MIndex <<- 0
add.FishingMvector_M()
#print("PRIMA DI CREARE LA TABELLA DELLA f (MASCHI)!", quote=F)
#try(  print(FleetList_simulation[[1]]@fishingmortality.M.vector)  )
if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_F_lifespan))  
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[2,1])      
} 

first_age_mal <- 0

    n_ages <- n_ages - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)

  FishingMvector_M.model <<- gtkListStoreNew("gchararray",  rep("gdouble", n_ages), "gboolean")  
  for (i in 1:length(FishingMvector_M)) {
    iter <-  FishingMvector_M.model$append()$iter
     FishingMvector_M.model$set(iter,0, FishingMvector_M[[i]]$year)
    #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:n_ages) {
        FishingMvector_M.model$set(iter, e, as.double(FishingMvector_M[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     FishingMvector_M.model$set(iter,(n_ages+1),TRUE)
  } 
  
#print("DOPO AVER CREATO LA TABELLA DELLA f (MASCHI)!", quote=F)
#try( print(FleetList_simulation[[1]]@fishingmortality.M.vector) )

FishingMvector_M.treeview$destroy()
  
FishingMvector_M.treeview <<- gtkTreeViewNewWithModel( FishingMvector_M.model)
FishingMvector_M.treeview$setRulesHint(TRUE)
FishingMvector_M.treeview$getSelection()$setMode("single")
FishingMvector_M.add_columns( FishingMvector_M.treeview)
#FishingMvector_M.sw$destroy()
#FishingMvector_M.sw <<- gtkScrolledWindowNew(NULL, NULL)
#FishingMvector_M.sw$setShadowType("etched-in")
#FishingMvector_M.sw$setPolicy("automatic", "automatic")
#FishingMvector_M.sw$SetUsize(300, 200)  
FishingMvector_M.sw$add(FishingMvector_M.treeview)
#vboxFishingMtable_males$packStart(FishingMvector_M.sw , expand=FALSE, TRUE, 0)

#gtkBoxReorderChild(vboxFishingMtable_males, FishingMvector_M.sw, 3)
#gtkBoxReorderChild(vboxFishingMtable_males, hboxFishingMvector_Mfile_save, 3)