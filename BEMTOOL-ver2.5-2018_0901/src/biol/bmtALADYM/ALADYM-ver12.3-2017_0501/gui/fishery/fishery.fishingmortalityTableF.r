# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





#------------------------------------------ load the file  (FEMALE)
FishingMvector_F <<- list()
FishingMvector_FIndex <<- 0
add.FishingMvector_F()
 # try( print(FleetList_simulation[[1]]@fishingmortality.F.vector) )
if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
  n_ages <- as.numeric(gtkEntryGetText(entryVBF_F_lifespan))  
} else {
  n_ages <- as.numeric(new_aldPopulation@lifespan[2,1])      
} 
#

    n_ages <- n_ages - trunc(Tr/12)
    first_age_fem <- trunc(Tr/12)

  FishingMvector_F.model <<- gtkListStoreNew("gchararray",  rep("gdouble", n_ages), "gboolean")  
  for (i in 1:length(FishingMvector_F)) {
    iter <-  FishingMvector_F.model$append()$iter
     FishingMvector_F.model$set(iter,0, FishingMvector_F[[i]]$year)
    #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:n_ages) {
        FishingMvector_F.model$set(iter, e, as.double(FishingMvector_F[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     FishingMvector_F.model$set(iter,(n_ages+1),TRUE)
  } 
 
  #  print("DOPO AVER CREATO LA TABELLA DELLA f (FEMMINE)!", quote=F)
#try( print(FleetList_simulation[[1]]@fishingmortality.F.vector)  )

FishingMvector_F.treeview$destroy()

FishingMvector_F.treeview <<- gtkTreeViewNewWithModel( FishingMvector_F.model)
FishingMvector_F.treeview$setRulesHint(TRUE)
FishingMvector_F.treeview$getSelection()$setMode("single")
FishingMvector_F.add_columns( FishingMvector_F.treeview)
#FishingMvector_F.sw$destroy()
#FishingMvector_F.sw <<- gtkScrolledWindowNew(NULL, NULL)
#FishingMvector_F.sw$setShadowType("etched-in")
#FishingMvector_F.sw$setPolicy("automatic", "automatic")
#FishingMvector_F.sw$SetUsize(300, 200)  
FishingMvector_F.sw$add(FishingMvector_F.treeview)
#vboxFishingMtable_females$packStart(FishingMvector_F.sw , expand=FALSE, TRUE, 0)

#gtkBoxReorderChild(vboxFishingMtable_females, FishingMvector_F.sw, 3)
#gtkBoxReorderChild(vboxFishingMtable_females, hboxFishingMvector_Ffile_save, 3)
