# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





hboxEnteringFishingMortality <- gtkHBox(homogeneous = FALSE)

hboxEnteringFishingMortality_overall <- gtkHBox(homogeneous = FALSE)

button_split_F <- gtkButtonNewWithLabel("Split F...")
button_split_F$AddCallback("clicked", split_F_button)

button_load_F_by_fleet <- gtkButtonNewWithLabel("Load F by fleet...")
button_load_F_by_fleet$AddCallback("clicked", loadFfromFile)

button_load_F_overall <- gtkButtonNewWithLabel("Load F overall...")
button_load_F_overall$AddCallback("clicked", loadF_overallfromFile)

button_saveall_Fmortalities_by_fleet <- gtkButtonNewWithLabel("Export F by fleet...")
button_saveall_Fmortalities_by_fleet$AddCallback("clicked", saveFtoFile)

button_saveall_Fmortalities_overall <- gtkButtonNewWithLabel("Export F overall...")
button_saveall_Fmortalities_overall$AddCallback("clicked", saveF_overalltoFile)

button_load_catch_by_age_for_splitting <- gtkButtonNewWithLabel("Load catch at age...")
button_load_catch_by_age_for_splitting$AddCallback("clicked", loadcatchAtAgefromFile)

button_save_catch_by_age_for_splitting <- gtkButtonNewWithLabel("Export catch at age...")
button_save_catch_by_age_for_splitting$AddCallback("clicked", savecatchAtAgetoFile)


tbl_F <- gtkTable(2,3,homogeneous = FALSE)
tbl_F$SetRowSpacings(4)
tbl_F$SetColSpacings(30)
tbl_F$SetBorderWidth(5) 

i=0
j=0   
tbl_F$Attach(gtkLabel("F to load"),i, i+1, j, j+1)  
j=j+1  
tbl_F$Attach(gtkLabel("F splitting method"),i, i+1, j, j+1)  

i=i+1
j=0   
tbl_F$Attach(radio_f_by_fleet,i, i+1, j, j+1) 
j=j+1  
tbl_F$Attach(radio_production_splitting,i, i+1, j, j+1)   

i=i+1
j=0   
tbl_F$Attach(radio_f_overall,i, i+1, j, j+1)
j=j+1  
tbl_F$Attach(radio_catch_by_age_splitting,i, i+1, j, j+1)    

i=i+1  # column 2
j=0                 
tbl_F$Attach(button_load_F_by_fleet ,i, i+1,  j, j+1)
j=j+1  
tbl_F$Attach(button_split_F,i, i+1, j, j+1)


i=i+1  # column 3
j=0
tbl_F$Attach(button_load_F_overall ,i, i+1,  j, j+1)
j=j+1  
tbl_F$Attach(button_load_catch_by_age_for_splitting,i, i+1, j, j+1) 

     

i=i+1  # column 2
j=0                 
tbl_F$Attach(button_saveall_Fmortalities_by_fleet ,i, i+1,  j, j+1)
j=j+1
tbl_F$Attach(button_save_catch_by_age_for_splitting,i, i+1, j, j+1) 

i=i+1  # column 3
j=0
tbl_F$Attach(button_saveall_Fmortalities_overall ,i, i+1,  j, j+1)


    
vboxFishingMortality$PackStart(tbl_F, expand = FALSE, fill = FALSE, padding = 10)

# insert table
FishingMvector_M.sw <<- gtkScrolledWindowNew(NULL, NULL)
FishingMvector_M.sw$setShadowType("etched-in")
FishingMvector_M.sw$setPolicy("automatic", "automatic")
FishingMvector_M.sw$SetUsize(450, 200)  


FishingMvector_M <<- list()
FishingMvector_MIndex <<- 0
# ------------------------------
# create model
# model <<- create.model()
FishingMvector_M.create_model()
# create tree view
FishingMvector_M.treeview <<- gtkTreeViewNewWithModel(FishingMvector_M.model)
FishingMvector_M.treeview$setRulesHint(TRUE)
FishingMvector_M.treeview$getSelection()$setMode("single")
FishingMvector_M.add_columns(FishingMvector_M.treeview)
FishingMvector_M.sw$add(FishingMvector_M.treeview) 
vboxFishingMtable_males <- gtkVBox(FALSE, 5)       
vboxFishingMtable_males$packStart(FishingMvector_M.sw , TRUE, TRUE)   

# insert table
FishingMvector_F.sw <<- gtkScrolledWindowNew(NULL, NULL)
FishingMvector_F.sw$setShadowType("etched-in")
FishingMvector_F.sw$setPolicy("automatic", "automatic")
FishingMvector_F.sw$SetUsize(450, 200)  


FishingMvector_F <<- list()
FishingMvector_FIndex <<- 0
# ------------------------------
# create model
# model <<- create.model()
FishingMvector_F.create_model()
# create tree view
FishingMvector_F.treeview <<- gtkTreeViewNewWithModel(FishingMvector_F.model)
FishingMvector_F.treeview$setRulesHint(TRUE)
FishingMvector_F.treeview$getSelection()$setMode("single")
FishingMvector_F.add_columns(FishingMvector_F.treeview)
FishingMvector_F.sw$add(FishingMvector_F.treeview) 
vboxFishingMtable_females <- gtkVBox(FALSE, 5)       
vboxFishingMtable_females$packStart(FishingMvector_F.sw , TRUE, TRUE)   

tbl_FishingMortality <- gtkTable(2,3,homogeneous = FALSE)

tbl_FishingMortality$SetRowSpacings(7)
tbl_FishingMortality$SetColSpacings(40)
tbl_FishingMortality$SetBorderWidth(5)

 i=0    # column 1
 j=0
 tbl_FishingMortality$Attach(gtkLabel("MALES"),i, i+1, j, j+1)
 j=j+1  
 tbl_FishingMortality$Attach(vboxFishingMtable_males,i, i+1, j, j+1)   



 i=i+1   # column 2
 j=0
 tbl_FishingMortality$Attach(gtkLabel("FEMALES"),i, i+1, j, j+1)
 j=j+1
 tbl_FishingMortality$Attach(vboxFishingMtable_females,i, i+1, j, j+1)   
 
 hboxEnteringFishingMortality$packStart(tbl_FishingMortality, expand = FALSE, fill = FALSE, padding=5) 
 
 
 
 
 
 # insert table
FishingMvector_M_overall.sw <<- gtkScrolledWindowNew(NULL, NULL)
FishingMvector_M_overall.sw$setShadowType("etched-in")
FishingMvector_M_overall.sw$setPolicy("automatic", "automatic")
FishingMvector_M_overall.sw$SetUsize(250, 200)  


FishingMvector_M_overall_list <<- list()
FishingMvector_M_overallIndex <<- 0
# ------------------------------
# create model
# model <<- create.model()
FishingMvector_M_overall.create_model()
# create tree view
FishingMvector_M_overall.treeview <<- gtkTreeViewNewWithModel(FishingMvector_M_overall.model)
FishingMvector_M_overall.treeview$setRulesHint(TRUE)
FishingMvector_M_overall.treeview$getSelection()$setMode("single")
FishingMvector_M_overall.add_columns(FishingMvector_M_overall.treeview)
FishingMvector_M_overall.sw$add(FishingMvector_M_overall.treeview) 
vboxFishingMvector_M_overall <- gtkVBox(FALSE, 5)       
vboxFishingMvector_M_overall$packStart(FishingMvector_M_overall.sw , TRUE, TRUE)  
 
 
 
 
# insert table
FishingMvector_F_overall.sw <<- gtkScrolledWindowNew(NULL, NULL)
FishingMvector_F_overall.sw$setShadowType("etched-in")
FishingMvector_F_overall.sw$setPolicy("automatic", "automatic")
FishingMvector_F_overall.sw$SetUsize(250, 200)  


FishingMvector_F_overall_list <<- list()
FishingMvector_F_overallIndex <<- 0
# ------------------------------
# create model
# model <<- create.model()
FishingMvector_F_overall.create_model()
# create tree view
FishingMvector_F_overall.treeview <<- gtkTreeViewNewWithModel(FishingMvector_F_overall.model)
FishingMvector_F_overall.treeview$setRulesHint(TRUE)
FishingMvector_F_overall.treeview$getSelection()$setMode("single")
FishingMvector_F_overall.add_columns(FishingMvector_F_overall.treeview)
FishingMvector_F_overall.sw$add(FishingMvector_F_overall.treeview) 
vboxFishingMvector_F_overall <- gtkVBox(FALSE, 5)       
vboxFishingMvector_F_overall$packStart(FishingMvector_F_overall.sw , TRUE, TRUE)    
 


 # insert table
catchAtAge_M.sw <<- gtkScrolledWindowNew(NULL, NULL)
catchAtAge_M.sw$setShadowType("etched-in")
catchAtAge_M.sw$setPolicy("automatic", "automatic")
catchAtAge_M.sw$SetUsize(250, 200)  


catchAtAgeM_list <<- list()
catchAtAgeM_index <<- 0
# ------------------------------
# create model
# model <<- create.model()
catchAtAge_M.create_model()
# create tree view
catchAtAge_M.treeview <<- gtkTreeViewNewWithModel(catchAtAge_M.model)
catchAtAge_M.treeview$setRulesHint(TRUE)
catchAtAge_M.treeview$getSelection()$setMode("single")
catchAtAge_M.add_columns(catchAtAge_M.treeview)
catchAtAge_M.sw$add(catchAtAge_M.treeview) 
vboxcatchAtAge_males <- gtkVBox(FALSE, 5)       
vboxcatchAtAge_males$packStart(catchAtAge_M.sw , TRUE, TRUE)   

# insert table
catchAtAge_F.sw <<- gtkScrolledWindowNew(NULL, NULL)
catchAtAge_F.sw$setShadowType("etched-in")
catchAtAge_F.sw$setPolicy("automatic", "automatic")
catchAtAge_F.sw$SetUsize(250, 200)  


catchAtAgeF_list <<- list()
catchAtAgeF_index <<- 0
# ------------------------------
# create model
# model <<- create.model()
catchAtAge_F.create_model()
# create tree view
catchAtAge_F.treeview <<- gtkTreeViewNewWithModel(catchAtAge_F.model)
catchAtAge_F.treeview$setRulesHint(TRUE)
catchAtAge_F.treeview$getSelection()$setMode("single")
catchAtAge_F.add_columns(catchAtAge_F.treeview)
catchAtAge_F.sw$add(catchAtAge_F.treeview) 
vboxcatchAtAge_females <- gtkVBox(FALSE, 5)       
vboxcatchAtAge_females$packStart(catchAtAge_F.sw , TRUE, TRUE)   









 tbl_FishingMortality_overall <- gtkTable(4,2,homogeneous = FALSE)

tbl_FishingMortality_overall$SetRowSpacings(7)
tbl_FishingMortality_overall$SetColSpacings(40)
tbl_FishingMortality_overall$SetBorderWidth(5)

 i=0    # column 1
 j = 0
 tbl_FishingMortality_overall$Attach(gtkLabel("Overall F"),i, i+2, j, j+1) 
 j=j+1
 tbl_FishingMortality_overall$Attach(gtkLabel("MALES"),i, i+1, j, j+1)
 j=j+1  
   j=j+1
 tbl_FishingMortality_overall$Attach(vboxFishingMvector_M_overall,i, i+1, j, j+1)   

 i=i+1   # column 2
 j=0
 j=j+1
 tbl_FishingMortality_overall$Attach(gtkLabel("FEMALES"),i, i+1, j, j+1)
 j=j+1
    j=j+1
 tbl_FishingMortality_overall$Attach(vboxFishingMvector_F_overall,i, i+1, j, j+1)   


 i=i+1    # column 3
 j = 0
 tbl_FishingMortality_overall$Attach(gtkLabel("Catch at age"),i, i+2, j, j+1) 
 j=j+1
 tbl_FishingMortality_overall$Attach(gtkLabel("MALES"),i, i+1, j, j+1)
 j=j+1  
   j=j+1
 tbl_FishingMortality_overall$Attach(vboxcatchAtAge_males,i, i+1, j, j+1)   

 i=i+1   # column 4
 j=0
 j=j+1
 tbl_FishingMortality_overall$Attach(gtkLabel("FEMALES"),i, i+1, j, j+1)
 j=j+1
    j=j+1
 tbl_FishingMortality_overall$Attach(vboxcatchAtAge_females,i, i+1, j, j+1)   





  hboxEnteringFishingMortality_overall$packStart(tbl_FishingMortality_overall, expand = FALSE, fill = FALSE, padding=5) 
 
# default: deactivated
  gtkWidgetSetSensitive(vboxFishingMortality, FALSE)
  gtkWidgetSetSensitive(button_load_F_by_fleet, FALSE)