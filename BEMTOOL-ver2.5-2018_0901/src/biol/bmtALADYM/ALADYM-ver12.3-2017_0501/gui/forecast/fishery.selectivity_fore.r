# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


 vboxSelectivity_Age_fore <- gtkVBox()
 vboxSelectivity_params_fore <- gtkVBox()
 vboxSelectivity_Length_fore <- gtkVBox()



tbl_fleets_fore_sel <- gtkTable(2,2,homogeneous = FALSE)
tbl_fleets_fore_sel$SetRowSpacings(10)
tbl_fleets_fore_sel$SetColSpacings(10)
tbl_fleets_fore_sel$SetBorderWidth(5)


i=0 # column 1
j=0
button_load_selectivity_fore <- gtkButtonNewWithLabel("Load selectivity params...")
button_load_selectivity_fore$AddCallback("clicked", loadSelectivityfromFile_fore)
tbl_fleets_fore_sel$Attach(button_load_selectivity_fore ,i, i+1,  j, j+1)

i=i+1
button_load_selectivity_vect_age_fore <- gtkButtonNewWithLabel("Load selectivity by age...")
button_load_selectivity_vect_age_fore$AddCallback("clicked", loadSelectivityAgefromFile_fore)
tbl_fleets_fore_sel$Attach(button_load_selectivity_vect_age_fore ,i, i+1,  j, j+1)

i=i+1  # column 1.ter
button_load_selectivity_vect_len_fore <- gtkButtonNewWithLabel("Load selectivity by length...")
button_load_selectivity_vect_len_fore$AddCallback("clicked", loadSelectivityLengthFromFile_fore)
tbl_fleets_fore_sel$Attach(button_load_selectivity_vect_len_fore ,i, i+1,  j, j+1)

i=i+1
button_saveall_selectivity_fore <- gtkButtonNewWithLabel("Export selectivity params...")
button_saveall_selectivity_fore$AddCallback("clicked", saveSelectivitytoFile_fore)
tbl_fleets_fore_sel$Attach(button_saveall_selectivity_fore ,i, i+1,  j, j+1)

i=i+1
button_saveall_selectivity_vect_age_fore <- gtkButtonNewWithLabel("Export selectivity by age...")
button_saveall_selectivity_vect_age_fore$AddCallback("clicked", saveSelectivityAgetoFile_fore)
tbl_fleets_fore_sel$Attach(button_saveall_selectivity_vect_age_fore ,i, i+1,  j, j+1)

i=i+1  # column 8
button_saveall_selectivity_vect_len_fore <- gtkButtonNewWithLabel("Export selectivity by length...")
button_saveall_selectivity_vect_len_fore$AddCallback("clicked", saveSelectivityLengthtoFile_fore)
tbl_fleets_fore_sel$Attach(button_saveall_selectivity_vect_len_fore ,i, i+1,  j, j+1)

vboxFisherySelectivity_fore$packStart(tbl_fleets_fore_sel, expand = FALSE, fill = FALSE, padding=0)



hboxSelectivitytypes_fore <- gtkHBox(FALSE, 5)
hboxSelectivity_input_descriptions_fore <- gtkHBox(FALSE, 5)   

lbl_sel_types_fore <- gtkLabel(paste("SELECTIVITY TYPE            1: ", SELECTIVITY_TYPE[1], "    2: ",  SELECTIVITY_TYPE[2], "    3: ",SELECTIVITY_TYPE[3], "    4: ",SELECTIVITY_TYPE[4], "    5: ",SELECTIVITY_TYPE[5], "    6: ",SELECTIVITY_TYPE[6], sep=""))
lbl_sel_description_input_fore <-  gtkLabel("\n\t\t\t\t\t\tType 1:        1st = SL50%, 2nd = SL75%-SL25%\n\t\t\t\t\t\tType 2:        1st = SL50%, 2nd = SL75%-SL25%, 3rd = DL50%\n\t\t\t\t\t\tType 3:        1st = mean, 2nd = st.dev\n\t\t\t\t\t\tType 4:        1st = mean, 2nd = st.dev\n\t\t\t\t\t\tType 5:        1st = mean1, 2nd = st.dev1, 3rd = mean2, 4th = st.dev2, 5th = b\n\t\t\t\t\t\tType 6:        1st = mean1, 2nd = st.dev1, 3rd = st.dev2\n")
#                                                                            
hboxSelectivitytypes_fore$packStart(lbl_sel_types_fore , expand=F, F, 0) 
hboxSelectivity_input_descriptions_fore$packStart(lbl_sel_description_input_fore , expand=F, F, 0) 
vboxFisherySelectivity_fore$packStart(hboxSelectivitytypes_fore , expand=F, F, 0) 
vboxFisherySelectivity_fore$packStart(hboxSelectivity_input_descriptions_fore , expand=F, F, 0) 

  
selectivities_fore.sw <<- gtkScrolledWindowNew(NULL, NULL)
selectivities_fore.sw$setShadowType("etched-in")
selectivities_fore.sw$setPolicy("automatic", "automatic")
selectivities_fore.sw$SetUsize(100, 70)  
selectivities_fore <<- list()
selectivities_foreIndex <<- 0
# ------------------------------
# create model
selectivities_fore.create_model()
# create tree view
selectivities_fore.treeview <<- gtkTreeViewNewWithModel(selectivities_fore.model)
selectivities_fore.treeview$setRulesHint(TRUE)
selectivities_fore.treeview$getSelection()$setMode("single")
selectivities_fore.add_columns(selectivities_fore.treeview)
selectivities_fore.sw$add(selectivities_fore.treeview)    

vboxSelectivity_params_fore$packStart(selectivities_fore.sw , expand = FALSE, TRUE, padding =0) 
#vboxFisherySelectivity_fore$packStart(selectivities_fore.sw , expand = FALSE, TRUE, padding =10) 



# -------------------------------------------------------------------------------------------------- by AGE

hboxSelectivity_Age_fore <- gtkHBox()
hboxSelectivity_Age_F_fore <- gtkHBox()
vboxSelectivity_Age_F_fore <- gtkVBox()

SelectivityAge_F_fore.sw <<- gtkScrolledWindowNew(NULL, NULL)
SelectivityAge_F_fore.sw$setShadowType("etched-in")
SelectivityAge_F_fore.sw$setPolicy("automatic", "automatic")
SelectivityAge_F_fore.sw$SetUsize(100, 70)  
SelectivityAge_F_fore_list <<- list()
SelectivityAge_F_foreIndex <<- 0
# ------------------------------
# create model
SelectivityAge_F_fore.create_model()
# create tree view
SelectivityAge_F_fore.treeview <<- gtkTreeViewNewWithModel(SelectivityAge_F_fore.model)
SelectivityAge_F_fore.treeview$setRulesHint(TRUE)
SelectivityAge_F_fore.treeview$getSelection()$setMode("single")
SelectivityAge_F_fore.add_columns(SelectivityAge_F_fore.treeview)
SelectivityAge_F_fore.sw$add(SelectivityAge_F_fore.treeview)    

vboxSelectivity_Age_F_fore$packStart(gtkLabel("FEMALES") , expand=FALSE, TRUE, 0) 
vboxSelectivity_Age_F_fore$packStart(SelectivityAge_F_fore.sw , expand=FALSE, TRUE, 0) 

hboxSelectivity_Age <- gtkHBox()


vboxSelectivity_Age_M_fore <- gtkVBox()
   
SelectivityAge_M_fore.sw <<- gtkScrolledWindowNew(NULL, NULL)
SelectivityAge_M_fore.sw$setShadowType("etched-in")
SelectivityAge_M_fore.sw$setPolicy("automatic", "automatic")
SelectivityAge_M_fore.sw$SetUsize(100, 70)  
SelectivityAge_M_fore_list <<- list()
SelectivityAge_M_foreIndex <<- 0
# ------------------------------
# create model
SelectivityAge_M_fore.create_model()
# create tree view
SelectivityAge_M_fore.treeview <<- gtkTreeViewNewWithModel(SelectivityAge_M_fore.model)
SelectivityAge_M_fore.treeview$setRulesHint(TRUE)
SelectivityAge_M_fore.treeview$getSelection()$setMode("single")
SelectivityAge_M_fore.add_columns(SelectivityAge_M_fore.treeview)
SelectivityAge_M_fore.sw$add(SelectivityAge_M_fore.treeview)    

vboxSelectivity_Age_M_fore$packStart(gtkLabel("MALES") , expand=FALSE, TRUE, 0) 
vboxSelectivity_Age_M_fore$packStart(SelectivityAge_M_fore.sw , expand=FALSE, TRUE, 0) 

hboxSelectivity_Age_fore$packStart(vboxSelectivity_Age_F_fore, expand=TRUE, TRUE, 5)
hboxSelectivity_Age_fore$packStart(vboxSelectivity_Age_M_fore, expand=TRUE, TRUE, 5)
vboxSelectivity_Age_fore$packStart(hboxSelectivity_Age_fore , expand=FALSE, FALSE, 0) 


# -------------------------------------------------------------------------------------------------- by LENGTH



hboxSelectivity_Length_fore <- gtkHBox()
hboxSelectivity_Length_F_fore <- gtkHBox()
vboxSelectivity_Length_F_fore <- gtkVBox()

SelectivityLength_F_fore.sw <<- gtkScrolledWindowNew(NULL, NULL)
SelectivityLength_F_fore.sw$setShadowType("etched-in")
SelectivityLength_F_fore.sw$setPolicy("automatic", "automatic")
SelectivityLength_F_fore.sw$SetUsize(100, 70)  
SelectivityLength_F_fore_list <<- list()
SelectivityLength_F_fore_foreIndex <<- 0
# ------------------------------
# create model
SelectivityLength_F_fore.create_model()
# create tree view
SelectivityLength_F_fore.treeview <<- gtkTreeViewNewWithModel(SelectivityLength_F_fore.model)
SelectivityLength_F_fore.treeview$setRulesHint(TRUE)
SelectivityLength_F_fore.treeview$getSelection()$setMode("single")
SelectivityLength_F_fore.add_columns(SelectivityLength_F_fore.treeview)
SelectivityLength_F_fore.sw$add(SelectivityLength_F_fore.treeview)    

vboxSelectivity_Length_F_fore$packStart(gtkLabel("FEMALES") , expand=FALSE, TRUE, 0) 
vboxSelectivity_Length_F_fore$packStart(SelectivityLength_F_fore.sw , expand=FALSE, TRUE, 0) 

hboxSelectivity_Length <- gtkHBox()


vboxSelectivity_Length_M_fore <- gtkVBox()
   
SelectivityLength_M_fore.sw <<- gtkScrolledWindowNew(NULL, NULL)
SelectivityLength_M_fore.sw$setShadowType("etched-in")
SelectivityLength_M_fore.sw$setPolicy("automatic", "automatic")
SelectivityLength_M_fore.sw$SetUsize(100, 70)  
SelectivityLength_M_fore_list <<- list()
SelectivityLength_M_foreIndex <<- 0
# ------------------------------
# create model
SelectivityLength_M_fore.create_model()
# create tree view
SelectivityLength_M_fore.treeview <<- gtkTreeViewNewWithModel(SelectivityLength_M_fore.model)
SelectivityLength_M_fore.treeview$setRulesHint(TRUE)
SelectivityLength_M_fore.treeview$getSelection()$setMode("single")
SelectivityLength_M_fore.add_columns(SelectivityLength_M_fore.treeview)
SelectivityLength_M_fore.sw$add(SelectivityLength_M_fore.treeview)    

vboxSelectivity_Length_M_fore$packStart(gtkLabel("MALES") , expand=FALSE, TRUE, 0) 
vboxSelectivity_Length_M_fore$packStart(SelectivityLength_M_fore.sw , expand=FALSE, TRUE, 0) 

hboxSelectivity_Length_fore$packStart(vboxSelectivity_Length_F_fore, expand=TRUE, TRUE, 5)
hboxSelectivity_Length_fore$packStart(vboxSelectivity_Length_M_fore, expand=TRUE, TRUE, 5)
vboxSelectivity_Length_fore$packStart(hboxSelectivity_Length_fore , expand=FALSE, FALSE, 0) 


# --------------------------------------------------------------------------------------------------------------



frame_1_selparams_fore <- gtkFrame(" Selectivity by parameters ")   
hbox_frame_1_selparams_fore <- gtkHBox(homogeneous = FALSE, 5)
vbox_frame_1_selparams_fore <- gtkVBox(homogeneous = FALSE, 5)             

hbox_frame_1_selparams_fore$packStart(vboxSelectivity_params_fore, expand = TRUE, fill = TRUE, padding = 10)
vbox_frame_1_selparams_fore$packStart(hbox_frame_1_selparams_fore, expand = TRUE, fill = TRUE, padding = 5)
frame_1_selparams_fore$add(vbox_frame_1_selparams_fore) 

frame_2_selAge_fore <- gtkFrame(" Selectivity vector by age ")   
hbox_frame_2_selAge_fore <- gtkHBox(homogeneous = FALSE, 5)
vbox_frame_2_selAge_fore <- gtkVBox(homogeneous = FALSE, 5)             

hbox_frame_2_selAge_fore$packStart(vboxSelectivity_Age_fore, expand = TRUE, fill = TRUE, padding = 10)
vbox_frame_2_selAge_fore$packStart(hbox_frame_2_selAge_fore, expand = TRUE, fill = TRUE, padding = 5)
frame_2_selAge_fore$add(vbox_frame_2_selAge_fore) 


frame_3_selLength_fore <- gtkFrame(" Selectivity vector by length ")   
hbox_frame_3_selLength_fore <- gtkHBox(homogeneous = FALSE, 5)
vbox_frame_3_selLength_fore <- gtkVBox(homogeneous = FALSE, 5)             

hbox_frame_3_selLength_fore$packStart(vboxSelectivity_Length_fore, expand = TRUE, fill = TRUE, padding = 10)
vbox_frame_3_selLength_fore$packStart(hbox_frame_3_selLength_fore, expand = TRUE, fill = TRUE, padding = 5)
frame_3_selLength_fore$add(vbox_frame_3_selLength_fore)


vboxFisherySelectivity_fore$packStart(tbl_fleets_fore_sel , expand=F, F, 0) 
vboxFisherySelectivity_fore$packStart(frame_1_selparams_fore, expand=FALSE, TRUE, 0) 
vboxFisherySelectivity_fore$packStart(frame_2_selAge_fore, expand=FALSE, TRUE, 0) 
vboxFisherySelectivity_fore$packStart(frame_3_selLength_fore, expand=FALSE, TRUE, 0) 

#hboxSelectivityfile_save_fore <- gtkHBox(FALSE, 5)
#btn_browse_fore <- gtkButton()
#gtkButtonSetLabel(btn_browse_fore, "Save selectivity parameters...")
#btn_browse_fore$AddCallback("clicked", save_file_selectivity_vector_fore)
#hboxSelectivityfile_save_fore$packStart(btn_browse_fore, expand = FALSE, fill = FALSE, padding = 5)
# vboxFisherySelectivity_fore$packStart(hboxSelectivityfile_save_fore, expand = FALSE, fill = FALSE, padding =5)