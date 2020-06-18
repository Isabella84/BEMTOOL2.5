# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





 vboxSelectivity_Age <- gtkVBox()
 vboxSelectivity_params <- gtkVBox()
 vboxSelectivity_Length <- gtkVBox()
 
  hbox_selectivity_options <- gtkHBox()

radio_selectivity_params <- gtkRadioButton()
radio_selectivity_params$add(gtkLabel("function parameters"))
radio_selectivity_vector_age <- gtkRadioButtonNewWithLabelFromWidget(radio_selectivity_params, "vector by age")
radio_selectivity_vector_length <- gtkRadioButtonNewWithLabelFromWidget(radio_selectivity_params, "vector by length")

hbox_selectivity_options$packStart(gtkLabel("Selectivity information to use"), expand = F, fill = F, padding = 15)
hbox_selectivity_options$packStart(radio_selectivity_params, expand = F, fill = F, padding = 15)
hbox_selectivity_options$packStart(radio_selectivity_vector_age, expand = F, fill = F, padding = 15)
hbox_selectivity_options$packStart(radio_selectivity_vector_length, expand = F, fill = F, padding = 15)

gSignalConnect(radio_selectivity_params, "toggled", deactivate_activate_Selectivity_input_table)
gSignalConnect(radio_selectivity_vector_age, "toggled", deactivate_activate_Selectivity_input_table)
gSignalConnect(radio_selectivity_vector_length, "toggled", deactivate_activate_Selectivity_input_table)

#vboxSelectivity_params <- gtkVBox()

hboxTitleSel <- gtkHBox(FALSE, 5) 

tbl_fleets_selectivity <- gtkTable(1,7,homogeneous = FALSE)
tbl_fleets_selectivity$SetRowSpacings(10)
tbl_fleets_selectivity$SetColSpacings(30)
tbl_fleets_selectivity$SetBorderWidth(5)

#j=j+1  # row 2

j=0
i=0  # column 1
button_load_selectivity <- gtkButtonNewWithLabel("Load selectivity params...")
button_load_selectivity$AddCallback("clicked", loadSelectivityfromFile)
tbl_fleets_selectivity$Attach(button_load_selectivity ,i, i+1,  j, j+1)

i=i+1  # column 1.bis
button_load_selectivity_vect_age <- gtkButtonNewWithLabel("Load selectivity by age...")
button_load_selectivity_vect_age$AddCallback("clicked", loadSelectivityAgefromFile)
tbl_fleets_selectivity$Attach(button_load_selectivity_vect_age ,i, i+1,  j, j+1)

i=i+1  # column 1.ter
button_load_selectivity_vect_len <- gtkButtonNewWithLabel("Load selectivity by length...")
button_load_selectivity_vect_len$AddCallback("clicked", loadSelectivityLengthFromFile)
tbl_fleets_selectivity$Attach(button_load_selectivity_vect_len ,i, i+1,  j, j+1)



i=i+1  # column 8
button_saveall_selectivity <- gtkButtonNewWithLabel("Export selectivity params...")
button_saveall_selectivity$AddCallback("clicked", saveSelectivitytoFile)
tbl_fleets_selectivity$Attach(button_saveall_selectivity ,i, i+1,  j, j+1)

i=i+1  # column 8
button_saveall_selectivity_vect_age <- gtkButtonNewWithLabel("Export selectivity by age...")
button_saveall_selectivity_vect_age$AddCallback("clicked", saveSelectivityAgetoFile)
tbl_fleets_selectivity$Attach(button_saveall_selectivity_vect_age ,i, i+1,  j, j+1)


i=i+1  # column 8
button_saveall_selectivity_vect_len <- gtkButtonNewWithLabel("Export selectivity by length...")
button_saveall_selectivity_vect_len$AddCallback("clicked", saveSelectivityLengthtoFile)
tbl_fleets_selectivity$Attach(button_saveall_selectivity_vect_len ,i, i+1,  j, j+1)


hboxSelectivitytypes <- gtkHBox(FALSE, 5)
hboxSelectivity_input_descriptions <- gtkHBox(FALSE, 5)   

lbl_sel_types <- gtkLabel(paste("SELECTIVITY TYPE            1: ", SELECTIVITY_TYPE[1], "    2: ",  SELECTIVITY_TYPE[2], "    3: ",SELECTIVITY_TYPE[3], "    4: ",SELECTIVITY_TYPE[4], "    5: ",SELECTIVITY_TYPE[5], "    6: ",SELECTIVITY_TYPE[6], sep=""))
lbl_sel_description_input <-  gtkLabel("\n\t\t\t\t\t\tType 1:        1st = SL50%, 2nd = SL75%-SL25%\n\t\t\t\t\t\tType 2:        1st = SL50%, 2nd = SL75%-SL25%, 3rd = DL50%\n\t\t\t\t\t\tType 3:        1st = mean, 2nd = st.dev\n\t\t\t\t\t\tType 4:        1st = mean, 2nd = st.dev\n\t\t\t\t\t\tType 5:        1st = mean1, 2nd = st.dev1, 3rd = mean2, 4th = st.dev2, 5th = b\n\t\t\t\t\t\tType 6:        1st = mean1, 2nd = st.dev1, 3rd = st.dev2\n")
#                                                                            
hboxSelectivitytypes$packStart(lbl_sel_types , expand=F, F, 0) 
hboxSelectivity_input_descriptions$packStart(lbl_sel_description_input , expand=F, F, 0) 


vboxSelectivity_params$packStart(hboxSelectivitytypes , expand=F, F, 0) 
vboxSelectivity_params$packStart(hboxSelectivity_input_descriptions , expand=F, F, 0) 

  
selectivity.sw <<- gtkScrolledWindowNew(NULL, NULL)
selectivity.sw$setShadowType("etched-in")
selectivity.sw$setPolicy("automatic", "automatic")
selectivity.sw$SetUsize(100, 100)  
selectivities <<- list()
selectivitiesIndex <<- 0
# ------------------------------
# create model
selectivities.create_model()
# create tree view
selectivities.treeview <<- gtkTreeViewNewWithModel(selectivities.model)
selectivities.treeview$setRulesHint(TRUE)
selectivities.treeview$getSelection()$setMode("single")
selectivities.add_columns(selectivities.treeview)
selectivity.sw$add(selectivities.treeview)    
vboxSelectivity_params$packStart(selectivity.sw , expand=FALSE, TRUE, 0) 


# -------------------------------------------------------------------------------------------------- by AGE
hboxSelectivity_Age_F <- gtkHBox()
vboxSelectivity_Age_F <- gtkVBox()
   
SelectivityAge_F.sw <<- gtkScrolledWindowNew(NULL, NULL)
SelectivityAge_F.sw$setShadowType("etched-in")
SelectivityAge_F.sw$setPolicy("automatic", "automatic")
SelectivityAge_F.sw$SetUsize(100, 100)  
SelectivityAge_F_list <<- list()
SelectivityAge_FIndex <<- 0
# ------------------------------
# create model

SelectivityAge_F.create_model()  
SelectivityAge_F.treeview <<- gtkTreeViewNewWithModel(SelectivityAge_F.model)   
SelectivityAge_F.treeview$setRulesHint(TRUE)
SelectivityAge_F.treeview$getSelection()$setMode("single")
SelectivityAge_F.add_columns(SelectivityAge_F.treeview)
SelectivityAge_F.sw$add(SelectivityAge_F.treeview) 
#if (!is.null(SelectivityAgeF_matrix)) {}
# create tree view

   
vboxSelectivity_Age_F$packStart(gtkLabel("FEMALES") , expand=FALSE, TRUE, 0) 
vboxSelectivity_Age_F$packStart(SelectivityAge_F.sw , expand=FALSE, TRUE, 5) 

hboxSelectivity_Age <- gtkHBox()
vboxSelectivity_Age_M <- gtkVBox()
   
SelectivityAge_M.sw <<- gtkScrolledWindowNew(NULL, NULL)
SelectivityAge_M.sw$setShadowType("etched-in")
SelectivityAge_M.sw$setPolicy("automatic", "automatic")
SelectivityAge_M.sw$SetUsize(100, 100)  
SelectivityAge_M_list <<- list()
SelectivityAge_MIndex <<- 0
# ------------------------------
# create model

SelectivityAge_M.create_model()  
SelectivityAge_M.treeview <<- gtkTreeViewNewWithModel(SelectivityAge_M.model) 
SelectivityAge_M.treeview$setRulesHint(TRUE)
SelectivityAge_M.treeview$getSelection()$setMode("single")
SelectivityAge_M.add_columns(SelectivityAge_M.treeview)
SelectivityAge_M.sw$add(SelectivityAge_M.treeview)    
#if (!is.null(SelectivityAgeM_matrix)) {}
# create tree view



vboxSelectivity_Age_M$packStart(gtkLabel("MALES") , expand=FALSE, TRUE, 0) 
vboxSelectivity_Age_M$packStart(SelectivityAge_M.sw , expand=FALSE, TRUE, 5) 

hboxSelectivity_Age$packStart(vboxSelectivity_Age_F, expand=TRUE, TRUE, 5)
hboxSelectivity_Age$packStart(vboxSelectivity_Age_M, expand=TRUE, TRUE, 5)
vboxSelectivity_Age$packStart(hboxSelectivity_Age , expand=FALSE, FALSE, 0) 


# --------------------------------------------------------------------------------------- by LENGTH

hboxSelectivity_Length_F <- gtkHBox()
vboxSelectivity_Length_F <- gtkVBox()
   
SelectivityLength_F.sw <<- gtkScrolledWindowNew(NULL, NULL)
SelectivityLength_F.sw$setShadowType("etched-in")
SelectivityLength_F.sw$setPolicy("automatic", "automatic")
SelectivityLength_F.sw$SetUsize(100, 100)  
SelectivityLength_F_list <<- list()
SelectivityLength_FIndex <<- 0
# ------------------------------
# create model

SelectivityLength_F.create_model()  
SelectivityLength_F.treeview <<- gtkTreeViewNewWithModel(SelectivityLength_F.model)   
SelectivityLength_F.treeview$setRulesHint(TRUE)
SelectivityLength_F.treeview$getSelection()$setMode("single")
SelectivityLength_F.add_columns(SelectivityLength_F.treeview)
SelectivityLength_F.sw$add(SelectivityLength_F.treeview) 
#if (!is.null(SelectivityAgeF_matrix)) {}
# create tree view

   
vboxSelectivity_Length_F$packStart(gtkLabel("FEMALES") , expand=FALSE, TRUE, 0) 
vboxSelectivity_Length_F$packStart(SelectivityLength_F.sw , expand=FALSE, TRUE, 5) 

hboxSelectivity_Length <- gtkHBox()
vboxSelectivity_Length_M <- gtkVBox()
   
SelectivityLength_M.sw <<- gtkScrolledWindowNew(NULL, NULL)
SelectivityLength_M.sw$setShadowType("etched-in")
SelectivityLength_M.sw$setPolicy("automatic", "automatic")
SelectivityLength_M.sw$SetUsize(100, 100)  
SelectivityLength_M_list <<- list()
SelectivityLength_MIndex <<- 0
# ------------------------------
# create model

SelectivityLength_M.create_model()  
SelectivityLength_M.treeview <<- gtkTreeViewNewWithModel(SelectivityLength_M.model) 
SelectivityLength_M.treeview$setRulesHint(TRUE)
SelectivityLength_M.treeview$getSelection()$setMode("single")
SelectivityLength_M.add_columns(SelectivityLength_M.treeview)
SelectivityLength_M.sw$add(SelectivityLength_M.treeview)    
#if (!is.null(SelectivityAgeM_matrix)) {}
# create tree view


vboxSelectivity_Length_M$packStart(gtkLabel("MALES") , expand=FALSE, TRUE, 0) 
vboxSelectivity_Length_M$packStart(SelectivityLength_M.sw , expand=FALSE, TRUE, 5) 

hboxSelectivity_Length$packStart(vboxSelectivity_Length_F, expand=TRUE, TRUE, 5)
hboxSelectivity_Length$packStart(vboxSelectivity_Length_M, expand=TRUE, TRUE, 5)
vboxSelectivity_Length$packStart(hboxSelectivity_Length , expand=FALSE, FALSE, 0) 

# ---------------------------------------------------------------------------------------------------------



frame_1_selparams <- gtkFrame(" Selectivity by parameters ")   
hbox_frame_1_selparams <- gtkHBox(homogeneous = FALSE, 5)
vbox_frame_1_selparams <- gtkVBox(homogeneous = FALSE, 5)             

hbox_frame_1_selparams$packStart(vboxSelectivity_params, expand = TRUE, fill = TRUE, padding = 10)
vbox_frame_1_selparams$packStart(hbox_frame_1_selparams, expand = TRUE, fill = TRUE, padding = 5)
frame_1_selparams$add(vbox_frame_1_selparams) 

frame_2_selAge <- gtkFrame(" Selectivity vector by age ")   
hbox_frame_2_selAge <- gtkHBox(homogeneous = FALSE, 5)
vbox_frame_2_selAge <- gtkVBox(homogeneous = FALSE, 5)             

hbox_frame_2_selAge$packStart(vboxSelectivity_Age, expand = TRUE, fill = TRUE, padding = 10)
vbox_frame_2_selAge$packStart(hbox_frame_2_selAge, expand = TRUE, fill = TRUE, padding = 5)
frame_2_selAge$add(vbox_frame_2_selAge) 


frame_3_selLength <- gtkFrame(" Selectivity vector by length ")   
hbox_frame_3_selLength <- gtkHBox(homogeneous = FALSE, 5)
vbox_frame_3_selLength <- gtkVBox(homogeneous = FALSE, 5)             

hbox_frame_3_selLength$packStart(vboxSelectivity_Length, expand = TRUE, fill = TRUE, padding = 10)
vbox_frame_3_selLength$packStart(hbox_frame_3_selLength, expand = TRUE, fill = TRUE, padding = 5)
frame_3_selLength$add(vbox_frame_3_selLength)


# per aggiungere tutto
vboxFisherySelectivity$packStart(hbox_selectivity_options, expand=FALSE, TRUE, 0) 
vboxFisherySelectivity$packStart(tbl_fleets_selectivity , expand=F, F, 0) 
vboxFisherySelectivity$packStart(frame_1_selparams, expand=FALSE, TRUE, 0) 
vboxFisherySelectivity$packStart(frame_2_selAge, expand=FALSE, TRUE, 0) 
vboxFisherySelectivity$packStart(frame_3_selLength, expand=FALSE, TRUE, 0) 