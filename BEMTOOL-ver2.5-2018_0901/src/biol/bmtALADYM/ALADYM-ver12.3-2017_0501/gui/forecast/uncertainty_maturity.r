 # ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




 # COSTRUZIONE OGGETTI 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같
 
 
 # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   FINESTRE CON DATI 


# for k parameters for uncertainty on growth ----------------------------------------------
maturity_uncert_males_from_file_sw <<- gtkScrolledWindowNew(NULL, NULL)
maturity_uncert_males_from_file_sw$setShadowType("etched-in")
maturity_uncert_males_from_file_sw$setPolicy("automatic", "automatic")
maturity_uncert_males_from_file_sw$SetUsize(100, dim_big_tables)  
maturity_uncert_males_from_file_list <<- list()
maturity_uncert_males_from_file_index <<- 0
maturity_uncert_males_from_file_create_model()
maturity_uncert_males_from_file_treeview <<- gtkTreeViewNewWithModel( maturity_uncert_males_from_file_model)
maturity_uncert_males_from_file_treeview$setRulesHint(TRUE)
maturity_uncert_males_from_file_treeview$getSelection()$setMode("single")
maturity_uncert_males_from_file_add_columns( maturity_uncert_males_from_file_treeview)
maturity_uncert_males_from_file_sw$destroy()
maturity_uncert_males_from_file_sw <<- gtkScrolledWindowNew(NULL, NULL)
maturity_uncert_males_from_file_sw$setShadowType("etched-in")
maturity_uncert_males_from_file_sw$setPolicy("automatic", "automatic")
maturity_uncert_males_from_file_sw$SetUsize(100, dim_big_tables)  
maturity_uncert_males_from_file_sw$add(maturity_uncert_males_from_file_treeview)
# ---------------------------------------------------------------------------------


# for k parameters for uncertainty on growth ----------------------------------------------
maturity_uncert_females_from_file_sw <<- gtkScrolledWindowNew(NULL, NULL)
maturity_uncert_females_from_file_sw$setShadowType("etched-in")
maturity_uncert_females_from_file_sw$setPolicy("automatic", "automatic")
maturity_uncert_females_from_file_sw$SetUsize(100, dim_big_tables)  
maturity_uncert_females_from_file_list <<- list()
maturity_uncert_females_from_file_index <<- 0
maturity_uncert_females_from_file_create_model()
maturity_uncert_females_from_file_treeview <<- gtkTreeViewNewWithModel( maturity_uncert_females_from_file_model)
maturity_uncert_females_from_file_treeview$setRulesHint(TRUE)
maturity_uncert_females_from_file_treeview$getSelection()$setMode("single")
maturity_uncert_females_from_file_add_columns( maturity_uncert_females_from_file_treeview)
maturity_uncert_females_from_file_sw$destroy()
maturity_uncert_females_from_file_sw <<- gtkScrolledWindowNew(NULL, NULL)
maturity_uncert_females_from_file_sw$setShadowType("etched-in")
maturity_uncert_females_from_file_sw$setPolicy("automatic", "automatic")
maturity_uncert_females_from_file_sw$SetUsize(100, dim_big_tables)  
maturity_uncert_females_from_file_sw$add(maturity_uncert_females_from_file_treeview)
# ---------------------------------------------------------------------------------


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   CHECKS 


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   COMBO 


combo_distr_maturity_uncert_males <- gtkComboBoxNewText()
for (choice in DISTRIBUTION_UNCERT) { combo_distr_maturity_uncert_males$appendText(choice) }

combo_distr_maturity_uncert_females <- gtkComboBoxNewText()
for (choice in DISTRIBUTION_UNCERT) { combo_distr_maturity_uncert_females$appendText(choice) }

gSignalConnect(combo_distr_maturity_uncert_males, "changed", change_label_maturity_uncert_distribution_males )
gSignalConnect(combo_distr_maturity_uncert_females, "changed", change_label_maturity_uncert_distribution_females)


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   BUTTON 

btn_load_maturity_uncert_males_from_file <- gtkButton()
btn_load_maturity_uncert_females_from_file <- gtkButton()

gtkButtonSetLabel(btn_load_maturity_uncert_males_from_file, "Load...")
gtkButtonSetLabel(btn_load_maturity_uncert_females_from_file, "Load...")

btn_load_maturity_uncert_males_from_file$AddCallback("clicked", load_maturity_uncert_males_from_file)
btn_load_maturity_uncert_females_from_file$AddCallback("clicked", load_maturity_uncert_females_from_file)


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   LABEL 

lbl_A_distr_maturity_uncert_males <- gtkLabel("A")  
lbl_B_distr_maturity_uncert_males <- gtkLabel("B")
lbl_A_distr_maturity_uncert_females <- gtkLabel("A")  
lbl_B_distr_maturity_uncert_females <- gtkLabel("B")

gtkLabelSetWidthChars(lbl_A_distr_maturity_uncert_males, LABEL_LENGTH) 
gtkLabelSetWidthChars(lbl_B_distr_maturity_uncert_males, LABEL_LENGTH) 
gtkLabelSetWidthChars(lbl_A_distr_maturity_uncert_females, LABEL_LENGTH) 
gtkLabelSetWidthChars(lbl_B_distr_maturity_uncert_females, LABEL_LENGTH) 


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   ENTRY 

entry_A_distr_maturity_uncert_males_L50 <- gtkEntry()
entry_B_distr_maturity_uncert_males_L50 <- gtkEntry()
entry_A_distr_maturity_uncert_females_L50 <- gtkEntry()
entry_B_distr_maturity_uncert_females_L50 <- gtkEntry()

entry_A_distr_maturity_uncert_males_MR <- gtkEntry()
entry_B_distr_maturity_uncert_males_MR <- gtkEntry()
entry_A_distr_maturity_uncert_females_MR <- gtkEntry()
entry_B_distr_maturity_uncert_females_MR <- gtkEntry()

entry_costant_males_L50 <- gtkEntry()
entry_costant_males_MR <- gtkEntry()
entry_costant_females_L50 <- gtkEntry()
entry_costant_females_MR <- gtkEntry()

if (phase=="FORECAST") {
gtkEntrySetText(entry_costant_males_L50, BAS$M_mean_L50 )
gtkEntrySetText(entry_costant_females_L50, BAS$F_mean_L50 )
gtkEntrySetText(entry_costant_males_MR, BAS$M_mean_MR )
gtkEntrySetText(entry_costant_females_MR, BAS$F_mean_MR )
}

gtkEntrySetWidthChars(entry_A_distr_maturity_uncert_males_L50, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entry_B_distr_maturity_uncert_males_L50, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entry_A_distr_maturity_uncert_females_L50, NUMERICAL_ENTRY_LENGTH)
gtkEntrySetWidthChars(entry_B_distr_maturity_uncert_females_L50, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entry_A_distr_maturity_uncert_males_MR, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entry_B_distr_maturity_uncert_males_MR, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entry_A_distr_maturity_uncert_females_MR, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entry_B_distr_maturity_uncert_females_MR, NUMERICAL_ENTRY_LENGTH) 

gtkEntrySetWidthChars(entry_costant_males_L50, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entry_costant_females_L50, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entry_costant_males_MR, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entry_costant_females_MR, NUMERICAL_ENTRY_LENGTH) 


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   RADIO 

radio_maturity_uncert_males_from_distribution <- gtkRadioButton()
radio_maturity_uncert_males_from_distribution$add(gtkLabel("from distribution"))
radio_maturity_uncert_males_from_external_file <- gtkRadioButtonNewWithLabelFromWidget(radio_maturity_uncert_males_from_distribution, "from external file")

radio_maturity_uncert_females_from_distribution <- gtkRadioButton()
radio_maturity_uncert_females_from_distribution$add(gtkLabel("from distribution"))
radio_maturity_uncert_females_from_external_file <- gtkRadioButtonNewWithLabelFromWidget(radio_maturity_uncert_females_from_distribution, "from external file")

gSignalConnect(radio_maturity_uncert_males_from_distribution, "toggled", deactivate_maturity_uncertainty_males_distr_extfile)
gSignalConnect(radio_maturity_uncert_males_from_external_file, "toggled", deactivate_maturity_uncertainty_males_distr_extfile)

gSignalConnect(radio_maturity_uncert_females_from_distribution, "toggled", deactivate_maturity_uncertainty_females_distr_extfile)
gSignalConnect(radio_maturity_uncert_females_from_external_file, "toggled", deactivate_maturity_uncertainty_females_distr_extfile)

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   HORIZONTAL BOX 

# hbox_growth_costant_params <- gtkHBox(FALSE, 5)
hbox_maturity_uncert_frames <- gtkHBox(FALSE, 5)
h_frame_maturity_uncert_males <- gtkHBox(FALSE, 5)
h_frame_maturity_uncert_females <- gtkHBox(FALSE, 5)
hbox_maturity_costant_params <-  gtkHBox(FALSE, 5)
hbox_males_radio_plus_load <- gtkHBox()
hbox_males_radio_plus_combo <- gtkHBox()
hbox_females_radio_plus_load <- gtkHBox()
hbox_females_radio_plus_combo <- gtkHBox()


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   VERTICAL BOX 

 vbox_global_maturity_uncert <- gtkVBox(FALSE, 5)
vbox_maturity_mean_devSt_males <- gtkVBox(FALSE, 5)
vbox_maturity_mean_devSt_females <- gtkVBox(FALSE, 5)


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   FRAME 

frame_maturity_uncert_males <- gtkFrame(" Error on MALE ")  
frame_maturity_uncert_females <- gtkFrame(" Error on FEMALE ")  
       

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   TABELLE 

tbl_maturity_costant_params <- gtkTable(3,3,homogeneous = TRUE)
tbl_maturity_settings_params <- gtkTable(1,2,homogeneous = FALSE)
tbl_maturity_mean_devSt_males <-  gtkTable(3, 3,homogeneous = FALSE)
tbl_maturity_mean_devSt_females <-  gtkTable(3, 3,homogeneous = FALSE)
tbl_maturity_males_settings <-  gtkTable(2, 3,homogeneous = FALSE)
tbl_maturity_females_settings <-  gtkTable(2, 3,homogeneous = FALSE)



# ----------------------------------  tbl_maturity_males_settings ----------------------------------------------- 
tbl_maturity_males_settings$SetRowSpacings(5)
tbl_maturity_males_settings$SetColSpacings(5)
tbl_maturity_males_settings$SetBorderWidth(5)
i=0   	# C0  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_maturity_males_settings$Attach(gtkLabel("Source of error"),i, i+1, j, j+1)
i=i+1   # C1  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_maturity_males_settings$Attach(hbox_males_radio_plus_combo,i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_maturity_males_settings$Attach(vbox_maturity_mean_devSt_males,i, i+1, j, j+1) 

i=i+1  	# C2  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_maturity_males_settings$Attach(hbox_males_radio_plus_load,i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_maturity_males_settings$Attach(maturity_uncert_males_from_file_sw,i, i+1, j, j+1) 


# ----------------------------------  tbl_maturity_females_settings ----------------------------------------------- 
tbl_maturity_females_settings$SetRowSpacings(5)
tbl_maturity_females_settings$SetColSpacings(5)
tbl_maturity_females_settings$SetBorderWidth(5)
i=0   	# C0  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_maturity_females_settings$Attach(gtkLabel("Source of error"),i, i+1, j, j+1)
i=i+1   # C1  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_maturity_females_settings$Attach(hbox_females_radio_plus_combo,i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_maturity_females_settings$Attach(vbox_maturity_mean_devSt_females,i, i+1, j, j+1) 

i=i+1  	# C2  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_maturity_females_settings$Attach(hbox_females_radio_plus_load,i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_maturity_females_settings$Attach(maturity_uncert_females_from_file_sw,i, i+1, j, j+1) 

# ----------------------------------  tbl_maturity_mean_devSt_males ----------------------------------------------- 
tbl_maturity_mean_devSt_males$SetRowSpacings(5)
tbl_maturity_mean_devSt_males$SetColSpacings(5)
tbl_maturity_mean_devSt_males$SetBorderWidth(5)
i=0   	# C0  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
j=j+1   # R1 '''''''''''''''''''  
tbl_maturity_mean_devSt_males$Attach(gtkLabel("L50% [mm]"),i, i+1, j, j+1)
j=j+1   # R2 '''''''''''''''''''  
tbl_maturity_mean_devSt_males$Attach(gtkLabel("L75%L25% [mm]"),i, i+1, j, j+1)
i=i+1   # C1  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_maturity_mean_devSt_males$Attach(lbl_A_distr_maturity_uncert_males,i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_maturity_mean_devSt_males$Attach(entry_A_distr_maturity_uncert_males_L50,i, i+1, j, j+1) 
j=j+1   # R2 '''''''''''''''''''  
tbl_maturity_mean_devSt_males$Attach(entry_A_distr_maturity_uncert_males_MR,i, i+1, j, j+1)
i=i+1  	# C2  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_maturity_mean_devSt_males$Attach(lbl_B_distr_maturity_uncert_males,i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_maturity_mean_devSt_males$Attach(entry_B_distr_maturity_uncert_males_L50,i, i+1, j, j+1) 
j=j+1   # R1 '''''''''''''''''''  
tbl_maturity_mean_devSt_males$Attach(entry_B_distr_maturity_uncert_males_MR,i, i+1, j, j+1)
 

# ----------------------------------  tbl_maturity_mean_devSt_females ----------------------------------------------- 
tbl_maturity_mean_devSt_females$SetRowSpacings(5)
tbl_maturity_mean_devSt_females$SetColSpacings(5)
tbl_maturity_mean_devSt_females$SetBorderWidth(5)
i=0   	# C0  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
j=j+1   # R1 '''''''''''''''''''  
tbl_maturity_mean_devSt_females$Attach(gtkLabel("L50% [mm]"),i, i+1, j, j+1)
j=j+1   # R2 '''''''''''''''''''  
tbl_maturity_mean_devSt_females$Attach(gtkLabel("L75%L25% [mm]"),i, i+1, j, j+1)
i=i+1   # C1  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_maturity_mean_devSt_females$Attach(lbl_A_distr_maturity_uncert_females,i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_maturity_mean_devSt_females$Attach(entry_A_distr_maturity_uncert_females_L50,i, i+1, j, j+1) 
j=j+1   # R2 '''''''''''''''''''  
tbl_maturity_mean_devSt_females$Attach(entry_A_distr_maturity_uncert_females_MR,i, i+1, j, j+1)
i=i+1  	# C2  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_maturity_mean_devSt_females$Attach(lbl_B_distr_maturity_uncert_females,i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_maturity_mean_devSt_females$Attach(entry_B_distr_maturity_uncert_females_L50,i, i+1, j, j+1) 
j=j+1   # R1 '''''''''''''''''''  
tbl_maturity_mean_devSt_females$Attach(entry_B_distr_maturity_uncert_females_MR,i, i+1, j, j+1)


# ----------------------------------  tbl_maturity_costant_params ----------------------------------------------- 
tbl_maturity_costant_params$SetRowSpacings(5)
tbl_maturity_costant_params$SetColSpacings(20)
tbl_maturity_costant_params$SetBorderWidth(5)
i=0   	# C0  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
j=j+1   # R1 '''''''''''''''''''  
tbl_maturity_costant_params$Attach(gtkLabel("MALES"),i, i+1, j, j+1)  
j=j+1   # R2 '''''''''''''''''''  
tbl_maturity_costant_params$Attach(gtkLabel("FEMALES"),i, i+1, j, j+1)    
i=i+1   # C1  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_maturity_costant_params$Attach(gtkLabel("L50% [mm]"),i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_maturity_costant_params$Attach(entry_costant_males_L50,i, i+1, j, j+1) 
j=j+1   # R2 '''''''''''''''''''  
tbl_maturity_costant_params$Attach(entry_costant_females_L50,i, i+1, j, j+1) 
i=i+1   # C2  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_maturity_costant_params$Attach(gtkLabel("L75%L25% [mm]"),i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_maturity_costant_params$Attach(entry_costant_males_MR,i, i+1, j, j+1) 
j=j+1   # R2 '''''''''''''''''''  
tbl_maturity_costant_params$Attach(entry_costant_females_MR,i, i+1, j, j+1) 


# ----------------------------------  tbl_maturity_settings_params ----------------------------------------------- 
tbl_maturity_settings_params$SetRowSpacings(5)
tbl_maturity_settings_params$SetColSpacings(20)
tbl_maturity_settings_params$SetBorderWidth(5)
i=0   	# C0  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_maturity_settings_params$Attach(frame_maturity_uncert_males,i, i+1, j, j+1)  
    
i=i+1   # C1  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_maturity_settings_params$Attach(frame_maturity_uncert_females,i, i+1, j, j+1)  




 # 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같



# COMPOSIZIONE INTERFACCIA 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같

vbox_global_maturity_uncert$packStart(hbox_maturity_costant_params, expand = F, fill = F, padding = 10)
vbox_global_maturity_uncert$packStart(hbox_maturity_uncert_frames, expand = F, fill = F, padding = 5)


hbox_maturity_costant_params$packStart(tbl_maturity_costant_params, expand = F, fill = F, padding = 10)
hbox_maturity_uncert_frames$packStart(tbl_maturity_settings_params, expand = F, fill = F, padding = 10)

h_frame_maturity_uncert_males$packStart(tbl_maturity_males_settings, expand = FALSE, fill = FALSE, padding = 5)
frame_maturity_uncert_males$add(h_frame_maturity_uncert_males)

h_frame_maturity_uncert_females$packStart(tbl_maturity_females_settings, expand = FALSE, fill = FALSE, padding = 5)
frame_maturity_uncert_females$add(h_frame_maturity_uncert_females)

hbox_males_radio_plus_load$packStart(radio_maturity_uncert_males_from_external_file, expand = F, fill = F, padding = 10)
hbox_males_radio_plus_load$packStart(btn_load_maturity_uncert_males_from_file, expand = F, fill = F, padding = 10)

hbox_males_radio_plus_combo$packStart(radio_maturity_uncert_males_from_distribution, expand = F, fill = F, padding = 10)
hbox_males_radio_plus_combo$packStart(combo_distr_maturity_uncert_males, expand = F, fill = F, padding = 10)

hbox_females_radio_plus_load$packStart(radio_maturity_uncert_females_from_external_file, expand = F, fill = F, padding = 10)
hbox_females_radio_plus_load$packStart(btn_load_maturity_uncert_females_from_file, expand = F, fill = F, padding = 10)

hbox_females_radio_plus_combo$packStart(radio_maturity_uncert_females_from_distribution, expand = F, fill = F, padding = 10)
hbox_females_radio_plus_combo$packStart(combo_distr_maturity_uncert_females, expand = F, fill = F, padding = 10)
	
vbox_maturity_mean_devSt_males$packStart(tbl_maturity_mean_devSt_males, expand = F, fill = F, padding = 10)
vbox_maturity_mean_devSt_females$packStart(tbl_maturity_mean_devSt_females, expand = F, fill = F, padding = 10)

# ---------------------------------------- ACTIVATE/DEACTIVATE ALL THE OBJECTS
	gtkWidgetSetSensitive(entry_costant_males_L50, FALSE)  
	gtkWidgetSetSensitive(entry_costant_females_L50, FALSE)  
	gtkWidgetSetSensitive(entry_costant_males_MR, FALSE)  
	gtkWidgetSetSensitive(entry_costant_females_MR, FALSE)  
 						
gtkWidgetSetSensitive(vbox_global_maturity_uncert, FALSE)  
									
 gtkComboBoxSetActive(combo_distr_maturity_uncert_females, (which(DISTRIBUTION_UNCERT == DISTRIBUTION_UNCERT[1])-1) )  
 gtkComboBoxSetActive(combo_distr_maturity_uncert_males, (which(DISTRIBUTION_UNCERT == DISTRIBUTION_UNCERT[1])-1) )  

 gtkToggleButtonSetActive(radio_maturity_uncert_males_from_distribution, TRUE)
 gtkToggleButtonSetActive(radio_maturity_uncert_females_from_distribution, TRUE)
 

 

		