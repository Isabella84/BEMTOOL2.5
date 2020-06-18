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
growth_uncert_k_from_file_sw <<- gtkScrolledWindowNew(NULL, NULL)
growth_uncert_k_from_file_sw$setShadowType("etched-in")
growth_uncert_k_from_file_sw$setPolicy("automatic", "automatic")
growth_uncert_k_from_file_sw$SetUsize(100, dim_big_tables)  
growth_uncert_k_from_file_list <<- list()
growth_uncert_k_from_file_index <<- 0
growth_uncert_k_from_file_create_model()
growth_uncert_k_from_file_treeview <<- gtkTreeViewNewWithModel( growth_uncert_k_from_file_model)
growth_uncert_k_from_file_treeview$setRulesHint(TRUE)
growth_uncert_k_from_file_treeview$getSelection()$setMode("single")
growth_uncert_k_from_file_add_columns( growth_uncert_k_from_file_treeview)
growth_uncert_k_from_file_sw$destroy()
growth_uncert_k_from_file_sw <<- gtkScrolledWindowNew(NULL, NULL)
growth_uncert_k_from_file_sw$setShadowType("etched-in")
growth_uncert_k_from_file_sw$setPolicy("automatic", "automatic")
growth_uncert_k_from_file_sw$SetUsize(100, dim_big_tables)  
growth_uncert_k_from_file_sw$add(growth_uncert_k_from_file_treeview)
# ---------------------------------------------------------------------------------



# for k parameters for uncertainty on growth ----------------------------------------------
growth_uncert_Linf_from_file_sw <<- gtkScrolledWindowNew(NULL, NULL)
growth_uncert_Linf_from_file_sw$setShadowType("etched-in")
growth_uncert_Linf_from_file_sw$setPolicy("automatic", "automatic")
growth_uncert_Linf_from_file_sw$SetUsize(100, dim_big_tables)  
growth_uncert_Linf_from_file_list <<- list()
growth_uncert_Linf_from_file_index <<- 0
growth_uncert_Linf_from_file_create_model()
growth_uncert_Linf_from_file_treeview <<- gtkTreeViewNewWithModel( growth_uncert_Linf_from_file_model)
growth_uncert_Linf_from_file_treeview$setRulesHint(TRUE)
growth_uncert_Linf_from_file_treeview$getSelection()$setMode("single")
growth_uncert_Linf_from_file_add_columns( growth_uncert_Linf_from_file_treeview)
growth_uncert_Linf_from_file_sw$destroy()
growth_uncert_Linf_from_file_sw <<- gtkScrolledWindowNew(NULL, NULL)
growth_uncert_Linf_from_file_sw$setShadowType("etched-in")
growth_uncert_Linf_from_file_sw$setPolicy("automatic", "automatic")
growth_uncert_Linf_from_file_sw$SetUsize(100, dim_big_tables)  
growth_uncert_Linf_from_file_sw$add(growth_uncert_Linf_from_file_treeview)
# ---------------------------------------------------------------------------------


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   CHECKS 

chkConfidenceIntervals_fore_M <- gtkCheckButton("Apply uncertainty on natural mortality M")
gSignalConnect(chkConfidenceIntervals_fore_M, "toggled", activ_deact_uncertainty_on_M)

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   COMBO 


combo_distr_growth_uncert_Linf <- gtkComboBoxNewText()
for (choice in DISTRIBUTION_UNCERT) { combo_distr_growth_uncert_Linf$appendText(choice) }

combo_distr_growth_uncert_k <- gtkComboBoxNewText()
for (choice in DISTRIBUTION_UNCERT) { combo_distr_growth_uncert_k$appendText(choice) }

combo_M_model_m <- gtkComboBoxNewText()
for (choice in MORTALITY_TYPE) { combo_M_model_m$appendText(choice) }

combo_M_model_f <- gtkComboBoxNewText()
for (choice in MORTALITY_TYPE) { combo_M_model_f$appendText(choice) }

gSignalConnect(combo_distr_growth_uncert_Linf, "changed", change_label_growthUncert_Linf_distribution )
gSignalConnect(combo_distr_growth_uncert_k, "changed", change_label_growthUncert_k_distribution)
gSignalConnect(combo_M_model_m, "changed", deactivate_M_unused_params_m)
gSignalConnect(combo_M_model_f, "changed", deactivate_M_unused_params_f)

gtkComboBoxSetActive(combo_M_model_m, (which(MORTALITY_TYPE == new_aldSimulation@naturalmortality.M.type)-1) )  
gtkComboBoxSetActive(combo_M_model_f, (which(MORTALITY_TYPE == new_aldSimulation@naturalmortality.F.type)-1) )  

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   BUTTON 

btn_load_growth_uncert_Linf_from_file <- gtkButton()
btn_load_growth_uncert_k_from_file <- gtkButton()

gtkButtonSetLabel(btn_load_growth_uncert_Linf_from_file, "Load...")
gtkButtonSetLabel(btn_load_growth_uncert_k_from_file, "Load...")

btn_load_growth_uncert_Linf_from_file$AddCallback("clicked", load_growth_uncert_Linf_from_file)
btn_load_growth_uncert_k_from_file$AddCallback("clicked", load_growth_uncert_k_from_file)


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   LABEL 

lbl_A_distr_growth_uncert_Linf <- gtkLabel("A")  
lbl_B_distr_growth_uncert_Linf <- gtkLabel("B")
lbl_A_distr_growth_uncert_k <- gtkLabel("A")  
lbl_B_distr_growth_uncert_k <- gtkLabel("B")
lbl_Mtmax_m <- gtkLabel("Mtmax")  
lbl_Mtmax_f <- gtkLabel("Mtmax")

gtkLabelSetWidthChars(lbl_A_distr_growth_uncert_Linf, LABEL_LENGTH) 
gtkLabelSetWidthChars(lbl_B_distr_growth_uncert_Linf, LABEL_LENGTH) 
gtkLabelSetWidthChars(lbl_A_distr_growth_uncert_k, LABEL_LENGTH) 
gtkLabelSetWidthChars(lbl_B_distr_growth_uncert_k, LABEL_LENGTH) 
gtkLabelSetWidthChars(lbl_Mtmax_m, LABEL_LENGTH) 
gtkLabelSetWidthChars(lbl_Mtmax_f, LABEL_LENGTH) 

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   ENTRY 

entry_A_distr_growth_uncert_Linf_m <- gtkEntry()
entry_B_distr_growth_uncert_Linf_m <- gtkEntry()
entry_A_distr_growth_uncert_k_m <- gtkEntry()
entry_B_distr_growth_uncert_k_m <- gtkEntry()

entry_A_distr_growth_uncert_Linf_f <- gtkEntry()
entry_B_distr_growth_uncert_Linf_f <- gtkEntry()
entry_A_distr_growth_uncert_k_f <- gtkEntry()
entry_B_distr_growth_uncert_k_f <- gtkEntry()

entry_costant_Linf_f <- gtkEntry()
entry_costant_k_f <- gtkEntry()
entry_costant_t0_f <- gtkEntry()

entry_costant_Linf_m <- gtkEntry()
entry_costant_k_m <- gtkEntry()
entry_costant_t0_m <- gtkEntry()

entry_Mtmax_m <- gtkEntry()
entry_Mtmax_f <- gtkEntry()

if (phase=="FORECAST") {
gtkEntrySetText(entry_costant_Linf_f, BAS$F_mean_Linf )
gtkEntrySetText(entry_costant_Linf_m, BAS$M_mean_Linf )
gtkEntrySetText(entry_costant_k_f, BAS$F_mean_k )
gtkEntrySetText(entry_costant_k_m, BAS$M_mean_k )
gtkEntrySetText(entry_costant_t0_f, BAS$F_mean_t0 )
gtkEntrySetText(entry_costant_t0_m, BAS$M_mean_t0 )
}

gtkEntrySetWidthChars(entry_A_distr_growth_uncert_Linf_m, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entry_B_distr_growth_uncert_Linf_m, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entry_A_distr_growth_uncert_k_m, NUMERICAL_ENTRY_LENGTH)
gtkEntrySetWidthChars(entry_B_distr_growth_uncert_k_m, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entry_A_distr_growth_uncert_Linf_f, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entry_B_distr_growth_uncert_Linf_f, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entry_A_distr_growth_uncert_k_f, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entry_B_distr_growth_uncert_k_f, NUMERICAL_ENTRY_LENGTH) 

gtkEntrySetWidthChars(entry_costant_Linf_f, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entry_costant_k_f, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entry_costant_t0_f, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entry_costant_Linf_m, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entry_costant_k_m, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entry_costant_t0_m, NUMERICAL_ENTRY_LENGTH) 

gtkEntrySetWidthChars(entry_Mtmax_m, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entry_Mtmax_f, NUMERICAL_ENTRY_LENGTH) 

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   RADIO 

radio_growth_uncert_k <- gtkRadioButton()
radio_growth_uncert_k$add(gtkLabel("Apply error on k parameter"))
radio_growth_uncert_Linf <- gtkRadioButtonNewWithLabelFromWidget(radio_growth_uncert_k, "Apply error on Linf parameter")

radio_growth_uncert_k_from_distribution <- gtkRadioButton()
radio_growth_uncert_k_from_distribution$add(gtkLabel("from distribution"))
radio_growth_uncert_k_from_external_file <- gtkRadioButtonNewWithLabelFromWidget(radio_growth_uncert_k_from_distribution, "from external file")

radio_growth_uncert_Linf_from_distribution <- gtkRadioButton()
radio_growth_uncert_Linf_from_distribution$add(gtkLabel("from distribution"))
radio_growth_uncert_Linf_from_external_file <- gtkRadioButtonNewWithLabelFromWidget(radio_growth_uncert_Linf_from_distribution, "from external file")
	

gSignalConnect(radio_growth_uncert_k, "toggled", deactivate_growth_uncertainty_Linf_k)
gSignalConnect(radio_growth_uncert_Linf, "toggled", deactivate_growth_uncertainty_Linf_k)
gSignalConnect(radio_growth_uncert_k_from_distribution, "toggled", deactivate_growth_uncertainty_k_distr_extfile)
gSignalConnect(radio_growth_uncert_k_from_external_file, "toggled", deactivate_growth_uncertainty_k_distr_extfile)
gSignalConnect(radio_growth_uncert_Linf_from_distribution, "toggled", deactivate_growth_uncertainty_Linf_distr_extfile)
gSignalConnect(radio_growth_uncert_Linf_from_external_file, "toggled", deactivate_growth_uncertainty_Linf_distr_extfile)

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   HORIZONTAL BOX 

hbox_growth_costant_params <- gtkHBox(FALSE, 5)
hbox_growth_uncert_frames <- gtkHBox(FALSE, 5)
h_frame_growth_uncert_Linf <- gtkHBox(FALSE, 5)
h_frame_growth_uncert_k <- gtkHBox(FALSE, 5)
h_frame_M_model <-  gtkHBox(FALSE, 5)
hbox_Linf_radio_plus_load <- gtkHBox()
hbox_Linf_radio_plus_combo <- gtkHBox()
hbox_k_radio_plus_load <- gtkHBox()
hbox_k_radio_plus_combo <- gtkHBox()


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   VERTICAL BOX 

vbox_global_growth_uncert <- gtkVBox(FALSE, 5)
vbox_growth_mean_devSt_Linf <- gtkVBox(FALSE, 5)
vbox_growth_mean_devSt_k <- gtkVBox(FALSE, 5)


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   FRAME 

frame_growth_uncert_Linf <- gtkFrame(" Error on Linf ")  
frame_growth_uncert_k <- gtkFrame(" Error on k ")  
frame_M_model <- gtkFrame(" M model ")         

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   TABELLE 

tbl_growth_costant_params <- gtkTable(3,7,homogeneous = FALSE)
tbl_growth_uncert_frames <- gtkTable(2,2,homogeneous = FALSE)
tbl_growth_mean_devSt_Linf <-  gtkTable(3, 3,homogeneous = FALSE)
tbl_growth_mean_devSt_k <-  gtkTable(3, 3,homogeneous = FALSE)
tbl_growth_Linf_settings <-  gtkTable(2, 3,homogeneous = FALSE)
tbl_growth_k_settings <-  gtkTable(2, 3,homogeneous = FALSE)
tbl_M_models  <- gtkTable(2, 4,homogeneous = FALSE)


# ----------------------------------  tbl_growth_k_settings ----------------------------------------------- 
tbl_growth_k_settings$SetRowSpacings(5)
tbl_growth_k_settings$SetColSpacings(5)
tbl_growth_k_settings$SetBorderWidth(5)
i=0   	# C0  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_growth_k_settings$Attach(gtkLabel("Source of error"),i, i+1, j, j+1)
i=i+1   # C1  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_growth_k_settings$Attach(hbox_k_radio_plus_combo,i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_growth_k_settings$Attach(vbox_growth_mean_devSt_k,i, i+1, j, j+1) 
i=i+1  	# C2  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_growth_k_settings$Attach(hbox_k_radio_plus_load,i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_growth_k_settings$Attach(growth_uncert_k_from_file_sw,i, i+1, j, j+1) 


# ----------------------------------  tbl_growth_Linf_settings ----------------------------------------------- 
tbl_growth_Linf_settings$SetRowSpacings(5)
tbl_growth_Linf_settings$SetColSpacings(5)
tbl_growth_Linf_settings$SetBorderWidth(5)
i=0   	# C0  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_growth_Linf_settings$Attach(gtkLabel("Source of error"),i, i+1, j, j+1)
i=i+1   # C1  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_growth_Linf_settings$Attach(hbox_Linf_radio_plus_combo,i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_growth_Linf_settings$Attach(vbox_growth_mean_devSt_Linf,i, i+1, j, j+1) 
i=i+1  	# C2  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_growth_Linf_settings$Attach(hbox_Linf_radio_plus_load,i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_growth_Linf_settings$Attach(growth_uncert_Linf_from_file_sw,i, i+1, j, j+1) 




# ----------------------------------  tbl_growth_mean_devSt_k ----------------------------------------------- 
tbl_growth_mean_devSt_k$SetRowSpacings(5)
tbl_growth_mean_devSt_k$SetColSpacings(5)
tbl_growth_mean_devSt_k$SetBorderWidth(5)
i=0   	# C0  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
j=j+1   # R1 '''''''''''''''''''  
tbl_growth_mean_devSt_k$Attach(gtkLabel("MALES"),i, i+1, j, j+1)
j=j+1   # R2 '''''''''''''''''''  
tbl_growth_mean_devSt_k$Attach(gtkLabel("FEMALES"),i, i+1, j, j+1)
i=i+1   # C1  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_growth_mean_devSt_k$Attach(lbl_A_distr_growth_uncert_k,i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_growth_mean_devSt_k$Attach(entry_A_distr_growth_uncert_k_m,i, i+1, j, j+1) 
j=j+1   # R2 '''''''''''''''''''  
tbl_growth_mean_devSt_k$Attach(entry_A_distr_growth_uncert_k_f,i, i+1, j, j+1)
i=i+1  	# C2  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_growth_mean_devSt_k$Attach(lbl_B_distr_growth_uncert_k,i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_growth_mean_devSt_k$Attach(entry_B_distr_growth_uncert_k_m,i, i+1, j, j+1) 
j=j+1   # R1 '''''''''''''''''''  
tbl_growth_mean_devSt_k$Attach(entry_B_distr_growth_uncert_k_f,i, i+1, j, j+1) 



# ----------------------------------  tbl_growth_mean_devSt_Linf ----------------------------------------------- 
tbl_growth_mean_devSt_Linf$SetRowSpacings(5)
tbl_growth_mean_devSt_Linf$SetColSpacings(5)
tbl_growth_mean_devSt_Linf$SetBorderWidth(5)
i=0   	# C0  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
j=j+1   # R1 '''''''''''''''''''  
tbl_growth_mean_devSt_Linf$Attach(gtkLabel("MALES"),i, i+1, j, j+1)
j=j+1   # R2 '''''''''''''''''''  
tbl_growth_mean_devSt_Linf$Attach(gtkLabel("FEMALES"),i, i+1, j, j+1)
i=i+1   # C1  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_growth_mean_devSt_Linf$Attach(lbl_A_distr_growth_uncert_Linf,i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_growth_mean_devSt_Linf$Attach(entry_A_distr_growth_uncert_Linf_m,i, i+1, j, j+1) 
j=j+1   # R2 '''''''''''''''''''  
tbl_growth_mean_devSt_Linf$Attach(entry_A_distr_growth_uncert_Linf_f,i, i+1, j, j+1)
i=i+1  	# C2  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_growth_mean_devSt_Linf$Attach(lbl_B_distr_growth_uncert_Linf,i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_growth_mean_devSt_Linf$Attach(entry_B_distr_growth_uncert_Linf_m,i, i+1, j, j+1) 
j=j+1   # R1 '''''''''''''''''''  
tbl_growth_mean_devSt_Linf$Attach(entry_B_distr_growth_uncert_Linf_f,i, i+1, j, j+1) 


# ----------------------------------  tbl_growth_costant_params ----------------------------------------------- 
tbl_growth_costant_params$SetRowSpacings(5)
tbl_growth_costant_params$SetColSpacings(20)
tbl_growth_costant_params$SetBorderWidth(5)
i=0   	# C0  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
j=j+1   # R1 '''''''''''''''''''  
tbl_growth_costant_params$Attach(gtkLabel("MALES"),i, i+1, j, j+1)  
j=j+1   # R2 '''''''''''''''''''  
tbl_growth_costant_params$Attach(gtkLabel("FEMALES"),i, i+1, j, j+1)    
i=i+1   # C1  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_growth_costant_params$Attach(gtkLabel("Linfinity [mm]"),i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_growth_costant_params$Attach(entry_costant_Linf_m,i, i+1, j, j+1) 
j=j+1   # R2 '''''''''''''''''''  
tbl_growth_costant_params$Attach(entry_costant_Linf_f,i, i+1, j, j+1) 
i=i+1   # C2  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_growth_costant_params$Attach(gtkLabel("K [years^-1]"),i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_growth_costant_params$Attach(entry_costant_k_m,i, i+1, j, j+1) 
j=j+1   # R2 '''''''''''''''''''  
tbl_growth_costant_params$Attach(entry_costant_k_f,i, i+1, j, j+1) 
i=i+1   # C3  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_growth_costant_params$Attach(gtkLabel("t0 [years]"),i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_growth_costant_params$Attach(entry_costant_t0_m,i, i+1, j, j+1) 
j=j+1   # R2 '''''''''''''''''''  
tbl_growth_costant_params$Attach(entry_costant_t0_f,i, i+1, j, j+1) 
i=i+1   # C4  -------------------------------------------
i=i+1   # C5  -------------------------------------------
i=i+1   # C6  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_growth_costant_params$Attach(chkConfidenceIntervals_fore_M,i, i+1, j, j+1) 
j=j+1   # R1 '''''''''''''''''''
tbl_growth_costant_params$Attach(frame_M_model,i, i+1, j, j+2) 





# ----------------------------------  tbl_growth_costant_params ----------------------------------------------- 
tbl_M_models$SetRowSpacings(5)
tbl_M_models$SetColSpacings(10)
tbl_M_models$SetBorderWidth(5)
i=0   # C0  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_M_models$Attach(gtkLabel("MALES"),i, i+1, j, j+1) 
j=j+1   # R1 '''''''''''''''''''  
tbl_M_models$Attach(gtkLabel("FEMALES"),i, i+1, j, j+1) 
i=i+1   # C1  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_M_models$Attach(combo_M_model_m,i, i+1, j, j+1) 
j=j+1   # R1 '''''''''''''''''''  
tbl_M_models$Attach(combo_M_model_f,i, i+1, j, j+1) 
i=i+1   # C2  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_M_models$Attach(lbl_Mtmax_m,i, i+1, j, j+1) 
j=j+1   # R1 '''''''''''''''''''  
tbl_M_models$Attach(lbl_Mtmax_f,i, i+1, j, j+1) 
i=i+1   # C3  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_M_models$Attach(entry_Mtmax_m,i, i+1, j, j+1) 
j=j+1   # R1 '''''''''''''''''''  
tbl_M_models$Attach(entry_Mtmax_f,i, i+1, j, j+1)   



# ----------------------------------  tbl_growth_uncert_frames ----------------------------------------------- 
tbl_growth_uncert_frames$SetRowSpacings(5)
tbl_growth_uncert_frames$SetColSpacings(5)
tbl_growth_uncert_frames$SetBorderWidth(5)
i=0   	# C0  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_growth_uncert_frames$Attach(radio_growth_uncert_Linf,i, i+1, j, j+1)  
j=j+1   # R1 '''''''''''''''''''  
tbl_growth_uncert_frames$Attach(frame_growth_uncert_Linf,i, i+1, j, j+1)  
i=i+1   # C1  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_growth_uncert_frames$Attach(radio_growth_uncert_k,i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_growth_uncert_frames$Attach(frame_growth_uncert_k,i, i+1, j, j+1) 



 # 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같



# COMPOSIZIONE INTERFACCIA 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같

vbox_global_growth_uncert$packStart(hbox_growth_costant_params, expand = F, fill = F, padding = 10)
vbox_global_growth_uncert$packStart(hbox_growth_uncert_frames, expand = F, fill = F, padding = 5)


hbox_growth_costant_params$packStart(tbl_growth_costant_params, expand = F, fill = F, padding = 10)
hbox_growth_uncert_frames$packStart(tbl_growth_uncert_frames, expand = F, fill = F, padding = 10)

h_frame_growth_uncert_Linf$packStart(tbl_growth_Linf_settings, expand = FALSE, fill = FALSE, padding = 5)
frame_growth_uncert_Linf$add(h_frame_growth_uncert_Linf)

h_frame_growth_uncert_k$packStart(tbl_growth_k_settings, expand = FALSE, fill = FALSE, padding = 5)
frame_growth_uncert_k$add(h_frame_growth_uncert_k)

h_frame_M_model$packStart(tbl_M_models, expand = FALSE, fill = FALSE, padding = 5)
frame_M_model$add(h_frame_M_model)

hbox_Linf_radio_plus_load$packStart(radio_growth_uncert_Linf_from_external_file, expand = F, fill = F, padding = 10)
hbox_Linf_radio_plus_load$packStart(btn_load_growth_uncert_Linf_from_file, expand = F, fill = F, padding = 10)

hbox_Linf_radio_plus_combo$packStart(radio_growth_uncert_Linf_from_distribution, expand = F, fill = F, padding = 10)
hbox_Linf_radio_plus_combo$packStart(combo_distr_growth_uncert_Linf, expand = F, fill = F, padding = 10)

hbox_k_radio_plus_load$packStart(radio_growth_uncert_k_from_external_file, expand = F, fill = F, padding = 10)
hbox_k_radio_plus_load$packStart(btn_load_growth_uncert_k_from_file, expand = F, fill = F, padding = 10)

hbox_k_radio_plus_combo$packStart(radio_growth_uncert_k_from_distribution, expand = F, fill = F, padding = 10)
hbox_k_radio_plus_combo$packStart(combo_distr_growth_uncert_k, expand = F, fill = F, padding = 10)

vbox_growth_mean_devSt_k$packStart(tbl_growth_mean_devSt_k, expand = F, fill = F, padding = 0)
vbox_growth_mean_devSt_Linf$packStart(tbl_growth_mean_devSt_Linf, expand = F, fill = F, padding = 0)
	


# ---------------------------------------- ACTIVATE/DEACTIVATE ALL THE OBJECTS
	gtkWidgetSetSensitive(entry_costant_Linf_f, FALSE)  
		gtkWidgetSetSensitive(entry_costant_Linf_m, FALSE)  
			gtkWidgetSetSensitive(entry_costant_k_f, FALSE)  
				gtkWidgetSetSensitive(entry_costant_k_m, FALSE)  
					gtkWidgetSetSensitive(entry_costant_t0_f, FALSE)  
						gtkWidgetSetSensitive(entry_costant_t0_m, FALSE)  
						
gtkWidgetSetSensitive(vbox_global_growth_uncert, FALSE)  
			
						
	gtkComboBoxSetActive(combo_distr_growth_uncert_Linf, (which(DISTRIBUTION_UNCERT == DISTRIBUTION_UNCERT[1])-1) )  
		gtkComboBoxSetActive(combo_distr_growth_uncert_k, (which(DISTRIBUTION_UNCERT == DISTRIBUTION_UNCERT[1])-1) )  

gtkToggleButtonSetActive(radio_growth_uncert_Linf, TRUE)

gtkToggleButtonSetActive(radio_growth_uncert_Linf_from_distribution, TRUE)
gtkWidgetSetSensitive(btn_load_growth_uncert_Linf_from_file, FALSE)
gtkWidgetSetSensitive(growth_uncert_Linf_from_file_sw, FALSE)

gtkToggleButtonSetActive(radio_growth_uncert_k_from_distribution, TRUE)
gtkWidgetSetSensitive(btn_load_growth_uncert_k_from_file, FALSE)
gtkWidgetSetSensitive(growth_uncert_k_from_file_sw, FALSE)

gtkToggleButtonSetActive(chkConfidenceIntervals_fore_M, FALSE)
gtkWidgetSetSensitive(frame_M_model, FALSE)
 

		