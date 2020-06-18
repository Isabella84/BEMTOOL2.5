 # ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




 # COSTRUZIONE OGGETTI 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같
 
 
 # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   FINESTRE CON DATI 


# for the external vector - valori da usare ----------------------------------------------
recruitments_fore_from_vector.sw_UN <<- gtkScrolledWindowNew(NULL, NULL)
recruitments_fore_from_vector.sw_UN$setShadowType("etched-in")
recruitments_fore_from_vector.sw_UN$setPolicy("automatic", "automatic")
recruitments_fore_from_vector.sw_UN$SetUsize(100, dim_big_tables)  
recruitments_fore_from_vector_UN <<- list()
recruitments_fore_from_vector_index_UN <<- 0
recruitments_fore_from_vector.create_model_UN()
recruitments_fore_from_vector.treeview_UN <<- gtkTreeViewNewWithModel( recruitments_fore_from_vector.model_UN)
recruitments_fore_from_vector.treeview_UN$setRulesHint(TRUE)
recruitments_fore_from_vector.treeview_UN$getSelection()$setMode("single")
recruitments_fore_from_vector.add_columns_UN( recruitments_fore_from_vector.treeview_UN)
recruitments_fore_from_vector.sw_UN$destroy()
recruitments_fore_from_vector.sw_UN <<- gtkScrolledWindowNew(NULL, NULL)
recruitments_fore_from_vector.sw_UN$setShadowType("etched-in")
recruitments_fore_from_vector.sw_UN$setPolicy("automatic", "automatic")
recruitments_fore_from_vector.sw_UN$SetUsize(100, dim_big_tables)  
recruitments_fore_from_vector.sw_UN$add(recruitments_fore_from_vector.treeview_UN)
# ---------------------------------------------------------------------------------


# for the external vector valori dell'errore da applicare ------------------------------
extErrorRecruitment_fore.sw <<- gtkScrolledWindowNew(NULL, NULL)
extErrorRecruitment_fore.sw$setShadowType("etched-in")
extErrorRecruitment_fore.sw$setPolicy("automatic", "automatic")
extErrorRecruitment_fore.sw$SetUsize(100, dim_big_tables)  
extErrorRecruitment_fore_list <<- list()
extErrorRecruitment_foreIndex <<- 0
extErrorRecruitment_fore.create_model()
extErrorRecruitment_fore.treeview <<- gtkTreeViewNewWithModel(extErrorRecruitment_fore.model)
extErrorRecruitment_fore.treeview$setRulesHint(TRUE)
extErrorRecruitment_fore.treeview$getSelection()$setMode("single")
extErrorRecruitment_fore.add_columns(extErrorRecruitment_fore.treeview)
extErrorRecruitment_fore.sw$add(extErrorRecruitment_fore.treeview)      
# ------------------------------------------------------------


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   COMBO 


combo_RecrNoise_dis_fore_UN <- gtkComboBoxNewText()
for (choice in DISTRIBUTION) { 
if (choice != "Gamma")   {
combo_RecrNoise_dis_fore_UN$appendText(choice) 
}
}

combo_hboxSR_Distribution_Uncert_UN <- gtkComboBoxNewText()
for (choice in DISTRIBUTION_UNCERT) { combo_hboxSR_Distribution_Uncert_UN$appendText(choice) }

combo_SRtype_fore_UN <- gtkComboBoxNewText() 
for (choice in SR_TYPE) { 
if (choice != "from vector") { combo_SRtype_fore_UN$appendText(choice) }
}  

gSignalConnect(combo_RecrNoise_dis_fore_UN, "changed", change_noise_AB_fore )
gSignalConnect(combo_SRtype_fore_UN, "changed", deactivate_SSR_error_unused_params)
gSignalConnect(combo_hboxSR_Distribution_Uncert_UN, "changed", change_label_SRR_error_distribution)       #change_CI_source_err_fore !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! sistemare qua

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   BUTTON 

btn_load_external_SRR_err <- gtkButton()
btn_load_external_recruitment_err_fore <- gtkButton()
# btn_load_recruitment_fromVector_fore <- gtkButton()

gtkButtonSetLabel(btn_load_external_SRR_err, "Load...")
gtkButtonSetLabel(btn_load_external_recruitment_err_fore, "Load...")
# gtkButtonSetLabel(btn_load_recruitment_fromVector_fore, "Load RECR err...")

btn_load_external_recruitment_err_fore$AddCallback("clicked", load_extErrorRecruitment_fore_file)
btn_load_external_SRR_err$AddCallback("clicked", load_recruitment_fromVector_fore_file_UN)
# btn_load_recruitment_fromVector_fore$AddCallback("clicked", reload_recruitment_fore_from_vector_table)




# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   LABEL 

lblUnitSpawners_fore_UN <- gtkLabel("Unit for spawners")
# lbl_CI_num_runs_fore <- gtkLabel("no. of runs") 
# lbl_SRparameters_fore_UN <- gtkLabel("SRR parameters")
lbl_SR_params_a_fore_UN <- gtkLabel("a")
lbl_SR_params_b_fore_UN <- gtkLabel("b")
lbl_SR_params_c_fore_UN <- gtkLabel("c")
lbl_A_noise_fore_UN <- gtkLabel("A")  
lbl_B_noise_fore_UN <- gtkLabel("B")
#lbl_A_noise_fore <- gtkLabel("A")  
#lbl_B_noise_fore <- gtkLabel("B")

gtkLabelSetWidthChars(lbl_A_noise_fore_UN, LABEL_LENGTH) 
gtkLabelSetWidthChars(lbl_B_noise_fore_UN, LABEL_LENGTH) 
gtkLabelSetWidthChars(lbl_A_noise_fore, LABEL_LENGTH) 
gtkLabelSetWidthChars(lbl_B_noise_fore, LABEL_LENGTH) 

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   ENTRY 

entryMean_aSRR <- gtkEntry()
entryMean_bSRR <- gtkEntry()
entryMean_cSRR <- gtkEntry()
entryStDev_aSRR <- gtkEntry()
entryStDev_bSRR <- gtkEntry()
entryStDev_cSRR <- gtkEntry()
entryNoise_a_fore_UN <- gtkEntry()
entryNoise_b_fore_UN <- gtkEntry()
entry_costant_recr_forecast_UN <- gtkEntry()

if (phase=="FORECAST") {
gtkEntrySetText(entry_costant_recr_forecast_UN, INP$Recruits[(simperiod*12+1)] )
}

# entry_CI_numb_runs_fore <- gtkEntry()

# gtkEntrySetWidthChars(entry_CI_numb_runs_fore, NUMERICAL_ENTRY_LENGTH)
# gtkEntrySetWidthChars(entrySR_params_a_fore_UN, NUMERICAL_ENTRY_LENGTH)
# gtkEntrySetWidthChars(entrySR_params_b_fore_UN, NUMERICAL_ENTRY_LENGTH)
# gtkEntrySetWidthChars(entrySR_params_c_fore_UN, NUMERICAL_ENTRY_LENGTH)
gtkEntrySetWidthChars(entryNoise_a_fore_UN, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entryNoise_b_fore_UN, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entry_costant_recr_forecast_UN, NUMERICAL_ENTRY_LENGTH)
gtkEntrySetWidthChars(entryMean_aSRR, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entryMean_bSRR, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entryMean_cSRR, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entryStDev_aSRR, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entryStDev_bSRR, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entryStDev_cSRR, NUMERICAL_ENTRY_LENGTH) 

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   RADIO 

radio_forecast_recruits_costant_UN <- gtkRadioButton()
radio_forecast_recruits_costant_UN$add(gtkLabel("Apply error on constant recruitment"))
radio_forecast_recruits_relationship_UN <- gtkRadioButtonNewWithLabelFromWidget(radio_forecast_recruits_costant_UN, "Apply error on SRR")

#radio_forecast_recruits_costant_vect_C_UN <- gtkRadioButton()
#radio_forecast_recruits_costant_vect_C_UN$add(gtkLabel("Scalar"))
#radio_forecast_recruits_costant_vect_V_UN <- gtkRadioButtonNewWithLabelFromWidget(radio_forecast_recruits_costant_vect_C_UN, "Vector")          


# gSignalConnect(radio_forecast_recruits_costant_vect_C_UN, "toggled", deactivate_scalar_costant_vector_recruitment_fore)         
#gSignalConnect(radio_forecast_recruits_costant_vect_V_UN, "toggled", deactivate_scalar_costant_vector_recruitment_fore)         

radio_tons_fore_UN <- gtkRadioButton()
radio_tons_fore_UN$add(gtkLabel("Tons (biomass)"))
radio_thousands_fore_UN <- gtkRadioButtonNewWithLabelFromWidget(radio_tons_fore_UN, "Thousands (number)")

radio_CI_err_additive_fore <- gtkRadioButton()
radio_CI_err_additive_fore$add(gtkLabel("Additive"))
radio_CI_err_multiplicative_fore <- gtkRadioButtonNewWithLabelFromWidget(radio_CI_err_additive_fore, "Multiplicative")


radio_recruitment_error_ext_file_fore <- gtkRadioButton()
radio_recruitment_error_ext_file_fore$add(gtkLabel("from external file"))
radio_recruitment_error_distribution_fore <- gtkRadioButtonNewWithLabelFromWidget(radio_recruitment_error_ext_file_fore, "from distribution")

	
# radio_SSR_error_ext_file <- gtkRadioButton()
# radio_SSR_error_ext_file$add(gtkLabel("from external file"))
# radio_SSR_error_ext_distribution <- gtkRadioButtonNewWithLabelFromWidget(radio_SSR_error_ext_file, "from distribution")
# !!!! aggiungere e legare la funzione checked/unchecked 

radio_distribution_SRR_uncert <- gtkRadioButton()
radio_distribution_SRR_uncert$add(gtkLabel("from distribution"))
radio_ext_file_SRR_uncert <- gtkRadioButtonNewWithLabelFromWidget(radio_distribution_SRR_uncert, "from external file")

gSignalConnect(radio_forecast_recruits_costant_UN, "toggled", change_CI_Type_Error_Costant_SRR)
gSignalConnect(radio_forecast_recruits_relationship_UN, "toggled", change_CI_Type_Error_Costant_SRR)
gSignalConnect(radio_distribution_SRR_uncert, "toggled", change_CI_Type_Error_SRR_betweenDistrOrExtFile)
gSignalConnect(radio_ext_file_SRR_uncert, "toggled", change_CI_Type_Error_SRR_betweenDistrOrExtFile)
gSignalConnect(radio_recruitment_error_ext_file_fore, "toggled", change_CI_Type_Error_Costant_betweenDistrOrExtFile)	
gSignalConnect(radio_recruitment_error_distribution_fore, "toggled", change_CI_Type_Error_Costant_betweenDistrOrExtFile)


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   HORIZONTAL BOX 

# hboxSRType_fore_UN <- gtkHBox(FALSE, 5)
hbox_recruits_fore_UN <- gtkHBox()
hbox_CI_fore <- gtkHBox()
# hboxSR_params_fore_UN <- gtkHBox(FALSE, 5)
hbox_entry_costant_recr_forecast_radios_UN <- gtkHBox()
hbox_recruits_fore_stockrecr_input_UN <- gtkHBox()
hboxExternalSRRErr_fore <- gtkHBox()
hbox_numero_run <- gtkHBox()
hboxErrorType_fore <- gtkHBox()
h_frame_recruits_fore_UN <- gtkHBox()
h_frame_CI_fore <- gtkHBox()
h_frame_uncert_cost_error <- gtkHBox()
h_frame_uncert_SRR_error <- gtkHBox()
hboxErrorNoise_dis_fore <- gtkHBox()
hboxExternalRecrErr_fore <- gtkHBox()
hboxComboSRR_uncertaity <- gtkHBox()

hboxMean_StDev_SRR_params_a <- gtkHBox()
hboxMean_StDev_SRR_params_b <- gtkHBox()
hboxMean_StDev_SRR_params_c <- gtkHBox()
hbox_unitOfSpawners_UN <- gtkHBox()
hbox_entry_costant_recr_forecast_UN <- gtkHBox()

hbox_SRR_radio_plus_load <- gtkHBox()
hbox_SRR_radio_plus_combo <- gtkHBox()

hbox_Costant_radio_plus_load <- gtkHBox()
hbox_Costant_radio_plus_combo <- gtkHBox()

hbox_costant_recr_forecast_UN <- gtkHBox()


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   VERTICAL BOX 

vboxExternalRecrErr_btn_fore <- gtkVBox()  
vbox_CI_fore <- gtkVBox()              
vbox_recruits_fore_UN <- gtkVBox()             
vboxExternalSRRErr_btn_fore <- gtkVBox()
vboxSRType_fore_UN  <- gtkVBox()

vbox_SRR_radio_plus_load <- gtkVBox()
vbox_SRR_radio_plus_combo <- gtkVBox()

vbox_Costant_radio_plus_combo <- gtkVBox()
vbox_Costant_radio_plus_load <- gtkVBox()

vbox_SRR_cost_mean_devSt <- gtkVBox()
vbox_SRR_a_b_c_params <- gtkVBox()


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   FRAME 

frame_recruits_fore_UN <- gtkFrame(" UNCERTAINTY ON RECRUITMENT ")   
frame_CI_fore <- gtkFrame(" UNCERTAINTY ON RECRUITMENT ") 
frame_uncert_cost_error <- gtkFrame(" Error on constant recruitment ")  
frame_uncert_SRR_error <- gtkFrame(" Error on SRR ")      


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   TABELLE 

tbl_CI_fore <- gtkTable(3,3,homogeneous = FALSE)
tbl_Noise_fore <- gtkTable(3,3,homogeneous = FALSE)
tbl_recr_stock_recr_relat_UN <- gtkTable(2, 2,homogeneous = FALSE)
tbl_distribution_file_SRR_uncert <- gtkTable(4, 2,homogeneous = FALSE)
tbl_constant_error_settings <-  gtkTable(1, 1,homogeneous = FALSE)
tbl_SRR_error_settings <-  gtkTable(1, 1,homogeneous = FALSE)
tbl_SRR_uncert_settings <-  gtkTable(2, 2,homogeneous = FALSE)

tbl_SRR_a_b_c_params <-  gtkTable(4, 3,homogeneous = FALSE)
tbl_SRR_cost_mean_devSt <-  gtkTable(2, 2,homogeneous = FALSE)

# ----------------------------------  tbl_SRR_cost_mean_devSt ----------------------------------------------- 
tbl_SRR_cost_mean_devSt$SetRowSpacings(5)
tbl_SRR_cost_mean_devSt$SetColSpacings(5)
tbl_SRR_cost_mean_devSt$SetBorderWidth(0)
i=0   	# C0  -------------------------------------------
i=i+1   # C1  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_SRR_cost_mean_devSt$Attach(lbl_A_noise_fore,i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_SRR_cost_mean_devSt$Attach(entryNoise_a_fore_UN,i, i+1, j, j+1) 
i=i+1  	# C2  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_SRR_cost_mean_devSt$Attach(lbl_B_noise_fore,i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_SRR_cost_mean_devSt$Attach(entryNoise_b_fore_UN,i, i+1, j, j+1) 





# ----------------------------------  tbl_SRR_a_b_c_params ----------------------------------------------- 
tbl_SRR_a_b_c_params$SetRowSpacings(5)
tbl_SRR_a_b_c_params$SetColSpacings(5)
tbl_SRR_a_b_c_params$SetBorderWidth(0)
i=0   	# C0  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
# tbl_SRR_a_b_c_params$Attach(combo_hboxSR_Distribution_Uncert_UN,i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_SRR_a_b_c_params$Attach(lbl_SR_params_a_fore_UN,i, i+1, j, j+1) 
j=j+1 	# R2 '''''''''''''''''''
tbl_SRR_a_b_c_params$Attach(lbl_SR_params_b_fore_UN,i, i+1, j, j+1) 
j=j+1 	# R3 '''''''''''''''''''
tbl_SRR_a_b_c_params$Attach(lbl_SR_params_c_fore_UN,i, i+1, j, j+1) 
i=i+1   # C1  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_SRR_a_b_c_params$Attach(lbl_A_noise_fore_UN,i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_SRR_a_b_c_params$Attach(entryMean_aSRR,i, i+1, j, j+1) 
j=j+1 	# R2 '''''''''''''''''''
tbl_SRR_a_b_c_params$Attach(entryMean_bSRR,i, i+1, j, j+1) 
j=j+1 	# R3 '''''''''''''''''''
tbl_SRR_a_b_c_params$Attach(entryMean_cSRR,i, i+1, j, j+1) 
i=i+1  	# C2  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_SRR_a_b_c_params$Attach(lbl_B_noise_fore_UN,i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_SRR_a_b_c_params$Attach(entryStDev_aSRR,i, i+1, j, j+1) 
j=j+1 	# R2 '''''''''''''''''''
tbl_SRR_a_b_c_params$Attach(entryStDev_bSRR,i, i+1, j, j+1) 
j=j+1 	# R3 '''''''''''''''''''
tbl_SRR_a_b_c_params$Attach(entryStDev_cSRR,i, i+1, j, j+1) 




# ----------------------------------  tbl_Noise_fore ----------------------------------------------- 
tbl_Noise_fore$SetRowSpacings(5)
tbl_Noise_fore$SetColSpacings(15)
tbl_Noise_fore$SetBorderWidth(5)
i=0   	# C0  -------------------------------------------
j=0   	# R -1 '''''''''''''''''''
tbl_Noise_fore$Attach(gtkLabel("Costant recruitment"),i, i+1, j, j+1)   
j=j+1   # R0 '''''''''''''''''''
tbl_Noise_fore$Attach(gtkLabel("Type of error"),i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_Noise_fore$Attach(gtkLabel("Source of error"),i, i+1, j, j+1) 
j=j+1 	# R2 '''''''''''''''''''
i=i+1   # C1  -------------------------------------------
j=0   	# R -1 '''''''''''''''''''
tbl_Noise_fore$Attach(hbox_costant_recr_forecast_UN,i, i+1, j, j+1)   
j=j+1  	# R0 '''''''''''''''''''
tbl_Noise_fore$Attach(hbox_CI_fore,i, i+1, j, j+1)   
j=j+1   # R1 '''''''''''''''''''  
tbl_Noise_fore$Attach(vbox_Costant_radio_plus_combo,i, i+1, j, j+1) 
j=j+1 	# R2 '''''''''''''''''''
tbl_Noise_fore$Attach(vbox_SRR_cost_mean_devSt,i, i+1, j, j+1) # hboxErrorNoise_dis_fore
i=i+1  	# C2  -------------------------------------------
j=0   	# R -1 ''''''''''''''''''' 
j=j+1   # R0 '''''''''''''''''''
j=j+1   # R1 '''''''''''''''''''  
tbl_Noise_fore$Attach(vbox_Costant_radio_plus_load,i, i+1, j, j+1)  
j=j+1 	# R2 ''''''''''''''''''' 
tbl_Noise_fore$Attach(hboxExternalRecrErr_fore,i, i+1, j, j+1)  
  
 
# ----------------------------------  tbl_distribution_file_SRR_uncert -----------------------------------------------   
tbl_distribution_file_SRR_uncert$SetRowSpacings(5)
tbl_distribution_file_SRR_uncert$SetColSpacings(15)
tbl_distribution_file_SRR_uncert$SetBorderWidth(0)
i=0   	# C0  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
 tbl_distribution_file_SRR_uncert$Attach(hboxComboSRR_uncertaity,i, i+2, j, j+1) 
j=j+1   # R1 ''''''''''''''''''' 
 tbl_distribution_file_SRR_uncert$Attach(gtkLabel("Source of error"),i, i+1, j, j+1) 
j=j+1 	# R2 '''''''''''''''''''
#tbl_distribution_file_SRR_uncert$Attach(radio_ext_file_SRR_uncert,i, i+1, j, j+1) 
i=i+1  	# C1  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
# tbl_distribution_file_SRR_uncert$Attach(hboxComboSRR_uncertaity,i, i+2, j, j+1) 
j=j+1   # R1 ''''''''''''''''''' 
tbl_distribution_file_SRR_uncert$Attach(vbox_SRR_radio_plus_combo,i, i+1, j, j+1)         
j=j+1 	# R2 '''''''''''''''''''
tbl_distribution_file_SRR_uncert$Attach(vbox_SRR_a_b_c_params,i, i+1, j, j+1) 
i=i+1  	# C2  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
# tbl_distribution_file_SRR_uncert$Attach(combo_SRtype_fore_UN,i, i+1, j, j+1) 
j=j+1   # R1 ''''''''''''''''''' 
tbl_distribution_file_SRR_uncert$Attach(vbox_SRR_radio_plus_load,i, i+1, j, j+1)    
j=j+1 	# R2 '''''''''''''''''''
tbl_distribution_file_SRR_uncert$Attach(hboxExternalSRRErr_fore,i, i+1, j, j+1)  


# ----------------------------------  tbl_constant_error_settings ----------------------------------------------- 
# contiene errore molt e add e tabella con scelta distribuzione o file esterno
tbl_constant_error_settings$SetRowSpacings(5)
tbl_constant_error_settings$SetColSpacings(30)
tbl_constant_error_settings$SetBorderWidth(5)
i=0   	# C0 -------------------------------------------
j=0    	# R0 '''''''''''''''''''
tbl_constant_error_settings$Attach(frame_uncert_cost_error,i, i+1, j, j+1)  

# ----------------------------------  tbl_SRR_error_settings ----------------------------------------------- 
# contiene errore molt e add e tabella con scelta distribuzione o file esterno
tbl_SRR_error_settings$SetRowSpacings(5)
tbl_SRR_error_settings$SetColSpacings(30)
tbl_SRR_error_settings$SetBorderWidth(5)
i=0   	# C0 -------------------------------------------
j=0    	# R0 '''''''''''''''''''
tbl_SRR_error_settings$Attach(frame_uncert_SRR_error,i, i+1, j, j+1) 



# ----------------------------------  tbl_SRR_uncert_settings ----------------------------------------------- 
tbl_SRR_uncert_settings$SetRowSpacings(5)
tbl_SRR_uncert_settings$SetColSpacings(30)
tbl_SRR_uncert_settings$SetBorderWidth(5)
i = 0 	# C0 -------------------------------------------
j = 0   # R0 '''''''''''''''''''
tbl_SRR_uncert_settings$Attach(tbl_distribution_file_SRR_uncert,i, i+1, j, j+1)                                                                
j = j+1 # R1 '''''''''''''''''''
# tbl_SRR_uncert_settings$Attach(hboxSR_params_fore_UN, i, i+1, j, j+1) 



# # ----------------------------------  tbl_recr_stock_recr_relat ----------------------------------------------- 
# tbl_recr_stock_recr_relat_UN$SetRowSpacings(5)
# tbl_recr_stock_recr_relat_UN$SetColSpacings(30)
# tbl_recr_stock_recr_relat_UN$SetBorderWidth(5)
# i=0  	# C0 ------------------------------------------- 
# j=0  	# R0 '''''''''''''''''''  
# tbl_recr_stock_recr_relat_UN$Attach(radio_forecast_recruits_costant_UN,i, i+1, j, j+1) 
# j=j+1   # R1 '''''''''''''''''''                                                                                             
# tbl_recr_stock_recr_relat_UN$Attach(radio_forecast_recruits_relationship_UN,i, i+1, j, j+1)
# i = i+1 # C1 -------------------------------------------
# j=0    	# R0 '''''''''''''''''''
# tbl_recr_stock_recr_relat_UN$Attach(tbl_constant_error_settings,i, i+1, j, j+1)
# j = j+1 # R1 ''''''''''''''''''' 
# tbl_recr_stock_recr_relat_UN$Attach(tbl_SRR_error_settings,i, i+1, j, j+1) 


# ----------------------------------  tbl_recr_stock_recr_relat ----------------------------------------------- 
tbl_recr_stock_recr_relat_UN$SetRowSpacings(5)
tbl_recr_stock_recr_relat_UN$SetColSpacings(30)
tbl_recr_stock_recr_relat_UN$SetBorderWidth(5)
i=0  	# C0 ------------------------------------------- 
j=0  	# R0 '''''''''''''''''''  
tbl_recr_stock_recr_relat_UN$Attach(radio_forecast_recruits_costant_UN,i, i+1, j, j+1) 
j=j+1   # R1 '''''''''''''''''''                                                                                             
tbl_recr_stock_recr_relat_UN$Attach(tbl_constant_error_settings,i, i+1, j, j+1)
i = i+1 # C1 -------------------------------------------
j=0    	# R0 '''''''''''''''''''
tbl_recr_stock_recr_relat_UN$Attach(radio_forecast_recruits_relationship_UN,i, i+1, j, j+1)
j = j+1 # R1 ''''''''''''''''''' 
tbl_recr_stock_recr_relat_UN$Attach(tbl_SRR_error_settings,i, i+1, j, j+1) 



 # 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같




# COMPOSIZIONE INTERFACCIA 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같

hbox_costant_recr_forecast_UN$packStart(entry_costant_recr_forecast_UN, expand = F, fill = F, padding = 5)

vbox_SRR_cost_mean_devSt$packStart(tbl_SRR_cost_mean_devSt, expand = F, fill = F, padding = 5)
vbox_SRR_a_b_c_params$packStart(tbl_SRR_a_b_c_params, expand = F, fill = F, padding = 5)

vbox_SRR_radio_plus_load$packStart(hbox_SRR_radio_plus_load, expand = F, fill = F, padding = 5)
hbox_SRR_radio_plus_load$packStart(radio_ext_file_SRR_uncert, expand = F, fill = F, padding = 5)
hbox_SRR_radio_plus_load$packStart(btn_load_external_SRR_err, expand = F, fill = F, padding = 5)

vbox_SRR_radio_plus_combo$packStart(hbox_SRR_radio_plus_combo, expand = F, fill = F, padding = 5)
hbox_SRR_radio_plus_combo$packStart(radio_distribution_SRR_uncert, expand = F, fill = F, padding = 5)
hbox_SRR_radio_plus_combo$packStart(combo_hboxSR_Distribution_Uncert_UN, expand = F, fill = F, padding = 5)

vbox_Costant_radio_plus_combo$packStart(hbox_Costant_radio_plus_combo, expand = F, fill = F, padding = 5)
hbox_Costant_radio_plus_combo$packStart(radio_recruitment_error_distribution_fore, expand = F, fill = F, padding = 5)
hbox_Costant_radio_plus_combo$packStart(combo_RecrNoise_dis_fore, expand = F, fill = F, padding = 5)

vbox_Costant_radio_plus_load$packStart(hbox_Costant_radio_plus_load, expand = F, fill = F, padding = 5)
hbox_Costant_radio_plus_load$packStart(radio_recruitment_error_ext_file_fore, expand = F, fill = F, padding = 5)
hbox_Costant_radio_plus_load$packStart(btn_load_external_recruitment_err_fore, expand = F, fill = F, padding = 5)


hbox_unitOfSpawners_UN$packStart(lblUnitSpawners_fore_UN, expand = F, fill = F, padding = 5)
hbox_unitOfSpawners_UN$packStart(hbox_entry_costant_recr_forecast_radios_UN, expand = F, fill = F, padding = 5)

hboxComboSRR_uncertaity$packStart(gtkLabel("SRR model"), expand = FALSE, fill = FALSE, padding = 5)
hboxComboSRR_uncertaity$packStart(combo_SRtype_fore_UN, expand = FALSE, fill = FALSE, padding = 5)

# vboxExternalRecrErr_btn_fore$packStart(btn_load_external_recruitment_err_fore, expand = FALSE, fill = FALSE, padding = 10)
# hboxExternalRecrErr_fore$packStart(vboxExternalRecrErr_btn_fore, expand = F, fill = T, padding = 10)
hboxExternalRecrErr_fore$packStart(extErrorRecruitment_fore.sw, expand = T, fill = T, padding = 5)

# vboxExternalSRRErr_btn_fore$packStart(btn_load_external_SRR_err, expand = FALSE, fill = FALSE, padding = 0)
# hboxExternalSRRErr_fore$packStart(vboxExternalSRRErr_btn_fore, expand = FALSE, fill = T, padding = 10)
hboxExternalSRRErr_fore$packStart(recruitments_fore_from_vector.sw_UN, expand = T, fill = T, padding = 5)

# hboxErrorNoise_dis_fore$packStart(combo_RecrNoise_dis_fore, expand = FALSE, fill = FALSE, padding = 10)
hboxErrorNoise_dis_fore$packStart(lbl_A_noise_fore, expand = FALSE, fill = FALSE, padding = 5)
hboxErrorNoise_dis_fore$packStart(entryNoise_a_fore, expand = FALSE, fill = FALSE, padding = 5)
hboxErrorNoise_dis_fore$packStart(lbl_B_noise_fore, expand = FALSE, fill = FALSE, padding = 5)
hboxErrorNoise_dis_fore$packStart(entryNoise_b_fore, expand = FALSE, fill = FALSE, padding = 5)


# hboxMean_StDev_SRR_params_a$packStart(combo_hboxSR_Distribution_Uncert_UN, expand = FALSE, fill = FALSE, padding = 10)
# hboxMean_StDev_SRR_params_a$packStart(gtkLabel("a"), expand = FALSE, fill = FALSE, padding = 10)
# hboxMean_StDev_SRR_params_a$packStart(gtkLabel("Mean"), expand = FALSE, fill = FALSE, padding = 5)
# hboxMean_StDev_SRR_params_a$packStart(entryMean_aSRR, expand = FALSE, fill = FALSE, padding = 5)
# hboxMean_StDev_SRR_params_a$packStart(gtkLabel("Sd"), expand = FALSE, fill = FALSE, padding = 5)
# hboxMean_StDev_SRR_params_a$packStart(entryStDev_aSRR, expand = FALSE, fill = FALSE, padding = 5)

# hboxMean_StDev_SRR_params_a$packStart(gtkLabel("b"), expand = FALSE, fill = FALSE, padding = 10)
# hboxMean_StDev_SRR_params_a$packStart(gtkLabel("Mean"), expand = FALSE, fill = FALSE, padding = 5)
# hboxMean_StDev_SRR_params_a$packStart(entryMean_bSRR, expand = FALSE, fill = FALSE, padding = 5)
# hboxMean_StDev_SRR_params_a$packStart(gtkLabel("Sd"), expand = FALSE, fill = FALSE, padding = 5)
# hboxMean_StDev_SRR_params_a$packStart(entryStDev_bSRR, expand = FALSE, fill = FALSE, padding = 5)

# hboxMean_StDev_SRR_params_a$packStart(gtkLabel("c"), expand = FALSE, fill = FALSE, padding = 10)
# hboxMean_StDev_SRR_params_a$packStart(gtkLabel("Mean"), expand = FALSE, fill = FALSE, padding = 5)
# hboxMean_StDev_SRR_params_a$packStart(entryMean_cSRR, expand = FALSE, fill = FALSE, padding = 5)
# hboxMean_StDev_SRR_params_a$packStart(gtkLabel("Sd"), expand = FALSE, fill = FALSE, padding = 5)
# hboxMean_StDev_SRR_params_a$packStart(entryStDev_cSRR, expand = FALSE, fill = FALSE, padding = 5)


h_frame_uncert_cost_error$packStart(tbl_Noise_fore, expand = FALSE, fill = FALSE, padding = 5)
frame_uncert_cost_error$add(h_frame_uncert_cost_error)

h_frame_uncert_SRR_error$packStart(tbl_SRR_uncert_settings, expand = FALSE, fill = FALSE, padding = 5)
frame_uncert_SRR_error$add(h_frame_uncert_SRR_error)



# hboxSR_params_fore_UN$packStart(lbl_SRparameters_fore_UN, expand = FALSE, fill = FALSE, padding = 5)
# hboxSR_params_fore_UN$packStart(lbl_SR_params_a_fore_UN, expand = FALSE, fill = FALSE, padding = 5)
# hboxSR_params_fore_UN$packStart(entrySR_params_a_fore_UN, expand = FALSE, fill = FALSE, padding = 5)
# hboxSR_params_fore_UN$packStart(lbl_SR_params_b_fore_UN, expand = FALSE, fill = FALSE, padding = 5)
# hboxSR_params_fore_UN$packStart(entrySR_params_b_fore_UN, expand = FALSE, fill = FALSE, padding = 5)
# hboxSR_params_fore_UN$packStart(lbl_SR_params_c_fore_UN, expand = FALSE, fill = FALSE, padding = 5)
# hboxSR_params_fore_UN$packStart(entrySR_params_c_fore_UN, expand = FALSE, fill = FALSE, padding = 5)
 
hbox_entry_costant_recr_forecast_radios_UN$packStart(radio_tons_fore_UN, expand = F, fill = F, padding = 5)
hbox_entry_costant_recr_forecast_radios_UN$packStart(radio_thousands_fore_UN, expand = F, fill = F, padding = 5)
 
hbox_recruits_fore_stockrecr_input_UN$packStart(tbl_recr_stock_recr_relat_UN, expand = F, fill = T, padding = 5)    
#hbox_recruits_fore_2input$packStart(hboxSR_params_fore, expand = F, fill = F, padding = 10)

vbox_recruits_fore_UN$packStart(hbox_recruits_fore_stockrecr_input_UN, expand = FALSE, fill = TRUE, padding = 5)
vbox_recruits_fore_UN$packStart(hbox_recruits_fore_UN, expand = FALSE, fill = TRUE, padding = 5)
vbox_recruits_fore_UN$packStart(hbox_unitOfSpawners_UN, expand = FALSE, fill = TRUE, padding = 5)

#hbox_numero_run$packStart(hbox_spunte_incertezza2, expand = F, fill = F, padding = 5)

# vbox_CI_fore$packStart(hbox_numero_run, expand = F, fill = F, padding = 10)


# hbox_CI_fore$packStart(tbl_CI_fore, expand = F, fill = F, padding = 10)

# hbox_CI_fore$packStart(lbl_CI_error_type_fore, expand = F, fill = F, padding = 10)
hbox_CI_fore$packStart(radio_CI_err_additive_fore, expand = F, fill = F, padding = 5)
hbox_CI_fore$packStart(radio_CI_err_multiplicative_fore, expand = F, fill = F, padding = 5)



# frame_recruits_fore_UN$add(vbox_recruits_fore_UN) 

frame_CI_fore$add(vbox_CI_fore)



h_frame_recruits_fore_UN$packStart(frame_recruits_fore_UN, expand = TRUE, fill = TRUE, padding = 5)		
 # vboxRecruitment_fore$packStart(hbox_spunte_incertezza, expand = FALSE, fill = FALSE, padding = 5)
 
    vboxRecruitment_fore$packStart(hbox_spunte_incertezza2, expand = FALSE, fill = FALSE, padding = 5)
	  vboxRecruitment_fore$packStart(hbox_spunte_incertezza3, expand = FALSE, fill = FALSE, padding = 5)


h_frame_CI_fore$packStart(frame_CI_fore, expand = TRUE, fill = TRUE, padding = 5)		



# ---------------------------------------- ACTIVATE/DEACTIVATE ALL THE OBJECTS
	gtkWidgetSetSensitive(vbox_recruits_fore_UN, FALSE)  
    gtkToggleButtonSetActive(radio_forecast_recruits_costant_UN, TRUE)
	gtkComboBoxSetActive(combo_SRtype_fore_UN, (which(SR_TYPE == SR_TYPE[1])-1) ) 
	gtkComboBoxSetActive(combo_hboxSR_Distribution_Uncert_UN, (which(DISTRIBUTION_UNCERT == DISTRIBUTION_UNCERT[1])-1) )  
		
	gtkToggleButtonSetActive(radio_recruitment_error_distribution_fore, TRUE)
	gtkToggleButtonSetActive(radio_forecast_recruits_costant_UN, TRUE)
    gtkToggleButtonSetActive(radio_forecast_recruits_relationship_UN, FALSE)
	gtkToggleButtonSetActive(radio_distribution_SRR_uncert, TRUE)	
	

