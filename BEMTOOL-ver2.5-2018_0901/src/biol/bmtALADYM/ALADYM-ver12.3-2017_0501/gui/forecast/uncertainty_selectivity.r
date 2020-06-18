 # ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




 # COSTRUZIONE OGGETTI 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같
 
 
 # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   FINESTRE CON DATI 


# for selectivity parameters from vector ----------------------------------------------
selectivity_uncert_vector_from_file_sw <<- gtkScrolledWindowNew(NULL, NULL)
selectivity_uncert_vector_from_file_sw$setShadowType("etched-in")
selectivity_uncert_vector_from_file_sw$setPolicy("automatic", "automatic")
selectivity_uncert_vector_from_file_sw$SetUsize(500, dim_big_tables)  
selectivity_uncert_vector_from_file_list <<- list()
selectivity_uncert_vector_from_file_index <<- 0
selectivity_uncert_vector_from_file_create_model()
selectivity_uncert_vector_from_file_treeview <<- gtkTreeViewNewWithModel( selectivity_uncert_vector_from_file_model)
selectivity_uncert_vector_from_file_treeview$setRulesHint(TRUE)
selectivity_uncert_vector_from_file_treeview$getSelection()$setMode("single")
selectivity_uncert_vector_from_file_add_columns( selectivity_uncert_vector_from_file_treeview)
selectivity_uncert_vector_from_file_sw$destroy()
selectivity_uncert_vector_from_file_sw <<- gtkScrolledWindowNew(NULL, NULL)
selectivity_uncert_vector_from_file_sw$setShadowType("etched-in")
selectivity_uncert_vector_from_file_sw$setPolicy("automatic", "automatic")
selectivity_uncert_vector_from_file_sw$SetUsize(500, dim_big_tables)  
selectivity_uncert_vector_from_file_sw$add(selectivity_uncert_vector_from_file_treeview)
# ---------------------------------------------------------------------------------


# for selectivity parameters from distribution ----------------------------------------------
selectivity_uncert_distribution_from_file_sw <<- gtkScrolledWindowNew(NULL, NULL)
selectivity_uncert_distribution_from_file_sw$setShadowType("etched-in")
selectivity_uncert_distribution_from_file_sw$setPolicy("automatic", "automatic")
selectivity_uncert_distribution_from_file_sw$SetUsize(500, dim_big_tables)  
selectivity_uncert_distribution_from_file_list <<- list()
selectivity_uncert_distribution_from_file_index <<- 0
selectivity_uncert_distribution_from_file_create_model()
selectivity_uncert_distribution_from_file_treeview <<- gtkTreeViewNewWithModel( selectivity_uncert_distribution_from_file_model)
selectivity_uncert_distribution_from_file_treeview$setRulesHint(TRUE)
selectivity_uncert_distribution_from_file_treeview$getSelection()$setMode("single")
selectivity_uncert_distribution_from_file_add_columns( selectivity_uncert_distribution_from_file_treeview)
selectivity_uncert_distribution_from_file_sw$destroy()
selectivity_uncert_distribution_from_file_sw <<- gtkScrolledWindowNew(NULL, NULL)
selectivity_uncert_distribution_from_file_sw$setShadowType("etched-in")
selectivity_uncert_distribution_from_file_sw$setPolicy("automatic", "automatic")
selectivity_uncert_distribution_from_file_sw$SetUsize(500, dim_big_tables)  
selectivity_uncert_distribution_from_file_sw$add(selectivity_uncert_distribution_from_file_treeview)
# ---------------------------------------------------------------------------------


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   CHECKS 


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   COMBO 


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   BUTTON 

btn_load_selectivity_uncert_distribution_from_file <- gtkButton()
btn_load_selectivity_uncert_vector_from_file <- gtkButton()

gtkButtonSetLabel(btn_load_selectivity_uncert_distribution_from_file, "Load...")
gtkButtonSetLabel(btn_load_selectivity_uncert_vector_from_file, "Load...")

btn_load_selectivity_uncert_distribution_from_file$AddCallback("clicked", load_selectivity_uncert_distribution_from_file )
btn_load_selectivity_uncert_vector_from_file$AddCallback("clicked", load_selectivity_uncert_vector_from_file )


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   LABEL 


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   ENTRY 


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   RADIO 

radio_selectivity_uncert_from_distribution <- gtkRadioButton()
radio_selectivity_uncert_from_distribution$add(gtkLabel("from distribution"))
radio_selectivity_uncert_from_external_file <- gtkRadioButtonNewWithLabelFromWidget(radio_selectivity_uncert_from_distribution, "from external file")

gSignalConnect(radio_selectivity_uncert_from_distribution, "toggled", deactivate_selectivity_uncertainty_distr_extfile )
gSignalConnect(radio_selectivity_uncert_from_external_file, "toggled", deactivate_selectivity_uncertainty_distr_extfile )

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   HORIZONTAL BOX 

# hbox_growth_costant_params <- gtkHBox(FALSE, 5)
# hbox_selectivity_uncert_frames <- gtkHBox(FALSE, 5)
# h_frame_maturity_uncert_males <- gtkHBox(FALSE, 5)
# h_frame_maturity_uncert_females <- gtkHBox(FALSE, 5)

hbox_selectivity_radio_plus_load_vector <- gtkHBox()
hbox_selectivity_radio_plus_load_distribution <- gtkHBox()
hbox_selectivity_settings <- gtkHBox()

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   VERTICAL BOX 

vbox_global_selectivity_uncert <- gtkVBox(FALSE, 5)
vbox_load_vector_button <- gtkVBox(FALSE, 5)
vbox_load_distribution_button <- gtkVBox(FALSE, 5)


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   FRAME 

# frame_maturity_uncert_males <- gtkFrame(" Error on MALE ")  
# frame_maturity_uncert_females <- gtkFrame(" Error on FEMALE ")  
       

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   TABELLE 

# tbl_maturity_costant_params <- gtkTable(3,3,homogeneous = TRUE)
# tbl_maturity_settings_params <- gtkTable(1,2,homogeneous = FALSE)
# tbl_maturity_mean_devSt_males <-  gtkTable(3, 3,homogeneous = FALSE)
# tbl_maturity_mean_devSt_females <-  gtkTable(3, 3,homogeneous = FALSE)
tbl_selectivity_settings <-  gtkTable(3, 3,homogeneous = FALSE)
# tbl_maturity_females_settings <-  gtkTable(2, 3,homogeneous = FALSE)



# ----------------------------------  tbl_maturity_males_settings ----------------------------------------------- 
tbl_selectivity_settings$SetRowSpacings(5)
tbl_selectivity_settings$SetColSpacings(5)
tbl_selectivity_settings$SetBorderWidth(5)
i=0   	# C0  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
tbl_selectivity_settings$Attach(gtkLabel("Source of error"),i, i+1, j, j+1)
j=j+1  	# R1 '''''''''''''''''''
tbl_selectivity_settings$Attach(vbox_load_distribution_button,i, i+1, j, j+1)   
j=j+1   # R2 '''''''''''''''''''
tbl_selectivity_settings$Attach(vbox_load_vector_button,i, i+1, j, j+1) 
i=i+1   # C3  -------------------------------------------
j=0   	# R0 '''''''''''''''''''
j=j+1   # R1 '''''''''''''''''''
tbl_selectivity_settings$Attach(selectivity_uncert_distribution_from_file_sw,i, i+1, j, j+1) 
j=j+1   # R2 '''''''''''''''''''  
tbl_selectivity_settings$Attach(selectivity_uncert_vector_from_file_sw,i, i+1, j, j+1) 



 # 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같



# COMPOSIZIONE INTERFACCIA 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같

vbox_global_selectivity_uncert$packStart(hbox_selectivity_settings, expand = F, fill = F, padding = 10)

vbox_load_vector_button$packStart(hbox_selectivity_radio_plus_load_vector, expand = F, fill = F, padding = 10)
vbox_load_distribution_button$packStart(hbox_selectivity_radio_plus_load_distribution, expand = F, fill = F, padding = 10)


hbox_selectivity_radio_plus_load_vector$packStart(radio_selectivity_uncert_from_external_file, expand = F, fill = F, padding = 10)
hbox_selectivity_radio_plus_load_vector$packStart(btn_load_selectivity_uncert_vector_from_file, expand = F, fill = F, padding = 10)

hbox_selectivity_radio_plus_load_distribution$packStart(radio_selectivity_uncert_from_distribution, expand = F, fill = F, padding = 10)
hbox_selectivity_radio_plus_load_distribution$packStart(btn_load_selectivity_uncert_distribution_from_file, expand = F, fill = F, padding = 10)

hbox_selectivity_settings$packStart(tbl_selectivity_settings, expand = F, fill = F, padding = 10)

# ---------------------------------------- ACTIVATE/DEACTIVATE ALL THE OBJECTS
 						
gtkWidgetSetSensitive(vbox_global_selectivity_uncert, FALSE)  
									
gtkToggleButtonSetActive(radio_selectivity_uncert_from_distribution, TRUE)
gtkWidgetSetSensitive(btn_load_selectivity_uncert_distribution_from_file, T)
gtkWidgetSetSensitive(selectivity_uncert_distribution_from_file_sw, T)
gtkWidgetSetSensitive(btn_load_selectivity_uncert_vector_from_file, F)
gtkWidgetSetSensitive(selectivity_uncert_vector_from_file_sw, F)

 

		