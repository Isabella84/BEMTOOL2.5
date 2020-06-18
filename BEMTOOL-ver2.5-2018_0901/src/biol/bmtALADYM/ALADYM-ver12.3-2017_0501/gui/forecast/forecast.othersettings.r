# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


 # COSTRUZIONE OGGETTI 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같
 
 
 # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   FINESTRE CON DATI 
 
 
 # # for the external vector - valori da usare ----------------------------------------------
# recruitments_fore_from_vector.sw <<- gtkScrolledWindowNew(NULL, NULL)
# recruitments_fore_from_vector.sw$setShadowType("etched-in")
# recruitments_fore_from_vector.sw$setPolicy("automatic", "automatic")
# recruitments_fore_from_vector.sw$SetUsize(100, dim_eff_tables)  
# recruitments_fore_from_vector <<- list()
# recruitments_fore_from_vector_index <<- 0
# recruitments_fore_from_vector.create_model()
# recruitments_fore_from_vector.treeview <<- gtkTreeViewNewWithModel( recruitments_fore_from_vector.model)
# recruitments_fore_from_vector.treeview$setRulesHint(TRUE)
# recruitments_fore_from_vector.treeview$getSelection()$setMode("single")
# recruitments_fore_from_vector.add_columns( recruitments_fore_from_vector.treeview)
# recruitments_fore_from_vector.sw$destroy()
# recruitments_fore_from_vector.sw <<- gtkScrolledWindowNew(NULL, NULL)
# recruitments_fore_from_vector.sw$setShadowType("etched-in")
# recruitments_fore_from_vector.sw$setPolicy("automatic", "automatic")
# recruitments_fore_from_vector.sw$SetUsize(100, dim_eff_tables)  
# recruitments_fore_from_vector.sw$add(recruitments_fore_from_vector.treeview)
# # -----------------------------------------------------------------------------------------


  # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   CHECK
chkFMSY <- gtkCheckButton("Reach F target in the given year")

gSignalConnect(chkFMSY, "toggled", change_chkFMSY)
  
 
  # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$  COMBO
combo_RecrNoise_dis_fore <- gtkComboBoxNewText()
for (choice in DISTRIBUTION) { 
if (choice != "Gamma")   {
combo_RecrNoise_dis_fore$appendText(choice) 
}
}


combo_SRtype_fore <- gtkComboBoxNewText()
for (choice in SR_TYPE) { 
if (choice != "from vector") { combo_SRtype_fore$appendText(choice) }
}  

combo_hboxSR_Distribution_Uncert <- gtkComboBoxNewText()
for (choice in DISTRIBUTION_UNCERT) { combo_hboxSR_Distribution_Uncert$appendText(choice) }


gSignalConnect(combo_SRtype_fore, "changed", deactivate_SR_unused_params_fore)
gSignalConnect(combo_hboxSR_Distribution_Uncert, "changed", change_CI_source_err_fore)       # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! sistemare qua
gSignalConnect(combo_RecrNoise_dis_fore, "changed", change_noise_AB_fore )


 # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   BUTTON

btn_load_recruitment_fromVector_fore <- gtkButton()

gtkButtonSetLabel(btn_load_recruitment_fromVector_fore, "Load...")

btn_load_recruitment_fromVector_fore$AddCallback("clicked", load_recruitment_fromVector_fore_file)

 # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   LABEL
lbl_TargetF <- gtkLabel("Target F")
lbl_TargetYear <- gtkLabel("Target year")
lbl_SRparameters_fore <- gtkLabel("SRR parameters")
lbl_SR_params_a_fore <- gtkLabel("a")
lbl_SR_params_b_fore <- gtkLabel("b")
lbl_SR_params_c_fore <- gtkLabel("c")
lbl_A_noise_fore <- gtkLabel("A")  
lbl_B_noise_fore <- gtkLabel("B")
lblUnitSpawners_fore <- gtkLabel("Unit for spawners")


gtkLabelSetWidthChars(lbl_A_noise_fore, LABEL_LENGTH) 
gtkLabelSetWidthChars(lbl_B_noise_fore, LABEL_LENGTH) 

 # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   ENTRY
entry_targetF <- gtkEntry() 
entry_targetMonth<- gtkEntry() 
entrySR_params_a_fore <- gtkEntry()
entrySR_params_b_fore <- gtkEntry()
entrySR_params_c_fore <- gtkEntry()
entryNoise_a_fore <- gtkEntry()
entryNoise_b_fore <- gtkEntry()
entry_costant_recr_forecast <- gtkEntry()

gtkEntrySetWidthChars(entry_costant_recr_forecast, NUMERICAL_ENTRY_LENGTH)
gtkEntrySetWidthChars(entryNoise_a_fore, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entryNoise_b_fore, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entrySR_params_a_fore, NUMERICAL_ENTRY_LENGTH)
gtkEntrySetWidthChars(entrySR_params_b_fore, NUMERICAL_ENTRY_LENGTH)
gtkEntrySetWidthChars(entrySR_params_c_fore, NUMERICAL_ENTRY_LENGTH)
gtkEntrySetWidthChars(entry_targetF, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetWidthChars(entry_targetMonth, NUMERICAL_ENTRY_LENGTH)  

if (phase=="FORECAST") {
gtkEntrySetText(entry_costant_recr_forecast, INP$Recruits[(simperiod*12+1)] )
}
 # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   RADIO

radio_forecast_recruits_costant <- gtkRadioButton()
radio_forecast_recruits_costant$add(gtkLabel("Constant"))
radio_forecast_recruits_relationship <- gtkRadioButtonNewWithLabelFromWidget(radio_forecast_recruits_costant, "Stock-recruitment relationship")

radio_forecast_recruits_costant_vect_C <- gtkRadioButton()
radio_forecast_recruits_costant_vect_C$add(gtkLabel("Scalar"))
radio_forecast_recruits_costant_vect_V <- gtkRadioButtonNewWithLabelFromWidget(radio_forecast_recruits_costant_vect_C, "Vector")          
     
radio_tons_fore <- gtkRadioButton()
radio_tons_fore$add(gtkLabel("Tons (biomass)"))
radio_thousands_fore <- gtkRadioButtonNewWithLabelFromWidget(radio_tons_fore, "Thousands (number)")

gSignalConnect(radio_forecast_recruits_costant, "toggled", deactivate_costant_relatinship_recr_fore)
gSignalConnect(radio_forecast_recruits_relationship, "toggled", deactivate_costant_relatinship_recr_fore)
gSignalConnect(radio_forecast_recruits_costant_vect_C, "toggled", deactivate_scalar_costant_vector_recruitment_fore)         
gSignalConnect(radio_forecast_recruits_costant_vect_V, "toggled", deactivate_scalar_costant_vector_recruitment_fore)  
  
 # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   HORIZONTAL BOX
hboxOtherSettings <- gtkHBox(FALSE, 5)
hbox_fore_alady_settings <- gtkHBox()
hbox_entry_costant_recr_forecast <- gtkHBox()
hboxSRType_fore <- gtkHBox(FALSE, 5)
hboxSR_params_fore <- gtkHBox(FALSE, 5)
# hbox_recruits_fore <- gtkHBox(homogeneous = FALSE, 5)
hbox_unitOfSpawners <- gtkHBox(homogeneous = FALSE, 5)
hbox_recruits_fore_stockrecr_input <- gtkHBox()
h_frame_recruits_fore <- gtkHBox(homogeneous = FALSE, 5)
hbox_entry_costant_recr_forecast_radios <- gtkHBox()
# hbox_spunte_incertezza2 <- gtkHBox()

 # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   VERTICAL BOX
vboxOtherSettings <- gtkVBox(FALSE, 5)
vboxRecruitment_fore <- gtkVBox(homogeneous = FALSE, 5)
vbox_recruits_fore <- gtkVBox(homogeneous = FALSE, 5)  
vbox_btn_load_Recr_from_vect <- gtkVBox(FALSE, 5)           

 # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   FRAME
frame_recruits_fore <- gtkFrame(" Recruitment for the forecast ")   

 # $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   TABELLE 
 tbl_recr_stock_recr_relat <- gtkTable(3, 3,homogeneous = FALSE)

 
# tbl_recr_stock_recr_relat -------------------------------------------
tbl_recr_stock_recr_relat$SetRowSpacings(10)
tbl_recr_stock_recr_relat$SetColSpacings(30)
tbl_recr_stock_recr_relat$SetBorderWidth(5)
i=0  	# C0 ------------------------------------- 
j=0  	# R0 ''''''''''''''''''''''''                                
tbl_recr_stock_recr_relat$Attach(radio_forecast_recruits_costant,i, i+1, j, j+1) 
j=j+1  	# R1 ''''''''''''''''''''''''                                 
tbl_recr_stock_recr_relat$Attach(radio_forecast_recruits_relationship,i, i+1, j, j+1)
i = i+1 # C1 ------------------------------------- 
j=0     # R0 ''''''''''''''''''''''''
tbl_recr_stock_recr_relat$Attach(hbox_entry_costant_recr_forecast,i, i+2, j, j+1)   
j = j+1 # R1 '''''''''''''''''''''''' 
tbl_recr_stock_recr_relat$Attach(hboxSRType_fore,i, i+1, j, j+1) 
i = i+1 # C2 ------------------------------------- 
j=0     # R0 ''''''''''''''''''''''''
j = j+1 # R1 ''''''''''''''''''''''''
tbl_recr_stock_recr_relat$Attach(hboxSR_params_fore,i, i+1, j, j+1) 
 
 


# COMPOSIZIONE INTERFACCIA 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같


hbox_fore_alady_settings$packStart(chkFMSY, expand = FALSE, fill = FALSE, padding = 5)
hbox_fore_alady_settings$packStart(lbl_TargetF, expand = FALSE, fill = FALSE, padding = 5)
hbox_fore_alady_settings$packStart(entry_targetF, expand = FALSE, fill = FALSE, padding = 5)
hbox_fore_alady_settings$packStart(lbl_TargetYear, expand = FALSE, fill = FALSE, padding = 5)
hbox_fore_alady_settings$packStart(entry_targetMonth, expand = FALSE, fill = FALSE, padding = 5)

vboxOtherSettings$packStart(hbox_fore_alady_settings, expand = FALSE, fill = FALSE, padding = 10)
hboxOtherSettings$packStart(vboxOtherSettings, expand = FALSE, fill = FALSE, padding = 5)


#lbl_min_noise_fore <-  gtkLabel("min") 

hboxSRType_fore$packStart(gtkLabel("SRR model"), expand = FALSE, fill = FALSE, padding = 5)
hboxSRType_fore$packStart(combo_SRtype_fore, expand = FALSE, fill = FALSE, padding = 5)

hboxSR_params_fore$packStart(lbl_SRparameters_fore, expand = FALSE, fill = FALSE, padding = 5)
hboxSR_params_fore$packStart(lbl_SR_params_a_fore, expand = FALSE, fill = FALSE, padding = 5)
hboxSR_params_fore$packStart(entrySR_params_a_fore, expand = FALSE, fill = FALSE, padding = 5)
hboxSR_params_fore$packStart(lbl_SR_params_b_fore, expand = FALSE, fill = FALSE, padding = 5)
hboxSR_params_fore$packStart(entrySR_params_b_fore, expand = FALSE, fill = FALSE, padding = 5)
hboxSR_params_fore$packStart(lbl_SR_params_c_fore, expand = FALSE, fill = FALSE, padding = 5)
hboxSR_params_fore$packStart(entrySR_params_c_fore, expand = FALSE, fill = FALSE, padding = 5)


hbox_unitOfSpawners$packStart(lblUnitSpawners_fore, expand = F, fill = F, padding = 10)
hbox_unitOfSpawners$packStart(hbox_entry_costant_recr_forecast_radios, expand = F, fill = F, padding = 10)

# !!!!!!!!!!!!!!!!!!! serve per la scelta tra scalare e vettore
# hbox_entry_costant_recr_forecast$packStart(radio_forecast_recruits_costant_vect_C, expand = F, fill = F, padding = 0)
hbox_entry_costant_recr_forecast$packStart(entry_costant_recr_forecast, expand = F, fill = F, padding = 10)
# !!!!!!!!!!!!!!!!!!! serve per la scelta tra scalare e vettore
# hbox_entry_costant_recr_forecast$packStart(radio_forecast_recruits_costant_vect_V, expand = F, fill = F, padding = 10)


vbox_btn_load_Recr_from_vect$packStart(btn_load_recruitment_fromVector_fore, expand = FALSE, fill = FALSE, padding = 0)
# !!!!!!!!!!!!!!!!!!! serve per la scelta tra scalare e vettore
# hbox_entry_costant_recr_forecast$packStart(vbox_btn_load_Recr_from_vect, expand = F, fill = F, padding = 10)
# !!!!!!!!!!!!!!!!!!! serve per la scelta tra scalare e vettore
# hbox_entry_costant_recr_forecast$packStart(recruitments_fore_from_vector.sw , TRUE, TRUE, 0) 


hbox_entry_costant_recr_forecast_radios$packStart(radio_tons_fore, expand = F, fill = F, padding = 0)
hbox_entry_costant_recr_forecast_radios$packStart(radio_thousands_fore, expand = F, fill = F, padding = 10)

hbox_recruits_fore_stockrecr_input$packStart(tbl_recr_stock_recr_relat, expand = FALSE, fill = T, padding = 15)

vbox_recruits_fore$packStart(hbox_recruits_fore_stockrecr_input, expand = FALSE, fill = TRUE, padding = 0)
vbox_recruits_fore$packStart(hbox_unitOfSpawners, expand = FALSE, fill = TRUE, padding = 5)

# vbox_recruits_fore$packStart(hbox_recruits_fore, expand = FALSE, fill = TRUE, padding = 5)

frame_recruits_fore$add(vbox_recruits_fore) 


h_frame_recruits_fore$packStart(frame_recruits_fore, expand = TRUE, fill = TRUE, padding = 10)		
vboxRecruitment_fore$packStart(h_frame_recruits_fore, expand = FALSE, fill = FALSE, padding = 5)

#h_frame_CI_fore <- gtkHBox(homogeneous = FALSE, 5)
#h_frame_CI_fore$packStart(frame_CI_fore, expand = TRUE, fill = TRUE, padding = 10)		
#vboxRecruitment_fore$packStart(h_frame_CI_fore, expand = FALSE, fill = FALSE, padding = 5)

vboxOtherSettings$packStart(vboxRecruitment_fore, expand = FALSE, fill = FALSE, padding = 5)



# qui finisce  la prima parte (deterministica)

suppressWarnings(source(paste(ALADYM_home, "/gui/forecast/uncertainty.r", sep="") ) ) 

    
if (IN_BEMTOOL ) { 
if (BMT_SCENARIO == BMT_HR_CHANGE_FISHMORTALITY) { 
 gtkEntrySetText(entry_targetF, Ref_point )   
  gtkEntrySetText(entry_targetMonth, years.forecast[bmt_time_span])   
  gtkToggleButtonSetActive(chkFMSY, TRUE) 
 gtkWidgetSetSensitive(entry_targetF, FALSE)     
  gtkWidgetSetSensitive(entry_targetMonth, FALSE) 
  gtkWidgetSetSensitive(lbl_TargetF, FALSE)    
  gtkWidgetSetSensitive(lbl_TargetYear, FALSE)
  
associated_fleetsegment <<- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <<- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!="" & associated_fleetsegment!="-"]
associated_fleetsegment_indices <<- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)
  
      fl_ord <- 1
        for (n_int in 1:length(BMT_FLEETSEGMENTS)) {
        if (n_int %in% associated_fleetsegment_indices) {
           # FleetList_forecast[[1]]
 FleetList_forecast[[fl_ord]]@scenario.reduction <- Forecast_reduction[1,fl_ord]   
           fl_ord <- fl_ord + 1
           }            
        }  
				    
 } 
}


  
    
  # ---------------------------------------- ACTIVATE/DEACTIVATE ALL THE OBJECTS
  
   gtkToggleButtonSetActive(radio_forecast_recruits_costant, TRUE)
  gtkWidgetSetSensitive(hboxSRType_fore, FALSE)
 gtkWidgetSetSensitive(hboxSR_params_fore, FALSE)
 
gtkComboBoxSetActive(combo_SRtype_fore, (which(SR_TYPE == SR_TYPE[1])-1) ) 
          gtkComboBoxSetActive(combo_RecrNoise_dis_fore, (which(DISTRIBUTION == DISTRIBUTION[1])-1) ) 
		  gtkComboBoxSetActive(combo_RecrNoise_dis_fore, 3 )

		   gtkWidgetSetSensitive(entry_targetF, FALSE)     
  gtkWidgetSetSensitive(entry_targetMonth, FALSE) 
  gtkWidgetSetSensitive(lbl_TargetF, FALSE)    
  gtkWidgetSetSensitive(lbl_TargetYear, FALSE)

       gtkEntrySetText(entryNoise_a_fore, "0") 
    gtkEntrySetText(entryNoise_b_fore, "0.3")   



 # gtkWidgetSetSensitive(lbl_errorsource_ci_fore, F)
  # gtkWidgetSetSensitive(radio_recruitment_error_distribution_fore, F)
   # gtkWidgetSetSensitive(radio_recruitment_error_ext_file_fore, F)
     # gtkWidgetSetSensitive(hboxNumRUNSCI_fore, F)
      # gtkWidgetSetSensitive(hboxErrorNoise_dis_fore, F)
       # gtkWidgetSetSensitive(hboxExternalRecrErr_fore, F)
        # gtkWidgetSetSensitive(lbl_errortype_ci_fore, F)
         # gtkWidgetSetSensitive(hboxErrorType_fore, F)  
          # gtkComboBoxSetActive(combo_SRtype_fore, 0 )

    # gtkToggleButtonSetActive(radio_CI_err_multiplicative_fore, TRUE)
	# gtkWidgetSetSensitive(h_frame_CI_fore, FALSE)


	# hbox_tbl_CI_fore <- gtkHBox(homogeneous = FALSE, 5)
#hbox_tbl_CI_fore$packStart(tbl_Noise_fore, expand = FALSE, fill = F, padding = 15)
#vbox_CI_fore$packStart(hbox_tbl_CI_fore, expand = FALSE, fill = F, padding = 10)

#
#gtkComboBoxSetActive(combo_RecrNoise_dis_fore, 3 )
#gtkEntrySetText(entry_noise_recr_max_fore, 1)
#gtkEntrySetText(entry_noise_recr_min_fore, 1)
#
#gtkToggleButtonSetActive(radio_forecast_recruits_costant, T)





    