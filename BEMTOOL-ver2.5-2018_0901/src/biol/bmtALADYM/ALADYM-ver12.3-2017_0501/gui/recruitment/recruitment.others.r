# BEMTOOL - Bio-Economic Model TOOLs - version 2.0
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





vboxRecrOthers <- gtkVBox(FALSE, 5)

# RECRUITMENT CALIBRATION

btn_load_external_recruitment_err <- gtkButton()
gtkButtonSetLabel(btn_load_external_recruitment_err, "Load...")
btn_load_external_recruitment_err$AddCallback("clicked", load_extErrorRecruitment_file)


 lbl_min_rec <- gtkLabel("min rec")   
 lbl_max_rec <- gtkLabel("max rec")
 
entry_minrec <- gtkEntry()
gtkEntrySetWidthChars(entry_minrec, NUMERICAL_ENTRY_LENGTH)
entry_maxrec <- gtkEntry()
gtkEntrySetWidthChars(entry_maxrec, NUMERICAL_ENTRY_LENGTH)

chkCalibration <- gtkCheckButton("Use recruitment calibration")
gSignalConnect(chkCalibration, "toggled", change_calibration_input)

# ----------------------------------------------------------------------------------------------------------
 


# CONFIDENCE INTEEVALS

chkConfidenceIntervals <- gtkCheckButton("Calculate confidence intervals")
gSignalConnect(chkConfidenceIntervals, "toggled", change_CI_input)

radio_CI_err_additive <- gtkRadioButton()
radio_CI_err_additive$add(gtkLabel("Additive error"))
radio_CI_err_multiplicative <- gtkRadioButtonNewWithLabelFromWidget(radio_CI_err_additive, "Multiplicative error")

radio_recruitment_error_ext_file <- gtkRadioButton()
radio_recruitment_error_ext_file$add(gtkLabel("from external file"))
radio_recruitment_error_distribution <- gtkRadioButtonNewWithLabelFromWidget(radio_recruitment_error_ext_file, "from distribution")
gSignalConnect(radio_recruitment_error_ext_file, "toggled", change_CI_source_err)
gSignalConnect(radio_recruitment_error_distribution, "toggled", change_CI_source_err)

lbl_CI_num_runs <- gtkLabel("no. of runs") 
 entry_CI_numb_runs <- gtkEntry()
gtkEntrySetWidthChars(entry_CI_numb_runs, NUMERICAL_ENTRY_LENGTH)


# ---------------------------------------------------------------------------------------------------



# for the distribution input

combo_RecrNoise_dis <- gtkComboBoxNewText()
gSignalConnect(combo_RecrNoise_dis, "changed", change_noise_AB )
for (choice in DISTRIBUTION) { combo_RecrNoise_dis$appendText(choice) }

lbl_min_noise <- gtkLabel("min")
lbl_max_noise <-  gtkLabel("max")
 entry_noise_recr_min <- gtkEntry()
gtkEntrySetWidthChars(entry_noise_recr_min, NUMERICAL_ENTRY_LENGTH)
entry_noise_recr_max <- gtkEntry()
gtkEntrySetWidthChars(entry_noise_recr_max, NUMERICAL_ENTRY_LENGTH)

lbl_A_noise <- gtkLabel("A")
gtkLabelSetWidthChars(lbl_A_noise, LABEL_LENGTH)     
lbl_B_noise <- gtkLabel("B")
gtkLabelSetWidthChars(lbl_B_noise, LABEL_LENGTH) 
entryNoise_a <- gtkEntry()
gtkEntrySetWidthChars(entryNoise_a, NUMERICAL_ENTRY_LENGTH)
entryNoise_b <- gtkEntry()
gtkEntrySetWidthChars(entryNoise_b, NUMERICAL_ENTRY_LENGTH)


# -----------------------------------------------------------------------------------------------------------
  


  # for the external vector ----------------------------------------------
  extErrorRecruitment.sw <<- gtkScrolledWindowNew(NULL, NULL)
extErrorRecruitment.sw$setShadowType("etched-in")
extErrorRecruitment.sw$setPolicy("automatic", "automatic")
extErrorRecruitment.sw$SetUsize(100, 65)  
extErrorRecruitment_list <<- list()
extErrorRecruitmentIndex <<- 0
# ------------------------------
# create model
extErrorRecruitment.create_model()
# create tree view
extErrorRecruitment.treeview <<- gtkTreeViewNewWithModel(extErrorRecruitment.model)
extErrorRecruitment.treeview$setRulesHint(TRUE)
extErrorRecruitment.treeview$getSelection()$setMode("single")
extErrorRecruitment.add_columns(extErrorRecruitment.treeview)
extErrorRecruitment.sw$add(extErrorRecruitment.treeview)      
  # ------------------------------------------------------------

 
vboxNoise <- gtkVBox(FALSE, 5)
			
tbl_Noise <- gtkTable(4,5,homogeneous = FALSE)
tbl_Noise$SetRowSpacings(10)
tbl_Noise$SetColSpacings(10)
tbl_Noise$SetBorderWidth(5)

i=0   # column 1    ---------------------------------------------------------------------------------
j=0   # row 1 
tbl_Noise$Attach(chkCalibration,i, i+2, j, j+1) 
j=j+1  # row 2  
#tbl_Noise$Attach(chkConfidenceIntervals,i, i+2, j, j+1)  
j=j+1  # row 3
lbl_errorsource_ci <- gtkLabel("Error source")
#tbl_Noise$Attach(lbl_errorsource_ci, i, i+1, j, j+1) 
j=j+1  # row 4


i=i+1  # column 2   ---------------------------------------------------------------------------------
j=0   # row 1 
j=j+1  # row 2
j=j+1  # row 3
#tbl_Noise$Attach(radio_recruitment_error_distribution,i, i+1, j, j+1) 
j=j+1 # row 4 
#tbl_Noise$Attach(radio_recruitment_error_ext_file,i, i+1, j, j+1) 


i=i+1  # column 2.bis   ---------------------------------------------------------------------------------
j=0   # row 1 
hboxCalibrationInputs_min_max <- gtkHBox(FALSE, 5)
hboxCalibrationInputs_min_max$packStart(lbl_min_rec, expand = FALSE, fill = FALSE, padding = 10)
hboxCalibrationInputs_min_max$packStart(entry_minrec, expand = FALSE, fill = FALSE, padding = 10)
hboxCalibrationInputs_min_max$packStart(lbl_max_rec, expand = FALSE, fill = FALSE, padding = 10)
hboxCalibrationInputs_min_max$packStart(entry_maxrec, expand = FALSE, fill = FALSE, padding = 10)
tbl_Noise$Attach(hboxCalibrationInputs_min_max,i, i+3, j, j+1)
j=j+1  # row 2
hboxNumRUNSCI <- gtkHBox()
 hboxNumRUNSCI$packStart(lbl_CI_num_runs, expand = FALSE, fill = FALSE, padding = 10)
 hboxNumRUNSCI$packStart(entry_CI_numb_runs, expand = FALSE, fill = FALSE, padding = 10)
#tbl_Noise$Attach(hboxNumRUNSCI,i, i+1, j, j+1)  
j=j+1  # row 3
hboxErrorNoise_dis <- gtkHBox(FALSE, 5)
hboxErrorNoise_dis$packStart(gtkLabel("Error distribution"), expand = FALSE, fill = FALSE, padding = 10)
hboxErrorNoise_dis$packStart(combo_RecrNoise_dis, expand = FALSE, fill = FALSE, padding = 10)
hboxErrorNoise_dis$packStart(lbl_min_noise, expand = FALSE, fill = FALSE, padding = 10)
hboxErrorNoise_dis$packStart(entry_noise_recr_min, expand = FALSE, fill = FALSE, padding = 10)
hboxErrorNoise_dis$packStart(lbl_max_noise, expand = FALSE, fill = FALSE, padding = 10)
hboxErrorNoise_dis$packStart(entry_noise_recr_max, expand = FALSE, fill = FALSE, padding = 10)
hboxErrorNoise_dis$packStart(lbl_A_noise, expand = FALSE, fill = FALSE, padding = 10)
hboxErrorNoise_dis$packStart(entryNoise_a, expand = FALSE, fill = FALSE, padding = 10)
hboxErrorNoise_dis$packStart(lbl_B_noise, expand = FALSE, fill = FALSE, padding = 10)
hboxErrorNoise_dis$packStart(entryNoise_b, expand = FALSE, fill = FALSE, padding = 10)
#tbl_Noise$Attach(hboxErrorNoise_dis,i, i+7, j, j+1)  
j=j+1 # row 4 
hboxExternalRecrErr <- gtkHBox(FALSE, 5)
vboxExternalRecrErr_btn <- gtkVBox(FALSE, 5)
vboxExternalRecrErr_btn$packStart(btn_load_external_recruitment_err, expand = FALSE, fill = FALSE, padding = 10)
hboxExternalRecrErr$packStart(vboxExternalRecrErr_btn, expand = FALSE, fill = T, padding = 10)
hboxExternalRecrErr$packStart(extErrorRecruitment.sw, expand = T, fill = T, padding = 10)
#tbl_Noise$Attach(hboxExternalRecrErr,i, i+7, j, j+1)  

i=i+1  # column 3   ---------------------------------------------------------------------------------
j=0   # row 1

j=j+1  # row 2
lbl_errortype_ci <- gtkLabel("Error type")
# tbl_Noise$Attach(lbl_errortype_ci ,i, i+1, j, j+1)
j=j+1  # row 3
 
j=j+1 # row 4

  

i=i+1   # column 4   ---------------------------------------------------------------------------------
j=0     # row 1

j=j+1   # row 2
hboxErrorType <- gtkHBox(FALSE, 5)
hboxErrorType$packStart(radio_CI_err_additive, expand = FALSE, fill = FALSE, padding = 10)
hboxErrorType$packStart(radio_CI_err_multiplicative, expand = FALSE, fill = FALSE, padding = 10)
#tbl_Noise$Attach(hboxErrorType,i, i+1, j, j+1) 
  j=j+1  # row 3
   j=j+1 # row 4 


vboxNoise$packStart(tbl_Noise, expand = TRUE, fill = TRUE)

vboxRecrOthers$packStart(vboxNoise, expand = FALSE, fill = FALSE, padding = 5)

gtkComboBoxSetActive(combo_RecrNoise_dis, 3 )
gtkEntrySetText(entry_noise_recr_max, 1)
gtkEntrySetText(entry_noise_recr_min, 1)

       # default
   gtkWidgetSetSensitive(entry_maxrec, FALSE)
   gtkWidgetSetSensitive(entry_minrec, FALSE)
   gtkWidgetSetSensitive(lbl_min_rec, FALSE)
   gtkWidgetSetSensitive(lbl_max_rec, FALSE)
   
 gtkWidgetSetSensitive(lbl_errorsource_ci, F)
  gtkWidgetSetSensitive(radio_recruitment_error_distribution, F)
   gtkWidgetSetSensitive(radio_recruitment_error_ext_file, F)
     gtkWidgetSetSensitive(hboxNumRUNSCI, F)
      gtkWidgetSetSensitive(hboxErrorNoise_dis, F)
       gtkWidgetSetSensitive(hboxExternalRecrErr, F)
        gtkWidgetSetSensitive(lbl_errortype_ci, F)
         gtkWidgetSetSensitive(hboxErrorType, F)  
                        