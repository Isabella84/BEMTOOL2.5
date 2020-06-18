# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





hboxDiscard_fore_calculation <- gtkHBox(FALSE, 5)
hboxDiscard_fore_calculation$packStart(gtkLabel("Discard calculation"), expand = FALSE, fill = FALSE, padding = 5) 

combo_discard_fore <- gtkComboBoxNewText()
gSignalConnect(combo_discard_fore, "changed", deactivate_Discard_unused_params_fore)
for (choice in DISCARD_CALC) {
combo_discard_fore$appendText(choice)
}


hboxDiscard_fore_calculation$packStart(combo_discard_fore, expand = FALSE, fill = FALSE, padding = 5)
vboxDiscard_fore$packStart(hboxDiscard_fore_calculation, expand = FALSE, fill = FALSE, padding = 10)    

radio_discard_revogive_fore <- gtkRadioButton()
radio_discard_revogive_fore$add(gtkLabel("Reverse ogive"))
radio_discard_vector_fore <- gtkRadioButtonNewWithLabelFromWidget(radio_discard_revogive_fore, "External vector")
lbl_option_discard_fore <- gtkLabel("                  Options for DISCARDS         ")
hboxDiscard_fore_calculation$packStart(lbl_option_discard_fore, expand = FALSE, fill = FALSE, padding = 5)
hboxDiscard_fore_calculation$packStart(radio_discard_revogive_fore, expand = FALSE, fill = FALSE, padding = 5)
hboxDiscard_fore_calculation$packStart(radio_discard_vector_fore, expand = FALSE, fill = FALSE, padding = 5)
  
discards_fore.sw <<- gtkScrolledWindowNew(NULL, NULL)
discards_fore.sw$setShadowType("etched-in")
discards_fore.sw$setPolicy("automatic", "automatic")
discards_fore.sw$SetUsize(100, 110)  
discards_fore <<- list()
discards_foreIndex <<- 0
# ------------------------------
# create model
discards_fore.create_model()
# create tree view
discards_fore.treeview <<- gtkTreeViewNewWithModel(discards_fore.model)
discards_fore.treeview$setRulesHint(TRUE)
discards_fore.treeview$getSelection()$setMode("single")
discards_fore.add_columns(discards_fore.treeview)
discards_fore.sw$add(discards_fore.treeview)   


discards_extvector_F_fore.sw <<- gtkScrolledWindowNew(NULL, NULL)
discards_extvector_F_fore.sw$setShadowType("etched-in")
discards_extvector_F_fore.sw$setPolicy("automatic", "automatic")
discards_extvector_F_fore.sw$SetUsize(250, 110)  

discards_extvector_F_list_fore <<- list()
discards_extvector_FIndex_fore <<- 0
# ------------------------------
# create model
discards_extvector_F_fore.create_model()
# create tree view
discards_extvector_F_fore.treeview <<- gtkTreeViewNewWithModel(discards_extvector_F_fore.model)
discards_extvector_F_fore.treeview$setRulesHint(TRUE)
discards_extvector_F_fore.treeview$getSelection()$setMode("single")
discards_extvector_F_fore.add_columns(discards_extvector_F_fore.treeview)
discards_extvector_F_fore.sw$add(discards_extvector_F_fore.treeview)    



discards_extvector_M_fore.sw <<- gtkScrolledWindowNew(NULL, NULL)
discards_extvector_M_fore.sw$setShadowType("etched-in")
discards_extvector_M_fore.sw$setPolicy("automatic", "automatic")
discards_extvector_M_fore.sw$SetUsize(250, 110)  

discards_extvector_M_list_fore <<- list()
discards_extvector_MIndex_fore <<- 0
# ------------------------------
# create model
discards_extvector_M_fore.create_model()
# create tree view
discards_extvector_M_fore.treeview <<- gtkTreeViewNewWithModel(discards_extvector_M_fore.model)
discards_extvector_M_fore.treeview$setRulesHint(TRUE)
discards_extvector_M_fore.treeview$getSelection()$setMode("single")
discards_extvector_M_fore.add_columns(discards_extvector_M_fore.treeview)
discards_extvector_M_fore.sw$add(discards_extvector_M_fore.treeview)    

tbl_discards_fore <- gtkTable(3,3,homogeneous = F)
tbl_discards_fore$SetRowSpacings(4)
tbl_discards_fore$SetColSpacings(30)
tbl_discards_fore$SetBorderWidth(5) 

i=0
j=0   
lbl_RO_fore <-  gtkLabel(" REVERSE OGIVE ")
tbl_discards_fore$Attach(lbl_RO_fore,i, i+1, j, j+1) 
j=j+2
tbl_discards_fore$Attach(discards_fore.sw,i, i+1, j, j+1)  

i=i+1
j=0
lbl_EV_fore <- gtkLabel(" EXTERNAL VECTOR ")   
tbl_discards_fore$Attach(lbl_EV_fore,i, i+2, j, j+1)
j=j+1 
lbl_EV_fore_fem <- gtkLabel(" Females ")
tbl_discards_fore$Attach(lbl_EV_fore_fem,i, i+1, j, j+1) 
j=j+1 
tbl_discards_fore$Attach(discards_extvector_F_fore.sw,i, i+1, j, j+1)  

i=i+1
j=0 
j=j+1 
lbl_EV_fore_mal <-  gtkLabel(" Males ")
tbl_discards_fore$Attach(lbl_EV_fore_mal,i, i+1, j, j+1)   
j=j+1 
tbl_discards_fore$Attach(discards_extvector_M_fore.sw,i, i+1, j, j+1)  
   
vboxDiscard_fore$packStart(tbl_discards_fore , expand = TRUE, F, 0) 

#source(paste(ALADYM_home, "/gui/forecast/fishery.discardTable_fore.r", sep=""))

#hboxDiscardfile_save <- gtkHBox(FALSE, 5)
#btn_browse_discard_save <- gtkButton()
#gtkButtonSetLabel(btn_browse_discard_save, "Save discard parameters...")
#btn_browse_discard_save$AddCallback("clicked", save_file_discards_vector)
#hboxDiscardfile_save$packStart(btn_browse_discard_save, expand = FALSE, fill = FALSE, padding=0)
#vboxDiscard$packStart(hboxDiscardfile_save, expand = FALSE, fill = FALSE, padding = 0)

#SAtool <- as.character(cfg[rownames(cfg) == paste("casestudy.S", m_int, ".StockAssessmentTool", sep=""),1]) 
#if (SAtool =="VIT" ) {
#    VIT.analysis.discard <- as.logical(cfg[rownames(cfg) == paste("casestudy.S", m_int, ".StockAssessmentTool", sep=""),4]) 
#    if (VIT.analysis.discard) {
#       gtkComboBoxSetActive(combo_discard_fore, 0 )
#    } else {
#       gtkComboBoxSetActive(combo_discard_fore, 2 ) 
#    }
#} else {
#      gtkComboBoxSetActive(combo_discard_fore, 2 )
#}
#


gSignalConnect(radio_discard_revogive_fore, "toggled", deactive_RO_EV_fore)
gSignalConnect(radio_discard_vector_fore, "toggled",deactive_RO_EV_fore)
