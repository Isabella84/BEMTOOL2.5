# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





hboxDiscard <- gtkHBox(FALSE, 5)
hboxDiscard$packStart(gtkLabel("Discard calculation"), expand = FALSE, fill = FALSE, padding = 5) 

combo_discard <- gtkComboBoxNewText()
gSignalConnect(combo_discard, "changed", deactivate_Discard_unused_params)
for (choice in DISCARD_CALC) {
combo_discard$appendText(choice)
}



hboxDiscard$packStart(combo_discard, expand = FALSE, fill = FALSE, padding = 5)


radio_discard_revogive <- gtkRadioButton()
radio_discard_revogive$add(gtkLabel("Reverse ogive"))
radio_discard_vector <- gtkRadioButtonNewWithLabelFromWidget(radio_discard_revogive, "External vector")
lbl_option_discard <- gtkLabel("                  Options for DISCARDS         ")
hboxDiscard$packStart(lbl_option_discard, expand = FALSE, fill = FALSE, padding = 5)
hboxDiscard$packStart(radio_discard_revogive, expand = FALSE, fill = FALSE, padding = 5)
hboxDiscard$packStart(radio_discard_vector, expand = FALSE, fill = FALSE, padding = 5)

#hboxDiscardFile <- gtkHBox(FALSE, 5)
#lbl_discardFile_title <- gtkLabel("Load values of discard from .csv file")
#hboxDiscardFile$packStart(lbl_discardFile_title, expand = FALSE, fill = FALSE, padding = 5) 
#btn_browse_discardFile <- gtkButton()
#gtkButtonSetLabel(btn_browse_discardFile, "Browse...")
#btn_browse_discardFile$AddCallback("clicked", select_file_discard)
#hboxDiscardFile$packStart(btn_browse_discardFile, expand = FALSE, fill = FALSE, padding = 5)
#
#lbl_discardFile <- gtkLabel("C:\\ ")
#hboxDiscardFile$packStart(lbl_discardFile, expand = FALSE, fill = FALSE, padding = 5)

#vboxDiscard$packStart(hboxDiscardFile, expand = FALSE, fill = FALSE, padding = 0)
  
discards.sw <<- gtkScrolledWindowNew(NULL, NULL)
discards.sw$setShadowType("etched-in")
discards.sw$setPolicy("automatic", "automatic")
discards.sw$SetUsize(100, 110)  

discards_list <<- list()
discardsIndex <<- 0
# ------------------------------
# create model
discards.create_model()
# create tree view
discards.treeview <<- gtkTreeViewNewWithModel(discards.model)
discards.treeview$setRulesHint(TRUE)
discards.treeview$getSelection()$setMode("single")
discards.add_columns(discards.treeview)
discards.sw$add(discards.treeview) 


discards_extvector_F.sw <<- gtkScrolledWindowNew(NULL, NULL)
discards_extvector_F.sw$setShadowType("etched-in")
discards_extvector_F.sw$setPolicy("automatic", "automatic")
discards_extvector_F.sw$SetUsize(250, 110)  

discards_extvector_F_list <<- list()
discards_extvector_FIndex <<- 0
# ------------------------------
# create model
discards_extvector_F.create_model()
# create tree view
discards_extvector_F.treeview <<- gtkTreeViewNewWithModel(discards_extvector_F.model)
discards_extvector_F.treeview$setRulesHint(TRUE)
discards_extvector_F.treeview$getSelection()$setMode("single")
discards_extvector_F.add_columns(discards_extvector_F.treeview)
discards_extvector_F.sw$add(discards_extvector_F.treeview)    



discards_extvector_M.sw <<- gtkScrolledWindowNew(NULL, NULL)
discards_extvector_M.sw$setShadowType("etched-in")
discards_extvector_M.sw$setPolicy("automatic", "automatic")
discards_extvector_M.sw$SetUsize(250, 110)  

discards_extvector_M_list <<- list()
discards_extvector_MIndex <<- 0
# ------------------------------
# create model
discards_extvector_M.create_model()
# create tree view
discards_extvector_M.treeview <<- gtkTreeViewNewWithModel(discards_extvector_M.model)
discards_extvector_M.treeview$setRulesHint(TRUE)
discards_extvector_M.treeview$getSelection()$setMode("single")
discards_extvector_M.add_columns(discards_extvector_M.treeview)
discards_extvector_M.sw$add(discards_extvector_M.treeview)    

tbl_discards <- gtkTable(3,3,homogeneous = F)
tbl_discards$SetRowSpacings(4)
tbl_discards$SetColSpacings(30)
tbl_discards$SetBorderWidth(5) 

i=0
j=0   
lbl_RO <-  gtkLabel(" REVERSE OGIVE ")
tbl_discards$Attach(lbl_RO,i, i+1, j, j+1) 
j=j+2
tbl_discards$Attach(discards.sw,i, i+1, j, j+1)  

i=i+1
j=0
lbl_EV <- gtkLabel(" EXTERNAL VECTOR ")   
tbl_discards$Attach(lbl_EV,i, i+2, j, j+1)
j=j+1 
lbl_EV_fem <- gtkLabel(" Females ")
tbl_discards$Attach(lbl_EV_fem,i, i+1, j, j+1) 
j=j+1 
tbl_discards$Attach(discards_extvector_F.sw,i, i+1, j, j+1)  

i=i+1
j=0 
j=j+1 
lbl_EV_mal <-  gtkLabel(" Males ")
tbl_discards$Attach(lbl_EV_mal,i, i+1, j, j+1)   
j=j+1 
tbl_discards$Attach(discards_extvector_M.sw,i, i+1, j, j+1)  
   

tbl_fleets_discard <- gtkTable(1,4,homogeneous = FALSE)
tbl_fleets_discard$SetRowSpacings(10)
tbl_fleets_discard$SetColSpacings(30)
tbl_fleets_discard$SetBorderWidth(5)

j=0
i=0  # column 4
button_load_discard <- gtkButtonNewWithLabel("Load discard params...")
button_load_discard$AddCallback("clicked", loadDiscardsfromFile)
tbl_fleets_discard$Attach(button_load_discard ,i, i+1,  j, j+1)

i=i+1  # column 4
button_load_discard_vect <- gtkButtonNewWithLabel("Load discard vector...")
button_load_discard_vect$AddCallback("clicked", loadDiscards_extvectorfromFile)
tbl_fleets_discard$Attach(button_load_discard_vect ,i, i+1,  j, j+1)

#i=i+1  # column 4
#button_load_landing_obl <- gtkButtonNewWithLabel("Load obligations...")
#button_load_landing_obl$AddCallback("clicked", load_lan_obligationfromFile)
#tbl_fleets_discard$Attach(button_load_landing_obl ,i, i+1,  j, j+1)

i=i+1  # column 9
button_saveall_discards <- gtkButtonNewWithLabel("Export discard params...")
button_saveall_discards$AddCallback("clicked", saveDiscardstoFile)
tbl_fleets_discard$Attach(button_saveall_discards ,i, i+1,  j, j+1)

i=i+1  # column 9
button_saveall_discards_vect <- gtkButtonNewWithLabel("Export discard vector...")
button_saveall_discards_vect$AddCallback("clicked", saveDiscards_extvectortoFile)
tbl_fleets_discard$Attach(button_saveall_discards_vect ,i, i+1,  j, j+1)

#i=i+1  # column 4
#button_save_landing_obl <- gtkButtonNewWithLabel("Export obligations...")
#button_save_landing_obl$AddCallback("clicked", export_lan_obligationtoFile)
#tbl_fleets_discard$Attach(button_save_landing_obl ,i, i+1,  j, j+1)

 vboxDiscard$packStart(tbl_fleets_discard , expand = TRUE, F, 0) 
 
 vboxDiscard$packStart(hboxDiscard, expand = FALSE, fill = FALSE, padding = 10)    
 vboxDiscard$packStart(tbl_discards , expand = TRUE, F, 0) 


if (!is.null(fleet.discard) ) {
if (nrow(fleet.discard) >0 ) {
reload_discard_table()
} else {
reload_EMPTY_discard_table()
}
} else {
reload_EMPTY_discard_table()
}
#source(paste(ALADYM_home, "/gui/fishery/fishery.discardTable.r", sep=""))

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
#       gtkComboBoxSetActive(combo_discard, 0 )
#    } else {
#       gtkComboBoxSetActive(combo_discard, 2 ) 
#    }
#} else {
#      gtkComboBoxSetActive(combo_discard, 2 )
#}
#


gSignalConnect(radio_discard_revogive, "toggled", deactive_RO_EV)
gSignalConnect(radio_discard_vector, "toggled",deactive_RO_EV)
