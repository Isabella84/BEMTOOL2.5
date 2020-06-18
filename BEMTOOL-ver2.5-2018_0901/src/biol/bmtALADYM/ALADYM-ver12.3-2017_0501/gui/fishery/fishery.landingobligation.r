# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.






#hboxlan_obligation <- gtkHBox(FALSE, 5)
#hboxlan_obligation$packStart(gtkLabel("Seed for landing obligation [Y/N]"), expand = FALSE, fill = FALSE, padding = 5) 
#
#entry_lan_obligation_seedvalue <- gtkEntry() 
#gtkEntrySetWidthChars(entry_lan_obligation_seedvalue, NUMERICAL_ENTRY_LENGTH) 
#gtkEntrySetText(entry_lan_obligation_seedvalue, "Y")  
#hboxlan_obligation$packStart(entry_lan_obligation_seedvalue, expand = FALSE, fill = FALSE, padding = 5) 
#
#btn_load_seed_lan_obligation <- gtkButton()
#gtkButtonSetLabel(btn_load_seed_lan_obligation, "Load seed value")
#btn_load_seed_lan_obligation$AddCallback("clicked", reload_lan_obligation_table)
#hboxlan_obligation$packStart(btn_load_seed_lan_obligation, expand = FALSE, fill = FALSE, padding = 5) 

#vboxLandingObligation$packStart(hboxlan_obligation, expand = FALSE, fill = FALSE, padding = 5)
  
lan_obligation.sw <<- gtkScrolledWindowNew(NULL, NULL)
lan_obligation.sw$setShadowType("etched-in")
lan_obligation.sw$setPolicy("automatic", "automatic")
lan_obligation.sw$SetUsize(120, 90)  
lan_obligation <<- list()
lan_obligationIndex <<- 0
# ------------------------------
# create model
lan_obligation.create_model()
# create tree view
lan_obligation.treeview <<- gtkTreeViewNewWithModel(lan_obligation.model)
lan_obligation.treeview$setRulesHint(TRUE)
lan_obligation.treeview$getSelection()$setMode("single")
lan_obligation.add_columns(lan_obligation.treeview)
lan_obligation.sw$add(lan_obligation.treeview)    


tbl_fleets_landObl <- gtkTable(1,2,homogeneous = FALSE)
tbl_fleets_landObl$SetRowSpacings(10)
tbl_fleets_landObl$SetColSpacings(30)
tbl_fleets_landObl$SetBorderWidth(5)

j=0
i=0  # column 4
button_load_landing_obl <- gtkButtonNewWithLabel("Load obligations...")
button_load_landing_obl$AddCallback("clicked", load_lan_obligationfromFile)
tbl_fleets_landObl$Attach(button_load_landing_obl ,i, i+1,  j, j+1)

i=i+1  # column 4
button_save_landing_obl <- gtkButtonNewWithLabel("Export obligations...")
button_save_landing_obl$AddCallback("clicked", export_lan_obligationtoFile)
tbl_fleets_landObl$Attach(button_save_landing_obl ,i, i+1,  j, j+1)

 vboxLandingObligation$packStart(tbl_fleets_landObl , expand = TRUE, F, 0) 

vboxLandingObligation$packStart(lan_obligation.sw , expand = FALSE, TRUE, 0) 
