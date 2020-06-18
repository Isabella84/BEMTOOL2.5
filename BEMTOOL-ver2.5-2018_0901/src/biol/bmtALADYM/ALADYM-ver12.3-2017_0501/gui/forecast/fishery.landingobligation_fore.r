# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




  
lan_obligation_fore.sw <<- gtkScrolledWindowNew(NULL, NULL)
lan_obligation_fore.sw$setShadowType("etched-in")
lan_obligation_fore.sw$setPolicy("automatic", "automatic")
lan_obligation_fore.sw$SetUsize(120, 90)  
lan_obligation_fore <<- list()
lan_obligationIndex_fore <<- 0
# ------------------------------
# create model
lan_obligation_fore.create_model()
# create tree view
lan_obligation_fore.treeview <<- gtkTreeViewNewWithModel(lan_obligation_fore.model)
lan_obligation_fore.treeview$setRulesHint(TRUE)
lan_obligation_fore.treeview$getSelection()$setMode("single")
lan_obligation_fore.add_columns(lan_obligation_fore.treeview)
lan_obligation_fore.sw$add(lan_obligation_fore.treeview)    


tbl_fleets_landObl_fore <- gtkTable(1,2,homogeneous = FALSE)
tbl_fleets_landObl_fore$SetRowSpacings(10)
tbl_fleets_landObl_fore$SetColSpacings(30)
tbl_fleets_landObl_fore$SetBorderWidth(5)

j=0
i=0  # column 4
button_load_landing_obl_fore <- gtkButtonNewWithLabel("Load obligations...")
button_load_landing_obl_fore$AddCallback("clicked", load_lan_obligationfromFile_fore)
tbl_fleets_landObl_fore$Attach(button_load_landing_obl_fore ,i, i+1,  j, j+1)

i=i+1  # column 4
button_save_landing_obl_fore <- gtkButtonNewWithLabel("Export obligations...")
button_save_landing_obl_fore$AddCallback("clicked", export_lan_obligationtoFile_fore)
tbl_fleets_landObl_fore$Attach(button_save_landing_obl_fore ,i, i+1,  j, j+1)

 vboxLandingObligation_fore$packStart(tbl_fleets_landObl_fore , expand = TRUE, F, 0) 


vboxLandingObligation_fore$packStart(lan_obligation_fore.sw , expand = FALSE, TRUE, 0) 
