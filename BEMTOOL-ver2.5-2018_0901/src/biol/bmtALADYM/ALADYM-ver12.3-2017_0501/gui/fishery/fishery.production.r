# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



vboxProduction <-  gtkVBox(FALSE, 5)
hboxProduction <- gtkHBox(FALSE, 5)
hboxProduction$packStart(gtkLabel("Seed value for PRODUCTION [kg]"), expand = FALSE, fill = FALSE, padding = 5) 

entry_Production_seedvalue <- gtkEntry() 
gtkEntrySetWidthChars(entry_Production_seedvalue, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetText(entry_Production_seedvalue, 0)  
hboxProduction$packStart(entry_Production_seedvalue, expand = FALSE, fill = FALSE, padding = 5) 

btn_load_seed_production <- gtkButton()
gtkButtonSetLabel(btn_load_seed_production, "Load seed value")
btn_load_seed_production$AddCallback("clicked", reload_production_table)
hboxProduction$packStart(btn_load_seed_production, expand = FALSE, fill = FALSE, padding = 5) 

vboxProduction$packStart(hboxProduction, expand = FALSE, fill = FALSE, padding = 5)

vboxProductionData$packStart(vboxProduction, expand = FALSE, fill = FALSE, padding = 5)

gtkBoxReorderChild(vboxProductionData, hboxProduction, 2)
  
productions.sw <<- gtkScrolledWindowNew(NULL, NULL)
productions.sw$setShadowType("etched-in")
productions.sw$setPolicy("automatic", "automatic")
productions.sw$SetUsize(300, 90)  
productions <<- list()
productionsIndex <<- 0
# ------------------------------
# create model
productions.create_model()
# create tree view
productions.treeview <<- gtkTreeViewNewWithModel(productions.model)
productions.treeview$setRulesHint(TRUE)
productions.treeview$getSelection()$setMode("single")
productions.add_columns(productions.treeview)
productions.sw$add(productions.treeview)    
vboxProduction$packStart(productions.sw , TRUE, F, 5) 

vboxmonthlyDiscard <-  gtkVBox(FALSE, 5)
hboxmonthlyDiscard <- gtkHBox(FALSE, 5)
hboxmonthlyDiscard$packStart(gtkLabel("Seed value for DISCARD [kg]"), expand = FALSE, fill = FALSE, padding = 5) 

entry_monthlyDiscard_seedvalue <- gtkEntry() 
gtkEntrySetWidthChars(entry_monthlyDiscard_seedvalue, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetText(entry_monthlyDiscard_seedvalue, 0)  
hboxmonthlyDiscard$packStart(entry_monthlyDiscard_seedvalue, expand = FALSE, fill = FALSE, padding = 5) 

btn_load_seed_monthlyDiscard <- gtkButton()
gtkButtonSetLabel(btn_load_seed_monthlyDiscard, "Load seed value")
btn_load_seed_monthlyDiscard$AddCallback("clicked", reload_monthlyDiscard_table)
hboxmonthlyDiscard$packStart(btn_load_seed_monthlyDiscard, expand = FALSE, fill = FALSE, padding = 5) 

vboxmonthlyDiscard$packStart(hboxmonthlyDiscard, expand = FALSE, fill = FALSE, padding = 5)

vboxProductionData$packStart(vboxmonthlyDiscard, expand = FALSE, fill = FALSE, padding = 5)

#gtkBoxReorderChild(vboxProductionData, hboxProduction, 2)
  
monthlyDiscard.sw <<- gtkScrolledWindowNew(NULL, NULL)
monthlyDiscard.sw$setShadowType("etched-in")
monthlyDiscard.sw$setPolicy("automatic", "automatic")
monthlyDiscard.sw$SetUsize(300, 90)  
monthlyDiscard_list <<- list()
monthlyDiscardIndex <<- 0
# ------------------------------
# create model
monthlyDiscard.create_model()
# create tree view
monthlyDiscard.treeview <<- gtkTreeViewNewWithModel(monthlyDiscard.model)
monthlyDiscard.treeview$setRulesHint(TRUE)
monthlyDiscard.treeview$getSelection()$setMode("single")
monthlyDiscard.add_columns(monthlyDiscard.treeview)
monthlyDiscard.sw$add(monthlyDiscard.treeview)    
vboxmonthlyDiscard$packStart(monthlyDiscard.sw , TRUE, F, 5) 

vboxPproduction <- gtkVBox(FALSE, 5)

hboxPproduction <- gtkHBox(FALSE, 5)
hboxPproduction$packStart(gtkLabel("Seed value for p PRODUCTION"), expand = FALSE, fill = FALSE, padding = 5) 

entry_pProduction_seedvalue <- gtkEntry() 
gtkEntrySetWidthChars(entry_pProduction_seedvalue, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetText(entry_pProduction_seedvalue, 0)  
hboxPproduction$packStart(entry_pProduction_seedvalue, expand = FALSE, fill = FALSE, padding = 5) 

btn_load_seed_pproduction <- gtkButton()
gtkButtonSetLabel(btn_load_seed_pproduction, "Load seed value")
btn_load_seed_pproduction$AddCallback("clicked", reload_pproduction_table)
hboxPproduction$packStart(btn_load_seed_pproduction, expand = FALSE, fill = FALSE, padding = 5) 

vboxPproduction$packStart(hboxPproduction, expand = FALSE, fill = FALSE, padding = 5)


pproductions.sw <<- gtkScrolledWindowNew(NULL, NULL)
pproductions.sw$setShadowType("etched-in")
pproductions.sw$setPolicy("automatic", "automatic")
pproductions.sw$SetUsize(300, 90)  
pproductions <<- list()
pproductionsIndex <<- 0
# ------------------------------
# create model
pproductions.create_model()
# create tree view
pproductions.treeview <<- gtkTreeViewNewWithModel(pproductions.model)
pproductions.treeview$setRulesHint(TRUE)
pproductions.treeview$getSelection()$setMode("single")
pproductions.add_columns(pproductions.treeview)
pproductions.sw$add(pproductions.treeview)    
vboxPproduction$packStart(pproductions.sw , expand=FALSE, F, 5) 

vboxProductionData$packStart(vboxProduction, expand=T, F, 5)   

    gtkWidgetSetSensitive(productions.treeview, TRUE) 
    gtkWidgetSetSensitive(pproductions.treeview, FALSE) 
    
     gtkWidgetSetSensitive(button_load_production, TRUE)
    gtkWidgetSetSensitive(button_load_p_production, FALSE)
     