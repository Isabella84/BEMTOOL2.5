# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.






# initializating  objects

#MCDAutility_table <<- data.frame(read.csv(paste(getwd(), "/Utility_params_default.csv", sep=""), sep=";"))
#MCDAweight_table <<- read.csv(paste(getwd(), "/Weights_default.csv", sep=""), sep=";")

vbox_mcda <- gtkVBox(FALSE, 5)

hbox_bmt_MCDAbuttons <- gtkHBox(homogeneous = FALSE, spacing = 0)




btn_loadMCDAutility <- gtkButton()
gtkButtonSetLabel(btn_loadMCDAutility, "      Load utility parameters...    ")
btn_loadMCDAutility$AddCallback("clicked", loadMCDAutilityfromFile)
hbox_bmt_MCDAbuttons$packStart(btn_loadMCDAutility, FALSE, FALSE, 10) 

btn_loadMCDAweight <- gtkButton()
gtkButtonSetLabel(btn_loadMCDAweight, "      Load weights...    ")
btn_loadMCDAweight$AddCallback("clicked", loadMCDAweightfromFile)
hbox_bmt_MCDAbuttons$packStart(btn_loadMCDAweight, FALSE, FALSE, 10) 

btn_runMCDA <- gtkButton()
gtkButtonSetLabel(btn_runMCDA, "      Run MCDA     ")
btn_runMCDA$AddCallback("clicked", runMCDA_bmt_action)
hbox_bmt_MCDAbuttons$packStart(btn_runMCDA, FALSE, FALSE, 10) 


bmt_vbox_MCDAutility <- gtkVBox(F, 5) 
bmt_hbox_MCDAutility  <- gtkHBox(F, 5) 

MCDAutility.sw <<- gtkScrolledWindowNew(NULL, NULL)
MCDAutility.sw$setShadowType("etched-in")
MCDAutility.sw$setPolicy("automatic", "automatic")
MCDAutility.sw$SetUsize(120, 200)  
MCDAutility_list <<- list()
MCDAutilityIndex <<- 0
# create model
MCDAutility.create_model()
# create tree view
MCDAutility.treeview <<- gtkTreeViewNewWithModel(MCDAutility.model)
MCDAutility.treeview$setRulesHint(TRUE)
MCDAutility.treeview$getSelection()$setMode("single")
MCDAutility.add_columns(MCDAutility.treeview)
MCDAutility.sw$add(MCDAutility.treeview) 

 # lbl_MCDAutility <<- gtkLabel(" Utility value at the following points ")
 lbl_MCDAutility <<- gtkLabel("Utility is bounded between 0 and 1. Level near 1 express high satisfaction, level near 0 express lower satisfaction")
 
bmt_vboxMCDAutility_table <- gtkVBox(F, 5)    
bmt_vboxMCDAutility_table$packStart(lbl_MCDAutility , expand = F, T, 10)
bmt_vboxMCDAutility_table$packStart(MCDAutility.sw  , expand = F, T, 0)
# bmt_vboxMCDAutility_table$packStart( gtkLabel("Utility is bounded between 0 and 1. Level near 1 express high satisfaction, level near 0 express lower satisfaction")  , expand = F, T, 0)
bmt_vboxMCDAutility_table$packStart( gtkLabel("* GVA_or_ROI_or_PROFITS can be: 1: GVA, 2: ROI, 3: PROFITS")  , expand = F, T, 0)
bmt_hbox_MCDAutility$packStart(bmt_vboxMCDAutility_table , expand = T, T, 10)

bmt_vbox_MCDAutility$packStart(bmt_vbox_MCDAutility, expand = F, T, 10)





bmt_vbox_MCDAweight <- gtkVBox(F, 5) 
bmt_hbox_MCDAweight  <- gtkHBox(F, 5) 

MCDAweight.sw <<- gtkScrolledWindowNew(NULL, NULL)
MCDAweight.sw$setShadowType("etched-in")
MCDAweight.sw$setPolicy("automatic", "automatic")
MCDAweight.sw$SetUsize(120, 200)  
MCDAweight_list <<- list()
MCDAweightIndex <<- 0
# create model
MCDAweight.create_model()
# create tree view
MCDAweight.treeview <<- gtkTreeViewNewWithModel(MCDAweight.model)
MCDAweight.treeview$setRulesHint(TRUE)
MCDAweight.treeview$getSelection()$setMode("single")
MCDAweight.add_columns(MCDAweight.treeview)
MCDAweight.sw$add(MCDAweight.treeview) 

 lbl_MCDAweight <<- gtkLabel(" Weights associated to each indicator ")
 
bmt_vboxMCDAweight_table <- gtkVBox(F, 5)    
bmt_vboxMCDAweight_table$packStart(lbl_MCDAweight , expand = F, T, 10)
bmt_vboxMCDAweight_table$packStart(MCDAweight.sw  , expand = F, F, 10)

bmt_hbox_MCDAweight$packStart(bmt_vboxMCDAweight_table , expand = T, T, 10)
bmt_vbox_MCDAweight$packStart(bmt_vbox_MCDAweight, expand = F, F, 10)

 vbox_mcda$packStart(hbox_bmt_MCDAbuttons , expand = F, F, 10)

vbox_mcda$packStart(bmt_hbox_MCDAutility , expand = F, T, 0)
vbox_mcda$packStart(bmt_hbox_MCDAweight , expand = F, T, 15)
 vbox_mcda$packStart(hbox_bmt_MCDAbuttons , expand = F, F, 10)