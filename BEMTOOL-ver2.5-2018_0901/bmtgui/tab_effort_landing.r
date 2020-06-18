# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.







# initializating  objects

vbox_effort_landing <- gtkVBox(FALSE, 5) 

bmt_combo_fleetsegments <<- gtkComboBoxNewText()
gSignalConnect(bmt_combo_fleetsegments, "changed", bmt_reload_fleetsegment_info)

if (!is.null(BMT_FLEETSEGMENTS)) {
for (item in BMT_FLEETSEGMENTS) {
  bmt_combo_fleetsegments$appendText(item)
}
}


bmt_button_load_effortdata <- gtkButtonNewWithLabel(" Load effort data... ")
bmt_button_load_effortdata$AddCallback("clicked", bmt_loadEffortData)   

#bmt_button_exp_effortdata <- gtkButtonNewWithLabel("Export effort data...")
#bmt_button_exp_effortdata$AddCallback("clicked", export_effort_variables) 

bmt_button_load_landingdata <- gtkButtonNewWithLabel(" Load landing data... ")
bmt_button_load_landingdata$AddCallback("clicked", assign_species_path)   

bmt_button_exp_landingdata <- gtkButtonNewWithLabel(" Apply and save... ")
bmt_button_exp_landingdata$AddCallback("clicked", save_landing_file)   

bmt_button_savechanges_fleet <- gtkButtonNewWithLabel(" Apply and save... ")
bmt_button_savechanges_fleet$AddCallback("clicked", setEffortLanding_fleet_settings)

hbox_buttons <- gtkHBox(FALSE, 5) 
hbox_choose_fleet <- gtkHBox(FALSE, 5) 

frame_economic_effort <- gtkFrame(" Effort data ")  
frame_economic_landing <- gtkFrame(" Landing data ")  

hbox_economic_effort <- gtkHBox(homogeneous = FALSE, 5)        
hbox_economic_landing <- gtkHBox(homogeneous = FALSE, 5)  

hbox_economic_effort$packStart(frame_economic_effort, expand = T, fill = T, padding = 10)                 
hbox_economic_landing$packStart(frame_economic_landing, expand = T, fill = T, padding = 10)                 

vbox_economic_effort <- gtkVBox(FALSE, 5)                             
vbox_economic_landing <- gtkVBox(FALSE, 5)                        


hbox_choose_fleet$packStart(bmt_combo_fleetsegments, expand = FALSE, fill = FALSE, padding = 10)
hbox_choose_fleet$packStart(bmt_button_savechanges_fleet, expand = FALSE, fill = FALSE, padding = 10)

vbox_effort_landing$packStart(hbox_choose_fleet, expand = FALSE, fill = FALSE, padding = 10)

hbox_buttons$packStart(bmt_button_load_effortdata, expand = FALSE, fill = FALSE, padding = 5)
#hbox_buttons$packStart(bmt_button_exp_effortdata, expand = FALSE, fill = FALSE, padding = 5)
# hbox_buttons$packStart(bmt_button_load_landingdata, expand = FALSE, fill = FALSE, padding = 5)
# hbox_buttons$packStart(bmt_button_exp_landingdata, expand = FALSE, fill = FALSE, padding = 5)


hbox_choose_fleet$packStart(hbox_buttons, expand = FALSE, fill = FALSE, padding = 10)
#vbox_economic$packStart(hbox_buttons, expand = FALSE, fill = FALSE, padding = 5)

vbox_effort_landing$packStart(hbox_economic_effort, expand = FALSE, fill = FALSE, padding = 5)
vbox_effort_landing$packStart(hbox_economic_landing, expand = FALSE, fill = FALSE, padding = 5)

 




BMTnotebook_effort <<- gtkNotebook()
BMTnotebook_effort$setTabPos("top")

# ---------------------------------------------------------------------------------- windows for the effort variables

bmt_vboxKW <- gtkVBox(F, 5) 
bmt_hboxKW  <- gtkHBox(F, 5) 

bmt_vboxGT <- gtkVBox(F, 5) 
bmt_hboxGT  <- gtkHBox(F, 5) 

bmt_vboxNUMBER <- gtkVBox(F, 5) 
bmt_hboxNUMBER  <- gtkHBox(F, 5) 

bmt_vboxDAY <- gtkVBox(F, 5) 
bmt_hboxDAY  <- gtkHBox(F, 5) 






# --------------------------- kw table
bmt_KW.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_KW.sw$setShadowType("etched-in")
bmt_KW.sw$setPolicy("automatic", "automatic")
bmt_KW.sw$SetUsize(120, 120)  
bmt_KW_list <<- list()
bmt_KWIndex <<- 0
# create model
if (length(BMT_FLEETSEGMENTS) != 0)  {
bmt_KW.create_model()
# create tree view
bmt_KW.treeview <<- gtkTreeViewNewWithModel(bmt_KW.model)
bmt_KW.treeview$setRulesHint(TRUE)
bmt_KW.treeview$getSelection()$setMode("single")
bmt_KW.add_columns(bmt_KW.treeview)
bmt_KW.sw$add(bmt_KW.treeview) 
}


# --------------------------- gt table
bmt_GT.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_GT.sw$setShadowType("etched-in")
bmt_GT.sw$setPolicy("automatic", "automatic")
bmt_GT.sw$SetUsize(120, 120)  
bmt_GT_list <<- list()
bmt_GTIndex <<- 0
# create model
if (length(BMT_FLEETSEGMENTS) != 0)  {
bmt_GT.create_model()
# create tree view
bmt_GT.treeview <<- gtkTreeViewNewWithModel(bmt_GT.model)
bmt_GT.treeview$setRulesHint(TRUE)
bmt_GT.treeview$getSelection()$setMode("single")
bmt_GT.add_columns(bmt_GT.treeview)
bmt_GT.sw$add(bmt_GT.treeview)  
}  


# --------------------------- number table
bmt_NUMBER.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_NUMBER.sw$setShadowType("etched-in")
bmt_NUMBER.sw$setPolicy("automatic", "automatic")
bmt_NUMBER.sw$SetUsize(120, 120)  
bmt_NUMBER_list <<- list()
bmt_NUMBERIndex <<- 0
# create model
if (length(BMT_FLEETSEGMENTS) != 0)  {
bmt_NUMBER.create_model()
# create tree view
bmt_NUMBER.treeview <<- gtkTreeViewNewWithModel(bmt_NUMBER.model)
bmt_NUMBER.treeview$setRulesHint(TRUE)
bmt_NUMBER.treeview$getSelection()$setMode("single")
bmt_NUMBER.add_columns(bmt_NUMBER.treeview)
bmt_NUMBER.sw$add(bmt_NUMBER.treeview)  
}  


# --------------------------- DAY table
bmt_DAY.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_DAY.sw$setShadowType("etched-in")
bmt_DAY.sw$setPolicy("automatic", "automatic")
bmt_DAY.sw$SetUsize(120, 120)  
bmt_DAY_list <<- list()
bmt_DAYIndex <<- 0
# create model
if (length(BMT_FLEETSEGMENTS) != 0)  {
bmt_DAY.create_model()
# create tree view
bmt_DAY.treeview <<- gtkTreeViewNewWithModel(bmt_DAY.model)
bmt_DAY.treeview$setRulesHint(TRUE)
bmt_DAY.treeview$getSelection()$setMode("single")
bmt_DAY.add_columns(bmt_DAY.treeview)
bmt_DAY.sw$add(bmt_DAY.treeview) 
} 



bmt_hboxKW$packStart(bmt_KW.sw , expand = T, T, 10) 
bmt_vboxKW$packStart(bmt_hboxKW , expand = F, T, 10)

bmt_hboxGT$packStart(bmt_GT.sw , expand = T, T, 10) 
bmt_vboxGT$packStart(bmt_hboxGT , expand = F, T, 10)

bmt_hboxNUMBER$packStart(bmt_NUMBER.sw , expand = T, T, 10) 
bmt_vboxNUMBER$packStart(bmt_hboxNUMBER , expand = F, T, 10)

bmt_hboxDAY$packStart(bmt_DAY.sw , expand = T, T, 10) 
bmt_vboxDAY$packStart(bmt_hboxDAY , expand = F, T, 10)


BMTnotebook_effort$appendPage(bmt_vboxNUMBER, gtkLabel(str=" Monthly fishing vessels "))
BMTnotebook_effort$appendPage(bmt_vboxDAY, gtkLabel(str=" Monthly average Days at sea "))
BMTnotebook_effort$appendPage(bmt_vboxGT, gtkLabel(str=" Monthly average GT "))
BMTnotebook_effort$appendPage(bmt_vboxKW, gtkLabel(str=" Monthly average KW "))


bmt_hbox_notebook_effort  <- gtkHBox(F, 5)
bmt_vbox_notebook_effort  <- gtkVBox(F, 5)
 
bmt_hbox_notebook_effort$packStart(BMTnotebook_effort, expand = T, fill = T, padding = 10)
bmt_vbox_notebook_effort$packStart(bmt_hbox_notebook_effort, expand = FALSE, fill = FALSE, padding = 10)
frame_economic_effort$add(bmt_vbox_notebook_effort) 

#frame_economic_effort$add(bmt_vbox_effort_F_relationship) 



# ---------------------------------------------------------------------------------- windows for the LANDING

bmt_vboxLANDING <- gtkVBox(F, 5) 
bmt_hboxLANDING <- gtkHBox(F, 5) 

# --------------------------- kw table
bmt_LANDING.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_LANDING.sw$setShadowType("etched-in")
bmt_LANDING.sw$setPolicy("automatic", "automatic")
bmt_LANDING.sw$SetUsize(120, 150)  
bmt_LANDING_list <<- list()
bmt_LANDINGIndex <<- 0
# create model
if (length(BMT_FLEETSEGMENTS) != 0)  {
bmt_LANDING.create_model()
# create tree view
bmt_LANDING.treeview <<- gtkTreeViewNewWithModel(bmt_LANDING.model)
bmt_LANDING.treeview$setRulesHint(TRUE)
bmt_LANDING.treeview$getSelection()$setMode("single")
bmt_LANDING.add_columns(bmt_LANDING.treeview)
bmt_LANDING.sw$add(bmt_LANDING.treeview)
} 

bmt_hboxLANDING$packStart(bmt_LANDING.sw , expand = T, T, 10) 
bmt_vboxLANDING$packStart(bmt_hboxLANDING , expand = F, T, 5)



bmt_eco_landing_species <<- gtkComboBoxNewText()
gSignalConnect(bmt_eco_landing_species, "changed", bmt_reload_landing_species_info)

for (item in BMT_SPECIES) {
  bmt_eco_landing_species$appendText(item)
}

hbox_landing_species <- gtkHBox(FALSE, 5) 
hbox_landing_species$packStart(bmt_eco_landing_species , expand = F, T, 10)

hbox_landing_species$packStart(bmt_button_exp_landingdata , expand = F, T, 10)
hbox_landing_species$packStart(bmt_button_load_landingdata , expand = F, T, 10)

vbox_landing_species <- gtkVBox(FALSE, 5) 
vbox_landing_species$packStart(hbox_landing_species , expand = F, T, 10)
vbox_landing_species$packStart(bmt_vboxLANDING , expand = F, T, 10)

frame_economic_landing$add(vbox_landing_species) 




# ------------------------------------------------------------------------------------------
 # commented on version 2.5.5
if (FALSE) {
bmt_vboxecoind_sim <- gtkVBox(F, 5) 
bmt_hboxecoind_sim  <- gtkHBox(F, 5) 

bmt_ecoind_sim.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_ecoind_sim.sw$setShadowType("etched-in")
bmt_ecoind_sim.sw$setPolicy("automatic", "automatic")
bmt_ecoind_sim.sw$SetUsize(120, 70)  
bmt_ecoind_sim_list <<- list()
bmt_ecoind_simIndex <<- 0
# create model
bmt_ecoind_sim.create_model()
# create tree view
bmt_ecoind_sim.treeview <<- gtkTreeViewNewWithModel(bmt_ecoind_sim.model)
bmt_ecoind_sim.treeview$setRulesHint(TRUE)
bmt_ecoind_sim.treeview$getSelection()$setMode("single")
bmt_ecoind_sim.add_columns(bmt_ecoind_sim.treeview)
bmt_ecoind_sim.sw$add(bmt_ecoind_sim.treeview) 

 lbl_ecoind_sim <<- gtkLabel(" Landing correction factors ") 
 
bmt_vboxecoind_sim_table <- gtkVBox(F, 5)    
bmt_vboxecoind_sim_table$packStart(lbl_ecoind_sim , expand = F, F, 5)
bmt_vboxecoind_sim_table$packStart(bmt_ecoind_sim.sw  , expand = F, F, 0)

bmt_hboxecoind_sim$packStart(bmt_vboxecoind_sim_table, expand = T, T, 10) 
bmt_vboxecoind_sim$packStart(bmt_hboxecoind_sim , expand = F, F, 0)

vbox_effort_landing$packStart(bmt_vboxecoind_sim , expand = F, F, 0)
 }



