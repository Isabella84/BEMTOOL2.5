# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.






# initializating  objects

vbox_economic_data <- gtkVBox(FALSE, 5) 

bmt_economicdata_fleet_combo <<- gtkComboBoxNewText()

bmt_button_load_economicdata <- gtkButtonNewWithLabel(" Load economic data... ")
bmt_button_load_economicdata$AddCallback("clicked", bmt_load_economicdata)   

bmt_button_exp_economicdata <- gtkButtonNewWithLabel(" Apply and save... ")
bmt_button_exp_economicdata$AddCallback("clicked", bmt_save_economicdata_file)     

hbox_economicdata_fleet_combo <<- gtkHBox(homogeneous = FALSE, 5)     
hbox_economicdata_fleet_combo$packStart(gtkLabel(" Fleet Segment "), expand = F, fill = F, padding = 10)  
hbox_economicdata_fleet_combo$packStart(bmt_economicdata_fleet_combo, expand = F, fill = F, padding = 10)  

hbox_economicdata_fleet_combo$packStart(bmt_button_exp_economicdata, expand = FALSE, fill = FALSE, padding = 10)
hbox_economicdata_fleet_combo$packStart(bmt_button_load_economicdata, expand = FALSE, fill = FALSE, padding = 10)

vbox_container_economicdata <- gtkVBox(FALSE, 5)                             
vbox_container_economicdata$packStart(hbox_economicdata_fleet_combo, expand = F, fill = F, padding = 0)   

vbox_economic_data$packStart(vbox_container_economicdata, expand = F, fill = F, padding = 10)  



bmt_button_load_costs <- gtkButtonNewWithLabel(" Load economic data... ")
#bmt_button_load_costs$AddCallback("clicked", bmt_load_Costs)   

bmt_button_save_costs <- gtkButtonNewWithLabel(" Apply and save... ")
#bmt_button_save_costs$AddCallback("clicked", bmt_save_Costs) 


# --------------------------- COSTS table
bmt_vboxCOSTS <- gtkVBox(F, 5) 
bmt_hboxCOSTS  <- gtkHBox(F, 5) 

bmt_vboxCOSTS_frame <- gtkVBox(F, 5) 
bmt_hboxCOSTS_frame  <- gtkHBox(F, 5) 

frame_economic_data_costs <- gtkFrame(" Costs ")
frame_economic_data_costs$add(bmt_vboxCOSTS) 

bmt_hboxCOSTS_frame$packStart(frame_economic_data_costs , expand = T, T, 10) 
bmt_vboxCOSTS_frame$packStart(bmt_hboxCOSTS_frame , expand = F, T, 10)
  

bmt_COSTS.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_COSTS.sw$setShadowType("etched-in")
bmt_COSTS.sw$setPolicy("automatic", "automatic")
bmt_COSTS.sw$SetUsize(120, 150)  
bmt_COSTS_list <<- list()
bmt_COSTSIndex <<- 0
# create model
if (length(BMT_FLEETSEGMENTS) != 0)  {
bmt_COSTS.create_model()
# create tree view
bmt_COSTS.treeview <<- gtkTreeViewNewWithModel(bmt_COSTS.model)
bmt_COSTS.treeview$setRulesHint(TRUE)
bmt_COSTS.treeview$getSelection()$setMode("single")
bmt_COSTS.add_columns(bmt_COSTS.treeview)
bmt_COSTS.sw$add(bmt_COSTS.treeview) 
}

bmt_hboxCOSTS$packStart(bmt_COSTS.sw , expand = T, T, 10) 
bmt_vboxCOSTS$packStart(bmt_hboxCOSTS , expand = F, T, 10)

vbox_container_economicdata$packStart(bmt_vboxCOSTS_frame, expand = F, fill = F, padding = 0) 




# --------------------------- revenues per species table
bmt_vboxREVENUES <- gtkVBox(F, 5) 
bmt_hboxREVENUES  <- gtkHBox(F, 5) 

bmt_vboxREVENUES_frame <- gtkVBox(F, 5) 
bmt_hboxREVENUES_frame  <- gtkHBox(F, 5) 

frame_economic_data_revenues <- gtkFrame(" Revenues per species ")
frame_economic_data_revenues$add(bmt_vboxREVENUES) 

bmt_hboxREVENUES_frame$packStart(frame_economic_data_revenues , expand = T, T, 10) 
bmt_vboxREVENUES_frame$packStart(bmt_hboxREVENUES_frame , expand = F, T, 10)
 

bmt_vboxREVENUES_landing_1 <- gtkVBox(F, 5)  
bmt_REVENUES.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_REVENUES.sw$setShadowType("etched-in")
bmt_REVENUES.sw$setPolicy("automatic", "automatic")
bmt_REVENUES.sw$SetUsize(120, 150)  
bmt_REVENUES_list <<- list()
bmt_REVENUESIndex <<- 0
# create model
if (length(BMT_FLEETSEGMENTS) != 0)  {
bmt_REVENUES.create_model()
# create tree view
bmt_REVENUES.treeview <<- gtkTreeViewNewWithModel(bmt_REVENUES.model)
bmt_REVENUES.treeview$setRulesHint(TRUE)
bmt_REVENUES.treeview$getSelection()$setMode("single")
bmt_REVENUES.add_columns(bmt_REVENUES.treeview)
bmt_REVENUES.sw$add(bmt_REVENUES.treeview)
}
bmt_vboxREVENUES_landing_1$packStart(gtkLabel(" Revenues of landings ") , expand = T, T, 3)
bmt_vboxREVENUES_landing_1$packStart(bmt_REVENUES.sw , expand = T, T, 5)


bmt_vboxREVENUES_discard_2 <- gtkVBox(F, 5)  
bmt_REVENUES_discard.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_REVENUES_discard.sw$setShadowType("etched-in")
bmt_REVENUES_discard.sw$setPolicy("automatic", "automatic")
bmt_REVENUES_discard.sw$SetUsize(120, 150)  
bmt_REVENUES_discard_list <<- list()
bmt_REVENUES_discardIndex <<- 0
# create model
if (length(BMT_FLEETSEGMENTS) != 0)  {
bmt_REVENUES_discard.create_model()
# create tree view
bmt_REVENUES_discard.treeview <<- gtkTreeViewNewWithModel(bmt_REVENUES_discard.model)
bmt_REVENUES_discard.treeview$setRulesHint(TRUE)
bmt_REVENUES_discard.treeview$getSelection()$setMode("single")
bmt_REVENUES_discard.add_columns(bmt_REVENUES_discard.treeview)
bmt_REVENUES_discard.sw$add(bmt_REVENUES_discard.treeview)
}

bmt_vboxREVENUES_discard_2$packStart(gtkLabel(" Revenues of discards ") , expand = T, T, 3)
bmt_vboxREVENUES_discard_2$packStart(bmt_REVENUES_discard.sw , expand = T, T, 5)

bmt_hboxREVENUES$packStart(bmt_vboxREVENUES_landing_1 , expand = T, T, 10)
bmt_hboxREVENUES$packStart(bmt_vboxREVENUES_discard_2 , expand = T, T, 10)
 
bmt_vboxREVENUES$packStart(bmt_hboxREVENUES , expand = F, T, 10)

vbox_container_economicdata$packStart(bmt_vboxREVENUES_frame, expand = F, fill = F, padding = 0)     



# --------------------------- OTHERS table
bmt_vboxOTHERS <- gtkVBox(F, 5) 
bmt_hboxOTHERS  <- gtkHBox(F, 5) 

bmt_vboxOTHERS_frame <- gtkVBox(F, 5) 
bmt_hboxOTHERS_frame  <- gtkHBox(F, 5) 

frame_economic_data_others <- gtkFrame(" Other data ")
frame_economic_data_others$add(bmt_vboxOTHERS) 

bmt_hboxOTHERS_frame$packStart(frame_economic_data_others , expand = T, T, 10) 
bmt_vboxOTHERS_frame$packStart(bmt_hboxOTHERS_frame , expand = F, T, 10)
  

bmt_OTHERS.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_OTHERS.sw$setShadowType("etched-in")
bmt_OTHERS.sw$setPolicy("automatic", "automatic")
bmt_OTHERS.sw$SetUsize(120, 150)  
bmt_OTHERS_list <<- list()
bmt_OTHERSIndex <<- 0
# create model
if (length(BMT_FLEETSEGMENTS) != 0)  {
bmt_OTHERS.create_model()
# create tree view
bmt_OTHERS.treeview <<- gtkTreeViewNewWithModel(bmt_OTHERS.model)
bmt_OTHERS.treeview$setRulesHint(TRUE)
bmt_OTHERS.treeview$getSelection()$setMode("single")
bmt_OTHERS.add_columns(bmt_OTHERS.treeview)
bmt_OTHERS.sw$add(bmt_OTHERS.treeview) 
}

bmt_hboxOTHERS$packStart(bmt_OTHERS.sw , expand = T, T, 10) 
bmt_vboxOTHERS$packStart(bmt_hboxOTHERS , expand = F, T, 10)

vbox_container_economicdata$packStart(bmt_vboxOTHERS_frame, expand = F, fill = F, padding = 0)   


gSignalConnect(bmt_economicdata_fleet_combo, "changed", bmt_reload_economicdata_fleet)  

