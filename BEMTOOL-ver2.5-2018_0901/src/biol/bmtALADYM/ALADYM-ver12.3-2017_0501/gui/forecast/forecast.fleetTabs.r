# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.






#print(".......................................... [forecast.fleetTabs.r]")
vboxFleetTabs <- gtkVBox(homogeneous = FALSE, 5)

hboxFisheryListFleets_fore <- gtkHBox(FALSE, 5)
vboxFisheryListFleets_fore <- gtkVBox(FALSE, 5)
# source(paste(ALADYM_home, "/gui/forecast/forecast.fleetTabs.fleetsegments.r", sep=""))


combo_EffortF <- gtkComboBoxNewText()
for (item in EFFORT_F_TYPE) {
combo_EffortF$appendText(item)
}

# ultimo aggiunto
combo_fleetsegments_fore <<- gtkComboBoxNewText()

if (IN_BEMTOOL) {
associated_fleetsegment <<- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <<- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <<- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

FLEETSEGMENTS_names <<- BMT_FLEETSEGMENTS[associated_fleetsegment_indices]
print(FLEETSEGMENTS_names)
}


if (!is.null(FLEETSEGMENTS_names)) {
for (item in FLEETSEGMENTS_names) {
combo_fleetsegments_fore$appendText(item)
}
}

combo_Scenariotype <- gtkComboBoxNewText()
# gSignalConnect(combo_Scenariotype, "changed", deactivate_SR_unused_params)
for (choice in SCENARIO_TYPE) { 
combo_Scenariotype$appendText(choice) 
}
gtkComboBoxSetActive(combo_Scenariotype, 0 )

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# additional code for BEMTOOL integration
#  gtkWidgetSetSensitive(combo_Scenariotype, FALSE) 
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

#hboxFisheryListFleets_fore$PackStart(gtkLabel("FLEET SEGMENTS"), expand = FALSE, fill = FALSE, padding = 10)
#hboxFisheryListFleets_fore$PackStart(combo_fleetsegments, expand = FALSE, fill = FALSE, padding = 20)
 
tbl_fleets_fore <- gtkTable(2,2,homogeneous = FALSE)
tbl_fleets_fore$SetRowSpacings(10)
tbl_fleets_fore$SetColSpacings(10)
tbl_fleets_fore$SetBorderWidth(5)

i=0  # column 1 
j=0
tbl_fleets_fore$Attach(gtkLabel("FLEET SEGMENTS"),i, i+1, j, j+1) 
   
i=i+1  # column 2    
tbl_fleets_fore$Attach(combo_fleetsegments_fore,i, i+1, j, j+1) 

i=i+1  # column 5
button_savechanges_fleet_fore <- gtkButtonNewWithLabel("Save changes")
button_savechanges_fleet_fore$AddCallback("clicked", update_gear_fore)
tbl_fleets_fore$Attach(button_savechanges_fleet_fore,i, i+1,  j, j+1)


i=0 # column 1
j=j+1
tbl_fleets_fore$Attach(gtkLabel("Effort-F relationship"),i, i+1,  j, j+1) 

i=i+1 # column 2
tbl_fleets_fore$Attach(combo_EffortF,i, i+1,  j, j+1)

i=i+1 # column 3
tbl_fleets_fore$Attach(gtkLabel(" a "),i, i+1,  j, j+1)

i=i+1 # column 4
entry_EffortF_a <- gtkEntry()
gtkEntrySetWidthChars(entry_EffortF_a, NUMERICAL_ENTRY_LENGTH)
tbl_fleets_fore$Attach(entry_EffortF_a,i, i+1,  j, j+1)

i=i+1 # column 5
tbl_fleets_fore$Attach(gtkLabel(" b "),i, i+1,  j, j+1)

i=i+1 # column 6
entry_EffortF_b <- gtkEntry()
gtkEntrySetWidthChars(entry_EffortF_b, NUMERICAL_ENTRY_LENGTH)
tbl_fleets_fore$Attach(entry_EffortF_b,i, i+1,  j, j+1)


i=0 # column 1
j=j+1
tbl_fleets_fore$Attach(gtkLabel("Scenario of reduction"),i, i+1,  j, j+1) 

i=i+1 # column 2
tbl_fleets_fore$Attach(combo_Scenariotype,i, i+1,  j, j+1)

i=0 # column 1
j=j+1
#button_load_selectivity_fore <- gtkButtonNewWithLabel("Load selectivity...")
#button_load_selectivity_fore$AddCallback("clicked", loadSelectivityfromFile_fore)
#tbl_fleets_fore$Attach(button_load_selectivity_fore ,i, i+1,  j, j+1)

#i=i+1
button_load_discard_fore <- gtkButtonNewWithLabel("Load discard...")
button_load_discard_fore$AddCallback("clicked", loadDiscardsfromFile_fore)
tbl_fleets_fore$Attach(button_load_discard_fore ,i, i+1,  j, j+1)

i=i+1  # column 5
button_load_effortdata_fore <- gtkButtonNewWithLabel("Load effort data...")
button_load_effortdata_fore$AddCallback("clicked", loadEffortDatafromFile_fore)
tbl_fleets_fore$Attach(button_load_effortdata_fore ,i, i+1,  j, j+1)

i=i+1  # column 5
button_load_fc_fore <- gtkButtonNewWithLabel("Load fishing coefficient...")
button_load_fc_fore$AddCallback("clicked", loadFishingCoefficientfromFile_fore)
tbl_fleets_fore$Attach(button_load_fc_fore ,i, i+1,  j, j+1)

#i=i+1
#button_saveall_selectivity_fore <- gtkButtonNewWithLabel("Export selectivity...")
#button_saveall_selectivity_fore$AddCallback("clicked", saveSelectivitytoFile_fore)
#tbl_fleets_fore$Attach(button_saveall_selectivity_fore ,i, i+1,  j, j+1)

i=i+1
button_saveall_discards_fore <- gtkButtonNewWithLabel("Export discard...")
button_saveall_discards_fore$AddCallback("clicked", saveDiscardstoFile_fore)
tbl_fleets_fore$Attach(button_saveall_discards_fore ,i, i+1,  j, j+1)

i=i+1  # column 5
button_saveall_effortdata_fore <- gtkButtonNewWithLabel("Export effort data...")
button_saveall_effortdata_fore$AddCallback("clicked", saveEffortDatatoFile_fore)
tbl_fleets_fore$Attach(button_saveall_effortdata_fore ,i, i+1,  j, j+1)

i=i+1  # column 5
button_saveall_fc_fore <- gtkButtonNewWithLabel("Export fishing coefficient...")
button_saveall_fc_fore$AddCallback("clicked", exportFishingCoefficienttoFile_fore)
tbl_fleets_fore$Attach(button_saveall_fc_fore ,i, i+1,  j, j+1)

vboxFleetTabs$packStart(tbl_fleets_fore, expand = FALSE, fill = FALSE, padding=0)
hboxFisheryListFleets_fore$PackStart(vboxFisheryListFleets_fore, expand = FALSE, fill = FALSE, padding = 10)
vboxFleetTabs$PackStart(hboxFisheryListFleets_fore, expand = FALSE, fill = FALSE, padding = 5)


#
#i=i+1
#j=0

#
##i=i+1
##j=0
##button_saveall_Fmortalities <- gtkButtonNewWithLabel("Save all mortalities...")
##button_saveall_Fmortalities$AddCallback("clicked", saveFtoFile)
##tbl_fleets_forsaving$Attach(button_saveall_Fmortalities ,i, i+1,  j, j+1)


#hboxScenarioType <- gtkHBox(FALSE, 5)
#hboxScenarioType$packStart(gtkLabel("Scenario of reduction"), expand = FALSE, fill = FALSE, padding = 5)

#combo_SRtype$setPopdownStrings(SR_TYPE)
#hboxScenarioType$packStart(combo_Scenariotype, expand = FALSE, fill = FALSE, padding = 20)

#vboxFleetTabs$packStart(hboxScenarioType, expand = FALSE, fill = FALSE, padding=10)

#hboxFisheryName_fore <- gtkHBox(FALSE, 5)
#hboxFisheryName_fore$PackStart(gtkLabel("Fleet segment name"), expand = FALSE, fill = FALSE, padding = 10)
#
#entryGearName_fore <- gtkEntry()
#gtkEntrySetEditable(entryGearName_fore, FALSE)
#hboxFisheryName_fore$PackStart(entryGearName_fore, expand = FALSE, fill = FALSE, padding = 10)
#vboxFleetTabs$PackStart(hboxFisheryName_fore, expand = FALSE, fill = FALSE, padding = 10)

notebook_forecast_fleets <- gtkNotebook()
notebook_forecast_fleets$setTabPos("top")

hboxFisherySelectivity_fore <- gtkHBox(FALSE, 5)
vboxFisherySelectivity_fore <- gtkVBox(homogeneous = FALSE, 5)
suppressWarnings(source(paste(ALADYM_home, "/gui/forecast/fishery.selectivity_fore.r", sep=""))  )
hboxFisherySelectivity_fore$packStart(vboxFisherySelectivity_fore, expand = TRUE, fill = TRUE, padding=5)				
notebook_forecast_fleets$appendPage(hboxFisherySelectivity_fore, gtkLabel(str=" SELECTIVITY "))


hboxDiscard_fore <- gtkHBox(FALSE, 5)
vboxDiscard_fore  <- gtkVBox(homogeneous = FALSE, 5)
suppressWarnings(source(paste(ALADYM_home, "/gui/forecast/fishery.discard_fore.r", sep="")) )
hboxDiscard_fore$packStart(vboxDiscard_fore , expand = TRUE, fill = TRUE, padding=5)

# ___________________________________________________________________________________________________________

frame_discard_fore <- gtkFrame(" DISCARD MODEL ")   
#hbox_discard_fore <- gtkHBox(homogeneous = FALSE, 5)
#vbox_discard_fore <- gtkVBox(homogeneous = FALSE, 5)             
#
#hbox_discard_fore$packStart(hboxDiscard_fore, expand = TRUE, fill = TRUE, padding = 10)
#vbox_discard_fore$packStart(hbox_discard_fore, expand = TRUE, fill = TRUE, padding = 10)
frame_discard_fore$add(hboxDiscard_fore) 

h_frame_discard_fore <- gtkHBox(homogeneous = FALSE, 5)
h_frame_discard_fore$packStart(frame_discard_fore, expand = TRUE, fill = TRUE, padding = 10)		

v_frame_discard_fore <- gtkVBox(homogeneous = FALSE, 5)
v_frame_discard_fore$packStart(h_frame_discard_fore, expand = TRUE, fill = F, padding = 5)
#vbox_Production_Discard$packStart(h_frame_discard, expand = FALSE, fill = TRUE, padding = 10)


vboxLandingObligation_fore <- gtkVBox(FALSE, 5)
suppressWarnings(source(paste(ALADYM_home, "/gui/forecast/fishery.landingobligation_fore.r", sep="")) )

frame_lan_obligation_fore <- gtkFrame(" LANDING OBLIGATION ")   
hbox_lan_obligation_fore <- gtkHBox(homogeneous = FALSE, 5)
vbox_lan_obligation_fore <- gtkVBox(homogeneous = FALSE, 5)             

hbox_lan_obligation_fore$packStart(vboxLandingObligation_fore, expand = TRUE, fill = TRUE, padding = 10)
vbox_lan_obligation_fore$packStart(hbox_lan_obligation_fore, expand = TRUE, fill = TRUE, padding = 10)
frame_lan_obligation_fore$add(vbox_lan_obligation_fore) 

h_frame_lan_obligation_fore <- gtkHBox(homogeneous = FALSE, 5)
h_frame_lan_obligation_fore$packStart(frame_lan_obligation_fore, expand = TRUE, fill = TRUE, padding = 10)		

v_frame_lan_obligation_fore <- gtkVBox(homogeneous = FALSE, 5)
v_frame_lan_obligation_fore$packStart(h_frame_lan_obligation_fore, expand = TRUE, fill = T, padding = 5)

v_frame_schema_discard_fore <- gtkVBox(homogeneous = FALSE, 5)
v_frame_schema_discard_fore$packStart(v_frame_discard_fore, expand = TRUE, fill = F, padding = 5)
v_frame_schema_discard_fore$packStart(v_frame_lan_obligation_fore, expand = TRUE, fill = T, padding = 5)

notebook_forecast_fleets$appendPage(v_frame_schema_discard_fore, gtkLabel(str=" DISCARD "))

# ________________________________________________________________________________________________________________



v_frame_schema_sur_fore <- gtkVBox(homogeneous = FALSE, 5)
#v_frame_schema_sur$packStart(v_frame_survivability, expand = TRUE, fill = F, padding = 5)

suppressWarnings(source(paste(ALADYM_home, "/gui/forecast/fishery.discardSurvivalRate_fore.r", sep="")) )
v_frame_schema_sur_fore$packStart(hbox_box_DiscardSurvivalRate_fore, expand = F, fill = F, padding = 5)

suppressWarnings(source(paste(ALADYM_home, "/gui/forecast/fishery.escapeSurvivalRate_fore.r", sep="")) )
v_frame_schema_sur_fore$packStart(hbox_box_EscapeSurvivalRate_fore, expand = F, fill = F, padding = 5)

notebook_forecast_fleets$appendPage(v_frame_schema_sur_fore, gtkLabel(str=" SURVIVABILITY "))


# ________________________________________________________________________________________________________________



                                     
vboxFishingFore <- gtkVBox(homogeneous = FALSE, 5)

vboxFishingFore_effortdata <- gtkVBox(homogeneous = FALSE, 5)

hboxVESSELS_fore <- gtkHBox(FALSE, 5)
vboxVESSELS_fore  <- gtkVBox(homogeneous = FALSE, 5)
suppressWarnings(source(paste(ALADYM_home, "/gui/forecast/fishery.vessels_fore.r", sep=""))  )
hboxVESSELS_fore$packStart(vboxVESSELS_fore , expand = TRUE, fill = TRUE, padding=5)	
vboxFishingFore_effortdata$packStart(hboxVESSELS_fore , expand = FALSE, fill = TRUE, padding=5)	

hboxDAYS_fore <- gtkHBox(FALSE, 5)
vboxDAYS_fore  <- gtkVBox(homogeneous = FALSE, 5)
suppressWarnings(source(paste(ALADYM_home, "/gui/forecast/fishery.days_fore.r", sep="")) )
hboxDAYS_fore$packStart(vboxDAYS_fore , expand = TRUE, fill = TRUE, padding=5)	
vboxFishingFore_effortdata$packStart(hboxDAYS_fore , expand = FALSE, fill = TRUE, padding=5)	

hboxGT_fore <- gtkHBox(FALSE, 5)
vboxGT_fore  <- gtkVBox(homogeneous = FALSE, 5)
suppressWarnings(source(paste(ALADYM_home, "/gui/forecast/fishery.gt_fore.r", sep="")))
hboxGT_fore$packStart(vboxGT_fore , expand = TRUE, fill = TRUE, padding=5)	
vboxFishingFore_effortdata$packStart(hboxGT_fore , expand = FALSE, fill = TRUE, padding=5)

hbox_frame_effortdata_fore <- gtkHBox(homogeneous = FALSE, 5)

h_frame_effortdata_fore <- gtkFrame(" EFFORT DATA ") 
h_frame_effortdata_fore$add(vboxFishingFore_effortdata)
hbox_frame_effortdata_fore$packStart(h_frame_effortdata_fore, expand = TRUE, fill = TRUE, padding = 10)		
# fisheryNotebook$appendPage(h_frame_effortdata, gtkLabel(str="FISHING EFFORT"))

vboxFishingFore$packStart(hbox_frame_effortdata_fore, expand = FALSE, fill = TRUE, padding = 5)

vboxFishingFore_coefficient <- gtkVBox(homogeneous = FALSE, 5)

hboxFISHINGEFFORT_fore <- gtkHBox(FALSE, 5)
vboxFISHINGEFFORT_fore  <- gtkVBox(homogeneous = FALSE, 5)
suppressWarnings(source(paste(ALADYM_home, "/gui/forecast/fishery.fishingeffort_fore.r", sep=""))  )
hboxFISHINGEFFORT_fore$packStart(vboxFISHINGEFFORT_fore , expand = TRUE, fill = TRUE, padding=5)	
vboxFishingFore_coefficient$packStart(hboxFISHINGEFFORT_fore , expand = FALSE, fill = TRUE, padding=5)	


h_frame_activity_fore <- gtkFrame(" FISHING COEFFICIENT ") 
h_frame_activity_fore$add(vboxFishingFore_coefficient)

 hbox_frame_fishingcoefficient_fore <- gtkHBox(homogeneous = FALSE, 5)
hbox_frame_fishingcoefficient_fore$packStart(h_frame_activity_fore, expand = TRUE, fill = TRUE, padding = 10)		

vboxFishingFore$packStart(hbox_frame_fishingcoefficient_fore, expand = FALSE, fill = TRUE, padding = 5)

			
notebook_forecast_fleets$appendPage(vboxFishingFore , gtkLabel(str=" FISHING EFFORT "))

vboxFleetTabs$packStart(notebook_forecast_fleets, expand = FALSE, fill = TRUE, padding=5)
#
#tbl_fleets_fore_forsaving <- gtkTable(1,7,homogeneous = FALSE)
#tbl_fleets_fore_forsaving$SetRowSpacings(20)
#tbl_fleets_fore_forsaving$SetColSpacings(30)
#tbl_fleets_fore_forsaving$SetBorderWidth(5)
#
#i=0  # column 1 
#j=0
#button_savechanges_fleet_fore <- gtkButtonNewWithLabel("Save all changes")
#button_savechanges_fleet_fore$AddCallback("clicked", update_gear_fore)
#tbl_fleets_fore_forsaving$Attach(button_savechanges_fleet_fore,i, i+1,  j, j+1)
#
#i=i+1
#j=0
#button_saveall_selectivity_fore <- gtkButtonNewWithLabel("Save all selectivities...")
#button_saveall_selectivity_fore$AddCallback("clicked", saveSelectivitytoFile_fore)
#tbl_fleets_fore_forsaving$Attach(button_saveall_selectivity_fore ,i, i+1,  j, j+1)
#
#i=i+1
#j=0
#button_saveall_discards_fore <- gtkButtonNewWithLabel("Save all discards...")
#button_saveall_discards_fore$AddCallback("clicked", saveDiscardstoFile_fore)
#tbl_fleets_fore_forsaving$Attach(button_saveall_discards_fore ,i, i+1,  j, j+1)
#
##i=i+1
##j=0
##button_saveall_Fmortalities <- gtkButtonNewWithLabel("Save all mortalities...")
##button_saveall_Fmortalities$AddCallback("clicked", saveFtoFile)
##tbl_fleets_forsaving$Attach(button_saveall_Fmortalities ,i, i+1,  j, j+1)
#
#vboxFleetTabs$packStart(tbl_fleets_fore_forsaving, expand = FALSE, fill = TRUE, padding=5)

hboxFleetTabs <- gtkHBox(homogeneous = FALSE, 5)
hboxFleetTabs$packStart(vboxFleetTabs, expand = TRUE, fill = TRUE, padding=5)

#if (length(FLEETSEGMENTS_names) > 0 ) {
#gtkComboBoxSetActive(combo_fleetsegments_fore, 0 )
#}
# deactivate_Discard_unused_params_fore()

if (!IN_BEMTOOL) {
gSignalConnect(radio_data, "toggled", deactivate_Pproduction_unused_params)
gSignalConnect(radio_pp, "toggled", deactivate_Pproduction_unused_params)
gSignalConnect(radio_effortdata, "toggled", deactivate_FishingEffort_unused_params)
gSignalConnect(radio_fishingcoeff, "toggled", deactivate_FishingEffort_unused_params)
}

gtkWidgetSetSensitive(combo_Scenariotype, FALSE)
gSignalConnect(combo_fleetsegments_fore, "changed", reload_fleetsegment_fore_info)
gtkComboBoxSetActive(combo_fleetsegments_fore, 0 )
