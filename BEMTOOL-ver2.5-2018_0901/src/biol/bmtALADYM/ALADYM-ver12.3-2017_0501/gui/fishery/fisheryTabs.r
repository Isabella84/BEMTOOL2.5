# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





# print(".......................................... [fisheryTabs.r]", quote=F)
fisheryNotebook <- gtkNotebook()
fisheryNotebook$setTabPos("top")

# ********************************************************** SELECTIVITY
vboxFisherySelectivity <- gtkVBox(FALSE, 5)
suppressWarnings(source(paste(ALADYM_home, "/gui/fishery/fishery.selectivity.r", sep="")) )
vbox_selectivity <- gtkVBox(homogeneous = FALSE, 5)             
#frame_selectivity <- gtkFrame(" Selectivity ")   
hbox_selectivity <- gtkHBox(homogeneous = FALSE, 5)

hbox_selectivity$packStart(vboxFisherySelectivity, expand = TRUE, fill = TRUE, padding = 10)
vbox_selectivity$packStart(hbox_selectivity, expand = FALSE, fill = TRUE, padding = 10)
#frame_selectivity$add(vbox_selectivity) 

h_frame_selectivity <- gtkHBox(homogeneous = FALSE, 5)
h_frame_selectivity$packStart(vbox_selectivity, expand = TRUE, fill = TRUE, padding = 10)		
fisheryNotebook$appendPage(h_frame_selectivity, gtkLabel(str=" SELECTIVITY "))


# ********************************************************** PRODUCTION

hbox_Production_Discard <- gtkHBox(FALSE, 5)

vbox_Production_Discard <- gtkVBox(FALSE, 5)
hbox_Production_Discard$packStart(vbox_Production_Discard, expand = TRUE, fill = TRUE, padding = 5)



tbl_EffortData_catch <- gtkTable(1,3,homogeneous = FALSE)
tbl_EffortData_catch$SetRowSpacings(4)
tbl_EffortData_catch$SetColSpacings(30)
tbl_EffortData_catch$SetBorderWidth(5) 

i=0
j=0   
tbl_EffortData_catch$Attach(gtkLabel("Data to load"),i, i+1, j, j+1)  

i=i+1
j=0   
tbl_EffortData_catch$Attach(radio_data,i, i+1, j, j+1)  

i=i+1
j=0   
tbl_EffortData_catch$Attach(radio_pp,i, i+1, j, j+1) 

i=i+1  # column 2
j=0                 
tbl_EffortData_catch$Attach(button_load_production ,i, i+1,  j, j+1)

i=i+1  # column 2
j=0                 
tbl_EffortData_catch$Attach(button_load_discard_data ,i, i+1,  j, j+1)

i=i+1  # column 3
j=0
tbl_EffortData_catch$Attach(button_load_p_production ,i, i+1,  j, j+1)


i=i+1  # column 2
j=0                 
tbl_EffortData_catch$Attach(button_exp_production ,i, i+1,  j, j+1)

i=i+1  # column 2
j=0                 
tbl_EffortData_catch$Attach(button_exp_discard_data ,i, i+1,  j, j+1)

i=i+1  # column 3
j=0
tbl_EffortData_catch$Attach(button_exp_p_production ,i, i+1,  j, j+1)


#hboxProductionData$packStart(tbl_EffortData, FALSE, FALSE, 5) 
    
vbox_Production_Discard$PackStart(tbl_EffortData_catch, expand = FALSE, fill = FALSE, padding = 10)


vboxProductionData <- gtkVBox(FALSE, 5)
suppressWarnings(source(paste(ALADYM_home, "/gui/fishery/fishery.production.r", sep=""))  )
#vbox_Production_Discard <- gtkVBox(homogeneous = FALSE, 5)
             
frame_production <- gtkFrame(" CATCH ")   
hbox_production <- gtkHBox(homogeneous = FALSE, 5)
vbox_production <- gtkVBox(homogeneous = FALSE, 5)             

hbox_production$packStart(vboxProductionData, expand = TRUE, fill = TRUE, padding = 10)
vbox_production$packStart(hbox_production, expand = TRUE, fill = TRUE, padding = 10)
frame_production$add(vbox_production) 

h_frame_production <- gtkHBox(homogeneous = FALSE, 5)
h_frame_production$packStart(frame_production, expand = TRUE, fill = TRUE, padding = 5)		
h_frame_production$packStart(hbox_production, expand = TRUE, fill = TRUE, padding = 5)		

vbox_Production_Discard$packStart(h_frame_production, expand = F, fill = FALSE, padding = 10)

h_PP <- gtkHBox(homogeneous = FALSE, 5)
h_PP$packStart(vboxPproduction, expand = TRUE, fill = TRUE, padding = 5)		

 vbox_Production_Discard$packStart(h_PP, expand = F, fill = FALSE, padding = 10)
 
 
 
fisheryNotebook$appendPage(hbox_Production_Discard, gtkLabel(str=" CATCH DATA "))

vboxEFFORT <- gtkVBox(FALSE, 5)


# ********************************************************** DISCARD
vboxDiscardSurvivalRate <- gtkVBox(FALSE, 5)
# suppressWarnings(source(paste(ALADYM_home, "/gui/fishery/fishery.discardSurvivalRate.r", sep="")) )
vbox_DiscardSurvivalRate <- gtkVBox(homogeneous = FALSE, 5)             
#frame_selectivity <- gtkFrame(" Selectivity ")   
hbox_DiscardSurvivalRate <- gtkHBox(homogeneous = FALSE, 5)

hbox_DiscardSurvivalRate$packStart(vboxDiscardSurvivalRate, expand = TRUE, fill = TRUE, padding = 10)
vbox_DiscardSurvivalRate$packStart(hbox_DiscardSurvivalRate, expand = FALSE, fill = TRUE, padding = 10)
#frame_selectivity$add(vbox_selectivity) 

h_frame_DiscardSurvivalRate <- gtkHBox(homogeneous = FALSE, 5)
h_frame_DiscardSurvivalRate$packStart(vbox_DiscardSurvivalRate, expand = TRUE, fill = TRUE, padding = 10)	


vboxDiscard <- gtkVBox(FALSE, 5)
suppressWarnings(source(paste(ALADYM_home, "/gui/fishery/fishery.discard.r", sep="")) )
#vbox_Production_Discard <- gtkVBox(homogeneous = FALSE, 5)             
frame_discard <- gtkFrame(" DISCARD MODEL ")   
hbox_discard <- gtkHBox(homogeneous = FALSE, 5)
vbox_discard <- gtkVBox(homogeneous = FALSE, 5)             

hbox_discard$packStart(vboxDiscard, expand = TRUE, fill = TRUE, padding = 10)
vbox_discard$packStart(hbox_discard, expand = TRUE, fill = TRUE, padding = 10)
frame_discard$add(vbox_discard) 

h_frame_discard <- gtkHBox(homogeneous = FALSE, 5)
h_frame_discard$packStart(frame_discard, expand = TRUE, fill = TRUE, padding = 10)		

v_frame_discard <- gtkVBox(homogeneous = FALSE, 5)
v_frame_discard$packStart(h_frame_discard, expand = TRUE, fill = F, padding = 5)
#vbox_Production_Discard$packStart(h_frame_discard, expand = FALSE, fill = TRUE, padding = 10)


vboxLandingObligation <- gtkVBox(FALSE, 5)
suppressWarnings(source(paste(ALADYM_home, "/gui/fishery/fishery.landingobligation.r", sep="")) )

frame_lan_obligation <- gtkFrame(" LANDING OBLIGATION ")   
hbox_lan_obligation <- gtkHBox(homogeneous = FALSE, 5)
vbox_lan_obligation <- gtkVBox(homogeneous = FALSE, 5)             

hbox_lan_obligation$packStart(vboxLandingObligation, expand = TRUE, fill = TRUE, padding = 10)
vbox_lan_obligation$packStart(hbox_lan_obligation, expand = TRUE, fill = TRUE, padding = 10)
frame_lan_obligation$add(vbox_lan_obligation) 

h_frame_lan_obligation <- gtkHBox(homogeneous = FALSE, 5)
h_frame_lan_obligation$packStart(frame_lan_obligation, expand = TRUE, fill = TRUE, padding = 10)		

v_frame_lan_obligation <- gtkVBox(homogeneous = FALSE, 5)
v_frame_lan_obligation$packStart(h_frame_lan_obligation, expand = TRUE, fill = T, padding = 5)


v_frame_schema_discard <- gtkVBox(homogeneous = FALSE, 5)
v_frame_schema_discard$packStart(v_frame_discard, expand = TRUE, fill = F, padding = 5)
v_frame_schema_discard$packStart(v_frame_lan_obligation, expand = TRUE, fill = T, padding = 5)


fisheryNotebook$appendPage(v_frame_schema_discard, gtkLabel(str=" DISCARD "))

v_frame_schema_sur <- gtkVBox(homogeneous = FALSE, 5)
#v_frame_schema_sur$packStart(v_frame_survivability, expand = TRUE, fill = F, padding = 5)

suppressWarnings(source(paste(ALADYM_home, "/gui/fishery/fishery.discardSurvivalRate.r", sep="")) )
v_frame_schema_sur$packStart(hbox_box_DiscardSurvivalRate, expand = F, fill = F, padding = 5)

suppressWarnings(source(paste(ALADYM_home, "/gui/fishery/fishery.escapeSurvivalRate.r", sep="")) )
v_frame_schema_sur$packStart(hbox_box_EscapeSurvivalRate, expand = F, fill = F, padding = 5)

fisheryNotebook$appendPage(v_frame_schema_sur, gtkLabel(str=" SURVIVABILITY "))



vboxEFFORTvars <- gtkVBox(FALSE, 5)


tbl_EffortData_eff <- gtkTable(1,3,homogeneous = FALSE)
tbl_EffortData_eff$SetRowSpacings(4)
tbl_EffortData_eff$SetColSpacings(30)
tbl_EffortData_eff$SetBorderWidth(5) 

i=0
j=0   
tbl_EffortData_eff$Attach(gtkLabel("Fishing effort to load"),i, i+1, j, j+1) 
i=i+1
j=0   
tbl_EffortData_eff$Attach(radio_effortdata,i, i+1, j, j+1)  
i=i+1
j=0   
tbl_EffortData_eff$Attach(radio_fishingcoeff,i, i+1, j, j+1)  
i=i+1  # column 2
j=0                 
tbl_EffortData_eff$Attach(button_load_effortdata ,i, i+1,  j, j+1)
i=i+1  # column 3
j=0                   
tbl_EffortData_eff$Attach(button_load_fishingcoeff  ,i, i+1,  j, j+1)
i=i+1  # column 2
j=0                 
tbl_EffortData_eff$Attach(button_exp_effortdata ,i, i+1,  j, j+1)
i=i+1  # column 3
j=0                  
tbl_EffortData_eff$Attach(button_exp_fishingcoeff  ,i, i+1,  j, j+1)
#hboxProductionData$packStart(tbl_EffortData, FALSE, FALSE, 5) 
    
vboxEFFORTvars$PackStart(tbl_EffortData_eff, expand = FALSE, fill = FALSE, padding = 10)



# ********************************************************** VESSELS

vboxVESSELS <- gtkVBox(FALSE, 0)
suppressWarnings(source(paste(ALADYM_home, "/gui/fishery/fishery.effort.vessels.r", sep="")) )
vbox_VESSELS <- gtkVBox(homogeneous = FALSE, 0)                
hbox_VESSELS <- gtkHBox(homogeneous = FALSE, 5)

hbox_VESSELS$packStart(vboxVESSELS, expand = TRUE, fill = TRUE, padding = 5)
vboxEFFORTvars$packStart(hbox_VESSELS, expand = FALSE, fill = TRUE, padding = 0)  

#vboxEFFORTvars$packStart(tblEFFORTvariables, expand = FALSE, fill = TRUE, padding = 5)  

# ********************************************************** DAYS

vboxDAYS<- gtkVBox(FALSE, 0)
suppressWarnings(source(paste(ALADYM_home, "/gui/fishery/fishery.effort.days.r", sep=""))  )
vbox_DAYS <- gtkVBox(homogeneous = FALSE, 0)                
hbox_DAYS <- gtkHBox(homogeneous = FALSE, 5)

hbox_DAYS$packStart(vboxDAYS, expand = TRUE, fill = TRUE, padding = 5)
vboxEFFORTvars$packStart(hbox_DAYS, expand = FALSE, fill = TRUE, padding = 0)

# ********************************************************** GT

vboxGT<- gtkVBox(FALSE, 0)
suppressWarnings(source(paste(ALADYM_home, "/gui/fishery/fishery.effort.gt.r", sep="")) )
vbox_GT <- gtkVBox(homogeneous = FALSE, 0)                
hbox_GT <- gtkHBox(homogeneous = FALSE, 5)

hbox_GT$packStart(vboxGT, expand = TRUE, fill = TRUE, padding = 5)
vboxEFFORTvars$packStart(hbox_GT, expand = FALSE, fill = TRUE, padding = 0)

hbox_frame_effortdata <- gtkHBox(homogeneous = FALSE, 5)

h_frame_effortdata <- gtkFrame(" EFFORT DATA ") 
h_frame_effortdata$add(vboxEFFORTvars)
hbox_frame_effortdata$packStart(h_frame_effortdata, expand = TRUE, fill = TRUE, padding = 10)		
# fisheryNotebook$appendPage(h_frame_effortdata, gtkLabel(str="FISHING EFFORT"))

vboxEFFORT$packStart(hbox_frame_effortdata, expand = FALSE, fill = TRUE, padding = 5)

# ********************************************************** FISHING EFFORT
vboxFISHINGEFFORT <- gtkVBox(FALSE, 0)
suppressWarnings(source(paste(ALADYM_home, "/gui/fishery/fishery.effort.fishingeffort.r", sep=""))  )

vbox_FISHINGEFFORT <- gtkVBox(homogeneous = FALSE, 5)                
hbox_FISHINGEFFORT <- gtkHBox(homogeneous = FALSE, 5)

hbox_FISHINGEFFORT$packStart(vboxFISHINGEFFORT, expand = TRUE, fill = TRUE, padding = 5)
#vboxEFFORTvars$packStart(hbox_FISHINGEFFORT, expand = TRUE, fill = TRUE, padding = 5)
 
#gtkWidgetSetSensitive(vboxFISHINGEFFORT, FALSE)

h_frame_activity <- gtkFrame(" FISHING COEFFICIENT ") 
h_frame_activity$add(hbox_FISHINGEFFORT)

hbox_frame_fishingcoefficient <- gtkHBox(homogeneous = FALSE, 5)
hbox_frame_fishingcoefficient$packStart(h_frame_activity, expand = TRUE, fill = TRUE, padding = 10)		

vboxEFFORT$packStart(hbox_frame_fishingcoefficient, expand = FALSE, fill = TRUE, padding = 5)

fisheryNotebook$appendPage(vboxEFFORT, gtkLabel(str=" FISHING EFFORT "))

hboxFisheryTabs$packStart(fisheryNotebook, expand = TRUE, fill = TRUE, padding = 5)


# ********************************************************** FISHING MORTALITY
vboxFishingMortality <- gtkVBox(FALSE, 5)

#vboxFishingMortality$packStart(tbl_fleets_Fmort, expand = FALSE, fill = FALSE, padding = 5)

suppressWarnings(source(paste(ALADYM_home, "/gui/fishery/fishery.fishingmortality.r", sep="")) )
vbox_fishingmortality <- gtkVBox(homogeneous = FALSE, 5)             
frame_fishingmortality <- gtkFrame(" F BY FLEET SEGMENT ")   
hbox_fishingmortality <- gtkHBox(homogeneous = FALSE, 5)

hbox_fishingmortality$packStart(hboxEnteringFishingMortality, expand = TRUE, fill = TRUE, padding = 5)
vbox_fishingmortality$packStart(hbox_fishingmortality, expand = FALSE, fill = TRUE, padding = 5)
frame_fishingmortality$add(vbox_fishingmortality) 

#
h_frame_fishingmortality <- gtkHBox(homogeneous = FALSE, 5)
h_frame_fishingmortality$packStart(frame_fishingmortality, expand = TRUE, fill = TRUE, padding = 5)	
vboxFishingMortality$packStart(h_frame_fishingmortality, expand = FALSE, fill = FALSE, padding = 5)



vbox_fishingmortality_overall <- gtkVBox(homogeneous = FALSE, 5)             
frame_fishingmortality_overall <- gtkFrame(" Splitting of overall F ")   
hbox_fishingmortality_overall <- gtkHBox(homogeneous = FALSE, 5)

hbox_fishingmortality_overall$packStart(hboxEnteringFishingMortality_overall, expand = TRUE, fill = TRUE, padding = 5)
vbox_fishingmortality_overall$packStart(hbox_fishingmortality_overall, expand = FALSE, fill = TRUE, padding = 5)
frame_fishingmortality_overall$add(vbox_fishingmortality_overall) 

#
h_frame_fishingmortality_overall <- gtkHBox(homogeneous = FALSE, 5)
h_frame_fishingmortality_overall$packStart(frame_fishingmortality_overall, expand = TRUE, fill = TRUE, padding = 5)	
vboxFishingMortality$packStart(h_frame_fishingmortality_overall, expand = FALSE, fill = FALSE, padding = 5)

		
fisheryNotebook$appendPage(vboxFishingMortality, gtkLabel(str=" FISHING MORTALITY "))

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION") ) {
gSignalConnect(entryVBF_M_lifespan, "changed", change_fishingM_M)
gSignalConnect(entryVBF_F_lifespan, "changed", change_fishingM_F)
gSignalConnect(entryVBF_M_lifespan, "changed", change_lifespanM)
gSignalConnect(entryVBF_F_lifespan, "changed", change_lifespanF)
gSignalConnect(entryOFFSPRING_tr, "changed", change_fishingM_M)
gSignalConnect(entryOFFSPRING_tr, "changed", change_fishingM_F)

gtkEntrySetText(entryVBF_M_lifespan, biological.lifeSpanM )
gtkEntrySetText(entryVBF_F_lifespan, biological.lifeSpanF )
}