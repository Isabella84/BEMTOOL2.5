# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




# ********************************* GENERAL DATA graphical elements

vboxGeneralData <- gtkVBox(homogeneous = FALSE, 5)

vboxProjectProperties <- gtkVBox(FALSE, 5)

tbl_Properties <- gtkTable(3,3,homogeneous = FALSE)
tbl_Properties$SetRowSpacings(7)
tbl_Properties$SetColSpacings(30)
tbl_Properties$SetBorderWidth(5)

i=0 # column 1
j=0  # row 1  
tbl_Properties$Attach(gtkLabel("Scientific name"),i, i+1, j, j+1) 
j=j+1  # row 2 
tbl_Properties$Attach(gtkLabel("Common name"),i, i+1,  j, j+1)
j=j+1   # row 3
tbl_Properties$Attach(gtkLabel( "Geographical Area"),i, i+1,  j, j+1)

i=i+1  # column 2
j=0  # row 1
entrySpecies <- gtkEntry()   
tbl_Properties$Attach(entrySpecies,i, i+1, j, j+1) 
j=j+1   # row 2
entrySpeciesCommonName <- gtkEntry()
tbl_Properties$Attach(entrySpeciesCommonName,i, i+1,  j, j+1)
j=j+1    # row 3
entryGSA <- gtkEntry()
tbl_Properties$Attach(entryGSA,i, i+1,  j, j+1)

i=i+1   # column 3  
j=0   # row 1
#tbl_Properties$Attach(gtkLabel( "(es: M. merluccius)"),i, i+1, j, j+1) 
tbl_Properties$Attach(gtkLabel( ""),i, i+1, j, j+1) 
j=j+1 
# tbl_Properties$Attach(gtkLabel( "(es: European hake)"),i, i+1,  j, j+1)
tbl_Properties$Attach(gtkLabel( ""),i, i+1,  j, j+1)
j=j+1
#tbl_Properties$Attach(gtkLabel( "[e.g. 18]"),i, i+1,  j, j+1)
#tbl_Properties$Attach(gtkLabel( ""),i, i+1,  j, j+1)


hboxProjectProperties <- gtkHBox(homogeneous = FALSE, 5)
hboxProjectProperties$packStart(tbl_Properties, expand = FALSE, fill = TRUE, padding = 10)

vboxProjectProperties$packStart(hboxProjectProperties, expand = FALSE, fill = TRUE, padding = 10)



vboxControlParameters <- gtkVBox(FALSE, 5)

tbl_Control <- gtkTable(6,5,homogeneous = FALSE)
tbl_Control$SetRowSpacings(7)
tbl_Control$SetColSpacings(30)
tbl_Control$SetBorderWidth(5)

i=0    # column 0
j=0   # row 1
tbl_Control$Attach(gtkLabel("Start year of simulation"),i, i+1, j, j+1)
j=j+1   # row 2
j=j+1    # row 3
tbl_Control$Attach(gtkLabel("Time slice per year [months]"),i, i+1, j, j+1) 
j=j+1    # row 4
tbl_Control$Attach(gtkLabel("Years to be pre-simulated"),i, i+1,  j, j+1) 
j=j+1     # row 5
tbl_Control$Attach(gtkLabel("Number of Run for seed randomization"),i, i+1,  j, j+1) 
j=j+1      # row 6
tbl_Control$Attach(gtkLabel("Number years for average in forecast"),i, i+1, j, j+1) 
 j=j+1      # row 7
chkReferencePoints <- gtkCheckButton("Reference points calculation")
tbl_Control$Attach(chkReferencePoints,i, i+1,  j, j+1) 

 
i=i+1   # column 1
j=0      # row 1
entry_StartYear_simulation <- gtkEntry()
gtkEntrySetWidthChars(entry_StartYear_simulation, NUMERICAL_ENTRY_LENGTH)   
# gSignalConnect(entry_StartYear_simulation, "changed", change_startend_year)
tbl_Control$Attach(entry_StartYear_simulation,i, i+1,  j, j+1) 
j=j+1     # row 2
j=j+1      # row 3
entryTimeSlicePerYear <- gtkEntry()
gtkEntrySetWidthChars(entryTimeSlicePerYear, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetText(entryTimeSlicePerYear, 12)
gtkEntrySetEditable(entryTimeSlicePerYear, FALSE)
tbl_Control$Attach(entryTimeSlicePerYear,i, i+1,j, j+1)
j=j+1    # row 4
entryYearsToBePreSimulated <- gtkEntry()
gtkEntrySetWidthChars(entryYearsToBePreSimulated, NUMERICAL_ENTRY_LENGTH) 
tbl_Control$Attach(entryYearsToBePreSimulated,i, i+1,  j, j+1)
j=j+1    # row 5
entryNumberRunSeedRandomization <- gtkEntry()
gtkEntrySetWidthChars(entryNumberRunSeedRandomization, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetText(entryNumberRunSeedRandomization, 100) 
gtkEntrySetEditable(entryNumberRunSeedRandomization, FALSE)
tbl_Control$Attach(entryNumberRunSeedRandomization,i, i+1,  j, j+1) 
j=j+1     # row 6
entryYearsForAverage <- gtkEntry() 
gtkEntrySetWidthChars(entryYearsForAverage, NUMERICAL_ENTRY_LENGTH)   
tbl_Control$Attach(entryYearsForAverage,i, i+1,  j, j+1) 

i=i+1    # column 2
 j=0     # row 1
 tbl_Control$Attach(gtkLabel("End year of simulation"),i, i+1,  j, j+1)    
j=j+1     # row 2
 tbl_Control$Attach(gtkLabel("End year of forecast"),i, i+1, j, j+1)
j=j+1     # row 3
j=j+1
tbl_Control$Attach(gtkLabel("[sugg. equal to 3 * Lifespan]"),i, i+1,  j, j+1) 

i=i+1    # column 3
 j=0
 entry_EndYear_simulation <- gtkEntry()
gtkEntrySetWidthChars(entry_EndYear_simulation, NUMERICAL_ENTRY_LENGTH)
 tbl_Control$Attach(entry_EndYear_simulation,i, i+1,  j, j+1)    
j=j+1
  entry_EndYear_forecast <- gtkEntry()
gtkEntrySetWidthChars(entry_EndYear_forecast, NUMERICAL_ENTRY_LENGTH)
 tbl_Control$Attach(entry_EndYear_forecast,i, i+1,  j, j+1)    


i=i+1    # column 4
 j=0     # row 1
  j=j+1     # row 2
btn_save_years <- gtkButton()
gtkButtonSetLabel(btn_save_years, "      Save years      ")
btn_save_years$AddCallback("clicked", change_startend_year)
tbl_Control$Attach(btn_save_years,i, i+1,  j, j+1)    


hboxControlParameters <- gtkHBox(homogeneous = FALSE, 5)
hboxControlParameters$packStart(tbl_Control, expand = FALSE, fill = TRUE, padding = 10)
vboxControlParameters$packStart(hboxControlParameters, expand = FALSE, fill = TRUE, padding = 10)



#hboxloadBIO <- gtkHBox(FALSE, 5)
#lbl_BIOfile <- gtkLabel("Load project configuration from .csv file")
#hboxloadBIO$packStart(lbl_BIOfile, expand = FALSE, fill = FALSE, padding = 5) 
#btn_browse_BIOfile <- gtkButton()
#gtkButtonSetLabel(btn_browse_BIOfile, "Browse...")
#btn_browse_BIOfile$AddCallback("clicked", select_file_BIO)
#hboxloadBIO$packStart(btn_browse_BIOfile, expand = FALSE, fill = FALSE, padding = 5)
#lbl_BIO_File <- gtkLabel("C:\\ ")
#gtkLabelSetMaxWidthChars(lbl_BIO_File ,60)
#hboxloadBIO$packStart(lbl_BIO_File, expand = FALSE, fill = FALSE, padding = 5)
#
#vboxGeneralData$packStart(hboxloadBIO, expand = FALSE, fill = FALSE,padding = 5)


hframeProjectProperties <- gtkHBox(homogeneous = FALSE, 5)
frameProjectProperties <- gtkFrame(" Project Properties ")   
frameProjectProperties$add(vboxProjectProperties)
hframeProjectProperties$packStart(frameProjectProperties, expand = TRUE, fill = TRUE, padding = 10)

vboxGeneralData$packStart(hframeProjectProperties, expand = FALSE, fill = FALSE, padding = 5)

hframeControlParameters <- gtkHBox(homogeneous = FALSE, 5)
frameControlParameters <- gtkFrame(" Control Parameters ")
frameControlParameters$add(vboxControlParameters)
hframeControlParameters$packStart(frameControlParameters, expand = TRUE, fill = TRUE, padding = 10)
vboxGeneralData$packStart(hframeControlParameters, expand = FALSE, fill = FALSE, padding = 10)
				
# ********************************* GENERAL DATA graphical elements - end




# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
if (IN_BEMTOOL) {
# additional code for BEMTOOL integration
gtkEntrySetText(entry_EndYear_simulation, as.numeric(years[length(years)]) )
gtkEntrySetEditable(entry_EndYear_simulation, FALSE)

gtkEntrySetText(entry_EndYear_forecast, as.numeric(years.forecast[length(years.forecast)]) )
gtkEntrySetEditable(entry_EndYear_forecast, FALSE)

gtkWidgetSetSensitive(btn_save_years, FALSE)
#
gtkEntrySetText(entry_StartYear_simulation, as.numeric(years[1]) )
gtkEntrySetEditable(entry_StartYear_simulation, FALSE)
#
gtkEntrySetText(entrySpecies, BMT_SPECIES[ALADYM_spe])
gtkEntrySetText(entryGSA, casestudy_name )
gtkEntrySetEditable(entrySpecies, FALSE)
gtkEntrySetEditable(entryGSA, FALSE)
#
ref_point_ald_bmt <- as.logical(cfg[rownames(cfg)==paste("casestudy.referencepoints.S", ALADYM_spe, "", sep=""), 1])   
gtkToggleButtonSetActive(chkReferencePoints, ref_point_ald_bmt )
gtkWidgetSetSensitive(chkReferencePoints, FALSE)
#
average_yrs <- as.numeric(as.character(cfg[rownames(cfg)==paste("casestudy.S", ALADYM_spe, ".AladymSimulation", sep=""), 2]) )  
gtkEntrySetText(entryYearsForAverage, average_yrs)
gtkEntrySetEditable(entryYearsForAverage, FALSE)
 }
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------