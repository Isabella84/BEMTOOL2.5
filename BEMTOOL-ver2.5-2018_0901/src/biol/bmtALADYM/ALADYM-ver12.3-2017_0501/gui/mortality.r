# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




# ********************************* MORTALITY graphical elements

vboxMortality <- gtkVBox(homogeneous = FALSE, 5)

hboxFentry <- gtkHBox(FALSE, 5)

## Create a radio button with a GtkEntry widget 
radio_Zentry <- gtkRadioButton()
radio_Zentry$add(gtkLabel("Entry with Z"))

radio_Fentry <- gtkRadioButtonNewWithLabelFromWidget(radio_Zentry, "Entry with F")

gSignalConnect(radio_Zentry, "toggled", deactivate_F)
gSignalConnect(radio_Fentry, "toggled", deactivate_Z)
  
tbl_entryFZ <- gtkTable(1,3,homogeneous = FALSE)
tbl_entryFZ$SetRowSpacings(7)
tbl_entryFZ$SetColSpacings(30)
tbl_entryFZ$SetBorderWidth(5)

i=0
j=0   
tbl_entryFZ$Attach(radio_Fentry,i, i+1, j, j+1)  

i=i+1
j=0   
tbl_entryFZ$Attach(radio_Zentry,i, i+1, j, j+1)  


hboxFentry$packStart(gtkLabel("Select the entering mortality     "), expand = FALSE, fill = FALSE, padding = 10)
hboxFentry$packStart(tbl_entryFZ, FALSE, FALSE, 15) 
    
vboxMortality$PackStart(hboxFentry, expand = FALSE, fill = FALSE, padding = 5)



gtkToggleButtonGetActive(radio_Zentry)
vbox_naturalmortality <- gtkVBox(homogeneous = FALSE, 5) 
suppressWarnings(source(paste(ALADYM_home, "/gui/mortality/mortality.m.r", sep=""))  )
            
frame_naturalmortality <- gtkFrame(" Natural mortality ")   
hbox_naturalmortality <- gtkHBox(homogeneous = FALSE, 5)

hbox_naturalmortality$packStart(hboxNaturalMortality, expand = FALSE, fill = TRUE, padding = 10)
vbox_naturalmortality$packStart(hbox_naturalmortality, expand = FALSE, fill = TRUE, padding = 5)
frame_naturalmortality$add(vbox_naturalmortality) 

h_frame_naturalmortality <- gtkHBox(homogeneous = FALSE, 5)
h_frame_naturalmortality$packStart(frame_naturalmortality, expand = TRUE, fill = TRUE, padding = 10)		
vboxMortality$packStart(h_frame_naturalmortality, expand = FALSE, fill = FALSE, padding = 5)

vbox_totalmortality <- gtkVBox(homogeneous = FALSE, 5)      
suppressWarnings(source(paste(ALADYM_home, "/gui/mortality/mortality.z.r", sep="")) )
       
frame_totalmortality <- gtkFrame(" Total mortality ")   
hbox_totalmortality <- gtkHBox(homogeneous = FALSE, 5)

hbox_totalmortality$packStart(hboxTotalMortality, expand = FALSE, fill = TRUE, padding = 10)
vbox_totalmortality$packStart(hbox_totalmortality, expand = FALSE, fill = TRUE, padding = 5)
frame_totalmortality$add(vbox_totalmortality) 

h_frame_totalmortality <- gtkHBox(homogeneous = FALSE, 5)
h_frame_totalmortality$packStart(frame_totalmortality, expand = TRUE, fill = TRUE, padding = 10)		
vboxMortality$packStart(h_frame_totalmortality, expand = FALSE, fill = FALSE, padding = 5)


source(paste(ALADYM_home, "/gui/mortality/mortality.f.r", sep="")) 
vbox_fishingmortality <- gtkVBox(homogeneous = FALSE, 5)            
frame_fishingmortality <- gtkFrame(" Fishing mortality ")   
hbox_fishingmortality <- gtkHBox(homogeneous = FALSE, 5)

hbox_fishingmortality$packStart(vboxFishingMortality, expand = FALSE, fill = TRUE, padding = 10)
vbox_fishingmortality$packStart(hbox_fishingmortality, expand = FALSE, fill = TRUE, padding = 10)
frame_fishingmortality$add(vbox_fishingmortality) 

h_frame_fishingmortality <- gtkHBox(homogeneous = FALSE, 5)
h_frame_fishingmortality$packStart(frame_fishingmortality, expand = TRUE, fill = TRUE, padding = 10)		
vboxMortality$packStart(h_frame_fishingmortality, expand = FALSE, fill = FALSE, padding = 5)


vboxMonthlySurvivability <- gtkVBox(FALSE, 5)
suppressWarnings(suppressWarnings(source(paste(ALADYM_home, "/gui/mortality/mortality.monthlysurvivability.r", sep="")) )  )

frame_survivability <- gtkFrame(" Survivability rate ")   
hbox_survivability <- gtkHBox(homogeneous = FALSE, 5)
vbox_survivability <- gtkVBox(homogeneous = FALSE, 5)             

hbox_survivability$packStart(vboxMonthlySurvivability, expand = TRUE, fill = TRUE, padding = 10)
vbox_survivability$packStart(hbox_survivability, expand = TRUE, fill = TRUE, padding = 10)
frame_survivability$add(vbox_survivability) 

h_frame_survivability <- gtkHBox(homogeneous = FALSE, 5)
h_frame_survivability$packStart(frame_survivability, expand = TRUE, fill = TRUE, padding = 10)		

v_frame_survivability <- gtkVBox(homogeneous = FALSE, 5)
v_frame_survivability$packStart(h_frame_survivability, expand = TRUE, fill = T, padding = 0)

vboxMortality$packStart(v_frame_survivability, expand = FALSE, fill = FALSE, padding = 5)