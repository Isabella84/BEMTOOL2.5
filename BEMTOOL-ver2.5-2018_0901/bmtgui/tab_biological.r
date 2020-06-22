# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.






# initializating  objects
combo_species <- gtkComboBoxNewText()
gSignalConnect(combo_species, "changed", reload_species_info)
for (choice in BMT_SPECIES) { combo_species$appendText(choice) }

btn_bio_setSpecies_settings <<- gtkButton()                                    
gtkButtonSetLabel(btn_bio_setSpecies_settings, "   Apply changes   ")                    
btn_bio_setSpecies_settings$AddCallback("clicked", setBiological_species_settings)    

frame_casestudy_bioparams <- gtkFrame(" Life history traits ")  
frame_casestudy_ALADYM <- gtkFrame(" Biological simulation ")  
frame_casestudy_stockassessment <- gtkFrame(" Stock assessment ")   

hbox_casestudy_bioparams <- gtkHBox(homogeneous = FALSE, 5)        
hbox_casestudy_ALADYM <- gtkHBox(homogeneous = FALSE, 5)                  
hbox_casestudy_stockassessment <- gtkHBox(homogeneous = FALSE, 5)                   

vbox_casestudy_bioparams <- gtkVBox(FALSE, 5)                             
vbox_casestudy_ALADYM <- gtkVBox(FALSE, 5)                        
vbox_casestudy_stockassessment <- gtkVBox(FALSE, 5)                     

hbox_biol_bmt <- gtkHBox(homogeneous = FALSE, 5)   
vbox_biol_bmt <-  gtkVBox(FALSE, 5) 

vbox_BiologicalAssessment_Sim <- gtkVBox(FALSE, 5)

hbox_casestudy_bioparams$packStart(tbl_CS_generalsettings, expand = FALSE, fill = FALSE, padding = 10) 
hbox_casestudy_ALADYM$packStart(tbl_CS_path, expand = FALSE, fill = FALSE, padding = 10) 
hbox_casestudy_stockassessment$packStart(tbl_CS_name, expand = FALSE, fill = FALSE, padding = 10) 

vbox_casestudy_bioparams$packStart(hbox_casestudy_bioparams, expand = FALSE, fill = FALSE, padding = 5)  
vbox_casestudy_ALADYM$packStart(hbox_casestudy_ALADYM, expand = FALSE, fill = FALSE, padding = 5)
vbox_casestudy_stockassessment$packStart(hbox_casestudy_stockassessment, expand = T, fill = FALSE, padding = 5)

frame_casestudy_bioparams$add(vbox_casestudy_bioparams) 
frame_casestudy_ALADYM$add(vbox_casestudy_ALADYM) 
frame_casestudy_stockassessment$add(vbox_casestudy_stockassessment) 

hbox_biol_bmt_2frames <- gtkHBox(homogeneous = T, 5)   
hbox_biol_bmt_2frames$packStart(frame_casestudy_bioparams, expand = T, fill = T, padding = 5) 
hbox_biol_bmt_2frames$packStart(frame_casestudy_ALADYM, expand = T, fill = T, padding = 5) 

vbox_biol_bmt$packStart(hbox_biol_bmt_2frames, expand = FALSE, fill = FALSE, padding = 0) 
#vbox_biol_bmt$packStart(frame_casestudy_ALADYM, expand = FALSE, fill = FALSE, padding = 5) 
hbox_biol_bmt$packStart(frame_casestudy_stockassessment, expand = T, fill = T, padding = 5) 

vbox_biol_bmt$packStart(hbox_biol_bmt, expand = FALSE, fill = FALSE, padding = 5) 

hbox_BIO_species <- gtkHBox(homogeneous = FALSE, 5)   
vbox_BIO_species <-  gtkVBox(FALSE, 5) 

hbox_BIO_species$packStart(gtkLabel("Select a species    "), expand = FALSE, fill = F, padding = 10)         #true
hbox_BIO_species$packStart(combo_species, expand = FALSE, fill = F, padding = 10)         #true
hbox_BIO_species$packStart(btn_bio_setSpecies_settings, expand = FALSE, fill = F, padding = 10)         #true

vbox_BIO_species$packStart(hbox_BIO_species, expand = FALSE, fill = F, padding = 0)         #true

vbox_BiologicalAssessment_Sim$packStart(vbox_BIO_species, expand = FALSE, fill = F, padding = 5)         #true
vbox_BiologicalAssessment_Sim$packStart(vbox_biol_bmt, expand = FALSE, fill = T, padding = 0)         #true


# ********************************* BIOLOGICAL graphical elements

vbox_biosettings <- gtkVBox(homogeneous = FALSE, 5)
source(paste(getwd(), "/bmtgui/biological/bmt.biological.params.r", sep=""))
hbox_casestudy_bioparams$packStart(vbox_biosettings, expand = FALSE, fill = FALSE, padding = 5)

vbox_biosettings_simu <- gtkVBox(homogeneous = FALSE, 5)
source(paste(getwd(), "/bmtgui/biological/bmt.aladym.params.r", sep=""))
hbox_casestudy_ALADYM$packStart(vbox_biosettings_simu, expand = FALSE, fill = FALSE, padding = 5)

vbox_BiologicalAssessment_Sim$packStart(hbox_casestudy_bioparams, expand = FALSE, fill = TRUE, padding = 10)         #true
vbox_BiologicalAssessment_Sim$packStart(hbox_casestudy_ALADYM, expand = FALSE, fill = TRUE, padding = 10)         #true

vbox_bio_assessment <- gtkVBox(homogeneous = FALSE, 5)
suppressWarnings(source(paste(getwd(), "/bmtgui/biological/bmt.biological.assessment.r", sep="")))		
hbox_casestudy_stockassessment$packStart(vbox_bio_assessment, expand = T, fill = T, padding = 5)

vbox_BiologicalAssessment_Sim$packStart(hbox_casestudy_stockassessment, expand = FALSE, fill = TRUE, padding = 10)         #true
#vbox_CS$packStart(, expand = FALSE, fill = TRUE, padding = 10) 

#  vbox_casestudy$packStart(interaction.sw, expand = FALSE, fill = TRUE, padding = 10)     
