# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





  bmt_chk_assessment_sex_VIT <- gtkCheckButton("Analysis by sex")
  bmt_chk_assessment_discard_VIT <- gtkCheckButton("Analysis with discard")

    gSignalConnect(bmt_chk_assessment_sex_VIT, "toggled", activate_MalesFemalesVIT_items)

  entry_assessment_minAge_VIT <- gtkEntry()
  gtkEntrySetWidthChars(entry_assessment_minAge_VIT, NUMERICAL_ENTRY_LENGTH) 
  
    entry_assessment_maxAge_VIT <- gtkEntry()
  gtkEntrySetWidthChars(entry_assessment_maxAge_VIT, NUMERICAL_ENTRY_LENGTH)

#btn_remove_interaction <- gtkButton()
#gtkButtonSetLabel(btn_remove_interaction, "      Remove      ")
#btn_remove_interaction$AddCallback("clicked", removeInteractions)
#tbl_CS$Attach(btn_remove_interaction,i+1, i+2, j, j+1)


#hboxCaseStudy <- gtkHBox(homogeneous = FALSE, 5)
#hboxCaseStudy$packStart(tbl_assessment_VIT, expand = FALSE, fill = TRUE, padding = 10)
#


hbox_assessment_VIT <- gtkHBox(homogeneous = FALSE, 5)    

hbox_assessment_VIT_sex_disc <- gtkHBox(homogeneous = FALSE, 5)    
#hbox_assessment_VIT_disc <- gtkHBox(homogeneous = FALSE, 5)          

vbox_assessment_VIT <- gtkVBox(FALSE, 5)     
vbox_assessment_VIT_outer <- gtkVBox(FALSE, 5)                  


hbox_assessment_VIT_sex_disc$packStart(bmt_chk_assessment_sex_VIT, expand = F, fill = F, padding = 10)
 hbox_assessment_VIT_sex_disc$packStart(bmt_chk_assessment_discard_VIT, expand = F, fill = F, padding = 10)
   hbox_assessment_VIT_sex_disc$packStart( gtkLabel("                      "), expand = F, fill = F, padding = 10)                   
  hbox_assessment_VIT_sex_disc$packStart( gtkLabel("      Range for F calculation        "), expand = F, fill = F, padding = 10)                   
 hbox_assessment_VIT_sex_disc$packStart( gtkLabel("Min age"), expand = F, fill = F, padding = 5)                
  hbox_assessment_VIT_sex_disc$packStart( entry_assessment_minAge_VIT, expand = F, fill = F, padding = 5)                
   hbox_assessment_VIT_sex_disc$packStart(   gtkLabel("Max age"), expand = F, fill = F, padding = 5)                
    hbox_assessment_VIT_sex_disc$packStart( entry_assessment_maxAge_VIT, expand = F, fill = F, padding = 5)                
  
 vbox_assessment_VIT$packStart(hbox_assessment_VIT_sex_disc, expand = T, fill = T, padding = 0)
# vbox_assessment_VIT$packStart(hbox_assessment_VIT_disc, expand = T, fill = T, padding = 0)
 
#hbox_assessment_VIT$packStart(tbl_assessment_VIT, expand = FALSE, fill = FALSE, padding = 10) 
#
#
#vbox_assessment_VIT$packStart(hbox_assessment_VIT, expand = T, fill = T, padding = 5)
#


VITpaths_males.sw <<- gtkScrolledWindowNew(NULL, NULL)
VITpaths_males.sw$setShadowType("etched-in")
VITpaths_males.sw$setPolicy("automatic", "automatic")
VITpaths_males.sw$SetUsize(250, 90)  
VITpaths_males_list <<- list()
VITpaths_malesIndex <<- 0
# ------------------------------
# create model
VITpaths_males.create_model()
# create tree view
VITpaths_males.treeview <<- gtkTreeViewNewWithModel(VITpaths_males.model)
VITpaths_males.treeview$setRulesHint(TRUE)
VITpaths_males.treeview$getSelection()$setMode("single")
VITpaths_males.add_columns(VITpaths_males.treeview)
VITpaths_males.sw$add(VITpaths_males.treeview) 



VITpaths_females.sw <<- gtkScrolledWindowNew(NULL, NULL)
VITpaths_females.sw$setShadowType("etched-in")
VITpaths_females.sw$setPolicy("automatic", "automatic")
VITpaths_females.sw$SetUsize(250, 90)  
VITpaths_females_list <<- list()
VITpaths_femalesIndex <<- 0
# ------------------------------
# create model
VITpaths_females.create_model()
# create tree view
VITpaths_females.treeview <<- gtkTreeViewNewWithModel(VITpaths_females.model)
VITpaths_females.treeview$setRulesHint(TRUE)
VITpaths_females.treeview$getSelection()$setMode("single")
VITpaths_females.add_columns(VITpaths_females.treeview)
VITpaths_females.sw$add(VITpaths_females.treeview) 

VITpaths_combined.sw <<- gtkScrolledWindowNew(NULL, NULL)
VITpaths_combined.sw$setShadowType("etched-in")
VITpaths_combined.sw$setPolicy("automatic", "automatic")
VITpaths_combined.sw$SetUsize(250, 90)  
VITpaths_combined_list <<- list()
VITpaths_combinedIndex <<- 0
# ------------------------------
# create model
VITpaths_combined.create_model()
# create tree view
VITpaths_combined.treeview <<- gtkTreeViewNewWithModel(VITpaths_combined.model)
VITpaths_combined.treeview$setRulesHint(TRUE)
VITpaths_combined.treeview$getSelection()$setMode("single")
VITpaths_combined.add_columns(VITpaths_combined.treeview)
VITpaths_combined.sw$add(VITpaths_combined.treeview) 



 tbl_assessment_VIT_files <- gtkTable(4,3,homogeneous = FALSE)
 tbl_assessment_VIT_files$SetRowSpacings(5)
 tbl_assessment_VIT_files$SetColSpacings(10)
 tbl_assessment_VIT_files$SetBorderWidth(5)

 
 lbl_CombinedVIT <- gtkLabel("COMBINED")
  lbl_MalesVIT <- gtkLabel("MALES")
   lbl_FemalesVIT <- gtkLabel("FEMALES")
  
 
  i = 0   # column 1 
  j=0 
tbl_assessment_VIT_files$Attach(lbl_CombinedVIT,i, i+1, j, j+1)
j=j+1
tbl_assessment_VIT_files$Attach(VITpaths_combined.sw,i, i+1, j, j+1)
  
   i = i+1  # column 2
  j=0
tbl_assessment_VIT_files$Attach(lbl_MalesVIT,i, i+1, j, j+1)
j=j+1
tbl_assessment_VIT_files$Attach(VITpaths_males.sw,i, i+1, j, j+1)

i = i+1  # column 3
  j=0
tbl_assessment_VIT_files$Attach(lbl_FemalesVIT,i, i+1, j, j+1)
j=j+1
tbl_assessment_VIT_files$Attach(VITpaths_females.sw,i, i+1, j, j+1)

 vbox_assessment_VIT_frame_paths <- gtkVBox(FALSE, 5)
 

hbox_assessment_set_options_file <- gtkHBox(homogeneous = FALSE, 5)    
 
  combo_years_VIT <- gtkComboBoxNewText()

hbox_assessment_set_options_file$packStart(gtkLabel("Set the results year"), expand = F, fill = F, padding = 10) 
hbox_assessment_set_options_file$packStart(combo_years_VIT, expand = F, fill = F, padding = 10) 


radio_combined <- gtkRadioButton()
radio_combined$add(gtkLabel("Combined"))

radio_males <- gtkRadioButtonNewWithLabelFromWidget(radio_combined, "Males")
radio_females <- gtkRadioButtonNewWithLabelFromWidget(radio_combined, "Females")

hbox_assessment_set_options_file$packStart(radio_combined, expand = F, fill = F, padding = 10) 
hbox_assessment_set_options_file$packStart(radio_males, expand = F, fill = F, padding = 10) 
hbox_assessment_set_options_file$packStart(radio_females, expand = F , fill = F, padding = 10) 

  btn_browse_VITresults_path <- gtkButton()
gtkButtonSetLabel(btn_browse_VITresults_path, "Browse...")
btn_browse_VITresults_path$AddCallback("clicked", assign_bio_assess_VIT_path)


      entry_assessment_VITresults <-  gtkEntry()
       gtkEntrySetWidthChars(entry_assessment_VITresults, 55) 
       gtkEntrySetEditable(entry_assessment_VITresults, F)

        hbox_assessment_set_options_file$packStart(btn_browse_VITresults_path, expand = F , fill = F, padding = 10) 
        hbox_assessment_set_options_file$packStart(entry_assessment_VITresults, expand = F , fill = F, padding = 10) 

vbox_assessment_VIT_frame_paths$packStart(hbox_assessment_set_options_file, expand = FALSE, fill = FALSE, padding = 0)      
vbox_assessment_VIT_frame_paths$packStart(tbl_assessment_VIT_files, expand = T, fill = FALSE, padding = 5) 



   
# vbox_assessment_VIT$packStart(VITpaths.sw, expand = FALSE, fill = FALSE, padding = 10) 


#hbox_two$packStart(tbl_CS_next, expand = TRUE, fill = TRUE, padding = 10)           


frame_assessment_VIT <- gtkFrame(" VIT results files ")   
frame_assessment_VIT$add(vbox_assessment_VIT_frame_paths) 

hbox_assessment_VIT_frame_paths <- gtkHBox(homogeneous = FALSE, 5)
hbox_assessment_VIT_frame_paths$packStart(frame_assessment_VIT, expand = T, fill = FALSE, padding = 10)    
 
vbox_assessment_VIT_outer$packStart(vbox_assessment_VIT, expand = T, fill = T, padding = 5) 
vbox_assessment_VIT_outer$packStart(hbox_assessment_VIT_frame_paths, expand = T, fill = T, padding = 5)

          gtkWidgetSetSensitive(radio_males, F) 
           gtkWidgetSetSensitive(radio_females, F) 
          gtkWidgetSetSensitive( VITpaths_females.sw, F) 
        gtkWidgetSetSensitive( VITpaths_males.sw, F) 
        gtkWidgetSetSensitive( VITpaths_combined.sw, T)  

