# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





vbox_Diagnosis_ImpactPressure <- gtkVBox(FALSE, 5) 

frame_diagnosis_totalyield <- gtkFrame(" Composition of the total catch ")  
frame_diagnosis_Fspecies <- gtkFrame(" Fishing mortality ")  

hbox_diagnosis_totalyield <- gtkHBox(homogeneous = FALSE, 5)        
hbox_diagnosis_Fspecies <- gtkHBox(homogeneous = FALSE, 5) 

vbox_diagnosis_totalyield <- gtkVBox(FALSE, 5)                             
vbox_diagnosis_Fspecies <- gtkVBox(FALSE, 5) 

hbox_diagnosis_totalyield$packStart(frame_diagnosis_totalyield, expand = T, fill = T, padding = 5)  
hbox_diagnosis_Fspecies$packStart(frame_diagnosis_Fspecies, expand = T, fill = T, padding = 5)  

vbox_diagnosis_totalyield$packStart(hbox_diagnosis_totalyield, expand = T, fill = T, padding = 5)  
vbox_diagnosis_Fspecies$packStart(hbox_diagnosis_Fspecies, expand = T, fill = T, padding = 5)  

#hbox_diagnosis_2frame_ip <- gtkHBox(homogeneous = FALSE, 5) 
#hbox_diagnosis_2frame_ip$packStart(vbox_diagnosis_totalyield, expand = T, fill = T, padding = 5)  
#hbox_diagnosis_2frame_ip$packStart(vbox_diagnosis_Fspecies, expand = T, fill = T, padding = 5)  


#vbox_Diagnosis_ImpactPressure$packStart(hbox_diagnosis_2frame_ip, expand = F, fill = F, padding = 10)  

# object
img_yield_path <-   paste(mat_cfg_general[1,3],  "/Diagnosis/Biological Pressure Impact/", mat_cfg_general[1,2], " - Catch by stock ALL.jpg", sep="") 
img_yield_pb <- gdkPixbufNewFromFileAtSize(img_yield_path, 350, 350)

yield_sw <<- gtkScrolledWindowNew(NULL, NULL)
yield_sw$setShadowType("etched-in")
yield_sw$setPolicy("automatic", "automatic")
yield_sw$SetUsize(350, 350)    

if (!is.null(img_yield_pb[[1]])) {
img_yield <- gtkImageNewFromPixbuf(img_yield_pb[[1]])
yield_sw$add(img_yield)
}

 
bmt_diagnosis_species_pressure <<- gtkComboBoxNewText()

Fmort_sw <<- gtkScrolledWindowNew(NULL, NULL)
Fmort_sw$setShadowType("etched-in")
Fmort_sw$setPolicy("automatic", "automatic")
Fmort_sw$SetUsize(350, 350)  

# frame_diagnosis_kobe
hbox_totalyield <- gtkHBox(homogeneous = FALSE, 5)   
vbox_totalyield <- gtkVBox(homogeneous = FALSE, 5) 
vbox_totalyield$packStart(yield_sw, expand = T, fill = T, padding = 10)
hbox_totalyield$packStart(vbox_totalyield, expand = T, fill = T, padding = 10)             
frame_diagnosis_totalyield$add(hbox_totalyield) 


# frame_diagnosis_indicator
hbox_Fspecies <- gtkHBox(homogeneous = FALSE, 5)   
vbox_Fspecies  <<- gtkVBox(homogeneous = FALSE, 5) 
hbox_Fspecies$packStart(vbox_Fspecies, expand = T, fill = T, padding = 10)             
frame_diagnosis_Fspecies$add(hbox_Fspecies) 


 
bmt_fishingmortality.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_fishingmortality.sw$setShadowType("etched-in")
bmt_fishingmortality.sw$setPolicy("automatic", "automatic")
bmt_fishingmortality.sw$SetUsize(450, 150)  
bmt_fishingmortality_list <<- list()
bmt_fishingmortalityIndex <<- 0


    for (choice in BMT_SPECIES) { 
    bmt_diagnosis_species_pressure$appendText(choice)   
    }   

lbl_Fishing_perc <<- gtkLabel(" Percentage F due to the single fleet segment ")
hbox_diagnosis_Fspecies_combo <<- gtkHBox(homogeneous = FALSE, 5)     
hbox_diagnosis_Fspecies_combo$packStart(gtkLabel(" Stock "), expand = F, fill = F, padding = 0)  
hbox_diagnosis_Fspecies_combo$packStart(bmt_diagnosis_species_pressure, expand = F, fill = F, padding = 0)   

vbox_Fspecies$packStart(hbox_diagnosis_Fspecies_combo, expand = T, fill = T, padding = 10) 
vbox_Fspecies$packStart(Fmort_sw , expand = T, fill = T, padding = 5)  
vbox_Fspecies$packStart(lbl_Fishing_perc, expand = F, fill = F, padding = 5) 
vbox_Fspecies$packStart(bmt_fishingmortality.sw ,  expand = T, fill = T, padding = 5)
 
gSignalConnect(bmt_diagnosis_species_pressure, "changed", show_species_pressure_indicator)
gtkComboBoxSetActive(bmt_diagnosis_species_pressure, 0 )  
 
# index_to_load = -1
#  selected <- gtkComboBoxGetActiveText(bmt_diagnosis_species_pressure)
#  index_to_update <- which(BMT_SPECIES == selected) 
  
bmt_fishingmortality.create_model()
bmt_fishingmortality.treeview <<- gtkTreeViewNewWithModel(bmt_fishingmortality.model)
bmt_fishingmortality.treeview$setRulesHint(TRUE)
bmt_fishingmortality.treeview$getSelection()$setMode("single")
bmt_fishingmortality.add_columns(bmt_fishingmortality.treeview)
bmt_fishingmortality.sw$add(bmt_fishingmortality.treeview) 


#hbox_diagnosis_2frame_ip <- gtkHBox(homogeneous = FALSE, 5) 
#hbox_diagnosis_2frame_ip$packStart(vbox_diagnosis_totalyield, expand = T, fill = T, padding = 5)  
#hbox_diagnosis_2frame_ip$packStart(vbox_diagnosis_Fspecies, expand = T, fill = T, padding = 5)  



 bmt_tbl_diagnosis_results_pressure <- gtkTable(2, 2, homogeneous = FALSE)
bmt_tbl_diagnosis_results_pressure$SetRowSpacings(10)
bmt_tbl_diagnosis_results_pressure$SetColSpacings(10)
bmt_tbl_diagnosis_results_pressure$SetBorderWidth(5)

i=0  # column 0 
j=0 
bmt_tbl_diagnosis_results_pressure$Attach(vbox_diagnosis_totalyield,i, i+1, j, j+1) 

i=i+1  # column 0 
j=0 
bmt_tbl_diagnosis_results_pressure$Attach(vbox_diagnosis_Fspecies,i, i+1, j, j+1) 


 vbox_Diagnosis_ImpactPressure$packStart(bmt_tbl_diagnosis_results_pressure, expand = F, fill = F, padding =0)  


#vbox_table_percentage  <- gtkVBox(homogeneous = FALSE, 5) 


#vbox_table_percentage$packStart(bmt_fishingmortality.sw, expand = F, fill = F, padding = 5)  

#hbox_table_percentage  <- gtkHBox(homogeneous = FALSE, 5) 
#hbox_table_percentage$packStart(vbox_table_percentage, expand = T, fill = T, padding = 5)  
#vbox_Diagnosis_ImpactPressure$packStart(hbox_table_percentage, expand = T, fill = F, padding = 5)  

#gtkBoxReorderChild(vbox_Diagnosis_ImpactPressure, hbox_table_percentage, 0)
#print("tab impact pressure")      



