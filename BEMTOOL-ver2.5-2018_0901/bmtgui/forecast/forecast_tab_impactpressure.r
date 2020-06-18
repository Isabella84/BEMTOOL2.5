# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.






frame_forecast_totalyield <- gtkFrame(" Composition of the total catch ")  
frame_forecast_Fspecies <- gtkFrame(" Fishing mortality ")  

hbox_forecast_totalyield <- gtkHBox(homogeneous = FALSE, 5)        
hbox_forecast_Fspecies <- gtkHBox(homogeneous = FALSE, 5) 

vbox_forecast_totalyield <- gtkVBox(FALSE, 5)                             
vbox_forecast_Fspecies <- gtkVBox(FALSE, 5) 

hbox_forecast_totalyield$packStart(frame_forecast_totalyield, expand = T, fill = T, padding = 5)  
hbox_forecast_Fspecies$packStart(frame_forecast_Fspecies, expand = T, fill = T, padding = 5)  

vbox_forecast_totalyield$packStart(hbox_forecast_totalyield, expand = T, fill = T, padding = 5)  
vbox_forecast_Fspecies$packStart(hbox_forecast_Fspecies, expand = T, fill = T, padding = 5)  

#hbox_forecast_2frame_ip <- gtkHBox(homogeneous = FALSE, 5) 
#hbox_forecast_2frame_ip$packStart(vbox_forecast_totalyield, expand = T, fill = T, padding = 5)  
#hbox_forecast_2frame_ip$packStart(vbox_forecast_Fspecies, expand = T, fill = T, padding = 5) 


 bmt_tbl_forecast_results_pressure <- gtkTable(2, 2, homogeneous = FALSE)
bmt_tbl_forecast_results_pressure$SetRowSpacings(10)
bmt_tbl_forecast_results_pressure$SetColSpacings(10)
bmt_tbl_forecast_results_pressure$SetBorderWidth(5)

i=0  # column 0 
j=0 
bmt_tbl_forecast_results_pressure$Attach(vbox_forecast_totalyield,i, i+1, j, j+1) 

i=i+1  # column 0 
j=0 
bmt_tbl_forecast_results_pressure$Attach(vbox_forecast_Fspecies,i, i+1, j, j+1) 


 

vbox_Forecast_ImpactPressure <- gtkVBox(FALSE, 5) 
vbox_Forecast_ImpactPressure$packStart(bmt_tbl_forecast_results_pressure, expand = F, fill = F, padding = 10)  

 # name_this_scenario <-   paste("HR", as.character(mat_cfg_scenario_settings_fore[2,2]), "-", as.character(mat_cfg_scenario_settings_fore[2,3]), sep="")

  name_this_scenario <-  SCENARIO_TO_LOAD_FROM_MENU # paste("HR", as.character(mat_cfg_scenario_settings_fore[2,2]), "-", as.character(mat_cfg_scenario_settings_fore[2,3]), sep="")

img_yield_path_fore <-   paste(mat_cfg_general[1,3],  "/", name_this_scenario  ,"/Biological Pressure Impact/", mat_cfg_general[1,2], " - Catch by stock ALL ", name_this_scenario,".jpg", sep="") 
img_yield_pb_fore <- gdkPixbufNewFromFileAtSize(img_yield_path_fore, 350, 350)

yield_sw_fore <<- gtkScrolledWindowNew(NULL, NULL)
yield_sw_fore$setShadowType("etched-in")
yield_sw_fore$setPolicy("automatic", "automatic")
yield_sw_fore$SetUsize(350, 350)    

if (!is.null(img_yield_pb_fore[[1]])) {
img_yield_fore <- gtkImageNewFromPixbuf(img_yield_pb_fore[[1]])
yield_sw_fore$add(img_yield_fore)
}

 
bmt_forecast_species_pressure <<- gtkComboBoxNewText()

Fmort_sw_fore <<- gtkScrolledWindowNew(NULL, NULL)
Fmort_sw_fore$setShadowType("etched-in")
Fmort_sw_fore$setPolicy("automatic", "automatic")
Fmort_sw_fore$SetUsize(350, 350)  

# frame_forecast_kobe
hbox_totalyield_fore <- gtkHBox(homogeneous = FALSE, 5)   
vbox_totalyield_fore <- gtkVBox(homogeneous = FALSE, 5) 
vbox_totalyield_fore$packStart(yield_sw_fore, expand = T, fill = T, padding = 10)
hbox_totalyield_fore$packStart(vbox_totalyield_fore, expand = T, fill = T, padding = 10)             
frame_forecast_totalyield$add(hbox_totalyield_fore) 


# frame_forecast_indicator
hbox_Fspecies_fore <- gtkHBox(homogeneous = FALSE, 5)   
vbox_Fspecies_fore  <<- gtkVBox(homogeneous = FALSE, 5) 
hbox_Fspecies_fore$packStart(vbox_Fspecies_fore, expand = T, fill = T, padding = 10)             
frame_forecast_Fspecies$add(hbox_Fspecies_fore) 


    for (choice in BMT_SPECIES) { 
    bmt_forecast_species_pressure$appendText(choice)   
    }   

hbox_forecast_Fspecies_combo <<- gtkHBox(homogeneous = FALSE, 5)     
hbox_forecast_Fspecies_combo$packStart(gtkLabel(" Stock "), expand = F, fill = F, padding = 0)  
hbox_forecast_Fspecies_combo$packStart(bmt_forecast_species_pressure, expand = F, fill = F, padding = 0)   

vbox_Fspecies_fore$packStart(hbox_forecast_Fspecies_combo, expand = T, fill = T, padding = 5) 
vbox_Fspecies_fore$packStart(Fmort_sw_fore , expand = T, fill = T, padding = 5)  

gSignalConnect(bmt_forecast_species_pressure, "changed", show_species_pressure_indicator_fore)
gtkComboBoxSetActive(bmt_forecast_species_pressure, 0 )  
 
# index_to_load = -1
#  selected <- gtkComboBoxGetActiveText(bmt_forecast_species_pressure)
#  index_to_update <- which(BMT_SPECIES == selected) 
  




#vbox_table_percentage  <- gtkVBox(homogeneous = FALSE, 5) 


#vbox_table_percentage$packStart(bmt_fishingmortality.sw, expand = F, fill = F, padding = 5)  

#hbox_table_percentage  <- gtkHBox(homogeneous = FALSE, 5) 
#hbox_table_percentage$packStart(vbox_table_percentage, expand = T, fill = T, padding = 5)  
#vbox_Diagnosis_ImpactPressure$packStart(hbox_table_percentage, expand = T, fill = F, padding = 5)  

#gtkBoxReorderChild(vbox_Diagnosis_ImpactPressure, hbox_table_percentage, 0)
#print("tab impact pressure")      



