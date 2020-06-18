# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




show_species_pressure_indicator_fore <-function(w) {
    
  index_to_load_fore = -1
  selected_fore <- gtkComboBoxGetActiveText(bmt_forecast_species_pressure)
  #print(paste("selected:",selected_fore))

  if (!is.null(selected_fore)) {
  index_to_update_fore <- which(BMT_SPECIES == selected_fore)    
 
#  name_this_scenario <-   paste("HR", as.character(mat_cfg_scenario_settings_fore[2,2]), "-", as.character(mat_cfg_scenario_settings_fore[2,3]), sep="")
  
 pressure_indicator_path_fore <- paste(mat_cfg_general[1,3],  "/", name_this_scenario ,"/Biological Pressure Impact/", mat_cfg_general[1,2], " - Fishing Mortality ", selected_fore," ", name_this_scenario,".jpg", sep="")   
img_pressure_indicator_pb_fore <- gdkPixbufNewFromFileAtSize(pressure_indicator_path_fore, 350, 350)

if (!is.null(img_pressure_indicator_pb_fore[[1]])) {
Fmort_sw_fore$destroy()
Fmort_sw_fore <<- gtkScrolledWindowNew(NULL, NULL)
Fmort_sw_fore$setShadowType("etched-in")
Fmort_sw_fore$setPolicy("automatic", "automatic")
Fmort_sw_fore$SetUsize(350, 350)  

img_pressure_indicator_fore <- gtkImageNewFromPixbuf(img_pressure_indicator_pb_fore[[1]])   
Fmort_sw_fore$add(img_pressure_indicator_fore)
vbox_Fspecies_fore$packStart(Fmort_sw_fore, expand = T, fill = T, padding = 10) 
#gtkBoxReorderChild(vbox_Fspecies, hbox_diagnosis_Fspecies_combo, 1)
#gtkBoxReorderChild(vbox_Fspecies, hbox_diagnosis_Fspecies_combo, 1)
#gtkBoxReorderChild(vbox_Fspecies, Fmort_sw, 2)
#gtkBoxReorderChild(vbox_Fspecies, lbl_Fishing_perc, 3)
#gtkBoxReorderChild(vbox_Fspecies, bmt_fishingmortality.sw , 4)

}

# # --------------------------- stock reduction table

}

}
