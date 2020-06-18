# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




show_species_bio_indicator_fore<-function(w) {
    
  index_to_load_fore = -1
  selected_fore <- gtkComboBoxGetActiveText(bmt_forecast_species)
  
  index_to_update_fore <- which(BMT_SPECIES == selected_fore)    
  
      
  index_to_load_indic_fore = -1
  selected_indic_fore <- gtkComboBoxGetActiveText(bmt_forecast_species_bio_indicator)
  
  index_to_update_indic_fore <- which(BIOINDICATORS == selected_indic_fore)   
 
# name_this_scenario <-   paste("HR", as.character(mat_cfg_scenario_settings_fore[2,2]), "-", as.character(mat_cfg_scenario_settings_fore[2,3]), sep="")
   
 bio_indicator_path_fore <- paste(mat_cfg_general[1,3],  "/", name_this_scenario  ,"/Biological Pressure Impact/", mat_cfg_general[1,2], " - ", selected_indic_fore," - ", selected_fore, " ",name_this_scenario ,".jpg", sep="")   
img_bio_indicator_pb_fore <<- gdkPixbufNewFromFileAtSize(bio_indicator_path_fore, 400, 400)

if (!is.null(img_bio_indicator_pb_fore[[1]])) {
bi_sw_fore$destroy()
bi_sw_fore <<- gtkScrolledWindowNew(NULL, NULL)
bi_sw_fore$setShadowType("etched-in")
bi_sw_fore$setPolicy("automatic", "automatic")
bi_sw_fore$SetUsize(400, 400)  

img_bio_indicator_fore <<- gtkImageNewFromPixbuf(img_bio_indicator_pb_fore[[1]])
bi_sw_fore$add(img_bio_indicator_fore)
vbox_indic_fore$packStart(bi_sw_fore, expand = T, fill = T, padding = 10)  
}


}
