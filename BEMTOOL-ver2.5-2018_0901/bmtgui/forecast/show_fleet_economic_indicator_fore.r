# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




show_fleet_economic_indicator_fore <-function(w) {
    
  index_to_load_fore = -1
  selected_fore <- gtkComboBoxGetActiveText(bmt_forecast_fleet_SF_combo)
  #print(paste("selected:",selected_fore))

  if (!is.null(selected_fore)) {
  index_to_update_fore <- which(BMT_FLEETSEGMENTS == selected_fore) 
  
     
#  name_this_scenario <-   paste("HR", as.character(mat_cfg_scenario_settings_fore[2,2]), "-", as.character(mat_cfg_scenario_settings_fore[2,3]), sep="")

# image 1 *********************************************************************     #GSA 18 - Revenues-Landing DTS_VL0006-VL1218 SIM.jpg
img_RL_path_fore <-   paste(mat_cfg_general[1,3],  "/", name_this_scenario  ,"/Economic indicators/", mat_cfg_general[1,2], " - Revenues-Landing ", selected_fore," ", name_this_scenario,".jpg", sep="") 
img_RL_pb_fore <- gdkPixbufNewFromFileAtSize(img_RL_path_fore, 300, 300)

if (!is.null(img_RL_pb_fore[[1]])) {
img_RL_sw_fore$destroy()
img_RL_sw_fore <<- gtkScrolledWindowNew(NULL, NULL)
img_RL_sw_fore$setShadowType("etched-in")
img_RL_sw_fore$setPolicy("automatic", "automatic")
img_RL_sw_fore$SetUsize(300, 300)  

img_RL_fore <- gtkImageNewFromPixbuf(img_RL_pb_fore[[1]])
img_RL_sw_fore$add(img_RL_fore)
vbox_image_RL_fore$packStart(img_RL_sw_fore,expand = F, fill = F, padding = 5)    
}

 
# image 2 *********************************************************************     #GSA 18 - Profit-Salary-Capital DTS_VL0006-VL1218 SIM.jpg
img_PS_path_fore <-   paste(mat_cfg_general[1,3], "/", name_this_scenario  ,"/Economic indicators/", mat_cfg_general[1,2], " - Profit-Salary-Capital ", selected_fore," ", name_this_scenario,".jpg", sep="") 
img_PS_pb_fore <- gdkPixbufNewFromFileAtSize(img_PS_path_fore, 300, 300)

if (!is.null(img_PS_pb_fore[[1]])) {
img_PS_sw_fore$destroy()
img_PS_sw_fore <<- gtkScrolledWindowNew(NULL, NULL)
img_PS_sw_fore$setShadowType("etched-in")
img_PS_sw_fore$setPolicy("automatic", "automatic")
img_PS_sw_fore$SetUsize(300, 300)  

img_PS_fore <- gtkImageNewFromPixbuf(img_PS_pb_fore[[1]])
img_PS_sw_fore$add(img_PS_fore)
vbox_image_PS_fore$packStart(img_PS_sw_fore,expand = F, fill = F, padding = 5)    
}

# image 3 *********************************************************************     #GSA 18 - Avg.salary-Employment PGP_VL0006-VL0612 SIM.jpg
img_SE_path_fore <-   paste(mat_cfg_general[1,3],  "/", name_this_scenario  ,"/Economic indicators/", mat_cfg_general[1,2], " - Avg.salary-Employment ", selected_fore," ", name_this_scenario,".jpg", sep="") 
img_SE_pb_fore <- gdkPixbufNewFromFileAtSize(img_SE_path_fore, 300, 300)

if (!is.null(img_SE_pb_fore[[1]])) {
img_SE_sw_fore$destroy()
img_SE_sw_fore <<- gtkScrolledWindowNew(NULL, NULL)
img_SE_sw_fore$setShadowType("etched-in")
img_SE_sw_fore$setPolicy("automatic", "automatic")
img_SE_sw_fore$SetUsize(300, 300)  

img_SE_fore <- gtkImageNewFromPixbuf(img_SE_pb_fore[[1]])
img_SE_sw_fore$add(img_SE_fore)
vbox_image_SE_fore$packStart(img_SE_sw_fore,expand = F, fill = F, padding = 5)    
}


# image 4 *********************************************************************     #GSA 18 - Net profit-R_BER DTS_VL2440 SIM.jpg
img_RB_path_fore <-   paste(mat_cfg_general[1,3], "/", name_this_scenario  ,"/Economic indicators/", mat_cfg_general[1,2], " - Net profit-R_BER ", selected_fore," ", name_this_scenario,".jpg", sep="") 
img_RB_pb_fore <- gdkPixbufNewFromFileAtSize(img_RB_path_fore, 300, 300)

if (!is.null(img_RB_pb_fore[[1]])) {
img_RB_sw_fore$destroy()
img_RB_sw_fore <<- gtkScrolledWindowNew(NULL, NULL)
img_RB_sw_fore$setShadowType("etched-in")
img_RB_sw_fore$setPolicy("automatic", "automatic")
img_RB_sw_fore$SetUsize(300, 300)  

img_RB_fore <- gtkImageNewFromPixbuf(img_RB_pb_fore[[1]])
img_RB_sw_fore$add(img_RB_fore)
vbox_image_RB_fore$packStart(img_RB_sw_fore,expand = F, fill = F, padding = 5)    
}




}

}
