# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




vbox_Forecast_StateFleets <- gtkVBox(FALSE, 5) 
hbox_forecast_SF <- gtkHBox(homogeneous = FALSE, 5)        

vbox_container_SF_fore <- gtkVBox(FALSE, 5)                             


vbox_Forecast_StateFleets$packStart(vbox_container_SF_fore, expand = F, fill = F, padding = 0)  


bmt_forecast_fleet_SF_combo <<- gtkComboBoxNewText()
        for (choice in BMT_FLEETSEGMENTS) { 
    bmt_forecast_fleet_SF_combo$appendText(choice)   
    } 
    
name_this_scenario <-  SCENARIO_TO_LOAD_FROM_MENU # paste("HR", as.character(mat_cfg_scenario_settings_fore[2,2]), "-", as.character(mat_cfg_scenario_settings_fore[2,3]), sep="")

hbox_forecast_Fleet_SF_combo <<- gtkHBox(homogeneous = FALSE, 5)     
hbox_forecast_Fleet_SF_combo$packStart(gtkLabel(" Fleet Segment "), expand = F, fill = F, padding = 10)  
hbox_forecast_Fleet_SF_combo$packStart(bmt_forecast_fleet_SF_combo, expand = F, fill = F, padding = 10)   

vbox_container_SF_fore$packStart(hbox_forecast_Fleet_SF_combo, expand = F, fill = F, padding = 5)   

index_to_load_fleetSF_fore = -1
  selected_fleetSF_fore <- gtkComboBoxGetActiveText(bmt_forecast_fleet_SF_combo)
  index_to_update_fleetSF_fore <- which(BMT_FLEETSEGMENTS == selected_fleetSF_fore) 



# image 1 *********************************************************************     #GSA 18 - Revenues-Landing DTS_VL0006-VL1218 SIM.jpg
img_RL_sw_fore <<- gtkScrolledWindowNew(NULL, NULL)
img_RL_sw_fore$setShadowType("etched-in")
img_RL_sw_fore$setPolicy("automatic", "automatic")
img_RL_sw_fore$SetUsize(300, 300)    

img_RL_path_fore <-   paste(mat_cfg_general[1,3],  "/",name_this_scenario,"/Economic indicators/", mat_cfg_general[1,2], " - Revenues-Landing ", selected_fleetSF_fore,".jpg", sep="") 
img_RL_pb_fore <- gdkPixbufNewFromFileAtSize(img_RL_path_fore, 300, 300)

if (!is.null(img_RL_pb_fore[[1]])) {
img_RL_fore <- gtkImageNewFromPixbuf(img_RL_pb_fore[[1]])
img_RL_sw_fore$add(img_RL_fore)
}

# image 2 *********************************************************************     #GSA 18 - Profit-Salary-Capital DTS_VL0006-VL1218 SIM.jpg
img_PS_sw_fore <<- gtkScrolledWindowNew(NULL, NULL)
img_PS_sw_fore$setShadowType("etched-in")
img_PS_sw_fore$setPolicy("automatic", "automatic")
img_PS_sw_fore$SetUsize(300, 300)    

img_PS_path_fore <-   paste(mat_cfg_general[1,3],  "/",name_this_scenario,"/Economic indicators/", mat_cfg_general[1,2], " - Profit-Salary-Capital ", selected_fleetSF_fore,".jpg", sep="") 
img_PS_pb_fore <- gdkPixbufNewFromFileAtSize(img_PS_path_fore, 300, 300)

if (!is.null(img_PS_pb_fore[[1]])) {
img_PS_fore <- gtkImageNewFromPixbuf(img_PS_pb_fore[[1]])
img_PS_sw_fore$add(img_PS_fore)
}


# image 3 *********************************************************************     #GSA 18 - Avg.salary-Employment PGP_VL0006-VL0612 SIM.jpg
img_SE_sw_fore <<- gtkScrolledWindowNew(NULL, NULL)
img_SE_sw_fore$setShadowType("etched-in")
img_SE_sw_fore$setPolicy("automatic", "automatic")
img_SE_sw_fore$SetUsize(300, 300)    

img_SE_path_fore <-   paste(mat_cfg_general[1,3],   "/",name_this_scenario,"/Economic indicators/", mat_cfg_general[1,2], " - Avg.salary-Employment ", selected_fleetSF_fore,".jpg", sep="") 
img_SE_pb_fore <- gdkPixbufNewFromFileAtSize(img_SE_path_fore, 300, 300)

if (!is.null(img_SE_pb_fore[[1]])) {
img_SE_fore <- gtkImageNewFromPixbuf(img_SE_pb_fore[[1]])
img_SE_sw_fore$add(img_SE_fore)
}


# image 4 *********************************************************************     #GSA 18 - Net profit-R_BER DTS_VL2440 SIM.jpg
img_RB_sw_fore <<- gtkScrolledWindowNew(NULL, NULL)
img_RB_sw_fore$setShadowType("etched-in")
img_RB_sw_fore$setPolicy("automatic", "automatic")
img_RB_sw_fore$SetUsize(300, 300)    

img_RB_path_fore <-   paste(mat_cfg_general[1,3],   "/",name_this_scenario,"/Economic indicators/", mat_cfg_general[1,2], " - Net profit-R_BER ", selected_fleetSF_fore,".jpg", sep="") 
img_RB_pb_fore <- gdkPixbufNewFromFileAtSize(img_RB_path_fore, 300, 300)

if (!is.null(img_RB_pb_fore[[1]])) {
img_RB_fore <- gtkImageNewFromPixbuf(img_RB_pb_fore[[1]])
img_RB_sw_fore$add(img_RB_fore)
}


bmt_tbl_forecast_SF <- gtkTable(4,2,homogeneous = FALSE)
bmt_tbl_forecast_SF$SetRowSpacings(3)
bmt_tbl_forecast_SF$SetColSpacings(30)
bmt_tbl_forecast_SF$SetBorderWidth(5)

i=0  # column 0 
j=0 
vbox_image_RL_fore <- gtkVBox(FALSE, 5)                             
vbox_image_RL_fore$packStart(gtkLabel("Total landings and Total revenues"), expand = F, fill = F, padding = 0)    
vbox_image_RL_fore$packStart(img_RL_sw_fore,expand = F, fill = F, padding = 0)    
bmt_tbl_forecast_SF$Attach(vbox_image_RL_fore,i, i+1, j, j+1)

j=j+1
vbox_image_PS_fore <- gtkVBox(FALSE, 5)                             
vbox_image_PS_fore$packStart(gtkLabel("Profits, Salaries and Capital costs"), expand = F, fill = F, padding = 0)  
vbox_image_PS_fore$packStart(img_PS_sw_fore, expand = F, fill = F, padding = 0)  
bmt_tbl_forecast_SF$Attach(vbox_image_PS_fore,i, i+1, j, j+1)

i=i+1  # column 1  
j=0 
vbox_image_SE_fore <- gtkVBox(FALSE, 5)                             
vbox_image_SE_fore$packStart(gtkLabel("Average salary and number of employees"), expand = F, fill = F, padding = 0)   
vbox_image_SE_fore$packStart(img_SE_sw_fore, expand = F, fill = F, padding = 0)   
bmt_tbl_forecast_SF$Attach(vbox_image_SE_fore,i, i+1, j, j+1)
j=j+1
vbox_image_RB_fore <- gtkVBox(FALSE, 5)                             
vbox_image_RB_fore$packStart(gtkLabel("Indicator Revenues/Break Even Revenues and Net profits"), expand = F, fill = F, padding = 0)    
vbox_image_RB_fore$packStart(img_RB_sw_fore, expand = F, fill = F, padding = 0)    
bmt_tbl_forecast_SF$Attach(vbox_image_RB_fore,i, i+1, j, j+1)


hbox_forecast_SF$packStart(bmt_tbl_forecast_SF, expand = T, fill = T, padding = 10)
   vbox_container_SF_fore$packStart(hbox_forecast_SF, expand = F, fill = F, padding =0) 
   
   gSignalConnect(bmt_forecast_fleet_SF_combo, "changed", show_fleet_economic_indicator_fore) 
     
         gtkComboBoxSetActive(bmt_forecast_fleet_SF_combo, 0 )    