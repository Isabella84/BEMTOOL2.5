# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



vbox_Diagnosis_StateFleets <- gtkVBox(FALSE, 5) 
hbox_diagnosis_SF <- gtkHBox(homogeneous = FALSE, 5)        

vbox_container_SF <- gtkVBox(FALSE, 5)                             


vbox_Diagnosis_StateFleets$packStart(vbox_container_SF, expand = F, fill = F, padding = 0)  


bmt_diagnosis_fleet_SF_combo <<- gtkComboBoxNewText()
        for (choice in BMT_FLEETSEGMENTS) { 
    bmt_diagnosis_fleet_SF_combo$appendText(choice)   
    } 
    


hbox_diagnosis_Fleet_SF_combo <<- gtkHBox(homogeneous = FALSE, 5)     
hbox_diagnosis_Fleet_SF_combo$packStart(gtkLabel(" Fleet Segment "), expand = F, fill = F, padding = 10)  
hbox_diagnosis_Fleet_SF_combo$packStart(bmt_diagnosis_fleet_SF_combo, expand = F, fill = F, padding = 10)   

vbox_container_SF$packStart(hbox_diagnosis_Fleet_SF_combo, expand = F, fill = F, padding = 5)   

index_to_load_fleetSF = -1
  selected_fleetSF <- gtkComboBoxGetActiveText(bmt_diagnosis_fleet_SF_combo)
  index_to_update_fleetSF <- which(BMT_FLEETSEGMENTS == selected_fleetSF) 



# image 1 *********************************************************************     #GSA 18 - Revenues-Landing DTS_VL0006-VL1218 SIM.jpg
img_RL_sw <<- gtkScrolledWindowNew(NULL, NULL)
img_RL_sw$setShadowType("etched-in")
img_RL_sw$setPolicy("automatic", "automatic")
img_RL_sw$SetUsize(300, 300)    

img_RL_path <-   paste(mat_cfg_general[1,3],  "/Diagnosis/Economic indicators/", mat_cfg_general[1,2], " - Revenues-Landing ", selected_fleetSF,".jpg", sep="") 
img_RL_pb <- gdkPixbufNewFromFileAtSize(img_RL_path, 300, 300)

if (!is.null(img_RL_pb[[1]])) {
img_RL <- gtkImageNewFromPixbuf(img_RL_pb[[1]])
img_RL_sw$add(img_RL)
}

# image 2 *********************************************************************     #GSA 18 - Profit-Salary-Capital DTS_VL0006-VL1218 SIM.jpg
img_PS_sw <<- gtkScrolledWindowNew(NULL, NULL)
img_PS_sw$setShadowType("etched-in")
img_PS_sw$setPolicy("automatic", "automatic")
img_PS_sw$SetUsize(300, 300)    

img_PS_path <-   paste(mat_cfg_general[1,3],  "/Diagnosis/Economic indicators/", mat_cfg_general[1,2], " - Profit-Salary-Capital ", selected_fleetSF,".jpg", sep="") 
img_PS_pb <- gdkPixbufNewFromFileAtSize(img_PS_path, 300, 300)

if (!is.null(img_PS_pb[[1]])) {
img_PS <- gtkImageNewFromPixbuf(img_PS_pb[[1]])
img_PS_sw$add(img_PS)
}


# image 3 *********************************************************************     #GSA 18 - Avg.salary-Employment PGP_VL0006-VL0612 SIM.jpg
img_SE_sw <<- gtkScrolledWindowNew(NULL, NULL)
img_SE_sw$setShadowType("etched-in")
img_SE_sw$setPolicy("automatic", "automatic")
img_SE_sw$SetUsize(300, 300)    

img_SE_path <-   paste(mat_cfg_general[1,3],  "/Diagnosis/Economic indicators/", mat_cfg_general[1,2], " - Avg.salary-Employment ", selected_fleetSF,".jpg", sep="") 
img_SE_pb <- gdkPixbufNewFromFileAtSize(img_SE_path, 300, 300)

if (!is.null(img_SE_pb[[1]])) {
img_SE <- gtkImageNewFromPixbuf(img_SE_pb[[1]])
img_SE_sw$add(img_SE)
}


# image 4 *********************************************************************     #GSA 18 - Net profit-R_BER DTS_VL2440 SIM.jpg
img_RB_sw <<- gtkScrolledWindowNew(NULL, NULL)
img_RB_sw$setShadowType("etched-in")
img_RB_sw$setPolicy("automatic", "automatic")
img_RB_sw$SetUsize(300, 300)    

img_RB_path <-   paste(mat_cfg_general[1,3],  "/Diagnosis/Economic indicators/", mat_cfg_general[1,2], " - Net profit-R_BER ", selected_fleetSF,".jpg", sep="") 
img_RB_pb <- gdkPixbufNewFromFileAtSize(img_RB_path, 300, 300)

if (!is.null(img_RB_pb[[1]])) {
img_RB <- gtkImageNewFromPixbuf(img_RB_pb[[1]])
img_RB_sw$add(img_RB)
}


bmt_tbl_diagnosis_SF <- gtkTable(4,2,homogeneous = FALSE)
bmt_tbl_diagnosis_SF$SetRowSpacings(0)
bmt_tbl_diagnosis_SF$SetColSpacings(20)
bmt_tbl_diagnosis_SF$SetBorderWidth(5)

i=0  # column 0 
j=0 
vbox_image_RL <- gtkVBox(FALSE, 5)                             
vbox_image_RL$packStart(gtkLabel("Total landings and Total revenues"), expand = F, fill = F, padding = 0)    
vbox_image_RL$packStart(img_RL_sw,expand = F, fill = F, padding = 0)    
bmt_tbl_diagnosis_SF$Attach(vbox_image_RL,i, i+1, j, j+1)

j=j+1
vbox_image_PS <- gtkVBox(FALSE, 5)                             
vbox_image_PS$packStart(gtkLabel("Profits, Salaries and Capital costs"), expand = F, fill = F, padding = 0)  
vbox_image_PS$packStart(img_PS_sw, expand = F, fill = F, padding = 0)  
bmt_tbl_diagnosis_SF$Attach(vbox_image_PS,i, i+1, j, j+1)

i=i+1  # column 1  
j=0 
vbox_image_SE <- gtkVBox(FALSE, 5)                             
vbox_image_SE$packStart(gtkLabel("Average salary and number of employees"), expand = F, fill = F, padding = 0)   
vbox_image_SE$packStart(img_SE_sw, expand = F, fill = F, padding = 0)   
bmt_tbl_diagnosis_SF$Attach(vbox_image_SE,i, i+1, j, j+1)
j=j+1
vbox_image_RB <- gtkVBox(FALSE, 5)                             
vbox_image_RB$packStart(gtkLabel("Indicator Revenues/Break Even Revenues and Net profits"), expand = F, fill = F, padding = 0)    
vbox_image_RB$packStart(img_RB_sw, expand = F, fill = F, padding = 0)    
bmt_tbl_diagnosis_SF$Attach(vbox_image_RB,i, i+1, j, j+1)


hbox_diagnosis_SF$packStart(bmt_tbl_diagnosis_SF, expand = T, fill = T, padding = 10)
   vbox_container_SF$packStart(hbox_diagnosis_SF, expand = F, fill = F, padding =0) 
   
   gSignalConnect(bmt_diagnosis_fleet_SF_combo, "changed", show_fleet_economic_indicator) 
     
         gtkComboBoxSetActive(bmt_diagnosis_fleet_SF_combo, 0 )    