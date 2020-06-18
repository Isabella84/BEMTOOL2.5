# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





frame_forecast_kobe <- gtkFrame(" General state of the stock ")  
frame_forecast_indicator <- gtkFrame(" Time series of State indicators ")  

hbox_forecast_kobe <- gtkHBox(homogeneous = FALSE, 5)        
hbox_forecast_indicator <- gtkHBox(homogeneous = FALSE, 5) 

vbox_forecast_kobe <- gtkVBox(FALSE, 5)                             
vbox_forecast_indicator <- gtkVBox(FALSE, 5) 

hbox_forecast_kobe$packStart(frame_forecast_kobe, expand = T, fill = T, padding = 10)  
hbox_forecast_indicator$packStart(frame_forecast_indicator, expand = T, fill = T, padding = 10)  

vbox_forecast_kobe$packStart(hbox_forecast_kobe, expand = T, fill = T, padding = 5)  
vbox_forecast_indicator$packStart(hbox_forecast_indicator, expand = T, fill = T, padding = 5)  

#hbox_forecast_2frame <- gtkHBox(homogeneous = FALSE, 5) 
#hbox_forecast_2frame$packStart(vbox_forecast_kobe, expand = T, fill = T, padding = 0)  
#hbox_forecast_2frame$packStart(vbox_forecast_indicator, expand = T, fill = T, padding = 0)  

bmt_tbl_forecast_results <- gtkTable(2, 2, homogeneous = FALSE)
bmt_tbl_forecast_results$SetRowSpacings(10)
bmt_tbl_forecast_results$SetColSpacings(10)
bmt_tbl_forecast_results$SetBorderWidth(5)

i=0  # column 0 
j=0 
bmt_tbl_forecast_results$Attach(vbox_forecast_kobe,i, i+1, j, j+1) 
i=i+1 
bmt_tbl_forecast_results$Attach(vbox_forecast_indicator,i, i+2, j, j+1)


vbox_Forecast_StateStocks <<- gtkVBox(FALSE, 5) 
vbox_Forecast_StateStocks$packStart(bmt_tbl_forecast_results, expand = F, fill = F, padding = 5)  

name_this_scenario <-  SCENARIO_TO_LOAD_FROM_MENU # paste("HR", as.character(mat_cfg_scenario_settings_fore[2,2]), "-", as.character(mat_cfg_scenario_settings_fore[2,3]), sep="")

# object
img_kobe_path_fore <-   paste(mat_cfg_general[1,3],  "/", name_this_scenario  ,"/", mat_cfg_general[1,2], " - KOBE plot ",name_this_scenario,".jpg", sep="")
img_KobePlot_pb_fore <- gdkPixbufNewFromFileAtSize(img_kobe_path_fore, 400, 400)

kb_sw_fore <<- gtkScrolledWindowNew(NULL, NULL)
kb_sw_fore$setShadowType("etched-in")
kb_sw_fore$setPolicy("automatic", "automatic")
kb_sw_fore$SetUsize(400, 400)    

if (!is.null(img_KobePlot_pb_fore[[1]])) {
img_KobePlot_fore <- gtkImageNewFromPixbuf(img_KobePlot_pb_fore[[1]])
kb_sw_fore$add(img_KobePlot_fore)
}  else {
img_kobe_path_fore <-   paste(mat_cfg_general[1,3],  "/", name_this_scenario  ,"/", mat_cfg_general[1,2], " - RISK EVALUATION plot ",name_this_scenario,".jpg", sep="")
img_KobePlot_pb_fore <- gdkPixbufNewFromFileAtSize(img_kobe_path_fore, 400, 400)
 if (!is.null(img_KobePlot_pb_fore[[1]])) {
img_KobePlot_fore <- gtkImageNewFromPixbuf(img_KobePlot_pb_fore[[1]])
kb_sw_fore$add(img_KobePlot_fore)
} 
}

 
bmt_forecast_species <<- gtkComboBoxNewText()

 for (choice in BMT_SPECIES) { 
    bmt_forecast_species$appendText(choice)    
    }

gtkComboBoxSetActive(bmt_forecast_species, 0)

bmt_forecast_species_bio_indicator <<- gtkComboBoxNewText()


bioindicator_path_fore <-   paste(mat_cfg_general[1,3],  "/", name_this_scenario  ,"/", mat_cfg_general[1,2], " - Biological indicators ",name_this_scenario,".csv", sep="")
bioindicator_mat_fore <- try(read.csv(bioindicator_path_fore, sep=";"))

if (class(bioindicator_mat_fore) != "try-error") {
BIOINDICATORS_fore <<- as.character(unique(bioindicator_mat_fore$Variable))
BIOINDICATORS_fore <<- BIOINDICATORS_fore[BIOINDICATORS_fore != "Bref" & BIOINDICATORS_fore != "SSBref" & BIOINDICATORS_fore != "SSB_SSBref"]

      for (choice in BIOINDICATORS_fore) { 
    bmt_forecast_species_bio_indicator$appendText(choice)    
    }
    
    gtkComboBoxSetActive(bmt_forecast_species_bio_indicator, 0)
      }

bi_sw_fore <<- gtkScrolledWindowNew(NULL, NULL)
bi_sw_fore$setShadowType("etched-in")
bi_sw_fore$setPolicy("automatic", "automatic")
bi_sw_fore$SetUsize(400, 400)  
#bi_sw$add(img_bio_indicator)  


# frame_forecast_kobe
hbox_kobe_fore <- gtkHBox(homogeneous = FALSE, 5)   
vbox_kobe_fore <- gtkVBox(homogeneous = FALSE, 5) 
vbox_kobe_fore$packStart(kb_sw_fore, expand = T, fill = T, padding = 10)

hbox_kobe_fore$packStart(vbox_kobe_fore, expand = T, fill = T, padding = 10)             
frame_forecast_kobe$add(hbox_kobe_fore) 


# frame_forecast_indicator
hbox_indic_fore <- gtkHBox(homogeneous = FALSE, 5)   
vbox_indic_fore  <- gtkVBox(homogeneous = FALSE, 5) 
hbox_indic_fore$packStart(vbox_indic_fore, expand = T, fill = T, padding = 10)             
frame_forecast_indicator$add(hbox_indic_fore) 

hbox_forecast_species_combo <- gtkHBox(homogeneous = FALSE, 5)        
hbox_forecast_species_combo$packStart(gtkLabel(" Stock "), expand = F, fill = F, padding = 0)  
hbox_forecast_species_combo$packStart(bmt_forecast_species, expand = F, fill = F, padding = 0)   
hbox_forecast_species_combo$packStart(gtkLabel(" State indicator "), expand = F, fill = F, padding = 0)   
hbox_forecast_species_combo$packStart(bmt_forecast_species_bio_indicator, expand = F, fill = F, padding = 0)  

vbox_indic_fore$packStart(hbox_forecast_species_combo, expand = F, fill = F, padding = 5)  
 
vbox_indic_fore$packStart(bi_sw_fore, expand = T, fill = T, padding = 10)  


gSignalConnect(bmt_forecast_species_bio_indicator, "changed", show_species_bio_indicator_fore)
gSignalConnect(bmt_forecast_species, "changed", show_species_bio_indicator_fore) 
gtkComboBoxSetActive(bmt_forecast_species, 0 ) 
gtkComboBoxSetActive(bmt_forecast_species_bio_indicator, 0 ) 

img_bio_indicator_pb_fore <<- NULL
img_bio_indicator_fore <<- NULL  


#gtkBoxReorderChild(vbox_Diagnosis_StateStocks, hbox_table_reduction, 0)
 
#vbox_kobe$packStart(, expand = F, fill = F, padding = 0)  
#vbox_kobe$packStart(stock_reduction.sw, expand = F, fill = F, padding = 10)   
