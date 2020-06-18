# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




vbox_Diagnosis_StateStocks <- gtkVBox(FALSE, 5) 

frame_diagnosis_kobe <- gtkFrame(" General state of the stock ")  
frame_diagnosis_indicator <- gtkFrame(" Time series of State indicators ")  

hbox_diagnosis_kobe <- gtkHBox(homogeneous = FALSE, 5)        
hbox_diagnosis_indicator <- gtkHBox(homogeneous = FALSE, 5) 

vbox_diagnosis_kobe <- gtkVBox(FALSE, 5)                             
vbox_diagnosis_indicator <- gtkVBox(FALSE, 5) 

hbox_diagnosis_kobe$packStart(frame_diagnosis_kobe, expand = T, fill = T, padding = 10)  
hbox_diagnosis_indicator$packStart(frame_diagnosis_indicator, expand = T, fill = T, padding = 10)  

vbox_diagnosis_kobe$packStart(hbox_diagnosis_kobe, expand = T, fill = T, padding = 5)  
vbox_diagnosis_indicator$packStart(hbox_diagnosis_indicator, expand = T, fill = T, padding = 10)  


# object
img_kobe_path <<-   paste(mat_cfg_general[1,3],  "/Diagnosis/", mat_cfg_general[1,2], " - KOBE plot.jpg", sep="")
img_KobePlot_pb <<- gdkPixbufNewFromFileAtSize(img_kobe_path, 350, 350)

kb_sw <<- gtkScrolledWindowNew(NULL, NULL)
kb_sw$setShadowType("etched-in")
kb_sw$setPolicy("automatic", "automatic")
kb_sw$SetUsize(350, 350)  

if (!is.null(img_KobePlot_pb[[1]])) {
kb_sw$destroy()
kb_sw <<- gtkScrolledWindowNew(NULL, NULL)
kb_sw$setShadowType("etched-in")
kb_sw$setPolicy("automatic", "automatic")
kb_sw$SetUsize(350, 350)    
img_KobePlot <<- gtkImageNewFromPixbuf(img_KobePlot_pb[[1]])
kb_sw$add(img_KobePlot)
 
}
  
 
bmt_diagnosis_species <<- gtkComboBoxNewText()
bmt_diagnosis_species_bio_indicator <<- gtkComboBoxNewText()

bioindicator_path <-   paste(mat_cfg_general[1,3],  "/Diagnosis/", mat_cfg_general[1,2], " - Biological indicators.csv", sep="")
bioindicator_mat <- try(read.csv(bioindicator_path, sep=";"))

if (class(bioindicator_mat) != "try-error") {
BIOINDICATORS <<- as.character(unique(bioindicator_mat$Variable))
BIOINDICATORS <<- BIOINDICATORS[BIOINDICATORS != "Bref" & BIOINDICATORS != "SSBref" & BIOINDICATORS != "SSB_SSBref"]

      for (choice in BIOINDICATORS) { 
    bmt_diagnosis_species_bio_indicator$appendText(choice)    
    }
      }

bi_sw <<- gtkScrolledWindowNew(NULL, NULL)
bi_sw$setShadowType("etched-in")
bi_sw$setPolicy("automatic", "automatic")
bi_sw$SetUsize(350, 350)  
#bi_sw$add(img_bio_indicator)  


reduction_path <- paste(mat_cfg_general[1,3],  "/Diagnosis/", mat_cfg_general[1,2], " - Reduction by stock.csv", sep="")
bmt_stock_reduction <- try(data.frame(read.csv( file=reduction_path, sep=";"),stringsAsFactors =F) )

if (class(bmt_stock_reduction) == "try-error") {
    bmt_stock_reduction <- NULL
} 
 
bmt_stock_reduction[,1] <- as.character(bmt_stock_reduction[,1] )
bmt_stock_reduction[,7] <- as.character(bmt_stock_reduction[,7] )
 # --------------------------- stock reduction table
stock_reduction.sw <<- gtkScrolledWindowNew(NULL, NULL)
stock_reduction.sw$setShadowType("etched-in")
stock_reduction.sw$setPolicy("automatic", "automatic")
stock_reduction.sw$SetUsize(120, 130)  
stock_reduction_list <<- list()
stock_reductionIndex <<- 0
stock_reduction.create_model()
stock_reduction.treeview <<- gtkTreeViewNewWithModel(stock_reduction.model)
stock_reduction.treeview$setRulesHint(TRUE)
stock_reduction.treeview$getSelection()$setMode("single")
stock_reduction.add_columns(stock_reduction.treeview)
stock_reduction.sw$add(stock_reduction.treeview) 



# frame_diagnosis_kobe
hbox_kobe <- gtkHBox(homogeneous = FALSE, 5)   
vbox_kobe <- gtkVBox(homogeneous = FALSE, 5)
 
vbox_kobe$packStart(kb_sw, expand = T, fill = T, padding = 10)

hbox_kobe$packStart(vbox_kobe, expand = T, fill = T, padding = 10)             
frame_diagnosis_kobe$add(hbox_kobe) 


# frame_diagnosis_indicator
hbox_indic <- gtkHBox(homogeneous = FALSE, 5)   
vbox_indic  <- gtkVBox(homogeneous = FALSE, 5) 
hbox_indic$packStart(vbox_indic, expand = T, fill = T, padding = 10)             
frame_diagnosis_indicator$add(hbox_indic) 

hbox_diagnosis_species_combo <- gtkHBox(homogeneous = FALSE, 5)        
hbox_diagnosis_species_combo$packStart(gtkLabel(" Stock "), expand = T, fill = T, padding = 0)  
hbox_diagnosis_species_combo$packStart(bmt_diagnosis_species, expand = T, fill = T, padding = 0)   
hbox_diagnosis_species_combo$packStart(gtkLabel(" State indicator "), expand = T, fill = T, padding = 0)   
hbox_diagnosis_species_combo$packStart(bmt_diagnosis_species_bio_indicator, expand = T, fill = T, padding = 0)  

vbox_indic$packStart(hbox_diagnosis_species_combo, expand = F, fill = F, padding = 10)  
vbox_indic$packStart(bi_sw, expand = T, fill = T, padding = 10)  



gSignalConnect(bmt_diagnosis_species_bio_indicator, "changed", show_species_bio_indicator)
gSignalConnect(bmt_diagnosis_species, "changed", show_species_bio_indicator)
gtkComboBoxSetActive(bmt_diagnosis_species_bio_indicator, 0 ) 

img_bio_indicator_pb <<- NULL
img_bio_indicator <<- NULL  

lbl_reduction <- gtkLabel(" % of reduction necessary for each stock to reach the reference point ")

vbox_table_reduction  <- gtkVBox(homogeneous = FALSE, 5) 
vbox_table_reduction$packStart(lbl_reduction, expand = F, fill = F, padding = 0) 
vbox_table_reduction$packStart(stock_reduction.sw, expand = F, fill = F, padding = 5)  

hbox_table_reduction  <- gtkHBox(homogeneous = FALSE, 5) 
hbox_table_reduction$packStart(vbox_table_reduction, expand = T, fill = T, padding = 10)  
#vbox_Diagnosis_StateStocks$packStart(hbox_table_reduction, expand = F, fill = F, padding =0)  


 bmt_tbl_diagnosis_results <- gtkTable(2, 2, homogeneous = FALSE)
bmt_tbl_diagnosis_results$SetRowSpacings(10)
bmt_tbl_diagnosis_results$SetColSpacings(10)
bmt_tbl_diagnosis_results$SetBorderWidth(5)

i=0  # column 0 
j=0 
bmt_tbl_diagnosis_results$Attach(vbox_diagnosis_kobe,i, i+1, j, j+1) 
j=j+1 
bmt_tbl_diagnosis_results$Attach(hbox_table_reduction,i, i+2, j, j+1)

i=i+1  # column 0 
j=0 
bmt_tbl_diagnosis_results$Attach(vbox_diagnosis_indicator,i, i+1, j, j+1) 

 vbox_Diagnosis_StateStocks$packStart(bmt_tbl_diagnosis_results, expand = T, fill = T, padding =0)  
#gtkBoxReorderChild(vbox_Diagnosis_StateStocks, hbox_table_reduction, 0)
 
#vbox_kobe$packStart(, expand = F, fill = F, padding = 0)  
#vbox_kobe$packStart(stock_reduction.sw, expand = F, fill = F, padding = 10)    