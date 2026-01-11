# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


vbox_economic_params_price <- gtkVBox(FALSE, 5) 


bmt_combo_price_models <<- gtkComboBoxNewText()
gSignalConnect(bmt_combo_price_models, "changed", setEconomicparams_price_settings)

for (item in PRICE_MODELS$option_name) {
  bmt_combo_price_models$appendText(item)
}

bmt_hbox_combo_price_models  <- gtkHBox(F, 5) 
 bmt_hbox_combo_price_models$packStart(gtkLabel(" Simulation model for price of landing ") , expand = F, F, 10)   
lblPriceFunction <<- gtkLabel() 
bmt_hbox_combo_price_models$packStart(bmt_combo_price_models, expand = F, F, 10)  
bmt_hbox_combo_price_models$packStart(lblPriceFunction , expand = F, F, 30)                               


vbox_economic_params_price$packStart(bmt_hbox_combo_price_models, expand = F, F, 5) 


# --------------------------------------------------------------------------------- 1) price_elast_landing_byfleet:	"Elasticity coefficient price-landings"
bmt_vboxprice_elast_landing_byfleet <- gtkVBox(F, 5) 
bmt_hboxprice_elast_landing_byfleet  <- gtkHBox(F, 5) 

bmt_price_elast_landing_byfleet.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_price_elast_landing_byfleet.sw$setShadowType("etched-in")
bmt_price_elast_landing_byfleet.sw$setPolicy("automatic", "automatic")
bmt_price_elast_landing_byfleet.sw$SetUsize(120, 100)  
bmt_price_elast_landing_byfleet_list <<- list()
bmt_price_elast_landing_byfleetIndex <<- 0
# create model
bmt_price_elast_landing_byfleet.create_model()
# create tree view
bmt_price_elast_landing_byfleet.treeview <<- gtkTreeViewNewWithModel(bmt_price_elast_landing_byfleet.model)
bmt_price_elast_landing_byfleet.treeview$setRulesHint(TRUE)
bmt_price_elast_landing_byfleet.treeview$getSelection()$setMode("single")
bmt_price_elast_landing_byfleet.add_columns(bmt_price_elast_landing_byfleet.treeview)
bmt_price_elast_landing_byfleet.sw$add(bmt_price_elast_landing_byfleet.treeview) 
 

bmt_hboxprice_elast_landing_byfleet$packStart(bmt_price_elast_landing_byfleet.sw , expand = T, T, 10) 
bmt_vboxprice_elast_landing_byfleet$packStart(bmt_hboxprice_elast_landing_byfleet , expand = F, F, 0)

lbl_price_elast_landing_byfleet <<- gtkLabel(" PRICE-LANDINGS by fleet segment (a) ") 
#vbox_economic_params_price$packStart(lbl_price_elast_landing_byfleet, expand = F, F, 5) 
#vbox_economic_params_price$packStart(bmt_vboxprice_elast_landing_byfleet , expand = F, F, 0) 


# --------------------------------------------------------------------------------- 3) price_elast_import:	"Elasticity coefficient price-imports"
bmt_vboxprice_elast_import <- gtkVBox(F, 5) 
bmt_hboxprice_elast_import  <- gtkHBox(F, 5) 

bmt_price_elast_import.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_price_elast_import.sw$setShadowType("etched-in")
bmt_price_elast_import.sw$setPolicy("automatic", "automatic")
bmt_price_elast_import.sw$SetUsize(120, 100)  
bmt_price_elast_import_list <<- list()
bmt_price_elast_importIndex <<- 0
# create model
bmt_price_elast_import.create_model()
# create tree view
bmt_price_elast_import.treeview <<- gtkTreeViewNewWithModel(bmt_price_elast_import.model)
bmt_price_elast_import.treeview$setRulesHint(TRUE)
bmt_price_elast_import.treeview$getSelection()$setMode("single")
bmt_price_elast_import.add_columns(bmt_price_elast_import.treeview)
bmt_price_elast_import.sw$add(bmt_price_elast_import.treeview) 
 

bmt_hboxprice_elast_import$packStart(bmt_price_elast_import.sw , expand = T, T, 10) 
bmt_vboxprice_elast_import$packStart(bmt_hboxprice_elast_import , expand = F, F, 0)

lbl_price_elast_import <<- gtkLabel(" PRICE-IMPORTS by fleet segment (b) ") 
#vbox_economic_params_price$packStart(lbl_price_elast_import, expand = F, F, 5) 
#vbox_economic_params_price$packStart(bmt_vboxprice_elast_import , expand = F, F, 0) 



# --------------------------------------------------------------------------------- 4) price_elast_MW :	"Elasticity coefficient price-MEAN WEIGHT"
bmt_vboxprice_elast_MW <- gtkVBox(F, 5) 
bmt_hboxprice_elast_MW  <- gtkHBox(F, 5) 

bmt_price_elast_MW.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_price_elast_MW.sw$setShadowType("etched-in")
bmt_price_elast_MW.sw$setPolicy("automatic", "automatic")
bmt_price_elast_MW.sw$SetUsize(120, 100)  
bmt_price_elast_MW_list <<- list()
bmt_price_elast_MWIndex <<- 0
# create model
bmt_price_elast_MW.create_model()
# create tree view
bmt_price_elast_MW.treeview <<- gtkTreeViewNewWithModel(bmt_price_elast_MW.model)
bmt_price_elast_MW.treeview$setRulesHint(TRUE)
bmt_price_elast_MW.treeview$getSelection()$setMode("single")
bmt_price_elast_MW.add_columns(bmt_price_elast_MW.treeview)
bmt_price_elast_MW.sw$add(bmt_price_elast_MW.treeview) 
 

bmt_hboxprice_elast_MW$packStart(bmt_price_elast_MW.sw , expand = T, T, 10) 
bmt_vboxprice_elast_MW$packStart(bmt_hboxprice_elast_MW , expand = F, F, 0)

lbl_price_elast_MW <<- gtkLabel(" PRICE-MEAN WEIGHT of landing (c) ") 
#vbox_economic_params_price$packStart(lbl_price_elast_import, expand = F, F, 5) 
#vbox_economic_params_price$packStart(bmt_vboxprice_elast_import , expand = F, F, 0) 


# --------------------------------------------------------------------------------- 5) price_importweight:	"Times series of import in weight by stock for the forecast period"
bmt_vboxprice_importweight <- gtkVBox(F, 5) 
bmt_hboxprice_importweight  <- gtkHBox(F, 5) 

bmt_price_importweight.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_price_importweight.sw$setShadowType("etched-in")
bmt_price_importweight.sw$setPolicy("automatic", "automatic")
bmt_price_importweight.sw$SetUsize(120, 100)  
bmt_price_importweight_list <<- list()
bmt_price_importweightIndex <<- 0
# create model
bmt_price_importweight.create_model()
# create tree view
bmt_price_importweight.treeview <<- gtkTreeViewNewWithModel(bmt_price_importweight.model)
bmt_price_importweight.treeview$setRulesHint(TRUE)
bmt_price_importweight.treeview$getSelection()$setMode("single")
bmt_price_importweight.add_columns(bmt_price_importweight.treeview)
bmt_price_importweight.sw$add(bmt_price_importweight.treeview) 
 

bmt_hboxprice_importweight$packStart(bmt_price_importweight.sw , expand = T, T, 10) 
bmt_vboxprice_importweight$packStart(bmt_hboxprice_importweight , expand = F, F, 0)

lbl_price_importweight <<- gtkLabel(" Times series of IMPORT IN WEIGHT by stock for the forecast period (imp[t]) ") 
#vbox_economic_params_price$packStart(lbl_price_elast_import, expand = F, F, 5) 
#vbox_economic_params_price$packStart(bmt_vboxprice_elast_import , expand = F, F, 0) 



# --------------------------------------------------------------------------------- 1) price_elast_landing_byfleet:	"Elasticity coefficient price-landings"
bmt_vboxprice_costant_byfleet_landing <- gtkVBox(F, 5)
bmt_hboxprice_costant_byfleet_landing  <- gtkHBox(F, 5)

price_costant_byfleet_landing.sw <<- gtkScrolledWindowNew(NULL, NULL)
price_costant_byfleet_landing.sw$setShadowType("etched-in")
price_costant_byfleet_landing.sw$setPolicy("automatic", "automatic")
price_costant_byfleet_landing.sw$SetUsize(120, 100)
price_costant_byfleet_landing_list <<- list()
price_costant_byfleet_landingIndex <<- 0
# create model
price_costant_byfleet_landing.create_model()
# create tree view
price_costant_byfleet_landing.treeview <<- gtkTreeViewNewWithModel(price_costant_byfleet_landing.model)
price_costant_byfleet_landing.treeview$setRulesHint(TRUE)
price_costant_byfleet_landing.treeview$getSelection()$setMode("single")
price_costant_byfleet_landing.add_columns(price_costant_byfleet_landing.treeview)
price_costant_byfleet_landing.sw$add(price_costant_byfleet_landing.treeview)


bmt_hboxprice_costant_byfleet_landing$packStart(price_costant_byfleet_landing.sw , expand = T, T, 10)
bmt_vboxprice_costant_byfleet_landing$packStart(bmt_hboxprice_costant_byfleet_landing , expand = F, F, 0)

lbl_price_costant_byfleet_landing <<- gtkLabel(" Costant price (?) by fleet segment (d) ")



 tbl_economic_params_price <- gtkTable(2,2,homogeneous = T)
 tbl_economic_params_price$SetRowSpacings(5)
 tbl_economic_params_price$SetColSpacings(10)
 tbl_economic_params_price$SetBorderWidth(5)
 
 
  i = 0   # column 1 
  j=0
cell_economic_price_11 <- gtkVBox(F, 5)    
cell_economic_price_11$packStart(lbl_price_elast_landing_byfleet, expand = F, F, 0) 
cell_economic_price_11$packStart(bmt_vboxprice_elast_landing_byfleet, expand = F, F, 5) 
tbl_economic_params_price$Attach(cell_economic_price_11,i, i+1, j, j+1)
 
 j=j+1 
cell_economic_price_21 <- gtkVBox(F, 5)    
cell_economic_price_21$packStart(lbl_price_elast_MW, expand = F, F, 0) 
cell_economic_price_21$packStart(bmt_vboxprice_elast_MW, expand = F, F, 5) 
tbl_economic_params_price$Attach(cell_economic_price_21,i, i+1, j, j+1)

 # j=j+1 



  i = i+1   # column 2 
  j=0 
cell_economic_price_12 <- gtkVBox(F, 5)    
cell_economic_price_12$packStart(lbl_price_elast_import, expand = F, F, 0) 
cell_economic_price_12$packStart(bmt_vboxprice_elast_import, expand = F, F, 5)  
tbl_economic_params_price$Attach(cell_economic_price_12,i, i+1, j, j+1)
 
  j=j+1 
cell_economic_price_22 <- gtkVBox(F, 5)    
cell_economic_price_22$packStart(lbl_price_costant_byfleet_landing, expand = F, F, 0) 
cell_economic_price_22$packStart(bmt_vboxprice_costant_byfleet_landing, expand = F, F, 5) 
tbl_economic_params_price$Attach(cell_economic_price_22,i, i+1, j, j+1)



 frame_economic_params_elasticity <- gtkFrame(" Elasticity coefficients ") 
 hbox_economic_params_elasticity <- gtkHBox(homogeneous = FALSE, 5)   
 vbox_economic_params_elasticity <- gtkVBox(FALSE, 5)        
 hbox_economic_params_elasticity$packStart(frame_economic_params_elasticity, expand = T, fill = T, padding = 10)                 
   vbox_economic_params_elasticity$packStart(hbox_economic_params_elasticity, expand = F, fill = F, padding = 5)  
   frame_economic_params_elasticity$add(tbl_economic_params_price)           
 
 vbox_economic_params_price$packStart(vbox_economic_params_elasticity , expand = F, F, 0)    
 
suppressWarnings(source(paste(getwd(), "/bmtgui/economic_params/economic_params_price_discard.r", sep="")) )
   
cell_economic_price_31 <- gtkVBox(F, 5)    
cell_economic_price_31$packStart(lbl_price_importweight, expand = F, F, 0) 
cell_economic_price_31$packStart(bmt_vboxprice_importweight, expand = F, F, 5)  


vbox_economic_params_price$packStart(cell_economic_price_31 , expand = F, F, 0)   
#vbox_economic_params_price$packStart(cell_economic_price_32 , expand = F, F, 0) 
                           
