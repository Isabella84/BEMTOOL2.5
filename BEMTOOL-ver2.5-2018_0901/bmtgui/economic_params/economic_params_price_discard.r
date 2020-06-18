# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy,
# completeness or appropriateness for any particular purpose.



vbox_economic_params_price_discard <- gtkVBox(FALSE, 5)

bmt_combo_price_models_discard <<- gtkComboBoxNewText()
gSignalConnect(bmt_combo_price_models_discard, "changed", setEconomicparams_price_settings_discard)

for (item in PRICE_MODELS_DISCARD$option_name) {
  bmt_combo_price_models_discard$appendText(item)
}

bmt_hbox_combo_price_models_discard  <- gtkHBox(F, 5)
 bmt_hbox_combo_price_models_discard$packStart(gtkLabel(" Simulation model for price of discard") , expand = F, F, 10)
lblPriceFunction_discard <<- gtkLabel()
bmt_hbox_combo_price_models_discard$packStart(bmt_combo_price_models_discard, expand = F, F, 10)
bmt_hbox_combo_price_models_discard$packStart(lblPriceFunction_discard , expand = F, F, 30)


vbox_economic_params_price_discard$packStart(bmt_hbox_combo_price_models_discard, expand = F, F, 5)


# --------------------------------------------------------------------------------- 1) price_elast_landing_byfleet:	"Elasticity coefficient price-landings"
bmt_vboxprice_elast_discard_byfleet <- gtkVBox(F, 5)
bmt_hboxprice_elast_discard_byfleet  <- gtkHBox(F, 5)

price_elast_discard_byfleet.sw <<- gtkScrolledWindowNew(NULL, NULL)
price_elast_discard_byfleet.sw$setShadowType("etched-in")
price_elast_discard_byfleet.sw$setPolicy("automatic", "automatic")
price_elast_discard_byfleet.sw$SetUsize(120, 100)
price_elast_discard_byfleet_list <<- list()
price_elast_discard_byfleetIndex <<- 0
# create model
price_elast_discard_byfleet.create_model()
# create tree view
price_elast_discard_byfleet.treeview <<- gtkTreeViewNewWithModel(price_elast_discard_byfleet.model)
price_elast_discard_byfleet.treeview$setRulesHint(TRUE)
price_elast_discard_byfleet.treeview$getSelection()$setMode("single")
price_elast_discard_byfleet.add_columns(price_elast_discard_byfleet.treeview)
price_elast_discard_byfleet.sw$add(price_elast_discard_byfleet.treeview)


bmt_hboxprice_elast_discard_byfleet$packStart(price_elast_discard_byfleet.sw , expand = T, T, 10)
bmt_vboxprice_elast_discard_byfleet$packStart(bmt_hboxprice_elast_discard_byfleet , expand = F, F, 0)

lbl_price_elast_discard_byfleet <<- gtkLabel(" Elasticity coefficient by fleet segment (a) ")
#vbox_economic_params_price$packStart(lbl_price_elast_landing_byfleet, expand = F, F, 5)
#vbox_economic_params_price$packStart(bmt_vboxprice_elast_landing_byfleet , expand = F, F, 0)



# --------------------------------------------------------------------------------- 1) price_elast_landing_byfleet:	"Elasticity coefficient price-landings"
bmt_vboxprice_costant_byfleet_discard <- gtkVBox(F, 5)
bmt_hboxprice_costant_byfleet_discard  <- gtkHBox(F, 5)

price_costant_byfleet_discard.sw <<- gtkScrolledWindowNew(NULL, NULL)
price_costant_byfleet_discard.sw$setShadowType("etched-in")
price_costant_byfleet_discard.sw$setPolicy("automatic", "automatic")
price_costant_byfleet_discard.sw$SetUsize(120, 100)
price_costant_byfleet_discard_list <<- list()
price_costant_byfleet_discardIndex <<- 0
# create model
price_costant_byfleet_discard.create_model()
# create tree view
price_costant_byfleet_discard.treeview <<- gtkTreeViewNewWithModel(price_costant_byfleet_discard.model)
price_costant_byfleet_discard.treeview$setRulesHint(TRUE)
price_costant_byfleet_discard.treeview$getSelection()$setMode("single")
price_costant_byfleet_discard.add_columns(price_costant_byfleet_discard.treeview)
price_costant_byfleet_discard.sw$add(price_costant_byfleet_discard.treeview)


bmt_hboxprice_costant_byfleet_discard$packStart(price_costant_byfleet_discard.sw , expand = T, T, 10)
bmt_vboxprice_costant_byfleet_discard$packStart(bmt_hboxprice_costant_byfleet_discard , expand = F, F, 0)

lbl_price_costant_byfleet_discard <<- gtkLabel(" Costant price (€) by fleet segment (b) ")
#vbox_economic_params_price$packStart(lbl_price_elast_landing_byfleet, expand = F, F, 5)
#vbox_economic_params_price$packStart(bmt_vboxprice_elast_landing_byfleet , expand = F, F, 0)



 tbl_economic_params_price_discard <- gtkTable(1,2,homogeneous = T)
 tbl_economic_params_price_discard$SetRowSpacings(5)
 tbl_economic_params_price_discard$SetColSpacings(10)
 tbl_economic_params_price_discard$SetBorderWidth(5)


  i = 0   # column 1
  j=0
cell_economic_price_discard_11 <- gtkVBox(F, 5)
cell_economic_price_discard_11$packStart(lbl_price_elast_discard_byfleet, expand = F, F, 0)
cell_economic_price_discard_11$packStart(bmt_vboxprice_elast_discard_byfleet, expand = F, F, 5)
tbl_economic_params_price_discard$Attach(cell_economic_price_discard_11,i, i+1, j, j+1)

i=i+1
cell_economic_price_discard_21 <- gtkVBox(F, 5)
cell_economic_price_discard_21$packStart(lbl_price_costant_byfleet_discard, expand = F, F, 0)
cell_economic_price_discard_21$packStart(bmt_vboxprice_costant_byfleet_discard, expand = F, F, 5)
tbl_economic_params_price_discard$Attach(cell_economic_price_discard_21,i, i+1, j, j+1)

 # j=j+1

 frame_economic_params_elasticity_discard <- gtkFrame(" DISCARD prices ")
 hbox_economic_params_elasticity_discard <- gtkHBox(homogeneous = FALSE, 5)
 vbox_economic_params_elasticity_discard <- gtkVBox(FALSE, 5)
 hbox_economic_params_elasticity_discard$packStart(frame_economic_params_elasticity_discard, expand = T, fill = T, padding = 10)
   vbox_economic_params_elasticity_discard$packStart(hbox_economic_params_elasticity_discard, expand = F, fill = F, padding = 5)
   frame_economic_params_elasticity_discard$add(tbl_economic_params_price_discard)

 vbox_economic_params_price_discard$packStart(vbox_economic_params_elasticity_discard , expand = F, F, 0)
 
 vbox_economic_params_price$packStart(vbox_economic_params_price_discard , expand = F, F, 0)
