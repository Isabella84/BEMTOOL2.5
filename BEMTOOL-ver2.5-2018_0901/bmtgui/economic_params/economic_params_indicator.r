# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


vbox_economic_params_indicators <- gtkVBox(FALSE, 5) 



# ------------------------------------------------------------------------------------ 1)	ecoind:	"Coefficients for economic indicators"
 
bmt_vboxecoind <- gtkVBox(F, 5) 
bmt_hboxecoind  <- gtkHBox(F, 5) 

bmt_ecoind.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_ecoind.sw$setShadowType("etched-in")
bmt_ecoind.sw$setPolicy("automatic", "automatic")
bmt_ecoind.sw$SetUsize(120, 170)  
bmt_ecoind_list <<- list()
bmt_ecoindIndex <<- 0
# create model
bmt_ecoind.create_model()
# create tree view
bmt_ecoind.treeview <<- gtkTreeViewNewWithModel(bmt_ecoind.model)
bmt_ecoind.treeview$setRulesHint(TRUE)
bmt_ecoind.treeview$getSelection()$setMode("single")
bmt_ecoind.add_columns(bmt_ecoind.treeview)
bmt_ecoind.sw$add(bmt_ecoind.treeview) 

 lbl_ecoind <<- gtkLabel(" Coefficients for economic INDICATORS ") 
 
bmt_vboxecoind_table <- gtkVBox(F, 5)    
bmt_vboxecoind_table$packStart(lbl_ecoind , expand = F, F, 0)
bmt_vboxecoind_table$packStart(bmt_ecoind.sw  , expand = F, F, 0)

bmt_hboxecoind$packStart(bmt_vboxecoind_table, expand = T, T, 10) 
bmt_vboxecoind$packStart(bmt_hboxecoind , expand = F, F, 0)

bmt_vboxecoind$packStart( gtkLabel("* landing options are:\t1. proportional: L[tot] = ll * sum(L[s])\t2. additive: L[tot] = u + v * sum(L[s]) + sum(L[s])\t3. multiplicative:  L[tot] = u * ( sum(L[s]) ^ v ) + sum(L[s])") , expand = F, F, 5)  


# ------------------------------------------------------------------------------------ 2)	indic_taxes:	"Additional taxes"
 
bmt_vboxindic_taxes <- gtkVBox(F, 5) 
bmt_hboxindic_taxes  <- gtkHBox(F, 5) 

bmt_indic_taxes.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_indic_taxes.sw$setShadowType("etched-in")
bmt_indic_taxes.sw$setPolicy("automatic", "automatic")
bmt_indic_taxes.sw$SetUsize(120, 150)  
bmt_indic_taxes_list <<- list()
bmt_indic_taxesIndex <<- 0
# create model
bmt_indic_taxes.create_model()
# create tree view
bmt_indic_taxes.treeview <<- gtkTreeViewNewWithModel(bmt_indic_taxes.model)
bmt_indic_taxes.treeview$setRulesHint(TRUE)
bmt_indic_taxes.treeview$getSelection()$setMode("single")
bmt_indic_taxes.add_columns(bmt_indic_taxes.treeview)
bmt_indic_taxes.sw$add(bmt_indic_taxes.treeview) 

 lbl_indic_taxes <<- gtkLabel(" Additional TAXES ") 
 
bmt_vboxindic_taxes_table <- gtkVBox(F, 5)    
bmt_vboxindic_taxes_table$packStart(lbl_indic_taxes , expand = F, F, 0)
bmt_vboxindic_taxes_table$packStart(bmt_indic_taxes.sw  , expand = F, F, 0)

bmt_hboxindic_taxes$packStart(bmt_vboxindic_taxes_table, expand = T, T, 10) 
bmt_vboxindic_taxes$packStart(bmt_hboxindic_taxes , expand = F, F, 0)



# ------------------------------------------------------------------------------------ 3)	indic_subsidies:	"Additional subsidies or income different from revenues"
 
bmt_vboxindic_subsidies <- gtkVBox(F, 5) 
bmt_hboxindic_subsidies  <- gtkHBox(F, 5) 

bmt_indic_subsidies.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_indic_subsidies.sw$setShadowType("etched-in")
bmt_indic_subsidies.sw$setPolicy("automatic", "automatic")
bmt_indic_subsidies.sw$SetUsize(120, 150)  
bmt_indic_subsidies_list <<- list()
bmt_indic_subsidiesIndex <<- 0
# create model
bmt_indic_subsidies.create_model()
# create tree view
bmt_indic_subsidies.treeview <<- gtkTreeViewNewWithModel(bmt_indic_subsidies.model)
bmt_indic_subsidies.treeview$setRulesHint(TRUE)
bmt_indic_subsidies.treeview$getSelection()$setMode("single")
bmt_indic_subsidies.add_columns(bmt_indic_subsidies.treeview)
bmt_indic_subsidies.sw$add(bmt_indic_subsidies.treeview) 

 lbl_indic_subsidies <<- gtkLabel(" Additional SUBSIDIES or income different from revenues ") 
 
bmt_vboxindic_subsidies_table <- gtkVBox(F, 5)    
bmt_vboxindic_subsidies_table$packStart(lbl_indic_subsidies , expand = F, F, 0)
bmt_vboxindic_subsidies_table$packStart(bmt_indic_subsidies.sw  , expand = F, F, 0)

bmt_hboxindic_subsidies$packStart(bmt_vboxindic_subsidies_table, expand = T, T, 10) 
bmt_vboxindic_subsidies$packStart(bmt_hboxindic_subsidies , expand = F, F, 0)





hbox_economic_params_indicators_container <- gtkHBox(homogeneous = FALSE, 5)   
vbox_economic_params_indicators_container <- gtkVBox(FALSE, 5)        
vbox_economic_params_indicators_container$packStart(bmt_vboxecoind, expand = F, fill = F, padding = 5)   
vbox_economic_params_indicators_container$packStart(bmt_vboxindic_taxes, expand = F, fill = F, padding = 5)  
vbox_economic_params_indicators_container$packStart(bmt_vboxindic_subsidies, expand = F, fill = F, padding = 5)  
#vbox_economic_params_indicators_container$packStart(bmt_vboxdiscard_cost, expand = F, fill = F, padding = 5)                     
#hbox_economic_params_indicators_container$packStart(vbox_economic_params_behaviour_container, expand = T, fill = T, padding = 10) 
#
vbox_economic_params_indicators$packStart(vbox_economic_params_indicators_container, expand = T, fill = T, padding = 5)    