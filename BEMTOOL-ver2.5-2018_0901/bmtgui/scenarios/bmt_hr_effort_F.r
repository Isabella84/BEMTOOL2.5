# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


# --------------------------------------------------------- effort-F relationship
bmt_effort_F.sw <<- gtkScrolledWindowNew(NULL, NULL)
bmt_effort_F.sw$setShadowType("etched-in")
bmt_effort_F.sw$setPolicy("automatic", "automatic")
bmt_effort_F.sw$SetUsize(140, 110)  
bmt_effort_F_list <<- list()
bmt_effort_FIndex <<- 0
# create model
#if (length(BMT_FLEETSEGMENTS) != 0)  {
bmt_effort_F.create_model()
# create tree view
bmt_effort_F.treeview <<- gtkTreeViewNewWithModel(bmt_effort_F.model)
bmt_effort_F.treeview$setRulesHint(TRUE)
bmt_effort_F.treeview$getSelection()$setMode("single")
bmt_effort_F.add_columns(bmt_effort_F.treeview)
bmt_effort_F.sw$add(bmt_effort_F.treeview) 
#}

bmt_vbox_effort_F_relationship <- gtkVBox(F, 5) 
bmt_hbox_effort_F_relationship  <- gtkHBox(F, 5) 

bmt_hbox_effort_F_relationship$packStart(bmt_effort_F.sw , expand = T, T, 5) 
bmt_vbox_effort_F_relationship$packStart(bmt_hbox_effort_F_relationship , expand = F, T, 5)

#bmt_vbox_effort_F_relationship$packStart( gtkLabel("* Type of relationship can be L: linear [y = a + bx], P: power [y = a * x^b]") , expand = F, F,0)  

bmt_vbox_effort_F_legend <- gtkVBox(F, 5) 
#bmt_hbox_effort_F_legend <- gtkHBox(F, 5) 
bmt_vbox_effort_F_legend$packStart( gtkLabel("* Type of relationship can be") , expand = F, F,0)  
bmt_vbox_effort_F_legend$packStart( gtkLabel("L: linear [y = a + bx], P: power [y = a * x^b]") , expand = F, F,0)  
#bmt_hbox_effort_F_legend$packStart(bmt_vbox_effort_F_legend , expand = F, F, 5)
#bmt_hbox_effort_F_relationship$packStart(bmt_hbox_effort_F_legend , expand = F, F, 5) 

bmt_vbox_effort_F_relationship$packStart( bmt_vbox_effort_F_legend , expand = F, F,0)  

vbox_hr_effort_F$packStart(bmt_vbox_effort_F_relationship , expand = T, fill = T, padding = 5)