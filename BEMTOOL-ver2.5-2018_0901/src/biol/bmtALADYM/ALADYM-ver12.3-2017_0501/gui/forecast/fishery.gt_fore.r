# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





# vboxActivity$packStart(hboxActivity, expand = FALSE, fill = FALSE, padding = 5)
#print(".......................................... [forecast.fleetTabs.GT.r]")
hboxGTFile_fore <- gtkHBox(FALSE, 5)
hboxGTFile_fore$packStart(gtkLabel("Load GT for forecast from .csv file"), expand = FALSE, fill = FALSE, padding = 5) 
btn_browse <- gtkButton()
gtkButtonSetLabel(btn_browse, "Browse...")
btn_browse$AddCallback("clicked", select_file_GT_fore)
hboxGTFile_fore$packStart(btn_browse, expand = FALSE, fill = FALSE, padding = 5)
 
lbl_GTFile_fore <- gtkLabel("C:\\ ")
hboxGTFile_fore$packStart(lbl_GTFile_fore, expand = FALSE, fill = FALSE, padding = 5)


#hboxGTfile_save_fore <- gtkHBox(FALSE, 5)
btn_browse <- gtkButton()
gtkButtonSetLabel(btn_browse, "Save GT parameters...")
btn_browse$AddCallback("clicked", save_file_GT_vector_fore)
hboxGTFile_fore$packStart(btn_browse, expand = FALSE, fill = FALSE, padding=5)
#hboxGTfile_save_fore$packStart(btn_browse, expand = FALSE, fill = FALSE, padding=5)
#vboxGT_fore$packStart(hboxGTfile_save_fore, expand = FALSE, fill = FALSE, padding = 5)
#

#vboxGT_fore$packStart(hboxGTFile_fore, expand = FALSE, fill = FALSE, padding = 5)
  
GT_fore.sw <<- gtkScrolledWindowNew(NULL, NULL)
GT_fore.sw$setShadowType("etched-in")
GT_fore.sw$setPolicy("automatic", "automatic")
GT_fore.sw$SetUsize(100, dim_eff_tables)  
GT_fore <<- list()
GT_foreIndex <<- 0
# ------------------------------
# create model
GT_fore.create_model()
# create tree view
GT_fore.treeview <<- gtkTreeViewNewWithModel(GT_fore.model)
GT_fore.treeview$setRulesHint(TRUE)
GT_fore.treeview$getSelection()$setMode("single")
GT_fore.add_columns(GT_fore.treeview)
GT_fore.sw$add(GT_fore.treeview)   

 lbl_fore_GTs <- gtkLabel("Monthly average GT")

vboxGT_fore$packStart(lbl_fore_GTs , expand=FALSE, FALSE, 0) 
vboxGT_fore$packStart(GT_fore.sw , expand=FALSE, FALSE, 0) 
# gtkWidgetSetSensitive(lbl_fore_GTs, FALSE)
#  gtkWidgetSetSensitive(GT_fore.treeview, FALSE)             