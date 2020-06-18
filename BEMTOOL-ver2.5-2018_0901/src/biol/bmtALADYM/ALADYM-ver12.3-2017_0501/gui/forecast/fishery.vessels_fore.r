# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.






# vboxActivity$packStart(hboxActivity, expand = FALSE, fill = FALSE, padding = 5)
#print(".......................................... [forecast.fleetTabs.vessels.r]")
hboxVESSELSFile_fore <- gtkHBox(FALSE, 5)
hboxVESSELSFile_fore$packStart(gtkLabel("Load VESSELS for forecast from .csv file"), expand = FALSE, fill = FALSE, padding = 5) 
btn_browse <- gtkButton()
gtkButtonSetLabel(btn_browse, "Browse...")
btn_browse$AddCallback("clicked", select_file_VESSELS_fore)
hboxVESSELSFile_fore$packStart(btn_browse, expand = FALSE, fill = FALSE, padding = 5)
 
lbl_VESSELSFile_fore <- gtkLabel("C:\\ ")
#hboxVESSELSFile_fore$packStart(lbl_VESSELSFile_fore, expand = FALSE, fill = FALSE, padding = 5)


#hboxVESSELSfile_save_fore <- gtkHBox(FALSE, 5)
btn_browse <- gtkButton()
gtkButtonSetLabel(btn_browse, "Save VESSELS parameters...")
btn_browse$AddCallback("clicked", save_file_VESSELS_vector_fore)
hboxVESSELSFile_fore$packStart(btn_browse, expand = FALSE, fill = FALSE, padding=5)
#hboxVESSELSfile_save_fore$packStart(btn_browse, expand = FALSE, fill = FALSE, padding=5)
#vboxVESSELS_fore$packStart(hboxVESSELSfile_save_fore, expand = FALSE, fill = FALSE, padding = 5)


#vboxVESSELS_fore$packStart(hboxVESSELSFile_fore, expand = FALSE, fill = FALSE, padding = 5)
  
VESSELS_fore.sw <<- gtkScrolledWindowNew(NULL, NULL)
VESSELS_fore.sw$setShadowType("etched-in")
VESSELS_fore.sw$setPolicy("automatic", "automatic")
VESSELS_fore.sw$SetUsize(100, dim_eff_tables)  
VESSELS_fore <<- list()
VESSELS_foreIndex <<- 0
# ------------------------------
# create model
VESSELS_fore.create_model()
# create tree view
VESSELS_fore.treeview <<- gtkTreeViewNewWithModel(VESSELS_fore.model)
VESSELS_fore.treeview$setRulesHint(TRUE)
VESSELS_fore.treeview$getSelection()$setMode("single")
VESSELS_fore.add_columns(VESSELS_fore.treeview)
VESSELS_fore.sw$add(VESSELS_fore.treeview)  

lbl_fore_VESSELS <- gtkLabel("Monthly VESSELS")

vboxVESSELS_fore$packStart(lbl_fore_VESSELS, expand=FALSE, FALSE, 0) 
vboxVESSELS_fore$packStart(VESSELS_fore.sw , expand=FALSE, FALSE, 0) 
 #gtkWidgetSetSensitive(lbl_fore_VESSELS, FALSE)
#  gtkWidgetSetSensitive(VESSELS_fore.sw, FALSE)