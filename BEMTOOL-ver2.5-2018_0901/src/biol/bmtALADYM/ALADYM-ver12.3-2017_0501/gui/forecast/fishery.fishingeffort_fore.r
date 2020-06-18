# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





# vboxActivity$packStart(hboxActivity, expand = FALSE, fill = FALSE, padding = 5)
#print(".......................................... [forecast.fleetTabs.fishingeffort.r]")
hboxFISHINGEFFORTFile_fore <- gtkHBox(FALSE, 5)
hboxFISHINGEFFORTFile_fore$packStart(gtkLabel("Load FISHINGEFFORT for forecast from .csv file"), expand = FALSE, fill = FALSE, padding = 5) 
btn_browse <- gtkButton()
gtkButtonSetLabel(btn_browse, "Browse...")
btn_browse$AddCallback("clicked", select_file_FISHINGEFFORT_fore)
hboxFISHINGEFFORTFile_fore$packStart(btn_browse, expand = FALSE, fill = FALSE, padding = 5)
 
lbl_FISHINGEFFORTFile_fore <- gtkLabel("C:\\ ")
hboxFISHINGEFFORTFile_fore$packStart(lbl_FISHINGEFFORTFile_fore, expand = FALSE, fill = FALSE, padding = 5)

#hboxFISHINGEFFORTfile_save_fore <- gtkHBox(FALSE, 5)
btn_browse <- gtkButton()
gtkButtonSetLabel(btn_browse, "Save FISHINGEFFORT parameters...")
btn_browse$AddCallback("clicked", save_file_FISHINGEFFORT_vector_fore)
hboxFISHINGEFFORTFile_fore$packStart(btn_browse, expand = FALSE, fill = FALSE, padding=5)
#hboxFISHINGEFFORTfile_save_fore$packStart(btn_browse, expand = FALSE, fill = FALSE, padding=5)
#vboxFISHINGEFFORT_fore$packStart(hboxFISHINGEFFORTfile_save_fore, expand = FALSE, fill = FALSE, padding = 5)
#

#vboxFISHINGEFFORT_fore$packStart(hboxFISHINGEFFORTFile_fore, expand = FALSE, fill = FALSE, padding = 5)
  
FISHINGEFFORT_fore.sw <<- gtkScrolledWindowNew(NULL, NULL)
FISHINGEFFORT_fore.sw$setShadowType("etched-in")
FISHINGEFFORT_fore.sw$setPolicy("automatic", "automatic")
FISHINGEFFORT_fore.sw$SetUsize(100, dim_eff_tables)  
FISHINGEFFORT_fore <<- list()
FISHINGEFFORT_foreIndex <<- 0
# ------------------------------
# create model
FISHINGEFFORT_fore.create_model()
# create tree view
FISHINGEFFORT_fore.treeview <<- gtkTreeViewNewWithModel(FISHINGEFFORT_fore.model)
FISHINGEFFORT_fore.treeview$setRulesHint(TRUE)
FISHINGEFFORT_fore.treeview$getSelection()$setMode("single")
FISHINGEFFORT_fore.add_columns(FISHINGEFFORT_fore.treeview)
FISHINGEFFORT_fore.sw$add(FISHINGEFFORT_fore.treeview)   

lbl_fore_FE <- gtkLabel("Monthly FISHING COEFFICIENT")

vboxFISHINGEFFORT_fore$packStart( lbl_fore_FE, expand=FALSE, FALSE, 0)  
vboxFISHINGEFFORT_fore$packStart(FISHINGEFFORT_fore.sw , expand=FALSE, FALSE, 0) 

