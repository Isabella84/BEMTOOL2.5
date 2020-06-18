# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.






#hboxFISHINGEFFORTFile <- gtkHBox(FALSE, 5)
#hboxFISHINGEFFORTFile$packStart(gtkLabel("Load FISHINGEFFORT data from .csv file"), expand = FALSE, fill = FALSE, padding = 5) 
#btn_browse <- gtkButton()
#gtkButtonSetLabel(btn_browse, "Browse...")
#btn_browse$AddCallback("clicked", select_file_FISHINGEFFORT)
#hboxFISHINGEFFORTFile$packStart(btn_browse, expand = FALSE, fill = FALSE, padding = 5)
# 
#lbl_FISHINGEFFORTFile <- gtkLabel("C:\\ ")
#hboxFISHINGEFFORTFile$packStart(lbl_FISHINGEFFORTFile, expand = FALSE, fill = FALSE, padding = 5)
# vboxFISHINGEFFORT$packStart(hboxFISHINGEFFORTFile, expand = FALSE, fill = FALSE, padding = 5)

hboxFISHINGEFFORT <- gtkHBox(FALSE, 5)
hboxFISHINGEFFORT$packStart(gtkLabel("Seed value for FISHINGEFFORT"), expand = FALSE, fill = FALSE, padding = 5) 

entry_FISHINGEFFORT_seedvalue <- gtkEntry() 
gtkEntrySetWidthChars(entry_FISHINGEFFORT_seedvalue, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetText(entry_FISHINGEFFORT_seedvalue, 0)  
hboxFISHINGEFFORT$packStart(entry_FISHINGEFFORT_seedvalue, expand = FALSE, fill = FALSE, padding = 5) 

btn_load_seed_FISHINGEFFORT <- gtkButton()
gtkButtonSetLabel(btn_load_seed_FISHINGEFFORT, "Load seed value")
btn_load_seed_FISHINGEFFORT$AddCallback("clicked", reload_FISHINGEFFORT_table)
hboxFISHINGEFFORT$packStart(btn_load_seed_FISHINGEFFORT, expand = FALSE, fill = FALSE, padding = 5) 

#btn_browse <- gtkButton()
#gtkButtonSetLabel(btn_browse, "Save FISHINGEFFORT data...")
#btn_browse$AddCallback("clicked", save_file_FISHINGEFFORT_vector)
# hboxFISHINGEFFORT$packStart(btn_browse, expand = FALSE, fill = FALSE, padding=0)

# vboxFISHINGEFFORT$packStart(hboxFISHINGEFFORT, expand = FALSE, fill = FALSE, padding = 5)             같같같같같같같같같같같같같같같같같같같같같같같같?
  
FISHINGEFFORT.sw <<- gtkScrolledWindowNew(NULL, NULL)
FISHINGEFFORT.sw$setShadowType("etched-in")
FISHINGEFFORT.sw$setPolicy("automatic", "automatic")
FISHINGEFFORT.sw$SetUsize(120, dim_eff_tables)  
FISHINGEFFORT <<- list()
FISHINGEFFORTIndex <<- 0
# ------------------------------
# create model
FISHINGEFFORT.create_model()
# create tree view
FISHINGEFFORT.treeview <<- gtkTreeViewNewWithModel(FISHINGEFFORT.model)
FISHINGEFFORT.treeview$setRulesHint(TRUE)
FISHINGEFFORT.treeview$getSelection()$setMode("single")
FISHINGEFFORT.add_columns(FISHINGEFFORT.treeview)
FISHINGEFFORT.sw$add(FISHINGEFFORT.treeview) 


 lbl_FC <- gtkLabel("Monthly FISHING COEFFICIENT")

vboxFISHINGEFFORT$packStart(lbl_FC , expand=FALSE, FALSE, 5)    
vboxFISHINGEFFORT$packStart(FISHINGEFFORT.sw , expand = FALSE, TRUE, 5) 

#hboxFISHINGEFFORTfile_save <- gtkHBox(FALSE, 5)
#btn_browse <- gtkButton()
#gtkButtonSetLabel(btn_browse, "Save FISHINGEFFORT data...")
#btn_browse$AddCallback("clicked", save_file_FISHINGEFFORT_vector)
#hboxFISHINGEFFORTfile_save$packStart(btn_browse, expand = FALSE, fill = FALSE, padding=0)
#vboxFISHINGEFFORT$packStart(hboxFISHINGEFFORTfile_save, expand = FALSE, fill = FALSE, padding = 0)

    gtkWidgetSetSensitive(button_load_fishingcoeff, FALSE)
     gtkWidgetSetSensitive(button_load_effortdata, TRUE)
