# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





#hboxDAYSFile <- gtkHBox(FALSE, 5)
#hboxDAYSFile$packStart(gtkLabel("Load DAYS data from .csv file"), expand = FALSE, fill = FALSE, padding = 5) 
#btn_browse <- gtkButton()
#gtkButtonSetLabel(btn_browse, "Browse...")
#btn_browse$AddCallback("clicked", select_file_DAYS)
#hboxDAYSFile$packStart(btn_browse, expand = FALSE, fill = FALSE, padding = 5)
# 
#lbl_DAYSFile <- gtkLabel("C:\\ ")
#hboxDAYSFile$packStart(lbl_DAYSFile, expand = FALSE, fill = FALSE, padding = 5)
# vboxDAYS$packStart(hboxDAYSFile, expand = FALSE, fill = FALSE, padding = 15)

hboxDAYS <- gtkHBox(FALSE, 5)
hboxDAYS$packStart(gtkLabel("Seed value for DAYS"), expand = FALSE, fill = FALSE, padding = 5) 

entry_DAYS_seedvalue <- gtkEntry() 
gtkEntrySetWidthChars(entry_DAYS_seedvalue, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetText(entry_DAYS_seedvalue, 0)  
hboxDAYS$packStart(entry_DAYS_seedvalue, expand = FALSE, fill = FALSE, padding = 5) 

btn_load_seed_DAYS <- gtkButton()
gtkButtonSetLabel(btn_load_seed_DAYS, "Load seed value")
btn_load_seed_DAYS$AddCallback("clicked", reload_DAYS_table)
hboxDAYS$packStart(btn_load_seed_DAYS, expand = FALSE, fill = FALSE, padding = 5) 

#btn_browse <- gtkButton()
#gtkButtonSetLabel(btn_browse, "Save DAYS data...")
#btn_browse$AddCallback("clicked", save_file_DAYS_vector)
# hboxDAYS$packStart(btn_browse, expand = FALSE, fill = FALSE, padding=0)

#vboxDAYS$packStart(hboxDAYS, expand = FALSE, fill = FALSE, padding = 5)      같같같같같같같같같같같같같같같같같같같같같같같같?
  
DAYS.sw <<- gtkScrolledWindowNew(NULL, NULL)
DAYS.sw$setShadowType("etched-in")
DAYS.sw$setPolicy("automatic", "automatic")
DAYS.sw$SetUsize(120, dim_eff_tables)  
DAYS <<- list()
DAYSIndex <<- 0
# ------------------------------
# create model
DAYS.create_model()
# create tree view
DAYS.treeview <<- gtkTreeViewNewWithModel(DAYS.model)
DAYS.treeview$setRulesHint(TRUE)
DAYS.treeview$getSelection()$setMode("single")
DAYS.add_columns(DAYS.treeview)
DAYS.sw$add(DAYS.treeview)    

lbl_DAYS <- gtkLabel("Monthly average DAYS")

vboxDAYS$packStart(lbl_DAYS, expand=FALSE, FALSE, 5) 
vboxDAYS$packStart(DAYS.sw , expand = FALSE, TRUE, 5) 
#hboxDAYS$packStart(DAYS.sw , TRUE, TRUE, 0) 
#vboxDAYS$packStart(hboxDAYS, expand = FALSE, fill = FALSE, padding = 5)
#

#hboxDAYSfile_save <- gtkHBox(FALSE, 5)
#btn_browse <- gtkButton()
#gtkButtonSetLabel(btn_browse, "Save DAYS data...")
#btn_browse$AddCallback("clicked", save_file_DAYS_vector)
#hboxDAYSfile_save$packStart(btn_browse, expand = FALSE, fill = FALSE, padding=0)
#vboxDAYS$packStart(hboxDAYSfile_save, expand = FALSE, fill = FALSE, padding = 0)
