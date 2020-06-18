# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.






#hboxGTFile <- gtkHBox(FALSE, 5)
#hboxGTFile$packStart(gtkLabel("Load GT data from .csv file"), expand = FALSE, fill = FALSE, padding = 5) 
#btn_browse <- gtkButton()
#gtkButtonSetLabel(btn_browse, "Browse...")
#btn_browse$AddCallback("clicked", select_file_GT)
#hboxGTFile$packStart(btn_browse, expand = FALSE, fill = FALSE, padding = 5)
# 
#lbl_GTFile <- gtkLabel("C:\\ ")
#hboxGTFile$packStart(lbl_GTFile, expand = FALSE, fill = FALSE, padding = 5)
# vboxGT$packStart(hboxGTFile, expand = FALSE, fill = FALSE, padding = 15)

hboxGT <- gtkHBox(FALSE, 5)
hboxGT$packStart(gtkLabel("Seed value for GT"), expand = FALSE, fill = FALSE, padding = 5) 

entry_GT_seedvalue <- gtkEntry() 
gtkEntrySetWidthChars(entry_GT_seedvalue, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetText(entry_GT_seedvalue, 0)  
hboxGT$packStart(entry_GT_seedvalue, expand = FALSE, fill = FALSE, padding = 5) 

btn_load_seed_GT <- gtkButton()
gtkButtonSetLabel(btn_load_seed_GT, "Load seed value")
btn_load_seed_GT$AddCallback("clicked", reload_GT_table)
hboxGT$packStart(btn_load_seed_GT, expand = FALSE, fill = FALSE, padding = 5) 

#btn_browse <- gtkButton()
#gtkButtonSetLabel(btn_browse, "Save GT data...")
#btn_browse$AddCallback("clicked", save_file_GT_vector)
# hboxGT$packStart(btn_browse, expand = FALSE, fill = FALSE, padding=0)

#vboxGT$packStart(hboxGT, expand = FALSE, fill = FALSE, padding = 5)           같같같같같같같같같같같같같같같같같같같같같같같같?
  
GT.sw <<- gtkScrolledWindowNew(NULL, NULL)
GT.sw$setShadowType("etched-in")
GT.sw$setPolicy("automatic", "automatic")
GT.sw$SetUsize(120, dim_eff_tables)  
GT <<- list()
GTIndex <<- 0
# ------------------------------
# create model
GT.create_model()
# create tree view
GT.treeview <<- gtkTreeViewNewWithModel(GT.model)
GT.treeview$setRulesHint(TRUE)
GT.treeview$getSelection()$setMode("single")
GT.add_columns(GT.treeview)
GT.sw$add(GT.treeview) 


 lbl_GTs <- gtkLabel("Monthly average GT")
 vboxGT$packStart(lbl_GTs , expand = FALSE, TRUE, 5)    
vboxGT$packStart(GT.sw , expand = FALSE, TRUE, 5) 

#hboxGTfile_save <- gtkHBox(FALSE, 5)
#btn_browse <- gtkButton()
#gtkButtonSetLabel(btn_browse, "Save GT data...")
#btn_browse$AddCallback("clicked", save_file_GT_vector)
#hboxGTfile_save$packStart(btn_browse, expand = FALSE, fill = FALSE, padding=0)
#vboxGT$packStart(hboxGTfile_save, expand = FALSE, fill = FALSE, padding = 0)
