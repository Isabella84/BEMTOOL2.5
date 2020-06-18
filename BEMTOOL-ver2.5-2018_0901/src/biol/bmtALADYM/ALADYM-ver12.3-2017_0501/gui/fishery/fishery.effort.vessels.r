# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





#hboxVESSELSFile <- gtkHBox(FALSE, 5)
#hboxVESSELSFile$packStart(gtkLabel("Load VESSELS data from .csv file"), expand = FALSE, fill = FALSE, padding = 5) 
#btn_browse <- gtkButton()
#gtkButtonSetLabel(btn_browse, "Browse...")
#btn_browse$AddCallback("clicked", select_file_VESSELS)
#hboxVESSELSFile$packStart(btn_browse, expand = FALSE, fill = FALSE, padding = 5)
# 
#lbl_VESSELSFile <- gtkLabel("C:\\ ")
#hboxVESSELSFile$packStart(lbl_VESSELSFile, expand = FALSE, fill = FALSE, padding = 5)
# vboxVESSELS$packStart(hboxVESSELSFile, expand = FALSE, fill = FALSE, padding = 15)

#i=0 # column 0
#j=0 # row 0                                                         
#tblEFFORTvariables$Attach(gtkLabel("Seed value for VESSELS"),i, i+1, j, j+1)
#j=j+1 # row 1          
#
#entry_VESSELS_seedvalue <- gtkEntry() 
#gtkEntrySetWidthChars(entry_VESSELS_seedvalue, NUMERICAL_ENTRY_LENGTH) 
#gtkEntrySetText(entry_VESSELS_seedvalue, 0)  
#                                               
#tblEFFORTvariables$Attach(entry_VESSELS_seedvalue,i, i+1, j, j+1)
#j=j+1 # row 2 
#
#btn_load_seed_VESSELS <- gtkButton()
#gtkButtonSetLabel(btn_load_seed_VESSELS, "Load seed value")
#btn_load_seed_VESSELS$AddCallback("clicked", reload_VESSELS_table)
#                                                        
#tblEFFORTvariables$Attach(btn_load_seed_VESSELS,i, i+1, j, j+1)
#
#i=i+1 # column 0
#j=0 # row 0   
#
#VESSELS.sw <<- gtkScrolledWindowNew(NULL, NULL)
#VESSELS.sw$setShadowType("etched-in")
#VESSELS.sw$setPolicy("automatic", "automatic")
#VESSELS.sw$SetUsize(120, 90)  
#VESSELS <<- list()
#VESSELSIndex <<- 0
## ------------------------------
## create model
#VESSELS.create_model()
## create tree view
#VESSELS.treeview <<- gtkTreeViewNewWithModel(VESSELS.model)
#VESSELS.treeview$setRulesHint(TRUE)
#VESSELS.treeview$getSelection()$setMode("single")
#VESSELS.add_columns(VESSELS.treeview)
#VESSELS.sw$add(VESSELS.treeview)    
#vboxVESSELS$packStart(VESSELS.sw , expand = FALSE, TRUE, 0) 
#                                                      
#tblEFFORTvariables$Attach(vboxVESSELS,i, i+1, j, j+1)
#



hboxVESSELS <- gtkHBox(FALSE, 5)
hboxVESSELS$packStart(gtkLabel("Seed value for VESSELS"), expand = FALSE, fill = FALSE, padding = 5) 

entry_VESSELS_seedvalue <- gtkEntry() 
gtkEntrySetWidthChars(entry_VESSELS_seedvalue, NUMERICAL_ENTRY_LENGTH) 
gtkEntrySetText(entry_VESSELS_seedvalue, 0)  
hboxVESSELS$packStart(entry_VESSELS_seedvalue, expand = FALSE, fill = FALSE, padding = 5) 

btn_load_seed_VESSELS <- gtkButton()
gtkButtonSetLabel(btn_load_seed_VESSELS, "Load seed value")
btn_load_seed_VESSELS$AddCallback("clicked", reload_VESSELS_table)
hboxVESSELS$packStart(btn_load_seed_VESSELS, expand = FALSE, fill = FALSE, padding = 5)

#btn_browse <- gtkButton()
#gtkButtonSetLabel(btn_browse, "Save VESSELS data...")
#btn_browse$AddCallback("clicked", save_file_VESSELS_vector)
# hboxVESSELS$packStart(btn_browse, expand = FALSE, fill = FALSE, padding=0) 

#vboxVESSELS$packStart(hboxVESSELS, expand = FALSE, fill = FALSE, padding = 5)          같같같같같같같같같같같같같같같같같같같같같같같같?
  
VESSELS.sw <<- gtkScrolledWindowNew(NULL, NULL)
VESSELS.sw$setShadowType("etched-in")
VESSELS.sw$setPolicy("automatic", "automatic")
VESSELS.sw$SetUsize(120, dim_eff_tables)  
VESSELS <<- list()
VESSELSIndex <<- 0
# ------------------------------
# create model
VESSELS.create_model()
# create tree view
VESSELS.treeview <<- gtkTreeViewNewWithModel(VESSELS.model)
VESSELS.treeview$setRulesHint(TRUE)
VESSELS.treeview$getSelection()$setMode("single")
VESSELS.add_columns(VESSELS.treeview)
VESSELS.sw$add(VESSELS.treeview) 


lbl_VESSELS <- gtkLabel("Monthly VESSELS")

vboxVESSELS$packStart(lbl_VESSELS, expand=FALSE, FALSE, 5)    
vboxVESSELS$packStart(VESSELS.sw , expand = FALSE, TRUE, 5) 

#hboxVESSELSfile_save <- gtkHBox(FALSE, 5)
#btn_browse <- gtkButton()
#gtkButtonSetLabel(btn_browse, "Save VESSELS data...")
#btn_browse$AddCallback("clicked", save_file_VESSELS_vector)
#hboxVESSELSfile_save$packStart(btn_browse, expand = FALSE, fill = FALSE, padding=0)
#vboxVESSELS$packStart(hboxVESSELSfile_save, expand = FALSE, fill = FALSE, padding = 0)
