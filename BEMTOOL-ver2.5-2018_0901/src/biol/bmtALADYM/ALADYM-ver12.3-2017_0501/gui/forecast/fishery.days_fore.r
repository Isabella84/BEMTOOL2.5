# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





# vboxActivity$packStart(hboxActivity, expand = FALSE, fill = FALSE, padding = 5)
#print(".......................................... [forecast.fleetTabs.DAYS.r]")
hboxDAYSFile_fore <- gtkHBox(FALSE, 5)
hboxDAYSFile_fore$packStart(gtkLabel("Load DAYS for forecast from .csv file"), expand = FALSE, fill = FALSE, padding = 5) 
btn_browse <- gtkButton()
gtkButtonSetLabel(btn_browse, "Browse...")
btn_browse$AddCallback("clicked", select_file_DAYS_fore)
hboxDAYSFile_fore$packStart(btn_browse, expand = FALSE, fill = FALSE, padding = 5)
 
lbl_DAYSFile_fore <- gtkLabel("C:\\ ")
hboxDAYSFile_fore$packStart(lbl_DAYSFile_fore, expand = FALSE, fill = FALSE, padding = 5)


#hboxDAYSfile_save_fore <- gtkHBox(FALSE, 5)
btn_browse <- gtkButton()
gtkButtonSetLabel(btn_browse, "Save DAYS parameters...")
btn_browse$AddCallback("clicked", save_file_DAYS_vector_fore)
hboxDAYSFile_fore$packStart(btn_browse, expand = FALSE, fill = FALSE, padding=5)
#hboxDAYSfile_save_fore$packStart(btn_browse, expand = FALSE, fill = FALSE, padding=5)
#vboxDAYS_fore$packStart(hboxDAYSfile_save_fore, expand = FALSE, fill = FALSE, padding = 5)
#


#vboxDAYS_fore$packStart(hboxDAYSFile_fore, expand = FALSE, fill = FALSE, padding = 5)
  
DAYS_fore.sw <<- gtkScrolledWindowNew(NULL, NULL)
DAYS_fore.sw$setShadowType("etched-in")
DAYS_fore.sw$setPolicy("automatic", "automatic")
DAYS_fore.sw$SetUsize(100, dim_eff_tables)  
DAYS_fore <<- list()
DAYS_foreIndex <<- 0
# ------------------------------
# create model
DAYS_fore.create_model()
# create tree view
DAYS_fore.treeview <<- gtkTreeViewNewWithModel(DAYS_fore.model)
DAYS_fore.treeview$setRulesHint(TRUE)
DAYS_fore.treeview$getSelection()$setMode("single")
DAYS_fore.add_columns(DAYS_fore.treeview)
DAYS_fore.sw$add(DAYS_fore.treeview)    

 lbl_fore_DAYS <- gtkLabel("Monthly average DAYS")



vboxDAYS_fore$packStart(lbl_fore_DAYS , expand=FALSE, FALSE, 0) 

vboxDAYS_fore$packStart(DAYS_fore.sw , expand=FALSE, FALSE, 0) 


# gtkWidgetSetSensitive(lbl_fore_DAYS, FALSE)
#  gtkWidgetSetSensitive(DAYS_fore.treeview, FALSE)