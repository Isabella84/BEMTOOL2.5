# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



#
#
# ------------------------------------------------------------------------------
# Function for the selection of the file where values of activity are saved
# and the loading of those values in the table
# ------------------------------------------------------------------------------
#
select_file_FISHINGEFFORT_fore <- function(w) {
#print(".......................................... [forecast.fleetTabs.fishingeffortFun.r] --> select_file_FISHINGEFFORT_fore()")
dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
FISHINGEFFORT_file_fore <<- dialog$getFilename()
fleet.FISHINGEFFORT_fore  <<- read.csv(dialog$getFilename(), sep=";") 

if (nchar(dialog$getFilename()) < 57)  {
  lbl_FISHINGEFFORT_txt_fore  <- dialog$getFilename()
} else {
  lbl_FISHINGEFFORT_txt_fore  <- substring(dialog$getFilename(), (nchar(dialog$getFilename())-57), nchar(dialog$getFilename()))
}
print(".....................................................................................................................................................")
print(paste("...", lbl_FISHINGEFFORT_txt_fore, sep=""))
gtkLabelSetText(lbl_FISHINGEFFORTFile_fore , paste("...", lbl_FISHINGEFFORT_txt_fore , sep=""))
 
 #print("controllo")
  check_ <- check_input("FISHINGEFFORT_VECTOR_FORE", fleet.FISHINGEFFORT_fore  )

if (check_$result == "KO") {
     showError(check_$msg)
} else {  
#------------------------------------------ load the file

FISHINGEFFORT_fore  <<- list()
FISHINGEFFORT_foreIndex <<- 0
add.FISHINGEFFORT_fore()
  FISHINGEFFORT_fore.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 12), "gboolean")  
  for (i in 1:length(FISHINGEFFORT_fore)) {
    iter <-  FISHINGEFFORT_fore.model$append()$iter
     FISHINGEFFORT_fore.model$set(iter,0, FISHINGEFFORT_fore[[i]]$year)
    #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:length(MONTHS)) {
       FISHINGEFFORT_fore.model$set(iter, e, as.double(FISHINGEFFORT_fore[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     FISHINGEFFORT_fore.model$set(iter,13,TRUE)
  } 

FISHINGEFFORT_fore.treeview <<- gtkTreeViewNewWithModel( FISHINGEFFORT_fore.model)
FISHINGEFFORT_fore.treeview$setRulesHint(TRUE)
FISHINGEFFORT_fore.treeview$getSelection()$setMode("single")
FISHINGEFFORT_fore.add_columns( FISHINGEFFORT_fore.treeview)
FISHINGEFFORT_fore.sw$destroy()
FISHINGEFFORT_fore.sw <<- gtkScrolledWindowNew(NULL, NULL)
FISHINGEFFORT_fore.sw$setShadowType("etched-in")
FISHINGEFFORT_fore.sw$setPolicy("automatic", "automatic")
FISHINGEFFORT_fore.sw$SetUsize(100, 120)  
FISHINGEFFORT_fore.sw$add(FISHINGEFFORT_fore.treeview)
vboxFISHINGEFFORT_fore$packStart(FISHINGEFFORT_fore.sw , TRUE, TRUE, 0)
gtkBoxReorderChild(vboxFISHINGEFFORT_fore, FISHINGEFFORT_fore.sw, 3)
gtkBoxReorderChild(vboxFISHINGEFFORT_fore, hboxFISHINGEFFORTfile_save_fore, 3)   
}
dialog$destroy()
} else {
dialog$destroy()
}
} 