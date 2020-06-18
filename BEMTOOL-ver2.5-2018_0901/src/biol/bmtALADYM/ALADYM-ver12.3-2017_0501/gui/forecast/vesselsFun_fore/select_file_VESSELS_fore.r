# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



#
#
#
# ------------------------------------------------------------------------------
# Function for the selection of the file where values of activity are saved
# and the loading of those values in the table
# ------------------------------------------------------------------------------
#
select_file_VESSELS_fore <- function(w) {
#print(".......................................... [forecast.fleetTabs.vesselsFun.r] --> select_file_VESSELS_fore()")
dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
VESSELS_file_fore <<- dialog$getFilename()
fleet.VESSELS_fore  <<- read.csv(dialog$getFilename(), sep=";") 

if (nchar(dialog$getFilename()) < 57)  {
  lbl_VESSELS_txt_fore  <- dialog$getFilename()
} else {
  lbl_VESSELS_txt_fore  <- substring(dialog$getFilename(), (nchar(dialog$getFilename())-57), nchar(dialog$getFilename()))
}
print(paste("...", lbl_VESSELS_txt_fore, sep=""))
gtkLabelSetText(lbl_VESSELSFile_fore , paste("...", lbl_VESSELS_txt_fore , sep=""))
 
 #print("controllo")
  check_ <- check_input("VESSELS_VECTOR_FORE", fleet.VESSELS_fore  )

if (check_$result == "KO") {
     showError(check_$msg)
} else {  
#------------------------------------------ load the file

VESSELS_fore  <<- list()
VESSELS_foreIndex <<- 0
add.VESSELS_fore()
  VESSELS_fore.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 12), "gboolean")  
  for (i in 1:length(VESSELS_fore)) {
    iter <-  VESSELS_fore.model$append()$iter
     VESSELS_fore.model$set(iter,0, VESSELS_fore[[i]]$year)
    #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:length(MONTHS)) {
       VESSELS_fore.model$set(iter, e, as.double(VESSELS_fore[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     VESSELS_fore.model$set(iter,13,TRUE)
  } 

VESSELS_fore.treeview <<- gtkTreeViewNewWithModel( VESSELS_fore.model)
VESSELS_fore.treeview$setRulesHint(TRUE)
VESSELS_fore.treeview$getSelection()$setMode("single")
VESSELS_fore.add_columns( VESSELS_fore.treeview)
VESSELS_fore.sw$destroy()
VESSELS_fore.sw <<- gtkScrolledWindowNew(NULL, NULL)
VESSELS_fore.sw$setShadowType("etched-in")
VESSELS_fore.sw$setPolicy("automatic", "automatic")
VESSELS_fore.sw$SetUsize(100, 120)  
VESSELS_fore.sw$add(VESSELS_fore.treeview)
vboxVESSELS_fore$packStart(VESSELS_fore.sw , TRUE, TRUE, 0)
gtkBoxReorderChild(vboxVESSELS_fore, VESSELS_fore.sw, 3)
gtkBoxReorderChild(vboxVESSELS_fore, hboxVESSELSfile_save_fore, 3)   
}
dialog$destroy()
} else {
dialog$destroy()
}
} 
