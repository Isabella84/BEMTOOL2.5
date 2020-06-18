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
select_file_DAYS_fore <- function(w) {
#print(".......................................... [forecast.fleetTabs.daysFun.r] --> select_file_DAYS_fore()")
dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
DAYS_file_fore <<- dialog$getFilename()
fleet.DAYS_fore  <<- read.csv(dialog$getFilename(), sep=";") 

if (nchar(dialog$getFilename()) < 57)  {
  lbl_DAYS_txt_fore  <- dialog$getFilename()
} else {
  lbl_DAYS_txt_fore  <- substring(dialog$getFilename(), (nchar(dialog$getFilename())-57), nchar(dialog$getFilename()))
}
#print(paste("...", lbl_DAYS_txt_fore, sep=""))
gtkLabelSetText(lbl_DAYSFile_fore , paste("...", lbl_DAYS_txt_fore , sep=""))
 
 #print("controllo")
  check_ <- check_input("DAYS_VECTOR_FORE", fleet.DAYS_fore  )

if (check_$result == "KO") {
     showError(check_$msg)
} else {  
#------------------------------------------ load the file

DAYS_fore  <<- list()
DAYS_foreIndex <<- 0
add.DAYS_fore()
  DAYS_fore.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 12), "gboolean")  
  for (i in 1:length(DAYS_fore)) {
    iter <-  DAYS_fore.model$append()$iter
     DAYS_fore.model$set(iter,0, DAYS_fore[[i]]$year)
    #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:length(MONTHS)) {
       DAYS_fore.model$set(iter, e, as.double(DAYS_fore[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     DAYS_fore.model$set(iter,13,TRUE)
  } 

DAYS_fore.treeview <<- gtkTreeViewNewWithModel( DAYS_fore.model)
DAYS_fore.treeview$setRulesHint(TRUE)
DAYS_fore.treeview$getSelection()$setMode("single")
DAYS_fore.add_columns( DAYS_fore.treeview)
DAYS_fore.sw$destroy()
DAYS_fore.sw <<- gtkScrolledWindowNew(NULL, NULL)
DAYS_fore.sw$setShadowType("etched-in")
DAYS_fore.sw$setPolicy("automatic", "automatic")
DAYS_fore.sw$SetUsize(100, 350)  
DAYS_fore.sw$add(DAYS_fore.treeview)
vboxDAYS_fore$packStart(DAYS_fore.sw , TRUE, TRUE, 0)
gtkBoxReorderChild(vboxDAYS_fore, DAYS_fore.sw, 3)
gtkBoxReorderChild(vboxDAYS_fore, hboxDAYSfile_save_fore, 3)   
}
dialog$destroy()
} else {
dialog$destroy()
}
} 