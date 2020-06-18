# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



#
# ------------------------------------------------------------------------------
# Function for the selection of the file where values of activity are saved
# and the loading of those values in the table
# ------------------------------------------------------------------------------
#
select_file_GT_fore <- function(w) {
#print(".......................................... [forecast.fleetTabs.gtFun.r] --> select_file_GT_fore()")
dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
GT_file_fore <<- dialog$getFilename()
fleet.GT_fore  <<- read.csv(dialog$getFilename(), sep=";") 

if (nchar(dialog$getFilename()) < 57)  {
  lbl_GT_txt_fore  <- dialog$getFilename()
} else {
  lbl_GT_txt_fore  <- substring(dialog$getFilename(), (nchar(dialog$getFilename())-57), nchar(dialog$getFilename()))
}
print(paste("...", lbl_GT_txt_fore, sep=""))
gtkLabelSetText(lbl_GTFile_fore , paste("...", lbl_GT_txt_fore , sep=""))
 
 #print("controllo")
  check_ <- check_input("GT_VECTOR_FORE", fleet.GT_fore  )

if (check_$result == "KO") {
     showError(check_$msg)
} else {  
#------------------------------------------ load the file

GT_fore  <<- list()
GT_foreIndex <<- 0
add.GT_fore()
  GT_fore.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 12), "gboolean")  
  for (i in 1:length(GT_fore)) {
    iter <-  GT_fore.model$append()$iter
     GT_fore.model$set(iter,0, GT_fore[[i]]$year)
    #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:length(MONTHS)) {
       GT_fore.model$set(iter, e, as.double(GT_fore[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     GT_fore.model$set(iter,13,TRUE)
  } 

GT_fore.treeview <<- gtkTreeViewNewWithModel( GT_fore.model)
GT_fore.treeview$setRulesHint(TRUE)
GT_fore.treeview$getSelection()$setMode("single")
GT_fore.add_columns( GT_fore.treeview)
GT_fore.sw$destroy()
GT_fore.sw <<- gtkScrolledWindowNew(NULL, NULL)
GT_fore.sw$setShadowType("etched-in")
GT_fore.sw$setPolicy("automatic", "automatic")
GT_fore.sw$SetUsize(100, 120)  
GT_fore.sw$add(GT_fore.treeview)
vboxGT_fore$packStart(GT_fore.sw , expand = FALSE, TRUE, 0)
gtkBoxReorderChild(vboxGT_fore, GT_fore.sw, 3)
# gtkBoxReorderChild(vboxGT_fore, hboxGTfile_save_fore, 3)   
}
dialog$destroy()
} else {
dialog$destroy()
}
} 
