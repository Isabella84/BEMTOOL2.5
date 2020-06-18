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
# Function for the selection of the file where values of discard are saved
# and the loading of those values in the table
# ------------------------------------------------------------------------------
#
select_file_discard_fore <- function(w) {
dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
discard_file_fore <<- dialog$getFilename()
fleet.discard_fore <<- read.csv(dialog$getFilename(), sep=";", na.strings = "")
   #print(fleet.discard)
if (nchar(dialog$getFilename()) < 57)  {
  lbl_discardFile_txt_fore <- dialog$getFilename()
} else {
  lbl_discardFile_txt_fore <- substring(dialog$getFilename(), (nchar(dialog$getFilename())-57), nchar(dialog$getFilename()))
}
#print(paste("...",lbl_discardFile_txt_fore, spe=""))
gtkLabelSetText(lbl_discardFile_fore, paste("...", lbl_discardFile_txt_fore, sep=""))

check_ <- check_input("DISCARD_TABLE_FORE", fleet.discard_fore)

if (check_$result == "KO") {
    showError(check_$msg)
} else { 
#------------------------------------------ load the file
discards_fore <<- list()
discards_foreIndex <<- 0
add.discards_fore()
  discards_fore.model <<- gtkListStoreNew("gchararray", "gchararray", rep("gdouble", 2), "gboolean")  
  for (i in 1:length(discards_fore)) {
    iter <-  discards_fore.model$append()$iter
     discards_fore.model$set(iter,0, discards_fore[[i]]$year)
    discards_fore.model$set(iter, 1, discards_fore[[i]]$month)           
   for (e in 1:2) {
        discards_fore.model$set(iter, e+1, as.double(discards_fore[[i]][e+2]))          # as.double(sexratios[[ind]][nc_i+1]) 
       # print(paste("n°", e+1, "in model:", as.double(selectivities[[i]][e+2])) )
    }
     discards_fore.model$set(iter,4,TRUE)
  } 

 discards_fore.treeview <<- gtkTreeViewNewWithModel( discards_fore.model)
 discards_fore.treeview$setRulesHint(TRUE)
 discards_fore.treeview$getSelection()$setMode("single")
discards_fore.add_columns( discards_fore.treeview)
discards_fore.sw$destroy()
discards_fore.sw <<- gtkScrolledWindowNew(NULL, NULL)
discards_fore.sw$setShadowType("etched-in")
discards_fore.sw$setPolicy("automatic", "automatic")
discards_fore.sw$SetUsize(100, 350)  
discards_fore.sw$add(discards_fore.treeview)
vboxDiscard_fore$packStart(discards_fore.sw , TRUE, TRUE, 0)

gtkBoxReorderChild(vboxDiscard_fore, discards_fore.sw, 3)
gtkBoxReorderChild(vboxDiscard_fore, hboxDiscardfile_save_fore, 3)

}
dialog$destroy()
} else {
dialog$destroy()
}
} 