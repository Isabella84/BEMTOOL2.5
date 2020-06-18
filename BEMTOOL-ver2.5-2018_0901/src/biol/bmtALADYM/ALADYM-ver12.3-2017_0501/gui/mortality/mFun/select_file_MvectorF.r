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
#
#
#
# ------------------------------------------------------------------------------
# Function for the selection of the file where values of natural mortality are 
# saved and the loading of those values in the table (FEMALES)
# ------------------------------------------------------------------------------
#
select_file_MvectorF <- function(w) {
dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
naturalmortality_file <<- dialog$getFilename()

vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Loading natural mortality for females...        ")

mortality.Mvector.females <<- read.csv(naturalmortality_file, sep=";", na.strings = "")

check_ <- check_input("NATURAL_MORTALITY_VECTOR_F", mortality.Mvector.females)

if (check_$result == "KO") {
wnd$destroy() 
     showError(check_$msg)
} else { 
#------------------------------------------ load the file
Mvector_F <<- list()
Mvector_FIndex <<- 0
add.Mvector_F()
  Mvector_F.model <<- gtkListStoreNew("gchararray", "gdouble", "gboolean")  
  for (i in 1:length(Mvector_F)) {
    iter <-  Mvector_F.model$append()$iter
     Mvector_F.model$set(iter,0, as.character(Mvector_F[[i]]$age_month))
     Mvector_F.model$set(iter, 1, as.double(Mvector_F[[i]]$M))          # as.double(sexratios[[ind]][nc_i+1]) 
     Mvector_F.model$set(iter,2,TRUE)
  } 

  Mvector_F.treeview$destroy()
 Mvector_F.treeview <<- gtkTreeViewNewWithModel( Mvector_F.model)
 Mvector_F.treeview$setRulesHint(TRUE)
 Mvector_F.treeview$getSelection()$setMode("single")
Mvector_F.add_columns( Mvector_F.treeview)
Mvector_F.sw$add(Mvector_F.treeview)
wnd$destroy() 
showMessageOK("        Natural mortality for females loaded!        ")

}
gtkWidgetSetSensitive(main_window, TRUE)

} 
} 