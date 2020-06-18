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
# Function for the selection of the file where values of natural mortality are 
# saved and the loading of those values in the table (MALES)
# ------------------------------------------------------------------------------
#
select_file_MvectorM <- function(w) {
dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
# naturalmortality_file <- "C:/FACCHINI_MT/ALADYM - sw/ALADYM-ver10.0.3d-2014_0403/Natural_mortality_PAPELON_tr_0.CSV"
naturalmortality_file <<- dialog$getFilename()
 vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Loading natural mortality for males...        ")

mortality.Mvector.males <<- read.csv(naturalmortality_file, sep=";", na.strings = "")

check_ <- check_input("NATURAL_MORTALITY_VECTOR_M", mortality.Mvector.males)

if (check_$result == "KO") {
wnd$destroy() 
     showError(check_$msg)
} else { 
#------------------------------------------ load the file

Mvector_M <<- list()
Mvector_MIndex <<- 0
add.Mvector_M()
  Mvector_M.model <<- gtkListStoreNew("gchararray", "gdouble", "gboolean")  
  for (i in 1:length(Mvector_M)) {
    iter <-  Mvector_M.model$append()$iter
     Mvector_M.model$set(iter, 0, as.character(Mvector_M[[i]]$age_month))
     Mvector_M.model$set(iter, 1, as.double(Mvector_M[[i]]$M))          # as.double(sexratios[[ind]][nc_i+1]) 
     Mvector_M.model$set(iter, 2,TRUE)
  } 

   Mvector_M.treeview$destroy()
 Mvector_M.treeview <<- gtkTreeViewNewWithModel( Mvector_M.model)
 Mvector_M.treeview$setRulesHint(TRUE)
 Mvector_M.treeview$getSelection()$setMode("single")
Mvector_M.add_columns( Mvector_M.treeview)
Mvector_M.sw$add(Mvector_M.treeview)

wnd$destroy() 
showMessageOK("        Natural mortality for males loaded!        ")
}
gtkWidgetSetSensitive(main_window, TRUE)


} 
} 