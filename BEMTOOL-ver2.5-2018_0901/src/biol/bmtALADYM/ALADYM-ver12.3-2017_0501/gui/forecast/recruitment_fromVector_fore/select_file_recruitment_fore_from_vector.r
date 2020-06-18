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
# Function for the selection of the file where values of stock-recruitment_fore_from_vector are 
# saved and the loading of those values in the table
# ------------------------------------------------------------------------------
#
select_file_recruitment_fore_from_vector <- function(w) {
dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
recruitment_fore_from_vector_file <<- dialog$getFilename()

vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Loading recruitment_fore_from_vector...        ")

recruitments_fore_from_vector.vector <<- read.csv(recruitments_fore_from_vector_file, sep=";")

check_ <- check_input("RECRUITMENT_VECTOR_FORE", recruitments_fore_from_vector.vector)

if (check_$result == "KO") {
    showError(check_$msg)
} else { 
#------------------------------------------ load the file

recruitments_fore_from_vector <<- list()
recruitments_fore_from_vector_index <<- 0
add.recruitments_fore_from_vector()
  recruitments_fore_from_vector.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 12), "gboolean")  
  for (i in 1:length(recruitments_fore_from_vector)) {
    iter <-  recruitments_fore_from_vector.model$append()$iter
     recruitments_fore_from_vector.model$set(iter,0, recruitments_fore_from_vector[[i]]$year)
    #print(paste("in model:", as.character(recruitments_fore_from_vector[[i]]$year)))
    for (e in 1:length(MONTHS)) {
        recruitments_fore_from_vector.model$set(iter, e, as.double(recruitments_fore_from_vector[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
       #print(paste("in model:", recruitments_fore_from_vector[[i]][e]) )
    }
     recruitments_fore_from_vector.model$set(iter,13,TRUE)
  } 

  recruitments_fore_from_vector.treeview$destroy()

 recruitments_fore_from_vector.treeview <<- gtkTreeViewNewWithModel( recruitments_fore_from_vector.model)
 recruitments_fore_from_vector.treeview$setRulesHint(TRUE)
 recruitments_fore_from_vector.treeview$getSelection()$setMode("single")
recruitments_fore_from_vector.add_columns( recruitments_fore_from_vector.treeview)
#recruitment_fore_from_vector.sw$destroy()
#recruitment_fore_from_vector.sw <<- gtkScrolledWindowNew(NULL, NULL)
#recruitment_fore_from_vector.sw$setShadowType("etched-in")
#recruitment_fore_from_vector.sw$setPolicy("automatic", "automatic")
#recruitment_fore_from_vector.sw$SetUsize(100, dim_eff_tables)  
recruitments_fore_from_vector.sw$add(recruitments_fore_from_vector.treeview)
#vboxSRrelationship$packStart(recruitment_fore_from_vector.sw , TRUE, TRUE, 0)
#
#gtkBoxReorderChild(vboxSRrelationship, recruitment_fore_from_vector.sw, 4)
#gtkBoxReorderChild(vboxSRrelationship, hboxStockRfile_save, 4)

}


wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Recruitment for forecast from vector loaded!        ")


} 
} 
