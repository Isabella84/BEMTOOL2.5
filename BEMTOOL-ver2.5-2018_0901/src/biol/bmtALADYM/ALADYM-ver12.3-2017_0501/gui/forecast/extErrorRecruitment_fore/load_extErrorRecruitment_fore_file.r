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
# Function for the selection of the file where values of stock-recruitment are 
# saved and the loading of those values in the table
# ------------------------------------------------------------------------------
#
load_extErrorRecruitment_fore_file <- function(w) {
dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
extErrorRecruitment_fore_file <<- dialog$getFilename()

vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Loading recruitment error...        ")

CI_external_matrix_fore <<- read.csv(extErrorRecruitment_fore_file, sep=";")
CI_NB_RUNS_FORE <<-  as.numeric(as.character(gtkEntryGetText(entry_CI_numb_runs_fore))) 

loca_go_on <- T
 if (loca_go_on) {

   if (!is.na(CI_NB_RUNS_FORE)) {
        if ( !is.numeric(CI_NB_RUNS_FORE) ) {
                     loca_go_on <- F  
        }
   }  else {
                    loca_go_on <- F  
   }
   }


if (!loca_go_on) {
#    showError("The number of the recruitment errors is not consistent with the inserted number of runs!")
wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Impossible load the recruitment error without setting no. of runs!        ")
} else { 
#------------------------------------------ load the file
reload_extErrorRecruitment_fore()
wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Recruitment error loaded!        ")


}



} 
} 
