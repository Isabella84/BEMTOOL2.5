# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

 
loadMCDAweightfromFile<-function(w) {
dialog <- gtkFileChooserDialog("Choose a CSV file", BMTmain_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
loaded_MCDAweight <- read.csv(dialog$getFilename(), sep=";", na.strings = "")  
vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
#gtkWidgetSetSensitive(BMTmain_window, FALSE)
#wnd <- showMessage("        Loading MCDA weight...        ")

MCDAweight_table <<- loaded_MCDAweight

#wnd$destroy()   
#gtkWidgetSetSensitive(BMTmain_window, TRUE)
wnd <- showMessageOK("        MCDA weight loaded!        ")

reload_MCDAweight()

} 
}
