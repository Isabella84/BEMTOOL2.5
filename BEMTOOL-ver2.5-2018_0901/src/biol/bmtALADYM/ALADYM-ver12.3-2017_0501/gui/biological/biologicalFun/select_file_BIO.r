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
select_file_BIO <- function(w) {
dialog <- gtkFileChooserDialog("Choose the parameters .CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
CONFIGURATION_file <<- dialog$getFilename()
 # CONFIGURATION_file <<- "C:/BEMTOOL GSA18/GSA18 ALADYM files/MULL BAR_configuration file_ALADYM.csv"
  # CONFIGURATION_file <<- "C:/Isabella/MAREA/LANDMED/WP4/ADRIATIC CASE STUDY/INPUT/aaa_configuration file_ALADYM - ANE.csv"
BIOparameters_table <<- data.frame(read.csv(CONFIGURATION_file, sep=";", na.strings = "") , stringsAsFactors = F )

 vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)                  # -------------------------------
wnd <- showMessage("        Loading configuration...        ")   #  --------------------------------------
##
suppressWarnings(setBiologicalParams(BIOparameters_table) )

#loadFleetsegmentintoGUI(FleetList_simulation[[1]]) 
if (IN_BEMTOOL & phase=="SIMULATION") {
lockFisheryValues()
}

gtkComboBoxSetActive(combo_fleetsegments, 0 )

#------------------------------------------ load the values
# if (exists("wnd") ) {
wnd$destroy()                                              # ---------------------------------------
gtkWidgetSetSensitive(main_window, TRUE)                 # ---------------------------------------  
#}
showMessageOK("        Configuration loaded!        ")

} 



}
