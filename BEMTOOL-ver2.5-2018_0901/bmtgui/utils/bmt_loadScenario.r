# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



#
#
#
#
#
#
#
#



bmt_loadScenario <- function(w) {

dialog <- gtkFileChooserDialog("BEMTOOL 2.1 - Choose the SCENARIO bmtcfg.csv file", BMTmain_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
path <- dialog$getFilename()

vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
 bmt_wnd_load_ecoparams <- showMessage("Loading SCENARIO configuration...") 
gtkWidgetSetSensitive(BMTmain_window, F)

#path <- "C:\\FACCHINI_MT\\_MAREA - SC11 LANDMED - BEMTOOL sw\\BEMTOOL app-ver1.4-2014\\BEMTOOL-ver2-2014\\bmtconfig.csv"
   LOADED_SCENARIO <<- T
   loadScenariocfg(path)
   
 bmt_wnd_load_ecoparams$destroy()
 wnd <- showMessageOK("        Scenario loaded!        ")
gtkWidgetSetSensitive(BMTmain_window, T)

     }    

}
