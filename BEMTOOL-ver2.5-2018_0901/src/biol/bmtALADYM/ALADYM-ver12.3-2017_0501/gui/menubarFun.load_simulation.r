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
load_simulation <- function(widget, window) {
dialog <- gtkFileChooserDialog("Choose the workspace .Rdata file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
work_space <- dialog$getFilename()
  # work_space <- "C:\\Isabella\\MAREA\\LANDMED\\WP4\\BEMTOOL-ver2-2014-MTF\\src\\biol\\bmtALADYM\\ALADYM-ver10.1.0-2014_19092014\\working files\\E. encrasicolus.Rdata"
  
vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
#deactivate_main_w()
wnd <- showMessage("        Loading SIMULATION workspace...        ")

# ptm <- proc.time()  

gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook, 1), TRUE)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook, 2), TRUE)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook, 3), TRUE)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook, 4), TRUE)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook, 5), TRUE)
gtkWidgetSetSensitive(btn_runScenario, TRUE)

# print(FleetList_forecast[[1]]@selectivity.vector)

# BIOparameters_table <- read.csv(CONFIGURATION_file, sep=";", na.strings = "")
# BIOparameters_table <- read.csv("C:\\FACCHINI_MT\\ALADYM - sw\\ALADYM-ver10.0.3a-2014_2502\\input test1\\ALADYM biological params - Test1.csv", sep=";")

setBiologicalParams_fromEnv(work_space)

# print(FleetList_forecast[[1]]@selectivity.vector)

new_aldSimulation <<- new_aldSimulation
 new_aldForecast <<- new_aldForecast
 new_aldPopulation <<-  new_aldPopulation
 FleetList_simulation <<- FleetList_simulation
 FleetList_forecast <<- FleetList_forecast
 
 

loadFleetsegment_foreintoGUI(FleetList_forecast[[1]])
 #  gtkComboBoxSetActive(combo_fleetsegments_fore, which(FLEETSEGMENTS_names == object@fleetname)-1)

gtkEntrySetText(entry_costant_Linf_f, BAS$F_mean_Linf )
gtkEntrySetText(entry_costant_Linf_m, BAS$M_mean_Linf )
gtkEntrySetText(entry_costant_k_f, BAS$F_mean_k )
gtkEntrySetText(entry_costant_k_m, BAS$M_mean_k )
gtkEntrySetText(entry_costant_t0_f, BAS$F_mean_t0 )
gtkEntrySetText(entry_costant_t0_m, BAS$M_mean_t0 )
gtkEntrySetText(entry_costant_males_L50, BAS$M_mean_L50 )
gtkEntrySetText(entry_costant_females_L50, BAS$F_mean_L50 )
gtkEntrySetText(entry_costant_males_MR, BAS$M_mean_MR )
gtkEntrySetText(entry_costant_females_MR, BAS$F_mean_MR )

last_year_recruitment <- round(meanWequals(INP$Recruits,(length(years)*12+1), INP$Time_slice)[length(years)], 0)

gtkEntrySetText(entry_costant_recr_forecast, last_year_recruitment )
gtkEntrySetText(entry_costant_recr_forecast_UN, last_year_recruitment )

# print(proc.time() - ptm, quote=F )

wnd$destroy()   
activate_main_w()
wnd <- showMessageOK("        SIMULATION workspace loaded!        ")

if (!IN_BEMTOOL) {
gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook, 5), TRUE)
gtkNotebookSetCurrentPage(notebook, 5)
gtkWidgetSetSensitive(btn_runScenario, TRUE)
}

}


}
