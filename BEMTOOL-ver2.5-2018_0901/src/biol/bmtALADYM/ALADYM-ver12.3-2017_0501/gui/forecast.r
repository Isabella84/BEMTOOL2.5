# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




# ********************************* FORECAST graphical elements

vboxForecast <- gtkVBox(homogeneous = FALSE, 5)
 
notebook_forecast <- gtkNotebook()
notebook_forecast$setTabPos("top")

#vboxFleetTabs <- gtkVBox(homogeneous = FALSE, 5)


suppressWarnings(source(paste(ALADYM_home, "/gui/forecast/forecast.othersettings.r", sep="")) )
notebook_forecast$appendPage(hboxOtherSettings, gtkLabel(str=" OTHER SETTINGS "))

suppressWarnings(source(paste(ALADYM_home, "/gui/forecast/forecast.fleetTabs.r", sep="")) )
notebook_forecast$appendPage(hboxFleetTabs, gtkLabel(str=" FLEET SETTINGS "))

hboxForecast <- gtkHBox(homogeneous = FALSE, 5)
hboxForecast$packStart(notebook_forecast, expand = TRUE, fill = TRUE, 5)


lbl_nameF <- gtkLabel("Name of the forecast")  
entry_nameF <- gtkEntry() 
gtkEntrySetWidthChars(entry_nameF, 15) 

hboxForecast_name <- gtkHBox(homogeneous = FALSE, 5)
hboxForecast_name$packStart(lbl_nameF, expand = F, fill = F, 10)
hboxForecast_name$packStart(entry_nameF, expand = F, fill = F, 10)

vboxForecast$packStart(hboxForecast_name, expand = FALSE, fill = TRUE, 10)
vboxForecast$packStart(hboxForecast, expand = FALSE, fill = TRUE, 0)

if (IN_BEMTOOL) {
  gtkEntrySetText(entry_nameF, harvest_rule_level)
  gtkWidgetSetSensitive(entry_nameF, F)
}
#if (IN_BEMTOOL) {
#
#if (BMT_SCENARIO != BMT_HR_CHANGE_FISHMORTALITY) {
#gtkNotebookSetCurrentPage(notebook_forecast, 1)
#}
#
#}