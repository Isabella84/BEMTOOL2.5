# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





clear_ForecastGUI<-function(w) {
#print(".......................................... [clear_ForecastGUI.r] --> clear_ForecastGUI()")
fleet.selectivity_fore <<- NULL
suppressWarnings(source(paste(ALADYM_home, "/gui/forecast/forecast.fleetTabs.selectivityTable.r", sep="")) )
  #gtkToggleButtonSetActive(radio_classicalogive, TRUE) 
# gtkLabelSetText(lbl_Selfile_fore, "C:\\ ")
 selectivity_file_fore <<- ""
  
fleet.discard_fore <<- NULL
suppressWarnings(source(paste(ALADYM_home, "/gui/forecast/forecast.fleetTabs.discardTable.r", sep="")) )
#gtkComboBoxSetActive(combo_discard, 0 )
# gtkLabelSetText(lbl_discardFile_fore, "C:\\ ")
discard_file_fore <<- ""

fleet.FISHINGEFFORT_fore <<- NULL
suppressWarnings(source(paste(ALADYM_home, "/gui/forecast/forecast.fleetTabs.fishingeffortTable.r", sep="")) )
#  gtkLabelSetText(lbl_FISHINGEFFORTFile_fore, "C:\\ ")
  #gtkEntrySetText(entry_fact_seedvalue, 1)
 FISHINGEFFORT_file_fore <<- ""

fleet.GT_fore <<- NULL
suppressWarnings(source(paste(ALADYM_home, "/gui/forecast/forecast.fleetTabs.gtTable.r", sep=""))  )
#  gtkLabelSetText(lbl_FISHINGEFFORTFile_fore, "C:\\ ")
  #gtkEntrySetText(entry_fact_seedvalue, 1)
 #GT_file_fore <<- ""
 
 fleet.VESSELS_fore <<- NULL
suppressWarnings(source(paste(ALADYM_home, "/gui/forecast/forecast.fleetTabs.vesselsTable.r", sep=""))  )
#  gtkLabelSetText(lbl_FISHINGEFFORTFile_fore, "C:\\ ")
  #gtkEntrySetText(entry_fact_seedvalue, 1)
 #GT_file_fore <<- ""
 
  fleet.DAYS_fore <<- NULL
suppressWarnings(source(paste(ALADYM_home, "/gui/forecast/forecast.fleetTabs.daysTable.r", sep="")) )

 
}
