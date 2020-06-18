# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





clear_FisheryGUI<-function(w) {
#print(".......................................... [clear_FisheryGUI.r] --> clear_FisheryGUI()")

if (new_aldSimulation@enteringMortality != "F") {
fleet.selectivity <<- NULL
reload_selectivity()
#source(paste(ALADYM_home, "/gui/fishery/fishery.selectivityTable.r", sep=""))
#  gtkToggleButtonSetActive(radio_classicalogive, TRUE) 
  }

fleet.discard <<- NULL
reload_discard_table()
#source(paste(ALADYM_home, "/gui/fishery/fishery.discardTable.r", sep=""))
gtkComboBoxSetActive(combo_discard, 2 ) 
deactivate_Discard_unused_params()

fleet.production <<- NULL
reload_EMPTY_production_table()
#source(paste(ALADYM_home, "/gui/fishery/fishery.productionTable.r", sep=""))
gtkToggleButtonSetActive(radio_data, TRUE) 
gtkEntrySetText(entry_Production_seedvalue, 0)  


fleet.pproduction <<- NULL
reload_EMPTY_pproduction_table()
#source(paste(ALADYM_home, "/gui/fishery/fishery.productionTable.r", sep=""))
gtkEntrySetText(entry_pProduction_seedvalue, 0)  

mortality.Fvector.females <<- NULL
FishingMvector_F <<- list()
change_fishingM_F()
    
mortality.Fvector.males <<- NULL 
FishingMvector_M <<- list()    
change_fishingM_M()
   
fleet.VESSELS <<- NULL    
reload_EMPTY_VESSELS_table()
gtkEntrySetText(entry_VESSELS_seedvalue, 0)  

fleet.DAYS <<- NULL
reload_EMPTY_DAYS_table()
gtkEntrySetText(entry_DAYS_seedvalue, 0)  

fleet.GT <<- NULL
reload_EMPTY_GT_table()
gtkEntrySetText(entry_GT_seedvalue, 0)  

fleet.FISHINGEFFORT <<- NULL
reload_EMPTY_FISHINGEFFORT_table()
gtkEntrySetText(entry_FISHINGEFFORT_seedvalue, 0)

escape_surv_extvector_Ftable_fore <<- NULL
reload_EMPTY_FISHINGEFFORT_table()




}