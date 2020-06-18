# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


 # vboxUncertainty <- gtkVBox(homogeneous = FALSE, 5)

# chkActivateUncertaintyForecast <- gtkCheckButton("Use uncertainty")
 chkConfidenceIntervals_fore <- gtkCheckButton("Recruitment")
 chkConfidenceIntervals_fore_crescita <- gtkCheckButton("Growth & Natural mortality")
   
chkConfidenceIntervals_fore_Maturity <- gtkCheckButton("Maturity")
chkConfidenceIntervals_fore_Selectivity <- gtkCheckButton("Selectivity")
	  	  
lbl_CI_num_runs_fore <- gtkLabel("no. of runs") 
entry_CI_numb_runs_fore <- gtkEntry()
gtkEntrySetWidthChars(entry_CI_numb_runs_fore, NUMERICAL_ENTRY_LENGTH)

gSignalConnect(chkConfidenceIntervals_fore, "toggled", change_CI_input_fore)
gSignalConnect(chkConfidenceIntervals_fore_crescita, "toggled", deactivate_growth_uncertainty)
gSignalConnect(chkConfidenceIntervals_fore_Maturity, "toggled", deactivate_maturity_uncertainty)
gSignalConnect(chkConfidenceIntervals_fore_Selectivity, "toggled", deactivate_selectivity_uncertainty) 

hbox_spunte_incertezza2 <- gtkHBox()
hbox_spunte_incertezza2$packStart(gtkLabel("Apply uncertainty on one or more of the items below"), expand = FALSE, fill = FALSE, padding = 5)
hbox_spunte_incertezza3 <- gtkHBox()
hbox_spunte_incertezza3$packStart(chkConfidenceIntervals_fore, expand = FALSE, fill = FALSE, padding = 5)
hbox_spunte_incertezza3$packStart(chkConfidenceIntervals_fore_crescita, expand = FALSE, fill = FALSE, padding = 5)
# hbox_spunte_incertezza3$packStart(chkConfidenceIntervals_fore_M, expand = FALSE, fill = FALSE, padding = 5)
hbox_spunte_incertezza3$packStart(chkConfidenceIntervals_fore_Maturity, expand = FALSE, fill = FALSE, padding = 5)
hbox_spunte_incertezza3$packStart(chkConfidenceIntervals_fore_Selectivity, expand = FALSE, fill = FALSE, padding = 5)
hbox_spunte_incertezza2$packStart(lbl_CI_num_runs_fore, expand = FALSE, fill = FALSE, padding = 30)
hbox_spunte_incertezza2$packStart(entry_CI_numb_runs_fore, expand = FALSE, fill = FALSE, padding = 0)


notebook_uncertainty <- gtkNotebook()
notebook_uncertainty$setTabPos("top")



  suppressWarnings(source(paste(ALADYM_home, "/gui/forecast/uncertainty_recruitment.r", sep="") ) )					

notebook_uncertainty$appendPage(vbox_recruits_fore_UN, gtkLabel(str=" RECRUITMENT "))

   suppressWarnings(source(paste(ALADYM_home, "/gui/forecast/uncertainty_growth.r", sep="") ) )					

notebook_uncertainty$appendPage(vbox_global_growth_uncert, gtkLabel(str=" GROWTH & NATURAL MORTALITY "))

   suppressWarnings(source(paste(ALADYM_home, "/gui/forecast/uncertainty_maturity.r", sep="") ) )

notebook_uncertainty$appendPage(vbox_global_maturity_uncert, gtkLabel(str=" MATURITY "))

   suppressWarnings(source(paste(ALADYM_home, "/gui/forecast/uncertainty_selectivity.r", sep="") ) )
   
notebook_uncertainty$appendPage(vbox_global_selectivity_uncert, gtkLabel(str=" SELECTIVITY "))

hboxUncertainty <- gtkHBox(homogeneous = FALSE, 5)
hboxUncertainty$packStart(notebook_uncertainty, expand = TRUE, fill = TRUE, 5)

vboxRecruitment_fore$packStart(hboxUncertainty, expand = FALSE, fill = TRUE, 0)

if (phase=="FORECAST") {
 if (new_aldSimulation@enteringMortality == "F" ) {
          gtkWidgetSetSensitive(chkConfidenceIntervals_fore_Selectivity, F) 
 }
 if (harvest_rule_code == "MEY") {
          gtkWidgetSetSensitive(lbl_CI_num_runs_fore, F)
              gtkWidgetSetSensitive(entry_CI_numb_runs_fore, F)
                  gtkWidgetSetSensitive(chkConfidenceIntervals_fore, F)
                      gtkWidgetSetSensitive(chkConfidenceIntervals_fore_crescita, F)
                          gtkWidgetSetSensitive(chkConfidenceIntervals_fore_Maturity, F)
                          gtkWidgetSetSensitive(chkConfidenceIntervals_fore_Selectivity, F)
 }
 }


