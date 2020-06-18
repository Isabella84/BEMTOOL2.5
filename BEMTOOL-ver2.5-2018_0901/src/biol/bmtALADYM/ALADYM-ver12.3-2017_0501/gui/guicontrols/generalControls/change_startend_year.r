# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



change_startend_year <- function(w) {

   go_on_sim <- TRUE
   
if (gtkEntryGetText(entry_StartYear_simulation) == "" ) {
       showError("Insert the start year of simulation!")
        go_on_sim <- FALSE
  } else if (gtkEntryGetText(entry_EndYear_simulation) == "" ) {
      showError("Insert the end year of simulation!")
      go_on_sim <- FALSE
  } else if (gtkEntryGetText(entry_EndYear_forecast) == "" ) {
      showError("Insert the end year of forecast!")
      go_on_sim <- FALSE
  } else if (as.numeric(as.character(gtkEntryGetText(entry_StartYear_simulation))) > as.numeric(as.character(gtkEntryGetText(entry_EndYear_simulation))) ) { 
     go_on_sim <- FALSE
     showError("The end year of simulation must be after or the same than the start year of simulation!")
   } else if (as.numeric(as.character(gtkEntryGetText(entry_EndYear_forecast))) <= as.numeric(as.character(gtkEntryGetText(entry_EndYear_simulation))) ) { 
     go_on_sim <- FALSE
     showError("The end year of forecast must be after the end year of simulation!")
   } else {
     years <<- c(as.numeric(as.character(gtkEntryGetText(entry_StartYear_simulation))): as.numeric(as.character(gtkEntryGetText(entry_EndYear_simulation))))
     years_forecast <<-  c( (as.numeric(as.character(gtkEntryGetText(entry_EndYear_simulation)))+1) : as.numeric(as.character(gtkEntryGetText(entry_EndYear_forecast))))
     }

if (go_on_sim) {
reload_recruitment_table()
reload_ZvectorF_table()
reload_ZvectorM_table()

if (!is.null(fleet.selectivity) ) {
if (nrow(fleet.selectivity) > 0) {
  reload_selectivity()
} else {
  reload_EMPTY_selectivity()
} 
} else {
  reload_EMPTY_selectivity()
} 


if (!is.null(fleet.discard) ) {
if (nrow(fleet.discard) > 0) {
  reload_discard_table()
} else {
  reload_EMPTY_discard_table()
} 
} else {
  reload_EMPTY_discard_table()
} 

if (!is.null(fleet.production) ) {
if (nrow(fleet.production) > 0) {
  reload_production_table()
} else {
  reload_EMPTY_production_table()
} 
} else {
    reload_EMPTY_production_table()
} 


if (!is.null(fleet.pproduction) ) {
if (nrow(fleet.pproduction) > 0) {
  reload_pproduction_table()
} else {
  reload_EMPTY_pproduction_table()
} 
} else {
    reload_EMPTY_pproduction_table()
} 


if (!is.null(fleet.DAYS) ) {
if (nrow(fleet.DAYS) > 0) {
  reload_DAYS_table()
} else {
  reload_EMPTY_DAYS_table()
} 
} else {
  reload_EMPTY_DAYS_table()
} 



if (!is.null(fleet.GT) ) {
if (nrow(fleet.GT) > 0) {
  reload_GT_table()
} else {
  reload_EMPTY_GT_table()
} 
} else {
  reload_EMPTY_GT_table()
}


if (!is.null(fleet.VESSELS) ) {
if (nrow(fleet.VESSELS) > 0) {
  reload_VESSELS_table()
} else {
  reload_EMPTY_VESSELS_table()
} 
} else {
  reload_EMPTY_VESSELS_table()
}


if (!is.null(fleet.FISHINGEFFORT) ) {
if (nrow(fleet.FISHINGEFFORT) > 0) {
  reload_FISHINGEFFORT_table()
} else {
  reload_EMPTY_FISHINGEFFORT_table()
} 
} else {
  reload_EMPTY_FISHINGEFFORT_table()
}

if (!is.null(mortality.Fvector.males) ) {
if (nrow(mortality.Fvector.males) > 0) {
  reload_fishingmortalityM()
} else {
  reload_EMPTY_fishingmortalityM()
} 
} else {
  reload_EMPTY_fishingmortalityM()
}


if (!is.null(mortality.Fvector.females) ) {
if (nrow(mortality.Fvector.females) > 0) {
  reload_fishingmortalityF()
} else {
  reload_EMPTY_fishingmortalityF()
} 
} else {
  reload_EMPTY_fishingmortalityF()
}

if (!is.null(FM_overall_matrix) ) {
if (nrow(FM_overall_matrix) > 0) {
  reload_fishingmortalityM_overall()
} else {
  reload_EMPTY_fishingmortalityM_overall()
} 
} else {
  reload_EMPTY_fishingmortalityM_overall()
}

if (!is.null(FF_overall_matrix) ) {
if (nrow(FF_overall_matrix) > 0) {
  reload_fishingmortalityF_overall()
} else {
  reload_EMPTY_fishingmortalityF_overall()
} 
} else {
  reload_EMPTY_fishingmortalityF_overall()
}


#change_fishingM_F()
#change_fishingM_M()

reload_EMPTY_discard_extvector_F()
reload_EMPTY_discard_extvector_M()

#reload_EMPTY_escape_survival_extvectorF()
#reload_EMPTY_escape_survival_extvectorM()


if (!is.null(fleet.selectivity_fore) ) {
if (nrow(fleet.selectivity_fore) > 0) {
  reload_selectivity_fore()
} else {
  reload_EMPTY_selectivity_fore()
} 
} else {
  reload_EMPTY_selectivity_fore()
} 

if (!is.null(fleet.GT_fore) ) {
  reload_GT_fore_table()
} else {
  reload_EMPTY_GT_fore_table()
}    

if (!is.null(fleet.DAYS_fore) ) {
  reload_DAYS_fore_table()
} else {
  reload_EMPTY_DAYS_fore_table()
}

if (!is.null(fleet.VESSELS_fore) ) {
  reload_VESSELS_fore_table()
} else {
  reload_EMPTY_VESSELS_fore_table()
}

if (!is.null(fleet.FISHINGEFFORT_fore) ) {
  reload_FISHINGEFFORT_fore_table()
} else {
  reload_EMPTY_FISHINGEFFORT_fore_table()
}

if (!is.null(fleet.lan_obligation_fore) ) {
  reload_lan_obligation_fore_table()
} else {
  reload_EMPTY_lan_obligation_fore_table()
} 

if (!is.null(fleet.discard_fore) ) {
if (nrow(fleet.discard_fore) > 0) {
  reload_discard_fore()
} else {
  reload_EMPTY_discard_fore()
} 
} else {
  reload_EMPTY_discard_fore()
} 

gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook, 1), TRUE)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook, 2), TRUE)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook, 3), TRUE)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook, 4), TRUE)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook, 5), F)

forecast <<- (as.numeric(as.character(gtkEntryGetText(entry_EndYear_simulation))) - as.numeric(as.character(gtkEntryGetText(entry_StartYear_simulation))) +1) * 12  +1

    reload_EMPTY_extErrorRecruitment_fore()

}
}