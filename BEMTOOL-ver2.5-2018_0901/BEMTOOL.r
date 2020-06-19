# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.
   
rm(list=ls(all=TRUE))

ALADYM_GUI_fleets_fore_succ <<- NULL
IN_BEMTOOL <<- TRUE  
BMT_STATE <<- "START"
phase <<- "SIMULATION"

LOADED_CASESTUDY <<- FALSE
LOADED_SCENARIO <<- FALSE
LOADED_RESULTS <<- FALSE
LOAD_SCENARIO_ACTION <<- F
#LOADED_SCENARIO_RESULTS <<- FALSE
BMT_R_GUI <<- TRUE

# interactions <<- NULL

options(stringsAsFactors = FALSE)

interactions <<-  data.frame(matrix(nrow=0,ncol=2))
colnames(interactions) <- c("Species", "Fleet_Segment")

nostocks <<-  data.frame(matrix(nrow=0,ncol=1))
colnames(nostocks) <- c("Stocks")

nofleetsegments <<-  data.frame(matrix(nrow=0,ncol=1))
colnames(nofleetsegments) <- c("Fleets")

#VITpathss_males <<- NULL
#VITpathss_females <<- NULL
#VITpathss_combined <<- NULL

#source(suppressWarnings(paste(getwd(), "/src/load_function.r", sep="")))
suppressWarnings(source(paste(getwd(), "/src/utils/requiredLibs.r", sep="")))
suppressWarnings(source(paste(getwd(), "/src/utils/ini.r", sep="")))
suppressWarnings(source(paste(getwd(), "/src/utils/scripts.r", sep="")))
 
suppressWarnings(source(paste(ALADYM_home, "/gui/scripts.r", sep="")) ) 
suppressWarnings(source(paste(ALADYM_home, "/gui/ini.r", sep="")) )

suppressWarnings(source(paste(ALADYM_home, "/src/load_functions.r", sep="")) )

suppressWarnings(source(paste(getwd(), "/bmtgui/utils/ini.r", sep="")))
suppressWarnings(source(paste(getwd(), "/bmtgui/utils/scripts.r", sep="")))

# inserire qui l'interfaccia

cat("\n\n")
print("***************************************************************************", quote=FALSE)
print("Loading BEMTOOL GUI (Graphical User Interface)...", quote=FALSE)
print("***************************************************************************", quote=FALSE)
cat("\n\n")

BMTmain_window <<- gtkWindow(show=FALSE)
BMTmain_window["title"] <- "BEMTOOL version 2.5-2017_2812"
BMT_sw_version <<-  "BEMTOOL version 2.5-2017_2812"

BMTmain_window$setDefaultSize(1024, 300)
gtkWindowSetResizable(BMTmain_window, FALSE)

BMTnotebook <<- gtkNotebook()
BMTnotebook$setTabPos("top")


hbox_bmt <- gtkHBox(homogeneous = FALSE, spacing = 0)

suppressWarnings(source(paste(getwd(), "/bmtgui/tab_casestudy.r", sep="")))					
BMTnotebook$appendPage(vbox_CS, gtkLabel(str=" Case study definition "))  

suppressWarnings(source(paste(getwd(), "/bmtgui/tab_biological.r", sep="")))					
BMTnotebook$appendPage(vbox_BiologicalAssessment_Sim, gtkLabel(str=" Biological parameters "))

suppressWarnings(source(paste(getwd(), "/bmtgui/tab_effort_landing.r", sep="")))					
BMTnotebook$appendPage(vbox_effort_landing, gtkLabel(str=" Effort & Landing data "))

suppressWarnings(source(paste(getwd(), "/bmtgui/tab_economic_data.r", sep="")))					
BMTnotebook$appendPage(vbox_economic_data, gtkLabel(str=" Economic data "))

suppressWarnings(source(paste(getwd(), "/bmtgui/tab_diagnosis.r", sep="")))					
BMTnotebook$appendPage(vbox_diagnosis, gtkLabel(str=" Diagnosis results "))

suppressWarnings(source(paste(getwd(), "/bmtgui/tab_economic_parameters.r", sep="")))					
BMTnotebook$appendPage(vbox_economic_params, gtkLabel(str=" Economic parameters "))

suppressWarnings(source(paste(getwd(), "/bmtgui/tab_scenarios.r", sep="")))					
BMTnotebook$appendPage(vbox_scenarios, gtkLabel(str=" Management rules "))

suppressWarnings(source(paste(getwd(), "/bmtgui/tab_forecast.r", sep="")))					
BMTnotebook$appendPage(vbox_forecast, gtkLabel(str=" Forecast "))

suppressWarnings(source(paste(getwd(), "/bmtgui/tab_mcda.r", sep="")))					
BMTnotebook$appendPage(vbox_mcda, gtkLabel(str=" Multi-Criteria Decision Analysis "))

gtkWindowSetPosition(BMTmain_window, 3)  

# la chiamata va all'interno di un bottone dopo la compilazione del file bmtconfig
# source(paste(getwd(), "/src/runBEMTOOLdiagnosis.r", sep=""))

vbox_bmt <- gtkVBox(homogeneous = FALSE, spacing = 0)
#vbox$packStart(menubar, expand = FALSE, fill = FALSE, padding = 0)

hbox_bmt <- gtkHBox(homogeneous = FALSE, spacing = 0)
lbl_current_step <- gtkLabel(" Step 1 of 9 ")
hbox_bmt$packStart(lbl_current_step , FALSE, FALSE, 5) 


btn_prev <- gtkButton()
gtkButtonSetLabel(btn_prev, "      << Prev    ")
btn_prev$AddCallback("clicked", goPrevTab)
hbox_bmt $packStart(btn_prev, FALSE, FALSE, 10) 

gtkWidgetSetSensitive(btn_prev, F)


btn_next <- gtkButton()
gtkButtonSetLabel(btn_next, "      Next >>    ")
btn_next$AddCallback("clicked", goNextTab)
hbox_bmt$packStart(btn_next, FALSE, FALSE, 10) 

btn_load_CS <- gtkButton()
gtkButtonSetLabel(btn_load_CS, "      LOAD Case Study definition...    ")
btn_load_CS$AddCallback("clicked", bmt_loadCaseStudy)
hbox_bmt$packStart(btn_load_CS, FALSE, FALSE, 5) 

btn_runSIMULATION_bmt <- gtkButton()
gtkButtonSetLabel(btn_runSIMULATION_bmt, "      Run NEW DIAGNOSIS    ")
btn_runSIMULATION_bmt$AddCallback("clicked", runSIMULATION_bmt_action)
hbox_bmt$packStart(btn_runSIMULATION_bmt, FALSE, FALSE, 10) 

gtkWidgetSetSensitive(btn_runSIMULATION_bmt, F)

btn_load_LOADSCENARIO <- gtkButton()
gtkButtonSetLabel(btn_load_LOADSCENARIO, "      LOAD Scenario configuration...    ")
btn_load_LOADSCENARIO$AddCallback("clicked", bmt_loadScenario)
hbox_bmt$packStart(btn_load_LOADSCENARIO, FALSE, FALSE, 5) 


btn_runFORECAST_bmt <- gtkButton()
gtkButtonSetLabel(btn_runFORECAST_bmt, "      Run NEW SCENARIO    ")
btn_runFORECAST_bmt$AddCallback("clicked", runFORECAST_bmt_action)
hbox_bmt$packStart(btn_runFORECAST_bmt, FALSE, FALSE, 10) 



gtkWidgetSetSensitive(btn_runFORECAST_bmt, F)

vbox_bmt$packStart(hbox_bmt, FALSE, FALSE, 5)   

vbox_bmt$packStart(BMTnotebook, expand=T, TRUE)
BMTmain_window$add(vbox_bmt)


BMTmain_window$show()


cat("\n\n")
print("***************************************************************************", quote=FALSE)
print("BEMTOOL GUI successfully loaded!", quote=FALSE)
print("***************************************************************************", quote=FALSE)
cat("\n\n")

#  gSignalConnect(BMTnotebook, "switch-page", bmt_change_step)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 0), T)  
#gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 8), T)

if (FALSE) {
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 1), F)          
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 2), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 3), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 4), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 5), F)          
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 6), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 7), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 8), T)
}
