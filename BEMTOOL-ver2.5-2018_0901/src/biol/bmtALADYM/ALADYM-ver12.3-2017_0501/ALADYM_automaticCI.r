# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

if (!exists("phase")) {
rm(list=ls(all=TRUE))
IN_BEMTOOL <<- FALSE
phase <<- ""
SKIP_spe <<- FALSE
}

if (!IN_BEMTOOL) {
ALADYM_home <<- getwd()
years <<- c(2013)
years_forecast <<- c(2014)
FLEETSEGMENTS_names <<- NULL
all_years <<- years
INTEGRATED_APPROACH <<- FALSE
SAtool <<- "NONE"
BMT_SCENARIO <<- 0
BMT_HR_CHANGE_FISHMORTALITY <<- 3
GLO <- new.env()
GLO$L_number <- 12
BMT_SPECIES <<- c("")
ALADYM_spe <<- 1
} else {
SKIP_spe <<- FALSE
years_forecast <<- years.forecast
# years <<- years
}

showCompTime <<- F

cat("\n\n")
print("***************************************************************************", quote=FALSE)
print("Loading ALADYM GUI (Graphical User Interface)...", quote=FALSE)
print("***************************************************************************", quote=FALSE)
cat("\n\n")


print(".......................................... [rungui.r]", quote=F)  
suppressWarnings(source(paste(ALADYM_home, "/src/load_functions.r", sep=""))  )
suppressWarnings(source(paste(ALADYM_home, "/gui/scripts.r", sep="") ) )
suppressWarnings(source(paste(ALADYM_home, "/gui/ini.r", sep="")) )
suppressWarnings(source(paste(ALADYM_home, "/src/paths.r", sep=""))  )
# initialization matrices



Tr <<- 0
if (!IN_BEMTOOL) {
biological.lifeSpanM <<- 1
biological.lifeSpanF <<- 1
} else {
biological.lifeSpanM <<-  as.numeric(as.character(Populations[[ALADYM_spe]]@lifespan$lifespan[[1]]))
biological.lifeSpanF <<- as.numeric(as.character(Populations[[ALADYM_spe]]@lifespan$lifespan[[2]]))
biological.lifeSpan <<- max(biological.lifeSpanM, biological.lifeSpanF)
}
months_vec_M <<- c(Tr:(biological.lifeSpanM*12))
months_vec_F <<- c(Tr:(biological.lifeSpanF*12))
biological.months_MM <<- length(months_vec_M)
biological.months_MF <<- length(months_vec_F)

main_window <<- gtkWindow(show=FALSE)
 if (!IN_BEMTOOL) {
main_window["title"] <- paste("ALADYM-ver10.1.4b-2015")
} else {
if (phase == "SIMULATION") {
main_window["title"] <- paste("BEMTOOL 2.0 biological simulation - ALADYM-ver10.1.4b-2015 [",BMT_SPECIES[ALADYM_spe],"]", sep="" )
} else {
main_window["title"] <- paste("BEMTOOL 2.0 biological forecast - ALADYM-ver10.1.4b-2015 [",BMT_SPECIES[ALADYM_spe],"]", sep="")
}
}
main_window$setDefaultSize(1024, 768)
gtkWindowSetResizable(main_window, FALSE)
#gtkWindowSetPosition(main_window, GTK_WIN_POS_CENTER_ALWAYS)

# define TAB
notebook <<- gtkNotebook()
notebook$setTabPos("top")

# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같?
# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같 ALADYM in BEMTOOL
# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같?
 if (IN_BEMTOOL) {
if (phase=="SIMULATION") {

all_years <<- years

suppressWarnings(source(paste(ALADYM_home, "/gui/generaldata.r", sep="") ))					
notebook$appendPage(vboxGeneralData, gtkLabel(str=" GENERAL DATA "))

suppressWarnings(source(paste(ALADYM_home, "/gui/biological.r", sep="") )	)				
notebook$appendPage(vboxBiological, gtkLabel(str=" BIOLOGICAL "))

suppressWarnings(source(paste(ALADYM_home, "/gui/recruitment.r", sep="") ) )					
notebook$appendPage(vboxRecruitment, gtkLabel(str=" RECRUITMENT "))

suppressWarnings(source(paste(ALADYM_home, "/gui/mortality.r", sep="") ) )					
notebook$appendPage(vboxMortality, gtkLabel(str=" MORTALITY "))

# ********************************* FISHERY graphical elements
suppressWarnings(source(paste(ALADYM_home, "/gui/fishery.r", sep="") ) )					
notebook$appendPage(vboxFishery, gtkLabel(str=" FISHERY "))

suppressWarnings( source(paste(ALADYM_home, "/gui/forecast.r", sep="") )	)	
}

if (phase=="FORECAST") {

Tr <<- INP$tr

path_to_save <- paste(casestudy_path, "/Diagnosis/working files/GUIfle_fore.Rdata", sep="")
load(path_to_save)  
FleetList_forecast <<- .GlobalEnv$ALADYM_GUI_fleets_fore[[ALADYM_spe]]

#print(paste("prima di chiamare ALADYM.r carico anno 1 specie", ALADYM_spe))
#print(FleetList_forecast[[1]]@fishingeffort.vector)

##gtkNotebookRemovePage(notebook, gtkNotebookPageNum(notebook, vboxFishery)) 
#gtkComboBoxSetActive(combo_fleetsegments_fore, -1 ) 
#gtkComboBoxSetActive(combo_fleetsegments_fore, 0 )

all_years <<- c(years, years_forecast)
    # ********************************* FISHERY graphical elements
    suppressWarnings(source(paste(ALADYM_home, "/gui/fishery.r", sep="") ))					
    notebook$appendPage(vboxFishery, gtkLabel(str=" SIMULATION "))
    # ********************************* FORECAST graphical elements
   suppressWarnings( source(paste(ALADYM_home, "/gui/forecast.r", sep="") )	)	
   
#  for (item in BMT_FLEETSEGMENTS[associated_fleetsegment_indices]) {
#  combo_fleetsegments_fore$appendText(item)
#    }

gtkComboBoxSetActive(combo_fleetsegments, 0 )
# loadFleetsegmentintoGUI(FleetList_simulation[[1]])
#gtkEntrySetEditable(entryGearName, FALSE) 
  #  gtkComboBoxSetActive(combo_fleetsegments_fore, 0 )
 notebook$appendPage(vboxForecast, gtkLabel(str=" FORECAST "))
} 

# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같?
# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같 ALADYM STAND-ALONE
# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같?
} else {

suppressWarnings(source(paste(ALADYM_home, "/gui/generaldata.r", sep="") ))					
notebook$appendPage(vboxGeneralData, gtkLabel(str=" GENERAL DATA "))

suppressWarnings(source(paste(ALADYM_home, "/gui/biological.r", sep="") )	)				
notebook$appendPage(vboxBiological, gtkLabel(str=" BIOLOGICAL "))

suppressWarnings(source(paste(ALADYM_home, "/gui/recruitment.r", sep="") ) )					
notebook$appendPage(vboxRecruitment, gtkLabel(str=" RECRUITMENT "))

suppressWarnings(source(paste(ALADYM_home, "/gui/mortality.r", sep="") ) )					
notebook$appendPage(vboxMortality, gtkLabel(str=" MORTALITY "))

# ********************************* FISHERY graphical elements
suppressWarnings(source(paste(ALADYM_home, "/gui/fishery.r", sep="") ) )					
notebook$appendPage(vboxFishery, gtkLabel(str=" FISHERY "))

suppressWarnings(source(paste(ALADYM_home, "/gui/forecast.r", sep="") )	)	
notebook$appendPage(vboxForecast, gtkLabel(str=" FORECAST "))
 
}  
# disable the forecast box

# ********************************* FORECAST graphical elements - end

vbox <- gtkVBox(homogeneous = FALSE, spacing = 0)
#vbox$packStart(menubar, expand = FALSE, fill = FALSE, padding = 0)

hbox <- gtkHBox(homogeneous = FALSE, spacing = 0)


 # 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같?
# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같 ALADYM in BEMTOOL
# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같?
if (IN_BEMTOOL) {
if (phase=="SIMULATION") {
hbox$packStart(gtkLabel("After setting all the parameters press the button to go on with BEMTOOL model."), expand = FALSE, fill = FALSE, padding = 5)

btn_browse_BIOfile <- gtkButton()
gtkButtonSetLabel(btn_browse_BIOfile, "Load SIMULATION parameters...")
btn_browse_BIOfile$AddCallback("clicked", select_file_BIO)
hbox$packStart(btn_browse_BIOfile, expand = FALSE, fill = FALSE, padding = 20)

btn_runSimulation <- gtkButton()
gtkButtonSetLabel(btn_runSimulation, "RUN SIMULATION")
btn_runSimulation$AddCallback("clicked", run_simulation)

hbox$packStart(btn_runSimulation, expand = FALSE, fill = FALSE, padding = 20)

btn_skipSimulation <- gtkButton()
gtkButtonSetLabel(btn_skipSimulation, "SKIP >>")
btn_skipSimulation$AddCallback("clicked", skip_simulation)
hbox$packStart(btn_skipSimulation, expand = FALSE, fill = FALSE, padding = 20)

} else {
btn_runScenario <- gtkButton()
gtkButtonSetLabel(btn_runScenario, "RUN FORECAST")
btn_runScenario$AddCallback("clicked", run_forecast)
hbox$packStart(gtkLabel("After setting the selectivity parameters press the button to go on with BEMTOOL scenario."), expand = FALSE, fill = FALSE, padding = 5)
hbox$packStart(btn_runScenario, expand = FALSE, fill = FALSE, padding = 20)

gtkNotebookSetCurrentPage(notebook, 1)
gtkNotebookSetCurrentPage(notebook_forecast,0)

}

# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같?
# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같 ALADYM STAND-ALONE
# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같?
} else {

btn_runSimulation <- gtkButton()
gtkButtonSetLabel(btn_runSimulation, "RUN SIMULATION")
btn_runSimulation$AddCallback("clicked", run_simulation)
hbox$packStart(btn_runSimulation, expand = FALSE, fill = FALSE, padding = 20)

btn_runScenario <- gtkButton()
gtkButtonSetLabel(btn_runScenario, "RUN FORECAST")
btn_runScenario$AddCallback("clicked", run_forecast)
hbox$packStart(btn_runScenario, expand = FALSE, fill = FALSE, padding = 20)


btn_browse_BIOfile <- gtkButton()
gtkButtonSetLabel(btn_browse_BIOfile, "Load SIMULATION parameters...")
btn_browse_BIOfile$AddCallback("clicked", select_file_BIO)
hbox$packStart(btn_browse_BIOfile, expand = FALSE, fill = FALSE, padding = 20)

btn_loadSimulation <- gtkButton()
gtkButtonSetLabel(btn_loadSimulation, "Load SIMULATION workspace...")
btn_loadSimulation$AddCallback("clicked", load_simulation)
hbox$packStart(btn_loadSimulation, expand = FALSE, fill = FALSE, padding = 20)

gtkWidgetSetSensitive(btn_runScenario, FALSE)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook, 1), FALSE)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook, 2), FALSE)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook, 3), FALSE)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook, 4), FALSE)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook, 5), FALSE)

}


vbox$packStart(hbox, FALSE, FALSE, 5)   

vbox$packStart(notebook, expand=FALSE, TRUE)
#vbox$packStart(statusbar, FALSE, FALSE, 0)
main_window$add(vbox)
gtkWindowSetPosition(main_window, 3)  

 
if (IN_BEMTOOL & phase=="SIMULATION") {
#if (exists("bmt_wnd_sim")) {
#suppressWarnings(bmt_wnd_sim$destroy() )
#}
lockFisheryValues()
} else if (IN_BEMTOOL & phase=="FORECAST") { 
lockFisheryValues()
lockFisheryValues_fore()
}

main_window$show()


cat("\n\n")
print("***************************************************************************", quote=FALSE)
print("ALADYM GUI successfully loaded!", quote=FALSE)
print("***************************************************************************", quote=FALSE)
cat("\n\n")


gtkToggleButtonSetActive(radio_effortdata, TRUE)
gtkToggleButtonSetActive(radio_data, TRUE)


if (!IN_BEMTOOL) {
deactivate_FishingEffort_unused_params()
deactivate_Pproduction_unused_params()
deactivate_FishingM_unused_params()
gtkComboBoxSetActive(combo_discard, 2)
deactivate_Discard_unused_params()
}

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION")) {
gtkToggleButtonSetActive(radio_Zentry, TRUE)
deactivate_F()
}

AUTOMATIC_CI <<- T

if (IN_BEMTOOL & (phase=="FORECAST") & AUTOMATIC_CI) {
     
    gtkEntrySetText(entry_CI_numb_runs_fore, "500") 
	  gtkToggleButtonSetActive(chkConfidenceIntervals_fore, TRUE)
    	
suppressWarnings(source(paste(ALADYM_home, "/gui/utilities/interconn.bmt/bmt_check_list_fore.r", sep="") ) )
go_on <<- TRUE
#suppressWarnings(source(paste(ALADYM_home, "/gui/utilities/validate_input_fore.r", sep="") ) )

if (go_on) {

# if (exists("ALADYM_GUI_fleets_fore") & (ALADYM_spe == 1) & (!INTEGRATED_APPROACH) ) { rm(ALADYM_GUI_fleets_fore) } 
if (MEY_CALCULATION) {
    suppressWarnings(source(paste(ALADYM_home, "/gui/menubarFun.run_forecast_bmt.MEYcode.r", sep="") ) )
 } else if (!INTEGRATED_APPROACH) {
    suppressWarnings(source(paste(ALADYM_home, "/gui/menubarFun.run_forecast_bmt.NIcode.r", sep="") ) )
 } else {    # integrated approach 
    suppressWarnings(source(paste(ALADYM_home, "/gui/menubarFun.run_forecast_bmt.Icode.r", sep="") ) ) 
 }

}
}



 if (FALSE) {
dialog <- gtkFileChooserDialog("Choose the parameters .CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])

CONFIGURATION_file <<- dialog$getFilename()
BIOparameters_table <<- data.frame(read.csv(CONFIGURATION_file, sep=";", na.strings = "") , stringsAsFactors = F )
BIOmatr <- BIOparameters_table
}