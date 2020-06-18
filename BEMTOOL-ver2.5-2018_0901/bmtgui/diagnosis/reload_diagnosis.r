# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



# hbox_diagnosis$destroy()
# rdata_path <-   paste(mat_cfg_general[1,3],  "/Diagnosis/", mat_cfg_general[1,2], " - BEMTOOL simulation.Rdata", sep="")


rdata_path <-   paste(mat_cfg_general[1,3],  "/Diagnosis/working files/", mat_cfg_general[1,2], " - BEMTOOL simulation-rev.Rdata", sep="")

# BMTmain_window_original <- BMTmain_window

print(paste("Loading..." , rdata_path ), quote=F)
res <- try(load(rdata_path) )

#source(suppressWarnings(paste(getwd(), "/src/load_function.r", sep="")))
BMT_STATE <<- "DO_SCENARIO"

# BMTmain_window <<- BMTmain_window_original
# hbox_diagnosis$destroy()

if ( class(res) != "try-error") {

if (exists("hbox_diagnosis")) {
hbox_diagnosis$destroy()
#rm(hbox_diagnosis)
hbox_diagnosis <- gtkHBox(homogeneous = FALSE, 5)  
}

bmt_wnd_diagnosis <- showMessage("Loading the last DIAGNOSIS results...") 
gtkWidgetSetSensitive(BMTmain_window, F)
  
hbox_diagnosis <- gtkHBox(homogeneous = FALSE, 5)   
BMTnotebook_diagnosis <<- gtkNotebook()
BMTnotebook_diagnosis$setTabPos("top")
                            
suppressWarnings(source(paste(getwd(), "/bmtgui/diagnosis/diagnosis_tab_statestocks.r", sep="")))					
BMTnotebook_diagnosis$appendPage(vbox_Diagnosis_StateStocks, gtkLabel(str=" State of the stocks "))

suppressWarnings(source(paste(getwd(), "/bmtgui/diagnosis/diagnosis_tab_impactpressure.r", sep="")))					
BMTnotebook_diagnosis$appendPage(vbox_Diagnosis_ImpactPressure, gtkLabel(str=" Impact/Pressure "))

suppressWarnings(source(paste(getwd(), "/bmtgui/diagnosis/diagnosis_tab_statefleets.r", sep="")))					
BMTnotebook_diagnosis$appendPage(vbox_Diagnosis_StateFleets, gtkLabel(str=" State of the fleets "))

hbox_diagnosis$packStart(BMTnotebook_diagnosis, expand = T, fill = T, padding = 10)                 

    for (choice in BMT_SPECIES) { 
    bmt_diagnosis_species$appendText(choice)    
    }
      gtkComboBoxSetActive(bmt_diagnosis_species, 0 )   
    # gtkComboBoxSetActive(bmt_diagnosis_species_pressure, 0 )    
  bmt_wnd_diagnosis$destroy()
gtkWidgetSetSensitive(BMTmain_window, T)    

} else {
hbox_diagnosis <- gtkHBox(homogeneous = FALSE, 5)  
 hbox_diagnosis$packStart(gtkLabel(" Any BEMTOOL simulation has been found for the loaded case study! "), expand = T, fill = T, padding = 10)                 
}

vbox_container$packStart(hbox_diagnosis, expand = T, fill = T, padding = 10)  

gtkButtonSetLabel(btn_next, "      Next >>    ")
gtkLabelSetText(lbl_current_step, " Step 5 of 9 ")  
gtkNotebookSetCurrentPage(BMTnotebook, 4)   
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 0), F)  
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 1), F)          
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 2), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 3), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 4), T)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 5), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 6), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 7), F)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(BMTnotebook, 8), F)
gtkWidgetSetSensitive(btn_runSIMULATION_bmt, F)
gtkWidgetSetSensitive(btn_runFORECAST_bmt, F)
gtkWidgetSetSensitive(btn_load_CS, F) 
gtkWidgetSetSensitive(btn_load_LOADSCENARIO, F) 

showMessageOK("Last DIAGNOSIS successfully loaded!")   

