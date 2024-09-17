# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.







# initializating  objects

vbox_diagnosis <<- gtkVBox(FALSE, 5) 
   

if (LOADED_CASESTUDY) { 
rdata_path <-   paste(mat_cfg_general[1,3],  "/Diagnosis/", mat_cfg_general[1,2], " - BEMTOOL simulation.Rdata", sep="")
res <- try(load(rdata_path) )

if ( class(res) != "try-error") {   
hbox_diagnosis <- gtkHBox(homogeneous = FALSE, 5)   
BMTnotebook_diagnosis <<- gtkNotebook()
BMTnotebook_diagnosis$setTabPos("top")
                            
suppressWarnings(source(paste(getwd(), "/bmtgui/diagnosis/diagnosis_tab_statestocks.r", sep="")))					
BMTnotebook_diagnosis$appendPage(vbox_Diagnosis_StateStocks, gtkLabel(str=" State of the stocks "))

suppressWarnings(source(paste(getwd(), "/bmtgui/diagnosis/diagnosis_tab_impactpressure.r", sep="")))					
BMTnotebook_diagnosis$appendPage(vbox_Diagnosis_ImpactPressure, gtkLabel(str=" Impact/Pressure "))

suppressWarnings(source(paste(getwd(), "/bmtgui/diagnosis/diagnosis_tab_statefleets.r", sep="")))					
BMTnotebook_diagnosis$appendPage(vbox_Diagnosis_StateFleets, gtkLabel(str=" State of the fleets "))

hbox_diagnosis$packStart(BMTnotebook_diagnosis, expand = T, fill = T, padding = 5)                 

vbox_container <- gtkVBox(FALSE, 5)                             
vbox_container$packStart(hbox_diagnosis, expand = T, fill = T, padding = 0)  
vbox_diagnosis$packStart(vbox_container, expand = T, fill = T, padding = 0)

} else {
hbox_diagnosis <- gtkHBox(homogeneous = FALSE, 5)  
vbox_container <- gtkVBox(FALSE, 5)                             
vbox_container$packStart(hbox_diagnosis, expand = T, fill = T, padding = 0)  
vbox_diagnosis$packStart(vbox_container, expand = T, fill = T, padding = 0)
# hbox_diagnosis$packStart(gtkLabel(" Any BEMTOOL simulation has been found for the loaded case study! "), expand = T, fill = T, padding = 10)                 
}
} else {  
hbox_diagnosis <- gtkHBox(homogeneous = FALSE, 5)  
vbox_container <- gtkVBox(FALSE, 5)                             
vbox_container$packStart(hbox_diagnosis, expand = T, fill = T, padding = 0)  
vbox_diagnosis$packStart(vbox_container, expand = T, fill = T, padding = 0)
#hbox_diagnosis <- gtkHBox(homogeneous = FALSE, 5)  
# hbox_diagnosis$packStart(gtkLabel(" Any BEMTOOL simulation has been found for the loaded case study! "), expand = T, fill = T, padding = 10)                 

} 

              
