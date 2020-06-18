# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.







# initializating  objects

vbox_economic_params <- gtkVBox(FALSE, 5) 
   
hbox_economic_params <- gtkHBox(homogeneous = FALSE, 5)   
BMTnotebook_economic_params <<- gtkNotebook()
BMTnotebook_economic_params$setTabPos("top")
                            
suppressWarnings(source(paste(getwd(), "/bmtgui/economic_params/economic_params_price.r", sep="")))					
BMTnotebook_economic_params$appendPage(vbox_economic_params_price, gtkLabel(str=" Price "))

suppressWarnings(source(paste(getwd(), "/bmtgui/economic_params/economic_params_cost.r", sep="")))					
BMTnotebook_economic_params$appendPage(vbox_economic_params_cost, gtkLabel(str=" Costs "))

suppressWarnings(source(paste(getwd(), "/bmtgui/economic_params/economic_params_labour.r", sep="")))					
BMTnotebook_economic_params$appendPage(vbox_economic_params_labour_cost, gtkLabel(str=" Labour "))

suppressWarnings(source(paste(getwd(), "/bmtgui/economic_params/economic_params_behaviour.r", sep="")))					
BMTnotebook_economic_params$appendPage(vbox_economic_params_behaviour, gtkLabel(str=" Behaviour of the fleet "))

suppressWarnings(source(paste(getwd(), "/bmtgui/economic_params/economic_params_indicator.r", sep="")))					
BMTnotebook_economic_params$appendPage(vbox_economic_params_indicators, gtkLabel(str=" Economic indicators "))

hbox_economic_params$packStart(BMTnotebook_economic_params, expand = T, fill = T, padding = 10)                 

btn_bio_setEconomicParams_settings <<- gtkButton()                                    
gtkButtonSetLabel(btn_bio_setEconomicParams_settings, "   Apply changes   ")                    
btn_bio_setEconomicParams_settings$AddCallback("clicked", setEconomicparams_ALLthesettings)    

vbox_container_economicparams <- gtkVBox(FALSE, 5)                             
vbox_container_economicparams$packStart(hbox_economic_params, expand = T, fill = T, padding = 5)  
vbox_economic_params$packStart(vbox_container_economicparams, expand = T, fill = T, padding = 5) 







               