# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




# ---------------------- Create and add gear to the list
add_gear_from_config <- function(name_of_gear) {
#print(".......................................... [add_gear.r]")
new_aldFleetsegment <<- new(Class="aldFleetsegment") 
new_aldFleetsegment@fleetname <- name_of_gear
#new_aldFleetsegment <<- setFleetsegmentfromConfig(new_aldFleetsegment, name_of_gear)

FleetList_simulation <<- c(FleetList_simulation, new_aldFleetsegment)  
lastAdded <- length(FleetList_simulation)

FleetList_forecast <<- c(FleetList_forecast, new_aldFleetsegment)

#FleetList_forecast[[lastAdded]] <<- updateFleetsegment_forefromGUI(FleetList_forecast[[lastAdded]])

return(new_aldFleetsegment@fleetname)
}