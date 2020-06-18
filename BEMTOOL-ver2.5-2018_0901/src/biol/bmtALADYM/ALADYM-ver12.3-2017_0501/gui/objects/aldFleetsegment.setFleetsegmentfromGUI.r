# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




#
#
#
# Function that create a new fleet segment (SIMULATION) saving all the parameters from the GUI
#           
setFleetsegmentfromGUI<-function(object) {
#print("PRIMA DI AGGIORNARE!", quote=F)
#try(  print(FleetList_simulation[[1]]@fishingmortality.M.vector)  )
#print(".......................................... [aldFleetsegment.r] --> setFleetsegmentfromGUI()", quote=F)
# object = FleetList_simulation[[1]]
string_fleet <- gtkEntryGetText(entryGearName)
m <- regexec("[[:alnum:]+[:punct:]+[:blank:]+]+", string_fleet)
object@fleetname <- as.character(regmatches(string_fleet, m))

if ( gtkToggleButtonGetActive(radio_selectivity_params) ) {

object@selectivity.mode <- "params"

if ( gtkToggleButtonGetActive(radio_classicalogive) ) { 
  object@selectivity.method  <- SELECTIVITY_TYPE[1]  
} else if (gtkToggleButtonGetActive(radio_ogivedes)) {
  object@selectivity.method  <- SELECTIVITY_TYPE[2]  
} else if (gtkToggleButtonGetActive(radio_normal)) {
  object@selectivity.method  <- SELECTIVITY_TYPE[3]  
} else if (gtkToggleButtonGetActive(radio_lognormal)) {
  object@selectivity.method  <- SELECTIVITY_TYPE[4]  
} else if (gtkToggleButtonGetActive(radio_binormal)) {
  object@selectivity.method  <- SELECTIVITY_TYPE[5]  
} else if (gtkToggleButtonGetActive(radio_twosided)) {
  object@selectivity.method  <- SELECTIVITY_TYPE[6]  
}
#print(paste("SELECTIVITY method:", object@selectivity.method))
#object@selectivity.file <- selectivity_file

object@selectivity.vector <- get_table("SELECTIVITY")

} else if (gtkToggleButtonGetActive(radio_selectivity_vector_age) ) {

 object@selectivity.mode <- "age"

  object@selectivity.method  <- SELECTIVITY_TYPE[1]   # default
  object@selectivity.vector <- get_table("SELECTIVITY") # default
  
  object@SelectivityAge.M.vector <- SelectivityAgeM_matrix 
  object@SelectivityAge.F.vector <- SelectivityAgeF_matrix 
    
} else if (gtkToggleButtonGetActive(radio_selectivity_vector_length) ) {

object@selectivity.mode <- "length"

}

if ( gtkToggleButtonGetActive(radio_data) ) {
  object@production.datatype  <- "Production data"
} else if ( gtkToggleButtonGetActive(radio_pp) ) {
  object@production.datatype  <- "P production"
} 
#object@production.file <- production_file
object@production.vector <- get_table("PRODUCTION")
object@pproduction.vector <- get_table("P_PRODUCTION")
object@discard.calculation <- gtkComboBoxGetActiveText(combo_discard)
#object@discard.file <- discard_file
object@discard.vector <- get_table("DISCARD")
object@discard_extvector.F.vector <- get_table("DISCARD_EXT_VECTOR_F")
object@discard_extvector.M.vector <- get_table("DISCARD_EXT_VECTOR_M")
object@landing.obligation.vector <- get_table("LANDING_OBLIGATION")



if ( gtkToggleButtonGetActive(radio_effortdata) ) {
  object@effort.datatype  <- "Effort data"
} else if ( gtkToggleButtonGetActive(radio_fishingcoeff) ) {
  object@effort.datatype  <- "Fishing coefficient"
} 

#object@fishingeffort.file <- FISHINGEFFORT_file
object@fishingeffort.vector <- get_table("FISHING_EFFORT")
#object@vessels.file <- VESSELS_file
object@vessels.vector <- get_table("VESSELS")
#object@days.file <- DAYS_file
object@days.vector <- get_table("DAYS")
#object@gt.file <- GT_file
object@gt.vector <- get_table("GT")

#object@fishingmortality.M.file <- fishingmortalityM_file
object@fishingmortality.M.vector <- get_table("FISHING_MORTALITY_M") 
#object@fishingmortality.F.file <- fishingmortalityF_file
object@fishingmortality.F.vector <- get_table("FISHING_MORTALITY_F") 

object@scenario.reduction <- SCENARIO_TYPE[1]

object@fishingeffort.vector <- get_table("FISHING_EFFORT")

print("Fleet segment saved!", quote=F)

#print("DOPO L'AGGIORNAMENTO!", quote=F)
#try(  print(FleetList_simulation[[2]]@fishingmortality.M.vector)  )

return(object)
}
