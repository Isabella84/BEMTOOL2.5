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
#
#
# ---------------------- Run forecast action
run_forecast <- function(widget, window) {

new_aldForecast <<- setForecastfromGUI(new_aldForecast)
go_on <<- check_aldForecast(new_aldForecast)

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
