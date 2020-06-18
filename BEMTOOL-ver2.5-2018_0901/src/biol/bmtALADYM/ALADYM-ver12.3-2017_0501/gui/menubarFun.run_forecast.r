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

#
#suppressWarnings(source(paste(ALADYM_home, "/gui/utilities/check_list_fore.r", sep="") ) )
#go_on <<- TRUE
#suppressWarnings(source(paste(ALADYM_home, "/gui/utilities/validate_input_fore.r", sep="") ) )
#
#if (go_on) {
#new_aldForecast <<- setForecastfromGUI(new_aldForecast)
#source(paste(ALADYM_home, "/src/runALADYMforecast.r", sep="") )
#}

new_aldForecast <<- setForecastfromGUI(new_aldForecast)

go_on <<- check_aldForecast(new_aldForecast)

 if (go_on) {
source(paste(ALADYM_home, "/src/runALADYMforecast.r", sep="") )
}


} 
