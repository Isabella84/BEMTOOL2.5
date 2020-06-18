# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





setClass(Class="aldFleetsegment",
        representation=representation(
            fleetname = "character",
            selectivity.mode = "character",     # params, age, length
            selectivity.vector = "data.frame",
            SelectivityAge.M.vector = "data.frame",
            SelectivityAge.F.vector = "data.frame",
            SelectivityLength.F.vector  = "data.frame",
            SelectivityLength.M.vector  = "data.frame",
            production.datatype = "character",   # Production data, P production
            production.vector = "data.frame",
            monthly.discard.vector = "data.frame",
            pproduction.vector = "data.frame",
            discard.calculation = "character",  # YES, NA, 0
            discard.datatype = "character",  # External vector,  # reverse ogive
            discard.vector = "data.frame",   
            discard_extvector.M.vector = "data.frame", 
            discard_extvector.F.vector = "data.frame",
            effort.datatype = "character",      # Effort data, Fishing coefficient
            fishingeffort.vector = "data.frame",
            vessels.vector = "data.frame",
            days.vector = "data.frame",
            gt.vector = "data.frame",
            fishingmortality.M.vector = "data.frame", 
            fishingmortality.F.vector = "data.frame",
            scenario.reduction = "character",
            landing.obligation.vector = "data.frame",       # Y, N
            discard.survivability.calculation = "character",  # Y, N
            discard.survivability.datatype = "character",  # C, DOS
            discard.survivability.params = "data.frame",
            escape.survivability.calculation = "character",   # Y, N
            escape.survivability.datatype = "character",   # C, DOS
            escape.survivability.constant = "data.frame",       # males and females
            escape.survivability.DOS.datatype = "character",
            escape.survivability.DOS.ogiveparams = "data.frame",
            escape.survivability.DOS.ext_vect.M = "data.frame",
            escape.survivability.DOS.ext_vect.F = "data.frame",
            EffortF.relationship = "data.frame",
	          catchAtAge.M.vector = "data.frame",
			      catchAtAge.F.vector = "data.frame"
            ) )
