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
setSimulationfromGUI<-function(object) {
object@time_slice_year <- as.numeric(gtkEntryGetText(entryTimeSlicePerYear))
# object@simulation_years = as.numeric(gtkEntryGetText(entryYearsToBeSimulated) )
object@start_year <- as.numeric(gtkEntryGetText(entry_StartYear_simulation) )
object@end_year <- as.numeric(gtkEntryGetText(entry_EndYear_simulation) )
object@presimulation_years <- as.numeric(gtkEntryGetText(entryYearsToBePreSimulated))
object@seed_rundomization_runs <- as.numeric(gtkEntryGetText(entryNumberRunSeedRandomization))

object@reference_points_calculation <- gtkToggleButtonGetActive(chkReferencePoints) 

object@CI_calculation <- gtkToggleButtonGetActive(chkConfidenceIntervals) 

if (object@CI_calculation) {
   object@CI_n_runs <- as.numeric(as.character(gtkEntryGetText(entry_CI_numb_runs) ))
   
   if (gtkToggleButtonGetActive(radio_CI_err_additive) ) {
       object@CI_error_type = 1 # additive (1)
   } else {
       object@CI_error_type = 2 # multiplicative (2)
   }
   
    if (gtkToggleButtonGetActive(radio_recruitment_error_ext_file) ) {
       object@CI_error_source = 1 # external file (1)
       
       # aggiungere lettura del vettore esterno dell'errore
       # object@CI_error.externalvector =    
        
   } else {
       object@CI_error_source = 2 # distribution (2)
       
 #if (type_noise == "Uniform") {
   noise_min <- as.numeric(gtkEntryGetText(entry_noise_recr_min))
noise_max <- as.numeric(gtkEntryGetText(entry_noise_recr_max))
#   object@recruitment.noise <- data.frame(rbind(list( distribution=gtkComboBoxGetActiveText(combo_RecrNoise_dis), min=noise_min, max=noise_max, A="", B=""  ) ), row.names=1)
 #} else {
    noiseA_value <- as.numeric(gtkEntryGetText(entryNoise_a))
noiseB_value <- as.numeric(gtkEntryGetText(entryNoise_b))
     object@recruitment.noise <- data.frame(rbind(list( distribution=gtkComboBoxGetActiveText(combo_RecrNoise_dis), min=noise_min, max=noise_max, A=noiseA_value, B=noiseB_value  ) ), row.names=1)
# }
     
   }
   
}

# A <- ifelse(gtkComboBoxGetActiveText(combo_OFFSPRING_rand))
# cambiare ......   0

# recruitment R 100 runs
object@R.100runs <- data.frame(rbind(list( distribution=gtkComboBoxGetActiveText(combo_OFFSPRING_rand), 
                                            min=as.numeric(gtkEntryGetText(entryOFFSPRING_rand_min)), 
                                            max=as.numeric(gtkEntryGetText(entryOFFSPRING_rand_max)), 
                                            A=as.numeric(gtkEntryGetText(entryOFFSPRING_rand_a)), 
                                            B=as.numeric(gtkEntryGetText(entryOFFSPRING_rand_b)) ) ), row.names=1)
   
object@monthlyoffspring <- data.frame(monthlyOffsprings)
object@Tr <- as.numeric(gtkEntryGetText(entryOFFSPRING_tr))

# stock recruitment relationship
object@stockr.relationship <- data.frame(rbind(list( relationship=gtkComboBoxGetActiveText(combo_SRtype), 
                                            a=as.numeric(gtkEntryGetText(entrySR_params_a)), 
                                            b=as.numeric(gtkEntryGetText(entrySR_params_b)), 
                                            c=as.numeric(gtkEntryGetText(entrySR_params_c)) ) ), row.names=1)

#object@stockr.file <- stockr_file
object@stockr.vector <- data.frame(get_table("RECRUITMENT"))

if ( gtkToggleButtonGetActive(radio_tons) ) {
  object@stockr.unit <- "Tons (biomass)"
} else if ( gtkToggleButtonGetActive(radio_thousands) ) {
  object@stockr.unit <- "Thousands (numbers)"
} 

object@spawners.ss = gtkComboBoxGetActiveText(combo_SS)
object@spawners.delayss = as.numeric(gtkEntryGetText(entry_delaySS))
object@recruitment.tuning <- gtkToggleButtonGetActive(chkCalibration)
object@recruitment.tuning.range <- list(min=as.numeric(gtkEntryGetText(entry_minrec)), max=as.numeric(gtkEntryGetText(entry_maxrec)))
 
 



# construction of list of natural mortality
object@naturalmortality.M.type <- gtkComboBoxGetActiveText(combo_Mtype_M)

  if (object@naturalmortality.M.type == "M constant") {
     object@naturalmortality.M.constant <- as.numeric(gtkEntryGetText(entryMconstant_M))
  } else if (object@naturalmortality.M.type == "From vector") {
    object@naturalmortality.M.vector <- data.frame(get_table("NATURAL_MORTALITY_M"))
  } else if (object@naturalmortality.M.type == "ProdbiomUniqueSolution") {
     object@naturalmortality.M.tmax <- as.numeric(gtkEntryGetText(entryMconstant_M))
  }

object@naturalmortality.F.type <- gtkComboBoxGetActiveText(combo_Mtype_F)

if (object@naturalmortality.F.type == "M constant") {
object@naturalmortality.F.constant <- as.numeric(gtkEntryGetText(entryMconstant_F))
} else if (object@naturalmortality.F.type == "From vector") {
object@naturalmortality.F.vector <- data.frame(get_table("NATURAL_MORTALITY_F"))
} else if (object@naturalmortality.F.type == "ProdbiomUniqueSolution") {
object@naturalmortality.F.tmax <- as.numeric(gtkEntryGetText(entryMconstant_M))
}

# construction of list of total mortality
#object@totalmortality.M.file <- totalmortalityM_file
object@totalmortality.M.vector <- data.frame(get_table("TOTAL_MORTALITY_M")) 
#object@totalmortality.F.file <- totalmortalityF_file
object@totalmortality.F.vector <- data.frame(get_table("TOTAL_MORTALITY_F")) 

if ( gtkToggleButtonGetActive(radio_Zentry) ) {
  object@enteringMortality <- "Z"
} else if ( gtkToggleButtonGetActive(radio_Fentry) ) {
  object@enteringMortality <- "F"
} 

# construction of list of fishing mortality
object@fishingmortality <- data.frame(rbind(list(rowname="M", min = as.numeric(gtkEntryGetText(entry_agerange_M_min)), max=as.numeric(gtkEntryGetText(entry_agerange_M_max))), list(rowname= "F", min=as.numeric(gtkEntryGetText(entry_agerange_F_min)), max=as.numeric(gtkEntryGetText(entry_agerange_F_max)))), row.names=1) 

object@yearsForAverage <- as.numeric(gtkEntryGetText(entryYearsForAverage))

object@monthlysurvivability <- data.frame(monthly.survivability)

return(object)
}