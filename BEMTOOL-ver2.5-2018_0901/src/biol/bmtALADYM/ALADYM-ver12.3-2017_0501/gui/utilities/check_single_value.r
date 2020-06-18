# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




check_single_value<-function(str_check, term) {
#
# check values in GENERAL DATA
#
 check_ <- ""

if (str_check == "Start year of simulation in GENERAL DATA") {
   check_ <- "n/i"
   number_to_check <- gtkEntryGetText(entry_StartYear_simulation) 
} else if (str_check == "End year of simulation in GENERAL DATA") {
   check_ <- "n/i"
   number_to_check <- gtkEntryGetText(entry_EndYear_simulation) 
} else if (str_check == "Years to be pre-simulated in GENERAL DATA") {
  check_ <- "n/i"
  number_to_check <- gtkEntryGetText(entryYearsToBePreSimulated)
#
# check values in BIOLOGICAL
#
# a, b lenght-weight
} else if (str_check == "a [g/mm^b] (MALES) in BIOLOGICAL") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entryAB_A_M)
} else if (str_check == "b (MALES) in BIOLOGICAL") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entryAB_B_M)            
} else if (str_check == "a [g/mm^b] (FEMALES) in BIOLOGICAL") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entryAB_A_F)
} else if (str_check == "b (FEMALES) in BIOLOGICAL") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entryAB_B_F)


# maturity ogive parameters
# L50%, males and females
} else if (str_check == "min of L50% [mm] (MALES) in BIOLOGICAL") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entryOGIVEL50_M_min)             
} else if (str_check == "max of L50% [mm] (MALES) in BIOLOGICAL") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entryOGIVEL50_M_max) 
} else if (str_check == "1st parameter of L50% [mm] (MALES) in BIOLOGICAL") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_L50dis_M) != "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entryOGIVEL50_M_a)
} else if (str_check == "2nd parameter of L50% [mm] (MALES) in BIOLOGICAL") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_L50dis_M) != "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entryOGIVEL50_M_b)                                                                 
} else if (str_check == "min of L50% [mm] (FEMALES) in BIOLOGICAL") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entryOGIVEL50_F_min)   
} else if (str_check == "max of L50% [mm] (FEMALES) in BIOLOGICAL") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entryOGIVEL50_F_max)                               
} else if (str_check == "1st parameter of L50% [mm] (FEMALES) in BIOLOGICAL") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_L50dis_F) != "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entryOGIVEL50_F_a)
} else if (str_check == "2nd parameter of L50% [mm] (FEMALES) in BIOLOGICAL") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_L50dis_F) != "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entryOGIVEL50_F_b) 
                                                                  
# L75L25%, males and females
} else if (str_check == "min of L75%L25% [mm] (MALES) in BIOLOGICAL") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entryOGIVEL75L25_M_min)             
} else if (str_check == "max of L75%L25% [mm] (MALES) in BIOLOGICAL") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entryOGIVEL75L25_M_max) 
} else if (str_check == "1st parameter of L75%L25% [mm] (MALES) in BIOLOGICAL") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_L75L25dis_M) != "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entryOGIVEL75L25_M_a)
} else if (str_check == "2nd parameter of L75%L25% [mm] (MALES) in BIOLOGICAL") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_L75L25dis_M) != "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entryOGIVEL75L25_M_b)                                                                 
} else if (str_check == "min of L75%L25% [mm] (FEMALES) in BIOLOGICAL") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entryOGIVEL75L25_F_min)   
} else if (str_check == "max of L75%L25% [mm] (FEMALES) in BIOLOGICAL") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entryOGIVEL75L25_F_max)                               
} else if (str_check == "1st parameter of L75%L25% [mm] (FEMALES) in BIOLOGICAL") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_L75L25dis_M) != "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entryOGIVEL75L25_F_a)
} else if (str_check == "2nd parameter of L75%L25% [mm] (FEMALES) in BIOLOGICAL") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_L75L25dis_M) != "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entryOGIVEL50_F_b)

# growth function parameters
# life span, males and females
} else if (str_check == "Lifespan [years] (MALES) in BIOLOGICAL") {
  check_ <- "n/i"
  number_to_check <- gtkEntryGetText(entryVBF_M_lifespan) 
} else if (str_check == "Lifespan [years] (FEMALES) in BIOLOGICAL") {
  check_ <- "n/i"
  number_to_check <- gtkEntryGetText(entryVBF_F_lifespan) 

# t0, males and females
} else if (str_check == "min of t0 [years] (MALES) in BIOLOGICAL") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entryVBFtzero_M_min)             
} else if (str_check == "max of t0 [years] (MALES) in BIOLOGICAL") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entryVBFtzero_M_max) 
} else if (str_check == "1st parameter of t0 [years] (MALES) in BIOLOGICAL") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_t0dis_M) != "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entryVBFt0_M_a)
} else if (str_check == "2nd parameter of t0 [years] (MALES) in BIOLOGICAL") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_t0dis_M) != "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entryVBFt0_M_b)                                                                 
} else if (str_check == "min of t0 [years] (FEMALES) in BIOLOGICAL") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entryVBFtzero_F_min)             
} else if (str_check == "max of t0 [years] (FEMALES) in BIOLOGICAL") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entryVBFtzero_F_max) 
} else if (str_check == "1st parameter of t0 [years] (FEMALES) in BIOLOGICAL") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_t0dis_F) != "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entryVBFt0_F_a)
} else if (str_check == "2nd parameter of t0 [years] (FEMALES) in BIOLOGICAL") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_t0dis_F) != "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entryVBFt0_F_b) 

# K, males and females
} else if (str_check == "min of K [years^-1] (MALES) in BIOLOGICAL") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entryVBFK_M_min)             
} else if (str_check == "max of K [years^-1] (MALES) in BIOLOGICAL") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entryVBFK_M_max) 
} else if (str_check == "1st parameter of K [years^-1] (MALES) in BIOLOGICAL") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_Kdis_M) != "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entryVBFK_M_a)
} else if (str_check == "2nd parameter of K [years^-1] (MALES) in BIOLOGICAL") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_Kdis_M) != "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entryVBFK_M_b)                                                                 
} else if (str_check == "min of K [years^-1] (FEMALES) in BIOLOGICAL") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entryVBFK_F_min)             
} else if (str_check == "max of K [years^-1] (FEMALES) in BIOLOGICAL") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entryVBFK_F_max) 
} else if (str_check == "1st parameter of K [years^-1] (FEMALES) in BIOLOGICAL") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_Kdis_F) != "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entryVBFK_F_a)
} else if (str_check == "2nd parameter of K [years^-1] (FEMALES) in BIOLOGICAL") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_Kdis_F) != "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entryVBFK_F_b) 

# Linfinity, males and females
} else if (str_check == "min of Linfinity [mm] (MALES) in BIOLOGICAL") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entryVBFLinf_M_min)             
} else if (str_check == "max of Linfinity [mm] (MALES) in BIOLOGICAL") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entryVBFLinf_M_max) 
} else if (str_check == "1st parameter of Linfinity [mm] (MALES) in BIOLOGICAL") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_Linfdis_M) != "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entryVBFLinf_M_a)
} else if (str_check == "2nd parameter of Linfinity [mm] (MALES) in BIOLOGICAL") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_Linfdis_M) != "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entryVBFLinf_M_b)                                                                 
} else if (str_check == "min of Linfinity [mm] (FEMALES) in BIOLOGICAL") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entryVBFLinf_F_min)             
} else if (str_check == "max of Linfinity [mm] (FEMALES) in BIOLOGICAL") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entryVBFLinf_F_max) 
} else if (str_check == "1st parameter of Linfinity [mm] (FEMALES) in BIOLOGICAL") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_Linfdis_F) != "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entryVBFLinf_F_a)
} else if (str_check == "2nd parameter of Linfinity [mm] (FEMALES) in BIOLOGICAL") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_Linfdis_F) != "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entryVBFLinf_F_b) 

# sex ratio
} else if (str_check == "Sex ratio F/F+M in BIOLOGICAL") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entry_SR_value) 

#
# check values in RECRUITMENT
#
# R 100 runs
} else if (str_check == "min of R [thousands] in RECRUITMENT") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entryOFFSPRING_rand_min) 
} else if (str_check == "max of R [thousands] in RECRUITMENT") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entryOFFSPRING_rand_max) 
} else if (str_check == "1st parameter of R [thousands] in RECRUITMENT") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_OFFSPRING_rand) != "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entryOFFSPRING_rand_a) 
} else if (str_check == "2nd parameter of R [thousands] in RECRUITMENT") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_OFFSPRING_rand) != "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entryOFFSPRING_rand_b) 
# Tr
} else if (str_check == "Tr [months] in RECRUITMENT") {
  check_ <- "n/i"
  number_to_check <- gtkEntryGetText(entryOFFSPRING_tr)
                                       
# stock recruitment 
} else if (str_check == "parameter a of SR relationship in RECRUITMENT") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_SRtype) != "from vector", "n", "")
  number_to_check <- gtkEntryGetText(entrySR_params_a)                               
} else if (str_check == "parameter b of SR relationship in RECRUITMENT") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_SRtype) != "from vector", "n", "")
  number_to_check <- gtkEntryGetText(entrySR_params_b)                               
} else if (str_check == "parameter c of SR relationship in RECRUITMENT") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_SRtype) == "Shepherd" | gtkComboBoxGetActiveText(combo_SRtype) == "Hockey-Stick quadratic" , "n", "")
  number_to_check <- gtkEntryGetText(entrySR_params_c)                               

# delay for SS                              
} else if (str_check == "Delay for SS calculation in RECRUITMENT") {
  check_ <- "n/i"
  number_to_check <- gtkEntryGetText(entry_delaySS)
  
# noise on recruitment  
} else if (str_check == "1st parameter of Noise in RECRUITMENT") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_RecrNoise_dis) != "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entryNoise_a)                               
} else if (str_check == "2nd parameter of Noise in RECRUITMENT") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_RecrNoise_dis) != "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entryNoise_b)                               
} else if (str_check == "min value of Noise in RECRUITMENT") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_RecrNoise_dis) == "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entry_noise_recr_min)                               
} else if (str_check == "max value of Noise in RECRUITMENT") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_RecrNoise_dis) == "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entry_noise_recr_max)                               

#
# check values in MORTALITY
#
# natural mortality, males and females
} else if (str_check == "Constant value of Natural Mortality (MALES) in MORTALITY") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_Mtype_M) == "From vector", "n", "")
  number_to_check <- gtkEntryGetText(entryMconstant_M)
} else if (str_check == "Constant value of Natural Mortality (FEMALES) in MORTALITY") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_Mtype_F) == "From vector", "n", "")
  number_to_check <- gtkEntryGetText(entryMconstant_F)
# total mortality, males and females
} else if (str_check == "Seed value of Total Mortality (MALES) in MORTALITY") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entry_Zseedvalue_M)
} else if (str_check == "Seed value of Total Mortality (FEMALES) in MORTALITY") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entry_Zseedvalue_F)
# fishing mortality, males and females
} else if (str_check == "min of Age range for F calculated (MALES) in MORTALITY") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entry_agerange_M_min)                               
} else if (str_check == "min of Age range for F calculated (FEMALES) in MORTALITY") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entry_agerange_F_min)                               
} else if (str_check == "max of Age range for F calculated (MALES) in MORTALITY") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entry_agerange_M_max)                               
} else if (str_check == "max of Age range for F calculated (FEMALES) in MORTALITY") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entry_agerange_F_max)                  

#
# check values in FISHERY
# 
} else if (str_check == "Fleet name in FISHERY") {
  check_ <- "unique"
  number_to_check <- gtkEntryGetText(entryGearName)
} else if (str_check == "Seed value of Production in FISHERY") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entry_Pproduction_seedvalue)
} else if (str_check == "Seed value of Fishing effort in FISHERY") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entry_fact_seedvalue)

#
# check values in FORECAST
#
# noise
} else if (str_check == "1st parameter of noise on recruitment in FORECAST") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_RecrNoise_dis_fore) != "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entryNoise_a_fore)                                                             
} else if (str_check == "2nd parameter of noise on recruitment in FORECAST") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_RecrNoise_dis_fore) != "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entryNoise_b_fore)                                                             
# others 
} else if (str_check == "min value of Noise in RECRUITMENT for forecast") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_RecrNoise_dis_fore) == "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entry_noise_recr_min_fore)                               
} else if (str_check == "max value of Noise in RECRUITMENT for forecast") {
  check_ <- ifelse(gtkComboBoxGetActiveText(combo_RecrNoise_dis_fore) == "Uniform", "n", "")
  number_to_check <- gtkEntryGetText(entry_noise_recr_max_fore)                               
} else if (str_check == "Number years for average in FORECAST") {
  check_ <- "n/i"
  number_to_check <- gtkEntryGetText(entry_yearsForAverage)                                                             
} else if (str_check == "Target F in FORECAST") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entry_targetF)                                                             
} else if (str_check == "First year in FORECAST") {
  check_ <- "n/i"
  number_to_check <- gtkEntryGetText(entry_firstmonth)                               
} else if (str_check == "Target year in FORECAST") {
  check_ <- "n/i"
  number_to_check <- gtkEntryGetText(entry_targetMonth)                               
} else if (str_check == "min value in CALIBRATION") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entry_minrec)
} else if (str_check == "max value in CALIBRATION") {
  check_ <- "n"
  number_to_check <- gtkEntryGetText(entry_maxrec)
} else if (str_check == "Constant recruitment in FORECAST") {
   check_ <- "n"
   if (gtkToggleButtonGetActive(chkConfidenceIntervals_fore)) {
      number_to_check <- gtkEntryGetText(entry_costant_recr_forecast_UN) 
   } else {
   number_to_check <- gtkEntryGetText(entry_costant_recr_forecast_UN) 
   }

} 
  # print(term)
#print(paste("Checking",check_, str_check, number_to_check, ifelse(!is.null(term), paste("vs", term), "")))

if (check_ == "n" | check_ == "n/i") {
    if (is.na(round(as.numeric(number_to_check), 0) == as.numeric(number_to_check))) {
    # not a number
      return(list(result="KO", msg=paste("Value of ",str_check ," must be a number!", sep="") ))
    } 
}

if (check_ == "n/i") {
if (!(round(as.numeric(number_to_check), 0) == as.numeric(number_to_check))) {
# not integer
  return(list(result="KO", msg=paste("Value of ",str_check ," must be an integer!", sep="") ))
}
}

#if (str_check == "Years to be simulated in GENERAL DATA" ) {
#  if (as.numeric(number_to_check) < 2) {
#  # specific check on "Years to be simulated"
#      return(list(result="KO", msg=paste("Value of \"",str_check ,"\" must be greater than 1!", sep="") ))
#  }
#}  

if (str_check == "Start year of simulation in GENERAL DATA" ) {
  if ( as.numeric(number_to_check) > as.numeric(term) ) {
  # specific check on "Start year of simulation in GENERAL DATA"
      return(list(result="KO", msg=paste(str_check ," must be less than End year of simulation!", sep="") ))
  } #else if ( (as.numeric(term) - as.numeric(number_to_check)) <= 1 ) {
#      return(list(result="KO", msg=paste("Number of years between ",str_check ," and End year of simulation must be greater than 1!", sep="") ))
#  }
}    


if (str_check == "max of L50% (MALES) in BIOLOGICAL" ) { 
 if (as.numeric(number_to_check) > as.numeric(term)) {
  # specific check on "Max of L50%"
      return(list(result="KO", msg=paste("Value of ",str_check ," must be greater than Max of Linf (MALES)!", sep="") ))
  }
}

if (str_check == "max of L50% (FEMALES) in BIOLOGICAL" ) { 
 if (as.numeric(number_to_check) > as.numeric(term)) {
  # specific check on "Max of L50%"
      return(list(result="KO", msg=paste("Value of ",str_check ," must be greater than Max of Linf (FEMALES)!", sep="") ))
  }
}

if (str_check == "Sex ratio in BIOLOGICAL" ) { 
 if (as.numeric(number_to_check) < 0 | as.numeric(number_to_check) > 1) {
  # specific check on "Sex ratio"
      return(list(result="KO", msg=paste("Value of ",str_check ," must be between 0 and 1!", sep="") ))
  }
}

if (str_check == "Tr in RECRUITMENT" ) { 
term_ <- min(term)
 if (as.numeric(number_to_check) > (as.numeric(term)*12) ) {
  # specific check on "Tr"
      return(list(result="KO", msg=paste("Value of ",str_check ," must be greater than Life span times 12!", sep="") ))
  }
}

if (str_check == "min of Age range for F calculated (MALES) in MORTALITY" | str_check == "min of Age range for F calculated (FEMALES) in MORTALITY") { 
 if (as.numeric(number_to_check) < trunc(as.numeric(Tr)/12) ) {
 print(paste(number_to_check ," < Tr !!!!") )
  # specific check on "Tr"
      return(list(result="KO", msg=paste("Value of ",str_check ," must be equal or greater than 0!", sep="") ))
  }
}
            
if ( str_check == "max of Age range for F calculated (MALES) in MORTALITY" | str_check == "max of Age range for F calculated (FEMALES) in MORTALITY" ) { 
 if (as.numeric(number_to_check) >= as.numeric(term) ) {
  # specific check on "Tr"
   print(paste(number_to_check ," >= lifespan ", term,"!!!!") )
      return(list(result="KO", msg=paste("Value of ",str_check ," must be less than Life span!", sep="") ))
  }
}

if ( str_check == "Fleet name in FISHERY") { 
 if (number_to_check %in% FLEET_NAMES ) {
  # specific check on "Tr"
      return(list(result="KO", msg=paste(str_check ," ", number_to_check, " already exists!", sep="") ))
  }
}


   
return(list(result="OK", msg=""))
}
 