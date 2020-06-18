# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





#-------------  
# CALCULATION AND STORE output for forecast
#------------- 
  End = forecast
  ptm_inner_output <- proc.time()
  #-------------------???????????????????????????
  if (FALSE) {
  SRO$Capture_biomass_gears<-ifelse(is.na(SRO$Capture_biomass_gears),NA,SRO$Capture_biomass_gears)
  SRO$Landing_biomass_gears<-ifelse(is.na(SRO$Landing_biomass_gears),NA,SRO$Landing_biomass_gears)
  SRO$Discard_biomass_gears<-ifelse(is.na(SRO$Discard_biomass_gears),NA,SRO$Discard_biomass_gears)
  
  colnames(SRO$Capture_biomass_gears) = FLEETSEGMENTS_names
  colnames(SRO$Landing_biomass_gears) = FLEETSEGMENTS_names
  colnames(SRO$Discard_biomass_gears) = FLEETSEGMENTS_names
  colnames(SRO$Capture_age_mean_gears) = FLEETSEGMENTS_names
  colnames(SRO$Landing_age_mean_gears) = FLEETSEGMENTS_names
  colnames(SRO$Discard_age_mean_gears) = FLEETSEGMENTS_names
  colnames(SRO$Capture_length_mean_gears) = FLEETSEGMENTS_names
  colnames(SRO$Landing_length_mean_gears) = FLEETSEGMENTS_names
  colnames(SRO$Discard_length_mean_gears) = FLEETSEGMENTS_names
  }
  #-------------------???????????????????????????
#  print(paste("Ex. gear 1 year", years.forecast[current_year]))
#  print(SRO$Capture_biomass_gears[(forecast+1):(GLO$L_number+1),1] )
#  print("*****************************************")
#  
  for(loca_i in ((End+1):(GLO$L_number))) {
	SRO$Z_calculated[loca_i]           <- log((sum(SRO$MFPopulation[loca_i, 1:GLO$MC_number]) + sum(SRO$FFPopulation[loca_i, 1:GLO$FC_number])) / (sum(SRO$MFPopulation[loca_i + 1, 2:GLO$MC_number]) + sum(SRO$FFPopulation[loca_i + 1, 2:GLO$FC_number]))) / GLO$Delta_t
    SRO$MZ_calculated[loca_i]          <- log(sum(SRO$MFPopulation[loca_i, 1:GLO$MC_number]) / (sum(SRO$MFPopulation[loca_i + 1, 2:GLO$MC_number]))) / GLO$Delta_t
    SRO$FZ_calculated[loca_i]          <- log(sum(SRO$FFPopulation[loca_i, 1:GLO$FC_number]) / (sum(SRO$FFPopulation[loca_i + 1, 2:GLO$FC_number]))) / GLO$Delta_t
    SRO$F_calculated[loca_i]           <- log(SRO$F_calculated[loca_i] / (sum(SRO$MFPopulation[loca_i, 2:GLO$MC_number]) + sum(SRO$FFPopulation[loca_i, 2:GLO$FC_number]))) / GLO$Delta_t    #***************
  }
 # il valore nel mese GLO$L_number + 1 sarà uguale a quello del mese precedente  
  SRO$Z_calculated[GLO$L_number + 1]   <- SRO$Z_calculated[GLO$L_number]
  SRO$MZ_calculated[GLO$L_number + 1]  <- SRO$MZ_calculated[GLO$L_number]
  SRO$FZ_calculated[GLO$L_number + 1]  <- SRO$FZ_calculated[GLO$L_number]
  SRO$F_calculated[GLO$L_number + 1]   <- SRO$F_calculated[GLO$L_number]


if (!IN_BEMTOOL | (IN_BEMTOOL & !INTEGRATED_APPROACH) ) {
 # calculation of   MZa_calculated e FZa_calculated except for the last year
 
 for(loca_i in ((End+1):(GLO$L_number - 11))) {
    SRO$MZa_calculated[loca_i]         <- log(sum(SRO$MFPopulation[loca_i,(min_ageM*12+1):(max_ageM*12+1-INP$tr)]) / (sum(SRO$MFPopulation[loca_i + 12, (min_ageM*12+13):(max_ageM*12+1-INP$tr)])))
    SRO$FZa_calculated[loca_i]         <- log(sum(SRO$FFPopulation[loca_i, (min_ageF*12+1):(max_ageF*12+1-INP$tr)]) / (sum(SRO$FFPopulation[loca_i + 12, (min_ageF*12+13):(max_ageF*12+1-INP$tr)])))
    
    # su tutto il life span
    SRO$MZa_calculated_ls[loca_i]         <- log(sum(SRO$MFPopulation[loca_i,(1:GLO$MC_number)]) / (sum(SRO$MFPopulation[loca_i + 12, (13:GLO$MC_number)])))
    SRO$FZa_calculated_ls[loca_i]         <- log(sum(SRO$FFPopulation[loca_i, (1:GLO$FC_number)]) / (sum(SRO$FFPopulation[loca_i + 12, (13:GLO$FC_number)])))

  }  

  
  # ultimo passo
  for(loca_i in ((GLO$L_number - 10):(GLO$L_number + 1))) {
  SRO$MZa_calculated[loca_i]         <- meanWequals(SRO$MZa_calculated[(GLO$L_number - 23): (GLO$L_number - 11)], 12, INP$Time_slice)  
  SRO$FZa_calculated[loca_i]       <- meanWequals(SRO$FZa_calculated[(GLO$L_number - 23): (GLO$L_number - 11)], 12, INP$Time_slice)
  
  # su tutto il life span
  SRO$MZa_calculated_ls[loca_i]         <- meanWequals(SRO$MZa_calculated_ls[(GLO$L_number - 23): (GLO$L_number - 11)], 12, INP$Time_slice)  
  SRO$FZa_calculated_ls[loca_i]       <- meanWequals(SRO$FZa_calculated_ls[(GLO$L_number - 23): (GLO$L_number - 11)], 12, INP$Time_slice)
  
  }
 } else if (INTEGRATED_APPROACH ) {     # & current_year == (foreperiod)
 # calculation of   MZa_calculated e FZa_calculated
 
 for(loca_i in (((simperiod*12+1)+1):(GLO$L_number - 11))) {
    SRO$MZa_calculated[loca_i]         <- log(sum(SRO$MFPopulation[loca_i, (min_ageM*12+1):(max_ageM*12+1-INP$tr)]) / (sum(SRO$MFPopulation[loca_i + 12, (min_ageM*12+13):(max_ageM*12+1-INP$tr)])))
    SRO$FZa_calculated[loca_i]         <- log(sum(SRO$FFPopulation[loca_i, (min_ageF*12+1):(max_ageF*12+1-INP$tr)]) / (sum(SRO$FFPopulation[loca_i + 12, (min_ageF*12+13):(max_ageF*12+1-INP$tr)])))
    
    # su tutto il life span
    SRO$MZa_calculated_ls[loca_i]         <- log(sum(SRO$MFPopulation[loca_i,(1:GLO$MC_number)]) / (sum(SRO$MFPopulation[loca_i + 12, (13:GLO$MC_number)])))
    SRO$FZa_calculated_ls[loca_i]         <- log(sum(SRO$FFPopulation[loca_i, (1:GLO$FC_number)]) / (sum(SRO$FFPopulation[loca_i + 12, (13:GLO$FC_number)])))

  }  
  
  # ultimo passo
  for(loca_i in ((GLO$L_number - 10):(GLO$L_number + 1))) {
  SRO$MZa_calculated[loca_i]         <- meanWequals(SRO$MZa_calculated[(GLO$L_number - 23): (GLO$L_number - 11)], 12, INP$Time_slice)  
  SRO$FZa_calculated[loca_i]       <- meanWequals(SRO$FZa_calculated[(GLO$L_number - 23): (GLO$L_number - 11)], 12, INP$Time_slice)
  
  # su tutto il life span
  SRO$MZa_calculated_ls[loca_i]         <- meanWequals(SRO$MZa_calculated_ls[(GLO$L_number - 23): (GLO$L_number - 11)], 12, INP$Time_slice)  
  SRO$FZa_calculated_ls[loca_i]       <- meanWequals(SRO$FZa_calculated_ls[(GLO$L_number - 23): (GLO$L_number - 11)], 12, INP$Time_slice)
  
  }
 
 }  

# # calculation of   MZa_calculated e FZa_calculated
#  for(loca_i in (End+1):(GLO$L_number - 11)) {  
#    SRO$MZa_calculated_ls[loca_i]         <- log(sum(SRO$MFPopulation[loca_i,(1:GLO$MC_number)]) / (sum(SRO$MFPopulation[loca_i + 12, (13:GLO$MC_number)])) )
#    SRO$FZa_calculated_ls[loca_i]         <- log(sum(SRO$FFPopulation[loca_i, (1:GLO$FC_number)]) / (sum(SRO$FFPopulation[loca_i + 12, (13:GLO$FC_number)])))
#    
#    if ((min_ageM*12+1)>(max_ageM*12+1-INP$tr)) {
#    SRO$MZa_calculated[loca_i] =  SRO$MZa_calculated_ls[loca_i]    
#    }    else {   
#    SRO$MZa_calculated[loca_i]         <- log(sum(SRO$MFPopulation[loca_i,(min_ageM*12+1):(max_ageM*12+1-INP$tr)]) / (sum(SRO$MFPopulation[loca_i + 12, (min_ageM*12+13):(max_ageM*12+1-INP$tr)])))
#    
#    }
#    if ((min_ageF*12+1)>(max_ageF*12+1-INP$tr)){
#    SRO$FZa_calculated[loca_i] =   SRO$FZa_calculated_ls[loca_i]
#    }else {
#    SRO$FZa_calculated[loca_i]         <- log(sum(SRO$FFPopulation[loca_i, (min_ageF*12+1):(max_ageF*12+1-INP$tr)]) / (sum(SRO$FFPopulation[loca_i + 12, (min_ageF*12+13):(max_ageF*12+1-INP$tr)])))
#    }
#  # tutto il life span
#  }   
#
#  # ultimo passo
#  if (GLO$L_number>23) {
#  for(loca_i in ((GLO$L_number - 10):(GLO$L_number + 1))) {
#  SRO$MZa_calculated[loca_i]         <- meanWequals(SRO$MZa_calculated[(GLO$L_number - 23): (GLO$L_number - 11)], 12, INP$Time_slice)  
#  SRO$FZa_calculated[loca_i]       <- meanWequals(SRO$FZa_calculated[(GLO$L_number - 23): (GLO$L_number - 11)], 12, INP$Time_slice)
#  
#  # su tutto il life span
#  SRO$MZa_calculated_ls[loca_i]         <- meanWequals(SRO$MZa_calculated_ls[(GLO$L_number - 23): (GLO$L_number - 11)], 12, INP$Time_slice)  
#  SRO$FZa_calculated_ls[loca_i]       <- meanWequals(SRO$FZa_calculated_ls[(GLO$L_number - 23): (GLO$L_number - 11)], 12, INP$Time_slice)
#  
#  }
#  }

  # -------------------------------------------------------------------------------------
 
 SRO$Biological_production            <- SRO$Death_biomass + SRO$Capture_biomass

# exploited POPULATION
  for(loca_i in ((End+1):(GLO$L_number + 1))) {


  # exploited population
	loca_numerosity                  <- sum(SRO$MFPopulation[loca_i,]) + sum(SRO$FFPopulation[loca_i,])
    SRO$FBiomass[loca_i]             <- (sum(SRO$MFPopulation[loca_i,] * BAS$MWeight) + sum(SRO$FFPopulation[loca_i,] * BAS$FWeight)) / 1E6
    SRO$FLength_mean[loca_i]         <- (sum(SRO$MFPopulation[loca_i,] * BAS$MLength) + sum(SRO$FFPopulation[loca_i,] * BAS$FLength))  / loca_numerosity
	SRO$FAge_mean[loca_i]            <- (sum(SRO$MFPopulation[loca_i,] * BAS$MAge) + sum(SRO$FFPopulation[loca_i,] * BAS$FAge)) / loca_numerosity
    
    
    # unexploited population
    loca_numerosity                  <- sum(SRO$MUPopulation[loca_i,]) + sum(SRO$FUPopulation[loca_i,])
    SRO$UBiomass[loca_i]             <- (sum(SRO$MUPopulation[loca_i,] * BAS$MWeight) + sum(SRO$FUPopulation[loca_i,] * BAS$FWeight)) / 1E6
    SRO$ULength_mean[loca_i]         <- (sum(SRO$MUPopulation[loca_i,] * BAS$MLength) + sum(SRO$FUPopulation[loca_i,] * BAS$FLength))  / loca_numerosity
    SRO$UAge_mean[loca_i]            <- (sum(SRO$MUPopulation[loca_i,] * BAS$MAge) + sum(SRO$FUPopulation[loca_i,] * BAS$FAge)) / loca_numerosity
    
     	if (SS==1) {
		# exploited population
      loca_numerositySS                <- sum(SRO$FFPopulation[loca_i,] * BAS$FMaturity)
      SRO$FSSBiomass[loca_i]           <- sum(SRO$FFPopulation[loca_i,] * BAS$FMaturity * BAS$FWeight)
      SRO$FSSLength_mean[loca_i]       <- sum(SRO$FFPopulation[loca_i,] * BAS$FMaturity * BAS$FLength )
       SRO$FSSAge_mean[loca_i]          <- sum(SRO$FFPopulation[loca_i,] * BAS$FMaturity * BAS$FAge)
   # unexploited population
      loca_numerositySS                <- sum(SRO$FUPopulation[loca_i,] * BAS$FMaturity)    
  SRO$USSBiomass[loca_i]           <- sum(SRO$FUPopulation[loca_i,] * BAS$FMaturity * BAS$FWeight)
      SRO$USSLength_mean[loca_i]       <- sum(SRO$FUPopulation[loca_i,] * BAS$FMaturity * BAS$FLength)
      SRO$USSAge_mean[loca_i]          <- sum(SRO$FUPopulation[loca_i,] * BAS$FMaturity * BAS$FAge) 
  } else {
		# exploited population
      loca_numerositySS                <- sum(SRO$FFPopulation[loca_i,] * BAS$FMaturity)+sum(SRO$MFPopulation[loca_i,] * BAS$MMaturity)
      SRO$FSSBiomass[loca_i]           <- (sum(SRO$FFPopulation[loca_i,] * BAS$FMaturity * BAS$FWeight)+sum(SRO$MFPopulation[loca_i,] * BAS$MMaturity*BAS$MWeight) )/ 1E6
      SRO$FSSLength_mean[loca_i]       <- (sum(SRO$FFPopulation[loca_i,] * BAS$FMaturity * BAS$FLength)+sum(SRO$MFPopulation[loca_i,] * BAS$MMaturity * BAS$MLength))  / loca_numerositySS
       SRO$FSSAge_mean[loca_i]          <- (sum(SRO$FFPopulation[loca_i,] * BAS$FMaturity * BAS$FAge) +sum(SRO$MFPopulation[loca_i,] * BAS$MMaturity * BAS$MAge))/ loca_numerositySS
   # unexploited population
  
	loca_numerositySS                <- sum(SRO$FUPopulation[loca_i,] * BAS$FMaturity)+sum(SRO$MUPopulation[loca_i,] * BAS$MMaturity  )      
	SRO$USSBiomass[loca_i]           <- (sum(SRO$FUPopulation[loca_i,] * BAS$FMaturity * BAS$FWeight)+sum(SRO$MUPopulation[loca_i,] * BAS$MMaturity * BAS$MWeight) )/ 1E6
    SRO$USSLength_mean[loca_i]       <- (sum(SRO$FUPopulation[loca_i,] * BAS$FMaturity * BAS$FLength)+sum(SRO$MUPopulation[loca_i,] * BAS$MMaturity * BAS$MLength)) / loca_numerositySS
    SRO$USSAge_mean[loca_i]          <-  (sum(SRO$FUPopulation[loca_i,] * BAS$FMaturity * BAS$FAge)+sum(SRO$MUPopulation[loca_i,] * BAS$MMaturity * BAS$MAge))/ loca_numerositySS     
 } 
   
	}

  # ESSB/USSB Indicator
  SRO$FSSBratioUSSB                  <-   SRO$FSSBiomass / SRO$USSBiomass

## stime dell'ultimo anno
#	 for(loca_irun in(End+1):(GLO$L_number - 11) ) { 
#   SRO$annual_F_calc_M_ls [loca_irun] = log(SRO$annual_F_calc_M_num_ls[loca_irun]/sum(SRO$MFPopulation[loca_irun+12,(13:GLO$MC_number)]) )             
#	SRO$annual_F_calc_F_ls [loca_irun] = log(SRO$annual_F_calc_F_num_ls[loca_irun]/sum(SRO$FFPopulation[loca_irun+12,(13:GLO$FC_number)]) )
#	SRO$annual_F_calc_ls [loca_irun] = log((SRO$annual_F_calc_F_num_ls[loca_irun]+SRO$annual_F_calc_M_num_ls[loca_irun])/(sum(SRO$FFPopulation[loca_irun+12,(13:GLO$FC_number)])+sum(SRO$MFPopulation[loca_irun+12,(13:GLO$MC_number)])))
#  if ((13+min_ageM*12)>(max_ageM*12+1-INP$tr))  {
#  SRO$annual_F_calc_M [loca_irun] = SRO$annual_F_calc_M_ls [loca_irun]
#  }  else {
#  SRO$annual_F_calc_M [loca_irun] = log(SRO$annual_F_calc_M_num[loca_irun]/sum(SRO$MFPopulation[loca_irun+12,(13+min_ageM*12):(max_ageM*12+1-INP$tr)]) )
#  }
#  if ((13+min_ageF*12)>(max_ageF*12+1-INP$tr))     {
#  SRO$annual_F_calc_F [loca_irun] =SRO$annual_F_calc_F_ls [loca_irun] 
#  } else {	
#  SRO$annual_F_calc_F [loca_irun] = log(SRO$annual_F_calc_F_num[loca_irun]/sum(SRO$FFPopulation[loca_irun+12,(13+min_ageF*12):(max_ageF*12+1-INP$tr)]) )
#  }	
#	}
#
#    # ultimo anno: prende il valore dell'anno prima
#	if (GLO$L_number>23) {
#		for(loca_i in ((GLO$L_number - 10):(GLO$L_number + 1))) {
#			SRO$annual_F_calc_M[loca_i]   <-meanWequals(as.numeric(as.character(SRO$annual_F_calc_M[(GLO$L_number - 23): (GLO$L_number - 11)])),12,INP$Time_slice)
#			SRO$annual_F_calc_F[loca_i]   <-meanWequals(as.numeric(as.character(SRO$annual_F_calc_F[(GLO$L_number - 23): (GLO$L_number - 11)])),12,INP$Time_slice)
#			SRO$annual_F_calc [loca_i]    <-meanWequals(as.numeric(as.character(SRO$annual_F_calc[(GLO$L_number - 23): (GLO$L_number - 11)])),12,INP$Time_slice) 
#		
#	  # su tutto il life span
#		SRO$annual_F_calc_M_ls[loca_i] <- meanWequals(as.numeric(as.character(SRO$annual_F_calc_M_ls[(GLO$L_number - 23): (GLO$L_number - 11)])),12,INP$Time_slice) 
#			SRO$annual_F_calc_F_ls[loca_i] <- meanWequals(as.numeric(as.character(SRO$annual_F_calc_F_ls[(GLO$L_number - 23): (GLO$L_number - 11)])),12,INP$Time_slice)
#			SRO$annual_F_calc_ls [loca_i] <- meanWequals(as.numeric(as.character(SRO$annual_F_calc_ls[(GLO$L_number - 23): (GLO$L_number - 11)])),12,INP$Time_slice)
#
#	 
#		}
#	}
#
#

if (!IN_BEMTOOL | (IN_BEMTOOL & !INTEGRATED_APPROACH) ) {  
# stime dell'ultimo anno
	 for(loca_irun in (End+1):(GLO$L_number - 11))  {      #    
	  SRO$annual_F_calc_M [loca_irun] = log(SRO$annual_F_calc_M_num[loca_irun]/sum(SRO$MFPopulation[loca_irun+12,(13+min_ageM*12):(max_ageM*12+1-INP$tr)]) )
	  SRO$annual_F_calc_F [loca_irun] = log(SRO$annual_F_calc_F_num[loca_irun]/sum(SRO$FFPopulation[loca_irun+12,(13+min_ageF*12):(max_ageF*12+1-INP$tr)]) )
	  SRO$annual_F_calc [loca_irun] = log((SRO$annual_F_calc_F_num[loca_irun]+SRO$annual_F_calc_M_num[loca_irun])/(sum(SRO$FFPopulation[loca_irun+12,(13+min_ageF*12):(max_ageF*12+1-INP$tr)])+sum(SRO$MFPopulation[loca_irun+12,(13+min_ageM*12):(max_ageM*12+1-INP$tr)]) ))
	 
    # su tutto il life span 
	  SRO$annual_F_calc_M_ls [loca_irun] = log(SRO$annual_F_calc_M_num_ls[loca_irun]/sum(SRO$MFPopulation[loca_irun+12,(13:GLO$MC_number)]) )
	  SRO$annual_F_calc_F_ls [loca_irun] = log(SRO$annual_F_calc_F_num_ls[loca_irun]/sum(SRO$FFPopulation[loca_irun+12,(13:GLO$FC_number)] ))
	  SRO$annual_F_calc_ls [loca_irun] = log((SRO$annual_F_calc_F_num_ls[loca_irun]+SRO$annual_F_calc_M_num_ls[loca_irun])/(sum(SRO$FFPopulation[loca_irun+12,(13:GLO$FC_number)])+sum(SRO$MFPopulation[loca_irun+12,(13:GLO$MC_number)]) ))
	  }

    # ultimo anno: prende il valore dell'anno prima
	for(loca_i in ((GLO$L_number - 10):(GLO$L_number + 1))) {
		SRO$annual_F_calc_M[loca_i]         <-meanWequals(SRO$annual_F_calc_M[(GLO$L_number - 23): (GLO$L_number - 11)],12,INP$Time_slice)
		SRO$annual_F_calc_F[loca_i]         <-meanWequals(SRO$annual_F_calc_F[(GLO$L_number - 23): (GLO$L_number - 11)],12,INP$Time_slice)
		SRO$annual_F_calc [loca_i]          <-meanWequals(SRO$annual_F_calc[(GLO$L_number - 23): (GLO$L_number - 11)],12,INP$Time_slice) 
	
  # su tutto il life span
    SRO$annual_F_calc_M_ls[loca_i]         <- meanWequals(SRO$annual_F_calc_M_ls[(GLO$L_number - 23): (GLO$L_number - 11)],12,INP$Time_slice) 
		SRO$annual_F_calc_F_ls[loca_i]         <- meanWequals(SRO$annual_F_calc_F_ls[(GLO$L_number - 23): (GLO$L_number - 11)],12,INP$Time_slice)
		SRO$annual_F_calc_ls [loca_i]          <- meanWequals(SRO$annual_F_calc_ls[(GLO$L_number - 23): (GLO$L_number - 11)],12,INP$Time_slice)

  }

    } else if (INTEGRATED_APPROACH ) {     #& current_year == (foreperiod)
    
  # stime dell'ultimo anno
	 for(loca_irun in ((simperiod*12+1)+1):(GLO$L_number - 11))  {      #    
	  SRO$annual_F_calc_M [loca_irun] = log(SRO$annual_F_calc_M_num[loca_irun]/sum(SRO$MFPopulation[loca_irun+12,(13+min_ageM*12):(max_ageM*12+1-INP$tr)]) )
	  SRO$annual_F_calc_F [loca_irun] = log(SRO$annual_F_calc_F_num[loca_irun]/sum(SRO$FFPopulation[loca_irun+12,(13+min_ageF*12):(max_ageF*12+1-INP$tr)]) )
	  SRO$annual_F_calc [loca_irun] = log((SRO$annual_F_calc_F_num[loca_irun]+SRO$annual_F_calc_M_num[loca_irun])/(sum(SRO$FFPopulation[loca_irun+12,(13+min_ageF*12):(max_ageF*12+1-INP$tr)])+sum(SRO$MFPopulation[loca_irun+12,(13+min_ageM*12):(max_ageM*12+1-INP$tr)]) ))
	 
    # su tutto il life span 
	  SRO$annual_F_calc_M_ls [loca_irun] = log(SRO$annual_F_calc_M_num_ls[loca_irun]/sum(SRO$MFPopulation[loca_irun+12,(13:GLO$MC_number)]) )
	  SRO$annual_F_calc_F_ls [loca_irun] = log(SRO$annual_F_calc_F_num_ls[loca_irun]/sum(SRO$FFPopulation[loca_irun+12,(13:GLO$FC_number)] ))
	  SRO$annual_F_calc_ls [loca_irun] = log((SRO$annual_F_calc_F_num_ls[loca_irun]+SRO$annual_F_calc_M_num_ls[loca_irun])/(sum(SRO$FFPopulation[loca_irun+12,(13:GLO$FC_number)])+sum(SRO$MFPopulation[loca_irun+12,(13:GLO$MC_number)]) ))
	  }

    # ultimo anno: prende il valore dell'anno prima
	for(loca_i in ((GLO$L_number - 10):(GLO$L_number + 1))) {
		SRO$annual_F_calc_M[loca_i]         <-meanWequals(SRO$annual_F_calc_M[(GLO$L_number - 23): (GLO$L_number - 11)],12,INP$Time_slice)
		SRO$annual_F_calc_F[loca_i]         <-meanWequals(SRO$annual_F_calc_F[(GLO$L_number - 23): (GLO$L_number - 11)],12,INP$Time_slice)
		SRO$annual_F_calc [loca_i]          <-meanWequals(SRO$annual_F_calc[(GLO$L_number - 23): (GLO$L_number - 11)],12,INP$Time_slice) 
	
  # su tutto il life span
    SRO$annual_F_calc_M_ls[loca_i]         <- meanWequals(SRO$annual_F_calc_M_ls[(GLO$L_number - 23): (GLO$L_number - 11)],12,INP$Time_slice) 
		SRO$annual_F_calc_F_ls[loca_i]         <- meanWequals(SRO$annual_F_calc_F_ls[(GLO$L_number - 23): (GLO$L_number - 11)],12,INP$Time_slice)
		SRO$annual_F_calc_ls [loca_i]          <- meanWequals(SRO$annual_F_calc_ls[(GLO$L_number - 23): (GLO$L_number - 11)],12,INP$Time_slice)

  }  
    
    }

	for (g in 1:nb_gears)  {
	SRO$F_calculated_by_gear[,g] = SRO$F_calculated * SRO$pj[,g]        
	SRO$annual_F_calc_by_gear [,g] =  SRO$annual_F_calc *SRO$pj[,g]
	
	SRO$annual_F_calc_by_gear_ls [,g] =  SRO$annual_F_calc_ls *SRO$pj[,g]
	}
	
for(loca_irun in c((End):(GLO$L_number))) { 
SRO$FFLanding[loca_irun +1, (1:(GLO$FC_number - 1))] <- rowSums(data.frame(SRO$FFLanding_gears[loca_irun +1, (1:(GLO$FC_number - 1)),]),na.rm=T)   #salvataggio catture
SRO$FFDiscard[loca_irun +1, (1:(GLO$FC_number - 1))] <- rowSums(data.frame(SRO$FFDiscard_gears[loca_irun +1, (1:(GLO$FC_number - 1)),]),na.rm=T)   #salvataggio catture
SRO$FFDiscard_landed[loca_irun +1, (1:(GLO$FC_number - 1))] <- rowSums(data.frame(SRO$FFDiscard_gears_landed[loca_irun +1, (1:(GLO$FC_number - 1)),]),na.rm=T)  
SRO$FFDiscard_sea[loca_irun +1, (1:(GLO$FC_number - 1))] <- rowSums(data.frame(SRO$FFDiscard_gears_sea[loca_irun +1, (1:(GLO$FC_number - 1)),]),na.rm=T)  
SRO$MFLanding[loca_irun +1, (1:(GLO$MC_number - 1))] <- rowSums(data.frame(SRO$MFLanding_gears[loca_irun +1, (1:(GLO$MC_number - 1)),]),na.rm=T)   #salvataggio catture
SRO$MFDiscard[loca_irun +1, (1:(GLO$MC_number - 1))] <- rowSums(data.frame(SRO$MFDiscard_gears[loca_irun +1, (1:(GLO$MC_number - 1)),]),na.rm=T)   #salvataggio catture
SRO$MFDiscard_landed[loca_irun +1, (1:(GLO$MC_number - 1))] <- rowSums(data.frame(SRO$MFDiscard_gears_landed[loca_irun +1, (1:(GLO$MC_number - 1)),]),na.rm=T)  
SRO$MFDiscard_sea[loca_irun +1, (1:(GLO$MC_number - 1))] <- rowSums(data.frame(SRO$MFDiscard_gears_sea[loca_irun +1, (1:(GLO$MC_number - 1)),]),na.rm=T)  
SRO$Landing_biomass[loca_irun +1] = sum(SRO$Landing_biomass_gears[loca_irun +1,],na.rm=T)
SRO$Discard_biomass[loca_irun +1] = sum(SRO$Discard_biomass_gears[loca_irun +1,],na.rm=T)
SRO$Discard_biomass_landed[loca_irun +1] = sum(SRO$Discard_biomass_gears_landed[loca_irun +1,],na.rm=T)
SRO$Discard_biomass_sea[loca_irun +1] = sum(SRO$Discard_biomass_gears_sea[loca_irun +1,],na.rm=T)
}

print(round( (proc.time() - ptm_inner_output) ,2) , quote=F )  
