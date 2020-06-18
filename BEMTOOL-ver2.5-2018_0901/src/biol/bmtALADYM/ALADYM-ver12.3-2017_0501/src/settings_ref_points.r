# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




loca_Nsimulation <- max(INP$FGrowth_tend,INP$MGrowth_tend) * 12 * x  +1


# selectivity
if (as.numeric(INP$OPT_F_TYPE)==1) {
# assegnazione dell'ultimo valore
INP$OPT_SG_TYPE2 <- matrix(nrow = loca_Nsimulation, ncol = nb_gears)
  for (g in 1:nb_gears) {
  #if (all(INP$OPT_SG_TYPE[(forecast-Average_forecast*12+1):forecast,g] == INP$OPT_SG_TYPE[forecast,g])) {
  #  for (i in 1:loca_Nsimulation){ 
  #  INP$OPT_SG_TYPE2[i,g] <- mean(INP$OPT_SG_TYPE[(forecast-Average_forecast*12+1):forecast,g]) 
  #  }
  # } else {
     for (i in 1:loca_Nsimulation){ 
      INP$OPT_SG_TYPE2[i,g] <- INP$OPT_SG_TYPE[forecast,g] 
    }
  }

 # assegnazione
 
INP$OPT_SG_TYPE <- INP$OPT_SG_TYPE2

para1 = matrix(nrow=(loca_Nsimulation),ncol=nb_gears)
para2 = matrix(nrow=(loca_Nsimulation),ncol=nb_gears)
para3 = matrix(nrow=(loca_Nsimulation),ncol=nb_gears)
para4 = matrix(nrow=(loca_Nsimulation),ncol=nb_gears)
para5 = matrix(nrow=(loca_Nsimulation),ncol=nb_gears)


for (g in 1:nb_gears) {

#if (all(INP$OPT_SG_TYPE[(forecast-Average_forecast*12+1):forecast,g] == INP$OPT_SG_TYPE[forecast,g])) {         # prima: se il modello di selettività non è mai cambiato negli ultimi x mesi, prendi una media dei parametri
 # for (i in 1:loca_Nsimulation){ 
#  para1[i,g] <-mean(param1[(forecast-Average_forecast*12+1):forecast,g])
#  para2[i,g]  <-mean(param2[(forecast-Average_forecast*12+1):forecast,g])
#  para3[i,g]  <-mean(param3[(forecast-Average_forecast*12+1):forecast,g])
#  para4 [i,g] <-mean(param4[(forecast-Average_forecast*12+1):forecast,g] )
#  para5 [i,g]  <-mean(param5[(forecast-Average_forecast*12+1):forecast,g] )
#
#  }
# } else {
 # prima: se il modello è cambiato negli ultimi x mesi, prendi i parametri dell'ultimo mese
 for (i in 1:loca_Nsimulation){ 
  para1[i,g] <-INP$param1[forecast,g]                                    #ORA: PRENDI SEMPRE ULTIMO MESE (PER STIMARE I REFERENCE POINT SU DIVERSI TC)
  para2[i,g]  <-INP$param2[forecast,g]
  para3[i,g]  <-INP$param3[forecast,g]
  para4 [i,g] <-INP$param4[forecast,g]
  para5 [i,g]  <-INP$param5[forecast,g]
 }
# }
}

#for (i in 1:loca_Nsimulation){ 
INP$param1 <-para1
INP$param2 <-para2
INP$param3 <-para3
INP$param4 <-para4
INP$param5 <-para5
#}

# discard = NA
}

INP$param62 = matrix(nrow=(loca_Nsimulation),ncol=nb_gears)
INP$param72 = matrix(nrow=(loca_Nsimulation),ncol=nb_gears)
INP$Discard2 = matrix(nrow=(loca_Nsimulation),ncol=nb_gears)

INP$Land_obl2  = matrix(nrow = loca_Nsimulation, ncol = nb_gears) 
 
INP$Discard = matrix(nrow=(loca_Nsimulation),ncol=nb_gears)
INP$Type_discard = matrix(nrow=(loca_Nsimulation),ncol=nb_gears)    

# effort e p_Production
INP$Fishing_efforts2 <- matrix(nrow = loca_Nsimulation, ncol = nb_gears)

if (as.numeric(INP$OPT_F_TYPE)==1) { 
INP$p_Production2 <- matrix(nrow = loca_Nsimulation, ncol = nb_gears)
}

for (g in 1:nb_gears) {
# se il modello di selettività non è cambiato negli ultimi x anni, prendi una media dei fishing effort e p production
if (all(INP$OPT_SG_TYPE[(forecast-INP$Average_forecast*12+1):forecast,g] == INP$OPT_SG_TYPE[forecast,g])) {
  for (i in 1:loca_Nsimulation){ 
  INP$Fishing_efforts2[i,g] <-mean(INP$Fishing_efforts[(forecast-INP$Average_forecast*12+1):forecast,g])
  INP$Discard2[i,g] <-ifelse(!is.na(INP$Discard[forecast,g]),INP$Discard[1,g],NA)
  INP$param62[i,g] <-ifelse(!is.na(INP$param6[forecast,g]), mean(INP$param6[(forecast-INP$Average_forecast*12+1):forecast,g]),NA)
  INP$param72[i,g] <-ifelse(!is.na(INP$param7[forecast,g]),mean(INP$param7[(forecast-INP$Average_forecast*12+1):forecast,g]),NA)
  
  INP$Land_obl2  [i,g]= INP$Land_obl [forecast,g]
  INP$Discard   [i,g]= INP$Discard  [forecast,g]
  INP$Type_discard  [i,g]= INP$Type_discard [forecast,g]                                          # <<----------------------------- NEW
     
   
  if (as.numeric(INP$OPT_F_TYPE)==1) { 
  INP$p_Production2 [i,g] <-mean(INP$p_Production[(forecast-INP$Average_forecast*12+1):forecast,g])
  }
  }
 } else {
 # se il modello di selettività è cambiato negli ultimi x anni, prendi l'ultimo valore di fishing effort e p production
 for (i in 1:loca_Nsimulation){ 
  INP$Fishing_efforts2[i,g] <-INP$Fishing_efforts[forecast,g]
  INP$Discard2[i,g] <-INP$Discard[forecast,g]
  INP$param62[i,g] <-INP$param6[forecast,g]
  INP$param72[i,g] <-INP$param7[forecast,g]
  if (as.numeric(INP$OPT_F_TYPE)==1) { 
  INP$p_Production2[i,g] <-INP$p_Production[forecast,g]
  }
  }
 }
}
INP$Fishing_efforts<-INP$Fishing_efforts2
INP$Discard<-INP$Discard2
INP$Land_obl <-INP$Land_obl2
INP$param6<-INP$param62
INP$param7<-INP$param72
if (as.numeric(INP$OPT_F_TYPE)==1) { 
INP$p_Production<-INP$p_Production2
}


# Sex Ratio
INP$Sex_ratio2 <- vector(mode="numeric", length=(loca_Nsimulation))
for (i in 1:loca_Nsimulation){ 
INP$Sex_ratio2[i]<-INP$Sex_ratio[forecast]
}
INP$Sex_ratio<-INP$Sex_ratio2

# Recruits
INP$Recruits2 <- vector(mode="numeric", length=(loca_Nsimulation))

for (i in 1:loca_Nsimulation){ 
INP$Recruits2[i]<-mean(INP$Recruits[(forecast-INP$Average_forecast*12+1):forecast])
}
INP$Recruits<-INP$Recruits2



  SRO$annual_F_calc_M_num                        <- vector(mode="numeric", length=(loca_Nsimulation))
  SRO$annual_F_calc_F_num                        <- vector(mode="numeric", length=(loca_Nsimulation)) 
  SRO$annual_F_calc_M_num_ls                        <- vector(mode="numeric", length=(loca_Nsimulation))
  SRO$annual_F_calc_F_num_ls                        <- vector(mode="numeric", length=(loca_Nsimulation))
  SRO$annual_F_calc_M                            <- vector(mode="numeric", length=(loca_Nsimulation)) 
  SRO$annual_F_calc_F                            <- vector(mode="numeric", length=(loca_Nsimulation)) 
  SRO$annual_F_calc                              <- vector(mode="numeric", length=(loca_Nsimulation))
  SRO$annual_F_calc_ls                             <- vector(mode="numeric", length=(loca_Nsimulation))
  SRO$annual_F_calc_M_ls                            <- vector(mode="numeric", length=(loca_Nsimulation)) 
  SRO$annual_F_calc_F_ls                            <- vector(mode="numeric", length=(loca_Nsimulation)) 
    #
  # Initialize the vectors to store the running data
  #

  BAS$MZ_estimated                    <- vector(mode="numeric", length=(loca_Nsimulation))
  BAS$FZ_estimated                    <- vector(mode="numeric", length=(loca_Nsimulation))

  BAS$MFSS_Number                     <- vector(mode="numeric", length=(loca_Nsimulation))
  BAS$MUSS_Number                     <- vector(mode="numeric", length=(loca_Nsimulation))

  BAS$MFR                             <- vector(mode="numeric", length=(loca_Nsimulation))
  BAS$MUR                             <- vector(mode="numeric", length=(loca_Nsimulation))

  BAS$FFSS_Number                     <- vector(mode="numeric", length=(loca_Nsimulation))
  BAS$FUSS_Number                     <- vector(mode="numeric", length=(loca_Nsimulation))

  BAS$FFR                             <- vector(mode="numeric", length=(loca_Nsimulation))
  BAS$FUR                             <- vector(mode="numeric", length=(loca_Nsimulation))

  #
  # Initialize some of the vectors to store the output data
  #
  SRO$F_calculated                    <- vector(mode="numeric", length=(loca_Nsimulation))
  SRO$F_calculated_by_gear            <- matrix(nrow=(loca_Nsimulation),ncol=nb_gears)
  SRO$pj                              <- matrix(nrow=(loca_Nsimulation),ncol=nb_gears)
  SRO$Capture_biomass                 <- vector(mode="numeric", length=(loca_Nsimulation))
  SRO$Death_biomass                   <- vector(mode="numeric", length=(loca_Nsimulation))
  SRO$Landing_biomass                 <- vector(mode="numeric", length=(loca_Nsimulation))
  SRO$Discard_biomass                 <- vector(mode="numeric", length=(loca_Nsimulation))
  SRO$Landing_length_mean           <- vector(mode="numeric", length=(loca_Nsimulation))
  SRO$Discard_length_mean           <- vector(mode="numeric", length=(loca_Nsimulation))
  SRO$Landing_age_mean           <- vector(mode="numeric", length=(loca_Nsimulation))
  SRO$Discard_age_mean           <- vector(mode="numeric", length=(loca_Nsimulation))
  
  #
  # Initialize some of the vectors to store the output data by gear
  #
  SRO$Capture_biomass_gears           <- matrix(data=0,nrow=(loca_Nsimulation),ncol=nb_gears)
  colnames(SRO$Capture_biomass_gears) = FLEETSEGMENTS_names
  
  SRO$Landing_biomass_gears           <- matrix(data=0,nrow=(loca_Nsimulation),ncol=nb_gears)
  colnames(SRO$Landing_biomass_gears) = FLEETSEGMENTS_names
  
  SRO$Discard_biomass_gears           <- matrix(data=0,nrow=(loca_Nsimulation),ncol=nb_gears)
  colnames(SRO$Discard_biomass_gears) = FLEETSEGMENTS_names
  
  SRO$Discard_biomass_gears_landed          <- matrix(data=0,nrow=(loca_Nsimulation),ncol=nb_gears)
  colnames(SRO$Discard_biomass_gears_landed) = FLEETSEGMENTS_names
  
  SRO$Discard_biomass_gears_sea           <- matrix(data=0,nrow=(loca_Nsimulation),ncol=nb_gears)
  colnames(SRO$Discard_biomass_gears_sea) = FLEETSEGMENTS_names
  
  SRO$Capture_length_mean             <- vector(mode="numeric", length=(loca_Nsimulation))
  SRO$Capture_age_mean                <- vector(mode="numeric", length=(loca_Nsimulation))
  
  SRO$Capture_age_mean_gears           <- matrix(data=0,nrow=(loca_Nsimulation),ncol=nb_gears)
  colnames(SRO$Capture_age_mean_gears) = FLEETSEGMENTS_names
  
  SRO$Landing_age_mean_gears           <- matrix(data=0,nrow=(loca_Nsimulation),ncol=nb_gears)
  colnames(SRO$Landing_age_mean_gears) = FLEETSEGMENTS_names
  
  SRO$Discard_age_mean_gears           <- matrix(data=0,nrow=(loca_Nsimulation),ncol=nb_gears)
  colnames(SRO$Discard_age_mean_gears) = FLEETSEGMENTS_names
  SRO$Discard_age_mean_gears_landed           <- matrix(data=0,nrow=(loca_Nsimulation),ncol=nb_gears)
  colnames(SRO$Discard_age_mean_gears_landed) = FLEETSEGMENTS_names
  SRO$Discard_age_mean_gears_sea           <- matrix(data=0,nrow=(loca_Nsimulation),ncol=nb_gears)
  colnames(SRO$Discard_age_mean_gears_sea) = FLEETSEGMENTS_names
  
  SRO$Capture_length_mean_gears       <- matrix(data=0,nrow=(loca_Nsimulation),ncol=nb_gears)
  colnames(SRO$Capture_length_mean_gears) = FLEETSEGMENTS_names
  
  SRO$Landing_length_mean_gears           <- matrix(data=0,nrow=(loca_Nsimulation),ncol=nb_gears)
  colnames(SRO$Landing_length_mean_gears) = FLEETSEGMENTS_names
  
  SRO$Discard_length_mean_gears           <- matrix(data=0,nrow=(loca_Nsimulation),ncol=nb_gears)
  colnames(SRO$Discard_length_mean_gears) = FLEETSEGMENTS_names
  SRO$Discard_length_mean_gears_landed           <- matrix(data=0,nrow=(loca_Nsimulation),ncol=nb_gears)
  colnames(SRO$Discard_length_mean_gears_landed) = FLEETSEGMENTS_names
  SRO$Discard_length_mean_gears_sea           <- matrix(data=0,nrow=(loca_Nsimulation),ncol=nb_gears)
  colnames(SRO$Discard_length_mean_gears_sea) = FLEETSEGMENTS_names
 
 
  SRO$MFCatch                    <- matrix(data=0, nrow=(loca_Nsimulation), ncol=GLO$MC_number)        #salvataggio catture
  SRO$FFCatch                    <- matrix(data=0, nrow=(loca_Nsimulation), ncol=GLO$FC_number)
  SRO$MFLanding                    <- matrix(data=0, nrow=(loca_Nsimulation), ncol=GLO$MC_number)        #salvataggio catture
  SRO$FFLanding                    <- matrix(data=0, nrow=(loca_Nsimulation), ncol=GLO$FC_number)
  SRO$MFDiscard                    <- matrix(data=0, nrow=(loca_Nsimulation), ncol=GLO$MC_number)        #salvataggio catture
  SRO$FFDiscard                    <- matrix(data=0, nrow=(loca_Nsimulation), ncol=GLO$FC_number)
  
  

  SRO$MFCatch_gears       <- array(data=0,dim=c((loca_Nsimulation),GLO$MC_number,nb_gears))       #salvataggio catture
  SRO$FFCatch_gears       <-  array(data=0,dim=c((loca_Nsimulation),GLO$FC_number,nb_gears))                  #salvataggio catture
  SRO$MFLanding_gears       <- array(data=0,dim=c((loca_Nsimulation),GLO$MC_number,nb_gears))       #salvataggio catture
  SRO$FFLanding_gears       <-  array(data=0,dim=c((loca_Nsimulation),GLO$FC_number,nb_gears))                  #salvataggio catture
  SRO$MFDiscard_gears       <- array(data=0,dim=c((loca_Nsimulation),GLO$MC_number,nb_gears))       #salvataggio catture
  SRO$FFDiscard_gears       <-  array(data=0,dim=c((loca_Nsimulation),GLO$FC_number,nb_gears))                  #salvataggio catture
  
  SRO$MFDiscard_landed <-      matrix(data=0, nrow=(loca_Nsimulation), ncol=GLO$MC_number) 
  SRO$FFDiscard_landed <-      matrix(data=0, nrow=(loca_Nsimulation), ncol=GLO$FC_number)
  SRO$MFDiscard_sea <-    matrix(data=0, nrow=(loca_Nsimulation), ncol=GLO$MC_number) 
  SRO$FFDiscard_sea <-    matrix(data=0, nrow=(loca_Nsimulation), ncol=GLO$FC_number)
  
  SRO$MFDiscard_gears_landed       <- array(data=0,dim=c((loca_Nsimulation),GLO$MC_number,nb_gears))      
  SRO$FFDiscard_gears_landed       <-  array(data=0,dim=c((loca_Nsimulation),GLO$FC_number,nb_gears))                 
  SRO$MFDiscard_gears_sea       <- array(data=0,dim=c((loca_Nsimulation),GLO$MC_number,nb_gears))      
  SRO$FFDiscard_gears_sea       <-  array(data=0,dim=c((loca_Nsimulation),GLO$FC_number,nb_gears))   
  

  SRO$FM_gears = matrix(data=0,nrow=0,ncol=GLO$MC_number)
  SRO$FF_gears = matrix(data=0,nrow =0,ncol=GLO$FC_number)
  
  SRO$annual_F_calc_by_gear  <- matrix(data=0,nrow=(loca_Nsimulation),ncol=nb_gears)
  SRO$annual_F_calc_by_gear_ls  <- matrix(data=0,nrow=(loca_Nsimulation),ncol=nb_gears)
  #
  # Initialize the matrices to store the populations data
  #

  SRO$MFPopulation2                    <- matrix(data=0, nrow=(loca_Nsimulation), ncol=GLO$MC_number)
  SRO$MUPopulation2                   <- matrix(data=0, nrow=(loca_Nsimulation), ncol=GLO$MC_number)

  SRO$FFPopulation2                    <- matrix(data=0, nrow=(loca_Nsimulation), ncol=GLO$FC_number)
  SRO$FUPopulation2                    <- matrix(data=0, nrow=(loca_Nsimulation), ncol=GLO$FC_number)

  SRO$MFPopulation2[1,]                    <- SRO$MFPopulation[forecast,]
  SRO$MUPopulation2[1,]                     <- SRO$MUPopulation[forecast,]

  SRO$FFPopulation2[1,]                     <- SRO$FFPopulation[forecast,] 
  SRO$FUPopulation2[1,]                     <- SRO$FUPopulation[forecast,]

  SRO$MFPopulation                    <- matrix(data=0, nrow=(loca_Nsimulation), ncol=GLO$MC_number)
  SRO$MUPopulation                   <- matrix(data=0, nrow=(loca_Nsimulation), ncol=GLO$MC_number)

  SRO$FFPopulation                    <- matrix(data=0, nrow=(loca_Nsimulation), ncol=GLO$FC_number)
  SRO$FUPopulation                    <- matrix(data=0, nrow=(loca_Nsimulation), ncol=GLO$FC_number)
  
  SRO$MFPopulation [1,]                    <- SRO$MFPopulation2[1,]
  SRO$MUPopulation [1,]                    <- SRO$MUPopulation2[1,]

  SRO$FFPopulation [1,]                    <- SRO$FFPopulation2 [1,]
  SRO$FUPopulation [1,]                    <- SRO$FUPopulation2  [1,]

  # new settings fro discard and escape survival rates
 



  # Working vectors
  loca_MF                             <- vector(mode="numeric", length=GLO$MC_number)
  loca_MZ                             <- vector(mode="numeric", length=GLO$MC_number)
  loca_FF                             <- vector(mode="numeric", length=GLO$FC_number)
  loca_FZ                             <- vector(mode="numeric", length=GLO$FC_number)

  #
  # Run the first simulation to Initialize the vectors to store the running population
  #
  
  GLO$Delta_t                         <- 1 / INP$Time_slice

  #update di F
   # INP$Fm = data.frame(read.table(name_F_by_gear,sep=";",header=TRUE) )     # AGGIUNTO
  
     nb_years_ref_points_calc <- max(INP$FGrowth_tend,INP$MGrowth_tend) * x 
      first_age_fem <- 0 
           first_age_mal <- 0  
    
     n_ages_f <- as.numeric(new_aldPopulation@lifespan[2,1]) 
          n_ages_m <- as.numeric(new_aldPopulation@lifespan[1,1]) 
    n_ages_f <- n_ages_f - trunc(INP$tr/12)
        n_ages_m <- n_ages_m - trunc(INP$tr/12)
    first_age <- trunc(INP$tr/12)

    names_forYe <- rep(c(1:nb_years_ref_points_calc), max(n_ages_f,n_ages_m)) 
    names_forYe <- names_forYe[order(names_forYe)]
  
      names_forYe_f <- rep(c(1:nb_years_ref_points_calc), n_ages_f) 
    names_forYe_f <- names_forYe_f[order(names_forYe_f)]
    
        names_forYe_m <- rep(c(1:nb_years_ref_points_calc), n_ages_m) 
    names_forYe_m <- names_forYe_m[order(names_forYe_m)]
  
   if (as.numeric(INP$OPT_F_TYPE)==2) {
   Fmales_backup <- colnames(INP$Fmales)
   INP$Fmales <- updateF(INP$Fmales,loca_Nsimulation)
    INP$Fmales[,2] <- names_forYe_m
   #colnames(INP$Fm) = as.character(Fm_backup) 
   
   
   
   Fmales_scaled = data.frame(INP$Fmales)
  # F_scaled[,3:ncol(INP$Fm)] <- 0
   for (g in 1:nb_gears) {
   Fmales_scaled[2+g] = Fmales_scaled[2+g] * f
   }

   #INP$Fmales <- cbind(INP$Fmales[,1:2],Fmales_scaled)
   INP$Fmales <- Fmales_scaled
   }
   
    if (as.numeric(INP$OPT_F_TYPE)==2) {
    
   Ffemales_backup <- colnames(INP$Ffemales)
   INP$Ffemales <- updateF(INP$Ffemales,loca_Nsimulation)
   INP$Ffemales[,2] <- names_forYe_f
   #colnames(INP$Fm) = as.character(Fm_backup) 
   Ffemales_scaled = data.frame(INP$Ffemales)
  # F_scaled[,3:ncol(INP$Fm)] <- 0
   for (g in 1:nb_gears) {
   Ffemales_scaled[2+g] = Ffemales_scaled[2+g] * f
   }
   INP$Ffemales <- Ffemales_scaled
   #INP$Ffemales <- cbind(INP$Ffemales[,1:2],Ffemales_scaled)
   }
   

   