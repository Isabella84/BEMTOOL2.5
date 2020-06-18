# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




RunModel <- function(Start,End) {

if (FALSE) {
Start = 1
End = forecast-1
}

  #
  # Initialize the vectors to store the results of the baseline value
  #

  BAS$MAge                            <- vector(mode="numeric", length=GLO$MC_number)
  BAS$MLength                         <- vector(mode="numeric", length=GLO$MC_number)
  BAS$MWeight                         <- vector(mode="numeric", length=GLO$MC_number)
  BAS$MM                              <- vector(mode="numeric", length=GLO$MC_number)
  BAS$MMaturity                       <- vector(mode="numeric", length=GLO$MC_number)

  BAS$FAge                            <- vector(mode="numeric", length=GLO$FC_number)
  BAS$FLength                         <- vector(mode="numeric", length=GLO$FC_number)
  BAS$FWeight                         <- vector(mode="numeric", length=GLO$FC_number)
  BAS$FM                              <- vector(mode="numeric", length=GLO$FC_number)
  BAS$FMaturity                       <- vector(mode="numeric", length=GLO$FC_number)

  #
  # Initialize the vectors to store the running population temporary
  #

  BAS$MFPopulation                    <- vector(mode="numeric", length=GLO$MC_number)
  BAS$MUPopulation                    <- vector(mode="numeric", length=GLO$MC_number)

  BAS$FFPopulation                    <- vector(mode="numeric", length=GLO$FC_number)
  BAS$FUPopulation                    <- vector(mode="numeric", length=GLO$FC_number)

  SRO$annual_F_calc_M_num                        <- vector(mode="numeric", length=(GLO$L_number + 1))
  SRO$annual_F_calc_F_num                        <- vector(mode="numeric", length=(GLO$L_number + 1)) 
  SRO$annual_F_calc_M_num_ls                        <- vector(mode="numeric", length=(GLO$L_number + 1))
  SRO$annual_F_calc_F_num_ls                        <- vector(mode="numeric", length=(GLO$L_number + 1))
  SRO$annual_F_calc_M                            <- vector(mode="numeric", length=(GLO$L_number + 1)) 
  SRO$annual_F_calc_F                            <- vector(mode="numeric", length=(GLO$L_number + 1)) 
  SRO$annual_F_calc                              <- vector(mode="numeric", length=(GLO$L_number + 1))
  SRO$annual_F_calc_ls                             <- vector(mode="numeric", length=(GLO$L_number + 1))
  SRO$annual_F_calc_M_ls                            <- vector(mode="numeric", length=(GLO$L_number + 1)) 
  SRO$annual_F_calc_F_ls                            <- vector(mode="numeric", length=(GLO$L_number + 1)) 
    #
  # Initialize the vectors to store the running data
  #

  BAS$MZ_estimated                    <- vector(mode="numeric", length=(GLO$L_number + 1))
  BAS$FZ_estimated                    <- vector(mode="numeric", length=(GLO$L_number + 1))

  BAS$MFSS_Number                     <- vector(mode="numeric", length=(GLO$L_number + 1))
  BAS$MUSS_Number                     <- vector(mode="numeric", length=(GLO$L_number + 1))

  BAS$MFR                             <- vector(mode="numeric", length=(GLO$L_number + 1))
  BAS$MUR                             <- vector(mode="numeric", length=(GLO$L_number + 1))

  BAS$FFSS_Number                     <- vector(mode="numeric", length=(GLO$L_number + 1))
  BAS$FUSS_Number                     <- vector(mode="numeric", length=(GLO$L_number + 1))

  BAS$FFR                             <- vector(mode="numeric", length=(GLO$L_number + 1))
  BAS$FUR                             <- vector(mode="numeric", length=(GLO$L_number + 1))

  #
  # Initialize some of the vectors to store the output data
  #
  SRO$F_calculated                    <- vector(mode="numeric", length=(GLO$L_number + 1))
  SRO$F_calculated_by_gear            <- matrix(nrow=(GLO$L_number+1),ncol=nb_gears)
  SRO$pj                              <- matrix(nrow=(GLO$L_number+1),ncol=nb_gears)
  SRO$Capture_biomass                 <- vector(mode="numeric", length=(GLO$L_number + 1))
  SRO$Death_biomass                   <- vector(mode="numeric", length=(GLO$L_number + 1))
  SRO$Landing_biomass                 <- vector(mode="numeric", length=(GLO$L_number + 1))
  SRO$Discard_biomass                 <- vector(mode="numeric", length=(GLO$L_number + 1))
  SRO$Discard_biomass_landed                 <- vector(mode="numeric", length=(GLO$L_number + 1))
  SRO$Discard_biomass_sea                 <- vector(mode="numeric", length=(GLO$L_number + 1))
  
  SRO$Landing_length_mean           <- vector(mode="numeric", length=(GLO$L_number + 1))
  SRO$Discard_length_mean           <- vector(mode="numeric", length=(GLO$L_number + 1))
  SRO$Discard_length_mean_landed           <- vector(mode="numeric", length=(GLO$L_number + 1))
  SRO$Discard_length_mean_sea           <- vector(mode="numeric", length=(GLO$L_number + 1))
  
  SRO$Landing_age_mean           <- vector(mode="numeric", length=(GLO$L_number + 1))
  SRO$Discard_age_mean           <- vector(mode="numeric", length=(GLO$L_number + 1))
  SRO$Discard_age_mean__landed           <- vector(mode="numeric", length=(GLO$L_number + 1))
  SRO$Discard_age_mean_sea           <- vector(mode="numeric", length=(GLO$L_number + 1))
  
  #
  # Initialize some of the vectors to store the output data by gear
  #
  SRO$Capture_biomass_gears           <- matrix(data=0,nrow=(GLO$L_number+1),ncol=nb_gears)
  colnames(SRO$Capture_biomass_gears) = FLEETSEGMENTS_names
  
  SRO$Landing_biomass_gears           <- matrix(data=0,nrow=(GLO$L_number+1),ncol=nb_gears)
  colnames(SRO$Landing_biomass_gears) = FLEETSEGMENTS_names
  
  SRO$Discard_biomass_gears           <- matrix(data=NA,nrow=(GLO$L_number+1),ncol=nb_gears)
  colnames(SRO$Discard_biomass_gears) = FLEETSEGMENTS_names
  SRO$Discard_biomass_gears_landed          <- matrix(data=NA,nrow=(GLO$L_number+1),ncol=nb_gears)
  colnames(SRO$Discard_biomass_gears_landed) = FLEETSEGMENTS_names
  
  SRO$Discard_biomass_gears_sea           <- matrix(data=NA,nrow=(GLO$L_number+1),ncol=nb_gears)
  colnames(SRO$Discard_biomass_gears_sea) = FLEETSEGMENTS_names
  
  SRO$Capture_length_mean             <- vector(mode="numeric", length=(GLO$L_number + 1))
  SRO$Capture_age_mean                <- vector(mode="numeric", length=(GLO$L_number + 1))
  
  SRO$Capture_age_mean_gears           <- matrix(data=0,nrow=(GLO$L_number+1),ncol=nb_gears)
  colnames(SRO$Capture_age_mean_gears) = FLEETSEGMENTS_names
  
  SRO$Landing_age_mean_gears           <- matrix(data=0,nrow=(GLO$L_number+1),ncol=nb_gears)
  colnames(SRO$Landing_age_mean_gears) = FLEETSEGMENTS_names
  
  SRO$Discard_age_mean_gears           <- matrix(data=0,nrow=(GLO$L_number+1),ncol=nb_gears)
  colnames(SRO$Discard_age_mean_gears) = FLEETSEGMENTS_names
  SRO$Discard_age_mean_gears_landed           <- matrix(data=0,nrow=(GLO$L_number+1),ncol=nb_gears)
  colnames(SRO$Discard_age_mean_gears_landed) = FLEETSEGMENTS_names
  SRO$Discard_age_mean_gears_sea           <- matrix(data=0,nrow=(GLO$L_number+1),ncol=nb_gears)
  colnames(SRO$Discard_age_mean_gears_sea) = FLEETSEGMENTS_names
  
  SRO$Capture_length_mean_gears       <- matrix(data=0,nrow=(GLO$L_number+1),ncol=nb_gears)
  colnames(SRO$Capture_length_mean_gears) = FLEETSEGMENTS_names
  
  SRO$Landing_length_mean_gears           <- matrix(data=0,nrow=(GLO$L_number+1),ncol=nb_gears)
  colnames(SRO$Landing_length_mean_gears) = FLEETSEGMENTS_names
  
  SRO$Discard_length_mean_gears           <- matrix(data=0,nrow=(GLO$L_number+1),ncol=nb_gears)
  colnames(SRO$Discard_length_mean_gears) = FLEETSEGMENTS_names
  SRO$Discard_length_mean_gears_landed           <- matrix(data=0,nrow=(GLO$L_number+1),ncol=nb_gears)
  colnames(SRO$Discard_length_mean_gears_landed) = FLEETSEGMENTS_names
  SRO$Discard_length_mean_gears_sea           <- matrix(data=0,nrow=(GLO$L_number+1),ncol=nb_gears)
  colnames(SRO$Discard_length_mean_gears_sea) = FLEETSEGMENTS_names
 
  SRO$MFCatch                    <- matrix(data=0, nrow=(GLO$L_number + 1), ncol=GLO$MC_number)        #salvataggio catture
  SRO$FFCatch                    <- matrix(data=0, nrow=(GLO$L_number + 1), ncol=GLO$FC_number)
  
  SRO$MFLanding                    <- matrix(data=0, nrow=(GLO$L_number + 1), ncol=GLO$MC_number)           #salvataggio catture
  SRO$FFLanding                    <- matrix(data=0, nrow=(GLO$L_number + 1), ncol=GLO$FC_number)
  
  SRO$MFDiscard                    <- matrix(data=0, nrow=(GLO$L_number + 1), ncol=GLO$MC_number)        #salvataggio catture
  SRO$FFDiscard                    <- matrix(data=0, nrow=(GLO$L_number + 1), ncol=GLO$FC_number)
  SRO$MFDiscard_landed                    <- matrix(data=0, nrow=(GLO$L_number + 1), ncol=GLO$MC_number)        #salvataggio catture
  SRO$FFDiscard_landed                    <- matrix(data=0, nrow=(GLO$L_number + 1), ncol=GLO$FC_number)
  SRO$MFDiscard_sea                    <- matrix(data=0, nrow=(GLO$L_number + 1), ncol=GLO$MC_number)        #salvataggio catture
  SRO$FFDiscard_sea                    <- matrix(data=0, nrow=(GLO$L_number + 1), ncol=GLO$FC_number)
  

  SRO$MFCatch_gears       <- array(data=0,dim=c((GLO$L_number+1),GLO$MC_number,nb_gears))       #salvataggio catture

  SRO$FFCatch_gears       <-  array(data=0,dim=c((GLO$L_number+1),GLO$FC_number,nb_gears))                  #salvataggio catture

  SRO$MFLanding_gears       <-  array(data=0,dim=c((GLO$L_number+1),GLO$MC_number,nb_gears))                  #salvataggio catture

  SRO$FFLanding_gears       <-  array(data=0,dim=c((GLO$L_number+1),GLO$FC_number,nb_gears))                   #salvataggio catture

  SRO$MFDiscard_gears       <-  array(data=NA,dim=c((GLO$L_number+1),GLO$MC_number,nb_gears))                   #salvataggio catture

  SRO$FFDiscard_gears       <-  array(data=NA,dim=c((GLO$L_number+1),GLO$FC_number,nb_gears))               #salvataggio catture
  
  SRO$MFDiscard_gears_landed       <-  array(data=NA,dim=c((GLO$L_number+1),GLO$MC_number,nb_gears))                   #salvataggio catture
  SRO$FFDiscard_gears_landed       <-  array(data=NA,dim=c((GLO$L_number+1),GLO$FC_number,nb_gears))               #salvataggio catture
  
  SRO$MFDiscard_gears_sea       <-  array(data=NA,dim=c((GLO$L_number+1),GLO$MC_number,nb_gears))                   #salvataggio catture
  SRO$FFDiscard_gears_sea       <-  array(data=NA,dim=c((GLO$L_number+1),GLO$FC_number,nb_gears))               #salvataggio catture

  dis_vec_sea_survM =  data.frame(matrix(NA, length(BAS$MLength),ncol=nb_gears))
  dis_vec_sea_survF =  data.frame(matrix(NA, length(BAS$MFLength),ncol=nb_gears))
   
  SRO$FM_gears = matrix(data=0,nrow=0,ncol=GLO$MC_number)
  SRO$FF_gears = matrix(data=0,nrow =0,ncol=GLO$FC_number)
  
  SRO$annual_F_calc_by_gear  <- matrix(data=0,nrow=(GLO$L_number+1),ncol=nb_gears)
  SRO$annual_F_calc_by_gear_ls  <- matrix(data=0,nrow=(GLO$L_number+1),ncol=nb_gears)
  #
  # Initialize the matrices to store the populations data
  #

  SRO$MFPopulation                    <- matrix(data=0, nrow=(GLO$L_number + 1), ncol=GLO$MC_number)
  SRO$MUPopulation                    <- matrix(data=0, nrow=(GLO$L_number + 1), ncol=GLO$MC_number)

  SRO$FFPopulation                    <- matrix(data=0, nrow=(GLO$L_number + 1), ncol=GLO$FC_number)
  SRO$FUPopulation                    <- matrix(data=0, nrow=(GLO$L_number + 1), ncol=GLO$FC_number)

  # Working vectors
  loca_MF                             <- vector(mode="numeric", length=GLO$MC_number)
  loca_MZ                             <- vector(mode="numeric", length=GLO$MC_number)
  loca_FF                             <- vector(mode="numeric", length=GLO$FC_number)
  loca_FZ                             <- vector(mode="numeric", length=GLO$FC_number)

  #
  # Run the first simulation to Initialize the vectors to store the running population
  #
  BAS$MZ_estimated                    <- INP$MZ_estimated
  BAS$FZ_estimated                    <- INP$FZ_estimated
  GLO$Delta_t                         <- 1 / INP$Time_slice

  #  RANDOM RUNS Males 
if (as.numeric(INP$OPT_F_TYPE)==2) {
  load_firstStepM_entrataF() 
  } else { 
  load_firstStepM(BAS$MZ_estimated[1], INP$param1[1,],INP$param2[1,],INP$param3[1,],INP$param4[1,],INP$param5[1,])
  }
  
 
  
  if(BAS$MLength[1] < 0) {
    print("Initial Length < 0", quote=FALSE)
  }
  SRO$MFPopulation[1,]                <- BAS$MFPopulation
  SRO$MUPopulation[1,]                <- BAS$MUPopulation

  # RANDOM RUNS Females
 if (as.numeric(INP$OPT_F_TYPE)==2) {
  load_firstStepF_entrataF() 
  }else {
  load_firstStepF(BAS$FZ_estimated[1], INP$param1[1,],INP$param2[1,],INP$param3[1,],INP$param4[1,],INP$param5[1,])
  }
  
  if(BAS$FLength[1] < 0) {
    print("Initial Length < 0", quote=FALSE)
  }
    
  
INP$Disc_Surv_rate_vectM_sim <- data.frame(matrix(NA, nrow=length(BAS$MLength),ncol=length(FLEETSEGMENTS_names)))
INP$Disc_Surv_rate_vectF_sim <- data.frame(matrix(NA, nrow=length(BAS$FLength),ncol=length(FLEETSEGMENTS_names)))

INP$Esc_Surv_rate_vectM_sim <- data.frame(matrix(NA, nrow=length(BAS$MLength),ncol=length(FLEETSEGMENTS_names)))
INP$Esc_Surv_rate_vectF_sim <- data.frame(matrix(NA, nrow=length(BAS$FLength),ncol=length(FLEETSEGMENTS_names)))

for (i in c(1:length(FLEETSEGMENTS_names))){
INP$Disc_Surv_rate_vectM_sim[,i] = logistic(1,INP$Disc_Surv_rate_ogive_param1_sim[i],INP$Disc_Surv_rate_ogive_param2_sim[i],BAS$MLength)
INP$Disc_Surv_rate_vectF_sim[,i]= logistic(1,INP$Disc_Surv_rate_ogive_param1_sim[i],INP$Disc_Surv_rate_ogive_param2_sim[i],BAS$FLength)

if (INP$Esc_Surv_rate_sim[i] == "Y") {
if (INP$Esc_Surv_rate_type_sim[i] == 2)  {
INP$Esc_Surv_rate_vectM_sim[,i] = logistic(1,INP$Esc_Surv_rate_ogive_param1_sim[i],INP$Esc_Surv_rate_ogive_param2_sim[i],BAS$MLength)
INP$Esc_Surv_rate_vectF_sim[,i]= logistic(1,INP$Esc_Surv_rate_ogive_param1_sim[i],INP$Esc_Surv_rate_ogive_param2_sim[i],BAS$FLength)
} else if (INP$Esc_Surv_rate_type_sim[i] == 3)  {
INP$Esc_Vector_sim_temp[INP$Esc_Vector_sim_temp$Sex == "M",i+1] <- as.numeric(as.character(FleetList_simulation[[i]]@escape.survivability.DOS.ext_vect.M[1,])) 
INP$Esc_Vector_sim_temp[INP$Esc_Vector_sim_temp$Sex == "F",i+1] <- as.numeric(as.character(FleetList_simulation[[i]]@escape.survivability.DOS.ext_vect.F[1,]))
}
} 

}
INP$Disc_Surv_rate_vectM_sim = INP$Disc_Surv_rate_vectM_sim[-1,]
INP$Disc_Surv_rate_vectF_sim = INP$Disc_Surv_rate_vectF_sim[-1,]

INP$Esc_Surv_rate_vectM_sim = INP$Esc_Surv_rate_vectM_sim[-1,]
INP$Esc_Surv_rate_vectF_sim = INP$Esc_Surv_rate_vectF_sim[-1,]

  SRO$FFPopulation[1,]                <- BAS$FFPopulation
  SRO$FUPopulation[1,]                <- BAS$FUPopulation

  # No Fertility
  loca_Fertility                     <- 1

  # Base value of natural mortality to calculate FMax
  loca_min_BAS.MM                     <- mean(BAS$MM)
  loca_min_BAS.FM                     <- mean(BAS$FM)

  #
  # PRE-SIMULATION PHASE
  #
  #-----------------------------
if (as.numeric(INP$OPT_F_TYPE)==2) {
  load_PRELIFE_EXPLOITED_entrataF(loca_Fertility,loca_min_BAS.MM,loca_min_BAS.FM)
  } else {
  load_PRELIFE_EXPLOITED(loca_Fertility,loca_min_BAS.MM,loca_min_BAS.FM)
  }
  load_PRELIFE_UNEXPLOITED(loca_Fertility,loca_min_BAS.MM,loca_min_BAS.FM)
   # print("Prelife executed", quote=F)
  #-----------------------------
                

# inizializzazione delle 4 popolazioni e delle rispettive reclute e riproduttori
  BAS$MFSS_Number[1]        <- ifelse(SS==1,0,sum(BAS$MFPopulation * BAS$MMaturity))
  BAS$MUSS_Number[1]        <- ifelse(SS==1,0,sum(BAS$MUPopulation * BAS$MMaturity))
  BAS$MFR[1]                <- BAS$MFPopulation[1]
  BAS$MUR[1]                <- BAS$MUPopulation[1]
  SRO$MFPopulation[1,]      <- BAS$MFPopulation
  SRO$MUPopulation[1,]      <- BAS$MUPopulation
  BAS$FFSS_Number[1]        <- sum(BAS$FFPopulation * BAS$FMaturity)
  BAS$FUSS_Number[1]        <- sum(BAS$FUPopulation * BAS$FMaturity)
  BAS$FFR[1]                <- BAS$FFPopulation[1]
  BAS$FUR[1]                <- BAS$FUPopulation[1]
  SRO$FFPopulation[1,]      <- BAS$FFPopulation
  SRO$FUPopulation[1,]      <- BAS$FUPopulation

	
  #
  # SIMULATION PHASE
  #
  #-----------------------------  
  
  write.table(INP$Recruits, file="reclute mensili_prima di SIMULATION_UNEXPLOITED.csv", sep=";")
  
   load_SIMULATION_UNEXPLOITED(loca_Fertility, Start, End)     # GLO$L_number 
   
     write.table(INP$Recruits, file="reclute mensili_prima di SIMULATION_EXPLOITED.csv", sep=";")    
   if (as.numeric(INP$OPT_F_TYPE)==2) {
   load_SIMULATION_EXPLOITED_entrataF(loca_Fertility,loca_min_BAS.MM,loca_min_BAS.FM, Start,End)
   } else {
   load_SIMULATION_EXPLOITED(loca_Fertility,loca_min_BAS.MM,loca_min_BAS.FM, Start,End)
   }
  #----------------------------- 

              


}
