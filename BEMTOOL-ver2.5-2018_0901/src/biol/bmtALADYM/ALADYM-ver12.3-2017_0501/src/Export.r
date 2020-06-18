# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



# export all the relevant data

Export <- function(End) {

  loca_fileName <- "export.dat"
  write("																										         ", file=loca_fileName, append=FALSE)
  write("																										         ", file=loca_fileName, append=TRUE)
  write("Model: ALADYM - Age length based dynamic model			         ", file=loca_fileName, append=TRUE)
  write("																										         ", file=loca_fileName, append=TRUE)
  write(paste("Version:", GLO$ThisIsVersion)								          , file=loca_fileName, append=TRUE)
  write("																										         ", file=loca_fileName, append=TRUE)
  write("Authors: Lembo G., Spedicato M.T., Bitetto I. 2012              ", file=loca_fileName, append=TRUE)
  write("																										         ", file=loca_fileName, append=TRUE)
  write("COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 							         ", file=loca_fileName, append=TRUE)
  write("																										         ", file=loca_fileName, append=TRUE)
  write("In case of use of the model, the Authors should be cited.   ", file=loca_fileName, append=TRUE)
  write("If you have any comments or suggestions please contact      ", file=loca_fileName, append=TRUE)
  write("the following e-mail address: lembo@coispa.it               ", file=loca_fileName, append=TRUE)
  write("																										         ", file=loca_fileName, append=TRUE)
  write("Aladym is believed to be reliable.													 ", file=loca_fileName, append=TRUE)
  write("However, we disclaim any implied warranty or representation ", file=loca_fileName, append=TRUE)
  write("about its accuracy, completeness or appropriateness for any ", file=loca_fileName, append=TRUE)
  write("particular purpose.                                         ", file=loca_fileName, append=TRUE)
  write("																										         ", file=loca_fileName, append=TRUE)
  write("Aladym is distributed under the GPL license model.          ", file=loca_fileName, append=TRUE)
  write("																										         ", file=loca_fileName, append=TRUE)

  write("Model Data                                  ", file=loca_fileName, ncolumns=max(GLO$MC_number, GLO$FC_number), append=TRUE)

  write("                                            ", file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  write("Males of exploited population                   ", file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  for(loca_i in (1:(End + 1))) {
  write(SRO$MFPopulation[loca_i,]                   , file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  }

  write("                                            ", file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  write("Males of unexploited population                 ", file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  for(loca_i in (1:(End + 1))) {
  write(SRO$MUPopulation[loca_i,]                   , file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  }

  write("                                            ", file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)
  write("Females of exploited population                 ", file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)
  for(loca_i in (1:(End + 1))) {
    write(SRO$FFPopulation[loca_i,]                   , file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)
  }

  write("                                            ", file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)
  write("Females of unexploited population               ", file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)
  for(loca_i in (1:(End + 1))) {
    write(SRO$FUPopulation[loca_i,]                   , file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)
  }

  write("                                            ", file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  write("Males biomass of exploited population            ", file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  for(loca_i in (1:(End + 1))) {
  write(SRO$MFPopulation[loca_i,] * BAS$MWeight     , file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  }

  write("                                            ", file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  write("Males biomass of unexploited population          ", file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  for(loca_i in (1:(End + 1))) {
  write(SRO$MUPopulation[loca_i,] * BAS$MWeight     , file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  }

  write("                                            ", file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)
  write("Females biomass of exploited population          ", file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)
  for(loca_i in (1:(End + 1))) {
    write(SRO$FFPopulation[loca_i,] * BAS$FWeight     , file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)
  }

  write("                                            ", file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)
  write("Females biomass of unexploited population        ", file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)
  for(loca_i in (1:(End + 1))) {
  write(SRO$FUPopulation[loca_i,] * BAS$FWeight     , file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)
  }

  # these refer to each cohort

  write("                                            ", file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  write("Length of males                                  ", file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  write(BAS$MLength                                   , file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)
  write("Length of females                              ", file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)
  write(BAS$FLength                                   , file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)

  write("                                            ", file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  write("Age of males                                     ", file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  write(BAS$MAge                                      , file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)
  write("Age of females                                  ", file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)
  write(BAS$FAge                                      , file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)

  write("                                            ", file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  write("M of males                                       ", file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  write(BAS$MM                                        , file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)
  write("M of females                                     ", file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)
  write(BAS$FM                                        , file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)

  write("                                            ", file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  write("Weight of male                              ", file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  write(BAS$MWeight                                   , file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)
  write("Weight of females                                ", file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)
  write(BAS$FWeight                                   , file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)

  write("                                            ", file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  write("Maturity of males                                ", file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  write(BAS$MMaturity                                 , file=loca_fileName, ncolumns=GLO$MC_number, append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)
  write("Maturity of females                              ", file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)
  write(BAS$FMaturity                                 , file=loca_fileName, ncolumns=GLO$FC_number, append=TRUE)


  # these refer to each time-slice

  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Z estimated (monthly)                               ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$Z_calculated                              , file=loca_fileName, ncolumns=(1), append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Z estimated (monthly) of males                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$MZ_calculated                             , file=loca_fileName, ncolumns=(1), append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Z estimated (monthly) of females                          ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$FZ_calculated                             , file=loca_fileName, ncolumns=(1), append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Annual Z estimated of males                    ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$MZa_calculated                            , file=loca_fileName, ncolumns=(1), append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Annual Z estimated of females                 ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$FZa_calculated                            , file=loca_fileName, ncolumns=(1), append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("F estimated (monthly)                                ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$F_calculated                              , file=loca_fileName, ncolumns=(1), append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Annual F estimated of males                     ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$annual_F_calc_M                            , file=loca_fileName, ncolumns=(1), append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Annual F estimated of females                   ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$annual_F_calc_F                            , file=loca_fileName, ncolumns=(1), append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Annual F estimated                  ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$annual_F_calc                            , file=loca_fileName, ncolumns=(1), append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("ESS_Number of males                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(BAS$MFSS_Number                               , file=loca_fileName, ncolumns=(1), append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("USS_Number of males                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(BAS$MUSS_Number                               , file=loca_fileName, ncolumns=(1), append=TRUE)


  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("ESS_Number of females                           ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(BAS$FFSS_Number                               , file=loca_fileName, ncolumns=(1), append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("USS_Number of females                          ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(BAS$FUSS_Number                               , file=loca_fileName, ncolumns=(1), append=TRUE)


  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Biomass of exploited population                               ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$FBiomass                                  , file=loca_fileName, ncolumns=(1), append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Biomass of unexploited  population                              ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$UBiomass                                  , file=loca_fileName, ncolumns=(1), append=TRUE)


  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("SSB of exploited  population                             ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$FSSBiomass                                , file=loca_fileName, ncolumns=(1), append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("SSB of unexploited  population                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$USSBiomass                                , file=loca_fileName, ncolumns=(1), append=TRUE)


  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("ESSB/USSB                         ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$FSSBratioUSSB                             , file=loca_fileName, ncolumns=(1), append=TRUE)


  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Mean length of exploited  population                          ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$FLength_mean                              , file=loca_fileName, ncolumns=(1), append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Mean length of unexploited  population                          ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$ULength_mean                              , file=loca_fileName, ncolumns=(1), append=TRUE)


  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Mean age of exploited  population                              ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$FAge_mean                                 , file=loca_fileName, ncolumns=(1), append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Mean age of unexploited  population                             ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$UAge_mean                                 , file=loca_fileName, ncolumns=(1), append=TRUE)


  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Mean length of exploited SS                        ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$FSSLength_mean                            , file=loca_fileName, ncolumns=(1), append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Mean length of unexploited SS                         ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$USSLength_mean                            , file=loca_fileName, ncolumns=(1), append=TRUE)


  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Mean age of exploited SS                             ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$FSSAge_mean                               , file=loca_fileName, ncolumns=(1), append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Mean age of unexploited SS                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$USSAge_mean                               , file=loca_fileName, ncolumns=(1), append=TRUE)


  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Biological Production                   ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$Biological_production                     , file=loca_fileName, ncolumns=(1), append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Natural death biomass                    ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$Death_biomass                             , file=loca_fileName, ncolumns=(1), append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Total Yield                                   ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$Capture_biomass                           , file=loca_fileName, ncolumns=(1), append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Mean length in catch                       ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$Capture_length_mean                       , file=loca_fileName, ncolumns=(1), append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Mean age in catch                          ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$Capture_age_mean                          , file=loca_fileName, ncolumns=(1), append=TRUE)

  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Total Landing                                   ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$Landing_biomass_gears                           , file=loca_fileName, ncolumns=(1), append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)

  write("Mean length in landing                       ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$Landing_length_mean                       , file=loca_fileName, ncolumns=(1), append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Mean age in landing                          ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$Landing_age_mean                          , file=loca_fileName, ncolumns=(1), append=TRUE)

  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Total Discard                                  ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$Discard_biomass                           , file=loca_fileName, ncolumns=(1), append=TRUE)

  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Mean length in discard                       ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$Discard_length_mean                       , file=loca_fileName, ncolumns=(1), append=TRUE)
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Mean age in discard                          ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(SRO$Discard_age_mean                          , file=loca_fileName, ncolumns=(1), append=TRUE)
  
# esportazione variabili per attrezzo   
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Yield by gear (tons)", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(paste(as.character(FLEETSEGMENTS_names))                     , file=loca_fileName, ncolumns=nb_gears, append=TRUE)
  for (loca_i in (1:(End + 1))) {
  write(SRO$Capture_biomass_gears[loca_i,],  file=loca_fileName, ncolumns=(nb_gears), append=TRUE) }
  
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Mean age in catch by gear", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(paste(as.character(FLEETSEGMENTS_names))                     , file=loca_fileName, ncolumns=(nb_gears), append=TRUE)
  for (loca_i in (1:(End + 1))) {
  write(SRO$Capture_age_mean_gears[loca_i,],                                     , file=loca_fileName, ncolumns=(nb_gears), append=TRUE)   }
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Mean length in catch per gear", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(paste(as.character(FLEETSEGMENTS_names) )                  , file=loca_fileName, ncolumns=(nb_gears), append=TRUE)
  for (loca_i in (1:(End + 1))) {
  write(SRO$Capture_length_mean_gears[loca_i,]                                  , file=loca_fileName, ncolumns=(nb_gears), append=TRUE)     }

  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Landing by gear (tons)", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(paste(as.character(FLEETSEGMENTS_names))                     , file=loca_fileName, ncolumns=nb_gears, append=TRUE)
  for (loca_i in (1:(End + 1))) {
  write(SRO$Landing_biomass_gears[loca_i,],  file=loca_fileName, ncolumns=(nb_gears), append=TRUE) }
  
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Mean age in landing by gear", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(paste(as.character(FLEETSEGMENTS_names))                     , file=loca_fileName, ncolumns=(nb_gears), append=TRUE)
  for (loca_i in (1:(End + 1))) {
  write(SRO$Landing_age_mean_gears[loca_i,],                                     , file=loca_fileName, ncolumns=(nb_gears), append=TRUE)   }
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Mean length in landing per gear", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(paste(as.character(FLEETSEGMENTS_names) )                  , file=loca_fileName, ncolumns=(nb_gears), append=TRUE)
  for (loca_i in (1:(End + 1))) {
  write(SRO$Landing_length_mean_gears[loca_i,]                                  , file=loca_fileName, ncolumns=(nb_gears), append=TRUE)     }

  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Discard by gear (tons)", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(paste(as.character(FLEETSEGMENTS_names))                     , file=loca_fileName, ncolumns=nb_gears, append=TRUE)
  for (loca_i in (1:(End + 1))) {
  write(SRO$Discard_biomass_gears[loca_i,],  file=loca_fileName, ncolumns=(nb_gears), append=TRUE) }
  
  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Mean age in discard by gear", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(paste(as.character(FLEETSEGMENTS_names))                     , file=loca_fileName, ncolumns=(nb_gears), append=TRUE)
	  for (loca_i in (1:(End + 1))) {
	  write(SRO$Discard_age_mean_gears[loca_i,],                                     , file=loca_fileName, ncolumns=(nb_gears), append=TRUE)   }
	  write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
  write("Mean length in discard per gear", file=loca_fileName, ncolumns=(1), append=TRUE)
  write(paste(as.character(FLEETSEGMENTS_names) )                  , file=loca_fileName, ncolumns=(nb_gears), append=TRUE)
  
	  for (loca_i in (1:(End + 1))) {
	  write(SRO$Discard_length_mean_gears[loca_i,]                                  , file=loca_fileName, ncolumns=(nb_gears), append=TRUE)     }

    write("                                            ", file=loca_fileName, ncolumns=(1), append=TRUE)
    write("Annual F calc by gear", file=loca_fileName, ncolumns=(1), append=TRUE)

    for (loca_i in (1:(End + 1))) {
	  write(SRO$annual_F_calc_by_gear[loca_i,]                                  , file=loca_fileName, ncolumns=(nb_gears), append=TRUE)     }

}
