# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



# ####################################################################################################
if (to_write == "Random runs") {
write("-----------------------", file=loca_fileName, append=TRUE)
write("Random runs Parameter  ", file=loca_fileName, append=TRUE)
write("-----------------------", file=loca_fileName, append=TRUE)
write("R_fun                  ", file=loca_fileName, append=TRUE)           
write(RND$R_fun                , file=loca_fileName, append=TRUE)
write("R_min                  ", file=loca_fileName, append=TRUE)
write(RND$R_min                , file=loca_fileName, append=TRUE)
write("R_max                  ", file=loca_fileName, append=TRUE)
write(RND$R_max                , file=loca_fileName, append=TRUE)
write("R_a                    ", file=loca_fileName, append=TRUE)
write(RND$R_a                  , file=loca_fileName, append=TRUE)
write("R_b                    ", file=loca_fileName, append=TRUE)
write(RND$R_b                  , file=loca_fileName, append=TRUE)
write("                       ", file=loca_fileName, append=TRUE)
write("                       ", file=loca_fileName, append=TRUE)
write("MGrowth_K_fun          ", file=loca_fileName, append=TRUE)
write(RND$MGrowth_K_fun        , file=loca_fileName, append=TRUE)
write("MGrowth_K_min          ", file=loca_fileName, append=TRUE)
write(RND$MGrowth_K_min        , file=loca_fileName, append=TRUE)
write("MGrowth_K_max          ", file=loca_fileName, append=TRUE)
write(RND$MGrowth_K_max        , file=loca_fileName, append=TRUE)
write("MGrowth_K_a            ", file=loca_fileName, append=TRUE)
write(RND$MGrowth_K_a          , file=loca_fileName, append=TRUE)
write("MGrowth_K_b            ", file=loca_fileName, append=TRUE)
write(RND$MGrowth_K_b          , file=loca_fileName, append=TRUE)
write("                       ", file=loca_fileName, append=TRUE)
write("                       ", file=loca_fileName, append=TRUE)
write("MGrowth_Linf_fun       ", file=loca_fileName, append=TRUE)
write(RND$MGrowth_Linf_fun     , file=loca_fileName, append=TRUE)
write("MGrowth_Linf_min       ", file=loca_fileName, append=TRUE)
write(RND$MGrowth_Linf_min     , file=loca_fileName, append=TRUE)
write("MGrowth_Linf_max       ", file=loca_fileName, append=TRUE)
write(RND$MGrowth_Linf_max     , file=loca_fileName, append=TRUE)
write("MGrowth_Linf_a         ", file=loca_fileName, append=TRUE)
write(RND$MGrowth_Linf_a       , file=loca_fileName, append=TRUE)
write("MGrowth_Linf_b         ", file=loca_fileName, append=TRUE)
write(RND$MGrowth_Linf_b       , file=loca_fileName, append=TRUE)
write("                       ", file=loca_fileName, append=TRUE)
write("                       ", file=loca_fileName, append=TRUE)
write("MML50p_fun             ", file=loca_fileName, append=TRUE)
write(RND$MML50p_fun           , file=loca_fileName, append=TRUE)
write("MML50p_min             ", file=loca_fileName, append=TRUE)
write(RND$MML50p_min           , file=loca_fileName, append=TRUE)
write("MML50p_max             ", file=loca_fileName, append=TRUE)
write(RND$MML50p_max           , file=loca_fileName, append=TRUE)
write("MML50p_a               ", file=loca_fileName, append=TRUE)
write(RND$MML50p_a             , file=loca_fileName, append=TRUE)
write("MML50p_b               ", file=loca_fileName, append=TRUE)
write(RND$MML50p_b             , file=loca_fileName, append=TRUE)
write("                       ", file=loca_fileName, append=TRUE)
write("                       ", file=loca_fileName, append=TRUE)
write("MML75pL25p_fun         ", file=loca_fileName, append=TRUE)
write(RND$MML75pL25p_fun       , file=loca_fileName, append=TRUE)      
write("MML75pL25p_min         ", file=loca_fileName, append=TRUE)
write(RND$MML75pL25p_min       , file=loca_fileName, append=TRUE)
write("MML75pL25p_max         ", file=loca_fileName, append=TRUE)
write(RND$MML75pL25p_max       , file=loca_fileName, append=TRUE)
write("MML75pL25p_a           ", file=loca_fileName, append=TRUE)
write(RND$MML75pL25p_a         , file=loca_fileName, append=TRUE)
write("MML75pL25p_b           ", file=loca_fileName, append=TRUE)
write(RND$MML75pL25p_b         , file=loca_fileName, append=TRUE)
write("                       ", file=loca_fileName, append=TRUE)
write("                       ", file=loca_fileName, append=TRUE)      
write("FGrowth_K_fun          ", file=loca_fileName, append=TRUE)
write(RND$FGrowth_K_fun        , file=loca_fileName, append=TRUE) 
write("FGrowth_K_min          ", file=loca_fileName, append=TRUE)
write(RND$FGrowth_K_min        , file=loca_fileName, append=TRUE)
write("FGrowth_K_max          ", file=loca_fileName, append=TRUE)
write(RND$FGrowth_K_max        , file=loca_fileName, append=TRUE)
write("FGrowth_K_a            ", file=loca_fileName, append=TRUE)
write(RND$FGrowth_K_a          , file=loca_fileName, append=TRUE)
write("FGrowth_K_b            ", file=loca_fileName, append=TRUE)
write(RND$FGrowth_K_b          , file=loca_fileName, append=TRUE)
write("                       ", file=loca_fileName, append=TRUE)
write("                       ", file=loca_fileName, append=TRUE)
write("FGrowth_Linf_fun       ", file=loca_fileName, append=TRUE)
write(RND$FGrowth_Linf_fun     , file=loca_fileName, append=TRUE)
write("FGrowth_Linf_min       ", file=loca_fileName, append=TRUE)
write(RND$FGrowth_Linf_min     , file=loca_fileName, append=TRUE)
write("FGrowth_Linf_max       ", file=loca_fileName, append=TRUE)
write(RND$FGrowth_Linf_max     , file=loca_fileName, append=TRUE)
write("FGrowth_Linf_a         ", file=loca_fileName, append=TRUE)
write(RND$FGrowth_Linf_a       , file=loca_fileName, append=TRUE)      
write("FGrowth_Linf_b         ", file=loca_fileName, append=TRUE)
write(RND$FGrowth_Linf_b       , file=loca_fileName, append=TRUE)      
write("                       ", file=loca_fileName, append=TRUE)
write("                       ", file=loca_fileName, append=TRUE)
write("FML50p_fun             ", file=loca_fileName, append=TRUE)
write(RND$FML50p_fun           , file=loca_fileName, append=TRUE)     
write("FML50p_min             ", file=loca_fileName, append=TRUE)
write(RND$FML50p_min           , file=loca_fileName, append=TRUE)      
write("FML50p_max             ", file=loca_fileName, append=TRUE)
write(RND$FML50p_max           , file=loca_fileName, append=TRUE)      
write("FML50p_a               ", file=loca_fileName, append=TRUE)
write(RND$FML50p_a             , file=loca_fileName, append=TRUE)     
write("FML50p_b               ", file=loca_fileName, append=TRUE)
write(RND$FML50p_b             , file=loca_fileName, append=TRUE)      
write("                       ", file=loca_fileName, append=TRUE)
write("                       ", file=loca_fileName, append=TRUE)      
write("FML75pL25p_fun         ", file=loca_fileName, append=TRUE)
write(RND$FML75pL25p_fun       , file=loca_fileName, append=TRUE)      
write("FML75pL25p_min         ", file=loca_fileName, append=TRUE)
write(RND$FML75pL25p_min       , file=loca_fileName, append=TRUE)     
write("FML75pL25p_max         ", file=loca_fileName, append=TRUE)
write(RND$FML75pL25p_max       , file=loca_fileName, append=TRUE)      
write("FML75pL25p_a           ", file=loca_fileName, append=TRUE)
write(RND$FML75pL25p_a         , file=loca_fileName, append=TRUE)      
write("FML75pL25p_b           ", file=loca_fileName, append=TRUE)
write(RND$FML75pL25p_b         , file=loca_fileName, append=TRUE)      
write("                       ", file=loca_fileName, append=TRUE)
write("                       ", file=loca_fileName, append=TRUE)
# ####################################################################################################
} else if (to_write == "Stochastic") {
write("-----------------------", file=loca_fileName, append=TRUE)
write("Stochastic Value       ", file=loca_fileName, append=TRUE)
write("-----------------------", file=loca_fileName, append=TRUE)
write("MGrowth_Linf_min       ", file=loca_fileName, append=TRUE)
write(INP$MGrowth_Linf_min     , file=loca_fileName, append=TRUE)
write("MGrowth_Linf_max       ", file=loca_fileName, append=TRUE)
write(INP$MGrowth_Linf_max     , file=loca_fileName, append=TRUE)
write("MGrowth_K_min          ", file=loca_fileName, append=TRUE)
write(INP$MGrowth_K_min        , file=loca_fileName, append=TRUE)
write("MGrowth_K_max          ", file=loca_fileName, append=TRUE)
write(INP$MGrowth_K_max        , file=loca_fileName, append=TRUE)
write("MGrowth_t0_min         ", file=loca_fileName, append=TRUE)
write(INP$MGrowth_t0_min       , file=loca_fileName, append=TRUE)
write("MGrowth_t0_max         ", file=loca_fileName, append=TRUE)
write(INP$MGrowth_t0_max       , file=loca_fileName, append=TRUE)
write("MGrowth_tend           ", file=loca_fileName, append=TRUE)
write(INP$MGrowth_tend         , file=loca_fileName, append=TRUE)
write("                       ", file=loca_fileName, append=TRUE)
write("                       ", file=loca_fileName, append=TRUE)
write("FGrowth_Linf_min       ", file=loca_fileName, append=TRUE)
write(INP$FGrowth_Linf_min     , file=loca_fileName, append=TRUE)
write("FGrowth_Linf_max       ", file=loca_fileName, append=TRUE)
write(INP$FGrowth_Linf_max     , file=loca_fileName, append=TRUE)
write("FGrowth_K_min          ", file=loca_fileName, append=TRUE)
write(INP$FGrowth_K_min        , file=loca_fileName, append=TRUE)
write("FGrowth_K_max          ", file=loca_fileName, append=TRUE)
write(INP$FGrowth_K_max        , file=loca_fileName, append=TRUE)
write("FGrowth_t0_min         ", file=loca_fileName, append=TRUE)
write(INP$FGrowth_t0_min       , file=loca_fileName, append=TRUE)
write("FGrowth_t0_max         ", file=loca_fileName, append=TRUE)
write(INP$FGrowth_t0_max       , file=loca_fileName, append=TRUE)
write("FGrowth_tend           ", file=loca_fileName, append=TRUE)
write(INP$FGrowth_tend         , file=loca_fileName, append=TRUE)
# ####################################################################################################
} else if (to_write == "Maturity") {
write("                       ", file=loca_fileName, append=TRUE)
write("                       ", file=loca_fileName, append=TRUE)
write("MML50p_min             ", file=loca_fileName, append=TRUE)
write(INP$MML50p_min           , file=loca_fileName, append=TRUE)
write("MML50p_max             ", file=loca_fileName, append=TRUE)
write(INP$MML50p_max           , file=loca_fileName, append=TRUE)
write("MML75pL25p_min         ", file=loca_fileName, append=TRUE)
write(INP$MML75pL25p_min       , file=loca_fileName, append=TRUE)
write("MML75pL25p_max         ", file=loca_fileName, append=TRUE)
write(INP$MML75pL25p_max       , file=loca_fileName, append=TRUE)
write("FML50p_min             ", file=loca_fileName, append=TRUE)
write(INP$FML50p_min           , file=loca_fileName, append=TRUE)
write("FML50p_max             ", file=loca_fileName, append=TRUE)
write(INP$FML50p_max           , file=loca_fileName, append=TRUE)
write("FML75pL25p_min         ", file=loca_fileName, append=TRUE)
write(INP$FML75pL25p_min       , file=loca_fileName, append=TRUE)
write("FML75pL25p_max         ", file=loca_fileName, append=TRUE)
write(INP$FML75pL25p_max       , file=loca_fileName, append=TRUE)
# ####################################################################################################
} else if (to_write == "Recruitment") {
write("                       ", file=loca_fileName, append=TRUE)
write("                       ", file=loca_fileName, append=TRUE)
write("FRLa                   ", file=loca_fileName, append=TRUE)
write(INP$FRLa                 , file=loca_fileName, append=TRUE)
write("FRLb                   ", file=loca_fileName, append=TRUE)
write(INP$FRLb                 , file=loca_fileName, append=TRUE)
write("FRLc                   ", file=loca_fileName, append=TRUE)
write(INP$FRLc                 , file=loca_fileName, append=TRUE)
write("FRLt                   ", file=loca_fileName, append=TRUE)
write(INP$FRLt                 , file=loca_fileName, append=TRUE)
# ####################################################################################################
} else if (to_write == "Fertility rate") { 
write("Fertility_Rate         ", file=loca_fileName, append=TRUE)                             # tassi di fertilità mensili
    for(loca_i in (1:12)) {
      write(c(loca_i, INP$Fertility_Rate[loca_i])     , file=loca_fileName, append=TRUE)
    }
write("Fertility_Delay        ", file=loca_fileName, append=TRUE)
write(INP$Fertility_Delay + 1  , file=loca_fileName, append=TRUE)
# ####################################################################################################
} else if (to_write == "Simulation") {
write("                       ", file=loca_fileName, append=TRUE)
write("                       ", file=loca_fileName, append=TRUE)
write("-----------------------", file=loca_fileName, append=TRUE)
write("Simulation Parameters  ", file=loca_fileName, append=TRUE)
write("-----------------------", file=loca_fileName, append=TRUE)
write("Time_slice             ", file=loca_fileName, append=TRUE)
write(INP$Time_slice           , file=loca_fileName, append=TRUE)
write("Year_simulation        ", file=loca_fileName, append=TRUE)
write(INP$Year_simulation      , file=loca_fileName, append=TRUE)
write("Life_before            ", file=loca_fileName, append=TRUE)
write(INP$Life_before          , file=loca_fileName, append=TRUE)
# ####################################################################################################
} else if (to_write == "Variable length") {
write("-----------------------", file=loca_fileName, append=TRUE)
write("Variable Length Param. ", file=loca_fileName, append=TRUE)
write("-----------------------", file=loca_fileName, append=TRUE)
write("MOPT_M_TYPE            ", file=loca_fileName, append=TRUE)                     #scrittura nei file di testo
write(INP$MOPT_M_TYPE          , file=loca_fileName, append=TRUE)
write("MM_fixed               ", file=loca_fileName, append=TRUE)
write(INP$MM_fixed             , file=loca_fileName, append=TRUE)

write("FOPT_M_TYPE            ", file=loca_fileName, append=TRUE)
write(INP$FOPT_M_TYPE          , file=loca_fileName, append=TRUE)
write("FM_fixed               ", file=loca_fileName, append=TRUE)
write(INP$FM_fixed             , file=loca_fileName, append=TRUE)
# ####################################################################################################
} else if (to_write == "Global") {
write("Fmax type              ", file=loca_fileName, append=TRUE)
write(INP$OPT_F_TYPE              , file=loca_fileName, append=TRUE)
write("                       ", file=loca_fileName, append=TRUE)
write("                       ", file=loca_fileName, append=TRUE)
write("-----------------------", file=loca_fileName, append=TRUE)
write("Global Parameters      ", file=loca_fileName, append=TRUE)
write("-----------------------", file=loca_fileName, append=TRUE)
write("MWLa                   ", file=loca_fileName, append=TRUE)
write(INP$MWLa                 , file=loca_fileName, append=TRUE)
write("MWLb                   ", file=loca_fileName, append=TRUE)
write(INP$MWLb                 , file=loca_fileName, append=TRUE)
write("FWLa                   ", file=loca_fileName, append=TRUE)
write(INP$FWLa                 , file=loca_fileName, append=TRUE)
write("FWLb                   ", file=loca_fileName, append=TRUE)
write(INP$FWLb                 , file=loca_fileName, append=TRUE)
# ####################################################################################################
} else if (to_write == "scrittura parametri") {
write("Nsimulation            ", file=loca_fileName, append=TRUE)
write(loca_Nsimulation         , file=loca_fileName, append=TRUE)
write("MQZ                    ", file=loca_fileName, append=TRUE)
    for(loca_i in 1:loca_Nsimulation) {
      write(c(loca_i,INP$MZ_estimated[loca_i])       , file=loca_fileName, append=TRUE)
    }
write("FQZ                    ", file=loca_fileName, append=TRUE)
    for(loca_i in 1:loca_Nsimulation) {
      write(c(loca_i,INP$FZ_estimated[loca_i])       , file=loca_fileName, append=TRUE)
    }
write("RGmin           ", file=loca_fileName, append=TRUE)
    for(loca_i in 1:loca_Nsimulation) {
      write(c(loca_i,INP$RGmin[loca_i])              , file=loca_fileName, append=TRUE)
    }
write("RGmax           ", file=loca_fileName, append=TRUE)
    for(loca_i in 1:loca_Nsimulation) {
      write(c(loca_i,INP$RGmax[loca_i])              , file=loca_fileName, append=TRUE)
    }
write("Recruits        ", file=loca_fileName, append=TRUE)
    for(loca_i in 1:loca_Nsimulation) {
      write(c(loca_i,INP$Recruits[loca_i])           , file=loca_fileName, append=TRUE)
    }
write("Sex_ratio       ", file=loca_fileName, append=TRUE)
    for(loca_i in 1:loca_Nsimulation) {
      write(c(loca_i,INP$Sex_ratio[loca_i])          , file=loca_fileName, append=TRUE)
    }
for (gear in 1:nb_gears){
write("                       ", file=loca_fileName, append=TRUE)
# write(paste("Fishing_effort",as.character(FLEETSEGMENTS_names[gear])), file=loca_fileName, append=TRUE)
write(paste("Fishing_effort",FleetList_simulation[[gear]]@fleetname), file=loca_fileName, append=TRUE)
    for (Loca_i in (1:loca_Nsimulation)){
    write(c(Loca_i,INP$Fishing_efforts[Loca_i,gear]) , file=loca_fileName, append=TRUE)     
    }
}
write("MZ_objective             ", file=loca_fileName, append=TRUE)
    for(loca_i in 1:loca_Nsimulation) {
      write(c(loca_i,INP$MZ_objective[loca_i]) , file=loca_fileName, append=TRUE)
    }
write("FZ_objective             ", file=loca_fileName, append=TRUE)
    for(loca_i in 1:loca_Nsimulation) {
      write(c(loca_i,INP$FZ_objective[loca_i]) , file=loca_fileName, append=TRUE)
    }
write("FMAX_vector             ", file=loca_fileName, append=TRUE)
    for(loca_i in 1:loca_Nsimulation) {
      write(c(loca_i,INP$FMAX_vector[loca_i]) , file=loca_fileName, append=TRUE)
    }
write("                       ", file=loca_fileName, append=TRUE)
write("                       ", file=loca_fileName, append=TRUE)
# ####################################################################################################
} else if (to_write == "Miscellaneous") {
write("-----------------------", file=loca_fileName, append=TRUE)
write("Miscellaneous Param.   ", file=loca_fileName, append=TRUE)
write("-----------------------", file=loca_fileName, append=TRUE)
write("Nrun                   ", file=loca_fileName, append=TRUE)
write(GLO$Nrun                 , file=loca_fileName, append=TRUE)
# ####################################################################################################
} else if (to_write == "Gear") {
write("Gears parameters       ", file=loca_fileName, append=TRUE)
write("Number of gears        ", file=loca_fileName, append=TRUE)
write(nb_gears              ,file=loca_fileName, append=TRUE)
# ####################################################################################################
} else if (to_write == "Forecast") {
write("SS option              ", file=loca_fileName, append=TRUE)
write(SS                 ,file=loca_fileName, append=TRUE)
write("forecast from month    ", file=loca_fileName, append=TRUE)
write(forecast                 ,file=loca_fileName, append=TRUE)
write("Target F               ", file=loca_fileName, append=TRUE)
write(Ref_point         , file=loca_fileName, append=TRUE)
write("Target month           ", file=loca_fileName, append=TRUE)
write(Ref_month        , file=loca_fileName, append=TRUE)
write("                       ", file=loca_fileName, append=TRUE)
write("range for output F males            ", file=loca_fileName, append=TRUE)
write(paste(min_ageM,"-",max_ageM), file=loca_fileName, append=TRUE)
write("range for output F females            ", file=loca_fileName, append=TRUE)
write(paste(min_ageF,"-",max_ageF), file=loca_fileName, append=TRUE)
# Scrittura dei vettori nel file datain.dat
for (gear in 1:nb_gears){
    write("                       ", file=loca_fileName, append=TRUE)
    write(paste("param1",FleetList_simulation[[gear]]@fleetname), file=loca_fileName, append=TRUE)
    for (Loca_i in 1:(loca_Nsimulation)){
    write(c(Loca_i,param1         [Loca_i,gear]) , file=loca_fileName, append=TRUE)     
    }
} 
for (gear in 1:nb_gears){
    write("                       ", file=loca_fileName, append=TRUE)
    write(paste("param2",FleetList_simulation[[gear]]@fleetname), file=loca_fileName, append=TRUE)
    for (Loca_i in (1:loca_Nsimulation)){
    write(c(Loca_i,param2         [Loca_i,gear]) , file=loca_fileName, append=TRUE)     
    }
} 
for (gear in 1:nb_gears){
    write("                       ", file=loca_fileName, append=TRUE)
    write(paste("param3",FleetList_simulation[[gear]]@fleetname), file=loca_fileName, append=TRUE)
    for (Loca_i in (1:loca_Nsimulation)){
    write(c(Loca_i,param3        [Loca_i,gear]) , file=loca_fileName, append=TRUE)     
    }
}
for (gear in 1:nb_gears){
    write("                       ", file=loca_fileName, append=TRUE)
    write(paste("param4",FleetList_simulation[[gear]]@fleetname), file=loca_fileName, append=TRUE)
    for (Loca_i in (1:loca_Nsimulation)){
    write(c(Loca_i,param4        [Loca_i,gear]) , file=loca_fileName, append=TRUE)     
    }
}
for (gear in 1:nb_gears){
    write("                       ", file=loca_fileName, append=TRUE)
    write(paste("param5",FleetList_simulation[[gear]]@fleetname), file=loca_fileName, append=TRUE)
    for (Loca_i in (1:loca_Nsimulation)){
    write(c(Loca_i,param5         [Loca_i,gear]) , file=loca_fileName, append=TRUE)     
    }
}
for (gear in 1:nb_gears){
    write("                       ", file=loca_fileName, append=TRUE)
    write(paste("param6",FleetList_simulation[[gear]]@fleetname), file=loca_fileName, append=TRUE)
    for (Loca_i in (1:loca_Nsimulation)){
    write(c(Loca_i,param6         [Loca_i,gear]) , file=loca_fileName, append=TRUE)     
    }
}
for (gear in 1:nb_gears){
    write("                       ", file=loca_fileName, append=TRUE)
    write(paste("param7",FleetList_simulation[[gear]]@fleetname), file=loca_fileName, append=TRUE)
    for (Loca_i in (1:loca_Nsimulation)){
    write(c(Loca_i,param7         [Loca_i,gear]) , file=loca_fileName, append=TRUE)     
    }
}
for (gear in 1:nb_gears){
    write("                       ", file=loca_fileName, append=TRUE)
    write(paste("Discard option",FleetList_simulation[[gear]]@fleetname), file=loca_fileName, append=TRUE)
    for (Loca_i in (1:loca_Nsimulation)){
    write(Discard         [Loca_i,gear] , file=loca_fileName, append=TRUE)     
    }
} 
for (gear in 1:nb_gears){
    write("                       ", file=loca_fileName, append=TRUE)
    write(paste("Fishing effort",FleetList_simulation[[gear]]@fleetname), file=loca_fileName, append=TRUE)
    for (Loca_i in (1:loca_Nsimulation)){
    write(c(Loca_i,INP$Fishing_efforts         [Loca_i,gear]) , file=loca_fileName, append=TRUE)     
    }
} 
for (gear in 1:nb_gears){
    write("                       ", file=loca_fileName, append=TRUE)
    write(paste("p_Production",FleetList_simulation[[gear]]@fleetname), file=loca_fileName, append=TRUE)
    for (Loca_i in 1:(loca_Nsimulation)){
    write(c(Loca_i,INP$p_Production         [Loca_i,gear]) , file=loca_fileName, append=TRUE)     
    }
    } 
}
# print(paste("...fine scrittura [", to_write, "]", sep=""))
