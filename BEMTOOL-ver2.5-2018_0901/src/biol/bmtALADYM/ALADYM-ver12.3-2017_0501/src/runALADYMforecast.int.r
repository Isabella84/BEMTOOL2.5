# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


showCompTime <<- T
cat("\n\n")
print("***************************************************************************", quote=FALSE)
print(paste("Launching BEMTOOL biological forecast [ALADYM] for species", BMT_SPECIES[ALADYM_spe], "year", years.forecast[current_year], "같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같"), quote=FALSE)
print("***************************************************************************", quote=FALSE)
cat("\n\n")
if (current_year == 1) {
  wnd_fore <- showMessage(paste("FORECAST for",BMT_SPECIES[ALADYM_spe],"in progress...") ) # else {
}
source( paste(ALADYM_home, "/src/paths.r", sep="") )
if (showCompTime)  {
runALADYMforecastINT_ptm <- proc.time()     
}








BAS$MMaturity_or= BAS$MMaturity
BAS$FMaturity_or=BAS$FMaturity
BAS$FM_or= BAS$FM
BAS$MM_or=BAS$MM

#---------------------------------------- 
# FORECAST PHASE
#---------------------------------------- 
# set ALADYM variables
forecast <-  simperiod * 12 + (current_year-1)*12 +1
GLO$L_number <- simperiod * 12 + current_year*12 
INP$Year_simulation  <- simperiod + foreperiod
bmt_average_forecast <<-  as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".AladymSimulation", sep=""),2]))  
print(paste("********************************************************************** Simulating months: from", forecast , "to",  GLO$L_number), quote=F)


Ref_point <-  NA             
Ref_month <-  NA 
Ref_year <-  NA     																

associated_fleetsegment <<- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <<- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <<- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)
FLEETSEGMENTS_names <<- BMT_FLEETSEGMENTS[associated_fleetsegment_indices] 
nb_gears <- length(FLEETSEGMENTS_names)




if (current_year == 1) {   # 같같같같같같같같같같같같같같같같같같같같같같같같같같?solo il 1?anno
INP$fish_coeff_baseline <- vector(mode="numeric", length= nb_gears)
 source(paste(ALADYM_home, "/src/DataIn_forecast.r", sep=""))
}  # 같같같같같같같같같같같같같같같같같같같같같같같같같같?FINE 1?anno


loca_Nsimulation <- (GLO$L_number + 1)  
nb_gears = length(FleetList_simulation)


RUN_CI_FORE <<- new_aldForecast@CI_calculation 

SS   <- which(SS_TYPE == new_aldSimulation@spawners.ss)  # 1 o 2 
min_ageM   <- as.numeric(data.frame(new_aldSimulation@fishingmortality)$min[rownames(data.frame(new_aldSimulation@fishingmortality)) == "M"])
max_ageM  <- as.numeric(data.frame(new_aldSimulation@fishingmortality)$max[rownames(data.frame(new_aldSimulation@fishingmortality)) == "M"])
min_ageF <- as.numeric(data.frame(new_aldSimulation@fishingmortality)$min[rownames(data.frame(new_aldSimulation@fishingmortality)) == "F"])
max_ageF  <- as.numeric(data.frame(new_aldSimulation@fishingmortality)$max[rownames(data.frame(new_aldSimulation@fishingmortality)) == "F"])    
INP$Average_forecast <- bmt_average_forecast


# ------------------------------------------------------- SENZA incertezza sul reclutamento  
  if (! new_aldForecast@CI_recruitment) {
        # ------------------------------------------------------- SENZA incertezza sul reclutamento - reclutamento COSTANTE 
    if (new_aldForecast@recruitment_constant_or_SRR == 1) {   
          INP$FRLt_fore = 4
          INP$Recruits[(forecast+1):(GLO$L_number+1)] = as.numeric(as.character(new_aldForecast@recruitment.constant)) 
        # ------------------------------------------------------- SENZA incertezza sul reclutamento - SRR
    } else {
          INP$FRLt_fore                    <- which(SR_TYPE == data.frame(new_aldForecast@stockr.relationship)$relationship[1])
          
          INP$FRLa_fore                    <- as.numeric(data.frame(new_aldForecast@stockr.relationship)$a[1])
          INP$FRLb_fore                    <- as.numeric(data.frame(new_aldForecast@stockr.relationship)$b[1])
          INP$FRLc_fore                    <- as.numeric(data.frame(new_aldForecast@stockr.relationship)$c[1])
          
          INP$Recruits[(forecast+1):(GLO$L_number+1)] =  NA
 }
     # ------------------------------------------------------- CON incertezza sul reclutamento   
 } else {
#        # ------------------------------------------------------- CON incertezza sul reclutamento - reclutamento COSTANTE   
    if (new_aldForecast@CI_recruitment_constant_or_SRR == 1) {
          INP$FRLt_fore <- 4
#        # ------------------------------------------------------- CON incertezza sul reclutamento - SRR  
  } 
  }



if (current_year==1) {  # 같같같같같같같같같같같같같같같같같같같같같같같같같같?solo il 1?anno
if((simperiod*12+1) < GLO$L_number) {
p_prod_temp = data.frame(matrix(0,nrow= (INP$Average_forecast*12+1), ncol=nb_gears))
p_prod_temp = data.frame(INP$p_Production[(((simperiod*12+1)-INP$Average_forecast*12+1):(simperiod*12+1)) ,] ) # considero i p production degli anni selezionati dall'utente
for (g in 1:nb_gears){
INP$p_Production[(forecast+1):((foreperiod+simperiod)*12+1) ,g] = mean(as.numeric(as.character(p_prod_temp[,g])) )  # medie degli ultimi x anni (solo per il forecast)
}
}

for (gear in 1:nb_gears) {
this_land_obl <- data.frame(FleetList_forecast[[gear]]@landing.obligation.vector)
 landobl_temp <- c()
       for (y in 1:length(years_forecast)) {
      if (as.character(FleetList_forecast[[gear]]@discard.calculation) == "YES") {
       row_obl <- as.character(this_land_obl[y, colnames(this_land_obl) != "year"]  )
       } else {
       row_obl <- rep("N", 12)
       }
       landobl_temp <- c(landobl_temp, row_obl)
       }
        INP$Land_obl[(forecast+1):((12*foreperiod)+forecast),gear] <- landobl_temp[1:(12*foreperiod)]                                             
}
}   # 같같같같같같같같같같같같같같같같같같같같같같같같같같?FINE 1?anno

        # INP$Recruits

eff_data <- get_effort_data()
eff_data_all <- data.frame(matrix(nrow=0, ncol=5))
colnames(eff_data_all) <- c("Gear",	"Month",	"Vessels",	"Days", "GT")

associated_fleetsegment <<- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <<- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!="" & associated_fleetsegment!="-"]
associated_fleetsegment_indices <<- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)


n_ord <- 1                                 
for (n_int in 1:length(BMT_FLEETSEGMENTS) ) {
if (n_int %in% associated_fleetsegment_indices) {
eff_data_fleet <- data.frame(matrix(nrow=0, ncol=5))
colnames(eff_data_fleet) <- c("Gear",	"Month",	"Vessels",	"Days", "GT")
 nb_years <- length(years_forecast)
for (yy in 1:nb_years) {                                                         
    vessel_temp <- as.numeric( as.character( Fleetyear[[yy+simperiod]]@fleetsegments[[n_int]]@VESSELS  ) )
    day_temp <-  as.numeric( as.character( Fleetyear[[yy+simperiod]]@fleetsegments[[n_int]]@DAYS.average  ) )
    gt_temp <-  as.numeric( as.character( Fleetyear[[yy+simperiod]]@fleetsegments[[n_int]]@GT.average  ) )
    to_add <- data.frame(cbind(rep(BMT_FLEETSEGMENTS[n_int], 12), c( ((((yy+length(years))-1)*12)) + c(1:12)), vessel_temp, day_temp, gt_temp) )
    colnames(to_add) <-c("Gear",	"Month",	"Vessels",	"Days", "GT")
    eff_data_fleet <- rbind(eff_data_fleet, to_add)
} # end years                                             
eff_data_fleet <- rbind(eff_data[eff_data$Gear == BMT_FLEETSEGMENTS[n_int], ], eff_data_fleet) 
eff_data_all <- rbind(eff_data_all, eff_data_fleet)
  n_ord <- n_ord+1
}
}


#-------------------------------------------------------------------------------
# Z estimated for M e F is the average on the last x years (as indicated by the user)
#-------------------------------------------------------------------------------
if (current_year==1) {  # 같같같같같같같같같같같같같같같같같같같같같같같같같같?solo il 1?anno
if (as.numeric(INP$OPT_F_TYPE)==1) {  
index_final <- (simperiod+foreperiod)*12       # ifelse(!INTEGRATED_APPROACH, GLO$L_number,

Z_da_mediare <- as.numeric(as.character(BAS$MZ_estimated[(forecast-INP$Average_forecast*12+1):forecast]))
x <- seq_along(Z_da_mediare)
d1 <- split(Z_da_mediare, ceiling(x/12))
d2 <- data.frame(d1)
d2$avg <- rowMeans(d2)
BAS$MZ_estimated[(forecast+1):(index_final+1)] = rep(d2$avg, length(years_forecast) )
INP$MZ_estimated[(forecast+1):(index_final+1)] = rep(d2$avg, length(years_forecast) )

Z_da_mediare <- as.numeric(as.character(BAS$FZ_estimated[(forecast-INP$Average_forecast*12+1):forecast]))
x <- seq_along(Z_da_mediare)
d1 <- split(Z_da_mediare, ceiling(x/12))
d2 <- data.frame(d1)
d2$avg <- rowMeans(d2)

BAS$FZ_estimated[(forecast+1):(index_final+1)] = rep(d2$avg, length(years_forecast) )
INP$FZ_estimated[(forecast+1):(index_final+1)] = rep(d2$avg, length(years_forecast) )

print("Total mortality M:", quote=F)
print(BAS$MZ_estimated[(forecast+1):(index_final+1)], quote=F )
print("Total mortality F:", quote=F)
print(BAS$FZ_estimated[(forecast+1):(index_final+1)], quote=F )
}
#-------------------------------------------------------------------------------
# Recruitment is the average on the last x years (as indicated by the user)
#-------------------------------------------------------------------------------
  if (RUN_CI_FORE) {
  if ( new_aldForecast@CI_recruitment ) {
     if (new_aldForecast@CI_recruitment_constant_or_SRR == 1) {
  if (new_aldForecast@CI_recruitment_1_file_or_distribution == 2 ) {    #external file (1) /distribution (2)                       ex: CI_error_source
  #  INP$R_type_sim           <- which(DISTRIBUTION == data.frame(new_aldForecast@recruitment.noise)$distribution[1])
    INP$R_type_for <- which(DISTRIBUTION == data.frame(new_aldForecast@CI_recruitment_1_2_distribution_params)$distribution[1])
    INP$R_pam1_for   <- as.numeric(data.frame(new_aldForecast@CI_recruitment_1_2_distribution_params)$A[1])
    INP$R_pam2_for  <- as.numeric(data.frame(new_aldForecast@CI_recruitment_1_2_distribution_params)$B[1])
    } else {
    INP$R_type_sim  <-  4
    INP$R_pam1_sim   <- 1
    INP$R_pam2_sim  <- 1    
   # da verificare
  INP$R_type_for <- INP$R_type_sim
  INP$R_pam1_for <-  INP$R_pam1_sim
  INP$R_pam2_for <-  INP$R_pam2_sim
  }
}
}
}
}  # 같같같같같같같같같같같같같같같같같같같같같같같같같같?FINE 1?anno  



if (! new_aldForecast@CI_recruitment) {
if ( INP$FRLt_fore==4 ) { 
INP$Recruits[(forecast+1):((simperiod+foreperiod)*12+1)] =  as.numeric(as.character(new_aldForecast@recruitment.constant))  
print("Entering recruits:", quote=F)
print(INP$Recruits[(forecast+1):(GLO$L_number+1)], quote=F )
}
} else {
 INP$Recruits_all_for_CI = INP$Recruits_original 
 print("Entering recruits on which to apply errors:", quote=F)
 print(INP$Recruits_all_for_CI[(forecast+1):(GLO$L_number+1)])
 #INP$Recruits_all_for_CI[(forecast+1):length(INP$Recruits)] =  as.numeric(as.character(new_aldForecast@recruitment.constant))  
}


# if (current_year != 1) {
# print(INP$fish_coeff_baseline )
# print(INP$p_Production)
##print(INP$Recruits_all_for_CI)
# }



if (showCompTime)  {
 proc_ <- proc.time()
print(paste("Reading forecast input [time]::::::::::::::::::::::::::::::::", round(as.numeric(proc_[3]-runALADYMforecastINT_ptm[3]),2), "sec" ), quote=F )   
rm(runALADYMforecastINT_ptm)
}

#   ALADYM_GUI_forecast_int[[ALADYM_spe]] <- new_aldForecast 
#     ALADYM_GUI_fleets_fore[[ALADYM_spe]]  <-  FleetList_forecast 



if ((forecast < GLO$L_number) & !is.na(Ref_point) & !is.na(Ref_month) & !is.na(INP$Average_forecast))  {
#--------------------------------------
# calcolo moltiplicatori per attrezzo
#--------------------------------------
forecast_simulation =  GLO$L_number - forecast +1
multipliers = matrix(nrow=forecast_simulation,ncol=nb_gears)                
 Forecast_reduction <- INP$Forecast_reduction
      
        for (n_int in 1:length(FLEETSEGMENTS_names)) {
           Forecast_reduction[1,n_int] <- toupper(FleetList_forecast[[n_int]]@scenario.reduction)  
        }      
    INP$Forecast_reduction <- Forecast_reduction  

  for (g in 1:nb_gears) {
  multipliers[,g] = REDUCTION(INP$Forecast_reduction[1,g],g)
  }


# caso in cui sto usando le riduzioni forzate, i fishing coefficient sono uguali a 1
INP$Fishing_efforts[(forecast):nrow(INP$Fishing_efforts),] <- 1  

# si innesta il codice dei CI *********************************************************************** 같같같같같같같같같같같같같같같같같?!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


if (!RUN_CI_FORE) {

loca_Fertility <- 1
loca_min_BAS.MM  <- mean(BAS$MM)
loca_min_BAS.FM <- mean(BAS$FM)
load_SIMULATION_UNEXPLOITED(loca_Fertility, forecast, GLO$L_number) 

if (as.numeric(INP$OPT_F_TYPE)==1) { 
load_FORECAST(forecast) 
} else {
load_FORECAST_entrataF(forecast)
}



source(paste(ALADYM_home, "/src/output.r" ,sep="") )
    
load_Annual_Z_Sinclair(GLO$L_number)
load_Annual_F_weighted(GLO$L_number)
load_Annual_Z(GLO$L_number)  
#Annual_F_by_gear(GLO$L_number) 
load_Annual_F(GLO$L_number) 
              
#Export(GLO$L_number)
export_tables(GLO$L_number)
export_tab_mort(GLO$L_number)
indicators(GLO$L_number)




} else {
source(paste(ALADYM_home, "/src/runCI.int.r" ,sep="") )
 } 



 } else if ( (forecast < GLO$L_number) & !is.na(INP$Average_forecast) )  {

# scenario non forzato, lavorano solo i fishing coefficient               
 forecast_simulation =  GLO$L_number - forecast +1
 multipliers = matrix(nrow=forecast_simulation,ncol=nb_gears)
  for (g in 1:nb_gears){
  multipliers[,g] = 1                 
  }

fishing_eff_result <- fact_calc(eff_data_all,"Y")
#print("Fishing efforts dalla funzione")
#print(fishing_eff_result[,8])

   for (gear in 1:nb_gears) {
    for (i in (forecast+1):(GLO$L_number+1)){
        INP$Fishing_efforts[i,gear] = fishing_eff_result[i,gear]
    }
    }

    # if (FALSE) {
    if (ALADYM_spe == 1) {
print("Fishing efforts dopo riassegnazione")
print(INP$Fishing_efforts[(forecast+1):(GLO$L_number+1),])
print("Entering p-Production:", quote=F)
print( INP$p_Production[(forecast+1):(GLO$L_number+1),], quote=F )
if (as.numeric(INP$OPT_F_TYPE)==1) {
print("Entering param1:", quote=F)
print( INP$param1[(forecast+1):(GLO$L_number+1),], quote=F )
}
print("Entering recruitment on which apply the perturbation:", quote=F)
print(INP$Recruits[(forecast+1):(GLO$L_number+1)])
}

 
 if (!RUN_CI_FORE) {

loca_Fertility <- 1
 loca_min_BAS.MM  <- mean(BAS$MM)
loca_min_BAS.FM <- mean(BAS$FM)
load_SIMULATION_UNEXPLOITED(loca_Fertility, forecast, GLO$L_number)     #
 


  if (as.numeric(INP$OPT_F_TYPE)==1) { 
      load_FORECAST(forecast) 
  } else {
      load_FORECAST_entrataF(forecast)
  }
  
 source(paste(ALADYM_home, "/src/output.r" ,sep="") )

load_Annual_Z_Sinclair(GLO$L_number)
load_Annual_F_weighted(GLO$L_number)
load_Annual_Z(GLO$L_number)  
load_Annual_F(GLO$L_number) 
              
export_tables(GLO$L_number)
export_tab_mort(GLO$L_number)
indicators(GLO$L_number)
    
 } else {

source( paste(ALADYM_home, "/src/runCI.int.r" ,sep="") )
 } 

 } 


 
 save_path <- paste(workingfilesDIR,"/", prefix_outfiles, "Selectivity params", suffix_outfiles,".csv", sep="") 
  save_path_fc <- paste(workingfilesDIR,"/", prefix_outfiles, "Fishing coefficients", suffix_outfiles,".csv", sep="")  
   save_path_eff <- paste(workingfilesDIR,"/", prefix_outfiles, "Effort data", suffix_outfiles,".csv", sep="")       
    
   write.table(cbind(INP$param1,cbind(INP$param2, cbind(INP$param3, cbind(INP$param4, INP$param5)))), save_path, sep=";", row.names=F)      
   write.table(INP$Fishing_efforts, save_path_fc, sep=";", row.names=F)
   write.table(eff_data_all, save_path_eff, sep=";", row.names=F)



if (current_year == foreperiod) {


print("Saving ALADYM plots...", quote=F)
PlotRecruitment()
cat(" [PlotRecruitment] ") 
#PlotInput()
#cat(" [PlotInput] ") 

if (!IN_BEMTOOL) {

PlotYear()
cat(" [PlotYear] ") 
Plot_yield()
cat(" [Plot_yield] ") 
Plot_length() 
cat(" [Plot_length] ")   
Plot_F()             
cat(" [Plot_F] ")

} else {

PlotYear_allFleets()
cat(" [PlotYear] ")
#Plot_yield_allFleets() 
#cat(" [Plot_yield] ") 
Plot_length_allFleets() 
cat(" [Plot_length] ") 
#Plot_F_allFleets()              
#cat(" [Plot_F] ")
  
} 







print("Program end.", quote=FALSE)
flush.console()

#save.image(file="simulation.RData")

print("Timing:", quote=FALSE)
print("Usr  Sys  Total", quote=FALSE)
print(proc.time() - GLO$starting_time)
flush.console()


INP$filenameForecast <- paste(casestudy_path, "/",harvest_rule_id, "/ALADYM/", BMT_SPECIES[ALADYM_spe], "/", casestudy_name, " - FORE ", BMT_SPECIES[ALADYM_spe], sep="")

   
#if(.Platform$OS.type == "unix") {
 # system(paste("cp datain.dat       '", INP$filenameForecast, ".din'  ", sep=""))
  #system(paste("cp export.dat       '", INP$filenameForecast, ".dou'  ", sep=""))
  #system(paste("cp simulation.RData '", INP$filename, ".RData'", sep=""))
# }



#if(.Platform$OS.type == "windows") {
 # shell(paste("copy datain.dat       \"", INP$filenameForecast, ".din\"  ", sep=""))
  #shell(paste("copy export.dat       \"", INP$filenameForecast, ".dou\"  ", sep=""))
  #shell(paste("copy simulation.RData \"", INP$filename, ".RData\"", sep=""))
#}




#unlink("export.dat")
#unlink("datain.dat")



}


#print(paste("alla fine di ruALDYMforecast.int .... anno ",current_year," specie", ALADYM_spe))

 
  .GlobalEnv$ALADYM_GUI_fleets_fore[[ALADYM_spe]] <- FleetList_forecast #fleet_list
#print(.GlobalEnv$ALADYM_GUI_fleets_fore[[ALADYM_spe]][[1]]@fishingeffort.vector) 



path_to_save <- paste(casestudy_path, "/",harvest_rule_id, "/working files/GUIfle_fore.Rdata", sep="")
save(ALADYM_GUI_fleets_fore, envir = .GlobalEnv, file= path_to_save)  


ALADYM_GUI_fleets_fore_succ <<-  ALADYM_GUI_fleets_fore


source(paste(getwd(), "/src/biol/bmtALADYM/saveEnvSpecies.r", sep=""))  



#cat("\n\n")
#print("***************************************************************************", quote=FALSE)
#print("ALADYM simulation completed!", quote=FALSE)
#print("***************************************************************************", quote=FALSE)
#cat("\n\n")


if (current_year == 1) {


wnd_fore$destroy()




main_window$destroy() 

}