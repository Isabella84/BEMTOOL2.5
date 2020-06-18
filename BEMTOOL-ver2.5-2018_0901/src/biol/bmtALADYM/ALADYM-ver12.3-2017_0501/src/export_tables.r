# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

# export all the relevant data

export_tables<-function(End) {

if (FALSE) {
End = GLO$L_number
}

if (showCompTime)  {
export_tables_ptm <- proc.time()  
}

if (IN_BEMTOOL) {
associated_fleetsegment <<- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <<- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <<- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

FLEETSEGMENTS_names <<- BMT_FLEETSEGMENTS[associated_fleetsegment_indices]

}


loca_xa <-c(1:(End/INP$Time_slice))

if (exists("num_iter_RPs")) {

loca_y <- loca_xa
} else {

if (INP$Year_simulation == length(years) ) {
    loca_y <- years
}   else {
loca_y <- c(years, years_forecast)
loca_y <- loca_y[loca_xa]
}

}

mortalities_temp = loca_y                                                                                   # Year
mortalities_temp = cbind(mortalities_temp,meanWequals(SRO$Z_calculated, End + 1, INP$Time_slice))    # Z_estimated_monthly 
mortalities_temp = cbind(mortalities_temp,meanWequals(SRO$MZ_calculated, End + 1, INP$Time_slice))   # Z_estimated_of_males(monthly)
mortalities_temp = cbind(mortalities_temp,meanWequals(SRO$FZ_calculated, End + 1, INP$Time_slice))   # Z_estimated_of_females(monthly)
#mortalities_temp = cbind(mortalities_temp,meanWequals(SRO$MZa_calculated, End + 1, INP$Time_slice))  # Annual_Z_estimated_of_males(min-max)
#mortalities_temp = cbind(mortalities_temp,meanWequals(SRO$FZa_calculated, End + 1, INP$Time_slice))  # Annual_Z_estimated_of_females(min-max)
#mortalities_temp = cbind(mortalities_temp,meanWequals(SRO$MZa_calculated_ls, End + 1, INP$Time_slice))   #aggiunto   # Annual_Z_estimated_of_males (life_span)
#mortalities_temp = cbind(mortalities_temp,meanWequals(SRO$FZa_calculated_ls, End + 1, INP$Time_slice))   #aggiunto    # Annual_Z_estimated_of_females (life span)

mortalities_temp = cbind(mortalities_temp,SRO$Annual_Z_males_SINCLAIR_ls)   #aggiunto   # Annual_Z_estimated_of_males (life_span)  CON METODO RIVISTO
mortalities_temp = cbind(mortalities_temp,SRO$Annual_Z_females_SINCLAIR_ls)   #aggiunto    # Annual_Z_estimated_of_females (life span)  CON METODO RIVISTO
mortalities_temp = cbind(mortalities_temp,SRO$Annual_Z_by_year_ls)  # Annual_Z_estimated life span CON METODO coorti
mortalities_temp = cbind(mortalities_temp,SRO$Annual_Z_by_year)  # Annual_Z_estimated age range     CON METODO coorti
mortalities_temp = cbind(mortalities_temp,meanWequals(SRO$F_calculated, End + 1, INP$Time_slice))        # F_estimated_monthly


if (length(FLEETSEGMENTS_names)!=1){                                                                                                 
   for (g in 1 : length(FLEETSEGMENTS_names)) {
   mortalities_temp = cbind(mortalities_temp,meanWequals(SRO$F_calculated_by_gear[,g], End + 1, INP$Time_slice))              # F_estimated_monthly by gear
   }
}

#mortalities_temp = cbind(mortalities_temp,SRO$Annual_F_weighted)                            # Annual_F_estimated(M:0-3&F:1-5)        CON METODO RIVISTO
mortalities_temp = cbind(mortalities_temp,SRO$Annual_F_ls_weighted)    # aggiunto           # Annual_F_estimated(life_span)                CON METODO RIVISTO
mortalities_temp = cbind(mortalities_temp,SRO$Annual_F_males_ls_weighted) #aggiunto             # Annual_F_estimated_of_males(life_span)   CON METODO RIVISTO
mortalities = cbind(mortalities_temp,SRO$Annual_F_females_ls_weighted)     #aggiunto              # Annual_F_estimated_of_females(life_span)  CON METODO RIVISTO


if (length(FLEETSEGMENTS_names)!=1){                                                                                             
   for (g in 1 : length(FLEETSEGMENTS_names)) {
#mortalities = cbind(mortalities,meanWequals(SRO$annual_F_calc_by_gear[,g], End + 1, INP$Time_slice))                         # Annual_F_estimated by gear  (M:0-3&F:1-5)
mortalities = cbind(mortalities,SRO$Annual_F_by_gear_weighted[,g])   
   }
}       

 
  
mortalities = cbind(mortalities,SRO$Annual_F_by_year_ls)                             # Annual_F_estimated life span        CON METODO coorti 
 
if (length(FLEETSEGMENTS_names)!=1){                                                                                             
   for (g in 1 : length(FLEETSEGMENTS_names)) {
mortalities = cbind(mortalities,SRO$Annual_F_by_gear_by_year_ls[,g])                                                # Annual_F_estimated age range  by gear      CON METODO coorti
   }
} 

mortalities= cbind(mortalities,SRO$Annual_F_by_year)                                                            # Annual_F_estimated_age range  CON METODO coorti
if (length(FLEETSEGMENTS_names)!=1){                                                                                             
   for (g in 1 : length(FLEETSEGMENTS_names)) {
mortalities = cbind(mortalities,SRO$Annual_F_by_gear_by_year[,g])                                                # Annual_F_estimated   life span by gear      CON METODO coorti
   }
} 



if (length(FLEETSEGMENTS_names)!=1){
colnames (mortalities) = c("Year","Z_estimated_monthly","Z_estimated_of_males(monthly)","Z_estimated_of_females(monthly)","Annual_Z_estimated_of_males (Sinclair)","Annual_Z_estimated_of_females (Sinclair)","Annual_Z_estimated_life_span","Annual_Z_estimated","F_estimated_monthly", paste("F_estimated_monthly_",FLEETSEGMENTS_names),"Annual_F_estimated(weighted)","Annual_F_estimated_of_males(weighted)", "Annual_F_estimated_of_females(weighted)",paste("Annual_F_estimated",FLEETSEGMENTS_names,"(weighted)",sep=""),"Annual_F_estimated_life_span",paste("Annual_F_estimated_ls_",FLEETSEGMENTS_names,sep=""),"Annual_F_estimated",paste("Annual_F_estimated_",FLEETSEGMENTS_names,sep=""))
}  else {
colnames (mortalities) = c("Year","Z_estimated_monthly","Z_estimated_of_males(monthly)","Z_estimated_of_females(monthly)","Annual_Z_estimated_of_males (Sinclair)","Annual_Z_estimated_of_females (Sinclair)","Annual_Z_estimated_life_span","Annual_Z_estimated","F_estimated_monthly","Annual_F_estimated(weighted)","Annual_F_estimated_of_males(weighted)", "Annual_F_estimated_of_females(weighted)","Annual_F_estimated_life_span","Annual_F_estimated")
}

write.table(mortalities, MORTALITY_table,row.names=FALSE, sep=";")
Population_temp =  loca_y
Population_temp = cbind(Population_temp,meanWequals(SRO$FBiomass, End + 1, INP$Time_slice))
Population_temp = cbind(Population_temp,meanWequals(SRO$UBiomass, End + 1, INP$Time_slice))
Population_temp = cbind(Population_temp,meanWequals((BAS$MFSS_Number+BAS$FFSS_Number)/1000 , End + 1, INP$Time_slice))
Population_temp = cbind(Population_temp,meanWequals((BAS$MUSS_Number+BAS$FUSS_Number)/1000 , End + 1, INP$Time_slice))
Population_temp = cbind(Population_temp,meanWequals(SRO$FSSBiomass, End + 1, INP$Time_slice))
Population_temp = cbind(Population_temp,meanWequals(SRO$USSBiomass, End + 1, INP$Time_slice))
Population_temp = cbind(Population_temp,meanWequals(SRO$FSSBratioUSSB, End + 1, INP$Time_slice))

Population_temp = cbind(Population_temp,meanWequals(SRO$FLength_mean[1:(End + 1)]        , End + 1, INP$Time_slice))
Population_temp = cbind(Population_temp,meanWequals(SRO$ULength_mean[1:(End + 1)]        , End + 1, INP$Time_slice))
Population_temp = cbind(Population_temp,meanWequals(SRO$FSSLength_mean[1:(End + 1)]        , End + 1, INP$Time_slice))

Population_temp = cbind(Population_temp,meanWequals(SRO$USSLength_mean[1:(End + 1)]        , End + 1, INP$Time_slice))
Population_temp = cbind(Population_temp,meanWequals(SRO$FAge_mean[1:(End + 1)]        , End + 1, INP$Time_slice))

Population_temp = cbind(Population_temp,meanWequals(SRO$UAge_mean[1:(End + 1)]        , End + 1, INP$Time_slice))
Population_temp = cbind(Population_temp,meanWequals(SRO$FSSAge_mean[1:(End + 1)]        , End + 1, INP$Time_slice))
Population_temp = cbind(Population_temp,meanWequals(SRO$USSAge_mean[1:(End + 1)]        , End + 1, INP$Time_slice))

mat_temp_all = meanWequals((SRO$MFPopulation[1:(End + 1),1] + SRO$FFPopulation[1:(End + 1),1])/1000  , End + 1, INP$Time_slice)

for (Col in 2:max(ncol(SRO$MFPopulation),ncol(SRO$FFPopulation))) {
if (Col <= ncol(SRO$MFPopulation)) {
num_males <- SRO$MFPopulation[1:(End + 1),Col]
} else {
num_males <-  rep(NA, max(ncol(SRO$MFPopulation),ncol(SRO$FFPopulation)))
}

if (Col <= ncol(SRO$FFPopulation)) {
num_females <- SRO$FFPopulation[1:(End + 1),Col]
} else {
num_females <- rep(NA, max(ncol(SRO$MFPopulation),ncol(SRO$FFPopulation)))
}

mat_temp = meanWequals(rowSums(cbind(num_males , num_females), na.rm=T)/1000       , End + 1, INP$Time_slice)
mat_temp_all = rbind(mat_temp_all, mat_temp)
}

mat_temp_all_by_year <- colSums(mat_temp_all )
Population_temp = cbind(Population_temp, mat_temp_all_by_year)

# ************************************************************************************************

mat_temp_all = meanWequals((SRO$MUPopulation[1:(End + 1),1] + SRO$FUPopulation[1:(End + 1),1])/1000 , End + 1, INP$Time_slice)

for (Col in 2:max(ncol(SRO$MUPopulation),ncol(SRO$FUPopulation))) {
if (Col <= ncol(SRO$MUPopulation)) {
num_males <- SRO$MUPopulation[1:(End + 1),Col]
} else {
num_males <-  rep(NA, max(ncol(SRO$MUPopulation),ncol(SRO$FUPopulation)))
}

if (Col <= ncol(SRO$FUPopulation)) {
num_females <- SRO$FUPopulation[1:(End + 1),Col]
} else {
num_females <- rep(NA, max(ncol(SRO$MUPopulation),ncol(SRO$FUPopulation)))
}

mat_temp = meanWequals(rowSums(cbind(num_males , num_females), na.rm=T)/1000       , End + 1, INP$Time_slice)

mat_temp_all = rbind(mat_temp_all, mat_temp)
}

mat_temp_all_by_year <- colSums(mat_temp_all )

Population = cbind(Population_temp, mat_temp_all_by_year)



colnames (Population) = c("Year","Total_biomass_exploited_pop","Total_biomass_unexploited_pop","SS_NUMBERS_exploited_pop","SS_NUMBERS_unexploited_pop","SSB_exploited_pop","SSB_unexploited_pop","ESSBratioUSSB","Mean_length_of_exploited_pop","Mean_length_of_unexploited_pop","Mean_length_SS_of_exploited_pop","Mean_length_SS_of_unexploited_pop","Mean_age_of_exploited_pop","Mean_age_of_unexploited_pop","Mean_age_SS_of_exploited_pop","Mean_age_SS_of_unexploited_pop","NUMBERS_exploited_pop","NUMBERS_unexploited_pop")

write.table(Population, POPULATION_table,row.names=FALSE, sep=";")


SSB_Table <- data.frame(matrix(0, nrow=length(loca_y), ncol=12)) 

for (mon in 1:12) {
   SSB_Table[, mon] <- SRO$FSSBiomass[mon +1+ 12*(c(0:(length(loca_y)-1)))]
}

SSB_Table <- cbind(loca_y, SSB_Table) 

colnames(SSB_Table) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

write.table(SSB_Table, InstantSSB_table,row.names=FALSE, sep=";")

Production_temp =  loca_y
Production_temp = cbind(Production_temp,sumWequals(SRO$Biological_production[1:(End + 1)] , End + 1, INP$Time_slice))
Production_temp = cbind(Production_temp,sumWequals(SRO$Death_biomass[1:(End + 1)] , End + 1, INP$Time_slice))
Production_temp = cbind(Production_temp,sumWequals(SRO$Capture_biomass[1:(End + 1)] , End + 1, INP$Time_slice))

if (length(FLEETSEGMENTS_names)!=1) {
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Production_temp = cbind(Production_temp,sumWequals(SRO$Capture_biomass_gears[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}
Production_temp = cbind(Production_temp,meanWequals(SRO$Capture_length_mean[1:(End + 1)]        , End + 1, INP$Time_slice))
if (length(FLEETSEGMENTS_names)!=1){
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Production_temp = cbind(Production_temp,meanWequals(SRO$Capture_length_mean_gears[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}

Production_temp = cbind(Production_temp,meanWequals(SRO$Capture_age_mean[1:(End + 1)]        , End + 1, INP$Time_slice))
if (length(FLEETSEGMENTS_names)!=1){
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Production_temp = cbind(Production_temp,meanWequals(SRO$Capture_age_mean_gears[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}

# landing
Production_temp = cbind(Production_temp,sumWequals(SRO$Landing_biomass[1:(End + 1)] , End + 1, INP$Time_slice))

if (length(FLEETSEGMENTS_names)!=1){
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Production_temp = cbind(Production_temp,sumWequals(SRO$Landing_biomass_gears[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}
Production_temp = cbind(Production_temp,meanWequals(SRO$Landing_length_mean[1:(End + 1)]        , End + 1, INP$Time_slice))
if (length(FLEETSEGMENTS_names)!=1){
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Production_temp = cbind(Production_temp,meanWequals(SRO$Landing_length_mean_gears[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}
Production_temp = cbind(Production_temp,meanWequals(SRO$Landing_age_mean[1:(End + 1)]        , End + 1, INP$Time_slice))
if (length(FLEETSEGMENTS_names)!=1){
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Production_temp = cbind(Production_temp,meanWequals(SRO$Landing_age_mean_gears[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}
#-----
#discard
Production_temp = cbind(Production_temp,sumWequals(SRO$Discard_biomass[1:(End + 1)] , End + 1, INP$Time_slice))
if (length(FLEETSEGMENTS_names)!=1){
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Production_temp = cbind(Production_temp,sumWequals(SRO$Discard_biomass_gears[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}
length_loca_this <- meanWequals(SRO$Discard_length_mean[1:(End + 1)], End + 1, INP$Time_slice) 
Production_temp = cbind(Production_temp, ifelse(is.na(length_loca_this), 0 , length_loca_this))
if (length(FLEETSEGMENTS_names)!=1){
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  length_loca_this <- meanWequals(SRO$Discard_length_mean_gears[1:(End + 1),g] , End + 1, INP$Time_slice)
  Production_temp = cbind(Production_temp, ifelse(is.na(length_loca_this), 0 , length_loca_this))
  }
}
Production_temp = cbind(Production_temp,meanWequals(SRO$Discard_age_mean[1:(End + 1)]        , End + 1, INP$Time_slice))

if (INP$Year_simulation == length(years) ) {
    land_mat_to_add_nrow <- years
}   else {
land_mat_to_add_nrow <- c(years, years_forecast)

}


land_mat_to_add <- data.frame(matrix("N", nrow=length(land_mat_to_add_nrow), ncol=length(FLEETSEGMENTS_names)))

if (length(FLEETSEGMENTS_names)!=1) {
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Production_temp = cbind(Production_temp,meanWequals(SRO$Discard_age_mean_gears[1:(End + 1),g] , End + 1, INP$Time_slice))
 
 #print(FleetList_forecast[[g]]@landing.obligation.vector)


# da cancellareeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
 
# land_obl_Matr_sim <- data.frame(matrix("N", nrow=length(years), ncol=(length(MONTHS)+1)))
#   colnames(land_obl_Matr_sim) <- c("year",	MONTHS)
#   land_obl_Matr_sim$year <- years
#
#land_obl_Matr_fore <- data.frame(matrix("N", nrow=length(years_forecast), ncol=(length(MONTHS)+1)))
#   colnames(land_obl_Matr_fore) <- c("year",	MONTHS)
#   land_obl_Matr_fore$year <- years_forecast
#
# 
#FleetList_simulation[[g]]@landing.obligation.vector <<- land_obl_Matr_sim
#FleetList_forecast[[g]]@landing.obligation.vector <<- land_obl_Matr_fore

# da cancellareeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
 
if (INP$Year_simulation == length(years) ) {
land_obl_f  <-  FleetList_simulation[[g]]@landing.obligation.vector
} else {
 land_obl_f  <- rbind(FleetList_simulation[[g]]@landing.obligation.vector, FleetList_forecast[[g]]@landing.obligation.vector )
}

 
  
#  apply(SRO$MFCatch_gears[,,n], MARGIN=2, FUN="sumWequals",  para_vectLength = (End + 1) , para_pointsMean = INP$Time_slice )
if (!exists("num_iter_RPs")) {
  land_mat_to_add[,g] <- apply(land_obl_f, MARGIN=1, FUN="ifLandingObl")
 }
 
}
}

#------

if(length(FLEETSEGMENTS_names)==1) {
colnames (Production_temp) = c("Year","Biological_Production","Natural_death_biomass","Total_Yield","Mean_length_in_catch","Mean_age_in_catch","Total_Landing","Mean_length_in_Landing","Mean_age_in_Landing","Total_Discard","Mean_length_in_Discard","Mean_age_in_Discard")
}  else {
colnames (Production_temp) = c("Year","Biological_Production","Natural_death_biomass","Total_Yield",paste("Yield_",FLEETSEGMENTS_names, sep=""),"Mean_length_in_catch",paste("Mean_length_in_catch_",FLEETSEGMENTS_names, sep=""),"Mean_age_in_catch",paste("Mean_age_in_catch_",FLEETSEGMENTS_names, sep=""),"Total_Landing",paste("Landing_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_Landing",paste("Mean_length_in_Landing_",FLEETSEGMENTS_names,sep=""),"Mean_age_in_Landing",paste("Mean_age_in_Landing_",FLEETSEGMENTS_names,sep=""),"Total_Discard",paste("Discard_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_Discard",paste("Mean_length_in_Discard_",FLEETSEGMENTS_names,sep=""),"Mean_age_in_Discard",paste("Mean_age_in_Discard_",FLEETSEGMENTS_names,sep=""))
}
Production_temp=data.frame(Production_temp)


 Production_temp$Discard_ratio <- 0
for (ndisc_rat in 1:nrow(Production_temp)) {
   Production_temp$Discard_ratio[ndisc_rat] = ifelse(Production_temp$Total_Discard[ndisc_rat]=="NA",NA,as.numeric(as.character(Production_temp$Total_Discard[ndisc_rat]))/as.numeric(as.character(Production_temp$Total_Yield[ndisc_rat])) )
}

#Production_temp$Discard_ratio = ifelse(Production_temp$Total_Discard=="NA",NA,as.numeric(as.character(Production_temp$Total_Discard))/as.numeric(as.character(Production_temp$Total_Yield)) ) 



# added 27052015
if (length(FLEETSEGMENTS_names)!=1) {
  for (g in 1 : length(FLEETSEGMENTS_names)) {
   evaluate_discard <- as.numeric(as.character(Production_temp[,colnames(Production_temp) == paste("Discard_",FLEETSEGMENTS_names[g],sep="")])) 
  
  to_add_discard_ratio_by_fleet <- rep(0, length(evaluate_discard))
      yield_by_f <-  as.numeric(as.character( Production_temp[,colnames(Production_temp) == paste("Yield_",FLEETSEGMENTS_names[g],sep="")] ))
      
for (dis_y in 1:length(evaluate_discard)) {
    if (is.na(evaluate_discard[dis_y])) {
         to_add_discard_ratio_by_fleet[dis_y] <- NA 
    } else {

        to_add_discard_ratio_by_fleet[dis_y] <- evaluate_discard[dis_y] / yield_by_f[dis_y]
    }
}
#to_add_discard_ratio_by_fleet = ifelse(Production_temp[,colnames(Production_temp) == paste("Discard_",FLEETSEGMENTS_names[g],sep="")] == "NA", NA, as.numeric(as.character(Production_temp[,colnames(Production_temp) == paste("Discard_",FLEETSEGMENTS_names[g],sep="")])) / as.numeric(as.character(paste("Yield_",FLEETSEGMENTS_names[g], sep=""))) )

to_add_discard_ratio_by_fleet <- data.frame(matrix(to_add_discard_ratio_by_fleet, ncol=1))
 colnames(to_add_discard_ratio_by_fleet) <- paste("Discard_ratio_",FLEETSEGMENTS_names[g],sep="")  
Production_temp <- data.frame(cbind(Production_temp, to_add_discard_ratio_by_fleet) )

} 
}

# -------------------- end added 27052015



colnames(land_mat_to_add) <- paste("LandingObligation_",FLEETSEGMENTS_names,sep="")

#print(land_mat_to_add)

Production_temp = data.frame(cbind(Production_temp, land_mat_to_add[1:length(loca_y),]) )

write.table(Production_temp, PRODUCTION_table,row.names=FALSE, sep=";")



if (FALSE) {
# detailed DISCARD TABLE

Discard_temp =  loca_y
Discard_temp = cbind(Discard_temp,sumWequals(SRO$Discard_biomass[1:(End + 1)] , End + 1, INP$Time_slice))
if (length(FLEETSEGMENTS_names)!=1){
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Discard_temp = cbind(Discard_temp,sumWequals(SRO$Discard_biomass_gears[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}

Discard_temp = cbind(Discard_temp,sumWequals(SRO$Discard_biomass_landed[1:(End + 1)] , End + 1, INP$Time_slice))
if (length(FLEETSEGMENTS_names)!=1){
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Discard_temp = cbind(Discard_temp,sumWequals(SRO$Discard_biomass_gears_landed[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}

Discard_temp = cbind(Discard_temp,sumWequals(SRO$Discard_biomass_sea[1:(End + 1)] , End + 1, INP$Time_slice))
if (length(FLEETSEGMENTS_names)!=1){
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Discard_temp = cbind(Discard_temp,sumWequals(SRO$Discard_biomass_gears_sea[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}

Discard_temp = cbind(Discard_temp,meanWequals(SRO$Discard_length_mean[1:(End + 1)] , End + 1, INP$Time_slice))
if (length(FLEETSEGMENTS_names)!=1){
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Discard_temp = cbind(Discard_temp,meanWequals(SRO$Discard_length_mean_gears[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}
Discard_temp = cbind(Discard_temp,meanWequals(SRO$Discard_length_mean_landed[1:(End + 1)] , End + 1, INP$Time_slice))
if (length(FLEETSEGMENTS_names)!=1){
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Discard_temp = cbind(Discard_temp,meanWequals(SRO$Discard_length_mean_gears_landed[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}
Discard_temp = cbind(Discard_temp,meanWequals(SRO$Discard_length_mean_sea[1:(End + 1)] , End + 1, INP$Time_slice))
if (length(FLEETSEGMENTS_names)!=1){
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Discard_temp = cbind(Discard_temp,meanWequals(SRO$Discard_length_mean_gears_sea[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}
Discard_temp = cbind(Discard_temp,meanWequals(SRO$Discard_age_mean[1:(End + 1)] , End + 1, INP$Time_slice))
if (length(FLEETSEGMENTS_names)!=1){
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Discard_temp = cbind(Discard_temp,meanWequals(SRO$Discard_age_mean_gears[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}
Discard_temp = cbind(Discard_temp,meanWequals(SRO$Discard_age_mean_landed[1:(End + 1)] , End + 1, INP$Time_slice))
if (length(FLEETSEGMENTS_names)!=1){
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Discard_temp = cbind(Discard_temp,meanWequals(SRO$Discard_age_mean_gears_landed[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}
Discard_temp= cbind(Discard_temp,meanWequals(SRO$Discard_age_mean_sea[1:(End + 1)] , End + 1, INP$Time_slice))
if (length(FLEETSEGMENTS_names)!=1){
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Discard_temp = cbind(Discard_temp,meanWequals(SRO$Discard_age_mean_gears_sea[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}
Discard = as.data.frame(Discard_temp)

if(length(FLEETSEGMENTS_names)!=1) {
colnames(Discard) = c("Year","Total_discard",paste("Discard",FLEETSEGMENTS_names),"Total_discard_landed",paste("Discard_landed_",FLEETSEGMENTS_names),"Total_discard_sea",paste("Discard_sea_",FLEETSEGMENTS_names),"Mean_length_in_total_discard",paste("Mean_length_in_discard_",FLEETSEGMENTS_names),"Mean_length_in_total_discard_landed",paste("Mean_length_in_discard_landed",FLEETSEGMENTS_names),"Mean_length_in_total_discard_sea",paste("Mean_length_in_discard_sea",FLEETSEGMENTS_names),"Mean_age_in_total_discard",paste("Mean_age_in_discard_",FLEETSEGMENTS_names),"Mean_age_in_total_discard_landed",paste("Mean_age_in_discard_landed_",FLEETSEGMENTS_names),"Mean_age_in_total_discard_sea",paste("Mean_age_in_discard_sea_",FLEETSEGMENTS_names))
} else {
colnames(Discard) = c("Year","Total_discard","Total_discard_landed","Total_discard_sea","Mean_length_in_total_discard","Mean_length_in_total_discard_landed","Mean_length_in_total_discard_sea","Mean_age_in_total_discard","Mean_age_in_total_discard_landed","Mean_age_in_total_discard_sea")

}
write.table(Discard, DETAILED_DISCARD_table,row.names=FALSE, sep=";")
}

Recruitment = data.frame(matrix(0, nrow=length(loca_y), ncol=2))
colnames(Recruitment) <- c("Year", "Recruitment")
Recruitment[,1]   <- loca_y
Recruitment[,2] <- meanWequals(INP$Recruits,End + 1, INP$Time_slice)    # modificata per reference points

write.table(Recruitment, RECRUITMENT_table, row.names=FALSE, sep=";" )


 if (showCompTime)  {
 proc_ <- proc.time()
# SIMULATION_EXPLOITED_ptm <- proc.time()
print(paste("export_tables [time]::::::::::::::::::::::::::::::::", round(as.numeric(proc_[3]-export_tables_ptm[3]),2), "sec" ), quote=F )   
#print(proc.time() - SIMULATION_EXPLOITED_ptm, quote=F ) 
rm(export_tables_ptm)
}


}
                      