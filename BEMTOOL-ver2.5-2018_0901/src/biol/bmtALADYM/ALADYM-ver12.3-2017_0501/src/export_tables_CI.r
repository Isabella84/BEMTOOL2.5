# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

# export all the relevant data

export_tables_CI<-function(End) {


if (FALSE) {
End <- GLO$L_number
}

if (showCompTime)  {
export_tables_CI_ptm <- proc.time()  
}

if (IN_BEMTOOL) {
associated_fleetsegment <<- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <<- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <<- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

FLEETSEGMENTS_names <<- BMT_FLEETSEGMENTS[associated_fleetsegment_indices]
#print(FLEETSEGMENTS_names)
}

tables_to_return <- vector( mode="list", length=3)

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



mortalities_temp = cbind(mortalities_temp,SRO$Annual_F_ls_weighted)    # aggiunto           # Annual_F_estimated(life_span)                CON METODO RIVISTO
mortalities_temp = cbind(mortalities_temp,SRO$Annual_F_males_ls_weighted) #aggiunto             # Annual_F_estimated_of_males(life_span)   CON METODO RIVISTO
mortalities = cbind(mortalities_temp,SRO$Annual_F_females_ls_weighted)     #aggiunto              # Annual_F_estimated_of_females(life_span)  CON METODO RIVISTO


if (nb_gears!=1){                                                                                             
   for (g in 1 : nb_gears) {

mortalities = cbind(mortalities,SRO$Annual_F_by_gear_weighted[,g])   
   }
}       






  
mortalities = cbind(mortalities,SRO$Annual_F_by_year_ls)                             # Annual_F_estimated life span        CON METODO coorti 
 
if (nb_gears!=1){                                                                                             
   for (g in 1 : nb_gears) {
mortalities = cbind(mortalities,SRO$Annual_F_by_gear_by_year_ls[,g])                                                # Annual_F_estimated age range  by gear      CON METODO coorti
   }
} 

mortalities= cbind(mortalities,SRO$Annual_F_by_year)                                                            # Annual_F_estimated_age range  CON METODO coorti
if (nb_gears!=1){                                                                                             
   for (g in 1 : nb_gears) {
mortalities = cbind(mortalities,SRO$Annual_F_by_gear_by_year[,g])                                                # Annual_F_estimated   life span by gear      CON METODO coorti
   }
} 


gears= as.character(t(FLEETSEGMENTS_names))






mortalities <- data.frame(mortalities)
	 colnames(mortalities)  <- mortalities_ALLruns_head[-length(mortalities_ALLruns_head)]

		tables_to_return[[1]] <- mortalities

mortalities <- data.frame(cbind(mortalities, rep(current_runCI, nrow(mortalities)))  )
	 colnames(mortalities)  <- mortalities_ALLruns_head
	 
	 if (!INTEGRATED_APPROACH) {
mortalities_ALLruns <<- data.frame(rbind(mortalities_ALLruns, mortalities))
} else {
if (current_year ==1) {
INP$mortalities_ALLruns <- data.frame(rbind(INP$mortalities_ALLruns, mortalities))
} else {
INP$mortalities_ALLruns <- data.frame(rbind(INP$mortalities_ALLruns, mortalities[mortalities$Year == years_forecast[current_year],]))
}
}
#write.table(mortalities, MORTALITY_table,row.names=FALSE, sep=";")




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
Population_temp = cbind(Population_temp,meanWequals((SRO$MFPopulation[1:(End + 1)] + SRO$FFPopulation[1:(End + 1)])/1000 , End + 1, INP$Time_slice))
Population = cbind(Population_temp,meanWequals((SRO$MUPopulation[1:(End + 1)] + SRO$FUPopulation[1:(End + 1)])/1000  , End + 1, INP$Time_slice))
#colnames(Population) <- Population_ALLruns_head
   Population <- data.frame(Population)
	 colnames(Population)  <- Population_ALLruns_head[-length(Population_ALLruns_head)]
tables_to_return[[3]] <- Population


Population <- data.frame(cbind(Population, rep(current_runCI, nrow(Population)))  )
	 colnames(Population)  <- Population_ALLruns_head
  
	 if (!INTEGRATED_APPROACH) {
   Population_ALLruns <<- data.frame(rbind(Population_ALLruns, Population))
  } else {
  if (current_year ==1) {
    INP$Population_ALLruns <- data.frame(rbind(INP$Population_ALLruns, Population)) 
    } else {
      INP$Population_ALLruns <- data.frame(rbind(INP$Population_ALLruns, Population[Population$Year == years_forecast[current_year],]))                                                              
    }
  }
#write.table(Population, POPULATION_table,row.names=FALSE, sep=";")



Production_temp =  loca_y
Production_temp = cbind(Production_temp,sumWequals(SRO$Biological_production[1:(End + 1)] , End + 1, INP$Time_slice))
Production_temp = cbind(Production_temp,sumWequals(SRO$Death_biomass[1:(End + 1)] , End + 1, INP$Time_slice))
Production_temp = cbind(Production_temp,sumWequals(SRO$Capture_biomass[1:(End + 1)] , End + 1, INP$Time_slice))

#print("------------------------------------------------------------------ CAPTURE BIOMASS")
#print(SRO$Capture_biomass[1:(End + 1)])

if (nb_gears!=1){
  for (g in 1 : nb_gears) {
  Production_temp = cbind(Production_temp,sumWequals(SRO$Capture_biomass_gears[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}
Production_temp = cbind(Production_temp,meanWequals(SRO$Capture_length_mean[1:(End + 1)]  , End + 1, INP$Time_slice))

if (nb_gears!=1){
  for (g in 1 : nb_gears) {
  Production_temp = cbind(Production_temp,meanWequals(SRO$Capture_length_mean_gears[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}
Production_temp = cbind(Production_temp,meanWequals(SRO$Capture_age_mean[1:(End + 1)]        , End + 1, INP$Time_slice))


if (nb_gears!=1){
  for (g in 1 : nb_gears) {
  Production_temp = cbind(Production_temp,meanWequals(SRO$Capture_age_mean_gears[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}

# landing
Production_temp = cbind(Production_temp,sumWequals(SRO$Landing_biomass[1:(End + 1)] , End + 1, INP$Time_slice))

if (nb_gears!=1){
  for (g in 1 : nb_gears) {
  Production_temp = cbind(Production_temp,sumWequals(SRO$Landing_biomass_gears[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}
Production_temp = cbind(Production_temp,meanWequals(SRO$Landing_length_mean[1:(End + 1)]        , End + 1, INP$Time_slice))

if (nb_gears!=1){
  for (g in 1 : nb_gears) {
  Production_temp = cbind(Production_temp,meanWequals(SRO$Landing_length_mean_gears[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}
Production_temp = cbind(Production_temp,meanWequals(SRO$Landing_age_mean[1:(End + 1)]        , End + 1, INP$Time_slice))
if (nb_gears!=1){
  for (g in 1 : nb_gears) {
  Production_temp = cbind(Production_temp,meanWequals(SRO$Landing_age_mean_gears[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}
#-----
#discard
Production_temp = cbind(Production_temp,sumWequals(SRO$Discard_biomass[1:(End + 1)] , End + 1, INP$Time_slice))
if (nb_gears!=1){
  for (g in 1 : nb_gears) {
  Production_temp = cbind(Production_temp,sumWequals(SRO$Discard_biomass_gears[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}
Production_temp = cbind(Production_temp,meanWequals(SRO$Discard_length_mean[1:(End + 1)]        , End + 1, INP$Time_slice))

if (nb_gears!=1){
  for (g in 1 : nb_gears) {
  Production_temp = cbind(Production_temp,meanWequals(SRO$Discard_length_mean_gears[1:(End + 1),g] , End + 1, INP$Time_slice))

  }
}
Production_temp = cbind(Production_temp,meanWequals(SRO$Discard_age_mean[1:(End + 1)]        , End + 1, INP$Time_slice))
                                                  
land_mat_to_add <- data.frame(matrix("N", nrow=length(c(years, years_forecast )), ncol=length(FLEETSEGMENTS_names)))

if (length(FLEETSEGMENTS_names)!=1 ) {
  for (g in 1 : length(FLEETSEGMENTS_names)) {
  Production_temp = cbind(Production_temp,meanWequals(SRO$Discard_age_mean_gears[1:(End + 1),g] , End + 1, INP$Time_slice))
 
 if (INP$Year_simulation == length(years) ) {
land_obl_f  <-  FleetList_simulation[[g]]@landing.obligation.vector
} else {

 land_obl_f  <- rbind(FleetList_simulation[[g]]@landing.obligation.vector, FleetList_forecast[[g]]@landing.obligation.vector )
}
 
  
#  apply(SRO$MFCatch_gears[,,n], MARGIN=2, FUN="sumWequals",  para_vectLength = (End + 1) , para_pointsMean = INP$Time_slice )

  land_mat_to_add[,g] <- apply(land_obl_f, MARGIN=1, FUN="ifLandingObl")

}
}

#------



Production_temp=data.frame(Production_temp)
	 colnames(Production_temp)  = Production_ALLruns_head[Production_ALLruns_head != "run" & !(Production_ALLruns_head %in% c("LandingObligation", paste("LandingObligation_",FLEETSEGMENTS_names,sep="") ) )  & !(Production_ALLruns_head %in% c("Discard_ratio", paste("Discard_ratio_",FLEETSEGMENTS_names,sep="") ) )]# Production_ALLruns_head[-c(length(Production_ALLruns_head), (length(Production_ALLruns_head)-1))]

   Production_temp$Discard_ratio <- 0
for (ndisc_rat in 1:nrow(Production_temp)) {
   Production_temp$Discard_ratio[ndisc_rat] = ifelse(Production_temp$Total_Discard[ndisc_rat]=="NA",NA,as.numeric(as.character(Production_temp$Total_Discard[ndisc_rat]))/as.numeric(as.character(Production_temp$Total_Yield[ndisc_rat])) )
}


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

Production_temp = data.frame(cbind(Production_temp, land_mat_to_add[1:length(loca_y),]) )

	 colnames(Production_temp) = Production_ALLruns_head[-length(Production_ALLruns_head)]
tables_to_return[[2]] = Production_temp

Production_temp = data.frame(cbind(Production_temp, rep(current_runCI, nrow(Production_temp)))  )
	 colnames(Production_temp) = Production_ALLruns_head

	 if (!INTEGRATED_APPROACH) {
Production_ALLruns <<- data.frame(rbind(Production_ALLruns, Production_temp))
} else {
  colnames(Production_temp)  <- colnames(INP$Production_ALLruns)
  if (current_year == 1) {
INP$Production_ALLruns <- data.frame(rbind(INP$Production_ALLruns, Production_temp))
} else {
INP$Production_ALLruns <- data.frame(rbind(INP$Production_ALLruns, Production_temp[Production_temp$Year == years_forecast[current_year],]))
}
}

#write.table(Production_temp, PRODUCTION_table,row.names=FALSE, sep=";")

if (FALSE) {
# detailed DISCARD TABLE

Discard_temp =  loca_y
Discard_temp = cbind(Discard_temp,sumWequals(SRO$Discard_biomass[1:(End + 1)] , End + 1, INP$Time_slice))
if (nb_gears!=1){
  for (g in 1 : nb_gears) {
  Discard_temp = cbind(Discard_temp,sumWequals(SRO$Discard_biomass_gears[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}

Discard_temp = cbind(Discard_temp,sumWequals(SRO$Discard_biomass_landed[1:(End + 1)] , End + 1, INP$Time_slice))
if (nb_gears!=1){
  for (g in 1 : nb_gears) {
  Discard_temp = cbind(Discard_temp,sumWequals(SRO$Discard_biomass_gears_landed[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}

Discard_temp = cbind(Discard_temp,sumWequals(SRO$Discard_biomass_sea[1:(End + 1)] , End + 1, INP$Time_slice))
if (nb_gears!=1){
  for (g in 1 : nb_gears) {
  Discard_temp = cbind(Discard_temp,sumWequals(SRO$Discard_biomass_gears_sea[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}

Discard_temp = cbind(Discard_temp,meanWequals(SRO$Discard_length_mean[1:(End + 1)] , End + 1, INP$Time_slice))
if (nb_gears!=1){
  for (g in 1 : nb_gears) {
  Discard_temp = cbind(Discard_temp,meanWequals(SRO$Discard_length_mean_gears[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}
Discard_temp = cbind(Discard_temp,meanWequals(SRO$Discard_length_mean_landed[1:(End + 1)] , End + 1, INP$Time_slice))
if (nb_gears!=1){
  for (g in 1 : nb_gears) {
  Discard_temp = cbind(Discard_temp,meanWequals(SRO$Discard_length_mean_gears_landed[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}
Discard_temp = cbind(Discard_temp,meanWequals(SRO$Discard_length_mean_sea[1:(End + 1)] , End + 1, INP$Time_slice))
if (nb_gears!=1){
  for (g in 1 : nb_gears) {
  Discard_temp = cbind(Discard_temp,meanWequals(SRO$Discard_length_mean_gears_sea[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}
Discard_temp = cbind(Discard_temp,meanWequals(SRO$Discard_age_mean[1:(End + 1)] , End + 1, INP$Time_slice))
if (nb_gears!=1){
  for (g in 1 : nb_gears) {
  Discard_temp = cbind(Discard_temp,meanWequals(SRO$Discard_age_mean_gears[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}
Discard_temp = cbind(Discard_temp,meanWequals(SRO$Discard_age_mean_landed[1:(End + 1)] , End + 1, INP$Time_slice))
if (nb_gears!=1){
  for (g in 1 : nb_gears) {
  Discard_temp = cbind(Discard_temp,meanWequals(SRO$Discard_age_mean_gears_landed[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}
Discard_temp= cbind(Discard_temp,meanWequals(SRO$Discard_age_mean_sea[1:(End + 1)] , End + 1, INP$Time_slice))
if (nb_gears!=1){
  for (g in 1 : nb_gears) {
  Discard_temp = cbind(Discard_temp,meanWequals(SRO$Discard_age_mean_gears_sea[1:(End + 1),g] , End + 1, INP$Time_slice))
  }
}
Discard = as.data.frame(Discard_temp)



Discard <- data.frame(cbind(Discard, rep(current_runCI, nrow(Discard)))  )
	 colnames(Discard)  <- Discard_ALLruns_head
 	 if (!INTEGRATED_APPROACH) {
Discard_ALLruns <<- data.frame(rbind(Discard_ALLruns, Discard)) 
} else {
  colnames(Discard)  <- colnames(INP$Discard_ALLruns)
  if (current_year ==1) {
INP$Discard_ALLruns <- data.frame(rbind(INP$Discard_ALLruns, Discard))
} else {
INP$Discard_ALLruns <- data.frame(rbind(INP$Discard_ALLruns, Discard[Discard$Year == years_forecast[current_year],]))
}
}

}
#write.table(Discard, DETAILED_DISCARD_table,row.names=FALSE, sep=";")


Recruitment = data.frame(matrix(0, nrow=length(loca_y), ncol=2))

Recruitment[,1]   <- loca_y
Recruitment[,2] <- meanWequals(INP$Recruits[1:(End + 1)], End + 1, INP$Time_slice)


Recruitment <- data.frame(cbind(Recruitment, rep(current_runCI, nrow(Recruitment)))  )
colnames(Recruitment)  <- Recruitment_ALLruns_head

 	 if (!INTEGRATED_APPROACH) {
Recruitment_ALLruns <<- data.frame(rbind(Recruitment_ALLruns, Recruitment))
} else {
  if (current_year ==1) {
INP$Recruitment_ALLruns <- data.frame(rbind(INP$Recruitment_ALLruns, Recruitment))
} else {
INP$Recruitment_ALLruns <- data.frame(rbind(INP$Recruitment_ALLruns, Recruitment[Recruitment$Year == years_forecast[current_year],]))
}
}

#write.table(Recruitment, RECRUITMENT_table, row.names=FALSE, sep=";" )

return(tables_to_return)
}
                      