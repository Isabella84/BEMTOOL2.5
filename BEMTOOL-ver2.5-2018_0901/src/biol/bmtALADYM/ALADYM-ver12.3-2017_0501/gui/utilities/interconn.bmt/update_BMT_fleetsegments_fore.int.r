# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




# ---------------------- Create and add gear to the list
update_BMT_fleetsegments_fore.int <- function(num_ORD_fleetsegm, num_fleet) {

#associated_fleetsegment <- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ])   
#associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
#associated_fleetsegment_indices <- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

# num_ORD_fleetsegm =  n_ord
# num_fleet =    n_int
  
vesselsMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+1)))
for (yea_f in 1:foreperiod ) {
yea <- yea_f + simperiod
vess <- as.numeric(as.character(Fleetyear[[yea]]@fleetsegments[[num_fleet]]@VESSELS ))
# print(vess)
vesselsMatr <- rbind(vesselsMatr, c(years.forecast[yea_f],vess))  
}
colnames(vesselsMatr) <- c("year",MONTHS)


daysMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+1)))
for (yea_f in 1:foreperiod) {
yea <- yea_f + simperiod
days <- as.numeric(as.character(Fleetyear[[yea]]@fleetsegments[[num_fleet]]@DAYS.average ))
    daysMatr <- rbind(daysMatr, c(years.forecast[yea_f], days))  
}
colnames(daysMatr) <- c("year",MONTHS)


gtMatr <- data.frame(matrix(nrow=0, ncol=(length(MONTHS)+1)))
for (yea_f in 1:foreperiod) {
yea <- yea_f + simperiod
gts <- as.numeric(as.character(Fleetyear[[yea]]@fleetsegments[[num_fleet]]@GT.average ))
    gtMatr <- rbind(gtMatr, c(years.forecast[yea_f], gts))  
}
colnames(gtMatr) <- c("year",	MONTHS)

if (!INTEGRATED_APPROACH | (INTEGRATED_APPROACH & current_year == 1 )) {
fisheffMatr <- data.frame(matrix(NA, nrow=foreperiod, ncol=(length(MONTHS)+1)))
colnames(fisheffMatr) <- c("year",	MONTHS)
fisheffMatr$year <- years_forecast 


#discMatr <- data.frame(matrix(nrow=0, ncol=4))
#for (yea_f in 1:foreperiod) {
#discs <- rep(0, 12) 
#to_add <- data.frame(cbind(rep(years.forecast[yea_f], 12), cbind(MONTHS, cbind(discs,discs))) )
#colnames(to_add) <- c("year" ,"month", "L50", "L75_L25")
 #   discMatr <- rbind(discMatr, to_add) 
#}
#discMatr[,3] <- as.numeric(as.character(discMatr[,3])) 
#discMatr[,4] <- as.numeric(as.character(discMatr[,4])) 


n_ages_males <- as.numeric(as.character(Populations[[ALADYM_spe]]@lifespan[1,1]))
Fmatrix_M <- data.frame(matrix(0, nrow=length(years.forecast), ncol=n_ages_males+1)) 
colnames(Fmatrix_M) <- c("year", paste("age", c(0:(n_ages_males-1)), sep=""))
Fmatrix_M[,1] <- years.forecast


n_ages_females <- as.numeric(as.character(Populations[[ALADYM_spe]]@lifespan[2,1]))
Fmatrix_F <- data.frame(matrix(0, nrow=length(years.forecast), ncol=n_ages_females+1)) 
colnames(Fmatrix_F) <- c("year", paste("age", c(0:(n_ages_females-1)), sep=""))
Fmatrix_F[,1] <- years.forecast



print(".......................................... [update_BMT_fleetsegments_fore.r]", quote=F)     

#if (ALADYM_GUI_simulations[[ALADYM_spe]]@enteringMortality != "F") {
## FleetList_forecast[[num_ORD_fleetsegm]]@selectivity.vector <<- selectivity_table 
#}  
FleetList_forecast[[num_ORD_fleetsegm]]@production.vector <<- data.frame()   
FleetList_forecast[[num_ORD_fleetsegm]]@vessels.vector <<- vesselsMatr
FleetList_forecast[[num_ORD_fleetsegm]]@days.vector <<- daysMatr
FleetList_forecast[[num_ORD_fleetsegm]]@gt.vector <<- gtMatr

    FleetList_forecast[[num_ORD_fleetsegm]]@fishingeffort.vector <<- fisheffMatr
  #else {
#   fisheffMatr <- FleetList_forecast[[num_ORD_fleetsegm]]@fishingeffort.vector
#}

#     
#FleetList_forecast[[num_ORD_fleetsegm]]@discard.vector <<- discMatr  
FleetList_forecast[[num_ORD_fleetsegm]]@fishingmortality.M.vector <<- Fmatrix_M
FleetList_forecast[[num_ORD_fleetsegm]]@fishingmortality.F.vector <<- Fmatrix_F

}

if (BMT_SCENARIO == BMT_HR_CHANGE_FISHMORTALITY)  {
FleetList_forecast[[num_ORD_fleetsegm]]@scenario.reduction <<-  as.numeric(as.character(cfg[rownames(cfg) == "casestudy.HR3",2])) #INP$Forecast_reduction[num_ORD_fleetsegm]
   }

   effF <- data.frame(matrix("", nrow=1, ncol=3), stringsAsFactors=F)
   colnames(effF) <- c("relationship_type", "a", "b")

   effF$relationship_type[1] <- as.character(cfg[rownames(cfg) == "casestudy.relatioship.effort-F.type",num_fleet]) #INP$Forecast_reduction[num_ORD_fleetsegm]
      effF$a[1] <- as.numeric(as.character(cfg[rownames(cfg) == "casestudy.relatioship.effort-F.a",num_fleet]) )
         effF$b[1] <- as.numeric(as.character(cfg[rownames(cfg) == "casestudy.relatioship.effort-F.b",num_fleet]) )

FleetList_forecast[[num_ORD_fleetsegm]]@EffortF.relationship <<-  effF #INP$Forecast_reduction[num_ORD_fleetsegm]


 if (BMT_SCENARIO != BMT_HR_CHANGE_FISHMORTALITY)  {  
########################################################################################################################################## 
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
##########################################################################################################################################


   effF <- data.frame(matrix("", nrow=length(BMT_FLEETSEGMENTS), ncol=3), stringsAsFactors=F)
   colnames(effF) <- c("relationship_type", "a", "b")

   for (g in 1:length(BMT_FLEETSEGMENTS)) {
   effF$relationship_type[g] <- as.character(cfg[rownames(cfg) == "casestudy.relatioship.effort-F.type",g]) #INP$Forecast_reduction[num_ORD_fleetsegm]
      effF$a[g] <- as.numeric(as.character(cfg[rownames(cfg) == "casestudy.relatioship.effort-F.a",g]) )
        effF$b[g] <- as.numeric(as.character(cfg[rownames(cfg) == "casestudy.relatioship.effort-F.b",g]) )
    }     

forecast <-  simperiod * 12 + (current_year-1)*12 +1
GLO$L_number <- simperiod * 12 + current_year*12  

if (!INTEGRATED_APPROACH | (INTEGRATED_APPROACH & current_year == 1)) {
fishing_eff_result <- fact_calc.gui(eff_data_all,"Y",all_years, forecast, effF)
}  else {
fishing_eff_result <- fact_calc(eff_data_all,"Y")
}

#print(fishing_eff_result)               

if (INTEGRATED_APPROACH) {
  all_years <-  all_years[1:(simperiod + current_year)]
} else {
fisheffMatr <- data.frame(matrix(1, nrow=length(years.forecast), ncol=(length(MONTHS)+1))) 
colnames(fisheffMatr) <- c("year",	MONTHS)
}         
#eff_data <- get_effort_data_fore()
#
#fishing_eff_result <- fact_calc.gui(eff_data,"Y",all_years, forecast)

if (length(associated_fleetsegment_indices)==1) {
fishing_eff_result <- data.frame(matrix(fishing_eff_result[(forecast+1):((simperiod+current_year)*12+1), ], ncol=1 ))
} else {
fishing_eff_result <- fishing_eff_result[(forecast+1):((simperiod+current_year)*12+1), ]
}

if (!INTEGRATED_APPROACH | (INTEGRATED_APPROACH & current_year ==1 ) ) {

if (INTEGRATED_APPROACH ) {
    fisheffMatr[current_year,] <-  c(years.forecast[current_year], fishing_eff_result[ , num_ORD_fleetsegm]) # rbind(fisheffMatr, c(years.foreca 
} else {
  for (yea in c(1:foreperiod) ) {
    fes <-  fishing_eff_result[ ( ((yea-1)*12)+1) : ( (yea)*12 ) , num_ORD_fleetsegm]
    fisheffMatr[yea,] <-  c(years.forecast[yea], fes) # rbind(fisheffMatr, c(years.forecast[yea], fes))      FleetList_forecast[[lastAdded]]@fishingeffort.vector[yea,mon]
}
}
     FleetList_forecast[[num_ORD_fleetsegm]]@fishingeffort.vector <<- fisheffMatr
}

}





   
# return(FleetList_forecast)
}
