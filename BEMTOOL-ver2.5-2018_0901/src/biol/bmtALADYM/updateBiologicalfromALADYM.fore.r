# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

updateBiologicalfromALADYM.fore<- function(ALADYM_spe, Populations, Interactionsyear, Fleetyear) {

n_ages_mal <- INP$MGrowth_tend
n_ages_fem <- INP$FGrowth_tend

first_age <- 0
n_ages_mal <- n_ages_mal - trunc(INP$tr/12)
n_ages_fem <- n_ages_fem - trunc(INP$tr/12)
first_age <- trunc(INP$tr/12)

  n_ages <- max(as.numeric(new_aldPopulation@lifespan[1,1]) , as.numeric(new_aldPopulation@lifespan[2,1]) )     
  n_ages <- n_ages - trunc(INP$tr/12)
  vector_ages <- c(first_age:(n_ages+first_age-1))

associated_fleetsegment <- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment != ""]
associated_fleetsegment_indices <- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)
n_fleet_for_species <- length(associated_fleetsegment_indices)

if (INTEGRATED_APPROACH) { 
   bmt_years_forecast <- TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR - simperiod
} else {
   bmt_years_forecast <- foreperiod
}
print(paste("Updating BMT objects from ALADYM forecast... [", years.forecast[bmt_years_forecast], "]", sep =""), quote=F)
# print(paste("Updating Biological data from ALADYM after the FORECAST for species [", BMT_SPECIES[ALADYM_spe], "]", sep=""), quote=FALSE )

# ----------------------------------------------------------------------------
# ASSIGN BMT VALUES TO THE POPULATION in aladym
# ----------------------------------------------------------------------------

      lw_a_M <- as.numeric(Populations[[ALADYM_spe]]@lengthweight[1,1])
      lw_b_M <- as.numeric(Populations[[ALADYM_spe]]@lengthweight[1,2])
      lw_a_F <- as.numeric(Populations[[ALADYM_spe]]@lengthweight[2,1])
      lw_b_F <- as.numeric(Populations[[ALADYM_spe]]@lengthweight[2,2])
      lw_a <- mean(c(lw_a_M, lw_a_F))
      lw_b <- mean(c(lw_b_M, lw_b_F))
# fertility rate
Populations[[ALADYM_spe]]@offspring.prop[1,]  <- c(INP$Fertility_Rate)

# ----------------------------------------------------------------------------
# ASSIGN BMT VALUES TO THE CATCHES (Landings, Discards and Catches) in aladym
# ----------------------------------------------------------------------------

# catch, landing and discard by age 

if (!RUN_CI_FORE) {

source(paste(ALADYM_home, "/src/paths.r", sep=""))

catch_by_age <- read.csv(CATCHBYAGE_table, sep=";")
landing_by_age <- read.csv(LANDINGBYAGE_table, sep=";")
discard_by_age <- read.csv(DISCARDBYAGE_table, sep=";")

catch_by_age <- catch_by_age[catch_by_age[,1] %in% c(years, years.forecast[1:bmt_years_forecast]),]
landing_by_age <- landing_by_age[landing_by_age[,1]  %in% c(years, years.forecast[1:bmt_years_forecast]),]
discard_by_age <- discard_by_age[discard_by_age[,1]  %in% c(years, years.forecast[1:bmt_years_forecast]),]

# catch_by_age_F <- catch_by_age[1:((bmt_years_forecast+simperiod)*n_fleet_for_species),]      # as.character(catch_by_age[,(ncol(catch_by_age)-1)] ) == "F"
catch_by_age_F <- catch_by_age[as.character(catch_by_age[,(ncol(catch_by_age)-1)] ) == "F",]      
catch_by_age_M <- catch_by_age[as.character(catch_by_age[,(ncol(catch_by_age)-1)] ) == "M",]  #catch_by_age[( ((bmt_years_forecast+simperiod)*n_fleet_for_species) +1):nrow(catch_by_age),] 
landing_by_age_F <- landing_by_age[as.character(landing_by_age[,(ncol(landing_by_age)-1)] ) == "F",]   #landing_by_age[1:((bmt_years_forecast+simperiod)*n_fleet_for_species),] 
landing_by_age_M <- landing_by_age[as.character(landing_by_age[,(ncol(landing_by_age)-1)] ) == "M",]  #landing_by_age[( ((bmt_years_forecast+simperiod)*n_fleet_for_species) +1):nrow(landing_by_age),] 
discard_by_age_F <- discard_by_age[as.character(discard_by_age[,(ncol(discard_by_age)-1)] ) == "F",]   #discard_by_age[1:((bmt_years_forecast+simperiod)*n_fleet_for_species),] 
discard_by_age_M <- discard_by_age[as.character(discard_by_age[,(ncol(discard_by_age)-1)] ) == "M",]  #discard_by_age[( ((bmt_years_forecast+simperiod)*n_fleet_for_species) +1):nrow(discard_by_age),] 

catch_total <- cbind(catch_by_age_F[,1], cbind(catch_by_age_F[,2:(length(vector_ages)+1)]  +  catch_by_age_M[,2:(length(vector_ages)+1)], as.character(catch_by_age_F[,ncol(catch_by_age_F)])))
landing_total <- cbind(landing_by_age_F[,1], cbind(landing_by_age_F[,2:(length(vector_ages)+1)]  +  landing_by_age_M[,2:(length(vector_ages)+1)], landing_by_age_F[,ncol(landing_by_age_F)]))
discard_total <- cbind(discard_by_age_F[,1], cbind(discard_by_age_F[,2:(length(vector_ages)+1)]  +  discard_by_age_M[,2:(length(vector_ages)+1)], discard_by_age_F[,ncol(discard_by_age_F)]))

colnames(catch_total) <- c(colnames(catch_by_age_F)[1:(ncol(catch_by_age_F)-2)], colnames(catch_by_age_F)[ncol(catch_by_age_F)])
colnames(landing_total) <- c(colnames(landing_by_age_F)[1:(ncol(landing_by_age_F)-2)], colnames(landing_by_age_F)[ncol(landing_by_age_F)])
colnames(discard_total) <- c(colnames(discard_by_age_F)[1:(ncol(discard_by_age_F)-2)], colnames(discard_by_age_F)[ncol(discard_by_age_F)])

  for (yy_f in 1:bmt_years_forecast) {
  yy <- simperiod+yy_f

      TOTAL_LANDING_numbers <-  data.frame(matrix(0, nrow=1, ncol=length(vector_ages)))
      rownames(TOTAL_LANDING_numbers) <- years.forecast[yy_f]
      colnames(TOTAL_LANDING_numbers) <- c(paste("age", vector_ages, sep=""))  
     
      TOTAL_CATCH_numbers <- data.frame(matrix(0, nrow=1, ncol=length(vector_ages) ))
      rownames(TOTAL_CATCH_numbers) <-  years.forecast[yy_f]
      colnames(TOTAL_CATCH_numbers) <- c(paste("age", vector_ages, sep=""))  
  
        TOTAL_DISCARD_numbers  <- data.frame(matrix(0, nrow=1, ncol=length(vector_ages)))
      rownames(TOTAL_DISCARD_numbers) <-   years.forecast[yy_f]
      colnames(TOTAL_DISCARD_numbers) <- c(paste("age", vector_ages, sep=""))  
     
      TOTAL_DISCARD_numbers_NA <- data.frame(matrix(NA, nrow=1, ncol=length(vector_ages)))
      rownames(TOTAL_DISCARD_numbers_NA) <-   years.forecast[yy_f]
      colnames(TOTAL_DISCARD_numbers_NA) <- c(paste("age",vector_ages, sep=""))  
  
   fleet_interaction_ord <- 1
for (n_fl in 1:length(BMT_FLEETSEGMENTS)) {

if (n_fl %in% associated_fleetsegment_indices)  {
  
 # catch_fleet <- catch_total[ (1+(fleet_interaction_ord-1)*(bmt_years_forecast+simperiod)) : (fleet_interaction_ord*(bmt_years_forecast+simperiod)),]
#    landing_fleet <- landing_total[ (1+(fleet_interaction_ord-1)*(bmt_years_forecast+simperiod)) : (fleet_interaction_ord*(bmt_years_forecast+simperiod)),]
#      discard_fleet <- discard_total[ (1+(fleet_interaction_ord-1)*(bmt_years_forecast+simperiod)) : (fleet_interaction_ord*(bmt_years_forecast+simperiod)),]

   catch_fleet <- catch_total[ catch_total[,ncol(catch_total)] == BMT_FLEETSEGMENTS[n_fl], ]
    landing_fleet <- landing_total[ landing_total[,ncol(landing_total)] == BMT_FLEETSEGMENTS[n_fl], ]
      discard_fleet <- discard_total[ discard_total[,ncol(discard_total)] == BMT_FLEETSEGMENTS[n_fl], ]    
     
       numbers_df <- data.frame(matrix(0, nrow=1, ncol=length(vector_ages) ))
        rownames(numbers_df) <- years.forecast[yy_f]
        colnames(numbers_df) <- c(paste("age", vector_ages, sep=""))  
        numbers_df[1,] <- catch_fleet[yy,2:(ncol(catch_fleet)-1)]   
        Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@numbers <- numbers_df 
        
        TOTAL_CATCH_numbers <- TOTAL_CATCH_numbers + numbers_df
        numbers_df[1,] <- landing_fleet[yy,2:(ncol(landing_fleet)-1)]   
        Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@numbers <- numbers_df 
        TOTAL_LANDING_numbers <- TOTAL_LANDING_numbers + numbers_df
        
		numbers_df[1,] <- discard_fleet[yy,2:(ncol(discard_fleet)-1)]
        if (is.na(INP$Discard[1,fleet_interaction_ord])) {
        Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@numbers <- TOTAL_DISCARD_numbers_NA 
         } else {
        Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@numbers <- numbers_df 
        TOTAL_DISCARD_numbers <- TOTAL_DISCARD_numbers + numbers_df
        } 

     fleet_interaction_ord <- fleet_interaction_ord+1 
}

}

Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@numbers <- TOTAL_CATCH_numbers 
Interactionsyear[[yy]][[ALADYM_spe]]@totallanding@numbers <- TOTAL_LANDING_numbers 
Interactionsyear[[yy]][[ALADYM_spe]]@totaldiscard@numbers <- TOTAL_DISCARD_numbers 


} # end years loop

}

source(paste(ALADYM_home, "/src/paths.r", sep=""))

# total weigth of catches, landing and discard 
production_tbl <- read.csv(PRODUCTION_table, sep=";")
mortalities_tbl <- read.csv(MORTALITY_table, sep=";")

production_tbl <- production_tbl[production_tbl$Year %in% c(years, years.forecast[1:bmt_years_forecast]),]
mortalities_tbl <- mortalities_tbl[mortalities_tbl$Year %in% c(years, years.forecast[1:bmt_years_forecast]),]

if (n_fleet_for_species > 1) {
catch_weight <- production_tbl[,4:(4 + n_fleet_for_species)]
catch_mean_length <- production_tbl[,(4 + n_fleet_for_species + 1):(4 + 2* n_fleet_for_species + 1)]
catch_mean_age <- production_tbl[,(4 + 2 * n_fleet_for_species + 2):(4 + 3* n_fleet_for_species + 2)]
landing_weight <- production_tbl[,(4 + 3 * n_fleet_for_species + 3):(4 + 4* n_fleet_for_species + 3)]
landing_mean_length <- production_tbl[,(4 + 4 * n_fleet_for_species + 4):(4 + 5* n_fleet_for_species + 4)]
landing_mean_age <- production_tbl[,(4 + 5 * n_fleet_for_species + 5):(4 + 6* n_fleet_for_species + 5)]
discard_weight <- production_tbl[,(4 + 6 * n_fleet_for_species + 6):(4 + 7* n_fleet_for_species + 6)]
discard_mean_length <- production_tbl[,(4 + 7 * n_fleet_for_species + 7):(4 + 8* n_fleet_for_species + 7)]
discard_mean_age <- production_tbl[,(4 + 8 * n_fleet_for_species + 8):(4 + 9* n_fleet_for_species + 8)]

} else {
catch_weight <- data.frame(cbind(production_tbl[,4], production_tbl[,4]))
colnames(catch_weight) <- c(colnames(production_tbl)[4], "X")
catch_mean_length <- data.frame(cbind(production_tbl[,5] , production_tbl[,5]))
colnames(catch_mean_length) <- c(colnames(production_tbl)[5], "X")
catch_mean_age <- data.frame(cbind(production_tbl[,6] , production_tbl[,6]))
colnames(catch_mean_age) <- c(colnames(production_tbl)[6],"X")
landing_weight <- data.frame(cbind(production_tbl[,7] , production_tbl[,7]))
colnames(landing_weight) <- c(colnames(production_tbl)[7], "X")
landing_mean_length <- data.frame(cbind(production_tbl[,8] , production_tbl[,8]))
colnames(landing_mean_length) <- c(colnames(production_tbl)[8], "X")
landing_mean_age <- data.frame(cbind(production_tbl[,9] , production_tbl[,9]))
colnames(landing_mean_age) <- c(colnames(production_tbl)[9], "X")
discard_weight <- data.frame(cbind(production_tbl[,10], production_tbl[,10]))
colnames(discard_weight) <- c(colnames(production_tbl)[10], "X")
discard_mean_length <- data.frame(cbind(production_tbl[,11] , production_tbl[,11]))
colnames(discard_mean_length) <- c(colnames(production_tbl)[11], "X")
discard_mean_age <- data.frame(cbind(production_tbl[,12] , production_tbl[,12]))
colnames(discard_mean_age) <- c(colnames(production_tbl)[12], "X")

}



    for (yy_f in 1:bmt_years_forecast) {
    yy <- yy_f + simperiod  

    Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@totalweight <- catch_weight$Total_Yield[yy]
    Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@meanLength <- catch_mean_length$Mean_length_in_catch[yy]
    Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@meanWeight <- lw_a * as.numeric(catch_mean_length$Mean_length_in_catch[yy])^ lw_b
  
   if (n_fleet_for_species > 1) {
     Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@fishing_mortality <-  mortalities_tbl[yy,(14+(length(associated_fleetsegment_indices)*3))]
 } else {
  Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@fishing_mortality <-  mortalities_tbl[yy,ncol(mortalities_tbl)]
 }
    
    Interactionsyear[[yy]][[ALADYM_spe]]@totallanding@totalweight <- landing_weight$Total_Landing[yy]
    Interactionsyear[[yy]][[ALADYM_spe]]@totallanding@meanLength <- landing_mean_length$Mean_length_in_Landing[yy]
    Interactionsyear[[yy]][[ALADYM_spe]]@totallanding@meanWeight <- lw_a * as.numeric(landing_mean_length$Mean_length_in_Landing[yy])^ lw_b
  
#	 if (sum(Interactionsyear[[yy]][[ALADYM_spe]]@totaldiscard@numbers, na.rm=T) == 0 ) {
#      Interactionsyear[[yy]][[ALADYM_spe]]@totaldiscard@totalweight <- 0
#    } else {
      Interactionsyear[[yy]][[ALADYM_spe]]@totaldiscard@totalweight <- ifelse(is.na(discard_weight$Total_Discard[yy]), -1, discard_weight$Total_Discard[yy])
#    }
    Interactionsyear[[yy]][[ALADYM_spe]]@totaldiscard@meanLength <- ifelse(is.na(discard_mean_length$Mean_length_in_Discard[yy]), -1, discard_mean_length$Mean_length_in_Discard[yy])
    Interactionsyear[[yy]][[ALADYM_spe]]@totaldiscard@meanWeight <- ifelse(is.na(discard_mean_length$Mean_length_in_Discard[yy]), -1, (lw_a * as.numeric(discard_mean_length$Mean_length_in_Discard[yy])^ lw_b))
    


	
   fleet_interaction_ord <- 1
for (n_fl in 1:length(BMT_FLEETSEGMENTS)) {
if (n_fl %in% associated_fleetsegment_indices)  {

        Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@totalweight <- as.numeric(as.character(catch_weight[yy,fleet_interaction_ord+1]) )
        Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@meanLength <- as.numeric(catch_mean_length[yy,fleet_interaction_ord+1] )
        Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@meanWeight <-  lw_a * as.numeric(catch_mean_length[yy,fleet_interaction_ord+1])^ lw_b
        
       if (n_fleet_for_species > 1) { 
           Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@fishing_mortality <-  mortalities_tbl[yy,(14+(length(associated_fleetsegment_indices)*3)+fleet_interaction_ord)] 
 		} else {
            Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@fishing_mortality <-  mortalities_tbl[yy,ncol(mortalities_tbl)]  
         }
        
        Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@totalweight <- as.numeric(landing_weight[yy,fleet_interaction_ord+1] )
        Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@meanLength <- as.numeric(landing_mean_length[yy,fleet_interaction_ord+1])
        Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@meanWeight <-  lw_a * as.numeric(landing_mean_length[yy,fleet_interaction_ord+1])^ lw_b
                
    Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@totalweight <- ifelse(is.na(discard_weight[yy,fleet_interaction_ord+1]), -1, as.numeric(discard_weight[yy,fleet_interaction_ord+1])) 
    Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@meanLength <- ifelse(is.na(discard_mean_length[yy,fleet_interaction_ord+1]), -1, as.numeric(discard_mean_length[yy,fleet_interaction_ord+1]))     
    Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@meanWeight <- ifelse(is.na(discard_mean_length[yy,fleet_interaction_ord+1]), -1, lw_a * as.numeric(discard_mean_length[yy,fleet_interaction_ord+1])^ lw_b )
  
  if (fleet_interaction_ord == 1) {
        # weight, mean length and mean weight related to all the catches (not by fleet segment)
        Interactionsyear[[yy]][[ALADYM_spe]]@meanLength_catches <- as.numeric(catch_mean_length[yy,1])
        Interactionsyear[[yy]][[ALADYM_spe]]@meanWeight_catches <-  lw_a * as.numeric(catch_mean_length[yy,1]) ^ lw_b
  }
        # inserire f e z nello slot
        if (n_fleet_for_species > 1) { 
  	  Interactionsyear[[yy]][[ALADYM_spe]]@mortalities$F[n_fl] <- mortalities_tbl[yy,(14+(length(associated_fleetsegment_indices)*3)+fleet_interaction_ord)]
 		} else {
    		 Interactionsyear[[yy]][[ALADYM_spe]]@mortalities$F[n_fl] <- mortalities_tbl[yy,ncol(mortalities_tbl)]    
         }

fleet_interaction_ord <- fleet_interaction_ord+1
  }

}
} # ens years loop


   for (yy_f in 1:bmt_years_forecast) {
    yy <- yy_f + simperiod  
 # print(paste("anno: ", yy, "F:", mortalities_tbl[yy,(14+(length(associated_fleetsegment_indices)*3))]))

if (n_fleet_for_species > 1) {
 Interactionsyear[[yy]][[ALADYM_spe]]@mortalities$F[length(BMT_FLEETSEGMENTS)+1] <- mortalities_tbl[yy,(14+(length(associated_fleetsegment_indices)*3))]
 } else {
  Interactionsyear[[yy]][[ALADYM_spe]]@mortalities$F[length(BMT_FLEETSEGMENTS)+1] <- mortalities_tbl[yy,ncol(mortalities_tbl)]
 }

 Interactionsyear[[yy]][[ALADYM_spe]]@mortalities$Z[length(BMT_FLEETSEGMENTS)+1] <-  mortalities_tbl[yy,8]
 }  

    # Interactionsyear[[65]][[1]]@mortalities$F


# ----------------------------------------------------------------------------
# ASSIGN BMT VALUES TO THE STOCKs in aladym
# ----------------------------------------------------------------------------

# L95 for stock

source(paste(ALADYM_home, "/src/paths.r", sep=""))

population_tbl <- read.csv(POPULATION_table, sep=";")
population_tbl <- population_tbl[population_tbl$Year %in% c(years, years.forecast[1:bmt_years_forecast]),]

if_integrated_do_last_year <- TRUE

#if (INTEGRATED_APPROACH & current_year != foreperiod ) {
#	if_integrated_do_last_year <- FALSE
#}

if (if_integrated_do_last_year) { 

source(paste(ALADYM_home, "/src/paths.r", sep=""))

indicators_tbl <- read.csv(INDICATORS_table, sep=";")
indicators_tbl <- indicators_tbl[indicators_tbl$Years %in% c(years, years.forecast[1:bmt_years_forecast]),]

for (yy_f in 1:bmt_years_forecast) {
yy <- yy_f + simperiod
     # fleet stock interaction
     Interactionsyear[[yy]][[ALADYM_spe]]@L95_catches <-  indicators_tbl$catch_L0_95[yy]
     # exploited stock
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@L95 <- indicators_tbl$ex_stock_L0_95[yy] #ex_stock_L0_95

     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@criticalLength <- indicators_tbl$Critical_length_expl_pop[yy]
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@meanLength <- population_tbl$Mean_length_of_exploited_pop[yy]
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@harvestRate <- indicators_tbl$Harvest_ratio[yy] 
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@exploitationRate <- indicators_tbl$Exploitation_rate[yy]
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@meanWeight <- lw_a * Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@meanLength  ^ lw_b
    
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@annual.numbers <- population_tbl$NUMBERS_exploited_pop[yy]*1000
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@annual.SB <- population_tbl$Total_biomass_exploited_pop[yy]
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@annual.SS.numbers <- population_tbl$SS_NUMBERS_exploited_pop[yy]*1000
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@annual.SSB <- population_tbl$SSB_exploited_pop[yy]
     # unexploited stock
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@L95 <- indicators_tbl$unex_stock_L0_95[yy]
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@criticalLength <- indicators_tbl$Critical_length_unexpl_pop[yy]
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@meanLength <- population_tbl$Mean_length_of_unexploited_pop[yy]
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@harvestRate <- 0
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@exploitationRate <- 0
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@meanWeight <-  lw_a * Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@meanLength  ^ lw_b
     
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@annual.numbers <- population_tbl$NUMBERS_unexploited_pop[yy]*1000
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SB <- population_tbl$Total_biomass_unexploited_pop[yy]
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SS.numbers <- population_tbl$SS_NUMBERS_unexploited_pop[yy]*1000
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SSB <- population_tbl$SSB_unexploited_pop[yy]

}

}
       # Interactionsyear[[6]][[1]]@exploitedStock 

ex_stocknumbers_df_M <- data.frame(matrix(0, nrow=length(vector_ages), ncol=length(MONTHS)))
rownames(ex_stocknumbers_df_M) <- c(paste("age", vector_ages, sep=""))
colnames(ex_stocknumbers_df_M) <- MONTHS
ex_stocknumbers_df_F <- data.frame(matrix(0, nrow=length(vector_ages), ncol=length(MONTHS)))
rownames(ex_stocknumbers_df_F) <- c(paste("age", vector_ages, sep=""))
colnames(ex_stocknumbers_df_F) <- MONTHS
unex_stocknumbers_df_M <- data.frame(matrix(0, nrow=length(vector_ages), ncol=length(MONTHS)))
rownames(unex_stocknumbers_df_M) <- c(paste("age", vector_ages, sep=""))
colnames(unex_stocknumbers_df_M) <- MONTHS
unex_stocknumbers_df_F <- data.frame(matrix(0, nrow=length(vector_ages), ncol=length(MONTHS)))
rownames(unex_stocknumbers_df_F) <- c(paste("age", vector_ages, sep=""))
colnames(unex_stocknumbers_df_F) <- MONTHS

ex_stockSB_df_M <- data.frame(matrix(0, nrow=length(vector_ages), ncol=length(MONTHS)))
rownames(ex_stockSB_df_M) <- c(paste("age", vector_ages, sep=""))
colnames(ex_stockSB_df_M) <- MONTHS
ex_stockSB_df_F <- data.frame(matrix(0, nrow=length(vector_ages), ncol=length(MONTHS)))
rownames(ex_stockSB_df_F) <- c(paste("age", vector_ages, sep=""))
colnames(ex_stockSB_df_F) <- MONTHS
unex_stockSB_df_M <- data.frame(matrix(0, nrow=length(vector_ages), ncol=length(MONTHS)))
rownames(unex_stockSB_df_M) <- c(paste("age", vector_ages, sep=""))
colnames(unex_stockSB_df_M) <- MONTHS
unex_stockSB_df_F <- data.frame(matrix(0, nrow=length(vector_ages), ncol=length(MONTHS)))
rownames(unex_stockSB_df_F) <- c(paste("age", vector_ages, sep=""))
colnames(unex_stockSB_df_F) <- MONTHS

ex_SSB_df <- data.frame(matrix(0, nrow=length(vector_ages), ncol=length(MONTHS)))
rownames(ex_SSB_df) <- c(paste("age", vector_ages, sep=""))
colnames(ex_SSB_df) <- MONTHS
unex_SSB_df <- data.frame(matrix(0, nrow=length(vector_ages), ncol=length(MONTHS)))
rownames(unex_SSB_df) <- c(paste("age", vector_ages, sep=""))
colnames(unex_SSB_df) <- MONTHS

# total spawners males + female for exploited
ex_SS_numbers <- data.frame(matrix((BAS$MFSS_Number + BAS$FFSS_Number), nrow=1))
# total spawners males + female for unexploited
unex_SS_numbers <- data.frame(matrix((BAS$MUSS_Number + BAS$FUSS_Number), nrow=1))

#n_ages_mal <- INP$MGrowth_tend
#n_ages_fem <- INP$FGrowth_tend
#
#first_age <- 0
#n_ages_mal <- n_ages_mal - trunc(INP$tr/12)
#n_ages_fem <- n_ages_fem - trunc(INP$tr/12)
#first_age <- trunc(INP$tr/12)

# exploited stock numbers
for (ye_f in 1:bmt_years_forecast) {
ye <- ye_f + simperiod

#print(paste("---------------------------------------------------- anno", years[ye]))
start_mon_simu <- (ye-1)*12+1 +1
end_mon_simu <- (ye*12) + 1

ind_month <- 1   
for (simu_mon in c(start_mon_simu:end_mon_simu)) {
#print(paste("Mese della simulazione", simu_mon))  
#print("MASCHI")

if (modulo(INP$tr,12) !=0 | INP$tr==0) {

for (ag in 1:INP$MGrowth_tend) {

  if ((ag-1) < first_age ) {
         ex_stocknumbers_df_M[ag,ind_month] <-  0
         unex_stocknumbers_df_M[ag,ind_month] <- 0
         ex_stockSB_df_M[ag,ind_month] <- 0
         unex_stockSB_df_M[ag,ind_month] <- 0

  # print(paste("eta #", ag,"tutti NA"))
      } else {
      if ( (ag*12-12+  modulo(INP$tr,12)) == INP$tr) {   # prima classe non nulla              
           start_mon_age <-  1 
           if (INP$tr == 0) {
           end_mon_age <- 12 - modulo(INP$tr,12) #(ag*12) 
           } else {
            end_mon_age <- 12 - modulo(INP$tr,12) + 1#(ag*12) 
           }
         #  print(paste("eta #", ag," start=", start_mon_age, " end=", end_mon_age))
           # mort_vect_this_age <-  c(rep(NA, ifelse(INP$tr == 0, modulo(INP$tr,12), (modulo(INP$tr,12)-1))) , BAS$MM[start_mon_age:end_mon_age])
        } else {
           start_mon_age <- which(as.numeric(as.character(BAS$MAge))==(as.numeric(as.character(ag))-1))+1 #(ag-1)*12+1 
           end_mon_age <- which(as.numeric(as.character(BAS$MAge))==as.numeric(as.character(ag))) #(ag*12)
          # print(paste("eta #", ag," start=", start_mon_age, " end=", end_mon_age)) 
           # mort_vect_this_age <- BAS$MM[start_mon_age:end_mon_age]
        } 
#
#      if (INP$tr==0) {
#         mort_vect_this_age <- mort_vect_this_age[(length(mort_vect_this_age)-12+1):length(mort_vect_this_age)]
#      }


# intervenire...........
# in entrambi gli script devo leggere dalle tabelle di ALADYM e nn dagli ambienti.... mettere il valore annuale uguale a tutti i mesi !!


         ex_stocknumbers_df_M[ag,ind_month] <-  sum(SRO$MFPopulation[simu_mon, c( start_mon_age:end_mon_age) ] )
         unex_stocknumbers_df_M[ag,ind_month] <- sum(SRO$MUPopulation[simu_mon,c( start_mon_age:end_mon_age) ] )
         ex_stockSB_df_M[ag,ind_month] <- sum(SRO$MFPopulation[simu_mon, c( start_mon_age:end_mon_age) ] *  BAS$MWeight[ c( start_mon_age:end_mon_age) ] ) / 1000000
         unex_stockSB_df_M[ag,ind_month] <- sum(SRO$MUPopulation[simu_mon, c( start_mon_age:end_mon_age) ] *  BAS$MWeight[ c( start_mon_age:end_mon_age) ] )  / 1000000

      }
} 

} else {


for (ag in 1:INP$MGrowth_tend) {
  if (ag==trunc(INP$tr/12) ) {   
           start_mon_age <- 1
           end_mon_age <- which(as.numeric(as.character(BAS$MAge))==as.numeric(as.character(ag))) #(ag*12)
         #  print(paste("eta #", ag," start=", start_mon_age, " end=", end_mon_age)) 
          # mort_vect_this_age <- BAS$FM[start_mon_age:end_mon_age]

            ex_stocknumbers_df_M[ag,ind_month] <-  sum(SRO$MFPopulation[simu_mon, c( start_mon_age:end_mon_age) ] )
         unex_stocknumbers_df_M[ag,ind_month] <- sum(SRO$MUPopulation[simu_mon,c( start_mon_age:end_mon_age) ] )
         ex_stockSB_df_M[ag,ind_month] <- sum(SRO$MFPopulation[simu_mon, c( start_mon_age:end_mon_age) ] *  BAS$MWeight[ c( start_mon_age:end_mon_age) ] ) / 1000000
         unex_stockSB_df_M[ag,ind_month] <- sum(SRO$MUPopulation[simu_mon, c( start_mon_age:end_mon_age) ] *  BAS$MWeight[ c( start_mon_age:end_mon_age) ] )  / 1000000

      } else {
           start_mon_age <- which(as.numeric(as.character(BAS$MAge))==(as.numeric(as.character(ag))-1))+1 #(ag-1)*12+1 
           end_mon_age <- which(as.numeric(as.character(BAS$MAge))==as.numeric(as.character(ag))) #(ag*12)
         #  print(paste("eta #", ag," start=", start_mon_age, " end=", end_mon_age)) 
          # mort_vect_this_age <- BAS$FM[start_mon_age:end_mon_age]
       #   Populs[[ALADYM_spe]]@M.vect$F[ag,]  <- mort_vect_this_age
       
         ex_stocknumbers_df_M[ag,ind_month] <-  sum(SRO$MFPopulation[simu_mon, c( start_mon_age:end_mon_age) ] )
         unex_stocknumbers_df_M[ag,ind_month] <- sum(SRO$MUPopulation[simu_mon,c( start_mon_age:end_mon_age) ] )
         ex_stockSB_df_M[ag,ind_month] <- sum(SRO$MFPopulation[simu_mon, c( start_mon_age:end_mon_age) ] *  BAS$MWeight[ c( start_mon_age:end_mon_age) ] ) / 1000000
         unex_stockSB_df_M[ag,ind_month] <- sum(SRO$MUPopulation[simu_mon, c( start_mon_age:end_mon_age) ] *  BAS$MWeight[ c( start_mon_age:end_mon_age) ] )  / 1000000

} 

} 

}


# FEMMINE

if (modulo(INP$tr,12) !=0 | INP$tr==0) {

for (ag in 1:INP$FGrowth_tend) {

  if ((ag-1) < first_age ) {
         ex_stocknumbers_df_F[ag,ind_month] <-  0
         unex_stocknumbers_df_F[ag,ind_month] <- 0
         ex_stockSB_df_F[ag,ind_month] <- 0
         unex_stockSB_df_F[ag,ind_month] <- 0

  # print(paste("eta #", ag,"tutti NA"))
      } else {
      if ( (ag*12-12+  modulo(INP$tr,12)) == INP$tr) {   # prima classe non nulla              
 start_mon_age <-  1 
           if (INP$tr == 0) {
           end_mon_age <- 12 - modulo(INP$tr,12) #(ag*12) 
           } else {
            end_mon_age <- 12 - modulo(INP$tr,12) + 1#(ag*12) 
           }
         #  print(paste("eta #", ag," start=", start_mon_age, " end=", end_mon_age))
           # mort_vect_this_age <-  c(rep(NA, ifelse(INP$tr == 0, modulo(INP$tr,12), (modulo(INP$tr,12)-1))) , BAS$MM[start_mon_age:end_mon_age])
        } else {
           start_mon_age <- which(as.numeric(as.character(BAS$FAge))==(as.numeric(as.character(ag))-1))+1 #(ag-1)*12+1 
           end_mon_age <- which(as.numeric(as.character(BAS$FAge))==as.numeric(as.character(ag))) #(ag*12)
          # print(paste("eta #", ag," start=", start_mon_age, " end=", end_mon_age)) 
           # mort_vect_this_age <- BAS$MM[start_mon_age:end_mon_age]
        } 
#
#      if (INP$tr==0) {
#         mort_vect_this_age <- mort_vect_this_age[(length(mort_vect_this_age)-12+1):length(mort_vect_this_age)]
#      }
         ex_stocknumbers_df_F[ag,ind_month] <-  sum(SRO$FFPopulation[simu_mon, c( start_mon_age:end_mon_age) ] )
         unex_stocknumbers_df_F[ag,ind_month] <- sum(SRO$FUPopulation[simu_mon,c( start_mon_age:end_mon_age) ] )
         ex_stockSB_df_F[ag,ind_month] <- sum(SRO$FFPopulation[simu_mon, c( start_mon_age:end_mon_age) ] *  BAS$FWeight[ c( start_mon_age:end_mon_age) ] ) / 1000000
         unex_stockSB_df_F[ag,ind_month] <- sum(SRO$FUPopulation[simu_mon, c( start_mon_age:end_mon_age) ] *  BAS$FWeight[ c( start_mon_age:end_mon_age) ] )  / 1000000

      }
} 

} else {


for (ag in 1:INP$FGrowth_tend) {
  if (ag==trunc(INP$tr/12) ) {   
           start_mon_age <- 1
           end_mon_age <- which(as.numeric(as.character(BAS$FAge))==as.numeric(as.character(ag))) #(ag*12)
         #  print(paste("eta #", ag," start=", start_mon_age, " end=", end_mon_age)) 
          # mort_vect_this_age <- BAS$FM[start_mon_age:end_mon_age]

            ex_stocknumbers_df_F[ag,ind_month] <-  sum(SRO$FFPopulation[simu_mon, c( start_mon_age:end_mon_age) ] )
         unex_stocknumbers_df_F[ag,ind_month] <- sum(SRO$FUPopulation[simu_mon,c( start_mon_age:end_mon_age) ] )
         ex_stockSB_df_F[ag,ind_month] <- sum(SRO$FFPopulation[simu_mon, c( start_mon_age:end_mon_age) ] *  BAS$FWeight[ c( start_mon_age:end_mon_age) ] ) / 1000000
         unex_stockSB_df_F[ag,ind_month] <- sum(SRO$FUPopulation[simu_mon, c( start_mon_age:end_mon_age) ] *  BAS$FWeight[ c( start_mon_age:end_mon_age) ] )  / 1000000

      } else {
           start_mon_age <- which(as.numeric(as.character(BAS$FAge))==(as.numeric(as.character(ag))-1))+1 #(ag-1)*12+1 
           end_mon_age <- which(as.numeric(as.character(BAS$FAge))==as.numeric(as.character(ag))) #(ag*12)
         #  print(paste("eta #", ag," start=", start_mon_age, " end=", end_mon_age)) 
          # mort_vect_this_age <- BAS$FM[start_mon_age:end_mon_age]
       #   Populs[[ALADYM_spe]]@M.vect$F[ag,]  <- mort_vect_this_age
       
         ex_stocknumbers_df_F[ag,ind_month] <-  sum(SRO$FFPopulation[simu_mon, c( start_mon_age:end_mon_age) ] )
         unex_stocknumbers_df_F[ag,ind_month] <- sum(SRO$FUPopulation[simu_mon,c( start_mon_age:end_mon_age) ] )
         ex_stockSB_df_F[ag,ind_month] <- sum(SRO$FFPopulation[simu_mon, c( start_mon_age:end_mon_age) ] *  BAS$FWeight[ c( start_mon_age:end_mon_age) ] ) / 1000000
         unex_stockSB_df_F[ag,ind_month] <- sum(SRO$FUPopulation[simu_mon, c( start_mon_age:end_mon_age) ] *  BAS$FWeight[ c( start_mon_age:end_mon_age) ] )  / 1000000

} 

} 

}

ind_month <- ind_month +1
}   # end months of the year

      Interactionsyear[[ye]][[ALADYM_spe]]@exploitedStock@numbers$M <- ex_stocknumbers_df_M
      Interactionsyear[[ye]][[ALADYM_spe]]@exploitedStock@numbers$F <- ex_stocknumbers_df_F
      Interactionsyear[[ye]][[ALADYM_spe]]@unexploitedStock@numbers$M <- unex_stocknumbers_df_M
      Interactionsyear[[ye]][[ALADYM_spe]]@unexploitedStock@numbers$F <- unex_stocknumbers_df_F 
      
      Interactionsyear[[ye]][[ALADYM_spe]]@exploitedStock@SB$M <- ex_stockSB_df_M
      Interactionsyear[[ye]][[ALADYM_spe]]@exploitedStock@SB$F <- ex_stockSB_df_F
      Interactionsyear[[ye]][[ALADYM_spe]]@unexploitedStock@SB$M <-  unex_stockSB_df_M
      Interactionsyear[[ye]][[ALADYM_spe]]@unexploitedStock@SB$F <-  unex_stockSB_df_F

      SS_temp <- ex_SS_numbers[c(start_mon_simu:end_mon_simu)] 
      colnames(SS_temp) <- MONTHS
      rownames(SS_temp) <- years.forecast[ye_f]
      Interactionsyear[[ye]][[ALADYM_spe]]@exploitedStock@SS.numbers <- SS_temp
      SS_temp <-  unex_SS_numbers[c(start_mon_simu:end_mon_simu)]
      colnames(SS_temp) <- MONTHS
      rownames(SS_temp) <- years.forecast[ye_f]      
      Interactionsyear[[ye]][[ALADYM_spe]]@unexploitedStock@SS.numbers <- SS_temp
      
      SSB_temp <- data.frame(matrix(SRO$FSSBiomass[c(start_mon_simu:end_mon_simu)], nrow=1))
      colnames(SSB_temp) <- MONTHS
      rownames(SSB_temp) <- years.forecast[ye_f]
      Interactionsyear[[ye]][[ALADYM_spe]]@exploitedStock@SSB <- SSB_temp
      SSB_temp <- data.frame(matrix(SRO$USSBiomass[c(start_mon_simu:end_mon_simu)], nrow=1))
      colnames(SSB_temp) <- MONTHS
      rownames(SSB_temp) <- years.forecast[ye_f]
      Interactionsyear[[ye]][[ALADYM_spe]]@unexploitedStock@SSB <- SSB_temp
}   # end year loop


# ----------------------------------------------------------------------------
# ASSIGN BMT VALUES TO THE REFERENCE POINTS in aladym
# ----------------------------------------------------------------------------

ALADYM_RP <- as.logical(cfg[rownames(cfg) == paste("casestudy.referencepoints.S",ALADYM_spe, sep=""),1])

if (FALSE) {

source(paste(ALADYM_home, "/src/paths.r", sep=""))
print(REFERENCEPOINTS_table)

referencepoints_tbl <- read.csv(REFERENCEPOINTS_table, sep=";")

# if the table has 4 rows the F0.1 corresponds to the row 3, else is not available

#rp_temp_0.1 <-  data.frame(matrix(NA, ncol=7, nrow=1))
#colnames(rp_temp_0.1) <- c("factor", "F", "Y", "Y_R", "B", "B_R", "SSB")
#rp_temp_0.2 <-  data.frame(matrix(NA, ncol=7, nrow=1))
#colnames(rp_temp_0.2) <- c("factor", "F", "Y", "Y_R", "B", "B_R", "SSB")
#rp_temp_Fmax <-  data.frame(matrix(NA, ncol=7, nrow=1))
#colnames(rp_temp_Fmax) <- c("factor", "F", "Y", "Y_R", "B", "B_R", "SSB")
#rp_temp_FMSY <-  data.frame(matrix(NA, ncol=7, nrow=1))
#colnames(rp_temp_FMSY) <- c("factor", "F", "Y", "Y_R", "B", "B_R", "SSB")

for (ye_f in 1:bmt_years_forecast) {


ye <- ye_f + simperiod


if (nrow(referencepoints_tbl) == 7) {
# there is no FMSY
# n.2 Fmax
 if (! is.na(as.numeric(referencepoints_tbl[2,2])) ) {
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,2] <- as.numeric(referencepoints_tbl[2,2])
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,3] <- as.numeric(referencepoints_tbl[2,3]) * as.numeric(referencepoints_tbl[2,6])  # yield
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,4] <- as.numeric(referencepoints_tbl[2,3])   # yield per recruits
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,7] <- as.numeric(referencepoints_tbl[2,4]) * as.numeric(referencepoints_tbl[2,6])  # ssb
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,10] <- as.numeric(referencepoints_tbl[2,4])  # ssb per recruits
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,5] <- as.numeric(referencepoints_tbl[2,5])  * as.numeric(referencepoints_tbl[2,6])  # biomass
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,6] <- as.numeric(referencepoints_tbl[2,5])  # biomass per recruits
} else {
 Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,2] <- as.numeric(referencepoints_tbl[5,2])
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,3] <- as.numeric(referencepoints_tbl[5,3]) * as.numeric(referencepoints_tbl[5,6])  # yield
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,4] <- as.numeric(referencepoints_tbl[5,3])   # yield per recruits
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,7] <- as.numeric(referencepoints_tbl[5,4]) * as.numeric(referencepoints_tbl[5,6])  # ssb
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,10] <- as.numeric(referencepoints_tbl[5,4])  # ssb per recruits
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,5] <- as.numeric(referencepoints_tbl[5,5])  * as.numeric(referencepoints_tbl[5,6])  # biomass
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@Fmax[4,6] <- as.numeric(referencepoints_tbl[5,5])  # biomass per recruits
}


# n.3 F0.1
 if (! is.na(as.numeric(referencepoints_tbl[3,2])) ) {
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,2] <- as.numeric(referencepoints_tbl[3,2])
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,3] <- as.numeric(referencepoints_tbl[3,3]) * as.numeric(referencepoints_tbl[3,6])  # yield
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,4] <- as.numeric(referencepoints_tbl[3,3])   # yield per recruits
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,7] <- as.numeric(referencepoints_tbl[3,4]) * as.numeric(referencepoints_tbl[3,6])  # ssb
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,10] <- as.numeric(referencepoints_tbl[3,4])  # ssb per recruits
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,5] <- as.numeric(referencepoints_tbl[3,5])  * as.numeric(referencepoints_tbl[3,6])  # biomass
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,6] <- as.numeric(referencepoints_tbl[3,5])  # biomass per recruits
} else {
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,2] <- as.numeric(referencepoints_tbl[6,2])
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,3] <- as.numeric(referencepoints_tbl[6,3]) * as.numeric(referencepoints_tbl[6,6])  # yield
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,4] <- as.numeric(referencepoints_tbl[6,3])   # yield per recruits
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,7] <- as.numeric(referencepoints_tbl[6,4]) * as.numeric(referencepoints_tbl[6,6])  # ssb
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,10] <- as.numeric(referencepoints_tbl[6,4])  # ssb per recruits
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,5] <- as.numeric(referencepoints_tbl[6,5])  * as.numeric(referencepoints_tbl[6,6])  # biomass
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.1[4,6] <- as.numeric(referencepoints_tbl[6,5])  # biomass per recruits
}

# n.4 F0.2
 if (! is.na(as.numeric(referencepoints_tbl[4,2])) ) {
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,2] <- as.numeric(referencepoints_tbl[4,2])
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,3] <- as.numeric(referencepoints_tbl[4,3]) * as.numeric(referencepoints_tbl[2,6])  # yield
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,4] <- as.numeric(referencepoints_tbl[4,3])   # yield per recruits
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,7] <- as.numeric(referencepoints_tbl[4,4]) * as.numeric(referencepoints_tbl[2,6])  # ssb
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,10] <- as.numeric(referencepoints_tbl[4,4])  # ssb per recruits
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,5] <- as.numeric(referencepoints_tbl[4,5])  * as.numeric(referencepoints_tbl[2,6])  # biomass
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,6] <- as.numeric(referencepoints_tbl[4,5])  # biomass per recruits
} else {
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,2] <- as.numeric(referencepoints_tbl[7,2])
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,3] <- as.numeric(referencepoints_tbl[7,3]) * as.numeric(referencepoints_tbl[7,6])  # yield
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,4] <- as.numeric(referencepoints_tbl[7,3])   # yield per recruits
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,7] <- as.numeric(referencepoints_tbl[7,4]) * as.numeric(referencepoints_tbl[7,6])  # ssb
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,10] <- as.numeric(referencepoints_tbl[7,4])  # ssb per recruits
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,5] <- as.numeric(referencepoints_tbl[7,5])  * as.numeric(referencepoints_tbl[7,6])  # biomass
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@F0.2[4,6] <- as.numeric(referencepoints_tbl[7,5])  # biomass per recruits
}

} else {
# there is only FMSY
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@FMSY[4,2] <- as.numeric(referencepoints_tbl[2,2])
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@FMSY[4,3] <- as.numeric(referencepoints_tbl[2,3])
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@FMSY[4,7] <- as.numeric(referencepoints_tbl[2,4])
Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints@FMSY[4,5] <- as.numeric(referencepoints_tbl[2,5])

#referencePoints_obj <- new(Class= "bmtBioreferencepoint", FMSY = rp_temp_FMSY) 
} # end selection reference points

}

}

#for (ye_f in 1:bmt_years_forecast) {
#ye <- ye_f + simperiod
#Interactionsyear[[ye]][[ALADYM_spe]]@referencePoints <- referencePoints_obj # confermi che i rep points sono uguali per tutti gli anni???
#}









# ALADYM to BMT:	INP	p_Production	-	starting from ALADYM format
   associated_fleetsegment <-as.vector(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ]) 
      associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
      n_fleet_for_species <- length(associated_fleetsegment)


      fleet_interaction_ord <- 1
for (n_fl in 1:n_fleet) {
if (n_fl %in% associated_fleetsegment_indices)  {
for (ye_f in 1:bmt_years_forecast) {
    ye <- ye_f + simperiod




    start_mon_simu <- (ye-1)*12+1 +1
    end_mon_simu <- (ye*12) + 1
 pp_df <- data.frame(matrix(INP$p_Production[c(start_mon_simu:end_mon_simu), fleet_interaction_ord], nrow=1))
 colnames(pp_df) <- MONTHS
 rownames(pp_df) <- years.forecast[ye_f]
 Interactionsyear[[ye]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$pProduction <- pp_df
 fc_df <- data.frame(matrix(INP$Fishing_efforts[c(start_mon_simu:end_mon_simu), fleet_interaction_ord], nrow=1))
 colnames(fc_df) <- MONTHS
 rownames(fc_df) <- years.forecast[ye_f]
 Fleetyear[[ye]]@fleetsegments[[n_fl]]@fishingcoefficient <- fc_df
} # end years

 fleet_interaction_ord <- fleet_interaction_ord+1
}
}  # end fleet loop


# INP	"INP$OPT_SG_TYPE, param1, param2, param3, param4, param5"
selectivity_df <- data.frame(matrix(0, nrow=12, ncol=6))
rownames(selectivity_df) <- MONTHS
colnames(selectivity_df) <- c("param1", "param2","param3","param4","param5","param6")
# structure of selectivity data frame
#selectivity_list <- list(modelSelectivity="classical ogive", parameters=selectivity_df)

fleet_interaction_ord <- 1
for (n_fl in 1:n_fleet) {
if (n_fl %in% associated_fleetsegment_indices)  {
for (ye_f in 1:bmt_years_forecast) {
ye <- ye_f + simperiod


    start_mon_simu <- (ye-1)*12+1 +1
    end_mon_simu <- (ye*12) + 1
    selectivity_df[,1] <- INP$param1[c(start_mon_simu:end_mon_simu), fleet_interaction_ord]
    selectivity_df[,2] <- INP$param2[c(start_mon_simu:end_mon_simu), fleet_interaction_ord]
    selectivity_df[,3] <- INP$param3[c(start_mon_simu:end_mon_simu), fleet_interaction_ord]
    selectivity_df[,4] <- INP$param4[c(start_mon_simu:end_mon_simu), fleet_interaction_ord]
    selectivity_df[,5] <- INP$param5[c(start_mon_simu:end_mon_simu), fleet_interaction_ord]
    selectivity_df[,6] <- INP$OPT_SG_TYPE[c(start_mon_simu:end_mon_simu), fleet_interaction_ord]
		Interactionsyear[[ye]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$selectivity <- selectivity_df
#    selectivity_list <- list(code=INP$OPT_SG_TYPE[1,fleet_interaction_ord], parameters=selectivity_df)
#    Interactionsyear[[ye]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$selectivity <- selectivity_list

		if (any(INP$Land_obl[c(start_mon_simu:end_mon_simu), fleet_interaction_ord] == "Y")) {
      Interactionsyear[[ye]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landing_obligation  <- "Y"
    } else {
    		Interactionsyear[[ye]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landing_obligation <- "N"
    }

} # end years
 fleet_interaction_ord <- fleet_interaction_ord+1
}
}  # end fleet loop 

ret <- list(popus=Populations, inters=Interactionsyear, fleets=Fleetyear)
return(ret)
}
