# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

updateBiologicalfromALADYM.fore.CI <- function(ALADYM_spe, Populations, Interactionsyear, Fleetyear) {


if (FALSE) {
ALADYM_spe = 1
}

n_ages_mal <- INP$MGrowth_tend
n_ages_fem <- INP$FGrowth_tend

first_age <- 0
n_ages_mal <- n_ages_mal - trunc(INP$tr/12)
n_ages_fem <- n_ages_fem - trunc(INP$tr/12)
first_age <- trunc(INP$tr/12)

associated_fleetsegment <- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment != ""]
associated_fleetsegment_indices <- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)
n_fleet_for_species <- length(associated_fleetsegment_indices)

if (INTEGRATED_APPROACH) { 
   bmt_years_forecast <- TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR - simperiod
} else {
   bmt_years_forecast <- foreperiod
}
print(paste("Updating BMT objects from ALADYM forecast with confidence intervals... [", years.forecast[bmt_years_forecast], "]", sep =""), quote=F)
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
## fertility rate
#Populations[[ALADYM_spe]]@offspring.prop[1,]  <- c(INP$Fertility_Rate)

# ----------------------------------------------------------------------------
# ASSIGN BMT VALUES TO THE CATCHES (Landings, Discards and Catches) in aladym
# ----------------------------------------------------------------------------

# catch, landing and discard by age 

if (!RUN_CI_FORE) {
percentiles_numb <-  c(0.05,0.25,0.5,0.75,0.95)
 	percs_table <-	data.frame(matrix(0, nrow=5, ncol=length( c(0:(max(INP$  MGrowth_tend, INP$  FGrowth_tend)-1)))))
   colnames(percs_table) <- c(paste("age", c(0:(max(INP$  MGrowth_tend, INP$  FGrowth_tend)-1)), sep=""))  
   	percs_table_one_row <-	data.frame(matrix(0, nrow=1, ncol=5))

     for (PERC in 1:length(percentiles_numb)) {
     
     
     for (yy_f in 1:bmt_years_forecast) {
          yy <- simperiod+yy_f
           fleet_interaction_ord <- 1
for (n_fl in 1:length(BMT_FLEETSEGMENTS)) {
if (n_fl %in% associated_fleetsegment_indices)  {
	percs_table <-	data.frame(matrix(0, nrow=5, ncol=max(INP$  MGrowth_tend, INP$  FGrowth_tend)))
   colnames(percs_table) <- c(paste("age", c(0:(max(INP$  MGrowth_tend, INP$  FGrowth_tend)-1)), sep=""))  
fleet_interaction_ord <- fleet_interaction_ord + 1
}
} 
     }
     
      for (yy_f in 1:bmt_years_forecast) {
    yy <- yy_f + simperiod  

    	percs_table <-	data.frame(matrix(0, nrow=5, ncol=max(INP$  MGrowth_tend, INP$  FGrowth_tend)))
   colnames(percs_table) <- c(paste("age", c(0:(max(INP$  MGrowth_tend, INP$  FGrowth_tend)-1)), sep=""))  

    	if (PERC == 1) {
    Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@totalweight.CI.perc <- percs_table_one_row
    Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@meanLength.CI.perc <- percs_table_one_row
    Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@meanWeight.CI.perc <- percs_table_one_row
    
    Interactionsyear[[yy]][[ALADYM_spe]]@totallanding@totalweight.CI.perc <- percs_table_one_row
    Interactionsyear[[yy]][[ALADYM_spe]]@totallanding@meanLength.CI.perc <- percs_table_one_row
    Interactionsyear[[yy]][[ALADYM_spe]]@totallanding@meanWeight.CI.perc <- percs_table_one_row

     Interactionsyear[[yy]][[ALADYM_spe]]@totaldiscard@totalweight.CI.perc <- percs_table_one_row
     Interactionsyear[[yy]][[ALADYM_spe]]@totaldiscard@meanLength.CI.perc <- percs_table_one_row
		 Interactionsyear[[yy]][[ALADYM_spe]]@totaldiscard@meanWeight.CI.perc <- percs_table_one_row
		
    Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@fishing_mortality.CI.perc <- percs_table_one_row 

    	}

    Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@totalweight.CI.perc[1,PERC] <- Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@totalweight
    Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@meanLength.CI.perc[1,PERC] <-  Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@meanLength
    Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@meanWeight.CI.perc[1,PERC] <- Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@meanWeight
           
    Interactionsyear[[yy]][[ALADYM_spe]]@totallanding@totalweight.CI.perc[1,PERC] <-  Interactionsyear[[yy]][[ALADYM_spe]]@totallanding@totalweight
    Interactionsyear[[yy]][[ALADYM_spe]]@totallanding@meanLength.CI.perc[1,PERC] <- Interactionsyear[[yy]][[ALADYM_spe]]@totallanding@meanLength
    Interactionsyear[[yy]][[ALADYM_spe]]@totallanding@meanWeight.CI.perc[1,PERC] <-  Interactionsyear[[yy]][[ALADYM_spe]]@totallanding@meanWeight

Interactionsyear[[yy]][[ALADYM_spe]]@totaldiscard@totalweight.CI.perc[1,PERC] <- Interactionsyear[[yy]][[ALADYM_spe]]@totaldiscard@totalweight
  
Interactionsyear[[yy]][[ALADYM_spe]]@totaldiscard@meanLength.CI.perc[1,PERC] <- Interactionsyear[[yy]][[ALADYM_spe]]@totaldiscard@meanLength
Interactionsyear[[yy]][[ALADYM_spe]]@totaldiscard@meanWeight.CI.perc[1,PERC] <- Interactionsyear[[yy]][[ALADYM_spe]]@totaldiscard@meanWeight

        # aggiunta del 28-12-2017 per memorizzare i percentili della F (in caso di incertezza della selettivit? sono necessari) 
    Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@fishing_mortality.CI.perc[1,PERC] <- Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@fishing_mortality

   
    fleet_interaction_ord <- 1
for (n_fl in 1:length(BMT_FLEETSEGMENTS)) {
if (n_fl %in% associated_fleetsegment_indices)  {

if (PERC == 1) {
    Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@totalweight.CI.perc <- percs_table_one_row
     Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@meanLength.CI.perc <- percs_table_one_row
Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@meanWeight.CI.perc <- percs_table_one_row
               
        Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@totalweight.CI.perc <- percs_table_one_row 
        Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@meanLength.CI.perc <- percs_table_one_row
        Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@meanWeight.CI.perc <- percs_table_one_row
                
    Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@totalweight.CI.perc <- percs_table_one_row
    Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@meanLength.CI.perc <- percs_table_one_row
    Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@meanWeight.CI.perc <- percs_table_one_row 
   
    # aggiunta del 28-12-2017 per memorizzare i percentili della F (in caso di incertezza della selettivit? sono necessari)
    Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@fishing_mortality.CI.perc <- percs_table_one_row 

    }
    
Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@totalweight.CI.perc[1,PERC] <- Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@totalweight
Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@meanLength.CI.perc[1,PERC] <- Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@meanLength
Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@meanWeight.CI.perc[1,PERC] <- Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@meanWeight

Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@totalweight.CI.perc[1,PERC] <- Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@totalweight
Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@meanLength.CI.perc[1,PERC] <- Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@meanLength
Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@meanWeight.CI.perc[1,PERC] <- Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@meanWeight
                
Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@totalweight.CI.perc[1,PERC] <- Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@totalweight
Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@meanLength.CI.perc[1,PERC] <- Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@meanLength
Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@meanWeight.CI.perc[1,PERC] <- Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@meanWeight 

        # aggiunta del 28-12-2017 per memorizzare i percentili della F (in caso di incertezza della selettivit? sono necessari) 
Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@fishing_mortality.CI.perc[1,PERC] <- Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@fishing_mortality
        
  
  if (fleet_interaction_ord == 1) {
        # weight, mean length and mean weight related to all the catches (not by fleet segment)
        Interactionsyear[[yy]][[ALADYM_spe]]@meanLength_catches.CI.perc[1,PERC] <-    Interactionsyear[[yy]][[ALADYM_spe]]@meanLength_catches
        Interactionsyear[[yy]][[ALADYM_spe]]@meanWeight_catches.CI.perc[1,PERC] <-   Interactionsyear[[yy]][[ALADYM_spe]]@meanWeight_catches
  }
fleet_interaction_ord <- fleet_interaction_ord+1
  }
}  
}
    
 
if_integrated_do_last_year <- TRUE

#if (INTEGRATED_APPROACH & current_year != foreperiod ) {
#	if_integrated_do_last_year <- FALSE
#}

if (if_integrated_do_last_year) { 
   
    
    for (yy_f in 1:bmt_years_forecast) {
yy <- yy_f + simperiod
     # fleet stock interaction
     if (PERC == 1) {
		    Interactionsyear[[yy]][[ALADYM_spe]]@L95_catches.CI.perc <- percs_table_one_row
		    Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@L95.CI.perc <- percs_table_one_row
				Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@criticalLength.CI.perc <-  percs_table_one_row
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@meanLength.CI.perc <- percs_table_one_row 
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@harvestRate.CI.perc <-  percs_table_one_row
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@exploitationRate.CI.perc <- percs_table_one_row
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@meanWeight.CI.perc <- percs_table_one_row
       
	   Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@annual.SB.CI.perc <- percs_table_one_row
        Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@annual.SSB.CI.perc <- percs_table_one_row
     	Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@annual.numbers.CI.perc <- percs_table_one_row
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@annual.SS.numbers.CI.perc <- percs_table_one_row

	 
	 # unexploited stock
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@L95.CI.perc <-   percs_table_one_row
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@criticalLength.CI.perc <- percs_table_one_row
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@meanLength.CI.perc <-  percs_table_one_row
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@harvestRate.CI.perc <- percs_table_one_row
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@exploitationRate.CI.perc <- percs_table_one_row
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@meanWeight.CI.perc <- percs_table_one_row
            
			Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SB.CI.perc <- percs_table_one_row
        Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SSB.CI.perc <- percs_table_one_row
     	Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@annual.numbers.CI.perc <- percs_table_one_row
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SS.numbers.CI.perc <- percs_table_one_row


		}
		 
     Interactionsyear[[yy]][[ALADYM_spe]]@L95_catches.CI.perc[1,PERC] <- Interactionsyear[[yy]][[ALADYM_spe]]@L95_catches
     # exploited stock
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@L95.CI.perc[1,PERC] <-  Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@L95
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@criticalLength.CI.perc[1,PERC] <- Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@criticalLength
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@meanLength.CI.perc[1,PERC] <-     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@meanLength
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@harvestRate.CI.perc[1,PERC] <-  Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@harvestRate 
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@exploitationRate.CI.perc[1,PERC] <- Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@exploitationRate
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@meanWeight.CI.perc[1,PERC] <-  Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@meanWeight
       
	   Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@annual.SB.CI.perc[1,PERC] <- Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@annual.SB
        Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@annual.SSB.CI.perc[1,PERC] <-Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@annual.SSB
    
	Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@annual.numbers.CI.perc[1,PERC] <- 	Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@annual.numbers
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@annual.SS.numbers.CI.perc[1,PERC] <- Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@annual.SS.numbers


	# unexploited stock
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@L95.CI.perc[1,PERC] <- Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@L95
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@criticalLength.CI.perc[1,PERC] <-  Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@criticalLength
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@meanLength.CI.perc[1,PERC] <- Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@meanLength
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@harvestRate.CI.perc[1,PERC] <- Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@harvestRate
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@exploitationRate.CI.perc[1,PERC] <-Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@exploitationRate
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@meanWeight.CI.perc[1,PERC] <-  Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@meanWeight
       
	Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@annual.numbers.CI.perc[1,PERC] <-	Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@annual.numbers
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SS.numbers.CI.perc[1,PERC] <- Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SS.numbers
    Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SB.CI.perc[1,PERC] <-Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SB
        Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SSB.CI.perc[1,PERC] <-  Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SSB

}
    
    
}
    
    
    
    
     }

} else {

production_tbl_percentiles <- try(read.csv(paste(PRODUCTION_table_CI, " quantiles.csv", sep=""), sep=";") )
mortalities_tbl_percentiles <- try(read.csv(paste(MORTALITY_table_CI, " quantiles.csv", sep=""), sep=";")  )

population_tbl_percentiles <- try(read.csv(paste(POPULATION_table_CI, " quantiles.csv", sep=""), sep=";") )


 percentiles_numb <-  c(0.05,0.25,0.5,0.75,0.95)
 	percs_table <-	data.frame(matrix(0, nrow=5, ncol=length( c(0:(max(INP$  MGrowth_tend, INP$  FGrowth_tend)-1)))))
   colnames(percs_table) <- c(paste("age", c(0:(max(INP$  MGrowth_tend, INP$  FGrowth_tend)-1)), sep=""))  
   
   	percs_table_one_row <-	data.frame(matrix(0, nrow=1, ncol=5))



for (PERC in 1:length(percentiles_numb)) {

  for (yy_f in 1:bmt_years_forecast) {
  yy <- simperiod+yy_f

        TOTAL_LANDING_numbers <-  data.frame(matrix(0, nrow=1, ncol=max(INP$  MGrowth_tend, INP$  FGrowth_tend)))
      rownames(TOTAL_LANDING_numbers) <- years.forecast[yy_f]
      colnames(TOTAL_LANDING_numbers) <- c(paste("age", c(0:(max(INP$  MGrowth_tend, INP$  FGrowth_tend)-1)), sep=""))  
     
      TOTAL_CATCH_numbers <- data.frame(matrix(0, nrow=1, ncol=max(INP$  MGrowth_tend, INP$  FGrowth_tend)))
      rownames(TOTAL_CATCH_numbers) <-  years.forecast[yy_f]
      colnames(TOTAL_CATCH_numbers) <- c(paste("age", c(0:(max(INP$  MGrowth_tend, INP$  FGrowth_tend)-1)), sep=""))  
  
        TOTAL_DISCARD_numbers  <- data.frame(matrix(0, nrow=1, ncol=max(INP$  MGrowth_tend, INP$  FGrowth_tend)))
      rownames(TOTAL_DISCARD_numbers) <-   years.forecast[yy_f]
      colnames(TOTAL_DISCARD_numbers) <- c(paste("age", c(0:(max(INP$  MGrowth_tend, INP$  FGrowth_tend)-1)), sep=""))  
     
      TOTAL_DISCARD_numbers_NA <- data.frame(matrix(NA, nrow=1, ncol=max(INP$  MGrowth_tend, INP$  FGrowth_tend)))
      rownames(TOTAL_DISCARD_numbers_NA) <-   years.forecast[yy_f]
      colnames(TOTAL_DISCARD_numbers_NA) <- c(paste("age", c(0:(max(INP$  MGrowth_tend, INP$  FGrowth_tend)-1)), sep=""))  
	  
  
   fleet_interaction_ord <- 1
for (n_fl in 1:length(BMT_FLEETSEGMENTS)) {

if (n_fl %in% associated_fleetsegment_indices)  {

		percs_table <-	data.frame(matrix(0, nrow=5, ncol=ncol(TOTAL_LANDING_numbers)))
   colnames(percs_table) <- c(paste("age", c(0:(max(INP$  MGrowth_tend, INP$  FGrowth_tend)-1)), sep=""))  
     fleet_interaction_ord <- fleet_interaction_ord+1 
}

}

} # end years loop

# total weigth of catches, landing and discard 
production_tbl  <-  production_tbl_percentiles[ production_tbl_percentiles$percentile == percentiles_numb[PERC], 1:(ncol(production_tbl_percentiles)-1)]
mortalities_tbl  <-  mortalities_tbl_percentiles[ mortalities_tbl_percentiles$percentile == percentiles_numb[PERC], 1:(ncol(mortalities_tbl_percentiles)-1)]

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

    	percs_table <-	data.frame(matrix(0, nrow=5, ncol=ncol(TOTAL_LANDING_numbers)))
   colnames(percs_table) <- c(paste("age", c(0:(max(INP$  MGrowth_tend, INP$  FGrowth_tend)-1)), sep=""))  

    	if (PERC == 1) {
    Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@totalweight.CI.perc <- percs_table_one_row
    Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@meanLength.CI.perc <- percs_table_one_row
    Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@meanWeight.CI.perc <- percs_table_one_row
    
    Interactionsyear[[yy]][[ALADYM_spe]]@totallanding@totalweight.CI.perc <- percs_table_one_row
    Interactionsyear[[yy]][[ALADYM_spe]]@totallanding@meanLength.CI.perc <- percs_table_one_row
    Interactionsyear[[yy]][[ALADYM_spe]]@totallanding@meanWeight.CI.perc <- percs_table_one_row

     Interactionsyear[[yy]][[ALADYM_spe]]@totaldiscard@totalweight.CI.perc <- percs_table_one_row
     Interactionsyear[[yy]][[ALADYM_spe]]@totaldiscard@meanLength.CI.perc <- percs_table_one_row
		 Interactionsyear[[yy]][[ALADYM_spe]]@totaldiscard@meanWeight.CI.perc <- percs_table_one_row
		 
		  Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@fishing_mortality.CI.perc <- percs_table_one_row
    	}

    Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@totalweight.CI.perc[1,PERC] <- catch_weight$Total_Yield[yy]
    Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@meanLength.CI.perc[1,PERC] <- catch_mean_length$Mean_length_in_catch[yy]
    Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@meanWeight.CI.perc[1,PERC] <- lw_a * as.numeric(catch_mean_length$Mean_length_in_catch[yy])^ lw_b
    
    Interactionsyear[[yy]][[ALADYM_spe]]@totallanding@totalweight.CI.perc[1,PERC] <- landing_weight$Total_Landing[yy]
    Interactionsyear[[yy]][[ALADYM_spe]]@totallanding@meanLength.CI.perc[1,PERC] <- landing_mean_length$Mean_length_in_Landing[yy]
    Interactionsyear[[yy]][[ALADYM_spe]]@totallanding@meanWeight.CI.perc[1,PERC] <- lw_a * as.numeric(landing_mean_length$Mean_length_in_Landing[yy])^ lw_b

if (n_fleet_for_species > 1) {
     Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@fishing_mortality.CI.perc[1,PERC]  <-  mortalities_tbl[yy,(14+(length(associated_fleetsegment_indices)*3))]
 } else {
    Interactionsyear[[yy]][[ALADYM_spe]]@totalcatch@fishing_mortality.CI.perc[1,PERC]  <-  mortalities_tbl[yy,ncol(mortalities_tbl)]
 }
 

	 if ( Interactionsyear[[yy]][[ALADYM_spe]]@totaldiscard@totalweight == 0 ) {
      Interactionsyear[[yy]][[ALADYM_spe]]@totaldiscard@totalweight.CI.perc[1,PERC] <- 0
    } else {
      Interactionsyear[[yy]][[ALADYM_spe]]@totaldiscard@totalweight.CI.perc[1,PERC] <- ifelse(is.na(discard_weight$Total_Discard[yy]), -1, discard_weight$Total_Discard[yy])
    }
Interactionsyear[[yy]][[ALADYM_spe]]@totaldiscard@meanLength.CI.perc[1,PERC] <- ifelse(is.na(discard_mean_length$Mean_length_in_Discard[yy]), -1, discard_mean_length$Mean_length_in_Discard[yy])
Interactionsyear[[yy]][[ALADYM_spe]]@totaldiscard@meanWeight.CI.perc[1,PERC] <- ifelse(is.na(discard_mean_length$Mean_length_in_Discard[yy]), -1, (lw_a * as.numeric(discard_mean_length$Mean_length_in_Discard[yy])^ lw_b))

       
   fleet_interaction_ord <- 1
for (n_fl in 1:length(BMT_FLEETSEGMENTS)) {
if (n_fl %in% associated_fleetsegment_indices)  {

if (PERC == 1) {
    Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@totalweight.CI.perc <- percs_table_one_row
     Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@meanLength.CI.perc <- percs_table_one_row
Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@meanWeight.CI.perc <- percs_table_one_row
        
        Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@totalweight.CI.perc <- percs_table_one_row 
        Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@meanLength.CI.perc <- percs_table_one_row
Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@meanWeight.CI.perc <- percs_table_one_row
                
    Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@totalweight.CI.perc <- percs_table_one_row
    Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@meanLength.CI.perc <- percs_table_one_row
    Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@meanWeight.CI.perc <- percs_table_one_row
    
     Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@fishing_mortality.CI.perc <- percs_table_one_row    
    }
    
        Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@totalweight.CI.perc[1,PERC] <- as.numeric(as.character(catch_weight[yy,fleet_interaction_ord+1]) )
        Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@meanLength.CI.perc[1,PERC] <- as.numeric(catch_mean_length[yy,fleet_interaction_ord+1] )
Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@meanWeight.CI.perc[1,PERC] <-  lw_a * as.numeric(catch_mean_length[yy,fleet_interaction_ord+1])^ lw_b
        
        Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@totalweight.CI.perc[1,PERC] <- as.numeric(landing_weight[yy,fleet_interaction_ord+1] )
        Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@meanLength.CI.perc[1,PERC] <- as.numeric(landing_mean_length[yy,fleet_interaction_ord+1])
Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$landings@meanWeight.CI.perc[1,PERC] <-  lw_a * as.numeric(landing_mean_length[yy,fleet_interaction_ord+1])^ lw_b
                
    Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@totalweight.CI.perc[1,PERC] <- ifelse(is.na(discard_weight[yy,fleet_interaction_ord+1]), -1, as.numeric(discard_weight[yy,fleet_interaction_ord+1])) 
    Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@meanLength.CI.perc[1,PERC] <- ifelse(is.na(discard_mean_length[yy,fleet_interaction_ord+1]), -1, as.numeric(discard_mean_length[yy,fleet_interaction_ord+1]))     
    Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$discards@meanWeight.CI.perc[1,PERC] <- ifelse(is.na(discard_mean_length[yy,fleet_interaction_ord+1]), -1, lw_a * as.numeric(discard_mean_length[yy,fleet_interaction_ord+1])^ lw_b )
  
          if (n_fleet_for_species > 1) { 
         Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@fishing_mortality.CI.perc[1,PERC] <-  mortalities_tbl[yy,(14+(length(associated_fleetsegment_indices)*3)+fleet_interaction_ord)] 
 		} else {
    		Interactionsyear[[yy]][[ALADYM_spe]]@interactions[[fleet_interaction_ord]]$catches@fishing_mortality.CI.perc[1,PERC] <- mortalities_tbl[yy,ncol(mortalities_tbl)]  
         }
  
  if (fleet_interaction_ord == 1) {
        # weight, mean length and mean weight related to all the catches (not by fleet segment)
        Interactionsyear[[yy]][[ALADYM_spe]]@meanLength_catches.CI.perc[1,PERC] <- as.numeric(catch_mean_length[yy,1])
        Interactionsyear[[yy]][[ALADYM_spe]]@meanWeight_catches.CI.perc[1,PERC] <-  lw_a * as.numeric(catch_mean_length[yy,1]) ^ lw_b
  }
        # inserire f e z nello slot
     #   if (n_fleet_for_species > 1) { 
#  	  Interactionsyear[[yy]][[ALADYM_spe]]@mortalities$F[n_fl] <- mortalities_tbl[yy,(14+(length(associated_fleetsegment_indices)*3)+fleet_interaction_ord)]  
# 		} else {
#    		 Interactionsyear[[yy]][[ALADYM_spe]]@mortalities$F[n_fl] <- mortalities_tbl[yy,ncol(mortalities_tbl)]       
#         }


fleet_interaction_ord <- fleet_interaction_ord+1
  }

}
} # ens years loop
# }  




# ----------------------------------------------------------------------------
# ASSIGN BMT VALUES TO THE STOCKs in aladym
# ----------------------------------------------------------------------------

# L95 for stock


population_tbl <- population_tbl_percentiles[population_tbl_percentiles$percentile == percentiles_numb[PERC], 1:(ncol(population_tbl_percentiles)-1)]

if_integrated_do_last_year <- TRUE

#if (INTEGRATED_APPROACH & current_year != foreperiod ) {
#	if_integrated_do_last_year <- FALSE
#}

if (if_integrated_do_last_year) { 
indicators_tbl_percentiles <- try(read.csv(paste(INDICATORS_table_CI, " quantiles.csv", sep=""), sep=";") )
indicators_tbl <- indicators_tbl_percentiles[indicators_tbl_percentiles$percentile == percentiles_numb[PERC], 1:(ncol(indicators_tbl_percentiles)-1)]

for (yy_f in 1:bmt_years_forecast) {
yy <- yy_f + simperiod
     # fleet stock interaction
     if (PERC == 1) {
		    Interactionsyear[[yy]][[ALADYM_spe]]@L95_catches.CI.perc <- percs_table_one_row
		    Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@L95.CI.perc <- percs_table_one_row
				Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@criticalLength.CI.perc <-  percs_table_one_row
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@meanLength.CI.perc <- percs_table_one_row 
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@harvestRate.CI.perc <-  percs_table_one_row
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@exploitationRate.CI.perc <- percs_table_one_row
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@meanWeight.CI.perc <- percs_table_one_row
       
	   Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@annual.SB.CI.perc <- percs_table_one_row
        Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@annual.SSB.CI.perc <- percs_table_one_row
     	Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@annual.numbers.CI.perc <- percs_table_one_row
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@annual.SS.numbers.CI.perc <- percs_table_one_row

	 
	 # unexploited stock
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@L95.CI.perc <-   percs_table_one_row
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@criticalLength.CI.perc <- percs_table_one_row
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@meanLength.CI.perc <-  percs_table_one_row
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@harvestRate.CI.perc <- percs_table_one_row
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@exploitationRate.CI.perc <- percs_table_one_row
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@meanWeight.CI.perc <- percs_table_one_row
            
			Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SB.CI.perc <- percs_table_one_row
        Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SSB.CI.perc <- percs_table_one_row
     	Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@annual.numbers.CI.perc <- percs_table_one_row
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SS.numbers.CI.perc <- percs_table_one_row


		}
		 
     Interactionsyear[[yy]][[ALADYM_spe]]@L95_catches.CI.perc[1,PERC] <- indicators_tbl$catch_L0_95[yy]
     # exploited stock
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@L95.CI.perc[1,PERC] <- indicators_tbl$ex_stock_L0_95[yy]
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@criticalLength.CI.perc[1,PERC] <- indicators_tbl$Critical_length_expl_pop[yy]
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@meanLength.CI.perc[1,PERC] <- population_tbl$Mean_length_of_exploited_pop[yy]
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@harvestRate.CI.perc[1,PERC] <- indicators_tbl$Harvest_ratio[yy] 
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@exploitationRate.CI.perc[1,PERC] <- indicators_tbl$Exploitation_rate[yy]
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@meanWeight.CI.perc[1,PERC] <- lw_a * Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@meanLength.CI.perc[1,PERC]  ^ lw_b
       
	   Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@annual.SB.CI.perc[1,PERC] <- population_tbl$Total_biomass_exploited_pop[yy]
        Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@annual.SSB.CI.perc[1,PERC] <- population_tbl$SSB_exploited_pop[yy]
    
	Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@annual.numbers.CI.perc[1,PERC] <- population_tbl$NUMBERS_exploited_pop[yy] *1000
     Interactionsyear[[yy]][[ALADYM_spe]]@exploitedStock@annual.SS.numbers.CI.perc[1,PERC] <- population_tbl$SS_NUMBERS_exploited_pop[yy]*1000


	# unexploited stock
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@L95.CI.perc[1,PERC] <- indicators_tbl$unex_stock_L0_95[yy]
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@criticalLength.CI.perc[1,PERC] <- indicators_tbl$Critical_length_unexpl_pop[yy]
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@meanLength.CI.perc[1,PERC] <- population_tbl$Mean_length_of_unexploited_pop[yy]
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@harvestRate.CI.perc[1,PERC] <- 0
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@exploitationRate.CI.perc[1,PERC] <- 0
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@meanWeight.CI.perc[1,PERC] <-  lw_a * Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@meanLength.CI.perc[1,PERC]  ^ lw_b
       
	Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@annual.numbers.CI.perc[1,PERC] <- population_tbl$NUMBERS_unexploited_pop[yy] *1000
     Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SS.numbers.CI.perc[1,PERC] <- population_tbl$SS_NUMBERS_unexploited_pop[yy]*1000
    Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SB.CI.perc[1,PERC] <- population_tbl$Total_biomass_unexploited_pop[yy]
        Interactionsyear[[yy]][[ALADYM_spe]]@unexploitedStock@annual.SSB.CI.perc[1,PERC] <- population_tbl$SSB_unexploited_pop[yy]

}

}

# ----------------------------------------------------------------------------
# ASSIGN BMT VALUES TO THE REFERENCE POINTS in aladym
# ----------------------------------------------------------------------------


}   # end loop percs

}

#
ret <- list(popus=Populations, inters=Interactionsyear, fleets=Fleetyear)
return(ret)
}
