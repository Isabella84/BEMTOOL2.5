# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




 casestudy_name <<- "HR12-MEY"

   new_aldForecast <<-  ALADYM_GUI_forecast_int[[ALADYM_spe]] #ALADYM_GUI_forecast_int["aldForecast"] $aldForecast[ALADYM_spe][[1]]
    new_aldSimulation <<-  ALADYM_GUI_simulations[[ALADYM_spe]] #ALADYM_GUI_forecast_int["aldForecast"] $aldForecast[ALADYM_spe][[1]]
        new_aldPopulation <<-  ALADYM_GUI_populations[[ALADYM_spe]] #ALADYM_GUI_forecast_int["aldForecast"] $aldForecast[ALADYM_spe][[1]]

    FleetList_simulation <<- .GlobalEnv$ALADYM_GUI_fleets[[ALADYM_spe]]

current_year <<- foreperiod

associated_fleetsegment <<- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <<- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <<- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

FLEETSEGMENTS_names <<- BMT_FLEETSEGMENTS[associated_fleetsegment_indices]
print(FLEETSEGMENTS_names)

#path_to_save <- paste(casestudy_path, "/",harvest_rule_id, "/ALADYM/GUIfle_fore.Rdata", sep="")
#load(path_to_save)
FleetList_forecast <- .GlobalEnv$ALADYM_GUI_fleets_fore[[ALADYM_spe]]
#print(paste("alla chiamata di run forecast anno 1 specie", ALADYM_spe))
#print(FleetList_forecast[[1]]@fishingeffort.vector)

 n_ord <- 1
for (n_int in 1:length(BMT_FLEETSEGMENTS) ) {
    if (n_int %in% associated_fleetsegment_indices) {
      # FleetList_forecast <<-
      update_BMT_fleetsegments_fore(n_ord, n_int) #, FleetList_forecast
      n_ord <- n_ord+1
      # FleetList_simulation
    }
}

.GlobalEnv$ALADYM_GUI_fleets_fore[[ALADYM_spe]]  <- FleetList_forecast

              source(paste(ALADYM_home, "/src/runALADYMforecast.r", sep="") )
              print(paste("Updating Biological data from ALADYM forecast for species [", BMT_SPECIES[ALADYM_spe], "]", sep=""), quote=FALSE )
              biologicalUpdateResults_fore <- updateBiologicalfromALADYM.fore(ALADYM_spe, .GlobalEnv$Populations, .GlobalEnv$Interactionsyear, .GlobalEnv$Fleetyear)
              Populations <<- biologicalUpdateResults_fore$popus
              Interactionsyear <<- biologicalUpdateResults_fore$inters
              Fleetyear <<- biologicalUpdateResults_fore$fleets

						#	if (RUN_CI_FORE) {
							biologicalUpdateResults_fore <- updateBiologicalfromALADYM.fore.CI(ALADYM_spe, .GlobalEnv$Populations, .GlobalEnv$Interactionsyear, .GlobalEnv$Fleetyear)
              Populations <<- biologicalUpdateResults_fore$popus
              Interactionsyear <<- biologicalUpdateResults_fore$inters
              Fleetyear <<- biologicalUpdateResults_fore$fleets
						#	}

              # fleet_list <- list(species=ALADYM_spe, aldFleets=FleetList_forecast)

ALADYM_spe <<- ALADYM_spe + 1
if (ALADYM_spe <= length(BMT_SPECIES)) {
ALADYM_flag <<- as.logical(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".AladymSimulation", sep=""),1])

  if (ALADYM_flag) {
              forecast <- (casestudy.endsimulation - casestudy.startsimulation +1) * 12  + 1

source(paste(getwd(), "/src/biol/bmtALADYM/reloadEnvSpecies.r", sep=""))

        suppressWarnings(source(paste(ALADYM_home, "/gui/menubarFun.run_forecast_bmt.MEYcode.from2.r", sep="") ) )
#source(paste(ALADYM_home, "/ALADYM.r", sep=""))

    }
      BMT_STATE <<- "WAIT"

   } else {
    # BMT_STATE <<- "FORECAST"
    
   
if (!INTEGRATED_APPROACH) {
  for (ye_f in 1:foreperiod) {
         yy <- ye_f + simperiod
       Fleetyear <<- updateFleetfromInteraction(Fleetyear, yy)
        } 
  
       # source(suppressWarnings(paste(getwd(), "/src/econ/readEconparams_fore.r", sep="")))  
           
        for (ye_f in 1:foreperiod) {
         yy <- ye_f + simperiod
         Fleetyear <<- runEcon.fore( Fleetyear, yy)
        }  
         
numb_yearsNPV <- ifelse(foreperiod < 15, foreperiod, 15)  
  numb_yearsNPV_plus <- ifelse(foreperiod < 16, foreperiod, 16)          
    
    for (fleet_i in 1:n_fleet) {
    NPV15_calculated <- Fleetyear[[simperiod]]@fleetsegments[[fleet_i]]@EC.NPV.discounted 
     NPV15_calculated_perc <- Fleetyear[[simperiod]]@fleetsegments[[fleet_i]]@EC.NPV.discounted.CI.perc
       for (ye_f in (simperiod+1):(simperiod+numb_yearsNPV)) {
                   NPV15_calculated <- NPV15_calculated + Fleetyear[[ye_f]]@fleetsegments[[fleet_i]]@EC.NPV.discounted  
                     NPV15_calculated_perc <- NPV15_calculated_perc + Fleetyear[[ye_f]]@fleetsegments[[fleet_i]]@EC.NPV.discounted.CI.perc 
        }

          bmtindicators_discount_rate <- as.numeric(as.character(eimat[8,fleet_i]))  
     for (ye_f in 1:foreperiod) {
            Fleetyear[[ye_f]]@fleetsegments[[fleet_i]]@EC.NPV15 <- NPV15_calculated 
            Fleetyear[[ye_f]]@fleetsegments[[fleet_i]]@EC.NPV15.CI.perc <- NPV15_calculated_perc 
            
						Fleetyear[[ye_f]]@fleetsegments[[fleet_i]]@EC.NPV15.infinite <-  (Fleetyear[[numb_yearsNPV_plus-1]]@fleetsegments[[fleet_i]]@EC.NPV.discounted/ bmtindicators_discount_rate)*( (1 + bmtindicators_discount_rate )^ (- (numb_yearsNPV_plus)) )  +   Fleetyear[[ye_f]]@fleetsegments[[fleet_i]]@EC.NPV15
					
					for (PERC in c(1:5)) {	
						Fleetyear[[ye_f]]@fleetsegments[[fleet_i]]@EC.NPV15.infinite.CI.perc[1,PERC] <-  (Fleetyear[[numb_yearsNPV_plus-1]]@fleetsegments[[fleet_i]]@EC.NPV.discounted.CI.perc[1,PERC]/ bmtindicators_discount_rate)*( (1 + bmtindicators_discount_rate )^ (- (numb_yearsNPV_plus)) )  +   Fleetyear[[ye_f]]@fleetsegments[[fleet_i]]@EC.NPV15.CI.perc[1,PERC]
						}
              # ((+E40/0.05)*(1.05)^ - numb_yearsNPV_plus)  + 
     }
     }
           
              
 }

 casestudy_name <<- "HR12-MEY"

name_bioind <<- paste(casestudy_path, "/MEY calculation/",harvest_rule_id, "/", casestudy_name, " - Biological indicators ", harvest_rule_id,".csv", sep="")
name_bioind_CI <<- paste(casestudy_path, "/MEY calculation/",harvest_rule_id, "/", casestudy_name, " - Biological indicators ", harvest_rule_id," quantiles.csv", sep="")
name_pressind <<- paste(casestudy_path, "/MEY calculation/",harvest_rule_id, "/", casestudy_name, " - Pressure impact indicators ", harvest_rule_id,".csv", sep="")
name_pressind_CI <<- paste(casestudy_path, "/MEY calculation/",harvest_rule_id, "/", casestudy_name, " - Pressure impact indicators ", harvest_rule_id," quantiles.csv", sep="")
name_econind <<- paste(casestudy_path, "/MEY calculation/",harvest_rule_id, "/", casestudy_name, " - Economic output ", harvest_rule_id,".csv", sep="")
name_econind_CI <<- paste(casestudy_path, "/MEY calculation/",harvest_rule_id, "/", casestudy_name, " - Economic output ", harvest_rule_id," quantiles.csv", sep="")

      
Rworking_env <- paste(casestudy_path, "/MEY calculation/",harvest_rule_id, "/", casestudy_name, " - BEMTOOL forecast ", harvest_rule_id, ".Rdata", sep="")
save.image(file=Rworking_env) 

Rworking_env_selection <- paste(casestudy_path, "/MEY calculation/",harvest_rule_id, "/", casestudy_name, " - BEMTOOL forecast ", harvest_rule_id, "-rev.Rdata", sep="")

save(file=Rworking_env_selection, list = c("casestudy_path","casestudy_name", "Populations", "FleetStockInteractions", "Interactionsyear" , "Fleetyear", "XSAinfo", "XSAinfo_empty", "VITinfo", "VITinfo_empty" , "SURBAinfo", "SURBAinfo_empty",  "ReportINFO", "ReportINFO_empty", "ALADYM_GUI_fleets",  "ALADYM_GUI_populations" , "ALADYM_GUI_simulations", "BMT_SPECIES", "BMT_FLEETSEGMENTS", "years", "years_forecast", "FleetList_forecast"))           #   "ALADYM_GUI_fleets_fore",
  
   
print("FORECAST in progress [State indicators]...", quote=F)
write.table(getBiologicalTable(c(years, years.forecast)), file=name_bioind, sep=";", row.names=F)

print("FORECAST in progress [Pressure/Impact indicators]...", quote=F)
write.table(getPressureImpactTable(c(years, years.forecast)), file=name_pressind, sep=";", row.names=F)

print("FORECAST in progress [Economic indicators]...", quote=F)
write.table(getEconomicTable(c(years, years.forecast)), file=name_econind, sep=";", row.names=F)
               
# write.table(getEconomicTable(c(years, years.forecast)), file="C:/test_eco_ind.csv", sep=";", row.names=F)

print("Saving Kobe plot...", quote=F)
bmtsaveKobePlot(c(years, years.forecast), name_bioind, name_pressind)

if (FALSE) {
print("Saving biological indicators plot...", quote=F)
saveSingleIndicatorPlot(c(years, years.forecast), name_bioind, name_pressind, name_bioind_CI, name_pressind_CI)
print("Saving F plot...", quote=F)
saveF_Plot(c(years, years.forecast), name_pressind, name_bioind)
print("Saving Yield barplot...", quote=F)
saveYield_barPlot(c(years, years.forecast), name_pressind)

print("Saving Revenues-Landings plot...", quote=F)
saveRevenues_Plot(c(years, years.forecast), name_econind, name_econind_CI)
print("Saving Avg.salary-Employment plot...", quote=F)
saveSalary_Plot(c(years, years.forecast), name_econind, name_econind_CI)
print("Saving Net profit-R/BER plot...", quote=F)
saveProfit_Plot(c(years, years.forecast), name_econind, name_econind_CI)
print("Saving Profit-Salary-Capital barplot...", quote=F)
save_EconomicbarPlot(c(years, years.forecast), name_econind, name_econind_CI)
print("Saving Estimated price plot...", quote=F)
plotEstimatedPrice(c(years, years.forecast), name_econind, name_econind_CI)
}
    
      source(suppressWarnings(paste(getwd(), "/src/hr/MEYcalculation.r", sep="") ) )
   }
