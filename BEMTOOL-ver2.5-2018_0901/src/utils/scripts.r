# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




source(paste(getwd(), "/src/biol/bmtMTF/MTF/scripts/load_libraries.r", sep=""))
source(paste(getwd(), "/src/biol/bmtSTF/STF/scripts/load_libraries.r", sep=""))

source(paste(getwd(), "/src/econ/core/BMTCapcost.r", sep=""))
source(paste(getwd(), "/src/econ/core/BMTEconind.r", sep=""))
source(paste(getwd(), "/src/econ/core/BMTFixcost.r", sep=""))
source(paste(getwd(), "/src/econ/core/BMTFlbeh.r", sep=""))
source(paste(getwd(), "/src/econ/core/BMTLabcost.r", sep=""))
source(paste(getwd(), "/src/econ/core/BMTLandsplit.r", sep=""))
source(paste(getwd(), "/src/econ/core/BMTPrice.r", sep=""))
source(paste(getwd(), "/src/econ/core/BMTPrice_discard.r", sep=""))
source(paste(getwd(), "/src/econ/core/BMTVarcost.r", sep=""))
source(paste(getwd(), "/src/econ/setFleet.r", sep=""))
source(paste(getwd(), "/src/econ/setFleet_fore.r", sep=""))
source(paste(getwd(), "/src/econ/runEcon.r", sep=""))
source(paste(getwd(), "/src/econ/runEcon.fore.r", sep=""))
source(paste(getwd(), "/src/econ/setEffortVarsForecast.r", sep=""))
source(paste(getwd(), "/src/econ/resetDAYSfromTAC.r", sep=""))
source(paste(getwd(), "/src/econ/updateDerivedEffortVars.nonint.r", sep=""))   
source(paste(getwd(), "/src/econ/updateDerivedEffortVars.int.r", sep=""))  

source(paste(getwd(), "/src/obj/bmtBioreferencepoint.r", sep=""))
source(paste(getwd(), "/src/obj/bmtCatch.r", sep=""))
source(paste(getwd(), "/src/obj/bmtFleetsegment.r", sep=""))  
source(paste(getwd(), "/src/obj/bmtStock.r", sep=""))
source(paste(getwd(), "/src/obj/bmtInteraction.r", sep=""))
source(paste(getwd(), "/src/obj/bmtPopulation.r", sep=""))
source(paste(getwd(), "/src/obj/bmtFleet.r", sep=""))

#source(paste(getwd(), "/src/biol/bmtALADYM/get_effort_data.r", sep=""))
#source(paste(getwd(), "/src/biol/bmtALADYM/get_production_data.r", sep=""))
source(paste(getwd(), "/src/biol/bmtALADYM/updateBiologicalfromALADYM.r", sep=""))
source(paste(getwd(), "/src/biol/bmtALADYM/updateBiologicalfromALADYM.fore.r", sep="")) 
source(paste(getwd(), "/src/biol/bmtALADYM/updateBiologicalfromALADYM.fore.CI.r", sep="")) 
source(paste(getwd(), "/src/biol/bmtMTF/get_annual_reduction_totalF.r", sep=""))
source(paste(getwd(), "/src/biol/bmtMTF/fromBMTtoXSAobject.r", sep="")) 
source(paste(getwd(), "/src/biol/bmtMTF/updateBiologicalfromMTF.r", sep=""))
source(paste(getwd(), "/src/biol/bmtMTF/correction_stock_nb_from_VIT.r", sep=""))  
source(paste(getwd(), "/src/biol/bmtMTF/MTF/scripts/MTF.r", sep=""))
source(paste(getwd(), "/src/biol/bmtSTF/STF/scripts/STF.r", sep=""))
source(paste(getwd(), "/src/biol/bmtVIT/readVIT.r", sep=""))
source(paste(getwd(), "/src/biol/bmtVIT/getVIToutput.r", sep=""))
source(paste(getwd(), "/src/biol/bmtVIT/updateBiologicalfromVIT.r", sep=""))
source(paste(getwd(), "/src/biol/bmtXSA/readXSA.r", sep=""))
source(paste(getwd(), "/src/biol/bmtXSA/updateBiologicalfromXSA.r", sep=""))
source(paste(getwd(), "/src/biol/bmtReport/readReport.r", sep=""))
source(paste(getwd(), "/src/biol/bmtReport/updateBiologicalfromReport.r", sep=""))
source(paste(getwd(), "/src/biol/bmtSURBA/readSURBA.r", sep=""))
source(paste(getwd(), "/src/biol/bmtSURBA/updateBiologicalfromSURBA.r", sep=""))
source(paste(getwd(), "/src/biol/updateFleetfromInteraction.r", sep=""))

source(paste(getwd(), "/src/hr/getBiologicalTable.r", sep=""))
source(paste(getwd(), "/src/hr/getBiologicalTable_CI.r", sep=""))
source(paste(getwd(), "/src/hr/getEconomicTable.r", sep=""))
source(paste(getwd(), "/src/hr/getEconomicTable_CI.r", sep=""))
source(paste(getwd(), "/src/hr/getPressureImpactTable.r", sep=""))
source(paste(getwd(), "/src/hr/getPressureImpactTable_CI.r", sep=""))
source(paste(getwd(), "/src/hr/calculateTAC.r", sep=""))
source(paste(getwd(), "/src/hr/plot/bmtsaveKobePlot.r", sep=""))
source(paste(getwd(), "/src/hr/plot/saveSingleIndicatorPlot.r", sep=""))
source(paste(getwd(), "/src/hr/plot/saveSingleIndicatorPlot_econ.r", sep=""))
source(paste(getwd(), "/src/hr/plot/saveF_Plot.r", sep=""))
source(paste(getwd(), "/src/hr/plot/saveYield_barPlot.r", sep=""))
source(paste(getwd(), "/src/hr/plot/saveRevenues_Plot.r", sep=""))
source(paste(getwd(), "/src/hr/plot/saveRevenues_discard_Plot.r", sep=""))
source(paste(getwd(), "/src/hr/plot/saveSalary_Plot.r", sep=""))
source(paste(getwd(), "/src/hr/plot/saveProfit_Plot.r", sep=""))
source(paste(getwd(), "/src/hr/plot/save_EconomicbarPlot.r", sep=""))
source(paste(getwd(), "/src/hr/plot/plotEstimatedPrice.r", sep=""))
source(paste(getwd(), "/src/hr/plot/plotEstimatedPrice_discard.r", sep=""))
source(paste(getwd(), "/src/hr/plot/plotEstimatedVC.r", sep=""))
source(paste(getwd(), "/src/hr/plot/plotEstimatedCC.r", sep=""))
source(paste(getwd(), "/src/hr/plot/plotEstimatedFC.r", sep=""))
source(paste(getwd(), "/src/hr/plot/plotEstimatedLC.r", sep=""))

source(paste(getwd(), "/src/hr/plot/saveObservedvsSimulatedPlot.r", sep=""))
source(paste(getwd(), "/src/hr/plot/saveMEY_Plot.r", sep=""))
#source(paste(getwd(), "/src/hr/plot/saveBiologicalCIplot.r", sep=""))
#source(paste(getwd(), "/src/hr/plot/saveEconomicCIplot.r", sep=""))

source(paste(getwd(), "/src/hr/eval/runEvaluation.r", sep=""))
source(paste(getwd(), "/src/hr/eval/eval_scenarios_bio.r", sep=""))
source(paste(getwd(), "/src/hr/eval/eval_scenarios_by_fleet.r", sep=""))
source(paste(getwd(), "/src/hr/eval/eval_scenarios_eco.r", sep=""))
source(paste(getwd(), "/src/hr/eval/eval_scenarios_bio_forecast.r", sep=""))
source(paste(getwd(), "/src/hr/eval/eval_scenarios_by_fleet_forecast.r", sep=""))
source(paste(getwd(), "/src/hr/eval/eval_scenarios_eco_forecast.r", sep=""))

source(paste(getwd(), "/src/hr/runTrafficLights.r", sep=""))

source(paste(getwd(), "/src/mcda/Functions.R", sep=""))
source(paste(getwd(), "/src/mcda/Run_MCDA.R", sep=""))    

source(paste(getwd(), "/src/utils/iscorrect_answer.r", sep=""))
source(paste(getwd(), "/src/utils/create_folders.r", sep=""))


