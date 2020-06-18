# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




if (!IN_BEMTOOL) {

if (!is.null(FLEETSEGMENTS_names)) {
scenario_fold <<- new_aldForecast@forecast_name
setwd(ALADYM_home)
if (length(scenario_fold ) != 0)  {
dir.create(scenario_fold)
dir_temp <- getwd()
setwd(paste(ALADYM_home, "/",scenario_fold, sep="") )
}
scenario_fold <- paste(scenario_fold, "/", sep="")
  } else {
 scenario_fold <<- "" 
  }
  
dir.create("Tables")
dir.create("Graphs")
dir.create("working files")

tablesDIR <<- paste(ALADYM_home, "/",scenario_fold, "Tables", sep="") 
graphsDIR <<- paste(ALADYM_home, "/",scenario_fold, "Graphs", sep="")
tablesDIR_input <<- tablesDIR
graphsDIR_input <<- graphsDIR

workingfilesDIR <<- paste(ALADYM_home,  "/",scenario_fold, "working files", sep="")

if (scenario_fold == "") { 
dir.create("Sim vs Obs")
simVSobsDIR <<- paste(ALADYM_home,  "/",scenario_fold,"Sim vs Obs", sep="")
}
   dir_temp <- getwd()
    setwd(paste(ALADYM_home,  "/",scenario_fold, "Tables", sep="") )
    dir.create("quantiles")
     setwd(dir_temp)
#ciDIR <<- paste(ALADYM_home, "/Tables/quantiles/", sep="")
suffix_outfiles <<- ""
prefix_outfiles <<- ""

} else {

if (phase == "SIMULATION") {
tablesDIR <<- paste(casestudy_path, "/Diagnosis/Biological Pressure Impact/ALADYM - ", BMT_SPECIES[ALADYM_spe], sep="") 
graphsDIR <<- paste(casestudy_path, "/Diagnosis/Biological Pressure Impact/ALADYM - ", BMT_SPECIES[ALADYM_spe], sep="")
tablesDIR_input <<- paste(casestudy_path, "/Diagnosis/Biological Pressure Impact/ALADYM - ", BMT_SPECIES[ALADYM_spe], "/Input", sep="") 
graphsDIR_input <<- paste(casestudy_path, "/Diagnosis/Biological Pressure Impact/ALADYM - ", BMT_SPECIES[ALADYM_spe], "/Input", sep="")
workingfilesDIR <<- paste(casestudy_path, "/Diagnosis/working files", sep="")
simVSobsDIR <<- paste(casestudy_path, "/Diagnosis/Biological Pressure Impact/ALADYM - ", BMT_SPECIES[ALADYM_spe],"/Sim vs Obs", sep="")
#ciDIR <<- paste(casestudy_path, "/Diagnosis/ALADYM/", BMT_SPECIES[ALADYM_spe],"/Uncertainty", sep="")
suffix_outfiles <<- ""
prefix_outfiles <<- paste(casestudy_name, " - ", sep="")
} else {
	if (!MEY_CALCULATION) {	
tablesDIR <<- paste(casestudy_path, "/", harvest_rule_id, "/Biological Pressure Impact/ALADYM - ", BMT_SPECIES[ALADYM_spe], sep="") 
tablesDIR_input <<- paste(casestudy_path, "/", harvest_rule_id, "/Biological Pressure Impact/ALADYM - ", BMT_SPECIES[ALADYM_spe], "/Input", sep="") 
graphsDIR <<- tablesDIR
graphsDIR_input <<- tablesDIR_input
workingfilesDIR <<- paste(casestudy_path,  "/", harvest_rule_id, "/working files", sep="")
 simVSobsDIR <<- paste(casestudy_path,  "/", harvest_rule_id, "/ALADYM/", BMT_SPECIES[ALADYM_spe],"/Sim vs Obs", sep="")
#ciDIR <<- paste(casestudy_path,  "/", harvest_rule_id, "/ALADYM/", BMT_SPECIES[ALADYM_spe],"/Uncertainty", sep="")
suffix_outfiles <<- paste(" ", harvest_rule_id, sep="")
prefix_outfiles <<- paste(casestudy_name, " - ", sep="")
 } else {
tablesDIR <<- paste(casestudy_path, "/MEY calculation/", harvest_rule_id, "/Biological Pressure Impact/ALADYM - ", BMT_SPECIES[ALADYM_spe], sep="") 
graphsDIR <<- paste(casestudy_path, "/MEY calculation/", harvest_rule_id,  "/Biological Pressure Impact/ALADYM - ", BMT_SPECIES[ALADYM_spe],sep="")
tablesDIR_input <<- paste(casestudy_path, "/MEY calculation/", harvest_rule_id, "/Biological Pressure Impact/ALADYM - ", BMT_SPECIES[ALADYM_spe], "/Input", sep="") 
graphsDIR_input <<- paste(casestudy_path, "/MEY calculation/", harvest_rule_id,  "/Biological Pressure Impact/ALADYM - ", BMT_SPECIES[ALADYM_spe], "/Input",sep="")

workingfilesDIR <<- paste(casestudy_path, "/MEY calculation/", harvest_rule_id, "/working files", sep="")
simVSobsDIR <<- paste(casestudy_path, "/MEY calculation/", harvest_rule_id, "/Biological Pressure Impact/ALADYM - ", BMT_SPECIES[ALADYM_spe],"/Sim vs Obs", sep="")
#ciDIR <<- paste(casestudy_path, "/MEY calculation/", harvest_rule_id, "/ALADYM/", BMT_SPECIES[ALADYM_spe],"/Uncertainty", sep="")
suffix_outfiles <<- paste(" ", harvest_rule_id, sep="")
prefix_outfiles <<- paste(casestudy_name, " - ", sep="")
  }

}
}

#
# 
  
  RPs_logfile <<- paste(tablesDIR, "/", prefix_outfiles, "RPs", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe],".log", sep="") 

## output file paths
CATCH_table <<- paste(tablesDIR, "/", prefix_outfiles, "Catch", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe],".csv", sep="") 
CATCHBYAGE_table <<- paste(tablesDIR, "/", prefix_outfiles, "Catch by age", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe], ".csv", sep="")  
CATCHBYAGE_table_CI <<- paste(tablesDIR, "/quantiles/", prefix_outfiles, "Catch by age C.I.", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe], sep="")          
DISCARD_table <<- paste(tablesDIR, "/", prefix_outfiles, "Discard", suffix_outfiles,  " - ",BMT_SPECIES[ALADYM_spe],".csv", sep="")         
DISCARDBYAGE_table <<- paste(tablesDIR, "/", prefix_outfiles, "Discard by age", suffix_outfiles,  " - ",BMT_SPECIES[ALADYM_spe],".csv", sep="") 
DISCARDBYAGE_table_CI <<- paste(tablesDIR, "/quantiles/", prefix_outfiles, "Discard by age C.I.", suffix_outfiles,   " - ",BMT_SPECIES[ALADYM_spe],sep="") 
FEMALEINPUT_table <<- paste(tablesDIR_input, "/", prefix_outfiles, "Input females", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe], ".csv", sep="")  
INDICATORS_table <<-  paste(tablesDIR, "/", prefix_outfiles, "Indicators", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe], ".csv", sep="") 
INDICATORS_table_CI <<-  paste(tablesDIR, "/quantiles/", prefix_outfiles, "Indicators C.I.", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe], sep="")   
LANDING_table <<- paste(tablesDIR, "/", prefix_outfiles, "Landing", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe], ".csv", sep="")                                        
LANDINGBYAGE_table <<- paste(tablesDIR, "/", prefix_outfiles, "Landing by age", suffix_outfiles,  " - ",BMT_SPECIES[ALADYM_spe],".csv", sep="") 
LANDINGBYAGE_table_CI <<- paste(tablesDIR, "/quantiles/", prefix_outfiles, "Landing by age C.I.", suffix_outfiles,  " - ",BMT_SPECIES[ALADYM_spe],sep="") 
MALEINPUT_table <<- paste(tablesDIR_input, "/", prefix_outfiles, "Input males", suffix_outfiles,  " - ",BMT_SPECIES[ALADYM_spe],".csv", sep="")  
MORTALITY_table <<- paste(tablesDIR, "/", prefix_outfiles, "Mortalities", suffix_outfiles,  " - ",BMT_SPECIES[ALADYM_spe],".csv",sep="")   
MORTALITY_table_CI <<- paste(tablesDIR, "/quantiles/", prefix_outfiles, "Mortalities C.I.", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe],sep="") 
MORTALITYCHANGE_table <<- paste(tablesDIR, "/", prefix_outfiles, "Mortalities change", suffix_outfiles,  " - ",BMT_SPECIES[ALADYM_spe],".csv",sep="") 
MORTALITYCHANGE_table_CI <<- paste(tablesDIR, "/quantiles/", prefix_outfiles, "Mortalities change C.I.", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe],sep="")     
POPULATION_table <<- paste(tablesDIR, "/", prefix_outfiles, "Population", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe], ".csv",sep="") 
POPULATION_table_CI <<- paste(tablesDIR, "/quantiles/", prefix_outfiles, "Population C.I.", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe], sep="")               

InstantSSB_table <<- paste(tablesDIR, "/", prefix_outfiles, "Instant SSB", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe], ".csv",sep="") 
InstantSSB_table_CI <<- paste(tablesDIR, "/", prefix_outfiles, "Instant SSB C.I.", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe],sep="") 

PRODUCTION_table <<- paste(tablesDIR, "/", prefix_outfiles, "Production", suffix_outfiles,  " - ",BMT_SPECIES[ALADYM_spe],".csv", sep="")   
PRODUCTION_table_CI <<- paste(tablesDIR, "/quantiles/", prefix_outfiles, "Production C.I.", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe], sep="")  
ERROR_CI_table <<- paste(tablesDIR, "/quantiles/", prefix_outfiles, "Recruitment error", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe], ".csv", sep="")  
if (!IN_BEMTOOL) {           
REFERENCEPOINTS_table <<- paste(ALADYM_home, "/Tables/Reference points - ",BMT_SPECIES[ALADYM_spe],".csv", sep="")
} else {

REFERENCEPOINTS_table <<- paste(casestudy_path, "/Diagnosis/Biological Pressure Impact/ALADYM - ", BMT_SPECIES[ALADYM_spe],"/", casestudy_name, " - Reference points - ",BMT_SPECIES[ALADYM_spe],".csv", sep="")
}

RECRUITMENT_CAL_table <<-paste(tablesDIR, "/", prefix_outfiles, "Recruitment calibrated", suffix_outfiles,   " - ",BMT_SPECIES[ALADYM_spe],".csv",  sep="")
YpR_table <<- paste(tablesDIR, "/", prefix_outfiles, "Yield Results", suffix_outfiles,  " - ",BMT_SPECIES[ALADYM_spe],".csv", sep="")
F_BYGEAR_table <<- paste(tablesDIR, "/", prefix_outfiles, "Annual F by gear", suffix_outfiles,  " - ",BMT_SPECIES[ALADYM_spe],".csv",sep="")
F_BYGEAR_table_CI <<- paste(tablesDIR, "/quantiles/", prefix_outfiles, "Annual F by gear C.I.", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe],sep="")
DETAILED_DISCARD_table <<-paste(tablesDIR, "/", prefix_outfiles, "Detailed Discard", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe], ".csv", sep="")
DETAILED_DISCARD_table_CI <<-paste(tablesDIR, "/quantiles/", prefix_outfiles, "Detailed Discard C.I.", suffix_outfiles,  " - ",BMT_SPECIES[ALADYM_spe], sep="")
RECRUITMENT_table <<-paste(tablesDIR_input, "/", prefix_outfiles, "Recruitment", suffix_outfiles,   " - ",BMT_SPECIES[ALADYM_spe],".csv",  sep="")
RECRUITMENT_table_CI <<-paste(tablesDIR, "/quantiles/", prefix_outfiles, "Recruitment C.I.", suffix_outfiles,  " - ",BMT_SPECIES[ALADYM_spe], sep="")

# input files from GUI data
F_BYGEAR_INP_table <<- paste(workingfilesDIR, "/", prefix_outfiles, "F by gear", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe], ".csv",sep="")
F_BYGEAR_splitted_CAA_table <<- paste(workingfilesDIR, "/", prefix_outfiles, "F splitted by gear with catch at age", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe], ".csv",sep="")
F_BYGEAR_splitted_P_table <<- paste(workingfilesDIR, "/", prefix_outfiles, "F splitted by gear with production", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe], ".csv",sep="")
DISCARD_BYAGE_EXTERNALVECT_INP_table <<- paste(workingfilesDIR, "/", prefix_outfiles, "Discard by age", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe], ".csv",sep="")

EFFORT_DATA_working_table <<- paste(workingfilesDIR,"/", prefix_outfiles, "Effort data", suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".csv", sep="")
FISHING_COEFF_working_table <<- paste(workingfilesDIR,"/", prefix_outfiles, "Fishing coefficient", suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".csv", sep="")
PRODUCTION_DATA_working_table <<- paste(workingfilesDIR,"/", prefix_outfiles, "Production data", suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".csv", sep="")
P_PRODUCTION_DATA_working_table <<- paste(workingfilesDIR,"/", prefix_outfiles, "P-production", suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".csv", sep="")
DISCARD_DATA_working_table <<- paste(workingfilesDIR,"/", prefix_outfiles, "Discard data", suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".csv", sep="")
PRODUCTION_DATA_byYEAR_working_table <<- paste(workingfilesDIR,"/", prefix_outfiles, "Production data by year", suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".csv", sep="")
SEL_working_tabl <<-  paste(workingfilesDIR,"/", prefix_outfiles, "Selectivity params", suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".csv", sep="")     
FC_working_tabl <<- paste(workingfilesDIR,"/", prefix_outfiles, "Fishing coefficients", suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".csv", sep="")     

CHISQUARED_LANDING_table <<- paste(simVSobsDIR, "/", prefix_outfiles, "Chi-squared Landing", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe], ".csv",sep="")                       
SIMULATEDVSOBSERVED_LANDING_table <<- paste(simVSobsDIR, "/", prefix_outfiles, "Sim. vs Obs. Landing", suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".csv",sep="")
CHISQUARED_DISCARD_table <<- paste(simVSobsDIR, "/", prefix_outfiles, "Chi-squared Discard", suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".csv",sep="")                       
SIMULATEDVSOBSERVED_DISCARD_table <<- paste(simVSobsDIR, "/", prefix_outfiles, "Sim. vs Obs. Discard", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe], ".csv",sep="")

SIMULATEDVSOBSERVED_CATCH_table <<-  paste(simVSobsDIR, "/", prefix_outfiles, "Sim. vs Obs. Catch", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe], ".csv",sep="")
SIMULATEDVSOBSERVED_SUMMARY_table  <<-  paste(simVSobsDIR, "/", prefix_outfiles, "Sim. vs Obs. Catch-Landing-Discard", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe], ".csv",sep="")

#CI_Biomass_exploited_table <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Biomass exploited", suffix_outfiles, ".csv", sep="")
#CI_Biomass_unexploited_table <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Biomass unexploited", suffix_outfiles,".csv", sep="")
#CI_SSB_exploited_table <<- paste(ciDIR, "/CI - ", prefix_outfiles, "SSB exploited", suffix_outfiles,".csv", sep="")
#CI_SSB_unexploited_table <<- paste(ciDIR, "/CI - ", prefix_outfiles, "SSB unexploited", suffix_outfiles,".csv", sep="")
#CI_MeanLength_exploited_table <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Mean length exploited", suffix_outfiles,".csv", sep="")
#CI_MeanLength_unexploited_table <<- paste(ciDIR, "/CI - ", prefix_outfiles, "CI Mean length unexploited", suffix_outfiles,".csv", sep="")
#CI_Rec_table <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Recruitment", suffix_outfiles,".csv", sep="")
#CI_Rec_table_year <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Recruitment by year", suffix_outfiles,".csv", sep="")
#CI_Catch_overall_table <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Catch", suffix_outfiles,".csv", sep="")
#CI_MeanLength_catch_overall_table <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Mean length catch", suffix_outfiles,".csv", sep="")
#CI_Landing_overall_table <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Landing", suffix_outfiles,".csv", sep="")
#CI_MeanLength_landing_overall_table <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Mean length landing", suffix_outfiles,".csv", sep="")
#CI_Discard_overall_table <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Discard", suffix_outfiles,".csv", sep="")
#CI_MeanLength_discard_overall_table <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Mean length discard", suffix_outfiles,".csv", sep="")
#
#CI_SSB_allruns_exploited_table <<- paste(ciDIR, "/CI - ", prefix_outfiles, "SSB exploited all runs", suffix_outfiles,".csv", sep="")
#
#CI_Landingbyfleet_table <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Landing by fleet", suffix_outfiles,".csv", sep="")
#CI_MeanLengthLandingbyfleet_table <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Mean length Landing by fleet", suffix_outfiles,".csv", sep="")
#CI_Discardbyfleet_table <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Discard by fleet", suffix_outfiles,".csv", sep="")
#CI_MeanLengthDiscardbyfleet_table <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Mean length Discard by fleet", suffix_outfiles,".csv", sep="")
#CI_Yieldbyfleet_table <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Yield by fleet", suffix_outfiles,".csv", sep="")
#CI_MeanLengthYieldbyfleet_table <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Mean length Yield by fleet", suffix_outfiles,".csv", sep="")
#
if (IN_BEMTOOL) {
MORTALITYCHANGEALLSPECIES_table <<-paste(casestudy_path, "/", harvest_rule_id, "/Biological Pressure Impact/", prefix_outfiles, " Avg. mortalities reductions", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe], ".csv",sep="") 
}



MEANAGE_graph <<- paste(graphsDIR, "/", prefix_outfiles, "MA ", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe], ".jpg", sep="") 
MEANAGEINCATCHBYGEAR_graph <<- paste(graphsDIR, "/", prefix_outfiles, "MA in CATCH by gear" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], sep="")
MEANAGEINLANDINGBYGEAR_graph <<-  paste(graphsDIR, "/", prefix_outfiles, "MA in LANDING by gear" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], sep="")
MEANAGEINDISCARDBYGEAR_graph <<- paste(graphsDIR, "/", prefix_outfiles, "MA in DISCARD by gear", suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], sep="")
F_BYGEAR_graph <<- paste(graphsDIR, "/", prefix_outfiles, "F by gear", suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], sep="")  
F_BYGEAR_graph_grouped <<- paste(graphsDIR, "/", prefix_outfiles, "F by gear", suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], sep="")  
MORTALITIES_Z_graph <<- paste(graphsDIR, "/", prefix_outfiles, "Mortality estimations", suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".jpg", sep="") 
MORTALITIES_F_graph <<- paste(graphsDIR, "/", prefix_outfiles, "Mortality estimations F", suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".jpg", sep="") 
MEANLENGTH_graph <<- paste(graphsDIR, "/", prefix_outfiles, "ML", suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".jpg", sep="") 
MEANLENGTH_graph_expl <<- paste(graphsDIR, "/", prefix_outfiles, "ML pop expl", suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".jpg", sep="") 
MEANLENGTH_graph_unexpl <<- paste(graphsDIR, "/", prefix_outfiles, "ML pop unexpl", suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".jpg", sep="") 
MEANLENGTH_graph_SS_expl <<- paste(graphsDIR, "/", prefix_outfiles, "ML pop SS expl", suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".jpg", sep="") 
MEANLENGTH_graph_SS_unexpl <<- paste(graphsDIR, "/", prefix_outfiles, "ML pop SS unexpl", suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".jpg", sep="") 
MEANLENGTHINCATCHBYGEAR_graph_grouped <<- paste(graphsDIR, "/", prefix_outfiles, "ML in Catch by gear" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], sep="") 
MEANLENGTHINLANDINGBYGEAR_graph_grouped <<- paste(graphsDIR, "/", prefix_outfiles, "ML in Landing by gear" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], sep="")
MEANLENGTHINDISCARDBYGEAR_graph_grouped <<- paste(graphsDIR, "/", prefix_outfiles, "ML in Discard by gear" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], sep="")
MEANLENGTHINCATCHBYGEAR_graph <<- paste(graphsDIR, "/", prefix_outfiles, "ML in Catch by gear" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], sep="") 
MEANLENGTHINLANDINGBYGEAR_graph <<- paste(graphsDIR, "/", prefix_outfiles, "ML in Landing by gear" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], sep="")
MEANLENGTHINDISCARDBYGEAR_graph <<- paste(graphsDIR, "/", prefix_outfiles, "ML in Discard by gear" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], sep="")

YIELDBYGEAR_graph_grouped <<- paste(graphsDIR, "/", prefix_outfiles, "Catch by gear" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], sep="")
LANDINGBYGEAR_graph_grouped <<- paste(graphsDIR, "/", prefix_outfiles, "Landing by gear" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], sep="")
DISCARDBYGEAR_graph_grouped <<- paste(graphsDIR, "/", prefix_outfiles, "Discard by gear" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], sep="")
YIELDBYGEAR_graph <<- paste(graphsDIR, "/", prefix_outfiles, "Catch by gear" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], sep="")
LANDINGBYGEAR_graph <<- paste(graphsDIR, "/", prefix_outfiles, "Landing by gear" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], sep="")
DISCARDBYGEAR_graph <<- paste(graphsDIR, "/", prefix_outfiles, "Discard by gear" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], sep="")

INPUT_graph <<- paste(graphsDIR_input, "/", prefix_outfiles, "Input parameters" , suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe],".jpg", sep="")
SELECTIVITY_graph <<- paste(graphsDIR_input, "/", prefix_outfiles, "Input Selectivity" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], sep="")
RECRUITMENT_graph <<- paste(graphsDIR_input, "/", prefix_outfiles, "Recruitment" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe],".jpg", sep="")

RPs_graph <<- paste(graphsDIR, "/", prefix_outfiles, "Reference points - ",BMT_SPECIES[ALADYM_spe],".jpg", sep="")
BIOMASSES_graph <<- paste(graphsDIR, "/", prefix_outfiles, "Biomasses" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".jpg", sep="")
BIOMASSES_graph_biomass_expl <<- paste(graphsDIR, "/", prefix_outfiles, "Biomass expl" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".jpg", sep="")
BIOMASSES_graph_biomass_unexpl <<- paste(graphsDIR, "/", prefix_outfiles, "Biomass unexpl" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".jpg", sep="")
BIOMASSES_graph_SSB_expl <<- paste(graphsDIR, "/", prefix_outfiles, "SSB expl" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".jpg", sep="")
BIOMASSES_graph_SSB_unexpl <<- paste(graphsDIR, "/", prefix_outfiles, "SSB unexpl" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".jpg", sep="")
BIOMASSES_graph_SSnumber_expl <<- paste(graphsDIR, "/", prefix_outfiles, "SS number expl" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".jpg", sep="")
BIOMASSES_graph_SSnumber_unexpl <<- paste(graphsDIR, "/", prefix_outfiles, "SS number unexpl" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".jpg", sep="")
PRODUCTION_graph <<- paste(graphsDIR, "/", prefix_outfiles, "Production" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".jpg", sep="")
PRODUCTION_graph_death_biomass <<- paste(graphsDIR, "/", prefix_outfiles, "Death biomass" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".jpg", sep="")
PRODUCTION_graph_bio_production <<- paste(graphsDIR, "/", prefix_outfiles, "Biological production" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".jpg", sep="")

INDICATORS_graph <<- paste(graphsDIR, "/", prefix_outfiles, "Indicators" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".jpg", sep="")
INDICATORS_graph_SPR <<- paste(graphsDIR, "/", prefix_outfiles, "SPR" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".jpg", sep="")
INDICATORS_graph_DR <<- paste(graphsDIR, "/", prefix_outfiles, "Discard Ratio" , suffix_outfiles," - ",BMT_SPECIES[ALADYM_spe], ".jpg", sep="")

SIMULATEDVSOBSERVED_LANDING_graph <<- paste(simVSobsDIR, "/", prefix_outfiles, "Sim. vs Obs. Landing", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe], sep="")
SIMULATEDVSOBSERVED_DISCARD_graph <<- paste(simVSobsDIR, "/", prefix_outfiles, "Sim. vs Obs. Discard", suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe], sep="")

F01_graph <<- paste(graphsDIR, "/", prefix_outfiles, "F01 curve" , suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe],".jpg", sep="")
F02_graph <<- paste(graphsDIR, "/", prefix_outfiles, "F02 curve" , suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe], ".jpg", sep="")
SSB_graph <<- paste(graphsDIR, "/", prefix_outfiles, "SSB curve" , suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe],".jpg", sep="")
B_graph <<- paste(graphsDIR, "/", prefix_outfiles, "BIOMASS curve" , suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe],".jpg", sep="")

RECRUITMENT_CAL_graph <<-paste(graphsDIR, "/", prefix_outfiles, "Comparison calibration", suffix_outfiles,   " - ",BMT_SPECIES[ALADYM_spe],".jpg",  sep="")
KOBE_graph <<- paste(graphsDIR, "/", prefix_outfiles, "KOBE plot" , suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe],".jpg", sep="")
Uncertainty_table <<- paste(tablesDIR, "/", prefix_outfiles, "Uncertainty table" , suffix_outfiles, " - ",BMT_SPECIES[ALADYM_spe],".csv", sep="")

#CI_Biomass_exploited_graph <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Biomass exploited", suffix_outfiles,".jpg", sep="")
#CI_Biomass_unexploited_graph <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Biomass unexploited", suffix_outfiles,".jpg", sep="")
#CI_SSB_exploited_graph <<- paste(ciDIR, "/CI - ", prefix_outfiles, "SSB exploited", suffix_outfiles,".jpg", sep="")
#CI_SSB_unexploited_graph <<- paste(ciDIR, "/CI - ", prefix_outfiles, "SSB unexploited", suffix_outfiles, ".jpg",sep="")
#CI_MeanLength_exploited_graph <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Mean length exploited", suffix_outfiles,".jpg", sep="")
#CI_MeanLength_unexploited_graph <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Mean length unexploited", suffix_outfiles,".jpg", sep="")
#CI_Rec_graph <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Recruitment", suffix_outfiles, ".jpg",sep="")
#CI_Catch_overall_graph <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Catch", suffix_outfiles,".jpg", sep="")
#CI_MeanLength_Catch_overall_graph <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Mean length catch", suffix_outfiles, ".jpg",sep="")
#CI_Landing_overall_graph <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Landing", suffix_outfiles,".jpg", sep="")
#CI_MeanLength_Landing_overall_graph <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Mean length landing", suffix_outfiles, ".jpg",sep="")
#CI_Discard_overall_graph <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Discard", suffix_outfiles,".jpg", sep="")
#CI_MeanLength_Discard_overall_graph <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Mean length discard", suffix_outfiles, ".jpg",sep="")
#

#CI_Landingbyfleet_graph <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Landing by fleet", suffix_outfiles, sep="")
#CI_Mean_Length_Landingbyfleet_graph <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Mean length Landing by fleet", suffix_outfiles, sep="")
#CI_Discardbyfleet_graph <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Discard by fleet", suffix_outfiles, sep="")
#CI_Mean_Length_Discardbyfleet_graph <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Mean length Discard by fleet", suffix_outfiles, sep="")
#CI_Yieldbyfleet_graph <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Yield by fleet", suffix_outfiles, sep="")
#CI_Mean_Length_Yieldbyfleet_graph <<- paste(ciDIR, "/CI - ", prefix_outfiles, "Mean length Yield by fleet", suffix_outfiles, sep="")