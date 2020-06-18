# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




# source code  
if (!IN_BEMTOOL) {
require(RGtk2) 
require(scales)
require(ALADYMTools)
}

#source(paste(ALADYM_home, "/src/catches_by_age.r", sep=""))
#source(paste(ALADYM_home, "/src/catches_by_age_CI.r", sep=""))
source(paste(ALADYM_home, "/src/DISCARDS.r", sep="")) 
source(paste(ALADYM_home, "/src/DISCARDS_all.r", sep="")) 
source(paste(ALADYM_home, "/src/Discardvector.r", sep=""))
source(paste(ALADYM_home, "/src/Discardvector_all.r", sep=""))  
source(paste(ALADYM_home, "/src/Export.r", sep=""))
source(paste(ALADYM_home, "/src/export_tables.r", sep=""))
source(paste(ALADYM_home, "/src/export_tables_CI.r", sep=""))
source(paste(ALADYM_home, "/src/export_tab_mort.r", sep=""))
source(paste(ALADYM_home, "/src/export_tab_mort_CI.r", sep=""))
#source(paste(ALADYM_home, "/src/firstStepF.r", sep=""))
#source(paste(ALADYM_home, "/src/firstStepF_entrataF.r", sep=""))
#source(paste(ALADYM_home, "/src/firstStepM_entrataF.r", sep=""))
#source(paste(ALADYM_home, "/src/firstStepM.r", sep="")) 
#source(paste(ALADYM_home, "/src/FORECAST.r", sep=""))
#source(paste(ALADYM_home, "/src/FORECAST_entrataF.r", sep=""))
source(paste(ALADYM_home, "/src/indicators.r", sep=""))
source(paste(ALADYM_home, "/src/indicators_CI.r", sep=""))
source(paste(ALADYM_home, "/src/logistic.r", sep="")) 
#source(paste(ALADYM_home, "/src/PRELIFE_EXPLOITED.r", sep=""))
#source(paste(ALADYM_home, "/src/PRELIFE_EXPLOITED_entrataF.r", sep=""))
#source(paste(ALADYM_home, "/src/PRELIFE_UNEXPLOITED.r", sep=""))  
source(paste(ALADYM_home, "/src/REDUCTION.r", sep=""))
source(paste(ALADYM_home, "/src/RFSS.r", sep=""))
source(paste(ALADYM_home, "/src/RunModel.r", sep=""))
source(paste(ALADYM_home, "/src/Sample_a_random.r", sep=""))
source(paste(ALADYM_home, "/src/SGEAR.r", sep=""))  
#source(paste(ALADYM_home, "/src/SGEAR_ext_vec.r", sep="")) 
source(paste(ALADYM_home, "/src/SGEAR_ext_vec_len.r", sep="")) 
source(paste(ALADYM_home, "/src/SGEAR_ext_vec_age.r", sep="")) 
#source(paste(ALADYM_home, "/src/simulation_exploited.r", sep=""))
#source(paste(ALADYM_home, "/src/simulation_exploited_entrataF.r", sep=""))
#source(paste(ALADYM_home, "/src/simulation_unexploited.r", sep=""))
source(paste(ALADYM_home,"/src/fact_calc.r",sep="") )
source(paste(ALADYM_home,"/src/P_production_calc.r",sep="") )
source(paste(ALADYM_home, "/src/input_tables.r", sep=""))
source(paste(ALADYM_home, "/src/Fvector.r", sep=""))
source(paste(ALADYM_home, "/src/updateF.r", sep=""))
source(paste(ALADYM_home, "/src/updateF.int.r", sep=""))

#source(paste(ALADYM_home, "/src/Annual_Z_Sinclair.r", sep=""))
#source(paste(ALADYM_home, "/src/Annual_Z.r", sep=""))
#source(paste(ALADYM_home, "/src/Annual_F_weighted.r", sep=""))
#source(paste(ALADYM_home, "/src/Annual_F.r", sep=""))
#source(paste(ALADYM_home, "/src/Annual_F_CI.r", sep=""))
#source(paste(ALADYM_home, "/src/Annual_F_by_gear.r", sep=""))
#source(paste(ALADYM_home, "/src/calibration.r", sep=""))
# functions to create plots
#source(paste(ALADYM_home, "/src/Plot_age.r", sep=""))
source(paste(ALADYM_home, "/src/Plot_F.r", sep=""))
source(paste(ALADYM_home, "/src/Plot_F_allFleets.r", sep=""))
source(paste(ALADYM_home, "/src/PlotInput.r", sep=""))
source(paste(ALADYM_home, "/src/PlotRecruitment.r", sep=""))
source(paste(ALADYM_home, "/src/Plot_length.r", sep=""))
source(paste(ALADYM_home, "/src/Plot_length_allFleets.r", sep=""))
source(paste(ALADYM_home, "/src/Plot_mortalities.r", sep=""))
source(paste(ALADYM_home, "/src/Plot_yield.r", sep=""))
source(paste(ALADYM_home, "/src/Plot_yield_allFleets.r", sep=""))
source(paste(ALADYM_home, "/src/PlotYear.r", sep=""))
source(paste(ALADYM_home, "/src/PlotYear_allFleets.r", sep=""))
source(paste(ALADYM_home, "/src/PlotRPs.r", sep=""))

# UTILITIES functions 
source(paste(ALADYM_home, "/src/utilities/meanWequals.r", sep=""))
#source(paste(ALADYM_home, "/src/utilities/leggi_input.r", sep=""))
source(paste(ALADYM_home, "/src/utilities/modulo.r", sep=""))
source(paste(ALADYM_home, "/src/utilities/sumWequals.r", sep=""))
source(paste(ALADYM_home, "/src/utilities/quantiles.r", sep=""))
source(paste(ALADYM_home, "/src/utilities/ifLandingObl.r", sep=""))

# source(paste(ALADYM_home, "/interconn.bmt/add_BMT_fleet.r", sep=""))


source(paste(ALADYM_home, "/src/logistic.r", sep=""))
source(paste(ALADYM_home, "/src/get_grid.r", sep=""))
source(paste(ALADYM_home, "/src/get_grid_growth.r", sep=""))
source(paste(ALADYM_home, "/src/get_grid_mortality.r", sep=""))
source(paste(ALADYM_home, "/src/get_grid_maturity.r", sep=""))
source(paste(ALADYM_home, "/src/Gislason_M.r", sep=""))
source(paste(ALADYM_home, "/src/ProdbiomUnSol.r", sep=""))
source(paste(ALADYM_home, "/src/get_grid_selectivity.r", sep=""))
