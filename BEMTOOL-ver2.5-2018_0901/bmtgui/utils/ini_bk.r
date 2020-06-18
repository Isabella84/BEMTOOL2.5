# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



EX_DIAGNOSIS <<- FALSE
EX_SCENARIO <<- FALSE
removing <<- F
   
bmt_combo_fleetsegments_effort_r4 <<- gtkComboBoxNewText()

bmt_combo_fleetsegments_MEY_r8 <<- gtkComboBoxNewText()

#bmt_forecast_executed_scenarios <<- gtkComboBoxNewText()


SCENARIO_TO_LOAD_FROM_MENU <<- ""

err_wnd_ts <<- NULL

ASSESSMENT_TOOLS <<- c("VIT", "XSA", "SURBA", "from Report", "none")

#BMTCFG_SUBTABLES <<- c("mat_cfg_general", "mat_cfg_F", "mat_cfg_S", "mat_cfg_SF", "mat_cfg_species_settings", "mat_cfg_assessment_tools", "mat_cfg_ALADYM_sim", "mat_cfg_general_fore",  "mat_cfg_EffortData", "mat_cfg_LandingData", "mat_cfg_REF_points", "mat_cfg_price", "mat_cfg_varCosts", "mat_cfg_labCosts", "mat_cfg_fixCosts", "mat_cfg_capCosts", "mat_cfg_FleetDyn", "mat_cfg_FleetAct", "mat_cfg_TechProgress", "mat_cfg_EconomicIndicator")

BMTCFG_SUBTABLES_SIMULATION <<- c("mat_cfg_general", "mat_cfg_F", "mat_cfg_S", "mat_cfg_SF", "mat_cfg_species_settings", "mat_cfg_assessment_tools", "mat_cfg_ALADYM_sim",   "mat_cfg_EffortData", "mat_cfg_LandingData", "mat_cfg_REF_points", "mat_cfg_general_fore") # , "mat_cfg_EconomicIndicator_simu"
                                                                                              # "bmt_fleet.price_elast_landing",   "bmt_fleet.price_correction_fact",
BMTCFG_SUBTABLES_ECONOMIC_PARAMS <<- c("bmt_fleet.price_elast_landing_byfleet",  "bmt_fleet.price_elast_import", "bmt_fleet.price_elast_MW", "bmt_fleet.price_importweight",  "bmt_fleet.cost_variable", "bmt_fleet.cost_fuelprice", "bmt_fleet.cost_fixed", "bmt_fleet.cost_capital", "bmt_fleet.cost_crew_minwage", "bmt_fleet.behav_dyn", "bmt_fleet.behav_act", "bmt_fleet.behav_progr", "bmt_fleet.ecoind", "bmt_fleet.indic_taxes", "bmt_fleet.indic_subsidies", "bmt_fleet.labour_fuel", "bmt_fleet.labour_commercial", "bmt_fleet.labour_others", "bmt_fleet.labour_sorting")

BMTCFG_SUBTABLES_ECONOMIC_FORECAST <<- c("mat_cfg_price", "mat_cfg_varCosts", "mat_cfg_labCosts", "mat_cfg_fixCosts", "mat_cfg_capCosts", "mat_cfg_FleetDyn", "mat_cfg_FleetAct", "mat_cfg_TechProgress", "mat_cfg_EconomicIndicator")

COSTS_vector <<- c("casestudy.fuelcosts", "casestudy.commercialcosts", "casestudy.othervariablecosts", "casestudy.totalvariablecosts", "casestudy.maintenancecosts", "casestudy.otherfixedcosts", "casestudy.essentialcosts", "casestudy.avoidablemaintenancecosts", "casestudy.unavoidablemaintenancecosts", "casestudy.totalfixedcosts" , "casestudy.labourcosts", "casestudy.depreciationcosts", "casestudy.opportunitycosts", "casestudy.totalcapitalcosts")
COSTS_vector_names <<- c(" FUEL costs ", " COMMERCIAL costs ", " other VARIABLE costs " , " total VARIABLE costs ", " MAINTENANCE costs ", " other FIXED costs ", " ESSENTIAL costs ", " avoidable MAINTENANCE costs ", " unavoidable MAINTENANCE costs ", " total FIXED costs " , " LABOUR costs ", " DEPRECIATION costs ", " OPPORTUNITY costs ", " total CAPITAL costs ")

OTHERS_vector <<- c("casestudy.maxavgseadays", "casestudy.totallandings", "casestudy.totalrevenues", "casestudy.otherincome", "casestudy.employment", "casestudy.capitalvalue")
OTHERS_vector_names <<- c(" max avg SEA DAYS ", " total LANDINGS ", " total REVENUES ", " other INCOME ", " EMPLOYMENT ", " CAPITAL value ")

COEFFICIENT_VARCOST_BIRDMOD_names <<- c(" fuel cost per unit of effort (a) ", " commercial cost per unit of landings (b) ", " other variable costs per unit of effort (c) ") # , " ice cost coefficient "
COEFFICIENT_VARCOST_MEFISTO_names <<- c(" fuel consumption per unit of effort (a) ", " commercial cost per unit of revenues (b) ", " other variable costs per unit of effort (c) ", " ice cost per unit of effort (d) ") 
COEFFICIENT_VARCOST_FISHRENT_names <<- c(" fuel consumption per unit of effort (a) ", " other variable costs per unit of revenues (b) ") 
COEFFICIENT_VARCOST_BEMMFISH_names <<- c(" total variable costs per unit of effort (a) ") 

COEFFICIENT_FIXEDCOST_BIRDMOD_names <<- c(" maintenance costs per unit of GT (a) ", " other fixed costs per unit of GT (b) ") 
COEFFICIENT_FIXEDCOST_MEFISTO_names <<- c(" unavoidable manintenance costs per vessel (a) ", " essential costs per vessel (b) ", " avoidable manintenance costs per vessel (c) ") 
COEFFICIENT_FIXEDCOST_FISHRENT_names <<- c(" total fixed costs per vessel (a) ") 

COEFFICIENT_CAPITALCOST_BIRDMOD_names <<- c(" depreciation costs per unit of GT (a) ", " interest costs per unit of GT (b) ") 
COEFFICIENT_CAPITALCOST_MEFISTO_names <<- c(" depreciation costs per unit of capital value (a) ", " interest costs per unit of capital value (b) ") 
COEFFICIENT_CAPITALCOST_FISHRENT_names <<- c(" capital costs per vessel (a) ") 

  VARCOSTS_head <<- COEFFICIENT_VARCOST_BIRDMOD_names
    FIXEDCOSTS_head <<- COEFFICIENT_FIXEDCOST_BIRDMOD_names
      CAPITALCOSTS_head <<- COEFFICIENT_CAPITALCOST_BIRDMOD_names

LABOURCOST_head <<- c(" crew share ", " min national wage ") 
BEHAV_DYN_head <<- c(" lower limit for investments ", " upper limit for investments ", " share of profit dedicated to investments ")
BEHAV_ACT_head <<- c(" lower limit ", " upper limit ", " profitability share to change ")
BEHAV_PROGR_head <<- c(" initial technological progress ", " fraction of technological progress varying with time ", " proportion of capital invested in technology ")
#ECOIND_head <<- c(" landing correction factor ", " revenues correction factor u ", " value of a vessel ", " average crew ", " discount rate ", " revenues correction factor v ", " type of revenues * ")

ECOIND_head <<- c(" landings option * "," landings correction factor (ll) ", " revenues correction factor (rr) ",  " landing correction factor (v) ", " landing correction factor (u) ", " value of a vessel ", " average crew ", " discount rate ")

PRICE_MODELS <<- list(model_name=c(1,2,3,4,5,6) , option_name=c("Option 1", "Option 2", "Option 3", "Option 4", "Option 5", "Option 6"))
PRICE_MODELS_DISCARD <<- list(model_name=c(1,2,3) , option_name=c("Option 1", "Option 2", "Option 3"))
VARCOST_MODELS <<- list(model_name=c(1,2,3,4) , option_name=c("Option 1", "Option 2", "Option 3", "Option 4"))
FIXEDCOST_MODELS <<- list(model_name=c(1,2,3) , option_name=c("Option 1", "Option 2", "Option 3"))
CAPITALCOST_MODELS <<- list(model_name=c(1,2,3) , option_name=c("Option 1", "Option 2", "Option 3"))

#PRICE_MODELS <<- list(model_name=c(1,2,3,4,5) , option_name=c("Option 1 (ex-BIRDMOD)", "Option 2 (ex-MEFISTO)", "Option 3 (ex-FISHRENT)", "Option 4 (ex-BEMMFISH)", "Option 5 (costant fun.)"))
#VARCOST_MODELS <<- list(model_name=c(1,2,3,4) , option_name=c("Option 1 (ex-BIRDMOD)", "Option 2 (ex-MEFISTO)", "Option 3 (ex-FISHRENT)", "Option 4 (ex-BEMMFISH)"))
#FIXEDCOST_MODELS <<- list(model_name=c(1,2,3) , option_name=c("Option 1 (ex-BIRDMOD)", "Option 2 (ex-MEFISTO)", "Option 3 (ex-FISHRENT)"))
#CAPITALCOST_MODELS <<- list(model_name=c(1,2,3) , option_name=c("Option 1 (ex-BIRDMOD)", "Option 2 (ex-MEFISTO)", "Option 3 (ex-FISHRENT)"))
#


BMT_SPECIES <<- c()
BMT_FLEETSEGMENTS <<- c()
BMT_YEARS_SIMULATION <<- c()
BMT_YEARS_FORECAST <<- c()

mat_cfg_F <<- data.frame(matrix(nrow=0, ncol=2))
colnames(mat_cfg_F) <- c("", 	"[code]")

mat_cfg_S <<- data.frame(matrix(nrow=0, ncol=2))
colnames(mat_cfg_S) <- c("", 	"[species]")

mat_cfg_SF <<- data.frame(matrix(nrow=0, ncol=0))

mat_cfg_general <<- data.frame(matrix( nrow=5, ncol=3))
colnames(mat_cfg_general) <- c("", 	"[case name]", "[store path]")
mat_cfg_general[,1] <- c( "casestudy.name", "casestudy.stockno", "casestudy.fleetsegmentno", "casestudy.startsimulation", "casestudy.endsimulation")	

mat_cfg_general_fore <<- data.frame(matrix( nrow=2, ncol=2))
mat_cfg_general_fore[,1] <- c( "casestudy.startforecast", "casestudy.endforecast")	

mat_cfg_species_settings <<- data.frame(matrix( nrow=0, ncol=20))
 colnames(mat_cfg_species_settings) <- c("", "[female.lifespan]", "[male.lifespan]", "[sexratio]", "[a.female]", "[b.female]", "[a.male]", "[b.male]", "[t0.female]", "[k.female]", "[linf.female]", "[t0.male]", "[k.male]", "[linf.male]", "[l50.female]", "[matrange.female]", "[l50.male]", "[matrange.male]", "[natural mortality constant]", "[stock recruitment relationship]")


mat_cfg_ALADYM_sim  <<- data.frame(matrix( nrow=0, ncol=3))
colnames(mat_cfg_ALADYM_sim) <- c("", "[ALADYM simulation]",	"[Average years for RP and forecast]")

mat_cfg_REF_points  <<- data.frame(matrix( nrow=0, ncol=4))
colnames(mat_cfg_REF_points) <- c("","[RP ALADYM calculation]",	"[RP ALADYM use]", "[external table RPs]")

mat_cfg_none  <<- data.frame(matrix( nrow=2, ncol=2))
mat_cfg_none[1,] <- c(	"", "[tool]")
mat_cfg_none[2,] <- c(	"casestudy.S1.StockAssessmentTool", "none")

mat_cfg_XSA  <<- data.frame(matrix( nrow=4, ncol=6))
mat_cfg_XSA[1,] <- c(	"", "[tool]", "", "", "",	"[XSA.nb_age_classes]")
mat_cfg_XSA[2,] <- c(	"casestudy.S1.StockAssessmentTool", "XSA", "", "", "",	"")
mat_cfg_XSA[3,] <- c(	"", "[file 2007-2011]",	"[input catches 2007-2011]",	"[output F 2007-2011]",	"[reference points]", "")
mat_cfg_XSA[4,] <- c("casestudy.S1.StockAss.fileC", rep("", 5))

mat_cfg_VIT  <<- data.frame(matrix( nrow=6, ncol=7))
mat_cfg_VIT[1,] <- c(	"", "[tool]", "[VIT.sex]",	"[VIT.length]",	"[VIT.discard]",	"[minAge]",	"[maxAge]")
mat_cfg_VIT[2,] <- c(	"casestudy.S1.StockAssessmentTool", "VIT", "", "", "",	"", "")
mat_cfg_VIT[3,] <- c(	"", "[file-2007]",	"[file-2008]",	"[file-2009]",	"[file-2010]",	"[file-2011]", "[file-2012]")
mat_cfg_VIT[4,] <- c("casestudy.S1.StockAss.fileF", rep("", 6))
mat_cfg_VIT[5,] <- c("casestudy.S1.StockAss.fileM", rep("", 6))
mat_cfg_VIT[6,] <- c("casestudy.S1.StockAss.fileC", rep("", 6))


mat_cfg_externalReport  <<- data.frame(matrix( nrow=4, ncol=6))
mat_cfg_externalReport[1,] <- c(	"", "[tool]", "", "", "",	"[Report.nb_age_classes]")
mat_cfg_externalReport[2,] <- c(	"casestudy.S1.StockAssessmentTool", "Report", "", "", "",	"")
mat_cfg_externalReport[3,] <- c(	"", "[file 2007-2011]",	 "",  "", "", "")
mat_cfg_externalReport[4,] <- c("casestudy.S1.StockAss.fileC", rep("", 5))

mat_cfg_SURBA  <<- data.frame(matrix( nrow=4, ncol=3))
mat_cfg_SURBA[1,] <- c(	"", "[tool]", "[SURBA.averageYears]")
mat_cfg_SURBA[2,] <- c(	"casestudy.S1.StockAssessmentTool", "SURBA", "")
mat_cfg_SURBA[3,] <- c(	"", "[file 2007-2011]",	 "")
mat_cfg_SURBA[4,] <- c("casestudy.S1.StockAss.fileC","","")

mat_cfg_XSA_list <<- list()
mat_cfg_VIT_list <<- list()
mat_cfg_externalReport_list <<- list()
mat_cfg_SURBA_list <<- list()
mat_cfg_none_list <<- list()


mat_cfg_assessment_tools <<-list()

matrix_VITpath <<- NULL
VITpaths_maless <<- NULL
VITpaths_femaless <<- NULL
VITpaths_combineds <<- NULL

EFFORT_NUMBER_list <<- list()
EFFORT_DAY_list <<- list()
EFFORT_GT_list <<- list()
EFFORT_KW_list <<- list()

EFFORT_NUMBER_list_fore <<- list()
EFFORT_DAY_list_fore <<- list()

LANDING_list_all <<- list()   # list of list  of matrices
LANDING_list <<- list()   # list of matrices

bmt_fleet.KW <<- NULL
bmt_fleet.GT <<- NULL
bmt_fleet.NUMBER <<- NULL
bmt_fleet.DAY <<- NULL
bmt_fleet.LANDING <<- NULL

mat_cfg_EffortData  <<- data.frame(matrix( nrow=4, ncol=5))
mat_cfg_EffortData[1,] <- c("", "[Monthly VESSELS file]",	"[Monthly DAYS.average file]",	"[Monthly GT.average file]",	"[Monthly KW.average file]")
mat_cfg_EffortData[3,] <- c("", "[Economic data file]", "", "","")

mat_cfg_EffortData[is.na(mat_cfg_EffortData)] <- ""

mat_cfg_LandingData  <<- data.frame(matrix( nrow=2, ncol=2))
mat_cfg_LandingData[1,] <- c("", "[S1 production file]")
mat_cfg_LandingData[2,] <- c("casestudy.TimeSeries.productionData", "")

mat_cfg_LandingData[is.na(mat_cfg_LandingData)] <- ""

# mat_cfg_price

price_cfg_names <<- c( "[elast. coeff. price-landings or costant price-landings]", "[elast. coeff. price-landings or correction factor]", "[elast. coeff. price-mean weight]", "[elast. coeff. price-discards or costant price-discards]")
varcost_cfg_names <<- c("[fuel cost or fuel use coeff or total var cost coeff]", "[commercial cost coeff]", "[other var costs coeff]", "[ice cost coeff]")
fixcost_cfg_names <<- c("[maintenance or unavoidable cost or total costs coeff]","[other fix cost or essential costs coeff]","[maintenance avoidable cost coeff]")
labcost_cfg_names <<- c("[crew share]",	"[fuel cost option]",	"[commercial cost option]",	"[other var costs option]",	"[min national wage]",	"[sorting coefficient]")
capcost_cfg_names <<- c("[depreciation cost or capital costs coeff]", "[interest (opportunity) cost coeff]")
fleedyn_cfg_names <<- c("[lower limit]", "[upper limit]", "[profitability share to change]")
fleeact_cfg_names <<- c("[lower limit]", "[upper limit]", "[profitability share to change]")
techprog_cfg_names <<- c("[initial tech progress coeff]" ,"[fraction varying with time]" ,"[proportion of capital invested in tech]")
#indicator_cfg_names <<- c("[correction factor for landings]", "[correction factor for revenues]", "[value of a single vessel]", "[average employees per vessel]", "[discount rate]", "[correction factor for revenues]",	"[type revenues]")
indicator_cfg_names <<- c("[type landings-revenues]", "[landings correction factor for opt 1]", "[revenues correction factor for opt 1]",  "[landing coefficient u for opt 2-3]", "[landing coefficient v for opt 2-3]", "[value of a single vessel]", "[average employees per vessel]", "[discount rate]")


mat_cfg_price  <<- data.frame(matrix("", nrow=3, ncol=(length(price_cfg_names)+1)), stringsAsFactors=F)
mat_cfg_price[1,] <- c("",price_cfg_names)
mat_cfg_varCosts <<-  data.frame(matrix( nrow=0, ncol=(length(varcost_cfg_names)+1)), stringsAsFactors=F)
mat_cfg_varCosts[1,] <- c("", varcost_cfg_names)
mat_cfg_labCosts <<-  data.frame(matrix( nrow=0, ncol=(length(labcost_cfg_names)+1)), stringsAsFactors=F)
mat_cfg_labCosts[1,] <- c("", labcost_cfg_names)
mat_cfg_fixCosts <<-  data.frame(matrix( nrow=0, ncol=(length(fixcost_cfg_names)+1)), stringsAsFactors=F)
mat_cfg_fixCosts[1,] <- c("", fixcost_cfg_names)
mat_cfg_capCosts <<- data.frame(matrix( nrow=0, ncol=(length(capcost_cfg_names)+1)), stringsAsFactors=F)
mat_cfg_capCosts[1,] <- c("", capcost_cfg_names)
mat_cfg_FleetDyn <<- data.frame(matrix( nrow=0, ncol=(length(fleedyn_cfg_names)+1)), stringsAsFactors=F)
mat_cfg_FleetDyn[1,] <- c("", fleedyn_cfg_names)
mat_cfg_FleetAct <<- data.frame(matrix( nrow=0, ncol=(length(fleeact_cfg_names)+1)), stringsAsFactors=F)
mat_cfg_FleetAct[1,] <- c("",fleeact_cfg_names)
mat_cfg_TechProgress <<- data.frame(matrix( nrow=0, ncol=(length(techprog_cfg_names)+1)), stringsAsFactors=F)
mat_cfg_TechProgress[1,] <- c("", techprog_cfg_names)
mat_cfg_EconomicIndicator <<- data.frame(matrix( nrow=0, ncol=(length(indicator_cfg_names)+1)), stringsAsFactors=F)
mat_cfg_EconomicIndicator[1,] <- c("", indicator_cfg_names)

#mat_cfg_EconomicIndicator_simu <<- data.frame(matrix( nrow=0, ncol=2), stringsAsFactors=F)
#mat_cfg_EconomicIndicator_simu[1,] <- c("", "[correction factor for landings]")

mat_cfg_effort_F <<- data.frame(matrix( nrow=4, ncol=0), stringsAsFactors=F)
mat_cfg_effort_F[,1] <- c("", "Type of relationship", "coeff a", "coeff b")

stock_reduction_list <<- list()	
bmt_stock_reduction <<- NULL

bmt_fishingmortality_list <<- list()	
bmt_fishingmortality <<- NULL

img_pressure_indicator_pb <<- NULL
img_pressure_indicator <<- NULL

bmt_COSTS_list <<- list()
bmt_economic.COSTS <<- NULL

bmt_REVENUES_list <<- list()
bmt_economic.REVENUES <<- NULL

bmt_REVENUES_DISCARD_list <<- list()
bmt_economic.REVENUES_discard <<- NULL

bmt_OTHERS_list <<- list()
bmt_economic.OTHERS <<- NULL


ECONOMICDATA_COSTS_list <<- list()
ECONOMICDATA_REVENUES_list <<- list()
ECONOMICDATA_REVENUES_discard_list <<- list()
ECONOMICDATA_OTHERS_list <<- list()

bmt_fleet.price_elast_landing_byfleet <<- NULL
bmt_price_elast_landing_byfleet_list  <<- list()

price_elast_discard_byfleet_MATRIX <<- NULL
price_elast_discard_byfleet_list  <<- list()

price_costant_byfleet_discard_MATRIX <<- NULL
price_costant_byfleet_discard_list <<- list()

price_costant_byfleet_landing_MATRIX <<- NULL
price_costant_byfleet_landing_list <<- list()

#bmt_fleet.price_elast_landing <<- NULL
#bmt_price_elast_landing_list  <<- list()

bmt_fleet.price_elast_import <<- NULL
bmt_price_elast_import_list  <<- list()

bmt_fleet.price_elast_MW <<- NULL
bmt_price_elast_MW_list  <<- list()

bmt_fleet.price_importweight <<- NULL
bmt_price_importweight_list  <<- list()

#bmt_fleet.price_correction_fact <<- NULL
#bmt_price_correction_fact_list  <<- list()

bmt_fleet.cost_variable  <<- NULL
bmt_cost_variable_list <<- list()

bmt_fleet.cost_fuelprice <<- NULL
bmt_cost_fuelprice_list <<- list()

bmt_fleet.cost_fixed  <<- NULL
bmt_cost_fixed_list <<- list()

bmt_fleet.cost_capital  <<- NULL
bmt_cost_capital_list <<- list()

bmt_fleet.cost_crew_minwage <<- NULL
bmt_cost_crew_minwage_list <<- list()

bmt_fleet.behav_dyn <<- NULL
bmt_behav_dyn_list <<- list()

bmt_fleet.behav_act <<- NULL
bmt_behav_act_list <<- list()

bmt_fleet.behav_progr <<- NULL
bmt_behav_progr_list <<- list()

bmt_fleet.ecoind <<- NULL
bmt_ecoind_list <<- list()

bmt_fleet.ecoind_sim <<- NULL
bmt_ecoind_sim_list <<- list()

bmt_fleet.indic_taxes <<- NULL
bmt_indic_taxes_list <<- list()

bmt_fleet.cost_equipment <<- NULL
bmt_cost_equipment_list <<- list()

bmt_fleet.indic_subsidies <<- NULL
bmt_indic_subsidies_list <<- list()

bmt_fleet.labour_fuel <<- NULL
bmt_labour_fuel_list <<- list()

bmt_fleet.labour_commercial <<- NULL
bmt_labour_commercial_list <<- list()

bmt_fleet.labour_others <<- NULL
bmt_labour_others_list <<- list()

bmt_fleet.labour_sorting <<- NULL
bmt_labour_sorting_list <<- list()

bmt_fleet.DAY_r4 <<- NULL
bmt_DAY_r4_list <<- list()

bmt_fleet.NUMBER_r4 <<- NULL
bmt_NUMBER_r4_list <<- list()

input_table_r5 <<- NULL
input_table_r5_list <<- list()

input_table_r6 <<- NULL
input_table_r6_list <<- list()

input_table_r7 <<- NULL
input_table_r7_list <<- list()

input_table_r6_species_settings <<- NULL
input_table_r6_species_settings_list <<- list()

input_table_r7_opt3_indices <<- NULL
input_table_r7_opt3_indices_list <<- list()

input_table_r7_opt3_tac <<- NULL
input_table_r7_opt3_tac_list <<- list()

mat_cfg_scenario_settings_fore <<- NULL

bmt_effort_F_list <<- list()
bmt_effort_F <<- NULL

MCDAutility_list <<- list()
MCDAutility_table <<- NULL

MCDAweight_list <<- list()
MCDAweight_table <<- NULL

