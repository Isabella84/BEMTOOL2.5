# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



source(paste(getwd(), "/bmtgui/biological/setBiologicalcfg.r", sep=""))
source(paste(getwd(), "/bmtgui/biological/reload_species_info.r", sep=""))
source(paste(getwd(), "/bmtgui/biological/deactivate_bio_items.r", sep=""))
source(paste(getwd(), "/bmtgui/biological/clear_bio_items.r", sep=""))   
source(paste(getwd(), "/bmtgui/biological/assessment/clear_bio_assessment_items.r", sep=""))   
source(paste(getwd(), "/bmtgui/biological/assessment/set_assessment_tool.r", sep="")) 

source(paste(getwd(), "/bmtgui/biological/assessment/VITpaths_males/add.VITpaths_males.r", sep=""))
source(paste(getwd(), "/bmtgui/biological/assessment/VITpaths_males/VITpaths_males.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/biological/assessment/VITpaths_males/VITpaths_males.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/biological/assessment/VITpaths_males/reload_VITpaths_males_table.r", sep=""))   

source(paste(getwd(), "/bmtgui/biological/assessment/VITpaths_females/add.VITpaths_females.r", sep=""))
source(paste(getwd(), "/bmtgui/biological/assessment/VITpaths_females/VITpaths_females.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/biological/assessment/VITpaths_females/VITpaths_females.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/biological/assessment/VITpaths_females/reload_VITpaths_females_table.r", sep="")) 

source(paste(getwd(), "/bmtgui/biological/assessment/VITpaths_combined/add.VITpaths_combined.r", sep=""))
source(paste(getwd(), "/bmtgui/biological/assessment/VITpaths_combined/VITpaths_combined.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/biological/assessment/VITpaths_combined/VITpaths_combined.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/biological/assessment/VITpaths_combined/reload_VITpaths_combined_table.r", sep="")) 



source(paste(getwd(), "/bmtgui/casestudy/setCaseStudycfg.r", sep=""))
source(paste(getwd(), "/bmtgui/casestudy/loadCaseStudycfg.r", sep=""))

source(paste(getwd(), "/bmtgui/casestudy/casestudy.interaction/add.interaction.r", sep=""))
source(paste(getwd(), "/bmtgui/casestudy/casestudy.interaction/interaction.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/casestudy/casestudy.interaction/interaction.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/casestudy/casestudy.interaction/reload_interactions_table.r", sep=""))
source(paste(getwd(), "/bmtgui/casestudy/casestudy.interaction/addInteractions.r", sep=""))
source(paste(getwd(), "/bmtgui/casestudy/casestudy.interaction/removeInteractions.r", sep=""))

source(paste(getwd(), "/bmtgui/casestudy/casestudy.loa/add.loa.r", sep=""))
source(paste(getwd(), "/bmtgui/casestudy/casestudy.loa/loa.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/casestudy/casestudy.loa/loa.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/casestudy/casestudy.loa/reload_loa_table.r", sep=""))

source(paste(getwd(), "/bmtgui/casestudy/casestudy.gear/add.gear.r", sep=""))
source(paste(getwd(), "/bmtgui/casestudy/casestudy.gear/gear.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/casestudy/casestudy.gear/gear.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/casestudy/casestudy.gear/reload_gear_table.r", sep=""))

source(paste(getwd(), "/bmtgui/casestudy/casestudy.nostocks/add.nostocks.r", sep=""))
source(paste(getwd(), "/bmtgui/casestudy/casestudy.nostocks/nostocks.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/casestudy/casestudy.nostocks/nostocks.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/casestudy/casestudy.nostocks/reload_nostocks_table.r", sep=""))

source(paste(getwd(), "/bmtgui/casestudy/casestudy.nofleetsegments/add.nofleetsegments.r", sep=""))
source(paste(getwd(), "/bmtgui/casestudy/casestudy.nofleetsegments/nofleetsegments.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/casestudy/casestudy.nofleetsegments/nofleetsegments.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/casestudy/casestudy.nofleetsegments/reload_nofleetsegments_table.r", sep=""))

source(paste(getwd(), "/bmtgui/casestudy/casestudy.species/add.species.r", sep=""))
source(paste(getwd(), "/bmtgui/casestudy/casestudy.species/species.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/casestudy/casestudy.species/species.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/casestudy/casestudy.species/reload_species_table.r", sep=""))

# ---------------------------------------------------------------------------------------- effort_landing folder

source(paste(getwd(), "/bmtgui/effort_landing/bmt_reload_fleetsegment_info.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/bmt_reload_fleetsegment_info_fore.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/bmt_reload_landing_species_info.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/bmt_loadEffortData.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/bmt_loadEffortData_fore.r", sep=""))

source(paste(getwd(), "/bmtgui/effort_landing/setEffortLanding_fleet_settings.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/setEffortLanding_fleet_settings_fore.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/set_effort_data_paths.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/set_effort_data_paths_fore.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/set_effort_data_lists.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/set_effort_data_lists_fore.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/set_landing_data_lists.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/save_effort_files.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/save_effort_files_fore.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/save_landing_file.r", sep=""))

source(paste(getwd(), "/bmtgui/effort_landing/effort.kw/add.KW.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/effort.kw/KW.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/effort.kw/KW.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/effort.kw/KW.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/effort.kw/bmt_reload_KW_table.r", sep=""))

source(paste(getwd(), "/bmtgui/effort_landing/effort.gt/add.GT.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/effort.gt/GT.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/effort.gt/GT.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/effort.gt/GT.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/effort.gt/bmt_reload_GT_table.r", sep=""))

source(paste(getwd(), "/bmtgui/effort_landing/effort.number/add.NUMBER.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/effort.number/NUMBER.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/effort.number/NUMBER.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/effort.number/NUMBER.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/effort.number/bmt_reload_NUMBER_table.r", sep=""))

source(paste(getwd(), "/bmtgui/effort_landing/effort.day/add.DAY.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/effort.day/DAY.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/effort.day/DAY.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/effort.day/DAY.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/effort.day/bmt_reload_DAY_table.r", sep=""))

source(paste(getwd(), "/bmtgui/effort_landing/landing/add.LANDING.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/landing/LANDING.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/landing/LANDING.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/landing/LANDING.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/effort_landing/landing/bmt_reload_LANDING_table.r", sep=""))

# ---------------------------------------------------------------------------------------- diagnosis folder

source(paste(getwd(), "/bmtgui/diagnosis/diagnosis.stock_reduction/add.stock_reduction.r", sep=""))
source(paste(getwd(), "/bmtgui/diagnosis/diagnosis.stock_reduction/stock_reduction.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/diagnosis/diagnosis.stock_reduction/stock_reduction.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/diagnosis/diagnosis.stock_reduction/bmt_reload_stock_reduction_table.r", sep=""))     

source(paste(getwd(), "/bmtgui/diagnosis/diagnosis.fishingmortality/add.bmt_fishingmortality.r", sep=""))
source(paste(getwd(), "/bmtgui/diagnosis/diagnosis.fishingmortality/bmt_fishingmortality.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/diagnosis/diagnosis.fishingmortality/bmt_fishingmortality.create_model.r", sep=""))
 
source(paste(getwd(), "/bmtgui/diagnosis/runSIMULATION_bmt_action.r", sep=""))
source(paste(getwd(), "/bmtgui/diagnosis/show_species_bio_indicator.r", sep=""))
source(paste(getwd(), "/bmtgui/diagnosis/show_species_pressure_indicator.r", sep=""))  
source(paste(getwd(), "/bmtgui/diagnosis/show_fleet_economic_indicator.r", sep=""))



source(paste(getwd(), "/bmtgui/economic_data/economicdata.costs/add.COSTS.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_data/economicdata.costs/COSTS.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_data/economicdata.costs/COSTS.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_data/economicdata.costs/COSTS.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_data/economicdata.costs/bmt_reload_COSTS_table.r", sep=""))

source(paste(getwd(), "/bmtgui/economic_data/economicdata.revenues/add.REVENUES.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_data/economicdata.revenues/REVENUES.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_data/economicdata.revenues/REVENUES.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_data/economicdata.revenues/REVENUES.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_data/economicdata.revenues/bmt_reload_REVENUES_table.r", sep=""))

source(paste(getwd(), "/bmtgui/economic_data/economicdata_revenues_discard/add.REVENUES_discard.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_data/economicdata_revenues_discard/REVENUES_discard.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_data/economicdata_revenues_discard/REVENUES_discard.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_data/economicdata_revenues_discard/REVENUES_discard.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_data/economicdata_revenues_discard/reload_REVENUES_discard.r", sep=""))

source(paste(getwd(), "/bmtgui/economic_data/economicdata.others/add.OTHERS.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_data/economicdata.others/OTHERS.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_data/economicdata.others/OTHERS.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_data/economicdata.others/OTHERS.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_data/economicdata.others/bmt_reload_OTHERS_table.r", sep=""))

source(paste(getwd(), "/bmtgui/economic_data/setEconomicdata_settings.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_data/bmt_reload_economicdata_fleet.r", sep=""))

source(paste(getwd(), "/bmtgui/economic_params/price/price_elast_landing_byfleet/add.price_elast_landing_byfleet.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_elast_landing_byfleet/price_elast_landing_byfleet.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_elast_landing_byfleet/price_elast_landing_byfleet.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_elast_landing_byfleet/price_elast_landing_byfleet.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_elast_landing_byfleet/bmt_reload_price_elast_landing_byfleet_table.r", sep=""))

source(paste(getwd(), "/bmtgui/economic_params/price/price_costant_byfleet_landing/add.price_costant_byfleet_landing.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_costant_byfleet_landing/price_costant_byfleet_landing.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_costant_byfleet_landing/price_costant_byfleet_landing.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_costant_byfleet_landing/price_costant_byfleet_landing.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_costant_byfleet_landing/reload_price_costant_byfleet_landing.r", sep=""))

source(paste(getwd(), "/bmtgui/economic_params/price/price_elast_discard_byfleet/add.price_elast_discard_byfleet.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_elast_discard_byfleet/price_elast_discard_byfleet.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_elast_discard_byfleet/price_elast_discard_byfleet.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_elast_discard_byfleet/price_elast_discard_byfleet.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_elast_discard_byfleet/reload_price_elast_discard_byfleet.r", sep=""))

source(paste(getwd(), "/bmtgui/economic_params/price/price_costant_byfleet_discard/add.price_costant_byfleet_discard.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_costant_byfleet_discard/price_costant_byfleet_discard.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_costant_byfleet_discard/price_costant_byfleet_discard.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_costant_byfleet_discard/price_costant_byfleet_discard.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_costant_byfleet_discard/reload_price_costant_byfleet_discard.r", sep=""))

#source(paste(getwd(), "/bmtgui/economic_params/price/price_elast_landing/add.price_elast_landing.r", sep=""))
#source(paste(getwd(), "/bmtgui/economic_params/price/price_elast_landing/price_elast_landing.add_columns.r", sep=""))
#source(paste(getwd(), "/bmtgui/economic_params/price/price_elast_landing/price_elast_landing.cell_edited.r", sep=""))
#source(paste(getwd(), "/bmtgui/economic_params/price/price_elast_landing/price_elast_landing.create_model.r", sep=""))
#source(paste(getwd(), "/bmtgui/economic_params/price/price_elast_landing/bmt_reload_price_elast_landing_table.r", sep=""))

source(paste(getwd(), "/bmtgui/economic_params/price/price_elast_import/add.price_elast_import.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_elast_import/price_elast_import.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_elast_import/price_elast_import.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_elast_import/price_elast_import.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_elast_import/bmt_reload_price_elast_import_table.r", sep=""))

source(paste(getwd(), "/bmtgui/economic_params/price/price_elast_MW/add.price_elast_MW.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_elast_MW/price_elast_MW.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_elast_MW/price_elast_MW.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_elast_MW/price_elast_MW.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_elast_MW/bmt_reload_price_elast_MW_table.r", sep=""))

source(paste(getwd(), "/bmtgui/economic_params/price/price_importweight/add.price_importweight.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_importweight/price_importweight.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_importweight/price_importweight.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_importweight/price_importweight.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/price/price_importweight/bmt_reload_price_importweight_table.r", sep=""))

#source(paste(getwd(), "/bmtgui/economic_params/price/price_correction_fact/add.price_correction_fact.r", sep=""))
#source(paste(getwd(), "/bmtgui/economic_params/price/price_correction_fact/price_correction_fact.add_columns.r", sep=""))
#source(paste(getwd(), "/bmtgui/economic_params/price/price_correction_fact/price_correction_fact.cell_edited.r", sep=""))
#source(paste(getwd(), "/bmtgui/economic_params/price/price_correction_fact/price_correction_fact.create_model.r", sep=""))
#source(paste(getwd(), "/bmtgui/economic_params/price/price_correction_fact/bmt_reload_price_correction_fact_table.r", sep=""))

source(paste(getwd(), "/bmtgui/economic_params/cost/cost_variable/add.cost_variable.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/cost/cost_variable/cost_variable.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/cost/cost_variable/cost_variable.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/cost/cost_variable/cost_variable.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/cost/cost_variable/bmt_reload_cost_variable_table.r", sep=""))

source(paste(getwd(), "/bmtgui/economic_params/cost/cost_fuelprice/add.cost_fuelprice.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/cost/cost_fuelprice/cost_fuelprice.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/cost/cost_fuelprice/cost_fuelprice.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/cost/cost_fuelprice/cost_fuelprice.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/cost/cost_fuelprice/bmt_reload_cost_fuelprice_table.r", sep=""))

source(paste(getwd(), "/bmtgui/economic_params/cost/cost_fixed/add.cost_fixed.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/cost/cost_fixed/cost_fixed.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/cost/cost_fixed/cost_fixed.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/cost/cost_fixed/cost_fixed.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/cost/cost_fixed/bmt_reload_cost_fixed_table.r", sep=""))

source(paste(getwd(), "/bmtgui/economic_params/cost/cost_capital/add.cost_capital.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/cost/cost_capital/cost_capital.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/cost/cost_capital/cost_capital.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/cost/cost_capital/cost_capital.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/cost/cost_capital/bmt_reload_cost_capital_table.r", sep=""))

source(paste(getwd(), "/bmtgui/economic_params/cost/cost_equipment/bmt_add.cost_equipment.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/cost/cost_equipment/bmt_cost_equipment.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/cost/cost_equipment/bmt_cost_equipment.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/cost/cost_equipment/bmt_cost_equipment.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/cost/cost_equipment/bmt_reload_cost_equipment.r", sep=""))

source(paste(getwd(), "/bmtgui/economic_params/labour/labour_crew_minwage/add.cost_crew_minwage.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/labour/labour_crew_minwage/cost_crew_minwage.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/labour/labour_crew_minwage/cost_crew_minwage.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/labour/labour_crew_minwage/cost_crew_minwage.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/labour/labour_crew_minwage/bmt_reload_cost_crew_minwage_table.r", sep=""))

source(paste(getwd(), "/bmtgui/economic_params/labour/labour_fuel/add.labour_fuel.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/labour/labour_fuel/labour_fuel.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/labour/labour_fuel/labour_fuel.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/labour/labour_fuel/labour_fuel.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/labour/labour_fuel/bmt_reload_labour_fuel_table.r", sep=""))  

source(paste(getwd(), "/bmtgui/economic_params/labour/labour_commercial/add.labour_commercial.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/labour/labour_commercial/labour_commercial.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/labour/labour_commercial/labour_commercial.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/labour/labour_commercial/labour_commercial.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/labour/labour_commercial/bmt_reload_labour_commercial_table.r", sep=""))  

source(paste(getwd(), "/bmtgui/economic_params/labour/labour_others/add.labour_others.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/labour/labour_others/labour_others.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/labour/labour_others/labour_others.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/labour/labour_others/labour_others.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/labour/labour_others/bmt_reload_labour_others_table.r", sep="")) 

#source(paste(getwd(), "/bmtgui/economic_params/labour/labour_sorting/add.labour_sorting.r", sep=""))
#source(paste(getwd(), "/bmtgui/economic_params/labour/labour_sorting/labour_sorting.add_columns.r", sep=""))
#source(paste(getwd(), "/bmtgui/economic_params/labour/labour_sorting/labour_sorting.cell_edited.r", sep=""))
#source(paste(getwd(), "/bmtgui/economic_params/labour/labour_sorting/labour_sorting.create_model.r", sep=""))
#source(paste(getwd(), "/bmtgui/economic_params/labour/labour_sorting/bmt_reload_labour_sorting_table.r", sep=""))  

source(paste(getwd(), "/bmtgui/economic_params/behaviour/behav_dyn/add.behav_dyn.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/behaviour/behav_dyn/behav_dyn.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/behaviour/behav_dyn/behav_dyn.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/behaviour/behav_dyn/behav_dyn.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/behaviour/behav_dyn/bmt_reload_behav_dyn_table.r", sep=""))

source(paste(getwd(), "/bmtgui/economic_params/behaviour/behav_act/add.behav_act.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/behaviour/behav_act/behav_act.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/behaviour/behav_act/behav_act.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/behaviour/behav_act/behav_act.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/behaviour/behav_act/bmt_reload_behav_act_table.r", sep=""))   

source(paste(getwd(), "/bmtgui/economic_params/behaviour/behav_progr/add.behav_progr.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/behaviour/behav_progr/behav_progr.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/behaviour/behav_progr/behav_progr.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/behaviour/behav_progr/behav_progr.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/behaviour/behav_progr/bmt_reload_behav_progr_table.r", sep=""))   

source(paste(getwd(), "/bmtgui/economic_params/indicators/ecoind/add.ecoind.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/indicators/ecoind/ecoind.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/indicators/ecoind/ecoind.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/indicators/ecoind/ecoind.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/indicators/ecoind/bmt_reload_ecoind_table.r", sep="")) 

#source(paste(getwd(), "/bmtgui/economic_params/ecoind_sim/add.ecoind_sim.r", sep=""))
#source(paste(getwd(), "/bmtgui/economic_params/ecoind_sim/ecoind_sim.add_columns.r", sep=""))
#source(paste(getwd(), "/bmtgui/economic_params/ecoind_sim/ecoind_sim.cell_edited.r", sep=""))
#source(paste(getwd(), "/bmtgui/economic_params/ecoind_sim/ecoind_sim.create_model.r", sep=""))
#source(paste(getwd(), "/bmtgui/economic_params/ecoind_sim/bmt_reload_ecoind_sim_table.r", sep="")) 

source(paste(getwd(), "/bmtgui/economic_params/indicators/indic_taxes/add.indic_taxes.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/indicators/indic_taxes/indic_taxes.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/indicators/indic_taxes/indic_taxes.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/indicators/indic_taxes/indic_taxes.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/indicators/indic_taxes/bmt_reload_indic_taxes_table.r", sep=""))

source(paste(getwd(), "/bmtgui/economic_params/labour/discard_cost/add.discard_cost.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/labour/discard_cost/discard_cost.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/labour/discard_cost/discard_cost.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/labour/discard_cost/discard_cost.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/labour/discard_cost/bmt_reload_discard_cost_table.r", sep=""))

source(paste(getwd(), "/bmtgui/economic_params/indicators/indic_subsidies/add.indic_subsidies.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/indicators/indic_subsidies/indic_subsidies.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/indicators/indic_subsidies/indic_subsidies.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/indicators/indic_subsidies/indic_subsidies.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/indicators/indic_subsidies/bmt_reload_indic_subsidies_table.r", sep="")) 


source(paste(getwd(), "/bmtgui/economic_params/setEconomicparams_ALLthesettings.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/setEconomicparams_capitalcost_settings.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/setEconomicparams_fixedcost_settings.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/setEconomicparams_price_settings.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/setEconomicparams_price_settings_discard.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/setEconomicparams_varcost_settings.r", sep=""))
source(paste(getwd(), "/bmtgui/economic_params/economic_activation_deactivation.r", sep=""))

source(paste(getwd(), "/bmtgui/scenarios/check_scenario_combination.r", sep="")) 
source(paste(getwd(), "/bmtgui/scenarios/activate_deactivate_scenario_options.r", sep=""))      
source(paste(getwd(), "/bmtgui/scenarios/getScenarioOptions.r", sep="")) 

source(paste(getwd(), "/bmtgui/scenarios/scenario_r4/r4.effort.number/add.NUMBER_r4.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r4/r4.effort.number/NUMBER_r4.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r4/r4.effort.number/NUMBER_r4.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r4/r4.effort.number/NUMBER_r4.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r4/r4.effort.number/bmt_reload_NUMBER_r4_table.r", sep=""))

source(paste(getwd(), "/bmtgui/scenarios/scenario_r4/r4.effort.day/add.DAY_r4.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r4/r4.effort.day/DAY_r4.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r4/r4.effort.day/DAY_r4.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r4/r4.effort.day/DAY_r4.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r4/r4.effort.day/bmt_reload_DAY_r4_table.r", sep=""))

source(paste(getwd(), "/bmtgui/scenarios/scenario_r4/activate_deactivate_reduction_mode_r4.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r4/bmt_applyAutomaticReduction.r", sep=""))

source(paste(getwd(), "/bmtgui/scenarios/effort_F/add.effort_F.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/effort_F/effort_F.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/effort_F/effort_F.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/effort_F/effort_F.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/effort_F/reload_effort_F.r", sep=""))

source(paste(getwd(), "/bmtgui/scenarios/scenario_r5/input_table_r5/add.input_table_r5.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r5/input_table_r5/input_table_r5.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r5/input_table_r5/input_table_r5.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r5/input_table_r5/input_table_r5.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r5/input_table_r5/reload_input_table_r5.r", sep=""))

source(paste(getwd(), "/bmtgui/scenarios/scenario_r6/input_table_r6/add.input_table_r6.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r6/input_table_r6/input_table_r6.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r6/input_table_r6/input_table_r6.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r6/input_table_r6/input_table_r6.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r6/input_table_r6/reload_input_table_r6.r", sep=""))

source(paste(getwd(), "/bmtgui/scenarios/scenario_r6/input_table_r6_species_settings/add.input_table_r6_species_settings.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r6/input_table_r6_species_settings/input_table_r6_species_settings.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r6/input_table_r6_species_settings/input_table_r6_species_settings.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r6/input_table_r6_species_settings/input_table_r6_species_settings.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r6/input_table_r6_species_settings/reload_input_table_r6_species_settings.r", sep=""))

source(paste(getwd(), "/bmtgui/scenarios/scenario_r7/input_table_r7/add.input_table_r7.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r7/input_table_r7/input_table_r7.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r7/input_table_r7/input_table_r7.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r7/input_table_r7/input_table_r7.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r7/input_table_r7/reload_input_table_r7.r", sep=""))

source(paste(getwd(), "/bmtgui/scenarios/scenario_r7/input_table_r7_opt3_tac/add.input_table_r7_opt3_tac.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r7/input_table_r7_opt3_tac/input_table_r7_opt3_tac.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r7/input_table_r7_opt3_tac/input_table_r7_opt3_tac.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r7/input_table_r7_opt3_tac/input_table_r7_opt3_tac.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r7/input_table_r7_opt3_tac/reload_input_table_r7_opt3_tac.r", sep=""))

source(paste(getwd(), "/bmtgui/scenarios/scenario_r7/input_table_r7_opt3_indices/add.input_table_r7_opt3_indices.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r7/input_table_r7_opt3_indices/input_table_r7_opt3_indices.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r7/input_table_r7_opt3_indices/input_table_r7_opt3_indices.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r7/input_table_r7_opt3_indices/input_table_r7_opt3_indices.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r7/input_table_r7_opt3_indices/reload_input_table_r7_opt3_indices.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/scenario_r7/activate_deactivate_opt3_r7.r", sep=""))  

source(paste(getwd(), "/bmtgui/scenarios/loadScenariocfg.r", sep=""))                                                                                   
source(paste(getwd(), "/bmtgui/scenarios/load_selected_scenario_results.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/loadRulesintoGUI.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/saveBMTCFG_scenario.r", sep=""))
source(paste(getwd(), "/bmtgui/scenarios/setScenarioOptions.r", sep=""))

source(paste(getwd(), "/bmtgui/forecast/show_species_bio_indicator_fore.r", sep=""))
source(paste(getwd(), "/bmtgui/forecast/show_species_pressure_indicator_fore.r", sep=""))
source(paste(getwd(), "/bmtgui/forecast/show_fleet_economic_indicator_fore.r", sep=""))
source(paste(getwd(), "/bmtgui/forecast/runFORECAST_bmt_action.r", sep=""))

source(paste(getwd(), "/bmtgui/mcda/utility/add.MCDAutility.r", sep=""))
source(paste(getwd(), "/bmtgui/mcda/utility/MCDAutility.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/mcda/utility/MCDAutility.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/mcda/utility/MCDAutility.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/mcda/utility/reload_MCDAutility.r", sep="")) 
source(paste(getwd(), "/bmtgui/mcda/utility/loadMCDAutilityfromFile.r", sep=""))   

source(paste(getwd(), "/bmtgui/mcda/weight/add.MCDAweight.r", sep=""))
source(paste(getwd(), "/bmtgui/mcda/weight/MCDAweight.add_columns.r", sep=""))
source(paste(getwd(), "/bmtgui/mcda/weight/MCDAweight.cell_edited.r", sep=""))
source(paste(getwd(), "/bmtgui/mcda/weight/MCDAweight.create_model.r", sep=""))
source(paste(getwd(), "/bmtgui/mcda/weight/reload_MCDAweight.r", sep=""))    
source(paste(getwd(), "/bmtgui/mcda/weight/loadMCDAweightfromFile.r", sep=""))  

source(paste(getwd(), "/bmtgui/mcda/runMCDA_bmt_action.r", sep=""))        

source(paste(getwd(), "/bmtgui/utils/goNextTab.r", sep=""))
source(paste(getwd(), "/bmtgui/utils/goPrevTab.r", sep=""))
source(paste(getwd(), "/bmtgui/utils/goMCDA.r", sep=""))
source(paste(getwd(), "/bmtgui/utils/bmt_loadCaseStudy.r", sep=""))
source(paste(getwd(), "/bmtgui/utils/bmt_loadScenario.r", sep=""))
source(paste(getwd(), "/bmtgui/utils/check_bmt_simulation_economic_ts.r", sep=""))


source(paste(getwd(), "/bmtgui/utils/showQuestionYN_diagnosis.r", sep=""))
source(paste(getwd(), "/bmtgui/utils/showQuestionYN_forecast.r", sep=""))