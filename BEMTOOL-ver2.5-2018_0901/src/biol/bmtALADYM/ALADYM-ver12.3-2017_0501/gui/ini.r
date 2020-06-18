# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



 
 dim_eff_tables <<- 60
 dim_big_tables <<- 150

MONTHS <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
DISTRIBUTION <- c("Lognormal","Gamma", "Normal","Uniform")
DISTRIBUTION_UNCERT <- c("Lognormal","Normal","Uniform")
# SR_TYPE <- c("R=S/(a+b*S)", "R=a*S*exp(-b*S)", "R=a*S/(1+(S/c)^b)", "From Vector", "Hockey-Stick", "Hockey-Stick quadratic")
SR_TYPE <- c("Beverton and Holt", "Ricker", "Shepherd", "from vector", "Hockey-Stick", "Hockey-Stick quadratic")
SELECTIVITY_TYPE <- c("Classical ogive",  "Ogive with deselection", "Normal", "Log-normal", "Bi-normal", "Two-sided")
SCENARIO_TYPE <- c("Linear", "Logistic",  "Exponential")
SS_TYPE <- c("only females", "overall")
MORTALITY_TYPE <-  c("M constant","Chen&Watanabe","From vector","ProdbiomUniqueSolution", "Gislason")
EFFORT_F_TYPE <- c(" LINEAR   y = bx + a ", " POWER   y = a * x^b ")
DISCARD_CALC <- c("YES", "0", "NA")
SR_UNIT <- c("Tons (biomass)", "Thousands (numbers)")
NUMERICAL_ENTRY_LENGTH <- 10
LABEL_LENGTH <- 10

# tabelle 


# liste
selectivity_uncert_distribution_from_file_matrix <<- NULL
selectivity_uncert_vector_from_file_matrix <<- NULL
maturity_uncert_males_from_file_matrix <<- NULL
maturity_uncert_females_from_file_matrix <<- NULL
growth_uncert_Linf_from_file_matrix <<- NULL
growth_uncert_k_from_file_matrix <<- NULL
table_recruitments_fore_from_vector_UN <<- NULL
offspring_prop_df <<- NULL
stockrecruitment.SRvector <<- NULL
recruitments_fore_from_vector.vector <<- NULL
recruitments_fore_from_vector.vector_UN <<- NULL
stockrecruitment.SRvector.seed <<- NULL
mortality.Mvector.males <<- NULL
mortality.Mvector.females <<- NULL
mortality.Zvector.males <<- NULL
mortality.Zvector.females <<- NULL
mortality.Fvector.males <<- NULL
mortality.Fvector.males_fore <<- NULL
mortality.Fvector.females <<- NULL
mortality.Fvector.females_fore <<- NULL
FF_overall_matrix <<- NULL
FM_overall_matrix <<- NULL
fleet.selectivity <<- NULL
fleet.selectivity_fore <<- NULL
SelectivityAgeF_matrix <<- NULL
SelectivityAgeM_matrix <<- NULL
SelectivityAgeF_fore_matrix <<- NULL
SelectivityAgeM_fore_matrix <<- NULL
SelectivityLengthF_matrix <<- NULL
SelectivityLengthM_matrix <<- NULL
SelectivityLengthF_fore_matrix <<- NULL
SelectivityLengthM_fore_matrix <<- NULL
fleet.FISHINGEFFORT <<- NULL
fleet.VESSELS <<- NULL
fleet.DAYS <<- NULL
fleet.GT <<- NULL  
fleet.FISHINGEFFORT_fore <<- NULL
fleet.VESSELS_fore <<- NULL
fleet.DAYS_fore <<- NULL
fleet.GT_fore <<- NULL
fleet.pproduction <<- NULL
fleet.production <<- NULL
fleet.monthlyDiscard <<- NULL
fleet.pproduction.seed <<- NULL
fleet.production.seed <<- NULL
fleet.discard <<- NULL
fleet.discard_fore <<- NULL
fleet.discard_extvector_F <<- NULL
fleet.discard_extvector_F_fore <<- NULL
fleet.discard_extvector_M <<- NULL
fleet.discard_extvector_M_fore <<- NULL
monthly.survivability_df <<- NULL
fleet.lan_obligation <<- NULL
fleet.lan_obligation_fore <<- NULL
escape_surv_extvector_Ftable  <<- NULL
escape_surv_extvector_Mtable  <<- NULL
escape_surv_extvector_Ftable_fore  <<- NULL
escape_surv_extvector_Mtable_fore  <<- NULL

catchAtAge.vector.males <<- NULL
catchAtAge.vector.females <<- NULL

CI_external_matrix <<- NULL
CI_NB_RUNS <<- 100
CI_external_matrix_fore <<- NULL
CI_NB_RUNS_FORE <<- 1
external_recruitment_matrix_fore <<- NULL 

if (!IN_BEMTOOL) {

if (exists("FLEETSEGMENTS_names")) {
if (length(FLEETSEGMENTS_names) >0 ) {
  for (choice in FLEETSEGMENTS_names) { 
         gtkComboBoxRemoveText(combo_fleetsegments, 0)
           gtkComboBoxRemoveText(combo_fleetsegments_fore, 0)
    }
   }
   
   }
   }

FLEETSEGMENTS_names <<- NULL

CONFIGURATION_file <<- "" 
# create a new object aladymPopulation
new_aldPopulation <<- new(Class="aldPopulation")
new_aldSimulation <<- new(Class="aldSimulation")
FleetList_simulation <<- list()
new_aldForecast <<- new(Class="aldForecast")
FleetList_forecast <<- list()


