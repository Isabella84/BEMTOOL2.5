# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.






setBiologicalParams_fromEnv<-function(work_space) {




load(work_space, .GlobalEnv) 

 

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




errorsVector <- c()



if (showCompTime)  {
setBiologicalParams_fromEnv_ptm <- proc.time()  
}


 



   # if (!IN_BEMTOOL) {
gtkEntrySetText(entry_StartYear_simulation, as.numeric(as.character(years[1])))
gtkEntrySetText(entry_EndYear_simulation, as.numeric(as.character(years[length(years)])))
gtkEntrySetText(entry_EndYear_forecast, as.numeric(as.character(years_forecast[length(years_forecast)])))

gtkEntrySetText(entryVBF_M_lifespan, biological.lifeSpanM )
gtkEntrySetText(entryVBF_F_lifespan, biological.lifeSpanF )

change_startend_year()     

all_years <<- c( years, years_forecast)  
GLO$L_number <- length(all_years) *12

mortality.Mvector.males <<- new_aldSimulation@ naturalmortality.M.vector
mortality.Mvector.females <<- new_aldSimulation@ naturalmortality.F.vector

# }




if (new_aldSimulation@enteringMortality == "Z") {  
gtkToggleButtonSetActive(radio_Zentry, T)
} else {
gtkToggleButtonSetActive(radio_Fentry, T)
}


if (new_aldSimulation@enteringMortality == "F") {  
if ( new_aldSimulation@Ftype == "F") {
gtkToggleButtonSetActive(radio_f_by_fleet, T)
} else {

FF_overall_matrix <<-  new_aldSimulation@fishingmortality.overall.F
reload_fishingmortalityF_overall()

FM_overall_matrix <<-  new_aldSimulation@fishingmortality.overall.M
reload_fishingmortalityM_overall()

gtkToggleButtonSetActive(radio_f_overall, T)
if ( new_aldSimulation@Fsplittingtype  == "CAA")  {
gtkToggleButtonSetActive(radio_catch_by_age_splitting, T)
} else {
gtkToggleButtonSetActive(radio_production_splitting, T)
}
}
}


gtkComboBoxSetActive(combo_L50dis_M, which(DISTRIBUTION == as.character(new_aldPopulation@ maturityogive[1,1]) ) -1 )
gtkEntrySetText(entryOGIVEL50_M_min, as.numeric(as.character(new_aldPopulation@ maturityogive[1,2]) ) )
gtkEntrySetText(entryOGIVEL50_M_max,as.numeric(as.character(new_aldPopulation@ maturityogive[1,3]) )  )
if (as.character(new_aldPopulation@ maturityogive[1,1])  != "Uniform") {  
gtkEntrySetText(entryOGIVEL50_M_a, as.numeric(as.character(new_aldPopulation@ maturityogive[1,4]) )  )   
gtkEntrySetText(entryOGIVEL50_M_b, as.numeric(as.character(new_aldPopulation@ maturityogive[1,5]) )  )
}

gtkComboBoxSetActive(combo_L75L25dis_M, which(DISTRIBUTION == as.character(new_aldPopulation@ maturityogive[2,1]) ) -1 )
gtkEntrySetText(entryOGIVEL75L25_M_min, as.numeric(as.character(new_aldPopulation@ maturityogive[2,2]) )  )
gtkEntrySetText(entryOGIVEL75L25_M_max, as.numeric(as.character(new_aldPopulation@ maturityogive[2,3]) )  )
if (as.character(new_aldPopulation@ maturityogive[2,1])  != "Uniform") {  
gtkEntrySetText(entryOGIVEL75L25_M_a, as.numeric(as.character(new_aldPopulation@ maturityogive[2,4]) )  )
gtkEntrySetText(entryOGIVEL75L25_M_b, as.numeric(as.character(new_aldPopulation@ maturityogive[2,5]) )  )
}


gtkComboBoxSetActive(combo_L50dis_F, which(DISTRIBUTION == as.character(new_aldPopulation@ maturityogive[3,1]) ) -1 )
gtkEntrySetText(entryOGIVEL50_F_min, as.numeric(as.character(new_aldPopulation@ maturityogive[3,2]) )  )
gtkEntrySetText(entryOGIVEL50_F_max, as.numeric(as.character(new_aldPopulation@ maturityogive[3,3]) )  )
if (as.character(new_aldPopulation@ maturityogive[3,1])  != "Uniform") {  
gtkEntrySetText(entryOGIVEL50_F_a, as.numeric(as.character(new_aldPopulation@ maturityogive[3,4]) )  )
gtkEntrySetText(entryOGIVEL50_F_b, as.numeric(as.character(new_aldPopulation@ maturityogive[3,5]) )  )
}


gtkComboBoxSetActive(combo_L75L25dis_F, which(DISTRIBUTION == as.character(new_aldPopulation@ maturityogive[4,1]) )-1 )
gtkEntrySetText(entryOGIVEL75L25_F_min, as.numeric(as.character(new_aldPopulation@ maturityogive[4,2]) )  )
gtkEntrySetText(entryOGIVEL75L25_F_max, as.numeric(as.character(new_aldPopulation@ maturityogive[4,3]) )  ) 
if (as.character(new_aldPopulation@ maturityogive[4,1])  != "Uniform") {
gtkEntrySetText(entryOGIVEL75L25_F_a, as.numeric(as.character(new_aldPopulation@ maturityogive[4,4]) )  )
gtkEntrySetText(entryOGIVEL75L25_F_b,  as.numeric(as.character(new_aldPopulation@ maturityogive[4,5]) )  )
}


# set the growth parameters 

gtkComboBoxSetActive(combo_t0dis_M,  which(DISTRIBUTION == as.character(new_aldPopulation@ growth[1,1]) ) -1)
gtkEntrySetText(entryVBFtzero_M_min, as.numeric(as.character(new_aldPopulation@ growth[1,2]))  )
gtkEntrySetText(entryVBFtzero_M_max, as.numeric(as.character(new_aldPopulation@ growth[1,3]))  )

if (as.character(new_aldPopulation@ growth[1,1]) != "Uniform") {
gtkEntrySetText(entryVBFt0_M_a, as.numeric(as.character(new_aldPopulation@ growth[1,4]))  )
gtkEntrySetText(entryVBFt0_M_b, as.numeric(as.character(new_aldPopulation@ growth[1,5]))  )
}


                                                                                                           
gtkComboBoxSetActive(combo_Kdis_M, which(DISTRIBUTION == as.character(new_aldPopulation@ growth[2,1]) ) -1)
gtkEntrySetText(entryVBFK_M_min, as.numeric(as.character(new_aldPopulation@ growth[2,2]))  )
gtkEntrySetText(entryVBFK_M_max,as.numeric(as.character(new_aldPopulation@ growth[2,3]))  )

if (as.character(new_aldPopulation@ growth[2,1]) != "Uniform") {
gtkEntrySetText(entryVBFK_M_a, as.numeric(as.character(new_aldPopulation@ growth[2,4]))  )
gtkEntrySetText(entryVBFK_M_b, as.numeric(as.character(new_aldPopulation@ growth[2,5]))  )
}


gtkComboBoxSetActive(combo_Linfdis_M, which(DISTRIBUTION == as.character(new_aldPopulation@ growth[3,1]) ) -1)
gtkEntrySetText(entryVBFLinf_M_min, as.numeric(as.character(new_aldPopulation@ growth[3,2]))  )
gtkEntrySetText(entryVBFLinf_M_max, as.numeric(as.character(new_aldPopulation@ growth[3,3])) )

if (as.character(new_aldPopulation@ growth[3,1]) != "Uniform") {
gtkEntrySetText(entryVBFLinf_M_a, as.numeric(as.character(new_aldPopulation@ growth[3,4])) )
gtkEntrySetText(entryVBFLinf_M_b, as.numeric(as.character(new_aldPopulation@ growth[3,5])) )
}


gtkComboBoxSetActive(combo_t0dis_F,  which(DISTRIBUTION == as.character(new_aldPopulation@ growth[4,1]) ) -1)
gtkEntrySetText(entryVBFtzero_F_min, as.numeric(as.character(new_aldPopulation@ growth[4,2]))  )
gtkEntrySetText(entryVBFtzero_F_max, as.numeric(as.character(new_aldPopulation@ growth[4,3]))  )

if (as.character(new_aldPopulation@ growth[4,1]) != "Uniform") {
gtkEntrySetText(entryVBFt0_F_a, as.numeric(as.character(new_aldPopulation@ growth[4,4]))  )
gtkEntrySetText(entryVBFt0_F_b, as.numeric(as.character(new_aldPopulation@ growth[4,5]))  )
}


gtkComboBoxSetActive(combo_Kdis_F,  which(DISTRIBUTION == as.character(new_aldPopulation@ growth[5,1]) )  -1)
gtkEntrySetText(entryVBFK_F_min, as.numeric(as.character(new_aldPopulation@ growth[5,2]))  )


gtkEntrySetText(entryVBFK_F_max, as.numeric(as.character(new_aldPopulation@ growth[5,3]))  )


if (as.character(new_aldPopulation@ growth[5,1])  != "Uniform") {
gtkEntrySetText(entryVBFK_F_a, as.numeric(as.character(new_aldPopulation@ growth[5,4]))  )
gtkEntrySetText(entryVBFK_F_b, as.numeric(as.character(new_aldPopulation@ growth[5,5]))  )
}

gtkComboBoxSetActive(combo_Linfdis_F,  which(DISTRIBUTION == as.character(new_aldPopulation@ growth[6,1]) )  -1)
gtkEntrySetText(entryVBFLinf_F_min, as.numeric(as.character(new_aldPopulation@ growth[6,2]))  )
gtkEntrySetText(entryVBFLinf_F_max,as.numeric(as.character(new_aldPopulation@ growth[6,3]))  )

if ( as.character(new_aldPopulation@ growth[6,1])   != "Uniform") {
gtkEntrySetText(entryVBFLinf_F_a, as.numeric(as.character(new_aldPopulation@ growth[6,4]))  )
gtkEntrySetText(entryVBFLinf_F_b, as.numeric(as.character(new_aldPopulation@ growth[6,5]))  )
}

gtkComboBoxSetActive(combo_OFFSPRING_rand,  which(DISTRIBUTION == as.character(new_aldSimulation@R.100runs[1,1]) )  -1)
gtkEntrySetText(entryOFFSPRING_rand_min, as.numeric(as.character(new_aldSimulation@R.100runs[1,2])))
gtkEntrySetText(entryOFFSPRING_rand_max, as.numeric(as.character(new_aldSimulation@R.100runs[1,3])))

if (as.character(new_aldSimulation@R.100runs[1,1])   != "Uniform") {
gtkEntrySetText(entryOFFSPRING_rand_a, as.numeric(as.character(new_aldSimulation@R.100runs[1,4])))
gtkEntrySetText(entryOFFSPRING_rand_b,as.numeric(as.character(new_aldSimulation@R.100runs[1,5])))
}

#
#gtkComboBoxSetActive(combo_RecrNoise_dis,  which(DISTRIBUTION == as.character(new_aldSimulation@ recruitment.noise[1,1]) ) -1)
#if (which(DISTRIBUTION == as.character(new_aldSimulation@ recruitment.noise[1,1]) ) != 4) {
#gtkEntrySetText(entryNoise_a, as.numeric(as.character(new_aldSimulation@ recruitment.noise[1,4])))
#gtkEntrySetText(entryNoise_b, as.numeric(as.character(new_aldSimulation@ recruitment.noise[1,5])))
#}


# ------------------------------------------------------------------- Confidence Intervals settings
#if (as.logical(new_aldSimulation@CI_calculation)) {
#gtkEntrySetText(entry_CI_numb_runs,  as.numeric(as.character(new_aldSimulation@CI_n_runs)) )
#gtkToggleButtonSetActive(chkConfidenceIntervals, T )
#}
#
#if (as.logical(new_aldSimulation@CI_calculation) & as.numeric(as.character(new_aldSimulation@CI_error_source )) == 1) {   #external file (1) /distribution (2)
#   gtkToggleButtonSetActive(radio_recruitment_error_ext_file, TRUE)
#   
##up_file_external_error <-  as.character(BIOmatr[as.character(BIOmatr$Parameters) == "CI error file", 2])
##if (nchar(up_file_external_error) > 1) {  
###  if (IN_BEMTOOL) {  
### up_file_external_error <- up_file_external_error    
###  } else {  
###  up_file_external_error <- paste(ALADYM_home, "/",up_file_external_error, sep="") 
###  }
##  }
#  
# CI_external_matrix <<- new_aldSimulation@CI_error.externalvector
#
## 
#if (nrow(CI_external_matrix) != 0) {
##  new_aldSimulation@CI_error.externalvector <<- CI_external_matrix   
#  reload_extErrorRecruitment()
# }
#
#} else if (as.logical(new_aldSimulation@CI_calculation) & as.numeric(as.character(new_aldSimulation@CI_error_source ))  == 2) {
#     gtkToggleButtonSetActive(radio_recruitment_error_distribution, TRUE)
#
#noise_dis <- new_aldSimulation@recruitment.noise$distribution
#noise_min <- new_aldSimulation@recruitment.noise$min
#noise_max <- new_aldSimulation@recruitment.noise$max
##
##if (noise_dis != "Uniform") {
##    noiseA_value <-  as.numeric(as.character(BIOmatr$A[as.character(BIOmatr$Parameters) == "CI error distribution"] ))
##noiseB_value <- as.numeric(as.character(BIOmatr$B[as.character(BIOmatr$Parameters) == "CI error distribution"] ))
##    }  else {
##       noiseA_value <-  ""
##noiseB_value <- ""
## 
##    }
##  object@recruitment.noise <- data.frame(rbind(list( distribution=noise_dis, min=noise_min, max=noise_max, A=noiseA_value, B=noiseB_value  ) ), row.names=1)
##
#gtkComboBoxSetActive(combo_RecrNoise_dis,  as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters == "CI error distribution"]))-1)
#gtkEntrySetText(entry_noise_recr_min, noise_min)
#gtkEntrySetText(entry_noise_recr_max, noise_max)
#
#
#if (as.numeric(as.character(BIOmatr$Distribution[BIOmatr$Parameters =="Offspring noise"])) != 4) {

#gtkEntrySetText(entryNoise_a, noiseA_value)
#gtkEntrySetText(entryNoise_b, noiseB_value)
#}
#
#}
#
#if (new_aldSimulation@CI_calculation & new_aldSimulation@CI_error_type == 1) {
#       gtkToggleButtonSetActive(radio_CI_err_additive, TRUE)
#} else {
#       gtkToggleButtonSetActive(radio_CI_err_multiplicative, TRUE)
#}

gtkToggleButtonSetActive(chkCalibration, as.logical(as.character(new_aldSimulation@recruitment.tuning)) )

if (as.logical(as.character(new_aldSimulation@recruitment.tuning))) {
  gtkEntrySetText(entry_minrec, as.numeric(as.character(new_aldSimulation@recruitment.tuning.range$min)))
  gtkEntrySetText(entry_maxrec, as.numeric(as.character(new_aldSimulation@recruitment.tuning.range$max)))
  gtkWidgetSetSensitive(entry_maxrec, TRUE)
  gtkWidgetSetSensitive(entry_minrec, TRUE)
}  else {
  gtkWidgetSetSensitive(entry_maxrec, FALSE)
  gtkWidgetSetSensitive(entry_minrec, FALSE)
}


gtkComboBoxSetActive(combo_SS,  which(SS_TYPE == as.character(new_aldSimulation@ spawners.ss) ) -1 )
gtkEntrySetText(entry_delaySS, as.numeric(as.character( new_aldSimulation@spawners.delayss )) )



# if (!IN_BEMTOOL) {
gtkEntrySetText(entrySpecies, as.character(new_aldPopulation@scientific_name ) )  
gtkEntrySetText(entryGSA, as.character(new_aldPopulation@GSA) )    
gtkEntrySetText(entryYearsForAverage, as.numeric(as.character(new_aldSimulation@yearsForAverage)) )  
gtkEntrySetText(entry_SR_value, as.numeric(as.character(new_aldPopulation@sexratio)) )


gtkEntrySetText(entryAB_A_M, as.numeric(as.character(new_aldPopulation@ lengthweight[1,1] )) )
gtkEntrySetText(entryAB_B_M, as.numeric(as.character(new_aldPopulation@ lengthweight[1,2] )) )
gtkEntrySetText(entryAB_A_F, as.numeric(as.character(new_aldPopulation@ lengthweight[2,1] )) )
gtkEntrySetText(entryAB_B_F, as.numeric(as.character(new_aldPopulation@ lengthweight[2,2] )) ) 







gtkEntrySetText(entryVBF_M_lifespan, as.numeric(as.character(new_aldPopulation@ lifespan[1,1])) )
gtkEntrySetText(entryVBF_F_lifespan, as.numeric(as.character(new_aldPopulation@ lifespan[2,1])) ) 

gtkEntrySetText(entry_agerange_M_min, as.numeric(as.character(new_aldSimulation@fishingmortality[1,1] )) )
gtkEntrySetText(entry_agerange_M_max, as.numeric(as.character(new_aldSimulation@fishingmortality[1,2] )) )
gtkEntrySetText(entry_agerange_F_min, as.numeric(as.character(new_aldSimulation@fishingmortality[2,1] )) )
gtkEntrySetText(entry_agerange_F_max, as.numeric(as.character(new_aldSimulation@fishingmortality[2,2] )) )
# }

gtkEntrySetText(entrySpeciesCommonName, as.character(new_aldPopulation@common_name) )
gtkEntrySetText(entryYearsToBePreSimulated, as.numeric(as.character(new_aldSimulation@presimulation_years)) )
gtkEntrySetText(entryOFFSPRING_tr, as.numeric(as.character(new_aldSimulation@Tr)) )






offspring_prop_df <<- new_aldSimulation@monthlyoffspring
 colnames(offspring_prop_df) <<- c(MONTHS)
  reload_monthlyOffsprings()

monthly.survivability_df <<- new_aldSimulation@monthlysurvivability
 colnames(monthly.survivability_df) <<- c(MONTHS)
reload_monthlySurvivability() 


# -------------------------------------------------------------------------------------------------------------------
# loading recruitment from file


stockrecruitment.SRvector <<- new_aldSimulation@stockr.vector
stockrecruitment.SRvector.seed <<- stockrecruitment.SRvector$seed[1] 
stockrecruitment.SRvector <<- stockrecruitment.SRvector[,colnames(stockrecruitment.SRvector) != "seed"] 


##------------------------------------------ load the file

gtkEntrySetText(entry_OFFSPRING_seedvalue, stockrecruitment.SRvector.seed)
recruitments <<- list()
recruitmentIndex <<- 0
add.recruitments()
  recruitments.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 12), "gboolean")  
  for (i in 1:length(recruitments)) {
    iter <-  recruitments.model$append()$iter
     recruitments.model$set(iter,0, recruitments[[i]]$year)
    #print(paste("in model:", as.character(recruitments[[i]]$year)))
    for (e in 1:length(MONTHS)) {
        recruitments.model$set(iter, e, as.double(recruitments[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
       #print(paste("in model:", recruitments[[i]][e]) )
    }
     recruitments.model$set(iter,13,TRUE)
  } 

   recruitments.treeview$destroy()
 recruitments.treeview <<- gtkTreeViewNewWithModel( recruitments.model)
 recruitments.treeview$setRulesHint(TRUE)
 recruitments.treeview$getSelection()$setMode("single")
recruitments.add_columns( recruitments.treeview) 
recruitment.sw$add(recruitments.treeview)








#}
#
#}
#
# -------------------------------------------------------------------------------------------------------------------
# loading gears from configuration file

#
#if (!IN_BEMTOOL) {
#geas <- geas_df[!is.na(geas_df)]

#
for (gg in 1:length(FLEETSEGMENTS_names)) {


 gtkComboBoxInsertText(combo_fleetsegments, gg-1 , FLEETSEGMENTS_names[gg])
 gtkComboBoxInsertText(combo_fleetsegments_fore,  gg-1 , FLEETSEGMENTS_names[gg])

#}
}
#


# -------------------------------------------------------------------------------------------------------------------
# loading natural mortality for males



#if (!is.na(up_file_M_males)) { 
#

Mvector_M <<- list()
Mvector_MIndex <<- 0
add.Mvector_M()
  Mvector_M.model <<- gtkListStoreNew("gchararray", "gdouble", "gboolean")  
  for (i in 1:length(Mvector_M)) {
    iter <-  Mvector_M.model$append()$iter
     Mvector_M.model$set(iter, 0, as.character(Mvector_M[[i]]$age_month))
     Mvector_M.model$set(iter, 1, as.double(Mvector_M[[i]]$M))          # as.double(sexratios[[ind]][nc_i+1]) 
     Mvector_M.model$set(iter, 2,TRUE)
  } 
#
   Mvector_M.treeview$destroy()
 Mvector_M.treeview <<- gtkTreeViewNewWithModel( Mvector_M.model)
 Mvector_M.treeview$setRulesHint(TRUE)
 Mvector_M.treeview$getSelection()$setMode("single")
Mvector_M.add_columns( Mvector_M.treeview)
Mvector_M.sw$add(Mvector_M.treeview)







# -------------------------------------------------------------------------------------------------------------------
# loading natural mortality for females






mortality.Mvector.females <<- new_aldSimulation@ naturalmortality.F.vector






##------------------------------------------ load the file
Mvector_F <<- list()
Mvector_FIndex <<- 0
add.Mvector_F()
  Mvector_F.model <<- gtkListStoreNew("gchararray", "gdouble", "gboolean")  
  for (i in 1:length(Mvector_F)) {
    iter <-  Mvector_F.model$append()$iter
     Mvector_F.model$set(iter,0, as.character(Mvector_F[[i]]$age_month))
     Mvector_F.model$set(iter, 1, as.double(Mvector_F[[i]]$M))          # as.double(sexratios[[ind]][nc_i+1]) 
     Mvector_F.model$set(iter,2,TRUE)
  } 
#
 Mvector_F.treeview$destroy()
 Mvector_F.treeview <<- gtkTreeViewNewWithModel( Mvector_F.model)
 Mvector_F.treeview$setRulesHint(TRUE)
 Mvector_F.treeview$getSelection()$setMode("single")
Mvector_F.add_columns( Mvector_F.treeview)
Mvector_F.sw$add(Mvector_F.treeview)







# -------------------------------------------------------------------------------------------------------------------
# loading total mortality



#if (!is.na(up_file_Z)) {   
#
#mortality.Zvector <<- read.csv(up_file_Z, sep=";", na.strings = "")





mortality.Zvector.males <<- new_aldSimulation@  totalmortality.M.vector
mortality.Zvector.females <<- new_aldSimulation@  totalmortality.F.vector
#
mortality.Zvector.males.seed <<- mortality.Zvector.males$seed[1] 
mortality.Zvector.males <<- mortality.Zvector.males[,colnames(mortality.Zvector.males) != "seed"] 

#
mortality.Zvector.females.seed <<- mortality.Zvector.females$seed[1] 
mortality.Zvector.females <<- mortality.Zvector.females[,colnames(mortality.Zvector.females) != "seed"] 




#









##------------------------------------------ load the file
gtkEntrySetText(entry_Zseedvalue_M, mortality.Zvector.males.seed)
Zvector_M <<- list()
Zvector_MIndex <<- 0
add.Zvector_M()
  Zvector_M.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 12), "gboolean")  
  for (i in 1:length(Zvector_M)) {
    iter <-  Zvector_M.model$append()$iter
     Zvector_M.model$set(iter,0, Zvector_M[[i]]$year)
   #print(paste("in model:", as.character(Zvector_M[[i]]$year)))
    for (e in 1:length(MONTHS)) {
        Zvector_M.model$set(iter, e, as.double(Zvector_M[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
      # print(paste("in model:", Zvector_M[[i]][e]) )
    }
     Zvector_M.model$set(iter,13,TRUE)
  } 

#
  Zvector_M.treeview$destroy()
 Zvector_M.treeview <<- gtkTreeViewNewWithModel( Zvector_M.model)
 Zvector_M.treeview$setRulesHint(TRUE)
 Zvector_M.treeview$getSelection()$setMode("single")
Zvector_M.add_columns( Zvector_M.treeview)

Zvector_M.sw$add(Zvector_M.treeview)


#------------------------------------------ load the file
gtkEntrySetText(entry_Zseedvalue_F, mortality.Zvector.females.seed)
Zvector_F <<- list()
Zvector_FIndex <<- 0
add.Zvector_F()
  Zvector_F.model <<- gtkListStoreNew("gchararray",  rep("gdouble", 12), "gboolean")  
  for (i in 1:length(Zvector_F)) {
    iter <-  Zvector_F.model$append()$iter
     Zvector_F.model$set(iter,0, Zvector_F[[i]]$year)
    #print(paste("in model:", as.character(Zvector_F[[i]]$year)))
    for (e in 1:length(MONTHS)) {
        Zvector_F.model$set(iter, e, as.double(Zvector_F[[i]][e+1]))          # as.double(sexratios[[ind]][nc_i+1]) 
     #  print(paste("in model:", Zvector_F[[i]][e]) )
    }
     Zvector_F.model$set(iter,13,TRUE)
  } 

#
  Zvector_F.treeview$destroy()
 Zvector_F.treeview <<- gtkTreeViewNewWithModel( Zvector_F.model)
 Zvector_F.treeview$setRulesHint(TRUE)
 Zvector_F.treeview$getSelection()$setMode("single")
Zvector_F.add_columns( Zvector_F.treeview)
Zvector_F.sw$add(Zvector_F.treeview)



#}
#}
#
# inizialize all the matrices to be imported
#

 
# if (!IN_BEMTOOL) {
n_ages_M  <- as.numeric(as.character(gtkEntryGetText(entryVBF_M_lifespan)))  
n_ages_F  <- as.numeric(as.character(gtkEntryGetText(entryVBF_F_lifespan)))  

first_age_mal <- 0
first_age_fem <- 0

   n_ages_M <- n_ages_M - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)
   n_ages_F <- n_ages_F - trunc(Tr/12)
    first_age_fem <- trunc(Tr/12)

 l_inf_F <- as.numeric(gtkEntryGetText(entryVBFLinf_F_max)) 
  l_inf_M <- as.numeric(gtkEntryGetText(entryVBFLinf_M_max))   
l_inf_lens_F <-c(0:(round(l_inf_F,0)+1))
l_inf_lens_M <-c(0:(round(l_inf_M,0)+1))


#} # end BEMTOOL situation


gtkComboBoxSetActive(combo_fleetsegments, 0)
gtkComboBoxSetActive(combo_fleetsegments_fore, 0)

gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook, 5), T)

#if (!IN_BEMTOOL) {
#  reload_fleetsegment_info()
#} 


#if (!IN_BEMTOOL) {
#gtkComboBoxSetActive(combo_fleetsegments, 0 ) 
#gtkComboBoxSetActive(combo_fleetsegments_fore, 0 ) 
#} 
      
 if (showCompTime)  {
# SIMULATION_EXPLOITED_ptm <- proc.time()
proc_ <- proc.time()
print(paste("setBiologicalParams_fromEnv [time]::::::::::::::::::::::::::::::::", round(as.numeric(proc_[3]-setBiologicalParams_fromEnv_ptm[3]),2), "sec" ), quote=F )   
#print(proc.time() - setBiologicalParams_ptm, quote=F ) 
rm(setBiologicalParams_fromEnv_ptm)
}

}

