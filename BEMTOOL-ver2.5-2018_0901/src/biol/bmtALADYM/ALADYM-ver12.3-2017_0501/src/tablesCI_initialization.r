# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


if (length(FLEETSEGMENTS_names)!=1) {
mortalities_ALLruns_head <<- c("Year","Z_estimated_monthly","Z_estimated_of_males_monthly","Z_estimated_of_females_monthly","Annual_Z_estimated_of_males_Sinclair","Annual_Z_estimated_of_females_Sinclair","Annual_Z_estimated_life_span","Annual_Z_estimated","F_estimated_monthly", paste("F_estimated_monthly_",FLEETSEGMENTS_names, sep=""),"Annual_F_estimated_weighted","Annual_F_estimated_of_males_weighted", "Annual_F_estimated_of_females_weighted",paste("Annual_F_estimated_",FLEETSEGMENTS_names,"_weighted",sep=""),"Annual_F_estimated_life_span",paste("Annual_F_estimated_ls_",FLEETSEGMENTS_names,sep=""),"Annual_F_estimated",paste("Annual_F_estimated_",FLEETSEGMENTS_names,sep=""), "run")

mortalities_CI_selection <<- c("Year","Annual_Z_estimated_life_span","Annual_Z_estimated","Annual_F_estimated_life_span",paste("Annual_F_estimated_ls_",FLEETSEGMENTS_names,sep=""),"Annual_F_estimated",paste("Annual_F_estimated_",FLEETSEGMENTS_names,sep=""), "run")

}  else {
mortalities_ALLruns_head <<- c("Year","Z_estimated_monthly","Z_estimated_of_males_monthly","Z_estimated_of_females_monthly","Annual_Z_estimated_of_males_Sinclair","Annual_Z_estimated_of_females_Sinclair","Annual_Z_estimated_life_span","Annual_Z_estimated","F_estimated_monthly","Annual_F_estimated_weighted","Annual_F_estimated_of_males_weighted", "Annual_F_estimated_of_females_weighted","Annual_F_estimated_life_span","Annual_F_estimated", "run")

mortalities_CI_selection <<- c("Year","Annual_Z_estimated_life_span","Annual_Z_estimated","Annual_F_estimated_life_span","Annual_F_estimated", "run")
}




Population_ALLruns_head <<- c("Year","Total_biomass_exploited_pop","Total_biomass_unexploited_pop","SS_NUMBERS_exploited_pop","SS_NUMBERS_unexploited_pop","SSB_exploited_pop","SSB_unexploited_pop","ESSBratioUSSB","Mean_length_of_exploited_pop","Mean_length_of_unexploited_pop","Mean_length_SS_of_exploited_pop","Mean_length_SS_of_unexploited_pop","Mean_age_of_exploited_pop","Mean_age_of_unexploited_pop","Mean_age_SS_of_exploited_pop","Mean_age_SS_of_unexploited_pop", "NUMBERS_exploited_pop","NUMBERS_unexploited_pop","run")

Population_CI_selection <<- c("Total_biomass_exploited_pop","Total_biomass_unexploited_pop","SSB_exploited_pop","SSB_unexploited_pop","ESSBratioUSSB","Mean_length_of_exploited_pop","Mean_length_of_unexploited_pop","Mean_length_SS_of_exploited_pop","Mean_length_SS_of_unexploited_pop")

# --------------------------------------------- (-1)
#Population_CI_selection <<- c("Total_biomass_exploited_pop","Total_biomass_unexploited_pop","SSB_exploited_pop","SSB_unexploited_pop","Mean_length_of_exploited_pop","Mean_length_of_unexploited_pop","Mean_length_SS_of_exploited_pop","Mean_length_SS_of_unexploited_pop")



if (length(FLEETSEGMENTS_names)==1) {
Production_ALLruns_head <<- c("Year","Biological_Production","Natural_death_biomass","Total_Yield","Mean_length_in_catch","Mean_age_in_catch","Total_Landing","Mean_length_in_Landing","Mean_age_in_Landing","Total_Discard","Mean_length_in_Discard","Mean_age_in_Discard", "Discard_ratio", "LandingObligation", "run")

Production_CI_selection <<- c("Biological_Production","Natural_death_biomass","Total_Yield","Mean_length_in_catch","Total_Landing","Mean_length_in_Landing","Total_Discard","Mean_length_in_Discard","Discard_ratio") 
# ---------------------------------------- (-4)    
#Production_CI_selection <<- c("Natural_death_biomass", "Mean_length_in_catch","Mean_length_in_Landing","Mean_length_in_Discard", "Discard_ratio")

}  else {
Production_ALLruns_head <<-  c("Year","Biological_Production","Natural_death_biomass","Total_Yield",paste("Yield_",FLEETSEGMENTS_names, sep=""),"Mean_length_in_catch",paste("Mean_length_in_catch",FLEETSEGMENTS_names, sep=""),"Mean_age_in_catch",paste("Mean_age_in_catch_",FLEETSEGMENTS_names, sep=""),"Total_Landing",paste("Landing_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_Landing",paste("Mean_length_in_Landing_",FLEETSEGMENTS_names,sep=""),"Mean_age_in_Landing",paste("Mean_age_in_Landing_",FLEETSEGMENTS_names,sep=""),"Total_Discard",paste("Discard_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_Discard",paste("Mean_length_in_Discard_",FLEETSEGMENTS_names,sep=""),"Mean_age_in_Discard",paste("Mean_age_in_Discard_",FLEETSEGMENTS_names,sep=""), "Discard_ratio",  paste("Discard_ratio_",FLEETSEGMENTS_names,sep=""), paste("LandingObligation_",FLEETSEGMENTS_names,sep=""),"run")

# ---------------------------------------------- (-4 AND BY FLEET SEGMENTS)
# Production_CI_selection <<-  c("Natural_death_biomass", "Mean_length_in_catch",paste("Mean_length_in_catch",FLEETSEGMENTS_names, sep=""), paste("Landing_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_Landing",paste("Mean_length_in_Landing_",FLEETSEGMENTS_names,sep=""),paste("Discard_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_Discard",paste("Mean_length_in_Discard_",FLEETSEGMENTS_names,sep=""))

Production_CI_selection <<-  c("Natural_death_biomass","Biological_Production","Total_Yield",paste("Yield_",FLEETSEGMENTS_names, sep=""),"Mean_length_in_catch",paste("Mean_length_in_catch",FLEETSEGMENTS_names, sep=""),"Total_Landing",paste("Landing_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_Landing",paste("Mean_length_in_Landing_",FLEETSEGMENTS_names,sep=""),"Total_Discard",paste("Discard_",FLEETSEGMENTS_names,sep=""),"Mean_length_in_Discard",paste("Mean_length_in_Discard_",FLEETSEGMENTS_names,sep=""), "Discard_ratio", paste("Discard_ratio_",FLEETSEGMENTS_names,sep=""))
}





Indicators_ALLruns_head <<- c("Years","Exploitation_rate","Harvest_ratio","Critical_length_unexpl_pop","Critical_age_unexpl_pop","Critical_length_expl_pop","Critical_age_expl_pop","unex_stock_L0_95","ex_stock_L0_95","catch_L0_95", "run")

Indicators_CI_selection <<- c("Exploitation_rate","Harvest_ratio","Critical_length_unexpl_pop","Critical_length_expl_pop")
max_age = max(INP$MGrowth_tend, INP$FGrowth_tend) #max(max_ageM,max_ageF)

Catches_by_age_ALLruns_head <<-  c("Year_Age",paste("Age_", c(seq(trunc(INP$tr/INP$Time_slice),(max_age-1),1)), sep=""),"sex","Gear", "run")

#if (FALSE) {

Landings_by_age_ALLruns_head <<-  c("Year_Age",paste("Age_",c(seq(trunc(INP$tr/INP$Time_slice),(max_age-1),1)), sep=""),"sex","Gear", "run")
Discards_by_age_ALLruns_head <<-  c("Year_Age",paste("Age_",c(seq(trunc(INP$tr/INP$Time_slice),(max_age-1),1)), sep=""),"sex","Gear", "run")

Annual_F_by_gear_ALLruns_head <<-  c("Year_Age",paste("Age_",c(seq(trunc(INP$tr/INP$Time_slice),(max_age-1),1)), sep=""),"sex","Gear", "run")

MortalityChange_ALLruns_head <<- c("Year", paste("Annual_reduction_", FLEETSEGMENTS_names, sep=""), "run" ) 

if (length(FLEETSEGMENTS_names)!=1){
MortalityChange_ALLruns_head <<- c("Year",paste("Annual_reduction_",FLEETSEGMENTS_names,sep=""), "Annual_reduction", "run")
}  else {
MortalityChange_ALLruns_head <<- c("Year","Annual_reduction", "run")
}

#}

Recruitment_ALLruns_head <<- c("Year", "Recruitment", "run")

Recruitment_ALLruns <<- data.frame(matrix(nrow=0, ncol=length(Recruitment_ALLruns_head)))
 colnames(Recruitment_ALLruns)  <- Recruitment_ALLruns_head

mortalities_ALLruns <<- data.frame(matrix(nrow=0, ncol=length(mortalities_ALLruns_head)))
colnames(mortalities_ALLruns)  <- mortalities_ALLruns_head

mortalities_change_ALLruns <<- data.frame(matrix(nrow=0, ncol=length(MortalityChange_ALLruns_head)))
colnames(mortalities_change_ALLruns)  <- MortalityChange_ALLruns_head

Population_ALLruns <<- data.frame(matrix(nrow=0, ncol=length(Population_ALLruns_head)))
colnames(Population_ALLruns)  <- Population_ALLruns_head

 Production_ALLruns <<- data.frame(matrix(nrow=0, ncol=length(Production_ALLruns_head)))
colnames( Production_ALLruns)  <- Production_ALLruns_head

#Discard_ALLruns <<- data.frame(matrix(nrow=0, ncol=length(Discard_ALLruns_head)))
#colnames( Discard_ALLruns)  <- Discard_ALLruns_head

Indicators_ALLruns <<- data.frame(matrix(nrow=0, ncol=length(Indicators_ALLruns_head)))
colnames( Indicators_ALLruns)  <- Indicators_ALLruns_head

#if (FALSE) {

Catches_by_age_ALLruns <<- data.frame(matrix(nrow=0, ncol=length(Catches_by_age_ALLruns_head)))
colnames( Catches_by_age_ALLruns)  <- Catches_by_age_ALLruns_head

Landings_by_age_ALLruns <<- data.frame(matrix(nrow=0, ncol=length(Landings_by_age_ALLruns_head)))
colnames( Landings_by_age_ALLruns)  <- Landings_by_age_ALLruns_head

Discards_by_age_ALLruns <<- data.frame(matrix(nrow=0, ncol=length(Discards_by_age_ALLruns_head)))
colnames( Discards_by_age_ALLruns)  <- Discards_by_age_ALLruns_head




Annual_F_by_gear_ALLruns <<- data.frame(matrix(nrow=0, ncol=length(Annual_F_by_gear_ALLruns_head)))
colnames( Annual_F_by_gear_ALLruns)  <- Annual_F_by_gear_ALLruns_head
#}

