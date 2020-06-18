# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




  indicators_CI <- function (End, mortality, production, population) {

    temp_digits_opt <- getOption("digits")
 options(digits = 15)

	if (FALSE) {
	     mortality  =  exported_tables[[1]]
	     production =  exported_tables[[2]]
			  population  =  exported_tables[[3]]
	}
# Exploitation rate
  loca <- 1:( End/ INP$Time_slice)
     
 # mortality = read.table(MORTALITY_table,header=TRUE,sep=";")
  
  gears= as.character(t(FLEETSEGMENTS_names))
  
#if (nb_gears!=1){
#colnames (mortality) = c("Year","Z_estimated_monthly","Z_estimated_of_males(monthly)","Z_estimated_of_females(monthly)","Annual_Z_estimated_of_males (Sinclair)","Annual_Z_estimated_of_females (Sinclair)","Annual_Z_estimated_life_span","Annual_Z_estimated","F_estimated_monthly", paste("F_estimated_monthly_",gears),"Annual_F_estimated(weighted)","Annual_F_estimated_of_males(weighted)", "Annual_F_estimated_of_females(weighted)",paste("Annual_F_estimated",gears,"(weighted)",sep=""),"Annual_F_estimated_life_span",paste("Annual_F_estimated_ls_",gears,sep=""),"Annual_F_estimated",paste("Annual_F_estimated_",gears,sep=""))
#}  else {
#colnames (mortality) = c("Year","Z_estimated_monthly","Z_estimated_of_males(monthly)","Z_estimated_of_females(monthly)","Annual_Z_estimated_of_males (Sinclair)","Annual_Z_estimated_of_females (Sinclair)","Annual_Z_estimated_life_span","Annual_Z_estimated","F_estimated_monthly","Annual_F_estimated(weighted)","Annual_F_estimated_of_males(weighted)", "Annual_F_estimated_of_females(weighted)","Annual_F_estimated_life_span","Annual_F_estimated")
#}

indicators = data.frame(matrix(mortality[,1], ncol=1))

total_mortality = matrix(0,ncol=1,nrow=nrow(indicators))

for (r in 1:nrow(indicators)){       # Z su l'age range deciso dall'utente
total_mortality[r] = mortality[r,colnames(mortality)=="Annual_Z_estimated"]
} 

explotation_rate = mortality[,colnames(mortality)=="Annual_F_estimated"]/total_mortality
indicators = cbind(indicators,data.frame(matrix(round(explotation_rate,3), ncol=1)) )

# Harvest ratio
#--------------------------
# production = read.table(PRODUCTION_table,header=TRUE,sep=";")
   
#if(nb_gears==1) {
#colnames (production) = c("Year","Biological_Production","Natural death_biomass","Total_Yield","Mean_length_in_catch","Mean_age_in_catch","Total_Landing","Mean_length_in_Landing","Mean_age_in_Landing","Total_Discard","Mean_length_in_Discard","Mean_age_in_Discard","Discard_ratio")
#
#}  else {
#colnames (production) = c("Year","Biological_Production","Natural_death_biomass","Total_Yield",paste("Yield_",gears,sep=""),"Mean_length_in_catch",paste("Mean_length_in_catch_",gears,sep=""),"Mean_age_in_catch",paste("Mean_age_in_catch_",gears,sep=""),"Total_Landing",paste("Landing_",gears,sep=""),"Mean_length_in_Landing",paste("Mean_length_in_Landing_",gears,sep=""),"Mean_age_in_Landing",paste("Mean_age_in_Landing_",gears,sep=""),"Total_Discard",paste("Discard_",gears,sep=""),"Mean_length_in_Discard",paste("Mean_length_in_Discard_",gears,sep=""),"Mean_age_in_Discard",paste("Mean_age_in_Discard_",gears,sep=""),"Discard_ratio")
#}

# population = read.table(POPULATION_table,header=TRUE,sep=";")

harvest_ratio = as.numeric(as.character(production$Total_Yield))/as.numeric(as.character(population$Total_biomass_exploited_pop ))

indicators = cbind(indicators,data.frame(matrix(round(harvest_ratio,3), ncol=1)))
#--------------------------
Max=max(length(BAS$MWeight),length(BAS$FWeight))

# Critical length (exploited)
Biomass_exp = matrix(nrow=(GLO$L_number + 1),ncol=max(length(BAS$MWeight),length(BAS$FWeight)))
for(loca_i in (1:(GLO$L_number + 1))) {   # tempo       
          for (g in 1: Max){              # età
  Biomass_exp[loca_i,g] = ifelse(g <= length(BAS$MWeight),  SRO$MFPopulation[loca_i,g] * BAS$MWeight[g],0)
  Biomass_exp[loca_i,g] = Biomass_exp[loca_i,g] + ifelse(g <= length(BAS$FWeight),  SRO$FFPopulation[loca_i,g] * BAS$FWeight[g],0)
  }
}
  
critical_length_exp = matrix(ncol=1,nrow=GLO$L_number + 1)
critical_age_exp = matrix(ncol=1,nrow=GLO$L_number + 1)


mean_len = matrix(ncol=1,nrow=Max)
mean_age = matrix(ncol=1,nrow=Max)

# lunghezze medie tra maschi e femmine
for (i in 1:Max) {
mean_len[i] = ifelse(!is.na(mean(BAS$MLength[i],BAS$FLength[i])),mean(BAS$MLength[i],BAS$FLength[i]),na.omit(c(BAS$MLength[i],BAS$FLength[i]))[1])
mean_age[i] = ifelse(!is.na(mean(BAS$MAge[i],BAS$FAge[i])),mean(BAS$MAge[i],BAS$FAge[i]),na.omit(c(BAS$MAge[i],BAS$FAge[i]))[1]) 
}
 
for(loca_i in (1:(GLO$L_number + 1))) {
  critical_length_exp[loca_i] = mean_len[which(Biomass_exp[loca_i,]==max(Biomass_exp[loca_i,]))] [1]
  critical_age_exp[loca_i] =  mean_age[which(Biomass_exp[loca_i,]==max(Biomass_exp[loca_i,]))]  [1]
}

#--------------------------

# Critical length (unexploited)

Biomass_unexp = matrix(nrow=(GLO$L_number + 1),ncol=max(length(BAS$MWeight),length(BAS$FWeight)))
for(loca_i in (1:(GLO$L_number + 1))) {   # tempo
          for (g in 1: Max){              # età
  Biomass_unexp[loca_i,g] = ifelse(g <= length(BAS$MWeight),  SRO$MUPopulation[loca_i,g] * BAS$MWeight[g],0)
  Biomass_unexp[loca_i,g] = Biomass_unexp[loca_i,g] + ifelse(g <= length(BAS$FWeight),  SRO$FUPopulation[loca_i,g] * BAS$FWeight[g],0)
  }
}
  
critical_length_unexp = matrix(ncol=1,nrow=GLO$L_number + 1)
critical_age_unexp = matrix(ncol=1,nrow=GLO$L_number + 1)

 
for(loca_i in (1:(GLO$L_number + 1))) {
  critical_length_unexp[loca_i] = mean_len[which(Biomass_unexp[loca_i,]==max(Biomass_unexp[loca_i,]))]
  critical_age_unexp[loca_i] =  mean_age[which(Biomass_unexp[loca_i,]==max(Biomass_unexp[loca_i,]))]
}
#--------------------------
len_unexp_ann = meanWequals(critical_length_unexp , GLO$L_number + 1, INP$Time_slice) [loca]
age_unexp_ann = meanWequals(critical_age_unexp , GLO$L_number + 1, INP$Time_slice) [loca]

len_exp_ann = meanWequals(critical_length_exp , GLO$L_number + 1, INP$Time_slice) [loca]
age_exp_ann = meanWequals(critical_age_exp , GLO$L_number + 1, INP$Time_slice)  [loca]


indicators = cbind(indicators,cbind(round(len_unexp_ann,3)[loca],cbind(round(age_unexp_ann,3),cbind(round(len_exp_ann,3),round(age_exp_ann,3)))))

   
 
 #indicators[(forecast/12):(GLO$L_number/12),4:7] = NA
# Percentiles unexploited stock
  Numbers_unexp = matrix(nrow=(GLO$L_number + 1),ncol=max(length(BAS$MWeight),length(BAS$FWeight)))
  
  for(loca_i in (1:(GLO$L_number + 1))) {   # tempo
          for (g in 1: Max){              # età
  Numbers_unexp[loca_i,g] = ifelse(g <= length(BAS$MWeight),  SRO$MUPopulation[loca_i,g],0)
  Numbers_unexp[loca_i,g] = Numbers_unexp[loca_i,g] + ifelse(g <= length(BAS$FWeight),  SRO$FUPopulation[loca_i,g],0)
  }
}
  
  Numbers_ann =  matrix(nrow=(GLO$L_number/12),ncol=max(length(BAS$MWeight),length(BAS$FWeight)))
  for (i in 1:ncol(Numbers_unexp)) {
  Numbers_ann[,i] = meanWequals(Numbers_unexp[1:(GLO$L_number + 1),i] , GLO$L_number + 1, INP$Time_slice)
  }
  Freq = cbind(mean_len,t(Numbers_ann))
  
  quant = matrix(nrow = (GLO$L_number/12), ncol=5)
  for (i in 1: (GLO$L_number/12)) {
  quant[i,] = quantiles(Freq[,c(1,(i+1))])
  }

indicators = cbind(indicators,round(quant[,5],3)[loca] )


# Percentiles exploited stock
  Numbers_exp = matrix(nrow=(GLO$L_number + 1),ncol=max(length(BAS$MWeight),length(BAS$FWeight)))
  
  for(loca_i in (1:(GLO$L_number + 1))) {   # tempo
          for (g in 1: Max){              # età
  Numbers_exp[loca_i,g] = ifelse(g <= length(BAS$MWeight),  SRO$MFPopulation[loca_i,g],0)
  Numbers_exp[loca_i,g] = Numbers_exp[loca_i,g] + ifelse(g <= length(BAS$FWeight),  SRO$FFPopulation[loca_i,g],0)
  }
}
  
  Numbers_ann =  matrix(nrow=(GLO$L_number/12),ncol=max(length(BAS$MWeight),length(BAS$FWeight)))
  for (i in 1:ncol(Numbers_exp)) {
  Numbers_ann[,i] = meanWequals(Numbers_exp[1:(GLO$L_number + 1),i] , GLO$L_number + 1, INP$Time_slice)
  }
  Freq = cbind(mean_len,t(Numbers_ann))
  
  quant = matrix(nrow = (GLO$L_number/12), ncol=5)
  for (i in 1: (GLO$L_number/12)) {
  quant[i,] = quantiles(Freq[,c(1,(i+1))])
  }

indicators = cbind(indicators,round(quant[,5],3)[loca] )


    
# Percentiles catch
  Numbers_catch = matrix(nrow=(GLO$L_number + 1),ncol=max(length(BAS$MWeight),length(BAS$FWeight)))
  
  for(loca_i in (1:(GLO$L_number + 1))) {   # tempo
          for (g in 1: Max){              # età
  Numbers_catch[loca_i,g] = ifelse(g <= length(BAS$MWeight),  SRO$MFCatch[loca_i,g],0)
  Numbers_catch[loca_i,g] = Numbers_catch[loca_i,g] + ifelse(g <= length(BAS$FWeight),  SRO$FFCatch[loca_i,g],0)
  }
}
  
  Numbers_ann =  matrix(nrow=(GLO$L_number/12),ncol=max(length(BAS$MWeight),length(BAS$FWeight)))
  for (i in 1:ncol(Numbers_catch)) {
  Numbers_ann[,i] = meanWequals(Numbers_catch[1:(GLO$L_number + 1),i] , GLO$L_number + 1, INP$Time_slice)
  }
  Freq = cbind(mean_len,t(Numbers_ann))
  
  quant = matrix(nrow = (GLO$L_number/12), ncol=5)
  for (i in 1: (GLO$L_number/12)) {
  quant[i,] = quantiles(Freq[,c(1,(i+1))])
  }

indicators = cbind(indicators,round(quant[,5],3)[loca] )
   
#colnames(indicators) = c("Years","Exploitation_rate","Harvest_ratio","Critical_length_unexpl_pop","Critical_age_unexpl_pop","Critical_length_expl_pop","Critical_age_expl_pop","unex_stock_L0_95","ex_stock_L0_95","catch_L0_95")
indicators <- data.frame(indicators)
colnames(indicators) <- Indicators_ALLruns_head[-length(Indicators_ALLruns_head)]

indicators <- data.frame(cbind(indicators, rep(current_runCI, nrow(indicators)))  )
	 colnames(indicators)  <- Indicators_ALLruns_head
	 
	 if (!INTEGRATED_APPROACH) {
Indicators_ALLruns <<- data.frame(rbind(Indicators_ALLruns, indicators))
} else {
if (current_year == 1) {
INP$Indicators_ALLruns <- data.frame(rbind(INP$Indicators_ALLruns, indicators))
} else {
  INP$Indicators_ALLruns <- data.frame(rbind(INP$Indicators_ALLruns, indicators[indicators$Years == years_forecast[current_year],]))
  #tow3[tow3$Year_Age == years_forecast[current_year],]))  
}
}

 options(digits = temp_digits_opt)


#write.table(indicators, INDICATORS_table,row.names=FALSE, sep=";")  

}

 
