# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

 
 
 
 
 

   if (showCompTime)  {
runCI_totalTIME_ptm <- proc.time()  
}

INP$nruns = as.numeric(as.character(new_aldForecast@CI_n_runs))     #    nruns<-input from GUI        # OK 
  
if (current_year == 1) {

# **************************************************************** MATURITY
  if (new_aldForecast@CI_maturity) {
paramsM=data.frame(ML50_M=as.numeric(as.character(new_aldForecast@CI_maturity_m_params$Mean_L50_MR[1])), ML_M=as.numeric(as.character(new_aldForecast@CI_maturity_m_params$Mean_L50_MR[2])))
paramsF=data.frame(ML50_F=as.numeric(as.character(new_aldForecast@CI_maturity_f_params$Mean_L50_MR[1])), ML_F=as.numeric(as.character(new_aldForecast@CI_maturity_f_params$Mean_L50_MR[2])))
SDsM=data.frame(SD_ML50_M= as.numeric(as.character(new_aldForecast@CI_maturity_m_params$StDev_L50_MR[1])), SD_ML_M=as.numeric(as.character(new_aldForecast@CI_maturity_m_params$StDev_L50_MR[2])))
SDsF=data.frame(SD_ML50_F= as.numeric(as.character(new_aldForecast@CI_maturity_f_params$StDev_L50_MR[1])), SD_ML_FM=as.numeric(as.character(new_aldForecast@CI_maturity_f_params$StDev_L50_MR[2])))
  if (new_aldForecast@CI_maturity_f_file_or_distribution == 2) {
		APPROACH="D" # or "E"
   	DIST= new_aldForecast@CI_maturity_f_distribution
  } else {
  		APPROACH="E" # or "E"
  } 
  ext_vector_mat_F <- new_aldForecast@CI_maturity_f_external_vector 
INP$Mat_grid_F  = get_grid_maturity(APPROACH,DIST,paramsF,SDsF,INP$nruns, ext_vector_mat_F) 
write.table(INP$Mat_grid_F , file = paste(tablesDIR_input,"/Maturity_F_uncertainty_grid_", INP$nruns ,"runs.csv",sep=""), sep=";", row.names=F)

   if (new_aldForecast@CI_maturity_m_file_or_distribution == 2) {
		APPROACH="D" # or "E"
   	DIST= new_aldForecast@CI_maturity_m_distribution
  } else {
  		APPROACH="E" # or "E"
  }
  
  ext_vector_mat_M <- new_aldForecast@CI_maturity_m_external_vector
INP$Mat_grid_M  = get_grid_maturity(APPROACH,DIST,paramsM,SDsM,INP$nruns, ext_vector_mat_M) 

  write.table(INP$Mat_grid_M , file = paste(tablesDIR_input,"/Maturity_M_uncertainty_grid_", INP$nruns ,"runs.csv",sep=""), sep=";", row.names=F)
}

# **************************************************************** GROWTH	
if (new_aldForecast@CI_growth) {     # S-R

	params_det_F=data.frame(Linf=BAS$F_mean_Linf , K=BAS$F_mean_k , t0=BAS$F_mean_t0)
	params_det_M=data.frame(Linf=BAS$M_mean_Linf , K=BAS$M_mean_k , t0=BAS$M_mean_t0)	
	
  if (new_aldForecast@CI_M) { 
    if(INP$FOPT_M_TYPE==4) { #Prodbiom da cancellare una volta che si e inserita la cella nell'interfaccia
INP$FMtmax =  as.numeric(as.character(new_aldForecast@CI_M_model$Mtmax_mf[2])) #unico input da inserire da GUI
l50F=mean(c(INP$FML50p_min, INP$FML50p_max))
tmaxF=INP$FGrowth_tend - 1
} else {
INP$FMtmax=NA
l50F=NA
tmaxF=NA
}
if(INP$MOPT_M_TYPE==4){
INP$MMtmax = as.numeric(as.character(new_aldForecast@CI_M_model$Mtmax_mf[1]))
l50M=mean(c(INP$MML50p_min, INP$MML50p_max))
tmaxM=INP$MGrowth_tend - 1
}else {
INP$MMtmax=NA
l50M=NA
tmaxM=NA}
 }
lengths_or <<- BAS$FLength
lengths=  lengths_or
ages =BAS$FAge 
if (new_aldForecast@CI_growth_Linf_k == 1) {
  if (new_aldForecast@CI_growth_1_file_or_distribution == 2) {
		APPROACH="D" # or "E"
  } else {
  		APPROACH="E" # or "E"
  }
} else {
  if (new_aldForecast@CI_growth_2_file_or_distribution == 2) {
		APPROACH="D" # or "E"
  } else {
  		APPROACH="E" # or "E"
  }
}
if (new_aldForecast@CI_growth_Linf_k == 1) {
  if (new_aldForecast@CI_growth_1_file_or_distribution == 2) {
		DIST= new_aldForecast@CI_growth_1_2_distribution
  } 
} else {
  if (new_aldForecast@CI_growth_2_file_or_distribution == 2) {
		DIST= new_aldForecast@CI_growth_2_2_distribution
  } 
}
 
if (new_aldForecast@CI_growth_Linf_k == 1) {
	Free="Linf" # or "K"
	params=data.frame(Linf=as.numeric(as.character(new_aldForecast@CI_growth_1_2_params$Mean_mf[2])) , K=BAS$F_mean_k, t0=BAS$F_mean_t0)
	SDs=data.frame(SD_Linf= as.numeric(as.character(new_aldForecast@CI_growth_1_2_params$StDev_mf[2])) , SD_K=NA)
} else {
	Free="K" # or "K"
	params=data.frame(Linf=BAS$F_mean_Linf , K=as.numeric(as.character(new_aldForecast@CI_growth_2_2_params$Mean_mf[2])), t0=BAS$F_mean_t0)
	SDs=data.frame(SD_Linf= NA, SD_K=as.numeric(as.character(new_aldForecast@CI_growth_2_2_params$StDev_mf[2])))
}
 
 if (new_aldForecast@CI_growth_Linf_k == 1) { 
  if (new_aldForecast@CI_growth_1_file_or_distribution == 1) {
		 ext_vector_growth_F <- new_aldForecast@CI_growth_1_1_external_vector[, 3]
  } else {
  ext_vector_growth_F <- NULL
  }
  } else {  
  if (new_aldForecast@CI_growth_2_file_or_distribution == 1) {
		 ext_vector_growth_F <- new_aldForecast@CI_growth_2_1_external_vector[, 3]
  } else {
  ext_vector_growth_F <- NULL
  }
  }
  
INP$VB_gridF  = get_grid_growth(APPROACH,DIST,Free,params,SDs,INP$nruns ,ages,lengths_or, ext_vector_growth_F) 
INP$VB_gridF$t0=rep(BAS$F_mean_t0,INP$nruns )
# write.table(INP$VB_gridF,"grid_growthF.csv",sep=";",row.names=F)
   write.table(INP$VB_gridF , file = paste(tablesDIR_input,"/Growth_F_uncertainty_grid_", INP$nruns ,"runs.csv",sep=""), sep=";", row.names=F) 
VB_CI_F= INP$VB_gridF
 
  if (new_aldForecast@CI_M) { 
mort_CI_F = get_grid_mortality(INP$FOPT_M_TYPE,INP$FMtmax,INP$VB_gridF,BAS$FLength,BAS$FAge,l50F,tmaxF,INP$FWLa,INP$FWLb)
# write.table(mort_CI_F,"grid_M_females.csv",sep=";",row.names=F) 

    write.table(mort_CI_F, file = paste(tablesDIR_input,"/NaturalMortality_F_uncertainty_grid_", INP$nruns ,"runs.csv",sep=""), sep=";", row.names=F) 
}

if (new_aldForecast@CI_growth_Linf_k == 1) {
	Free="Linf" # or "K"
	params=data.frame(Linf=as.numeric(as.character(new_aldForecast@CI_growth_1_2_params$Mean_mf[1])) , K=BAS$M_mean_k, t0=BAS$M_mean_t0)
	SDs=data.frame(SD_Linf= as.numeric(as.character(new_aldForecast@CI_growth_1_2_params$StDev_mf[1])) , SD_K=NA)
} else {
	Free="K" # or "K"
	params=data.frame(Linf=BAS$M_mean_Linf , K=as.numeric(as.character(new_aldForecast@CI_growth_2_2_params$Mean_mf[1])), t0=BAS$M_mean_t0)
	SDs=data.frame(SD_Linf= NA, SD_K=as.numeric(as.character(new_aldForecast@CI_growth_2_2_params$StDev_mf[1])))
}

lengths_or <<- BAS$MLength
lengths=  lengths_or
ages =BAS$MAge 

 if (new_aldForecast@CI_growth_Linf_k == 1) { 
  if (new_aldForecast@CI_growth_1_file_or_distribution == 1) {
		 ext_vector_growth_M <- new_aldForecast@CI_growth_1_1_external_vector[, 2]
  } else {
  ext_vector_growth_M <- NULL
  }
  } else {  
  if (new_aldForecast@CI_growth_2_file_or_distribution == 1) {
		 ext_vector_growth_M <- new_aldForecast@CI_growth_2_1_external_vector[, 2]
  } else {
  ext_vector_growth_M <- NULL
  }
  }
   
INP$VB_gridM  = get_grid_growth(APPROACH,DIST,Free,params,SDs,INP$nruns ,ages,lengths_or, ext_vector_growth_M) 
INP$VB_gridM$t0=rep(BAS$M_mean_t0,INP$nruns )

     write.table(INP$VB_gridM, file = paste(tablesDIR_input,"/Growth_M_uncertainty_grid_", INP$nruns ,"runs.csv",sep=""), sep=";", row.names=F) 
# write.table(INP$VB_gridM,"grid_growthM.csv",sep=";",row.names=F)

VB_CI_M= INP$VB_gridM   

  if (new_aldForecast@CI_M) {       
mort_CI_M=get_grid_mortality(INP$MOPT_M_TYPE,INP$FMtmax,INP$VB_gridM,BAS$MLength,BAS$MAge,l50M,tmaxM,INP$MWLa,INP$MWLb)
 write.table(mort_CI_M, file = paste(ALADYM_home, "/",scenario_fold,"/NaturalMortality_M_uncertainty_grid_", INP$nruns ,"runs.csv",sep=""), sep=";", row.names=F) 
      write.table(mort_CI_M, file = paste(tablesDIR_input,"/NaturalMortality_M_uncertainty_grid_", INP$nruns ,"runs.csv",sep=""), sep=";", row.names=F) 
#write.table(mort_CI_M,"grid_M_males.csv",sep=";",row.names=F)
}

# ELIMINATA UNA PARTE DI CODICE  #**********************************************************    VEDI IN RUN CI.R USATO IN RETE TRE
    } 

 
 
 # **************************************************************** RECRUITMENT   
if (new_aldForecast@CI_recruitment) {
   
    if (new_aldForecast@CI_recruitment_constant_or_SRR == 2) {
if (new_aldForecast@CI_recruitment_2_file_or_distribution == 2) {
loca_para <- new_aldForecast@CI_recruitment_2_2_distribution_params
params=data.frame(param1=loca_para$Mean_abc[1], param2=loca_para$Mean_abc[2],param3=loca_para$Mean_abc[3])
SDs=data.frame(SD1=loca_para$StDev_abc[1], SD2=loca_para$StDev_abc[2],SD3=loca_para$StDev_abc[3])
DIST= new_aldForecast@CI_recruitment_2_2_distribution  #"Normal"
} else {
params=data.frame(param1=NA, param2=NA,param3=NA)
SDs=data.frame(SD1=NA, SD2=NA,SD3=NA)
DIST= NA #"Normal"

}
       
APPROACH <- ifelse( new_aldForecast@CI_recruitment_2_file_or_distribution == 1, "E", "D")  # D: distribution; E:external
#********************************
# nruns= INP$nruns 
INP$SRR_CI_a_b_grid  = get_grid(APPROACH,DIST,  new_aldForecast@CI_recruitment_2_SRR_model ,params,SDs,INP$nruns )

      write.table(INP$SRR_CI_a_b_grid, file = paste(tablesDIR_input,"/SRR_uncertainty_grid_", INP$nruns ,"runs.csv",sep=""), sep=";", row.names=F) 

# ELIMINATA UNA PARTE DI CODICE  #**********************************************************    VEDI IN RUN CI.R USATO IN RETE TRE
    } else {
    
 # INP$Recruits_original <- INP$Recruits  

if (gtkToggleButtonGetActive(radio_CI_err_additive_fore)) {
        INP$error_type <- 1  # input from GUI #additive is 1, multiplicative 2
        err_in_title <- "additive"
} else if (gtkToggleButtonGetActive(radio_CI_err_multiplicative_fore)) {
         INP$error_type <- 2  # input from GUI #additive is 1, multiplicative 2
          err_in_title <- "multiplicative"
}   
   
   
vector_err = data.frame(matrix(0, nrow=INP$nruns, ncol=length(years_forecast)))	
colnames(vector_err) <- years_forecast

#vector_recruits = data.frame(matrix(0, nrow=INP$nruns, ncol=GLO$L_number+1)) 
INP$vector_recruits = data.frame(matrix(0, nrow=INP$nruns, ncol=length(INP$Recruits_original))) 

for (nru in 1:INP$nruns)  {
INP$vector_recruits[nru,] <- INP$Recruits_original
}

   
# error_source <- new_aldForecast@CI_error_source # 2 #input from GUI # 1/2         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# errore può venire dall'esterno (opzione 1) oppure può essere calcolato da aladym   (opzione 2 )
# calibration solo senza intervalli di confidenza

   if (new_aldForecast@CI_recruitment_1_file_or_distribution == 1) {                  # external
          err_distr_in_title <- "from vector"
   } else if (new_aldForecast@CI_recruitment_1_file_or_distribution == 2) {         # from distribution   
if (INP$R_type_for == 1) {
	err_distr_in_title <- "lognormal"	
	 vector_err[] <- rlnorm( (INP$nruns*length(years_forecast)), INP$R_pam1_for,INP$R_pam2_for)
		} else if (INP$R_type_for == 2) {
		err_distr_in_title <- "gamma"
			 vector_err[] <- rgamma( (INP$nruns*length(years_forecast)), INP$R_pam1_for,INP$R_pam2_for)
		} else if (INP$R_type_for == 3) {
		  err_distr_in_title <- "normal"
		  	 vector_err[] <- rnorm( (INP$nruns*length(years_forecast)), INP$R_pam1_for,INP$R_pam2_for)
		} else if (INP$R_type_for == 4) {
       err_distr_in_title <- "uniform"
       	vector_err[] <- runif( (INP$nruns*length(years_forecast)), INP$R_pam1_for,INP$R_pam2_for)
    }
   
   }  

 vector_err_original <- vector_err
  head(vector_err_original) 
 COUNTER_NEGATIVE_RECRUITMENT_total <- 0     
  
   adj_rec_file=paste(tablesDIR_input,"/adj_rec.dat",sep="")
       
  if (INP$error_type == 1) {
   for (i_loca_i in 1:length(years_forecast)) {
     
      if (new_aldForecast@CI_recruitment_1_file_or_distribution == 1) {
        annual_error_ <- new_aldForecast@CI_recruitment_1_1_error_vector[, (i_loca_i +1)] 
         vector_err[,i_loca_i] <-  new_aldForecast@CI_recruitment_1_1_error_vector[, (i_loca_i +1)] 
   } else {
        annual_error_ <-  vector_err[,i_loca_i]
   } 
   
#   head(INP$vector_recruits[,(((i_loca_i+simperiod-1)*12+1)+1):((i_loca_i+simperiod)*12+1)])
#   head(annual_error_)
   
      COUNTER_NEGATIVE_RECRUITMENT <- 0
        reclute_con_rumore_this_year <-  annual_error_ + INP$vector_recruits[,(((i_loca_i+simperiod-1)*12+1)+1):((i_loca_i+simperiod)*12+1)]
        reclute_con_rumore_this_year_idx <- which(reclute_con_rumore_this_year[,] < 0 ,arr.ind=T)

          INP$vector_recruits[,(((i_loca_i+simperiod-1)*12+1)+1):((i_loca_i+simperiod)*12+1)]   <- reclute_con_rumore_this_year 

annual_error_to_be_replaced <- unique(reclute_con_rumore_this_year_idx[,1])
 
       while(length(annual_error_to_be_replaced) != COUNTER_NEGATIVE_RECRUITMENT) {
                  COUNTER_NEGATIVE_RECRUITMENT <- COUNTER_NEGATIVE_RECRUITMENT+1
                  
        if (INP$R_type_for == 1) {
	err_distr_in_title <- "lognormal"	
	 annual_error_this_year_single_run <- rlnorm( 1, INP$R_pam1_for,INP$R_pam2_for)
		} else if (INP$R_type_for == 2) {
		err_distr_in_title <- "gamma"
			 annual_error_this_year_single_run <- rgamma( 1, INP$R_pam1_for,INP$R_pam2_for)
		} else if (INP$R_type_for == 3) {
		  err_distr_in_title <- "normal"
		  		 annual_error_this_year_single_run <- rnorm( 1, INP$R_pam1_for,INP$R_pam2_for)
		} else if (INP$R_type_for == 4) {
       err_distr_in_title <- "uniform"
       	 annual_error_this_year_single_run <- runif( 1, INP$R_pam1_for,INP$R_pam2_for)
    }  
   
 reclute_con_rumore_this_year_single_run <- annual_error_this_year_single_run + INP$vector_recruits[reclute_con_rumore_this_year_idx[COUNTER_NEGATIVE_RECRUITMENT,1],(((i_loca_i+simperiod-1)*12+1)+1):((i_loca_i+simperiod)*12+1)]        #  vector_err[,i_loca_i]
    if (all(reclute_con_rumore_this_year_single_run > 0) ) {
           vector_err[reclute_con_rumore_this_year_idx[COUNTER_NEGATIVE_RECRUITMENT,1],i_loca_i] <-  annual_error_this_year_single_run
         INP$vector_recruits[reclute_con_rumore_this_year_idx[COUNTER_NEGATIVE_RECRUITMENT,1],(((i_loca_i+simperiod-1)*12+1)+1):((i_loca_i+simperiod)*12+1)]  <- reclute_con_rumore_this_year_single_run   
     print(paste("Replaced negative perturbed recruitment in", years_forecast[i_loca_i],"run:", annual_error_to_be_replaced[COUNTER_NEGATIVE_RECRUITMENT] ) )
     write(paste("Replaced negative perturbed recruitment in", years_forecast[i_loca_i],"run:", annual_error_to_be_replaced[COUNTER_NEGATIVE_RECRUITMENT ]) , file = adj_rec_file, append = TRUE)

      } else {
    COUNTER_NEGATIVE_RECRUITMENT <- COUNTER_NEGATIVE_RECRUITMENT - 1  
      }
    }
                                                                                                  
COUNTER_NEGATIVE_RECRUITMENT_total <- COUNTER_NEGATIVE_RECRUITMENT_total + COUNTER_NEGATIVE_RECRUITMENT
    
}   #time series + error  <- 500 vettori di reclutmanto dal 2007 al 2012 

#   for (i_loca_i in 1:length(years_forecast)) {
#      if (new_aldForecast@CI_recruitment_1_file_or_distribution == 1) {
#        annual_error_ <- new_aldForecast@CI_recruitment_1_1_error_vector[, (i_loca_i +1)] 
#                vector_err[,i_loca_i] <-  new_aldForecast@CI_recruitment_1_1_error_vector[, (i_loca_i +1)] 
#   } else {
#        annual_error_ <-   vector_err[,i_loca_i]
#
#   } 
#                                                                                                        #  vector_err[,i_loca_i]
#   INP$vector_recruits[,(((i_loca_i+simperiod-1)*12+1)+1):((i_loca_i+simperiod)*12+1)]  <- annual_error_ + INP$vector_recruits[,(((i_loca_i+simperiod-1)*12+1)+1):((i_loca_i+simperiod)*12+1)]
#}   #time series + error  <- 500 vettori di reclutmanto dal 2007 al 2012  
#     
     } else {
    
     for (i_loca_i in 1:length(years_forecast)) {
     
      if (new_aldForecast@CI_recruitment_1_file_or_distribution == 1) {
        annual_error_ <- new_aldForecast@CI_recruitment_1_1_error_vector[, (i_loca_i +1)] 
         vector_err[,i_loca_i] <-  new_aldForecast@CI_recruitment_1_1_error_vector[, (i_loca_i +1)] 
   } else {
        annual_error_ <-  vector_err[,i_loca_i]
   } 
   
#   head(INP$vector_recruits[,(((i_loca_i+simperiod-1)*12+1)+1):((i_loca_i+simperiod)*12+1)])
#   head(annual_error_)
   
      COUNTER_NEGATIVE_RECRUITMENT <- 0
        reclute_con_rumore_this_year <-  annual_error_ * INP$vector_recruits[,(((i_loca_i+simperiod-1)*12+1)+1):((i_loca_i+simperiod)*12+1)]
        reclute_con_rumore_this_year_idx <- which(reclute_con_rumore_this_year[,] < 0 ,arr.ind=T)

     INP$vector_recruits[,(((i_loca_i+simperiod-1)*12+1)+1):((i_loca_i+simperiod)*12+1)]   <- reclute_con_rumore_this_year 
                  
annual_error_to_be_replaced <- unique(reclute_con_rumore_this_year_idx[,1])
 
       while(length(annual_error_to_be_replaced) != COUNTER_NEGATIVE_RECRUITMENT) {
                  COUNTER_NEGATIVE_RECRUITMENT <- COUNTER_NEGATIVE_RECRUITMENT+1
                  
        if (INP$R_type_for == 1) {
	err_distr_in_title <- "lognormal"	
	 annual_error_this_year_single_run <- rlnorm( 1, INP$R_pam1_for,INP$R_pam2_for)
		} else if (INP$R_type_for == 2) {
		err_distr_in_title <- "gamma"
			 annual_error_this_year_single_run <- rgamma( 1, INP$R_pam1_for,INP$R_pam2_for)
		} else if (INP$R_type_for == 3) {
		  err_distr_in_title <- "normal"
		  		 annual_error_this_year_single_run <- rnorm( 1, INP$R_pam1_for,INP$R_pam2_for)
		} else if (INP$R_type_for == 4) {
       err_distr_in_title <- "uniform"
       	 annual_error_this_year_single_run <- runif( 1, INP$R_pam1_for,INP$R_pam2_for)
    }  
   
 reclute_con_rumore_this_year_single_run <- annual_error_this_year_single_run * INP$vector_recruits[reclute_con_rumore_this_year_idx[COUNTER_NEGATIVE_RECRUITMENT,1],(((i_loca_i+simperiod-1)*12+1)+1):((i_loca_i+simperiod)*12+1)]        #  vector_err[,i_loca_i]
    if (all(reclute_con_rumore_this_year_single_run > 0) ) {
           vector_err[reclute_con_rumore_this_year_idx[COUNTER_NEGATIVE_RECRUITMENT,1],i_loca_i] <-  annual_error_this_year_single_run
         INP$vector_recruits[reclute_con_rumore_this_year_idx[COUNTER_NEGATIVE_RECRUITMENT,1],(((i_loca_i+simperiod-1)*12+1)+1):((i_loca_i+simperiod)*12+1)]  <- reclute_con_rumore_this_year_single_run   
       print(paste("Replaced negative perturbed recruitment in", years_forecast[i_loca_i],"run:", annual_error_to_be_replaced[COUNTER_NEGATIVE_RECRUITMENT] ) )
     write(paste("Replaced negative perturbed recruitment in", years_forecast[i_loca_i],"run:", annual_error_to_be_replaced[COUNTER_NEGATIVE_RECRUITMENT ]) , file = adj_rec_file, append = TRUE)
} else {
    COUNTER_NEGATIVE_RECRUITMENT <- COUNTER_NEGATIVE_RECRUITMENT - 1  
      }
      
      }
                                                                                                      
COUNTER_NEGATIVE_RECRUITMENT_total <- COUNTER_NEGATIVE_RECRUITMENT_total + COUNTER_NEGATIVE_RECRUITMENT
    
}   #time series + error  <- 500 vettori di reclutmanto dal 2007 al 2012  
 
#   for (i_loca_i in 1:length(years_forecast)) {
#   
#         if (new_aldForecast@CI_recruitment_1_file_or_distribution == 1) {
#        annual_error_ <- new_aldForecast@CI_recruitment_1_1_error_vector[, (i_loca_i +1)] 
#        vector_err[,i_loca_i] <-  new_aldForecast@CI_recruitment_1_1_error_vector[, (i_loca_i +1)] 
#
#   } else {
#        annual_error_ <-   vector_err[,i_loca_i]
#   }
#                                                                                                          # vector_err[,i_loca_i]
#    INP$vector_recruits[,(((i_loca_i+simperiod-1)*12+1)+1):((i_loca_i+simperiod)*12+1)]  <- round(annual_error_ * INP$vector_recruits[,(((i_loca_i+simperiod-1)*12+1)+1):((i_loca_i+simperiod)*12+1)] , 0)
#}      # Recruit time series * error  <- 500 vettori di reclutmanto dal 2007 al 2012  
#
}


   if (COUNTER_NEGATIVE_RECRUITMENT_total > 0) {
         showError(paste("A number of negative values for levels of recruitment for Confidence Intervals calculation\nfor ", BMT_SPECIES[ALADYM_spe]," has been found equal to ", COUNTER_NEGATIVE_RECRUITMENT_total, "!\nThe negative values have been replaced and the computation launched!" ,sep=""))
   }   
  
  
   
realNumberOfRuns <- INP$nruns 
      
vector_err <- data.frame( cbind(c(1:realNumberOfRuns), vector_err) )
 colnames( vector_err) <- c("run_N", years_forecast)     
 write.table(vector_err , file = paste(tablesDIR_input,"/Recruitment_uncertainty_grid_", INP$nruns ,"runs.csv",sep=""), sep=";", row.names=F)
 
 vector_err_original <- data.frame( cbind(c(1:realNumberOfRuns), vector_err_original) )
 colnames( vector_err_original) <- c("run_N", years_forecast)     
 write.table(vector_err_original , file = paste(tablesDIR_input,"/Recruitment_uncertainty_grid_", INP$nruns ,"runs_without ADJ.csv",sep=""), sep=";", row.names=F)

    }
	}

  }
  
  

# print(INP$Recruits_original)


loca_Fertility <- 1
loca_min_BAS.MM  <- mean(BAS$MM)
loca_min_BAS.FM <- mean(BAS$FM) 


if (current_year == 1)  {
   source(paste(ALADYM_home, "/src/tablesCI_initialization.r" ,sep="") )
 
INP$Recruitment_ALLruns <- Recruitment_ALLruns
INP$mortalities_ALLruns <- mortalities_ALLruns
INP$mortalities_change_ALLruns <- mortalities_change_ALLruns
INP$Population_ALLruns <- Population_ALLruns
INP$Production_ALLruns <- Production_ALLruns
INP$Indicators_ALLruns <- Indicators_ALLruns
INP$Catches_by_age_ALLruns <-  Catches_by_age_ALLruns
INP$Landings_by_age_ALLruns <- Landings_by_age_ALLruns
INP$Discards_by_age_ALLruns <- Discards_by_age_ALLruns
INP$Annual_F_by_gear_ALLruns <- Annual_F_by_gear_ALLruns
    
}


# -----------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------


current_runCI <<- 0

 numero_cicli_ci <- INP$nruns

 
 if (new_aldForecast@CI_selectivity & current_year == 1) {

if (new_aldForecast@CI_selectivity_file_or_distribution == 2) {
   GRID_SELECTIVITY <- get_grid_selectivity(new_aldForecast@CI_selectivity_2_distribution_vector, as.numeric(as.character(new_aldForecast@CI_n_runs)) )
} else {
   GRID_SELECTIVITY <- new_aldForecast@CI_selectivity_1_external_vector
}
         write.table(GRID_SELECTIVITY, file = paste(tablesDIR_input,"/Selectivity_uncertainty_grid_", INP$nruns ,"runs.csv",sep=""), sep=";", row.names=F) 
}
 
#if (current_year == 1) {
# ex_runs <- 0
#while(ex_runs < INP$nruns) {
SRO_temp <- new.env()
#SRO_temp <- get("SRO")
for (obj_name in ls(SRO)) {
     assign(obj_name, get(obj_name, envir = SRO), envir=SRO_temp)
}

#RND_temp <- get("RND")
RND_temp <- new.env()
for (obj_name in ls(RND)) {
     assign(obj_name, get(obj_name, envir = RND), envir=RND_temp)
}

#BAS_temp <-get("BAS")
BAS_temp <- new.env()
for (obj_name in ls(BAS)) {
     assign(obj_name, get(obj_name, envir = BAS), envir=BAS_temp)
}

#GLO_temp <- get("GLO")
GLO_temp <- new.env()
for (obj_name in ls(GLO)) {
     assign(obj_name, get(obj_name, envir = GLO), envir=GLO_temp)
}


#}

fishing_coeff_temp <<- INP$Fishing_efforts
Fmort_temp <<- INP$Fm
Fmort_temp_males <<- INP$Fmales
Fmort_temp_females <<- INP$Ffemales
p_Prod_temp <<- INP$p_Production

#Recruitment_ALLruns_temp2 <<- INP$Recruitment_ALLruns 
#mortalities_ALLruns_temp2 <<- INP$mortalities_ALLruns
#mortalities_change_ALLruns_temp2 <<- INP$mortalities_change_ALLruns
#Population_ALLruns_temp2 <<- INP$Population_ALLruns
#Production_ALLruns_temp2 <<- INP$Production_ALLruns
#Discard_ALLruns_temp2 <<- INP$Discard_ALLruns
#Indicators_ALLruns_temp2 <<- Catches_by_age_ALLruns", "Landings_by_age_ALLruns", "Discards_by_age_ALLruns", "Annual_F_by_gear_ALLruns") 

BAS$MUPopulation_original <-  BAS$MUPopulation 
 BAS$FUPopulation_original <-  BAS$FUPopulation 
 
 BAS$MFPopulation_original  <-  BAS$MFPopulation
  BAS$FFPopulation_original  <-  BAS$FFPopulation
  
  BAS$FFSS_Number_original <-   BAS$FFSS_Number
  BAS$MFSS_Number_original <-  BAS$MFSS_Number
  
  BAS$MFR_original <- BAS$MFR
  BAS$FFR_original <- BAS$FFR


if (current_year != 1) { 
path_to_save <- paste(casestudy_path, "/",harvest_rule_id, "/working files/CI_", ALADYM_spe,".Rdata", sep="")
 load(path_to_save)
}
 
 
for (numero_ciclo in 1:numero_cicli_ci) {
  BAS$MUPopulation <-  BAS$MUPopulation_original 
 BAS$FUPopulation <-  BAS$FUPopulation_original 

 BAS$MFPopulation <- BAS$MFPopulation_original 
 BAS$FFPopulation <-  BAS$FFPopulation_original 
 
   BAS$FFSS_Number <-   BAS$FFSS_Number_original
  BAS$MFSS_Number <-  BAS$MFSS_Number_original
  
    BAS$MFR <- BAS$MFR_original
  BAS$FFR <- BAS$FFR_original
  
  loca_Fertility <- 1
loca_min_BAS.MM  <- mean(as.numeric(as.character(BAS$MM)))
loca_min_BAS.FM <- mean(as.numeric(as.character(BAS$FM))) 

  
current_runCI <<- numero_ciclo

if (new_aldForecast@CI_maturity) {
BAS$MMaturity = SGEAR(1,INP$Mat_grid_M[numero_ciclo,1], INP$Mat_grid_M[numero_ciclo,2], NA, NA, NA, BAS$MLength)
BAS$FMaturity = SGEAR(1,INP$Mat_grid_F[numero_ciclo,1], INP$Mat_grid_F[numero_ciclo,2], NA, NA, NA, BAS$FLength)
}


if (new_aldForecast@CI_selectivity) {

   for (gear in 1:length(FLEETSEGMENTS_names)) {
  
  if (as.numeric(INP$OPT_F_TYPE)==1) {
         this_gear_CI <- GRID_SELECTIVITY[GRID_SELECTIVITY$fleet == FLEETSEGMENTS_names[gear], ]
   
   INP$param1[(1+forecast):nrow(INP$param1),gear]  <- this_gear_CI$param1[numero_ciclo]
   INP$param2[(1+forecast):nrow(INP$param2),gear]  <- this_gear_CI$param2[numero_ciclo]
   INP$param3[(1+forecast):nrow(INP$param3),gear]  <- this_gear_CI$param3[numero_ciclo]
   INP$param4[(1+forecast):nrow(INP$param4),gear]  <- this_gear_CI$param4[numero_ciclo]
   INP$param5[(1+forecast):nrow(INP$param5),gear]  <- this_gear_CI$param5[numero_ciclo]
   INP$OPT_SG_TYPE[(1+forecast):nrow(INP$OPT_SG_TYPE),gear] <- this_gear_CI$sel_type[numero_ciclo]
   
  }  


}

}


if (new_aldForecast@CI_recruitment) {
INP$Recruits[] = c(as.numeric(as.character(INP$vector_recruits[numero_ciclo,])) , rep(NA, length(c((GLO$L_number+2):length(INP$Recruits)))))
INP$Recruits_all_for_CI = INP$Recruits
INP$Recruits_original <- INP$Recruits_all_for_CI  
# print(paste("************* ",INP$Recruits_all_for_CI ))

       if (new_aldForecast@CI_recruitment_constant_or_SRR == 2) {
       if ( new_aldForecast@CI_recruitment_2_SRR_model  ==   "Beverton and Holt") {
         INP$FRLt_fore <-  1
       } else if ( new_aldForecast@CI_recruitment_2_SRR_model  ==   "Ricker") {
          INP$FRLt_fore <-  2
       } else if ( new_aldForecast@CI_recruitment_2_SRR_model  ==   "Shepherd") {
          INP$FRLt_fore <-  3
       } else if ( new_aldForecast@CI_recruitment_2_SRR_model  ==   "Hockey-Stick") {
          INP$FRLt_fore <-  5
       }  else if ( new_aldForecast@CI_recruitment_2_SRR_model  ==   "Hockey-Stick quadratic") {
          INP$FRLt_fore <-  6
       }
INP$FRLa_fore <- as.numeric(INP$SRR_CI_a_b_grid[numero_ciclo,1])
INP$FRLb_fore  <- as.numeric(INP$SRR_CI_a_b_grid[numero_ciclo,2])  
INP$FRLc_fore  <- as.numeric(INP$SRR_CI_a_b_grid[numero_ciclo,3]) 

  } else {
INP$Recruits[] = as.numeric(as.character(INP$vector_recruits[numero_ciclo,]))

#print("Entering recruits with error:", quote=F)
#print(INP$Recruits)
}

}


rm(SRO)  
rm(RND)
rm(BAS)
#rm(GLO)

#if (ALADYM_spe == 1) {
#print(INP$param6)
#}

if (current_year == 1) {
SRO <- new.env()
# SRO <- get("SRO_temp")
for (obj_name in ls(SRO_temp)) {
     assign(obj_name, get(obj_name, envir = SRO_temp), envir=SRO)
}

RND <- new.env()
#RND <-  get("RND_temp")
for (obj_name in ls(RND_temp)) {
     assign(obj_name, get(obj_name, envir = RND_temp), envir=RND)
}

BAS <- new.env()
#BAS <-  get("BAS_temp")
for (obj_name in ls(BAS_temp)) {
     assign(obj_name, get(obj_name, envir = BAS_temp), envir=BAS)
}


if (new_aldForecast@CI_growth) {
 
BAS$MLength = vBF(INP$VB_gridM[numero_ciclo,1],INP$VB_gridM[numero_ciclo,2],INP$MGrowth_t0_max, BAS$MAge) 
BAS$FLength = vBF(INP$VB_gridF[numero_ciclo,1],INP$VB_gridF[numero_ciclo,2],INP$FGrowth_t0_max, BAS$FAge)

BAS$MWeight = INP$MWLa * BAS$MLength ^ INP$MWLb
BAS$FWeight = INP$FWLa * BAS$FLength ^ INP$FWLb

 
     if (new_aldForecast@CI_M) {       
BAS$MM = mort_CI_M[,numero_ciclo]
BAS$FM = mort_CI_F[,numero_ciclo]



loca_Fertility <- 1

loca_min_BAS.MM  <- mean(as.numeric(as.character(BAS$MM)))
loca_min_BAS.FM <- mean(as.numeric(as.character(BAS$FM)))

  }   
  
  
index_final <- ifelse(!INTEGRATED_APPROACH, GLO$L_number, (simperiod+foreperiod)*12)

print("Individual mean weight M:", quote=F)
print(BAS$MWeight[(forecast+1):(index_final+1)], quote=F )
print("Individual mean weight F:", quote=F)
print(BAS$FWeight[(forecast+1):(index_final+1)], quote=F )
 
}


if (new_aldForecast@CI_maturity) {
BAS$MMaturity = SGEAR(1,INP$Mat_grid_M[numero_ciclo,1], INP$Mat_grid_M[numero_ciclo,2], NA, NA, NA, BAS$MLength)
BAS$FMaturity = SGEAR(1,INP$Mat_grid_F[numero_ciclo,1], INP$Mat_grid_F[numero_ciclo,2], NA, NA, NA, BAS$FLength)

print("maturity for M:", quote=F)
print(BAS$MMaturity[(forecast+1):(index_final+1)], quote=F )
print("maturity for F:", quote=F)
print(BAS$FMaturity[(forecast+1):(index_final+1)], quote=F )
}

if (new_aldForecast@CI_selectivity) {

   for (gear in 1:length(FLEETSEGMENTS_names)) {
  
  if (as.numeric(INP$OPT_F_TYPE)==1) {
         this_gear_CI <- GRID_SELECTIVITY[GRID_SELECTIVITY$fleet == FLEETSEGMENTS_names[gear], ]
   
   INP$param1[(1+forecast):nrow(INP$param1),gear]  <- this_gear_CI$param1[numero_ciclo]
   INP$param2[(1+forecast):nrow(INP$param2),gear]  <- this_gear_CI$param2[numero_ciclo]
   INP$param3[(1+forecast):nrow(INP$param3),gear]  <- this_gear_CI$param3[numero_ciclo]
   INP$param4[(1+forecast):nrow(INP$param4),gear]  <- this_gear_CI$param4[numero_ciclo]
   INP$param5[(1+forecast):nrow(INP$param5),gear]  <- this_gear_CI$param5[numero_ciclo]
   INP$OPT_SG_TYPE[(1+forecast):nrow(INP$OPT_SG_TYPE),gear] <- this_gear_CI$sel_type[numero_ciclo]
   
  }  


}

}

#GLO <- new.env()
##GLO <-  get("GLO_temp") #new.env()
#for (obj_name in ls(GLO_temp)) {
#     assign(obj_name, get(obj_name, envir = GLO_temp), envir=GLO)
#}

} else {

SRO <- new.env()
SRO_this_run <- get(paste("SRO_", ALADYM_spe, "_run", numero_ciclo, sep=""))
for (obj_name in ls(SRO_this_run)) {
     assign(obj_name, get(obj_name, envir = SRO_this_run), envir=SRO)
}

RND <- new.env()
RND_this_run <- get(paste("RND_", ALADYM_spe, "_run", numero_ciclo, sep=""))
for (obj_name in ls(RND_temp)) {
     assign(obj_name, get(obj_name, envir = RND_this_run), envir=RND)
}

BAS <- new.env()
BAS_this_run <- get(paste("BAS_", ALADYM_spe, "_run", numero_ciclo, sep=""))
for (obj_name in ls(BAS_temp)) {
     assign(obj_name, get(obj_name, envir = BAS_this_run), envir=BAS)
}


if (new_aldForecast@CI_growth) {

BAS$MLength = vBF(INP$VB_gridM[numero_ciclo,1],INP$VB_gridM[numero_ciclo,2],INP$MGrowth_t0_max, BAS$MAge) 
BAS$FLength = vBF(INP$VB_gridF[numero_ciclo,1],INP$VB_gridF[numero_ciclo,2],INP$FGrowth_t0_max, BAS$FAge)

BAS$MWeight = INP$MWLa * BAS$MLength ^ INP$MWLb
BAS$FWeight = INP$FWLa * BAS$FLength ^ INP$FWLb

 
     if (new_aldForecast@CI_M) {       
BAS$MM = mort_CI_M[,numero_ciclo]
BAS$FM = mort_CI_F[,numero_ciclo]

loca_Fertility <- 1

loca_min_BAS.MM  <- mean(as.numeric(as.character(BAS$MM)))
loca_min_BAS.FM <- mean(as.numeric(as.character(BAS$FM)))
  }
  
  
index_final <- ifelse(!INTEGRATED_APPROACH, GLO$L_number, (simperiod+foreperiod)*12)

print("Individual mean weight M:", quote=F)
print(BAS$MWeight[(forecast+1):(index_final+1)], quote=F )
print("Individual mean weight F:", quote=F)
print(BAS$FWeight[(forecast+1):(index_final+1)], quote=F )
    
}


if (new_aldForecast@CI_maturity) {
BAS$MMaturity = SGEAR(1,INP$Mat_grid_M[numero_ciclo,1], INP$Mat_grid_M[numero_ciclo,2], NA, NA, NA, BAS$MLength)
BAS$FMaturity = SGEAR(1,INP$Mat_grid_F[numero_ciclo,1], INP$Mat_grid_F[numero_ciclo,2], NA, NA, NA, BAS$FLength)

print("maturity for M:", quote=F)
print(BAS$MMaturity[(forecast+1):(index_final+1)], quote=F )
print("maturity for F:", quote=F)
print(BAS$FMaturity[(forecast+1):(index_final+1)], quote=F )
}

if (new_aldForecast@CI_selectivity) {

   for (gear in 1:length(FLEETSEGMENTS_names)) {
  
  if (as.numeric(INP$OPT_F_TYPE)==1) {
         this_gear_CI <- GRID_SELECTIVITY[GRID_SELECTIVITY$fleet == FLEETSEGMENTS_names[gear], ]
   
   INP$param1[(1+forecast):nrow(INP$param1),gear]  <- this_gear_CI$param1[numero_ciclo]
   INP$param2[(1+forecast):nrow(INP$param2),gear]  <- this_gear_CI$param2[numero_ciclo]
   INP$param3[(1+forecast):nrow(INP$param3),gear]  <- this_gear_CI$param3[numero_ciclo]
   INP$param4[(1+forecast):nrow(INP$param4),gear]  <- this_gear_CI$param4[numero_ciclo]
   INP$param5[(1+forecast):nrow(INP$param5),gear]  <- this_gear_CI$param5[numero_ciclo]
   INP$OPT_SG_TYPE[(1+forecast):nrow(INP$OPT_SG_TYPE),gear] <- this_gear_CI$sel_type[numero_ciclo]
   
  }
  
  print("Selectivity param 1:", quote=F)
print( INP$param1[(1+forecast):nrow(INP$param1),], quote=F )  

  
  print("Selectivity param 2:", quote=F)
print( INP$param2[(1+forecast):nrow(INP$param2),], quote=F )  

}

}

#GLO <- new.env()
#GLO_this_run <- get(paste("GLO_", ALADYM_spe, "_run", i, sep=""))
#for (obj_name in ls(GLO_temp)) {
#     assign(obj_name, get(obj_name, envir = GLO_this_run), envir=GLO)
#}

}


#index_final <- ifelse(!INTEGRATED_APPROACH, GLO$L_number, (simperiod+foreperiod)*12)
#
#print("Individual mean weight M:", quote=F)
#print(BAS$MWeight[(forecast+1):(index_final+1)], quote=F )
#print("Individual mean weight F:", quote=F)
#print(BAS$FWeight[(forecast+1):(index_final+1)], quote=F )


if ( as.numeric(INP$OPT_F_TYPE) == 1 ) { 

index_final <- ifelse(!INTEGRATED_APPROACH, GLO$L_number, (simperiod+foreperiod)*12)
#
#Z_da_mediare <- as.numeric(as.character(BAS$MZ_estimated[(forecast-INP$Average_forecast*12+1):forecast]))
#x <- seq_along(Z_da_mediare)
#d1 <- split(Z_da_mediare, ceiling(x/12))
#d2 <- data.frame(d1)
#d2$avg <- rowMeans(d2)
#
#BAS$MZ_estimated[(forecast+1):(index_final+1)] = rep(d2$avg, length(years_forecast) )
#INP$MZ_estimated[(forecast+1):(index_final+1)] = rep(d2$avg, length(years_forecast) )
#
#Z_da_mediare <- as.numeric(as.character(BAS$FZ_estimated[(forecast-INP$Average_forecast*12+1):forecast]))
#x <- seq_along(Z_da_mediare)
#d1 <- split(Z_da_mediare, ceiling(x/12))
#d2 <- data.frame(d1)
#d2$avg <- rowMeans(d2)
#
#BAS$FZ_estimated[(forecast+1):(index_final+1)] = rep(d2$avg, length(years_forecast) )
#INP$FZ_estimated[(forecast+1):(index_final+1)] = rep(d2$avg, length(years_forecast) )
#
print("Total mortality M:", quote=F)
print(BAS$MZ_estimated[(forecast+1):(index_final+1)], quote=F )
print("Total mortality F:", quote=F)
print(BAS$FZ_estimated[(forecast+1):(index_final+1)], quote=F )
}

if (new_aldForecast@CI_recruitment) {
INP$Recruits[] = c(as.numeric(as.character(INP$vector_recruits[numero_ciclo,])) , rep(NA, length(c((GLO$L_number+2):length(INP$Recruits)))))
}
INP$Fishing_efforts[] = fishing_coeff_temp

INP$Fm = Fmort_temp 
INP$Fmales = Fmort_temp_males
INP$Ffemales = Fmort_temp_females

INP$p_Production[] = p_Prod_temp 

print("--------------------------------------------------------------------------------------------------------------------------", quote=F)
print("--------------------------------------------------------------------------------------------------------------------------", quote=F)
print(paste("Entering recruits run ",numero_ciclo,":", sep=""), quote=F)
print(INP$Recruits[(forecast+1):(GLO$L_number+1)])
print("--------------------------------------------------------------------------------------------------------------------------", quote=F)


#print("FFCatch before forecast:", quote=F)
#print(SRO$FFCatch[(forecast+1):(GLO$L_number+1),])
#print("--------------------------------------------------------------------------------------------------------------------------", quote=F)

load_SIMULATION_UNEXPLOITED(loca_Fertility, forecast, GLO$L_number)   
 
if (as.numeric(INP$OPT_F_TYPE)==1) { 
  load_FORECAST(forecast)
   } else {  
  load_FORECAST_entrataF(forecast)
}

source( paste(ALADYM_home, "/src/output.r" ,sep="") )

load_Annual_Z_Sinclair(GLO$L_number)
load_Annual_F_weighted(GLO$L_number)   
load_Annual_Z(GLO$L_number)
  
load_Annual_F_CI(GLO$L_number)               
#Export(GLO$L_number)   
exported_tables <- export_tables_CI(GLO$L_number)
export_tab_mort_CI(GLO$L_number, exported_tables[[1]])
indicators_CI(GLO$L_number, exported_tables[[1]], exported_tables[[2]], exported_tables[[3]])          # function (End, mortality, production, population){

#if (ALADYM_spe == 1) {
#print("--------------------------------------------------------------------------------------------------------------------------", quote=F)
#print(paste("FFCatch (age 0) after year", years.forecast[current_year], ":"), quote=F)
#print(SRO$FFCatch[(forecast+1):(GLO$L_number+1),(INP$tr:(GLO$MC_number + INP$tr - (INP$MGrowth_tend-1)* INP$Time_slice))])
#}
#i= ex_runs                                                 
print(paste("******************************************************* RUN", numero_ciclo, "COMPLETED!"), quote=F)  

source(paste(getwd(), "/src/biol/bmtALADYM/saveEnvSpecies.CI.int.r", sep=""))  

}    # END iterations 


if (new_aldForecast@CI_growth & current_year==1) {

source(paste(ALADYM_home, "/src/graph_growth.r" ,sep="") )   
if ((INP$FOPT_M_TYPE == 2) ) {  
write.table(M_uncertF,paste(tablesDIR, "/Natural_mortalityF.csv", sep=""),sep=";",row.names=F)
}

if ((INP$MOPT_M_TYPE == 2) ) {  
write.table(M_uncertM,paste(tablesDIR, "/Natural_mortalityM.csv", sep=""),sep=";",row.names=F)
}

}

path_to_save <- paste(casestudy_path, "/",harvest_rule_id, "/working files/CI_", ALADYM_spe,".Rdata", sep="")
save(list = c(paste("GLO_", ALADYM_spe, "_run", 1:INP$nruns, sep=""), paste("BAS_", ALADYM_spe, "_run", 1:INP$nruns, sep=""), paste("RND_", ALADYM_spe, "_run", 1:INP$nruns, sep=""), paste("SRO_", ALADYM_spe, "_run", 1:INP$nruns, sep="")),  file= path_to_save)

temp_digits_opt <- getOption("digits")
 options(digits = 15)

INP$current_runCI <- current_runCI

#write.table(INP$mortalities_ALLruns,"C://test_CI.csv" ,row.names=FALSE, sep=";")

INP$mortalities_ALLruns <- INP$mortalities_ALLruns[order(INP$mortalities_ALLruns$run, INP$mortalities_ALLruns$Year), ]
INP$mortalities_change_ALLruns <- INP$mortalities_change_ALLruns[order(INP$mortalities_change_ALLruns$run, INP$mortalities_change_ALLruns$Year), ]
INP$Population_ALLruns <- INP$Population_ALLruns[order(INP$Population_ALLruns$run, INP$Population_ALLruns$Year), ]
INP$Production_ALLruns <- INP$Production_ALLruns[order(INP$Production_ALLruns$run, INP$Production_ALLruns$Year), ]
#INP$Discard_ALLruns <- INP$Discard_ALLruns[order(INP$Discard_ALLruns$run, INP$Discard_ALLruns$Year), ]
INP$Indicators_ALLruns <- INP$Indicators_ALLruns[order(INP$Indicators_ALLruns$run, INP$Indicators_ALLruns$Years), ]

 
numbs <- unique(INP$Catches_by_age_ALLruns$run)
catches_to_write <- data.frame(matrix(0, nrow=0, ncol=ncol(INP$Catches_by_age_ALLruns)))
colnames(catches_to_write) <- colnames(INP$Catches_by_age_ALLruns)

numbs <- unique(INP$Landings_by_age_ALLruns$run)
landings_to_write <- data.frame(matrix(0, nrow=0, ncol=ncol(INP$Landings_by_age_ALLruns)))
colnames(landings_to_write) <- colnames(INP$Landings_by_age_ALLruns)

numbs <- unique(INP$Discards_by_age_ALLruns$run)
discards_to_write <- data.frame(matrix(0, nrow=0, ncol=ncol(INP$Discards_by_age_ALLruns)))
colnames(discards_to_write) <- colnames(INP$Discards_by_age_ALLruns)

numbs <- unique(INP$Annual_F_by_gear_ALLruns$run)
F_to_write <- data.frame(matrix(0, nrow=0, ncol=ncol(INP$Annual_F_by_gear_ALLruns)))
colnames(F_to_write) <- colnames(INP$Annual_F_by_gear_ALLruns)


#INP$Catches_by_age_ALLruns[INP$Catches_by_age_ALLruns$sex == "F" & INP$Catches_by_age_ALLruns$Gear == FLEETSEGMENTS_names[1] & INP$Catches_by_age_ALLruns$run == numbs[1], ]
for (nrun in numbs) {
 for (sexx in c("F","M")) {
    for (flee in FLEETSEGMENTS_names)  {
      catches_to_write <- data.frame(rbind(catches_to_write, INP$Catches_by_age_ALLruns[INP$Catches_by_age_ALLruns$sex == sexx & INP$Catches_by_age_ALLruns$Gear == flee & INP$Catches_by_age_ALLruns$run == nrun, ]))
      landings_to_write <- data.frame(rbind(landings_to_write, INP$Landings_by_age_ALLruns[INP$Landings_by_age_ALLruns$sex == sexx & INP$Landings_by_age_ALLruns$Gear == flee & INP$Landings_by_age_ALLruns$run == nrun, ]))
      discards_to_write <- data.frame(rbind(discards_to_write, INP$Discards_by_age_ALLruns[INP$Discards_by_age_ALLruns$sex == sexx & INP$Discards_by_age_ALLruns$Gear == flee & INP$Discards_by_age_ALLruns$run == nrun, ]))
      F_to_write <- data.frame(rbind(F_to_write, INP$Annual_F_by_gear_ALLruns[INP$Annual_F_by_gear_ALLruns$sex == sexx & INP$Annual_F_by_gear_ALLruns$Gear == flee & INP$Annual_F_by_gear_ALLruns$run == nrun, ]))
      }
 }
 }

INP$Catches_by_age_ALLruns <- catches_to_write
INP$Landings_by_age_ALLruns <- landings_to_write
INP$Discards_by_age_ALLruns <- discards_to_write
INP$Annual_F_by_gear_ALLruns <- F_to_write

#INP$Catches_by_age_ALLruns <- INP$Catches_by_age_ALLruns[with(INP$Catches_by_age_ALLruns, order(run, sex, Gear, Year_Age)), ]
#INP$Landings_by_age_ALLruns <- INP$Landings_by_age_ALLruns[with(INP$Landings_by_age_ALLruns, order(run, sex, Gear, Year_Age)), ]
#INP$Discards_by_age_ALLruns <- INP$Discards_by_age_ALLruns[with(INP$Discards_by_age_ALLruns, order(run, sex, Gear, Year_Age)), ]
#INP$Annual_F_by_gear_ALLruns <- INP$Annual_F_by_gear_ALLruns[with(INP$Annual_F_by_gear_ALLruns, order(run, sex, Gear, Year_Age)), ]
#
INP$Recruitment_ALLruns <- INP$Recruitment_ALLruns[order(INP$Recruitment_ALLruns$run, INP$Recruitment_ALLruns$Year), ]
 
 if (current_year == foreperiod) { 
write.table(INP$mortalities_ALLruns, paste(MORTALITY_table_CI, " ", current_runCI, " runs.csv", sep="") ,row.names=FALSE, sep=";")
write.table(INP$mortalities_change_ALLruns, paste(MORTALITYCHANGE_table_CI, " ", current_runCI, " runs.csv", sep="") ,row.names=FALSE, sep=";")
write.table(INP$Population_ALLruns, paste(POPULATION_table_CI, " ", current_runCI, " runs.csv", sep="") ,row.names=FALSE, sep=";")
write.table(INP$Production_ALLruns, paste(PRODUCTION_table_CI, " ", current_runCI, " runs.csv", sep=""),row.names=FALSE, sep=";")
 write.table(INP$Production_ALLruns, paste(PRODUCTION_table_CI, " ", current_runCI, " - ",current_year," runs.csv", sep=""),row.names=FALSE, sep=";")
#write.table(INP$Discard_ALLruns, paste(DETAILED_DISCARD_table_CI, " ", current_runCI, " runs.csv", sep=""),row.names=FALSE, sep=";")
write.table(INP$Indicators_ALLruns, paste(INDICATORS_table_CI, " ", current_runCI, " runs.csv", sep=""),row.names=FALSE, sep=";")
#write.table(INP$Catches_by_age_ALLruns, paste(CATCHBYAGE_table_CI, " ", current_runCI, " runs.csv", sep=""),row.names=FALSE, sep=";")
#write.table(INP$Catches_by_age_ALLruns, paste(CATCHBYAGE_table_CI, " ", current_runCI, " - ",current_year," runs.csv", sep=""),row.names=FALSE, sep=";")
#write.table(INP$Landings_by_age_ALLruns, paste(LANDINGBYAGE_table_CI, " ", current_runCI, " runs.csv", sep=""),row.names=FALSE, sep=";")
#write.table(INP$Discards_by_age_ALLruns, paste(DISCARDBYAGE_table_CI, " ", current_runCI, " runs.csv", sep=""),row.names=FALSE, sep=";")   
#write.table(INP$Annual_F_by_gear_ALLruns, paste(F_BYGEAR_table_CI, " ", current_runCI, " runs.csv", sep=""),row.names=FALSE, sep=";")
write.table(INP$Recruitment_ALLruns, paste(RECRUITMENT_table_CI, " ", current_runCI, " runs.csv", sep=""),row.names=FALSE, sep=";") 
}

allYears <- c(years, years_forecast[1:current_year])

# °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
# °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
# SALVATAGGIO MEDIANEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
# °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
# °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°

 
  if (current_year == 1) {
INP$mortalities_percs <- data.frame(matrix(0, nrow=0, ncol=length(mortalities_ALLruns_head)))
colnames(INP$mortalities_percs) <- c(mortalities_ALLruns_head[-length(mortalities_ALLruns_head)], "percentile")																		
#INP$mortalities_percs$percentile <- c(rep(0.05, length(allYears)), rep(0.25, length(allYears)), rep(0.5, length(allYears)), rep(0.75, length(allYears)) , rep(0.95, length(allYears)) ) 
#INP$mortalities_percs$Year <- rep(allYears, 5)  	 # 0.05,0.25,0.5,0.75,0.95
#
INP$recruitment_percs <- data.frame(matrix(0, nrow=0, ncol=length(Recruitment_ALLruns_head)))
colnames(INP$recruitment_percs) <-  c(Recruitment_ALLruns_head[-length(Recruitment_ALLruns_head)], "percentile")		
#INP$recruitment_percs$percentile <- c(rep(0.05, length(allYears)), rep(0.25, length(allYears)), rep(0.5, length(allYears)), rep(0.75, length(allYears)) , rep(0.95, length(allYears)) ) 
#INP$recruitment_percs$Year <- rep(allYears, 5)  	 # 0.05,0.25,0.5,0.75,0.95
#
INP$mortalities_change_percs <- data.frame(matrix(0, nrow=0, ncol=length(MortalityChange_ALLruns_head)))
colnames(INP$mortalities_change_percs) <-  c(MortalityChange_ALLruns_head[-length(MortalityChange_ALLruns_head)], "percentile")		
#INP$mortalities_change_percs$percentile <- c(rep(0.05, length(allYears)), rep(0.25, length(allYears)), rep(0.5, length(allYears)), rep(0.75, length(allYears)) , rep(0.95, length(allYears)) ) 
#INP$mortalities_change_percs$Year <- rep(allYears, 5)  	 # 0.05,0.25,0.5,0.75,0.95
#
INP$population_percs <- data.frame(matrix(0, nrow=0, ncol=length(Population_ALLruns_head)))
colnames(INP$population_percs) <-  c(Population_ALLruns_head[-length(Population_ALLruns_head)], "percentile")		
#INP$population_percs$percentile <- c(rep(0.05, length(allYears)), rep(0.25, length(allYears)), rep(0.5, length(allYears)), rep(0.75, length(allYears)) , rep(0.95, length(allYears)) ) 
#INP$population_percs$Year <- rep(allYears, 5)  	 # 0.05,0.25,0.5,0.75,0.95
#
INP$production_percs <- data.frame(matrix(0, nrow=0, ncol=length(Production_ALLruns_head)))
colnames(INP$production_percs) <-  c(Production_ALLruns_head[-length(Production_ALLruns_head)], "percentile")		
#INP$production_percs$percentile <- c(rep(0.05, length(allYears)), rep(0.25, length(allYears)), rep(0.5, length(allYears)), rep(0.75, length(allYears)) , rep(0.95, length(allYears)) ) 
#INP$production_percs$Year <- rep(allYears, 5)  	 # 0.05,0.25,0.5,0.75,0.95
#
#INP$discard_percs <- data.frame(matrix(0, nrow=0, ncol=length(Discard_ALLruns_head)))
#colnames(INP$discard_percs) <-  c(Discard_ALLruns_head[-length(Discard_ALLruns_head)], "percentile")		
##INP$discard_percs$percentile <- c(rep(0.05, length(allYears)), rep(0.25, length(allYears)), rep(0.5, length(allYears)), rep(0.75, length(allYears)) , rep(0.95, length(allYears)) ) 
#INP$discard_percs$Year <- rep(allYears, 5)  	 # 0.05,0.25,0.5,0.75,0.95
#                                                          # length(allYears)*5
#INP$discard_percs <- data.frame(matrix(0, nrow=0, ncol=length(Discard_ALLruns_head)))
#colnames(INP$discard_percs) <-  c(Discard_ALLruns_head[-length(Discard_ALLruns_head)], "percentile")		
#INP$discard_percs$percentile <- c(rep(0.05, length(allYears)), rep(0.25, length(allYears)), rep(0.5, length(allYears)), rep(0.75, length(allYears)) , rep(0.95, length(allYears)) ) 
#INP$discard_percs$Year <- rep(allYears, 5)  	 # 0.05,0.25,0.5,0.75,0.95
#
INP$indicators_percs <- data.frame(matrix(0, nrow=length(allYears)*5, ncol=length(Indicators_ALLruns_head)))
colnames(INP$indicators_percs) <-  c(Indicators_ALLruns_head[-length(Indicators_ALLruns_head)], "percentile")		
#INP$indicators_percs$percentile <- c(rep(0.05, length(allYears)), rep(0.25, length(allYears)), rep(0.5, length(allYears)), rep(0.75, length(allYears)) , rep(0.95, length(allYears)) ) 
#INP$indicators_percs$Years <- rep(allYears, 5)  	 # 0.05,0.25,0.5,0.75,0.95
#
INP$catches_by_age_percs <- data.frame(matrix(0, nrow=0, ncol=length(Catches_by_age_ALLruns_head)))
colnames(INP$catches_by_age_percs) <-  c(Catches_by_age_ALLruns_head[-length(Catches_by_age_ALLruns_head)], "percentile")		
#catches_by_age_percs$percentile <- c(rep(0.05, length(allYears)), rep(0.25, length(allYears)), rep(0.5, length(allYears)), rep(0.75, length(allYears)) , rep(0.95, length(allYears)) ) 
#catches_by_age_percs$Year_Age <- rep(allYears, 5)  	 # 0.05,0.25,0.5,0.75,0.95

INP$landings_by_age_percs <- data.frame(matrix(0, nrow=0, ncol=length(Landings_by_age_ALLruns_head)))
colnames(INP$landings_by_age_percs) <-  c(Landings_by_age_ALLruns_head[-length(Landings_by_age_ALLruns_head)], "percentile")		
#landings_by_age_percs$percentile <- c(rep(0.05, length(allYears)), rep(0.25, length(allYears)), rep(0.5, length(allYears)), rep(0.75, length(allYears)) , rep(0.95, length(allYears)) ) 
#landings_by_age_percs$Year_Age <- rep(allYears, 5)  	 # 0.05,0.25,0.5,0.75,0.95

INP$discards_by_age_percs <- data.frame(matrix(0, nrow=0, ncol=length(Discards_by_age_ALLruns_head)))
colnames(INP$discards_by_age_percs) <-  c(Discards_by_age_ALLruns_head[-length(Discards_by_age_ALLruns_head)], "percentile")		
#discards_by_age_percs$percentile <- c(rep(0.05, length(allYears)), rep(0.25, length(allYears)), rep(0.5, length(allYears)), rep(0.75, length(allYears)) , rep(0.95, length(allYears)) ) 
#discards_by_age_percs$Year_Age <- rep(allYears, 5)  	 # 0.05,0.25,0.5,0.75,0.95

INP$F_by_age_percs <- data.frame(matrix(0, nrow=0, ncol=length(Annual_F_by_gear_ALLruns_head)))
colnames(INP$F_by_age_percs) <-  c(Annual_F_by_gear_ALLruns_head[-length(Annual_F_by_gear_ALLruns_head)], "percentile")		
#F_by_age_percs$percentile <- c(rep(0.05, length(allYears)), rep(0.25, length(allYears)), rep(0.5, length(allYears)), rep(0.75, length(allYears)) , rep(0.95, length(allYears)) ) 
#F_by_age_percs$Year_Age <- rep(allYears, 5)  	 # 0.05,0.25,0.5,0.75,0.95

}

percentiles_numb <-  c(0.05,0.25,0.5,0.75,0.95)

print(Indicators_CI_selection)
print(Population_CI_selection)

for (yea in 1:length(allYears) ) {

mortalities_ALLruns_thisye <- INP$mortalities_ALLruns[INP$mortalities_ALLruns$Year == allYears[yea], ]
population_ALLruns_thisye <- INP$Population_ALLruns[INP$Population_ALLruns$Year == allYears[yea], ]
production_ALLruns_thisye <- INP$Production_ALLruns[INP$Production_ALLruns$Year == allYears[yea], ]
#discard_ALLruns_thisye <- INP$Discard_ALLruns[INP$Discard_ALLruns$Year == allYears[yea], ]
indicators_ALLruns_thisye <- INP$Indicators_ALLruns[INP$Indicators_ALLruns$Year == allYears[yea], ]
Recruitment_ALLruns_thisye <- INP$Recruitment_ALLruns[INP$Recruitment_ALLruns$Year == allYears[yea], ]


for (perc in 1:length(percentiles_numb)) {
# ----------------------------------------------------------------------------------------------------------
    percs_to_add <- data.frame(matrix(0, nrow=1, ncol=length(INP$recruitment_percs)))
    colnames(percs_to_add) <- colnames(INP$recruitment_percs)
    percs_to_add$percentile <- percentiles_numb[perc]
    percs_to_add$Year <- allYears[yea]  	 # 0.05,0.25,0.5,0.75,0.95
for (nco in 2:(ncol(INP$recruitment_percs)-1)) {
    if (allYears[yea] %in% years) { 
    if (current_year == 1) { 
    percs_to_add[1,nco] <-  as.character(Recruitment_ALLruns_thisye[1,nco] )
		# INP$recruitment_percs[INP$recruitment_percs$Year == allYears[yea] & INP$recruitment_percs$percentile == percentiles_numb[perc] ,nco] <- as.character(Recruitment_ALLruns_thisye[1,nco] )
		}
    } else {
    percs_to_add[1,nco] <- as.numeric(as.character(quantile(as.numeric(as.character(Recruitment_ALLruns_thisye[,nco])) , probs =  c(0.05,0.25,0.5,0.75,0.95), na.rm=T )[perc]   ))
		#INP$recruitment_percs[INP$recruitment_percs$Year == allYears[yea] & INP$recruitment_percs$percentile == percentiles_numb[perc] ,nco] <- 
    }
}

 if ( ( (allYears[yea] %in% years) & (current_year == 1) ) | years_forecast[current_year] ==  allYears[yea] ) {
INP$recruitment_percs <- data.frame(rbind(INP$recruitment_percs, percs_to_add ))
  }
#}

# ----------------------------------------------------------------------------------------------------------
#for (perc in 1:length(percentiles_numb)) {

    percs_to_add <- data.frame(matrix(0, nrow=1, ncol=length(INP$mortalities_percs)))
    colnames(percs_to_add) <- colnames(INP$mortalities_percs)
    percs_to_add$percentile <- percentiles_numb[perc]
    percs_to_add$Year <- allYears[yea]  	 # 0.05,0.25,0.5,0.75,0.95
    
#for (nco in 2:(ncol(INP$mortalities_percs)-1)) {
for (nco_name in mortalities_CI_selection) {

    if (allYears[yea] %in% years) {
        if (current_year== 1) {     
		percs_to_add[1,colnames(percs_to_add) == nco_name] <-  as.character(mortalities_ALLruns_thisye[1,colnames(mortalities_ALLruns_thisye) == nco_name] )
    # INP$mortalities_percs[INP$mortalities_percs$Year == allYears[yea] & INP$mortalities_percs$percentile == percentiles_numb[perc] ,nco] <- as.character(mortalities_ALLruns_thisye[1,nco] )
		}
    } else {  
		percs_to_add[1,colnames(percs_to_add) == nco_name] <-  as.numeric(as.character(quantile(as.numeric(as.character(mortalities_ALLruns_thisye[,colnames(mortalities_ALLruns_thisye) == nco_name])) , probs =  c(0.05,0.25,0.5,0.75,0.95), na.rm=T )[perc]   ))  
    # INP$mortalities_percs[INP$mortalities_percs$Year == allYears[yea] & INP$mortalities_percs$percentile == percentiles_numb[perc] ,nco] <- as.numeric(as.character(quantile(as.numeric(as.character(mortalities_ALLruns_thisye[,nco])) , probs =  c(0.05,0.25,0.5,0.75,0.95), na.rm=T )[perc]   ))  
    }
}

 if ( ( (allYears[yea] %in% years) & (current_year == 1) ) | years_forecast[current_year] ==  allYears[yea] ) {
INP$mortalities_percs <- data.frame(rbind(INP$mortalities_percs, percs_to_add ))
}
#}

# ----------------------------------------------------------------------------------------------------------
#for (perc in 1:length(percentiles_numb)) {

    percs_to_add <- data.frame(matrix(0, nrow=1, ncol=length(INP$population_percs)))
    colnames(percs_to_add) <- colnames(INP$population_percs)
    percs_to_add$percentile <- percentiles_numb[perc]
    percs_to_add$Year <- allYears[yea]  	 # 0.05,0.25,0.5,0.75,0.95

# for (nco in 2:(ncol(INP$population_percs)-1)) {
for (nco_name in Population_CI_selection) {

    if (allYears[yea] %in% years) { 
    if (current_year== 1) {       
		 percs_to_add[1, colnames(percs_to_add) == nco_name] <- as.character(population_ALLruns_thisye[1,colnames(population_ALLruns_thisye) == nco_name] )  
       # INP$population_percs[INP$population_percs$Year == allYears[yea] & INP$population_percs$percentile == percentiles_numb[perc] ,nco] <- as.character(population_ALLruns_thisye[1,nco] ) 
		}
     } else {
     
     if (nco_name != "ESSBratioUSSB") {  
      percs_to_add[1, colnames(percs_to_add) == nco_name] <- as.numeric(as.character(quantile(as.numeric(as.character(population_ALLruns_thisye[,colnames(population_ALLruns_thisye) == nco_name])) , probs =  c(0.05,0.25,0.5,0.75,0.95), na.rm=T )[perc]   )) 
       } else {
    percs_to_add[1, colnames(percs_to_add) == nco_name] <-   as.numeric(as.character(quantile( ( as.numeric(as.character(population_ALLruns_thisye[,colnames(population_ALLruns_thisye) == "SSB_exploited_pop" ])) / as.numeric(as.character(population_ALLruns_thisye[,colnames(population_ALLruns_thisye) == "SSB_unexploited_pop" ])) ) , probs =  c(0.05,0.25,0.5,0.75,0.95), na.rm=T )[perc]   )) 
      }
    # INP$population_percs[INP$population_percs$Year == allYears[yea] & INP$population_percs$percentile == percentiles_numb[perc] ,nco]<- as.numeric(as.character(quantile(as.numeric(as.character(population_ALLruns_thisye[,nco])) , probs =  c(0.05,0.25,0.5,0.75,0.95), na.rm=T )[perc]   )) 
		}
	
}

 if ( ( (allYears[yea] %in% years) & (current_year == 1) ) | years_forecast[current_year] ==  allYears[yea] ) {
INP$population_percs <- data.frame(rbind(INP$population_percs, percs_to_add ))
}

#}

# ----------------------------------------------------------------------------------------------------------
#for (perc in 1:length(percentiles_numb)) {

    percs_to_add <- data.frame(matrix(0, nrow=1, ncol=length(INP$production_percs)))
    colnames(percs_to_add) <- colnames(INP$production_percs)
    percs_to_add$percentile <- percentiles_numb[perc]
    percs_to_add$Year <- allYears[yea]  	 # 0.05,0.25,0.5,0.75,0.95

#for (nco in 2:(ncol(INP$production_percs)-length(FLEETSEGMENTS_names)-1 ) ) {  
for (nco_name in Production_CI_selection) {  
    if (allYears[yea] %in% years) { 
        if (current_year== 1) {    
         percs_to_add[1,colnames(percs_to_add) == nco_name] <- ifelse(as.character(production_ALLruns_thisye[1,colnames(production_ALLruns_thisye) == nco_name] ) =="NA", 0, as.character(production_ALLruns_thisye[1,colnames(production_ALLruns_thisye) == nco_name] ))    
		    # INP$production_percs[INP$production_percs$Year == allYears[yea] & INP$production_percs$percentile == percentiles_numb[perc] ,nco] <- ifelse(as.character(production_ALLruns_thisye[1,nco] ) =="NA", 0, as.character(production_ALLruns_thisye[1,nco] ))
		    }
     } else {
     
     perc_val <- as.numeric(as.character(quantile(as.numeric(as.character(production_ALLruns_thisye[,colnames(production_ALLruns_thisye) == nco_name])) , probs =  c(0.05,0.25,0.5,0.75,0.95), na.rm=T )[perc]   ))
       percs_to_add[1,colnames(percs_to_add) == nco_name] <- ifelse(is.na(perc_val), 0, perc_val)
		# INP$production_percs[INP$production_percs$Year == allYears[yea] & INP$production_percs$percentile == percentiles_numb[perc] ,nco] <- ifelse(is.na(perc_val), 0, perc_val)
}
}

  # percs_to_add[1,nco] <- production_ALLruns_thisye[1,(ncol(percs_to_add)-1)]   # da testare!!!!
if (length(FLEETSEGMENTS_names) == 1) {
percs_to_add[1 ,(ncol(percs_to_add)-1)] <- production_ALLruns_thisye[1,(ncol(percs_to_add)-1)]
} else {
percs_to_add[1 ,(ncol(percs_to_add)-length(FLEETSEGMENTS_names)):(ncol(percs_to_add)-1)] <- production_ALLruns_thisye[1,(ncol(percs_to_add)-length(FLEETSEGMENTS_names)):(ncol(percs_to_add)-1)]
}

 if ( ( (allYears[yea] %in% years) & (current_year == 1) ) | years_forecast[current_year] ==  allYears[yea] ) {
INP$production_percs <- data.frame(rbind(INP$production_percs, percs_to_add ))
}

#}

# ----------------------------------------------------------------------------------------------------------
#for (perc in 1:length(percentiles_numb)) {

#    percs_to_add <- data.frame(matrix(0, nrow=1, ncol=length(INP$discard_percs)))
#    colnames(percs_to_add) <- colnames(INP$discard_percs)
#    percs_to_add$percentile <- percentiles_numb[perc]
#    percs_to_add$Year <- allYears[yea]  	 # 0.05,0.25,0.5,0.75,0.95
#    
#for (nco in 2:(ncol(INP$discard_percs)-1)) {
#    if (allYears[yea] %in% years) { 
#        if (current_year== 1) {       
#	  percs_to_add[1,nco] <-  as.character(discard_ALLruns_thisye[1,nco] )
#  #	INP$discard_percs[INP$discard_percs$Year == allYears[yea] & INP$discard_percs$percentile == percentiles_numb[perc] ,nco]  <- as.character(discard_ALLruns_thisye[1,nco] )
#    }
#    } else {
#	  percs_to_add[1,nco] <-   as.numeric(as.character(quantile(as.numeric(as.character(discard_ALLruns_thisye[,nco])), probs =  c(0.05,0.25,0.5,0.75,0.95), na.rm=T )[perc]   ))
#  #	INP$discard_percs[INP$discard_percs$Year == allYears[yea] & INP$discard_percs$percentile == percentiles_numb[perc] ,nco]  <- as.numeric(as.character(quantile(as.numeric(as.character(discard_ALLruns_thisye[,nco])), probs =  c(0.05,0.25,0.5,0.75,0.95), na.rm=T )[perc]   ))
#		}
#}
#
# if ( ( (allYears[yea] %in% years) & (current_year == 1) ) | years_forecast[current_year] ==  allYears[yea] ) {
#INP$discard_percs <- data.frame(rbind(INP$discard_percs, percs_to_add ))
#}

#}

# ----------------------------------------------------------------------------------------------------------
#for (perc in 1:length(percentiles_numb)) {
    percs_to_add <- data.frame(matrix(0, nrow=1, ncol=length(INP$indicators_percs)))
    colnames(percs_to_add) <- colnames(INP$indicators_percs)
    percs_to_add$percentile <- percentiles_numb[perc]
    percs_to_add$Years <- allYears[yea]  	 # 0.05,0.25,0.5,0.75,0.95

# for (nco in 2:(ncol(INP$indicators_percs)-1)) {
for (nco_name in Indicators_CI_selection) {

    if (allYears[yea] %in% years) {  
        if (current_year== 1) {       
		   percs_to_add[1,colnames(percs_to_add) == nco_name] <-   as.character(indicators_ALLruns_thisye[1,colnames(indicators_ALLruns_thisye) == nco_name] ) 
      # INP$indicators_percs[INP$indicators_percs$Year == allYears[yea] & INP$indicators_percs$percentile == percentiles_numb[perc] ,nco]  <- as.character(indicators_ALLruns_thisye[1,nco] ) 
   }
    } else {
    
    if (!(nco_name %in% c("Exploitation_rate",	"Harvest_ratio" )) ) {
         percs_to_add[1,colnames(percs_to_add) == nco_name] <-  as.numeric(as.character(quantile(as.numeric(as.character(indicators_ALLruns_thisye[,colnames(indicators_ALLruns_thisye) == nco_name])), probs =  c(0.05,0.25,0.5,0.75,0.95), na.rm=T )[perc]   ))
    } else {
    if (nco_name == "Exploitation_rate" ) {    # F/Z
       percs_to_add[1,colnames(percs_to_add) == nco_name]  <- as.numeric(as.character(quantile(  (as.numeric(as.character(mortalities_ALLruns_thisye[,colnames(mortalities_ALLruns_thisye) == "Annual_F_estimated"])) / as.numeric(as.character(mortalities_ALLruns_thisye[,colnames(mortalities_ALLruns_thisye) == "Annual_Z_estimated"])) ) , probs =  c(0.05,0.25,0.5,0.75,0.95), na.rm=T )[perc]   ))
    }  else {  # cattura / biomassa expl
       percs_to_add[1,colnames(percs_to_add) == nco_name]  <- as.numeric(as.character(quantile(  (as.numeric(as.character(production_ALLruns_thisye[,colnames(production_ALLruns_thisye) == "Total_Yield"])) / as.numeric(as.character(population_ALLruns_thisye[,colnames(population_ALLruns_thisye) == "Total_biomass_exploited_pop"])) ) , probs =  c(0.05,0.25,0.5,0.75,0.95), na.rm=T )[perc]   ))
    }
    }  
    

    # INP$indicators_percs[INP$indicators_percs$Year == allYears[yea] & INP$indicators_percs$percentile == percentiles_numb[perc] ,nco]  <- as.numeric(as.character(quantile(as.numeric(as.character(indicators_ALLruns_thisye[,nco])), probs =  c(0.05,0.25,0.5,0.75,0.95), na.rm=T )[perc]   ))
		
    
    
    }
}

 if ( ( (allYears[yea] %in% years) & (current_year == 1) ) | years_forecast[current_year] ==  allYears[yea] ) {
INP$indicators_percs <- data.frame(rbind(INP$indicators_percs, percs_to_add ))
} # end ciclo percs

}  # end ciclo year
     

}


#
#INP$catches_by_age_percs <- INP$catches_by_age_percs[with(INP$catches_by_age_percs, order(percentile, sex, Gear,  Year_Age)),]
#INP$landings_by_age_percs <- INP$landings_by_age_percs[with(INP$landings_by_age_percs, order(percentile, sex, Gear,  Year_Age)),]
#INP$discards_by_age_percs <- INP$discards_by_age_percs[with(INP$discards_by_age_percs, order(percentile, sex, Gear,  Year_Age)),]
#INP$F_by_age_percs <- INP$F_by_age_percs[with(INP$F_by_age_percs, order(percentile, sex, Gear,  Year_Age)),]

write.table(INP$mortalities_percs[INP$mortalities_percs$percentile == 0.5, 1:(ncol(INP$mortalities_percs)-1)], MORTALITY_table, sep=";", row.names=F)
write.table(INP$population_percs[INP$population_percs$percentile == 0.5, 1:(ncol(INP$population_percs)-1)], POPULATION_table, sep=";", row.names=F)
write.table(INP$production_percs[INP$production_percs$percentile == 0.5, 1:(ncol(INP$production_percs)-1)], PRODUCTION_table, sep=";", row.names=F)
#write.table(INP$discard_percs[INP$discard_percs$percentile == 0.5, 1:(ncol(INP$discard_percs)-1)], DETAILED_DISCARD_table, sep=";", row.names=F)
write.table(INP$indicators_percs[INP$indicators_percs$percentile == 0.5, 1:(ncol(INP$indicators_percs)-1)], INDICATORS_table, sep=";", row.names=F)
                                                                                                
write.table(INP$recruitment_percs[INP$recruitment_percs$percentile == 0.5, 1:(ncol(INP$recruitment_percs)-1)], file=RECRUITMENT_table, sep=";", row.names=F)



write.table(INP$mortalities_percs, file= paste(MORTALITY_table_CI, " quantiles.csv", sep=""), sep=";", row.names=F)
write.table(INP$population_percs, file= paste(POPULATION_table_CI, " quantiles.csv", sep=""), sep=";", row.names=F)
write.table(INP$production_percs, file= paste(PRODUCTION_table_CI, " quantiles.csv", sep=""), sep=";", row.names=F)
# write.table(INP$production_percs, file= paste(PRODUCTION_table_CI, "-",current_year," quantiles.csv", sep=""), sep=";", row.names=F)
# write.table(INP$discard_percs, file= paste(DETAILED_DISCARD_table_CI, " quantiles.csv", sep=""), sep=";", row.names=F)
write.table(INP$indicators_percs,  file=paste(INDICATORS_table_CI, " quantiles.csv", sep=""), sep=";", row.names=F)

write.table(INP$recruitment_percs, file= paste(RECRUITMENT_table_CI, " quantiles.csv", sep=""), sep=";", row.names=F)


new_recr <- c()
if (!is.null(INP$vector_recruits)) {
for (num in 1:ncol(INP$vector_recruits) ) {
   new_recr <- c(new_recr, as.numeric(as.character(quantile(as.numeric(as.character(INP$vector_recruits[,num])) , probs =  0.5, na.rm=T )   )))
}
}


rm(SRO)
SRO <- new.env()
# SRO <- get("SRO_temp")
for (obj_name in ls(SRO_temp)) {
     assign(obj_name, get(obj_name, envir = SRO_temp), envir=SRO)
}

rm(RND)
RND <- new.env()
#RND <-  get("RND_temp")
for (obj_name in ls(RND_temp)) {
     assign(obj_name, get(obj_name, envir = RND_temp), envir=RND)
}

rm(BAS)
BAS <- new.env()
#BAS <-  get("BAS_temp")
for (obj_name in ls(BAS_temp)) {
     assign(obj_name, get(obj_name, envir = BAS_temp), envir=BAS)
}

rm(GLO)
GLO <- new.env()
#GLO <-  get("GLO_temp") #new.env()
for (obj_name in ls(GLO_temp)) {
     assign(obj_name, get(obj_name, envir = GLO_temp), envir=GLO)
}

 if (FALSE) {

 if (!is.null(new_recr)) {
INP$Recruits[] = new_recr
}
INP$Fishing_efforts[] = fishing_coeff_temp

INP$Fm = Fmort_temp 
INP$Fmales = Fmort_temp_males
INP$Ffemales = Fmort_temp_females

INP$p_Production[] = p_Prod_temp


print("--------------------------------------------------------------------------------------------------------------------------", quote=F)
print("--------------------------------------------------------------------------------------------------------------------------", quote=F)
print("Entering recruits LAST run:", quote=F)
print(INP$Recruits[(forecast+1):(GLO$L_number+1)])




load_SIMULATION_UNEXPLOITED(loca_Fertility, forecast, GLO$L_number)    
if (as.numeric(INP$OPT_F_TYPE)==1) { 
  load_FORECAST(forecast)
   } else {
  load_FORECAST_entrataF(forecast)
}

source(paste(ALADYM_home, "/src/output.r" ,sep="") )

load_Annual_Z_Sinclair(GLO$L_number)
load_Annual_F_weighted(GLO$L_number)
load_Annual_Z(GLO$L_number)  
##Annual_F_by_gear(GLO$L_number) 
load_Annual_F(GLO$L_number) 
#              
#Export(GLO$L_number)
export_tables(GLO$L_number)
export_tab_mort(GLO$L_number)
indicators(GLO$L_number)
    }
    
INP$FRLt_fore <- INP$FRLt_fore_temp

