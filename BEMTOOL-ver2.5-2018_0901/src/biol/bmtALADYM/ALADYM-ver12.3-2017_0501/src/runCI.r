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
  

  if (new_aldForecast@CI_maturity){
  
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
  
# APPROACH="D" # or "E"
#DIST= "Normal"
#path=NA
#********************************
  
  ext_vector_mat_F <- new_aldForecast@CI_maturity_f_external_vector
   
INP$Mat_grid_F  = get_grid_maturity(APPROACH,DIST,paramsF,SDsF,INP$nruns, ext_vector_mat_F) 
# write.table(INP$Mat_grid_F,"grid_maturityF.csv",sep=";",row.names=F)

write.table(INP$Mat_grid_F , file = paste(tablesDIR_input,"/Maturity_F_uncertainty_grid_", INP$nruns ,"runs.csv",sep=""), sep=";", row.names=F)


   if (new_aldForecast@CI_maturity_m_file_or_distribution == 2) {
		APPROACH="D" # or "E"
   	DIST= new_aldForecast@CI_maturity_m_distribution
  } else {
  		APPROACH="E" # or "E"
  }
  
# APPROACH="D" # or "E"
#DIST= "Normal"
#path=NA
#********************************

  ext_vector_mat_M <- new_aldForecast@CI_maturity_m_external_vector

INP$Mat_grid_M  = get_grid_maturity(APPROACH,DIST,paramsM,SDsM,INP$nruns, ext_vector_mat_M) 
# write.table(INP$Mat_grid_M,"grid_maturityM.csv",sep=";",row.names=F)

write.table(INP$Mat_grid_M , file = paste(tablesDIR_input,"/Maturity_M_uncertainty_grid_", INP$nruns ,"runs.csv",sep=""), sep=";", row.names=F)


}

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

write.table(INP$SRR_CI_a_b_grid , file = paste(tablesDIR_input,"/SRR_uncertainty_grid_", INP$nruns ,"runs.csv",sep=""), sep=";", row.names=F)

# ELIMINATA UNA PARTE DI CODICE  #**********************************************************    VEDI IN RUN CI.R USATO IN RETE TRE
    } else {
    
 INP$Recruits_original <- INP$Recruits  

if (gtkToggleButtonGetActive(radio_CI_err_additive_fore)) {
        INP$error_type <- 1  # input from GUI #additive is 1, multiplicative 2
        err_in_title <- "additive"
} else if (gtkToggleButtonGetActive(radio_CI_err_multiplicative_fore)) {
         INP$error_type <- 2  # input from GUI #additive is 1, multiplicative 2
          err_in_title <- "multiplicative"
}   
   
   
vector_err = data.frame(matrix(0, nrow=INP$nruns, ncol=length(years_forecast)))	
colnames(vector_err) <- years_forecast

vector_recruits = data.frame(matrix(0, nrow=INP$nruns, ncol=GLO$L_number+1)) 

for (nru in 1:INP$nruns)  {
vector_recruits[nru,] <- INP$Recruits_original
}

   
# error_source <- new_aldForecast@CI_error_source # 2 #input from GUI # 1/2         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# errore puo venire dall'esterno (opzione 1) oppure puo essere calcolato da aladym   (opzione 2 )
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
   
   head(vector_recruits[,(((i_loca_i+simperiod-1)*12+1)+1):((i_loca_i+simperiod)*12+1)])
   head(annual_error_)
   
      COUNTER_NEGATIVE_RECRUITMENT <- 0
        reclute_con_rumore_this_year <-  annual_error_ + vector_recruits[,(((i_loca_i+simperiod-1)*12+1)+1):((i_loca_i+simperiod)*12+1)]
        reclute_con_rumore_this_year_idx <- which(reclute_con_rumore_this_year[,] < 0 ,arr.ind=T)

    vector_recruits[,(((i_loca_i+simperiod-1)*12+1)+1):((i_loca_i+simperiod)*12+1)]  <- reclute_con_rumore_this_year 
          
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
   
 reclute_con_rumore_this_year_single_run <- annual_error_this_year_single_run + vector_recruits[reclute_con_rumore_this_year_idx[COUNTER_NEGATIVE_RECRUITMENT,1],(((i_loca_i+simperiod-1)*12+1)+1):((i_loca_i+simperiod)*12+1)]        #  vector_err[,i_loca_i]
    if (all(reclute_con_rumore_this_year_single_run > 0) ) {
           vector_err[reclute_con_rumore_this_year_idx[COUNTER_NEGATIVE_RECRUITMENT,1],i_loca_i] <-  annual_error_this_year_single_run
         vector_recruits[reclute_con_rumore_this_year_idx[COUNTER_NEGATIVE_RECRUITMENT,1],(((i_loca_i+simperiod-1)*12+1)+1):((i_loca_i+simperiod)*12+1)]  <- reclute_con_rumore_this_year_single_run   
     print(paste("Replaced negative perturbed recruitment in run:", reclute_con_rumore_this_year_idx[COUNTER_NEGATIVE_RECRUITMENT,1] ) )
     write(paste("Replaced negative perturbed recruitment in run:", reclute_con_rumore_this_year_idx[COUNTER_NEGATIVE_RECRUITMENT,1]) , file = adj_rec_file, append = TRUE)
      } else {
    COUNTER_NEGATIVE_RECRUITMENT <- COUNTER_NEGATIVE_RECRUITMENT - 1  
      }
      }
                                                                                                  
COUNTER_NEGATIVE_RECRUITMENT_total <- COUNTER_NEGATIVE_RECRUITMENT_total + COUNTER_NEGATIVE_RECRUITMENT
    
}   #time series + error  <- 500 vettori di reclutmanto dal 2007 al 2012  

     } else {
      
 for (i_loca_i in 1:length(years_forecast)) {
     
      if (new_aldForecast@CI_recruitment_1_file_or_distribution == 1) {
        annual_error_ <- new_aldForecast@CI_recruitment_1_1_error_vector[, (i_loca_i +1)] 
         vector_err[,i_loca_i] <-  new_aldForecast@CI_recruitment_1_1_error_vector[, (i_loca_i +1)] 
   } else {
        annual_error_ <-  vector_err[,i_loca_i]
   } 
   
   head(vector_recruits[,(((i_loca_i+simperiod-1)*12+1)+1):((i_loca_i+simperiod)*12+1)])
   head(annual_error_)
   
      COUNTER_NEGATIVE_RECRUITMENT <- 0
        reclute_con_rumore_this_year <-  annual_error_ * vector_recruits[,(((i_loca_i+simperiod-1)*12+1)+1):((i_loca_i+simperiod)*12+1)]
        reclute_con_rumore_this_year_idx <- which(reclute_con_rumore_this_year[,] < 0 ,arr.ind=T)

      vector_recruits[,(((i_loca_i+simperiod-1)*12+1)+1):((i_loca_i+simperiod)*12+1)]  <- reclute_con_rumore_this_year 

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
   
 reclute_con_rumore_this_year_single_run <- annual_error_this_year_single_run * vector_recruits[reclute_con_rumore_this_year_idx[COUNTER_NEGATIVE_RECRUITMENT,1],(((i_loca_i+simperiod-1)*12+1)+1):((i_loca_i+simperiod)*12+1)]       
 
  #  vector_err[,i_loca_i]
    if (all(reclute_con_rumore_this_year_single_run > 0) ) {
           vector_err[reclute_con_rumore_this_year_idx[COUNTER_NEGATIVE_RECRUITMENT,1],i_loca_i] <-  annual_error_this_year_single_run
         vector_recruits[reclute_con_rumore_this_year_idx[COUNTER_NEGATIVE_RECRUITMENT,1],(((i_loca_i+simperiod-1)*12+1)+1):((i_loca_i+simperiod)*12+1)]  <- reclute_con_rumore_this_year_single_run   
     print(paste("Replaced negative perturbed recruitment in run:", reclute_con_rumore_this_year_idx[COUNTER_NEGATIVE_RECRUITMENT,1] ) )
     write(paste("Replaced negative perturbed recruitment in run:", reclute_con_rumore_this_year_idx[COUNTER_NEGATIVE_RECRUITMENT,1]) , file = adj_rec_file, append = TRUE)
      } else {
    COUNTER_NEGATIVE_RECRUITMENT <- COUNTER_NEGATIVE_RECRUITMENT - 1  
      }
      }
                                                                                                  
COUNTER_NEGATIVE_RECRUITMENT_total <- COUNTER_NEGATIVE_RECRUITMENT_total + COUNTER_NEGATIVE_RECRUITMENT
    
}   #time series + error  <- 500 vettori di reclutmanto dal 2007 al 2012  

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
 
# write.table(vector_err, file = ERROR_CI_table, sep=";", row.names=F) 
    }

	} else {
  INP$Recruits =  INP$Recruits_original 
  }

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

# ************************************************************************* FEMALES
lengths_or <<- BAS$FLength
lengths  = lengths_or
ages = BAS$FAge 


# Free="Linf" # or "K"

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

# APPROACH="D" # or "E"

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
  if (new_aldForecast@CI_growth_1_file_or_distribution == 2) {
	Free="Linf" # or "K"
	params=data.frame(Linf=as.numeric(as.character(new_aldForecast@CI_growth_1_2_params$Mean_mf[2])) , K=BAS$F_mean_k, t0=BAS$F_mean_t0)
	SDs=data.frame(SD_Linf= as.numeric(as.character(new_aldForecast@CI_growth_1_2_params$StDev_mf[2])) , SD_K=NA)
	} else {
   params = params_det_F
  }
} else {
  if (new_aldForecast@CI_growth_2_file_or_distribution == 2) {
	Free="K" # or "K"
	params=data.frame(Linf=BAS$F_mean_Linf , K=as.numeric(as.character(new_aldForecast@CI_growth_2_2_params$Mean_mf[2])), t0=BAS$F_mean_t0)
	SDs=data.frame(SD_Linf= NA, SD_K=as.numeric(as.character(new_aldForecast@CI_growth_2_2_params$StDev_mf[2])))
	} else {
   params = params_det_F
  }
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
  if (new_aldForecast@CI_growth_1_file_or_distribution == 2) {
	Free="Linf" # or "K"
	params=data.frame(Linf=as.numeric(as.character(new_aldForecast@CI_growth_1_2_params$Mean_mf[1])) , K=BAS$M_mean_k, t0=BAS$M_mean_t0)
	SDs=data.frame(SD_Linf= as.numeric(as.character(new_aldForecast@CI_growth_1_2_params$StDev_mf[1])) , SD_K=NA)
	} else {
   params = params_det_M
  }
	
} else {
  if (new_aldForecast@CI_growth_2_file_or_distribution == 2) {
	Free="K" # or "K"
	params=data.frame(Linf=BAS$M_mean_Linf , K=as.numeric(as.character(new_aldForecast@CI_growth_2_2_params$Mean_mf[1])), t0=BAS$M_mean_t0)
	SDs=data.frame(SD_Linf= NA, SD_K=as.numeric(as.character(new_aldForecast@CI_growth_2_2_params$StDev_mf[1])))
	} else {
   params = params_det_M
  }
}


# ************************************************************************* MALES
#
#t0<- INP$MGrowth_t0_max # 1450
lengths_or <<- BAS$MLength
lengths =  lengths_or
ages = BAS$MAge 
#


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
 write.table(mort_CI_M, file = paste(tablesDIR_input,"/NaturalMortality_M_uncertainty_grid_", INP$nruns ,"runs.csv",sep=""), sep=";", row.names=F)
#write.table(mort_CI_M,"grid_M_males.csv",sep=";",row.names=F)
}

# ELIMINATA UNA PARTE DI CODICE  #**********************************************************    VEDI IN RUN CI.R USATO IN RETE TRE
    } 
	
	
	
	

# create matrices to save results     (matrici 2007-2012 x nruns)

# tables INITIALIZATION

   source(paste(ALADYM_home, "/src/tablesCI_initialization.r" ,sep="") )

current_runCI <<- 0

 numero_cicli_ci <- INP$nruns
 
 MATRICIONA_PER_TEST <- vector(mode="list", length=numero_cicli_ci)
 MATRICIONA_PER_TEST_unfished_M <- vector(mode="list", length=numero_cicli_ci)
 MATRICIONA_PER_TEST_unfished_F <- vector(mode="list", length=numero_cicli_ci)  
 MATRICIONA_PER_TEST_unfished_all <- vector(mode="list", length=numero_cicli_ci) 
  
 MATRICIONA_PER_TEST_rec_unfished <-  vector(mode="list", length=numero_cicli_ci) 
  
 MATRICIONA_PER_TEST_mort_nat_M <-  vector(mode="list", length=numero_cicli_ci) 
 MATRICIONA_PER_TEST_mort_nat_F <-  vector(mode="list", length=numero_cicli_ci) 

MATRICIONA_PER_TEST_Z_males <-  vector(mode="list", length=numero_cicli_ci) 
 MATRICIONA_PER_TEST_Z_females <-  vector(mode="list", length=numero_cicli_ci) 
  
if (new_aldForecast@CI_selectivity) {

if (new_aldForecast@CI_selectivity_file_or_distribution == 2) {
   GRID_SELECTIVITY <- get_grid_selectivity(new_aldForecast@CI_selectivity_2_distribution_vector, as.numeric(as.character(new_aldForecast@CI_n_runs)) )
} else {
   GRID_SELECTIVITY <- new_aldForecast@CI_selectivity_1_external_vector
}

 write.table(GRID_SELECTIVITY, file = paste(tablesDIR_input,"/Selectivity_uncertainty_grid_", INP$nruns ,"runs.csv",sep=""), sep=";", row.names=F)
}
  
   BAS$MUPopulation_original <-  BAS$MUPopulation 
 BAS$FUPopulation_original <-  BAS$FUPopulation 
 
 BAS$MFPopulation_original  <-  BAS$MFPopulation
  BAS$FFPopulation_original  <-  BAS$FFPopulation
  
  BAS$FFSS_Number_original <-   BAS$FFSS_Number
  BAS$MFSS_Number_original <-  BAS$MFSS_Number
  
  BAS$MFR_original <- BAS$MFR
  BAS$FFR_original <- BAS$FFR
  
for (numero_ciclo in 1:numero_cicli_ci) {

   BAS$MUPopulation <-  BAS$MUPopulation_original 
 BAS$FUPopulation <-  BAS$FUPopulation_original 

 BAS$MFPopulation <- BAS$MFPopulation_original 
 BAS$FFPopulation <-  BAS$FFPopulation_original 
 
   BAS$FFSS_Number <-   BAS$FFSS_Number_original
  BAS$MFSS_Number <-  BAS$MFSS_Number_original
  
    BAS$MFR <- BAS$MFR_original
  BAS$FFR <- BAS$FFR_original
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################

  # source(paste(ALADYM_home, "/src/DataIn_forecast.r", sep=""))
  
   
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################

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
}


if (new_aldForecast@CI_recruitment) {

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
INP$Recruits[] = as.numeric(as.character(vector_recruits[numero_ciclo,]))
print("Entering recruits with error:", quote=F)
print(INP$Recruits)
}

}


MATRICIONA_PER_TEST_mort_nat_M[[numero_ciclo]] <- BAS$MM
MATRICIONA_PER_TEST_mort_nat_F[[numero_ciclo]] <- BAS$FM
#  write.table(MATRICIONA_PER_TEST_rec_unfished[[i]], paste("rec_in_input_to_unexploit", i,".csv", sep=""), sep=";", row.names=F)
#print(paste("M males: ", loca_min_BAS.MM) )
#print(paste("M females: ", loca_min_BAS.FM) )

#print("Recruits:")
#print(INP$Recruits)

MATRICIONA_PER_TEST_Z_females[[numero_ciclo]] <-  BAS$FZ_estimated
MATRICIONA_PER_TEST_Z_males[[numero_ciclo]] <-  BAS$MZ_estimated

MATRICIONA_PER_TEST_rec_unfished[[numero_ciclo]] <- INP$Recruits
 
 

 
load_SIMULATION_UNEXPLOITED(loca_Fertility,  forecast, GLO$L_number)    


if (as.numeric(INP$OPT_F_TYPE)==1) { 
  load_FORECAST(forecast)
   } else {
  load_FORECAST_entrataF(forecast)
}

#    write.table(MATRICIONA_PER_TEST_rec_unfished[[i]], paste("rec_in_input_to_exploit", i,".csv", sep=""), sep=";", row.names=F)
#  write.table(MATRICIONA_PER_TEST_rec_unfished[[i]], paste("rec_in_input_to_exploit", i,".csv", sep=""), sep=";", row.names=F)
   #write.table(MATRICIONA_PER_TEST_Z_females[[numero_ciclo]], paste("Z_females", numero_ciclo,".csv", sep=""), sep=";", row.names=F) 
#    write.table(MATRICIONA_PER_TEST_Z_males[[numero_ciclo]], paste("Z_males", numero_ciclo,".csv", sep=""), sep=";", row.names=F) 
#    
source(paste(ALADYM_home, "/src/output.r" ,sep="") )

load_Annual_Z_Sinclair(GLO$L_number)
load_Annual_F_weighted(GLO$L_number)
load_Annual_Z(GLO$L_number)  
load_Annual_F_CI(GLO$L_number) 
              
  
exported_tables <- export_tables_CI(GLO$L_number)
#export_tab_mort_CI(GLO$L_number, exported_tables[[1]])
indicators_CI(GLO$L_number, exported_tables[[1]], exported_tables[[2]], exported_tables[[3]])          # function (End, mortality, production, population){

MATRICIONA_PER_TEST[[numero_ciclo]] <- SRO$MFPopulation + SRO$FFPopulation

MATRICIONA_PER_TEST_unfished_all[[numero_ciclo]] <- SRO$FUPopulation +   SRO$MUPopulation 

MATRICIONA_PER_TEST_unfished_F[[numero_ciclo]] <- SRO$FUPopulation

MATRICIONA_PER_TEST_unfished_M[[numero_ciclo]] <- SRO$MUPopulation 

#write.table(MATRICIONA_PER_TEST_unfished_M[[numero_ciclo]], paste("UM_run", numero_ciclo,".csv", sep=""), sep=";", row.names=F)
#write.table(MATRICIONA_PER_TEST_unfished_F[[numero_ciclo]], paste("UF_run", numero_ciclo,".csv", sep=""), sep=";", row.names=F)
# write.table(MATRICIONA_PER_TEST_unfished_all[[numero_ciclo]], paste("Unfished_run", numero_ciclo,".csv", sep=""), sep=";", row.names=F)
#
#print("--------------------------------------------------------------------------------------------------------------------------", quote=F)
#print("FFCatch after forecast:", quote=F)
#print(SRO$FFCatch[(forecast+1):(GLO$L_number+1),])
                                                
print(paste("******************************************************* RUN", numero_ciclo, "COMPLETED!"), quote=F)  

}    # END iterations 
 
# MATRICIONA_PER_TEST

if (new_aldForecast@CI_growth) {

source(paste(ALADYM_home, "/src/graph_growth.r" ,sep="") )   
if ((INP$FOPT_M_TYPE == 2) ) {  
write.table(M_uncertF,paste(tablesDIR, "/Natural_mortalityF.csv", sep=""),sep=";",row.names=F)
}

if ((INP$MOPT_M_TYPE == 2) ) {  
write.table(M_uncertM,paste(tablesDIR, "/Natural_mortalityM.csv", sep=""),sep=";",row.names=F)
}

}
temp_digits_opt <- getOption("digits")
 options(digits = 15)

INP$current_runCI <- current_runCI

write.table(mortalities_ALLruns, paste(MORTALITY_table_CI, " ", current_runCI, " runs.csv", sep="") ,row.names=FALSE, sep=";")
#write.table(mortalities_change_ALLruns, paste(MORTALITYCHANGE_table_CI, " ", current_runCI, " runs.csv", sep="") ,row.names=FALSE, sep=";")
write.table(Population_ALLruns, paste(POPULATION_table_CI, " ", current_runCI, " runs.csv", sep="") ,row.names=FALSE, sep=";")
write.table(Production_ALLruns, paste(PRODUCTION_table_CI, " ", current_runCI, " runs.csv", sep=""),row.names=FALSE, sep=";")
#write.table(Discard_ALLruns, paste(DETAILED_DISCARD_table_CI, " ", current_runCI, " runs.csv", sep=""),row.names=FALSE, sep=";")
write.table(Indicators_ALLruns, paste(INDICATORS_table_CI, " ", current_runCI, " runs.csv", sep=""),row.names=FALSE, sep=";")
#write.table(Catches_by_age_ALLruns, paste(CATCHBYAGE_table_CI, " ", current_runCI, " runs.csv", sep=""),row.names=FALSE, sep=";")
#write.table(Landings_by_age_ALLruns, paste(LANDINGBYAGE_table_CI, " ", current_runCI, " runs.csv", sep=""),row.names=FALSE, sep=";")
#write.table(Discards_by_age_ALLruns, paste(DISCARDBYAGE_table_CI, " ", current_runCI, " runs.csv", sep=""),row.names=FALSE, sep=";")   
#write.table(Annual_F_by_gear_ALLruns, paste(F_BYGEAR_table_CI, " ", current_runCI, " runs.csv", sep=""),row.names=FALSE, sep=";")
write.table(Recruitment_ALLruns, paste(RECRUITMENT_table_CI, " ", current_runCI, " runs.csv", sep=""),row.names=FALSE, sep=";")


allYears <- c(years, years_forecast)

# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같?
# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같?
# SAVING MEDIANS
# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같?
# 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같?

mortalities_percs <<- data.frame(matrix(0, nrow=length(allYears)*5, ncol=length(mortalities_ALLruns_head)))
colnames(mortalities_percs) <- c(mortalities_ALLruns_head[-length(mortalities_ALLruns_head)], "percentile")																		
mortalities_percs$percentile <- c(rep(0.05, length(allYears)), rep(0.25, length(allYears)), rep(0.5, length(allYears)), rep(0.75, length(allYears)) , rep(0.95, length(allYears)) ) 
mortalities_percs$Year <- rep(allYears, 5)  	 # 0.05,0.25,0.5,0.75,0.95

recruitment_percs <<- data.frame(matrix(0, nrow=length(allYears)*5, ncol=length(Recruitment_ALLruns_head)))
colnames(recruitment_percs) <-  c(Recruitment_ALLruns_head[-length(Recruitment_ALLruns_head)], "percentile")		
recruitment_percs$percentile <- c(rep(0.05, length(allYears)), rep(0.25, length(allYears)), rep(0.5, length(allYears)), rep(0.75, length(allYears)) , rep(0.95, length(allYears)) ) 
recruitment_percs$Year <- rep(allYears, 5)  	 # 0.05,0.25,0.5,0.75,0.95


population_percs <<- data.frame(matrix(0, nrow=length(allYears)*5, ncol=length(Population_ALLruns_head)))
colnames(population_percs) <-  c(Population_ALLruns_head[-length(Population_ALLruns_head)], "percentile")		
population_percs$percentile <- c(rep(0.05, length(allYears)), rep(0.25, length(allYears)), rep(0.5, length(allYears)), rep(0.75, length(allYears)) , rep(0.95, length(allYears)) ) 
population_percs$Year <- rep(allYears, 5)  	 # 0.05,0.25,0.5,0.75,0.95


production_percs <<- data.frame(matrix(0, nrow=length(allYears)*5, ncol=length(Production_ALLruns_head)))
colnames(production_percs) <-  c(Production_ALLruns_head[-length(Production_ALLruns_head)], "percentile")		
production_percs$percentile <- c(rep(0.05, length(allYears)), rep(0.25, length(allYears)), rep(0.5, length(allYears)), rep(0.75, length(allYears)) , rep(0.95, length(allYears)) ) 
production_percs$Year <- rep(allYears, 5)  	 # 0.05,0.25,0.5,0.75,0.95




indicators_percs <<- data.frame(matrix(0, nrow=length(allYears)*5, ncol=length(Indicators_ALLruns_head)))
colnames(indicators_percs) <-  c(Indicators_ALLruns_head[-length(Indicators_ALLruns_head)], "percentile")		
indicators_percs$percentile <- c(rep(0.05, length(allYears)), rep(0.25, length(allYears)), rep(0.5, length(allYears)), rep(0.75, length(allYears)) , rep(0.95, length(allYears)) ) 
indicators_percs$Years <- rep(allYears, 5)  	 # 0.05,0.25,0.5,0.75,0.95


percentiles_numb <-  c(0.05,0.25,0.5,0.75,0.95)                        # production_percs[ production_percs$Year > 2014,  c(1,  56 ,146)]
                                                                  # Production_ALLruns[Production_ALLruns$Year == allYears[yea], ]


for (yea in 1:length(allYears) ) {

mortalities_ALLruns_thisye <- mortalities_ALLruns[mortalities_ALLruns$Year == allYears[yea], ]
population_ALLruns_thisye <- Population_ALLruns[Population_ALLruns$Year == allYears[yea], ]
production_ALLruns_thisye <- Production_ALLruns[Production_ALLruns$Year == allYears[yea], ]
indicators_ALLruns_thisye <- Indicators_ALLruns[Indicators_ALLruns$Year == allYears[yea], ]
Recruitment_ALLruns_thisye <- Recruitment_ALLruns[Recruitment_ALLruns$Year == allYears[yea], ]


for (perc in 1:length(percentiles_numb)) {
# ----------------------------------------------------------------------------------------------------------
for (nco in 2:(ncol(recruitment_percs)-1)) {
    if (allYears[yea] %in% years) {   
		recruitment_percs[recruitment_percs$Year == allYears[yea] & recruitment_percs$percentile == percentiles_numb[perc] ,nco] <- as.character(Recruitment_ALLruns_thisye[1,nco] )
		} else {
		recruitment_percs[recruitment_percs$Year == allYears[yea] & recruitment_percs$percentile == percentiles_numb[perc] ,nco] <- as.numeric(as.character(quantile(as.numeric(as.character(Recruitment_ALLruns_thisye[,nco])) , probs =  c(0.05,0.25,0.5,0.75,0.95), na.rm=T )[perc]   ))
    }
}

#}

# ----------------------------------------------------------------------------------------------------------
#for (perc in 1:length(percentiles_numb)) {

for (nco_name in mortalities_CI_selection) {
    if (allYears[yea] %in% years) {   

		mortalities_percs[mortalities_percs$Year == allYears[yea] & mortalities_percs$percentile == percentiles_numb[perc] , colnames(mortalities_percs) == nco_name] <- as.character(mortalities_ALLruns_thisye[1, colnames(mortalities_ALLruns_thisye) == nco_name] )

		} else {

		mortalities_percs[mortalities_percs$Year == allYears[yea] & mortalities_percs$percentile == percentiles_numb[perc] , colnames(mortalities_percs) == nco_name] <- as.numeric(as.character(quantile(as.numeric(as.character(mortalities_ALLruns_thisye[, colnames(mortalities_ALLruns_thisye) == nco_name])) , probs =  c(0.05,0.25,0.5,0.75,0.95), na.rm=T ) [perc] ))
    }
}




#}

# ----------------------------------------------------------------------------------------------------------
#for (perc in 1:length(percentiles_numb)) {

for (nco_name in Population_CI_selection) {
    if (allYears[yea] %in% years) {   

		 population_percs[population_percs$Year == allYears[yea] & population_percs$percentile == percentiles_numb[perc] , colnames(population_percs) == nco_name] <- as.character(population_ALLruns_thisye[1,colnames(population_ALLruns_thisye) == nco_name] ) 

		 } else {
		 
   		   if (nco_name != "ESSBratioUSSB") {  
 population_percs[population_percs$Year == allYears[yea] & population_percs$percentile == percentiles_numb[perc] ,colnames(population_percs) == nco_name] <- as.numeric(as.character(quantile(as.numeric(as.character(population_ALLruns_thisye[,colnames(population_ALLruns_thisye) == nco_name])) , probs =  c(0.05,0.25,0.5,0.75,0.95), na.rm=T )[perc] )) 
       } else {
        population_percs[population_percs$Year == allYears[yea] & population_percs$percentile == percentiles_numb[perc] ,colnames(population_percs) == nco_name] <- as.numeric(as.character(quantile( (as.numeric(as.character(population_ALLruns_thisye[,colnames(population_ALLruns_thisye) == "SSB_exploited_pop"]))  /   as.numeric(as.character(population_ALLruns_thisye[,colnames(population_ALLruns_thisye) == "SSB_unexploited_pop"]))  ), probs =  c(0.05,0.25,0.5,0.75,0.95), na.rm=T )[perc] )) 
      }

		}

}

# nuovo 21052015

# population_percs[population_percs$Year == allYears[yea] & population_percs$percentile == percentiles_numb[perc] ,colnames(population_percs) == "ESSBratioUSSB"] <-   as.numeric(as.character(population_percs[population_percs$Year == allYears[yea] & population_percs$percentile == percentiles_numb[perc] ,colnames(population_percs) == "SSB_exploited_pop"] )) / as.numeric(as.character(population_percs[population_percs$Year == allYears[yea] & population_percs$percentile == percentiles_numb[perc] ,colnames(population_percs) == "SSB_unexploited_pop"] ))
 
 # ---------end # nuovo 21052015
 
#}

# ----------------------------------------------------------------------------------------------------------
#for (perc in 1:length(percentiles_numb)) {






for (nco_name in Production_CI_selection) {  

    if (allYears[yea] %in% years) {   

production_percs[production_percs$Year == allYears[yea] & production_percs$percentile == percentiles_numb[perc] ,colnames(production_percs) == nco_name] <- ifelse(as.character(production_ALLruns_thisye[1,colnames(production_ALLruns_thisye) == nco_name] ) =="NA", 0, as.character(production_ALLruns_thisye[1,colnames(production_ALLruns_thisye) == nco_name]  ))

     } else {
     perc_val <- as.numeric(as.character( quantile( as.numeric(as.character(production_ALLruns_thisye[,colnames(production_ALLruns_thisye) == nco_name])) , probs =  c(0.05,0.25,0.5,0.75,0.95), na.rm=T )[perc]  ))

		production_percs[production_percs$Year == allYears[yea] & production_percs$percentile == percentiles_numb[perc] ,colnames(production_percs) == nco_name] <- ifelse(is.na(perc_val), 0, perc_val)
}
}


# nuovooooooooooooo 21052015

if (FALSE) {
production_percs[production_percs$Year == allYears[yea] & production_percs$percentile == percentiles_numb[perc] ,colnames(production_percs) == "Total_Landing"] <- sum(	as.numeric(as.character(production_percs[production_percs$Year == allYears[yea] & production_percs$percentile == percentiles_numb[perc] ,colnames(production_percs) %in% paste("Landing_",FLEETSEGMENTS_names,sep="")] )), na.rm=T) 

production_percs[production_percs$Year == allYears[yea] & production_percs$percentile == percentiles_numb[perc] ,colnames(production_percs) == "Total_Discard"] <- sum(	as.numeric(as.character(production_percs[production_percs$Year == allYears[yea] & production_percs$percentile == percentiles_numb[perc] ,colnames(production_percs) %in% paste("Discard_",FLEETSEGMENTS_names,sep="")])), na.rm=T) 


if (length(FLEETSEGMENTS_names) > 1) {
    for (fsthis in FLEETSEGMENTS_names) {
      production_percs[production_percs$Year == allYears[yea] & production_percs$percentile == percentiles_numb[perc] ,colnames(production_percs) == paste("Yield_",fsthis,sep="")] <- sum(as.numeric(as.character(production_percs[production_percs$Year == allYears[yea] & production_percs$percentile == percentiles_numb[perc] ,colnames(production_percs) == paste("Landing_",fsthis,sep="")])), 	as.numeric(as.character(production_percs[production_percs$Year == allYears[yea] & production_percs$percentile == percentiles_numb[perc] ,colnames(production_percs) == paste("Discard_",fsthis,sep="")])), na.rm=T)
    }
}

production_percs[production_percs$Year == allYears[yea] & production_percs$percentile == percentiles_numb[perc] ,colnames(production_percs) == "Total_Yield"] <- sum(as.numeric(as.character( production_percs[production_percs$Year == allYears[yea] & production_percs$percentile == percentiles_numb[perc] ,colnames(production_percs) == "Total_Landing"])), as.numeric(as.character(	production_percs[production_percs$Year == allYears[yea] & production_percs$percentile == percentiles_numb[perc] ,colnames(production_percs) == "Total_Discard"])), na.rm=T) 


production_percs[production_percs$Year == allYears[yea] & production_percs$percentile == percentiles_numb[perc] ,colnames(production_percs) == "Biological_Production"] <- sum(	as.numeric(as.character(production_percs[production_percs$Year == allYears[yea] & production_percs$percentile == percentiles_numb[perc] ,colnames(production_percs) == "Natural_death_biomass"])), as.numeric(as.character(production_percs[production_percs$Year == allYears[yea] & production_percs$percentile == percentiles_numb[perc] ,colnames(production_percs) == "Total_Yield"])), na.rm=T)

production_percs[production_percs$Year == allYears[yea] & production_percs$percentile == percentiles_numb[perc] ,colnames(production_percs) == "Discard_ratio"] <- as.numeric(as.character(production_percs[production_percs$Year == allYears[yea] & production_percs$percentile == percentiles_numb[perc] ,colnames(production_percs) == "Total_Discard"] ))/ as.numeric(as.character(production_percs[production_percs$Year == allYears[yea] & production_percs$percentile == percentiles_numb[perc] ,colnames(production_percs) == "Total_Yield"]))
 }

# ---------- end # nuovooooooooooooo 21052015
if (length(FLEETSEGMENTS_names) == 1) {
production_percs[production_percs$Year == allYears[yea] & production_percs$percentile == percentiles_numb[perc] ,(ncol(production_percs)-1)] <- production_ALLruns_thisye[1,(ncol(production_percs)-1)]
} else {
production_percs[production_percs$Year == allYears[yea] & production_percs$percentile == percentiles_numb[perc] ,(ncol(production_percs)-length(FLEETSEGMENTS_names)):(ncol(production_percs)-1)] <- production_ALLruns_thisye[1,(ncol(production_percs)-length(FLEETSEGMENTS_names)):(ncol(production_percs)-1)]
}



for (nco_name in Indicators_CI_selection) {

    if (allYears[yea] %in% years) {  

		  indicators_percs[indicators_percs$Year == allYears[yea] & indicators_percs$percentile == percentiles_numb[perc] ,colnames(indicators_percs) == nco_name]  <- as.character(indicators_ALLruns_thisye[1,colnames(indicators_ALLruns_thisye) == nco_name] ) 
    } else {

        if (!(nco_name %in% c("Exploitation_rate",	"Harvest_ratio" )) ) {
		indicators_percs[indicators_percs$Year == allYears[yea] & indicators_percs$percentile == percentiles_numb[perc] ,colnames(indicators_percs) == nco_name]  <- as.numeric(as.character(quantile(as.numeric(as.character(indicators_ALLruns_thisye[,colnames(indicators_ALLruns_thisye) == nco_name])), probs =  c(0.05,0.25,0.5,0.75,0.95), na.rm=T )[perc] ))
    } else {
    if (nco_name == "Exploitation_rate" ) {    # F/Z
    		indicators_percs[indicators_percs$Year == allYears[yea] & indicators_percs$percentile == percentiles_numb[perc] ,colnames(indicators_percs) == nco_name]  <- as.numeric(as.character(quantile( ( as.numeric(as.character(mortalities_ALLruns_thisye[,colnames(mortalities_ALLruns_thisye) == "Annual_F_estimated"])) / as.numeric(as.character(mortalities_ALLruns_thisye[,colnames(mortalities_ALLruns_thisye) == "Annual_Z_estimated"]))), probs =  c(0.05,0.25,0.5,0.75,0.95), na.rm=T )[perc] ))
    }  else {  # cattura / biomassa expl
	indicators_percs[indicators_percs$Year == allYears[yea] & indicators_percs$percentile == percentiles_numb[perc] ,colnames(indicators_percs) == nco_name]  <- as.numeric(as.character(quantile( ( as.numeric(as.character(production_ALLruns_thisye[,colnames(production_ALLruns_thisye) == "Total_Yield"])) / as.numeric(as.character(population_ALLruns_thisye[,colnames(population_ALLruns_thisye) == "Total_biomass_exploited_pop"]))), probs =  c(0.05,0.25,0.5,0.75,0.95), na.rm=T )[perc] ))
    }
    }  






		}
}

#indicators_percs[indicators_percs$Year == allYears[yea] & indicators_percs$percentile == percentiles_numb[perc] ,colnames(indicators_percs) == "Harvest_ratio"]  <- as.numeric(as.character(production_percs[production_percs$Year == allYears[yea] & production_percs$percentile == percentiles_numb[perc] ,colnames(production_percs) == "Total_Yield"] )) / as.numeric(as.character( population_percs[population_percs$Year == allYears[yea] & population_percs$percentile == percentiles_numb[perc] ,colnames(population_percs) == "Total_biomass_exploited_pop"] ))

}  # end ciclo percs
     
}   # end ciclo year



write.table(mortalities_percs[mortalities_percs$percentile == 0.5, 1:(ncol(mortalities_percs)-1)], MORTALITY_table, sep=";", row.names=F)
write.table(population_percs[population_percs$percentile == 0.5, 1:(ncol(population_percs)-1)], POPULATION_table, sep=";", row.names=F)
write.table(production_percs[production_percs$percentile == 0.5, 1:(ncol(production_percs)-1)], PRODUCTION_table, sep=";", row.names=F)
write.table(indicators_percs[indicators_percs$percentile == 0.5, 1:(ncol(indicators_percs)-1)], INDICATORS_table, sep=";", row.names=F)
                                                                                                

write.table(recruitment_percs[recruitment_percs$percentile == 0.5, 1:(ncol(recruitment_percs)-1)], file=RECRUITMENT_table, sep=";", row.names=F)


write.table(mortalities_percs, file= paste(MORTALITY_table_CI, " quantiles.csv", sep=""), sep=";", row.names=F)
write.table(population_percs, file= paste(POPULATION_table_CI, " quantiles.csv", sep=""), sep=";", row.names=F)
write.table(production_percs, file= paste(PRODUCTION_table_CI, " quantiles.csv", sep=""), sep=";", row.names=F)
write.table(indicators_percs,  file=paste(INDICATORS_table_CI, " quantiles.csv", sep=""), sep=";", row.names=F)
                                                                                                
write.table(recruitment_percs, file= paste(RECRUITMENT_table_CI, " quantiles.csv", sep=""), sep=";", row.names=F)

source(paste(ALADYM_home, "/src/export_tab_mort_change_perc.r", sep=""))

 if (showCompTime)  {
 proc_ <- proc.time()
# SIMULATION_EXPLOITED_ptm <- proc.time()
print(paste("runCI total TIME [time]::::::::::::::::::::::::::::::::", round(as.double(proc_[3]-runCI_totalTIME_ptm[3]),2), "sec" ), quote=F )   
#print(proc.time() - SIMULATION_EXPLOITED_ptm, quote=F ) 
rm(runCI_totalTIME_ptm)
}


 options(digits = temp_digits_opt)