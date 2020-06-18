# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


getScenarioOptions <- function(loca_mat) {

      # loca_mat = scen_mat
      loca_ret <- list()
# BMT_HR_CHANGE_SELECTIVITY <- 1
# BMT_HR_CHANGE_FISHEFFORT <- 2
# BMT_HR_CHANGE_FISHMORTALITY <- 3
# BMT_HR_CHANGE_TOTAL_FISHMORTALITY <- 4
# BMT_HR_STATUS_QUO <- 5
# BMT_HR_TAC_VARIATION <- 6
# BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT <- 7
# BMT_HR_CHANGE_SELECTIVITY_BEHAVIOURAL <- 8
# BMT_HR_CHANGE_FISHEFFORT_BEHAVIOURAL <- 9
# BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT_BEHAVIOURAL <- 10
# BMT_HR_STATUS_QUO_BEHAVIOURAL <- 11
                                                        
if (MEY_CALCULATION) {
loca_ret <- list(MEY.fleet = as.character(loca_mat[nrow(loca_mat), 3]) , MEY.effort_variable =  as.character(loca_mat[nrow(loca_mat), 4]), MEY.nb_years = as.numeric(as.character(loca_mat[nrow(loca_mat), 5]) ))

      } else if (BMT_SCENARIO == BMT_HR_CHANGE_FISHEFFORT) {    # 2
            loca_ret <- list(HR2.vessels = str_replace_all(as.character(loca_mat[3,2]), "\\\\", "/" ), HR2.days=str_replace_all(as.character(loca_mat[3,3]), "\\\\", "/" ))
            
      } else if (BMT_SCENARIO == BMT_HR_CHANGE_FISHMORTALITY ) {  # 3
      matttemp <- data.frame(loca_mat[7:(6+length(BMT_FLEETSEGMENTS)),1:4])
      colnames(matttemp)[1] <- ""
      colnames(matttemp)[2] <- as.character(loca_mat[6,2])
      colnames(matttemp)[3] <- as.character(loca_mat[6,3])
        colnames(matttemp)[4] <- as.character(loca_mat[6,4])
            loca_ret <- list(HR3.timespan = as.numeric(as.character( loca_mat[5,2] )), HR3.reduction = as.numeric(as.character( loca_mat[5,3] )), HR3.fleet_reductions = matttemp  )
      
      } else if (BMT_SCENARIO == BMT_HR_CHANGE_TOTAL_FISHMORTALITY ) {  # 4
      
      matttemp <- data.frame(loca_mat[(10+length(BMT_FLEETSEGMENTS)):(9+2*length(BMT_FLEETSEGMENTS)),1:4])
      colnames(matttemp)[1] <- ""
      colnames(matttemp)[2] <- as.character(loca_mat[(9+length(BMT_FLEETSEGMENTS)),2])
      colnames(matttemp)[3] <- as.character(loca_mat[(9+length(BMT_FLEETSEGMENTS)),3])
      colnames(matttemp)[4] <- as.character(loca_mat[(9+length(BMT_FLEETSEGMENTS)),4])
      
      matttemp_2 <- data.frame(loca_mat[(11+2*length(BMT_FLEETSEGMENTS)):(10+2*length(BMT_FLEETSEGMENTS)+length(BMT_SPECIES)),1:6])
      colnames(matttemp_2)[1] <- ""
      colnames(matttemp_2)[2] <- as.character(loca_mat[(10+2*length(BMT_FLEETSEGMENTS)),2])
      colnames(matttemp_2)[3] <- as.character(loca_mat[(10+2*length(BMT_FLEETSEGMENTS)),3])
      colnames(matttemp_2)[4] <- as.character(loca_mat[(10+2*length(BMT_FLEETSEGMENTS)),4])
      colnames(matttemp_2)[5] <- as.character(loca_mat[(10+2*length(BMT_FLEETSEGMENTS)),5])
      colnames(matttemp_2)[6] <- as.character(loca_mat[(10+2*length(BMT_FLEETSEGMENTS)),6])
      
      loca_ret <- list(HR4.timespan = as.numeric(as.character(loca_mat[8+length(BMT_FLEETSEGMENTS),2])), HR4.reduction = as.numeric(as.character(loca_mat[8+length(BMT_FLEETSEGMENTS),3])) ,
                                HR4.fleet_reductions = matttemp, HR4.species_settings = matttemp_2)
      
#      } #else if (BMT_SCENARIO == BMT_HR_STATUS_QUO ) {  
#        loca_ret <- list(HR2.vessels = str_replace_all(as.character(loca_mat[3,2]), "\\\\", "/" ), HR2.days=str_replace_all(as.character(loca_mat[3,3]), "\\\\", "/" ))
#      
     } else if (BMT_SCENARIO == BMT_HR_TAC_VARIATION ) {   # 6
      
      matttemp <- data.frame(loca_mat[(15+2*length(BMT_FLEETSEGMENTS)+length(BMT_SPECIES)):((14+3*length(BMT_FLEETSEGMENTS)+length(BMT_SPECIES))),1:2])
      colnames(matttemp)[1] <- ""
      colnames(matttemp)[2] <- as.character(loca_mat[(14+2*length(BMT_FLEETSEGMENTS)+length(BMT_SPECIES)),2])  
      
      path_indices <-  str_replace_all(as.character(loca_mat[(13+2*length(BMT_FLEETSEGMENTS)+length(BMT_SPECIES)),4]), "\\\\", "/" )
           path_tac <-  str_replace_all(as.character(loca_mat[(13+2*length(BMT_FLEETSEGMENTS)+length(BMT_SPECIES)),5]), "\\\\", "/" ) 
      
      loca_ret <- list(HR6.optionTAC = as.numeric(as.character(loca_mat[(13+2*length(BMT_FLEETSEGMENTS)+length(BMT_SPECIES)),2])), HR6.species =  as.character(loca_mat[(13+2*length(BMT_FLEETSEGMENTS)+length(BMT_SPECIES)),3]),  HR6.fleet_quotas = matttemp, HR6.abundance_indices = path_indices, HR6.previous_tac= path_tac) 
      
      } else if (BMT_SCENARIO == BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT ) {  # 7
      
  #loca_ret <- list(HR2.vessels = str_replace_all(as.character(loca_mat[3,2]), "\\\\", "/" ), HR2.days=str_replace_all(as.character(loca_mat[3,3]), "\\\\", "/" ))
                  
      loca_ret <- list(HR7.vessels =  str_replace_all(as.character(loca_mat[16+3*length(BMT_FLEETSEGMENTS)+length(BMT_SPECIES),2]), "\\\\", "/" ), HR7.days= str_replace_all(as.character(loca_mat[16+3*length(BMT_FLEETSEGMENTS)+length(BMT_SPECIES),3]), "\\\\", "/" ) )
      
      } else if (BMT_SCENARIO == BMT_HR_CHANGE_FISHEFFORT_BEHAVIOURAL ) {  # 9
      loca_ret <- list(HR9.vessels =  str_replace_all(as.character(loca_mat[19+3*length(BMT_FLEETSEGMENTS)+length(BMT_SPECIES),2]), "\\\\", "/" ), HR9.days= str_replace_all(as.character(loca_mat[19+3*length(BMT_FLEETSEGMENTS)+length(BMT_SPECIES),3]), "\\\\", "/" ) )
      
      } else if (BMT_SCENARIO == BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT_BEHAVIOURAL ) {  # 10
      loca_ret <- list(HR10.vessels =  str_replace_all(as.character(loca_mat[21+3*length(BMT_FLEETSEGMENTS)+length(BMT_SPECIES),2]), "\\\\", "/" ), HR10.days= str_replace_all(as.character(loca_mat[21+3*length(BMT_FLEETSEGMENTS)+length(BMT_SPECIES),3]), "\\\\", "/" ) ) 
      
      }      
 
#   all_CFG_mat <- loca_mat[(nrow(mat_cfg_EconomicIndicator)+1):nrow(loca_mat),]
#    mat_cfg_effort_F <<- data.frame(all_CFG_mat[, 1:(length(BMT_FLEETSEGMENTS)+1)] )
#    mat_cfg_effort_F[,1] <- c("", "Type of relationship", "coeff a", "coeff b")
 
 loca_ret <- c(loca_ret, effort_F= list(loca_mat[(nrow(loca_mat)-3):nrow(loca_mat), 1:(length(BMT_FLEETSEGMENTS)+1)]) )     
  loca_ret$effort_F[,1] <- c("", "Type of relationship", "coeff a", "coeff b")
   
	 return(loca_ret)
}