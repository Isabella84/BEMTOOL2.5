# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




#
#
#
#           
setForecastfromGUI <- function(object) {
  # object = new_aldForecast
# object@reduction_scenario = gtkComboBoxGetActiveText(combo_Scenariotype)

string_nameFore <- gtkEntryGetText(entry_nameF)
m <- regexec("[[:alnum:]+[:punct:]+[:blank:]+]+", string_nameFore)
object@forecast_name <- as.character(regmatches(string_nameFore, m))

object@start_year = years_forecast[1]
object@end_year = years_forecast[length(years_forecast)]

object@reduction_of_total_F = gtkToggleButtonGetActive(chkFMSY) 

if (object@reduction_of_total_F) {
object@target_F = as.numeric(gtkEntryGetText(entry_targetF) )
object@target_year = as.numeric(gtkEntryGetText(entry_targetMonth)  )
}

object@no_years_average = new_aldSimulation@yearsForAverage

# noise
#noise_min_fore <- as.numeric(gtkEntryGetText(entry_noise_recr_min_fore))
#noise_max_fore <- as.numeric(gtkEntryGetText(entry_noise_recr_max_fore))
#
#object@noise <- data.frame(rbind(list( distribution=gtkComboBoxGetActiveText(combo_RecrNoise_dis_fore), 
#                                            min=noise_min_fore, max=noise_max_fore,
#                                            A=as.numeric(gtkEntryGetText(entryNoise_a_fore)), 
#                                            B=as.numeric(gtkEntryGetText(entryNoise_b_fore)) ) ), row.names=1)
#                                            
#  

object@CI_recruitment <- gtkToggleButtonGetActive(chkConfidenceIntervals_fore) 
object@CI_growth <- gtkToggleButtonGetActive(chkConfidenceIntervals_fore_crescita) 
object@CI_M <- gtkToggleButtonGetActive(chkConfidenceIntervals_fore_M) 
object@CI_maturity <- gtkToggleButtonGetActive(chkConfidenceIntervals_fore_Maturity) 
object@CI_selectivity <- gtkToggleButtonGetActive(chkConfidenceIntervals_fore_Selectivity) 

object@CI_calculation <- ifelse(any(c(object@CI_recruitment, object@CI_growth, object@CI_M, object@CI_maturity , object@CI_selectivity)), TRUE, FALSE)

 if (object@CI_calculation ) {
 object@CI_n_runs <- as.numeric(gtkEntryGetText(entry_CI_numb_runs_fore) ) 
 }



 if (object@CI_selectivity ) {
 
    if ( gtkToggleButtonGetActive(radio_selectivity_uncert_from_external_file) ) {
          object@CI_selectivity_file_or_distribution = 1
 } else {
         object@CI_selectivity_file_or_distribution = 2
 }
 
   if (  object@CI_selectivity_file_or_distribution == 1)  {
      object@CI_selectivity_1_external_vector = selectivity_uncert_vector_from_file_matrix
}  else {
     object@CI_selectivity_2_distribution_vector  = selectivity_uncert_distribution_from_file_matrix 
     # get_grid_selectivity(selectivity_uncertainty_distribution_matrix, as.numeric(as.character(new_aldForecast@CI_n_runs)) )
}

}


if (! object@CI_recruitment ) {

# leggi unità per spawners
   if ( gtkToggleButtonGetActive(radio_tons_fore) ) {
  object@stockr.unit <- "Tons (biomass)"
} else if ( gtkToggleButtonGetActive(radio_thousands_fore) ) {
  object@stockr.unit <- "Thousands (numbers)"
} 

# leggi reclutamento costante oppure parametri relazione stock-recruitment
if (gtkToggleButtonGetActive(radio_forecast_recruits_costant)) {
      object@recruitment_constant_or_SRR <- 1
     object@recruitment.constant  <- as.numeric(gtkEntryGetText(entry_costant_recr_forecast) ) 
}  else {
     # stock recruitment relationship
   object@recruitment_constant_or_SRR <- 2
object@stockr.relationship <- data.frame(rbind(list( relationship=gtkComboBoxGetActiveText(combo_SRtype_fore), 
                                            a=as.numeric(gtkEntryGetText(entrySR_params_a_fore)), 
                                            b=as.numeric(gtkEntryGetText(entrySR_params_b_fore)), 
                                            c=as.numeric(gtkEntryGetText(entrySR_params_c_fore)) ) ), row.names=1)
}


} else {

if ( gtkToggleButtonGetActive(radio_tons_fore_UN) ) {
  object@stockr.unit <- "Tons (biomass)"
} else if ( gtkToggleButtonGetActive(radio_thousands_fore_UN) ) {
  object@stockr.unit <- "Thousands (numbers)"
} 

 # se l'errore è applicato a reclutamento costante 
 if (gtkToggleButtonGetActive(radio_forecast_recruits_costant_UN)) {
       object@CI_recruitment_constant_or_SRR = 1
     object@CI_recruitment_1_value <- as.numeric(gtkEntryGetText(entry_costant_recr_forecast_UN) ) 
  
    if (gtkToggleButtonGetActive(radio_CI_err_additive_fore) ) {
       object@CI_recruitment_1_error_type = 1 # additive (1)
   } else {
       object@CI_recruitment_1_error_type = 2 # multiplicative (2)
   }
   
    if (gtkToggleButtonGetActive(radio_recruitment_error_ext_file_fore) ) {
       object@CI_recruitment_1_file_or_distribution = 1 # external file (1)
      object@CI_recruitment_1_1_error_vector = CI_external_matrix_fore    
        
   } else {
     object@CI_recruitment_1_file_or_distribution = 2 # distribution (2)       
    noiseA_value_fore <- as.numeric(gtkEntryGetText(entryNoise_a_fore_UN) )
    noiseB_value_fore <- as.numeric(gtkEntryGetText(entryNoise_b_fore_UN) )                                                                           #  min=noise_min_fore, max=noise_max_fore,
     object@CI_recruitment_1_2_distribution_params<- data.frame(rbind(list( distribution=gtkComboBoxGetActiveText(combo_RecrNoise_dis_fore), A=noiseA_value_fore, B=noiseB_value_fore  ) ), row.names=1)
     
   }
   
   
 } else {
 
   # stock recruitment relationship
     object@CI_recruitment_constant_or_SRR = 2
object@CI_recruitment_2_SRR_model <- gtkComboBoxGetActiveText(combo_SRtype_fore_UN)

    if (gtkToggleButtonGetActive(radio_distribution_SRR_uncert))  {
    object@CI_recruitment_2_file_or_distribution <- 2
    object@CI_recruitment_2_2_distribution <- gtkComboBoxGetActiveText(combo_hboxSR_Distribution_Uncert_UN)
       object@CI_recruitment_2_2_distribution_params = data.frame( 
       Mean_abc = c( as.numeric(gtkEntryGetText(entryMean_aSRR)), as.numeric(gtkEntryGetText(entryMean_bSRR)),  as.numeric(gtkEntryGetText(entryMean_cSRR)) ) , 
        StDev_abc = c( as.numeric(gtkEntryGetText(entryStDev_aSRR)), as.numeric(gtkEntryGetText(entryStDev_bSRR)),  as.numeric(gtkEntryGetText(entryStDev_cSRR)) ) )    
    } else {
      object@CI_recruitment_2_file_or_distribution <- 1 
      object@CI_recruitment_2_1_error_vector  = table_recruitments_fore_from_vector_UN    
    }

       
 }
 
  }



if (object@CI_growth) {

    if (gtkToggleButtonGetActive(radio_growth_uncert_Linf))  {
			object@CI_growth_Linf_k =  1
			
			if (gtkToggleButtonGetActive(radio_growth_uncert_Linf_from_distribution) ) {
				object@CI_growth_1_file_or_distribution = 2  #external file (1) /distribution (2)  

				object@CI_growth_1_2_distribution <- gtkComboBoxGetActiveText(combo_distr_growth_uncert_Linf)
				object@CI_growth_1_2_params = data.frame( 
       Mean_mf = c( as.numeric(gtkEntryGetText(entry_A_distr_growth_uncert_Linf_m)), as.numeric(gtkEntryGetText(entry_A_distr_growth_uncert_Linf_f)) ) , 
        StDev_mf = c( as.numeric(gtkEntryGetText(entry_B_distr_growth_uncert_Linf_m)), as.numeric(gtkEntryGetText(entry_B_distr_growth_uncert_Linf_f)) ) ) 
				
			} else {
				object@CI_growth_1_file_or_distribution = 1 #external file (1) /distribution (2)
				 object@CI_growth_1_1_external_vector  = growth_uncert_Linf_from_file_matrix 	
			}
			
	} else {
				object@CI_growth_Linf_k = 2

				
			if (gtkToggleButtonGetActive(radio_growth_uncert_k_from_distribution) ) {
				object@CI_growth_2_file_or_distribution = 2  #external file (1) /distribution (2)  

				object@CI_growth_2_2_distribution <- gtkComboBoxGetActiveText(combo_distr_growth_uncert_k)
				object@CI_growth_2_2_params = data.frame( 
       Mean_mf = c( as.numeric(gtkEntryGetText(entry_A_distr_growth_uncert_k_m)), as.numeric(gtkEntryGetText(entry_A_distr_growth_uncert_k_f)) ) , 
        StDev_mf = c( as.numeric(gtkEntryGetText(entry_B_distr_growth_uncert_k_m)), as.numeric(gtkEntryGetText(entry_B_distr_growth_uncert_k_f)) ) ) 
				
			} else {
				object@CI_growth_2_file_or_distribution = 1 #external file (1) /distribution (2)
				 object@CI_growth_2_1_external_vector  = growth_uncert_k_from_file_matrix 	
			}

	
	}
	}
		
	
if (object@CI_M) {
		object@CI_M_model = data.frame( 
       model_mf = c( gtkComboBoxGetActiveText(combo_M_model_m), gtkComboBoxGetActiveText(combo_M_model_f) ) , 
       Mtmax_mf = c( as.numeric(gtkEntryGetText(entry_Mtmax_m)), as.numeric(gtkEntryGetText(entry_Mtmax_f)) ) )  
}





if (object@CI_maturity) {

			if (gtkToggleButtonGetActive(radio_maturity_uncert_males_from_distribution) ) {
				object@CI_maturity_m_file_or_distribution = 2  #external file (1) /distribution (2)  

				object@CI_maturity_m_distribution = gtkComboBoxGetActiveText(combo_distr_maturity_uncert_males)
        object@CI_maturity_m_params = data.frame( Mean_L50_MR = c( as.numeric(gtkEntryGetText(entry_A_distr_maturity_uncert_males_L50)), as.numeric(gtkEntryGetText(entry_A_distr_maturity_uncert_males_MR)) ) , StDev_L50_MR = c( as.numeric(gtkEntryGetText(entry_B_distr_maturity_uncert_males_L50)), as.numeric(gtkEntryGetText(entry_B_distr_maturity_uncert_males_MR)) ) ) 		
			} else {
				object@CI_maturity_m_file_or_distribution = 1 #external file (1) /distribution (2)
				 object@CI_maturity_m_external_vector  = maturity_uncert_males_from_file_matrix 	
			}
			
						if (gtkToggleButtonGetActive(radio_maturity_uncert_males_from_distribution) ) {
				object@CI_maturity_f_file_or_distribution = 2  #external file (1) /distribution (2)  

				object@CI_maturity_f_distribution = gtkComboBoxGetActiveText(combo_distr_maturity_uncert_females)
        object@CI_maturity_f_params =data.frame( Mean_L50_MR = c( as.numeric(gtkEntryGetText(entry_A_distr_maturity_uncert_females_L50)), as.numeric(gtkEntryGetText(entry_A_distr_maturity_uncert_females_MR)) ) , StDev_L50_MR = c( as.numeric(gtkEntryGetText(entry_B_distr_maturity_uncert_females_L50)), as.numeric(gtkEntryGetText(entry_B_distr_maturity_uncert_females_MR)) ) ) 		
			} else {
				object@CI_maturity_f_file_or_distribution = 1 #external file (1) /distribution (2)
				 object@CI_maturity_f_external_vector  = maturity_uncert_females_from_file_matrix 	
			}
	
}
 
 
                                            
                                            
return(object)
}