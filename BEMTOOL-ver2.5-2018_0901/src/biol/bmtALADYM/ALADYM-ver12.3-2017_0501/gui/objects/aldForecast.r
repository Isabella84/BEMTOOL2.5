# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




setClass(Class="aldForecast",
        representation=representation(
      forecast_name = "character",
      start_year = "numeric",
      end_year = "numeric",
      reduction_of_total_F = "logical",
      target_F = "numeric",
      target_year = "numeric",    
      no_years_average = "numeric",
# per run deterministici e non
			stockr.unit = "character",
# solo per run deterministici
      recruitment_constant_or_SRR = "numeric", # (1) Apply error on constant recruitment , (2) Apply error on SRR  						
			recruitment.constant = "numeric",
			stockr.relationship = "data.frame",
# per incertezza
      CI_recruitment = "logical",     # ex: CI_calculation
      CI_growth = "logical",
      CI_M = "logical",
      CI_maturity = "logical",
      CI_selectivity = "logical",
      CI_calculation = "logical",  
      CI_n_runs = "numeric",
            
			CI_recruitment_constant_or_SRR = "numeric",      # (1) Apply error on constant recruitment , (2) Apply error on SRR
      CI_recruitment_1_value = "numeric",      # numerical value of recruitment
      CI_recruitment_1_error_type = "numeric",  #additive (1) /multiplicative (2)         ex: CI_error_type
      CI_recruitment_1_file_or_distribution = "numeric",  #external file (1) /distribution (2)                       ex: CI_error_source
      CI_recruitment_1_1_error_vector = "data.frame", # external file                       ex: CI_error.externalvector
      CI_recruitment_1_2_distribution_params = "data.frame",                             # ex: recruitment.noise
			CI_recruitment_2_SRR_model = "character",         # da confermare
			CI_recruitment_2_file_or_distribution = "numeric",    #external file (1) /distribution (2)  
			CI_recruitment_2_2_distribution = "character", 
      CI_recruitment_2_1_error_vector = "data.frame", # external file     
 			CI_recruitment_2_2_distribution_params = "data.frame",
			
			CI_growth_Linf_k =  "numeric",      # (1) Linf , (2) k
			CI_growth_1_file_or_distribution = "numeric",  #external file (1) /distribution (2)                   
			CI_growth_2_file_or_distribution = "numeric",  #external file (1) /distribution (2)     
			CI_growth_1_1_external_vector = "data.frame",     # external file    
			CI_growth_1_2_distribution = "character",     # distribuzione    
			CI_growth_1_2_params = "data.frame",     # parametri per sesso
			CI_growth_2_1_external_vector = "data.frame",     # external file    
			CI_growth_2_2_distribution = "character",     # external file    
			CI_growth_2_2_params = "data.frame",     # external file    	

			CI_M_model = "data.frame",
			
			CI_maturity_m_file_or_distribution = "numeric",  #external file (1) /distribution (2)                   
			CI_maturity_f_file_or_distribution = "numeric",  #external file (1) /distribution (2) 
			CI_maturity_m_external_vector = "data.frame",     # external file    
			CI_maturity_m_distribution = "character",     # distribuzione    
			CI_maturity_m_params = "data.frame",     # distribuzione 
			CI_maturity_f_external_vector = "data.frame",     # external file    
			CI_maturity_f_distribution = "character",     # distribuzione
			CI_maturity_f_params = "data.frame" ,
      
      CI_selectivity_file_or_distribution = "numeric",     # scelta  
      CI_selectivity_1_external_vector = "data.frame",     # file    
      CI_selectivity_2_distribution_vector = "data.frame"     # distribuzione   
       ))