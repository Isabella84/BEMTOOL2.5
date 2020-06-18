# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

check_aldForecast <- function(loca_aldForecast) {
 # loca_aldForecast <- new_aldForecast
# controlla nome       
loca_go_on <- T

# controlla il nome dello scenario
if (loca_go_on) {
if (length(loca_aldForecast@forecast_name) == 0 )  {
      showError("Empty field for forecast name!")
     loca_go_on <- F 
} 
}


# controlla gli input per la riduzione della F totale
if (loca_go_on) {
if ( loca_aldForecast@reduction_of_total_F ) {
if (!is.na(loca_aldForecast@target_year) ) {
        if ( !( loca_aldForecast@target_year > loca_aldForecast@start_year & loca_aldForecast@target_year <= loca_aldForecast@end_year ) ) {
                showError("Target year is not consistent with the forecast period!") 
                     loca_go_on <- F  
        }
   }  else {
         showError("Invalid value for Target year!") 
         loca_go_on <- F  
   }
   }
   }
   
   if (loca_go_on) {
if ( loca_aldForecast@reduction_of_total_F ) {
 if (is.na(loca_aldForecast@target_F) ) {
        showError("Invalid value for Target F!") 
        loca_go_on <- F  
   }     
} 
}


 
 # 
 if (loca_go_on) {
if ( loca_aldForecast@CI_M ) {
if (!loca_aldForecast@CI_growth) {
         showError("Impossible to apply uncertainty on Natural mortality if uncertainty on growth is not checked!") 
                     loca_go_on <- F  
   }
}
}


if (loca_go_on) {
if ( loca_aldForecast@CI_M ) {
if (!loca_aldForecast@CI_growth) {
         showError("Impossible to apply uncertainty on Natural mortality if uncertainty on growth is not checked!") 
                     loca_go_on <- F  
   }
}
}



 if (loca_go_on) {
if ( loca_aldForecast@CI_M ) {
if (loca_aldForecast@CI_M_model$model_mf[1] == "ProdbiomUniqueSolution" ) {
 if (is.na(loca_aldForecast@CI_M_model$Mtmax_mf[1]) ) {
            showError("Invalid value for Mtmax (males) for ProdbiomUniqueSolution model in M uncertainty!") 
              loca_go_on <- F  
                } 
   }
}
}



 if (loca_go_on) {
if ( loca_aldForecast@CI_M ) {
if (loca_aldForecast@CI_M_model$model_mf[2] == "ProdbiomUniqueSolution" ) {
 if (is.na(loca_aldForecast@CI_M_model$Mtmax_mf[2]) ) {
            showError("Invalid value for Mtmax (females) for ProdbiomUniqueSolution model in M uncertainty!") 
                     loca_go_on <- F  
                     }
   }
}
}



# controlla che in caso di reclutamento costante la cella sia numerica
 if (loca_go_on) {
if ( !loca_aldForecast@CI_recruitment ) {
if (loca_aldForecast@recruitment_constant_or_SRR == 1) {
 if (is.na(loca_aldForecast@recruitment.constant) ) {
             showError("Invalid value for costant recruitment in deterministic run!") 
             loca_go_on <- F  
        }
   }
   }
   }
   
   
# controlla che in caso di relazione stock-recruitment le celle necessarie in base al modello siano numeriche
if (loca_go_on) {
if ( !loca_aldForecast@CI_recruitment ) {
     if ( loca_aldForecast@recruitment_constant_or_SRR == 2) {
      if (is.na(loca_aldForecast@stockr.relationship$a) ) {
                showError("Invalid value for a parameter of SRR in deterministic run!") 
                     loca_go_on <- F  
        }
   }
   }
   }
   
   if (loca_go_on) {
if ( !loca_aldForecast@CI_recruitment ) {
     if ( loca_aldForecast@recruitment_constant_or_SRR == 2) {
           if (is.na(loca_aldForecast@stockr.relationship$b) ) {
                showError("Invalid value for b parameter of SRR in deterministic run!") 
                     loca_go_on <- F  
        }
   }  
   }
   }

 if (loca_go_on) {
if ( !loca_aldForecast@CI_recruitment ) {
     if ( loca_aldForecast@recruitment_constant_or_SRR == 2) {
if ( ! loca_aldForecast@stockr.relationship$relationship %in% c(SR_TYPE[c(1,2,5)]) ) {
           if (is.na(loca_aldForecast@stockr.relationship$c) ) {
                showError("Invalid value for c parameter of SRR in deterministic run!") 
                     loca_go_on <- F  
        }
}
   }
   }
   }

# controlla in caso di incertezza (complessiva) la cella del nuemro di run sia numerica
 if (loca_go_on) {
if ( loca_aldForecast@CI_calculation) {
   if (is.na(loca_aldForecast@CI_n_runs)) {
                showError("Invalid value for no. of runs!") 
                     loca_go_on <- F  
   }  
   }
   }

# controlla che in caso di reclutamento costante la cella sia numerica
 if (loca_go_on) {
if ( loca_aldForecast@CI_recruitment ) {
if ( loca_aldForecast@CI_recruitment_constant_or_SRR == 1)  {
   if (is.na(loca_aldForecast@CI_recruitment_1_value)) {
                showError("Invalid value for costant recruitment in stochastic run!") 
                     loca_go_on <- F  
        }
   }  
   }
   }
   
   
# in caso di distribuzione le celle siano numeriche
 if (loca_go_on) {
if ( loca_aldForecast@CI_recruitment  ) {
if (loca_aldForecast@CI_recruitment_constant_or_SRR == 1) {
     if ( loca_aldForecast@CI_recruitment_1_file_or_distribution == 2 ) {
        if (is.na(loca_aldForecast@CI_recruitment_1_2_distribution_params$A) ) {
                showError("Invalid value for error distribution parameters (mean) on costant recruitment!") 
                     loca_go_on <- F  
   } 
     }
 }
}
}
    
  if (loca_go_on) {
if ( loca_aldForecast@CI_recruitment ) {
if ( loca_aldForecast@CI_recruitment_constant_or_SRR == 1 ) {
        if ( loca_aldForecast@CI_recruitment_1_file_or_distribution == 2 ) {  
        if (is.na(loca_aldForecast@CI_recruitment_1_2_distribution_params$B)) {
                showError("Invalid value for error distribution parameters (st.dev.) on costant recruitment!") 
                     loca_go_on <- F  
   }  
   }
   }
   }
   }
   
    


# in caso di vettore esterno la matrice abbia righe = n_run e colonne uguale al numero di anni di forecast
if (loca_go_on) {
if ( loca_aldForecast@CI_recruitment  ) {
if (  loca_aldForecast@CI_recruitment_constant_or_SRR == 1 ) {
if ( loca_aldForecast@CI_recruitment_1_file_or_distribution == 1) {
   if (!(nrow(loca_aldForecast@CI_recruitment_1_1_error_vector) == loca_aldForecast@CI_n_runs & ncol(loca_aldForecast@CI_recruitment_1_1_error_vector) == (length(years_forecast)+1) ) ) {
                showError("Invalid dimension for external table of recruitment errors!") 
                loca_go_on <- F  
   }  
   }
   }
   }
   }


# in caso di vettore esterno la matrice abbia righe = n_run e colonne uguale al numero di anni di forecast
if (loca_go_on) {
if ( loca_aldForecast@CI_selectivity  ) {
if (  loca_aldForecast@CI_selectivity_file_or_distribution == 1 ) {
   if (!(nrow(loca_aldForecast@CI_selectivity_1_external_vector) == loca_aldForecast@CI_n_runs * length(FLEETSEGMENTS_names) & ncol(loca_aldForecast@CI_selectivity_1_external_vector) == 8 ) ) {
                showError("Invalid dimension for external table of parameters by fleet for selectivity uncertainty!") 
                loca_go_on <- F  
   }  

   }
   }
   }
   
   
   # in caso di vettore esterno la matrice abbia righe = n_run e colonne uguale al numero di anni di forecast
if (loca_go_on) {
if ( loca_aldForecast@CI_selectivity  ) {
if (  loca_aldForecast@CI_selectivity_file_or_distribution == 2 ) {
   if (!(nrow(loca_aldForecast@CI_selectivity_2_distribution_vector) == length(FLEETSEGMENTS_names) & ncol(loca_aldForecast@CI_selectivity_2_distribution_vector) == 13 ) ) {
                showError("Invalid dimension for external table of parameters distribution by fleet for selectivity uncertainty!") 
                loca_go_on <- F  
   }  

   }
   }
   }


   
# controlla che in caso di relazione stock-recruitment le celle necessarie in base al modello siano numeriche
if (loca_go_on) {
if ( loca_aldForecast@CI_recruitment) {
if (loca_aldForecast@CI_recruitment_constant_or_SRR == 2) {
        if (loca_aldForecast@CI_recruitment_2_file_or_distribution == 2) {
          if (is.na(loca_aldForecast@CI_recruitment_2_2_distribution_params$Mean_abc[1]) ) {
                showError("Invalid value for a parameter (mean) of SRR in stochastic run!") 
                     loca_go_on <- F  

   }  
   }
   }
   }
   }
   
   if (loca_go_on) {
if ( loca_aldForecast@CI_recruitment ) {
if ( loca_aldForecast@CI_recruitment_constant_or_SRR == 2) {
        if (loca_aldForecast@CI_recruitment_2_file_or_distribution == 2) {
   if (is.na(loca_aldForecast@CI_recruitment_2_2_distribution_params$StDev_abc[1]) ) {
                showError("Invalid value for a parameter (st. dev.) of SRR in stochastic run!") 
                     loca_go_on <- F  
        } else if ( loca_aldForecast@CI_recruitment_2_2_distribution != "Uniform" & as.numeric(as.character(loca_aldForecast@CI_recruitment_2_2_distribution_params$StDev_abc[1])) < 0 ) {
                showError("Value for a parameter (st. dev.) of SRR in stochastic run must be equal to 0 or positive!") 
                   loca_go_on <- F  
        } 
   }  
    }
    }
    }

 
 if (loca_go_on) {
if ( loca_aldForecast@CI_recruitment ) {  
if (  loca_aldForecast@CI_recruitment_constant_or_SRR == 2) {  
        if (loca_aldForecast@CI_recruitment_2_file_or_distribution == 2) {
          if (is.na(loca_aldForecast@CI_recruitment_2_2_distribution_params$Mean_abc[2])  ) {
                showError("Invalid value for b parameter (mean) of SRR in stochastic run!") 
                     loca_go_on <- F  
        }
   }  
   }
   }
   }

   
   if (loca_go_on) {
if ( loca_aldForecast@CI_recruitment) {
if (  loca_aldForecast@CI_recruitment_constant_or_SRR == 2) {
        if (loca_aldForecast@CI_recruitment_2_file_or_distribution == 2) {
   if (is.na(loca_aldForecast@CI_recruitment_2_2_distribution_params$StDev_abc[2]) ) {
                showError("Invalid value for b parameter (st. dev.) of SRR in stochastic run!") 
                     loca_go_on <- F  
   }  else if (loca_aldForecast@CI_recruitment_2_2_distribution != "Uniform" & as.numeric(as.character(loca_aldForecast@CI_recruitment_2_2_distribution_params$StDev_abc[2])) < 0 ) {
                showError("Value for b parameter (st. dev.) of SRR in stochastic run must be equal to 0 or positive!") 
                   loca_go_on <- F  
        } 
   } 
   }
   }
   }    

   if (loca_go_on) {
if ( loca_aldForecast@CI_recruitment ) {
if ( loca_aldForecast@CI_recruitment_constant_or_SRR == 2) {
        if (loca_aldForecast@CI_recruitment_2_file_or_distribution == 2) {
if ( ! loca_aldForecast@CI_recruitment_2_SRR_model %in% c(SR_TYPE[c(1,2,5)]) ) {
   if (is.na(loca_aldForecast@CI_recruitment_2_2_distribution_params$Mean_abc[3])) {
                showError("Invalid value for c parameter (mean) of SRR in stochastic run!") 
                     loca_go_on <- F  
   }  
   }
   }
   }
   }
     }
  
     if (loca_go_on) {
if ( loca_aldForecast@CI_recruitment ) {
if ( loca_aldForecast@CI_recruitment_constant_or_SRR == 2) {
        if (loca_aldForecast@CI_recruitment_2_file_or_distribution == 2) {
if ( ! loca_aldForecast@CI_recruitment_2_SRR_model %in% c(SR_TYPE[c(1,2,5)]) ) { 
   if (is.na(loca_aldForecast@CI_recruitment_2_2_distribution_params$StDev_abc[3]) ) {
                showError("Invalid value for c parameter (st. dev.) of SRR in stochastic run!") 
                     loca_go_on <- F  
   } else if ( loca_aldForecast@CI_recruitment_2_2_distribution != "Uniform" & as.numeric(as.character(loca_aldForecast@CI_recruitment_2_2_distribution_params$StDev_abc[3])) < 0 ) {
                showError("Value for c parameter (st. dev.) of SRR in stochastic run must be equal to 0 or positive!") 
                   loca_go_on <- F  
        } 
}
   }
   }
   }
   }
   
   
# in caso di distribuzione le celle siano numeriche
 if (loca_go_on) {
if ( loca_aldForecast@CI_growth  ) {
if (  loca_aldForecast@CI_growth_Linf_k == 1 ) {
     if ( loca_aldForecast@CI_growth_1_file_or_distribution == 2 ) {
        if (is.na(loca_aldForecast@CI_growth_1_2_params[1,1])  ) {
                showError("Invalid value for error distribution parameters (mean) on Linf for MALES in growth uncertainty!") 
                     loca_go_on <- F  
   }     
     }
 }
}
}

 if (loca_go_on) {
if ( loca_aldForecast@CI_growth ) {
if ( loca_aldForecast@CI_growth_Linf_k == 1 ) {
     if ( loca_aldForecast@CI_growth_1_file_or_distribution == 2 ) {
      if (is.na(loca_aldForecast@CI_growth_1_2_params[2,1]) ) {
            showError("Invalid value for error distribution parameters (mean) on Linf for FEMALES in growth uncertainty!") 
             loca_go_on <- F  
   }  
     }
 }
}
}


 if (loca_go_on) {
if ( loca_aldForecast@CI_growth ) {
if ( loca_aldForecast@CI_growth_Linf_k == 1 ) {
     if ( loca_aldForecast@CI_growth_1_file_or_distribution == 2 ) {
      if (is.na(loca_aldForecast@CI_growth_1_2_params[1,2])) {
           showError("Invalid value for error distribution parameters (st.dev.) on Linf for MALES in growth uncertainty!") 
            loca_go_on <- F  
   }  else if ( loca_aldForecast@CI_growth_1_2_distribution != "Uniform" & as.numeric(as.character(loca_aldForecast@CI_growth_1_2_params[1,2])) < 0 ) {
                showError("Value for error distribution parameters (st.dev.) on Linf for MALES in growth uncertainty must be equal to 0 or positive!") 
                loca_go_on <- F  
        }   
     }
 }
}
}

 if (loca_go_on) {
if ( loca_aldForecast@CI_growth ) {
if ( loca_aldForecast@CI_growth_Linf_k == 1 ) {
     if ( loca_aldForecast@CI_growth_1_file_or_distribution == 2 ) {
     if (is.na(loca_aldForecast@CI_growth_1_2_params[2,2]) ) {
          showError("Invalid value for error distribution parameters (st.dev.) on Linf for FEMALES in growth uncertainty!") 
          loca_go_on <- F  
   } else if (loca_aldForecast@CI_growth_1_2_distribution != "Uniform" & as.numeric(as.character(loca_aldForecast@CI_growth_1_2_params[2,2])) < 0 ) {
                showError("Value for error distribution parameters (st.dev.) on Linf for FEMALES in growth uncertainty must be equal to 0 or positive!") 
                loca_go_on <- F  
        }    
      
     }
 }
}
}
   
   
   
   
      
# in caso di distribuzione le celle siano numeriche
 if (loca_go_on) {
if ( loca_aldForecast@CI_growth ) {
if ( loca_aldForecast@CI_growth_Linf_k == 2 ) {
     if ( loca_aldForecast@CI_growth_2_file_or_distribution == 2 ) {
          if (is.na(loca_aldForecast@CI_growth_2_2_params[1,1]) ) {
             showError("Invalid value for error distribution parameters (mean) on K for MALES in growth uncertainty!") 
             loca_go_on <- F  
   }    
     }
 }
}
}

 if (loca_go_on) {
if ( loca_aldForecast@CI_growth ) {
if ( loca_aldForecast@CI_growth_Linf_k == 2 ) {
     if ( loca_aldForecast@CI_growth_2_file_or_distribution == 2 ) {
     if (is.na(loca_aldForecast@CI_growth_2_2_params[2,1]) ) {
             showError("Invalid value for error distribution parameters (mean) on K for FEMALES in growth uncertainty!") 
                     loca_go_on <- F  
   }  
     }
 }
}
}


 if (loca_go_on) {
if ( loca_aldForecast@CI_growth ) {
if ( loca_aldForecast@CI_growth_Linf_k == 2 ) {
     if ( loca_aldForecast@CI_growth_2_file_or_distribution == 2 ) {
          if (is.na(loca_aldForecast@CI_growth_2_2_params[1,2]) ) {
             showError("Invalid value for error distribution parameters (st.dev.) on K for MALES in growth uncertainty!") 
                     loca_go_on <- F  
   } else if ( loca_aldForecast@CI_growth_2_2_distribution != "Uniform" & as.numeric(as.character(loca_aldForecast@CI_growth_2_2_params[1,2])) < 0 ) {
                showError("Value for error distribution parameters (st.dev.) on K for MALES in growth uncertainty must be equal to 0 or positive!") 
                loca_go_on <- F  
        } 
     }
 }
}
}

 if (loca_go_on) {
if ( loca_aldForecast@CI_growth ) {
if ( loca_aldForecast@CI_growth_Linf_k == 2 ) {
     if ( loca_aldForecast@CI_growth_2_file_or_distribution == 2 ) {
        if (is.na(loca_aldForecast@CI_growth_2_2_params[2,2]) ) {
             showError("Invalid value for error distribution parameters (st.dev.) on K for FEMALES in growth uncertainty!") 
             loca_go_on <- F  
   }  else if ( loca_aldForecast@CI_growth_2_2_distribution != "Uniform" & as.numeric(as.character(loca_aldForecast@CI_growth_2_2_params[2,2])) < 0 ) {
                showError("Value for error distribution parameters (st.dev.) on K for FEMALES in growth uncertainty must be equal to 0 or positive!") 
                loca_go_on <- F  
        } 
     }
 }
}
}
   
   

# controlla in caso di incertezza su SRR in caso di file esterno la matrice abbia numero di righe= n_run e numero di colonne piene corrispondenti al modello scelto
if (loca_go_on) {
if ( loca_aldForecast@CI_recruitment ) {
if (  loca_aldForecast@CI_recruitment_constant_or_SRR == 2) {
if ( loca_aldForecast@CI_recruitment_2_file_or_distribution == 1) {
   if (!(nrow(loca_aldForecast@CI_recruitment_2_1_error_vector) == loca_aldForecast@CI_n_runs & ncol(loca_aldForecast@CI_recruitment_2_1_error_vector) == 4 ) ) {
         showError("Invalid dimension for external table of recruitment errors on SRR parameters!") 
         loca_go_on <- F  
   }  
   }
   }
   }
   }
   
   
 
 if (loca_go_on) {
if ( loca_aldForecast@CI_maturity ) {
if ( loca_aldForecast@CI_maturity_m_file_or_distribution == 1) {
   if (!(nrow(loca_aldForecast@CI_maturity_m_external_vector) == loca_aldForecast@CI_n_runs & ncol(loca_aldForecast@CI_maturity_m_external_vector) == 3 ) ) {
         showError("Invalid dimension for external table of errors on maturity ogive parameters for MALES!") 
         loca_go_on <- F  
   }  
   }

   }
   }
   
   
    if (loca_go_on) {
if ( loca_aldForecast@CI_maturity ) {
if ( loca_aldForecast@CI_maturity_f_file_or_distribution == 1) {
   if (!(nrow(loca_aldForecast@CI_maturity_f_external_vector) == loca_aldForecast@CI_n_runs & ncol(loca_aldForecast@CI_maturity_f_external_vector) == 3 ) ) {
         showError("Invalid dimension for external table of errors on maturity ogive parameters for FEMALES!") 
         loca_go_on <- F  
   }  
   }

   }
   }
 
   
   
   # in caso di distribuzione le celle siano numeriche
 if (loca_go_on) {
if ( loca_aldForecast@CI_maturity  ) {

     if ( loca_aldForecast@CI_maturity_m_file_or_distribution == 2 ) {
        if (is.na(loca_aldForecast@CI_maturity_m_params[1,1])  ) {
                showError("Invalid value for error distribution parameters (mean) on L50% for MALES in maturity uncertainty!") 
                     loca_go_on <- F  
   }     
     }

}
}
   
 
  if (loca_go_on) {
if ( loca_aldForecast@CI_maturity  ) {

     if ( loca_aldForecast@CI_maturity_f_file_or_distribution == 2 ) {
        if (is.na(loca_aldForecast@CI_maturity_f_params[1,1])  ) {
                showError("Invalid value for error distribution parameters (mean) on L50% for FEMALES in maturity uncertainty!") 
                     loca_go_on <- F  
   }     
     }

}
}  
   


      # in caso di distribuzione le celle siano numeriche
 if (loca_go_on) {
if ( loca_aldForecast@CI_maturity  ) {

     if ( loca_aldForecast@CI_maturity_m_file_or_distribution == 2 ) {
        if (is.na(loca_aldForecast@CI_maturity_m_params[2,1])  ) {
                showError("Invalid value for error distribution parameters (mean) on L75%L25% for MALES in maturity uncertainty!") 
                     loca_go_on <- F  
   }     
     }

}
}
   
 
  if (loca_go_on) {
if ( loca_aldForecast@CI_maturity  ) {

     if ( loca_aldForecast@CI_maturity_f_file_or_distribution == 2 ) {
        if (is.na(loca_aldForecast@CI_maturity_f_params[2,1])  ) {
                showError("Invalid value for error distribution parameters (mean) on L75%L25% for FEMALES in maturity uncertainty!") 
                     loca_go_on <- F  
   }     
     }

}
} 
   


 if (loca_go_on) {
if ( loca_aldForecast@CI_maturity ) {
     if ( loca_aldForecast@CI_maturity_m_file_or_distribution == 2 ) {
          if (is.na(loca_aldForecast@CI_maturity_m_params[1,2]) ) {
             showError("Invalid value for error distribution parameters (st.dev.) on L50% for MALES in maturity uncertainty!") 
                     loca_go_on <- F  
   } else if (loca_aldForecast@CI_maturity_m_distribution != "Uniform" &  as.numeric(as.character(loca_aldForecast@CI_maturity_m_params[1,2])) < 0 ) {
                showError("Value for error distribution parameters (st.dev.) on L50% for MALES in maturity uncertainty must be equal to 0 or positive!") 
                loca_go_on <- F  
        } 
     }

}
}




 if (loca_go_on) {
if ( loca_aldForecast@CI_maturity ) {
     if ( loca_aldForecast@CI_maturity_m_file_or_distribution == 2 ) {
          if (is.na(loca_aldForecast@CI_maturity_m_params[2,2]) ) {
             showError("Invalid value for error distribution parameters (st.dev.) on L75%L25% for MALES in maturity uncertainty!") 
                     loca_go_on <- F  
   } else if ( loca_aldForecast@CI_maturity_m_distribution != "Uniform" & as.numeric(as.character(loca_aldForecast@CI_maturity_m_params[2,2])) < 0 ) {
                showError("Value for error distribution parameters (st.dev.) on L75%L25% for MALES in maturity uncertainty must be equal to 0 or positive!") 
                loca_go_on <- F  
        } 
     }

}
}



if (loca_go_on) {
if ( loca_aldForecast@CI_maturity ) {
     if ( loca_aldForecast@CI_maturity_f_file_or_distribution == 2 ) {
          if (is.na(loca_aldForecast@CI_maturity_f_params[1,2]) ) {
             showError("Invalid value for error distribution parameters (st.dev.) on L50% for FEMALES in maturity uncertainty!") 
                     loca_go_on <- F  
   } else if ( loca_aldForecast@CI_maturity_f_distribution != "Uniform" & as.numeric(as.character(loca_aldForecast@CI_maturity_f_params[1,2])) < 0 ) {
                showError("Value for error distribution parameters (st.dev.) on L50% for FEMALES in maturity uncertainty must be equal to 0 or positive!") 
                loca_go_on <- F  
        } 
     }

}
}


if (loca_go_on) {
if ( loca_aldForecast@CI_maturity ) {
     if ( loca_aldForecast@CI_maturity_f_file_or_distribution == 2 ) {
          if (is.na(loca_aldForecast@CI_maturity_f_params[2,2]) ) {
             showError("Invalid value for error distribution parameters (st.dev.) on L75%L25% for FEMALES in maturity uncertainty!") 
                     loca_go_on <- F  
   } else if ( loca_aldForecast@CI_maturity_f_distribution != "Uniform" & as.numeric(as.character(loca_aldForecast@CI_maturity_f_params[2,2])) < 0 ) {
                showError("Value for error distribution parameters (st.dev.) on L75%L25% for FEMALES in maturity uncertainty must be equal to 0 or positive!") 
                loca_go_on <- F  
        } 
     }

}
}


   
   if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION")) {
       checkentryM <- gtkToggleButtonGetActive(radio_Zentry)  
   } else {
       checkentryM <- ifelse(new_aldSimulation@enteringMortality =="Z", T, F) 
   }
  

if (checkentryM & !gtkToggleButtonGetActive(chkFMSY)) {

for (fs in 1:length(FLEETSEGMENTS_names) ) {
check_ <- check_input("SELECTIVITY_TABLE_FORE", FleetList_forecast[[fs]]@selectivity.vector)
if (check_$result == "KO" & loca_go_on ) {
     loca_go_on <- FALSE
    showError(paste(FleetList_forecast[[fs]]@fleetname, ":", check_$msg))
}
}
}




for (fs in 1:length(FLEETSEGMENTS_names) ) {

if ( FleetList_forecast[[fs]]@discard.calculation == "YES" & loca_go_on ) {

if ( FleetList_forecast[[fs]]@discard.datatype == "Reverse ogive") {
     check_ <- check_input("DISCARD_TABLE_FORE", FleetList_forecast[[fs]]@discard.vector)
     if (check_$result == "KO" & loca_go_on) {
     loca_go_on <- FALSE
    showError(paste(FleetList_forecast[[fs]]@fleetname, ":", check_$msg))
}

}
}

}


if (!gtkToggleButtonGetActive(chkFMSY)) {

for (fs in 1:length(FLEETSEGMENTS_names) ) {

if (!checkentryM) {
 check_ <- check_input("FISHINGEFFORT_VECTOR_FORE", FleetList_forecast[[fs]]@fishingeffort.vector)
     if (check_$result == "KO" & loca_go_on) {
     loca_go_on <- FALSE
    showError(paste(FleetList_forecast[[fs]]@fleetname, ":", check_$msg))
}
}

 check_ <- check_input("PPRODUCTION_VECTOR_FORE", FleetList_forecast[[fs]]@pproduction.vector)
     if (check_$result == "KO" & loca_go_on) {
   #  go_on <- FALSE
   # just a warning
    showError(paste(FleetList_forecast[[fs]]@fleetname, ":", check_$msg))
}


#if ( all(FleetList_forecast[[fs]]@production.vector[,2:13] == 0)  & go_on) {
#    go_on <- FALSE
#    showError(paste(FleetList_forecast[[fs]]@fleetname, ": in PRODUCTION .csv file all values are equal to 0!"))
#}

if (BMT_SCENARIO != BMT_HR_CHANGE_FISHMORTALITY) {

if (! gtkToggleButtonGetActive(radio_fishingcoeff)) {

if (!INTEGRATED_APPROACH) {
    matrix_to_be_checked <- FleetList_forecast[[fs]]@vessels.vector[,2:13]
} else {
    matrix_to_be_checked <- FleetList_forecast[[fs]]@vessels.vector[1:current_year,2:13]
}

if ( all(matrix_to_be_checked[,] == 0) & loca_go_on) {
    loca_go_on <- FALSE
    showError(paste(FleetList_forecast[[fs]]@fleetname, ": all values for VESSELS are equal to 0!"))
}

if (!INTEGRATED_APPROACH) {
    matrix_to_be_checked <- FleetList_forecast[[fs]]@days.vector[,2:13]
} else {
    matrix_to_be_checked <- FleetList_forecast[[fs]]@days.vector[1:current_year,2:13]
}


if ( all(matrix_to_be_checked[,] == 0) & loca_go_on) {
    loca_go_on <- FALSE
    showError(paste(FleetList_forecast[[fs]]@fleetname, ": all values for DAYS are equal to 0!"))
}

if (!INTEGRATED_APPROACH) {
    matrix_to_be_checked <- FleetList_forecast[[fs]]@gt.vector[,2:13]
} else {
    matrix_to_be_checked <- FleetList_forecast[[fs]]@gt.vector[1:current_year,2:13]
}

if ( all(matrix_to_be_checked[,] == 0) & loca_go_on) {
    loca_go_on <- FALSE
    showError(paste(FleetList_forecast[[fs]]@fleetname, ": all values for GTs are equal to 0!"))
}
}
 }


}  

}
 

 return(loca_go_on)
}