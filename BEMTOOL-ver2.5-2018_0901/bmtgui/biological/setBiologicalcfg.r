# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



#
#
#
#
#
#
#
#
#
#
setBiological_species_settings <- function(w) {

# temp <- mat_cfg_species_settings

index_to_update = -1
selected <- gtkComboBoxGetActiveText(combo_species)
#wnd <- showMessage(paste("        Saving changes to ", selected, "...        ", sep=""))

index_to_update <- which(BMT_SPECIES == selected)    

row_to_update <- which(mat_cfg_species_settings[,1] == paste("casestudy.S", index_to_update, ".params", sep=""))

name_values <- colnames(mat_cfg_species_settings)[-1]


all_OK_values <- c()

  # -------------------------------------------------------- LIFE SPAN     
        if (all(all_OK_values)  )  {
         if (!is.na(as.numeric(as.character(gtkEntryGetText(bmt_entryLS_F)) ) ) )  {
           toadd_female_lifespan <- as.numeric(gtkEntryGetText(bmt_entryLS_F))
           all_OK_values <- c(all_OK_values, T)
    } else {
           all_OK_values <- c(all_OK_values, F)
           showError("Value for Life span of females is not valid!")
    }
    }
    
         if (all(all_OK_values)  )  {
    if ( !is.na(as.numeric(as.character(gtkEntryGetText(bmt_entryLS_M)) ) ) )  {
           toadd_male_lifespan <- as.numeric(gtkEntryGetText(bmt_entryLS_M))
           all_OK_values <- c(all_OK_values, T)
    } else {
           all_OK_values <- c(all_OK_values, F)
           showError("Value for Life span of males is not valid!")
    }
    }

      # -------------------------------------------------------- SEX RATIO  
        if (all(all_OK_values)  )  {
         if (!is.na(as.numeric(as.character(gtkEntryGetText(bmt_entry_sexratio)) ) ) )  {
           toadd_sexratio <- as.numeric(gtkEntryGetText(bmt_entry_sexratio))
           if ( (toadd_sexratio >= 0 & toadd_sexratio<= 1) ) {
                all_OK_values <- c(all_OK_values, T)
           } else {
           all_OK_values <- c(all_OK_values, F)
           showError("Value for Sex ratio is not valid!")
            }
    }
    } 

      # -------------------------------------------------------- LENGTH-WEIGTH RELATIONSHIP 
     if (all(all_OK_values)  )  {
          if (!is.na(as.numeric(as.character(gtkEntryGetText(bmt_entryAB_A_F)) ) ) )  {
           toadd_a_female <- as.numeric(gtkEntryGetText(bmt_entryAB_A_F))
           all_OK_values <- c(all_OK_values, T)
    } else {
           all_OK_values <- c(all_OK_values, F)
           showError("Value for \"a\" parameter of L-W RELATIONSHIP (females) is not valid!")
    }
    }
    
         if (all(all_OK_values)  )  {

    if (!is.na(as.numeric(as.character(gtkEntryGetText(bmt_entryAB_B_F)) ) ) )  {
           toadd_b_female <- as.numeric(gtkEntryGetText(bmt_entryAB_B_F))
           all_OK_values <- c(all_OK_values, T)
    } else {
           all_OK_values <- c(all_OK_values, F)
           showError("Value for \"b\" parameter of L-W RELATIONSHIP (females) is not valid!")
    }
    }
    
     if (all(all_OK_values)  )  {
    if (!is.na(as.numeric(as.character(gtkEntryGetText(bmt_entryAB_A_M)) ) ) )  {
           toadd_a_male <- as.numeric(gtkEntryGetText(bmt_entryAB_A_M))
           all_OK_values <- c(all_OK_values, T)
    } else {
           all_OK_values <- c(all_OK_values, F)
           showError("Value for \"a\" parameter of L-W RELATIONSHIP (males) is not valid!")
    }
    }
    
         if (all(all_OK_values)  )  {
    if (!is.na(as.numeric(as.character(gtkEntryGetText(bmt_entryAB_B_M)) ) ) )  {
           toadd_b_male <- as.numeric(gtkEntryGetText(bmt_entryAB_B_M))
           all_OK_values <- c(all_OK_values, T)
    } else {
           all_OK_values <- c(all_OK_values, F)
           showError("Value for \"b\" parameter of L-W RELATIONSHIP (males) is not valid!")
    }
    }
  
  # -------------------------------------------------------- GROWTH FUNCTION  
         if (all(all_OK_values)  )  {
    if (!is.na(as.numeric(as.character(gtkEntryGetText(bmt_entryVBF_t0_F)) ) ) )  {
           toadd_t0_female <- as.numeric(gtkEntryGetText(bmt_entryVBF_t0_F))
           all_OK_values <- c(all_OK_values, T)
    } else {
           all_OK_values <- c(all_OK_values, F)
           showError("Value for \"t0\" parameter of GROWTH FUNCTION (females) is not valid!")
    }
    }
    
         if (all(all_OK_values)  )  {
        if (!is.na(as.numeric(as.character(gtkEntryGetText(bmt_entryVBF_k_F)) ) ) )  {
           toadd_k_female <- as.numeric(gtkEntryGetText(bmt_entryVBF_k_F))
           all_OK_values <- c(all_OK_values, T)
    } else {
           all_OK_values <- c(all_OK_values, F)
           showError("Value for \"k\" parameter of GROWTH FUNCTION (females) is not valid!")
    }
    }
   
        if (all(all_OK_values)  )  { 
     if (!is.na(as.numeric(as.character(gtkEntryGetText(bmt_entryVBF_linf_F)) ) ) )  {
           toadd_linf_female <- as.numeric(gtkEntryGetText(bmt_entryVBF_linf_F))
           all_OK_values <- c(all_OK_values, T)
    } else {
           all_OK_values <- c(all_OK_values, F)
           showError("Value for \"Linf\" parameter of GROWTH FUNCTION (females) is not valid!")
    }
    }
   
        if (all(all_OK_values)  )  { 
     if (!is.na(as.numeric(as.character(gtkEntryGetText(bmt_entryVBF_t0_M)) ) ) )  {
           toadd_t0_male <- as.numeric(gtkEntryGetText(bmt_entryVBF_t0_M))
           all_OK_values <- c(all_OK_values, T)
    } else {
           all_OK_values <- c(all_OK_values, F)
           showError("Value for \"t0\" parameter of GROWTH FUNCTION (males) is not valid!")
    }
    }
    
           if (all(all_OK_values)  )  {
  if (!is.na(as.numeric(as.character(gtkEntryGetText(bmt_entryVBF_k_M)) ) ) )  {
           toadd_k_male <- as.numeric(gtkEntryGetText(bmt_entryVBF_k_M))
           all_OK_values <- c(all_OK_values, T)
    } else {
           all_OK_values <- c(all_OK_values, F)
           showError("Value for \"k\" parameter of GROWTH FUNCTION (males) is not valid!")
    }
    }
    
         if (all(all_OK_values)  )  {
 if (!is.na(as.numeric(as.character(gtkEntryGetText(bmt_entryVBF_linf_M)) ) ) )  {
           toadd_linf_male <- as.numeric(gtkEntryGetText(bmt_entryVBF_linf_M))
           all_OK_values <- c(all_OK_values, T)
    } else {
           all_OK_values <- c(all_OK_values, F)
           showError("Value for \"Linf\" parameter of GROWTH FUNCTION (males) is not valid!")
    }
    }
    
   # -------------------------------------------------------- MATURITY PARAMS
      
         if (all(all_OK_values)  )  {
if (!is.na(as.numeric(as.character(gtkEntryGetText(bmt_entryOGIVEL50_F)) ) ) )  {
           toadd_l50_female <- as.numeric(gtkEntryGetText(bmt_entryOGIVEL50_F))
           all_OK_values <- c(all_OK_values, T)
    } else {
           all_OK_values <- c(all_OK_values, F)
           showError("Value for \"L50%\" parameter of MATURITY OGIVE (females) is not valid!")
    }
    }
    
        if (all(all_OK_values)  )  {
 if (!is.na(as.numeric(as.character(gtkEntryGetText(bmt_entryMR_F)) ) ) )  {
           toadd_matrange_female <- as.numeric(gtkEntryGetText(bmt_entryMR_F))
           all_OK_values <- c(all_OK_values, T)
    } else {
           all_OK_values <- c(all_OK_values, F)
           showError("Value for \"L75%L25%\" parameter of MATURITY OGIVE (females) is not valid!")
    }
    }
    
           if (all(all_OK_values)  )  {
  if (!is.na(as.numeric(as.character(gtkEntryGetText(bmt_entryOGIVEL50_M)) ) ) )  {
           toadd_l50_male <- as.numeric(gtkEntryGetText(bmt_entryOGIVEL50_M))
           all_OK_values <- c(all_OK_values, T)
    } else {
           all_OK_values <- c(all_OK_values, F)
           showError("Value for \"L50%\" parameter of MATURITY OGIVE (males) is not valid!")
    }
    }
    
         if (all(all_OK_values)  )  {
if (!is.na(as.numeric(as.character(gtkEntryGetText(bmt_entryMR_M)) ) ) )  {
           toadd_matrange_male <- as.numeric(gtkEntryGetText(bmt_entryMR_M))
           all_OK_values <- c(all_OK_values, T)
    } else {
           all_OK_values <- c(all_OK_values, F)
           showError("Value for \"L75%L25%\" parameter of MATURITY OGIVE (males) is not valid!")
    }
    }
    
    
     if (all(all_OK_values)  )  {
     
     if (gtkToggleButtonGetActive(bmt_chk_runALADYM) ) {
if (!is.na(as.numeric(as.character(gtkEntryGetText(entry_biosettings_avg_years)) ) ) )  {
           toadd_RP_avg_years <- as.numeric(gtkEntryGetText(entry_biosettings_avg_years))
           
           end_ <<- as.numeric(as.character(mat_cfg_general[mat_cfg_general[,1] == "casestudy.endsimulation",2])) 
          start_ <<- as.numeric(as.character(mat_cfg_general[mat_cfg_general[,1] == "casestudy.startsimulation",2])) 
           
            if ( toadd_RP_avg_years <= (end_-start_+1) ) {
           all_OK_values <- c(all_OK_values, T)
           } else {
               all_OK_values <- c(all_OK_values, F)
           showError("\"Years for ALADYM RP and forecast\" must be less or equal to the simulation period!")
           }
    } else {
           all_OK_values <- c(all_OK_values, F)
           showError("Value for \"Years for ALADYM RP and forecast\" is not valid!")
    }
   
   }
   
    }
    
       if (all(all_OK_values)  )  {
    toadd_Mconstant <- gtkToggleButtonGetActive(bmt_chk_Mcostant) 
    toadd_SRrelationship <-  gtkToggleButtonGetActive(bmt_chk_SRrelationship) 
    toadd_runALADYM <-  gtkToggleButtonGetActive(bmt_chk_runALADYM)  
                    #c("","[RP ALADYM calculation]",	"[RP ALADYM use]", "[external table RPs]")
    toadd_RP_ALA_calculation  <- gtkToggleButtonGetActive(bmt_chk_RP_ALADYM_calc)
      toadd_RP_ALA_use  <- gtkToggleButtonGetActive(bmt_chk_RP_ALADYM_use)   
         
         toadd_RP_externalfile <- "" 
          if (!toadd_RP_ALA_use) {
            toadd_RP_externalfile <- gtkEntryGetText(lbl_bio_externalRP) 
            if (toadd_RP_externalfile == "") {
                   all_OK_values <- c(all_OK_values, F)
                     showError("Select the Reference Points external file!")
            } else {
               all_OK_values <- c(all_OK_values, T)
            }
          } else {
          
 if (!toadd_RP_ALA_calculation) {                      # "/", casestudy_name, " - Reference points - ",BMT_SPECIES[ALADYM_spe],".csv"
     REFERENCEPOINTS_table_temp <- paste(casestudy_path, "/Diagnosis/Biological Pressure Impact/ALADYM - ", selected,"/", casestudy_name, " - Reference points - ",selected,".csv", sep="") 
   
  #  REFERENCEPOINTS_table_temp <<- paste(tablesDIR_temp, "/", prefix_outfiles_temp, "Reference points", suffix_outfiles_temp, ".csv",sep="")
    referencepoints_tbl_temp <- try(read.csv(REFERENCEPOINTS_table_temp, sep=";"), silent=TRUE)
    if ( class(referencepoints_tbl_temp) ==  "try-error"  ) {
         all_OK_values <- c(all_OK_values, F)
         
         showError(paste("ALADYM reference points for ",selected," not found! Be sure the reference points calculation is checked before running ALADYM simulation.", sep=""))
    }
 }         
          
          }
   
       }


       if (all(all_OK_values)  )  { 
  
        index_to_update_ass = -1
selected_ass <- gtkComboBoxGetActiveText(combo_assessment_tool)

if (!is.null(selected_ass)) {
       # checks in XSA XSA XSA XSA XSA XSA XSA XSA XSA XSA XSA XSA XSA XSA XSA XSA XSA XSA XSA XSA XSA XSA XSA XSA XSA XSA XSA             
   if (selected_ass == "XSA")  {
  
        mat_temp <<- mat_cfg_XSA_list[[which(BMT_SPECIES == selected)]] 
          numb_ages_xsa <- as.numeric(as.character(gtkEntryGetText(entry_assessment_numberAgeClasses_XSA)) ) 
          
              if (!is.na(numb_ages_xsa) ) {
               if ( numb_ages_xsa > max(toadd_male_lifespan, toadd_female_lifespan)) {
                      all_OK_values <- c(all_OK_values, F)
                     showError("Value for XSA \"Number of age classes\" must be less or equal to the maximum life span!")
               } else {
                    mat_temp[2,6] <- as.numeric(as.character(gtkEntryGetText(entry_assessment_numberAgeClasses_XSA)) )
                    all_OK_values <- c(all_OK_values, T)
                    }
                  } else {
                     all_OK_values <- c(all_OK_values, F)
                     showError("Value for XSA \"Number of age classes\" is not valid!")
                  }
           
       
        if (all(all_OK_values)  )  { 
                   toadd_XSA_res_file <- gtkEntryGetText(entry_assessment_XSA_results) 
            if (toadd_XSA_res_file == "") {
                   all_OK_values <- c(all_OK_values, F)
                     showError("Select XSA results file!")
            } else {
               levels(mat_temp[,2]) <<- factor(c(levels(mat_temp[,2]), toadd_XSA_res_file))      
            mat_temp[4,2] <-  toadd_XSA_res_file
               all_OK_values <- c(all_OK_values, T)
            } 
            }
            
        if (all(all_OK_values)  )  { 
                   toadd_Catch_fleet_file <- gtkEntryGetText(entry_assessment_assess_Catchfleet) 
            if (toadd_Catch_fleet_file == "") {
                   all_OK_values <- c(all_OK_values, F)
                     showError("Select Catch by fleet file!")
            } else {
             levels(mat_temp[,3]) <<- factor(c(levels(mat_temp[,3]), toadd_Catch_fleet_file))      
              mat_temp[4,3] <-  toadd_Catch_fleet_file
               all_OK_values <- c(all_OK_values, T)
            }     
            }  
        
         if (all(all_OK_values)  )  { 
                   toadd_F_fleet_file <- gtkEntryGetText(entry_assessment_assess_Ffleet) 
            if (toadd_F_fleet_file == "") {
                   all_OK_values <- c(all_OK_values, F)
                     showError("Select F by fleet file!")
            } else {
            levels(mat_temp[,4]) <<- factor(c(levels(mat_temp[,4]), toadd_F_fleet_file))      
             mat_temp[4,4] <-  toadd_F_fleet_file
               all_OK_values <- c(all_OK_values, T)
            }
            } 
            
          if (all(all_OK_values)  )  { 
                   toadd_assess_RPs_file <- gtkEntryGetText(entry_assessment_assess_RPs) 
            if (toadd_assess_RPs_file == "") {
                   all_OK_values <- c(all_OK_values, F)
                     showError("Select F by fleet file!")
            } else {
            levels(mat_temp[,5]) <<- factor(c(levels(mat_temp[,5]), toadd_assess_RPs_file))      
             mat_temp[4,5] <-  toadd_assess_RPs_file
               all_OK_values <- c(all_OK_values, T)
            } 
            }
      # checks in VIT VIT VIT VIT VIT VIT VIT VIT VIT VIT VIT VIT VIT VIT VIT VIT VIT VIT VIT VIT VIT VIT VIT VIT VIT VIT VIT VIT VIT VIT VIT VIT             
              } else if (selected_ass == "VIT") {
    
        mat_temp <-  mat_cfg_VIT_list[[which(BMT_SPECIES == selected)]]
         
          min_ <- as.numeric(as.character(gtkEntryGetText(entry_assessment_minAge_VIT)) )
           max_ <- as.numeric(as.character(gtkEntryGetText(entry_assessment_maxAge_VIT)) )
                                                                      
        if (!is.na(min_ ) ) { 
                if (!is.na(max_ ) ) {
                      if ( min_ > max_) {
                         all_OK_values <- c(all_OK_values, F)
                          showError("Value for VIT \"Min age\" must be less or equal to value for VIT \"Max age\"!")
                      }  else {
                             all_OK_values <- c(all_OK_values, T)
                      }
                  } else {
                     all_OK_values <- c(all_OK_values, F)
                     showError("Value for VIT \"Max age\" is not valid!")
                  }
                  } else {
                     all_OK_values <- c(all_OK_values, F)
                     showError("Value for VIT \"Min age\" is not valid!")
                  }
                  
          if (all(all_OK_values)  )  { 
               if ( max_ > max(toadd_male_lifespan, toadd_female_lifespan)) {
                      all_OK_values <- c(all_OK_values, F)
                     showError("Value for VIT \"Max age\" must be less or equal to the maximum life span!")
               } else { 
            levels(mat_temp[,6]) <- factor(c(levels(mat_temp[,6]), min_))
            levels(mat_temp[,7]) <- factor(c(levels(mat_temp[,7]), max_))            
                    mat_temp[2,6]  <- min_
                    mat_temp[2,7]  <- max_
                    all_OK_values <- c(all_OK_values, T)
               }
          }
       
          if (all(all_OK_values)) {
             
            if ( (ncol(mat_temp)-1) >= length(BMT_YEARS_SIMULATION) ) {
                 ncolye <- length(BMT_YEARS_SIMULATION)
            } else {
                ncolye <- ncol(mat_temp)-1
            }
               
          if (gtkToggleButtonGetActive(bmt_chk_assessment_sex_VIT)) {
               levels(mat_temp[,3]) <- factor(c(levels(mat_temp[,3]), TRUE))
               mat_temp[2,3]  <- TRUE
          if (any(mat_temp[c(4,5), c(2:ncolye)] == "")) {
                showMessageOK("Check the missing VIT results path(s) before go next!")
          }
          } else {
            if (any(mat_temp[6, c(2:ncolye)] == "")) {
                showMessageOK("Check the missing VIT results path(s) before go next!")
          }
          levels(mat_temp[,3]) <- factor(c(levels(mat_temp[,3]), FALSE))
               mat_temp[2,3]  <- FALSE
          }
          
          levels(mat_temp[,4]) <- factor(c(levels(mat_temp[,4]), FALSE))
           mat_temp[2,4]  <- FALSE  # in this version the analysis by length is always FALSE
          
          if (gtkToggleButtonGetActive(bmt_chk_assessment_discard_VIT)) {
                    levels(mat_temp[,5]) <- factor(c(levels(mat_temp[,5]), TRUE))
               mat_temp[2,5]  <- TRUE
          } else {
                    levels(mat_temp[,5]) <- factor(c(levels(mat_temp[,5]), FALSE))
               mat_temp[2,5]  <- FALSE
          }
     }
    
            # checks in SURBA SURBA SURBA SURBA SURBA SURBA SURBA SURBA SURBA SURBA SURBA SURBA SURBA SURBA SURBA SURBA SURBA SURBA SURBA SURBA                            
              } else if (selected_ass == "SURBA") {
           mat_temp <- mat_cfg_SURBA_list[[which(BMT_SPECIES == selected)]] 

            numb_ages_surba <- as.numeric(as.character(gtkEntryGetText(entry_assessment_numberAgeClasses_SURBA)) ) 
          
              if (!is.na(numb_ages_surba) ) {
               if ( numb_ages_surba > max(toadd_male_lifespan, toadd_female_lifespan)) {
                      all_OK_values <- c(all_OK_values, F)
                     showError("Value for SURBA \"Number of age classes\" must be less or equal to the maximum life span!")
               } else {
                         levels(mat_temp[,3]) <- factor(c(levels(mat_temp[,3]), numb_ages_surba))
                    mat_temp[2,3] <- numb_ages_surba
                    all_OK_values <- c(all_OK_values, T)
                    }
                  } else {
                     all_OK_values <- c(all_OK_values, F)
                     showError("Value for SURBA \"Number of age classes\" is not valid!")
                  }


            if (all(all_OK_values)  )  { 
                   toadd_SURBA_res_file <- gtkEntryGetText(entry_assessment_SURBAresults) 
            if (toadd_SURBA_res_file == "") {
                   all_OK_values <- c(all_OK_values, F)
                     showError("Select SURBA results file!")
            } else {
              levels(mat_temp[,2]) <- factor(c(levels(mat_temp[,2]), toadd_SURBA_res_file))
            mat_temp[4,2] <-  toadd_SURBA_res_file
               all_OK_values <- c(all_OK_values, T)
            } 
            }


           # checks in Report  Report  Report  Report  Report  Report  Report   
              } else if (selected_ass == "from Report") {
         mat_temp <- mat_cfg_externalReport_list[[which(BMT_SPECIES == selected)]] 
         
            numb_ages_report <- as.numeric(as.character(gtkEntryGetText(entry_assessment_numberAgeClasses_Report)) ) 
          
              if (!is.na(numb_ages_report) ) {
               if ( numb_ages_report > max(toadd_male_lifespan, toadd_female_lifespan)) {
                      all_OK_values <- c(all_OK_values, F)
                     showError("Value for Report \"Number of age classes\" must be less or equal to the maximum life span!")
               } else {
                     levels(mat_temp[,6]) <- factor(c(levels(mat_temp[,6]), numb_ages_report))
                    mat_temp[2,6] <- numb_ages_report
                    all_OK_values <- c(all_OK_values, T)
                    }
                  } else {
                     all_OK_values <- c(all_OK_values, F)
                     showError("Value for Report \"Number of age classes\" is not valid!")
                  }


            if (all(all_OK_values)  )  { 
                   toadd_Report_res_file <- gtkEntryGetText(entry_assessment_Reportresults) 
            if (toadd_Report_res_file == "") {
                   all_OK_values <- c(all_OK_values, F)
                     showError("Select Report results file!")
            } else {
          levels(mat_temp[,2]) <- factor(c(levels(mat_temp[,2]), toadd_Report_res_file))
            mat_temp[4,2] <-  toadd_Report_res_file
               all_OK_values <- c(all_OK_values, T)
            } 
            }

          
              }
     }   
     
     }
     

    if (all(all_OK_values)) {
    
     mat_cfg_assessment_tools[[index_to_update]]  <<- mat_temp
    
    vect <- c(toadd_female_lifespan, toadd_male_lifespan, toadd_sexratio, toadd_a_female, toadd_b_female, toadd_a_male, toadd_b_male, toadd_t0_female, toadd_k_female, toadd_linf_female, toadd_t0_male, toadd_k_male, toadd_linf_male, toadd_l50_female, toadd_matrange_female, toadd_l50_male, toadd_matrange_male)
    
    for (npar in 1:length(vect)) {
    levels(mat_cfg_species_settings[, npar+1]) <<- factor(c(levels(mat_cfg_species_settings[, npar+1]), vect[npar]))      
       mat_cfg_species_settings[row_to_update, npar+1] <<- vect[npar]
    }

        levels(mat_cfg_species_settings[, 19]) <<- factor(c(levels(mat_cfg_species_settings[,19]), toadd_Mconstant))      
  mat_cfg_species_settings[row_to_update, 19] <<- toadd_Mconstant # ifelse(as.logical(, "TRUE", "FALSE") )
          levels(mat_cfg_species_settings[, 20]) <<- factor(c(levels(mat_cfg_species_settings[,20]), toadd_SRrelationship))      
  mat_cfg_species_settings[row_to_update, 20] <<- toadd_SRrelationship
  
            levels(mat_cfg_ALADYM_sim[, 2]) <<- factor(c(levels(mat_cfg_ALADYM_sim[, 2]), toadd_runALADYM))      
    mat_cfg_ALADYM_sim[row_to_update, 2] <<-  toadd_runALADYM
   if (    mat_cfg_ALADYM_sim[row_to_update, 2]) {
                levels(mat_cfg_ALADYM_sim[, 3]) <<- factor(c(levels(mat_cfg_ALADYM_sim[, 3]), toadd_RP_avg_years))      
    mat_cfg_ALADYM_sim[row_to_update, 3] <<- toadd_RP_avg_years
    }
    print(paste("aggiorno riga:", row_to_update))
   
                   levels(mat_cfg_REF_points[, 2]) <<- factor(c(levels(mat_cfg_REF_points[, 2]), toadd_RP_ALA_calculation))       
        mat_cfg_REF_points[row_to_update,2] <<-  toadd_RP_ALA_calculation
                           levels(mat_cfg_REF_points[, 3]) <<- factor(c(levels(mat_cfg_REF_points[, 3]), toadd_RP_ALA_use))       
    mat_cfg_REF_points[row_to_update,3] <<- toadd_RP_ALA_use
                       levels(mat_cfg_REF_points[, 4]) <<- factor(c(levels(mat_cfg_REF_points[, 4]), toadd_RP_externalfile))       
    mat_cfg_REF_points[row_to_update,4] <<- toadd_RP_externalfile
#}

}

adding_order <- as.numeric(substr(mat_cfg_species_settings[,1],12,12))   
mat_cfg_species_settings <<- mat_cfg_species_settings[ order(adding_order),] 
mat_cfg_ALADYM_sim <<- mat_cfg_ALADYM_sim[ order(adding_order),]
mat_cfg_REF_points <<- mat_cfg_REF_points[ order(adding_order),]



}



assign_bio_externalRPspath <- function(w) {
      path <- choose.files(default = "", caption = "")
      
      if ( !is.na(path) )  {
           gtkEntrySetText(lbl_bio_externalRP, path) 
     } else {
         showError("Select the Reference Points external file!")
     }     
}



assign_bio_Reportresults_path <- function(w) {
      path <- choose.files(default = "", caption = "")
      
      if ( !is.na(path) )  {
           gtkEntrySetText(entry_assessment_Reportresults, path) 
     } else {
         showError("Select the External file results!")
     }     
}



assign_bio_SURBAresults_path <- function(w) {
      path <- choose.files(default = "", caption = "")
      if ( !is.na(path) )  {
     gtkEntrySetText(entry_assessment_SURBAresults, path) 
     }    
}



assign_bio_XSAresults_path <- function(w) {
      path <- choose.files(default = "", caption = "")
      if ( !is.na(path) )  {
     gtkEntrySetText(entry_assessment_XSA_results, path) 
     }    
}

assign_bio_assess_Catchfleet_path <- function(w) {
      path <- choose.files(default = "", caption = "")
      if ( !is.na(path) )  {
     gtkEntrySetText(entry_assessment_assess_Catchfleet, path) 
     }    
}


assign_bio_assess_Ffleet_path <- function(w) {
      path <- choose.files(default = "", caption = "")
      if ( !is.na(path) )  {
     gtkEntrySetText(entry_assessment_assess_Ffleet, path) 
     }    
}

assign_bio_assess_RPs_path <- function(w) {
      path <- choose.files(default = "", caption = "")
      if ( !is.na(path) )  {
     gtkEntrySetText(entry_assessment_assess_RPs, path) 
     }    
}


assign_bio_assess_VIT_path <- function(w) {
      path <- choose.files(default = "", caption = "")
      if ( !is.na(path) )  {
     gtkEntrySetText(entry_assessment_VITresults, path) 
     
   index_to_update = -1
selected <- gtkComboBoxGetActiveText(combo_years_VIT)
#wnd <- showMessage(paste("        Saving changes to ", selected, "...        ", sep=""))

index_to_update <- which(BMT_YEARS_SIMULATION == selected)

   index_to_update_spe = -1
selected_spe <- gtkComboBoxGetActiveText(combo_species)
#wnd <- showMessage(paste("        Saving changes to ", selected, "...        ", sep=""))

index_to_update_spe <- which(BMT_SPECIES == selected_spe)
    
     temp_VIT <- data.frame(mat_cfg_VIT_list[[index_to_update_spe]] )

   levels(temp_VIT[,index_to_update+1]) <- factor(c(levels(temp_VIT[,index_to_update+1]), path))      
     if ( gtkToggleButtonGetActive(radio_combined) ) {

         temp_VIT[6, index_to_update+1] <-  path
     } else if (gtkToggleButtonGetActive(radio_males) ) {
         temp_VIT[5, index_to_update+1] <-  path
     } else if (gtkToggleButtonGetActive(radio_females) ) {
        temp_VIT[4, index_to_update+1] <-  path
     }
  
   mat_cfg_VIT_list[[index_to_update_spe]] <<- temp_VIT 
   matrix_VITpath <<- data.frame(t(temp_VIT[3:6, -1]) )
	  matrix_VITpath <<- matrix_VITpath[matrix_VITpath[,1] != "", ]
	 matrix_VITpath[,1] <<- BMT_YEARS_SIMULATION
	  colnames(matrix_VITpath) <<-  c("Year", "Combined", "Males", "Females")
   
  
       if ( gtkToggleButtonGetActive(radio_combined) ) {
           reload_VITpaths_combined_table()
     } else if (gtkToggleButtonGetActive(radio_males) ) {
            reload_VITpaths_males_table()
     } else if (gtkToggleButtonGetActive(radio_females) ) {
          reload_VITpaths_females_table()
     }

     }
         
}
