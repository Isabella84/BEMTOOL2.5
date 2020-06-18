# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


setScenarioOptions <- function() {

print(mat_cfg_scenario_settings_fore_options)

all_scenario_codes <- c(BMT_HR_CHANGE_SELECTIVITY, BMT_HR_CHANGE_FISHEFFORT, BMT_HR_CHANGE_FISHMORTALITY, BMT_HR_CHANGE_TOTAL_FISHMORTALITY, BMT_HR_STATUS_QUO, BMT_HR_TAC_VARIATION, BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT, BMT_HR_CHANGE_SELECTIVITY_BEHAVIOURAL, BMT_HR_CHANGE_FISHEFFORT_BEHAVIOURAL, BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT_BEHAVIOURAL, BMT_HR_STATUS_QUO_BEHAVIOURAL)

# SCEN_EFFS <- c(BMT_HR_CHANGE_FISHEFFORT, BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT, BMT_HR_CHANGE_FISHEFFORT_BEHAVIOURAL, BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT_BEHAVIOURAL)   
scenario_numb <- check_scenario_combination()

 final_mat <- data.frame(matrix("", ncol=6, nrow=0)) 

if (scenario_numb >0 & scenario_numb <=12) {


 colnames(final_mat) <- paste("col", 1:6, sep="")
 levels( mat_cfg_scenario_settings_fore[,2]) <<- factor( c(levels( mat_cfg_scenario_settings_fore[,2]), scenario_numb) )         
 mat_cfg_scenario_settings_fore[2,2] <<- scenario_numb 
levels( mat_cfg_scenario_settings_fore[,3]) <<- factor(c(levels( mat_cfg_scenario_settings_fore[,3]), gtkEntryGetText(bmt_entry_scenario_name)))         
 mat_cfg_scenario_settings_fore[2,3] <<-  gtkEntryGetText(bmt_entry_scenario_name) 
 to_add <- data.frame(cbind(mat_cfg_scenario_settings_fore, matrix("", nrow=2, ncol=3)))
  colnames(to_add) <- paste("col", 1:6, sep="")
 final_mat <- data.frame(rbind(final_mat, to_add))

 for (num_scen in all_scenario_codes)  {
 
 if (num_scen == BMT_HR_CHANGE_SELECTIVITY ) {      # ----------------------------------------------------------------------------------------------------------
 to_add <- data.frame(matrix(c(paste("casestudy.HR",num_scen, sep=""),	"<NO INPUT REQUIRED>", "","","",""), nrow=1))
    colnames(to_add) <- paste("col", 1:6, sep="")
  final_mat <- data.frame(rbind(final_mat, to_add))
  
} else if (num_scen == BMT_HR_CHANGE_FISHEFFORT ) {  # ----------------------------------------------------------------------------------------------------------

      to_add <- data.frame(matrix(c("", "[Monthly VESSELS file for FORECAST]",	"[Monthly DAYS.average file for FORECAST]", "","",""), nrow=1))
    colnames(to_add) <- paste("col", 1:6, sep="")
    final_mat <- data.frame(rbind(final_mat, to_add))
     
     if (scenario_numb == BMT_HR_CHANGE_FISHEFFORT) {
     
           save_effort_files_fore(NULL)
           
     # name_this_scenario <- paste("HR", mat_cfg_scenario_settings_fore[2,2], "-", mat_cfg_scenario_settings_fore[2,3], sep="")
         mat_cfg_scenario_settings_fore_options <<- vector(mode="list", length=2)

		 #        vess_path_ <- paste(mat_cfg_general[1,3],  "/", name_this_scenario, "/", name_this_scenario, " - monthly data on number ", BMT_YEARS_FORECAST[1] , "-", BMT_YEARS_FORECAST[length(BMT_YEARS_FORECAST)] ,".csv", sep="")
#       days_path_  <- paste(mat_cfg_general[1,3],  "/", name_this_scenario, "/", name_this_scenario, " - monthly data on seadays ", BMT_YEARS_FORECAST[1] , "-", BMT_YEARS_FORECAST[length(BMT_YEARS_FORECAST)] ,".csv", sep="") 

  vess_path_ <- paste(getwd(), "\\monthly data on number.csv", sep="")
       days_path_ <- paste(getwd(), "\\monthly data on seadays.csv", sep="")
        
          mat_cfg_scenario_settings_fore_options[[1]] <<- vess_path_
       mat_cfg_scenario_settings_fore_options[[2]]  <<-  days_path_ 
     to_add <- data.frame(matrix(c(paste("casestudy.HR",num_scen , sep=""), vess_path_, days_path_   , "","","") , nrow=1)) 
         colnames(to_add) <-  paste("col", 1:6, sep="")
    final_mat <- data.frame(rbind(final_mat, to_add)) 
    } else {
     to_add <- data.frame(matrix(c(paste("casestudy.HR",num_scen , sep=""), "", "" , "","",""), nrow=1))
         colnames(to_add) <-  paste("col", 1:6, sep="")
    final_mat <- data.frame(rbind(final_mat, to_add))
    }


} else if (num_scen == BMT_HR_CHANGE_FISHMORTALITY ) {     # ----------------------------------------------------------------------------------------------------------
         to_add <- data.frame(matrix(c("", "[Time span]",	"[% of reduction]", "","",""), nrow=1))
                  colnames(to_add) <-  paste("col", 1:6, sep="")
    final_mat <- data.frame(rbind(final_mat, to_add))

    if (scenario_numb == BMT_HR_CHANGE_FISHMORTALITY) {
     to_add <- data.frame(matrix(c(paste("casestudy.HR",num_scen , sep=""),  gtkEntryGetText(bmt_entry_timespan_r5), gtkEntryGetText(bmt_entry_reduction_r5) , "","",""), nrow=1))
     } else {
        to_add <- data.frame(matrix(c(paste("casestudy.HR",num_scen , sep=""),  "", "" , "","",""), nrow=1))  
     }
          colnames(to_add) <-  paste("col", 1:6, sep="")    
          final_mat <- data.frame(rbind(final_mat, to_add))
          
          to_add <- data.frame(matrix(c("", "[reduction scenario 1: Exponential, 2: Linear, 3: Logistic]",	"[% of reduction for VESSELS]",	"[% of reduction for DAYS]", "", ""), nrow=1))
          colnames(to_add) <-  paste("col", 1:6, sep="")  
          final_mat <- data.frame(rbind(final_mat, to_add))
          
          input_table_r5[,1] <- as.character(input_table_r5[,1]) 
               input_table_r5[,2] <- as.character(input_table_r5[,2])
                    input_table_r5[,3] <- as.character(input_table_r5[,3])
                         input_table_r5[,4] <- as.character(input_table_r5[,4])
          
          to_add <- data.frame(cbind(input_table_r5, matrix("", nrow=length(BMT_FLEETSEGMENTS), ncol=2)))
          to_add$fleet_segment <- paste("casestudy.HR", num_scen, ".F", 1:length(BMT_FLEETSEGMENTS), sep="")  
         
          colnames(to_add) <-  paste("col", 1:6, sep="")  
          final_mat <- data.frame(rbind(final_mat, to_add))

        input_table_r5$fleet_segment <- BMT_FLEETSEGMENTS
   if (scenario_numb == BMT_HR_CHANGE_FISHMORTALITY) {
mat_cfg_scenario_settings_fore_options <<- list(HR3.timespan = as.numeric(as.character( gtkEntryGetText(bmt_entry_timespan_r5) )), HR3.reduction = as.numeric(as.character( gtkEntryGetText(bmt_entry_reduction_r5) )), HR3.fleet_reductions = input_table_r5  )
             }
             
} else if (num_scen == BMT_HR_CHANGE_TOTAL_FISHMORTALITY ) {     # ----------------------------------------------------------------------------------------------------------

          to_add <- data.frame(matrix(c("", "[Time span]",	"[% of reduction]", "","",""), nrow=1))
                  colnames(to_add) <-  paste("col", 1:6, sep="")
    final_mat <- data.frame(rbind(final_mat, to_add))

    if (scenario_numb == BMT_HR_CHANGE_TOTAL_FISHMORTALITY) {
     to_add <- data.frame(matrix(c(paste("casestudy.HR",num_scen , sep=""),  gtkEntryGetText(bmt_entry_timespan_r6), gtkEntryGetText(bmt_entry_reduction_r6) , "","",""), nrow=1))
     } else {
     to_add <- data.frame(matrix(c(paste("casestudy.HR",num_scen , sep=""), "", "" , "","",""), nrow=1))
     }
          colnames(to_add) <-  paste("col", 1:6, sep="")    
          final_mat <- data.frame(rbind(final_mat, to_add))
          
          to_add <- data.frame(matrix(c("", "[to be reduced: Y, N]",	"[% of reduction for VESSELS]",	"[% of reduction for DAYS]", "", ""), nrow=1))
          colnames(to_add) <-  paste("col", 1:6, sep="")  
          final_mat <- data.frame(rbind(final_mat, to_add))
          
          input_table_r6[,1] <- as.character(input_table_r6[,1]) 
               input_table_r6[,2] <- as.character(input_table_r6[,2])
                    input_table_r6[,3] <- as.character(input_table_r6[,3])
                         input_table_r6[,4] <- as.character(input_table_r6[,4])
          
          to_add <- data.frame(cbind(input_table_r6, matrix("", nrow=length(BMT_FLEETSEGMENTS), ncol=2)))
          to_add$fleet_segment <- paste("casestudy.HR", num_scen, ".F", 1:length(BMT_FLEETSEGMENTS), sep="")  
         
          colnames(to_add) <-  paste("col", 1:6, sep="")  
          final_mat <- data.frame(rbind(final_mat, to_add))

        input_table_r6$fleet_segment <- BMT_FLEETSEGMENTS


         to_add <- data.frame(matrix(c("", "[% landings/catch]",	"[SRR model 1: Richer, 2: Shepherd, 3: Bevholt, 4: Segreg, 5: Geomean]",	"[Fsetting 1: Last, 2: Rescaled]",	"[SSR]",	"[Average years]"), nrow=1))
          colnames(to_add) <-  paste("col", 1:6, sep="")  
          final_mat <- data.frame(rbind(final_mat, to_add))
          
          input_table_r6_species_settings[,1] <- as.character(input_table_r6_species_settings[,1]) 
               input_table_r6_species_settings[,2] <- as.character(input_table_r6_species_settings[,2])
                    input_table_r6_species_settings[,3] <- as.character(input_table_r6_species_settings[,3])
                         input_table_r6_species_settings[,4] <- as.character(input_table_r6_species_settings[,4])
                            input_table_r6_species_settings[,5] <- as.character(input_table_r6_species_settings[,5])
                                  input_table_r6_species_settings[,6] <- as.character(input_table_r6_species_settings[,6])
          
          to_add <- input_table_r6_species_settings
          to_add$species <- paste("casestudy.HR", num_scen, ".S", 1:length(BMT_SPECIES), sep="")  
         
          colnames(to_add) <-  paste("col", 1:6, sep="")  
          final_mat <- data.frame(rbind(final_mat, to_add))

        input_table_r6_species_settings$species <- BMT_SPECIES

         if (scenario_numb == BMT_HR_CHANGE_TOTAL_FISHMORTALITY) {
         
mat_cfg_scenario_settings_fore_options <<- list(HR4.timespan = as.numeric(as.character(gtkEntryGetText(bmt_entry_timespan_r6))), HR4.reduction = as.numeric(as.character( gtkEntryGetText(bmt_entry_reduction_r6))) ,   HR4.fleet_reductions = input_table_r6, HR4.species_settings = input_table_r6_species_settings)

             }



} else if (num_scen == BMT_HR_STATUS_QUO ) {      # ----------------------------------------------------------------------------------------------------------
 to_add <- data.frame(matrix(c(paste("casestudy.HR",num_scen, sep=""),	"<NO INPUT REQUIRED>", "","","",""), nrow=1))
    colnames(to_add) <- paste("col", 1:6, sep="")
  final_mat <- data.frame(rbind(final_mat, to_add))
  
} else if (num_scen == BMT_HR_TAC_VARIATION ) {      # ----------------------------------------------------------------------------------------------------------
   to_add <- data.frame(matrix(c("", "[option TAC]",	"[species]",	"[option 3 - Abundance indices]",	"[option 3 - Previous TAC file]",""), nrow=1))
     colnames(to_add) <-  paste("col", 1:6, sep="")
    final_mat <- data.frame(rbind(final_mat, to_add))

    if (scenario_numb == BMT_HR_TAC_VARIATION) {
       spe_tac_ <- which(BMT_SPECIES == gtkComboBoxGetActiveText(bmt_combo_TAC_species_r7)) 
       opt_tac_ <- ifelse(gtkComboBoxGetActiveText(bmt_combo_optionTAC_r7) == "Option 1", 1, ifelse(gtkComboBoxGetActiveText(bmt_combo_optionTAC_r7) == "Option 2", 2, 3))
           
     if (opt_tac_ == 3) {
       name_this_scenario <- paste("HR", mat_cfg_scenario_settings_fore[2,2], "-", mat_cfg_scenario_settings_fore[2,3], sep="")
         pathTAC_1 <- paste(mat_cfg_general[1,3],  "/", name_this_scenario, "/", name_this_scenario, " - opt3 Abundance indices.csv", sep="")
          pathTAC_2 <- paste(mat_cfg_general[1,3],  "/", name_this_scenario, "/", name_this_scenario, " - opt3 Previous TACs.csv", sep="")  
     } else {
         pathTAC_1 <- ""
          pathTAC_2 <- ""
     }  

     to_add <- data.frame(matrix(c(paste("casestudy.HR",num_scen , sep=""),  opt_tac_,spe_tac_, pathTAC_1,pathTAC_2,""), nrow=1))
     } else {
        to_add <- data.frame(matrix(c(paste("casestudy.HR",num_scen , sep=""),  "", "" , "","",""), nrow=1))  
     }
          colnames(to_add) <-  paste("col", 1:6, sep="")    
          final_mat <- data.frame(rbind(final_mat, to_add))
          
          to_add <- data.frame(matrix(c("", "[% for the quota splitting]",	"",	"", "", ""), nrow=1))
          colnames(to_add) <-  paste("col", 1:6, sep="")  
          final_mat <- data.frame(rbind(final_mat, to_add))
          
          input_table_r7[,1] <- as.character(input_table_r7[,1]) 
               input_table_r7[,2] <- as.character(input_table_r7[,2])
          
          to_add <- data.frame(cbind(input_table_r7, matrix("", nrow=length(BMT_FLEETSEGMENTS), ncol=4)))
          to_add$fleet_segment <- paste("casestudy.HR", num_scen, ".F", 1:length(BMT_FLEETSEGMENTS), sep="")  
         
          colnames(to_add) <-  paste("col", 1:6, sep="")  
          final_mat <- data.frame(rbind(final_mat, to_add))

        input_table_r7$fleet_segment <- BMT_FLEETSEGMENTS
   if (scenario_numb == BMT_HR_TAC_VARIATION) {
  mat_cfg_scenario_settings_fore_options <<-  list(HR6.optionTAC = opt_tac_, HR6.species =  spe_tac_,  HR6.fleet_quotas = input_table_r7, HR6.abundance_indices = pathTAC_1, HR6.previous_tac= pathTAC_2) 
             }
  
} else if (num_scen == BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT ) {  # ----------------------------------------------------------------------------------------------------------

      to_add <- data.frame(matrix(c("", "[Monthly VESSELS file for FORECAST]",	"[Monthly DAYS.average file for FORECAST]", "","",""), nrow=1))
    colnames(to_add) <- paste("col", 1:6, sep="")
    final_mat <- data.frame(rbind(final_mat, to_add))
     
     if (scenario_numb == BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT) {
           save_effort_files_fore(NULL)
           mat_cfg_scenario_settings_fore_options <<- vector(mode="list", length=2)

     # name_this_scenario <- paste("HR", mat_cfg_scenario_settings_fore[2,2], "-", mat_cfg_scenario_settings_fore[2,3], sep="")
      #  vess_path_ <- paste(mat_cfg_general[1,3],  "/", name_this_scenario, "/", name_this_scenario, " - monthly data on number ", BMT_YEARS_FORECAST[1] , "-", BMT_YEARS_FORECAST[length(BMT_YEARS_FORECAST)] ,".csv", sep="")
      # days_path_  <- paste(mat_cfg_general[1,3],  "/", name_this_scenario, "/", name_this_scenario, " - monthly data on seadays ", BMT_YEARS_FORECAST[1] , "-", BMT_YEARS_FORECAST[length(BMT_YEARS_FORECAST)] ,".csv", sep="") 

        #  mat_cfg_scenario_settings_fore_options[[1]] <<- vess_path_
    #   mat_cfg_scenario_settings_fore_options[[2]]  <<-  days_path_ 
	   
	     vess_path_ <- paste(getwd(), "\\monthly data on number.csv", sep="")
       days_path_ <- paste(getwd(), "\\monthly data on seadays.csv", sep="")
        
          mat_cfg_scenario_settings_fore_options[[1]] <<- vess_path_
       mat_cfg_scenario_settings_fore_options[[2]]  <<-  days_path_ 
	   
     to_add <- data.frame(matrix(c(paste("casestudy.HR",num_scen , sep=""), vess_path_, days_path_   , "","","") , nrow=1)) 
         colnames(to_add) <-  paste("col", 1:6, sep="")
    final_mat <- data.frame(rbind(final_mat, to_add)) 
    } else {
     to_add <- data.frame(matrix(c(paste("casestudy.HR",num_scen , sep=""), "", "" , "","",""), nrow=1))
         colnames(to_add) <-  paste("col", 1:6, sep="")
    final_mat <- data.frame(rbind(final_mat, to_add))
    }


} else if (num_scen == BMT_HR_CHANGE_SELECTIVITY_BEHAVIOURAL ) {      # ----------------------------------------------------------------------------------------------------------
 to_add <- data.frame(matrix(c(paste("casestudy.HR",num_scen, sep=""),	"<NO INPUT REQUIRED>", "","","",""), nrow=1))
    colnames(to_add) <- paste("col", 1:6, sep="")
  final_mat <- data.frame(rbind(final_mat, to_add))
  
} else if (num_scen == BMT_HR_CHANGE_FISHEFFORT_BEHAVIOURAL ) {  # ----------------------------------------------------------------------------------------------------------
      to_add <- data.frame(matrix(c("", "[Monthly VESSELS file for FORECAST]",	"[Monthly DAYS.average file for FORECAST]", "","",""), nrow=1))
    colnames(to_add) <- paste("col", 1:6, sep="")
    final_mat <- data.frame(rbind(final_mat, to_add))
     
     if (scenario_numb == BMT_HR_CHANGE_FISHEFFORT_BEHAVIOURAL) {
     
           save_effort_files_fore(NULL)
     
   mat_cfg_scenario_settings_fore_options <<- vector(mode="list", length=2)    

   # name_this_scenario <- paste("HR", mat_cfg_scenario_settings_fore[2,2], "-", mat_cfg_scenario_settings_fore[2,3], sep="")

     #   vess_path_ <- paste(mat_cfg_general[1,3],  "/", name_this_scenario, "/", name_this_scenario, " - monthly data on number ", BMT_YEARS_FORECAST[1] , "-", BMT_YEARS_FORECAST[length(BMT_YEARS_FORECAST)] ,".csv", sep="")
    #   days_path_  <- paste(mat_cfg_general[1,3],  "/", name_this_scenario, "/", name_this_scenario, " - monthly data on seadays ", BMT_YEARS_FORECAST[1] , "-", BMT_YEARS_FORECAST[length(BMT_YEARS_FORECAST)] ,".csv", sep="") 

      #    mat_cfg_scenario_settings_fore_options[[1]] <<- vess_path_
     #  mat_cfg_scenario_settings_fore_options[[2]]  <<-  days_path_ 
	   
	     vess_path_ <- paste(getwd(), "\\monthly data on number.csv", sep="")
       days_path_ <- paste(getwd(), "\\monthly data on seadays.csv", sep="")
        
          mat_cfg_scenario_settings_fore_options[[1]] <<- vess_path_
       mat_cfg_scenario_settings_fore_options[[2]]  <<-  days_path_ 
	   
     to_add <- data.frame(matrix(c(paste("casestudy.HR",num_scen , sep=""), vess_path_, days_path_   , "","","") , nrow=1)) 
         colnames(to_add) <-  paste("col", 1:6, sep="")
    final_mat <- data.frame(rbind(final_mat, to_add)) 
    } else {
     to_add <- data.frame(matrix(c(paste("casestudy.HR",num_scen , sep=""), "", "" , "","",""), nrow=1))
         colnames(to_add) <-  paste("col", 1:6, sep="")
    final_mat <- data.frame(rbind(final_mat, to_add))
    }


} else if (num_scen == BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT_BEHAVIOURAL ) {  # ----------------------------------------------------------------------------------------------------------
      to_add <- data.frame(matrix(c("", "[Monthly VESSELS file for FORECAST]",	"[Monthly DAYS.average file for FORECAST]", "","",""), nrow=1))
    colnames(to_add) <- paste("col", 1:6, sep="")
    final_mat <- data.frame(rbind(final_mat, to_add))
     
     if (scenario_numb == BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT_BEHAVIOURAL) {
     
           save_effort_files_fore(NULL)
     
         mat_cfg_scenario_settings_fore_options <<- vector(mode="list", length=2)    

		 # name_this_scenario <- paste("HR", mat_cfg_scenario_settings_fore[2,2], "-", mat_cfg_scenario_settings_fore[2,3], sep="")

    #    vess_path_ <- paste(mat_cfg_general[1,3],  "/", name_this_scenario, "/", name_this_scenario, " - monthly data on number ", BMT_YEARS_FORECAST[1] , "-", BMT_YEARS_FORECAST[length(BMT_YEARS_FORECAST)] ,".csv", sep="")
    #   days_path_  <- paste(mat_cfg_general[1,3],  "/", name_this_scenario, "/", name_this_scenario, " - monthly data on seadays ", BMT_YEARS_FORECAST[1] , "-", BMT_YEARS_FORECAST[length(BMT_YEARS_FORECAST)] ,".csv", sep="") 

		     vess_path_ <- paste(getwd(), "\\monthly data on number.csv", sep="")
       days_path_ <- paste(getwd(), "\\monthly data on seadays.csv", sep="")
	
        mat_cfg_scenario_settings_fore_options[[1]] <<- vess_path_
       mat_cfg_scenario_settings_fore_options[[2]]  <<-  days_path_ 
	   
     to_add <- data.frame(matrix(c(paste("casestudy.HR",num_scen , sep=""), vess_path_, days_path_   , "","","") , nrow=1)) 
         colnames(to_add) <-  paste("col", 1:6, sep="")
    final_mat <- data.frame(rbind(final_mat, to_add)) 
    } else {
     to_add <- data.frame(matrix(c(paste("casestudy.HR",num_scen , sep=""), "", "" , "","",""), nrow=1))
         colnames(to_add) <-  paste("col", 1:6, sep="")
    final_mat <- data.frame(rbind(final_mat, to_add))
    }


} else if (num_scen == BMT_HR_STATUS_QUO_BEHAVIOURAL ) {      # ----------------------------------------------------------------------------------------------------------
 to_add <- data.frame(matrix(c(paste("casestudy.HR",num_scen, sep=""),	"<NO INPUT REQUIRED>", "","","",""), nrow=1))
    colnames(to_add) <- paste("col", 1:6, sep="")
  final_mat <- data.frame(rbind(final_mat, to_add))
  
}



}
       # ----------------------------------------------------------------------------------------------------------   MEYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY
  
    to_add <- data.frame(matrix(c("", "[MEY calculation]",	"[involved fleet segments]",	"[effort variable]",	"[number of years]", ""), nrow=1))
    colnames(to_add) <- paste("col", 1:6, sep="")
    final_mat <- data.frame(rbind(final_mat, to_add), stringsAsFactors=F)
     
    if (MEY_CALCULATION) {  
    mey_fleet_ <- gtkComboBoxGetActiveText(bmt_combo_fleetsegments_MEY_r8)
    eff_varia_mey_ <- ifelse(gtkToggleButtonGetActive(radio_scenario_r8_effort_vars_vessel), "VESSELS", "DAYS")     
mat_cfg_scenario_settings_fore_options <<- list(MEY.fleet = mey_fleet_ , MEY.effort_variable =  eff_varia_mey_, MEY.nb_years = gtkEntryGetText(bmt_entry_scenario_r8_years) )
    to_add <- data.frame(matrix(c("casestudy.MEY", MEY_CALCULATION,	mey_fleet_,	eff_varia_mey_,	 gtkEntryGetText(bmt_entry_scenario_r8_years) , ""), nrow=1))
       } else {
   to_add <- data.frame(matrix(c("casestudy.MEY", MEY_CALCULATION,	"",	"",	 "", ""), nrow=1), stringsAsFactors=F)  
       } 

    colnames(to_add) <- paste("col", 1:6, sep="")
    final_mat <- data.frame(rbind(final_mat, to_add), stringsAsFactors=F)
     
     to_add <- data.frame(bmt_effort_F, stringsAsFactors=F)
     to_add <- data.frame(rbind( c("", paste("[F", 1:length(BMT_FLEETSEGMENTS), "]", sep="") ), to_add), stringsAsFactors=F)
     to_add[,1] <-  c("", "casestudy.relatioship.effort-F.type", "casestudy.relatioship.effort-F.a", "casestudy.relatioship.effort-F.b")
		 if (ncol(final_mat) < ncol(to_add)) {
		      final_mat <- data.frame(cbind(final_mat, matrix("", nrow=nrow(final_mat), ncol=(ncol(to_add) - ncol(final_mat)))), stringsAsFactors=F)
		  colnames(final_mat) <- paste("col", 1:ncol(final_mat), sep="")
		  colnames(to_add) <- paste("col", 1:ncol(final_mat), sep="")
      final_mat <- data.frame(rbind(final_mat, to_add))
		  
		 } else if (ncol(final_mat) >= ncol(to_add)) {
		 
		  to_add <- data.frame(cbind(to_add, matrix("", nrow=nrow(to_add), ncol=(ncol(final_mat) - ncol(to_add)) )), stringsAsFactors=F)
		  colnames(to_add) <- c(paste("col", 1:ncol(final_mat), sep=""), rep("", (ncol(final_mat) - ncol(to_add))) )
		   final_mat <- data.frame(rbind(final_mat, to_add))
		 }


     
     
     # else if (ncol(final_mat) < ncol(to_add)) {
       	# colnames(to_add) <- c(paste("col", 1:ncol(final_mat), sep=""), rep("", (ncol(final_mat) - ncol(to_add))))
#         final_mat <- data.frame(rbind(final_mat, to_add)) 
     # }
	    	

print(final_mat)

}

return(final_mat)
}