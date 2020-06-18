# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




loadScenariocfg <- function(path) {
	# path = "C:\\BEMTOOL-ver2.0.7-2015_20102015_ultima\\bmtconfig_PARAMETRIZZAZIONE_forecast.csv"
 all_CFG_mat <- try(data.frame(read.csv(path, sep=";",  header=F, na.strings = ""), stringsAsFactors=F))
 
EFFORT_NUMBER_list_fore <<- list()
EFFORT_DAY_list_fore <<- list()
    #scenario_cfg <- c(1:(23+length(BMT_SPECIES)+ length(BMT_FLEETSEGMENTS)*3))

     mat_cfg_scenario_settings_fore <<- data.frame(all_CFG_mat[1:2,1:3])
    casestudy_name <<- paste("HR", as.character(mat_cfg_scenario_settings_fore[2,2]), "-", as.character(mat_cfg_scenario_settings_fore[2,3]), sep="")  
    
    mat_cfg_MEY <<-  data.frame(all_CFG_mat[which(all_CFG_mat[,1] == "casestudy.MEY"),1:5] ) 
    colnames(mat_cfg_MEY)[1] <- ""
    colnames(mat_cfg_MEY)[2] <- as.character(all_CFG_mat[which(all_CFG_mat[,1] == "casestudy.MEY")-1,2]) 
    colnames(mat_cfg_MEY)[3] <- as.character(all_CFG_mat[which(all_CFG_mat[,1] == "casestudy.MEY")-1,3]) 
    colnames(mat_cfg_MEY)[4] <- as.character(all_CFG_mat[which(all_CFG_mat[,1] == "casestudy.MEY")-1,4]) 
    colnames(mat_cfg_MEY)[5] <- as.character(all_CFG_mat[which(all_CFG_mat[,1] == "casestudy.MEY")-1,5]) 
                
    # print(mat_cfg_MEY)
    
     BMT_SCENARIO <<- as.numeric(as.character(mat_cfg_scenario_settings_fore[2,2])) 
     MEY_CALCULATION <<- as.logical(mat_cfg_MEY[1,2])
     
                   
     #  scen_mat <- all_CFG_mat[3:(30+3*length(BMT_FLEETSEGMENTS)+length(BMT_SPECIES)),]
        scen_mat <- all_CFG_mat[3:( which(all_CFG_mat[,1] == "casestudy.priceFun")-2),]   
     mat_cfg_scenario_settings_fore_options <<- getScenarioOptions(scen_mat)  



     # 같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같 economic parameters
                              

   # econ_mat <- all_CFG_mat[(31+3*length(BMT_FLEETSEGMENTS)+length(BMT_SPECIES)):nrow(all_CFG_mat),]
   econ_mat <- all_CFG_mat[( which(all_CFG_mat[,1] == "casestudy.priceFun")-1):nrow(all_CFG_mat),]

  #   all_CFG_mat <- all_CFG_mat[(31+3*length(BMT_FLEETSEGMENTS)+length(BMT_SPECIES)):nrow(all_CFG_mat),]
  all_CFG_mat <- all_CFG_mat[( which(all_CFG_mat[,1] == "casestudy.priceFun")-1):nrow(all_CFG_mat),]
  
    ncol_price <- ifelse(length(BMT_YEARS_FORECAST) <= (length(price_cfg_names)+1), (length(price_cfg_names)+1), (length(BMT_YEARS_FORECAST)+1))

    
  #   mat_cfg_price <<- data.frame(all_CFG_mat[1:(length(BMT_SPECIES)*length(BMT_FLEETSEGMENTS)+4+length(BMT_SPECIES)), 1:ncol_price] )
      mat_cfg_price <<- all_CFG_mat[1:which(all_CFG_mat[,1] == paste("casestudy.import.S", length(BMT_SPECIES), sep="") ), 1:ncol_price] 

#        print(mat_cfg_price)

    all_CFG_mat <- all_CFG_mat[(nrow(mat_cfg_price)+1):nrow(all_CFG_mat),]
     ncol_varcost <- ifelse(length(BMT_YEARS_FORECAST) <= (length(varcost_cfg_names)+1), (length(varcost_cfg_names)+1), (length(BMT_YEARS_FORECAST)+1))
     
    mat_cfg_varCosts <<- all_CFG_mat[1:(length(BMT_FLEETSEGMENTS)+5), 1:ncol_varcost] 

#    print(mat_cfg_varCosts)

    all_CFG_mat <- all_CFG_mat[(nrow(mat_cfg_varCosts)+1):nrow(all_CFG_mat),]
    mat_cfg_labCosts <<- data.frame(all_CFG_mat[1:(length(BMT_FLEETSEGMENTS)+3), 1:(length(labcost_cfg_names)+1)] , stringsAsFactors=F)

#    print(mat_cfg_labCosts)

        all_CFG_mat <- all_CFG_mat[(nrow(mat_cfg_labCosts)+1):nrow(all_CFG_mat),]
    mat_cfg_fixCosts <<- data.frame(all_CFG_mat[1:(length(BMT_FLEETSEGMENTS)+3), 1:(length(fixcost_cfg_names)+1)] , stringsAsFactors=F)

#    print(mat_cfg_fixCosts)

     all_CFG_mat <- all_CFG_mat[(nrow(mat_cfg_labCosts)+1):nrow(all_CFG_mat),]
    mat_cfg_capCosts <<- data.frame(all_CFG_mat[1:(length(BMT_FLEETSEGMENTS)+3), 1:(length(capcost_cfg_names)+1)] , stringsAsFactors=F)

#         print(mat_cfg_capCosts)

    all_CFG_mat <- all_CFG_mat[(nrow(mat_cfg_capCosts)+1):nrow(all_CFG_mat),]
    mat_cfg_FleetDyn <<- data.frame(all_CFG_mat[1:(length(BMT_FLEETSEGMENTS)+3), 1:(length(fleedyn_cfg_names)+1)] , stringsAsFactors=F)

#     print(mat_cfg_FleetDyn)

   all_CFG_mat <- all_CFG_mat[(nrow(mat_cfg_FleetDyn)+1):nrow(all_CFG_mat),]
    mat_cfg_FleetAct <<- data.frame(all_CFG_mat[1:(length(BMT_FLEETSEGMENTS)+3), 1:(length(fleeact_cfg_names)+1)] , stringsAsFactors=F)

#     print(mat_cfg_FleetAct)

       all_CFG_mat <- all_CFG_mat[(nrow(mat_cfg_FleetAct)+1):nrow(all_CFG_mat),]
    mat_cfg_TechProgress <<- data.frame(all_CFG_mat[1:(length(BMT_FLEETSEGMENTS)+3), 1:(length(techprog_cfg_names)+1)] , stringsAsFactors=F)

#     print(mat_cfg_TechProgress)

        all_CFG_mat <- all_CFG_mat[(nrow(mat_cfg_TechProgress)+1):nrow(all_CFG_mat),]
        ncol_indicators <- ifelse(length(BMT_YEARS_FORECAST) <= (length(indicator_cfg_names)+1), (length(indicator_cfg_names)+1), (length(BMT_YEARS_FORECAST)+1))
    mat_cfg_EconomicIndicator <<- data.frame(all_CFG_mat[1:(nrow(all_CFG_mat)-4), 1:ncol_indicators] , stringsAsFactors=F)

    mat_cfg_EconomicIndicator <<- data.frame(all_CFG_mat[1:nrow(all_CFG_mat), 1:ncol_indicators] , stringsAsFactors=F)
   
           gtkComboBoxSetActive(bmt_combo_price_models, -1)
      gtkComboBoxSetActive(bmt_combo_price_models, (which(PRICE_MODELS$model_name == mat_cfg_price[2,2])-1))

        gtkComboBoxSetActive(bmt_combo_price_models_discard, -1)
      gtkComboBoxSetActive(bmt_combo_price_models_discard, (which(PRICE_MODELS_DISCARD$model_name == mat_cfg_price[2,3])-1))


    gtkComboBoxSetActive(bmt_combo_varcost_models, -1)
     gtkComboBoxSetActive(bmt_combo_varcost_models, (which(VARCOST_MODELS$model_name == mat_cfg_varCosts[2,2])-1))
#      bmt_reload_cost_variable_table()
    
        gtkComboBoxSetActive(bmt_combo_fixedcost_models, -1)  
     gtkComboBoxSetActive(bmt_combo_fixedcost_models, (which(FIXEDCOST_MODELS$model_name == mat_cfg_fixCosts[2,2])-1))
#      bmt_reload_cost_fixed_table()
        
         gtkComboBoxSetActive(bmt_combo_capitalcost_models, -1)  
     gtkComboBoxSetActive(bmt_combo_capitalcost_models, (which(CAPITALCOST_MODELS$model_name == mat_cfg_capCosts[2,2])-1))
#      bmt_reload_cost_capital_table()
  
     
      matrix_temp <-   mat_cfg_labCosts[4:(length(BMT_FLEETSEGMENTS)+3), ]
    cost_crew_minwage_matrix <- data.frame(matrix(0, nrow=length(LABOURCOST_head), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(cost_crew_minwage_matrix) <-  c(" ",BMT_FLEETSEGMENTS)
     cost_crew_minwage_matrix[,1] <- LABOURCOST_head
     


      labour_fuel_matrix <- data.frame(matrix(FALSE, nrow=1, ncol=length(BMT_FLEETSEGMENTS)))
   colnames(labour_fuel_matrix) <-  c(BMT_FLEETSEGMENTS)
#     labour_fuel_matrix[1,1] <- c(" fuel costs in labour costs dynamic ")



           labour_commercial_matrix <- data.frame(matrix(FALSE, nrow=1, ncol=length(BMT_FLEETSEGMENTS)))
   colnames(labour_commercial_matrix) <-  c(BMT_FLEETSEGMENTS)
#     labour_commercial_matrix[1,1] <- c(" commercial costs in labour costs dynamic ") 



           labour_others_matrix <- data.frame(matrix(FALSE, nrow=1, ncol=length(BMT_FLEETSEGMENTS)))
   colnames(labour_others_matrix) <-  c(BMT_FLEETSEGMENTS)
#     labour_others_matrix[1,1] <- c(" other variable costs in labour costs dynamic ")

    labour_sorting_matrix <- data.frame(matrix(0, nrow=1, ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(labour_sorting_matrix) <-  c(" ",BMT_FLEETSEGMENTS)
     labour_sorting_matrix[1,1] <- " sorting coefficient "
     
 
for (flee in 1:length(BMT_FLEETSEGMENTS)) {
       cost_crew_minwage_matrix[1, flee+1] <- as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.labCostFun.F", flee, sep="") ,2] ))
         cost_crew_minwage_matrix[2, flee+1] <- as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.labCostFun.F", flee, sep="") ,6] ))

       labour_sorting_matrix[1, flee+1] <- as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.labCostFun.F", flee, sep="") ,7] ))

        labour_fuel_matrix[1, flee] <- as.logical(as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.labCostFun.F", flee, sep="") ,3])) )
                labour_commercial_matrix[1, flee] <- as.logical(as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.labCostFun.F", flee, sep="") ,4])) )
                        labour_others_matrix[1, flee] <- as.logical(as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.labCostFun.F", flee, sep="") ,5])) )
}

   gtkToggleButtonSetActive(bmt_chk_economic_labour_sorting_coeff, T)
   
if (all(is.na(labour_sorting_matrix))) {
          gtkToggleButtonSetActive(radio_sorting_equalToDiscardRate, T)
}  else {    
          gtkToggleButtonSetActive(radio_sorting_fromVector, T)
}

#              print(cost_crew_minwage_matrix)  
#              print(labour_fuel_matrix)
#            print(labour_commercial_matrix)    
#       print(labour_others_matrix)  
#       print(labour_sorting_matrix)
#
  bmt_fleet.cost_crew_minwage <<- cost_crew_minwage_matrix
       bmt_reload_cost_crew_minwage_table()
         bmt_fleet.labour_fuel <<- labour_fuel_matrix
       bmt_reload_labour_fuel_table()
      bmt_fleet.labour_commercial <<- labour_commercial_matrix
       bmt_reload_labour_commercial_table()
      bmt_fleet.labour_others <<- labour_others_matrix
       bmt_reload_labour_others_table()
             bmt_fleet.labour_sorting <<- labour_sorting_matrix
       bmt_reload_labour_sorting_table()


			 if (mat_cfg_FleetDyn[2,2] == 1) {
			    gtkToggleButtonSetActive(chk_behav_dyn, T)
			 }  else {
			       gtkToggleButtonSetActive(chk_behav_dyn, F)
			 }

 matrix_temp <-   mat_cfg_FleetDyn[4:(length(BMT_FLEETSEGMENTS)+3), ]
    dyn_matrix <- data.frame(matrix(0, nrow=length(BEHAV_DYN_head), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(dyn_matrix) <-  c(" ",BMT_FLEETSEGMENTS)
     dyn_matrix[,1] <- BEHAV_DYN_head
for (flee in 1:length(BMT_FLEETSEGMENTS)) {
       dyn_matrix[1, flee+1] <- as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.fleDynFun.F", flee, sep="") ,2] ))
         dyn_matrix[2, flee+1] <- as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.fleDynFun.F", flee, sep="") ,3] ))
         dyn_matrix[3, flee+1] <- as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.fleDynFun.F", flee, sep="") ,4] ))
}

#print(dyn_matrix)

  bmt_fleet.behav_dyn <<- dyn_matrix
       bmt_reload_behav_dyn_table()


      if (mat_cfg_FleetAct[2,2] == 1) {
			    gtkToggleButtonSetActive(chk_behav_act, T)
			 }  else {
			    gtkToggleButtonSetActive(chk_behav_act, F)
			 }

        matrix_temp <-   mat_cfg_FleetAct[4:(length(BMT_FLEETSEGMENTS)+3), ]
    act_matrix <- data.frame(matrix(0, nrow=length(BEHAV_ACT_head), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(act_matrix) <-  c(" ",BMT_FLEETSEGMENTS)
     act_matrix[,1] <- BEHAV_ACT_head
for (flee in 1:length(BMT_FLEETSEGMENTS)) {
       act_matrix[1, flee+1] <- as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.actDynFun.F", flee, sep="") ,2] ))
         act_matrix[2, flee+1] <- as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.actDynFun.F", flee, sep="") ,3] ))
         act_matrix[3, flee+1] <- as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.actDynFun.F", flee, sep="") ,4] ))
}
  bmt_fleet.behav_act <<- act_matrix
       bmt_reload_behav_act_table()
       
#       print(act_matrix)


      if (mat_cfg_TechProgress[2,2] == 1) {
			    gtkToggleButtonSetActive(chk_behav_progr, T)
			 }  else {
			    gtkToggleButtonSetActive(chk_behav_progr, F)
			 }


          matrix_temp <-   mat_cfg_TechProgress[4:(length(BMT_FLEETSEGMENTS)+3), ]
    progr_matrix <- data.frame(matrix(0, nrow=length(BEHAV_PROGR_head), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(progr_matrix) <-  c(" ",BMT_FLEETSEGMENTS)
     progr_matrix[,1] <- BEHAV_PROGR_head
for (flee in 1:length(BMT_FLEETSEGMENTS)) {
       progr_matrix[1, flee+1] <- as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.tecProgFun.F", flee, sep="") ,2] ))
         progr_matrix[2, flee+1] <- as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.tecProgFun.F", flee, sep="") ,3] ))
         progr_matrix[3, flee+1] <- as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.tecProgFun.F", flee, sep="") ,4] ))
}
  bmt_fleet.behav_progr <<- progr_matrix
       bmt_reload_behav_progr_table()
       
#       print(progr_matrix)


    matrix_temp <-   mat_cfg_EconomicIndicator[4:(length(BMT_FLEETSEGMENTS)+3), ]
    ecoind_matrix <- data.frame(matrix(0, nrow=length(ECOIND_head), ncol=(length(BMT_FLEETSEGMENTS)+1)))
   colnames(ecoind_matrix) <-  c(" ",BMT_FLEETSEGMENTS)
     ecoind_matrix[,1] <- ECOIND_head
for (flee in 1:length(BMT_FLEETSEGMENTS)) {
      for (indi in 1:length(ECOIND_head)) {
       ecoind_matrix[indi, flee+1] <- as.numeric(as.character(matrix_temp[as.character(matrix_temp[,1]) == paste("casestudy.econIndFun.F", flee, sep="") ,(indi+1)] ))
}
}
  bmt_fleet.ecoind <<- ecoind_matrix
       bmt_reload_ecoind_table()
                                                          
#    print(ecoind_matrix)
																									
    matrix_temp <-   mat_cfg_EconomicIndicator[	(which(as.character(mat_cfg_EconomicIndicator[,1]) == "casestudy.addtaxes.F1")-1):(which(as.character(mat_cfg_EconomicIndicator[,1]) == "casestudy.addtaxes.F1")+(length(BMT_FLEETSEGMENTS))-1), ]
    indic_taxes_matrix <- data.frame(matrix(0, nrow=length(BMT_FLEETSEGMENTS), ncol=(length(BMT_YEARS_FORECAST)+1)))
   colnames(indic_taxes_matrix) <-  c("FleetSegment",BMT_YEARS_FORECAST)
     indic_taxes_matrix$FleetSegment <- paste(" ", BMT_FLEETSEGMENTS, " ", sep="")
for (flee in 1:length(BMT_FLEETSEGMENTS)) {
      for (ye in 1:length(BMT_YEARS_FORECAST)) {
       indic_taxes_matrix[flee, ye+1] <- as.numeric(as.character(matrix_temp[which(as.character(matrix_temp[,1]) == paste("casestudy.addtaxes.F", flee, sep="") ),(ye+1)] ))
}
}
  bmt_fleet.indic_taxes <<- indic_taxes_matrix
       bmt_reload_indic_taxes_table()

#       print( indic_taxes_matrix)

    matrix_temp <-  mat_cfg_EconomicIndicator[	(which(as.character(mat_cfg_EconomicIndicator[,1]) == "casestudy.addincome.F1")):(which(as.character(mat_cfg_EconomicIndicator[,1]) == "casestudy.addincome.F1")+(length(BMT_FLEETSEGMENTS))-1), ]
      indic_subsidies_matrix <- data.frame(matrix(0, nrow=length(BMT_FLEETSEGMENTS), ncol=(length(BMT_YEARS_FORECAST)+1)))  
   colnames(indic_subsidies_matrix) <-  c("FleetSegment",BMT_YEARS_FORECAST)
     indic_subsidies_matrix$FleetSegment <- paste(" ", BMT_FLEETSEGMENTS, " ", sep="")
for (flee in 1:length(BMT_FLEETSEGMENTS)) {
      for (ye in 1:length(BMT_YEARS_FORECAST)) {
       indic_subsidies_matrix[flee, ye+1] <- as.numeric(as.character(matrix_temp[which(as.character(matrix_temp[,1]) == paste("casestudy.addincome.F", flee, sep="")) ,(ye+1)] ))
}
}
  bmt_fleet.indic_subsidies <<- indic_subsidies_matrix
       bmt_reload_indic_subsidies_table()
 
 
  matrix_temp <-   mat_cfg_EconomicIndicator[	(which(as.character(mat_cfg_EconomicIndicator[,1]) == "casestudy.newequipment.F1")-1):(which(as.character(mat_cfg_EconomicIndicator[,1]) == "casestudy.newequipment.F1")+(length(BMT_FLEETSEGMENTS))-1), ]
    newequi_matrix <- data.frame(matrix(0, nrow=length(BMT_FLEETSEGMENTS), ncol=(length(BMT_YEARS_FORECAST)+1)))
   colnames(newequi_matrix) <-  c("FleetSegment",BMT_YEARS_FORECAST)
     newequi_matrix$FleetSegment <- paste(" ", BMT_FLEETSEGMENTS, " ", sep="")
for (flee in 1:length(BMT_FLEETSEGMENTS)) {
      for (ye in 1:length(BMT_YEARS_FORECAST)) {
       newequi_matrix[flee, ye+1] <- as.numeric(as.character(matrix_temp[which(as.character(matrix_temp[,1]) == paste("casestudy.newequipment.F", flee, sep="") ),(ye+1)] ))
}
}
  bmt_fleet.cost_equipment <<- indic_taxes_matrix
       bmt_reload_cost_equipment()
       
#    print( indic_subsidies_matrix)
                
#  bmt_wnd_load_ecoparams$destroy()
#gtkWidgetSetSensitive(BMTmain_window, T)   
			 if (!is.null( mat_cfg_scenario_settings_fore_options$effort_F ) ) {
			   bmt_effort_F <<- mat_cfg_scenario_settings_fore_options$effort_F[2:4,]
			  # print(bmt_effort_F)
				 bmt_effort_F[,1] <- c("Type of relationship", "coeff a", "coeff b") 
 				  reload_effort_F()
			 }





name_this_scenario <-   paste("HR", as.character(mat_cfg_scenario_settings_fore[2,2]), "-", as.character(mat_cfg_scenario_settings_fore[2,3]), sep="")



} 