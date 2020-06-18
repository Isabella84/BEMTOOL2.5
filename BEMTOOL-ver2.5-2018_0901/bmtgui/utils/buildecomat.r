# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


  # -------------------------------------------------------------------------------------------------------------------------------------------------------------
    # -------------------------------------------------------------------------------------------------------------------------------------------------------------
      # -------------------------------------------------------------------------------------------------------------------------------------------------------------

ncol_price <- ifelse(length(BMT_YEARS_FORECAST) <= (length(price_cfg_names)+1), (length(price_cfg_names)+1), (length(BMT_YEARS_FORECAST)+1))

mat_cfg_price  <<- data.frame(matrix("0", nrow=(4+ length(BMT_SPECIES)*length(BMT_FLEETSEGMENTS)+length(BMT_SPECIES)), ncol=ncol_price), stringsAsFactors=F)
mat_cfg_price[1,] <- c("", "[model]", rep("", ncol_price-2))
mat_cfg_price[2,] <- c("",1,rep("", ncol_price-2))
mat_cfg_price[3,] <- c("", price_cfg_names, rep("", ncol_price-5))

for (fl in 1:length(BMT_FLEETSEGMENTS) ) {
    for (sp in 1:length(BMT_SPECIES) ) {
        nrow_ <- (fl-1)*length(BMT_SPECIES)+3+sp  
         mat_cfg_price[nrow_,1] <- paste("casestudy.priceFun.F" ,fl ,".S", sp, sep="")   
    }
}
mat_cfg_price[2,1]  <- "casestudy.priceFun" 

#mat_cfg_price_ts <- data.frame(matrix("0", nrow=1+ length(BMT_SPECIES), ncol=ncol_price), stringsAsFactors=F)
mat_cfg_price[3+ length(BMT_SPECIES)*length(BMT_FLEETSEGMENTS)+1,] <- c("", paste("[", years.forecast, "]", sep=""))
mat_cfg_price[(3+ length(BMT_SPECIES)*length(BMT_FLEETSEGMENTS)+2): (3+ length(BMT_SPECIES)*length(BMT_FLEETSEGMENTS)+length(BMT_SPECIES)+1),1] <- c(paste("casestudy.import.S", 1:length(BMT_SPECIES), sep=""))


gtkComboBoxSetActive(bmt_combo_price_models,0)
bmt_reload_price_elast_landing_byfleet_table()
reload_price_costant_byfleet_landing() 
  bmt_reload_price_elast_import_table()  
  bmt_reload_price_elast_MW_table()     
  bmt_reload_price_importweight_table()
  
   gtkComboBoxSetActive(bmt_combo_price_models_discard, 0) 
   reload_price_elast_discard_byfleet()
   reload_price_costant_byfleet_discard() 
#  bmt_reload_price_correction_fact_table()
  
#print("------------------------------------------- mat_cfg_price")
#print(mat_cfg_price) 
#  # -------------------------------------------------------------------------------------------------------------------------------------------------------------
    # -------------------------------------------------------------------------------------------------------------------------------------------------------------
      # -------------------------------------------------------------------------------------------------------------------------------------------------------------
      
#mat_cfg_varCosts <<-  data.frame(matrix("", nrow=0, ncol=5), stringsAsFactors=F)
#mat_cfg_varCosts[1,] <- c("", "[fuel cost or fuel use coeff or total var cost coeff]", "[commercial cost coeff]", "[other var costs coeff]", "[ice cost coeff]")
#

ncol_varcosts <- ifelse(length(BMT_YEARS_FORECAST) <= (length(varcost_cfg_names)+1), (length(varcost_cfg_names)+1), (length(BMT_YEARS_FORECAST)+1))

mat_cfg_varCosts  <<- data.frame(matrix("0", nrow=(4+ length(BMT_FLEETSEGMENTS)+1), ncol=ncol_varcosts), stringsAsFactors=F)
mat_cfg_varCosts[1,] <- c("", "[model]", rep("", ncol_varcosts-2))
mat_cfg_varCosts[2,] <- c("",1,rep("", ncol_price-2))
mat_cfg_varCosts[3,] <- c("", varcost_cfg_names, rep("", ncol_price-5))

for (fl in 1:length(BMT_FLEETSEGMENTS) ) { 
         mat_cfg_varCosts[fl+3,1] <- paste("casestudy.varCostFun.F" ,fl , sep="")        
}

mat_cfg_varCosts[2,1]  <- "casestudy.varCostFun" 

if ((1+length(years.forecast)) < ncol_varcosts) {
      add_empty <-  rep("", ncol_varcosts-(1+length(years.forecast)))
} else {
      add_empty <- c()
}

#mat_cfg_price_ts <- data.frame(matrix("0", nrow=1+ length(BMT_SPECIES), ncol=ncol_price), stringsAsFactors=F)
mat_cfg_varCosts[3+length(BMT_FLEETSEGMENTS)+1,] <- c("", paste("[", years.forecast, "]", sep=""), add_empty)
mat_cfg_varCosts[(3+ length(BMT_FLEETSEGMENTS)+2),1] <- "casestudy.fuelprice"

   gtkComboBoxSetActive(bmt_combo_varcost_models, 0) 
      bmt_reload_cost_variable_table() 
      bmt_reload_cost_fuelprice_table()

#print("------------------------------------------- mat_cfg_varCosts")
#print(mat_cfg_varCosts)
#

# -------------------------------------------------------------------------------------------------------------------------------------------------------------
    # -------------------------------------------------------------------------------------------------------------------------------------------------------------
      # -------------------------------------------------------------------------------------------------------------------------------------------------------------

mat_cfg_labCosts  <<- data.frame(matrix("0", nrow=(2+ length(BMT_FLEETSEGMENTS)+1), ncol=(length(labcost_cfg_names)+1)), stringsAsFactors=F)
mat_cfg_labCosts[1,] <- c("", "[model]", "","","","","")
mat_cfg_labCosts[2,] <- c("",1,"","","","","")
mat_cfg_labCosts[3,] <- c("", labcost_cfg_names)

for (fl in 1:length(BMT_FLEETSEGMENTS) ) { 
         mat_cfg_labCosts[fl+3,1] <- paste("casestudy.labCostFun.F" ,fl , sep="")        
}

mat_cfg_labCosts[2,1]  <- "casestudy.labCostFun" 

       bmt_reload_labour_fuel_table()
       bmt_reload_labour_commercial_table()
       bmt_reload_labour_others_table()
#       bmt_reload_labour_sorting_table()
       bmt_reload_cost_crew_minwage_table()

#print("------------------------------------------- mat_cfg_labCosts")
#print(mat_cfg_labCosts)
#
  # -------------------------------------------------------------------------------------------------------------------------------------------------------------
    # -------------------------------------------------------------------------------------------------------------------------------------------------------------
      # -------------------------------------------------------------------------------------------------------------------------------------------------------------

mat_cfg_fixCosts  <<- data.frame(matrix("0", nrow=(2+ length(BMT_FLEETSEGMENTS)+1), ncol=(length(fixcost_cfg_names)+1)), stringsAsFactors=F)
mat_cfg_fixCosts[1,] <- c("", "[model]", "","")
mat_cfg_fixCosts[2,] <- c("",1,"","")
mat_cfg_fixCosts[3,] <- c("", fixcost_cfg_names)

for (fl in 1:length(BMT_FLEETSEGMENTS) ) { 
         mat_cfg_fixCosts[fl+3,1] <- paste("casestudy.fixCostFun.F" ,fl , sep="")        
}

mat_cfg_fixCosts[2,1]  <- "casestudy.fixCostFun" 

     gtkComboBoxSetActive(bmt_combo_fixedcost_models, 0)
      bmt_reload_cost_fixed_table()      

#print("------------------------------------------- mat_cfg_fixCosts")
#print(mat_cfg_fixCosts )
#    
# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------------------

mat_cfg_capCosts  <<- data.frame(matrix("0", nrow=(2+ length(BMT_FLEETSEGMENTS)+1), ncol=(length(capcost_cfg_names)+1)), stringsAsFactors=F)
mat_cfg_capCosts[1,] <- c("", "[model]", "")
mat_cfg_capCosts[2,] <- c("",1,"")
mat_cfg_capCosts[3,] <- c("", capcost_cfg_names)

for (fl in 1:length(BMT_FLEETSEGMENTS) ) { 
         mat_cfg_capCosts[fl+3,1] <- paste("casestudy.capCostFun.F" ,fl , sep="")        
}

mat_cfg_capCosts[2,1]  <- "casestudy.capCostFun" 

gtkComboBoxSetActive(bmt_combo_capitalcost_models, 0)
bmt_reload_cost_capital_table()

#print("------------------------------------------- mat_cfg_capCosts")
#print(mat_cfg_capCosts)
#

# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------------------

mat_cfg_FleetDyn  <<- data.frame(matrix("0", nrow=(2+ length(BMT_FLEETSEGMENTS)+1), ncol=(length(fleedyn_cfg_names)+1)), stringsAsFactors=F)
mat_cfg_FleetDyn[1,] <- c("", "[model]", "", "")
mat_cfg_FleetDyn[2,] <- c("",1,"", "")
mat_cfg_FleetDyn[3,] <- c("", fleedyn_cfg_names)

for (fl in 1:length(BMT_FLEETSEGMENTS) ) { 
         mat_cfg_FleetDyn[fl+3,1] <- paste("casestudy.fleDynFun.F" ,fl , sep="")        
}

mat_cfg_FleetDyn[2,1]  <- "casestudy.fleDynFun" 

bmt_reload_behav_dyn_table()

#print("------------------------------------------- mat_cfg_FleetDyn")
#print(mat_cfg_FleetDyn)
#
# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------------------

mat_cfg_FleetAct  <<- data.frame(matrix("0", nrow=(2+ length(BMT_FLEETSEGMENTS)+1), ncol=(length(fleeact_cfg_names)+1)), stringsAsFactors=F)
mat_cfg_FleetAct[1,] <- c("", "[model]", "", "")
mat_cfg_FleetAct[2,] <- c("",1,"", "")
mat_cfg_FleetAct[3,] <- c("", fleeact_cfg_names)

for (fl in 1:length(BMT_FLEETSEGMENTS) ) { 
         mat_cfg_FleetAct[fl+3,1] <- paste("casestudy.actDynFun.F" ,fl , sep="")        
}

mat_cfg_FleetAct[2,1]  <- "casestudy.actDynFun" 

bmt_reload_behav_act_table()

#print("------------------------------------------- mat_cfg_FleetAct")
#print(mat_cfg_FleetAct)
#
# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------------------

mat_cfg_TechProgress  <<- data.frame(matrix("0", nrow=(2+ length(BMT_FLEETSEGMENTS)+1), ncol=(length(techprog_cfg_names)+1)), stringsAsFactors=F)
mat_cfg_TechProgress[1,] <- c("", "[model]", "", "")
mat_cfg_TechProgress[2,] <- c("",1,"", "")
mat_cfg_TechProgress[3,] <- c("", techprog_cfg_names)

for (fl in 1:length(BMT_FLEETSEGMENTS) ) { 
         mat_cfg_TechProgress[fl+3,1] <- paste("casestudy.tecProgFun.F" ,fl , sep="")        
}

mat_cfg_TechProgress[2,1]  <- "casestudy.tecProgFun" 

bmt_reload_behav_progr_table()
       
#print("------------------------------------------- mat_cfg_TechProgress")
#print(mat_cfg_TechProgress)
#

# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------------------

ncol_indic <- ifelse(length(BMT_YEARS_FORECAST) <= (length(indicator_cfg_names)+1), (length(indicator_cfg_names)+1), (length(BMT_YEARS_FORECAST)+1))

mat_cfg_EconomicIndicator  <<- data.frame(matrix("0", nrow=(7+ length(BMT_FLEETSEGMENTS)*5), ncol=ncol_indic), stringsAsFactors=F)
mat_cfg_EconomicIndicator[1,] <- c("", "[model]",  rep("", ncol_indic-2))
mat_cfg_EconomicIndicator[2,] <- c("",5,rep("", ncol_indic-2))
mat_cfg_EconomicIndicator[3,] <- c("", indicator_cfg_names, rep("", ncol_indic-1 - length(indicator_cfg_names)))

if ((1+length(years.forecast)) < ncol_indic) {
      add_empty <-  rep("", ncol_indic-(1+length(years.forecast)))
} else {
      add_empty <- c()
}

for (fl in 1:length(BMT_FLEETSEGMENTS) ) { 
         mat_cfg_EconomicIndicator[fl+3,1] <- paste("casestudy.econIndFun.F" ,fl , sep="")        
}

         mat_cfg_EconomicIndicator[fl+4,] <- c("", paste("[", years.forecast, "]", sep=""), add_empty)       

for (fl in 1:length(BMT_FLEETSEGMENTS) ) { 
         mat_cfg_EconomicIndicator[length(BMT_FLEETSEGMENTS)+fl+4,1] <- paste("casestudy.addincome.F" ,fl , sep="")        
}

mat_cfg_EconomicIndicator[length(BMT_FLEETSEGMENTS)*2+5,]  <- c("", paste("[", years.forecast, "]", sep=""), add_empty)  

for (fl in 1:length(BMT_FLEETSEGMENTS) ) { 
         mat_cfg_EconomicIndicator[length(BMT_FLEETSEGMENTS)*2+fl+5,1] <- paste("casestudy.addtaxes.F" ,fl , sep="")        
}

 
mat_cfg_EconomicIndicator[length(BMT_FLEETSEGMENTS)*3+6,]  <- c("", paste("[", years.forecast, "]", sep=""), add_empty)  

for (fl in 1:length(BMT_FLEETSEGMENTS) ) { 
         mat_cfg_EconomicIndicator[length(BMT_FLEETSEGMENTS)*3+fl+6,1] <- paste("casestudy.newequipment.F" ,fl , sep="")        
}

mat_cfg_EconomicIndicator[length(BMT_FLEETSEGMENTS)*4+7,]  <- c("", paste("[", years.forecast, "]", sep=""), add_empty) 

for (fl in 1:length(BMT_FLEETSEGMENTS) ) { 
         mat_cfg_EconomicIndicator[length(BMT_FLEETSEGMENTS)*4+fl+7,1] <- paste("casestudy.discardCost.F" ,fl , sep="")        
}

          bmt_reload_ecoind_table()
             bmt_reload_indic_taxes_table()
                   bmt_reload_indic_subsidies_table()
                        bmt_reload_cost_equipment()
                                     bmt_reload_discard_cost_table()

#print("------------------------------------------- mat_cfg_EconomicIndicator")
#print(mat_cfg_EconomicIndicator)