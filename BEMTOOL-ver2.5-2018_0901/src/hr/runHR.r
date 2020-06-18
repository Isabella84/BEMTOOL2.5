# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





# #############################################################################################
# BEGIN OF SCENARIO
# #############################################################################################

cat("\n\n")
print("***************************************************************************", quote=FALSE)
print(paste("SCENARIO n°", BMT_SCENARIO), quote=FALSE)
print("***************************************************************************", quote=FALSE)
cat("\n\n")



# for the non-integrated approach ( first all the years in the biological forecast, then all the years in the economic forecast 

if (BMT_SCENARIO %in% c(BMT_HR_CHANGE_SELECTIVITY, BMT_HR_STATUS_QUO, BMT_HR_CHANGE_FISHEFFORT, BMT_HR_CHANGE_FISHMORTALITY, BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT) ) {  # HR 1, 2, 3, 5, 7      OK
        INTEGRATED_APPROACH <<- FALSE
        
#if (BMT_SCENARIO == BMT_HR_CHANGE_FISHEFFORT | BMT_SCENARIO == BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT) {  # HR 2
##      if (!MEY_CALCULATION) {
##              readline(prompt = "Please insert Effort variable in the EffortGUI file and press any digit to run the FORECAST >> ")
##      # readline(prompt = "Change the effort variables by month in the Effort_data.csv for the FORECAST. Press any key to continue >> ")
##      }       
#}

if (BMT_SCENARIO != BMT_HR_CHANGE_FISHMORTALITY)  {   # all scenarios except BMT_HR_CHANGE_FISHMORTALITY       # != HR 3

        Fleetyear <<- setEffortVarsForecast(Fleetyear)  
        source(paste(getwd(), "/src/biol/runBiol.fore.r", sep=""))
        
    } else {    # equal to BMT_HR_CHANGE_FISHMORTALITY             # HR 3
    
         source(paste(getwd(), "/src/biol/runBiol.fore.r", sep="")) 
                  
#       source(paste(getwd(), "/src/biol/bmtALADYM/save_mortalities_change.r", sep=""))                          
#       Fleetyear <<- setEffortVarsForecast(Fleetyear)  
       
    }    
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################    
    
} else if (BMT_SCENARIO == BMT_HR_CHANGE_TOTAL_FISHMORTALITY) {  # HR 4     
      INTEGRATED_APPROACH <<- FALSE

       source(paste(getwd(), "/src/biol/runBiol.fore.r", sep=""))                           
       Fleetyear <<- setEffortVarsForecast(Fleetyear)  
       res_landSplit <<- BMTLandsplit(Fleetyear, Interactionsyear)      
        Fleetyear <<- res_landSplit[[1]]
        Interactionsyear <<- res_landSplit[[2]]
       
        for (ye_f in 1:foreperiod) {
         yy <- ye_f + simperiod
        Fleetyear <<- runEcon.fore(Fleetyear, yy)
        }

########################################################################################################################################
########################################################################################################################################
########################################################################################################################################    

} else if (BMT_SCENARIO == BMT_HR_TAC_VARIATION) {   # HR 6
        INTEGRATED_APPROACH <<- TRUE
          
        TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR <<-  simperiod  + 1
          
        Fleetyear <<- setEffortVarsForecast(Fleetyear)
          
        source(paste(getwd(), "/src/biol/runBiol.fore.r", sep=""))

########################################################################################################################################
########################################################################################################################################
########################################################################################################################################    
                   # BMT_SCENARIO %in% c(BMT_HR_CHANGE_SELECTIVITY, BMT_HR_STATUS_QUO, BMT_HR_CHANGE_FISHEFFORT, BMT_HR_CHANGE_FISHMORTALITY, BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT
} else if (BMT_SCENARIO %in% c(BMT_HR_CHANGE_SELECTIVITY_BEHAVIOURAL, BMT_HR_CHANGE_FISHEFFORT_BEHAVIOURAL, BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT_BEHAVIOURAL, BMT_HR_STATUS_QUO_BEHAVIOURAL ) ) {  #8, 9, 10, 11    OK
        
    INTEGRATED_APPROACH <<- TRUE
    current_year <<- 1

       # for (ye_f in 1:foreperiod) {                      
        TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR <<-  simperiod  + current_year
#        Fleetyear[[6]]@fleetsegments[[1]]@investment[1]
#        Fleetyear[[6]]@fleetsegments[[1]]@VESSELS
        Fleetyear <<- BMTFlbeh(option, Fleetyear, fdmat, eimat, admat, tpmat, TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR, simperiod, length(BMT_FLEETSEGMENTS))
          
        Fleetyear <<- setEffortVarsForecast(Fleetyear)  
     
         #  Fleetyear[[6]]@fleetsegments[[1]]
        #no_loop_integrated <- ye_f
        source(paste(getwd(), "/src/biol/runBiol.fore.r", sep=""))

              
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################    

} 

    harvest_rule_code <- as.character(cfg[rownames(cfg) == "casestudy.HR",1])
    harvest_rule_level <- as.character(cfg[rownames(cfg) == "casestudy.HR",2])
  
#write.table(cfg, file=paste(casestudy_path, "/", casestudy_name, "_HR", harvest_rule_code, harvest_rule_level,".csv", sep=""), sep=";", row.names=T, col.names = F)

    if (BMT_SCENARIO != BMT_HR_CHANGE_TOTAL_FISHMORTALITY) {
           BMT_STATE <<- "WAIT"
     } else {
           BMT_STATE <<- "FORECAST"
      }  
  
source(paste(getwd(), "/src/runBEMTOOLforecast.r", sep=""))    
      