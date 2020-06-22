# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





setEffortVarsForecast <- function(Fyear) {

if (BMT_SCENARIO %in% c(BMT_HR_CHANGE_SELECTIVITY, BMT_HR_STATUS_QUO, BMT_HR_TAC_VARIATION) ) {  # 1 e 5  e 6

if (BMT_SCENARIO == BMT_HR_CHANGE_SELECTIVITY) {
print(" -------------------------------- SCENARIO 1 [Change in gear selectivity - Non-integrated]", quote=F)
} else if (BMT_SCENARIO == BMT_HR_STATUS_QUO) {
print(" -------------------------------- SCENARIO 5 [Status quo - Non-integrated]", quote=F)
} else {
print(" -------------------------------- SCENARIO 6 [Introduction/variations of TAC - Non-integrated]", quote=F)
}

print("- VESSELS, average DAYS, average GT and average KW equal to the past/present", quote=F)
if (BMT_SCENARIO == BMT_HR_TAC_VARIATION) {
   print("Then, DAYS are equal to zero once the TAC has been reached!", quote=F)
}

    for (n_int in 1:n_fleet) {     
      for (yy_f in 1:foreperiod) {
        yy <- simperiod+yy_f
        Fyear[[yy]]@fleetsegments[[n_int]]@VESSELS[1,] <- as.numeric(as.character(Fyear[[simperiod]]@fleetsegments[[n_int]]@VESSELS[1,]))
        Fyear[[yy]]@fleetsegments[[n_int]]@GT.average[1,] <- as.numeric(as.character(Fyear[[simperiod]]@fleetsegments[[n_int]]@GT.average[1,]))
        Fyear[[yy]]@fleetsegments[[n_int]]@DAYS.average[1,] <- as.numeric(as.character(Fyear[[simperiod]]@fleetsegments[[n_int]]@DAYS.average[1,])) 
        Fyear[[yy]]@fleetsegments[[n_int]]@KW.average[1,] <- as.numeric(as.character(Fyear[[simperiod]]@fleetsegments[[n_int]]@KW.average[1,])) 
      }
    }
   
    Fyear <- updateDerivedEffortVars.nonint(Fyear)
    
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################    

    
} else if (BMT_SCENARIO %in% c(BMT_HR_CHANGE_FISHEFFORT , BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT) ) {  # 2 e 7

if (!MEY_CALCULATION) {
if (BMT_SCENARIO == BMT_HR_CHANGE_FISHEFFORT) {
print(" -------------------------------- SCENARIO 2 [Change of fishing effort - Non-integrated]", quote=F)
settings_field <- "casestudy.HR2" 
} else {
print(" -------------------------------- SCENARIO 7 [Change in gear selectivity & fishing effort - Non-integrated]", quote=F)
settings_field <- "casestudy.HR7" 
}
} else {
print(paste(" -------------------------------- MEY CALCULATION [Change in fishing effort - Non-integrated]", harvest_rule_level), quote=F)
}

if (!MEY_CALCULATION) {    

print("- VESSELS and average DAYS input by the user", quote=F)
print("- average GT and average KW equal to the past/present", quote=F)
# reading from the GUI the values inserted by the user 
vessels_fore <<- read.csv(file=as.character(cfg[rownames(cfg) == settings_field,1]), sep=";", na.strings = "NA", header=FALSE)     # vessels
nm <- as.character(vessels_fore[,1])
empty_indices <- which(nm!="")
nm <- nm[empty_indices]
vessels_fore <<- vessels_fore[,2:ncol(vessels_fore)]
rownames(vessels_fore)[empty_indices] <- nm


averageDAYS_fore <<- read.csv(file=as.character(cfg[rownames(cfg) == settings_field,2]), sep=";", na.strings = "NA", header=FALSE)     # vessels
nm <- as.character(averageDAYS_fore[,1])
empty_indices <- which(nm!="")
nm <- nm[empty_indices]
averageDAYS_fore <<- averageDAYS_fore[,2:ncol(averageDAYS_fore)]
rownames(averageDAYS_fore)[empty_indices] <- nm
       
      for (n_int in 1:n_fleet) {     
          for (y_int in 1:foreperiod ) {
          yy_f <- y_int + simperiod         
          index_col <-  (n_int-1)*foreperiod  + y_int
              for (month in 1:12) { 
              # update the monthly number of vessels input by the user                                
              Fyear[[yy_f]]@fleetsegments[[n_int]]@VESSELS[month] <- as.numeric(as.character(vessels_fore[rownames(vessels_fore) == paste("casestudy.month", month,sep=""),index_col]))
              
               # update the monthly average days input by the user                                
              Fyear[[yy_f]]@fleetsegments[[n_int]]@DAYS.average[month] <- as.numeric(as.character(averageDAYS_fore[rownames(averageDAYS_fore) == paste("casestudy.month", month,sep=""),index_col]))
              } # end loop month
          }   # end loop years

          # GT and KW are the same of the present/past
           for (yy_f in 1:foreperiod) {
                yy <- simperiod+yy_f                               
                Fyear[[yy]]@fleetsegments[[n_int]]@GT.average <- Fyear[[simperiod]]@fleetsegments[[n_int]]@GT.average
                Fyear[[yy]]@fleetsegments[[n_int]]@KW.average <- Fyear[[simperiod]]@fleetsegments[[n_int]]@KW.average
          }

      }   # end loop fleetsegments

} else {

print(paste("- VESSELS and average DAYS are those of the last year *", levs[MEY_LEVEL]), quote=F)
print("- average GT and average KW equal to the past/present", quote=F)

  if (MEY_FLEETSEGMENT == "ALL") { 
    
    for (n_int in 1:n_fleet) {     
      for (yy_f in 1:foreperiod) {
        yy <- simperiod+yy_f
        if (MEY_EFFORT_VAR == "VESSELS") {
        Fyear[[yy]]@fleetsegments[[n_int]]@VESSELS[1,] <- as.numeric(as.character(Fyear[[simperiod]]@fleetsegments[[n_int]]@VESSELS[1,])) * levs[MEY_LEVEL]
        } else {
        Fyear[[yy]]@fleetsegments[[n_int]]@VESSELS[1,] <- as.numeric(as.character(Fyear[[simperiod]]@fleetsegments[[n_int]]@VESSELS[1,]))
        }
        
        if (MEY_EFFORT_VAR == "DAYS") { 
        Fyear[[yy]]@fleetsegments[[n_int]]@DAYS.average[1,] <- as.numeric(as.character(Fyear[[simperiod]]@fleetsegments[[n_int]]@DAYS.average[1,])) * levs[MEY_LEVEL]
        } else {
        Fyear[[yy]]@fleetsegments[[n_int]]@DAYS.average[1,] <- as.numeric(as.character(Fyear[[simperiod]]@fleetsegments[[n_int]]@DAYS.average[1,])) 
        }
        
        Fyear[[yy]]@fleetsegments[[n_int]]@GT.average[1,] <- as.numeric(as.character(Fyear[[simperiod]]@fleetsegments[[n_int]]@GT.average[1,]))
        # Fyear[[yy]]@fleetsegments[[n_int]]@DAYS.average[1,] <- as.numeric(as.character(Fyear[[simperiod]]@fleetsegments[[n_int]]@DAYS.average[1,])) 
        Fyear[[yy]]@fleetsegments[[n_int]]@KW.average[1,] <- as.numeric(as.character(Fyear[[simperiod]]@fleetsegments[[n_int]]@KW.average[1,])) 
      }
    }

    } else {               
    
    index_fleet <- which(BMT_FLEETSEGMENTS == MEY_FLEETSEGMENT)

      for (n_int in 1:n_fleet) {     
      for (yy_f in 1:foreperiod) {
        yy <- simperiod+yy_f
        if (MEY_EFFORT_VAR == "VESSELS") {
        if (n_int == index_fleet) {
          Fyear[[yy]]@fleetsegments[[n_int]]@VESSELS[1,] <- as.numeric(as.character(Fyear[[simperiod]]@fleetsegments[[n_int]]@VESSELS[1,])) * levs[MEY_LEVEL]
        } else {
           Fyear[[yy]]@fleetsegments[[n_int]]@VESSELS[1,] <- as.numeric(as.character(Fyear[[simperiod]]@fleetsegments[[n_int]]@VESSELS[1,]))
        }
        } else {
        Fyear[[yy]]@fleetsegments[[n_int]]@VESSELS[1,] <- as.numeric(as.character(Fyear[[simperiod]]@fleetsegments[[n_int]]@VESSELS[1,]))
        }
        
        if (MEY_EFFORT_VAR == "DAYS") { 
           if (n_int == index_fleet) {
               Fyear[[yy]]@fleetsegments[[n_int]]@DAYS.average[1,] <- as.numeric(as.character(Fyear[[simperiod]]@fleetsegments[[n_int]]@DAYS.average[1,])) * levs[MEY_LEVEL]  
           } else {
               Fyear[[yy]]@fleetsegments[[n_int]]@DAYS.average[1,] <- as.numeric(as.character(Fyear[[simperiod]]@fleetsegments[[n_int]]@DAYS.average[1,]))    
          }
        } else {
        Fyear[[yy]]@fleetsegments[[n_int]]@DAYS.average[1,] <- as.numeric(as.character(Fyear[[simperiod]]@fleetsegments[[n_int]]@DAYS.average[1,])) 
        }
        
        Fyear[[yy]]@fleetsegments[[n_int]]@GT.average[1,] <- as.numeric(as.character(Fyear[[simperiod]]@fleetsegments[[n_int]]@GT.average[1,]))
        # Fyear[[yy]]@fleetsegments[[n_int]]@DAYS.average[1,] <- as.numeric(as.character(Fyear[[simperiod]]@fleetsegments[[n_int]]@DAYS.average[1,])) 
        Fyear[[yy]]@fleetsegments[[n_int]]@KW.average[1,] <- as.numeric(as.character(Fyear[[simperiod]]@fleetsegments[[n_int]]@KW.average[1,])) 
      }
    }

    }



}  

        Fyear <- updateDerivedEffortVars.nonint(Fyear)

########################################################################################################################################
########################################################################################################################################
########################################################################################################################################

      
} else if (BMT_SCENARIO == BMT_HR_CHANGE_FISHMORTALITY) { # 3
print(" -------------------------------- SCENARIO 3 [Change of fishing mortality (by fleet segment) - Non-integrated]", quote=F)
print("- applying the reduction of F by fleet segments (from ALADYM) to annual VESSELS and annual average DAYS according to the proportions input by the user", quote=F)
print("- annual average GT and annual average KW equal to the past/present", quote=F)
   
   
   #if (phase=="SIMULATION") {
#       mortalities_change_path <- paste(casestudy_path, "\\Diagnosis\\ALADYM\\[", casestudy_name, "] Average mortalities reductions SIM.csv", sep="")
# } else {
# if (!MEY_CALCULATION) {
#       mortalities_change_path <- paste(casestudy_path, "\\", harvest_rule_id,"\\ALADYM\\[", casestudy_name, "] Average mortalities reductions FORE ", harvest_rule_id,".csv", sep="")
# } else {
#       mortalities_change_path <- paste(casestudy_path, "\\MEY calculation\\", harvest_rule_id,"\\ALADYM\\[", casestudy_name, "] Average mortalities reduction FORE ", harvest_rule_id,".csv", sep="")
# }
# }   
           
            mortalities_table_change <- read.csv(MORTALITYCHANGEALLSPECIES_table, sep=";")  
 
   # mortalities_table_change <- read.csv(mortalities_change_path, sep=";")     

    mortalities_table_change <- data.frame(mortalities_table_change[,2:ncol(mortalities_table_change)] )
    for (n_int in 1:length(BMT_FLEETSEGMENTS)) {                
      for (yy_f in 1:foreperiod) {
        yy <- simperiod+yy_f
        annual_reduction_F_byfleet <- mortalities_table_change[yy, n_int]
        
        pV <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.HR3.F", n_int, sep=""),2]) ) / 100 
        pD <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.HR3.F", n_int, sep=""),3]) ) / 100  

        Fyear[[yy]]@fleetsegments[[n_int]]@VESSELS.annual <- Fyear[[yy-1]]@fleetsegments[[n_int]]@VESSELS.annual  * ( (100 - abs(annual_reduction_F_byfleet))/100 ) ^ pV
        Fyear[[yy]]@fleetsegments[[n_int]]@DAYS.average.annual <- Fyear[[yy-1]]@fleetsegments[[n_int]]@DAYS.average.annual * ( (100 -  abs(annual_reduction_F_byfleet))/100 ) ^ pD 
      
        Fyear[[yy]]@fleetsegments[[n_int]]@GT.average.annual <- Fyear[[simperiod]]@fleetsegments[[n_int]]@GT.average.annual
        Fyear[[yy]]@fleetsegments[[n_int]]@KW.average.annual <- Fyear[[simperiod]]@fleetsegments[[n_int]]@KW.average.annual
      }
    } 
    
       Fyear <- updateDerivedEffortVars.nonint(Fyear)

########################################################################################################################################
########################################################################################################################################
########################################################################################################################################

} else if (BMT_SCENARIO == BMT_HR_CHANGE_TOTAL_FISHMORTALITY) {  #4
print(" -------------------------------- SCENARIO 4 [Change of total fishing mortality - Non-integrated]", quote=F)
print("- applying the calculated reduction of total F (from Medium Term Forecast) to annual VESSELS and annual average DAYS according to the proportions input by the user", quote=F)
print("- annual average GT and annual average KW equal to the past/present", quote=F)

    annual_reduction_totalF <- get_annual_reduction_totalF()                    

    for (n_int in 1:length(BMT_FLEETSEGMENTS)) {
    
      to_be_reduced <-  as.character(cfg[rownames(cfg) == paste("casestudy.HR4.F", n_int, sep=""),1])  
      
      if (to_be_reduced == "Y") {
      pV <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.HR4.F", n_int, sep=""),2]) ) / 100 
      pD <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.HR4.F", n_int, sep=""),3]) ) / 100  

      for (yy_f in 1:foreperiod) {
        yy <- simperiod+yy_f
        Fyear[[yy]]@fleetsegments[[n_int]]@VESSELS.annual <- Fyear[[yy-1]]@fleetsegments[[n_int]]@VESSELS.annual  * ( (100 - abs(annual_reduction_totalF[yy,1]))/100 ) ^ pV
        Fyear[[yy]]@fleetsegments[[n_int]]@DAYS.average.annual <- Fyear[[yy-1]]@fleetsegments[[n_int]]@DAYS.average.annual * ( (100 - abs(annual_reduction_totalF[yy,1]))/100 ) ^ pD 
      
        Fyear[[yy]]@fleetsegments[[n_int]]@GT.average.annual <- Fyear[[simperiod]]@fleetsegments[[n_int]]@GT.average.annual
        Fyear[[yy]]@fleetsegments[[n_int]]@KW.average.annual <- Fyear[[simperiod]]@fleetsegments[[n_int]]@KW.average.annual
      }
      
    } else {
      for (yy_f in 1:foreperiod) {
        yy <- simperiod+yy_f
        Fyear[[yy]]@fleetsegments[[n_int]]@VESSELS.annual <- Fyear[[simperiod]]@fleetsegments[[n_int]]@VESSELS.annual
        Fyear[[yy]]@fleetsegments[[n_int]]@DAYS.average.annual <- Fyear[[simperiod]]@fleetsegments[[n_int]]@DAYS.average.annual
        
        Fyear[[yy]]@fleetsegments[[n_int]]@GT.average.annual <- Fyear[[simperiod]]@fleetsegments[[n_int]]@GT.average.annual
        Fyear[[yy]]@fleetsegments[[n_int]]@KW.average.annual <- Fyear[[simperiod]]@fleetsegments[[n_int]]@KW.average.annual

      }
    }
}
      
      Fyear <- updateDerivedEffortVars.nonint(Fyear)
      
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################

} else if (BMT_SCENARIO %in%  c(BMT_HR_CHANGE_SELECTIVITY_BEHAVIOURAL, BMT_HR_STATUS_QUO_BEHAVIOURAL) ) {  # 8 e 11 

if (BMT_SCENARIO == BMT_HR_CHANGE_SELECTIVITY_BEHAVIOURAL) {
    print(" -------------------------------- SCENARIO 8 [Change of selectivity & behavioural module - Integrated]", quote=F)
} else if ( BMT_SCENARIO == BMT_HR_STATUS_QUO_BEHAVIOURAL ) {
    print(" -------------------------------- SCENARIO 11 [Status quo & behavioural module - Integrated]", quote=F)
}

print("- VESSELS, average DAYS, average GT and average KW equal to the past/present", quote=F)
print("Then, VESSELS and average DAYS are updated by the BEHAVIOURAL MODULE", quote=F)

    for (n_int in 1:n_fleet) {
        df_vess <- Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR-1]]@fleetsegments[[n_int]]@VESSELS 
        rownames(df_vess) <- years.forecast[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR-simperiod]   
        Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR]]@fleetsegments[[n_int]]@VESSELS <- df_vess
        
        df_GT <- Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR-1]]@fleetsegments[[n_int]]@GT.average
        rownames(df_GT) <-  years.forecast[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR-simperiod]  
        Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR]]@fleetsegments[[n_int]]@GT.average <- df_GT
        
        df_DAYS <- Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR-1]]@fleetsegments[[n_int]]@DAYS.average
        rownames(df_DAYS) <-  years.forecast[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR-simperiod] 
        Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR]]@fleetsegments[[n_int]]@DAYS.average <- df_DAYS
        
        df_KW <- Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR-1]]@fleetsegments[[n_int]]@KW.average
        rownames(df_KW) <-  years.forecast[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR-simperiod]  
        Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR]]@fleetsegments[[n_int]]@KW.average <- df_KW
    } 

                 if (FALSE) {
       for (n_int in 1:n_fleet) {
          print("______________________________________________________________________________", quote=F)
          print(BMT_FLEETSEGMENTS[n_int], quote=F)  
          print("______________________________________________________________________________", quote=F)
          print("BEMTOOL --> VESSELS:", quote=F)
          print( Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR]]@fleetsegments[[n_int]]@VESSELS)
          print("BEMTOOL --> DAYS.average:", quote=F)
          print( Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR]]@fleetsegments[[n_int]]@DAYS.average)
       }
          }
    
   
    #  if (FALSE) {       # FORZATURA SUL BEHAVIOURAL MODULE PER VEDERE SE FUNZIONA
    # the following instruction will be done only for one year at time
      for (n_int in 1:n_fleet) {
         #   if (!is.na(Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR]]@fleetsegments[[n_int]]@investment.CI.perc$number[3]) ) {
      if (!is.na(Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR]]@fleetsegments[[n_int]]@investment$number) ) {
            for (month in 1:12) { 
            # update the monthly number of vessels input by the user 
            # CALCOLO BATTELLI MENSILI DA COMPORTAMENTO DEGLI OPERATORI E SUCCESSIVO CONFRONTO CON DATO DI INPUT DA SCENARIO
#Fleet_by_behaviour <- max(Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR-1]]@fleetsegments[[n_int]]@VESSELS[month] + Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR]]@fleetsegments[[n_int]]@investment.CI.perc$number[3], 0)
Fleet_by_behaviour <- max(Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR-1]]@fleetsegments[[n_int]]@VESSELS[month] + Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR]]@fleetsegments[[n_int]]@investment$number, 0)

Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR]]@fleetsegments[[n_int]]@VESSELS[month] <- Fleet_by_behaviour
           
           # update the monthly average days input by the user 
               # CALCOLO GIORNI MEDI MENSILI DA COMPORTAMENTO OPERATORI E SUCCESSIVO CONFRONTO CON DATO DI INPUT DA SCENARIO
Monthdays_by_behaviour <- Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR-1]]@fleetsegments[[n_int]]@DAYS.average[month]*(Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR]]@fleetsegments[[n_int]]@DAYS.average.annual/Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR-1]]@fleetsegments[[n_int]]@DAYS.average.annual)
           Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR]]@fleetsegments[[n_int]]@DAYS.average[month] <- Monthdays_by_behaviour
         
            } # end loop month    
       }
       }
  # }     
               Fyear <- updateDerivedEffortVars.int(Fyear, TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR)

               
               if (FALSE) {
     for (n_int in 1:n_fleet) {
    print("______________________________________________________________________________", quote=F)
    print(BMT_FLEETSEGMENTS[n_int], quote=F)  
    print("______________________________________________________________________________", quote=F)
    print("after BEHAVIOURAL MODULE --> VESSELS:", quote=F)
    print(paste("(with investment = ", Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR]]@fleetsegments[[n_int]]@investment[1], ")", sep=""), quote=F)
    print( Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR]]@fleetsegments[[n_int]]@VESSELS)
    print("after BEHAVIOURAL MODULE --> DAYS.average:", quote=F)
    print( Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR]]@fleetsegments[[n_int]]@DAYS.average)
     }
            }


########################################################################################################################################
########################################################################################################################################
########################################################################################################################################    


} else if (BMT_SCENARIO %in% c(BMT_HR_CHANGE_FISHEFFORT_BEHAVIOURAL , BMT_HR_CHANGE_SELECTIVITY_FISHEFFORT_BEHAVIOURAL) ) {    # 9 e 10
if (BMT_SCENARIO == BMT_HR_CHANGE_FISHEFFORT_BEHAVIOURAL) {
print(" -------------------------------- SCENARIO 9 [Change of fishing effort & behavioural module - Integrated]", quote=F)
settings_field <- "casestudy.HR9" 
} else {
print(" -------------------------------- SCENARIO 10 [Change in gear selectivity & fishing effort & behavioural module - Integrated]", quote=F)
settings_field <- "casestudy.HR10" 
}
print("- VESSELS and average DAYS input by the user", quote=F)   
print("- average GT and average KW equal to the past/present", quote=F)
print("Then, VESSELS and average DAYS are compared with the BEHAVIOURAL MODULE and updated", quote=F)

# read the values set by the user
# reading from the GUI the values inserted by the user 
vessels_fore <<- read.csv(file=as.character(cfg[rownames(cfg) == settings_field,1]), sep=";", na.strings = "NA", header=FALSE)     # vessels
nm <- as.character(vessels_fore[,1])
empty_indices <- which(nm!="")
nm <- nm[empty_indices]
vessels_fore <<- vessels_fore[,2:ncol(vessels_fore)]
rownames(vessels_fore)[empty_indices] <- nm


averageDAYS_fore <<- read.csv(file=as.character(cfg[rownames(cfg) == settings_field,2]), sep=";", na.strings = "NA", header=FALSE)     # vessels
nm <- as.character(averageDAYS_fore[,1])
empty_indices <- which(nm!="")
nm <- nm[empty_indices]
averageDAYS_fore <<- averageDAYS_fore[,2:ncol(averageDAYS_fore)]
rownames(averageDAYS_fore)[empty_indices] <- nm
       
      for (n_int in 1:n_fleet) {     
          for (y_int in 1:foreperiod ) {
          yy_f <- y_int + simperiod         
          index_col <-  (n_int-1)*foreperiod  + y_int
              for (month in 1:12) { 
              # update the monthly number of vessels input by the user                                
              Fyear[[yy_f]]@fleetsegments[[n_int]]@VESSELS[month] <- as.numeric(as.character(vessels_fore[rownames(vessels_fore) == paste("casestudy.month", month,sep=""),index_col]))
              
               # update the monthly average days input by the user                                
              Fyear[[yy_f]]@fleetsegments[[n_int]]@DAYS.average[month] <- as.numeric(as.character(averageDAYS_fore[rownames(averageDAYS_fore) == paste("casestudy.month", month,sep=""),index_col]))
              } # end loop month
          }   # end loop years

          # GT and KW are the same of the present/past
           for (yy_f in 1:foreperiod) {
                yy <- simperiod+yy_f                               
                Fyear[[yy]]@fleetsegments[[n_int]]@GT.average <- Fyear[[simperiod]]@fleetsegments[[n_int]]@GT.average
                Fyear[[yy]]@fleetsegments[[n_int]]@KW.average <- Fyear[[simperiod]]@fleetsegments[[n_int]]@KW.average
          }

      }   # end loop fleetsegments
  
        #  if (FALSE) {       # FORZATURA SUL BEHAVIOURAL MODULE PER VEDERE SE FUNZIONA
    # the following instruction will be done only for one year at time
      for (n_int in 1:n_fleet) {
            for (month in 1:12) { 
 # update the monthly number of vessels input by the user 
            # CALCOLO BATTELLI MENSILI DA COMPORTAMENTO DEGLI OPERATORI E SUCCESSIVO CONFRONTO CON DATO DI INPUT DA SCENARIO
            Fleet_by_behaviour <- max(Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR-1]]@fleetsegments[[n_int]]@VESSELS[month]+Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR]]@fleetsegments[[n_int]]@investment[1], 0)
            Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR]]@fleetsegments[[n_int]]@VESSELS[month] <- Fleet_by_behaviour
           
               # update the monthly average days input by the user 
               # CALCOLO GIORNI MEDI MENSILI DA COMPORTAMENTO OPERATORI E SUCCESSIVO CONFRONTO CON DATO DI INPUT DA SCENARIO
            Monthdays_by_behaviour <- Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR-1]]@fleetsegments[[n_int]]@DAYS.average[month]*(Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR]]@fleetsegments[[n_int]]@DAYS.average.annual/Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR-1]]@fleetsegments[[n_int]]@DAYS.average.annual)
           Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR]]@fleetsegments[[n_int]]@DAYS.average[month] <- Monthdays_by_behaviour
     
            } # end loop month
      }
      
     # }
      

           if (FALSE) {
     for (n_int in 1:n_fleet) {
    print("______________________________________________________________________________", quote=F)
    print(BMT_FLEETSEGMENTS[n_int], quote=F)  
    print("______________________________________________________________________________", quote=F)
    print("after BEHAVIOURAL MODULE --> VESSELS:", quote=F)
    print(paste("(with investment = ", Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR]]@fleetsegments[[n_int]]@investment[1], ")", sep=""), quote=F)
    print( Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR]]@fleetsegments[[n_int]]@VESSELS)
    print("after BEHAVIOURAL MODULE --> DAYS.average:", quote=F)
    print( Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR]]@fleetsegments[[n_int]]@DAYS.average)
     }
      }
      
            Fyear <- updateDerivedEffortVars.int(Fyear, TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR)

}


return(Fyear)
}
 
