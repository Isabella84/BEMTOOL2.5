# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





 updateDerivedEffortVars.nonint<-function(Fyear) {
   
if (phase == "SIMULATION") { 
start_index <- 1
end_index <- simperiod
BMT_SCENARIO <- 0
} else {
start_index <- simperiod+1
end_index <- simperiod+foreperiod
}
 
  
  # in Read econ data only:
  # - monthly number of vessels
  # - monthly average GT
  # - monthly average DAYS
  # - monthly average KW
  
for (n_int in 1:n_fleet) {     
  for (y_int in start_index:end_index) {
 
 if (BMT_SCENARIO != BMT_HR_CHANGE_FISHMORTALITY & BMT_SCENARIO != BMT_HR_CHANGE_TOTAL_FISHMORTALITY) {
 # -------------------------------------------------------- about VESSELS
  # update annual VESSELS
   Fyear[[y_int]]@fleetsegments[[n_int]]@VESSELS.annual <- mean(as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@VESSELS)))
   
  # -------------------------------------------------------- about DAYS
   # update monthly DAYS
   days_df <- data.frame(matrix(as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@DAYS.average)) *  as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@VESSELS)), nrow=1))
   colnames(days_df) <- MONTHS
   Fyear[[y_int]]@fleetsegments[[n_int]]@DAYS <- days_df
   # update annual DAYS
   Fyear[[y_int]]@fleetsegments[[n_int]]@DAYS.annual <- sum(as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@DAYS)) )
   # update annual average DAYS
   Fyear[[y_int]]@fleetsegments[[n_int]]@DAYS.average.annual <- as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@DAYS.annual))/  as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@VESSELS.annual ))
  
   # -------------------------------------------------------- about GT  
   # note: all the GT variables are constant in all the simulation and forecast
   # update monthly GT
   GT_df <- data.frame(matrix(as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@GT.average)) *  as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@VESSELS)), nrow=1))
   colnames(GT_df) <- MONTHS
   Fyear[[y_int]]@fleetsegments[[n_int]]@GT <- GT_df 
   # update annual GT
   Fyear[[y_int]]@fleetsegments[[n_int]]@GT.annual <-  mean(as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@GT)) )
   # upadate annual average GT
   Fyear[[y_int]]@fleetsegments[[n_int]]@GT.average.annual <- as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@GT.annual))/ as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@VESSELS.annual))
   
   # -------------------------------------------------------- about KW  
   # note: all the KW variables are constant in all the simulation and forecast
   # update monthly KW
   KW_df <- data.frame(matrix(as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@KW.average)) *  as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@VESSELS)), nrow=1))
   colnames(KW_df) <- MONTHS
   Fyear[[y_int]]@fleetsegments[[n_int]]@KW <- KW_df 
   # update annual KW
   Fyear[[y_int]]@fleetsegments[[n_int]]@KW.annual <-  mean(as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@KW)) )
   # upadate annual average KW
   Fyear[[y_int]]@fleetsegments[[n_int]]@KW.average.annual <- as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@KW.annual))/ as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@VESSELS.annual))
   
   
   # -------------------------------------------------------- about GTdays  
   # update monthly GTdays
   # from Paolo description: Total GT by month multiplied by the average number of days at sea per vessel by month. 
   # This is estimated by each month as (capacity.GT[month]*no_daysatsea[month]/no_vessels[month]).
   GTDAYS_df <- data.frame(matrix(as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@GT)) * as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@DAYS.average)), nrow=1))
   colnames(GTDAYS_df) <- MONTHS
   Fyear[[y_int]]@fleetsegments[[n_int]]@GT.DAYS <- GTDAYS_df
      # update annual GTdays
   # from Paolo description: Total GT multiplied by the average number of days at sea per vessel in the current year. This is estimated as (capacity.GT.annual*no_daysatsea.annual/no_vessels.annual).
   Fyear[[y_int]]@fleetsegments[[n_int]]@GT.DAYS.annual <- as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@GT.annual)) * as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@DAYS.average.annual))
   
   
    # -------------------------------------------------------- about KWdays
     # update monthly KWdays
   # from Paolo description: Total KW by month multiplied by the average number of days at sea per vessel by month. 
   # This is estimated by each month as (capacity.KW[month]*no_daysatsea[month]/no_vessels[month]).
   KWDAYS_df <- data.frame(matrix(as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@KW)) * as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@DAYS.average)), nrow=1))
   colnames(KWDAYS_df) <- MONTHS
   Fyear[[y_int]]@fleetsegments[[n_int]]@KW.DAYS <- KWDAYS_df
      # update annual KWdays
   # from Paolo description: Total KW multiplied by the average number of days at sea per vessel in the current year. This is estimated as (capacity.KW.annual*no_daysatsea.annual/no_vessels.annual).
   Fyear[[y_int]]@fleetsegments[[n_int]]@KW.DAYS.annual <- as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@KW.annual)) * as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@DAYS.average.annual))  
   
   } else {
   
   # -------------------------------------------------------- about VESSELS: no update
    
  # -------------------------------------------------------- about DAYS
   # update annual DAYS
   Fyear[[y_int]]@fleetsegments[[n_int]]@DAYS.annual <- as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@DAYS.average.annual)) * as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@VESSELS.annual ))
  
   # -------------------------------------------------------- about GT  
   # note: all the GT variables are constant in all the simulation and forecast
   # update annual GT
   Fyear[[y_int]]@fleetsegments[[n_int]]@GT.annual <- as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@GT.average.annual)) * as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@VESSELS.annual ))
   
   # -------------------------------------------------------- about KW  
   # note: all the KW variables are constant in all the simulation and forecast
      # update annual KW
   Fyear[[y_int]]@fleetsegments[[n_int]]@KW.annual <- as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@KW.average.annual)) * as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@VESSELS.annual ))
   
   # -------------------------------------------------------- about GTdays  
       # update annual GTdays
   # from Paolo description: Total GT multiplied by the average number of days at sea per vessel in the current year. This is estimated as (capacity.GT.annual*no_daysatsea.annual/no_vessels.annual).
   Fyear[[y_int]]@fleetsegments[[n_int]]@GT.DAYS.annual <- as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@GT.annual)) * as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@DAYS.average.annual))
   
   
    # -------------------------------------------------------- about KWdays
      # update annual KWdays
   # from Paolo description: Total KW multiplied by the average number of days at sea per vessel in the current year. This is estimated as (capacity.KW.annual*no_daysatsea.annual/no_vessels.annual).
   Fyear[[y_int]]@fleetsegments[[n_int]]@KW.DAYS.annual <- as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@KW.annual)) * as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@DAYS.average.annual))  

   }
}
}

return(Fyear)
}
