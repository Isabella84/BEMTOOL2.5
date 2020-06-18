# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





 # INTEGRATED APPROACH 
 # (update one year at time; change the order of updating of days variable
 
# update derived variables in the current year of the forecast 

updateDerivedEffortVars.int <- function(FY, TIME_TO_CHANGE) {
  
for (n_int in 1:n_fleet) {     

 # -------------------------------------------------------- about VESSELS
  # update annual VESSELS
   FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@VESSELS.annual <- mean(as.numeric(as.character(FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@VESSELS)))
   
# -------------------------------------------------------- about DAYS
# update monthly DAYS
 # CALCOLO GIORNI TOTALI MENSILI (come sopra)
 days_df <- data.frame(matrix( as.numeric(as.character(FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@DAYS.average)) * as.numeric(as.character(FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@VESSELS)) , nrow=1)) 
   colnames(days_df) <- MONTHS   
   FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@DAYS  <- days_df
   
      FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@DAYS[1,] <- as.numeric(as.character(   FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@DAYS[1,] )  )
 
  # CALCOLO GIORNI TOTALI ANNUALI (come sopra)
   # update annual DAYS
  FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@DAYS.annual <- sum(as.numeric(as.character(FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@DAYS)) )

  # CALCOLO GIORNI MEDI ANNUALI (come sopra) - aggiornamento della variabile che era stata stimata dal modulo comportamentale
  FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@DAYS.average.annual <- as.numeric(as.character(FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@DAYS.annual))/  as.numeric(as.character(FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@VESSELS.annual ))
  
  
   # -------------------------------------------------------- about GT  
   # note: all the GT variables are constant in all the simulation and forecast
   # update monthly GT
   GT_df <- data.frame(matrix(as.numeric(as.character(FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@GT.average)) *  as.numeric(as.character(FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@VESSELS)), nrow=1))
   colnames(GT_df) <- MONTHS
   FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@GT <- GT_df 
   # update annual GT
   FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@GT.annual <-  mean(as.numeric(as.character(FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@GT)) )
   # upadate annual average GT
   FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@GT.average.annual <- as.numeric(as.character(FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@GT.annual))/ as.numeric(as.character(FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@VESSELS.annual))
   
   # -------------------------------------------------------- about KW  
   # note: all the KW variables are constant in all the simulation and forecast
    # update monthly KW
   KW_df <- data.frame(matrix(as.numeric(as.character(FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@KW.average)) *  as.numeric(as.character(FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@VESSELS)), nrow=1))
   colnames(KW_df) <- MONTHS
   FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@KW <- KW_df
   # update annual KW
   FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@KW.annual <-  mean(as.numeric(as.character(FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@KW)) )
   # upadate annual average KW
   FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@KW.average.annual <- as.numeric(as.character(FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@KW.annual))/ as.numeric(as.character(FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@VESSELS.annual))
   
      # -------------------------------------------------------- about GTdays  
   # update monthly GTdays
   # from Paolo description: Total GT by month multiplied by the average number of days at sea per vessel by month. 
   # This is estimated by each month as (capacity.GT[month]*no_daysatsea[month]/no_vessels[month]).
   GTDAYS_df <- data.frame(matrix(as.numeric(as.character(FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@GT)) * as.numeric(as.character(FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@DAYS.average)), nrow=1))
   colnames(GTDAYS_df) <- MONTHS
   FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@GT.DAYS <- GTDAYS_df
      # update annual GTdays
   # from Paolo description: Total GT multiplied by the average number of days at sea per vessel in the current year. This is estimated as (capacity.GT.annual*no_daysatsea.annual/no_vessels.annual).
   FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@GT.DAYS.annual <- as.numeric(as.character(FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@GT.annual)) * as.numeric(as.character(FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@DAYS.average.annual))
   
   
    # -------------------------------------------------------- about KWdays
     # update monthly KWdays
   # from Paolo description: Total KW by month multiplied by the average number of days at sea per vessel by month. 
   # This is estimated by each month as (capacity.KW[month]*no_daysatsea[month]/no_vessels[month]).
   KWDAYS_df <- data.frame(matrix(as.numeric(as.character(FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@KW)) * as.numeric(as.character(FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@DAYS.average)), nrow=1))
   colnames(KWDAYS_df) <- MONTHS
   FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@KW.DAYS <- KWDAYS_df
      # update annual KWdays
   # from Paolo description: Total KW multiplied by the average number of days at sea per vessel in the current year. This is estimated as (capacity.KW.annual*no_daysatsea.annual/no_vessels.annual).
   FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@KW.DAYS.annual <- as.numeric(as.character(FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@KW.annual)) * as.numeric(as.character(FY[[TIME_TO_CHANGE]]@fleetsegments[[n_int]]@DAYS.average.annual))  

}

return(FY)
}
