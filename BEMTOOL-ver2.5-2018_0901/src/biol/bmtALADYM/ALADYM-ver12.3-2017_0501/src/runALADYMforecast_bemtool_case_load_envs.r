# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



source(paste(getwd(), "/src/biol/bmtALADYM/reloadEnvSpecies.r", sep=""))

INP$Year_simulation  <- simperiod + foreperiod
GLO$L_number <- INP$Time_slice * INP$Year_simulation      # numero di mesi della simulazione     
forecast <-  simperiod * 12 + 1

if (READ_ENV_OK)  {            # if da saltare nella simulazione passo passo

 bmt_average_forecast <<-  as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", ALADYM_spe, ".AladymSimulation", sep=""),2])) 

associated_fleetsegment <<- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <<- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <<- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

FLEETSEGMENTS_names <<- BMT_FLEETSEGMENTS[associated_fleetsegment_indices] 
nb_gears <- length(FLEETSEGMENTS_names)


		
if (BMT_SCENARIO != BMT_HR_CHANGE_FISHMORTALITY )  {     # ----------------------------------------------- different from REDUCTION OF F scenario
    Ref_point <-  NA     # Reference point per short e medium term forecast            
    Ref_month <-  NA   
    Ref_year <-  NA 
} else {     # ----------------------------------------------------------------------------------------------------- different from REDUCTION OF F scenario	
        # read the forecast reduction input by the user
        bmt_forecast_reduction <-  as.numeric(as.character(new_aldForecast@target_F)) #as.numeric(as.character(cfg[rownames(cfg) == "casestudy.HR3",2]))
        # set the time span in which get the reduction input by the user
        bmt_time_span <- as.numeric(as.character(new_aldForecast@target_year)) - years[length(years)]  #as.numeric(as.character(cfg[rownames(cfg) == "casestudy.HR3",1]))
        # set the reference month 
        Ref_month <- (simperiod + bmt_time_span) * 12
        
        phase <<- "SIMULATION"
        source( paste(ALADYM_home, "/src/paths.r", sep="") )    
        mortalities_table <- read.csv(MORTALITY_table, sep=";") 
        phase <<- "FORECAST"
        source( paste(ALADYM_home, "/src/paths.r", sep="") )    
        
        fcurrent_spe <- mortalities_table$Annual_F_estimated[simperiod]
        Ref_point <-  fcurrent_spe * (100 - bmt_forecast_reduction)/100 


        fl_ord <- 1
        for (n_int in 1:length(BMT_FLEETSEGMENTS)) {
        if (n_int %in% associated_fleetsegment_indices) {
        
red_value <- FleetList_forecast[[fl_ord]]@scenario.reduction
if (red_value == 1) {
 INP$Forecast_reduction[1,fl_ord] <-  "Exponential"
} else if (red_value == 2) {
INP$Forecast_reduction[1,fl_ord] <-  "Linear"
} else {
INP$Forecast_reduction[1,fl_ord] <-  "Logistic"
}

         #  Forecast_reduction[1,fl_ord] <- as.character(cfg[rownames(cfg) == paste("casestudy.HR3.F", n_int, sep=""),1])  
         # INP$Forecast_reduction[1,fl_ord] <- FleetList_forecast[[fl_ord]]@scenario.reduction # as.character(cfg[rownames(cfg) == paste("casestudy.HR3.F", n_int, sep=""),1])  
           fl_ord <- fl_ord + 1
           }            
        }     
            
 Forecast_reduction <- INP$Forecast_reduction

}													
}