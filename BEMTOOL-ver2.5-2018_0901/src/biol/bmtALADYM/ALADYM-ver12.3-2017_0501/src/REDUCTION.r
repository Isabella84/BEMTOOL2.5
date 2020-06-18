# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

# gear varia tra 1 e nb_gears

REDUCTION <- function (code_red,gear) {


    forecast_simulation =  GLO$L_number - forecast +1
    anni_forecast=  forecast_simulation/12
    #if (as.numeric(INP$OPT_F_TYPE)==2) {
    Fishing_mort = SRO$Annual_F_by_gear_by_year[(forecast-1)/INP$Time_slice,gear] 
    Fishing_mort_tot = sum(SRO$Annual_F_by_gear_by_year[(forecast-1)/INP$Time_slice,]) 
    #}else {
    #Fishing_mort =  meanWequals(SRO$annual_F_calc_by_gear[,gear], forecast + 1, INP$Time_slice) [(forecast-1)/INP$Time_slice]
    #Fishing_mort_tot =  meanWequals(SRO$annual_F_calc, forecast + 1, INP$Time_slice) [(forecast-1)/INP$Time_slice]
    #}
    multiplier_ann <- vector(length=anni_forecast,mode="numeric")
    multiplier_ann[] <-1  
    multiplier <- vector(length=forecast_simulation,mode="numeric")
    multiplier[] <-1
    Ref_point_gear = Ref_point * Fishing_mort/Fishing_mort_tot  # equivalente a: meanWequals(SRO$pj[,gear], forecast + 1, INP$Time_slice) [(forecast-1)/INP$Time_slice]    
    
	if (as.character(code_red) == "LOGISTIC" & (!is.na(Ref_point_gear))) {
    p=Ref_point_gear/Fishing_mort
    t50 = 0.5*(Ref_month-forecast)/12
    R = ((Ref_month-forecast)/12)/4
	
	# moltiplicatori per riduzioni (anni)	
        for (loca_i in (1:anni_forecast)){
        multiplier_ann[loca_i] = logistic(p,t50,R,loca_i+1)             
        } 
        if (Ref_month!=GLO$L_number){         
        for (loca_i in ((1+(Ref_month-forecast+1)/12):((GLO$L_number-forecast+1)/12))){
        multiplier_ann[loca_i] = multiplier_ann[(Ref_month-forecast+1)/12] 
        } 
        }
	# moltiplicatori per riduzioni (mesi)	
		for (loca_i in 1:forecast_simulation){
            if (!is.na(Ref_point_gear)& modulo(loca_i,12)==0) {
            multiplier[loca_i] =  multiplier_ann[loca_i/12] 
            # EXP(LN(0.6/0.83)/(168-60))
            } else {
            multiplier[loca_i] =  multiplier_ann[1+as.integer(loca_i/12)] 
            }
        }    
      
    }  else if (code_red == "EXPONENTIAL" & (!is.na(Ref_point))) {
       k = -log(Ref_point_gear/Fishing_mort) /((Ref_month-forecast+1)/12)
	   
            for (loca_i in (1:anni_forecast)){
            multiplier_ann[loca_i] = exp(-k*(loca_i))         
            } 
            if (Ref_month!=GLO$L_number){   
            for (loca_i in ((1+(Ref_month-forecast+1)/12):((GLO$L_number-forecast+1)/12))){
            multiplier_ann[loca_i] = multiplier_ann[(Ref_month-forecast+1)/12] 
            } 
            }
            for (loca_i in 1:forecast_simulation){
                if (modulo(loca_i,12)==0) {
                multiplier[loca_i] =  multiplier_ann[loca_i/12] 
                # EXP(LN(0.6/0.83)/(168-60))
                } else {
                multiplier[loca_i] =  multiplier_ann[1+as.integer(loca_i/12)] 
				}
                  }    
                  
       } else if (code_red == "LINEAR" & (!is.na(Ref_point))) {
       p = Ref_point_gear/Fishing_mort
       b =  (p-1)/((Ref_month-forecast+1) /12)                    #(Ref_point-Fishing_mort)/ 
            for (loca_i in (1:anni_forecast)){
            multiplier_ann[loca_i] = 1 + b*(loca_i)   
            }   
            if (Ref_month!=GLO$L_number){    
            for (loca_i in ((1+(Ref_month-forecast+1)/12):((GLO$L_number-forecast+1)/12))){
            multiplier_ann[loca_i] = multiplier_ann[(Ref_month-forecast+1)/12] 
            } 
            }
            for (loca_i in 1:forecast_simulation){
                if (!is.na(Ref_point_gear)& modulo(loca_i,12)==0) {
                multiplier[loca_i] =  multiplier_ann[loca_i/12]
                } else {
                multiplier[loca_i] =  multiplier_ann[1+as.integer(loca_i/12)]
                }
            }    
                        
        } 
        
if (all(INP$p_Production[c((forecast-12+1):forecast),gear]==0)) {
multiplier[] <-1
}          
return(multiplier)
}
