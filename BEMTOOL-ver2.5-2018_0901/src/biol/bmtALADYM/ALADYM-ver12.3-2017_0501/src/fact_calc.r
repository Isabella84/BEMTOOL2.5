# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



# eff_data_original= eff_data
















fact_calc <-function(eff_data,FORECAST) {
     

     nb_gears = length(FLEETSEGMENTS_names)
# eff_data= eff_data_all
# FORECAST = "Y"

Mean = data.frame(matrix(nrow=(GLO$L_number+1),ncol=nb_gears))
fact =data.frame( matrix(nrow=(GLO$L_number+1),ncol=nb_gears))
fact_mean = data.frame(matrix(nrow=(GLO$L_number+1),ncol=nb_gears))
change_eff= data.frame(matrix(nrow=(GLO$L_number+1),ncol=nb_gears))
fact_adj = data.frame(matrix(nrow=(GLO$L_number+1),ncol=nb_gears))
change_eff_mean = data.frame(matrix(nrow=length(years_forecast),ncol=nb_gears))

      Vessels =  data.frame(matrix(nrow=(GLO$L_number+1),ncol=nb_gears) )
      Days =   data.frame(matrix(nrow=(GLO$L_number+1),ncol=nb_gears) )
      GT =   data.frame(matrix(nrow=(GLO$L_number+1),ncol=nb_gears) )
      Prod =  data.frame(matrix(nrow=(GLO$L_number+1),ncol=nb_gears) )
      
      for (gear in 1:nb_gears) {
        Temp = eff_data[eff_data$Gear == unique(eff_data$Gear)[gear],]
        for (loca_i in 1:(GLO$L_number +1)) {
        Vessels[loca_i,gear]= ifelse(!is.na(Temp$Vessels[loca_i]),as.numeric(as.character(Temp$Vessels[loca_i])),1)    # se ci sono tutte e 3 le variabili, le considera tutte e tre, altrimenti solo quelle che ci sono
        Days[loca_i,gear]= ifelse(!is.na(Temp$Days[loca_i]), as.numeric(as.character(Temp$Days[loca_i])),1)
        GT[loca_i,gear]= ifelse(!is.na(Temp$GT[loca_i]), as.numeric(as.character(Temp$GT[loca_i])),1)
        }      
      }  
      
      # creazione prodotti delle variabili di sforzo
      for (c in 1:nb_gears) {
        for (r in 1:(GLO$L_number +1) ) {
        Prod[r,c] = Vessels[r,c]*Days[r,c]*GT[r,c]
        }      
      }
      
       # calcolo le medie annuali
      for (c in 1:nb_gears){
         for (year in 1:((forecast-1)/12)) {
         Mean[((year-1)*12+2):(year*12+1),c] = meanWequals (Prod[,c], forecast, INP$Time_slice) [year]
         }
      }
         Mean[1,] = Mean[2,]
      
    if (FORECAST=="N") {
      # calcolo fishing coefficient fact
      for (c in 1:nb_gears) {
        for (r in 1:(forecast)){
        fact[r,c] = Prod[r,c] /Mean[r,c]
        }      
      }
  
     fact_adj = fact
  
    } else if (FORECAST== "Y") {
      forecast_length= GLO$L_number - forecast+1
      Prod2 = data.frame( matrix(nrow=(forecast),ncol=nb_gears))


      # calcolo la media di baseline
      for (c in 1:nb_gears){
        for (year in (1+(forecast-1)/12):(GLO$L_number/12)) {
#             if (!INTEGRATED_APPROACH) {
     #        Mean[((year-1)*12+2):(year*12+1),c] = mean(Mean[(forecast-INP$Average_forecast*12+1):forecast,c])       



   if (!INTEGRATED_APPROACH) {
   Mean[((year-1)*12+2):(year*12+1),c] = mean(Mean[(forecast-12+1):forecast,c])       
       } else {
       if (current_year == 1) {
            INP$fish_coeff_baseline[c] = mean(Mean[(forecast-12+1):forecast,c]) 
       }
       }
		   # } else {
#             Mean[((year-1)*12+2):(year*12+1),c] = mean(Mean[((simperiod*12+1)-INP$Average_forecast*12+1):(simperiod*12+1),c])       
#             }
         }

      }
    



    if (INTEGRATED_APPROACH) {
    for (c in 1:nb_gears){
      for (year in (1+(forecast-1)/12):(GLO$L_number/12)) {
         Mean[((year-1)*12+2):(year*12+1),c] <- INP$fish_coeff_baseline[c]
         }
         }
    }
     


      # calcolo fishing coefficient fact
for (c in 1:nb_gears){
        for (r in (forecast+1):(GLO$L_number+1)){
        fact[r,c] = Prod[r,c] /Mean[r,c]
        }      
 }

## 




fact_mean_final = data.frame(matrix(INP$Fishing_efforts[c((forecast-12+1):forecast),], ncol=nb_gears) ) 
colnames(fact_mean_final) <- FLEETSEGMENTS_names      
fact_mean_temp = data.frame(matrix(INP$Fishing_efforts[c((forecast-12+1):forecast),], ncol=nb_gears) ) 
colnames(fact_mean_temp) <- FLEETSEGMENTS_names   


for (nl in 2:  length(years_forecast) ){
fact_mean_final =    data.frame(rbind(fact_mean_final, fact_mean_temp) )
colnames(fact_mean_final) <- FLEETSEGMENTS_names

}
#fact_mean_final = rbind(fact_mean_final[1,],fact_mean_final)

mata <- data.frame(matrix(INP$Fishing_efforts[1:forecast,] , ncol=nb_gears) )
colnames(mata) <- FLEETSEGMENTS_names

fact_mean_final = data.frame(rbind(mata,fact_mean_final))
colnames(fact_mean_final) <- FLEETSEGMENTS_names

##             }
#         }
#      }
      
#INP$typeEffFishMort = c("LIN", "EXP","EXP","EXP","EXP","EXP","EXP","LIN")# rep("LIN",8) #or "EXP" 
#INP$COEF1 =rep( 0.01 ,8)
#INP$COEF2 = rep(0.5 ,8)
change_eff_adj = change_eff


for (coll in 1:nb_gears) {
     for (roww in (forecast+1):(GLO$L_number+1)) {
        change_eff [roww,coll] =  ifelse(!is.finite(fact[roww,coll] /fact_mean_final[roww,coll] ), 0, fact[roww,coll]/fact_mean_final[roww,coll] )  # fact[(forecast+1):(GLO$L_number+1)]/fact_mean [(forecast+1):(GLO$L_number+1)]#   
        }




#change_eff_mean [,coll] = meanWequals (as.numeric(change_eff[(forecast+1):(GLO$L_number+1),coll]), length(c((forecast+1):(GLO$L_number+1)))+1 , INP$Time_slice) # rimuove inf ed NA

#
#    for (years in 1:length(years_forecast)) {
#        change_eff_adj [c((forecast+(years-1)*12):(forecast+years*12)),coll] = change_eff_mean [years,coll]          # è calcolato sulle medie annuali 
#     }
    change_eff_adj <-  change_eff 

}    



for (jea in 1:nb_gears) {






if (as.character(INP$typeEffFishMort[jea]) == "LIN")  {


fact_adj[(forecast+1):(GLO$L_number+1),jea] = fact_mean_final[(forecast+1):(GLO$L_number+1),jea]* (INP$COEF1[jea] + INP$COEF2[jea] * change_eff_adj[(forecast+1):(GLO$L_number+1),jea] )
} else {
fact_adj[(forecast+1):(GLO$L_number+1),jea] = fact_mean_final[(forecast+1):(GLO$L_number+1),jea] * (INP$COEF1[jea] * change_eff_adj[(forecast+1):(GLO$L_number+1),jea]^INP$COEF2[jea])
}
#fact_adj_LIN = fact_adj    
}

# fact_adj[1:forecast,] =  INP$Fishing_efforts[1:forecast,]
 
 if (!INTEGRATED_APPROACH) {

    fact_adj[1:forecast,] =  INP$Fishing_efforts[1:forecast,]
 }

 
# print("calculated f coeff :")
# print(fact_adj)

}      # if FORECAST OR NOT

  fact_adj[1, ] = 1
  
 
  
return(fact_adj)

#plot(fact[,1],type="l")
#lines(fact[,1],col="red")
#lines(fact_adj[,1],col="green")

}
