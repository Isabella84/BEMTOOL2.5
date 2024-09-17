# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



# BMTPrice.R - Bemtool economic indicators
# Author: Paolo Accadia

BMTEconind <- function(Flyear, tsmat1, tsmat2, eimat, currenttime, n_fleet) {

if (FALSE) {
  Flyear = Fleetyear
  currenttime=8
}

    for (i in 1:n_fleet) {
# --------------------------------------------------------------    
Flyear[[currenttime]]@fleetsegments[[i]]@average.wage <- ifelse(as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@employment)) !=0, as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@labour.cost)) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@employment)) , 0)
# --------------------------------------------------------------  
Flyear[[currenttime]]@fleetsegments[[i]]@total.cost <- sum(as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$tot.var.cost)),as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$tot.fix.cost)),as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@labour.cost)),as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@capital.cost$tot.cap.cost)), na.rm=T)
# --------------------------------------------------------------  subsidies
Flyear[[currenttime]]@fleetsegments[[i]]@other.income <- sum(as.numeric(as.character(Flyear[[1]]@fleetsegments[[i]]@other.income)) , as.numeric(as.character(tsmat2[i,currenttime-simperiod])), na.rm=T)  
# --------------------------------------------------------------        
Flyear[[currenttime]]@fleetsegments[[i]]@total.income <- sum(as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@total.revenues)), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@other.income)), na.rm=T)
# --------------------------------------------------------------        
Flyear[[currenttime]]@fleetsegments[[i]]@gross.value.added <- sum(as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@total.revenues)), - as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$tot.var.cost)), - as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$tot.fix.cost)), - as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$maint.cost)) , na.rm=T) 
# --------------------------------------------------------------  
Flyear[[currenttime]]@fleetsegments[[i]]@gross.cash.flow <- sum(as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@gross.value.added)), - as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@labour.cost)), na.rm=T)
# --------------------------------------------------------------  
Flyear[[currenttime]]@fleetsegments[[i]]@profit <- sum(as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@gross.cash.flow)), - as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@capital.cost$tot.cap.cost)),na.rm=T)
# --------------------------------------------------------------        
Flyear[[currenttime]]@fleetsegments[[i]]@taxes <- as.numeric(as.character(tsmat1[i,currenttime-simperiod]))
# --------------------------------------------------------------  
Flyear[[currenttime]]@fleetsegments[[i]]@net.profit <- sum(as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@profit)),-as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@taxes)), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@other.income)) ,na.rm=T)
# --------------------------------------------------------------        
bmtindicators_discount_rate <- as.numeric(as.character(eimat[8,i]))

Flyear[[currenttime]]@fleetsegments[[i]]@EC.BER  <- sum(as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$other.fix.cost)), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@capital.cost$depreciation)), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@capital.cost$interest)), na.rm=T) /  (1 -  ( sum( as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@labour.cost)), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$tot.var.cost)), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost$maint.cost)) , na.rm=T)/  as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@total.revenues))  ) )            
  
  # nuova da Monica
  # BER  = ( other.fix.cost + tot.cap.cost) /  (1 -  ( ( labour.cost + tot.var.cost + maint.cost )/  total.revenues  ) )    
  # BER =  (other fixed costs +annual depreciation+ opportunity cost of capital )/ (1-( labour cost+ + repair costs + tot  variable costs )/ current revenue])  
      
      # vecchia formula di Paolo  (labour.cost + fixed.cost + @capital.cost) / (1-(variable.cost / total.revenues) )   
      # Flyear[[currenttime]]@fleetsegments[[i]]@Economic.indicators[1] <- (Flyear[[currenttime]]@fleetsegments[[i]]@labour.cost + Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost[1] + Flyear[[currenttime]]@fleetsegments[[i]]@capital.cost[1]) / (1-(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost[1] / Flyear[[currenttime]]@fleetsegments[[i]]@total.revenues))
      
# --------------------------------------------------------------  # CR/BER (Current revenue to break-even revenue ratio) :
Flyear[[currenttime]]@fleetsegments[[i]]@EC.R_BER <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@total.revenues)) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@EC.BER ))
# --------------------------------------------------------------  # ROI (Return of Investment) :
Flyear[[currenttime]]@fleetsegments[[i]]@EC.ROI <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@profit)) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@capital.value ))
# --------------------------------------------------------------  # NPV discounted :
Flyear[[currenttime]]@fleetsegments[[i]]@EC.NPV.discounted <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@profit)) * ((1 + bmtindicators_discount_rate )^(-(currenttime-simperiod)))    
# --------------------------------------------------------------  # tot.landings.day :
Flyear[[currenttime]]@fleetsegments[[i]]@EC.tot.landings.day <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@total.landings)) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@DAYS.annual))
# --------------------------------------------------------------  # tot.landings.vessel :
Flyear[[currenttime]]@fleetsegments[[i]]@EC.tot.landings.vessel <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@total.landings)) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@VESSELS.annual))
# --------------------------------------------------------------  # tot.revenues.day :
Flyear[[currenttime]]@fleetsegments[[i]]@EC.tot.revenues.day <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@total.revenues)) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@DAYS.annual ))
# --------------------------------------------------------------  # tot.revenues.vessel:
Flyear[[currenttime]]@fleetsegments[[i]]@EC.tot.revenues.vessel <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@total.revenues)) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@VESSELS.annual))  
# --------------------------------------------------------------  # GVA.vessel (Gross Value Added) :    
Flyear[[currenttime]]@fleetsegments[[i]]@EC.GVA.vessel <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@gross.value.added )) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@VESSELS.annual ))
# --------------------------------------------------------------  # GCF.vessel :
Flyear[[currenttime]]@fleetsegments[[i]]@EC.GCF.vessel <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@gross.cash.flow)) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@VESSELS.annual))
# --------------------------------------------------------------  # profit.vessel : 
Flyear[[currenttime]]@fleetsegments[[i]]@EC.profit.vessel <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@profit)) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@VESSELS.annual))
# --------------------------------------------------------------  # net.profit.vessel :
Flyear[[currenttime]]@fleetsegments[[i]]@EC.net.profit.vessel <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@net.profit)) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@VESSELS.annual))
# --------------------------------------------------------------  # RoFTA (Return of fixed tangible assets) :
Flyear[[currenttime]]@fleetsegments[[i]]@EC.RoFTA <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@profit )) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@capital.value ))
# --------------------------------------------------------------  # vessel.util.ratio :
Flyear[[currenttime]]@fleetsegments[[i]]@EC.vessel.util.ratio <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@DAYS.annual)) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@MAXDAYS.average.annual  ))
# --------------------------------------------------------------  # RoFTA (Return of fixed tangible assets - net profit) :
Flyear[[currenttime]]@fleetsegments[[i]]@EC.NP.cap.value <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@net.profit)) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@capital.value  ))
    
      for (PERC in c(1:5)) {			
Flyear[[currenttime]]@fleetsegments[[i]]@average.wage.CI.perc[1,PERC] <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@labour.cost.CI.perc[1,PERC])) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@employment.CI.perc[1,PERC]))
# -------------------------------------------------------------- 
Flyear[[currenttime]]@fleetsegments[[i]]@total.cost.CI.perc[1,PERC] <- sum(as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$tot.var.cost[PERC] )),as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$tot.fix.cost[PERC] )),as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@labour.cost.CI.perc[1,PERC] )),as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@capital.cost$tot.cap.cost )), na.rm=T)

# subsidies
Flyear[[currenttime]]@fleetsegments[[i]]@other.income <- sum(as.numeric(as.character(Flyear[[1]]@fleetsegments[[i]]@other.income)) , as.numeric(as.character(tsmat2[i,currenttime-simperiod])), na.rm=T)  
      
Flyear[[currenttime]]@fleetsegments[[i]]@total.income <- sum(as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@total.revenues)), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@other.income)), na.rm=T)
      
Flyear[[currenttime]]@fleetsegments[[i]]@gross.value.added.CI.perc[1,PERC] <- sum(as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@total.revenues.CI.perc[1,PERC])), - as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$tot.var.cost[PERC])), - as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$tot.fix.cost[PERC])), - as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$maint.cost[PERC])) , na.rm=T) 

Flyear[[currenttime]]@fleetsegments[[i]]@gross.cash.flow.CI.perc[1,PERC] <- sum(as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@gross.value.added.CI.perc[PERC])), - as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@labour.cost.CI.perc[1,PERC])), na.rm=T)

Flyear[[currenttime]]@fleetsegments[[i]]@profit.CI.perc[1,PERC] <- sum(as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@gross.cash.flow.CI.perc[1,PERC])), - as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@capital.cost$tot.cap.cost)),na.rm=T)
      
#Flyear[[currenttime]]@fleetsegments[[i]]@taxes <- as.numeric(as.character(tsmat1[i,currenttime-simperiod]))

Flyear[[currenttime]]@fleetsegments[[i]]@net.profit.CI.perc[1,PERC] <- sum(as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@profit.CI.perc[1,PERC])),-as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@taxes)), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@other.income)) ,na.rm=T)
      
      bmtindicators_discount_rate <- as.numeric(as.character(eimat[8,i]))
      
      # Economic.indicators  :                  
      # BER (break.even.revenue) : 
      
      Flyear[[currenttime]]@fleetsegments[[i]]@EC.BER.CI.perc[1,PERC]  <- sum(as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$other.fix.cost[PERC])), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@capital.cost$depreciation)), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@capital.cost$interest)), na.rm=T) /  (1 -  ( sum( as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@labour.cost.CI.perc[1,PERC])), as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$tot.var.[PERC])) , as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost.CI.perc$maint.cost[PERC])) , na.rm=T)/  as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@total.revenues.CI.perc[1,PERC]))  ) )            
  
  # nuova da Monica
  # BER  = ( other.fix.cost + tot.cap.cost) /  (1 -  ( ( labour.cost + tot.var.cost + maint.cost )/  total.revenues  ) )    
  # BER =  (other fixed costs +annual depreciation+ opportunity cost of capital )/ (1-( labour cost+ + repair costs + tot  variable costs )/ current revenue])  
      
      # vecchia formula di Paolo  (labour.cost + fixed.cost + @capital.cost) / (1-(variable.cost / total.revenues) )   
      # Flyear[[currenttime]]@fleetsegments[[i]]@Economic.indicators[1] <- (Flyear[[currenttime]]@fleetsegments[[i]]@labour.cost + Flyear[[currenttime]]@fleetsegments[[i]]@fixed.cost[1] + Flyear[[currenttime]]@fleetsegments[[i]]@capital.cost[1]) / (1-(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost[1] / Flyear[[currenttime]]@fleetsegments[[i]]@total.revenues))
      
      # CR/BER (Current revenue to break-even revenue ratio) :
      Flyear[[currenttime]]@fleetsegments[[i]]@EC.R_BER.CI.perc[1,PERC] <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@total.revenues.CI.perc[1,PERC])) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@EC.BER.CI.perc[1,PERC] ))

      # ROI (Return of Investment) :
      Flyear[[currenttime]]@fleetsegments[[i]]@EC.ROI.CI.perc[1,PERC] <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@profit.CI.perc[1,PERC])) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@capital.value ))
   
      # NPV discounted :
      Flyear[[currenttime]]@fleetsegments[[i]]@EC.NPV.discounted.CI.perc[1,PERC] <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@profit.CI.perc[1,PERC])) * ((1 + bmtindicators_discount_rate )^(-(currenttime-simperiod)))    
      
      # tot.landings.day :
      Flyear[[currenttime]]@fleetsegments[[i]]@EC.tot.landings.day.CI.perc[1,PERC] <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@total.landings.CI.perc[1,PERC])) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@DAYS.annual))
      
      # tot.landings.vessel :
      Flyear[[currenttime]]@fleetsegments[[i]]@EC.tot.landings.vessel.CI.perc[1,PERC] <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@total.landings.CI.perc[1,PERC])) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@VESSELS.annual))
      
      # tot.revenues.day :
      Flyear[[currenttime]]@fleetsegments[[i]]@EC.tot.revenues.day.CI.perc[1,PERC] <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@total.revenues.CI.perc[1,PERC])) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@DAYS.annual ))
      
      # tot.revenues.vessel:
      Flyear[[currenttime]]@fleetsegments[[i]]@EC.tot.revenues.vessel.CI.perc[1,PERC] <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@total.revenues.CI.perc[1,PERC])) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@VESSELS.annual))  
      
      # GVA.vessel (Gross Value Added) :    
      Flyear[[currenttime]]@fleetsegments[[i]]@EC.GVA.vessel.CI.perc[1,PERC] <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@gross.value.added.CI.perc[1,PERC] )) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@VESSELS.annual ))
      
      # GCF.vessel :
      Flyear[[currenttime]]@fleetsegments[[i]]@EC.GCF.vessel.CI.perc[1,PERC] <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@gross.cash.flow.CI.perc[1,PERC])) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@VESSELS.annual))
      
      # profit.vessel : 
      Flyear[[currenttime]]@fleetsegments[[i]]@EC.profit.vessel.CI.perc[1,PERC] <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@profit.CI.perc[1,PERC])) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@VESSELS.annual))
      
      # net.profit.vessel :
      Flyear[[currenttime]]@fleetsegments[[i]]@EC.net.profit.vessel.CI.perc[1,PERC] <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@net.profit.CI.perc[1,PERC])) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@VESSELS.annual))
      
      # RoFTA (Return of fixed tangible assets) :
      Flyear[[currenttime]]@fleetsegments[[i]]@EC.RoFTA.CI.perc[1,PERC] <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@profit.CI.perc[1,PERC] )) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@capital.value ))
      
      # vessel.util.ratio :
#      Flyear[[currenttime]]@fleetsegments[[i]]@EC.vessel.util.ratio.CI.perc[1,PERC] <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@DAYS.annual)) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@MAXDAYS.average.annual  ))

      # RoFTA (Return of fixed tangible assets - net profit) :
      Flyear[[currenttime]]@fleetsegments[[i]]@EC.NP.cap.value.CI.perc[1,PERC] <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@net.profit.CI.perc[1,PERC])) / as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@capital.value  ))		
			
			}

    }
  return(Flyear)
  }
