# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





# runEcon.r - Bemtool economic indicators in the simulation period
# Author: Paolo Accadia

setFleet<- function(Fyear) {

 # Fyear = Fleetyear

#for (y_int in 1:foreperiod) {
#    for (m in 1:m_stock) {
#        Fyear[[simperiod+y_int]]@import.weight[m] <- as.numeric(as.character(pmat4[[m,y_int]]))
#    }
#    for (n_int in 1:n_fleet) {
#        Fyear[[simperiod+y_int]]@fleetsegments[[n_int]]@fuel.price <- as.numeric(as.character(vcvec[[y_int]]))
#        Fyear[[simperiod+y_int]]@fleetsegments[[n_int]]@other.income <- as.numeric(as.character(tsmat1[[n_int,y_int]]))
#        Fyear[[simperiod+y_int]]@fleetsegments[[n_int]]@taxes <- as.numeric(as.character(tsmat2[[n_int,y_int]]))
#    } 
#}  
                                   
# original from PAccadia: 
# cfg6 <- read.csv(file=paste(getwd(), as.character(cfg[rownames(cfg) == "casestudy.econIndFun",2]), sep=""), sep=";", na.strings = "NA", header=FALSE) 
cfg6 <- read.csv(file=as.character(cfg[rownames(cfg) == "casestudy.TimeSeries.economicData",1]), sep=";", na.strings = "NA", header=FALSE) 
nm <- as.character(cfg6[,1])
empty_indices <- which(nm!="")
nm <- nm[empty_indices]
cfg6 <- cfg6[,2:ncol(cfg6)]
rownames(cfg6)[empty_indices] <- nm

for (n_int in 1:n_fleet) {     
       for (y_int in 1:simperiod) {         
           index_col <-  (n_int-1)*simperiod  + y_int
           Fyear[[y_int]]@fleetsegments[[n_int]]@employment <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.employment",index_col]))
           Fyear[[y_int]]@fleetsegments[[n_int]]@employment.CI.perc[1,] <-  as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.employment",index_col]))
           
					 Fyear[[y_int]]@fleetsegments[[n_int]]@capital.value <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.capitalvalue",index_col]))
           Fyear[[y_int]]@fleetsegments[[n_int]]@capital.value.CI.perc[1,]   <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.capitalvalue",index_col]))
           
					 Fyear[[y_int]]@fleetsegments[[n_int]]@MAXDAYS.average.annual <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.maxavgseadays",index_col]))
           
           Fyear[[y_int]]@fleetsegments[[n_int]]@total.landings <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.totallandings",index_col]))
           Fyear[[y_int]]@fleetsegments[[n_int]]@total.landings.CI.perc[1,]  <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.totallandings",index_col]))
					 
					 Fyear[[y_int]]@fleetsegments[[n_int]]@total.revenues.landings <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.totalrevenues",index_col]))
           Fyear[[y_int]]@fleetsegments[[n_int]]@total.revenues.landings.CI.perc[1,]  <-  as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.totalrevenues",index_col]))
                      
           Fyear[[y_int]]@fleetsegments[[n_int]]@variable.cost$fuel.cost <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.fuelcosts",index_col])) 
           Fyear[[y_int]]@fleetsegments[[n_int]]@variable.cost$commercial.cost <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.commercialcosts",index_col]))
           Fyear[[y_int]]@fleetsegments[[n_int]]@variable.cost$other.var.cost <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.othervariablecosts",index_col]))
           Fyear[[y_int]]@fleetsegments[[n_int]]@variable.cost$tot.var.cost <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.totalvariablecosts",index_col]))
           
           Fyear[[y_int]]@fleetsegments[[n_int]]@variable.cost.CI.perc$fuel.cost <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.fuelcosts",index_col])) 
           Fyear[[y_int]]@fleetsegments[[n_int]]@variable.cost.CI.perc$commercial.cost <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.commercialcosts",index_col]))
           Fyear[[y_int]]@fleetsegments[[n_int]]@variable.cost.CI.perc$other.var.cost <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.othervariablecosts",index_col]))
           Fyear[[y_int]]@fleetsegments[[n_int]]@variable.cost.CI.perc$tot.var.cost <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.totalvariablecosts",index_col]))
           
           Fyear[[y_int]]@fleetsegments[[n_int]]@fixed.cost$maint.cost <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.maintenancecosts",index_col]))
           Fyear[[y_int]]@fleetsegments[[n_int]]@fixed.cost$other.fix.cost <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.otherfixedcosts",index_col])) 
           Fyear[[y_int]]@fleetsegments[[n_int]]@fixed.cost$ess.cost <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.essentialcosts",index_col]))
           Fyear[[y_int]]@fleetsegments[[n_int]]@fixed.cost$avoid.main.cost <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.avoidablemaintenancecosts",index_col]))
           Fyear[[y_int]]@fleetsegments[[n_int]]@fixed.cost$unavoid.main.cost <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.unavoidablemaintenancecosts",index_col]))
           Fyear[[y_int]]@fleetsegments[[n_int]]@fixed.cost$tot.fix.cost <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.totalfixedcosts",index_col]))
           
           Fyear[[y_int]]@fleetsegments[[n_int]]@fixed.cost.CI.perc$maint.cost <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.maintenancecosts",index_col]))
           Fyear[[y_int]]@fleetsegments[[n_int]]@fixed.cost.CI.perc$other.fix.cost <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.otherfixedcosts",index_col])) 
           Fyear[[y_int]]@fleetsegments[[n_int]]@fixed.cost.CI.perc$ess.cost <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.essentialcosts",index_col]))
           Fyear[[y_int]]@fleetsegments[[n_int]]@fixed.cost.CI.perc$avoid.main.cost <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.avoidablemaintenancecosts",index_col]))
           Fyear[[y_int]]@fleetsegments[[n_int]]@fixed.cost.CI.perc$unavoid.main.cost <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.unavoidablemaintenancecosts",index_col]))
           Fyear[[y_int]]@fleetsegments[[n_int]]@fixed.cost.CI.perc$tot.fix.cost <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.totalfixedcosts",index_col]))
     
     
           Fyear[[y_int]]@fleetsegments[[n_int]]@labour.cost <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.labourcosts",index_col])) 
           Fyear[[y_int]]@fleetsegments[[n_int]]@labour.cost.CI.perc[1,] <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.labourcosts",index_col])) 
                      
           Fyear[[y_int]]@fleetsegments[[n_int]]@capital.cost$depreciation <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.depreciationcosts",index_col]))
           Fyear[[y_int]]@fleetsegments[[n_int]]@capital.cost$interest <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.opportunitycosts",index_col]))
           Fyear[[y_int]]@fleetsegments[[n_int]]@capital.cost$tot.cap.cost <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.totalcapitalcosts",index_col]))
           
           Fyear[[y_int]]@fleetsegments[[n_int]]@other.income <- as.numeric(as.character(cfg6[rownames(cfg6) == "casestudy.otherincome",index_col]))
           Fyear[[y_int]]@fleetsegments[[n_int]]@taxes <- 0
           Fyear[[y_int]]@fleetsegments[[n_int]]@investment$number <- 0
           Fyear[[y_int]]@fleetsegments[[n_int]]@investment$value <- 0
           Fyear[[y_int]]@fleetsegments[[n_int]]@technology <- 1
           
           for (m in 1:m_stock) {
#Fyear[[y_int]]@fleetsegments[[n_int]]@landing.weight[m] <- as.numeric(as.character(cfg6[rownames(cfg6) == paste("casestudy.landings.S", m,sep=""),index_col]))
Fyear[[y_int]]@fleetsegments[[n_int]]@revenues.landing[m] <- as.numeric(as.character(cfg6[rownames(cfg6) == paste("casestudy.revenues.S", m,sep=""),index_col]))  
Fyear[[y_int]]@fleetsegments[[n_int]]@revenues.landing.CI.perc[,m] <- as.numeric(as.character(cfg6[rownames(cfg6) == paste("casestudy.revenues.S", m,sep=""),index_col]))  
       
Fyear[[y_int]]@fleetsegments[[n_int]]@revenues.landed_discard[m] <- as.numeric(as.character(cfg6[rownames(cfg6) == paste("casestudy.revenues.discard.S", m,sep=""),index_col]))  
Fyear[[y_int]]@fleetsegments[[n_int]]@revenues.landed_discard.CI.perc[,m] <- as.numeric(as.character(cfg6[rownames(cfg6) == paste("casestudy.revenues.discard.S", m,sep=""),index_col]))  

Fyear[[y_int]]@fleetsegments[[n_int]]@revenues[m] <- sum(as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@revenues.landing[m] )), as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@revenues.landed_discard[m] )), na.rm=T)  
Fyear[[y_int]]@fleetsegments[[n_int]]@revenues.CI.perc[,m] <- as.numeric(as.character( Fyear[[y_int]]@fleetsegments[[n_int]]@revenues[m] ))  

           }    # end loop stocks

Fyear[[y_int]]@fleetsegments[[n_int]]@total.revenues.landed_discard <- sum(as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@revenues.landed_discard)) , na.rm=T)
Fyear[[y_int]]@fleetsegments[[n_int]]@total.revenues.landed_discard.CI.perc[1,]  <-  rowSums(Fyear[[y_int]]@fleetsegments[[n_int]]@revenues.landed_discard.CI.perc, na.rm=T)

Fyear[[y_int]]@fleetsegments[[n_int]]@total.revenues <- sum(as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@total.revenues.landings )), as.numeric(as.character(Fyear[[y_int]]@fleetsegments[[n_int]]@total.revenues.landed_discard )) , na.rm=T)
Fyear[[y_int]]@fleetsegments[[n_int]]@total.revenues.CI.perc[1,]  <-  colSums(rbind(Fyear[[y_int]]@fleetsegments[[n_int]]@total.revenues.landings.CI.perc[1,], Fyear[[y_int]]@fleetsegments[[n_int]]@total.revenues.landed_discard.CI.perc[1,]), na.rm=T)


                
      }   # end loop years
}   # end loop fleetsegments
  

######### INPUT MONTHLY DATA ON NUMBER, GT, KW AND SEADAYS

# original from PAccadia: 
#cfg2 <- read.csv(file=paste(getwd(), as.character(cfg[rownames(cfg) == "casestudy.econIndFun",3]), sep=""), sep=";", na.strings = "NA", header=FALSE)
#cfg3 <- read.csv(file=paste(getwd(), as.character(cfg[rownames(cfg) == "casestudy.econIndFun",4]), sep=""), sep=";", na.strings = "NA", header=FALSE)
#cfg4 <- read.csv(file=paste(getwd(), as.character(cfg[rownames(cfg) == "casestudy.econIndFun",5]), sep=""), sep=";", na.strings = "NA", header=FALSE)
#cfg5 <- read.csv(file=paste(getwd(), as.character(cfg[rownames(cfg) == "casestudy.fleDynFun",2]), sep=""), sep=";", na.strings = "NA", header=FALSE) 
vessels_ts <- read.csv(file=as.character(cfg[rownames(cfg) == "casestudy.TimeSeries.effort",1]), sep=";", na.strings = "NA", header=FALSE)     # vessels
averageDAYS_ts <- read.csv(file=as.character(cfg[rownames(cfg) == "casestudy.TimeSeries.effort",2]), sep=";", na.strings = "NA", header=FALSE)      #days
averageGT_ts <- read.csv(file=as.character(cfg[rownames(cfg) == "casestudy.TimeSeries.effort",3]), sep=";", na.strings = "NA", header=FALSE)     #gt
averageKW_ts <- read.csv(file=as.character(cfg[rownames(cfg) == "casestudy.TimeSeries.effort",4]), sep=";", na.strings = "NA", header=FALSE)     #kw


# [Monthly VESSELS file]	[Monthly DAYS.average file]	[Monthly GT.average file]	[Monthly KW.average file]

nm <- as.character(vessels_ts[,1])
empty_indices <- which(nm!="")
nm <- nm[empty_indices]
vessels_ts <- vessels_ts[,2:ncol(vessels_ts)]
rownames(vessels_ts)[empty_indices] <- nm

nm <- as.character(averageDAYS_ts[,1])
empty_indices <- which(nm!="")
nm <- nm[empty_indices]
averageDAYS_ts <- averageDAYS_ts[,2:ncol(averageDAYS_ts)]
rownames(averageDAYS_ts)[empty_indices] <- nm

nm <- as.character(averageGT_ts[,1])
empty_indices <- which(nm!="")
nm <- nm[empty_indices]
averageGT_ts <- averageGT_ts[,2:ncol(averageGT_ts)]
rownames(averageGT_ts)[empty_indices] <- nm

nm <- as.character(averageKW_ts[,1])
empty_indices <- which(nm!="")
nm <- nm[empty_indices]
averageKW_ts <- averageKW_ts[,2:ncol(averageKW_ts)]
rownames(averageKW_ts)[empty_indices] <- nm

for (n_int in 1:n_fleet) {     
       for (y_int in 1:simperiod) {         
           index_col <-  (n_int-1)*simperiod  + y_int
           #print(index_col) 
           for (month in 1:12) {                                 
              Fyear[[y_int]]@fleetsegments[[n_int]]@VESSELS[month] <- as.numeric(as.character(vessels_ts[rownames(vessels_ts) == paste("casestudy.month", month,sep=""),index_col]))
              Fyear[[y_int]]@fleetsegments[[n_int]]@GT.average[month] <- as.numeric(as.character(averageGT_ts[rownames(averageGT_ts) == paste("casestudy.month", month,sep=""),index_col]))
              Fyear[[y_int]]@fleetsegments[[n_int]]@KW.average[month] <- as.numeric(as.character(averageKW_ts[rownames(averageKW_ts) == paste("casestudy.month", month,sep=""),index_col])) 
              Fyear[[y_int]]@fleetsegments[[n_int]]@DAYS.average[month] <- as.numeric(as.character(averageDAYS_ts[rownames(averageDAYS_ts) == paste("casestudy.month", month,sep=""),index_col]))
           } # end loop month
      }   # end loop years
}   # end loop fleetsegments


# fixed on 27.01.2015
for (num_spe in 1:length(BMT_SPECIES)) {
    landingSpecies_ts <- read.csv(file=as.character(cfg[rownames(cfg) == "casestudy.TimeSeries.productionData",num_spe]), sep=";", na.strings = "NA", header=FALSE)   
    
    nm <- as.character(landingSpecies_ts[,1])
empty_indices <- which(nm!="")
nm <- nm[empty_indices]
landingSpecies_ts <- landingSpecies_ts[,2:ncol(landingSpecies_ts)]
rownames(landingSpecies_ts)[empty_indices] <- nm
    
for (n_int in 1:length(BMT_FLEETSEGMENTS)) {     
   for (y_int in 1:simperiod) {         
           index_col <-  (n_int-1)*simperiod  + y_int
           #print(index_col) 
           Fyear[[y_int]]@fleetsegments[[n_int]]@landing.weight[num_spe] <-  sum(as.numeric(as.character(landingSpecies_ts[5:16,index_col])))
            Fyear[[y_int]]@fleetsegments[[n_int]]@landing.weight.CI.perc[,num_spe] <-  sum(as.numeric(as.character(landingSpecies_ts[5:16,index_col])))
      }   # end loop years
}   # end loop fleetsegments
         
}



return(Fyear)
}
    