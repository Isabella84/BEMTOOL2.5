# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





# ---------------------- Create and add gear to the list
add_BMT_fleetsegments <- function(num_ORD_fleetsegm, num_fleet) {

# num_ORD_fleetsegm =  n_ord
# num_fleet =    n_int

lastAdded <- num_ORD_fleetsegm                     

for (yea in 1:simperiod) {
lands <- as.numeric(as.character(Interactionsyear[[yea]][[ALADYM_spe]]@interactions[[num_ORD_fleetsegm]]$historicalLandings ))
# print(lands)
if (yea==1) {
    productionMatr <- rbind(productionMatr, c(years[yea],lands[1], lands))  
} else {
    productionMatr <- rbind(productionMatr, c(years[yea],"", lands))  
}
}

colnames(productionMatr) <- c("year",	"seed",	MONTHS)
FleetList_simulation[[lastAdded]]@production.vector  <<-  productionMatr


prod_data <- get_production_data()
p_production_mat <- P_production_calc.gui(prod_data,(simperiod*12+1))

for (yea in 1:simperiod) {
# fes <- rep(1, 12)
if (yea==1) {
    fes <-  p_production_mat[ ( (yea-1)*12 +1) : ( (yea)*12 +1) , num_ORD_fleetsegm]
    pproductionMatr <- rbind(pproductionMatr, c(years[yea], fes))  
} else {
    fes <-  p_production_mat[ ( (yea-1)*12 +2) : ( (yea)*12 +1) , num_ORD_fleetsegm]
    pproductionMatr <- rbind(pproductionMatr, c(years[yea],"", fes))  
}
}

colnames(pproductionMatr) <- c("year",	"seed",	MONTHS)
 FleetList_simulation[[lastAdded]]@pproduction.vector   <<-  pproductionMatr

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

for (yea in 1:simperiod) {
vess <- as.numeric(as.character(Fleetyear[[yea]]@fleetsegments[[num_fleet]]@VESSELS ))
# print(vess)
if (yea==1) {
    vesselsMatr <- rbind(vesselsMatr, c(years[yea],vess[1], vess))  
} else {
    vesselsMatr <- rbind(vesselsMatr, c(years[yea],"", vess))  
}
}

colnames(vesselsMatr) <- c("year",	"seed",	MONTHS)
 FleetList_simulation[[lastAdded]]@vessels.vector   <<-  vesselsMatr
 



for (yea in 1:simperiod) {
days <- as.numeric(as.character(Fleetyear[[yea]]@fleetsegments[[num_fleet]]@DAYS.average ))
# print(vess)
if (yea==1) {
    daysMatr <- rbind(daysMatr, c(years[yea],days[1], days))  
} else {
    daysMatr <- rbind(daysMatr, c(years[yea],"", days))  
}
}
colnames(daysMatr) <- c("year",	"seed",	MONTHS)
 FleetList_simulation[[lastAdded]]@days.vector  <<-  daysMatr




for (yea in 1:simperiod) {
gts <- as.numeric(as.character(Fleetyear[[yea]]@fleetsegments[[num_fleet]]@GT.average ))
# print(vess)
if (yea==1) {
    gtMatr <- rbind(gtMatr, c(years[yea],gts[1], gts))  
} else {
    gtMatr <- rbind(gtMatr, c(years[yea],"", gts))  
}
}
colnames(gtMatr) <- c("year",	"seed",	MONTHS)
 FleetList_simulation[[lastAdded]]@gt.vector  <<-  gtMatr

                            
eff_data <- get_effort_data()
fishing_eff_result <- fact_calc.gui(eff_data,"N",all_years, forecast, NULL)
if (length(associated_fleetsegment_indices)==1) {
fishing_eff_result <- data.frame(matrix(fishing_eff_result[1:((simperiod*12)+1), ], ncol=1 ))
} else {
fishing_eff_result <- fishing_eff_result[1:((simperiod*12)+1), ]
}


                            
for (yea in 1:simperiod) {
# fes <- rep(1, 12)
fes <-  fishing_eff_result[ ( (yea-1)*12 +1) : ( (yea)*12 ) , num_ORD_fleetsegm]
if (yea==1) {
    fisheffMatr <- rbind(fisheffMatr, c(years[yea],fes[1], fes))  
} else {
    fisheffMatr <- rbind(fisheffMatr, c(years[yea],"", fes))  
}
}
colnames(fisheffMatr) <- c("year",	"seed",	MONTHS)
 FleetList_simulation[[lastAdded]]@fishingeffort.vector  <<-  fisheffMatr


for (yea in 1:simperiod) {
discs <- rep(0, 12) 
to_add <- data.frame(cbind(rep(years[yea], 12), cbind(MONTHS, cbind(discs,discs))) )
colnames(to_add) <- c("year" ,"month", "L50", "L75_L25")
if (yea==1) {
    seed_row <- data.frame(matrix(c("", "seed", 0,0), nrow=1))
    colnames(seed_row) <-  c("year" ,"month", "L50", "L75_L25")
    discMatr <- rbind(discMatr, seed_row)
}
    discMatr <- rbind(discMatr, to_add) 
}
discMatr[,3] <- as.numeric(as.character(discMatr[,3])) 
discMatr[,4] <- as.numeric(as.character(discMatr[,4])) 

FleetList_simulation[[lastAdded]]@discard.vector  <<-  discMatr
FleetList_simulation[[lastAdded]]@effort.datatype <<- "Effort data"

FleetList_simulation[[lastAdded]]@discard.survivability.calculation  <<- "N"
FleetList_simulation[[lastAdded]]@escape.survivability.calculation  <<- "N"

FleetList_simulation[[lastAdded]]@selectivity.mode  <<- "P"

 effF <- data.frame(matrix("", nrow=1, ncol=3), stringsAsFactors=F)
   colnames(effF) <- c("relationship_type", "a", "b")

   effF$relationship_type[1] <- "L" #INP$Forecast_reduction[num_ORD_fleetsegm]
      effF$a[1] <- 0
         effF$b[1] <- 1

FleetList_simulation[[lastAdded]]@EffortF.relationship <<-  effF #INP$Forecast_reduction[num_ORD_fleetsegm]



#lista_fore <- c(lista_fore, FleetList_simulation[[lastAdded]])

#return(list(lista, lista_fore))
}
