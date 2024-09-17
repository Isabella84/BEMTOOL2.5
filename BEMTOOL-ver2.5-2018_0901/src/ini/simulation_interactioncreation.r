# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





# #############################################################################################
# STOCKS and CATCH INITIALIZATION in INTERACTION
# #############################################################################################

for (y in 1:simperiod) {

# ---------------------------------------------------------------------------------------------------
# Initialization of Stocks
# --------------------------------------------------------------------------------------------------- 

# read number of species from cfg and names and do a loop with the following actions:
for (m in 1:length(BMT_SPECIES)) {
# create a couple of exploited and unexploited Stock object to be updated either directly from stock assessment or from Aladym simulation
#print(paste("creating couple of bmtStock for EXPLOITED and UNEXPLOITED [", BMT_SPECIES[m], "]...", sep=""),  quote=FALSE) 
source(suppressWarnings(paste(getwd(), "/src/ini/stocks.ini.r", sep="")))
if (m==1) {
Stocks <- list(new_Stocks) 
} else {
Stocks <- c(Stocks, list(new_Stocks)) 
} 
     
# ---------------------------------------------------------------------------------------------------
# Initialization of Landings, Discards and Catches
# --------------------------------------------------------------------------------------------------- 
# create the objects Landings, Discards and Catches for m-th species for the associated fleetsegments 
# the number of associated fleet segments varies on the basis of the species 
# NOTE: n_fleet is the number of associated fleet segments  
  # ( see the number of [casestudy.S1.associatedFleetsegment] )
associated_fleetsegment <- as.vector(cfg[rownames(cfg)==paste("casestudy.S", m, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

 nord_fl<- 1
for (n_fl in associated_fleetsegment_indices) {
# create the objects Catches
# print(paste("creating catches (species-fleetsegment associations) [", BMT_SPECIES[m], "][", BMT_FLEETSEGMENTS[n_fl] , "]...", sep=""),  quote=FALSE) 
source(suppressWarnings(paste(getwd(), "/src/ini/catches.ini.r", sep="")))

if (nord_fl==1) {
CatchesAssociations <- list(new_CatchesAssociation) # for one fleetsegment 
} else {
CatchesAssociations <- c(CatchesAssociations, list(new_CatchesAssociation)) # for one fleetsegment       
} 

nord_fl<-nord_fl+1
}

# ---------------------------------------------------------------------------------------------------
# Creation of Interaction for the m species
# --------------------------------------------------------------------------------------------------- 
# for each m species an Interaction object is created containing slots to archive information for all the process in the model
# print(paste("creating FleetStock interaction [", BMT_SPECIES[m], "]...", sep=""),  quote=FALSE) 
source(paste(getwd(), "/src/ini/interaction.ini.r", sep=""))

if (m==1) {
 FleetStockInteractions <- list(new_fleetstock_interaction)
} else {
 FleetStockInteractions <- c(FleetStockInteractions, new_fleetstock_interaction)
}


} # end loop species

if (y==1) {
# store the Interactions at time t=1 
Interactionsyear  <<- list(FleetStockInteractions)
} else {
# store the Interactions at time t>1 
Interactionsyear  <<- c(Interactionsyear, list(FleetStockInteractions))
}

}
