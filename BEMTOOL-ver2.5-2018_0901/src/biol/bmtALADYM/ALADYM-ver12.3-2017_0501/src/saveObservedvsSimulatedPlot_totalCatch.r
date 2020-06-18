# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




saveObservedvsSimulatedPlot_totalCatch <- function() {

all_the_years <- years
# Simulated <-  read.csv(paste(ALADYM_home, "/Tables/Production.csv", sep=""),sep=";",header=T)  PRODUCTION_table

Simulated <-  read.csv(PRODUCTION_table,sep=";",header=T) 

if (!IN_BEMTOOL) {
BMT_SPECIES <-new_aldPopulation@scientific_name
}
 
associated_fleetsegment_indices <- c(1:length(FLEETSEGMENTS_names))

landing_table <- data.frame(matrix(nrow=length(years), ncol=((length(associated_fleetsegment_indices)+1)*3)))
discard_table <- data.frame(matrix(nrow=length(years), ncol=((length(associated_fleetsegment_indices)+1)*3)))
catch_table <- data.frame(matrix(nrow=length(years), ncol=((length(associated_fleetsegment_indices)+1)*3)))
chisquare_table <- data.frame(matrix(nrow=length(years), ncol=((length(associated_fleetsegment_indices)+1)*3)))

colnames(catch_table) <-  c(paste(c( FLEETSEGMENTS_names, "Total"), "observed landing"), paste(c(FLEETSEGMENTS_names, "Total"), "simulated landing"), paste(c(FLEETSEGMENTS_names, "Total"), "relative percentage difference"))

colnames(chisquare_table) <-  c(paste(c(FLEETSEGMENTS_names, "Total"), "observed landing"), paste(c(FLEETSEGMENTS_names, "Total"), "simulated landing"), paste(c(FLEETSEGMENTS_names, "Total"), "Ratios"))

 # ffl_ord <- 1
for (ffl in 1:length(FLEETSEGMENTS_names)) {
 # if (ffl %in% associated_fleetsegment_indices) {

 if (length(FLEETSEGMENTS_names) != 1) {
# Simulated_landings <- round(as.numeric(as.character(Simulated[,(4 + ffl)], 0)))
Simulated_landings <- round(as.numeric(as.character(Simulated[,(7+(length(FLEETSEGMENTS_names)*3)+ffl)])), 2)
Simulated_discards <- round(as.numeric(as.character(Simulated[,(10+(length(FLEETSEGMENTS_names)*6)+ffl)])), 2)
} else {
Simulated_landings <- round(as.numeric(as.character(Simulated[,7])), 2)
Simulated_discards <- round(as.numeric(as.character(Simulated[,10])), 2)
}

total_catch <- data.frame(matrix(0, nrow=2, ncol=length(Simulated_landings) ))
total_catch [1,]  <- Simulated_landings
total_catch [2,]  <- Simulated_discards

Simulated_catches <- colSums(total_catch, na.rm=T) 

catch_table[,ffl] <- Simulated_catches
chisquare_table[,ffl] <- Simulated_catches

prod_data <- get_production_data()

ObsProduction_temp = prod_data[prod_data$Gear == FLEETSEGMENTS_names[ffl],] 

ObsProduction_temp <- ObsProduction_temp[2:nrow(ObsProduction_temp),]
ObsProduction_temp$year <- 1
for (yea_t in 1:length(years)) {
ObsProduction_temp$year[as.numeric(as.character(ObsProduction_temp$Month))/12 > (yea_t-1) & as.numeric(as.character(ObsProduction_temp$Month))/12 <= (yea_t)] <- years[yea_t]
} 

ObsProduction = aggregate(as.numeric(as.character(ObsProduction_temp$Production)), by=list(ObsProduction_temp$year), FUN="sum")

ObsProduction <- as.numeric(as.character(ObsProduction$x))

ObsProduction <- ObsProduction/1000



if (!all(is.na(Simulated_discards)))  {
Simulated_discards[is.na(Simulated_discards)] <- 0

discard_table[,ffl] <- Simulated_discards
chisquare_table[,ffl] <- Simulated_discards

disc_data <- get_discard_data()

ObsProduction_temp_disc = disc_data[disc_data$Gear == FLEETSEGMENTS_names[ffl],] 

ObsProduction_temp_disc <- ObsProduction_temp_disc[2:nrow(ObsProduction_temp_disc),]
ObsProduction_temp_disc$year <- 1
for (yea_t in 1:length(years)) {
ObsProduction_temp_disc$year[as.numeric(as.character(ObsProduction_temp_disc$Month))/12 > (yea_t-1) & as.numeric(as.character(ObsProduction_temp_disc$Month))/12 <= (yea_t)] <- years[yea_t]
} 

ObsProduction_disc = aggregate(as.numeric(as.character(ObsProduction_temp_disc$Discard)), by=list(ObsProduction_temp_disc$year), FUN="sum")

ObsProduction_disc <- as.numeric(as.character(ObsProduction_disc$x))

ObsProduction_disc <- ObsProduction_disc/1000

 }












#ObsProduction = ifelse (ObsProduction_temp$Unit[1]=="kg",ObsProduction/1000,ObsProduction)

for (yea in 1:length(years)) {

landing_table[yea,((length(associated_fleetsegment_indices)+1) + ffl)] <- round(ObsProduction[yea],2)
chisquare_table[yea,((length(associated_fleetsegment_indices)+1) + ffl)] <- round(ObsProduction[yea],2)

}

obs <- landing_table[,((length(associated_fleetsegment_indices)+1) + ffl)] 
sim <- landing_table[,ffl]
landing_table[,((length(associated_fleetsegment_indices)+1)*2 + ffl)] <- round((sim-obs)/obs * 100, 2) 
chisquare_table[,((length(associated_fleetsegment_indices)+1)*2 + ffl)] <- ((obs - sim)^2) /obs 
 #ffl <- ffl + 1
#}
}


if (length(associated_fleetsegment_indices) >1) {
landing_table[,(length(associated_fleetsegment_indices)+1)] <- rowSums(landing_table[,1:(length(associated_fleetsegment_indices))])
landing_table[,(length(associated_fleetsegment_indices)*2+2)] <- rowSums(landing_table[,(length(associated_fleetsegment_indices)+2):(length(associated_fleetsegment_indices)*2+1)])

chisquare_table[,(length(associated_fleetsegment_indices)+1)] <- rowSums(chisquare_table[,1:(length(associated_fleetsegment_indices))])
chisquare_table[,(length(associated_fleetsegment_indices)*2+2)] <- rowSums(chisquare_table[,(length(associated_fleetsegment_indices)+2):(length(associated_fleetsegment_indices)*2+1)])

} else {
landing_table[,(length(associated_fleetsegment_indices)+1)] <- landing_table[,1:(length(associated_fleetsegment_indices))]
landing_table[,(length(associated_fleetsegment_indices)*2+2)] <- landing_table[,(length(associated_fleetsegment_indices)+2):(length(associated_fleetsegment_indices)*2+1)]

chisquare_table[,(length(associated_fleetsegment_indices)+1)] <- chisquare_table[,1:(length(associated_fleetsegment_indices))]
chisquare_table[,(length(associated_fleetsegment_indices)*2+2)] <- chisquare_table[,(length(associated_fleetsegment_indices)+2):(length(associated_fleetsegment_indices)*2+1)]
}

obs <- landing_table[,(length(associated_fleetsegment_indices)*2+2)]
sim <- landing_table[,(length(associated_fleetsegment_indices)+1)]
landing_table[,(length(associated_fleetsegment_indices)*3+3)] <- round((sim-obs)/obs * 100, 2)  

chisquare_table[,(length(associated_fleetsegment_indices)*3+3)] <- "" 

colnames_temp<-  c(c(FLEETSEGMENTS_names, "Total"), c(FLEETSEGMENTS_names, "Total"), c(FLEETSEGMENTS_names, "Total"))
landing_table <- cbind(years, landing_table)
chisquare_table <- data.frame(cbind(years, chisquare_table))
colnames(landing_table) <- c("Year", colnames_temp)
colnames(chisquare_table) <- c("Year", colnames_temp)

landing_table_toplot <- landing_table
chisquare_table_toplot <- chisquare_table

 colnames_temp<- colnames(landing_table)
 landing_table <- rbind(colnames_temp, landing_table)
 
  colnames_temp<- colnames(chisquare_table)
 chisquare_table <- data.frame(rbind(colnames_temp, chisquare_table))
 
 landing_table<- rbind(c("", "Simulated Landing [tons]", rep("", length(associated_fleetsegment_indices)), "Observed Landing [tons]", rep("", length(associated_fleetsegment_indices)), "Relative difference [%]",  rep("", length(associated_fleetsegment_indices)) ), landing_table)
 
  chisquare_table<- data.frame(rbind(c("", "Simulated Landing [tons]", rep("", length(associated_fleetsegment_indices)), "Observed Landing [tons]", rep("", length(associated_fleetsegment_indices)), "Ratios",  rep("", length(associated_fleetsegment_indices)) ), chisquare_table) )

  chisquare_table_toplot <- chisquare_table_toplot[, colnames(chisquare_table_toplot) != "Total"]
  chisquare_table <- chisquare_table[, as.character(chisquare_table[2,]) != "Total"]
  
  if (length(associated_fleetsegment_indices) > 1) {
  sum_of_fleets <- colSums(chisquare_table_toplot[c(1:length(years)), c((length(associated_fleetsegment_indices)*2+2):(length(associated_fleetsegment_indices)*3+1))], na.rm=T)
  } else {
  sum_of_fleets <- chisquare_table_toplot[c(1:length(years)), (length(associated_fleetsegment_indices)*2+2)]
  }
  
# if (ss == 1) {
    chisquares <- list(sum_of_fleets) 
# } else {
#    chisquares <- c(chisquares, list(sum_of_fleets) )
# }
 
  #table_path <- paste(casestudy_path, "\\Diagnosis\\ALADYM\\[", casestudy_name, "] Simulated vs Observed Landing - ", BMT_SPECIES[ss],".csv", sep="") 
  write.table(landing_table, SIMULATEDVSOBSERVED_LANDING_table, col.names=FALSE, row.names=FALSE, sep=";")

   
for (ffl in 1:length(FLEETSEGMENTS_names)) {
 ffl_ord <- ffl
  #if (ffl %in% associated_fleetsegment_indices) {
  
 plot_path <- paste(SIMULATEDVSOBSERVED_LANDING_graph,"_", FLEETSEGMENTS_names[ffl], ".jpg", sep="")
 
  plot_title <- paste("Simulated vs Observed Landing - " ,BMT_SPECIES[ALADYM_spe], sep="")
  plot_title_sub <- paste(" simulation [", years[1], "-", years[length(years)],"]", sep="")
  jpeg(file=plot_path, width=21, height=21, bg="white", units="cm",res=200)
  
par(mar=c(6, 7, 11, 7))  # c(bottom, left, top, right)
   
obs_toplot <- landing_table_toplot[,((length(FLEETSEGMENTS_names)+1) + ffl_ord)+1] 
sim_toplot <- landing_table_toplot[,ffl_ord+1]  
all_ <- c(sim_toplot, obs_toplot)                                                                                                                       

min_y <- 0
max_y <- max(all_, na.rm=T)

 if (length(years) > 1) {
  plot(as.numeric(as.character(landing_table_toplot$Year)),sim_toplot,type="l",xlab="Year",ylab="Landing [tons]", ylim=c(min_y,max_y + (max_y-min_y)*0.3),axes=F, col="black", lwd=2, cex.axis=1.5, cex.lab=1.5, cex.main=1.8, main=plot_title)
  } else {
   plot(as.numeric(as.character(landing_table_toplot$Year)),sim_toplot,type="p",xlab="Year",ylab="Landing [tons]", ylim=c(min_y,max_y + (max_y-min_y)*0.3),axes=F, col="black", pch=19, cex=2, main=plot_title, cex.axis=1.5, cex.lab=1.5, cex.main=1.8)
  }
  points(as.numeric(as.character(landing_table_toplot$Year)),obs_toplot, col="blue", pch=19, cex=2)        
  axis(1, at=as.numeric(as.character(landing_table_toplot$Year)), cex.axis=1.5)
  axis(2, cex.axis=1.5)
  mtext(plot_title_sub, 3, line=1, cex=1.3)
  mtext(FLEETSEGMENTS_names[ffl], 3, line=3, cex=1.6)
  box()                                                           #pch=16,
  legend("topright", c("Observed", "Simulated"), border="white", lty = c(-1,1), lwd = c(-1,2), pch = c(16,-1),  col=c("blue", "black"), cex=1.3, bty="n")
  
#saveObservedvsSimulatedPlot(years, name_pressind)  
                        

               mtext( GLO$ThisIsVersion,cex=1,side=4,outer=FALSE) #  
#title(plot_title) 

  dev.off()

#}
}

#}

#}


chis_tab <- data.frame(matrix(ncol=3, nrow=0))
colnames(chis_tab) <- c("Stock", "Fleet_segment", "Chi_squared")
this_species <- BMT_SPECIES[ALADYM_spe]
for (ss in 1:length(this_species)) {
      #ALADYM_flag <- as.logical(cfg[rownames(cfg) == paste("casestudy.S", ss, ".AladymSimulation", sep=""),1])
     # if (ALADYM_flag)  {
#associated_fleetsegment <- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ss, ".associatedFleetsegment", sep=""), ])   
#associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
#associated_fleetsegment_indices <- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)
                                                                                                                                                      
#for (ffl_ord in 1:length(FLEETSEGMENTS_names)) {                                                                                                        
#chisquares[[ss]] ,                             # new_aldPopulation@scientific_name
to_add <- data.frame(cbind(rep(this_species[ss], (length(FLEETSEGMENTS_names)+1)), cbind(c(FLEETSEGMENTS_names, "ALL"), c( chisquares[[ss]], sum(chisquares[[ss]])))) )
colnames(to_add) <- c("Stock", "Fleet_segment", "Chi_squared")
chis_tab <- rbind(chis_tab, to_add)
#}
}
#}

write.table(chis_tab, CHISQUARED_LANDING_table, row.names=FALSE, sep=";")
            
}
  
#saveObservedvsSimulatedPlot(years, name_pressind)  
                        