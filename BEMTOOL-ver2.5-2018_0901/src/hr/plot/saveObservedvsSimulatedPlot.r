# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





saveObservedvsSimulatedPlot <- function(all_the_years, pressure_path) {

 PRESSIMPindicators <-  read.csv(pressure_path,sep=";",header=T)
 
for (ss in 1:length(BMT_SPECIES)) {
  ALADYM_flag <- as.logical(cfg[rownames(cfg) == paste("casestudy.S", ss, ".AladymSimulation", sep=""),1])
   if (ALADYM_flag)  {
associated_fleetsegment <- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ss, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

landing_table <- data.frame(matrix(nrow=length(all_the_years), ncol=((length(associated_fleetsegment_indices)+1)*3)))
chisquare_table <- data.frame(matrix(nrow=length(all_the_years), ncol=((length(associated_fleetsegment_indices)+1)*3)))

colnames(landing_table) <-  c(paste(c(BMT_FLEETSEGMENTS[associated_fleetsegment_indices], "Total"), "observed yield"), paste(c(BMT_FLEETSEGMENTS[associated_fleetsegment_indices], "Total"), "simulated yield"), paste(c(BMT_FLEETSEGMENTS[associated_fleetsegment_indices], "Total"), "relative percentage difference"))

colnames(chisquare_table) <-  c(paste(c(BMT_FLEETSEGMENTS[associated_fleetsegment_indices], "Total"), "observed yield"), paste(c(BMT_FLEETSEGMENTS[associated_fleetsegment_indices], "Total"), "simulated yield"), paste(c(BMT_FLEETSEGMENTS[associated_fleetsegment_indices], "Total"), "Ratios"))

  ffl_ord <- 1
for (ffl in 1:length(BMT_FLEETSEGMENTS)) {
  if (ffl %in% associated_fleetsegment_indices) {

Simulated_landings <- round(as.numeric(as.character(PRESSIMPindicators$Value[as.character(PRESSIMPindicators$Stock ) == BMT_SPECIES[ss] & as.character(PRESSIMPindicators$Variable ) == "Landing"  &  as.character(PRESSIMPindicators$Fleet_segment) == BMT_FLEETSEGMENTS[ffl]]  )), 0)

landing_table[,ffl_ord] <- Simulated_landings
chisquare_table[,ffl_ord] <- Simulated_landings

for (yea in 1:length(all_the_years)) {
landing_table[yea,((length(associated_fleetsegment_indices)+1) + ffl_ord)] <- round(sum(as.numeric(as.character(Interactionsyear[[yea]][[ss]]@interactions[[ffl_ord]]$historicalLandings))) / 1000 , 0)
chisquare_table[yea,((length(associated_fleetsegment_indices)+1) + ffl_ord)] <- round(sum(as.numeric(as.character(Interactionsyear[[yea]][[ss]]@interactions[[ffl_ord]]$historicalLandings))) / 1000 , 0)
}

obs <- landing_table[,((length(associated_fleetsegment_indices)+1) + ffl_ord)] 
sim <- landing_table[,ffl_ord]
landing_table[,((length(associated_fleetsegment_indices)+1)*2 + ffl_ord)] <- round((sim-obs)/obs * 100, 2) 
chisquare_table[,((length(associated_fleetsegment_indices)+1)*2 + ffl_ord)] <- ((obs - sim)^2) /obs 
 ffl_ord <- ffl_ord + 1
}
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

colnames_temp<-  c(c(BMT_FLEETSEGMENTS[associated_fleetsegment_indices], "Total"), c(BMT_FLEETSEGMENTS[associated_fleetsegment_indices], "Total"), c(BMT_FLEETSEGMENTS[associated_fleetsegment_indices], "Total"))
landing_table <- cbind(all_the_years, landing_table)
chisquare_table <- data.frame(cbind(all_the_years, chisquare_table))
colnames(landing_table) <- c("Year", colnames_temp)
colnames(chisquare_table) <- c("Year", colnames_temp)

landing_table_toplot <- landing_table
chisquare_table_toplot <- chisquare_table

 colnames_temp<- colnames(landing_table)
 landing_table <- rbind(colnames_temp, landing_table)
 
  colnames_temp<- colnames(chisquare_table)
 chisquare_table <- data.frame(rbind(colnames_temp, chisquare_table))
 
 landing_table<- rbind(c("", "Simulated Yield [tons]", rep("", length(associated_fleetsegment_indices)), "Observed Yield [tons]", rep("", length(associated_fleetsegment_indices)), "Relative difference [%]",  rep("", length(associated_fleetsegment_indices)) ), landing_table)
 
  chisquare_table<- data.frame(rbind(c("", "Simulated Yield [tons]", rep("", length(associated_fleetsegment_indices)), "Observed Yield [tons]", rep("", length(associated_fleetsegment_indices)), "Ratios",  rep("", length(associated_fleetsegment_indices)) ), chisquare_table) )

  chisquare_table_toplot <- chisquare_table_toplot[, colnames(chisquare_table_toplot) != "Total"]
  chisquare_table <- chisquare_table[, as.character(chisquare_table[2,]) != "Total"]
  
  if (length(associated_fleetsegment_indices) > 1) {
  sum_of_fleets <- colSums(chisquare_table_toplot[c(1:(simperiod)), c((length(associated_fleetsegment_indices)*2+2):(length(associated_fleetsegment_indices)*3+1))])
  } else {
  sum_of_fleets <- chisquare_table_toplot[c(1:(simperiod)), (length(associated_fleetsegment_indices)*2+2)]
  }
  
 if (ss == 1) {
    chisquares <- list(sum_of_fleets) 
 } else {
    chisquares <- c(chisquares, list(sum_of_fleets) )
 }
 
  table_path <- paste(casestudy_path, "/Diagnosis/ALADYM/", casestudy_name, " - Simulated vs Observed Yield - ", BMT_SPECIES[ss],".csv", sep="") 
  write.table(landing_table, table_path, col.names=FALSE, row.names=FALSE, sep=";")

    ffl_ord <- 1
for (ffl in 1:length(BMT_FLEETSEGMENTS)) {
  if (ffl %in% associated_fleetsegment_indices) {
  
 plot_path <- paste(casestudy_path, "/Diagnosis/ALADYM/", casestudy_name, " - Simulated vs Observed Yield ", BMT_FLEETSEGMENTS[ffl] ," - ", BMT_SPECIES[ss],".jpg", sep="")
  plot_title <- paste("Simulated vs Observed Yield - ",BMT_SPECIES[ss], sep="")
  plot_title_sub <- paste( casestudy_name, " simulation [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
  jpeg(file=plot_path, width=21, height=21, bg="white", units="cm",res=200)
  
par(mar=c(6, 7, 11, 7))  # c(bottom, left, top, right)
   
obs_toplot <- landing_table_toplot[,((length(associated_fleetsegment_indices)+1) + ffl_ord)+1] 
sim_toplot <- landing_table_toplot[,ffl_ord+1]  
all_ <- c(sim_toplot, obs_toplot)                                                                                                                       
  
 if (simperiod > 1) {
  plot(as.numeric(as.character(landing_table_toplot$Year)),sim_toplot,type="l",xlab="Year",ylab="Yield [tons]", ylim=c(0,(max(all_)+max(all_)*0.3)),axes=F, col="black", lwd=2, cex.axis=1.5, cex.lab=1.5, cex.main=1.8, main=plot_title)
  } else {
   plot(as.numeric(as.character(landing_table_toplot$Year)),sim_toplot,type="p",xlab="Year",ylab="Yield [tons]", ylim=c(0,(max(all_)+max(all_)*0.3)),axes=F, col="black", pch=19, cex=2, main=plot_title, cex.axis=1.5, cex.lab=1.5, cex.main=1.8) 
  }
  points(as.numeric(as.character(landing_table_toplot$Year)),obs_toplot, col="blue", pch=19, cex=2)        
  axis(1, at=as.numeric(as.character(landing_table_toplot$Year)), cex.axis=1.5)
  axis(2, cex.axis=1.5)
  mtext(plot_title_sub, 3, line=1, cex=1.3)
  mtext(BMT_FLEETSEGMENTS[ffl], 3, line=3, cex=1.6)
   mtext( BMT_sw_version,cex=0.6,side=4,outer=FALSE)
  box()                                                           #pch=16,
  legend("topright", c("Observed", "Simulated"), border="white", lty = c(-1,1), lwd = c(-1,2), pch = c(16,-1),  col=c("blue", "black"), cex=1.3, bty="n")
#title(plot_title) 

  dev.off()
 ffl_ord <- ffl_ord + 1
}
}

}

}


chis_tab <- data.frame(matrix(ncol=3, nrow=0))
colnames(chis_tab) <- c("Stock", "Fleet_segment", "Chi_squared")
for (ss in 1:length(BMT_SPECIES)) {
      ALADYM_flag <- as.logical(cfg[rownames(cfg) == paste("casestudy.S", ss, ".AladymSimulation", sep=""),1])
      if (ALADYM_flag)  {
associated_fleetsegment <- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ss, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

to_add <- data.frame(cbind(rep(BMT_SPECIES[ss], (length(associated_fleetsegment_indices)+1)), cbind(c(BMT_FLEETSEGMENTS[associated_fleetsegment_indices], "ALL"), c(chisquares[[ss]] , sum(chisquares[[ss]])))) )
colnames(to_add) <- c("Stock", "Fleet_segment", "Chi_squared")
chis_tab <- rbind(chis_tab, to_add)
}

}

table_path_chi <- paste(casestudy_path, "/Diagnosis/", casestudy_name, " - Chi-squared.csv", sep="") 
write.table(chis_tab, table_path_chi, row.names=FALSE, sep=";")
            
}
  
#saveObservedvsSimulatedPlot(years, name_pressind)  
                        
