# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





saveYield_barPlot <- function(all_the_years, pressure_path) {

# BIOindicators <- getBiologicalTable(all_the_years)
PRESSIMPindicators <- read.csv(pressure_path, sep=";",header=T)
Yield <- PRESSIMPindicators[as.character(PRESSIMPindicators$Variable) == "Catch", ]

fleets_to_plot <- c(as.character(unique(PRESSIMPindicators$Fleet_segment)))

for (ff in 1:length(fleets_to_plot)) {

  plot_title <- paste("Catch by stock - ", ifelse(fleets_to_plot[ff] == "ALL", "Overall", fleets_to_plot[ff]) , sep="")

if (phase =="SIMULATION") {                                              
  plot_path <- paste(casestudy_path, "/Diagnosis/Biological Pressure Impact/", casestudy_name, " - Catch by stock ", fleets_to_plot[ff], ".jpg", sep="")
     plot_title_sub <- paste( casestudy_name, " simulation [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
} else {
if (!MEY_CALCULATION) {
  plot_path <- paste(casestudy_path, "/",harvest_rule_id,"/Biological Pressure Impact/", casestudy_name, " - Catch by stock ", fleets_to_plot[ff], " ", harvest_rule_id,".jpg", sep="")
         plot_title_sub <- paste( casestudy_name, " ",harvest_rule_id , " [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
  } else {
    plot_path <- paste(casestudy_path, "/MEY calculation/",harvest_rule_id,"/Biological Pressure Impact/", casestudy_name, " - Catch by stock ", fleets_to_plot[ff], " ", harvest_rule_id,".jpg", sep="")
       plot_title_sub <- paste(casestudy_name, " ",harvest_rule_id , " [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
  }
}

if (fleets_to_plot[ff] == "ALL") {
Yield_temp <- Yield
} else {
Yield_temp <- Yield[as.character(Yield$Fleet_segment) == fleets_to_plot[ff],]
}

total_yield <- aggregate(as.numeric(as.character(Yield_temp$Value)), by=list(Yield_temp$Stock, Yield_temp$Year), FUN="sum")
colnames(total_yield) <- c("Stock", "Year", "Yield")

null_species <- which(!(BMT_SPECIES %in% as.character(unique(total_yield$Stock)) ) )

for (nsp in null_species) {
to_add <- data.frame(cbind(rep(BMT_SPECIES[nsp], length(all_the_years)), cbind(all_the_years, rep(0, length(all_the_years)) )    ) )
colnames(to_add) <- c("Stock", "Year", "Yield")
total_yield <- rbind(total_yield , to_add )
}

total_yield$Yield <-  as.numeric(as.character(total_yield$Yield ))
 
counts <- table(as.character(total_yield$Stock), as.character(total_yield$Year))

for (nr in 1:nrow(counts)) {
    for (nc in 1:ncol(counts)) {
        counts[nr, nc] <- total_yield$Yield[as.character(total_yield$Stock) == rownames(counts)[nr] & as.character(total_yield$Year) == colnames(counts)[nc]]
    }
}

if (all(is.finite(counts))) {  

jpeg(file=plot_path, width=21, height=21, bg="white", units="cm",res=200)
par(mar=c(6, 7, 8, 7))  # c(bottom, left, top, right) 

# rownames(counts) <- BMT_SPECIES         #, Yield$Fleet_segment
barplot(counts, main=plot_title, xlab="Year", col=c(4:(nrow(counts)+3)),legend = rownames(counts), ylim=c(0, (max(colSums(counts))+max(colSums(counts))*0.3)), cex.axis=1.5, cex.lab=1.5, cex.main=1.8, cex.names =1.5, args.legend = list(cex = 1.3, bty="n") , las=1)     
 mtext( BMT_sw_version,side=4,outer=FALSE)
mtext(plot_title_sub, 3, line=1, cex=1.3) 
mtext("tons",side=2,line=5, cex=1.5)
box()    
dev.off() 
}



}

}
                    