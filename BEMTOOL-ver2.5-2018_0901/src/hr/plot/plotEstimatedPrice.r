# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





plotEstimatedPrice <- function(all_the_years, econind_path, econind_path_quantiles) {

if (FALSE){
   all_the_years <- c(years, years.forecast)
   econind_path <- name_econind
   econind_path_quantiles <- name_econind_CI  
}

ECOIndicators_quantiles <- NULL
  
if (phase == "FORECAST") {
ECOIndicators_quantiles <- try(read.csv(econind_path_quantiles, sep=";",header=T))

if (class(ECOIndicators_quantiles) == "try-error") { 
  ECOIndicators_quantiles <- NULL
}

}

ECOindicators <- read.csv(econind_path,sep=";",header=T)

fleets_to_plot <- as.character(unique(ECOindicators$Fleet_segment))
fleets_to_plot <- fleets_to_plot[fleets_to_plot != "ALL"]

species_to_plot <- as.character(unique(ECOindicators$Species))
species_to_plot <- species_to_plot[species_to_plot != "ALL"]

for (ff in 1:length(fleets_to_plot)) {

if (exists("prices_toplot_1")) {
rm(prices_toplot_1)
rm(prices_toplot_2)
rm(prices_toplot_4)
rm(prices_toplot_5)
}

for (ss in 1:length(species_to_plot)) {

associated_fleetsegment <<- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ss, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <<- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <<- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

FLEETSEGMENTS_names <<- BMT_FLEETSEGMENTS[associated_fleetsegment_indices]

if (fleets_to_plot[ff] %in% FLEETSEGMENTS_names) {

if (exists("prices_toplot")) {
rm(prices_toplot)
}

prices_toplot <- ECOindicators[as.character(ECOindicators$Variable) == "price" & as.character(ECOindicators$Fleet_segment) == fleets_to_plot[ff] & as.character(ECOindicators$Species) == species_to_plot[ss],1:(ncol(ECOindicators)-1)]

if (exists("prices_toplot_1")) {
rm(prices_toplot_1)
rm(prices_toplot_2)
rm(prices_toplot_4)
rm(prices_toplot_5)
}

if (!is.null(ECOIndicators_quantiles)) {

# print(nrow(ECOIndicators_quantiles[ECOIndicators_quantiles$Species == BMT_SPECIES[ss], ]) )

if (nrow(ECOIndicators_quantiles[ECOIndicators_quantiles$Species == BMT_SPECIES[ss], ]) != 0) {
prices_toplot_1 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "price" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & as.character(ECOIndicators_quantiles$Species) == species_to_plot[ss] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.05, 1:(ncol(ECOIndicators_quantiles)-2)]

prices_toplot_2 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "price" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & as.character(ECOIndicators_quantiles$Species) == species_to_plot[ss] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.25, 1:(ncol(ECOIndicators_quantiles)-2)]

prices_toplot_4 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "price" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & as.character(ECOIndicators_quantiles$Species) == species_to_plot[ss] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.75, 1:(ncol(ECOIndicators_quantiles)-2)]

prices_toplot_5 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "price" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & as.character(ECOIndicators_quantiles$Species) == species_to_plot[ss] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.95, 1:(ncol(ECOIndicators_quantiles)-2)]

 prices_toplot_1$Value <-  as.numeric(as.character(prices_toplot_1$Value))
   prices_toplot_1$Value[is.na( prices_toplot_1$Value)] <- 0
   
      prices_toplot_2$Value <-  as.numeric(as.character(prices_toplot_2$Value))
   prices_toplot_2$Value[is.na( prices_toplot_2$Value)] <- 0
   
       prices_toplot_4$Value <-  as.numeric(as.character(prices_toplot_4$Value))
  prices_toplot_4$Value[is.na( prices_toplot_4$Value)] <- 0
    
      prices_toplot_5$Value <-  as.numeric(as.character(prices_toplot_5$Value))
    prices_toplot_5$Value[is.na( prices_toplot_5$Value)] <- 0


} 
}

   plot_title <- paste("Price - ", species_to_plot[ss], " - ", fleets_to_plot[ff] , sep="")

if (!MEY_CALCULATION) {
  plot_path <- paste(casestudy_path, "/",harvest_rule_id,"/Economic indicators/", casestudy_name, " - Price ",species_to_plot[ss], " - ", fleets_to_plot[ff] , " ", harvest_rule_id,".jpg", sep="")
   plot_title_sub <- paste( casestudy_name, " ",harvest_rule_id , " [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
  } else {
    plot_path <- paste(casestudy_path, "/MEY calculation/",harvest_rule_id,"/Economic indicators/", casestudy_name, " - Price ",species_to_plot[ss], " - ", fleets_to_plot[ff] , " ", harvest_rule_id,".jpg", sep="")
    plot_title_sub <- paste( casestudy_name, " ",harvest_rule_id , " [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
  }

 
jpeg(file=plot_path, width=21, height=21, bg="white", units="cm",res=200)
#windows()  
par(mar=c(6, 7, 8, 7))   # c(bottom, left, top, right) 


   prices_toplot_noNA <-  as.numeric(as.character(prices_toplot$Value))
   prices_toplot_noNA[is.na(prices_toplot_noNA)] <- 0
   
    prices_toplot_noNA[!is.finite(prices_toplot_noNA)] <- 0
    prices_toplot_noNA[!is.numeric(prices_toplot_noNA)] <- 0  

min_y_prices <- min(prices_toplot_noNA, na.rm=T)
if (is.na(min_y_prices)) {
min_y_prices <- 0
} else {
min_y_prices  <- ifelse(min_y_prices<0, min_y_prices, 0)
}

if (exists("prices_toplot_1")) {
min_y_prices  <- min(min_y_prices, min(as.numeric(as.character(prices_toplot_1$Value)), na.rm=T) , na.rm=T)
}

max_y_prices <- max(prices_toplot_noNA,  na.rm=T)
if (is.na(max_y_prices)) {
max_y_prices <- 0
}

if (exists("prices_toplot_5")) {
max_y_prices  <- max(max_y_prices, max(as.numeric(as.character(prices_toplot_1$Value)), na.rm=T) , na.rm=T)
} 


min_y_range_prices <- min_y_prices
max_y_range_prices <- ( max_y_prices + (max_y_prices - min_y_prices) * 0.2 )
                                                                                                        
plot(as.numeric(as.character(prices_toplot$Year)),prices_toplot_noNA,type="l",xlab="Year", ylab="", axes=F, col="blue", lwd=2, ylim=c(min_y_range_prices, max_y_range_prices),  cex.lab=1.5, cex.main=1.8, main=plot_title)
lines(as.numeric(as.character(prices_toplot$Year)),prices_toplot_noNA, type="p", col="blue", lwd=2, pch=19)     
axis(1, at=as.numeric(as.character(prices_toplot$Year)),cex.axis=1.5 )
axis(2, las=1, cex.axis=1.5)  ## las=1 makes horizontal labels
mtext("price [?_kg]",side=2,line=5, cex=1.5)
mtext(plot_title_sub, 3, line=1, cex=1.3)

 if (phase == "FORECAST") {
    lines(rep(years[length(years)],2), c(min_y_range_prices, max_y_range_prices), col="red", lty=2)
    text(rep(years[length(years)],2)+2,max_y_range_prices, label="FORECAST (estimated)", pos=1, cex=0.65) 
  }  

 mtext( BMT_sw_version,side=4,line=2,outer=FALSE)
                          #!is.null(ECOIndicators_quantiles)
 if (!is.null(ECOIndicators_quantiles)) {
if (nrow(ECOIndicators_quantiles[ECOIndicators_quantiles$Species == BMT_SPECIES[ss], ]) != 0 ) {
  
polygon(c(as.numeric(as.character(prices_toplot$Year)), rev(as.numeric(as.character(prices_toplot$Year)))), c( as.numeric(as.character(prices_toplot_1$Value)), rev(as.numeric(as.character(prices_toplot_2$Value)) )), col = alpha("black", 0.05), border = NA)
polygon(c(as.numeric(as.character(prices_toplot$Year)), rev(as.numeric(as.character(prices_toplot$Year)))), c(as.numeric(as.character(prices_toplot_2$Value)), rev(as.numeric(as.character(prices_toplot_noNA)) )), col = alpha("black", 0.15), border = NA)
polygon(c(as.numeric(as.character(prices_toplot$Year)), rev(as.numeric(as.character(prices_toplot$Year)))),c(as.numeric(as.character(prices_toplot_noNA)), rev(as.numeric(as.character(prices_toplot_4$Value)) )), col = alpha("black", 0.15), border = NA)
polygon(c(as.numeric(as.character(prices_toplot$Year)), rev(as.numeric(as.character(prices_toplot$Year)))),c(as.numeric(as.character(prices_toplot_4$Value)), rev(as.numeric(as.character(prices_toplot_5$Value)) )), col = alpha("black", 0.05), border = NA)
    
    lines(as.numeric(as.character(prices_toplot$Year)), as.numeric(as.character(prices_toplot_1$Value)),  type="l", lty=2) 
    	lines(as.numeric(as.character(prices_toplot$Year)),as.numeric(as.character(prices_toplot_2$Value)) ,  type="l", lty=3) 
    	lines(as.numeric(as.character(prices_toplot$Year)), as.numeric(as.character(prices_toplot_4$Value )),  type="l", lty=3) 
       lines(as.numeric(as.character(prices_toplot$Year)), as.numeric(as.character(prices_toplot_5$Value)),  type="l", lty=2) 

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","blue","black","black"), bty="n")

	#lines(as.numeric(as.character(total.prices_toplot_inf$Year)),as.numeric(as.character(total.prices_toplot_inf$Value)) , col="blue",  type="l", lty=2) 
	 # lines(as.numeric(as.character(total.prices_toplot_sup$Year)),as.numeric(as.character(total.prices_toplot_sup$Value)), col="blue",  type="l", lty=3) 
	 # legend ("bottomright", lwd=c(1,1),lty=c(2,3),legend=c( "0.05", "0.95"),col=c("black","black"), bty="n" )
}
}

box()
dev.off() 

}

}    

}

}                
