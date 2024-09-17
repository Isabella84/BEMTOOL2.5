# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





plotEstimatedLC <- function(all_the_years, econind_path, econind_path_quantiles) {

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
# fleets_to_plot <- fleets_to_plot[fleets_to_plot != "ALL"]

for (ff in 1:length(fleets_to_plot)) {


vc_toplot <- ECOindicators[as.character(ECOindicators$Variable) == "labour.cost" & as.character(ECOindicators$Fleet_segment) == fleets_to_plot[ff] ,1:(ncol(ECOindicators)-1)]

vc_toplot$Value <- as.numeric(as.character(vc_toplot$Value)) / 1000

if (exists("vc_toplot_1")) {
rm(vc_toplot_1)
rm(vc_toplot_2)
rm(vc_toplot_4)
rm(vc_toplot_5)

}

if (!is.null(ECOIndicators_quantiles)) {

# print(nrow(ECOIndicators_quantiles[ECOIndicators_quantiles$Species == BMT_SPECIES[ss], ]) )

if (nrow(ECOIndicators_quantiles) != 0) {
vc_toplot_1 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "labour.cost" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.05, 1:(ncol(ECOIndicators_quantiles)-2)]
vc_toplot_1$Value <- as.numeric(as.character(vc_toplot_1$Value)) / 1000
#vc_toplot_1 <- rbind(vc_toplot[vc_toplot$Year %in% years,] , vc_toplot_1)

vc_toplot_2 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "labour.cost" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.25, 1:(ncol(ECOIndicators_quantiles)-2)]
vc_toplot_2$Value <- as.numeric(as.character(vc_toplot_2$Value)) / 1000
#vc_toplot_2 <- rbind(vc_toplot[vc_toplot$Year %in% years,] , vc_toplot_2)

vc_toplot_4 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "labour.cost" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.75, 1:(ncol(ECOIndicators_quantiles)-2)]
vc_toplot_4$Value <- as.numeric(as.character(vc_toplot_4$Value)) / 1000
#vc_toplot_4 <- rbind(vc_toplot[vc_toplot$Year %in% years,] , vc_toplot_4)

vc_toplot_5 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "labour.cost" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.95, 1:(ncol(ECOIndicators_quantiles)-2)]
vc_toplot_5$Value <- as.numeric(as.character(vc_toplot_5$Value)) / 1000
#vc_toplot_5 <- rbind(vc_toplot[vc_toplot$Year %in% years,] , vc_toplot_5)
} 
}

   plot_title <- paste("Labour costs - ", fleets_to_plot[ff] , sep="")

if (!MEY_CALCULATION) {
  plot_path <- paste(casestudy_path, "/",harvest_rule_id,"/Economic indicators/", casestudy_name, " - Labour costs - ", fleets_to_plot[ff] , " ", harvest_rule_id,".jpg", sep="")
   plot_title_sub <- paste( casestudy_name, " ",harvest_rule_id , " [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
  } else {
    plot_path <- paste(casestudy_path, "/MEY calculation/",harvest_rule_id,"/Economic indicators/", casestudy_name, " - Labour costs - ", fleets_to_plot[ff] , " ", harvest_rule_id,".jpg", sep="")
    plot_title_sub <- paste( casestudy_name, " ",harvest_rule_id , " [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
  }

 
jpeg(file=plot_path, width=21, height=21, bg="white", units="cm",res=200)
#windows()  
par(mar=c(6, 10, 8, 7))   # c(bottom, left, top, right) 


min_y_prices <- min(as.numeric(as.character(vc_toplot$Value)), na.omit=T)
if (is.na(min_y_prices)) {
min_y_prices <- 0
} else {
min_y_prices  <- ifelse(min_y_prices<0, min_y_prices, 0)
}

if (exists("vc_toplot_1")) {
min_y_prices  <- min(min_y_prices, min(as.numeric(as.character(vc_toplot_1$Value)), na.omit=T) , na.omit=T)
}

max_y_prices <- max(as.numeric(as.character(vc_toplot$Value)))
if (is.na(max_y_prices)) {
max_y_prices <- 0
}

if (exists("vc_toplot_5")) {
max_y_prices  <- max(max_y_prices, max(as.numeric(as.character(vc_toplot_5$Value)), na.omit=T) , na.omit=T)
} 


min_y_range_prices <- min_y_prices
max_y_range_prices <- ( max_y_prices + (max_y_prices - min_y_prices) * 0.3 )

y_lim <- c(min_y_range_prices, max_y_range_prices)
                                                                                                        
plot(as.numeric(as.character(vc_toplot$Year)),as.numeric(as.character(vc_toplot$Value)),type="l",xlab="Year", ylab="", axes=F, col="blue", lwd=2, ylim=c(min_y_range_prices, max_y_range_prices),  cex.lab=1.5, cex.main=1.8, main=plot_title)
lines(as.numeric(as.character(vc_toplot$Year)),as.numeric(as.character(vc_toplot$Value)), type="p", col="blue", lwd=2, pch=19)     
axis(1, at=as.numeric(as.character(vc_toplot$Year)),cex.axis=1.5 )
axis(2, las=1, cex.axis=1.5)  ## las=1 makes horizontal labels
mtext("labour costs [.000 ?]",side=2,line=5, cex=1.5)
mtext(plot_title_sub, 3, line=1, cex=1.3)

 if (phase == "FORECAST") {
    lines(rep(years[length(years)],2), y_lim, col="red", lty=2)
    text(rep(years[length(years)],2)+2,y_lim[2], label="FORECAST (estimated)", pos=1, cex=0.65) 
  }  

 mtext( BMT_sw_version,side=4,line=2,outer=FALSE)
                          #!is.null(ECOIndicators_quantiles)
 if (!is.null(ECOIndicators_quantiles)) {
 
 	 polygon(c(as.numeric(as.character(vc_toplot$Year)), rev(as.numeric(as.character(vc_toplot$Year)))), c( vc_toplot_1$Value, rev(vc_toplot_2$Value)), col = alpha("black", 0.05), border = NA)
polygon(c(as.numeric(as.character(vc_toplot$Year)), rev(as.numeric(as.character(vc_toplot$Year)))), c(vc_toplot_2$Value, rev(vc_toplot$Value)), col = alpha("black", 0.15), border = NA)
polygon(c(as.numeric(as.character(vc_toplot$Year)), rev(as.numeric(as.character(vc_toplot$Year)))),c(vc_toplot$Value, rev(vc_toplot_4$Value)), col = alpha("black", 0.15), border = NA)
polygon(c(as.numeric(as.character(vc_toplot$Year)), rev(as.numeric(as.character(vc_toplot$Year)))),c(vc_toplot_4$Value, rev(vc_toplot_5$Value)), col = alpha("black", 0.05), border = NA)
    
    lines(as.numeric(as.character(vc_toplot$Year)), vc_toplot_1$Value,  type="l", lty=2) 
    	lines(as.numeric(as.character(vc_toplot$Year)),vc_toplot_2$Value ,  type="l", lty=3) 
    	lines(as.numeric(as.character(vc_toplot$Year)), vc_toplot_4$Value ,  type="l", lty=3) 
       lines(as.numeric(as.character(vc_toplot$Year)), vc_toplot_5$Value,  type="l", lty=2) 

	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","blue","black","black"), bty="n")
 
#    lines(as.numeric(as.character(total.prices_toplot_inf$Year)),as.numeric(as.character(total.prices_toplot_inf$Value)) , col="blue",  type="l", lty=2) 
#	  lines(as.numeric(as.character(total.prices_toplot_sup$Year)),as.numeric(as.character(total.prices_toplot_sup$Value)), col="blue",  type="l", lty=3) 
#	  legend ("bottomright", lwd=c(1,1),lty=c(2,3),legend=c( "0.05", "0.95"),col=c("black","black"), bty="n" )

}

box()
dev.off() 
  

}

}                
