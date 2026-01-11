# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy,
# completeness or appropriateness for any particular purpose.





saveRevenues_discard_Plot <- function(all_the_years, econind_path, econind_path_quantiles) {


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

for (ff in 1:length(fleets_to_plot)) {
landings_toplot <- ECOindicators[as.character(ECOindicators$Variable) == "total.discards" & as.character(ECOindicators$Fleet_segment) == fleets_to_plot[ff],1:(ncol(ECOindicators)-1)]
revenues_toplot <- ECOindicators[as.character(ECOindicators$Variable) == "total.revenues.discard" & as.character(ECOindicators$Fleet_segment) == fleets_to_plot[ff],1:(ncol(ECOindicators)-1)]

if (!is.null(ECOIndicators_quantiles)) {
landings_toplot_1 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "total.discards" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.05, 1:(ncol(ECOIndicators_quantiles)-2)]
revenues_toplot_1 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "total.revenues.discard" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.05, 1:(ncol(ECOIndicators_quantiles)-2)]
#landings_toplot_1 <- rbind(landings_toplot[landings_toplot$Year %in% years,] , landings_toplot_1)
#revenues_toplot_1 <- rbind(revenues_toplot[revenues_toplot$Year %in% years,] , revenues_toplot_1)

landings_toplot_2 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "total.discards" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.25, 1:(ncol(ECOIndicators_quantiles)-2)]
revenues_toplot_2 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "total.revenues.discard" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.25, 1:(ncol(ECOIndicators_quantiles)-2)]
#landings_toplot_2 <- rbind(landings_toplot[landings_toplot$Year %in% years,] , landings_toplot_2)
#revenues_toplot_2 <- rbind(revenues_toplot[revenues_toplot$Year %in% years,] , revenues_toplot_2)

landings_toplot_4 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "total.discards" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.75, 1:(ncol(ECOIndicators_quantiles)-2)]
revenues_toplot_4 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "total.revenues.discard" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.75, 1:(ncol(ECOIndicators_quantiles)-2)]
# landings_toplot_4 <- rbind(landings_toplot[landings_toplot$Year %in% years,] , landings_toplot_4)
#revenues_toplot_4 <- rbind(revenues_toplot[revenues_toplot$Year %in% years,] , revenues_toplot_4)

landings_toplot_5 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "total.discards" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.95, 1:(ncol(ECOIndicators_quantiles)-2)]
revenues_toplot_5 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "total.revenues.discard" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.95, 1:(ncol(ECOIndicators_quantiles)-2)]
#landings_toplot_5 <- rbind(landings_toplot[landings_toplot$Year %in% years,] , landings_toplot_5)
#revenues_toplot_5 <- rbind(revenues_toplot[revenues_toplot$Year %in% years,] , revenues_toplot_5)
}

   plot_title <- paste("Revenues-Discard - ", ifelse(fleets_to_plot[ff] == "ALL", "Overall", fleets_to_plot[ff]) , sep="")
if (phase =="SIMULATION") {
  plot_path <- paste(casestudy_path, "/Diagnosis/Economic indicators/", casestudy_name, " - Revenues-Discard ",fleets_to_plot[ff], ".jpg", sep="")
  # plot_title <- paste("[", casestudy_name, "] Revenues-Landing ", fleets_to_plot[ff], " SIM", sep="")
  plot_title_sub <- paste( casestudy_name, " simulation [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
} else {
if (!MEY_CALCULATION) {
  plot_path <- paste(casestudy_path, "/",harvest_rule_id,"/Economic indicators/", casestudy_name, " - Revenues-Discard ",fleets_to_plot[ff], " ", harvest_rule_id,".jpg", sep="")
  # plot_title <- paste("[", casestudy_name, "] Revenues-Landing ", fleets_to_plot[ff], " FORE ", harvest_rule_id, sep="")
   plot_title_sub <- paste( casestudy_name, " ",harvest_rule_id , " [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
  } else {
    plot_path <- paste(casestudy_path, "/MEY calculation/",harvest_rule_id,"/Economic indicators/", casestudy_name, " - Revenues-Discard ",fleets_to_plot[ff], " ", harvest_rule_id,".jpg", sep="")
  #plot_title <- paste("[", casestudy_name, "] Revenues-Landing ", fleets_to_plot[ff], " ", harvest_rule_id, sep="")
    plot_title_sub <- paste( casestudy_name, " ",harvest_rule_id , " [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
  }
}

if (!all(as.numeric(as.character(landings_toplot$Value)) == 0)) {

jpeg(file=plot_path, width=21, height=21, bg="white", units="cm",res=200)
#windows()
par(mar=c(6, 7, 8, 7))   # c(bottom, left, top, right)


min_y_revenues <- min(as.numeric(as.character(revenues_toplot$Value)), na.omit=T)
if (is.na(min_y_revenues)) {
min_y_revenues <- 0
} else {
min_y_revenues  <- ifelse(min_y_revenues<0, min_y_revenues, 0)
}

if (!is.null(ECOIndicators_quantiles)) {
min_y_revenues  <- min(min_y_revenues, min(as.numeric(as.character(revenues_toplot_1$Value)), na.omit=T) , na.omit=T)
}

max_y_revenues <- max(as.numeric(as.character(revenues_toplot$Value)))
if (is.na(max_y_revenues)) {
max_y_revenues <- 0
}

if (!is.null(ECOIndicators_quantiles)) {
max_y_revenues  <- max(max_y_revenues, max(as.numeric(as.character(revenues_toplot_5$Value)), na.omit=T) , na.omit=T)
}


min_y_range_revenues <- min_y_revenues /1000
max_y_range_revenues <- ( max_y_revenues + (max_y_revenues - min_y_revenues) * 0.2 )/1000

y_lim = c(min_y_range_revenues, max_y_range_revenues)

plot(as.numeric(as.character(revenues_toplot$Year)),as.numeric(as.character(revenues_toplot$Value))/1000,type="l",xlab="Year", ylab="", axes=F, col="red", lwd=2, ylim=y_lim,  cex.lab=1.5, cex.main=1.8, main=plot_title)
lines(as.numeric(as.character(revenues_toplot$Year)),as.numeric(as.character(revenues_toplot$Value))/1000, type="p", col="red", lwd=2, pch=19)
axis(1, at=as.numeric(as.character(revenues_toplot$Year)),cex.axis=1.5 )
axis(2, las=1, cex.axis=1.5)  ## las=1 makes horizontal labels
mtext(",000 ?",col="red",side=2,line=5, cex=1.5)
mtext(plot_title_sub, 3, line=1, cex=1.3)

 if (phase == "FORECAST") {
    lines(rep(years[length(years)],2), y_lim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,y_lim[2], label="FORECAST", pos=1, cex=0.55)
  }

 mtext( BMT_sw_version,side=4,line=6,outer=FALSE)

if (!is.null(ECOIndicators_quantiles)) {

if (!all(revenues_toplot_1$Value == 0))  {
 polygon(c(as.numeric(as.character(revenues_toplot$Year)), rev(as.numeric(as.character(revenues_toplot$Year)))), c( as.numeric(as.character(revenues_toplot_1$Value))/1000, rev(as.numeric(as.character(revenues_toplot_2$Value))/1000)), col = alpha("red", 0.05), border = NA)
polygon(c(as.numeric(as.character(revenues_toplot$Year)), rev(as.numeric(as.character(revenues_toplot$Year)))), c(as.numeric(as.character(revenues_toplot_2$Value))/1000, rev(as.numeric(as.character(revenues_toplot$Value))/1000)), col = alpha("red", 0.15), border = NA)
polygon(c(as.numeric(as.character(revenues_toplot$Year)), rev(as.numeric(as.character(revenues_toplot$Year)))),c(as.numeric(as.character(revenues_toplot$Value))/1000, rev(as.numeric(as.character(revenues_toplot_4$Value))/1000)), col = alpha("red", 0.15), border = NA)
polygon(c(as.numeric(as.character(revenues_toplot$Year)), rev(as.numeric(as.character(revenues_toplot$Year)))),c(as.numeric(as.character(revenues_toplot_4$Value))/1000, rev(as.numeric(as.character(revenues_toplot_5$Value))/1000)), col = alpha("red", 0.05), border = NA)

    lines(as.numeric(as.character(revenues_toplot$Year)), as.numeric(as.character(revenues_toplot_1$Value))/1000, col="red", type="l", lty=2)
    	lines(as.numeric(as.character(revenues_toplot$Year)),as.numeric(as.character(revenues_toplot_2$Value)) /1000, col="red", type="l", lty=3)
    	lines(as.numeric(as.character(revenues_toplot$Year)), as.numeric(as.character(revenues_toplot_4$Value))/1000 , col="red", type="l", lty=3)
       lines(as.numeric(as.character(revenues_toplot$Year)), as.numeric(as.character(revenues_toplot_5$Value))/1000, col="red", type="l", lty=2)

       legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median revenues", "0.75", "0.95"),col=c("red","red","red","red","red"), bty="n")
legend ("topright", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median landing", "0.75", "0.95"),col=c("blue","blue","blue","blue","blue"), bty="n")


#     lines(as.numeric(as.character(total.revenues_toplot_inf$Year)),as.numeric(as.character(total.revenues_toplot_inf$Value))/1000 , col="red",  type="l", lty=2)
#	  lines(as.numeric(as.character(total.revenues_toplot_sup$Year)),as.numeric(as.character(total.revenues_toplot_sup$Value))/1000 , col="red",  type="l", lty=3)
#	  legend ("bottomright", lwd=c(1,1),lty=c(2,3),legend=c( "0.05", "0.95"),col=c("black","black"), bty="n" )
}

}

par(new=TRUE)


min_y_landing <- min(as.numeric(as.character(landings_toplot$Value)),  na.omit=T)
if (is.na(min_y_landing)) {
min_y_landing <- 0
} else {
min_y_landing  <- ifelse(min_y_landing<0, min_y_landing, 0)
}

if (!is.null(ECOIndicators_quantiles)) {
min_y_landing  <- min(min_y_landing, min(as.numeric(as.character(landings_toplot_1$Value)), na.omit=T), na.omit=T)
}


max_y_landing <- max(as.numeric(as.character(landings_toplot$Value)))
if (is.na(max_y_landing)) {
max_y_landing <- 0
}

if (!is.null(ECOIndicators_quantiles)) {
max_y_landing  <- max(max_y_landing, max(as.numeric(as.character(landings_toplot_5$Value)), na.omit=T) , na.omit=T)
}


min_y_landing_range <- min_y_landing/1000
max_y_landing_range <- (max_y_landing + (max_y_landing - min_y_landing) * 0.2 )/1000

y_lim = c(min_y_landing_range, max_y_landing_range)

plot(as.numeric(as.character(landings_toplot$Year)),as.numeric(as.character(landings_toplot$Value))/1000,type="l",xlab="", ylab="", axes=F, col="blue", lwd=2, ylim=y_lim)
lines(as.numeric(as.character(landings_toplot$Year)),as.numeric(as.character(landings_toplot$Value))/1000, type="p", col="blue", lwd=2, pch=19)
axis(4, las=1, cex.axis=1.5)
mtext("tons",col="blue",side=4,line=5, cex=1.5)

if (!is.null(ECOIndicators_quantiles)) {

     polygon(c(as.numeric(as.character(landings_toplot$Year)), rev(as.numeric(as.character(landings_toplot$Year)))), c( as.numeric(as.character(landings_toplot_1$Value))/1000, rev(as.numeric(as.character(landings_toplot_2$Value))/1000)), col = alpha("blue", 0.05), border = NA)
polygon(c(as.numeric(as.character(landings_toplot$Year)), rev(as.numeric(as.character(landings_toplot$Year)))), c(as.numeric(as.character(landings_toplot_2$Value))/1000, rev(as.numeric(as.character(landings_toplot$Value))/1000)), col = alpha("blue", 0.15), border = NA)
polygon(c(as.numeric(as.character(landings_toplot$Year)), rev(as.numeric(as.character(landings_toplot$Year)))),c(as.numeric(as.character(landings_toplot$Value))/1000, rev(as.numeric(as.character(landings_toplot_4$Value))/1000)), col = alpha("blue", 0.15), border = NA)
polygon(c(as.numeric(as.character(landings_toplot$Year)), rev(as.numeric(as.character(landings_toplot$Year)))),c(as.numeric(as.character(landings_toplot_4$Value))/1000, rev(as.numeric(as.character(landings_toplot_5$Value))/1000)), col = alpha("blue", 0.05), border = NA)

    lines(as.numeric(as.character(landings_toplot$Year)), as.numeric(as.character(landings_toplot_1$Value))/1000, col="blue", type="l", lty=2)
    	lines(as.numeric(as.character(landings_toplot$Year)),as.numeric(as.character(landings_toplot_2$Value))/1000 ,col="blue",  type="l", lty=3)
    	lines(as.numeric(as.character(landings_toplot$Year)),as.numeric(as.character(landings_toplot_4$Value))/1000, col="blue", type="l", lty=3)
       lines(as.numeric(as.character(landings_toplot$Year)), as.numeric(as.character(landings_toplot_5$Value))/1000, col="blue", type="l", lty=2)

              legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median revenues", "0.75", "0.95"),col=c("red","red","red","red","red"), bty="n")
legend ("topright", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median discard", "0.75", "0.95"),col=c("blue","blue","blue","blue","blue"), bty="n")

#     lines(as.numeric(as.character(total.landings_toplot_inf$Year)),as.numeric(as.character(total.landings_toplot_inf$Value))/1000 , col="blue", type="l", lty=2)
#	  lines(as.numeric(as.character(total.landings_toplot_sup$Year)),as.numeric(as.character(total.landings_toplot_sup$Value))/1000 , col="blue", type="l", lty=3)
#	  legend ("topleft", lwd=c(2,1,1),lty=c(1,2,3),legend=c( "0.05", "0.95"),col=c("blue","blue"), bty="n" )
}

box()                                                                                                                      #
#legend("top", c("Total revenues", "Total landings"), border="white", pch=16, col=c("red", "blue"), horiz=T, fill=NULL,  bty="n",cex=1.3)
dev.off()

}
}

}

# saveRevenues_Plot(years, name_econind)
