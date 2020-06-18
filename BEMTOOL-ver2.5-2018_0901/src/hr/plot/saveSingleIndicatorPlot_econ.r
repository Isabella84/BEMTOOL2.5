# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





saveSingleIndicatorPlot_econ <- function(all_the_years, path_eco, path_eco_quantiles) {

NAMES_TO_PLOT <- c("VESSELS.annual", "DAYS.annual", "gross.value.added", "Economic.indicators[NPV.discounted]", "Economic.indicators[RoFTA]")

if (FALSE){
   all_the_years <- c(years, years.forecast)
   path_eco <- name_econind
   path_eco_quantiles <- name_econind_CI
}

  ECOIndicators_quantiles <- NULL

if (phase == "FORECAST") {

ECOIndicators_quantiles <- suppressWarnings(try(data.frame(read.csv(path_eco_quantiles, sep=";",header=T), stringsAsFactors=F)))

if (class(ECOIndicators_quantiles) == "try-error") { 
  ECOIndicators_quantiles <- NULL
}

if (!is.null(ECOIndicators_quantiles)) { 
ECOIndicators_quantiles_temp <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Year) != "ALL" & as.character(ECOIndicators_quantiles$Variable) %in% NAMES_TO_PLOT, ]
ECOIndicators_quantiles_temp$Type <- "Econ"
Indicators_to_plot_quantiles <- data.frame(ECOIndicators_quantiles_temp , stringsAsFactors=F)

}
 
}

ECOIndicators <- data.frame(read.csv(path_eco, sep=";",header=T), stringsAsFactors=F)
ECOIndicators_temp <- ECOIndicators[as.character(ECOIndicators$Year) != "ALL" & as.character(ECOIndicators$Variable) %in% NAMES_TO_PLOT, ]
ECOIndicators_temp$Type <- "Econ"

Indicators_to_plot <- data.frame( ECOIndicators_temp , stringsAsFactors=F)

indicators_to_plot_names <- as.character(unique(Indicators_to_plot$Variable))

#print("Saving biological and impact-pressure indicators plots...", quote=F)


for (ind in 1:length(indicators_to_plot_names)) {  

one_indicator <- Indicators_to_plot[as.character(Indicators_to_plot$Variable) == indicators_to_plot_names[ind] & as.numeric(as.character(Indicators_to_plot$Year)) %in% all_the_years, ]

if ( indicators_to_plot_names[ind] != "VESSELS.annual" & indicators_to_plot_names[ind] !=  "DAYS.annual") {
if (exists("one_indicator_inf")) {
rm(one_indicator_inf)
rm(one_indicator_sup)

rm(one_indicator_inf_025)
rm(one_indicator_sup_075)
}
}

if ( indicators_to_plot_names[ind] != "VESSELS.annual" & indicators_to_plot_names[ind] !=  "DAYS.annual") {
if (phase == "FORECAST" & !is.null(ECOIndicators_quantiles)) {

  one_indicator_inf <- Indicators_to_plot_quantiles[as.character(Indicators_to_plot_quantiles$Variable) == indicators_to_plot_names[ind] & as.numeric(as.character(Indicators_to_plot_quantiles$Year)) %in% all_the_years & Indicators_to_plot_quantiles$quantile == 0.05, ]
  one_indicator_sup <- Indicators_to_plot_quantiles[as.character(Indicators_to_plot_quantiles$Variable) == indicators_to_plot_names[ind] & as.numeric(as.character(Indicators_to_plot_quantiles$Year)) %in% all_the_years & Indicators_to_plot_quantiles$quantile == 0.95, ]
  
 one_indicator_inf_025 <- Indicators_to_plot_quantiles[as.character(Indicators_to_plot_quantiles$Variable) == indicators_to_plot_names[ind] & as.numeric(as.character(Indicators_to_plot_quantiles$Year)) %in% all_the_years & Indicators_to_plot_quantiles$quantile == 0.25, ]
 one_indicator_sup_075 <- Indicators_to_plot_quantiles[as.character(Indicators_to_plot_quantiles$Variable) == indicators_to_plot_names[ind] & as.numeric(as.character(Indicators_to_plot_quantiles$Year)) %in% all_the_years & Indicators_to_plot_quantiles$quantile == 0.75, ]
  
}
}

fs_thisindicator <- as.character(unique(one_indicator$Fleet_segment))

by_fleet <-  !all(fs_thisindicator %in% c("ALL"))

   indicator_name <- as.character(indicators_to_plot_names[ind] )

for (fleet in 1:length(fs_thisindicator)) {
     
plot_title <- paste(indicator_name, sep="")

if (phase =="SIMULATION") {                          # ifelse(as.character(one_indicator$Type[1]) == "Biol", "Biological indicators", "Pressure impact indicators")                        
  plot_path <- paste(casestudy_path, "/Diagnosis/Economic indicators/", casestudy_name, " - ", indicator_name, ifelse(by_fleet, paste(" " , fs_thisindicator[fleet] , sep=""), ""), ".jpg", sep="")  
plot_title_sub <- paste( casestudy_name, " simulation [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
} else {
if (!MEY_CALCULATION) {                                       #ifelse(as.character(one_indicator$Type[1]) == "Biol", "Biological indicators", "Pressure impact indicators"),
  plot_path <- paste(casestudy_path, "/",harvest_rule_id,"/Economic indicators/", casestudy_name, " - ", indicator_name, ifelse(by_fleet, paste(" " , fs_thisindicator[fleet] , sep=""), ""), " ", harvest_rule_id,".jpg", sep="")
   plot_title_sub <- paste( casestudy_name,  " ",harvest_rule_id , " [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
  } else {                                                                          # ifelse(as.character(one_indicator$Type[1]) == "Biol", "Biological indicators", "Pressure impact indicators")
    plot_path <- paste(casestudy_path, "/MEY calculation/",harvest_rule_id,"/Economic indicators/", casestudy_name, " - ", indicator_name, ifelse(by_fleet, paste(" " , fs_thisindicator[fleet] , sep=""), "") , " ", harvest_rule_id,".jpg", sep="")
    plot_title_sub <- paste( casestudy_name, " ",harvest_rule_id , " [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
  }
}

one_indicator_fl <- one_indicator[one_indicator$Fleet_segment==fs_thisindicator[fleet], ]

if ( indicators_to_plot_names[ind] != "VESSELS.annual" & indicators_to_plot_names[ind] !=  "DAYS.annual") {
if (exists("one_indicator_fl_inf")) {
rm(one_indicator_fl_inf)
rm(one_indicator_fl_sup)
rm(one_indicator_fl_inf_025)
rm(one_indicator_fl_sup_075)
}

                                                    # !is.null(BIOIndicators_quantiles)
if (phase == "FORECAST" & exists("one_indicator_inf") != 0 ) {
one_indicator_fl_inf <- one_indicator_inf[one_indicator_inf$Fleet_segment==fs_thisindicator[fleet], ]
one_indicator_fl_sup <- one_indicator_sup[one_indicator_sup$Fleet_segment==fs_thisindicator[fleet], ]
one_indicator_fl_inf_025 <- one_indicator_inf_025[one_indicator_inf_025$Fleet_segment==fs_thisindicator[fleet], ]
one_indicator_fl_sup_075 <- one_indicator_sup_075[one_indicator_sup_075$Fleet_segment==fs_thisindicator[fleet], ]
}
}

meas_unit <- as.character(one_indicator_fl$Unit[1])

jpeg(file=plot_path, width=21, height=21, bg="white", units="cm",res=200)
#windows()    
par(mar=c(5, 6, 11, 6))  # c(bottom, left, top, right)

if (meas_unit != "") {
y_lab <- paste(indicator_name, " [", meas_unit,"]", sep="")
} else {
y_lab <- indicator_name
}
  
    y_to_plot <- as.numeric(as.character(one_indicator_fl$Value))
#        to_be_plotted <- which(!is.na(y_to_plot))
            y_to_plot[which(is.na(y_to_plot))]  <- 0
                  #      y_to_plot <- y_to_plot[to_be_plotted] 
 
 min_y <- min(y_to_plot, na.rm=T) 
   max_y <- max(y_to_plot, na.rm=T) 

if ( indicators_to_plot_names[ind] != "VESSELS.annual" & indicators_to_plot_names[ind] !=  "DAYS.annual") { 
                                                       # !is.null(BIOIndicators_quantiles)
if (phase == "FORECAST" & exists("one_indicator_fl_inf") ) {
     y_to_plot_inf <- as.numeric(as.character(one_indicator_fl_inf$Value))
          y_to_plot_sup <- as.numeric(as.character(one_indicator_fl_sup$Value))
          y_to_plot_inf_025 <- as.numeric(as.character(one_indicator_fl_inf_025$Value))
          y_to_plot_sup_075 <- as.numeric(as.character(one_indicator_fl_sup_075$Value))
      #  to_be_plotted_inf <- which(!is.na(y_to_plot_inf))
#          to_be_plotted_sup <- which(!is.na(y_to_plot_sup))
         
      y_to_plot_inf[which(is.na(y_to_plot_inf))] <- 0 
      y_to_plot_sup[which(is.na(y_to_plot_sup))] <- 0 
      y_to_plot_inf_025[which(is.na(y_to_plot_inf_025))] <- 0 
      y_to_plot_sup_075[which(is.na(y_to_plot_sup_075))] <- 0
                
       min_y <- min(y_to_plot, y_to_plot_inf, y_to_plot_sup,  y_to_plot_inf_025, y_to_plot_sup_075, na.rm=T)   
          max_y <- max(y_to_plot, y_to_plot_inf, y_to_plot_sup, y_to_plot_inf_025, y_to_plot_sup_075, na.rm=T)         
                }
}

if (is.na(min_y)) {
min_y <- 0
} else {
min_y  <- ifelse(min_y<0, min_y, 0)
}


if (is.na(max_y)) {
max_y <- 0
} 

                                                                                                        
plot(as.numeric(as.character(one_indicator_fl$Year)),as.numeric(as.character(y_to_plot)),type="l",xlab="Year",ylab=y_lab, ylim=c(min_y, (max_y + (max_y-min_y)*0.35 )), xlim=c(min(as.numeric(as.character(one_indicator_fl$Year))), max(as.numeric(as.character(one_indicator_fl$Year)))), axes=F, pch =16, lwd=2, col="blue", cex.lab=1.5, cex.main=1.8, main=plot_title) 
lines(as.numeric(as.character(one_indicator_fl$Year)), y_to_plot, type="p", col="blue", lwd=2, pch=19)     
                                                     # [to_be_plotted]
 
 if (phase == "FORECAST") {
    lines(rep(years[length(years)],2), c(min_y, (max_y + max_y*0.15 )), col="red", lty=2)
    text(rep(years[length(years)],2)+1,min_y, label="FORECAST", pos=3,  cex=0.55) 
  }  
            
if ( indicators_to_plot_names[ind] != "VESSELS.annual" & indicators_to_plot_names[ind] !=  "DAYS.annual") {
                                    #  !is.null(BIOIndicators_quantiles)    
if (phase == "FORECAST" & exists("one_indicator_fl_inf") & indicators_to_plot_names[ind] != "Z" & indicators_to_plot_names[ind] != "F") {

polygon(c(as.numeric(as.character(one_indicator_fl$Year)), rev(as.numeric(as.character(one_indicator_fl$Year)))), c(y_to_plot_inf, rev(y_to_plot_inf_025)), col = alpha("black", 0.05), border = NA)
polygon(c(as.numeric(as.character(one_indicator_fl$Year)), rev(as.numeric(as.character(one_indicator_fl$Year)))), c(y_to_plot_inf_025, rev(y_to_plot)), col = alpha("black", 0.15), border = NA)
polygon(c(as.numeric(as.character(one_indicator_fl$Year)), rev(as.numeric(as.character(one_indicator_fl$Year)))), c(y_to_plot, rev(y_to_plot_sup_075)), col = alpha("black", 0.15), border = NA)
polygon(c(as.numeric(as.character(one_indicator_fl$Year)), rev(as.numeric(as.character(one_indicator_fl$Year)))), c(y_to_plot_sup_075, rev(y_to_plot_sup)), col = alpha("black", 0.05), border = NA)
    
    lines(as.numeric(as.character(one_indicator_fl$Year)), y_to_plot_inf ,  type="l", lty=2)   	  
    lines(as.numeric(as.character(one_indicator_fl$Year)), y_to_plot_inf_025 ,  type="l", lty=3) 
    lines(as.numeric(as.character(one_indicator_fl$Year)), y_to_plot_sup_075 ,  type="l", lty=3) 
	  lines(as.numeric(as.character(one_indicator_fl$Year)), y_to_plot_sup ,  type="l", lty=2) 
	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","blue","black","black"), bty="n" )
}
}

cat(paste(" [", indicators_to_plot_names[ind], "] ", sep="") )                                  
#}

axis(1, at=as.numeric(as.character(one_indicator_fl$Year)), cex.axis=1.5)
axis(2, cex.axis=1.5)

 mtext( BMT_sw_version,side=4,outer=FALSE)

if (by_fleet) {
    mtext(paste(" " , fs_thisindicator[fleet] , sep=""), 3, line=3, cex=1.6)
    mtext(plot_title_sub, 3, line=1, cex=1.3)
} else {
    mtext(plot_title_sub, 3, line=1, cex=1.3)
}


box()

# title(plot_title) 

dev.off() 

}

}


}
 
 
 # saveSingleIndicatorPlot(years, name_bioind, name_pressind)                       