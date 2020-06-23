# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





saveSingleIndicatorPlot <- function(all_the_years, path_biological, path_pressure, path_biological_quantiles, path_pressure_quantiles) {

if (FALSE){
   all_the_years <- c(years, years.forecast)  # years #
   path_biological <- name_bioind
   path_pressure <- name_pressind
   path_biological_quantiles <- name_bioind_CI
   path_pressure_quantiles <- name_pressind_CI 
}

  BIOIndicators_quantiles <- NULL
  PRESSIndicators_quantiles <- NULL

if (phase == "FORECAST") {
BIOIndicators_quantiles <- suppressWarnings(try(data.frame(read.csv(path_biological_quantiles, sep=";",header=T), stringsAsFactors=F)))

if (class(BIOIndicators_quantiles) == "try-error") { 
  BIOIndicators_quantiles <- NULL
}

if (!is.null(BIOIndicators_quantiles)) { 
BIOIndicators_quantiles_temp <- BIOIndicators_quantiles[as.character(BIOIndicators_quantiles$Year) != "ALL", ]
BIOIndicators_quantiles_temp$Type <- "Biol"
}
 
PRESSIndicators_quantiles <- suppressWarnings(try(data.frame(read.csv(path_pressure_quantiles, sep=";",header=T), stringsAsFactors=F))  )

if (class(PRESSIndicators_quantiles) == "try-error") { 
  PRESSIndicators_quantiles <- NULL
}

if (!is.null(PRESSIndicators_quantiles) ) {
PRESSIndicators_quantiles_temp <- PRESSIndicators_quantiles[as.character(PRESSIndicators_quantiles$Year) != "ALL", ]
PRESSIndicators_quantiles_temp$Type <- "Press"
Indicators_to_plot_quantiles <- data.frame(rbind(BIOIndicators_quantiles_temp, PRESSIndicators_quantiles_temp), stringsAsFactors=F)
}
}

BIOIndicators <- data.frame(read.csv(path_biological, sep=";",header=T), stringsAsFactors=F)
BIOIndicators_temp <- BIOIndicators[as.character(BIOIndicators$Year) != "ALL", ]
BIOIndicators_temp$Type <- "Biol"

PRESSIndicators <- data.frame(read.csv(path_pressure, sep=";",header=T), stringsAsFactors=F)
PRESSIndicators_temp <- PRESSIndicators[as.character(PRESSIndicators$Year) != "ALL", ]
PRESSIndicators_temp$Type <- "Press"

Indicators_to_plot <- data.frame(rbind(BIOIndicators_temp, PRESSIndicators_temp), stringsAsFactors=F)

indicators_to_plot_names <- as.character(unique(Indicators_to_plot$Variable))

#print("Saving biological and impact-pressure indicators plots...", quote=F)

for (ss in 1:length(BMT_SPECIES)) {
print("", quote=F)
print(paste("Saving biological and impact-pressure indicators plots for ", BMT_SPECIES[ss],"...", sep=""), quote=F)

for (ind in 1:length(indicators_to_plot_names)) {  

one_indicator <- Indicators_to_plot[as.character(Indicators_to_plot$Variable) == indicators_to_plot_names[ind] & as.character(Indicators_to_plot$Stock) == BMT_SPECIES[ss] & as.numeric(as.character(Indicators_to_plot$Year)) %in% all_the_years, ]

if (exists("one_indicator_inf")) {
rm(one_indicator_inf)
rm(one_indicator_sup)

rm(one_indicator_inf_025)
rm(one_indicator_sup_075)

if (indicators_to_plot_names[ind] == "Discard_ratio") {
rm(one_indicator_inf_05)
 rm(one_indicator_sup_05)
}

}

if (phase == "FORECAST" & !is.null(BIOIndicators_quantiles)) {
if (nrow(Indicators_to_plot_quantiles[Indicators_to_plot_quantiles$Stock == BMT_SPECIES[ss],]) != 0) {
  one_indicator_inf <- Indicators_to_plot_quantiles[as.character(Indicators_to_plot_quantiles$Variable) == indicators_to_plot_names[ind] & as.character(Indicators_to_plot_quantiles$Stock) == BMT_SPECIES[ss] & as.numeric(as.character(Indicators_to_plot_quantiles$Year)) %in% all_the_years & Indicators_to_plot_quantiles$quantile == 0.05, ]
  one_indicator_sup <- Indicators_to_plot_quantiles[as.character(Indicators_to_plot_quantiles$Variable) == indicators_to_plot_names[ind] & as.character(Indicators_to_plot_quantiles$Stock) == BMT_SPECIES[ss] & as.numeric(as.character(Indicators_to_plot_quantiles$Year)) %in% all_the_years & Indicators_to_plot_quantiles$quantile == 0.95, ]
  
 one_indicator_inf_025 <- Indicators_to_plot_quantiles[as.character(Indicators_to_plot_quantiles$Variable) == indicators_to_plot_names[ind] & as.character(Indicators_to_plot_quantiles$Stock) == BMT_SPECIES[ss] & as.numeric(as.character(Indicators_to_plot_quantiles$Year)) %in% all_the_years & Indicators_to_plot_quantiles$quantile == 0.25, ]
 one_indicator_sup_075 <- Indicators_to_plot_quantiles[as.character(Indicators_to_plot_quantiles$Variable) == indicators_to_plot_names[ind] & as.character(Indicators_to_plot_quantiles$Stock) == BMT_SPECIES[ss] & as.numeric(as.character(Indicators_to_plot_quantiles$Year)) %in% all_the_years & Indicators_to_plot_quantiles$quantile == 0.75, ]  
  
}
}

if (indicators_to_plot_names[ind] == "F_Fref" | indicators_to_plot_names[ind] == "SSB_SSBref" ) {
    indicator_name <- as.character(one_indicator$Comments[1] )
} else {
    indicator_name <- as.character(indicators_to_plot_names[ind] )
}

fs_thisindicator <- as.character(unique(one_indicator$Fleet_segment))

by_fleet <-  !all(fs_thisindicator %in% c("ALL"))

for (fleet in 1:length(fs_thisindicator)) {
     
plot_title <- paste(indicator_name ," - ", BMT_SPECIES[ss], sep="")

if (phase =="SIMULATION") {                          # ifelse(as.character(one_indicator$Type[1]) == "Biol", "Biological indicators", "Pressure impact indicators")                        
  plot_path <- paste(casestudy_path, "/Diagnosis/Biological Pressure Impact/", casestudy_name, " - ", indicator_name, ifelse(by_fleet, paste(" " , fs_thisindicator[fleet] , sep=""), ""), " - ", BMT_SPECIES[ss], ".jpg", sep="")  
plot_title_sub <- paste( casestudy_name, " simulation [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
} else {
if (!MEY_CALCULATION) {                                       #ifelse(as.character(one_indicator$Type[1]) == "Biol", "Biological indicators", "Pressure impact indicators"),
  plot_path <- paste(casestudy_path, "/",harvest_rule_id,"/Biological Pressure Impact/", casestudy_name, " - ", indicator_name, ifelse(by_fleet, paste(" " , fs_thisindicator[fleet] , sep=""), ""), " - ", BMT_SPECIES[ss], " ", harvest_rule_id,".jpg", sep="")
   plot_title_sub <- paste( casestudy_name,  " ",harvest_rule_id , " [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
  } else {                                                                          # ifelse(as.character(one_indicator$Type[1]) == "Biol", "Biological indicators", "Pressure impact indicators")
    plot_path <- paste(casestudy_path, "/MEY calculation/",harvest_rule_id,"/Biological Pressure Impact/", casestudy_name, " - ", indicator_name, ifelse(by_fleet, paste(" " , fs_thisindicator[fleet] , sep=""), "") , " - ", BMT_SPECIES[ss], " ", harvest_rule_id,".jpg", sep="")
    plot_title_sub <- paste( casestudy_name, " ",harvest_rule_id , " [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
  }
}

one_indicator_fl <- one_indicator[as.character(one_indicator$Fleet_segment) == as.character(fs_thisindicator[fleet]), ]

#if (indicators_to_plot_names[ind] == "Discard_ratio")  {
#one_indicator_fl$Value <- one_indicator_fl$Value * 100
#}

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

#if (indicators_to_plot_names[ind] == "Discard_ratio")  {
#one_indicator_fl_inf$Value <- one_indicator_fl_inf$Value * 100
#one_indicator_fl_sup$Value <- one_indicator_fl_sup$Value * 100
#one_indicator_fl_inf_025$Value <- one_indicator_fl_inf_025$Value * 100
#one_indicator_fl_sup_075$Value <- one_indicator_fl_sup_075$Value * 100
#}

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

if (is.na(min_y)) {
min_y <- 0
} else {
min_y  <- ifelse(min_y<0, min_y, 0)
}


if (is.na(max_y)) {
max_y <- 0
} 

                                                                                                        
plot(as.numeric(as.character(one_indicator_fl$Year)),y_to_plot,type="l",xlab="Year",ylab=y_lab, ylim=c(min_y, (max_y + max_y*0.35 )), xlim=c(min(as.numeric(as.character(one_indicator_fl$Year))), max(as.numeric(as.character(one_indicator_fl$Year)))), axes=F, pch =16, lwd=2, col="blue", cex.lab=1.5, cex.main=1.8, main=plot_title) 
lines(as.numeric(as.character(one_indicator_fl$Year)), y_to_plot, type="p", col="blue", lwd=2, pch=19)     
                                                     # [to_be_plotted]
 
 if (phase == "FORECAST") {
    lines(rep(years[length(years)],2), c(min_y, (max_y + max_y*0.15 )), col="red", lty=2)
    text(rep(years[length(years)],2)+1,min_y, label="FORECAST", pos=3,  cex=0.55) 
  }  
                                                #  !is.null(BIOIndicators_quantiles)    
if (phase == "FORECAST" & exists("one_indicator_fl_inf") & indicators_to_plot_names[ind] != "Z" & indicators_to_plot_names[ind] != "F") {

if (indicators_to_plot_names[ind] == "Catch") {
print(as.numeric(as.character(y_to_plot_inf)))
print(as.numeric(as.character(y_to_plot_inf_025)) )
print(as.numeric(as.character(y_to_plot)))
print(as.numeric(as.character(y_to_plot_sup_075)))
print(as.numeric(as.character(y_to_plot_sup)))
       }
       
polygon(c(as.numeric(as.character(one_indicator_fl$Year)), rev(as.numeric(as.character(one_indicator_fl$Year)))), c(as.numeric(as.character(y_to_plot_inf)), rev(as.numeric(as.character(y_to_plot_inf_025)) )), col = alpha("black", 0.05), border = NA)
polygon(c(as.numeric(as.character(one_indicator_fl$Year)), rev(as.numeric(as.character(one_indicator_fl$Year)))), c(as.numeric(as.character(y_to_plot_inf_025)), rev(as.numeric(as.character(y_to_plot)) )), col = alpha("black", 0.15), border = NA)
polygon(c(as.numeric(as.character(one_indicator_fl$Year)), rev(as.numeric(as.character(one_indicator_fl$Year)))), c(as.numeric(as.character(y_to_plot)), rev(as.numeric(as.character(y_to_plot_sup_075)) )), col = alpha("black", 0.15), border = NA)
polygon(c(as.numeric(as.character(one_indicator_fl$Year)), rev(as.numeric(as.character(one_indicator_fl$Year)))), c(as.numeric(as.character(y_to_plot_sup_075)), rev(as.numeric(as.character(y_to_plot_sup)) )), col = alpha("black", 0.05), border = NA)
    
    lines(as.numeric(as.character(one_indicator_fl$Year)), as.numeric(as.character(y_to_plot_inf)) ,  type="l", lty=2)   	  
    lines(as.numeric(as.character(one_indicator_fl$Year)), as.numeric(as.character(y_to_plot_inf_025)) ,  type="l", lty=3) 
    lines(as.numeric(as.character(one_indicator_fl$Year)), as.numeric(as.character(y_to_plot_sup_075)) ,  type="l", lty=3) 
	  lines(as.numeric(as.character(one_indicator_fl$Year)), as.numeric(as.character(y_to_plot_sup)) ,  type="l", lty=2) 
	  legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","blue","black","black"), bty="n" )
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

}
 
 
 # saveSingleIndicatorPlot(years, name_bioind, name_pressind)                       
