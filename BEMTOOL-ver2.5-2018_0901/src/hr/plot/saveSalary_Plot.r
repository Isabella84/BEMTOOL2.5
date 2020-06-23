# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





saveSalary_Plot <- function(all_the_years, econind_path, econind_path_quantiles) {


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

#BIOindicators_temp <- BIOindicators[as.character(BIOindicators$Year) != "ALL", ]

for (ff in 1:length(fleets_to_plot)) {

employment_toplot <- ECOindicators[as.character(ECOindicators$Variable) == "employment" & as.character(ECOindicators$Fleet_segment) == fleets_to_plot[ff],1:(ncol(ECOindicators)-1)]
wage_toplot <- ECOindicators[as.character(ECOindicators$Variable) == "average.wage" & as.character(ECOindicators$Fleet_segment) == fleets_to_plot[ff],1:(ncol(ECOindicators)-1)]

employment_toplot$Value <- as.numeric(as.character(employment_toplot$Value))
#wage_toplot$Value <- as.numeric(as.character(wage_toplot$Value))  /1000

employment_toplot$Year <- as.numeric(as.character(employment_toplot$Year))
wage_toplot$Year <- as.numeric(as.character(wage_toplot$Year))


if (!is.null(ECOIndicators_quantiles)) {
employment_toplot_1 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "employment" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[,ncol(ECOIndicators_quantiles)] == 0.05, 1:(ncol(ECOIndicators_quantiles)-2)]
wage_toplot_1 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "average.wage" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[,ncol(ECOIndicators_quantiles)] == 0.05,1:(ncol(ECOIndicators_quantiles)-2)]

#employment_toplot_1 <- rbind(employment_toplot[employment_toplot$Year %in% years,] , employment_toplot_1)
#wage_toplot_1 <- rbind(wage_toplot[wage_toplot$Year %in% years,] , wage_toplot_1)

employment_toplot_2 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "employment" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[,ncol(ECOIndicators_quantiles)] == 0.25, 1:(ncol(ECOIndicators_quantiles)-2)]
wage_toplot_2 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "average.wage" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[,ncol(ECOIndicators_quantiles)] == 0.25,1:(ncol(ECOIndicators_quantiles)-2)]

#employment_toplot_2 <- rbind(employment_toplot[employment_toplot$Year %in% years,] , employment_toplot_2)
#wage_toplot_2 <- rbind(wage_toplot[wage_toplot$Year %in% years,] , wage_toplot_2)

employment_toplot_4 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "employment" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[,ncol(ECOIndicators_quantiles)] == 0.75, 1:(ncol(ECOIndicators_quantiles)-2)]
wage_toplot_4 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "average.wage" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[,ncol(ECOIndicators_quantiles)] == 0.75,1:(ncol(ECOIndicators_quantiles)-2)]

#employment_toplot_4 <- rbind(employment_toplot[employment_toplot$Year %in% years,] , employment_toplot_4)
#wage_toplot_4 <- rbind(wage_toplot[wage_toplot$Year %in% years,] , wage_toplot_4)

employment_toplot_5 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "employment" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[,ncol(ECOIndicators_quantiles)] == 0.95, 1:(ncol(ECOIndicators_quantiles)-2)]
wage_toplot_5 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "average.wage" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[,ncol(ECOIndicators_quantiles)] == 0.95,1:(ncol(ECOIndicators_quantiles)-2)]

#employment_toplot_5 <- rbind(employment_toplot[employment_toplot$Year %in% years,] , employment_toplot_5)
#wage_toplot_5 <- rbind(wage_toplot[wage_toplot$Year %in% years,] , wage_toplot_5)

employment_toplot_1$Value <- as.numeric(as.character(employment_toplot_1$Value)) 
wage_toplot_1$Value <- as.numeric(as.character(wage_toplot_1$Value))  /1000

employment_toplot_2$Value <- as.numeric(as.character(employment_toplot_2$Value))
wage_toplot_2$Value <- as.numeric(as.character(wage_toplot_2$Value)) /1000

employment_toplot_4$Value <- as.numeric(as.character(employment_toplot_4$Value))
wage_toplot_4$Value <- as.numeric(as.character(wage_toplot_4$Value)) /1000

employment_toplot_5$Value <- as.numeric(as.character(employment_toplot_5$Value))
wage_toplot_5$Value <- as.numeric(as.character(wage_toplot_5$Value)) /1000

employment_toplot_1$Year <- as.numeric(as.character(employment_toplot_1$Year))
wage_toplot_1$Year <- as.numeric(as.character(wage_toplot_1$Year))

employment_toplot_2$Year <- as.numeric(as.character(employment_toplot_2$Year))
wage_toplot_2$Year <- as.numeric(as.character(wage_toplot_2$Year))

employment_toplot_4$Year <- as.numeric(as.character(employment_toplot_4$Year))
wage_toplot_4$Year <- as.numeric(as.character(wage_toplot_4$Year))

employment_toplot_5$Year <- as.numeric(as.character(employment_toplot_5$Year))
wage_toplot_5$Year <- as.numeric(as.character(wage_toplot_5$Year))

}

wage_toplot$Value <- as.numeric(as.character(wage_toplot$Value))  /1000


  plot_title <- paste("Avg.salary-Employment - ", ifelse(fleets_to_plot[ff] == "ALL", "Overall", fleets_to_plot[ff]) , sep="")

if (phase =="SIMULATION") {                                               
  plot_path <- paste(casestudy_path, "/Diagnosis/Economic indicators/", casestudy_name, " - Avg.salary-Employment ",fleets_to_plot[ff], ".jpg", sep="")
 # plot_title <- paste("[", casestudy_name, "] Avg.salary-Employment ", fleets_to_plot[ff], " SIM", sep="")
   plot_title_sub <- paste( casestudy_name, " simulation [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
} else {
if (!MEY_CALCULATION) {
  plot_path <- paste(casestudy_path, "/",harvest_rule_id,"/Economic indicators/", casestudy_name, " - Avg.salary-Employment ",fleets_to_plot[ff], " ", harvest_rule_id,".jpg", sep="")
  #plot_title <- paste("[", casestudy_name, "] Avg.salary-Employment ", fleets_to_plot[ff], " FORE ", harvest_rule_id, sep="")
   plot_title_sub <- paste( casestudy_name, " ",harvest_rule_id , " [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
  } else {
    plot_path <- paste(casestudy_path, "/MEY calculation/",harvest_rule_id,"/Economic indicators/", casestudy_name, " - Avg.salary-Employment ",fleets_to_plot[ff], " ", harvest_rule_id,".jpg", sep="")
 # plot_title <- paste("[", casestudy_name, "] Avg.salary-Employment ", fleets_to_plot[ff], " ", harvest_rule_id, sep="")
    plot_title_sub <- paste( casestudy_name, " ",harvest_rule_id , " [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")

  }
}


min_y_salary <- min(wage_toplot$Value, na.omit=T) 
if (is.na(min_y_salary)) {
min_y_salary <- 0
} else {
min_y_salary  <- ifelse(min_y_salary<0, min_y_salary, 0)
}

if (!is.null(ECOIndicators_quantiles)) {
min_y_salary  <- min(min_y_salary, min(as.numeric(as.character(wage_toplot_1$Value)), na.omit=T) , na.omit=T)
}


max_y_salary <- max(wage_toplot$Value, na.omit=T)
if (is.na(max_y_salary)) {
max_y_salary <- 0
} 

if (!is.null(ECOIndicators_quantiles)) {
max_y_salary  <- max(max_y_salary, max(as.numeric(as.character(wage_toplot_5$Value)), na.omit=T), na.omit=T)
}

y_lim =  c(min_y_salary, max_y_salary + (max_y_salary - min_y_salary )*0.3 )
   
jpeg(file=plot_path, width=21, height=21, bg="white", units="cm",res=200)
#windows()  
par(mar=c(6, 7, 8, 7))  # c(bottom, left, top, right)    # ylim=c(min(min(average.wage_toplot$Value),min(employment_toplot$Value)),max(max(average.wage_toplot$Value),max(employment_toplot$Value)))/1000,         
plot(wage_toplot$Year,wage_toplot$Value,type="l",xlab="Year", ylab="", axes=F, col="red", lwd=2, ylim=y_lim, main=plot_title, cex.lab=1.5, cex.main=1.8)
lines(wage_toplot$Year,wage_toplot$Value, type="p", col="red", lwd=2, pch=19)        
axis(1, at=wage_toplot$Year , cex.axis=1.5)
axis(2, las=1, cex.axis=1.5)  ## las=1 makes horizontal labels

 if (phase == "FORECAST") {
    lines(rep(years[length(years)],2), y_lim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,y_lim[2], label="FORECAST", pos=1, cex=0.55) 
  }  

if (!is.null(ECOIndicators_quantiles)) {

 polygon(c(as.numeric(as.character(wage_toplot$Year)), rev(as.numeric(as.character(wage_toplot$Year)))), c( wage_toplot_1$Value, rev(wage_toplot_2$Value)), col = alpha("red", 0.05), border = NA)
polygon(c(as.numeric(as.character(wage_toplot$Year)), rev(as.numeric(as.character(wage_toplot$Year)))), c(wage_toplot_2$Value, rev(wage_toplot$Value)), col = alpha("red", 0.15), border = NA)
polygon(c(as.numeric(as.character(wage_toplot$Year)), rev(as.numeric(as.character(wage_toplot$Year)))),c(wage_toplot$Value, rev(wage_toplot_4$Value)), col = alpha("red", 0.15), border = NA)
polygon(c(as.numeric(as.character(wage_toplot$Year)), rev(as.numeric(as.character(wage_toplot$Year)))),c(wage_toplot_4$Value, rev(wage_toplot_5$Value)), col = alpha("red", 0.05), border = NA)
    
    lines(as.numeric(as.character(wage_toplot$Year)), wage_toplot_1$Value, col="red", type="l", lty=2) 
    	lines(as.numeric(as.character(wage_toplot$Year)),wage_toplot_2$Value , col="red", type="l", lty=3) 
    	lines(as.numeric(as.character(wage_toplot$Year)), wage_toplot_4$Value , col="red", type="l", lty=3) 
       lines(as.numeric(as.character(wage_toplot$Year)), wage_toplot_5$Value, col="red", type="l", lty=2) 

legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median wage", "0.75", "0.95"),col=c("red","red","red","red","red"), bty="n")
legend ("topright", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median empl", "0.75", "0.95"),col=c("blue","blue","blue","blue","blue"), bty="n")

#     lines(as.numeric(as.character(average.wage_toplot_inf$Year)),as.numeric(as.character(average.wage_toplot_inf$Value))/1000 , col="red",  type="l", lty=2) 
#	  lines(as.numeric(as.character(average.wage_toplot_sup$Year)),as.numeric(as.character(average.wage_toplot_sup$Value))/1000 , col="red",  type="l", lty=3) 
#	  legend ("bottomright", lwd=c(1,1),lty=c(2,3),legend=c( "0.05", "0.95"),col=c("black","black"), bty="n" )
}

mtext(",000 ?",col="red",side=2,line=4, cex=1.5)
mtext(plot_title_sub, 3, line=1, cex=1.3)

 mtext( BMT_sw_version,side=4,line=5,outer=FALSE)

par(new=TRUE)


min_y_empl <- min(employment_toplot$Value, na.omit=T)
if (is.na(min_y_empl)) {
min_y_empl <- 0
} else {
min_y_empl  <- ifelse(min_y_empl<0, min_y_empl, 0)
}

if (!is.null(ECOIndicators_quantiles)) {
min_y_empl  <- min(min_y_empl, min(as.numeric(as.character(employment_toplot_1$Value)), na.omit=T) , na.omit=T)
}


max_y_empl <- max(employment_toplot$Value, na.omit=T)
if (is.na(max_y_empl)) {
max_y_empl <- 0
}

if (!is.null(ECOIndicators_quantiles)) {
max_y_empl  <- max(max_y_empl, max(as.numeric(as.character(employment_toplot_5$Value)), na.omit=T) , na.omit=T)
}
  


plot(employment_toplot$Year,employment_toplot$Value,type="l",xlab="", ylab="", axes=F, col="blue", lwd=2, ylim=c(min_y_empl, max_y_empl + (max_y_empl - min_y_empl )*0.2 ))  
lines(employment_toplot$Year,employment_toplot$Value, type="p", col="blue", lwd=2, pch=19)  
axis(4, las=1, cex.axis=1.5) 
mtext("num.",col="blue",side=4,line=4, cex=1.5)

if (!is.null(ECOIndicators_quantiles)) {

 polygon(c(as.numeric(as.character(employment_toplot$Year)), rev(as.numeric(as.character(employment_toplot$Year)))), c( employment_toplot_1$Value, rev(employment_toplot_2$Value)), col = alpha("blue", 0.05), border = NA)
polygon(c(as.numeric(as.character(employment_toplot$Year)), rev(as.numeric(as.character(employment_toplot$Year)))), c(employment_toplot_2$Value, rev(employment_toplot$Value)), col = alpha("blue", 0.15), border = NA)
polygon(c(as.numeric(as.character(employment_toplot$Year)), rev(as.numeric(as.character(employment_toplot$Year)))),c(employment_toplot$Value, rev(employment_toplot_4$Value)), col = alpha("blue", 0.15), border = NA)
polygon(c(as.numeric(as.character(employment_toplot$Year)), rev(as.numeric(as.character(employment_toplot$Year)))),c(employment_toplot_4$Value, rev(employment_toplot_5$Value)), col = alpha("blue", 0.05), border = NA)
    
    lines(as.numeric(as.character(employment_toplot$Year)), employment_toplot_1$Value, col="blue", type="l", lty=2) 
    	lines(as.numeric(as.character(employment_toplot$Year)),employment_toplot_2$Value , col="blue", type="l", lty=3) 
    	lines(as.numeric(as.character(employment_toplot$Year)), employment_toplot_4$Value , col="blue", type="l", lty=3) 
       lines(as.numeric(as.character(employment_toplot$Year)), employment_toplot_5$Value, col="blue", type="l", lty=2) 

	 # legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"),col=c("black","black","blue","black","black"), bty="n")

#     lines(as.numeric(as.character(employment_toplot_inf$Year)),as.numeric(as.character(employment_toplot_inf$Value)) , col="blue",  type="l", lty=2) 
#	  lines(as.numeric(as.character(employment_toplot_sup$Year)),as.numeric(as.character(employment_toplot_sup$Value)), col="blue",  type="l", lty=3) 
	#  legend ("bottomright", lwd=c(1,1),lty=c(2,3),legend=c( "0.05", "0.95"),col=c("black","black"), bty="n" )
}

box()
#legend("top", c("Avg. salary", "Employment"), border="white", pch=16, col=c("red", "blue"), horiz=T, fill=NULL, bty="n",  cex=1.3)
dev.off() 

}

}

# saveSalary_Plot(years, name_econind)                       
