# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





saveProfit_Plot <- function(all_the_years, econind_path, econind_path_quantiles) {


if (FALSE){
   all_the_years <- c(years)
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

#fleets_to_plot <- fleets_to_plot[fleets_to_plot != "ALL"]

for (ff in 1:length(fleets_to_plot)) {

profit_toplot <- ECOindicators[as.character(ECOindicators$Variable) == "net.profit" & as.character(ECOindicators$Fleet_segment) == fleets_to_plot[ff],1:(ncol(ECOindicators)-1)]
 # R/BER è il rapporto fra "total.revenues" e "Economic.indicators[break.even.revenue]".
total.revenues <- ECOindicators[as.character(ECOindicators$Variable) == "total.revenues" & as.character(ECOindicators$Fleet_segment) == fleets_to_plot[ff],1:(ncol(ECOindicators)-1)]
break.even.revenue <- ECOindicators[as.character(ECOindicators$Variable) == "Economic.indicators[break.even.revenue]" & as.character(ECOindicators$Fleet_segment) == fleets_to_plot[ff],1:(ncol(ECOindicators)-1)]
R_BER_toplot <- merge(total.revenues, break.even.revenue, by=c("Case_study", "Scenario", "ID_scenario", "Fleet_segment", "Species", "Year"))
R_BER_toplot <- R_BER_toplot[, c(1:6, 8, 11)]
colnames(R_BER_toplot) <-  c("Case_study", "Scenario", "ID_scenario" ,"Fleet_segment" ,"Species", "Year" , "total.revenues",  "break.even.revenue")
R_BER_toplot$R_BER <- ifelse(R_BER_toplot$break.even.revenue == 0, 0, R_BER_toplot$total.revenues / R_BER_toplot$break.even.revenue )


profit_toplot$Value <- as.numeric(as.character(profit_toplot$Value)) / 1000
R_BER_toplot$R_BER <- as.numeric(as.character(R_BER_toplot$R_BER)) 

profit_toplot$Year <- as.numeric(as.character(profit_toplot$Year))
R_BER_toplot$Year <- as.numeric(as.character(R_BER_toplot$Year))

if (!is.null(ECOIndicators_quantiles)) {

profit_toplot_1 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "net.profit" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.05, 1:(ncol(ECOIndicators_quantiles)-2)]
#profit_toplot_1 <- rbind(ECOindicators[ECOindicators$Year %in% years & as.character(ECOindicators$Variable) == "net.profit" & as.character(ECOindicators$Fleet_segment) == fleets_to_plot[ff],  1:(ncol(ECOindicators)-1)] , profit_toplot_1)

profit_toplot_2 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "net.profit" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.25, 1:(ncol(ECOIndicators_quantiles)-2)]
#profit_toplot_2 <- rbind(ECOindicators[ECOindicators$Year %in% years & as.character(ECOindicators$Variable) == "net.profit" & as.character(ECOindicators$Fleet_segment) == fleets_to_plot[ff], 1:(ncol(ECOindicators)-1)] , profit_toplot_2)

profit_toplot_4 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "net.profit" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.75, 1:(ncol(ECOIndicators_quantiles)-2)]
#profit_toplot_4 <- rbind(ECOindicators[ECOindicators$Year %in% years & as.character(ECOindicators$Variable) == "net.profit" & as.character(ECOindicators$Fleet_segment) == fleets_to_plot[ff], 1:(ncol(ECOindicators)-1)] , profit_toplot_4)

profit_toplot_5 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "net.profit" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.95, 1:(ncol(ECOIndicators_quantiles)-2)]
#profit_toplot_5 <- rbind(ECOindicators[ECOindicators$Year %in% years & as.character(ECOindicators$Variable) == "net.profit" & as.character(ECOindicators$Fleet_segment) == fleets_to_plot[ff], 1:(ncol(ECOindicators)-1)] , profit_toplot_5)

 # R/BER è il rapporto fra "total.revenues" e "Economic.indicators[break.even.revenue]".
revenues_1 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "total.revenues" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.05, 1:(ncol(ECOIndicators_quantiles)-2)]
#revenues_1 <- rbind(ECOindicators[ECOindicators$Year %in% years & as.character(ECOindicators$Variable) == "total.revenues" & as.character(ECOindicators$Fleet_segment) == fleets_to_plot[ff], 1:(ncol(ECOindicators)-1)] , revenues_1)

revenues_2 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "total.revenues" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.25, 1:(ncol(ECOIndicators_quantiles)-2)]
#revenues_2 <- rbind(ECOindicators[ECOindicators$Year %in% years & as.character(ECOindicators$Variable) == "total.revenues" & as.character(ECOindicators$Fleet_segment) == fleets_to_plot[ff], 1:(ncol(ECOindicators)-1)] , revenues_2)

revenues_4 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "total.revenues" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.75, 1:(ncol(ECOIndicators_quantiles)-2)]
#revenues_4 <- rbind(ECOindicators[ECOindicators$Year %in% years & as.character(ECOindicators$Variable) == "total.revenues" & as.character(ECOindicators$Fleet_segment) == fleets_to_plot[ff], 1:(ncol(ECOindicators)-1)] , revenues_4)

revenues_5 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "total.revenues" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.95, 1:(ncol(ECOIndicators_quantiles)-2)]
#revenues_5 <- rbind(ECOindicators[ECOindicators$Year %in% years & as.character(ECOindicators$Variable) == "total.revenues" & as.character(ECOindicators$Fleet_segment) == fleets_to_plot[ff], 1:(ncol(ECOindicators)-1)] , revenues_5)

BER_1 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "Economic.indicators[break.even.revenue]" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.05,1:(ncol(ECOIndicators_quantiles)-2)]
#BER_1 <- rbind(ECOindicators[ECOindicators$Year %in% years & as.character(ECOindicators$Variable) == "Economic.indicators[break.even.revenue]" & as.character(ECOindicators$Fleet_segment) == fleets_to_plot[ff], 1:(ncol(ECOindicators)-1)] , BER_1)

BER_2 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "Economic.indicators[break.even.revenue]" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.25,1:(ncol(ECOIndicators_quantiles)-2)]
#BER_2 <- rbind(ECOindicators[ECOindicators$Year %in% years & as.character(ECOindicators$Variable) == "Economic.indicators[break.even.revenue]" & as.character(ECOindicators$Fleet_segment) == fleets_to_plot[ff], 1:(ncol(ECOindicators)-1)] , BER_2)

BER_4 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "Economic.indicators[break.even.revenue]" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.75,1:(ncol(ECOIndicators_quantiles)-2)]
#BER_4 <- rbind(ECOindicators[ECOindicators$Year %in% years & as.character(ECOindicators$Variable) == "Economic.indicators[break.even.revenue]" & as.character(ECOindicators$Fleet_segment) == fleets_to_plot[ff], 1:(ncol(ECOindicators)-1)] , BER_4)

BER_5 <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "Economic.indicators[break.even.revenue]" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.95,1:(ncol(ECOIndicators_quantiles)-2)]
#BER_5 <- rbind(ECOindicators[ECOindicators$Year %in% years & as.character(ECOindicators$Variable) == "Economic.indicators[break.even.revenue]" & as.character(ECOindicators$Fleet_segment) == fleets_to_plot[ff], 1:(ncol(ECOindicators)-1)] , BER_5)

R_BER_toplot_1 <- merge(revenues_1, BER_1, by=c("Case_study", "Scenario", "ID_scenario", "Fleet_segment", "Species", "Year"))
R_BER_toplot_2 <- merge(revenues_2, BER_2, by=c("Case_study", "Scenario", "ID_scenario", "Fleet_segment", "Species", "Year"))
R_BER_toplot_4 <- merge(revenues_4, BER_4, by=c("Case_study", "Scenario", "ID_scenario", "Fleet_segment", "Species", "Year"))
R_BER_toplot_5 <- merge(revenues_5, BER_5, by=c("Case_study", "Scenario", "ID_scenario", "Fleet_segment", "Species", "Year"))

R_BER_toplot_1 <- R_BER_toplot_1[, c(1:6, 8, 11)]
R_BER_toplot_2 <- R_BER_toplot_2[, c(1:6, 8, 11)]
R_BER_toplot_4 <- R_BER_toplot_4[, c(1:6, 8, 11)]
R_BER_toplot_5 <- R_BER_toplot_5[, c(1:6, 8, 11)]

colnames(R_BER_toplot_1) <-  c("Case_study", "Scenario", "ID_scenario" ,"Fleet_segment" ,"Species", "Year" , "total.revenues",  "break.even.revenue")
colnames(R_BER_toplot_2) <-  c("Case_study", "Scenario", "ID_scenario" ,"Fleet_segment" ,"Species", "Year" , "total.revenues",  "break.even.revenue")
colnames(R_BER_toplot_4) <-  c("Case_study", "Scenario", "ID_scenario" ,"Fleet_segment" ,"Species", "Year" , "total.revenues",  "break.even.revenue")
colnames(R_BER_toplot_5) <-  c("Case_study", "Scenario", "ID_scenario" ,"Fleet_segment" ,"Species", "Year" , "total.revenues",  "break.even.revenue")

R_BER_toplot_1$R_BER <- ifelse(R_BER_toplot_1$break.even.revenue == 0, 0, R_BER_toplot_1$total.revenues / R_BER_toplot_1$break.even.revenue )
R_BER_toplot_2$R_BER <- ifelse(R_BER_toplot_2$break.even.revenue == 0, 0, R_BER_toplot_2$total.revenues / R_BER_toplot_2$break.even.revenue )
R_BER_toplot_4$R_BER <- ifelse(R_BER_toplot_4$break.even.revenue == 0, 0, R_BER_toplot_4$total.revenues / R_BER_toplot_4$break.even.revenue )
R_BER_toplot_5$R_BER <- ifelse(R_BER_toplot_5$break.even.revenue == 0, 0, R_BER_toplot_5$total.revenues / R_BER_toplot_5$break.even.revenue )

profit_toplot_1$Value <- as.numeric(as.character(profit_toplot_1$Value)) / 1000
profit_toplot_2$Value <- as.numeric(as.character(profit_toplot_2$Value)) / 1000
profit_toplot_4$Value <- as.numeric(as.character(profit_toplot_4$Value)) / 1000
profit_toplot_5$Value <- as.numeric(as.character(profit_toplot_5$Value)) / 1000

R_BER_toplot_1$R_BER <- as.numeric(as.character(R_BER_toplot_1$R_BER)) 
R_BER_toplot_2$R_BER <- as.numeric(as.character(R_BER_toplot_2$R_BER)) 
R_BER_toplot_4$R_BER <- as.numeric(as.character(R_BER_toplot_4$R_BER)) 
R_BER_toplot_5$R_BER <- as.numeric(as.character(R_BER_toplot_5$R_BER)) 

profit_toplot$Year <- as.numeric(as.character(profit_toplot$Year))
profit_toplot_1$Year <- as.numeric(as.character(profit_toplot_1$Year))
profit_toplot_2$Year <- as.numeric(as.character(profit_toplot_2$Year))
profit_toplot_4$Year <- as.numeric(as.character(profit_toplot_4$Year))
profit_toplot_5$Year <- as.numeric(as.character(profit_toplot_5$Year))

R_BER_toplot_1 <- R_BER_toplot_1[order(R_BER_toplot_1$Year),]
R_BER_toplot_2 <- R_BER_toplot_2[order(R_BER_toplot_2$Year),]
R_BER_toplot_4 <- R_BER_toplot_4[order(R_BER_toplot_4$Year),]
R_BER_toplot_5 <- R_BER_toplot_5[order(R_BER_toplot_5$Year),]

#R_BER_toplot_inf <- merge(total.revenues_inf, break.even.revenue_inf, by=c("Case_study", "Scenario", "ID_scenario", "Fleet_segment", "Species", "Year"))
#R_BER_toplot_inf <- R_BER_toplot_inf[, c(1:6, 8, 11)]
#colnames(R_BER_toplot_inf) <-  c("Case_study", "Scenario", "ID_scenario" ,"Fleet_segment" ,"Species", "Year" , "total.revenues",  "break.even.revenue")
#R_BER_toplot_inf$R_BER <- ifelse(R_BER_toplot_inf$break.even.revenue == 0, 0, R_BER_toplot_inf$total.revenues / R_BER_toplot_inf$break.even.revenue )

#net.profit_toplot_inf$Value <- as.numeric(as.character(net.profit_toplot_inf$Value))
#R_BER_toplot_inf$R_BER <- as.numeric(as.character(R_BER_toplot_inf$R_BER))

#net.profit_toplot_inf$Year <- as.numeric(as.character(net.profit_toplot_inf$Year))
#R_BER_toplot_inf$Year <- as.numeric(as.character(R_BER_toplot_inf$Year))

#net.profit_toplot_sup <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "net.profit" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.95, 1:(ncol(ECOIndicators_quantiles)-2)]
# # R/BER è il rapporto fra "total.revenues" e "Economic.indicators[break.even.revenue]".
#total.revenues_sup <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "total.revenues" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.95, 1:(ncol(ECOIndicators_quantiles)-2)]
#break.even.revenue_sup <- ECOIndicators_quantiles[as.character(ECOIndicators_quantiles$Variable) == "Economic.indicators[break.even.revenue]" & as.character(ECOIndicators_quantiles$Fleet_segment) == fleets_to_plot[ff] & ECOIndicators_quantiles[, ncol(ECOIndicators_quantiles)] == 0.95,1:(ncol(ECOIndicators_quantiles)-2)]
#R_BER_toplot_sup <- merge(total.revenues_sup, break.even.revenue_sup, by=c("Case_study", "Scenario", "ID_scenario", "Fleet_segment", "Species", "Year"))
#R_BER_toplot_sup <- R_BER_toplot_sup[, c(1:6, 8, 11)]
#colnames(R_BER_toplot_sup) <-  c("Case_study", "Scenario", "ID_scenario" ,"Fleet_segment" ,"Species", "Year" , "total.revenues",  "break.even.revenue")
#R_BER_toplot_sup$R_BER <- ifelse(R_BER_toplot_sup$break.even.revenue == 0, 0, R_BER_toplot_sup$total.revenues / R_BER_toplot_sup$break.even.revenue )
#
#net.profit_toplot_sup$Value <- as.numeric(as.character(net.profit_toplot_sup$Value))
#R_BER_toplot_sup$R_BER <- as.numeric(as.character(R_BER_toplot_sup$R_BER))
#
#net.profit_toplot_sup$Year <- as.numeric(as.character(net.profit_toplot_sup$Year))
#R_BER_toplot_sup$Year <- as.numeric(as.character(R_BER_toplot_sup$Year))
}


plot_title <- paste("Net profit-R/BER - ", ifelse(fleets_to_plot[ff] == "ALL", "Overall", fleets_to_plot[ff]) , sep="")

if (phase =="SIMULATION") {                                               
  plot_path <- paste(casestudy_path, "/Diagnosis/Economic indicators/", casestudy_name, " - Net profit-R_BER ",fleets_to_plot[ff], ".jpg", sep="")
    plot_title_sub <- paste( casestudy_name, " simulation [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
} else {
if (!MEY_CALCULATION) {
  plot_path <- paste(casestudy_path, "/",harvest_rule_id,"/Economic indicators/", casestudy_name, " - Net profit-R_BER ",fleets_to_plot[ff], " ", harvest_rule_id,".jpg", sep="")
   plot_title_sub <- paste( casestudy_name, " ",harvest_rule_id , " [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
  } else {
  plot_path <- paste(casestudy_path, "/MEY calculation/",harvest_rule_id,"/Economic indicators/", casestudy_name, " - Net profit-R_BER ",fleets_to_plot[ff], " ", harvest_rule_id,".jpg", sep="")
    plot_title_sub <- paste( casestudy_name, " ",harvest_rule_id , " [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
  }
}

min_y_profit <- min(profit_toplot$Value, na.omit=T)
if (is.na(min_y_profit)) {
min_y_profit <- 0
} else {
min_y_profit  <- ifelse(min_y_profit<0, min_y_profit, 0)
}

if (!is.null(ECOIndicators_quantiles)) {
min_y_profit  <- min(min_y_profit, profit_toplot_1$Value, na.omit=T)
}


max_y_profit <- max(profit_toplot$Value, na.omit=T)
if (is.na(max_y_profit)) {
max_y_profit <- 0
}

if (!is.null(ECOIndicators_quantiles)) {
max_y_profit  <- max(max_y_profit, profit_toplot_5$Value , na.omit=T)
}

y_lim <- c(min_y_profit, (max_y_profit + (max_y_profit - min_y_profit )*0.3 ))

    # windows()
jpeg(file=plot_path, width=21, height=21, bg="white", units="cm",res=200)
par(mar=c(6, 7, 8, 7))  # c(bottom, left, top, right)                                                                                                           
plot(as.numeric(as.character(profit_toplot$Year)), as.numeric(as.character(profit_toplot$Value)),type="l",xlab="Year", ylab="", axes=F, col="blue", lwd=2, ylim=y_lim, main=plot_title, cex.lab=1.5, cex.main=1.8)
lines(as.numeric(as.character(profit_toplot$Year)), as.numeric(as.character(profit_toplot$Value)), type="p", col="blue", lwd=2, pch=19) 


if (!is.null(ECOIndicators_quantiles)) {

polygon(c(as.numeric(as.character(profit_toplot$Year)), rev(as.numeric(as.character(profit_toplot$Year)))), c( profit_toplot_1$Value, rev(profit_toplot_2$Value)), col = alpha("blue", 0.05), border = NA)
polygon(c(as.numeric(as.character(profit_toplot$Year)), rev(as.numeric(as.character(profit_toplot$Year)))), c(profit_toplot_2$Value, rev(profit_toplot$Value)), col = alpha("blue", 0.15), border = NA)
polygon(c(as.numeric(as.character(profit_toplot$Year)), rev(as.numeric(as.character(profit_toplot$Year)))),c(profit_toplot$Value, rev(profit_toplot_4$Value)), col = alpha("blue", 0.15), border = NA)
polygon(c(as.numeric(as.character(profit_toplot$Year)), rev(as.numeric(as.character(profit_toplot$Year)))),c(profit_toplot_4$Value, rev(profit_toplot_5$Value)), col = alpha("blue", 0.05), border = NA)
    
    lines(as.numeric(as.character(profit_toplot$Year)), profit_toplot_1$Value, col="blue" , type="l", lty=2) 
    	lines(as.numeric(as.character(profit_toplot$Year)),profit_toplot_2$Value , col="blue", type="l", lty=3) 
    	lines(as.numeric(as.character(profit_toplot$Year)),profit_toplot_4$Value, col="blue", type="l", lty=3) 
       lines(as.numeric(as.character(profit_toplot$Year)), profit_toplot_5$Value, col="blue",  type="l", lty=2) 
       
   legend ("topleft", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median net profit", "0.75", "0.95"),col=c("blue","blue","blue","blue","blue"), bty="n")
legend ("topright", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median R/BER", "0.75", "0.95"),col=c("red","red","red","red","red"), bty="n")


#     lines(as.numeric(as.character(net.profit_toplot_inf$Year)),as.numeric(as.character(net.profit_toplot_inf$Value))/1000 , col="blue",  type="l", lty=2) 
#	  lines(as.numeric(as.character(net.profit_toplot_sup$Year)),as.numeric(as.character(net.profit_toplot_sup$Value))/1000 , col="blue",  type="l", lty=3) 
#	  legend ("bottomright", lwd=c(1,1),lty=c(2,3),legend=c( "0.05", "0.95"),col=c("black","black"), bty="n" )
}
       
axis(1, at=profit_toplot$Year, cex.axis=1.5 )
axis(2, las=1, cex.axis=1.5)  ## las=1 makes horizontal labels
mtext(",000 €",col="blue",side=2,line=5, cex=1.5)
mtext(plot_title_sub, 3, line=1, cex=1.3)

 if (phase == "FORECAST") {
    lines(rep(years[length(years)],2), y_lim, col="red", lty=2)
    text(rep(years[length(years)],2)+1,y_lim[2], label="FORECAST", pos=1, cex=0.55) 
  }  

 mtext( BMT_sw_version,side=4,line=6,outer=FALSE)
 
par(new=TRUE)


min_y_BER <- min(R_BER_toplot$R_BER, na.omit=T)
if (is.na(min_y_BER)) {
min_y_BER <- 0
} else {
min_y_BER  <- ifelse(min_y_BER<0, min_y_BER, 0)
}

if (!is.null(ECOIndicators_quantiles)) {
min_y_BER  <- min(min_y_BER, as.numeric(as.character(R_BER_toplot_1$R_BER)) , na.omit=T)
}


max_y_BER <- max(R_BER_toplot$R_BER, na.omit=T)
if (is.na(max_y_BER)) {
max_y_BER <- 0
} 

if (!is.null(ECOIndicators_quantiles)) {
max_y_BER  <- max(max_y_BER, as.numeric(as.character(R_BER_toplot_5$R_BER)) , na.omit=T)
}

y_lim =  c(min_y_BER, max_y_BER + (max_y_BER - min_y_BER )*0.3 )


plot(R_BER_toplot$Year, R_BER_toplot$R_BER,type="l",xlab="", ylab="", axes=F, col="red", lwd=2, ylim= y_lim) 
lines(R_BER_toplot$Year, R_BER_toplot$R_BER, type="p", col="red", lwd=2, pch=19) 

if (!is.null(ECOIndicators_quantiles)) {

polygon(c(R_BER_toplot$Year, rev(R_BER_toplot$Year)), c( R_BER_toplot_1$R_BER, rev(R_BER_toplot_2$R_BER)), col = alpha("red", 0.05), border = NA)
polygon(c(R_BER_toplot$Year, rev(R_BER_toplot$Year)), c(R_BER_toplot_2$R_BER, rev(R_BER_toplot$R_BER)), col = alpha("red", 0.15), border = NA)
polygon(c(R_BER_toplot$Year, rev(R_BER_toplot$Year)),c(R_BER_toplot$R_BER, rev(R_BER_toplot_4$R_BER)), col = alpha("red", 0.15), border = NA)
polygon(c(R_BER_toplot$Year, rev(R_BER_toplot$Year)),c(R_BER_toplot_4$R_BER, rev(R_BER_toplot_5$R_BER)), col = alpha("red", 0.05), border = NA)
    
    lines(R_BER_toplot$Year, R_BER_toplot_1$R_BER, col="red", type="l", lty=2) 
    	lines(R_BER_toplot$Year,R_BER_toplot_2$R_BER , col="red", type="l", lty=3) 
    	lines(R_BER_toplot$Year,R_BER_toplot_4$R_BER, col="red", type="l", lty=3) 
       lines(R_BER_toplot$Year, R_BER_toplot_5$R_BER, col="red", type="l", lty=2) 


#R_BER_toplot_inf = R_BER_toplot_inf[order(as.numeric(as.character(R_BER_toplot_inf$Year))),]
#R_BER_toplot_sup = R_BER_toplot_sup[order(as.numeric(as.character(R_BER_toplot_sup$Year))),]
#
#     lines(as.numeric(as.character(R_BER_toplot_inf$Year)),as.numeric(as.character(R_BER_toplot_inf$R_BER))*100 , col="red",  type="l", lty=2) 
#	  lines(as.numeric(as.character(R_BER_toplot_sup$Year)),as.numeric(as.character(R_BER_toplot_sup$R_BER))*100 , col="red",  type="l", lty=3) 
	  # legend ("bottomright", lwd=c(1,1),lty=c(2,3),legend=c( "0.05", "0.95"),col=c("black","black"), bty="n" )
}
 
axis(4, las=1, cex.axis=1.5) 
mtext("R/BER",col="red",side=4,line=5, cex=1.5)       
box()
#legend("top", c("Net profit","R/BER" ), border="white", pch=16, col=c( "blue", "red"), horiz=T, fill=NULL, bty="n", cex=1.3)
dev.off() 

}

}

#saveProfit_Plot(years, name_econind)
                        