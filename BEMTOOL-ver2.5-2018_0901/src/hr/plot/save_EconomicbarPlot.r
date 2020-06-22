# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





save_EconomicbarPlot <- function(all_the_years, econind_path, econind_path_quantiles) {

if (FALSE){
   all_the_years <- years
   econind_path <- name_econind
   econind_path_quantiles <- "" 
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
ECOindicators <- ECOindicators[ECOindicators$Year != "ALL",]
ECOindicators_fl <- ECOindicators[as.numeric(as.character(ECOindicators$Year)) %in% all_the_years & (as.character(ECOindicators$Variable) == "profit" | as.character(ECOindicators$Variable) == "labour.cost" | as.character(ECOindicators$Variable) == "capital.cost[tot.cap.cost]") & as.character(ECOindicators$Fleet_segment) == fleets_to_plot[ff],1:(ncol(ECOindicators)-1)]

if (!is.null(ECOIndicators_quantiles)) {
  ECOIndicators_quantiles_inf <-  ECOIndicators_quantiles[ECOIndicators_quantiles$Year != "ALL" & ECOIndicators_quantiles[,ncol(ECOIndicators_quantiles)] == 0.05,]
  ECOIndicators_quantiles_sup <-  ECOIndicators_quantiles[ECOIndicators_quantiles$Year != "ALL" & ECOIndicators_quantiles[,ncol(ECOIndicators_quantiles)] == 0.95,]
  
  ECOindicators_fl_inf <- ECOIndicators_quantiles_inf[as.numeric(as.character(ECOIndicators_quantiles_inf$Year)) %in% all_the_years & (as.character(ECOIndicators_quantiles_inf$Variable) == "profit" | as.character(ECOIndicators_quantiles_inf$Variable) == "labour.cost" | as.character(ECOIndicators_quantiles_inf$Variable) == "capital.cost[tot.cap.cost]") & as.character(ECOIndicators_quantiles_inf$Fleet_segment) == fleets_to_plot[ff],1:(ncol(ECOIndicators_quantiles_inf)-2)]
ECOindicators_fl_sup <- ECOIndicators_quantiles_sup[as.numeric(as.character(ECOIndicators_quantiles_sup$Year)) %in% all_the_years & (as.character(ECOIndicators_quantiles_sup$Variable) == "profit" | as.character(ECOIndicators_quantiles_sup$Variable) == "labour.cost" | as.character(ECOIndicators_quantiles_sup$Variable) == "capital.cost[tot.cap.cost]") & as.character(ECOIndicators_quantiles_sup$Fleet_segment) == fleets_to_plot[ff],1:(ncol(ECOIndicators_quantiles_sup)-2)]

}


   plot_title <- paste("Profit-Salary-Capital - ", ifelse(fleets_to_plot[ff] == "ALL", "Overall", fleets_to_plot[ff]) , sep="")
   
if (phase =="SIMULATION") {                                               
  plot_path <- paste(casestudy_path, "/Diagnosis/Economic indicators/", casestudy_name, " - Profit-Salary-Capital ",fleets_to_plot[ff], ".jpg", sep="")
  plot_title_sub <- paste( casestudy_name, " simulation [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
} else {
if (!MEY_CALCULATION) {
  plot_path <- paste(casestudy_path, "/",harvest_rule_id,"/Economic indicators/", casestudy_name, " - Profit-Salary-Capital ",fleets_to_plot[ff], " ", harvest_rule_id,".jpg", sep="")
     plot_title_sub <- paste( casestudy_name, " ", harvest_rule_id , " [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
    } else {
      plot_path <- paste(casestudy_path, "/MEY calculation/",harvest_rule_id,"/Economic indicators/", casestudy_name, " - Profit-Salary-Capital ",fleets_to_plot[ff], " ", harvest_rule_id,".jpg", sep="")
      plot_title_sub <- paste( casestudy_name, " ",harvest_rule_id , " [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
  }
}

#windows()

jpeg(file=plot_path, width=21, height=21, bg="white", units="cm",res=200)
par(mar=c(6, 8, 8, 7))  # c(bottom, left, top, right)                                                                                                           
counts <- table(as.character(ECOindicators_fl$Variable),ECOindicators_fl$Year)

counts <- counts[, colnames(counts) != "ALL"]

for (nr in 1:nrow(counts)) {
    for (nc in 1:ncol(counts)) {
        counts[nr, nc] <- as.numeric(as.character(ECOindicators_fl$Value[as.character(ECOindicators_fl$Variable) == rownames(counts)[nr] & as.character(ECOindicators_fl$Year) == colnames(counts)[nc]]))/1000
    }
}

min_y <- ifelse(min(counts, na.rm=T)>0, 0, min(counts, na.rm=T))
max_y <- ifelse(max(counts, na.rm=T)<0, 0, max(counts, na.rm=T)) 


if (!is.null(ECOIndicators_quantiles)) {

counts_inf <- table(as.character(ECOindicators_fl_inf$Variable),ECOindicators_fl_inf$Year)
counts_inf <- counts_inf[, colnames(counts_inf) != "ALL"]

for (nr in 1:nrow(counts_inf)) {
    for (nc in 1:ncol(counts_inf)) {
    if (as.numeric(colnames(counts_inf)[nc]) %in% years) {
  counts_inf[nr, nc] <- as.numeric(as.character(ECOindicators_fl$Value[as.character(ECOindicators_fl$Variable) == rownames(counts)[nr] & as.character(ECOindicators_fl$Year) == colnames(counts)[nc]]))/1000 
    } else {
        counts_inf[nr, nc] <- as.numeric(as.character(ECOindicators_fl_inf$Value[as.character(ECOindicators_fl_inf$Variable) == rownames(counts_inf)[nr] & as.character(ECOindicators_fl_inf$Year) == colnames(counts_inf)[nc]]))/1000
  
    }
      }
}


counts_sup <- table(as.character(ECOindicators_fl_sup$Variable),ECOindicators_fl_sup$Year)
counts_sup <- counts_sup[, colnames(counts_sup) != "ALL"]

for (nr in 1:nrow(counts_sup)) {
    for (nc in 1:ncol(counts_sup)) {
    if (as.numeric(colnames(counts_sup)[nc]) %in% years) {
  counts_sup[nr, nc] <- as.numeric(as.character(ECOindicators_fl$Value[as.character(ECOindicators_fl$Variable) == rownames(counts)[nr] & as.character(ECOindicators_fl$Year) == colnames(counts)[nc]]))/1000 
    } else {
        counts_sup[nr, nc] <- as.numeric(as.character(ECOindicators_fl_sup$Value[as.character(ECOindicators_fl_sup$Variable) == rownames(counts_sup)[nr] & as.character(ECOindicators_fl_sup$Year) == colnames(counts_sup)[nc]])) /1000
        }
    }
}
#errbar(bp, counts, counts_sup, counts_inf, add=T, xlab="", pch=20)
}




if (!is.null(ECOIndicators_quantiles)) { 
min_y <- ifelse(min(counts, counts_inf, counts_sup, na.rm=T)>0, 0, min(counts, counts_inf, counts_sup, na.rm=T))
max_y <- ifelse(max(counts, counts_inf, counts_sup, na.rm=T)<0, 0, max(counts, counts_inf, counts_sup, na.rm=T)) 
}                                  
                                                                                  # rownames(counts)
bp = barplot(counts, xlab="Year", col=c(4:(nrow(counts)+3)), legend = c("Capital costs", "Labour costs", "Profit") , ylim=c(min_y-(max_y-min_y)* 0.3, max_y + (max_y-min_y)* 0.3), cex.lab=1.5, cex.main=1.8, main=plot_title, beside=T, cex.axis = 1.5, cex.names =1.5,  args.legend = list(cex = 1.3, bty="n") , las=1)

if (!is.null(ECOIndicators_quantiles)) {
errbar(bp, counts, counts_sup, counts_inf, add=T, xlab="", pch=20)
}

 mtext( BMT_sw_version,side=4,outer=FALSE)
    
mtext(",000 ?",side=2,line=6, cex=1.5)
axis(1, labels = F, tick=F, cex.axis=1.5)
mtext(plot_title_sub, 3, line=1, cex=1.3)
box()
dev.off() 
# rownames(counts) <- BMT_FLEETSEGMENTS         #, Yield$Fleet_segment
#windows()
#barplot(counts, xlab="Year", col=c(4:(nrow(counts)+3)),legend = rownames(counts), ylim=c(0, (max(colSums(counts))+max(colSums(counts))*0.3)), main=plot_title)
#windows()

#twobar <- data.frame(neg=c(8,0,9,2), pos=c(12,9,0,4))
#windows()
#likert(twobar, horiz =F) 
#likert(counts, xlab="Year", col=c(4:(nrow(counts)+3)),legend = rownames(counts), ylim=c(0, (max(colSums(counts))+max(colSums(counts))*0.3)), main=plot_title)
#(twobar) 

}

}
  
  
  # save_EconomicbarPlot(years, name_econind)                      
