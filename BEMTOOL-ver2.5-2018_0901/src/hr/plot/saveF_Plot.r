# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





saveF_Plot <- function(all_the_years,  pressure_path, biological_path) {

      if (FALSE) {
      all_the_years <- years
      pressure_path <- name_pressind
      biological_path <- name_bioind
      }

                                                                # , biological_path
 PRESSIMPindicators <-  read.csv(pressure_path,sep=";",header=T)
 BIOindicators <- read.csv(biological_path,sep=";",header=T)

 ALL_indicators <- rbind(BIOindicators, PRESSIMPindicators)
# BIOindicators_temp <- BIOindicators[as.character(BIOindicators$Year) != "ALL", ]

ALL_indicators_by_year <- ALL_indicators[as.character(ALL_indicators$Year) != "ALL", ]
ALL_indicators_year_all <- ALL_indicators[as.character(ALL_indicators$Year) == "ALL", ]
 # PRESSIMPindicators <- PRESSIMPindicators[as.character(PRESSIMPindicators$Year) != "ALL", ]

if (phase == "SIMULATION") {
#TableF <- data.frame(matrix(nrow=0, ncol=4))
TableF <- data.frame(matrix(nrow=0, ncol=7))
# colnames(TableF) <- c("Stock", "% needed reduction (last year)", "Fcurrent", "Reference point")   
colnames(TableF) <- c("Stock", "% needed reduction (last year)", "Fcurrent", "Reference point", "SSBcurrent",	"SSBref",	"Comments")       
}
 
for (ss in 1:length(BMT_SPECIES)) {

#ALADYM_flag <- as.logical(cfg[rownames(cfg) == paste("casestudy.S", ss, ".AladymSimulation", sep=""),1])
#SAtool <- as.character(cfg[rownames(cfg) == paste("casestudy.S", ss, ".StockAssessmentTool", sep=""),1])
#


associated_fleetsegment <- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ss, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

if (phase == "SIMULATION") {
    Table_percF <- data.frame(matrix(0, nrow=length(all_the_years), ncol=length(associated_fleetsegment_indices)+1) )
    colnames(Table_percF) <- c("Year", paste("Percentage F due to", BMT_FLEETSEGMENTS[associated_fleetsegment_indices]))
    Table_percF$Year <- all_the_years

TableF_Fref <-  as.numeric(as.character(ALL_indicators_year_all$Value[as.character(ALL_indicators_year_all$Stock ) == BMT_SPECIES[ss] & as.character(ALL_indicators_year_all$Variable ) == "Fref" ]  ))

TableF_Fcurrent <-   as.numeric(as.character(ALL_indicators_by_year$Value[ as.character(ALL_indicators_by_year$Stock ) == BMT_SPECIES[ss] & as.character(ALL_indicators_by_year$Variable ) == "F" &  as.numeric(as.character(ALL_indicators_by_year$Year )) == years[simperiod] & as.character(ALL_indicators_by_year$Fleet_segment ) == "ALL" ] ))

TableF_SSBcurrent <-   as.numeric(as.character(ALL_indicators_by_year$Value[ as.character(ALL_indicators_by_year$Stock ) == BMT_SPECIES[ss] & as.character(ALL_indicators_by_year$Variable ) == "SSB_exploited_pop" &  as.numeric(as.character(ALL_indicators_by_year$Year )) == years[simperiod] ] ))

TableF_SSBref <-  as.numeric(as.character(ALL_indicators_year_all$Value[as.character(ALL_indicators_year_all$Stock ) == BMT_SPECIES[ss] & as.character(ALL_indicators_year_all$Variable ) == "SSBref" ]  ))

TableF_Comment <- as.character(ALL_indicators_year_all$Comments[as.character(ALL_indicators_year_all$Stock ) == BMT_SPECIES[ss] & as.character(ALL_indicators_year_all$Variable ) == "Fref" ] )

  TableF_Freduction <-  round( ( round(TableF_Fcurrent,2) - round(TableF_Fref,2) ) / round(TableF_Fcurrent,2) * 100 , 0)
  
TableF_toadd <- data.frame(cbind(BMT_SPECIES[ss], TableF_Freduction, round(TableF_Fcurrent,2), round(TableF_Fref, 2) , round(TableF_SSBcurrent,2), round(TableF_SSBref,2), TableF_Comment))
colnames(TableF_toadd) <- c("Stock", "% needed reduction (last year)", "Fcurrent", "Reference point", "SSBcurrent",	"SSBref",	"Comments")  
TableF <- rbind(TableF, TableF_toadd)
}


TotalF_mat <- ALL_indicators_by_year[as.character(ALL_indicators_by_year$Stock ) == BMT_SPECIES[ss] & as.character(ALL_indicators_by_year$Variable ) == "F" & as.character(ALL_indicators_by_year$Fleet_segment ) == "ALL", ]

  ffl_ord <- 1
  for (ffl in 1:length(BMT_FLEETSEGMENTS) ) {
  if (ffl %in% associated_fleetsegment_indices) {
     by_fleet <- as.numeric(as.character(ALL_indicators_by_year$Value[as.character(ALL_indicators_by_year$Variable) == "F" &  as.character(ALL_indicators_by_year$Stock) == BMT_SPECIES[ss] &  as.character(ALL_indicators_by_year$Fleet_segment) == BMT_FLEETSEGMENTS[ffl]] ))
TotalF_mat <- cbind(TotalF_mat, by_fleet)
     colnames(TotalF_mat)[ncol(TotalF_mat)] <- paste("F", ffl, sep="")

     if (phase =="SIMULATION" ) {
     for (yea in 1:simperiod) {     
       Table_percF[yea,ffl_ord+1] <- TotalF_mat[yea, colnames(TotalF_mat) == paste("F", ffl, sep="")] / TotalF_mat$Value[yea]* 100
       }
      }
     ffl_ord <-   ffl_ord + 1       
     }
  }

    plot_title <- paste("Fishing Mortality - ",BMT_SPECIES[ss], sep="")

if (phase =="SIMULATION") {                                             
  plot_path <- paste(casestudy_path, "/Diagnosis/Biological Pressure Impact/", casestudy_name, " - Fishing Mortality ",BMT_SPECIES[ss], ".jpg", sep="")
   plot_title_sub <- paste( casestudy_name, " simulation [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
} else {
if (!MEY_CALCULATION) {
  plot_path <- paste(casestudy_path, "/",harvest_rule_id,"/Biological Pressure Impact/", casestudy_name, " - Fishing Mortality ",BMT_SPECIES[ss], " ", harvest_rule_id,".jpg", sep="")
   plot_title_sub <- paste( casestudy_name, " ",harvest_rule_id , " [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
  } else {
    plot_path <- paste(casestudy_path, "/MEY calculation/",harvest_rule_id,"/Biological Pressure Impact/", casestudy_name, " - Fishing Mortality ",BMT_SPECIES[ss], " ", harvest_rule_id,".jpg", sep="")
     plot_title_sub <- paste(casestudy_name, " ",harvest_rule_id , " [", all_the_years[1], "-", all_the_years[length(all_the_years)],"]", sep="")
  }
}

    numbers <- TotalF_mat[,c(11:ncol(TotalF_mat))]
    numbers <- cbind(numbers, as.numeric(as.character(TotalF_mat$Value)) )

    plotting <- TRUE
    for (cl in 1:ncol(numbers)) {
      if (!all(is.finite(as.numeric(as.character(numbers[,cl])))) & plotting) {
          plotting <- FALSE
      }
     }

jpeg(file=plot_path, width=21, height=21, bg="white", units="cm",res=200)
par(mar=c(5, 6, 7, 6))    # c(bottom, left, top, right)                                                                                                                          
plot(as.numeric(as.character(TotalF_mat$Year)),as.numeric(as.character(TotalF_mat$Value)),type="l",xlab="Year",
ylab="Fishing Mortality", ylim=c(min(numbers[,], na.rm=T),(max(numbers[,], na.rm=T)+max(numbers[,], na.rm=T)*(0.07*length(BMT_FLEETSEGMENTS)))),axes=F, col=2, lwd=2,cex.lab=1.5, cex.main=1.8, main=plot_title)    
axis(1, at=as.numeric(as.character(TotalF_mat$Year)),  cex.axis=1.5)              
axis(2,  cex.axis=1.5)
 mtext( BMT_sw_version,side=4,outer=FALSE)

  if (phase == "FORECAST") {
    lines(rep(years[length(years)],2),c(min(numbers[,], na.rm=T),(max(numbers[,], na.rm=T)+max(numbers[,], na.rm=T)*(0.07*length(BMT_FLEETSEGMENTS)))), col="red", lty=2)
    text(rep(years[length(years)],2)+1,min(numbers[,], na.rm=T), label="FORECAST", pos=3,  cex=0.55) 
  }  

mtext(plot_title_sub, 3,  cex=1.3)
box()

ALADYM_flag <<- as.logical(cfg[rownames(cfg) == paste("casestudy.S", ss, ".AladymSimulation", sep=""),1])

  if (plotting | ALADYM_flag) {
    fl_ord <- 1
 for (ffl in 1:length(BMT_FLEETSEGMENTS)) {
   if (ffl %in% associated_fleetsegment_indices) {
     lines(as.numeric(as.character(TotalF_mat$Year)),as.numeric(as.character(TotalF_mat[,colnames(TotalF_mat) == paste("F", ffl, sep="")])), col=fl_ord+2, lwd=2)    
     fl_ord <- fl_ord + 1
  }
  }
} 
  legend("topright", c("Overall F", paste("F of", BMT_FLEETSEGMENTS[associated_fleetsegment_indices])), 
  border="white", pch=16, col=c(2:(length(associated_fleetsegment_indices)+2)), cex=1.05, bty="n") #,cex=1

#title(plot_title) 

dev.off() 

if (phase == "SIMULATION") {
    write.table(Table_percF, file = paste(casestudy_path, "/Diagnosis/Biological Pressure Impact/", casestudy_name, " - Percentage F by fleet - ", BMT_SPECIES[ss], ".csv", sep=""), sep=";", row.names=F)
}


} # end loop SPECIES

if (phase == "SIMULATION") {
write.table(TableF, file = paste(casestudy_path, "/Diagnosis/", casestudy_name, " - Reduction by stock.csv", sep=""), sep=";", row.names=F)
}

}

# saveF_Plot(years, name_pressind, name_bioind)
                        