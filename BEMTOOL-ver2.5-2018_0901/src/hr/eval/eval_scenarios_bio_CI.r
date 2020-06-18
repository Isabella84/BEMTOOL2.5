# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





eval_scenarios_bio_CI<- function() {

workdir <- getwd()
setwd(casestudy_path)
dir.create("Evaluation CI")
setwd(paste(casestudy_path, "/Evaluation CI", sep=""))
dir.create("Biological and Pressure evaluation")
setwd(workdir)

casestudy_path <- str_replace_all(casestudy_path, "\\\\", "/" )

scenarios_dirs <- list.dirs(path = casestudy_path, recursive=F,  full.names = FALSE)

# scenarios_dirs <- scenarios_dirs[str_detect(scenarios_dirs[], "Scenario")  ]
scenarios_dirs <- scenarios_dirs[str_detect(scenarios_dirs[], SCENARIO_IDENTIFIER)  ] 

scenario_names <- c()
for (len in 1:length(scenarios_dirs)) {
vect_ <- str_split(as.character(scenarios_dirs[len]), "/")
vect_ <- vect_[[1]]
scenario_names <- c(scenario_names, vect_[length(vect_)])
}

mins <- list()
maxs <- list()

  for (specc in 1:length(BMT_SPECIES) ) {
  
  spe_list_min <- c()
    spe_list_max <- c()
  
for (SCENARIO in 1:length(scenarios_dirs)) {

files_results <- list.files(path = paste(casestudy_path, "/", scenarios_dirs[SCENARIO], sep=""), full.names=F)

bio_file <- files_results[!is.na(files_results[str_extract(files_results[], "Biological indicators") == "Biological indicators" ] ) ]

quanfile <- bio_file[str_detect(bio_file[], "quantiles")  ]

if (length(quanfile) != 0) {
bio_file <- bio_file[bio_file == quanfile]
}

res <- read.csv(paste( casestudy_path, "/", scenarios_dirs[SCENARIO], "/", bio_file, sep=""), sep=";")

if (SCENARIO == 1) {
  res_all <- res 
} else {
  res_all <- rbind(res_all, res )
}

name_all_scenarios <- paste(casestudy_path, "/Evaluation CI/", casestudy_name, " - Biological indicators - ALL SCENARIOS quantiles.csv", sep="")

write.table(res_all, file=name_all_scenarios, sep=";", row.names=F)

resSSB <- res[res$Variable == "SSB_exploited_pop" | res$Variable == "SSBref" ,]

          spe_list_min <- c( spe_list_min, min(resSSB$Value[resSSB$Stock == BMT_SPECIES[specc]]))
           spe_list_max <- c(spe_list_max, max(resSSB$Value[resSSB$Stock == BMT_SPECIES[specc]]))

}

if (specc == 1) {
    mins <- list(spe_list_min)
    maxs <- list(spe_list_max)
} else {
    mins <- c( mins, list(spe_list_min))
    maxs <- c( maxs, list(spe_list_max))
}

}



for (spiec in 1:length(BMT_SPECIES) ) {

     min_SSB <- min(mins[[spiec]] )
     max_SSB <- max(maxs[[spiec]] )

for (SCENARIO in 1:length(scenarios_dirs)) {

files_results <- list.files(path =  paste(casestudy_path,"/", scenarios_dirs[SCENARIO], sep=""), full.names=F)

bio_file <- files_results[!is.na(files_results[str_extract(files_results[], "Biological indicators") == "Biological indicators" ] ) ]

quanfile <- bio_file[str_detect(bio_file[], "quantiles")  ]

if (length(quanfile) != 0) {
bio_file <- bio_file[bio_file == quanfile]
}

res <- read.csv(paste(casestudy_path,"/",  scenarios_dirs[SCENARIO], "/", bio_file, sep=""), sep=";")

resSSB <- res[res$Variable == "SSB_exploited_pop" & res$Stock == BMT_SPECIES[spiec] ,]

toPlot_005 <- resSSB[resSSB$quantile == 0.05,]
toPlot_025 <- resSSB[resSSB$quantile == 0.25,]
toPlot_05  <- resSSB[resSSB$quantile == 0.5,]
toPlot_075 <- resSSB[resSSB$quantile == 0.75,]
toPlot_095 <- resSSB[resSSB$quantile == 0.95,]

if (SCENARIO == 1) {
jpeg(filename = paste(casestudy_path, "/Evaluation CI/Biological and Pressure evaluation/", BMT_SPECIES[spiec] , " - SSB.jpg", sep=""), width=21, height=21, bg="white", units="cm",res=200) 
par(mar=c(5, 6, 6, 6))  # c(bottom, left, top, right)

plot( toPlot_05$Value, x=as.numeric(as.character(toPlot_05$Year)), type="b", col=SCENARIO, ylim=c(min_SSB,max_SSB+(max_SSB-min_SSB)*0.4), pch=19, axes=F, ylab="SSB [tons]", xlab="Years", cex.lab=1.5, lwd=2)

axis(1, at=as.numeric(as.character(toPlot_05$Year)), as.numeric(as.character(toPlot_05$Year)), cex.axis=1.3)  
box()
axis(2, cex.axis=1.3)

mtext( BMT_sw_version,side=4,outer=FALSE)

legend("topleft",  legend = scenario_names, col=c(1:length(scenario_names)), lwd=2,   bty="n", cex=1.3)
legend ("topright", lwd=c(1,1,2,1,1),lty=c(2,3,1,3,2),legend=c( "0.05","0.25","median", "0.75", "0.95"), bty="n")

title(paste(BMT_SPECIES[spiec] , "- SSB"), cex=2.5)
} else {
 lines( toPlot_05$Value, x=as.numeric(as.character(toPlot_05$Year)), type="b", col=SCENARIO, pch=19, lwd=2)
}



 polygon(c(as.numeric(as.character(toPlot_05$Year)), rev(as.numeric(as.character(toPlot_05$Year)))), c( toPlot_005$Value, rev(toPlot_025$Value)), col = alpha(SCENARIO, 0.05), border = NA)
polygon(c(as.numeric(as.character(toPlot_05$Year)), rev(as.numeric(as.character(toPlot_05$Year)))), c(toPlot_025$Value, rev(toPlot_05$Value)), col = alpha(SCENARIO, 0.15), border = NA)
polygon(c(as.numeric(as.character(toPlot_05$Year)), rev(as.numeric(as.character(toPlot_05$Year)))),c(toPlot_05$Value, rev(toPlot_075$Value)), col = alpha(SCENARIO, 0.15), border = NA)
polygon(c(as.numeric(as.character(toPlot_05$Year)), rev(as.numeric(as.character(toPlot_05$Year)))),c(toPlot_075$Value, rev(toPlot_095$Value)), col = alpha(SCENARIO, 0.05), border = NA)
    
    lines(as.numeric(as.character(toPlot_05$Year)), toPlot_005$Value, col=SCENARIO, type="l", lty=2) 
    	lines(as.numeric(as.character(toPlot_05$Year)),toPlot_025$Value, col=SCENARIO, type="l", lty=3) 
    	lines(as.numeric(as.character(toPlot_05$Year)), toPlot_075$Value , col=SCENARIO, type="l", lty=3) 
       lines(as.numeric(as.character(toPlot_05$Year)), toPlot_095$Value, col=SCENARIO, type="l", lty=2) 


}

SSB_rif <- res$Value[res$Variable == "SSBref" & res$Stock == BMT_SPECIES[spiec] ]
 
 lines( c(years[1],years.forecast[foreperiod]), rep(SSB_rif,2), col="red", lwd=2)
 text(years[2], SSB_rif+(SSB_rif*0.05), label="SSB ref", pos=1) 

dev.off()

}

}
