# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





eval_scenarios_bio_forecast <- function() {

workdir <- getwd()
setwd(casestudy_path)
dir.create("Evaluation")
setwd(paste(casestudy_path, "/Evaluation", sep=""))
dir.create("Biological and Pressure evaluation")
setwd(paste(casestudy_path, "/Evaluation/Biological and Pressure evaluation", sep=""))
dir.create("FORECAST focus")
setwd(workdir)

casestudy_path <- str_replace_all(casestudy_path, "\\\\", "/" )

scenarios_dirs <- list.dirs(path = casestudy_path, recursive=F,  full.names = FALSE)

#scenarios_dirs <- scenarios_dirs[str_detect(scenarios_dirs[], "Scenario")  ]
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

files_results <- list.files(path = paste(casestudy_path,"/", scenarios_dirs[SCENARIO], sep="") , full.names=F)

bio_file <- files_results[!is.na(files_results[str_extract(files_results[], "Biological indicators") == "Biological indicators" ] ) ]

quanfile <- bio_file[str_detect(bio_file[], "quantiles")  ]

if (length(quanfile) != 0) {
bio_file <- bio_file[bio_file != quanfile]
}

res <- read.csv(paste(casestudy_path,"/", scenarios_dirs[SCENARIO], "/", bio_file, sep=""), sep=";")

res <- res [res$Year %in% c(years[simperiod], years.forecast), ]

resSSB <- res[res$Variable == "SSB_exploited_pop" ,]

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

files_results <- list.files(path = paste(casestudy_path,"/", scenarios_dirs[SCENARIO], sep=""), full.names=F)

bio_file <- files_results[!is.na(files_results[str_extract(files_results[], "Biological indicators") == "Biological indicators" ] ) ]

quanfile <- bio_file[str_detect(bio_file[], "quantiles")  ]

if (length(quanfile) != 0) {
bio_file <- bio_file[bio_file != quanfile]
}

res <- read.csv(paste(casestudy_path,"/",  scenarios_dirs[SCENARIO], "/", bio_file, sep=""), sep=";")
res <- res [res  $Year %in% c(years[simperiod], years.forecast), ]

resSSB <- res[res$Variable == "SSB_exploited_pop" & res$Stock ==BMT_SPECIES[spiec] ,]

if (SCENARIO == 1) {
jpeg(filename = paste(casestudy_path, "/Evaluation/Biological and Pressure evaluation/FORECAST focus/", BMT_SPECIES[spiec] , " - SSB_FORECAST.jpg", sep=""),width=21, height=21, bg="white", units="cm",res=200)
par(mar=c(5, 6, 6, 6))  # c(bottom, left, top, right)
plot( resSSB$Value, x=as.numeric(as.character(resSSB$Year)), type="b", col=SCENARIO, ylim=c(min_SSB,max_SSB+(max_SSB-min_SSB)*0.4), pch=19, axes=F, ylab="SSB [tons]", xlab="Years", cex.lab=1.5, lwd=2)
box()
axis(1, at=as.numeric(as.character(resSSB$Year)), as.numeric(as.character(resSSB$Year)), cex.axis=1.3)
axis(2, cex.axis=1.3)
 mtext( BMT_sw_version,side=4,outer=FALSE)
 
legend("topleft", legend = scenario_names, col=c(1:length(scenario_names)), lwd=2,  bty="n", cex=1.3)
title(paste(BMT_SPECIES[spiec] , "- SSB forecast"),  cex=2.5)
} else {
 lines( resSSB$Value, x=as.numeric(as.character(resSSB$Year)), type="b", col=SCENARIO, pch=19, lwd=2)
}

}

dev.off()

}

}
