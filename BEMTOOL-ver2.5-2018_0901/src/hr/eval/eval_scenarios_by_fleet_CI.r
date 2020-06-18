# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




eval_scenarios_by_fleet<- function() {

workdir <- getwd()
setwd(casestudy_path)
dir.create("Evaluation")
setwd(paste(casestudy_path, "/Evaluation", sep=""))
dir.create("Biological and Pressure evaluation")
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


#scenarios <- c("HR2-FmsyHake_5y_trawlers_permWD", "HR9-FmsyHake_5y_trawlers_permWD_BEHAV")

min_ML <- 0
max_ML <- 100

min_Y <- 0
max_Y <- 100000

min_L <- 0
max_L <- 100000

min_D <- 0
max_D <- 100000

min_Dratio <- 0
max_Dratio <- 100000



for (SCENARIO in 1:length(scenarios_dirs)) {

files_results <- list.files(path = paste(casestudy_path,"/", scenarios_dirs[SCENARIO], sep=""), full.names=F)
press_file <- files_results[!is.na(files_results[str_extract(files_results[], "Pressure impact indicators") == "Pressure impact indicators" ] ) ]

quanfile <- press_file[str_detect(press_file[], "quantiles")  ]

if (length(quanfile) != 0) {
press_file <- press_file[press_file != quanfile]
}

if (SCENARIO == 1) {
  res_all <- read.csv(paste(casestudy_path, "/", scenario_names[SCENARIO], "/", press_file, sep=""), sep=";")
} else {
  res_all <- rbind(res_all, read.csv(paste(casestudy_path, "/", scenario_names[SCENARIO], "/", press_file, sep=""), sep=";"))
}
}

name_all_scenarios <- paste(casestudy_path, "/Evaluation/", casestudy_name, " - Pressure impact indicators - ALL SCENARIOS.csv", sep="")

write.table(res_all, file=name_all_scenarios, sep=";", row.names=F)
 
 
resY_all <- res_all[res_all$Variable == "Catch" ,]
resML_all <- res_all[res_all$Variable == "Mean_length_catch" ,]
resD_all <- res_all[res_all$Variable == "Discard" ,]
resDratio_all <- res_all[res_all$Variable == "Discard_ratio" ,]
resL_all <- res_all[res_all$Variable == "Landing" ,]
resML_all$Value[is.na(resML_all$Value)] <- 0 




for (SPECIES in BMT_SPECIES) {            # --------- species loop

print(paste("Saving plots for ", SPECIES, "...", sep=""), quote=F)

for (FLEET_SEGMENT in c("ALL", BMT_FLEETSEGMENTS) ) {        # ---------------- fleet segment loop

# to determine if the species is caugth by this fleet segment
resY_temp <- resY_all[resY_all$Variable == "Catch" & resY_all$Stock ==SPECIES & resY_all$Fleet_segment == FLEET_SEGMENT,]

if (nrow(resY_temp) != 0) {

# ---------------------------------------------------------------------- plot DISCARD

for (SCENARIO in 1:length(scenarios_dirs)) {
files_results <- list.files(path = paste(casestudy_path,"/", scenarios_dirs[SCENARIO], sep=""), full.names=F)
press_file <- files_results[!is.na(files_results[str_extract(files_results[], "Pressure impact indicators") == "Pressure impact indicators" ] ) ]
quanfile <- press_file[str_detect(press_file[], "quantiles")  ]

if (length(quanfile) != 0) {
press_file <- press_file[press_file != quanfile]
}

res <- read.csv(paste(casestudy_path, "/", scenario_names[SCENARIO], "/", press_file, sep=""), sep=";")
resD <- res[res$Variable == "Discard" & res$Stock ==SPECIES & res$Fleet_segment == FLEET_SEGMENT,]

if (!all(is.na(resD$Value)) )  {

#if (!exists("min_D")) {
if (SCENARIO == 1) {

jpeg(filename = paste(casestudy_path, "/Evaluation/Biological and Pressure evaluation/", SPECIES , " - Discard ",FLEET_SEGMENT,".jpg", sep=""), width=21, height=21, bg="white", units="cm",res=200)
#max <- max(selY$Value, sqY$Value, tempWDY$Value, sel_tempWDY$Value)
#min <- min(selY$Value, sqY$Value, tempWDY$Value, sel_tempWDY$Value)

min_D <- min(resD_all$Value[resD_all$Variable == "Discard" & resD_all$Stock ==SPECIES & resD_all$Fleet_segment == FLEET_SEGMENT], na.rm=T)
max_D <-  max(resD_all$Value[resD_all$Variable == "Discard" & resD_all$Stock ==SPECIES & resD_all$Fleet_segment == FLEET_SEGMENT], na.rm=T)
par(mar=c(5, 6, 6, 6))  # c(bottom, left, top, right)
plot( resD$Value, type="b", col=1, ylim=c(min_D,max_D+(max_D-min_D)*0.4), pch=19, axes=F, ylab="Discard [tons]", xlab="Years", cex.lab=1.5, lwd=2)
box()
#axis(1, at=as.numeric(as.character(resSSB$Year)), as.numeric(as.character(resSSB$Year)), cex.axis=1.3)  
axis(1, at=as.numeric(as.character(rresD$Year)), as.numeric(as.character(rresD$Year)), cex.axis=1.3)
axis(2, cex.axis=1.3)
mtext( BMT_sw_version,side=4,outer=FALSE)
legend("topleft",scenario_names, col=c(1:length(scenario_names)), lwd=2,   bty="n", cex=1.3)
title(paste(SPECIES , "- Discard - ", FLEET_SEGMENT))
} else {
 lines( resD$Value, type="b", col=SCENARIO, pch=19, lwd=2)
}

}

}
try(dev.off())

cat(paste(" Discard -", sep="") )                                  

# ---------------------------------------------------------------------- plot DISCARD RATIO

for (SCENARIO in 1:length(scenarios_dirs)) {

files_results <- list.files(path = paste( casestudy_path,"/", scenarios_dirs[SCENARIO], sep=""), full.names=F)
press_file <- files_results[!is.na(files_results[str_extract(files_results[], "Pressure impact indicators") == "Pressure impact indicators" ] ) ]
quanfile <- press_file[str_detect(press_file[], "quantiles")  ]

if (length(quanfile) != 0) {
press_file <- press_file[press_file != quanfile]
}

res <- read.csv(paste(casestudy_path, "/", scenario_names[SCENARIO], "/", press_file, sep=""), sep=";")
resDratio <- res[res$Variable == "Discard_ratio" & res$Stock ==SPECIES & res$Fleet_segment == FLEET_SEGMENT,]

 if (!all(is.na(resDratio$Value)) )  {

if (SCENARIO == 1) {

jpeg(filename = paste(casestudy_path, "/Evaluation/Biological and Pressure evaluation/", SPECIES , " - Discard ratio ",FLEET_SEGMENT,".jpg", sep=""), width=21, height=21, bg="white", units="cm",res=200)

min_Dratio <- min(resDratio_all$Value[resDratio_all$Variable == "Discard_ratio" & resDratio_all$Stock ==SPECIES & resDratio_all$Fleet_segment == FLEET_SEGMENT], na.rm=T)
max_Dratio <-  max(resDratio_all$Value[resDratio_all$Variable == "Discard_ratio" & resDratio_all$Stock ==SPECIES & resDratio_all$Fleet_segment == FLEET_SEGMENT], na.rm=T)
 par(mar=c(5, 6, 6, 6))  # c(bottom, left, top, right)
plot( resDratio$Value, type="b", col=1, ylim=c(min_Dratio,max_Dratio+(max_Dratio-min_Dratio)*0.4), pch=19, axes=F, ylab="Discard ratio [D/Y]", xlab="Years", cex.lab=1.5, lwd=2)
box()
axis(1, at=as.numeric(as.character(resDratio$Year)), as.numeric(as.character(resDratio$Year)), cex.axis=1.3)
axis(2, cex.axis=1.3)
mtext( BMT_sw_version,side=4,outer=FALSE)
legend("topleft",scenario_names, col=c(1:length(scenario_names)), lwd=2,   bty="n", cex=1.3)
title(paste(SPECIES , "- Discard ratio - ", FLEET_SEGMENT))
} else {
 lines( resDratio$Value, type="b", col=SCENARIO, pch=19, lwd=2)
}

  }
 
}
try(dev.off())

cat(paste(" Discard ratio -", sep="") )                                  


# ---------------------------------------------------------------------- plot LANDING

for (SCENARIO in 1:length(scenarios_dirs)) {

files_results <- list.files(path = paste( casestudy_path,"/", scenarios_dirs[SCENARIO], sep=""), full.names=F)
press_file <- files_results[!is.na(files_results[str_extract(files_results[], "Pressure impact indicators") == "Pressure impact indicators" ] ) ]
quanfile <- press_file[str_detect(press_file[], "quantiles")  ]

if (length(quanfile) != 0) {
press_file <- press_file[press_file != quanfile]
}

res <- read.csv(paste(casestudy_path, "/", scenario_names[SCENARIO], "/", press_file, sep=""), sep=";")
resL <- res[res$Variable == "Landing" & res$Stock ==SPECIES & res$Fleet_segment == FLEET_SEGMENT,]

if (SCENARIO == 1) {

jpeg(filename = paste(casestudy_path, "/Evaluation/Biological and Pressure evaluation/", SPECIES , " - Landing ",FLEET_SEGMENT,".jpg", sep=""), width=21, height=21, bg="white", units="cm",res=200)

min_L <- min(resL_all$Value[resL_all$Variable == "Landing" & resL_all$Stock ==SPECIES & resL_all$Fleet_segment == FLEET_SEGMENT])
max_L <-  max(resL_all$Value[resL_all$Variable == "Landing" & resL_all$Stock ==SPECIES & resL_all$Fleet_segment == FLEET_SEGMENT])
 par(mar=c(5, 6, 6, 6))  # c(bottom, left, top, right)
plot( resL$Value, type="b", col=1, ylim=c(min_L,max_L+(max_L-min_L)*0.4), pch=19, axes=F, ylab="Landing [tons]", xlab="Years", cex.lab=1.5, lwd=2)
box()
axis(1, at=as.numeric(as.character(resL$Year)), as.numeric(as.character(resL$Year)), cex.axis=1.3)
axis(2, cex.axis=1.3)
mtext( BMT_sw_version,side=4,outer=FALSE)
legend("topleft",scenario_names, col=c(1:length(scenario_names)), lwd=2,   bty="n", cex=1.3)
title(paste(SPECIES , "- Landing - ", FLEET_SEGMENT))
} else {
 lines( resL$Value, type="b", col=SCENARIO, pch=19, lwd=2)
}

 
}
try(dev.off())
cat(paste(" Landing -", sep="") )                                  



# ---------------------------------------------------------------------- plot CATCH

for (SCENARIO in 1:length(scenarios_dirs)) {

files_results <- list.files(path = paste( casestudy_path,"/", scenarios_dirs[SCENARIO], sep=""), full.names=F)
press_file <- files_results[!is.na(files_results[str_extract(files_results[], "Pressure impact indicators") == "Pressure impact indicators" ] ) ]
quanfile <- press_file[str_detect(press_file[], "quantiles")  ]

if (length(quanfile) != 0) {
press_file <- press_file[press_file != quanfile]
}

res <- read.csv(paste(casestudy_path, "/", scenario_names[SCENARIO], "/", press_file, sep=""), sep=";")
resY <- res[res$Variable == "Catch" & res$Stock ==SPECIES & res$Fleet_segment == FLEET_SEGMENT,]

if (SCENARIO == 1) {

jpeg(filename = paste(casestudy_path, "/Evaluation/Biological and Pressure evaluation/", SPECIES , " - Catch ",FLEET_SEGMENT,".jpg", sep=""), width=21, height=21, bg="white", units="cm",res=200)

min_Y <- min(resY_all$Value[resY_all$Variable == "Catch" & resY_all$Stock ==SPECIES & resY_all$Fleet_segment == FLEET_SEGMENT])
max_Y <-  max(resY_all$Value[resY_all$Variable == "Catch" & resY_all$Stock ==SPECIES & resY_all$Fleet_segment == FLEET_SEGMENT])
 par(mar=c(5, 6, 6, 6))  # c(bottom, left, top, right)
plot( resY$Value, type="b", col=1, ylim=c(min_Y,max_Y+(max_Y-min_Y)*0.4), pch=19, axes=F, ylab="Catch [tons]", xlab="Years", cex.lab=1.5, lwd=2)
box()
axis(1, at=as.numeric(as.character(resY$Year)), as.numeric(as.character(resY$Year)), cex.axis=1.3)
axis(2, cex.axis=1.3)
mtext( BMT_sw_version,side=4,outer=FALSE)
legend("topleft",scenario_names, col=c(1:length(scenario_names)), lwd=2,   bty="n", cex=1.3)
title(paste(SPECIES , "- Catch - ", FLEET_SEGMENT))
} else {
 lines( resY$Value, type="b", col=SCENARIO, pch=19, lwd=2)
}

}
try(dev.off())

cat(paste(" Catch -", sep="") )                                  

if (FLEET_SEGMENT != "ALL") {

# ---------------------------------------------------------------------- plot MEAN LENGTH

for (SCENARIO in 1:length(scenarios_dirs)) {

files_results <- list.files(path = paste(casestudy_path,"/", scenarios_dirs[SCENARIO], sep=""), full.names=F)
press_file <- files_results[!is.na(files_results[str_extract(files_results[], "Pressure impact indicators") == "Pressure impact indicators" ] ) ]
quanfile <- press_file[str_detect(press_file[], "quantiles")  ]

if (length(quanfile) != 0) {
press_file <- press_file[press_file != quanfile]
}

res <- read.csv(paste(casestudy_path, "/", scenario_names[SCENARIO], "/", press_file, sep=""), sep=";")
resML <- res[res$Variable == "Mean_length_catch" & res$Stock ==SPECIES & res$Fleet_segment == FLEET_SEGMENT,]
resML$Value[is.na(resML$Value)] <- 0 


if (SCENARIO == 1) {
jpeg(filename = paste(casestudy_path, "/Evaluation/Biological and Pressure evaluation/", SPECIES , " - Mean_length_catch ",FLEET_SEGMENT,".jpg", sep=""), width=21, height=21, bg="white", units="cm",res=200)

min_ML <- min(resML_all$Value[resML_all$Variable == "Mean_length_catch" & resML_all$Stock ==SPECIES & resML_all$Fleet_segment == FLEET_SEGMENT])
max_ML <-  max(resML_all$Value[resML_all$Variable == "Mean_length_catch" & resML_all$Stock ==SPECIES & resML_all$Fleet_segment == FLEET_SEGMENT])
par(mar=c(5, 6, 6, 6))  # c(bottom, left, top, right)
plot( resML$Value, type="b", col=1, ylim=c(min_ML,max_ML+(max_ML-min_ML)*0.4), pch=19, axes=F, ylab="Mean_length_catch [mm]", xlab="Years", cex.lab=1.5, lwd=2)
box()
axis(1, at=as.numeric(as.character(resML$Year)), as.numeric(as.character(resML$Year)), cex.axis=1.3)
axis(2, cex.axis=1.3)
mtext( BMT_sw_version,side=4,outer=FALSE)
legend("topleft",scenario_names, col=c(1:length(scenario_names)), lwd=2,   bty="n", cex=1.3)
title(paste(SPECIES , "- Mean_length_catch - ", FLEET_SEGMENT))
} else {
 lines( resML$Value, type="b", col=SCENARIO, pch=19, lwd=2)
}


 
}
try(dev.off())

cat(paste(" Mean length -", sep="") )                                  


}  
} 
                                
print(paste(" for ", FLEET_SEGMENT, " saved!", sep=""), quote=F )
}
}

}

