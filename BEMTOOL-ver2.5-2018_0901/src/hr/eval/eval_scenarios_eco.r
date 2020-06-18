# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




eval_scenarios_eco<- function() {

workdir <- getwd()
setwd(casestudy_path)
dir.create("Evaluation")
setwd(paste(casestudy_path, "/Evaluation", sep=""))
dir.create("Economic evaluation")
dir.create("Effort change")
setwd(workdir)

casestudy_path <- str_replace_all(casestudy_path, "\\\\", "/" )

name_econind_this <- paste(casestudy_path, "/",harvest_rule_id, "/", casestudy_name, " - Economic output ", harvest_rule_id,".csv", sep="")

scenarios_dirs <- list.dirs(path = casestudy_path, recursive=F,  full.names = FALSE)

#scenarios_dirs <- scenarios_dirs[str_detect(scenarios_dirs[], "Scenario")  ]
scenarios_dirs <- scenarios_dirs[str_detect(scenarios_dirs[], SCENARIO_IDENTIFIER)  ] 

scenario_names <- c()
for (len in 1:length(scenarios_dirs)) {
vect_ <- str_split(as.character(scenarios_dirs[len]), "/")
vect_ <- vect_[[1]]
scenario_names <- c(scenario_names, vect_[length(vect_)])
}


min_GVA  <- 100
min_E <- 100
min_SAL <- 100
min_NPV  <- 100
min_CRBER <- 100
min_ROFTA <- 100
min_AvgDAYS <- 100
min_VESSEL <- 100

min_totLan <- 0
min_totRev <- 0

max_GVA  <- 0
max_E <- 0
max_SAL <- 0
max_NPV  <- 0
max_CRBER <- 0
max_ROFTA <- 0
max_AvgDAYS <- 0
max_VESSEL <- 0

max_totLan <- 0
max_totRev <- 0


for (SCENARIO in 1:length(scenarios_dirs)) {


files_results <- list.files(path = paste(casestudy_path,"/", scenarios_dirs[SCENARIO], sep=""), full.names=F)

eco_file <- files_results[!is.na(files_results[str_extract(files_results[], "Economic output") == "Economic output" ] ) ]

quanfile <- eco_file[str_detect(eco_file[], "quantiles")  ]

if (length(quanfile) != 0) {
eco_file <- eco_file[eco_file != quanfile]
}

res <- read.csv(paste(casestudy_path,"/", scenarios_dirs[SCENARIO], "/", eco_file, sep=""), sep=";")

res$ID_scenario <- scenarios_dirs[SCENARIO] 

if (SCENARIO == 1) {
  res_all <- res
} else {
  res_all <- rbind(res_all, res)
}

}

name_all_scenarios <- paste(casestudy_path, "/Evaluation/", casestudy_name, " - Economic output - ALL SCENARIOS.csv", sep="")

write.table(res_all, file=name_all_scenarios, sep=";", row.names=F)
 
resGVAperV_all <- res_all[res_all$Variable == "Economic.indicators[GVA.vessel]" ,]
resGVA_all <- res_all[res_all$Variable == "gross.value.added" ,]
resE_all <- res_all[res_all$Variable == "employment" ,]
resSAL_all <- res_all[res_all$Variable == "average.wage" ,]
resNPV_all <- res_all[res_all$Variable == "Economic.indicators[NPV.discounted]" ,]
resCRBER_all <- res_all[res_all$Variable == "Economic.indicators[CR.BER]" ,]
resROFTA_all <- res_all[res_all$Variable == "Economic.indicators[NP.cap.value]" ,]

resAvgDAYS_all <- res_all[res_all$Variable == "DAYS.average.annual" ,]
resVESSEL_all <- res_all[res_all$Variable == "VESSELS.annual" ,]

resTotLand_all <- res_all[res_all$Variable == "total.landings" ,]
resTotRev_all <- res_all[res_all$Variable == "total.revenues" ,]

all_fleets_vect <- c(BMT_FLEETSEGMENTS, "ALL")

for (FLEET_SEGMENT in all_fleets_vect) {
        # print(FLEET_SEGMENT) 

#if (nrow(resY_temp) != 0) {
for (SCENARIO in 1:length(scenarios_dirs)) {


files_results <- list.files(path = paste(casestudy_path,"/", scenarios_dirs[SCENARIO], sep=""), full.names=F)

eco_file <- files_results[!is.na(files_results[str_extract(files_results[], "Economic output") == "Economic output" ] ) ]

quanfile <- eco_file[str_detect(eco_file[], "quantiles")  ]

if (length(quanfile) != 0) {
eco_file <- eco_file[eco_file != quanfile]
}

res <- read.csv(paste(casestudy_path,"/", scenarios_dirs[SCENARIO], "/", eco_file, sep=""), sep=";")
 
  resGVA <- res[res$Variable == "gross.value.added" & res$Fleet_segment == FLEET_SEGMENT,]

if (SCENARIO == 1) {
jpeg(filename = paste(casestudy_path, "/Evaluation/Economic evaluation/Gross Value Added ",FLEET_SEGMENT,".jpg", sep=""), width=21, height=21, bg="white", units="cm",res=200)
#max <- max(selY$Value, sqY$Value, tempWDY$Value, sel_tempWDY$Value)
#min <- min(selY$Value, sqY$Value, tempWDY$Value, sel_tempWDY$Value)

min_GVA <- min(as.numeric(as.character(resGVA_all$Value[resGVA_all$Variable == "gross.value.added"  & resGVA_all$Fleet_segment == FLEET_SEGMENT])) )
max_GVA <-  max(as.numeric(as.character(resGVA_all$Value[resGVA_all$Variable == "gross.value.added"  & resGVA_all$Fleet_segment == FLEET_SEGMENT])) )
par(mar=c(5, 6, 6, 6))  # c(bottom, left, top, right)
plot( as.numeric(as.character(resGVA$Value))/1000, type="b", col=1, ylim=c(min_GVA,max_GVA+(max_GVA-min_GVA)*0.4)/1000, pch=19, axes=F, ylab="Gross Value Added [,000 €]", xlab="Years", cex.lab=1.5, lwd=2)
 box()                                              
axis(1, at=seq(1,length(resGVA$Year),1), resGVA$Year , cex.axis=1.3)   
axis(2, cex.axis=1.3)
mtext( BMT_sw_version,side=4,outer=FALSE)
legend("topleft",scenario_names, col=c(1:length(scenario_names)), bty="n", lwd=2, cex=1.3)
title(paste( "Gross Value Added - ", FLEET_SEGMENT))
} else {
 lines( as.numeric(as.character(resGVA$Value))/1000, type="b", col=SCENARIO, pch=19, lwd=2)
}


}
dev.off()  

for (SCENARIO in 1:length(scenarios_dirs)) {

files_results <- list.files(path = paste(casestudy_path,"/", scenarios_dirs[SCENARIO], sep=""), full.names=F)

eco_file <- files_results[!is.na(files_results[str_extract(files_results[], "Economic output") == "Economic output" ] ) ]

quanfile <- eco_file[str_detect(eco_file[], "quantiles")  ]

if (length(quanfile) != 0) {
eco_file <- eco_file[eco_file != quanfile]
}

res <- read.csv(paste(casestudy_path,"/", scenarios_dirs[SCENARIO], "/", eco_file, sep=""), sep=";")

 resE <- res[res$Variable == "employment" & res$Fleet_segment == FLEET_SEGMENT,]


if (SCENARIO == 1) {
jpeg(filename = paste(casestudy_path, "/Evaluation/Economic evaluation/Employment ",FLEET_SEGMENT,".jpg", sep=""), width=21, height=21, bg="white", units="cm",res=200)
#max <- max(selY$Value, sqY$Value, tempWDY$Value, sel_tempWDY$Value)
#min <- min(selY$Value, sqY$Value, tempWDY$Value, sel_tempWDY$Value)

min_E <- min(as.numeric(as.character(resE_all$Value[resE_all$Variable == "employment"  & resE_all$Fleet_segment == FLEET_SEGMENT])) )
max_E <-  max(as.numeric(as.character(resE_all$Value[resE_all$Variable == "employment"  & resE_all$Fleet_segment == FLEET_SEGMENT])) )
 par(mar=c(5, 6, 6, 6))  # c(bottom, left, top, right)
plot( as.numeric(as.character(resE$Value)), type="b", col=1, ylim=c(min_E,max_E+(max_E-min_E)*0.4), pch=19, axes=F, ylab="Employment", xlab="Years", cex.lab=1.5, lwd=2)
box()
axis(1, at=seq(1,length(resE$Year),1), resE$Year, cex.axis=1.3 )   
axis(2, cex.axis=1.3)
mtext( BMT_sw_version,side=4,outer=FALSE)
legend("topleft",scenario_names, col=c(1:length(scenario_names)), bty="n", lwd=2, cex=1.3)
title(paste( "Employement - ", FLEET_SEGMENT))
} else {
 lines( as.numeric(as.character(resE$Value)), type="b", col=SCENARIO, pch=19, lwd=2)
}

 }
dev.off() 


for (SCENARIO in 1:length(scenarios_dirs)) {


files_results <- list.files(path = paste(casestudy_path,"/", scenarios_dirs[SCENARIO], sep=""), full.names=F)

eco_file <- files_results[!is.na(files_results[str_extract(files_results[], "Economic output") == "Economic output" ] ) ]

quanfile <- eco_file[str_detect(eco_file[], "quantiles")  ]

if (length(quanfile) != 0) {
eco_file <- eco_file[eco_file != quanfile]
}

res <- read.csv(paste(casestudy_path,"/", scenarios_dirs[SCENARIO], "/", eco_file, sep=""), sep=";")


 resSAL <- res[res$Variable == "average.wage" & res$Fleet_segment == FLEET_SEGMENT,]


if (SCENARIO == 1) {
jpeg(filename = paste(casestudy_path, "/Evaluation/Economic evaluation/Average wage ",FLEET_SEGMENT,".jpg", sep=""), width=21, height=21, bg="white", units="cm",res=200)
#max <- max(selY$Value, sqY$Value, tempWDY$Value, sel_tempWDY$Value)
#min <- min(selY$Value, sqY$Value, tempWDY$Value, sel_tempWDY$Value)

min_SAL <- min(as.numeric(as.character(resSAL_all$Value[resSAL_all$Variable == "average.wage"  & resSAL_all$Fleet_segment == FLEET_SEGMENT])) )
max_SAL <-  max(as.numeric(as.character(resSAL_all$Value[resSAL_all$Variable == "average.wage"  & resSAL_all$Fleet_segment == FLEET_SEGMENT])) )
 par(mar=c(5, 6, 6, 6))  # c(bottom, left, top, right)
plot( as.numeric(as.character(resSAL$Value)), type="b", col=1, ylim=c(min_SAL, max_SAL+(max_SAL-min_SAL)*0.4), pch=19, axes=F, ylab="Average wage [€]", xlab="Years", cex.lab=1.5, lwd=2)
box()
axis(1, at=seq(1,length(resSAL$Year),1), resSAL$Year , cex.axis=1.3)   
axis(2, cex.axis=1.3)
mtext( BMT_sw_version,side=4,outer=FALSE)
legend("topleft",scenario_names, col=c(1:length(scenario_names)), bty="n", lwd=2, cex=1.3)
title(paste( "Average wage - ", FLEET_SEGMENT))
} else {
 lines( as.numeric(as.character(resSAL$Value)), type="b", col=SCENARIO, pch=19, lwd=2)
}

 
}
dev.off() 



for (SCENARIO in 1:length(scenarios_dirs)) {


files_results <- list.files(path = paste(casestudy_path,"/", scenarios_dirs[SCENARIO], sep=""), full.names=F)

eco_file <- files_results[!is.na(files_results[str_extract(files_results[], "Economic output") == "Economic output" ] ) ]

quanfile <- eco_file[str_detect(eco_file[], "quantiles")  ]

if (length(quanfile) != 0) {
eco_file <- eco_file[eco_file != quanfile]
}

res <- read.csv(paste(casestudy_path,"/", scenarios_dirs[SCENARIO], "/", eco_file, sep=""), sep=";")

#       resNPV_all <- res_all[res_all$Variable == "Economic.indicators[NPV15]" ,]
#resCRBER_all <- res_all[res_all$Variable == "Economic.indicators[CR.BER]" ,]
#resROFTA_all <- res_all[res_all$Variable == "Economic.indicators[RoFTA]" ,]

 resROFTA <- res[res$Variable == "Economic.indicators[NP.cap.value]" & res$Fleet_segment == FLEET_SEGMENT,]


if (SCENARIO == 1) {
jpeg(filename = paste(casestudy_path, "/Evaluation/Economic evaluation/RoFTA ",FLEET_SEGMENT,".jpg", sep=""), width=21, height=21, bg="white", units="cm",res=200)
#max <- max(selY$Value, sqY$Value, tempWDY$Value, sel_tempWDY$Value)
#min <- min(selY$Value, sqY$Value, tempWDY$Value, sel_tempWDY$Value)

min_ROFTA <- min(as.numeric(as.character(resROFTA_all$Value[resROFTA_all$Variable == "Economic.indicators[NP.cap.value]"  & resROFTA_all$Fleet_segment == FLEET_SEGMENT])) )
max_ROFTA <-  max(as.numeric(as.character(resROFTA_all$Value[resROFTA_all$Variable == "Economic.indicators[NP.cap.value]"  & resROFTA_all$Fleet_segment == FLEET_SEGMENT])) )
 par(mar=c(5, 6, 6, 6))  # c(bottom, left, top, right)
plot( as.numeric(as.character(resROFTA$Value)) , type="b", col=1, ylim=c(min_ROFTA, max_ROFTA+(max_ROFTA-min_ROFTA)*0.4), pch=19, axes=F, ylab="RoFTA", xlab="Years", cex.lab=1.5, lwd=2)
box()
axis(1, at=seq(1,length(resROFTA$Year),1), resROFTA$Year , cex.axis=1.3)   
axis(2, cex.axis=1.3)
mtext( BMT_sw_version,side=4,outer=FALSE)
legend("topleft",scenario_names, col=c(1:length(scenario_names)), bty="n", lwd=2, cex=1.3)
title(paste( "RoFTA - ", FLEET_SEGMENT))
} else {
 lines( as.numeric(as.character(resROFTA$Value)), type="b", col=SCENARIO, pch=19, lwd=2)
}

 
}
dev.off() 


if (FLEET_SEGMENT != "ALL") {

for (SCENARIO in 1:length(scenarios_dirs)) {


files_results <- list.files(path = paste(casestudy_path,"/", scenarios_dirs[SCENARIO], sep=""), full.names=F)

eco_file <- files_results[!is.na(files_results[str_extract(files_results[], "Economic output") == "Economic output" ] ) ]

quanfile <- eco_file[str_detect(eco_file[], "quantiles")  ]

if (length(quanfile) != 0) {
eco_file <- eco_file[eco_file != quanfile]
}

res <- read.csv(paste(casestudy_path,"/", scenarios_dirs[SCENARIO], "/", eco_file, sep=""), sep=";")


#resCRBER_all <- res_all[res_all$Variable == "Economic.indicators[CR.BER]" ,]

 resNPV <- res[res$Variable == "Economic.indicators[NPV.discounted]" & res$Fleet_segment == FLEET_SEGMENT,]


if (SCENARIO == 1) {
jpeg(filename = paste(casestudy_path, "/Evaluation/Economic evaluation/NPV discounted ",FLEET_SEGMENT,".jpg", sep=""), width=21, height=21, bg="white", units="cm",res=200)
#max <- max(selY$Value, sqY$Value, tempWDY$Value, sel_tempWDY$Value)
#min <- min(selY$Value, sqY$Value, tempWDY$Value, sel_tempWDY$Value)

min_NPV <- min(as.numeric(as.character(resNPV_all$Value[resNPV_all$Variable == "Economic.indicators[NPV.discounted]"  & resNPV_all$Fleet_segment == FLEET_SEGMENT])) )
max_NPV <-  max(as.numeric(as.character(resNPV_all$Value[resNPV_all$Variable == "Economic.indicators[NPV.discounted]"  & resNPV_all$Fleet_segment == FLEET_SEGMENT])) )
par(mar=c(5, 6, 6, 6))  # c(bottom, left, top, right)
plot( as.numeric(as.character(resNPV$Value)) , type="b", col=1, ylim=c(min_NPV, max_NPV+(max_NPV-min_NPV)*0.4), pch=19, axes=F, ylab="NPV discounted", xlab="Years", cex.lab=1.5, lwd=2)
box()
axis(1, at=seq(1,length(resNPV$Year),1), resNPV$Year , cex.axis=1.3)   
axis(2, cex.axis=1.3)
mtext( BMT_sw_version,side=4,outer=FALSE)
legend("topleft",scenario_names, col=c(1:length(scenario_names)), bty="n", lwd=2, cex=1.3)
title(paste( "NPV - ", FLEET_SEGMENT))
} else {
 lines( as.numeric(as.character(resNPV$Value)) , type="b", col=SCENARIO, pch=19, lwd=2)
}

 
}
dev.off() 

}


 for (SCENARIO in 1:length(scenarios_dirs)) {


files_results <- list.files(path = paste(casestudy_path,"/", scenarios_dirs[SCENARIO], sep=""), full.names=F)

eco_file <- files_results[!is.na(files_results[str_extract(files_results[], "Economic output") == "Economic output" ] ) ]

quanfile <- eco_file[str_detect(eco_file[], "quantiles")  ]

if (length(quanfile) != 0) {
eco_file <- eco_file[eco_file != quanfile]
}

res <- read.csv(paste(casestudy_path,"/", scenarios_dirs[SCENARIO], "/", eco_file, sep=""), sep=";")


#resCRBER_all <- res_all[res_all$Variable == "Economic.indicators[CR.BER]" ,]

 resCRBER <- res[res$Variable == "Economic.indicators[CR.BER]" & res$Fleet_segment == FLEET_SEGMENT,]


if (SCENARIO == 1) {
jpeg(filename = paste(casestudy_path, "/Evaluation/Economic evaluation/CR.BER ",FLEET_SEGMENT,".jpg", sep=""), width=21, height=21, bg="white", units="cm",res=200)
#max <- max(selY$Value, sqY$Value, tempWDY$Value, sel_tempWDY$Value)
#min <- min(selY$Value, sqY$Value, tempWDY$Value, sel_tempWDY$Value)

min_CRBER <- min(as.numeric(as.character(resCRBER_all$Value[resCRBER_all$Variable == "Economic.indicators[CR.BER]"  & resCRBER_all$Fleet_segment == FLEET_SEGMENT])) )
max_CRBER <-  max(as.numeric(as.character(resCRBER_all$Value[resCRBER_all$Variable == "Economic.indicators[CR.BER]"  & resCRBER_all$Fleet_segment == FLEET_SEGMENT])) )
par(mar=c(5, 6, 6, 6))  # c(bottom, left, top, right)
plot( as.numeric(as.character(resCRBER$Value)), type="b", col=1, ylim=c(min_CRBER, max_CRBER+(max_CRBER-min_CRBER)*0.4), pch=19, axes=F, ylab="CR/BER", xlab="Years", cex.lab=1.5, lwd=2)
box()
axis(1, at=seq(1,length(resCRBER$Year),1), resCRBER$Year , cex.axis=1.3)   
axis(2, cex.axis=1.3)
mtext( BMT_sw_version,side=4,outer=FALSE)
legend("topleft",scenario_names, col=c(1:length(scenario_names)), bty="n", lwd=2, cex=1.3 )
title(paste( "CR/BER - ", FLEET_SEGMENT))
} else {
 lines( as.numeric(as.character(resCRBER$Value)), type="b", col=SCENARIO, pch=19, lwd=2)
}

 
}
dev.off() 


if (FLEET_SEGMENT != "ALL") {

for (SCENARIO in 1:length(scenarios_dirs)) {

files_results <- list.files(path = paste(casestudy_path,"/", scenarios_dirs[SCENARIO], sep=""), full.names=F)

eco_file <- files_results[!is.na(files_results[str_extract(files_results[], "Economic output") == "Economic output" ] ) ]

quanfile <- eco_file[str_detect(eco_file[], "quantiles")  ]

if (length(quanfile) != 0) {
eco_file <- eco_file[eco_file != quanfile]
}

res <- read.csv(paste(casestudy_path,"/", scenarios_dirs[SCENARIO], "/", eco_file, sep=""), sep=";")

resAvgDAYS <- res[res$Variable == "DAYS.average.annual" & res$Fleet_segment == FLEET_SEGMENT,]


if (SCENARIO == 1) {
jpeg(filename = paste(casestudy_path, "/Evaluation/Effort change/Annual average DAYS ",FLEET_SEGMENT,".jpg", sep=""), width=21, height=21, bg="white", units="cm",res=200)
#max <- max(selY$Value, sqY$Value, tempWDY$Value, sel_tempWDY$Value)
#min <- min(selY$Value, sqY$Value, tempWDY$Value, sel_tempWDY$Value)

min_AvgDAYS <- min(as.numeric(as.character(resAvgDAYS_all$Value[resAvgDAYS_all$Variable == "DAYS.average.annual"  & resAvgDAYS_all$Fleet_segment == FLEET_SEGMENT])) )
max_AvgDAYS <-  max(as.numeric(as.character(resAvgDAYS_all$Value[resAvgDAYS_all$Variable == "DAYS.average.annual"  & resAvgDAYS_all$Fleet_segment == FLEET_SEGMENT])) )
 par(mar=c(5, 6, 6, 6))  # c(bottom, left, top, right)
plot( as.numeric(as.character(resAvgDAYS$Value)), type="b", col=1, ylim=c(min_AvgDAYS,max_AvgDAYS+(max_AvgDAYS-min_AvgDAYS)*0.4), pch=19, axes=F, ylab="Average annual days", xlab="Years", cex.lab=1.5, lwd=2)
box()
axis(1, at=seq(1,length(resAvgDAYS$Year),1), resAvgDAYS$Year, cex.axis=1.3 )   
axis(2, cex.axis=1.3)
mtext( BMT_sw_version,side=4,outer=FALSE)
legend("topleft",scenario_names, col=c(1:length(scenario_names)), bty="n", lwd=2, cex=1.3)
title(paste( "Average annual days - ", FLEET_SEGMENT))
} else {
 lines( as.numeric(as.character(resAvgDAYS$Value)), type="b", col=SCENARIO, pch=19, lwd=2)
}

 }
dev.off() 

}



for (SCENARIO in 1:length(scenarios_dirs)) {

files_results <- list.files(path = paste(casestudy_path,"/", scenarios_dirs[SCENARIO], sep=""), full.names=F)

eco_file <- files_results[!is.na(files_results[str_extract(files_results[], "Economic output") == "Economic output" ] ) ]

quanfile <- eco_file[str_detect(eco_file[], "quantiles")  ]

if (length(quanfile) != 0) {
eco_file <- eco_file[eco_file != quanfile]
}

res <- read.csv(paste(casestudy_path,"/", scenarios_dirs[SCENARIO], "/", eco_file, sep=""), sep=";")

resVESSEL <- res[res$Variable == "VESSELS.annual" & res$Fleet_segment == FLEET_SEGMENT,]


if (SCENARIO == 1) {
jpeg(filename = paste(casestudy_path, "/Evaluation/Effort change/Annual VESSEL ",FLEET_SEGMENT,".jpg", sep=""), width=21, height=21, bg="white", units="cm",res=200)
#max <- max(selY$Value, sqY$Value, tempWDY$Value, sel_tempWDY$Value)
#min <- min(selY$Value, sqY$Value, tempWDY$Value, sel_tempWDY$Value)

min_VESSEL <- min(as.numeric(as.character(resVESSEL_all$Value[resVESSEL_all$Variable == "VESSELS.annual"  & resVESSEL_all$Fleet_segment == FLEET_SEGMENT])) )
max_VESSEL <-  max(as.numeric(as.character(resVESSEL_all$Value[resVESSEL_all$Variable == "VESSELS.annual"  & resVESSEL_all$Fleet_segment == FLEET_SEGMENT])) )
 par(mar=c(5, 6, 6, 6))  # c(bottom, left, top, right)
plot( as.numeric(as.character(resVESSEL$Value)), type="b", col=1, ylim=c(min_VESSEL,max_VESSEL+(max_VESSEL-min_VESSEL)*0.4), pch=19, axes=F, ylab="Annual vessels", xlab="Years", cex.lab=1.5, lwd=2)
box()
axis(1, at=seq(1,length(resVESSEL$Year),1), resVESSEL$Year, cex.axis=1.3 )   
axis(2, cex.axis=1.3)
mtext( BMT_sw_version,side=4,outer=FALSE)
legend("topleft",scenario_names, col=c(1:length(scenario_names)), bty="n", lwd=2, cex=1.3)
title(paste( "Annual vessels - ", FLEET_SEGMENT))
} else {
 lines( as.numeric(as.character(resVESSEL$Value)) , type="b", col=SCENARIO, pch=19, lwd=2)
}

 }
dev.off() 


 
#max_totLan <- 0
#max_totRev <- 0
#
#resTotLand_all <- res_all[res_all$Variable == "total.landings" ,]   
#resTotRev_all <- res_all[res_all$Variable == "total.revenues" ,]    

for (SCENARIO in 1:length(scenarios_dirs)) {

files_results <- list.files(path = paste(casestudy_path,"/", scenarios_dirs[SCENARIO], sep=""), full.names=F)

eco_file <- files_results[!is.na(files_results[str_extract(files_results[], "Economic output") == "Economic output" ] ) ]

quanfile <- eco_file[str_detect(eco_file[], "quantiles")  ]

if (length(quanfile) != 0) {
eco_file <- eco_file[eco_file != quanfile]
}

res <- read.csv(paste(casestudy_path,"/", scenarios_dirs[SCENARIO], "/", eco_file, sep=""), sep=";")

resTotLand <- res[res$Variable == "total.landings" & res$Fleet_segment == FLEET_SEGMENT,]

resTotLand$Value <- as.numeric(as.character(resTotLand$Value))/1000

if (SCENARIO == 1) {
jpeg(filename = paste(casestudy_path, "/Evaluation/Economic evaluation/Total Landing ",FLEET_SEGMENT,".jpg", sep=""), width=21, height=21, bg="white", units="cm",res=200)
#max <- max(selY$Value, sqY$Value, tempWDY$Value, sel_tempWDY$Value)
#min <- min(selY$Value, sqY$Value, tempWDY$Value, sel_tempWDY$Value)

min_totLan <- min(as.numeric(as.character(resTotLand_all$Value[resTotLand_all$Variable == "total.landings"  & resTotLand_all$Fleet_segment == FLEET_SEGMENT]))) / 1000
max_totLan <-  max(as.numeric(as.character(resTotLand_all$Value[resTotLand_all$Variable == "total.landings"  & resTotLand_all$Fleet_segment == FLEET_SEGMENT]))) / 1000
 par(mar=c(5, 6, 6, 6))  # c(bottom, left, top, right)
plot( as.numeric(as.character(resTotLand$Value)) , type="b", col=1, ylim=c(min_totLan,max_totLan+(max_totLan-min_totLan)*0.4), pch=19, axes=F, ylab="Total landing [tons]", xlab="Years", cex.lab=1.5, lwd=2)
box()
axis(1, at=seq(1,length(resTotLand$Year),1), resTotLand$Year, cex.axis=1.3 )   
axis(2, cex.axis=1.3)
mtext( BMT_sw_version,side=4,outer=FALSE)
legend("topleft",scenario_names, col=c(1:length(scenario_names)), bty="n", lwd=2, cex=1.3)
title(paste( "Total landing - ", FLEET_SEGMENT))
} else {
 lines( as.numeric(as.character(resTotLand$Value)) , type="b", col=SCENARIO, pch=19, lwd=2)
}

 }
dev.off() 


 
#max_totLan <- 0
#max_totRev <- 0
#
#resTotLand_all <- res_all[res_all$Variable == "total.landings" ,]   
#resTotRev_all <- res_all[res_all$Variable == "total.revenues" ,]    

for (SCENARIO in 1:length(scenarios_dirs)) {

files_results <- list.files(path = paste(casestudy_path,"/", scenarios_dirs[SCENARIO], sep=""), full.names=F)

eco_file <- files_results[!is.na(files_results[str_extract(files_results[], "Economic output") == "Economic output" ] ) ]

quanfile <- eco_file[str_detect(eco_file[], "quantiles")  ]

if (length(quanfile) != 0) {
eco_file <- eco_file[eco_file != quanfile]
}

res <- read.csv(paste(casestudy_path,"/", scenarios_dirs[SCENARIO], "/", eco_file, sep=""), sep=";")

resTotRev <- res[res$Variable == "total.revenues" & res$Fleet_segment == FLEET_SEGMENT,]

resTotRev$Value <- as.numeric(as.character(resTotRev$Value)) / 1000

if (SCENARIO == 1) {
jpeg(filename = paste(casestudy_path, "/Evaluation/Economic evaluation/Total Revenue ",FLEET_SEGMENT,".jpg", sep=""), width=21, height=21, bg="white", units="cm",res=200)
#max <- max(selY$Value, sqY$Value, tempWDY$Value, sel_tempWDY$Value)
#min <- min(selY$Value, sqY$Value, tempWDY$Value, sel_tempWDY$Value)

min_totRev <- min(as.numeric(as.character(resTotRev_all$Value[resTotRev_all$Variable == "total.revenues"  & resTotRev_all$Fleet_segment == FLEET_SEGMENT]))) / 1000
max_totRev <-  max(as.numeric(as.character(resTotRev_all$Value[resTotRev_all$Variable == "total.revenues"  & resTotRev_all$Fleet_segment == FLEET_SEGMENT]))) / 1000
 par(mar=c(5, 6, 6, 6))  # c(bottom, left, top, right)
plot( as.numeric(as.character(resTotRev$Value)) , type="b", col=1, ylim=c(min_totRev,max_totRev+(max_totRev-min_totRev)*0.4), pch=19, axes=F, ylab="Total revenue [.000 €]", xlab="Years", cex.lab=1.5, lwd=2)
box()
axis(1, at=seq(1,length(resTotRev$Year),1), resTotRev$Year, cex.axis=1.3 )   
axis(2, cex.axis=1.3)
mtext( BMT_sw_version,side=4,outer=FALSE)
legend("topleft",scenario_names, col=c(1:length(scenario_names)), bty="n", lwd=2, cex=1.3)
title(paste( "Total revenue - ", FLEET_SEGMENT))
} else {
 lines( as.numeric(as.character(resTotRev$Value)) , type="b", col=SCENARIO, pch=19, lwd=2)
}

 }
dev.off() 



}

}


