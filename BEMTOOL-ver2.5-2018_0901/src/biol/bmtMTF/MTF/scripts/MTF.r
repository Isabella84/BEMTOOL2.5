# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





MTF <-function(cod,avg_y,year_start,year_end,SRR,model,F01,Fsqsetting,ref_year)  {

#cod=XSAinfo[[MTF_spe]]$results
#year_start= start_year
#year_end = end_year


# summary(cod)

#*******************************************************************************
# Long term projections

# Here we project until ref_year with 4 different scenarios
# and include stochasticity from the recruitment residuals
# (assuming you have them - if you don't have an SRR then you will need to adjust
# the following code and ignore the bits about residuals)
# The F in 2012 is assumed to be Fstatusquo

# The 4 scenarios:
# 1) constant F = F0.1
# 2) 10% reduction in F per annum
# 3) Hit F = F0.1 by 2015, then fix at F = F0.1
# 4) Hit F = F0.1 by ref_year, then fix at F = F0.1
# Recruitment should be a constant (perhaps a mean) or from an SRR

# First set up an FLQuant of residuals from the SRR
nyears <- 10
niters  <- 500

# Setting up residuals based on the fitted SRR
if (SRR=="Y"){
srr <- fmle(as.FLSR(cod, model = model))

srrDev  <- FLQuant(sample(c(residuals(srr)),
                    nyears*niters, replace=T),
                    dimnames=list(year=casestudy.startforecast:casestudy.endforecast,
                    iter=1:niters))
#jpeg(file=paste(MTF_home_species_res, "/srr.jpg", sep=""), width=21, height=21, bg="white", units="cm",res=200) 
#plot(srr)
#dev.off() 

save_path <- paste(casestudy_path, "/", harvest_rule_id,"/Biological Pressure Impact/MSTF - ", BMT_SPECIES[MTF_spe],"/", casestudy_name, " - srr ", harvest_rule_id,".jpg", sep="") 
#jpeg(file=save_path, width=21, height=21, bg="white", units="cm",res=200) 
#plot(srr)
#dev.off()  
} else {
# *********** Another new bit ********
# If you don't have enough years in your stock recruitment relationship
# then you can artificially generate residuals by assuming that they are
# randomly selected from a log normal distribution with a chosen standard deviation
srr_sd_residuals <- 0.3
srrDev  <- log(FLQuant(rlnorm(nyears*niters,mean = 0, sd = srr_sd_residuals),
                    dimnames=list(year=casestudy.startforecast:casestudy.endforecast,
                    iter=1:niters)))
}
#*****************************************************

# Propagate the stock object
Fmean = harvest(cod)[,ac(year_end)]

Fmean = harvest(cod)[,as.character(year_end)] # data.frame(hke.stk@harvest[,ac(year_end)]@.Data)
Fbar = mean(Fmean[(range(cod)[6]+1):(range(cod)[7]+1),1])

if (Fsqsetting=="rescaled") {
for (i in 1:(nrow(harvest(cod)[,ac(year_end)])))  {
Fmean[i,1] = mean(harvest(cod)[i,ac((year_end-avg_y+1):(year_end))])
}
Fbar_mean = mean(Fmean[(range(cod)[6]+1):(range(cod)[7]+1),1])

for (i in (1:(nrow(harvest(cod)[,ac(year_end)]))))  {
harvest(cod)[i,ac(year_end)] = Fmean[i,1]* Fbar/ Fbar_mean
}
}

hke.ltf = stf(cod,nyears=(end_fore-year_end),wts.nyears=1,fbar.nyears=1)

hke.ltf.niters  <- propagate(hke.ltf,niters)


Fstatusquo <- c(fbar(cod)[,ac(year_end)]) 


reduction <<-  as.numeric(as.character(cfg[rownames(cfg) == "casestudy.HR4",2]) )  
F01 <-  Fstatusquo * (1 - reduction/100)
# 1) constant F0.1  

constantF01 <- FLQuant(F01,dimnames=list(year=(year_end+1):casestudy.endforecast))

# set F in 2011 as Fstatusquo
constantF01[,ac(year_end+1)] <- Fstatusquo
# 2) Decrease from Fstatusquo by 10% pa
Fdecrease10 <- FLQuant(Fstatusquo * 0.9^(0:9),dimnames=list(year=(year_end+1):casestudy.endforecast))
if (ref_year < casestudy.endforecast) {
Fdecrease10[,ac((ref_year+1):casestudy.endforecast)] <- Fdecrease10[,ac(ref_year)]
}
## 3) Decrease from Fstatusquo to F0.1 by 2015, then constant F0.1
#FsqtoF01_2015 <- FLQuant(c(seq(from=Fstatusquo, to = F01, length = (2015 - year_end)),rep(F01,5)),
#  dimnames=list(year=(year_end+1):casestudy.endforecast))
#FsqtoF01_2015[,ac((ref_year+1):end_fore)] <- F01 # FsqtoF01_2015[,ac(ref_year)]  
#  
# 4) Decrease from Fstatusquo to F0.1 by ref_year, then constant F0.1
FsqtoF01_ref_year <- FLQuant(c(seq(from=Fstatusquo, to = F01, length = (ref_year - year_end)),rep(F01,5)),
  dimnames=list(year=(year_end+1):casestudy.endforecast))
  if (ref_year < casestudy.endforecast) {
FsqtoF01_ref_year[,ac((ref_year+1):casestudy.endforecast)] <- F01 # FsqtoF01_2015[,ac(ref_year)]  
}
# Put all these into an FLQuants
LTFFs <- FLQuants(f = constantF01, f = Fdecrease10, f = FsqtoF01_ref_year)

require(plyr)
# Project forward using fwd() to give 3 FLStock objects inside an FLStocks object
# Note that we include the residuals for stochastic recruitment
                                                                                     #   
if (SRR=="Y"){
hke.ltf.niters <- fwd(hke.ltf.niters, ctrl=LTFFs, sr= list(model = model, params = params((srr)) ) ,  sr.residuals=exp(srrDev), sr.residuals.mult=T)
} else {
geomean <- prod(rec(cod)[,ac((year_end-avg_y+1):year_end)]) ^ (1/avg_y)
hke.ltf.niters <- fwd(hke.ltf.niters, ctrl=LTFFs, sr= list(model = "geomean", params = FLPar(geomean)),
                sr.residuals=exp(srrDev), sr.residuals.mult=T)
}

# creazione tabelle
proj.years <- (year_end+1):casestudy.endforecast
wts.nyears <- 1
fbar.nyears <- 1

# scenario constant F0.1
#------------------------------
Fbar <- iter(constantF01@.Data,1:length(proj.years))
ctrl <- fwdControl(data.frame(year=proj.years,
                              quantity='f',
                              val = Fbar))
if (SRR=="Y"){
res <- fwd(hke.ltf, ctrl=ctrl, sr= list(model = model, params = params(srr) ) ,  sr.residuals=exp(srrDev), sr.residuals.mult=T)
                } else {
geomean <- prod(rec(cod)[,ac((year_end-avg_y+1):year_end)]) ^ (1/avg_y)
res <- fwd(hke.ltf, ctrl=ctrl, sr = list(model = "geomean", params = FLPar(geomean)),
                sr.residuals=exp(srrDev), sr.residuals.mult=T)
}

## Plot 

#jpeg(file=paste(MTF_home_species_res, "/Medium_term_forecast(constant F0.1).jpg", sep=""), width=21, height=21, bg="white", units="cm",res=200)   
#plot(res)
#dev.off()

save_path <- paste(casestudy_path, "/", harvest_rule_id,"/Biological Pressure Impact/MSTF - ", BMT_SPECIES[MTF_spe],"/", casestudy_name, " - Medium_term_forecast(constant F0.1) ", harvest_rule_id,".jpg", sep="")
#jpeg(file=save_path, width=21, height=21, bg="white", units="cm",res=200) 
#plot(res)
#dev.off()  

#Four tables: SSB, Rec, Fbar and Catch
# 5, 25, 50, 75, 95
# for every year
ssb_op <- t(apply(tapply(as.data.frame(ssb(res))$data,INDEX=as.data.frame(ssb(res))$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))
rec_op <- t(apply(tapply(as.data.frame(rec(res))$data,INDEX=as.data.frame(rec(res))$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))
catch_op <- t(apply(tapply(as.data.frame(catch(res))$data,INDEX=as.data.frame(catch(res))$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))
fbar_op <- t(apply(tapply(as.data.frame(fbar(res))$data,INDEX=as.data.frame(fbar(res))$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))
#write.table(ssb_op,file=paste(MTF_home_species_res, "/ssb(constant F0.1).csv", sep=""),sep=";")
#write.table(rec_op,file=paste(MTF_home_species_res, "/rec(constant F0.1).csv", sep=""),sep=";")
#write.table(catch_op,file=paste(MTF_home_species_res, "/catch(constant F0.1).csv", sep=""),sep=";")

save_path <- paste(casestudy_path, "/", harvest_rule_id,"/Biological Pressure Impact/MSTF - ", BMT_SPECIES[MTF_spe],"/", casestudy_name, " - ssb(constant F0.1) ", harvest_rule_id,".csv", sep="")
write.table(ssb_op,file=save_path,sep=";")
save_path <- paste(casestudy_path, "/", harvest_rule_id,"/Biological Pressure Impact/MSTF - ", BMT_SPECIES[MTF_spe],"/", casestudy_name, " - rec(constant F0.1) ", harvest_rule_id,".csv", sep="")
write.table(rec_op,file=save_path,sep=";")
save_path <- paste(casestudy_path, "/", harvest_rule_id,"/Biological Pressure Impact/MSTF - ", BMT_SPECIES[MTF_spe],"/", casestudy_name, " - catch(constant F0.1) ", harvest_rule_id,".csv", sep="")
write.table(catch_op,file=save_path,sep=";")

perc_red = data.frame(matrix(nrow=nrow(fbar_op),ncol=1))                                                                                                    
for (i in 2:nrow(fbar_op)){
perc_red[i,1] = (fbar_op[i,3]-fbar_op[i-1,3])/fbar_op[i-1,3] * 100
}
fbar_op = cbind(fbar_op,perc_red)
colnames(fbar_op)= c(colnames(fbar_op)[1:5],"Percentage_Reduction")
#write.table(fbar_op,file=paste(MTF_home_species_res, "/fbar(constant F0.1).csv", sep=""),sep=";")

save_path <- paste(casestudy_path, "/", harvest_rule_id,"/Biological Pressure Impact/MSTF - ", BMT_SPECIES[MTF_spe],"/", casestudy_name, " - fbar(constant F0.1) ", harvest_rule_id,".csv", sep="")
write.table(fbar_op,file=save_path,sep=";")
#-------------------------

# scenario decrease 10%
#------------------------------
Fbar <- iter(Fdecrease10@.Data,1:length(proj.years))
ctrl <- fwdControl(data.frame(year=proj.years,
                              quantity='f',
                              val = Fbar))
if (SRR=="Y"){
res <- fwd(hke.ltf, ctrl=ctrl, sr= list(model = model, params = params(srr) ) ,  sr.residuals=exp(srrDev), sr.residuals.mult=T)
                } else {
geomean <- prod(rec(cod)[,ac((year_end-avg_y+1):year_end)]) ^ (1/avg_y)
res <- fwd(hke.ltf, ctrl=ctrl, sr= list(model = "geomean", params = FLPar(geomean)),
                sr.residuals=exp(srrDev), sr.residuals.mult=T)
}
## Plot 
#
#jpeg(file=paste(MTF_home_species_res, "/Medium_term_forecast(decrease 10percent).jpg", sep=""), width=21, height=21, bg="white", units="cm",res=200)   
#plot(res)
#dev.off()

save_path <- paste(casestudy_path, "/", harvest_rule_id,"/Biological Pressure Impact/MSTF - ", BMT_SPECIES[MTF_spe],"/", casestudy_name, " - Medium_term_forecast(decrease 10percent) ", harvest_rule_id,".jpg", sep="")
#jpeg(file=save_path, width=21, height=21, bg="white", units="cm",res=200) 
#plot(res)
#dev.off() 


#Four tables: SSB, Rec, Fbar and Catch
# 5, 25, 50, 75, 95
# for every year
ssb_op <- t(apply(tapply(as.data.frame(ssb(res))$data,INDEX=as.data.frame(ssb(res))$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))
rec_op <- t(apply(tapply(as.data.frame(rec(res))$data,INDEX=as.data.frame(rec(res))$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))
catch_op <- t(apply(tapply(as.data.frame(catch(res))$data,INDEX=as.data.frame(catch(res))$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))
fbar_op <- t(apply(tapply(as.data.frame(fbar(res))$data,INDEX=as.data.frame(fbar(res))$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))
#write.table(ssb_op,file=paste(MTF_home_species_res, "/ssb(decrease 10percent).csv", sep=""),sep=";")
#write.table(rec_op,file=paste(MTF_home_species_res, "/rec(decrease 10percent).csv", sep=""),sep=";")
#write.table(catch_op,file=paste(MTF_home_species_res, "/catch(decrease 10percent).csv", sep=""),sep=";")

save_path <- paste(casestudy_path, "/", harvest_rule_id,"/Biological Pressure Impact/MSTF - ", BMT_SPECIES[MTF_spe],"/", casestudy_name, " - ssb(decrease 10percent) ", harvest_rule_id,".csv", sep="")
write.table(ssb_op,file=save_path,sep=";")
save_path <- paste(casestudy_path, "/", harvest_rule_id,"/Biological Pressure Impact/MSTF - ", BMT_SPECIES[MTF_spe],"/", casestudy_name, " - rec(decrease 10percent) ", harvest_rule_id,".csv", sep="")
write.table(rec_op,file=save_path,sep=";")
save_path <- paste(casestudy_path, "/", harvest_rule_id,"/Biological Pressure Impact/MSTF - ", BMT_SPECIES[MTF_spe],"/", casestudy_name, " - catch(decrease 10percent) ", harvest_rule_id,".csv", sep="")
write.table(catch_op,file=save_path,sep=";")

perc_red = data.frame(matrix(nrow=nrow(fbar_op),ncol=1))
for (i in 2:nrow(fbar_op)){
perc_red[i,1] = (fbar_op[i,3]-fbar_op[i-1,3])/fbar_op[i-1,3] * 100
}
fbar_op = cbind(fbar_op,perc_red)
colnames(fbar_op)= c(colnames(fbar_op)[1:5],"Percentage_Reduction")
#write.table(fbar_op,file=paste(MTF_home_species_res, "/fbar(decrease 10percent).csv", sep=""),sep=";")

save_path <- paste(casestudy_path, "/", harvest_rule_id,"/Biological Pressure Impact/MSTF - ", BMT_SPECIES[MTF_spe],"/",casestudy_name, " - fbar(decrease 10percent) ", harvest_rule_id,".csv", sep="")
write.table(fbar_op,file=save_path,sep=";")
#-------------------------

# scenario F0.1 in 2015
#------------------------------
#Fbar <- iter(FsqtoF01_2015@.Data,1:length(proj.years))
#ctrl <- fwdControl(data.frame(year=proj.years,
#                              quantity='f',
#                              val = Fbar))
#if (SRR=="Y"){
#res <- fwd(hke.ltf, ctrl=ctrl, sr= list(model = model, params = params(srr) ) ,  sr.residuals=exp(srrDev), sr.residuals.mult=T)
#                } else {
#geomean <- prod(rec(cod)[,ac((year_end-avg_y+1):year_end)]) ^ (1/avg_y)
#res <- fwd(hke.ltf, ctrl=ctrl, sr= list(model = "geomean", params = FLPar(geomean)),
#                sr.residuals=exp(srrDev), sr.residuals.mult=T)
#}
#Four tables: SSB, Rec, Fbar and Catch
# 5, 25, 50, 75, 95
# for every year
#ssb_op <- t(apply(tapply(as.data.frame(ssb(res))$data,INDEX=as.data.frame(ssb(res))$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))
#rec_op <- t(apply(tapply(as.data.frame(rec(res))$data,INDEX=as.data.frame(rec(res))$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))
#catch_op <- t(apply(tapply(as.data.frame(catch(res))$data,INDEX=as.data.frame(catch(res))$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))
#fbar_op <- t(apply(tapply(as.data.frame(fbar(res))$data,INDEX=as.data.frame(fbar(res))$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))
#write.table(ssb_op,file="Results_MTF/ssb(F01 in 2015).csv",sep=";")
#write.table(rec_op,file="Results_MTF/rec(F01 in 2015).csv",sep=";")
#write.table(catch_op,file="Results_MTF/catch(F01 in 2015).csv",sep=";")
#perc_red = data.frame(matrix(nrow=nrow(fbar_op),ncol=1))
#for (i in 2:nrow(fbar_op)){
#perc_red[i,1] = (fbar_op[i,3]-fbar_op[i-1,3])/fbar_op[i-1,3] * 100
#}
#fbar_op = cbind(fbar_op,perc_red)
#colnames(fbar_op)= c(colnames(fbar_op)[1:5],"Percentage_Reduction")
#write.table(fbar_op,file="Results_MTF/fbar(F01 in 2015).csv",sep=";")
##-------------------------

# scenario F0.1 in ref_year
#------------------------------
Fbar <- iter(FsqtoF01_ref_year@.Data,1:length(proj.years))
ctrl <- fwdControl(data.frame(year=proj.years,
                              quantity='f',
                              val = Fbar))
if (SRR=="Y"){
res <- fwd(hke.ltf, ctrl=ctrl, sr= list(model = model, params = params(srr) ) ,  sr.residuals=exp(srrDev), sr.residuals.mult=T)
                } else {
geomean <- prod(rec(cod)[,ac((year_end-avg_y+1):year_end)]) ^ (1/avg_y)
res <- fwd(hke.ltf, ctrl=ctrl, sr= list(model = "geomean", params = FLPar(geomean)),
                sr.residuals=exp(srrDev), sr.residuals.mult=T)
}


#jpeg(file=paste(MTF_home_species_res, "/Medium_term_forecast(F01 in ref_year).jpg", sep=""), width=21, height=21, bg="white", units="cm",res=200)   
#plot(res)
#dev.off()

save_path <- paste(casestudy_path, "/", harvest_rule_id,"/Biological Pressure Impact/MTF - ", BMT_SPECIES[MTF_spe],"/", casestudy_name, " -  Medium_term_forecast(F01 in ref_year) - ", harvest_rule_id,".jpg", sep="")
#jpeg(file=save_path, width=21, height=21, bg="white", units="cm",res=200) 
#plot(res)
#dev.off() 

#Four tables: SSB, Rec, Fbar and Catch
# 5, 25, 50, 75, 95
# for every year
ssb_op <- t(apply(tapply(as.data.frame(ssb(res))$data,INDEX=as.data.frame(ssb(res))$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))
rec_op <- t(apply(tapply(as.data.frame(rec(res))$data,INDEX=as.data.frame(rec(res))$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))
catch_op <- t(apply(tapply(as.data.frame(catch(res))$data,INDEX=as.data.frame(catch(res))$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))
fbar_op <- t(apply(tapply(as.data.frame(fbar(res))$data,INDEX=as.data.frame(fbar(res))$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))

#write.table(ssb_op,file=paste(MTF_home_species_res, "/ssb(F01 in ref_year).csv", sep=""),sep=";")
#write.table(rec_op,file=paste(MTF_home_species_res, "/rec(F01 in ref_year).csv", sep=""),sep=";")
#write.table(catch_op,file=paste(MTF_home_species_res, "/catch(F01 in ref_year).csv", sep=""),sep=";")

save_path <- paste(casestudy_path, "/", harvest_rule_id,"/Biological Pressure Impact/MSTF - ", BMT_SPECIES[MTF_spe],"/", casestudy_name, " - ssb(F01 in ref_year) ", harvest_rule_id,".csv", sep="")
write.table(ssb_op,file=save_path,sep=";")
save_path <- paste(casestudy_path, "/", harvest_rule_id,"/Biological Pressure Impact/MSTF - ", BMT_SPECIES[MTF_spe],"/", casestudy_name, " - rec(F01 in ref_year) ", harvest_rule_id,".csv", sep="")
write.table(rec_op,file=save_path,sep=";")
save_path <- paste(casestudy_path, "/", harvest_rule_id,"/Biological Pressure Impact/MSTF - ", BMT_SPECIES[MTF_spe],"/", casestudy_name, " - catch(F01 in ref_year) ", harvest_rule_id,".csv", sep="")
write.table(catch_op,file=save_path,sep=";")


perc_red = data.frame(matrix(nrow=nrow(fbar_op),ncol=1))
for (i in 2:nrow(fbar_op)){
perc_red[i,1] = (fbar_op[i,3]-fbar_op[i-1,3])/fbar_op[i-1,3] * 100
}
fbar_op = cbind(fbar_op,perc_red)
colnames(fbar_op)= c(colnames(fbar_op)[1:5],"Percentage_Reduction")
# write.table(fbar_op,file=paste(MTF_home_species_res, "/fbar(F01 in ref_year).csv", sep=""),sep=";")

save_path <- paste(casestudy_path, "/", harvest_rule_id,"/Biological Pressure Impact/MSTF - ", BMT_SPECIES[MTF_spe],"/", casestudy_name, " - fbar(F01 in ref_year) ", harvest_rule_id,".csv", sep="")
write.table(fbar_op,file=save_path,sep=";")

if (MTF_spe == 1) {
save_path <- paste(casestudy_path, "/", harvest_rule_id,"/Biological Pressure Impact/", casestudy_name, " - fbar(F01 in ref_year) - ", harvest_rule_id,".csv", sep="")
write.table(fbar_op,file=save_path,sep=";")
}

return(res)                                                 
}
