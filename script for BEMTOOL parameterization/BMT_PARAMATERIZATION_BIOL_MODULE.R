# Biological parameterization of BEMTOOL
setwd("C:\\Users\\Utente\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\FAIRSEA\\Pilot action ITALY\\Parametrizzazione bio-press")

dir.create("BMT_INPUT")
path_input="C:\\Users\\Utente\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\FAIRSEA\\Pilot action ITALY\\Parametrizzazione bio-press\\Solea input"

ts<-c(2005:2020)
spe="SOL"


#RECRUITS
Rec_file=read.table(paste(path_input,"\\Recruits.csv",sep=""),sep=";", header=T)
Rec=Rec_file[Rec_file[,1] %in% ts,]

DF_R=data.frame(year=ts,	seed= c(Rec[1,2],rep("",(length(ts)-1))),	Jan=Rec[,2],	Feb=Rec[,2],	Mar=Rec[,2],	Apr=Rec[,2],	May=Rec[,2],	Jun=Rec[,2],	Jul=Rec[,2],	Aug=Rec[,2],	Sep=Rec[,2],	Oct=Rec[,2],	Nov=Rec[,2],	Dec=Rec[,2])
write.table(DF_R,paste(getwd(),"\\BMT_INPUT\\Recruits_",spe,".csv",sep=""),sep=";",row.names=F)                    

# M
M_file=read.table(paste(path_input,"\\Natural_mortality_vector.csv",sep=""),sep=";", header=T)

tr=2  # in months, tr is the age of recruitment to the fishing ground 
lifespan=10


age_month=seq(tr,lifespan*12,1)
M=age_month
for (r in 1:(length(age_month)-1)){
  age=trunc(age_month[r]/12,0)
  M[r]=M_file[1,age+2]
}
M[length(age_month)]=M[length(age_month)-1]

DF_M=data.frame(age_month=age_month,	M=M)

write.table(DF_M,paste(getwd(),"\\BMT_INPUT\\M_",spe,".csv",sep=""),sep=";",row.names=F)                    

# Z
# ATTENTION!!!! RUN OR CASE 1 OR CASE 2!
#______________________________________________________

# CASE 1 - if you have F at age (stock assessment with age structured models)

# F_file=read.table(paste(path_input,"\\F_at_age.csv",sep=""),sep=";", header=T)
# Fish_mort=F_file[F_file[,1] %in% ts,]
# M_vector=read.table(paste(getwd(),"\\BMT_INPUT","\\M_",spe,".csv",sep=""),sep=";", header=T)
# mean_M=mean(M_vector[,2])
# 
# F_range=c(1,4)
# 
# F_bar=data.frame(year=ts,Fbar=rowMeans(Fish_mort[,c((F_range[1]+2):(F_range[2]+2))]))
# 
# Z_bar=F_bar
# Z_bar[,2]=Z_bar[,2]+mean_M
# 
# 
# DF_Z=data.frame(year=ts,	seed= c(Z_bar[1,2],rep("",(length(ts)-1))),	Jan=Z_bar[,2],	Feb=Z_bar[,2],	Mar=Z_bar[,2],	Apr=Z_bar[,2],	May=Z_bar[,2],	Jun=Z_bar[,2],	Jul=Z_bar[,2],	Aug=Z_bar[,2],	Sep=Z_bar[,2],	Oct=Z_bar[,2],	Nov=Z_bar[,2],	Dec=Z_bar[,2],sex="M")
# DF_Z2=DF_Z
# DF_Z2$sex="F"
# DF_Z=rbind(DF_Z,DF_Z2)
# 
# write.table(DF_Z,paste(getwd(),"\\BMT_INPUT\\Zmean_",F_range[1],"-",F_range[2],"_",spe,".csv",sep=""),sep=";",row.names=F)                    
# CASE 2 - if you have F mean (stock assessment with production models)

F_file=read.table(paste(path_input,"\\F_mean.csv",sep=""),sep=";", header=T)
Fish_mort=F_file[F_file[,1] %in% ts,]
M_vector=read.table(paste(getwd(),"\\BMT_INPUT","\\M_",spe,".csv",sep=""),sep=";", header=T)
mean_M=mean(M_vector[,2])


Z_bar=Fish_mort
Z_bar[,2]=Z_bar[,2]+mean_M


DF_Z=data.frame(year=ts,	seed= c(Z_bar[1,2],rep("",(length(ts)-1))),	Jan=Z_bar[,2],	Feb=Z_bar[,2],	Mar=Z_bar[,2],	Apr=Z_bar[,2],	May=Z_bar[,2],	Jun=Z_bar[,2],	Jul=Z_bar[,2],	Aug=Z_bar[,2],	Sep=Z_bar[,2],	Oct=Z_bar[,2],	Nov=Z_bar[,2],	Dec=Z_bar[,2],sex="M")
DF_Z2=DF_Z
DF_Z2$sex="F"
DF_Z=rbind(DF_Z,DF_Z2)

write.table(DF_Z,paste(getwd(),"\\BMT_INPUT\\Zmean_",spe,".csv",sep=""),sep=";",row.names=F)                  


Fleet_segments <-c("Abruzzo_DTS",
                  "Abruzzo_PGP",
                   "EmiliaRomagna_DTS",
                   "EmiliaRomagna_PGP",
                  "EmiliaRomagna_TBB",
                   "FriuliVeneziaGiulia_DTS",
                  "FriuliVeneziaGiulia_PGP",
                   "FriuliVeneziaGiulia_TBB",
                  "Marche_DTS",
                   "Marche_PGP",
                  "Marche_TBB",
                   "Molise_DTS",
                  "Molise_PGP",
                   "Veneto_DTS",
                  "Veneto_PGP",
                   "Veneto_TBB",
                  "HRV_DFN",
                   "SVN_DFN")
Months=c("Jan",
         "Feb",
         "Mar",
         "Apr",
         "May",
         "Jun",
         "Jul",
         "Aug",
         "Set",
         "Oct",
         "Nov",
         "Dec")



month_vec=c("seed",rep(c(Months),length(ts)))
month_vec=rep(month_vec,length(Fleet_segments))
y=1
year_vec=c("",rep(ts[y],12))

for (y in 2:length(ts)){
year_vec2=c(year_vec,rep(ts[y],12))
year_vec=year_vec2
}
year_vec=rep(year_vec,length(Fleet_segments))

# Selectivity models:
# 1 classical ogive, 2 ogive with de-selection, 3 gaussian symmetric, 4 log-normal, 5 bi-normal, 6 norma asymmetric

models = rep(2,18)
param1=c(160,	180,	160,	180,	160,	160,	180,	160,	160,	180,	160,	160,	180,	160,	180,	160,270,270)
param2=rep(20,18)
param3=c(330,	270,	330,	270,	290,	330,	270,	290,	330,	270,	290,	330,	270,	330,	270,	290, 370,370)
param4=rep(NA,18)
param5=rep(NA,18)
param6=rep(NA,18)

x=length(ts)*12+1
param1_vec=rep(param1[1],x)
for (fs in 2:length(Fleet_segments)){
param1_vec2= c(param1_vec,rep(param1[fs],x) )
param1_vec=  param1_vec2
}
param2_vec=rep(param2[1],x)
for (fs in 2:length(Fleet_segments)){
  param2_vec2= c(param2_vec,rep(param2[fs],x) )
  param2_vec=  param2_vec2
}
param3_vec=rep(param3[1],x)
for (fs in 2:length(Fleet_segments)){
  param3_vec2= c(param3_vec,rep(param3[fs],x) )
  param3_vec=  param3_vec2
}
param4_vec=rep(param4[1],x)
for (fs in 2:length(Fleet_segments)){
  param4_vec2= c(param4_vec,rep(param4[fs],x) )
  param4_vec=  param4_vec2
}
param5_vec=rep(param5[1],x)
for (fs in 2:length(Fleet_segments)){
  param5_vec2= c(param5_vec,rep(param5[fs],x) )
  param5_vec=  param5_vec2
}

sel_type_vec=rep(models[1],x)
for (fs in 2:length(Fleet_segments)){
  sel_type_vec2= c(sel_type_vec,rep(models[fs],x) )
  sel_type_vec=  sel_type_vec2
}

fleet_segment_vec=rep(Fleet_segments[1],x)
for (fs in 2:length(Fleet_segments)){
  fleet_segment_vec2= c(fleet_segment_vec,rep(Fleet_segments[fs],x) )
  fleet_segment_vec=  fleet_segment_vec2
}

DF=data.frame(year=year_vec,	month=month_vec,	param1=param1_vec,	param2=param2_vec,	param3=param3_vec,	param4=param4_vec,	param5=param5_vec,	sel_type=sel_type_vec,	fleet_segment=fleet_segment_vec)

write.table(DF,paste(getwd(),"\\BMT_INPUT\\Selectivity_",spe,".csv",sep=""),sep=";",row.names=F)                    

# 
# discard = c("Y","Y")
# L50=c(90,90)
# L75_L25=c(20,20)
# 
# 
# discard_vec=rep(discard[1],x)
# for (fs in 2:length(Fleet_segments)){
#   discard_vec_vec2= c(discard_vec,rep(discard[fs],x) )
#   discard_vec_vec=  discard_vec_vec2
# }
# 
# L50_vec=rep(L50[1],x)
# for (fs in 2:length(Fleet_segments)){
#   L50_vec2= c(L50_vec,rep(param1[fs],x) )
#   L50_vec=  L50_vec2
# }
# 
# L75_L25_vec=rep(L75_L25[1],x)
# for (fs in 2:length(Fleet_segments)){
#   L75_L25_vec2= c(L75_L25_vec,rep(L75_L25[fs],x) )
#   L75_L25_vec=  L75_L25_vec2
# }
# 
# DF=data.frame(year=year_vec,	month=month_vec,	L50=L50_vec,	L75_L25=L75_L25_vec,	discard=discard_vec,	fleet_segment=fleet_segment_vec)
# 
# 
# write.table(DF,paste(getwd(),"\\BMT_INPUT\\Discard_reverse_ogive_",spe,".csv",sep=""),sep=";",row.names=F)                    

Mat_file=read.table(paste(path_input,"\\Maturity_at_age.csv",sep=""),sep=";", header=T)

Linf=350
K= 0.57
t0=-0.38

Mat_file$Length=Linf*(1-exp(-K*(Mat_file$Age-t0)))
L50=round(mean(Mat_file[Mat_file[,2]<0.8 & Mat_file[,2]>0.4,3]),2)
print(paste("The L50 should be around ", L50, " mm according to the maturity vector of the assessment",sep=""),quote=F)

