# Script to create economic time series file for BMT
setwd("C:\\IMPLEMED_GSA6\\parametrizzaz economica")

Fleet_segments <- c("GSA6_DTS_VL0612",
                    "GSA6_DTS_VL1218",
                    "GSA6_DTS_VL1824",
                    "GSA6_DTS_VL2440", 
                    "GSA6_PGP_VL0018",
                    "GSA6_HOK_VL0624")


Fleet_segments_ALL=Fleet_segments




dir.create("BMT_INPUT")
path_input= getwd()

ts<-c(2006:2020)


trans_year=c(2019,2020)

Associations=read.table(paste(path_input,"\\Associations.csv",sep=""),sep=";", header=T)

AER_data=read.table(paste(path_input,"\\AER.csv",sep=""),sep=";", header=T)

AER_data=AER_data[AER_data$year %in% ts & as.character(AER_data$FS) %in% unique(Associations$AER_FS),]


FDI_data=read.table(paste(path_input,"\\FDI_data.csv",sep=""),sep=";", header=T)

FDI_data=FDI_data[FDI_data$year %in% ts & as.character(FDI_data$FS) %in% as.character(unique(Associations$FS)),]

FirstColumn= c("casestudy.fleetsegmentcode",
               "casestudy.fishingtechnique",
               "casestudy.loa",
               "casestudy.maxavgseadays",
             "casestudy.totallandings", # DA FDI
             "casestudy.revenues.S1",# DA FDI
             "casestudy.revenues.S2",# DA FDI
             "casestudy.revenues.S3",# DA FDI
             "casestudy.totalrevenues",# DA FDI
             "casestudy.revenues.discard.S1",
             "casestudy.revenues.discard.S2",
             "casestudy.revenues.discard.S3",
             "casestudy.fuelcosts",
             "casestudy.commercialcosts",
             "casestudy.othervariablecosts",
             "casestudy.totalvariablecosts",
             "casestudy.maintenancecosts",
             "casestudy.otherfixedcosts",
             "casestudy.essentialcosts",
             "casestudy.avoidablemaintenancecosts",
             "casestudy.unavoidablemaintenancecosts",
             "casestudy.totalfixedcosts",
             "casestudy.labourcosts",
             "casestudy.depreciationcosts",
             "casestudy.opportunitycosts",
             "casestudy.totalcapitalcosts",
             "casestudy.otherincome",
             "casestudy.employment",
             "casestudy.capitalvalue")

stks=c("HKE","MUT","DPS")
nb_stk=length(stks)

for (fs in 1:length(Fleet_segments)){
  
DF0=matrix(0,nrow=(3+2*nb_stk),ncol=length(ts))
colnames(DF0)=ts

FS_associated=as.character(Associations$AER_FS[Associations$FS==Fleet_segments[fs]])
AER_data_temp1=AER_data[as.character(AER_data$FS)==FS_associated,]

ty=1

#maxseadays: we assume that are the same for all FS of the Country (differentiated by gear) 
AER_data_temp=AER_data_temp1[AER_data_temp1$variable_code=="maxseadays",] 

if (nrow(AER_data_temp)>0){
  AER_data_temp=AER_data_temp[as.character(AER_data_temp$value)!="NULL",]
  
  AER_data_temp=aggregate(as.numeric(AER_data_temp$value),by=list(AER_data_temp$FS,AER_data_temp$year),FUN="max")
  
  colnames(AER_data_temp)=c("FS","year","value")
  
  for (rr in 1:nrow(AER_data_temp)){
    
    year=AER_data_temp$year[rr] 
    DF0[ty,colnames(DF0)==year]=AER_data_temp$value[rr]
    
  }
  
} else {
  DF0[ty,]=0  
}


ty=2 #total landing (in FDI there is the distinction of the GSA)
#
if(fs>4){
FDI_data_temp=FDI_data[as.character(FDI_data$FS)==as.character(unique(Associations$AER_FS))[1],] 
# 
colnames(FDI_data_temp)[20]="totwghtlandg"
colnames(FDI_data_temp)[20]="LandEUR"
# # LandEUR
 if (nrow(FDI_data_temp)>0){
# #   
 FDI_data_temp1=aggregate(as.numeric(FDI_data_temp$totwghtlandg),by=list(FDI_data_temp$FS,FDI_data_temp$year),FUN="sum")
 FDI_data_temp2=aggregate(as.numeric(FDI_data_temp$total.value.of.landings..euro.),by=list(FDI_data_temp$FS,FDI_data_temp$year,FDI_data_temp$species),FUN="sum")
# 
#   
 colnames(FDI_data_temp1)=c("FS","year","value")
 colnames(FDI_data_temp2)=c("FS","year","species","LandEUR")
# #   
 for (rr in 1:nrow(FDI_data_temp1)){
     
 year=FDI_data_temp1$year[rr] 
 DF0[ty,colnames(DF0)==year]=FDI_data_temp1$value[rr]
# #     
  }
# #   
  } else {
   DF0[ty,]=0  
   
 }

} else {
  DF0[ty,]=0  
  
}

#add_data=read.table(paste(path_input,"\\MBL2 SOL.csv",sep=""),sep=";", header=T)

Days_by_FS=read.table("Days.csv",sep=";",header=T)
Days_by_FS_temp=Days_by_FS[Days_by_FS$FS==Fleet_segments[fs] & Days_by_FS$Year %in% ts,] 



Vessels_by_FS=read.table("Vessels.csv",sep=";",header=T)
Vessels_by_FS_temp=Vessels_by_FS[Vessels_by_FS$FS==Fleet_segments[fs] & Vessels_by_FS$Year %in% ts,] 

for (st in 1:nb_stk) {
ty=3 + (st-1) 
if(fs>4){
#revenues by stock
#if (nrow(FDI_data_temp2)>0){
add_data_temp=FDI_data_temp2[FDI_data_temp2$FS==Fleet_segments[fs] & FDI_data_temp2$species==stks[st],] 
if (nrow(add_data_temp)>0){
# 
#   #AER_data_temp=AER_data_temp[as.character(AER_data_temp$value)!="NULL",]
#???add_data_temp$LandkEUR=add_data_temp$LandkEUR*1000
   add_data_temp=aggregate(as.numeric(add_data_temp$LandEUR),by=list(add_data_temp$FS,add_data_temp$year),FUN="sum")
   colnames(add_data_temp)=c("FS","year","value")
#   
#   
#   
#   
   for (rr in 1:nrow(add_data_temp)){
     
     year=add_data_temp$year[rr] 
     DF0[ty,colnames(DF0)==year]=add_data_temp$value[rr]
     
   }
   
# 
   }
   } else {
  DF0[ty,]=0  
  
}

}

   ty=3 + st
   
   if(fs>4){
  # #revenues total
 FDI_data_temp=FDI_data[FDI_data$FS==Fleet_segments[1] ,] 
  # 
 if (nrow(FDI_data_temp)>0){
  #   
   FDI_data_temp=aggregate(as.numeric(FDI_data_temp$totvallandg),by=list(FDI_data_temp$FS,FDI_data_temp$year),FUN="sum")
  #   
   colnames(FDI_data_temp)=c("FS","year","value")
  #   
   for (rr in 1:nrow(FDI_data_temp)){
  #     
     year=FDI_data_temp$year[rr] 
     DF0[ty,colnames(DF0)==year]=FDI_data_temp$value[rr]
  #     
   }
  #   
 } 
 }else {
     DF0[ty,]=0  
   
  }
  # 



types_costs=c("totenercost","commercialcost","totvarcost","tottotvarcost","totrepcost","totnovarcost","essentialcosts","avoidablemaintenancecosts","unavoidablemaintenancecosts","totnovarcost","totcrewwage","totdepcost","totopportunitycost","totcapcost","tototherinc","totjob","totdeprep") 

DF1=matrix(0,nrow=length(types_costs),ncol=length(ts))
colnames(DF1)=ts

#rownames(DF1)=seq(1,12,1)

# Fleet_segments[1]

# for (ty in 1:length(types_costs)){

ty=1 #energy costs 
print(ty)
  
AER_data_temp=AER_data_temp1[AER_data_temp1$variable_code==types_costs[ty],] 

Days_at_sea=AER_data_temp1[AER_data_temp1$variable_code=="totseadays",] 

#unique(AER_data_temp1$variable_code)

AER_data_temp=AER_data_temp[as.character(AER_data_temp$value)!="NULL",]
Days_at_sea=Days_at_sea[as.character(Days_at_sea$value)!="NULL",]

AER_data_temp=aggregate(as.numeric(AER_data_temp$value),by=list(AER_data_temp$FS,AER_data_temp$year),FUN="sum")
Days_at_sea=aggregate(as.numeric(Days_at_sea$value),by=list(Days_at_sea$FS,Days_at_sea$year),FUN="sum")

AER_data_temp$Days_at_sea=Days_at_sea$x 
AER_data_temp$avgenercosts=AER_data_temp$x/Days_at_sea$x

colnames(AER_data_temp)[1:3]=c("FS","year","totenercost")

Days_by_FS_temp=aggregate(as.numeric(Days_by_FS_temp$Days),by=list(Days_by_FS_temp$FS,Days_by_FS_temp$Year),FUN="sum")
colnames(Days_by_FS_temp)=c("FS","year","DaysFS")

AER_data_temp=merge(AER_data_temp,Days_by_FS_temp,by =c("year"),all=T)

#AER_data_temp[AER_data_temp$year %in% c(2006:2007),]$avgenercosts=AER_data_temp[AER_data_temp$year == 2008,]$avgenercosts

AER_data_temp[is.na(AER_data_temp$avgenercosts),]$avgenercosts<-mean(AER_data_temp[AER_data_temp$year %in% c(2014:2016),]$avgenercosts)

AER_data_temp[is.na(AER_data_temp$DaysFS),]$DaysFS<-AER_data_temp[AER_data_temp$year %in% c(2015),]$DaysFS

AER_data_temp$Est_enercost=AER_data_temp$DaysFS*AER_data_temp$avgenercosts



for (rr in 1:nrow(AER_data_temp)){
  
 year=AER_data_temp$year[rr] 
 DF1[ty,colnames(DF1)==year]=AER_data_temp$Est_enercost[rr]
   
}


ty=2 # commercial costs
DF1[ty,]=0  

ty=3 # totvarcost

AER_data_temp=AER_data_temp1[AER_data_temp1$variable_code==types_costs[ty],] 

Days_at_sea=AER_data_temp1[AER_data_temp1$variable_code=="totseadays",] 

#unique(AER_data_temp1$variable_code)

AER_data_temp=AER_data_temp[as.character(AER_data_temp$value)!="NULL",]
Days_at_sea=Days_at_sea[as.character(Days_at_sea$value)!="NULL",]

AER_data_temp=aggregate(as.numeric(AER_data_temp$value),by=list(AER_data_temp$FS,AER_data_temp$year),FUN="sum")
Days_at_sea=aggregate(as.numeric(Days_at_sea$value),by=list(Days_at_sea$FS,Days_at_sea$year),FUN="sum")

AER_data_temp$Days_at_sea=Days_at_sea$x 
AER_data_temp$avgvarcosts=AER_data_temp$x/Days_at_sea$x

colnames(AER_data_temp)[1:3]=c("FS","year","totvarcost")

Days_by_FS_temp=aggregate(as.numeric(Days_by_FS_temp$DaysFS),by=list(Days_by_FS_temp$FS,Days_by_FS_temp$year),FUN="sum")
colnames(Days_by_FS_temp)=c("FS","year","DaysFS")

AER_data_temp=merge(AER_data_temp,Days_by_FS_temp,by =c("year"),all=T)


AER_data_temp[is.na(AER_data_temp$avgvarcosts),]$avgvarcosts<-mean(AER_data_temp[AER_data_temp$year %in% c(2014:2016),]$avgvarcosts)

AER_data_temp[is.na(AER_data_temp$DaysFS),]$DaysFS<-AER_data_temp[AER_data_temp$year %in% c(2015),]$DaysFS

#AER_data_temp[AER_data_temp$year %in% c(2006:2007),]$avgvarcosts=AER_data_temp[AER_data_temp$year == 2008,]$avgvarcosts

AER_data_temp$Est_varcost=AER_data_temp$DaysFS*AER_data_temp$avgvarcosts


for (rr in 1:nrow(AER_data_temp)){
  
  year=AER_data_temp$year[rr] 
  DF1[ty,colnames(DF1)==year]=AER_data_temp$Est_varcost[rr]
  
}

DF1[4,]=DF1[3,]+DF1[1,]
  
ty=5 #repair costs
Vessels_by_FS=read.table("Vessels.csv",sep=";",header=T)
Vessels_by_FS_temp=Vessels_by_FS[Vessels_by_FS$FS==Fleet_segments[fs] & Vessels_by_FS$Year %in% ts,] 


AER_data_temp=AER_data_temp1[AER_data_temp1$variable_code==types_costs[ty],] 

Vessels=AER_data_temp1[AER_data_temp1$variable_code=="totves",] 

#unique(AER_data_temp1$variable_code)

AER_data_temp=AER_data_temp[as.character(AER_data_temp$value)!="NULL",]
Vessels=Vessels[as.character(Vessels$value)!="NULL",]

AER_data_temp=aggregate(as.numeric(AER_data_temp$value),by=list(AER_data_temp$FS,AER_data_temp$year),FUN="sum")
Vessels=aggregate(as.numeric(Vessels$value),by=list(Vessels$FS,Vessels$year),FUN="sum")

Vessels=Vessels[Vessels$Group.2 <trans_year[1],]

AER_data_temp$Vessels=Vessels$x 
AER_data_temp$avgrepcosts=AER_data_temp$x/Vessels$x

colnames(AER_data_temp)[1:3]=c("FS","year","repcost")

Vessels_by_FS_temp=aggregate(as.numeric(Vessels_by_FS_temp$Vess),by=list(Vessels_by_FS_temp$FS,Vessels_by_FS_temp$Year),FUN="sum")
colnames(Vessels_by_FS_temp)=c("FS","year","VesselsFS")

AER_data_temp=merge(AER_data_temp,Vessels_by_FS_temp,by =c("year"),all=T)


#AER_data_temp[AER_data_temp$year %in% c(2005:2007),]$avgrepcosts=AER_data_temp[AER_data_temp$year == 2008,]$avgrepcosts
AER_data_temp[is.na(AER_data_temp$avgrepcosts),]$avgrepcosts<-mean(AER_data_temp[AER_data_temp$year %in% c(2014:2016),]$avgrepcosts)

#AER_data_temp[is.na(AER_data_temp$VesselsFS),]$VesselsFS<-AER_data_temp[AER_data_temp$year %in% c(2015),]$VesselsFS


AER_data_temp$Est_repcost=AER_data_temp$VesselsFS
AER_data_temp$Est_repcost=AER_data_temp$VesselsFS *AER_data_temp$avgrepcosts


for (rr in 1:nrow(AER_data_temp)){
  
  year=AER_data_temp$year[rr] 
  DF1[ty,colnames(DF1)==year]=AER_data_temp$Est_repcost[rr]
  
}


ty=6 # totnovarcost
Vessels_by_FS=read.table("Vessels.csv",sep=";",header=T)
Vessels_by_FS_temp=Vessels_by_FS[Vessels_by_FS$FS==Fleet_segments[fs] & Vessels_by_FS$Year %in% ts,] 

AER_data_temp=AER_data_temp1[AER_data_temp1$variable_code==types_costs[ty],] 

Vessels=AER_data_temp1[AER_data_temp1$variable_code=="totves",] 

#unique(AER_data_temp1$variable_code)

AER_data_temp=AER_data_temp[as.character(AER_data_temp$value)!="NULL",]
Vessels=Vessels[as.character(Vessels$value)!="NULL",]

AER_data_temp=aggregate(as.numeric(AER_data_temp$value),by=list(AER_data_temp$FS,AER_data_temp$year),FUN="sum")
Vessels=aggregate(as.numeric(Vessels$value),by=list(Vessels$FS,Vessels$year),FUN="sum")

Vessels=Vessels[Vessels$Group.2 <trans_year[1],]

AER_data_temp$Vessels=Vessels$x 
AER_data_temp$avgfixcosts=AER_data_temp$x/Vessels$x

colnames(AER_data_temp)[1:3]=c("FS","year","totfixcost")

Vessels_by_FS_temp=aggregate(as.numeric(Vessels_by_FS_temp$Vess),by=list(Vessels_by_FS_temp$FS,Vessels_by_FS_temp$Year),FUN="sum")
colnames(Vessels_by_FS_temp)=c("FS","year","VesselsFS")

AER_data_temp=merge(AER_data_temp,Vessels_by_FS_temp,by =c("year"),all=T)


AER_data_temp[is.na(AER_data_temp$avgfixcosts),]$avgfixcosts<-mean(AER_data_temp[AER_data_temp$year %in% c(2014:2016),]$avgfixcosts)

#AER_data_temp[is.na(AER_data_temp$VesselsFS),]$VesselsFS<-AER_data_temp[AER_data_temp$year %in% c(2015),]$VesselsFS

AER_data_temp$Est_fixcost=AER_data_temp$VesselsFS
AER_data_temp$Est_fixcost=  AER_data_temp$VesselsFS *AER_data_temp$avgfixcosts


for (rr in 1:nrow(AER_data_temp)){
  
  year=AER_data_temp$year[rr] 
  DF1[ty,colnames(DF1)==year]=AER_data_temp$Est_fixcost[rr]
  
}

DF1[10,]=DF1[6,]

# totcrewwage

ty=11 # totcrewwage

AER_data_temp=AER_data_temp1[AER_data_temp1$variable_code==types_costs[ty],] 

Vessels=AER_data_temp1[AER_data_temp1$variable_code=="totves",] 

#unique(AER_data_temp1$variable_code)

AER_data_temp=AER_data_temp[as.character(AER_data_temp$value)!="NULL",]
Vessels=Vessels[as.character(Vessels$value)!="NULL",]

AER_data_temp=aggregate(as.numeric(AER_data_temp$value),by=list(AER_data_temp$FS,AER_data_temp$year),FUN="sum")
Vessels=aggregate(as.numeric(Vessels$value),by=list(Vessels$FS,Vessels$year),FUN="sum")

Vessels=Vessels[Vessels$Group.2 <trans_year[1],]

AER_data_temp$Vessels=Vessels$x 
AER_data_temp$avgempl=AER_data_temp$x/Vessels$x

colnames(AER_data_temp)[1:3]=c("FS","year","totcrewwage")

Vessels_by_FS_temp=aggregate(as.numeric(Vessels_by_FS_temp$VesselsFS),by=list(Vessels_by_FS_temp$FS,Vessels_by_FS_temp$year),FUN="sum")
colnames(Vessels_by_FS_temp)=c("FS","year","VesselsFS")

AER_data_temp=merge(AER_data_temp,Vessels_by_FS_temp,by =c("year"),all=T)

AER_data_temp[is.na(AER_data_temp$avgempl),]$avgempl<-mean(AER_data_temp[AER_data_temp$year %in% c(2014:2016),]$avgempl)

#AER_data_temp[is.na(AER_data_temp$VesselsFS),]$VesselsFS<-AER_data_temp[AER_data_temp$year %in% c(2015),]$VesselsFS


AER_data_temp$Est_crew=AER_data_temp$VesselsFS *AER_data_temp$avgempl


for (rr in 1:nrow(AER_data_temp)){
  
  year=AER_data_temp$year[rr] 
  DF1[ty,colnames(DF1)==year]=AER_data_temp$Est_crew[rr]
  
}


# "totdepcost" = depreciation costs
ty=12

AER_data_temp=AER_data_temp1[AER_data_temp1$variable_code==types_costs[ty],] 

Vessels=AER_data_temp1[AER_data_temp1$variable_code=="totves",] 

AER_data_temp=AER_data_temp[as.character(AER_data_temp$value)!="NULL",]
Vessels=Vessels[as.character(Vessels$value)!="NULL",]

AER_data_temp=aggregate(as.numeric(AER_data_temp$value),by=list(AER_data_temp$FS,AER_data_temp$year),FUN="sum")
Vessels=aggregate(as.numeric(Vessels$value),by=list(Vessels$FS,Vessels$year),FUN="sum")

Vessels=Vessels[Vessels$Group.2 <trans_year[1],]

AER_data_temp$Vessels=Vessels$x 
AER_data_temp$avgdep=AER_data_temp$x/Vessels$x

colnames(AER_data_temp)[1:3]=c("FS","year","totdeprep")

Vessels_by_FS_temp=aggregate(as.numeric(Vessels_by_FS_temp$VesselsFS),by=list(Vessels_by_FS_temp$FS,Vessels_by_FS_temp$year),FUN="sum")
colnames(Vessels_by_FS_temp)=c("FS","year","VesselsFS")

AER_data_temp=merge(AER_data_temp,Vessels_by_FS_temp,by =c("year"),all=T)

#AER_data_temp[AER_data_temp$year %in% c(2005:2007),]$avgdep=AER_data_temp[AER_data_temp$year == 2008,]$avgdep
AER_data_temp[is.na(AER_data_temp$avgdep),]$avgdep<-mean(AER_data_temp[AER_data_temp$year %in% c(2014:2016),]$avgdep)

AER_data_temp$Est_dep=AER_data_temp$VesselsFS *AER_data_temp$avgdep


for (rr in 1:nrow(AER_data_temp)){
  
  year=AER_data_temp$year[rr] 
  DF1[ty,colnames(DF1)==year]=AER_data_temp$Est_dep[rr]
  
}

ty=15 #   "tototherinc" 
DF1[ty,]=0

ty=16 #"totjob"

AER_data_temp=AER_data_temp1[AER_data_temp1$variable_code==types_costs[ty],] 

Vessels=AER_data_temp1[AER_data_temp1$variable_code=="totves",] 

AER_data_temp=AER_data_temp[as.character(AER_data_temp$value)!="NULL",]
Vessels=Vessels[as.character(Vessels$value)!="NULL",]

AER_data_temp=aggregate(as.numeric(AER_data_temp$value),by=list(AER_data_temp$FS,AER_data_temp$year),FUN="sum")
Vessels=aggregate(as.numeric(Vessels$value),by=list(Vessels$FS,Vessels$year),FUN="sum")

Vessels=Vessels[Vessels$Group.2 <trans_year[1],]

AER_data_temp$Vessels=Vessels$x 
AER_data_temp$avgjob=AER_data_temp$x/Vessels$x

colnames(AER_data_temp)[1:3]=c("FS","year","totinvest")

Vessels_by_FS_temp=aggregate(as.numeric(Vessels_by_FS_temp$VesselsFS),by=list(Vessels_by_FS_temp$FS,Vessels_by_FS_temp$year),FUN="sum")
colnames(Vessels_by_FS_temp)=c("FS","year","VesselsFS")

AER_data_temp=merge(AER_data_temp,Vessels_by_FS_temp,by =c("year"),all=T)

#AER_data_temp[AER_data_temp$year %in% c(2005:2007),]$avgjob=AER_data_temp[AER_data_temp$year == 2008,]$avgjob
AER_data_temp[is.na(AER_data_temp$avgjob),]$avgjob<-mean(AER_data_temp[AER_data_temp$year %in% c(2014:2016),]$avgjob)

AER_data_temp$Est_job=AER_data_temp$VesselsFS *AER_data_temp$avgjob


for (rr in 1:nrow(AER_data_temp)){
  
  year=AER_data_temp$year[rr] 
  DF1[ty,colnames(DF1)==year]=AER_data_temp$Est_job[rr]
  
}


#}


ty=17 # capital value
AER_data_temp=AER_data_temp1[AER_data_temp1$variable_code==types_costs[ty],] 

Vessels=AER_data_temp1[AER_data_temp1$variable_code=="totves",] 

AER_data_temp=AER_data_temp[as.character(AER_data_temp$value)!="NULL",]
Vessels=Vessels[as.character(Vessels$value)!="NULL",]

AER_data_temp=aggregate(as.numeric(AER_data_temp$value),by=list(AER_data_temp$FS,AER_data_temp$year),FUN="sum")
Vessels=aggregate(as.numeric(Vessels$value),by=list(Vessels$FS,Vessels$year),FUN="sum")

Vessels=Vessels[Vessels$Group.2 <trans_year[1],]

AER_data_temp$Vessels=Vessels$x 
AER_data_temp$avginvest=AER_data_temp$x/Vessels$x

colnames(AER_data_temp)[1:3]=c("FS","year","totinvest")

Vessels_by_FS_temp=aggregate(as.numeric(Vessels_by_FS_temp$VesselsFS),by=list(Vessels_by_FS_temp$FS,Vessels_by_FS_temp$year),FUN="sum")
colnames(Vessels_by_FS_temp)=c("FS","year","VesselsFS")

AER_data_temp=merge(AER_data_temp,Vessels_by_FS_temp,by =c("year"),all=T)

#AER_data_temp[AER_data_temp$year %in% c(2005:2007),]$avginvest=AER_data_temp[AER_data_temp$year == 2008,]$avginvest
AER_data_temp[is.na(AER_data_temp$avginvest),]$avginvest<-mean(AER_data_temp[AER_data_temp$year %in% c(2014:2016),]$avginvest)

AER_data_temp$Est_invest=AER_data_temp$VesselsFS *AER_data_temp$avginvest

for (rr in 1:nrow(AER_data_temp)){
  
  year=AER_data_temp$year[rr] 
  DF1[ty,colnames(DF1)==year]=AER_data_temp$Est_invest[rr]
  
}



ty=13 # opportunity costs= capital value*(1+interest rate)/(1+inflation rate) -1

intr=c(3.258333333,4.07,3.7125,0.995833333, 1.693333333, 3.035,2.663333333,1.169166667,0.386416667, 0.054166667,  -0.1575, -0.341666667, -0.375, -0.411666667)/100
#c(3.43,3.43,	3.43,	3.43,	1.43,	1.34,	1.25,	0.75,	0.375,	0.1,	0.05,	0.05,	0.05,	0.05,	0.05)/100
inflr= c(3.52,
         2.79,
         4.08,
         -0.29,
         1.80,
         3.20,
         2.45,
         1.41,
         -0.15,
         -0.50,
         -0.20,
         1.96,
         1.68,
         0.70)/100
#  c(3.125,3.125,	3.125,	3.125,	1.23,	2.065,	2.9,	3.1,	0.9,	0.25,	0.1,	0.2,	0.3,	0.3,	0.4)/100

for (rr in 1:nrow(AER_data_temp)){
  
  year=AER_data_temp$year[rr] 
  DF1[ty,colnames(DF1)==year]=AER_data_temp$Est_invest[rr]*(1+intr[rr])/(1+inflr[rr]) -1
  
}


ty=14
DF1[ty,]=DF1[12,] +DF1[13,]


#DF1[ty,]=DF1[12,]

DF1=rbind(DF0,DF1)

DF1[,colnames(DF1) %in% trans_year]= DF1[,as.numeric(colnames(DF1)) == (trans_year[1]-1)]

write.table(DF1,paste(getwd(),"\\BMT_INPUT\\TS_",Fleet_segments[fs],".csv",sep=""),sep=";",row.names=F)      
 }



data1=read.table(paste(getwd(),"\\BMT_INPUT\\TS_",Fleet_segments_ALL[1],".csv",sep=""),sep=";",header=T)  
for (fs in 2:length(Fleet_segments_ALL)){
data2=read.table(paste(getwd(),"\\BMT_INPUT\\TS_",Fleet_segments_ALL[fs],".csv",sep=""),sep=";",header=T) 

data1=cbind(data1,data2)

} 
colnames(data1)=rep(ts,length(Fleet_segments_ALL))


SecondRow=t(data.frame(rep(Fleet_segments_ALL[1],length(ts)))  )

for (fs in Fleet_segments_ALL[-1]){
  print(fs)
  SecondRow2=t(data.frame(rep(fs,length(ts))))
  
  SecondRow=cbind(SecondRow,SecondRow2) 
  
}

empty_row=t(data.frame(rep("",length(SecondRow))))

DF_final=(rbind(SecondRow,empty_row,empty_row))

DF_final=rbind(DF_final,as.matrix(data1))

DF_final= cbind(FirstColumn,DF_final)

colnames(DF_final)[1]=""

write.table(DF_final,paste(getwd(),"\\BMT_INPUT\\TIME_SERIES2.csv",sep=""),sep=";",row.names=F)      



