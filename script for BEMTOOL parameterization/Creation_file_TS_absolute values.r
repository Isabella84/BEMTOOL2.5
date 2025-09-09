# Script to create economic time series file for BMT

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

FS_ready <-c("HRV_DFN",
             "SVN_DFN")

setwd("C:\\Users\\Utente\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\FAIRSEA\\Pilot action ITALY\\Parametrizzazione economica")

dir.create("BMT_INPUT")
path_input= getwd()

ts<-c(2005:2020)


trans_year=c(2019,2020)


AER_data=read.table(paste(path_input,"\\AER.csv",sep=""),sep=";", header=T)

AER_data=AER_data[AER_data$year %in% ts & as.character(AER_data$FS) %in% FS_ready,]


FDI_data=read.table(paste(path_input,"\\FDI_data.csv",sep=""),sep=";", header=T)

FDI_data=FDI_data[FDI_data$year %in% ts & as.character(FDI_data$FS) %in% FS_ready,]

FirstColumn= c("casestudy.maxavgseadays",
             "casestudy.totallandings", # DA FDI
             "casestudy.revenues.S1",# DA FDI
             "casestudy.totalrevenues",# DA FDI
             "casestudy.revenues.discard.S1",
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

stks=c("SOL")
nb_stk=length(stks)

DF0=matrix(0,nrow=(3+2*nb_stk),ncol=length(ts))
colnames(DF0)=ts

AER_data_temp1=AER_data[AER_data$FS==FS_ready[1],]

ty=1

#maxseadays
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


ty=2
#total landing
FDI_data_temp=FDI_data[FDI_data$FS==FS_ready[1],] 

if (nrow(AER_data_temp)>0){
  
  FDI_data_temp=aggregate(as.numeric(FDI_data_temp$totwghtlandg),by=list(FDI_data_temp$FS,FDI_data_temp$year),FUN="sum")
  
  colnames(FDI_data_temp)=c("FS","year","value")
  
  for (rr in 1:nrow(FDI_data_temp)){
    
    year=FDI_data_temp$year[rr] 
    DF0[ty,colnames(DF0)==year]=FDI_data_temp$value[rr]
    
  }
  
} else {
  DF0[ty,]=0  
  
}



for (st in 1:nb_stk) {
ty=3 + (st-1) 
#revenues by stock
FDI_data_temp=FDI_data[FDI_data$FS==FS_ready[1] & FDI_data$species==stks[st],] 

if (nrow(FDI_data_temp)>0){
  #AER_data_temp=AER_data_temp[as.character(AER_data_temp$value)!="NULL",]
  
  FDI_data_temp=aggregate(as.numeric(FDI_data_temp$totvallandg),by=list(FDI_data_temp$FS,FDI_data_temp$year),FUN="sum")
  
  colnames(FDI_data_temp)=c("FS","year","value")
  
  for (rr in 1:nrow(FDI_data_temp)){
    
    year=FDI_data_temp$year[rr] 
    DF0[ty,colnames(DF0)==year]=FDI_data_temp$value[rr]
    
  }
  
} else {
  DF0[ty,]=0  
  
}

}


  ty=3 + st
  #revenues by stock
  FDI_data_temp=FDI_data[FDI_data$FS==FS_ready[1] ,] 
  
  if (nrow(AER_data_temp)>0){
    
    FDI_data_temp=aggregate(as.numeric(FDI_data_temp$totvallandg),by=list(FDI_data_temp$FS,FDI_data_temp$year),FUN="sum")
    
    colnames(FDI_data_temp)=c("FS","year","value")
    
    for (rr in 1:nrow(FDI_data_temp)){
      
      year=FDI_data_temp$year[rr] 
      DF0[ty,colnames(DF0)==year]=FDI_data_temp$value[rr]
      
    }
    
  } else {
    DF0[ty,]=0  
    
  }
  


types_costs=c("totenercost","commercialcost","totvarcost","tottotvarcost","totrepcost","totnovarcost","essentialcosts","avoidablemaintenancecosts","unavoidablemaintenancecosts","totnovarcost","totcrewwage","totdepcost","totopportunity","totcapcost","tototherinc","totjob","totdeprep") 

DF1=matrix(0,nrow=length(types_costs),ncol=length(ts))
colnames(DF1)=ts

#rownames(DF1)=seq(1,12,1)

# Fleet_segments[1]

for (ty in 1:length(types_costs)){
  
  print(ty)
 
  
AER_data_temp=AER_data_temp1[AER_data_temp1$variable_code==types_costs[ty],] 

if (nrow(AER_data_temp)>0){
AER_data_temp=AER_data_temp[as.character(AER_data_temp$value)!="NULL",]

AER_data_temp=aggregate(as.numeric(AER_data_temp$value),by=list(AER_data_temp$FS,AER_data_temp$year),FUN="sum")

colnames(AER_data_temp)=c("FS","year","value")

for (rr in 1:nrow(AER_data_temp)){
  
 year=AER_data_temp$year[rr] 
 DF1[ty,colnames(DF1)==year]=AER_data_temp$value[rr]
   
}

} else {
  DF1[ty,]=0  
  
}


  
}
DF1[4,]=DF1[3,]+DF1[1,]

# if (ty==13) { # opportunity costs= capital value*(1+interest rate)/(1+inflation rate) -1
#interest 
   intr=c(7.8, 5.8,4.9, 4.3, 8.3, 9.4, 7.9, 7.9, 8.4, 8.4, 8, 7.6, 7.8, 8, 7.6)/100
  
    # inflation https://www.macrotrends.net/countries/HRV/croatia/inflation-rate-cpi#:~:text=Croatia%20inflation%20rate%20for%202019,a%202.25%25%20increase%20from%202016.
   
   
   # interest rate https://data.worldbank.org/indicator/FR.INR.RINR?locations=HR
   # + un pdf scaricato 1HRVEA2019001.pdf
   inflr=c(3.32, 3.19, 2.9, 6.08, 2.38, 1.03, 2.27, 3.41, 2.22, -0.22, -0.46, -1.12, 1.13, 1.5,0.77)/100 
   
   for (rr in 1:length(c(2005:2018))){
   DF1[13,rr]=DF1[17,rr]*(1+intr[rr])/(1+inflr[rr]) - 1
   }
   
 DF1[14,]=DF1[12,] +DF1[13,]   
#   #  
#   
#   #   year=AER_data_temp$year[rr] 
#   #  
#   
#   
#   
#   
#   
#   
# }
# 
# if (ty==14) {

#   
# }

DF1=rbind(DF0,DF1)

DF1[,colnames(DF1) %in% trans_year]= DF1[,as.numeric(colnames(DF1)) == (trans_year[1]-1)]

write.table(DF1,paste(getwd(),"\\BMT_INPUT\\TS_",FS_ready[1],".csv",sep=""),sep=";",row.names=F)      


#---------------------------OTHER FS---------------------------------

for (fs in FS_ready[-1]){

  DF0=matrix(0,nrow=(3+2*nb_stk),ncol=length(ts))
  colnames(DF0)=ts
  
  AER_data_temp1=AER_data[AER_data$FS==fs,]
  
  ty=1
  
  #maxseadays
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
  
  
  ty=2
  #total landing
  FDI_data_temp=FDI_data[FDI_data$FS==fs,] 
  
  if (nrow(AER_data_temp)>0){
    #AER_data_temp=AER_data_temp[as.character(AER_data_temp$value)!="NULL",]
    
    FDI_data_temp=aggregate(as.numeric(FDI_data_temp$totwghtlandg),by=list(FDI_data_temp$FS,FDI_data_temp$year),FUN="sum")
    
    colnames(FDI_data_temp)=c("FS","year","value")
    
    for (rr in 1:nrow(FDI_data_temp)){
      
      year=FDI_data_temp$year[rr] 
      DF0[ty,colnames(DF0)==year]=FDI_data_temp$value[rr]
      
    }
    
  } else {
    DF0[ty,]=0  
    
  }
  
  
  
  for (st in 1:nb_stk) {
    ty=3 + (st-1) 
    #revenues by stock
    FDI_data_temp=FDI_data[FDI_data$FS==fs & FDI_data$species==stks[st],] 
    
    if (nrow(FDI_data_temp)>0){
      #AER_data_temp=AER_data_temp[as.character(AER_data_temp$value)!="NULL",]
      
      FDI_data_temp=aggregate(as.numeric(FDI_data_temp$totvallandg),by=list(FDI_data_temp$FS,FDI_data_temp$year),FUN="sum")
      
      colnames(FDI_data_temp)=c("FS","year","value")
      
      for (rr in 1:nrow(FDI_data_temp)){
        
        year=FDI_data_temp$year[rr] 
        DF0[ty,colnames(DF0)==year]=FDI_data_temp$value[rr]
        
      }
      
    } else {
      DF0[ty,]=0  
      
    }
    
  }
  
  
  ty=3 + st
  #revenues by stock
  FDI_data_temp=FDI_data[FDI_data$FS==fs,] 
  
  if (nrow(FDI_data_temp)>0){
    
    FDI_data_temp=aggregate(as.numeric(FDI_data_temp$totvallandg),by=list(FDI_data_temp$FS,FDI_data_temp$year),FUN="sum")
    
    colnames(FDI_data_temp)=c("FS","year","value")
    
    for (rr in 1:nrow(FDI_data_temp)){
      
      year=FDI_data_temp$year[rr] 
      DF0[ty,colnames(DF0)==year]=FDI_data_temp$value[rr]
      
    }
    
  } else {
    DF0[ty,]=0  
    
  }
  
  
  # VERIFICARE SE "totinvest" sono gli opportunity costs!!!!
  # e se "totdeprep" sono i depreciation costs!!
  
  types_costs=c("totenercost","commercialcost","totvarcost","totvarcost","totrepcost","totnovarcost","essentialcosts","avoidablemaintenancecosts","unavoidablemaintenancecosts","totnovarcost","totcrewwage","totdepcost","totopportunity","totcapcost","tototherinc","totjob","totdeprep") 
  
  DF1=matrix(0,nrow=length(types_costs),ncol=length(ts))
  colnames(DF1)=ts
  
  #rownames(DF1)=seq(1,12,1)
  
  # Fleet_segments[1]
  
  for (ty in 1:length(types_costs)){
    
    print(ty)
    
  
    AER_data_temp=AER_data_temp1[AER_data_temp1$variable_code==types_costs[ty],] 
    
    if (nrow(AER_data_temp)>0){
      AER_data_temp=AER_data_temp[as.character(AER_data_temp$value)!="NULL",]
      
      AER_data_temp=aggregate(as.numeric(AER_data_temp$value),by=list(AER_data_temp$FS,AER_data_temp$year),FUN="sum")
      
      colnames(AER_data_temp)=c("FS","year","value")
      
      for (rr in 1:nrow(AER_data_temp)){
        
        year=AER_data_temp$year[rr] 
        DF1[ty,colnames(DF1)==year]=AER_data_temp$value[rr]
        
      }
      
    } else {
      DF1[ty,]=0  
      
    }
   
    
    
  }
  
  
  
  # if (ty==13) {# opportunity costs= capital value*(1+interest rate)/(1+inflation rate) -1
  #   
  #   if (Fleet_segments[])
  #   intr=c(3.43,3.43,	3.43,	3.43,	1.43,	1.34,	1.25,	0.75,	0.375,	0.1,	0.05,	0.05,	0.05,	0.05,	0.05)
  #   inflr=c(3.125,3.125,	3.125,	3.125,	1.23,	2.065,	2.9,	3.1,	0.9,	0.25,	0.1,	0.2,	0.3,	0.3,	0.4)
  #   
  #   #  for (rr in 1:nrow(AER_data_temp)){
  #   
  #   #   year=AER_data_temp$year[rr] 
  #   #  DF1[ty,colnames(DF1)==year]=AER_data_temp$Est_invest[rr]*(1+intr[rr])/(1+inflr[rr]) -1
  #   
  #   #}
  #   
  #   
  #   
  #   
  # }
  # 
  # if (ty==14) {
  #   DF1[ty,]=DF1[12,] +DF1[13,]
  #   
  # }
  # interest rate https://www.statista.com/statistics/916255/annual-average-interest-rate-on-new-residential-loans-in-slovenia/
  
  intr=c(5.83, 5.83, 6.5, 6.73, 4.45, 3.34, 3.77, 3.37, 3.2, 3.21, 2.53, 2.33, 2.5, 2.44, 2.35)/100
  
  # inflation https://www.macrotrends.net/countries/SVN/slovenia/inflation-rate-cpi#:~:text=Slovenia%20inflation%20rate%20for%202019,a%201.48%25%20increase%20from%202016.
  
  inflr=c(2.45,2.46,3.66, 5.65, 0.84, 1.80, 1.80, 2.60, 1.77,0.20, -0.53,0-0.05, 1.43, 1.74, 1.63)/100 
  for (rr in 1:length(c(2005:2018))){
    DF1[13,rr]=DF1[17,rr]*(1+intr[rr])/(1+inflr[rr]) - 1
  }
  DF1[14,]=DF1[12,] +DF1[13,]  
  
  
  DF1=rbind(DF0,DF1)
  
  DF1[,colnames(DF1) %in% trans_year]= DF1[,as.numeric(colnames(DF1)) == (trans_year[1]-1)]
  

  write.table(DF1,paste(getwd(),"\\BMT_INPUT\\TS_",fs,".csv",sep=""),sep=";",row.names=F)      
  
  
    
} # ciclo FS









