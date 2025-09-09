# Script to create GT files by month for BMT

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

setwd("C:\\Users\\Utente\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\FAIRSEA\\Pilot action ITALY\\Parametrizzazione bio-press")

dir.create("BMT_INPUT")
path_input="C:\\Users\\Utente\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\FAIRSEA\\Pilot action ITALY\\Parametrizzazione bio-press\\Solea input"

ts<-c(2005:2020)


trans_year=c(2019,2020)

#GT
GT_data=read.table(paste(path_input,"\\GT.csv",sep=""),sep=";", header=T)

GT_data=GT_data[GT_data$Year %in% ts & GT_data$FS %in% Fleet_segments,]


DF1=matrix(0,nrow=12,ncol=length(ts))
colnames(DF1)=ts
rownames(DF1)=seq(1,12,1)

# Fleet_segments[1]
GT_data_temp=GT_data[GT_data$FS==Fleet_segments[1],]

for (rr in 1:nrow(GT_data_temp)){
  
 year=GT_data_temp$Year[rr] 
 month=GT_data_temp$Month[rr]
 #unit=GT_data_temp$Unit[rr]
 
 if (month!=0){
 DF1[rownames(DF1)==month,colnames(DF1)==year]=GT_data_temp$GT[rr]
 } else if (month==0){
     DF1[,colnames(DF1)==year]=GT_data_temp$GT[rr]
   }  
   
 }
 
DF1[,colnames(DF1) %in% trans_year]= DF1[,as.numeric(colnames(DF1)) == (trans_year[1]-1)]

for (fs in Fleet_segments[-1]){

print(fs,quote=F)
DF_next=matrix(0,nrow=12,ncol=length(ts))
colnames(DF_next)=ts
rownames(DF_next)=seq(1,12,1)

# Fleet_segments[1]
GT_data_temp=GT_data[GT_data$FS==fs,]

for (rr in 1:nrow(GT_data_temp)){
  
  year=GT_data_temp$Year[rr] 
  month=GT_data_temp$Month[rr]
  unit=GT_data_temp$Unit[rr]
  
  if (month!=0){
      DF_next[rownames(DF_next)==month,colnames(DF_next)==year]=GT_data_temp$GT[rr]  
    
  } else if (month==0){
    
        DF_next[,colnames(DF_next)==year]=GT_data_temp$GT[rr]  
    }  
    
}

DF_next[,colnames(DF_next) %in% trans_year]= DF_next[,as.numeric(colnames(DF_next)) == (trans_year[1]-1)]

DF1=cbind(DF1,DF_next)

} # ciclo FS

GT=DF1

#VESSELS
Vess_data=read.table(paste(path_input,"\\Vessels.csv",sep=""),sep=";", header=T)

Vess_data=Vess_data[Vess_data$Year %in% ts & Vess_data$FS %in% Fleet_segments,]


DF1=matrix(0,nrow=12,ncol=length(ts))
colnames(DF1)=ts
rownames(DF1)=seq(1,12,1)

# Fleet_segments[1]
Vess_data_temp=Vess_data[Vess_data$FS==Fleet_segments[1],]

for (rr in 1:nrow(Vess_data_temp)){
  
  year=Vess_data_temp$Year[rr] 
  month=Vess_data_temp$Month[rr]
  #unit=Vess_data_temp$Unit[rr]
  
  if (month!=0){
    DF1[rownames(DF1)==month,colnames(DF1)==year]=Vess_data_temp$Vess[rr]
  } else if (month==0){
    DF1[,colnames(DF1)==year]=Vess_data_temp$Vess[rr]
  }  
  
}



DF1[,colnames(DF1) %in% trans_year]= DF1[,as.numeric(colnames(DF1)) == (trans_year[1]-1)]

for (fs in Fleet_segments[-1]){
  
  print(fs,quote=F)
  DF_next=matrix(0,nrow=12,ncol=length(ts))
  colnames(DF_next)=ts
  rownames(DF_next)=seq(1,12,1)
  
  # Fleet_segments[1]
  Vess_data_temp=Vess_data[Vess_data$FS==fs,]
  
  for (rr in 1:nrow(Vess_data_temp)){
    
    year=Vess_data_temp$Year[rr] 
    month=Vess_data_temp$Month[rr]
    unit=Vess_data_temp$Unit[rr]
    
    if (month!=0){
      DF_next[rownames(DF_next)==month,colnames(DF_next)==year]=Vess_data_temp$Vess[rr]  
      
    } else if (month==0){
      
      DF_next[,colnames(DF_next)==year]=Vess_data_temp$Vess[rr]  
    }  
    
  }
  DF_next[,colnames(DF_next) %in% trans_year]= DF_next[,as.numeric(colnames(DF_next)) == (trans_year[1]-1)]
  
  DF1=cbind(DF1,DF_next)
  
} # ciclo FS


Vess=DF1


Avg_GT=GT/Vess

for (f in 1:length(Fleet_segments)){
ini=1+(f-1)*length(ts)
fin=(f)*length(ts)
if (length(Vess[,ini:fin][which(Vess[,ini:fin]==0)])>0){
Avg_GT[,ini:fin][Vess[,ini:fin]==0] = mean(Avg_GT[,ini:fin][Vess[,ini:fin]!=0],na.rm=T )  
}
}

# 
# for (f in 1:length(Fleet_segments)){
#   print(Fleet_segments[f])
#   ini=1 +(f-1)*length(ts)
#   fin=(f)*length(ts)
#   if (length(DF1[,ini:fin][which(DF1[,ini:fin]==0)])>0){
#     DF1[,ini:fin][which(DF1[,ini:fin]==0)] = mean(DF1[,ini:fin][which(DF1[,ini:fin]!=0)]) 
#   }
# }

#FirstRow=t(data.frame(rep(ts,length(Fleet_segments))))
#FirstRow=c("Units: kg",FirstRow)

SecondRow=t(data.frame(rep(Fleet_segments[1],length(ts)))  )

for (fs in Fleet_segments[-1]){
  print(fs)
  SecondRow2=t(data.frame(rep(fs,length(ts))))
  
  SecondRow=cbind(SecondRow,SecondRow2) 
 
}

empty_row=t(data.frame(rep("",length(SecondRow))))

DF_final=(rbind(SecondRow,empty_row,empty_row))

DF_final=rbind(DF_final,as.matrix(Avg_GT))

DF_final= cbind(c(
                       "casestudy.fleetsegmentcode",
                       "casestudy.fishingtechnique",
                       "casestudy.loa",
                       "casestudy.month1",
                       "casestudy.month2",
                       "casestudy.month3",
                       "casestudy.month4",
                       "casestudy.month5",
                       "casestudy.month6",
                       "casestudy.month7",
                       "casestudy.month8",
                       "casestudy.month9",
                       "casestudy.month10",
                       "casestudy.month11",
                       "casestudy.month12"),DF_final)

colnames(DF_final)[1]="Units: GT"

write.table(DF_final,paste(getwd(),"\\BMT_INPUT\\GT.csv",sep=""),sep=";",row.names=F)      







