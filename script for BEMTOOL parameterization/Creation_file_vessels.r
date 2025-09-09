# Script to create Vessels files by month for BMT

Fleet_segments <-c("GSA6_DTS_VL0612","GSA6_DTS_VL1218","GSA6_DTS_VL1824","GSA6_DTS_VL2440","GSA6_PGP_VL0018","GSA6_HOK_VL0624")

setwd("C:\\IMPLEMED_GSA6")

dir.create("BMT_INPUT")
path_input="C:\\IMPLEMED_GSA6"

ts<-c(2006:2020)

trans_year=c(2020)

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

for (f in 1:length(Fleet_segments)){
  print(Fleet_segments[f])
  ini=1 +(f-1)*length(ts)
  fin=(f)*length(ts)
  if (length(DF1[,ini:fin][which(DF1[,ini:fin]==0)])>0){
  DF1[,ini:fin][which(DF1[,ini:fin]==0)] = mean(DF1[,ini:fin][which(DF1[,ini:fin]!=0)]) 
  }
}

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

DF_final=rbind(DF_final,as.matrix(DF1))

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
colnames(DF_final)[1]="Units: number"
write.table(DF_final,paste(getwd(),"\\BMT_INPUT\\Vessels.csv",sep=""),sep=";",row.names=F)                    


