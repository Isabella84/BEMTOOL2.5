# Script to create Landing files by month for BMT
spe <-"DPS"
Fleet_segments <-c("GSA6_DTS_VL0612","GSA6_DTS_VL1218","GSA6_DTS_VL1824","GSA6_DTS_VL2440","GSA6_PGP_VL0018","GSA6_HOK_VL0624")

setwd("C:\\IMPLEMED_GSA6")

dir.create("BMT_INPUT")
path_input="C:\\IMPLEMED_GSA6"

ts<-c(2006:2020)


trans_year=c(2020)

#LANDINGS
Landing_data=read.table(paste(path_input,"\\Landings_DPS.csv",sep=""),sep=";", header=T)

Landing_data=Landing_data[Landing_data$Year %in% ts & Landing_data$FS %in% Fleet_segments,]


DF1=matrix(0,nrow=12,ncol=length(ts))
colnames(DF1)=ts
rownames(DF1)=seq(1,12,1)

# Fleet_segments[1]
Landing_data_temp=Landing_data[Landing_data$FS==Fleet_segments[1],]

for (rr in 1:nrow(Landing_data_temp)){
  
 year=Landing_data_temp$Year[rr] 
 month=Landing_data_temp$Month[rr]
 unit=Landing_data_temp$Unit[rr]
 
 if (month!=0){
 if (unit=="tons"){
 DF1[rownames(DF1)==month,colnames(DF1)==year]=Landing_data_temp$Land[rr]*1000
 } else if (unit=="kg"){
   DF1[rownames(DF1)==month,colnames(DF1)==year]=Landing_data_temp$Land[rr]  
 }
  
 } else if (month==0){
   
   if (unit=="tons"){
     DF1[,colnames(DF1)==year]=Landing_data_temp$Land[rr]*1000/12
   } else if (unit=="kg"){
     DF1[,colnames(DF1)==year]=Landing_data_temp$Land[rr]/12  
   }  
   
 }
 
  
}


DF1[,colnames(DF1) %in% trans_year]= DF1[,as.numeric(colnames(DF1)) == (trans_year[1]-1)]

for (fs in Fleet_segments[-1]){

print(fs,quote=F)
DF_next=matrix(0,nrow=12,ncol=length(ts))
colnames(DF_next)=ts
rownames(DF_next)=seq(1,12,1)

# Fleet_segments[1]
Landing_data_temp=Landing_data[Landing_data$FS==fs,]

for (rr in 1:nrow(Landing_data_temp)){
  
  year=Landing_data_temp$Year[rr] 
  month=Landing_data_temp$Month[rr]
  unit=Landing_data_temp$Unit[rr]
  
  if (month!=0){
    if (unit=="tons"){
      DF_next[rownames(DF_next)==month,colnames(DF_next)==year]=Landing_data_temp$Land[rr]*1000
    } else if (unit=="kg"){
      DF_next[rownames(DF_next)==month,colnames(DF_next)==year]=Landing_data_temp$Land[rr]  
    }
    
  } else if (month==0){
    
    if (unit=="tons"){
      DF_next[,colnames(DF_next)==year]=Landing_data_temp$Land[rr]*1000/12
    } else if (unit=="kg"){
      DF_next[,colnames(DF_next)==year]=Landing_data_temp$Land[rr]/12  
    }  
    
  }
  
  
  
}
DF_next[,colnames(DF_next) %in% trans_year]= DF_next[,as.numeric(colnames(DF_next)) == (trans_year[1]-1)]

DF1=cbind(DF1,DF_next)

} # ciclo FS


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
colnames(DF_final)[1]="Units: kg"
write.table(DF_final,paste(getwd(),"\\BMT_INPUT\\Landing_",spe,".csv",sep=""),sep=";",row.names=F)                    


