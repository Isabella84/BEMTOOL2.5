# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




# read production by month file
# Prod_month =  read.table(prod_data_name,sep=";",header=T)
Prod_month = prod_data
# eliminate seed
Prod_month = Prod_month[which(as.numeric(as.character(Prod_month$Month))!=0),]

# associate Year
Year_vec_temp = matrix(rep(1,12),ncol=1)

for (y in 2:((forecast-1)/12)) {
Year_vec = rbind(Year_vec_temp,matrix(rep(y,12),ncol=1))
Year_vec_temp = Year_vec
}

# add the column of the year
Year_vec_temp = c(rep(Year_vec_temp,nb_gears))
Prod_month$Year= Year_vec_temp 

Prod_month$Production = as.numeric(as.character(Prod_month$Production))

# production by year
Prod_year=aggregate(Prod_month$Production,by=list(Prod_month$Year),FUN="sum")
colnames(Prod_year) =c("Year","Production")

# set landing by year
if ((Prod_month$Unit[1]=="kg")|(Prod_month$Unit[1]=="Kg")){
TrueYield<- Prod_year$Production  /1000  #tons
} else if (Prod_month$Unit[1]=="tons"){
TrueYield<- Prod_year$Production 
} else {
print("The production data has to be entered in tons or in Kg",quote=F)
}      
     
Rtrue_month<- INP$Recruits[1:forecast] 
Recruitsdat <- meanWequals(INP$Recruits, forecast, INP$Time_slice)    # recruits by year
           # recruits by month

#Normalise the data to a 0,1 scale and then attempt to recreate the series (This is for the Plaice data).
Recruitsnorm <- Recruitsdat/max(Recruitsdat)      

Rminsrch<- INP$Cal_min
Rmaxsrch<-INP$Cal_max

#print(paste("fuori Recruitsnorm:", Recruitsnorm), quote=F)
#print(paste("fuori theta:", theta), quote=F)
print ("Calibration in progress...",quote=F)
system.time(calres<-optimize(load_calibration,c(Rminsrch,Rmaxsrch)))

#Control to see if optimum value is close to the ends of the search range
if (calres$minimum>0.98*Rmaxsrch) print("Please choose a higher Rmaxsrch",quote=F)
if (calres$minimum<1.02*Rminsrch) print("Please choose a lower Rminsrch",quote=F)

#-------
#Generate a time series plot of true recruits and predicted recruits
t_<-seq(1, forecast)
Rcalc<- (calres$minimum)*Rtrue_month/max(Rtrue_month) #* 10000 #Recruitsnorm


Y_max = max(max(Rcalc), max(Rtrue_month))
Y_min = min(min(Rcalc), min(Rtrue_month))
#print(paste("Y_min:", Y_min))
#print(paste("Y_max:", Y_max))
jpeg(file=RECRUITMENT_CAL_graph, width=30, height=10, bg="white", units="cm",res=200)  
par(mfrow=c(1,2))

plot(t_,Rcalc,type='l', col="blue", xlab="Time in years", ylab='Recruits number', main="Recruits calibration curve comparison",ylim=c(Y_min,Y_max))
lines(t_,Rtrue_month,type='l', col="green")
legend("topleft",c("Calculated", "Observed"), lty=c(1,1),col =c("blue","green"))  

#Distance measures mean percent error
#error1 = sum(abs((Rtrue_month-Rcalc)/Rtrue_month))*100/length(Rtrue_month)
#Generate a time series plot of true Catch and predicted Catch
INP$Recruits[1:(forecast)]<-Rcalc[1:(forecast)] 
#write.table(INP$Recruits[1:(forecast)],file="Tables/Recruitment_calibrated.csv",sep=";",row.names=F)
write.table(INP$Recruits[1:(forecast)],file=RECRUITMENT_CAL_table,sep=";",row.names=F)

RunModel(1,(forecast-1))

Yieldcalc<- sumWequals(SRO$Capture_biomass, forecast, INP$Time_slice) 
Yieldtrue<- TrueYield

Prod_month$Month <- as.character(Prod_month$Month)
Yieldtrue_month = aggregate(Prod_month$Production,by=list(Prod_month$Month),FUN="sum") 

if (Prod_month$Unit[1]=="kg"|Prod_month$Unit[1]=="Kg")  {
Yieldtrue_month[,2] <- Yieldtrue_month[,2]/1000    #**** tons
} else if (Prod_month$Unit[1]=="tons") {
Yieldtrue_month = aggregate(Prod_month$Production,by=list(Prod_month$Month),FUN="sum")
} else {
print("The production data has to be entered in tons or in Kg",quote=F)
}

Yieldcalc_month = SRO$Capture_biomass[-1]                     
Yieldcalc_month = Yieldcalc_month[1:(forecast-1)]

Y_max = max(max(Yieldcalc_month), max(Yieldtrue_month[,2]))
Y_min = min(min(Yieldcalc_month), min(Yieldtrue_month[,2]))
t_<-c(seq(1, (length(Yieldcalc_month))))

#windows()
plot(t_,Yieldcalc_month,type='l', col="blue", xlab="Time in years", ylab='Yield (tonnes)', main="Yield curve comparison (by month)",ylim=c(Y_min,Y_max))
lines(t_,Yieldtrue_month[,2],type='l', col="green")
legend("topleft",c("Calculated", "Observed"), lty=c(1,1),col =c("blue","green"))  
dev.off()

#Distance measures mean percent error
error=sum(abs((Yieldtrue-Yieldcalc)/Yieldtrue))*100/length(Yieldtrue)

print(paste("Simulation with calibration executed (error on yield = ",round(error,2),"%)",sep=""),quote=FALSE)
