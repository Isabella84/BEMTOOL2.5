# BEMTOOL - Bio-Economic Model TOOLs
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2013
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.
#*******************************************************************************
# This code is a modification of the code by Finlay Scott developped and used in STECF SGMED January 2012  
# for Assessment, Reference Points and Forecasting with Hake GSA 09
#*******************************************************************************


XSA<-function(year_start,year_end,name_ind,name_tun,catch_fleet,nb_fleets){

hke.stk <- readFLStock(name_ind, no.discards=TRUE)
# Set the harvest units, fbar range and plus group age
units(harvest(hke.stk))<-"f"
range(hke.stk)["minfbar"] <- minfbar
range(hke.stk)["maxfbar"] <- maxfbar
hke.stk <- setPlusGroup(hke.stk, plusgroup)

# Read in the tuning data
hke.idx <- readFLIndices(name_tun)

#*******************************************************************************
# Perform assessment

# Set the control objext
xsa_control <-FLXSA.control(x=NULL, tol=1e-09, maxit=30, min.nse=min.nse, fse=fse,
rage=rage, qage=qage, shk.n=shk.n, shk.f=shk.f, shk.yrs=shk.yrs, shk.ages=shk.ages,
window=100, tsrange=tsrange, tspower=tspower, vpa=vpa)

# Perform the XSA and put the results back into the stock object
hke.stk <- hke.stk + FLXSA(hke.stk,hke.idx, xsa_control)

# Have a quick look to see if all is well
#plot(hke.stk)

catch_fleets=read.table(file=catch_fleet,sep=";",header=TRUE)
prop = data.frame(catch_fleets)
prop[,2:ncol(prop)]<-NA

# costruzione heading
heading = c("Age")
for (i in (year_start:year_end)){
heading = c(heading,rep(i,nb_fleets)) 
}

colnames(catch_fleets) = heading
colnames(prop) = heading

for (j in (1:(ncol(catch_fleets)-1))){
    for (i in (1:(nrow(catch_fleets))))  {
    prop[i,j+1] = catch_fleets[i,j+1]/sum(catch_fleets[i,colnames(catch_fleets)==heading[j+1]])  
    }
}


F_by_fleet = data.frame(catch_fleets)
F_by_fleet[,2:ncol(F_by_fleet)] = NA 
harvest = data.frame(hke.stk@harvest@.Data)
colnames(harvest)=seq(year_start,year_end,1)
colnames(F_by_fleet) = heading

for (j in (1:(ncol(F_by_fleet)-1))){
    for (i in (1:nrow(F_by_fleet)))  {
    F_by_fleet[i,j+1] = harvest[i,colnames(harvest)==heading[j+1]] * prop[i,j+1]                                        
    }
}


write.table(F_by_fleet,file="Results/F_by_fleet.csv",sep=";",row.names=FALSE)
# write.table(harvest,file="F_aggregated.csv",sep=";",row.names=FALSE)
return(hke.stk)
}