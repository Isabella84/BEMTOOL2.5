# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




# export all the relevant data

export_tab_mort<-function(End) {

if (showCompTime)  {
export_tab_mort_ptm <- proc.time()  
}


if (IN_BEMTOOL) {
associated_fleetsegment <<- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <<- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <<- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

FLEETSEGMENTS_names <<- BMT_FLEETSEGMENTS[associated_fleetsegment_indices]

}

mortalities=read.table(MORTALITY_table,header=T,sep=";")

Annual_reduction = data.frame(matrix(nrow=nrow(mortalities),ncol = (length(FLEETSEGMENTS_names)+1)))

for (i in 2:nrow(mortalities))  {
Annual_reduction[i,length(FLEETSEGMENTS_names)+1] =  round((mortalities[i,(ncol(mortalities)-length(FLEETSEGMENTS_names))]- mortalities[i-1,(ncol(mortalities)-length(FLEETSEGMENTS_names))])/mortalities[i-1,(ncol(mortalities)-length(FLEETSEGMENTS_names))]*100,2)

}

for (j in 1:length(FLEETSEGMENTS_names)) {
    for (i in 2:nrow(mortalities))  {
Annual_reduction[i,j] =  round((mortalities[i,(ncol(mortalities)-length(FLEETSEGMENTS_names)+j)]- mortalities[i-1,(ncol(mortalities)-length(FLEETSEGMENTS_names)+j)])/mortalities[i-1,(ncol(mortalities)-length(FLEETSEGMENTS_names)+j)]*100,2)

}
}

mortalities =  cbind(mortalities[,1],Annual_reduction)

if (length(FLEETSEGMENTS_names)!=1){
colnames (mortalities) = c("Year",paste("Annual_reduction_",FLEETSEGMENTS_names,sep=""), "Annual_reduction")
}  else {
colnames (mortalities) = c("Year","Annual_reduction")
}

write.table(mortalities, MORTALITYCHANGE_table,row.names=FALSE, sep=";")


 if (showCompTime)  {
 proc_ <- proc.time()
# SIMULATION_EXPLOITED_ptm <- proc.time()
print(paste("export_tab_mort [time]::::::::::::::::::::::::::::::::", round(as.numeric(proc_[3]-export_tab_mort_ptm[3]),2), "sec" ), quote=F )   
#print(proc.time() - SIMULATION_EXPLOITED_ptm, quote=F ) 
rm(export_tab_mort_ptm)
}

}
