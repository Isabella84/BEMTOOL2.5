# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




loca_mortalities=read.table(MORTALITY_table,header=T,sep=";")

loca_Annual_reduction = data.frame(matrix(nrow=nrow(loca_mortalities),ncol = (length(FLEETSEGMENTS_names)+1)))

for (i in 2:nrow(loca_mortalities))  {
loca_Annual_reduction[i,length(FLEETSEGMENTS_names)+1] =  round((loca_mortalities[i,(ncol(loca_mortalities)-length(FLEETSEGMENTS_names))]- loca_mortalities[i-1,(ncol(loca_mortalities)-length(FLEETSEGMENTS_names))])/loca_mortalities[i-1,(ncol(loca_mortalities)-length(FLEETSEGMENTS_names))]*100,2)

}

for (j in 1:length(FLEETSEGMENTS_names)) {
    for (i in 2:nrow(loca_mortalities))  {
loca_Annual_reduction[i,j] =  round((loca_mortalities[i,(ncol(loca_mortalities)-length(FLEETSEGMENTS_names)+j)]- loca_mortalities[i-1,(ncol(loca_mortalities)-length(FLEETSEGMENTS_names)+j)])/loca_mortalities[i-1,(ncol(loca_mortalities)-length(FLEETSEGMENTS_names)+j)]*100,2)

}
}

loca_mortalities =  cbind(loca_mortalities[,1],loca_Annual_reduction)

if (length(FLEETSEGMENTS_names)!=1){
colnames (loca_mortalities) = c("Year",paste("Annual_reduction_",FLEETSEGMENTS_names,sep=""), "Annual_reduction")
}  else {
colnames (loca_mortalities) = c("Year","Annual_reduction")
}

write.table(loca_mortalities, MORTALITYCHANGE_table,row.names=FALSE, sep=";")
