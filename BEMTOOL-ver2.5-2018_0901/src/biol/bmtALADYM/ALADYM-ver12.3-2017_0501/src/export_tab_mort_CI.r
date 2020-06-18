# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



# export all the relevant data

export_tab_mort_CI<-function(End, mortalities) {

if (FALSE) {
End = GLO$L_number
mortalities = exported_tables[[1]]
}

if (showCompTime)  {
export_tab_mort_CI_ptm <- proc.time()  
}

if (IN_BEMTOOL) {
associated_fleetsegment <<- as.vector(cfg[rownames(cfg)==paste("casestudy.S", ALADYM_spe, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <<- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!=""]
associated_fleetsegment_indices <<- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

FLEETSEGMENTS_names <<- BMT_FLEETSEGMENTS[associated_fleetsegment_indices]
print(FLEETSEGMENTS_names)
}

if (length(FLEETSEGMENTS_names)== 1){
Annual_reduction = data.frame(matrix(nrow=nrow(mortalities),ncol = (length(FLEETSEGMENTS_names)) ))
for (i in 2:nrow(mortalities))  {
Annual_reduction[i,length(FLEETSEGMENTS_names)] =  round((mortalities[i,(ncol(mortalities)-length(FLEETSEGMENTS_names))]- mortalities[i-1,(ncol(mortalities)-length(FLEETSEGMENTS_names))])/mortalities[i-1,(ncol(mortalities)-length(FLEETSEGMENTS_names))]*100,2)
}
} else {
Annual_reduction = data.frame(matrix(nrow=nrow(mortalities),ncol = (length(FLEETSEGMENTS_names)+1) ))
for (i in 2:nrow(mortalities))  {
Annual_reduction[i,length(FLEETSEGMENTS_names)+1] =  round((mortalities[i,(ncol(mortalities)-length(FLEETSEGMENTS_names))]- mortalities[i-1,(ncol(mortalities)-length(FLEETSEGMENTS_names))])/mortalities[i-1,(ncol(mortalities)-length(FLEETSEGMENTS_names))]*100,2)
}
}

#for (i in 2:nrow(mortalities))  {
#Annual_reduction[i,length(FLEETSEGMENTS_names)+1] =  round((mortalities[i,(ncol(mortalities)-length(FLEETSEGMENTS_names))]- mortalities[i-1,(ncol(mortalities)-length(FLEETSEGMENTS_names))])/mortalities[i-1,(ncol(mortalities)-length(FLEETSEGMENTS_names))]*100,2)
#}

for (j in 1:length(FLEETSEGMENTS_names)) {
    for (i in 2:nrow(mortalities))  {
Annual_reduction[i,j] =  round((mortalities[i,(ncol(mortalities)-length(FLEETSEGMENTS_names)+j)]- mortalities[i-1,(ncol(mortalities)-length(FLEETSEGMENTS_names)+j)])/mortalities[i-1,(ncol(mortalities)-length(FLEETSEGMENTS_names)+j)]*100,2)

}
}


Annual_reduction = data.frame(cbind(mortalities[,1], Annual_reduction) )


if (length(FLEETSEGMENTS_names)!=1){
MC_head <<- c("Year",paste("Annual_reduction_",FLEETSEGMENTS_names,sep=""), "Annual_reduction")
}  else {
MC_head  <<- c("Year","Annual_reduction")
}
  
colnames(Annual_reduction) <- MC_head
  
write.table(Annual_reduction, MORTALITYCHANGE_table,row.names=FALSE, sep=";")

if (length(FLEETSEGMENTS_names)!=1){
colnames (Annual_reduction) = c("Year",paste("Annual_reduction_",FLEETSEGMENTS_names,sep=""), "Annual_reduction")
}  else {
colnames (Annual_reduction) = c("Year","Annual_reduction")
}

Annual_reduction <- data.frame(cbind(Annual_reduction, rep(current_runCI, nrow(Annual_reduction)))  )
	 colnames(Annual_reduction)  <- MortalityChange_ALLruns_head
	 
	 
	 	 if (!INTEGRATED_APPROACH) {
mortalities_change_ALLruns <<- data.frame(rbind(mortalities_change_ALLruns, Annual_reduction))
} else {
colnames(mortalities) <- colnames(INP$mortalities_change_ALLruns )
if (current_year == 1) {
INP$mortalities_change_ALLruns <- data.frame(rbind(INP$mortalities_change_ALLruns, Annual_reduction))
} else {
  INP$mortalities_change_ALLruns <- data.frame(rbind(INP$mortalities_change_ALLruns, Annual_reduction[Annual_reduction$Year == years_forecast[current_year],])) 
  #tow3[tow3$Year_Age == years_forecast[current_year],]))  
}
}




}
