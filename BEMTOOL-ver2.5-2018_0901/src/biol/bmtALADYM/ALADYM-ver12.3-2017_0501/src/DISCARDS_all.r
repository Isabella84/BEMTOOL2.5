# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




DISCARDS_all <- function(loca_start, loca_end, sex) {

if (showCompTime)  {
DISCARDS_all_ptm <- proc.time()  
}

 allDiscards_len <- vector(mode = "list", length = length(FLEETSEGMENTS_names))                  # GUI: ciclo sulla lista offspring

if (sex == "M") {
  Length <- BAS$MLength
} else {
  Length <- BAS$FLength
}


for (g in 1:length(FLEETSEGMENTS_names)) {

discard_mat <- data.frame(matrix(0, nrow=length(c(loca_start:loca_end)), ncol=length(Length)))

indices <- c(loca_start:loca_end)

for (nr in 1:length(indices) ) {

if (!is.na(INP$Discard[indices[nr], g]) & as.character(INP$Discard[indices[nr], g])=="0") {
loca_temp <- 0
} else if (!is.na(INP$Discard[indices[nr], g]) & as.character(INP$Discard[indices[nr], g])=="Y") {
loca_temp <- 1 / (1 + exp(-(log(9) / INP$param7[indices[nr],g]) * (INP$param6[indices[nr],g] - Length)))
}  else if (is.na(INP$Discard[indices[nr], g])) {
 Length[] <- NA
loca_temp <- Length
 }
   discard_mat[nr,]  <-  loca_temp
}

allDiscards_len[[g]] <- discard_mat[]

}

if (showCompTime)  {
proc_ <- proc.time()
print(paste("DISCARDS_all [time]::::::::::::::::::::::::::::::::", round(as.numeric(proc_[3]-DISCARDS_all_ptm[3]),2), "sec" ) , quote=F )    
#print(proc.time() - SGEAR_ext_vec_ptm, quote=F ) 
 rm(DISCARDS_all_ptm)
}


return(allDiscards_len)
}





#plot(logistic(p,t50,R,seq(-1,10,1))*100,type="l",col="red",lwd=5,xlab="year",ylab="% reduction")
#abline(c(0.61*100,seq(1,10,1)),c(0,0))
#abline(c(100,seq(1,10,1)),c(0,0))
#title("multiplier logistic reduction")
