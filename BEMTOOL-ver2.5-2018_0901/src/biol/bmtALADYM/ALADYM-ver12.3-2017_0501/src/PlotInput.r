# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



PlotInput <- function() {

jpeg(file=INPUT_graph, width=45, height=21, bg="white", units="cm",res=200)

if (as.numeric(INP$OPT_F_TYPE)==1) {
   par(mfrow=c(2,5), oma = c(0, 0, 3, 0))
}  else {
   par(mfrow=c(2,4), oma = c(0, 0, 3, 0))
}

#riga delle femmine
intervallo_x = max(BAS$FAge) - min(BAS$FAge)
intervallo_y = max(BAS$FLength/10) - min(BAS$FLength/10)
vBF = substitute(plain(L[t]) ==Linf*(1-e^(-k*(t-t0))) , list(Linf = round(BAS$FLinf/10,1), k=round(BAS$FK,2), t0=round(BAS$Ft0,2)) )
plot(BAS$FAge, BAS$FLength/10,col="red", type="l", lwd=4, xlab = "Age [year]", ylab = "Length [cm]", main = "von Bertalanffy females")
text(min(BAS$FAge)+intervallo_x/3,max(BAS$FLength/10), vBF)
mtext( GLO$ThisIsVersion,cex=0.6,side=4,outer=FALSE)

intervallo_x = max(BAS$FLength) - min(BAS$FLength)
intervallo_y = max(BAS$FWeight) - min(BAS$FWeight)
retta = substitute(plain(W) == a*L^b, list(a=INP$FWLa, b=round(INP$FWLb , 2)) )
plot(BAS$FLength, BAS$FWeight,col="red", type="l", lwd=4, xlab = "Length [mm]", ylab = "Weight", main = "Length-weight relationship females")
text(min(BAS$FLength)+intervallo_x/3,max(BAS$FWeight), retta)
mtext(GLO$ThisIsVersion,cex=0.6,side=4,outer=FALSE)

plot(BAS$FAge, BAS$FM,col="red", type="l", lwd=4, xlab = "Age [year]", ylab = "M", main = "Natural mortality females")
mtext( GLO$ThisIsVersion,cex=0.6,side=4,outer=FALSE)


loca_xa <- 1:(GLO$L_number / INP$Time_slice)

if (INP$Year_simulation == length(years)) {
    loca_xa <- years
} else {
    loca_xa <- c(years, years_forecast)
}



if  (as.numeric(INP$OPT_F_TYPE)==1) {
plot(loca_xa,meanWequals(BAS$FZ_estimated,GLO$L_number + 1, INP$Time_slice),col="red", type="l", lwd=4, xlab = "Age [year]", ylab = "Z", main = "Total mortality input females")
mtext( GLO$ThisIsVersion,cex=0.6,side=4,outer=FALSE)
} #else {
#plot(0,0,axis=NULL,xlab="",ylab="",type="n",main="Total mortality input females")
#mtext( GLO$ThisIsVersion,cex=0.6,side=4,outer=FALSE)
#}

plot(BAS$FLength/10,BAS$FMaturity,col="red", type="l", lwd=4, xlab = "Length [cm]", ylab = "Proportion of matures", main = "Maturity ogive females")
legend("bottomright",paste(c("L50= ","MR= "),c(round(BAS$FL50m/10,2),round(BAS$FML7525m/10,2))),cex=1 )
mtext(GLO$ThisIsVersion,cex=0.6,side=4,outer=FALSE)

#riga dei maschi
intervallo_x = max(BAS$MAge) - min(BAS$MAge)
intervallo_y = max(BAS$MLength/10) - min(BAS$MLength/10)
vBF = substitute(plain(L[t]) ==Linf*(1-e^(-k*(t-t0))) , list(Linf = round(BAS$MLinf/10,1), k=round(BAS$MK,2), t0=round(BAS$Mt0,2)) )
plot(BAS$MAge, BAS$MLength/10,col="blue", type="l", lwd=4, xlab = "Age [year]", ylab = "Length [cm]", main = "von Bertalanffy males")
text(min(BAS$MAge)+intervallo_x/3,max(BAS$MLength/10), vBF)
mtext(GLO$ThisIsVersion,cex=0.6,side=4,outer=FALSE)

intervallo_x = max(BAS$MLength) - min(BAS$MLength)
intervallo_y = max(BAS$MWeight) - min(BAS$MWeight)
retta = substitute(plain(W) == a*L^b, list(a=INP$MWLa, b=round(INP$MWLb , 2)) )
plot(BAS$MLength, BAS$MWeight,col="blue", type="l", lwd=4, xlab = "Length [mm]", ylab = "Weight", main = "Length-weight relationship males")
text(min(BAS$MLength)+intervallo_x/3,max(BAS$MWeight), retta)
mtext(GLO$ThisIsVersion,cex=0.6,side=4,outer=FALSE)

plot(BAS$MAge, BAS$MM,col="blue", type="l", lwd=4, xlab = "Age [year]", ylab = "M", main = "Natural mortality males")
 mtext( GLO$ThisIsVersion,cex=0.6,side=4,outer=FALSE)
 
#loca_xa <- 1:(GLO$L_number / INP$Time_slice)
if  (as.numeric(INP$OPT_F_TYPE)==1) {
plot(loca_xa,meanWequals(BAS$MZ_estimated,GLO$L_number + 1, INP$Time_slice),col="blue", type="l", lwd=4, xlab = "Age [year]", ylab = "Z", main = "Total mortality input males")
mtext(GLO$ThisIsVersion,cex=0.6,side=4,outer=FALSE)
} #else {
#plot(0,0,axis=NULL,xlab="",ylab="",type="n",main="Total mortality input males")
#mtext( GLO$ThisIsVersion,cex=0.6,side=4,outer=FALSE)
#}



plot(BAS$MLength/10,BAS$MMaturity,col="blue", type="l", lwd=4, xlab = "Length [cm]", ylab = "Proportion of matures", main = "Maturity ogive males")
legend("bottomright",paste(c("L50= ","MR= "),c(round(BAS$FL50m/10,2),round(BAS$FML7525m/10,2))),cex=1 )
mtext(GLO$ThisIsVersion,cex=0.6,side=4,outer=FALSE)

        mtext(BMT_SPECIES[ALADYM_spe], outer = TRUE, cex = 1.5)
dev.off()

# Plot selectivity

if  (as.numeric(INP$OPT_F_TYPE)==1) {

 

#windows(width=45, height=21)
#par(mfrow=c(nb_gears,4)) # M e F e i due istanti
#    for (g in 1:nb_gears){   #righe
#            plot(BAS$FLength,SGEAR(INP$OPT_SG_TYPE[1,g],INP$param1[1,g],INP$param2[1,g],INP$param3[1,g],INP$param4[1,g],INP$param5[1,g], BAS$FLength),col="red",main=paste("Selectivity seed females for",FLEETSEGMENTS_names[g]),type="l",xlab="mm",ylab="Proportion of retained",lwd=3)
#            plot(BAS$MLength,SGEAR(INP$OPT_SG_TYPE[1,g],INP$param1[1,g],INP$param2[1,g],INP$param3[1,g],INP$param4[1,g],INP$param5[1,g], BAS$MLength),col="blue",main=paste("Selectivity seed males for",FLEETSEGMENTS_names[g]),type="l",xlab="mm",ylab="Proportion of retained",lwd=3)
#    }

nb_graphs=nb_gears*2
nb_sheets= ifelse(modulo(nb_graphs,8)==0, as.integer(nb_graphs/8),as.integer(nb_graphs/8)+1)

for (i in 1:nb_sheets) {
if (nb_sheets!= 1) {
nome_file <- paste(SELECTIVITY_graph,"_sheet", i,".jpg", sep="")
} else {
nome_file <- paste(SELECTIVITY_graph,".jpg", sep="")
}
 jpeg(file=nome_file, width=35, height=21, bg="white", units="cm",res=200)
par(mfrow=c(2,4), oma = c(0, 0, 3, 0)) # par(mfrow=c(nb_gears,2)) # M e F e i due istanti 
if ((8*i) > nb_graphs ){
nb_loops = nb_graphs - 8*(i-1)
}  else {
nb_loops = 8*i
} 


for (m in (8*i-7):(8*(i-1)+(nb_loops)) ) {
  if (modulo(m,2) == 1) {
  g= (m/2) + 1
          if (!is.na(FLEETSEGMENTS_names[g])) {
          if (INP$OPT_SG_TYPE[1,g] != 7) {
            plot(BAS$FLength,SGEAR(INP$OPT_SG_TYPE[1,g],INP$param1[1,g],INP$param2[1,g],INP$param3[1,g],INP$param4[1,g],INP$param5[1,g], BAS$FLength),col="red",main=paste("Selectivity seed females for",FLEETSEGMENTS_names[g]),type="l",xlab="mm",ylab="Proportion of retained",lwd=3)
            mtext( GLO$ThisIsVersion,cex=0.6,side=4,outer=FALSE)
            } 
            } else {  
          plot(0,0,type="n")
}        
    } else {
     g= (m/2)
      if (!is.na(FLEETSEGMENTS_names[g])) {
       if (INP$OPT_SG_TYPE[1,g] != 7) {
            plot(BAS$MLength,SGEAR(INP$OPT_SG_TYPE[1,g],INP$param1[1,g],INP$param2[1,g],INP$param3[1,g],INP$param4[1,g],INP$param5[1,g], BAS$MLength),col="blue",main=paste("Selectivity seed males for",FLEETSEGMENTS_names[g]),type="l",xlab="mm",ylab="Proportion of retained",lwd=3)
            mtext( GLO$ThisIsVersion,cex=0.6,side=4,outer=FALSE)
               }
                } else {  
          plot(0,0,type="n")
}        
    } 
   # print(nb_gears[g])
}

dev.off()
}

}


}
