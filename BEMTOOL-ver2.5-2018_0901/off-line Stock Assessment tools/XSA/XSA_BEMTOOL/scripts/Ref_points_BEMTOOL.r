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


RP<-function(model,hke.stk) {
#*******************************************************************************
# Fit stock recruitment relationship (SRR)


srr <- fmle(as.FLSR(hke.stk, model = model))


#*******************************************************************************
# Get reference points

# To get the reference points you can use the FLBRP package

# First we make the FLBRP objects. These take the FLStock and FLSR objects.
# FLBRP uses equilibrium analysis to get the reference points.
# You need to make some assumptions about the equilibrium stock properties
# like weight, maturity, natural mortality etc.

hke.brp <- brp(FLBRP(hke.stk, sr = srr))

# Have a quick look
##require(plyr)
#jpeg(file=paste("Results/Reference_points (",model,")",".jpg",sep=""), width=21, height=21, bg="white", units="cm",res=200)   
#plot(hke.brp)
#dev.off()
#
jpeg(file=paste("Results/srr (",model,")",".jpg",sep=""), width=21, height=21, bg="white", units="cm",res=200)   
plot(srr)
dev.off()

# We can now get the reference points
ref_points<- refpts(hke.brp)
rp_table = data.frame(ref_points@.Data)[,1:5]

temp <- rownames(rp_table)
rp_table = data.frame(rp_table, row.names = NULL)
rp_table <- cbind(temp, rp_table)
colnames(rp_table) =c("", "F","Total Yield","Recruitment","SSB","Biomass")

write.table(rp_table,file=paste("Results/Reference_points (",model,")",".csv",sep=""),sep=";",row.names=FALSE, col.names=T)
#*******************************************************************************
#return (srr)
}