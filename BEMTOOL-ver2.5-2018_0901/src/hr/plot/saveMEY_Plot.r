# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





saveMEY_Plot <- function() {

#name_MEY_levels_file <<- "C:\\[GSA 18] MEY by effort levels.csv"
name_MEY_levels_file <<- paste(casestudy_path, "/MEY calculation/", casestudy_name, " - MEY by effort levels.csv", sep="") 

MEYresults <- read.csv(name_MEY_levels_file,sep=";",header=T)
MEYresults$Effort.level[1] <- 0

#plot_path = paste("C:\\[", casestudy_name, "] MEY plot.jpg", sep="")
plot_path <- paste(casestudy_path, "/MEY calculation/", casestudy_name, " - MEY plot.jpg", sep="")
plot_title <- paste( casestudy_name, "MEY")

y_min <- min(MEYresults[,6:7])
if (is.na(y_min)) {
y_min <- 0
} else {
y_min  <- ifelse(y_min<0, y_min, 0)
}

y_max <- max(MEYresults[,6:7])
if (is.na(y_max)) {
y_max <- 0
} 


#windows()
 jpeg(file=plot_path, width=21, height=21, bg="white", units="cm",res=200)
par(mar=c(5, 8, 7, 6))     # c(bottom, left, top, right)                                                                                                             
plot(MEYresults$Effort.level,MEYresults$Gross.value.added/1000,type="l",xlab="Effort levels", ylab="", axes=F, col="blue", lwd=2, cex.lab=1.5, cex.main=1.8, main=plot_title, ylim=c(y_min, y_max + (y_max - y_min )*0.2 )/1000)
abline(h=0)
abline(v=MEYresults$Effort.level[MEYresults$Gross.value.added/1000 == max(MEYresults$Gross.value.added/1000)], col="blue", lwd=1, lty=2)
axis(1, at=MEYresults$Effort.level , cex.axis=1.5)
axis(2, las=1, cex.axis=1.5)  ## las=1 makes horizontal labels

 mtext( BMT_sw_version,cex=0.6,side=4,outer=FALSE)
 
mtext(",000 €",col="blue",side=2,line=6, cex=1.5)
lines(MEYresults$Effort.level,MEYresults$Profit/1000, type="l", col="red", lwd=2, pch=19)   
abline(v=MEYresults$Effort.level[MEYresults$Profit/1000 == max(MEYresults$Profit/1000)], col="red", lwd=1, lty=2) 
par(new=TRUE)

y_min_ROI <- min(MEYresults[,8])
if (is.na(y_min_ROI)) {
y_min_ROI <- 0
} else {
y_min_ROI  <- ifelse(y_min_ROI<0, y_min_ROI, 0)
}

y_max_ROI <- max(MEYresults[,8])
if (is.na(y_max_ROI)) {
y_max_ROI <- 0
} 

plot(MEYresults$Effort.level,MEYresults$ROI, type="l", col="green", xlab="", ylab="", axes=F, lwd=2, pch=19, ylim=c(y_min_ROI, y_max_ROI + (y_max_ROI - y_min_ROI )*0.2 )) 
abline(v=MEYresults$Effort.level[MEYresults$ROI == max(MEYresults$ROI)], col="green", lwd=1, lty=2)     
axis(4, las=1, cex.axis=1.5) 
#mtext("%",col="blue",side=4,line=4, cex=1.2)       
box()
legend("topright", c("Gross Value Added", "Profit", "ROI" ), border="white", pch=16, col=c( "blue", "red", "green"), horiz=F, fill=NULL, bty="n", cex=1.3)
dev.off() 

}
 
 #saveMEY_Plot()                       