# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





#b.	Calculation of annual reduction of total F as follows:
#   P(t) = (F(t) - F(t-1))/F(t-1) *100
#where the values of F(t-1) and F(t) are related to the 50th percentile (see table fbar_op.csv)
#
get_annual_reduction_totalF<- function() {

reduction_path <- paste(casestudy_path, "/", harvest_rule_id,"/Biological Pressure Impact/", casestudy_name, " - fbar(F01 in ref_year) - ", harvest_rule_id,".csv", sep="")
#fbar_table <- read.csv(paste(MTF_home, "/Results_MTF/fbar(F01 in ",casestudy.endforecast,").csv", sep=""),sep=";") 
 fbar_table <- read.csv(reduction_path,sep=";") 
 
fbar_table$Diff <- -1
fbar_table$totalF <- -1

for (ti in (simperiod+1):(simperiod + foreperiod) ) {
fbar_table$Diff[ti] <- - ( fbar_table[(ti),3] - fbar_table[(ti-1),3] )
fbar_table$totalF[ti] <- fbar_table$Diff[ti] / fbar_table[(ti-1),3] * 100 
}

reduction_totalF <- data.frame(matrix( fbar_table$totalF, ncol=1))
colnames(reduction_totalF) <- "reduction_totalF"
rownames(reduction_totalF) <- rownames(fbar_table)

return(reduction_totalF)
}