# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

# INPUT: 
# SRR_type: type of SRR, corresponding to INP$FRLt
# row of parameters of SRR (3 elements)
# row of CVs associated to the parameters of SRR (3 elements)
# APPROACH: "D" (distribution" or "E" (external, from .csv)
# DIST: pdf to be used to create the grid. The values that can be assumed are: "Lognormal"     "Normal"    "Uniform"  
# nruns: number of runs
# path: path of the csv file where the grid of parameters are located (apply the "browse" command)

if (FALSE) {
a<- INP$FRLa_fore # 1450
b <-INP$FRLb_fore #91872
c <-INP$FRLc_fore
SD_a= 0.12 * a  # aggiungere nell'interfaccia SD per ciascuno dei tre parametri.
SD_b= 0.12 * b
SD_c= 0.12
params=data.frame(param1=a, param2=b,param3=c)
SDs=data.frame(SD1=SD_a, SD2=SD_b,SD3=SD_c)
APPROACH="D"
DIST= "Normal"
nruns=500
path=NA # "C:\\ALADYM-ver10.2.1-2016\\griglia_usata_APPROCCIO2_HOCKEYSTICK_DPS19.csv"
}

#GRID = get_grid(APPROACH,DIST,SRR_type,params,SDs,nruns,path)


get_grid <- function(APPROACH,DIST,SRR_type,params,SDs,nruns) {

if (FALSE) {
SRR_type =  new_aldForecast@CI_recruitment_2_SRR_model
}

if (APPROACH=="D") {
if (DIST == "Normal"){
SRR_CI_vect_a <- rnorm(nruns, mean = params[,1], sd = SDs[,1])
SRR_CI_vect_b <- rnorm(nruns, mean = params[,2], sd = SDs[,2])
SRR_CI_vect_c <- rnorm(nruns, mean = params[,3], sd = SDs[,3])

GRID=cbind(SRR_CI_vect_a,SRR_CI_vect_b,SRR_CI_vect_c)
colnames(GRID)= c("Var1","Var2","Var3")

} else if (DIST == "Lognormal") {
SRR_CI_vect_a <- rlnorm(nruns, mean = params[,1], sd = SDs[,1])
SRR_CI_vect_b <- rlnorm(nruns, mean = params[,2], sd = SDs[,2])
SRR_CI_vect_c <- rlnorm(nruns, mean = params[,3], sd = SDs[,3])

GRID=cbind(SRR_CI_vect_a,SRR_CI_vect_b,SRR_CI_vect_c)
colnames(GRID)= c("Var1","Var2","Var3")

} else if (DIST == "Uniform") {
SRR_CI_vect_a <- runif(nruns, min = params[,1], max = SDs[,1])
SRR_CI_vect_b <- runif(nruns, min = params[,2], max = SDs[,2])
SRR_CI_vect_c <- runif(nruns, min = params[,3], max = SDs[,3])

GRID=cbind(SRR_CI_vect_a,SRR_CI_vect_b,SRR_CI_vect_c)
colnames(GRID)= c("Var1","Var2","Var3")

}

} else if (APPROACH=="E"){
GRID =  new_aldForecast@CI_recruitment_2_1_error_vector[, c(2:4)] 
colnames(GRID) <- c("Var1","Var2","Var3")
}

return(GRID)
}
