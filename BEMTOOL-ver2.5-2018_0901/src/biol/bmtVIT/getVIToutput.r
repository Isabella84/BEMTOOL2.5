# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





getVIToutput<-function(VITpath, n_ages, n_fleet) {


if (FALSE) {
         VITpath=  filePath_C
         n_ages =  ages_C
        n_fleet=  n_fleets
}

VITmatrix <- read.csv(VITpath, sep=";", header=FALSE)

first_age <- as.numeric(as.character(VITmatrix[2,2]))

VIT.age_classes <- c(first_age:(n_ages+first_age-1))
                       # li leggo dalla cfg table
# set cursor = 1 (BEGIN)
cur <- 1

heading <-  t(VITmatrix[cur, which(VITmatrix[cur,] != "")] )
VIT.age_stocks <- data.frame(VITmatrix[c((cur+1):(n_ages+2)), 1:8] )
colnames(VIT.age_stocks) <- heading

VIT.age_stocks <- VIT.age_stocks[1:(nrow(VIT.age_stocks)-1),]
#print(VIT.age_stocks)

# update cursor to next table
cur <- cur +  (nrow(VIT.age_stocks)+1) +2

heading <- t( VITmatrix[cur, which(VITmatrix[cur,] != "")])
VIT.catches_nb <- data.frame(VITmatrix[c((cur+1):(cur+1+n_ages+2)), 1:(n_fleet+2)] )
colnames(VIT.catches_nb) <- heading
#print(VIT.catches_nb)

# update cursor to next table
cur <- cur +  nrow(VIT.catches_nb) +2

heading <-  t(VITmatrix[cur, which(VITmatrix[cur,] != "")])
VIT.catches_w <- data.frame(VITmatrix[c((cur+1):(cur+1+n_ages+1)), 1:(n_fleet+2)] )
colnames(VIT.catches_w) <- heading
#print(VIT.catches_w)


# update cursor to next table
cur <- cur +  nrow(VIT.catches_w) +2

heading <-  t(VITmatrix[cur, which(VITmatrix[cur,] != "")]  )
VIT.VPA_results_nb <- data.frame(VITmatrix[c((cur+1):(cur+1+n_ages+2)), 1:(n_fleet+2)] )
colnames(VIT.VPA_results_nb) <- heading
#print(VIT.VPA_results_nb)


# update cursor to next table
cur <- cur +  nrow(VIT.VPA_results_nb) +2

heading <-  t(VITmatrix[cur, which(VITmatrix[cur,] != "")]  )
VIT.VPA_results_w <- data.frame(VITmatrix[c((cur+1):(cur+1+n_ages+1)), 1:(n_fleet+2)] )
colnames(VIT.VPA_results_w) <- heading
#print(VIT.VPA_results_w)


# update cursor to next table
cur <- cur +  nrow(VIT.VPA_results_w) +2

heading <- t( VITmatrix[cur, which(VITmatrix[cur,] != "")] )
VIT.VPA_results_mortalities <- data.frame(VITmatrix[c((cur+1):(cur+1+n_ages+1)), 1:(n_fleet+3)] )
colnames(VIT.VPA_results_mortalities) <- heading
#print(VIT.VPA_results_mortalities)


# update cursor to next table
cur <- cur +  nrow(VIT.VPA_results_mortalities) +1

heading <-  t(VITmatrix[cur, which(VITmatrix[cur,] != "")] )
VIT.critical_length <- data.frame(VITmatrix[c((cur+1):(cur+3)), 1:3] )
colnames(VIT.critical_length) <- heading
#print(VIT.critical_length)


cur <- cur +  nrow(VIT.critical_length) +1

heading <-  t(VITmatrix[cur, which(VITmatrix[cur,] != "")] )
VIT.others <- data.frame(VITmatrix[c((cur+1):(cur+8)), 1:3] )
colnames(VIT.others) <- heading
#print(VIT.others)


cur <- cur +  nrow(VIT.others) +1

heading <-  t(VITmatrix[cur, which(VITmatrix[cur,] != "")]  )
VIT.others_2 <- data.frame(matrix(VITmatrix[c((cur+1):(cur+1)), 1:6] , nrow=1, ncol=6))
colnames(VIT.others_2) <- heading
#print(VIT.others_2)


cur <- cur +  nrow(VIT.others_2) +1

heading <- t( VITmatrix[cur, which(VITmatrix[cur,] != "")] )
VIT.reference_points <- data.frame(VITmatrix[c((cur+1):(cur+5+n_fleet)), 1:5] )   # eliminati i reference points per attrezzo
colnames(VIT.reference_points) <- heading[1:5]
#print(VIT.reference_points)

#
#cur <- cur +  nrow(VIT.reference_points) +1
#
#heading <- t( VITmatrix[cur, which(VITmatrix[cur,] != "")] )
#VIT.others_3 <- data.frame(matrix(VITmatrix[c((cur+1):(cur+1)), 1:6] , nrow=1, ncol=6))
#colnames(VIT.others_3) <- heading
##print(VIT.others_3)
#
#
#cur <- cur +  nrow(VIT.others_3) +1
#
#heading <-  t(VITmatrix[cur, which(VITmatrix[cur,] != "")] )
#VIT.last <- data.frame(VITmatrix[c((cur+1):nrow(VITmatrix)), 1:5])
#colnames(VIT.last) <- heading[1:5]
##print(VIT.last)

VITout <- list( age_classes=VIT.age_classes,  age_stocks=VIT.age_stocks,  catches_nb=VIT.catches_nb,  catches_w=VIT.catches_w ,  VPA_results_nb=VIT.VPA_results_nb, VPA_results_w=VIT.VPA_results_w, VPA_results_mortalities=VIT.VPA_results_mortalities, critical_length=VIT.critical_length, others=VIT.others, others_2=VIT.others_2, reference_points=VIT.reference_points) #, others_3=VIT.others_3, last=VIT.last)



return(VITout)
}