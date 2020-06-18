# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


#*******************************************************************************
# This code is a modification of the code by Finlay Scott developped and used in STECF SGMED January 2012  
# for Assessment, Reference Points and Forecasting with Hake GSA 09
#*******************************************************************************

#catch_fleets=read.table(file="catch.n.fleets_PIL GSA 17.csv",sep=";",header=TRUE)

splitF_bymeansPRODUCTION <- function(F_overall, PROD) {

if (FALSE) {
   F_overall = all_Fmortalities_O
    PROD =  production_to_use
}


# catch_fleets_new = read.table(file="catch_by_fleet.csv",sep=";",header=TRUE) 

PROD_by_fleet <- PROD

PROD_by_fleet <- rbind(PROD_by_fleet , c("Overall_prod", colSums(PROD_by_fleet[,2:ncol(PROD_by_fleet)]) ) )

props_PROD_by_fleet <- PROD_by_fleet

for (nr in 1:nrow(props_PROD_by_fleet)) {
  props_PROD_by_fleet[nr,2:ncol(props_PROD_by_fleet)]  <- as.numeric(as.character(props_PROD_by_fleet[nr,2:ncol(props_PROD_by_fleet)] )) / as.numeric(as.character(props_PROD_by_fleet[nrow(props_PROD_by_fleet),2:ncol(props_PROD_by_fleet)] ))
}


n_ages_M  <- as.numeric(as.character(gtkEntryGetText(entryVBF_M_lifespan)))  
n_ages_F  <- as.numeric(as.character(gtkEntryGetText(entryVBF_F_lifespan)))
first_age_mal <- 0
first_age_fem <- 0

   n_ages_M <- n_ages_M - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)
   n_ages_F <- n_ages_F - trunc(Tr/12)
    first_age_fem <- trunc(Tr/12)

print(paste("Life span: males", n_ages_M, "females", n_ages_F) ) 

F_by_fleey_age <- data.frame(matrix(nrow=(n_ages_M*length(years) + n_ages_F*length(years)), ncol=3+length(FLEETSEGMENTS_names) ))
colnames(F_by_fleey_age) <- c("Age", "Year", paste("fs", c(1:length(FLEETSEGMENTS_names)), sep=""), "Sex") 

    vect_year <- data.frame(matrix(nrow=0, ncol=1))
        vect_sex <- data.frame(matrix(nrow=0, ncol=1))
            vect_ages <- data.frame(matrix(nrow=0, ncol=1))

 for (yy in 1:length(years) ) { 
vect_year <- rbind(vect_year, matrix(rep(years[yy], (n_ages_M+n_ages_F)), ncol=1) )
vect_sex <- rbind(vect_sex,   matrix(c(rep("M", n_ages_M) , rep("F", n_ages_F)), ncol=1) )
vect_ages <- rbind(vect_ages,  matrix(c(c(first_age_mal:(n_ages_M+first_age_mal-1)) , c(first_age_fem:(n_ages_F+first_age_fem-1))), ncol=1) )
 }
    
 F_by_fleey_age$Age <- vect_ages[,1]
  F_by_fleey_age$Year <- vect_year[,1]  
    F_by_fleey_age$Sex <- vect_sex[,1] 
  
  F_splitted_M <- data.frame(matrix(ncol=length(FLEETSEGMENTS_names), nrow=n_ages_M))
   F_splitted_F <- data.frame(matrix(ncol=length(FLEETSEGMENTS_names), nrow=n_ages_F))
          
 for (yy in 1:length(years) ) { 
 
all_Fmortalities_O_this_ye <- F_overall[,  c( yy+1,ncol(F_overall) ) ] 
 F_splitted_M[,1:ncol(F_splitted_M)] <- all_Fmortalities_O_this_ye[all_Fmortalities_O_this_ye$Sex == "M", -ncol(all_Fmortalities_O_this_ye) ] 
  F_splitted_F[,1:ncol(F_splitted_F)] <- all_Fmortalities_O_this_ye[all_Fmortalities_O_this_ye$Sex == "F",  -ncol(all_Fmortalities_O_this_ye)]

props_this_ye_by_fleet <- as.numeric(as.character(props_PROD_by_fleet[1:(nrow(props_PROD_by_fleet)-1), yy+1] )) 

 F_splitted_M[,] <-  sweep(F_splitted_M, MARGIN=2, props_this_ye_by_fleet,'*')
  F_splitted_F[,] <-  sweep(F_splitted_F, MARGIN=2, props_this_ye_by_fleet,'*')
                                                                                     
F_by_fleey_age[F_by_fleey_age$Year == years[yy] & F_by_fleey_age$Sex == "M", 3:(ncol(F_by_fleey_age)-1) ] <- F_splitted_M

F_by_fleey_age[F_by_fleey_age$Year == years[yy] & F_by_fleey_age$Sex == "F", 3:(ncol(F_by_fleey_age)-1) ] <- F_splitted_F

}

write.table(F_by_fleey_age,file=F_BYGEAR_splitted_P_table,sep=";",row.names=FALSE)

return(F_by_fleey_age)
}
