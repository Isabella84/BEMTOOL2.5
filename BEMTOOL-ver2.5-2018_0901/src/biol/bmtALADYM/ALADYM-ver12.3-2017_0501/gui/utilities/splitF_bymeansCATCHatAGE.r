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

splitF_bymeansCATCHatAGE <- function(F_overall, catch_fleets_new) {

if (FALSE) {
     F_overall = F_overall_matrix
    catch_fleets_new =  loaded_CatchatAGE
}


# catch_fleets_new = read.table(file="catch_by_fleet.csv",sep=";",header=TRUE) 

propCatch_by_fleet <- catch_fleets_new

propCatch_by_fleet$TotalCatch <-  rowSums(catch_fleets_new[,3:(ncol(catch_fleets_new)-1)])

propCatch_by_fleet[,c(3:(ncol(propCatch_by_fleet)-2),ncol(propCatch_by_fleet))] <- propCatch_by_fleet[,c(3:(ncol(propCatch_by_fleet)-2),ncol(propCatch_by_fleet))]/ propCatch_by_fleet$TotalCatch

# F_overall = read.table(file="Overall F.csv",sep=";",header=TRUE) 

F_by_fleet <- catch_fleets_new

F_by_fleet[, 3:(ncol(F_by_fleet)-1)] <- NA

for (nc in 2:(ncol(F_overall)-1)) {
  F_by_fleet[F_by_fleet$Year == years[nc-1] & F_by_fleet$Sex == "F", 3:(ncol(F_by_fleet)-1)] <-  propCatch_by_fleet[propCatch_by_fleet$Year == years[nc-1] & propCatch_by_fleet$Sex == "F", 3:(ncol(propCatch_by_fleet) -2) ]  * F_overall[F_overall$Sex == "F", nc] 
    F_by_fleet[F_by_fleet$Year == years[nc-1] & F_by_fleet$Sex == "M", 3:(ncol(F_by_fleet)-1)] <-  propCatch_by_fleet[propCatch_by_fleet$Year == years[nc-1] & propCatch_by_fleet$Sex == "M", 3:(ncol(propCatch_by_fleet) -2) ]  * F_overall[F_overall$Sex == "M", nc] 
}

write.table(F_by_fleet,file=F_BYGEAR_splitted_CAA_table,sep=";",row.names=FALSE)

return(F_by_fleet)
}
