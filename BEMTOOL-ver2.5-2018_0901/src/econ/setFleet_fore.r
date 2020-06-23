# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





# runEcon.r - Bemtool economic indicators in the simulation period
# Author: Paolo Accadia

setFleet_fore<- function(Fyear) {

 # Fyear = Fleetyear



for (y_int in 1:foreperiod) {
    for (m in 1:m_stock) {
        Fyear[[simperiod+y_int]]@import.weight[m] <- as.numeric(as.character(pmat4[[m,y_int]]))
    }
    for (n_int in 1:n_fleet) {
        Fyear[[simperiod+y_int]]@fleetsegments[[n_int]]@fuel.price <- as.numeric(as.character(vcvec[[y_int]]))
        Fyear[[simperiod+y_int]]@fleetsegments[[n_int]]@other.income <- as.numeric(as.character(tsmat1[[n_int,y_int]]))
        Fyear[[simperiod+y_int]]@fleetsegments[[n_int]]@taxes <- as.numeric(as.character(tsmat2[[n_int,y_int]]))
        Fyear[[simperiod+y_int]]@fleetsegments[[n_int]]@new.equipment.costs <- as.numeric(as.character(tsmat3[[n_int,y_int]]))
    } 
}  


return(Fyear)
}
    
