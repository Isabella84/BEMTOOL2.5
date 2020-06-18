# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





resetDAYSfromTAC <- function(Fyear) {

print("- Average DAYS equal to zero once the TAC has been reached.", quote=F)

associated_fleetsegment <<- as.vector(cfg[rownames(cfg)==paste("casestudy.S", species_TAC, ".associatedFleetsegment", sep=""), ])   
associated_fleetsegment <<- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment != "" & associated_fleetsegment != "-"]
associated_fleetsegment_indices <<- which(associated_fleetsegment %in% BMT_FLEETSEGMENTS)

n_int_ord <- 1
    for (n_int in 1:length(BMT_FLEETSEGMENTS)) { 
    if (n_int %in% associated_fleetsegment_indices) {   

    for (mon in c(1:12) ) {                      # c( (((TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR-1)*12)+2): (((TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR)*12)+1)) 
         if (as.numeric(as.character(INP$Fishing_efforts[  (TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR-1)*12+1+mon , n_int_ord])) == 0) {
              Fyear[[TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR]]@fleetsegments[[n_int]]@DAYS.average[1,mon] <- 0
         } 
    }
   n_int_ord <- n_int_ord+1                              
}
}
   Fyear <- updateDerivedEffortVars.int(Fyear, TIME_TO_CHANGE_WITH_THE_CURRENT_YEAR)

return(Fyear)
}
 