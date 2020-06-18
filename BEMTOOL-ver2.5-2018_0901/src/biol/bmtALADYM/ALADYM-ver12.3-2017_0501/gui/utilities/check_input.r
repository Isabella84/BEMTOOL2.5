# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





check_input<-function(check_str, check_obj) {
 # print(check_str)
if (check_str == "RECRUITMENT_VECTOR" | check_str == "TOTAL_MORTALITY_VECTOR" | check_str =="DISCARD_TABLE" | check_str =="SELECTIVITY_TABLE" | check_str =="FISHINGEFFORT_VECTOR" |  check_str =="PPRODUCTION_VECTOR") {
colnames(check_obj)[1] <- "YEAR"
      
      if ( check_str =="DISCARD_TABLE" | check_str =="SELECTIVITY_TABLE" ) {
          years_to_check <- check_obj$YEAR[-1]
      } else {
          years_to_check <- check_obj$YEAR
      }
       #  print(years_to_check)
    if (!(all(years_to_check %in% years) & all(years %in% years_to_check))) {
        return(list(result="KO", msg=paste(check_str, ".csv file is not consistent with the set years for simulation!") ))
    }   
}

if (check_str =="DISCARD_TABLE_FORE" | check_str =="SELECTIVITY_TABLE_FORE" | check_str =="FISHINGEFFORT_VECTOR_FORE" | check_str =="VESSELS_VECTOR_FORE" | check_str =="DAYS_VECTOR_FORE" | check_str =="GT_VECTOR_FORE") {
colnames(check_obj)[1] <- "YEAR"
     years_to_check <- check_obj$YEAR  
        # print(years_to_check)
    if (!(all(years_to_check %in% years_forecast) & all(years_forecast %in% years_to_check))) {
        return(list(result="KO", msg=paste(check_str, ".csv file is not consistent with the set years for forecast!") ))
    }   
}

if (check_str == "NATURAL_MORTALITY_VECTOR_M" ) {

    if (nrow(check_obj) != biological.months_MM ) {
        return(list(result="KO", msg=paste(check_str, ".csv file: the number of rows (months of recruitment) is not consistent\nwith the lifespan of MALES and selected tr!") ))
    }
} 

if (check_str == "NATURAL_MORTALITY_VECTOR_F" ) {

if (nrow(check_obj) != biological.months_MF ) {
        return(list(result="KO", msg=paste(check_str, ".csv file: the number of rows (months of recruitment) is not consistent\nwith the lifespan of FEMALES and selected tr!") ))
    }
}

if ( check_str == "SELECTIVITY_TABLE" | check_str == "SELECTIVITY_TABLE_FORE" | check_str =="DISCARD_TABLE") {
if (all(check_obj[, 3:ncol(check_obj)] == 0) ) {
        return(list(result="KO", msg=paste(check_str, ".csv file: all values are equal to 0!") ))
}
} else {
if (all(check_obj[, 2:ncol(check_obj)] == 0) ) {
        return(list(result="KO", msg=paste(check_str, ".csv file: all values are equal to 0!") ))
}
}

if (check_str == "DISCARD_EXTERNAL_VECTOR_TABLE_M" ) {

if (nrow(check_obj) != length(years) ) {
        return(list(result="KO", msg=paste(check_str, ".csv file: the number of rows is not consistent with the number of years in the simulation!") ))
    } else if (ncol(check_obj) != (biological.lifeSpanM+1) ) {
        return(list(result="KO", msg=paste(check_str, ".csv file: the number of columns is not consistent with the life span of males!") ))
    } 
}

if (check_str == "DISCARD_EXTERNAL_VECTOR_TABLE_F" ) {

if (nrow(check_obj) != length(years) ) {
        return(list(result="KO", msg=paste(check_str, ".csv file: the number of rows is not consistent with the number of years in the simulation!") ))
    } else if (ncol(check_obj) != (biological.lifeSpanF+1) ) {
        return(list(result="KO", msg=paste(check_str, ".csv file: the number of columns is not consistent with the life span of females!") ))
    } 
}

return(list(result="OK", msg=""))
} 









#
#
#
#
#
#


check_structure<-function(check_str, check_obj) {

if (check_str == "SELECTIVITY_MATRIX") {
selectivity_params <<- get_selectivity_param_name()
number_of_params <- length(selectivity_params)-1
    if ( (ncol(check_obj) - 2) != number_of_params ) {
    return(list(result="KO", msg=paste(check_str, "number of column of parameters in .csv file is not consistent with the selected model!") ))
    }   
}

return(list(result="OK", msg=""))
} 


#controlli: 
#
#1) deve essere numero intero
#2) non deve essere vuoto (per quasi tutti i campi)
#3) salvare i parametri o il file coerentemente con selezione combo box e parametri
