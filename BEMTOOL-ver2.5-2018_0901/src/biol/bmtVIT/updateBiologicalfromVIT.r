# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





updateBiologicalfromVIT <-function(doALADYM_simulation, populs, IsY, source_obj, yy, mm, sex, disc, mort_const, num_classes, year_interact ) {    # testare

if (FALSE) {
populs <- Populations
source_obj <- VIToutput[[1]]
n_ass_fleet <- n_fleet_for_species
sex <-  "C" 
IsY <- Interactionsyear
disc <- VIT.analysis.discard
mort_const <- mortality_constant
doALADYM_simulation <- ALADYM_flag
yy <- y_ord
mm <- m_int
num_classes <- age_classes_C
year_interact <- y_int
}

      associated_fleetsegment <- as.vector(cfg[rownames(cfg) == paste("casestudy.S", mm, ".associatedFleetsegment", sep=""), ]) 
      associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!="" & associated_fleetsegment!="-"]
      associated_fleetsegment_indices <- which(BMT_FLEETSEGMENTS %in% associated_fleetsegment)
# ----------------------------------------------------------------------------
# READ AND ASSIGN PARAMETERS TO THE POPULATION OF SPECIES mm (ONLY IF yy = 1)
# ---------------------------------------------------------------------------- 
sexratio <- as.numeric(populs[[mm]]@sexratio)

# ages <- length(source_obj$age_classes)

first_age <-  source_obj$age_classes[1]

#ages_F <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", mm, ".StockAssessmentTool", sep=""),5]))
#ages_M <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", mm, ".StockAssessmentTool", sep=""),6]))

minAge <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", mm, ".StockAssessmentTool", sep=""),5]))
maxAge <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", mm, ".StockAssessmentTool", sep=""),6]))

#num_classes <- max(ages_F, ages_M)
#print("numero di classi di VIT:")
#print(num_classes)

ages_F <- as.numeric(populs[[mm]]@lifespan[2,1])
ages_M <- as.numeric(populs[[mm]]@lifespan[1,1])

num_classes_real <- max(ages_F, ages_M)  

gap_classes <- ifelse( (num_classes_real - num_classes)<0, 0, (num_classes_real - num_classes) ) - first_age
#print("numero di classi reali:")
#print(num_classes_real )

lw_params <- populs[[mm]]@lengthweight
a.F <-  as.numeric(lw_params[rownames(lw_params) == "F", colnames(lw_params) == "a"])    # used for female and for combined (as the males and females are the same)
b.F <-  as.numeric(lw_params[rownames(lw_params) == "F", colnames(lw_params) == "b"])
a.M <-  as.numeric(lw_params[rownames(lw_params) == "M", colnames(lw_params) == "a"])
b.M <-  as.numeric(lw_params[rownames(lw_params) == "M", colnames(lw_params) == "b"])



# update maturity vector
fV <- as.numeric(as.character((source_obj$ age_stocks  [1:length(source_obj$age_classes), 8]))) 

if (sex == "C" ) {
if ((num_classes == num_classes_real) | (num_classes_real - num_classes)<0) {

if (yy == 1) {
 populs[[mm]]@maturity.vect[1,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] <- fV[1:num_classes_real]
 populs[[mm]]@maturity.vect[2,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] <- fV[1:num_classes_real]
} else {
 populs[[mm]]@maturity.vect[1,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] <- populs[[mm]]@maturity.vect[1,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] + fV[1:num_classes_real]
 populs[[mm]]@maturity.vect[2,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] <- populs[[mm]]@maturity.vect[2,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] + fV[1:num_classes_real]
}

 } else {
 
 if (yy == 1 ) {
 populs[[mm]]@maturity.vect[1,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] <- c(fV, rep(fV[num_classes], gap_classes))
 populs[[mm]]@maturity.vect[2,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] <-  c(fV, rep(fV[num_classes], gap_classes))
 } else {
  populs[[mm]]@maturity.vect[1,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] <-   populs[[mm]]@maturity.vect[1,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real]  + c(fV, rep(fV[num_classes], gap_classes))
 populs[[mm]]@maturity.vect[2,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] <-   populs[[mm]]@maturity.vect[2,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] +  c(fV, rep(fV[num_classes], gap_classes)) 
 }
 }
} else if (sex=="F") {
 if ( (length(source_obj$age_classes) == num_classes_real) | (num_classes_real - length(source_obj$age_classes))<0) {
 if (yy == 1 ) {
 populs[[mm]]@maturity.vect[2,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] <- fV[1:num_classes_real]
 } else {
 populs[[mm]]@maturity.vect[2,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] <-  populs[[mm]]@maturity.vect[2,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] + fV[1:num_classes_real]
 }
 } else {
  if (yy== 1 ) {
 populs[[mm]]@maturity.vect[2,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] <-  c(fV, rep(fV[length(source_obj$age_classes) ], gap_classes))
 } else {
  populs[[mm]]@maturity.vect[2,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] <-  populs[[mm]]@maturity.vect[2,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] + c(fV, rep(fV[length(source_obj$age_classes) ], gap_classes))
 }
 }
} else if (sex=="M") {
 if ( (length(source_obj$age_classes)  == num_classes_real) | (num_classes_real - length(source_obj$age_classes))<0) {
 if (yy== 1 ) {
 populs[[mm]]@maturity.vect[1,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] <- fV[1:num_classes_real]
 } else {
 populs[[mm]]@maturity.vect[1,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] <- populs[[mm]]@maturity.vect[1,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real]  + fV[1:num_classes_real] 
 }
 } else {
 if (yy== 1 ) {
     populs[[mm]]@maturity.vect[1,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] <-  c(fV, rep(fV[length(source_obj$age_classes) ], gap_classes ))
 }  else {
     populs[[mm]]@maturity.vect[1,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] <-  populs[[mm]]@maturity.vect[1,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] + c(fV, rep(fV[length(source_obj$age_classes) ], gap_classes))
 }
 }
}

# natural mortality
# difference between Z and total F by age considering the first value of the natural mortality vector; same values for males and females
fV_Z <- as.numeric(as.character((source_obj$VPA_results_mortalities   [1:length(source_obj$age_classes), 2]))) 
fV_F <- as.numeric(as.character((source_obj$VPA_results_mortalities   [1:length(source_obj$age_classes), 3]))) 
diff_Z_F <-  fV_Z - fV_F

if (mort_const) {

if (yy== 1) {
if (sex == "C" ) {
 populs[[mm]]@M.cost[1,1] <- diff_Z_F[1]
 populs[[mm]]@M.cost[2,1] <- diff_Z_F[1]
} else if (sex == "F" ) {
 populs[[mm]]@M.cost[2,1] <- diff_Z_F[1]
} else if (sex == "M" ) {
 populs[[mm]]@M.cost[1,1] <- diff_Z_F[1]
}
} else {
if (sex == "C" ) {
 populs[[mm]]@M.cost[1,1] <- as.numeric(as.character(populs[[mm]]@M.cost[1,1]))  + diff_Z_F[1]
 populs[[mm]]@M.cost[2,1] <- as.numeric(as.character(populs[[mm]]@M.cost[2,1])) + diff_Z_F[1]
} else if (sex == "F" ) {
 populs[[mm]]@M.cost[2,1] <- as.numeric(as.character(populs[[mm]]@M.cost[2,1])) + diff_Z_F[1]
} else if (sex == "M" ) {
 populs[[mm]]@M.cost[1,1] <- as.numeric(as.character(populs[[mm]]@M.cost[1,1]))  + diff_Z_F[1]
} 
} 

} else {

if (sex == "C" ) {
if (yy== 1 ) {
  for (month in 1:length(MONTHS) ) { 
   if (num_classes == num_classes_real | (num_classes_real - num_classes)<0 ) {
  populs[[mm]]@M.vect$M[which(rownames(populs[[mm]]@M.vect$M) == paste("age",first_age, sep="")):num_classes_real,month] <- diff_Z_F[1:num_classes_real] 
   } else {
    populs[[mm]]@M.vect$M[which(rownames(populs[[mm]]@M.vect$M) == paste("age",first_age, sep="")):num_classes_real,month] <- c(diff_Z_F, rep(diff_Z_F[num_classes], gap_classes ))  
   }
  }
  
  } else {
    for (month in 1:length(MONTHS) ) { 
   if (num_classes == num_classes_real | (num_classes_real - num_classes)<0) {
  populs[[mm]]@M.vect$M[which(rownames(populs[[mm]]@M.vect$M) == paste("age",first_age, sep="")):num_classes_real,month] <- populs[[mm]]@M.vect$M[which(rownames(populs[[mm]]@M.vect$M) == paste("age",first_age, sep="")):num_classes_real,month] + diff_Z_F[1:num_classes_real] 
   } else {
  populs[[mm]]@M.vect$M[which(rownames(populs[[mm]]@M.vect$M) == paste("age",first_age, sep="")):num_classes_real,month] <-   populs[[mm]]@M.vect$M[which(rownames(populs[[mm]]@M.vect$M) == paste("age",first_age, sep="")):num_classes_real,month] + c(diff_Z_F, rep(diff_Z_F[num_classes], gap_classes ))  
   }
  }
  
  }
  
  
  if (yy == 1 ) {
  for (month in 1:length(MONTHS) ) { 
   if (num_classes == num_classes_real | (num_classes_real - num_classes)<0) {
  populs[[mm]]@M.vect$F[which(rownames(populs[[mm]]@M.vect$F) == paste("age",first_age, sep="")):num_classes_real,month] <- diff_Z_F[1:num_classes_real] 
  } else {
    populs[[mm]]@M.vect$F[which(rownames(populs[[mm]]@M.vect$F) == paste("age",first_age, sep="")):num_classes_real,month] <- c(diff_Z_F, rep(diff_Z_F[num_classes], gap_classes ))
  }
  }
  
  } else {
    for (month in 1:length(MONTHS) ) { 
   if (num_classes == num_classes_real | (num_classes_real - num_classes)<0) {
  populs[[mm]]@M.vect$F[which(rownames(populs[[mm]]@M.vect$F) == paste("age",first_age, sep="")):num_classes_real,month] <- populs[[mm]]@M.vect$F[which(rownames(populs[[mm]]@M.vect$F) == paste("age",first_age, sep="")):num_classes_real,month] + diff_Z_F[1:num_classes_real] 
  } else {
    populs[[mm]]@M.vect$F[which(rownames(populs[[mm]]@M.vect$F) == paste("age",first_age, sep="")):num_classes_real,month] <-   populs[[mm]]@M.vect$F[which(rownames(populs[[mm]]@M.vect$F) == paste("age",first_age, sep="")):num_classes_real,month]  + c(diff_Z_F, rep(diff_Z_F[num_classes], gap_classes ))
  }
  }
  
  }
} else if (sex == "F" ) {

if (yy== 1 ) {
  for (month in 1:length(MONTHS) ) { 
   if (length(source_obj$age_classes) == num_classes_real | (num_classes_real - length(source_obj$age_classes) )<0) {
  populs[[mm]]@M.vect$F[which(colnames(populs[[mm]]@M.vect) == paste("age",first_age, sep="")):num_classes_real,month] <- diff_Z_F[1:num_classes_real] 
  } else {
    populs[[mm]]@M.vect$F[which(colnames(populs[[mm]]@M.vect) == paste("age",first_age, sep="")):num_classes_real,month] <- c(diff_Z_F, rep(diff_Z_F[length(source_obj$age_classes)], gap_classes ))
  }
  }
  } else {
    for (month in 1:length(MONTHS) ) { 
   if (length(source_obj$age_classes) == num_classes_real  | (num_classes_real - length(source_obj$age_classes) )<0) {
  populs[[mm]]@M.vect$F[which(colnames(populs[[mm]]@M.vect) == paste("age",first_age, sep="")):num_classes_real,month] <- populs[[mm]]@M.vect$F[,month]  + diff_Z_F[1:num_classes_real] 
  } else {
  populs[[mm]]@M.vect$F[which(colnames(populs[[mm]]@M.vect) == paste("age",first_age, sep="")):num_classes_real,month] <- populs[[mm]]@M.vect$F[,month] + c(diff_Z_F, rep(diff_Z_F[length(source_obj$age_classes)], gap_classes ))
  }
  }
  
  }
} else if (sex == "M" ) {
if (yy== 1 ) {
  for (month in 1:length(MONTHS) ) { 
   if (length(source_obj$age_classes) == num_classes_real  | (num_classes_real - length(source_obj$age_classes) )<0) {
  populs[[mm]]@M.vect$M[which(rownames(populs[[mm]]@M.vect$M) == paste("age",first_age, sep="")):num_classes_real,month] <- diff_Z_F[1:num_classes_real] 
  } else {
    populs[[mm]]@M.vect$M[which(rownames(populs[[mm]]@M.vect$M) == paste("age",first_age, sep="")):num_classes_real,month] <- c(diff_Z_F, rep(diff_Z_F[length(source_obj$age_classes)], gap_classes ))
  }
  }
  } else {
    for (month in 1:length(MONTHS) ) { 
   if (length(source_obj$age_classes) == num_classes_real  | (num_classes_real - length(source_obj$age_classes) )<0) {
  populs[[mm]]@M.vect$M[which(rownames(populs[[mm]]@M.vect$M) == paste("age",first_age, sep="")):num_classes_real,month] <-   populs[[mm]]@M.vect$M[which(rownames(populs[[mm]]@M.vect$M) == paste("age",first_age, sep="")):num_classes_real,month]  + diff_Z_F[1:num_classes_real] 
  } else {
    populs[[mm]]@M.vect$M[which(rownames(populs[[mm]]@M.vect$M) == paste("age",first_age, sep="")):num_classes_real,month] <- populs[[mm]]@M.vect$M[which(rownames(populs[[mm]]@M.vect$M) == paste("age",first_age, sep="")):num_classes_real,month] + c(diff_Z_F, rep(diff_Z_F[length(source_obj$age_classes)], gap_classes ))
  }
  }
  
  }
}
}




# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# if (!doALADYM_simulation) {
# ----------------------------------------------------------------------------
# READ AND ASSIGN VALUES TO EACH FLEET SEGMENT
# ----------------------------------------------------------------------------
n_fl <- 0
for (nFLEET in 1:length(BMT_FLEETSEGMENTS)) {

if (nFLEET %in% associated_fleetsegment_indices) {
n_fl <- n_fl + 1
# catch in numbers

numbers_temp <-  data.frame(matrix(0, ncol=(num_classes+first_age), nrow=1))
colnames(numbers_temp) <- c(paste("age", c(0:(num_classes+first_age-1)), sep=""))
fV <- as.numeric(as.character((source_obj$ catches_nb  [1:length(source_obj$age_classes), (2+n_fl)]))) 
if ( length(source_obj$age_classes) == num_classes | (num_classes_real - length(source_obj$age_classes) )<0) {
  numbers_temp[1,which(colnames(numbers_temp) == paste("age",first_age, sep="")):(num_classes+first_age)] <- fV
} else {
  numbers_temp[1,which(colnames(numbers_temp) == paste("age",first_age, sep="")):(num_classes+first_age)] <- c(fV, rep(0, gap_classes )) 
}

if (disc) { 

if (sex != "M") { 
IsY[[year_interact]][[mm]]@interactions[[n_fl]]$catches@numbers <- data.frame(numbers_temp)
} else {
IsY[[year_interact]][[mm]]@interactions[[n_fl]]$catches@numbers <- as.numeric(as.character(IsY[[year_interact]][[mm]]@interactions[[n_fl]]$catches@numbers)) + data.frame(numbers_temp)
}

} else {

if (sex != "M") { 
IsY[[year_interact]][[mm]]@interactions[[n_fl]]$landings@numbers <- data.frame(numbers_temp)
} else {
IsY[[year_interact]][[mm]]@interactions[[n_fl]]$landings@numbers <- as.numeric(as.character(IsY[[year_interact]][[mm]]@interactions[[n_fl]]$landings@numbers)) + data.frame(numbers_temp)
}

}

# catch in weight (total weight)
fV <- as.numeric(as.character((source_obj$ catches_w  [1:(length(source_obj$age_classes)), (2+n_fl)])))
weight_temp <-   sum( fV )/ 1000000

if (disc) { 

if (sex != "M") { 
IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $catches  @totalweight <- weight_temp  
} else {
IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $catches  @totalweight <- as.numeric(as.character(IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $catches  @totalweight))  + weight_temp  
} 
 
} else {

if (sex != "M") { 
IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $landings  @totalweight <- weight_temp  
} else {
IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $landings  @totalweight <- as.numeric(as.character(IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $landings  @totalweight))  + weight_temp  
} 

} 
  
# mean length by fleet segment
fV <- as.numeric(as.character((source_obj$ catches_nb  [nrow(source_obj$ catches_nb), 2+n_fl]))) 


if (disc) { 

if (sex != "M") {                                                          
IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $catches  @meanLength  <- fV
} else {
IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $catches  @meanLength  <- fV*(1-sexratio) + as.numeric(as.character(IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $catches  @meanLength)) * sexratio
}

} else {

if (sex != "M") {                                                          
IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $landings  @meanLength  <- fV
} else {
IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $landings  @meanLength  <- fV*(1-sexratio) + as.numeric(as.character(IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $landings  @meanLength)) * sexratio
}

}
 
# mean weight by fleet segment

if (disc) { 

if (sex != "M") {
IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $catches  @meanWeight <- a.F * as.numeric(as.character(IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $catches  @meanLength ))^ b.F 
} else {
IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $catches  @meanWeight <- as.numeric(as.character(IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $catches  @meanWeight))*sexratio + (a.M * as.numeric(as.character(IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $catches  @meanLength ))^ b.M ) * (1-sexratio)
}

} else {

if (sex != "M") {
IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $landings  @meanWeight <- a.F * as.numeric(as.character(IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $landings  @meanLength ))^ b.F 
} else {
IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $landings  @meanWeight <- as.numeric(as.character(IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $landings  @meanWeight))*sexratio + (a.M * as.numeric(as.character(IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $landings  @meanLength ))^ b.M ) * (1-sexratio)
}

}

# set the discard not available
# catch in numbers
numbers_temp <-  data.frame(matrix(NA, ncol=(num_classes+first_age), nrow=1))
colnames(numbers_temp) <- c(paste("age", c(0:(num_classes+first_age-1)), sep=""))
IsY[[year_interact]][[mm]]@interactions[[n_fl]]$discards@numbers <- data.frame(numbers_temp)
# catch in weight (total weight)
IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $discards  @totalweight <- -1    
# mean length by fleet segment
IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $discards  @meanLength  <- -1 
# mean weight by fleet segment
#if (sex == "C" | sex == "F") {
IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $discards  @meanWeight <- -1 


# some actions for discard data
if (disc) {
numbers_temp <-  data.frame(matrix(NA, ncol=(num_classes+first_age), nrow=1))
colnames(numbers_temp) <- c(paste("age", c(0:(num_classes+first_age-1)), sep=""))
IsY[[year_interact]][[mm]]@interactions[[n_fl]]$landings@numbers <- data.frame(numbers_temp)
# catch in weight (total weight)
IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $landings  @totalweight <- -1    
# mean length by fleet segment
IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $landings  @meanLength  <- -1 
# mean weight by fleet segment
#if (sex == "C" | sex == "F") {
IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $landings  @meanWeight <- -1 


} else {

numbers_temp <-  data.frame(matrix(NA, ncol=(num_classes+first_age), nrow=1))
colnames(numbers_temp) <- c(paste("age", c(0:(num_classes+first_age-1)), sep=""))
IsY[[year_interact]][[mm]]@interactions[[n_fl]]$catches@numbers <- data.frame(numbers_temp)
# catch in weight (total weight)
IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $catches  @totalweight <- -1    
# mean length by fleet segment
IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $catches  @meanLength  <- -1 
# mean weight by fleet segment
#if (sex == "C" | sex == "F") {
IsY[[year_interact]][[mm]]@interactions[[n_fl]]   $catches  @meanWeight <- -1 

}                               

if (sex != "M") {                                                                                 
IsY[[year_interact]][[mm]]@mortalities[nFLEET,2] <- mean(as.numeric(as.character((source_obj$ VPA_results_mortalities  [which(source_obj$ VPA_results_mortalities$Class == which(source_obj$age_classes == minAge)): which(source_obj$ VPA_results_mortalities$Class == which(source_obj$age_classes == maxAge)), (3+n_fl)]))), na.rm=T) #F  
} else {
IsY[[year_interact]][[mm]]@mortalities[nFLEET,2] <- as.numeric(as.character(IsY[[year_interact]][[mm]]@mortalities[nFLEET,2])) * sexratio  +  mean(as.numeric(as.character((source_obj$ VPA_results_mortalities  [which(source_obj$ VPA_results_mortalities$Class == which(source_obj$age_classes == minAge)): which(source_obj$ VPA_results_mortalities$Class == which(source_obj$age_classes == maxAge)), (3+n_fl)])))) * (1-sexratio) #F  
}

}
}  # end catches loop

if (sex != "M") {
IsY[[year_interact]][[mm]]@mortalities[(length(BMT_FLEETSEGMENTS)+1),2] <-  mean(as.numeric(as.character((source_obj$ VPA_results_mortalities  [which(source_obj$ VPA_results_mortalities$Class == which(source_obj$age_classes == minAge)): which(source_obj$ VPA_results_mortalities$Class == which(source_obj$age_classes == maxAge)), 3]))), na.rm=T) #F
IsY[[year_interact]][[mm]]@mortalities[(length(BMT_FLEETSEGMENTS)+1),1]<-  mean(as.numeric(as.character((source_obj$ VPA_results_mortalities  [which(source_obj$ VPA_results_mortalities$Class == which(source_obj$age_classes == minAge)): which(source_obj$ VPA_results_mortalities$Class == which(source_obj$age_classes == maxAge)), 2]))), na.rm=T) #Z
} else {
IsY[[year_interact]][[mm]]@mortalities[(length(BMT_FLEETSEGMENTS)+1),2] <-  as.numeric(as.character(IsY[[year_interact]][[mm]]@mortalities[(length(BMT_FLEETSEGMENTS)+1),2]))*sexratio + mean(as.numeric(as.character((source_obj$ VPA_results_mortalities  [which(source_obj$ VPA_results_mortalities$Class == which(source_obj$age_classes == minAge)): which(source_obj$ VPA_results_mortalities$Class == which(source_obj$age_classes == maxAge)), 3]))), na.rm=T) * (1-sexratio) #F
IsY[[year_interact]][[mm]]@mortalities[(length(BMT_FLEETSEGMENTS)+1),1]<-  as.numeric(as.character(IsY[[year_interact]][[mm]]@mortalities[(length(BMT_FLEETSEGMENTS)+1),1]))*sexratio + mean(as.numeric(as.character((source_obj$ VPA_results_mortalities  [which(source_obj$ VPA_results_mortalities$Class == which(source_obj$age_classes == minAge)): which(source_obj$ VPA_results_mortalities$Class == which(source_obj$age_classes == maxAge)), 2]))), na.rm=T) * (1-sexratio) #Z

}
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# READ AND ASSIGN VALUES TO THE EXPLOITED STOCK
# ----------------------------------------------------------------------------         

# critical length for exploited stock
fV <- as.numeric(as.character((source_obj$ critical_length  [1, 3])))
IsY[[year_interact]][[mm]]@exploitedStock @criticalLength <-  fV     

# stock in number
if ( length(source_obj$age_classes) == num_classes  | (num_classes_real - length(source_obj$age_classes) )<0) {
  fV <- as.numeric(as.character((source_obj$ VPA_results_nb  [1:(length(source_obj$age_classes)), 3])))    
} else {
   fV <- c(as.numeric(as.character((source_obj$ VPA_results_nb  [1:(length(source_obj$age_classes)), 3]))) ,  rep(0, gap_classes )) 
}

#fV <- as.numeric(as.character((source_obj$ VPA_results_nb  [1:(length(source_obj$age_classes)), 3])))    
numbers_temp <- data.frame(matrix(NA, nrow=(num_classes+first_age), ncol=length(MONTHS)))
rownames(numbers_temp) <- c(paste("age", c(0:(num_classes+first_age-1)), sep=""))
colnames(numbers_temp) <- MONTHS
for (month in 1:length(MONTHS) ) { numbers_temp[which(rownames(numbers_temp) == paste("age",first_age, sep="")):(num_classes+first_age),month] <- fV }

if (sex == "C") {
# females
numbers_F <- numbers_temp * sexratio 
#numbers_F[nrow(numbers_F),] <- rep(0, ncol(numbers_F))
# males
numbers_M <- numbers_temp * (1 - sexratio )
#numbers_M[nrow(numbers_M),] <-  rep(0, ncol(numbers_M))
IsY[[year_interact]][[mm]]@exploitedStock @numbers $F <- numbers_F
IsY[[year_interact]][[mm]]@exploitedStock @numbers $M <- numbers_M  
 } else if (sex =="F") {
 IsY[[year_interact]][[mm]]@exploitedStock @numbers $F <- numbers_temp
 IsY[[year_interact]][[mm]]@exploitedStock @numbers $M <- numbers_temp    #dummy
 } else {
 IsY[[year_interact]][[mm]]@exploitedStock @numbers $M <- numbers_temp 
 }

# SS in numbers
# sum the products numbers*maturity (by age)
if (sex != "F") {
ss_numbers_temp_males <- sum(IsY[[year_interact]][[mm]]@exploitedStock @numbers $M[,1] * populs[[mm]]@maturity.vect[1,], na.rm=T) 
ss_numbers_temp_females <- sum(IsY[[year_interact]][[mm]]@exploitedStock @numbers $F[,1] * populs[[mm]]@maturity.vect[2,], na.rm=T) 
ss_numb <- ss_numbers_temp_males + ss_numbers_temp_females
ss_numbers_temp <- data.frame(matrix(ss_numb, nrow=1, ncol=length(MONTHS)))
colnames(ss_numbers_temp) <- MONTHS
IsY[[year_interact]][[mm]]@exploitedStock @SS.numbers  <- ss_numbers_temp 
} 

# spawning stock biomass by month (SSB)
fV <- as.numeric(as.character((source_obj$ VPA_results_w  [nrow(source_obj$ VPA_results_w ), 3])))  
SSB_temp <- data.frame(matrix(0, nrow=1, ncol=length(MONTHS)))
rownames(SSB_temp) <- years[yy]
colnames(SSB_temp) <- MONTHS
for (month in 1:length(MONTHS) ) { SSB_temp[,month] <- fV }    # uguale per tutti i mesi
if (sex != "M") { 
IsY[[year_interact]][[mm]]@exploitedStock @SSB  <- SSB_temp/1000000 
} else {
IsY[[year_interact]][[mm]]@exploitedStock @SSB  <- as.numeric(as.character(IsY[[year_interact]][[mm]]@exploitedStock@SSB)) + SSB_temp/1000000 
}   

# stock mean length 
fV <- as.numeric(as.character((source_obj$ VPA_results_nb  [nrow(source_obj$ VPA_results_nb ), 3]))) 
if (sex != "M") { 
IsY[[year_interact]][[mm]]@exploitedStock @meanLength  <- fV
} else {
IsY[[year_interact]][[mm]]@exploitedStock @meanLength <- as.numeric(as.character(IsY[[year_interact]][[mm]]@exploitedStock @meanLength)) * sexratio + fV * (1- sexratio)
}

# stock mean weight
if (sex == "C") {
IsY[[year_interact]][[mm]]@exploitedStock @meanWeight <-   a.F * as.numeric(IsY[[year_interact]][[mm]]@exploitedStock @meanLength) ^ b.F
 } else {
female_w <-   a.F * as.numeric(IsY[[year_interact]][[mm]]@exploitedStock @meanLength) ^ b.F 
male_w <-   a.M * as.numeric(IsY[[year_interact]][[mm]]@exploitedStock @meanLength) ^ b.M  
IsY[[year_interact]][[mm]]@exploitedStock @meanWeight <-  female_w * sexratio + male_w * (1-sexratio)
 }

# stock L95
IsY[[year_interact]][[mm]]@exploitedStock @L95 <- -1

# stock in weight (SB) 
if ( length(source_obj$age_classes) == num_classes | (num_classes_real - length(source_obj$age_classes) )<0) {
fV <- as.numeric(as.character((source_obj$ VPA_results_w  [1:(length(source_obj$age_classes)), 3])))   
} else {
fV <- c ( as.numeric(as.character((source_obj$ VPA_results_w  [1:(length(source_obj$age_classes)), 3]))) , rep(0, gap_classes ))   
}

weights_temp <- data.frame(matrix(NA, nrow=(num_classes+first_age), ncol=length(MONTHS)))
rownames(weights_temp) <- c(paste("age", c(0:(num_classes+first_age-1)), sep=""))
colnames(weights_temp) <- MONTHS
for (month in 1:length(MONTHS) ) { weights_temp[which(rownames(weights_temp) == paste("age",first_age, sep="")):(num_classes+first_age),month] <- fV  }

if (sex == "C") {
weights_F <- (weights_temp * sexratio) / 1000000 
#weights_F[nrow(weights_F),] <- rep(0, ncol(weights_F))
# males
weights_M <- ( weights_temp * (1 - sexratio ) ) /1000000
#weights_M[nrow(weights_M),] <-  rep(0, ncol(weights_M))
IsY[[year_interact]][[mm]]@exploitedStock @SB $F <- weights_F
IsY[[year_interact]][[mm]]@exploitedStock @SB $M <- weights_M     

 } else if (sex =="F") {
 IsY[[year_interact]][[mm]]@exploitedStock @SB $F <-  weights_temp / 1000000
 IsY[[year_interact]][[mm]]@exploitedStock @SB $M <-  weights_temp / 1000000 # dummy  
 } else {
 IsY[[year_interact]][[mm]]@exploitedStock @SB $M <-  weights_temp / 1000000
 }


 IsY[[year_interact]][[mm]]@totalcatch@Ctype <- "CATCHES" 
  IsY[[year_interact]][[mm]]@totallanding@Ctype <- "LANDINGS" 
   IsY[[year_interact]][[mm]]@totaldiscard@Ctype <- "DISCARDS" 

 # saving catch all 
numbers_temp <-  data.frame(matrix(0, ncol=(num_classes+first_age), nrow=1))
colnames(numbers_temp) <- c(paste("age", c(0:(num_classes+first_age-1)), sep=""))
fV <- as.numeric(as.character((source_obj$ catches_nb  [1:length(source_obj$age_classes), 2]))) 

NA_temp <-  data.frame(matrix(NA, ncol=(num_classes+first_age), nrow=1))
colnames(NA_temp) <- c(paste("age", c(0:(num_classes+first_age-1)), sep=""))


if ( length(source_obj$age_classes) == num_classes  | (num_classes_real - length(source_obj$age_classes) )<0) {
numbers_temp[1,which(colnames(numbers_temp) == paste("age",first_age, sep="")):(num_classes+first_age)] <- fV 
} else {
numbers_temp[1,which(colnames(numbers_temp) == paste("age",first_age, sep="")):(num_classes+first_age)] <- c (fV , rep(0, gap_classes ))   
}

if (disc) {
if (sex != "M") { 
IsY[[year_interact]][[mm]]@totalcatch@numbers <- data.frame(numbers_temp)
} else {
IsY[[year_interact]][[mm]]@totalcatch@numbers <- as.numeric(as.character(IsY[[year_interact]][[mm]]@totalcatch@numbers)) + data.frame(numbers_temp)
}
IsY[[year_interact]][[mm]]@totallanding@numbers <- NA_temp
IsY[[year_interact]][[mm]]@totaldiscard@numbers <- NA_temp

} else {
if (sex != "M") { 
IsY[[year_interact]][[mm]]@totallanding@numbers <- data.frame(numbers_temp)
} else {
IsY[[year_interact]][[mm]]@totallanding@numbers <- as.numeric(as.character(IsY[[year_interact]][[mm]]@totallanding@numbers)) + data.frame(numbers_temp)
}

IsY[[year_interact]][[mm]]@totalcatch@numbers <- NA_temp
IsY[[year_interact]][[mm]]@totaldiscard@numbers <- NA_temp

}

# catch in weight (total weight)
fV <- as.numeric(as.character((source_obj$ catches_w  [1:(length(source_obj$age_classes)), (2)])))
weight_temp <-   sum( fV )/ 1000000

if (disc) {
if (sex != "M") { 
IsY[[year_interact]][[mm]]@totalcatch @totalweight <- weight_temp  
} else {
IsY[[year_interact]][[mm]]@totalcatch @totalweight <- as.numeric(as.character(IsY[[year_interact]][[mm]]@totalcatch @totalweight))  + weight_temp  
} 
IsY[[year_interact]][[mm]]@totallanding@totalweight <- -1
IsY[[year_interact]][[mm]]@totaldiscard@totalweight <- -1

} else {

if (sex != "M") { 
IsY[[year_interact]][[mm]]@totallanding @totalweight <- weight_temp  
} else {
IsY[[year_interact]][[mm]]@totallanding @totalweight <- as.numeric(as.character(IsY[[year_interact]][[mm]]@totallanding @totalweight))  + weight_temp  
}

IsY[[year_interact]][[mm]]@totalcatch@totalweight <- -1
IsY[[year_interact]][[mm]]@totaldiscard@totalweight <- -1 
}
  
  
# mean length by fleet segment
fV <- as.numeric(as.character((source_obj$ catches_nb  [nrow(source_obj$ catches_nb), 2]))) 

if (disc) {
if (sex != "M") {                                                          
IsY[[year_interact]][[mm]]@totalcatch  @meanLength  <- fV
} else {
IsY[[year_interact]][[mm]]@totalcatch  @meanLength  <- fV * (1-sexratio) + as.numeric(as.character(IsY[[year_interact]][[mm]]@totalcatch @meanLength)) * sexratio 
}

IsY[[year_interact]][[mm]]@totallanding@meanLength <- -1
IsY[[year_interact]][[mm]]@totaldiscard@meanLength <- -1

} else {

if (sex != "M") {                                                          
IsY[[year_interact]][[mm]]@totallanding  @meanLength  <- fV
} else {
IsY[[year_interact]][[mm]]@totallanding  @meanLength  <- fV * (1-sexratio) + as.numeric(as.character(IsY[[year_interact]][[mm]]@totallanding @meanLength)) * sexratio 
}

IsY[[year_interact]][[mm]]@totalcatch@meanLength <- -1
IsY[[year_interact]][[mm]]@totaldiscard@meanLength <- -1 

}

if (disc) {
# mean weight by fleet segment
if (sex != "M") {
IsY[[year_interact]][[mm]]@totalcatch @meanWeight <- a.F * as.numeric(as.character(IsY[[year_interact]][[mm]]@totalcatch @meanLength ))^ b.F 
} else {
IsY[[year_interact]][[mm]]@totalcatch  @meanWeight <- as.numeric(as.character(IsY[[year_interact]][[mm]]@totalcatch @meanWeight)) * sexratio + (a.M * as.numeric(as.character(IsY[[year_interact]][[mm]]@totalcatch  @meanLength ))^ b.M ) * (1-sexratio)
}

IsY[[year_interact]][[mm]]@totallanding@meanWeight <- -1
IsY[[year_interact]][[mm]]@totaldiscard@meanWeight <- -1

} else {
if (sex != "M") {
IsY[[year_interact]][[mm]]@totallanding @meanWeight <- a.F * as.numeric(as.character(IsY[[year_interact]][[mm]]@totallanding @meanLength ))^ b.F 
} else {
IsY[[year_interact]][[mm]]@totallanding  @meanWeight <- as.numeric(as.character(IsY[[year_interact]][[mm]]@totallanding @meanWeight)) * sexratio + (a.M * as.numeric(as.character(IsY[[year_interact]][[mm]]@totallanding  @meanLength ))^ b.M ) * (1-sexratio)
}

IsY[[year_interact]][[mm]]@totalcatch@meanWeight <- -1
IsY[[year_interact]][[mm]]@totaldiscard@meanWeight <- -1 

}

 # ---------------------------------------

      
# harvest rate
# sum of total catch by age divided by sum of mean weight by age
#totalCatch_ <- as.numeric(as.character((source_obj$ catches_w  [(num_classes+1), 2])))
#totalSB_ <- as.numeric(as.character((source_obj$ VPA_results_w  [(num_classes+1), 3])))                           
if (sex != "F") {
if (disc) { 
totalCatch_ <- IsY[[year_interact]][[mm]]@totalcatch @totalweight
} else {
totalCatch_ <- IsY[[year_interact]][[mm]]@totallanding @totalweight
}
totalSB_ <- sum(IsY[[year_interact]][[mm]]@exploitedStock @SB $F[,1] + IsY[[year_interact]][[mm]]@exploitedStock @SB $M[,1] , na.rm=T) 
 
IsY[[year_interact]][[mm]]@exploitedStock @harvestRate <- totalCatch_/totalSB_                        
}      


# exploitation rate
# mean of Total F by age divided by mean of Z by age
#mean_totalF_ <-  mean(as.numeric(as.character((source_obj$ VPA_results_mortalities  [(1:(num_classes)), 3])))) 
#mean_totalZ_ <-  mean(as.numeric(as.character((source_obj$ VPA_results_mortalities  [(1:(num_classes)), 2])))) 

mean_totalF_ <-  mean(as.numeric(as.character((source_obj$ VPA_results_mortalities  [((minAge+1):(maxAge+1)), 3])))) 
mean_totalZ_ <-  mean(as.numeric(as.character((source_obj$ VPA_results_mortalities  [((minAge+1):(maxAge+1)), 2])))) 

IsY[[year_interact]][[mm]]@exploitedStock @exploitationRate <- mean_totalF_/mean_totalZ_  

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# READ AND ASSIGN VALUES TO THE UNEXPLOITED STOCK
# ----------------------------------------------------------------------------

# stock in numbers
numbers_temp_unex <- data.frame(matrix(NA, nrow=(num_classes+first_age), ncol=length(MONTHS)))
rownames(numbers_temp_unex) <- c(paste("age", c(0:(num_classes+first_age-1)), sep=""))
colnames(numbers_temp_unex) <- MONTHS
IsY[[year_interact]][[mm]]@unexploitedStock @numbers $F <- numbers_temp_unex
IsY[[year_interact]][[mm]]@unexploitedStock @numbers $M <- numbers_temp_unex

# SS in numbers
ss_numbers_temp_unex <- data.frame(matrix(NA, nrow=1, ncol=length(MONTHS)))
colnames(ss_numbers_temp_unex) <- MONTHS
IsY[[year_interact]][[mm]]@unexploitedStock @SS.numbers  <- ss_numbers_temp_unex 

# spawning stock biomass by month (SSB)
SSB_temp_unex <- data.frame(matrix(NA, nrow=1, ncol=length(MONTHS)))
rownames(SSB_temp_unex) <- years[yy]
colnames(SSB_temp_unex) <- MONTHS
IsY[[year_interact]][[mm]]@unexploitedStock @SSB  <- SSB_temp_unex   
  
# stock mean length 
IsY[[year_interact]][[mm]]@unexploitedStock @meanLength  <- -1

# stock mean weight
# if (sex == "C" | sex == "F") {
IsY[[year_interact]][[mm]]@unexploitedStock @meanWeight <-   -1
#  } 

# stock L95
IsY[[year_interact]][[mm]]@unexploitedStock @L95 <- -1

# unique not NA value for UNEXPLOITED
# critical length for unexploited stock
fV <- as.numeric(as.character((source_obj$ critical_length  [2, 3])))
if (sex != "M") {
IsY[[year_interact]][[mm]]@unexploitedStock @criticalLength <-  fV 
} else {
IsY[[year_interact]][[mm]]@unexploitedStock @criticalLength <- as.numeric(as.character(IsY[[year_interact]][[mm]]@unexploitedStock @criticalLength)) * sexratio +  fV* (1-sexratio) 
} 

# stock in weight (SB) 
weights_temp_unex <- data.frame(matrix(NA, nrow=(num_classes+first_age), ncol=length(MONTHS)))
rownames(weights_temp_unex) <- c(paste("age", c(0:(num_classes+first_age-1)), sep=""))
colnames(weights_temp_unex) <- MONTHS
IsY[[year_interact]][[mm]]@unexploitedStock @SB $F <- weights_temp_unex
IsY[[year_interact]][[mm]]@unexploitedStock @SB $M <- weights_temp_unex      
      
# harvest rate
# sum of total catch by age divided by sum of mean weight by age                        
IsY[[year_interact]][[mm]]@unexploitedStock @harvestRate <- 0 

# exploitation rate
# mean of Total F by age divided by mean of Z by age
IsY[[year_interact]][[mm]]@unexploitedStock @exploitationRate <- 0

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# READ AND ASSIGN VALUES TO THE OTHER VARIABLES OF THE INTERACTION
# ----------------------------------------------------------------------------

# mean length total catches
# fV <- as.numeric(as.character((source_obj$ catches_nb  [nrow(source_obj$ catches_nb), 2])))                                                          
# IsY[[year_interact]][[mm]]@meanLength_catches <- fV
IsY[[year_interact]][[mm]]@meanLength_catches <- as.numeric(as.character(IsY[[year_interact]][[mm]]@totalcatch  @meanLength ))
 
# mean weight total catches
# if (sex == "C" | sex == "F") {
#IsY[[year_interact]][[mm]]@meanWeight_catches <- a.F * as.numeric(IsY[[year_interact]][[mm]]@meanLength_catches )^ b.F     
IsY[[year_interact]][[mm]]@meanWeight_catches <-  as.numeric(as.character(IsY[[year_interact]][[mm]]@totalcatch  @meanWeight ))
# } 

# L95
IsY[[year_interact]][[mm]]@L95_catches <- -1



# }  #  end if doALADYM_simulation

# ----------------------------------------------------------------------------
# READ AND ASSIGN VALUES TO THE REFERENCE POINTS
# ----------------------------------------------------------------------------
# mean_totalF_ <-  mean(as.numeric(as.character((source_obj$ VPA_results_mortalities  [(1:(num_classes)), 3]))))   
     
mean_totalF_ <- IsY[[year_interact]][[mm]]@mortalities$F[length(BMT_FLEETSEGMENTS)+1]

if (sex != "M") {
# mean_totalF_ <-  mean(as.numeric(as.character((source_obj$ VPA_results_mortalities  [((minAge+1):(maxAge+1)), 3]))))  

if (nrow(source_obj$ reference_points[source_obj$ reference_points[,1] == "F(0.1)" ,] ) != 0) {
IsY[[year_interact]][[mm]]@referencePoints@F0.1[1, 1] <- as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "F(0.1)",2]))
IsY[[year_interact]][[mm]]@referencePoints@F0.1[1, 2] <- as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "F(0.1)",2])) * mean_totalF_
IsY[[year_interact]][[mm]]@referencePoints@F0.1[1, 3] <- as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "F(0.1)",3])) *  as.numeric(as.character(source_obj$ VPA_results_nb[1,2])) / 1000000 # Y
IsY[[year_interact]][[mm]]@referencePoints@F0.1[1, 4] <- as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "F(0.1)",3]))
IsY[[year_interact]][[mm]]@referencePoints@F0.1[1, 5] <- as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "F(0.1)",4])) *  as.numeric(as.character(source_obj$ VPA_results_nb[1,2])) / 1000000 # B
IsY[[year_interact]][[mm]]@referencePoints@F0.1[1, 6] <- as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "F(0.1)",4]))
IsY[[year_interact]][[mm]]@referencePoints@F0.1[1, 7] <- as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "F(0.1)",5])) *  as.numeric(as.character(source_obj$ VPA_results_nb[1,2])) / 1000000 # SSB
IsY[[year_interact]][[mm]]@referencePoints@F0.1[1, 10] <- as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "F(0.1)",5]))
 
}

if (nrow(source_obj$ reference_points[source_obj$ reference_points[,1] == "Max(:)" ,] ) != 0) {

IsY[[year_interact]][[mm]]@referencePoints@Fmax[1, 1] <- as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] =="Max(:)",2]))
IsY[[year_interact]][[mm]]@referencePoints@Fmax[1, 2] <- as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] =="Max(:)",2])) * mean_totalF_
IsY[[year_interact]][[mm]]@referencePoints@Fmax[1, 3] <- as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "Max(:)",3])) *  as.numeric(as.character(source_obj$ VPA_results_nb[1,2])) / 1000000 # Y
IsY[[year_interact]][[mm]]@referencePoints@Fmax[1, 4] <- as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] =="Max(:)",3]))
IsY[[year_interact]][[mm]]@referencePoints@Fmax[1, 5] <- as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "Max(:)",4])) *  as.numeric(as.character(source_obj$ VPA_results_nb[1,2])) / 1000000 # B
IsY[[year_interact]][[mm]]@referencePoints@Fmax[1, 6] <- as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] =="Max(:)",4]))
IsY[[year_interact]][[mm]]@referencePoints@Fmax[1, 7] <- as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "Max(:)",5])) *  as.numeric(as.character(source_obj$ VPA_results_nb[1,2])) / 1000000 # SSB
IsY[[year_interact]][[mm]]@referencePoints@Fmax[1, 10] <- as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] =="Max(:)",5]))  
} 
} else {

if (nrow(source_obj$ reference_points[source_obj$ reference_points[,1] == "F(0.1)" ,] ) != 0) {
# weighted mean for overall (M+F)
# mean_totalF_ <-  mean(as.numeric(as.character((source_obj$ VPA_results_mortalities  [((minAge+1):(maxAge+1)), 3]))))  
IsY[[year_interact]][[mm]]@referencePoints@F0.1[1, 2] <- as.numeric(as.character(IsY[[year_interact]][[mm]]@referencePoints@F0.1[1, 2]))*sexratio + (as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "F(0.1)",2])) * mean_totalF_  )*(1-sexratio)

# IsY[[year_interact]][[mm]]@referencePoints@F0.1[1, 1] <- as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "F(0.1)",2]))
IsY[[year_interact]][[mm]]@referencePoints@F0.1[1, 1] <- as.numeric(as.character(IsY[[year_interact]][[mm]]@referencePoints@F0.1[1, 2]  )) / mean_totalF_ 

IsY[[year_interact]][[mm]]@referencePoints@F0.1[1, 3] <- as.numeric(as.character(IsY[[year_interact]][[mm]]@referencePoints@F0.1[1, 3])) + (as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "F(0.1)",3])) *  as.numeric(as.character(source_obj$ VPA_results_nb[1,2])) / 1000000 ) # Y
IsY[[year_interact]][[mm]]@referencePoints@F0.1[1, 4] <- as.numeric(as.character(IsY[[year_interact]][[mm]]@referencePoints@F0.1[1, 4])) + as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "F(0.1)",3]))
IsY[[year_interact]][[mm]]@referencePoints@F0.1[1, 5] <- as.numeric(as.character(IsY[[year_interact]][[mm]]@referencePoints@F0.1[1, 5] )) + as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "F(0.1)",4])) *  as.numeric(as.character(source_obj$ VPA_results_nb[1,2])) / 1000000 # B
IsY[[year_interact]][[mm]]@referencePoints@F0.1[1, 6] <- as.numeric(as.character(IsY[[year_interact]][[mm]]@referencePoints@F0.1[1, 6] )) + as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "F(0.1)",4]))
IsY[[year_interact]][[mm]]@referencePoints@F0.1[1, 7] <- as.numeric(as.character( IsY[[year_interact]][[mm]]@referencePoints@F0.1[1, 7] )) + as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "F(0.1)",5])) *  as.numeric(as.character(source_obj$ VPA_results_nb[1,2])) / 1000000 # SSB
IsY[[year_interact]][[mm]]@referencePoints@F0.1[1, 10] <- as.numeric(as.character( IsY[[year_interact]][[mm]]@referencePoints@F0.1[1, 10] )) + as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "F(0.1)",5]))
 }


 if (nrow(source_obj$ reference_points[source_obj$ reference_points[,1] == "Max(:)" ,] ) != 0) {
# weighted mean for overall (M+F)
# mean_totalF_ <-  mean(as.numeric(as.character((source_obj$ VPA_results_mortalities  [((minAge+1):(maxAge+1)), 3]))))  
IsY[[year_interact]][[mm]]@referencePoints@Fmax[1, 2] <- as.numeric(as.character(IsY[[year_interact]][[mm]]@referencePoints@Fmax[1, 2]))*sexratio + (as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "Max(:)",2])) * mean_totalF_  )*(1-sexratio)

# IsY[[year_interact]][[mm]]@referencePoints@F0.1[1, 1] <- as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "F(0.1)",2]))
IsY[[year_interact]][[mm]]@referencePoints@Fmax[1, 1] <- as.numeric(as.character(IsY[[year_interact]][[mm]]@referencePoints@Fmax[1, 2]  )) / mean_totalF_ 

IsY[[year_interact]][[mm]]@referencePoints@Fmax[1, 3] <- as.numeric(as.character(IsY[[year_interact]][[mm]]@referencePoints@Fmax[1, 3])) + (as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "Max(:)",3])) *  as.numeric(as.character(source_obj$ VPA_results_nb[1,2])) / 1000000 ) # Y
IsY[[year_interact]][[mm]]@referencePoints@Fmax[1, 4] <- as.numeric(as.character(IsY[[year_interact]][[mm]]@referencePoints@Fmax[1, 4])) + as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "Max(:)",3]))
IsY[[year_interact]][[mm]]@referencePoints@Fmax[1, 5] <- as.numeric(as.character(IsY[[year_interact]][[mm]]@referencePoints@Fmax[1, 5] )) + as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "Max(:)",4])) *  as.numeric(as.character(source_obj$ VPA_results_nb[1,2])) / 1000000 # B
IsY[[year_interact]][[mm]]@referencePoints@Fmax[1, 6] <- as.numeric(as.character(IsY[[year_interact]][[mm]]@referencePoints@Fmax[1, 6] )) + as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "Max(:)",4]))
IsY[[year_interact]][[mm]]@referencePoints@Fmax[1, 7] <- as.numeric(as.character( IsY[[year_interact]][[mm]]@referencePoints@Fmax[1, 7] )) + as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "Max(:)",5])) *  as.numeric(as.character(source_obj$ VPA_results_nb[1,2])) / 1000000 # SSB
IsY[[year_interact]][[mm]]@referencePoints@Fmax[1, 10] <- as.numeric(as.character( IsY[[year_interact]][[mm]]@referencePoints@Fmax[1, 10] )) + as.numeric(as.character(source_obj$ reference_points[source_obj$ reference_points[,1] == "Max(:)",5]))
}

}
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

return( list(populs, IsY) )
}
