# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





updateBiologicalfromReport <-function(populs, IsY, source_obj, yy, mm, mort_const, year_interact) {

if (FALSE) {
populs <- Populations
source_obj <- ReportRes
IsY <- Interactionsyear
mort_const <- mortality_constant
mm <- m_int
yy <- y_ord
year_interact <- y_int
}
      associated_fleetsegment <- as.vector(cfg[rownames(cfg) == paste("casestudy.S", mm, ".associatedFleetsegment", sep=""), ]) 
      associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!="" & associated_fleetsegment!="-"]
      associated_fleetsegment_indices <- which(BMT_FLEETSEGMENTS %in% associated_fleetsegment)
      n_ass_fleet <- length(associated_fleetsegment_indices)
# ----------------------------------------------------------------------------
# READ AND ASSIGN PARAMETERS TO THE POPULATION OF SPECIES mm (ONLY IF yy = 1)
# ---------------------------------------------------------------------------- 
sexratio <- as.numeric(populs[[mm]]@sexratio)
ages_C <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", mm, ".StockAssessmentTool", sep=""),5]))

num_classes <- ages_C
#print("numero di classi di VIT:")
#print(num_classes)

t0 <- as.numeric(populs[[mm]]@growth[2,1])
k <-  as.numeric(populs[[mm]]@growth[2,2])
linf <-   as.numeric(populs[[mm]]@growth[2,3])
  
ages_F <- as.numeric(populs[[mm]]@lifespan[2,1])
ages_M <- as.numeric(populs[[mm]]@lifespan[1,1])

num_classes_real <- max(ages_F, ages_M)  
#print("numero di classi reali:")
#print(num_classes_real )

lw_params <- populs[[mm]]@lengthweight
a.F <-  as.numeric(lw_params[rownames(lw_params) == "F", colnames(lw_params) == "a"])    # used for female and for combined (as the males and females are the same)
b.F <-  as.numeric(lw_params[rownames(lw_params) == "F", colnames(lw_params) == "b"])
a.M <-  as.numeric(lw_params[rownames(lw_params) == "M", colnames(lw_params) == "a"])
b.M <-  as.numeric(lw_params[rownames(lw_params) == "M", colnames(lw_params) == "b"])

first_age <- source_obj$age_classes[1]

if (yy == 1) {
# update maturity vector

mean_maturity <- data.frame(matrix(0, ncol=length(source_obj$maturity), nrow=num_classes))

    for (ny in 1:length(source_obj$maturity) )  {
         mean_maturity[,ny] <- as.numeric( as.character( source_obj$maturity[[ny]]) )
    }

fR <- rowMeans(mean_maturity) 
#if (sex == "C" ) {

if (num_classes == num_classes_real) {
 populs[[mm]]@maturity.vect[1,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] <- fR
 populs[[mm]]@maturity.vect[2,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] <- fR
 } else {
 populs[[mm]]@maturity.vect[1,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] <- c(fR, rep(fR[num_classes], (num_classes_real - num_classes - first_age)))
 populs[[mm]]@maturity.vect[2,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] <-  c(fR, rep(fR[num_classes], (num_classes_real - num_classes - first_age)))
 }
#}

# natural mortality
# difference between Z and total F by age considering the first value of the natural mortality vector; same values for males and females
mean_M <- data.frame(matrix(0, ncol=length(source_obj$natural_mortality), nrow=num_classes))
    for (ny in 1:length(source_obj$natural_mortality) )  {
         mean_M[,ny] <- as.numeric( as.character( source_obj$natural_mortality[[ny]]) )
    }

fR_M <- rowMeans(mean_M) 


if (mort_const) {
 populs[[mm]]@M.cost[1,1] <- fR_M[1]
 populs[[mm]]@M.cost[2,1] <- fR_M[1]
} else {
  for (month in 1:length(MONTHS) ) { 
   if (num_classes == num_classes_real) {
  populs[[mm]]@M.vect$M[which(rownames(populs[[mm]]@M.vect$M) == paste("age",first_age, sep="")):num_classes_real,month] <- fR_M 
   } else {
  populs[[mm]]@M.vect$M[which(rownames(populs[[mm]]@M.vect$M) == paste("age",first_age, sep="")):num_classes_real,month] <- c(fR_M, rep(fR_M[num_classes], (num_classes_real - num_classes - first_age)))  
   }
  }
  for (month in 1:length(MONTHS) ) { 
   if (num_classes == num_classes_real) {
  populs[[mm]]@M.vect$F[which(rownames(populs[[mm]]@M.vect$F) == paste("age",first_age, sep="")):num_classes_real,month] <- fR_M 
  } else {
    populs[[mm]]@M.vect$F[which(rownames(populs[[mm]]@M.vect$F) == paste("age",first_age, sep="")):num_classes_real,month] <- c(fR_M, rep(fR_M[num_classes], (num_classes_real - num_classes - first_age)))
  }
  }
#}
}

}  # end if first year

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

numbers_temp_NA <-  data.frame(matrix(NA, ncol=(num_classes+first_age), nrow=1))
colnames(numbers_temp_NA) <- c(paste("age", c(0:(num_classes+first_age-1)), sep=""))

 mid_ages <-  c(0:(num_classes+first_age-1)) + 0.5
mid_lengths <- c(0:(num_classes+first_age-1))
for (ag in 1:length(mid_ages)) { mid_lengths[ag] <- VB(mid_ages[ag],linf,k,t0) }


# assign totalcatch, totallanding and totaldiscard

# ------------------------------------------------


IsY[[year_interact]][[mm]]@totalcatch@Ctype <- "CATCHES"

# catch in numbers
numbers_temp <-  data.frame(matrix(0, ncol=(num_classes+first_age), nrow=1))
colnames(numbers_temp) <- c(paste("age", c(0:(num_classes+first_age-1)), sep=""))
fR <- as.numeric(as.character((source_obj$ catches_nb  [1:num_classes, yy]))) 
numbers_temp[1,which(colnames(numbers_temp) == paste("age",first_age, sep="")):(num_classes+first_age)] <- fR  

if (!is.na(numbers_temp[1,1]) ) {
  IsY[[year_interact]][[mm]]@totalcatch@numbers <- data.frame(numbers_temp)
} else {
  IsY[[year_interact]][[mm]]@totalcatch@numbers <- data.frame(numbers_temp_NA)
}

# catch in weight (total weight)
fR <- as.numeric(as.character((source_obj$ catches_wt  [1:(num_classes), (yy)])))

if (!is.na(numbers_temp[1,1]) ) {
weight_temp <-  sum( IsY[[year_interact]][[mm]]@totalcatch@numbers * fR , na.rm=T) /1000    
IsY[[year_interact]][[mm]]@totalcatch@totalweight <- weight_temp  
} else {
IsY[[year_interact]][[mm]]@totalcatch@totalweight <- -1
} 
   
# mean length by fleet segment
# mean length
# the mean length associated to each age (+0.5) is calculated by means of von Bertalanffy function. Then a weighted mean of mean lengths by age is calculated.
if (!is.na(numbers_temp[1,1]) ) {
IsY[[year_interact]][[mm]]@totalcatch@meanLength <- sum(IsY[[year_interact]][[mm]]@totalcatch@numbers * mid_lengths, na.rm=T) / sum(IsY[[year_interact]][[mm]]@totalcatch@numbers , na.rm=T)
} else {
IsY[[year_interact]][[mm]]@totalcatch@meanLength  <- -1
}

# mean weight
fR <- data.frame(source_obj$catches_wt)
fR <- as.numeric(fR[,yy])
if (!is.na(numbers_temp[1,1]) ) {
IsY[[year_interact]][[mm]]@totalcatch@meanWeight <- sum(IsY[[year_interact]][[mm]]@totalcatch@numbers * fR, na.rm=T) / sum(IsY[[year_interact]][[mm]]@totalcatch@numbers , na.rm=T) * 1000
} else {
IsY[[year_interact]][[mm]]@totalcatch@meanWeight <- -1
}




IsY[[year_interact]][[mm]]@totallanding@Ctype <- "LANDINGS"

# catch in numbers
numbers_temp <-  data.frame(matrix(0, ncol=(num_classes+first_age), nrow=1))
colnames(numbers_temp) <- c(paste("age", c(0:(num_classes+first_age-1)), sep=""))
fR <- as.numeric(as.character((source_obj$ landings_nb  [1:num_classes, yy]))) 
numbers_temp[1,which(colnames(numbers_temp) == paste("age",first_age, sep="")):(num_classes+first_age)] <- fR  

if (!is.na(numbers_temp[1,1]) ) {
  IsY[[year_interact]][[mm]]@totallanding@numbers <- data.frame(numbers_temp)
} else {
  IsY[[year_interact]][[mm]]@totallanding@numbers <- data.frame(numbers_temp_NA)
}

# catch in weight (total weight)
fR <- as.numeric(as.character((source_obj$ landings_wt  [1:(num_classes), (yy)])))

if (!is.na(numbers_temp[1,1]) ) {
weight_temp <-  sum( IsY[[year_interact]][[mm]]@totallanding@numbers * fR , na.rm=T) /1000    
IsY[[year_interact]][[mm]]@totallanding@totalweight <- weight_temp  
} else {
IsY[[year_interact]][[mm]]@totallanding@totalweight <- -1
} 
   
# mean length by fleet segment
# mean length
# the mean length associated to each age (+0.5) is calculated by means of von Bertalanffy function. Then a weighted mean of mean lengths by age is calculated.
if (!is.na(numbers_temp[1,1]) ) {
IsY[[year_interact]][[mm]]@totallanding@meanLength <- sum(IsY[[year_interact]][[mm]]@totallanding@numbers * mid_lengths,na.rm=T) / sum(IsY[[year_interact]][[mm]]@totallanding@numbers, na.rm=T )
} else {
IsY[[year_interact]][[mm]]@totallanding@meanLength  <- -1
}

# mean weight
fR <- data.frame(source_obj$landings_wt)
fR <- as.numeric(fR[,yy])
if (!is.na(numbers_temp[1,1]) ) {
IsY[[year_interact]][[mm]]@totallanding@meanWeight <- sum(IsY[[year_interact]][[mm]]@totallanding@numbers * fR, na.rm=T) / sum(IsY[[year_interact]][[mm]]@totallanding@numbers, na.rm=T ) * 1000
} else {
IsY[[year_interact]][[mm]]@totallanding@meanWeight <- -1
}





IsY[[year_interact]][[mm]]@totaldiscard@Ctype <- "DISCARDS"

# catch in numbers
numbers_temp <-  data.frame(matrix(0, ncol=(num_classes+first_age), nrow=1))
colnames(numbers_temp) <- c(paste("age", c(0:(num_classes+first_age-1)), sep=""))
fR <- as.numeric(as.character((source_obj$ discards_nb  [1:num_classes, yy]))) 
numbers_temp[1,which(colnames(numbers_temp) == paste("age",first_age, sep="")):(num_classes+first_age)] <- fR  

if (!is.na(numbers_temp[1,1]) ) {
  IsY[[year_interact]][[mm]]@totaldiscard@numbers <- data.frame(numbers_temp)
} else {
  IsY[[year_interact]][[mm]]@totaldiscard@numbers <- data.frame(numbers_temp_NA)
}

# catch in weight (total weight)
fR <- as.numeric(as.character((source_obj$ discards_wt  [1:(num_classes), (yy)])))

if (!is.na(numbers_temp[1,1]) ) {
weight_temp <-  sum( IsY[[year_interact]][[mm]]@totaldiscard@numbers * fR , na.rm=T) /1000    
IsY[[year_interact]][[mm]]@totaldiscard@totalweight <- weight_temp  
} else {
IsY[[year_interact]][[mm]]@totaldiscard@totalweight <- -1
} 
   
# mean length by fleet segment
# mean length
# the mean length associated to each age (+0.5) is calculated by means of von Bertalanffy function. Then a weighted mean of mean lengths by age is calculated.
if (!is.na(numbers_temp[1,1]) ) {
IsY[[year_interact]][[mm]]@totaldiscard@meanLength <- sum(IsY[[year_interact]][[mm]]@totaldiscard@numbers * mid_lengths, na.rm=T) / sum(IsY[[year_interact]][[mm]]@totaldiscard@numbers , na.rm=T)
} else {
IsY[[year_interact]][[mm]]@totaldiscard@meanLength  <- -1
}

# mean weight
fR <- data.frame(source_obj$discards_wt)
fR <- as.numeric(fR[,yy])
if (!is.na(numbers_temp[1,1]) ) {
IsY[[year_interact]][[mm]]@totaldiscard@meanWeight <- sum(IsY[[year_interact]][[mm]]@totaldiscard@numbers * fR, na.rm=T) / sum(IsY[[year_interact]][[mm]]@totaldiscard@numbers, na.rm=T ) * 1000
} else {
IsY[[year_interact]][[mm]]@totaldiscard@meanWeight <- -1
}



# ----------------------------------------------------------------------------
# READ AND ASSIGN VALUES TO EACH FLEET SEGMENT
# ----------------------------------------------------------------------------
n_fl_int <- 0
for (nFLEET in 1:length(BMT_FLEETSEGMENTS)) {

if (nFLEET %in% associated_fleetsegment_indices) {
n_fl_int <- n_fl_int + 1

# ----------------------------------------------------------------------------
# READ AND ASSIGN CATCH of Ctype=CATCHES
# ----------------------------------------------------------------------------

IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$catches@Ctype <- "CATCHES"

# catch in numbers
numbers_temp <-  data.frame(matrix(0, ncol=(num_classes+first_age), nrow=1))
colnames(numbers_temp) <- c(paste("age", c(0:(num_classes+first_age-1)), sep=""))
fR <- as.numeric(as.character((source_obj$ catches_by_fleet  [1:num_classes, ((yy-1)*n_ass_fleet + n_fl_int)]))) 
numbers_temp[1,which(colnames(numbers_temp) == paste("age",first_age, sep="")):(num_classes+first_age)] <- fR  

if (!is.na(numbers_temp[1,1]) ) {
  IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$catches@numbers <- data.frame(numbers_temp)
} else {
  IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$catches@numbers <- data.frame(numbers_temp_NA)
}

# catch in weight (total weight)
fR <- as.numeric(as.character((source_obj$ catches_wt  [1:(num_classes), (yy)])))

if (!is.na(numbers_temp[1,1]) ) {
weight_temp <-  sum( IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$catches@numbers * fR , na.rm=T) /1000    
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]   $catches  @totalweight <- weight_temp  
} else {
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]   $catches  @totalweight <- -1
} 
   
# mean length by fleet segment
# mean length
# the mean length associated to each age (+0.5) is calculated by means of von Bertalanffy function. Then a weighted mean of mean lengths by age is calculated.
if (!is.na(numbers_temp[1,1]) ) {
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$catches@meanLength <- sum(IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$catches@numbers * mid_lengths, na.rm=T) / sum(IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$catches@numbers , na.rm=T)
} else {
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$catches@meanLength  <- -1
}

# mean weight
fR <- data.frame(source_obj$catches_wt)
fR <- as.numeric(fR[,yy])
if (!is.na(numbers_temp[1,1]) ) {
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$catches@meanWeight <- sum(IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$catches@numbers * fR, na.rm=T) / sum(IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$catches@numbers, na.rm=T ) * 1000
} else {
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$catches@meanWeight <- -1
}

# ----------------------------------------------------------------------------
# READ AND ASSIGN CATCH of Ctype=LANDINGS
# ----------------------------------------------------------------------------

IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@Ctype <- "LANDINGS"

# catch in numbers
numbers_temp <-  data.frame(matrix(0, ncol=(num_classes+first_age), nrow=1))
colnames(numbers_temp) <- c(paste("age", c(0:(num_classes+first_age-1)), sep=""))
fR <- as.numeric(as.character((source_obj$ landings_by_fleet  [1:num_classes, ((yy-1)*n_ass_fleet + n_fl_int)]))) 
numbers_temp[1,which(colnames(numbers_temp) == paste("age",first_age, sep="")):(num_classes+first_age)] <- fR  

if (!is.na(numbers_temp[1,1]) ) {
  IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@numbers <- data.frame(numbers_temp)
} else {
  IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@numbers <- data.frame(numbers_temp_NA)
}

# catch in weight (total weight)
fR <- as.numeric(as.character((source_obj$ landings_wt  [1:(num_classes), (yy)])))

if (!is.na(numbers_temp[1,1]) ) {
weight_temp <-  sum( IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@numbers * fR , na.rm=T) /1000    
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]   $landings  @totalweight <- weight_temp  
} else {
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]   $landings  @totalweight <- -1
} 
   
# mean length by fleet segment
# mean length
# the mean length associated to each age (+0.5) is calculated by means of von Bertalanffy function. Then a weighted mean of mean lengths by age is calculated.
if (!is.na(numbers_temp[1,1]) ) {
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@meanLength <- sum(IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@numbers * mid_lengths, na.rm=T) / sum(IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@numbers , na.rm=T)
} else {
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@meanLength  <- -1
}

# mean weight
fR <- data.frame(source_obj$landings_wt)
fR <- as.numeric(fR[,yy])
if (!is.na(numbers_temp[1,1]) ) {
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@meanWeight <- sum(IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@numbers * fR, na.rm=T) / sum(IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@numbers , na.rm=T) * 1000
} else {
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@meanWeight <- -1
}


# ----------------------------------------------------------------------------
# READ AND ASSIGN CATCH of Ctype=DISCARDS
# ----------------------------------------------------------------------------

IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$catches@Ctype <- "DISCARDS"

# catch in numbers
numbers_temp <-  data.frame(matrix(0, ncol=(num_classes+first_age), nrow=1))
colnames(numbers_temp) <- c(paste("age", c(0:(num_classes+first_age-1)), sep=""))
fR <- as.numeric(as.character((source_obj$ discards_by_fleet  [1:num_classes, ((yy-1)*n_ass_fleet + n_fl_int)]))) 
numbers_temp[1,which(colnames(numbers_temp) == paste("age",first_age, sep="")):(num_classes+first_age)] <- fR  

if (!is.na(numbers_temp[1,1]) ) {
  IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$discards@numbers <- data.frame(numbers_temp)
} else {
  IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$discards@numbers <- data.frame(numbers_temp_NA)
}

# catch in weight (total weight)
fR <- as.numeric(as.character((source_obj$ discards_wt  [1:(num_classes), (yy)])))

if (!is.na(numbers_temp[1,1]) ) {
weight_temp <-  sum( IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$discards@numbers * fR, na.rm=T ) /1000    
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]   $discards  @totalweight <- weight_temp  
} else {
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]   $discards  @totalweight <- -1
} 
   
# mean length by fleet segment
# mean length
# the mean length associated to each age (+0.5) is calculated by means of von Bertalanffy function. Then a weighted mean of mean lengths by age is calculated.
if (!is.na(numbers_temp[1,1]) ) {
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$discards@meanLength <- sum(IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$discards@numbers * mid_lengths, na.rm=T) / sum(IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$discards@numbers , na.rm=T)
} else {
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$discards@meanLength  <- -1
}

# mean weight
fR <- data.frame(source_obj$discards_wt)
fR <- as.numeric(fR[,yy])
if (!is.na(numbers_temp[1,1]) ) {
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$discards@meanWeight <- sum(IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$discards@numbers * fR, na.rm=T) / sum(IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$discards@numbers , na.rm=T) * 1000
} else {
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$discards@meanWeight <- -1
}




min_ <- as.numeric(as.character(source_obj$ age_rangeF[1,1] ))
max_ <- as.numeric(as.character(source_obj$ age_rangeF[1,2] ))

fR <- data.frame(matrix(as.numeric(as.character(source_obj$ fishing_mortality[,yy] )) , nrow=1))
colnames(fR) <- c(paste("age",first_age:(num_classes+first_age-1), sep=""))

IsY[[year_interact]][[mm]]@mortalities[(length(BMT_FLEETSEGMENTS)+1),2] <-  mean(as.numeric(as.character(fR[which(colnames(fR) == paste("age",min_, sep="")):which(colnames(fR) == paste("age",max_, sep=""))])), na.rm=T) #F

fR <- data.frame(matrix(as.numeric(as.character(source_obj$ natural_mortality[,yy] )) + as.numeric(as.character(source_obj$ fishing_mortality[,yy] )) , nrow=1))
colnames(fR) <- c(paste("age",first_age:(num_classes+first_age-1), sep=""))

IsY[[year_interact]][[mm]]@mortalities[(length(BMT_FLEETSEGMENTS)+1),1]<-  mean(as.numeric(as.character(fR[which(colnames(fR) == paste("age",min_, sep="")):which(colnames(fR) == paste("age",max_, sep=""))])), na.rm=T) #Z


}

}  # end catches loop

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# READ AND ASSIGN VALUES TO THE EXPLOITED STOCK
# ----------------------------------------------------------------------------         

## critical length for exploited stock
#fR <- as.numeric(as.character((source_obj$ critical_length  [1, 3])))
#IsY[[year_interact]][[mm]]@exploitedStock @criticalLength <-  fV     

# stock in number
fR <- as.numeric(as.character((source_obj$ stock_nb  [1:(num_classes), yy])))    
numbers_temp <- data.frame(matrix(NA, nrow=(num_classes+first_age), ncol=length(MONTHS)))
rownames(numbers_temp) <- c(paste("age", c(0:(num_classes+first_age-1)), sep=""))
colnames(numbers_temp) <- MONTHS
for (month in 1:length(MONTHS) ) { numbers_temp[which(rownames(numbers_temp) == paste("age",first_age, sep="")):(num_classes+first_age),month] <- fR }

# females
numbers_F <- numbers_temp * sexratio 
#numbers_F[nrow(numbers_F),] <- rep(0, ncol(numbers_F))
# males
numbers_M <- numbers_temp * (1 - sexratio )
#numbers_M[nrow(numbers_M),] <-  rep(0, ncol(numbers_M))
IsY[[year_interact]][[mm]]@exploitedStock @numbers $F <- numbers_F
IsY[[year_interact]][[mm]]@exploitedStock @numbers $M <- numbers_M  

# stock weight in numbers
fR <- data.frame(source_obj$stock_wt)      
weights_temp_ex <- data.frame(matrix(NA, nrow=(num_classes+first_age), ncol=length(MONTHS)))
rownames(weights_temp_ex) <- c(paste("age", c(0:(num_classes+first_age-1)), sep=""))
colnames(weights_temp_ex) <- MONTHS
for (month in 1:length(MONTHS) ) { weights_temp_ex[which(rownames(weights_temp_ex) == paste("age",first_age, sep="")):(num_classes+first_age),month] <- as.numeric(as.character(fR[,yy])) }

# females
weights_F <- weights_temp_ex * IsY[[year_interact]][[mm]]@exploitedStock @numbers $F  / 1000 # to get the tons 
# males
weights_M <- weights_temp_ex * IsY[[year_interact]][[mm]]@exploitedStock @numbers $M  / 1000 # to get the tons
IsY[[year_interact]][[mm]]@exploitedStock @SB $F <- weights_F
IsY[[year_interact]][[mm]]@exploitedStock @SB $M <- weights_M 
 
# mean weight
mean_weight <- ( sum(as.numeric(as.character(weights_temp_ex[,1])) * (numbers_temp[,1] *1000) , na.rm=T ) / sum(numbers_temp[,1] *1000, na.rm=T) ) * 1000 # to get the grams  
IsY[[year_interact]][[mm]]@exploitedStock @meanWeight <- mean_weight

# mean length
IsY[[year_interact]][[mm]]@exploitedStock @meanLength <- sum(as.numeric(as.character(numbers_temp[,1])) * mid_lengths, na.rm=T) / sum(as.numeric(as.character(numbers_temp[,1])), na.rm=T)

# stock L95
IsY[[year_interact]][[mm]]@exploitedStock @L95 <- -1

# SS in numbers
# sum the products numbers*maturity (by age)
ss_numbers_temp_males <- sum(IsY[[year_interact]][[mm]]@exploitedStock @numbers $M[,1] * populs[[mm]]@maturity.vect[1,], na.rm=T) 
ss_numbers_temp_females <- sum(IsY[[year_interact]][[mm]]@exploitedStock @numbers $F[,1] * populs[[mm]]@maturity.vect[2,], na.rm=T) 
ss_numb <- ss_numbers_temp_males + ss_numbers_temp_females
ss_numbers_temp <- data.frame(matrix(ss_numb, nrow=1, ncol=length(MONTHS)))
colnames(ss_numbers_temp) <- MONTHS
IsY[[year_interact]][[mm]]@exploitedStock @SS.numbers  <- ss_numbers_temp 

# SSB
# sum of the products numbers*maturity * mean weight  (by age); to be divided by 1000
ss_weights_temp_males <- sum(IsY[[year_interact]][[mm]]@exploitedStock @numbers $M[,1] * populs[[mm]]@maturity.vect[1,] * as.numeric(as.character(weights_temp_ex[,1])), na.rm=T) 
ss_weights_temp_females <- sum(IsY[[year_interact]][[mm]]@exploitedStock @numbers $F[,1] * populs[[mm]]@maturity.vect[2,] * as.numeric(as.character(weights_temp_ex[,1])), na.rm=T) 
ss_weights <- ss_weights_temp_males + ss_weights_temp_females
ss_weights_temp <- data.frame(matrix(ss_weights, nrow=1, ncol=length(MONTHS)))
colnames(ss_weights_temp) <- MONTHS
IsY[[year_interact]][[mm]]@exploitedStock @SSB  <- ss_weights_temp
            
# critical length
# the age corresponding to the maximum SB is selected. Then, the mean length associated to this age (+0.5) is calculated by means of reverse von Bertalanffy function.
biomass <- IsY[[year_interact]][[mm]]@exploitedStock @SB $F[,1] + IsY[[year_interact]][[mm]]@exploitedStock @SB $M[,1]
critical_age <- which(biomass == max(biomass))
IsY[[year_interact]][[mm]]@exploitedStock @criticalLength  <- mid_lengths[critical_age]

# harvest rate
# sum of total weight of all the fleet segment divided by total SB (males and females) of the first month summed up over the ages
total_weight_catches <- 0
for (fl_n in 1:n_ass_fleet) {
  total_weight_catches <- total_weight_catches + IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$catches@totalweight
}
total_SB <- sum(IsY[[year_interact]][[mm]]@exploitedStock @SB$F[,1], na.rm=T) + sum(IsY[[year_interact]][[mm]]@exploitedStock @SB$M[,1], na.rm=T)
IsY[[year_interact]][[mm]]@exploitedStock @harvestRate <- total_weight_catches/total_SB

# exploitation Rate
# mean of harvest (between ages minfbar and maxfbar) divided by mean of (harvest+m) (only values between the ages minfbar and maxfbar) considering the mean over the ages
min_ <- as.numeric(as.character(source_obj$ age_rangeF[1,1] ))
max_ <- as.numeric(as.character(source_obj$ age_rangeF[1,2] ))

fR <- data.frame(matrix(as.numeric(as.character(source_obj$ fishing_mortality[,yy] )), nrow=1 ))
colnames(fR) <- c(paste("age",first_age:(num_classes+first_age-1), sep=""))

mean_harvest <- mean(as.numeric(as.character(fR[which(colnames(fR) == paste("age",min_, sep="")):which(colnames(fR) == paste("age",max_, sep=""))] )), na.rm=T)
fR <-  data.frame(matrix(as.numeric(as.character(source_obj$ natural_mortality[,yy] )) + as.numeric(as.character(source_obj$ fishing_mortality[,yy] )), nrow=1))

colnames(fR) <- c(paste("age",first_age:(num_classes+first_age-1), sep=""))
mean_harvest_m <-  mean(as.numeric(as.character(fR[which(colnames(fR) == paste("age",min_, sep="")):which(colnames(fR) == paste("age",max_, sep=""))] )), na.rm=T)
IsY[[year_interact]][[mm]]@exploitedStock @exploitationRate <- mean_harvest/mean_harvest_m
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
IsY[[year_interact]][[mm]]@unexploitedStock @meanWeight <-   -1

# stock L95
IsY[[year_interact]][[mm]]@unexploitedStock @L95 <- -1

# unique not NA value for UNEXPLOITED
# critical length for unexploited stock
fV <- as.numeric(as.character((source_obj$ critical_length  [2, 3])))
IsY[[year_interact]][[mm]]@unexploitedStock @criticalLength <-  fV  

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
tot_numb_catch <- as.numeric(as.character((source_obj$ catches_nb  [,yy])))                                                          
IsY[[year_interact]][[mm]]@meanLength_catches <- sum(tot_numb_catch * mid_lengths, na.rm=T) / sum(tot_numb_catch, na.rm=T)
 
# mean weight total catches
IsY[[year_interact]][[mm]]@meanWeight_catches <- a.F * as.numeric(IsY[[year_interact]][[mm]]@meanLength_catches )^ b.F 

# L95
IsY[[year_interact]][[mm]]@L95_catches <- -1


# }  #  end if doALADYM_simulation

# ----------------------------------------------------------------------------
# READ AND ASSIGN VALUES TO THE REFERENCE POINTS
# ----------------------------------------------------------------------------
 min_ <- as.numeric(as.character(source_obj$ age_rangeF[1,1] ))
max_ <- as.numeric(as.character(source_obj$ age_rangeF[1,2] ))

fR <- data.frame(matrix(as.numeric(as.character(source_obj$ fishing_mortality[,yy] )), nrow=1))
colnames(fR) <- c(paste("age",first_age:(num_classes+first_age-1), sep=""))

mean_totalF_ <- mean(as.numeric(as.character(fR[which(colnames(fR) == paste("age",min_, sep="")):which(colnames(fR) == paste("age",max_, sep=""))] )), na.rm=T) 

fR <- source_obj$ reference_points 

IsY[[year_interact]][[mm]]@referencePoints@F0.1[3, 1] <-  as.numeric(as.character(fR[1,1]))/ mean_totalF_ # factor
IsY[[year_interact]][[mm]]@referencePoints@F0.1[3, 2] <-  as.numeric(as.character(fR[1,1]))
IsY[[year_interact]][[mm]]@referencePoints@F0.1[3, 3] <-  as.numeric(as.character(fR[1,2]))
IsY[[year_interact]][[mm]]@referencePoints@F0.1[3, 4] <-  IsY[[year_interact]][[mm]]@referencePoints@F0.1[3, 3] /  as.numeric(as.character(fR[1,3]))
IsY[[year_interact]][[mm]]@referencePoints@F0.1[3, 5] <-  as.numeric(as.character(fR[1,5]))
IsY[[year_interact]][[mm]]@referencePoints@F0.1[3, 6] <-  IsY[[year_interact]][[mm]]@referencePoints@F0.1[3, 5] /  as.numeric(as.character(fR[1,3]))
IsY[[year_interact]][[mm]]@referencePoints@F0.1[3, 7] <-  as.numeric(as.character(fR[1,4]))
 
#rp_temp_max <-  data.frame(matrix(0, ncol=7, nrow=1))
#colnames(rp_temp_max) <- c("factor", "F", "Y", "Y_R", "B", "B_R", "SSB")
#                               # data.frame	values by items [factor, Y, Y/R, B, B/R, SSB]
IsY[[year_interact]][[mm]]@referencePoints@Fmax[3, 1] <-  as.numeric(as.character(fR[2,1]))/ mean_totalF_ # factor
IsY[[year_interact]][[mm]]@referencePoints@Fmax[3, 2] <-  as.numeric(as.character(fR[2,1]))
IsY[[year_interact]][[mm]]@referencePoints@Fmax[3, 3] <-  as.numeric(as.character(fR[2,2]))
IsY[[year_interact]][[mm]]@referencePoints@Fmax[3, 4] <-  IsY[[year_interact]][[mm]]@referencePoints@Fmax[3, 3] /  as.numeric(as.character(fR[2,3]))
IsY[[year_interact]][[mm]]@referencePoints@Fmax[3, 5] <-  as.numeric(as.character(fR[2,5]))
IsY[[year_interact]][[mm]]@referencePoints@Fmax[3, 6] <-  IsY[[year_interact]][[mm]]@referencePoints@Fmax[3, 5] /  as.numeric(as.character(fR[2,3]))
IsY[[year_interact]][[mm]]@referencePoints@Fmax[3, 7] <-  as.numeric(as.character(fR[2,4]))


IsY[[year_interact]][[mm]]@referencePoints@FMSY[3, 1] <-  as.numeric(as.character(fR[4,1]))/ mean_totalF_ # factor
IsY[[year_interact]][[mm]]@referencePoints@FMSY[3, 2] <-  as.numeric(as.character(fR[4,1]))
IsY[[year_interact]][[mm]]@referencePoints@FMSY[3, 3] <-  as.numeric(as.character(fR[4,2]))
IsY[[year_interact]][[mm]]@referencePoints@FMSY[3, 4] <-  IsY[[year_interact]][[mm]]@referencePoints@FMSY[3, 3] /  as.numeric(as.character(fR[4,3]))
IsY[[year_interact]][[mm]]@referencePoints@FMSY[3, 5] <-  as.numeric(as.character(fR[4,5]))
IsY[[year_interact]][[mm]]@referencePoints@FMSY[3, 6] <-  IsY[[year_interact]][[mm]]@referencePoints@FMSY[3, 5] /  as.numeric(as.character(fR[4,3]))
IsY[[year_interact]][[mm]]@referencePoints@FMSY[3, 7] <-  as.numeric(as.character(fR[4,4])) 
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------


return( list(populs, IsY) )
}
