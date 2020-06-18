# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





updateBiologicalfromXSA <-function(doALADYM_simulation, populs, IsY, source_obj, yy, mm, mort_const, year_interact) {

if (FALSE) {
populs <- Populations
source_obj <- XSAoutput
n_ass_fleet <- n_fleet_for_species 
IsY <- Interactionsyear 
mort_const <- mortality_constant
doALADYM_simulation <- ALADYM_flag
yy <- y_ord
mm <- m_int
year_interact <- y_int
}

      associated_fleetsegment <- as.vector(cfg[rownames(cfg) == paste("casestudy.S", mm, ".associatedFleetsegment", sep=""), ]) 
      associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!="" & associated_fleetsegment!="-"]
      associated_fleetsegment_indices <- which(BMT_FLEETSEGMENTS %in% associated_fleetsegment)

sexratio <- as.numeric(populs[[mm]]@sexratio)

ages_F <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", mm, ".StockAssessmentTool", sep=""),5]))
num_classes <- ages_F     # only the F life span will be taken
#print("numero di classi di XSA:")
#print(num_classes)
  
ages_F <- as.numeric(populs[[mm]]@lifespan[2,1])
num_classes_real <- ages_F    # only the F life span will be taken
#print("numero di classi reali:")
#print(num_classes_real) 

t0 <- as.numeric(populs[[mm]]@growth[2,1])
k <-  as.numeric(populs[[mm]]@growth[2,2])
linf <- as.numeric(populs[[mm]]@growth[2,3])

# ----------------------------------------------------------------------------
# READ AND ASSIGN PARAMETERS TO THE POPULATION OF SPECIES mm (ONLY IF yy = 1)
# ---------------------------------------------------------------------------- 
# if (yy ==  which(years == as.numeric(colnames(missing_SA[mm, (missing_SA[mm,] == "Y")][1]))) ) {
# maturity vector
fX <- rowMeans(data.frame(source_obj$results@mat@.Data))

first_age <-  as.numeric(names(fX)[1])


if (num_classes == num_classes_real) {
   populs[[mm]]@maturity.vect[1,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] <- fX
   populs[[mm]]@maturity.vect[2,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] <- fX
 } else {
  populs[[mm]]@maturity.vect[1,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] <- c(fX, rep(fX[num_classes], (num_classes_real - num_classes - first_age)))
  populs[[mm]]@maturity.vect[2,which(colnames(populs[[mm]]@maturity.vect) == paste("age",first_age, sep="")):num_classes_real] <-  c(fX, rep(fX[num_classes], (num_classes_real - num_classes - first_age)))
 }

# natural mortality
fX <- rowMeans(data.frame(source_obj$results@m@.Data))

 if (mort_const) {
 populs[[mm]]@M.cost[1,1] <- fX[1]
 populs[[mm]]@M.cost[2,1] <- fX[1]
} else {
  for (month in 1:length(MONTHS) ) {
  if (num_classes == num_classes_real) {
      populs[[mm]]@M.vect$M[which(rownames(populs[[mm]]@M.vect$M) == paste("age",first_age, sep="")):num_classes_real,month] <- fX 
 } else {
  populs[[mm]]@M.vect$M[which(rownames(populs[[mm]]@M.vect$M) == paste("age",first_age, sep="")):num_classes_real,month] <-  c(fX, rep(fX[num_classes], (num_classes_real - num_classes - first_age)))
  populs[[mm]]@M.vect$M[which(rownames(populs[[mm]]@M.vect$M) == paste("age",first_age, sep="")):num_classes_real,month]  <-  c(fX, rep(fX[num_classes], (num_classes_real - num_classes - first_age)))
 }
  }
  for (month in 1:length(MONTHS) ) {
  if (num_classes == num_classes_real) {
      populs[[mm]]@M.vect$F[which(rownames(populs[[mm]]@M.vect$F) == paste("age",first_age, sep="")):num_classes_real,month] <- fX 
 } else {
  populs[[mm]]@M.vect$F[which(rownames(populs[[mm]]@M.vect$F) == paste("age",first_age, sep="")):num_classes_real,month] <-  c(fX, rep(fX[num_classes], (num_classes_real - num_classes - first_age)))
  populs[[mm]]@M.vect$F[which(rownames(populs[[mm]]@M.vect$F) == paste("age",first_age, sep="")):num_classes_real,month]  <-  c(fX, rep(fX[num_classes], (num_classes_real - num_classes - first_age)))
 }
  }
}

# }

# if (!doALADYM_simulation) {

mid_ages <-  c(0:(num_classes+first_age-1)) + 0.5
mid_lengths <- c(0:(num_classes+first_age-1))
for (ag in 1:length(mid_ages)) { mid_lengths[ag] <- VB(mid_ages[ag],linf,k,t0) }

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
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
colnames(numbers_temp) <- c(paste("age", c(0:(num_classes-1+first_age)), sep="")) 
fX <- source_obj$catches[,(yy-1)*length(associated_fleetsegment_indices) + n_fl_int+1]
numbers_temp[1,which(colnames(numbers_temp) == paste("age",first_age, sep="")):(num_classes+first_age)] <- fX * 1000     # to get the numbers
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$catches@numbers <- data.frame(numbers_temp)

# catch in weight (total weight)
fX <- data.frame(source_obj$results@catch.wt@.Data)
fX <- fX[,yy]  
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$catches@totalweight <-  sum( IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$catches@numbers * fX , na.rm=T) /1000          # to get the tons

# mean length
# the mean length associated to each age (+0.5) is calculated by means of von Bertalanffy function. Then a weighted mean of mean lengths by age is calculated.
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$catches@meanLength <- sum(IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$catches@numbers * mid_lengths, na.rm=T) / sum(IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$catches@numbers, na.rm=T )
# mean weight
fX <- data.frame(source_obj$results@catch.wt@.Data)
fX <- fX[,yy]
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$catches@meanWeight <- sum(IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$catches@numbers * fX, na.rm=T) / sum(IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$catches@numbers , na.rm=T) * 1000


isthere_discard <- ifelse(data.frame(source_obj$results@discards@.Data)[1, yy] == 0, FALSE, TRUE)
if (!isthere_discard) {
# ----------------------------------------------------------------------------
# READ AND ASSIGN CATCH of Ctype=LANDINGS
# ----------------------------------------------------------------------------
# landings in numbers
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@Ctype <- "LANDINGS"
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@numbers <- IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$catches@numbers

# landings in weights
fX <- data.frame(source_obj$results@landings.wt@.Data)
fX <- fX[,yy]
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@totalweight <-  sum( IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@numbers  * fX , na.rm=T) /1000        # to get the tons

# mean length
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@meanLength <- sum(IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@numbers * mid_lengths, na.rm=T) / sum(IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@numbers , na.rm=T)

# mean weight
fX <- data.frame(source_obj$results@catch.wt@.Data)
fX <- fX[,yy]
# "weighted mean of mean weights by age; to be multiplied by 1000 to get grams; computable only for LANDINGS"
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@meanWeight <- sum(IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@numbers * fX, na.rm=T) / sum(IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@numbers, na.rm=T) * 1000
 
# ----------------------------------------------------------------------------
# READ AND ASSIGN CATCH of Ctype=DISCARDS
# ----------------------------------------------------------------------------
# discards in numbers
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$discards@Ctype <- "DISCARDS"
disc_numbers <- data.frame(matrix(rep(0,(num_classes+first_age)), nrow=1))
colnames(disc_numbers) <- c(paste("age", c(0:(num_classes+first_age-1)), sep="")) 
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$discards@numbers <- disc_numbers

# discards in weights
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$discards@totalweight <- -1 # sum( IsY[[yy]][[mm]]@interactions[[n_fl]]$discards@numbers  * fX ) /1000        # to get the tons

# mean length
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$discards@meanLength <- -1 # sum(IsY[[yy]][[mm]]@interactions[[n_fl]]$discards@numbers * mid_lengths) / sum(IsY[[yy]][[mm]]@interactions[[n_fl]]$discards@numbers )

# mean weight
# "weighted mean of mean weights by age; to be multiplied by 1000 to get grams; computable only for LANDINGS"
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$discards@meanWeight <- -1 # sum(IsY[[yy]][[mm]]@interactions[[n_fl]]$landings@numbers * fX) / sum(IsY[[yy]][[mm]]@interactions[[n_fl]]$landings@numbers)

} else {
# ----------------------------------------------------------------------------
# READ AND ASSIGN CATCH of Ctype=LANDINGS
# ----------------------------------------------------------------------------
# landings in numbers
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@Ctype <- "LANDINGS"
numbers_temp <-  data.frame(matrix(NA, ncol=(num_classes+first_age), nrow=1))
colnames(numbers_temp) <- c(paste("age", c(0:(num_classes+first_age-1)), sep="")) 
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@numbers <- data.frame(numbers_temp)

# landings in weights
fX <- data.frame(source_obj$results@landings.wt@.Data)
fX <- fX[,yy]
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@totalweight <-  -1      # to get the tons

# mean length
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@meanLength <- -1

# mean weight
#fX <- data.frame(source_obj$results@catch.wt@.Data)
#fX <- fX[,yy]
# "weighted mean of mean weights by age; to be multiplied by 1000 to get grams; computable only for LANDINGS"
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@meanWeight <- -1
 
# ----------------------------------------------------------------------------
# READ AND ASSIGN CATCH of Ctype=DISCARDS
# ----------------------------------------------------------------------------
# discards in numbers
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$landings@Ctype <- "DISCARDS"
disc_numbers <- data.frame(matrix(rep(NA,(num_classes+first_age)), nrow=1))
colnames(disc_numbers) <- c(paste("age", c(0:(num_classes+first_age-1)), sep="")) 
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$discards@numbers <- disc_numbers

# discards in weights
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$discards@totalweight <- -1 # sum( IsY[[yy]][[mm]]@interactions[[n_fl]]$discards@numbers  * fX ) /1000        # to get the tons

# mean length
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$discards@meanLength <- -1 # sum(IsY[[yy]][[mm]]@interactions[[n_fl]]$discards@numbers * mid_lengths) / sum(IsY[[yy]][[mm]]@interactions[[n_fl]]$discards@numbers )

# mean weight
# "weighted mean of mean weights by age; to be multiplied by 1000 to get grams; computable only for LANDINGS"
IsY[[year_interact]][[mm]]@interactions[[n_fl_int]]$discards@meanWeight <- -1 # sum(IsY[[yy]][[mm]]@interactions[[n_fl]]$landings@numbers * fX) / sum(IsY[[yy]][[mm]]@interactions[[n_fl]]$landings@numbers)
}

 min_ <- data.frame(matrix(source_obj$ results@range, nrow=1))[1,6]
max_ <- data.frame(matrix(source_obj$ results@range, nrow=1))[1,7]
 
 } 
 IsY[[year_interact]][[mm]]@mortalities[nFLEET,2] <-  mean(source_obj$Fbyf[(min_+1):(max_+1), (1+nFLEET+(length(BMT_FLEETSEGMENTS)*(yy-1)))], na.rm=T) #F            
}  # end catches loop

min_ <- data.frame(matrix(source_obj$ results@range, nrow=1))[1,6]
max_ <- data.frame(matrix(source_obj$ results@range, nrow=1))[1,7]
fX <- data.frame(matrix(data.frame(source_obj$ results@harvest@.Data)[,yy], nrow=1))
colnames(fX) <- c(paste("age",first_age:(num_classes+first_age-1), sep=""))
                                                                    
IsY[[year_interact]][[mm]]@mortalities[(length(BMT_FLEETSEGMENTS)+1),2] <-  mean(as.numeric(as.character(fX[which(colnames(fX) == paste("age",min_, sep="")):which(colnames(fX) == paste("age",max_, sep=""))])), na.rm=T) #F

fX <- data.frame(matrix(data.frame(source_obj$ results@m@.Data)[,yy] + data.frame(source_obj$ results@harvest@.Data)[,yy], nrow=1))
colnames(fX) <- c(paste("age",first_age:(num_classes+first_age-1), sep=""))

IsY[[year_interact]][[mm]]@mortalities[(length(BMT_FLEETSEGMENTS)+1),1]<-  mean(as.numeric(as.character(fX[which(colnames(fX) == paste("age",min_, sep="")):which(colnames(fX) == paste("age",max_, sep=""))])), na.rm=T) #Z

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# READ AND ASSIGN STOCK EXPLOITED
# ----------------------------------------------------------------------------

# stock in number
fX <- data.frame(source_obj$ results@stock.n@.Data)      
numbers_temp_ex <- data.frame(matrix(NA, nrow=(num_classes+first_age), ncol=length(MONTHS)))
rownames(numbers_temp_ex) <- c(paste("age", c(0:(num_classes+first_age-1)), sep=""))
colnames(numbers_temp_ex) <- MONTHS
for (month in 1:length(MONTHS) ) { numbers_temp_ex[which(rownames(numbers_temp_ex) == paste("age",first_age, sep="")):(num_classes+first_age),month] <- fX[,yy] }
# females
numbers_F <- numbers_temp_ex * sexratio * 1000
# males
numbers_M <- numbers_temp_ex * (1 - sexratio ) * 1000
IsY[[year_interact]][[mm]]@exploitedStock @numbers $F <- numbers_F
IsY[[year_interact]][[mm]]@exploitedStock @numbers $M <- numbers_M  

# stock weight in numbers
fX <- data.frame(source_obj$ results@stock.wt@.Data)      
weights_temp_ex <- data.frame(matrix(0, nrow=(num_classes+first_age), ncol=length(MONTHS)))
rownames(weights_temp_ex) <- c(paste("age", c(0:(num_classes+first_age-1)), sep=""))
colnames(weights_temp_ex) <- MONTHS
for (month in 1:length(MONTHS) ) { weights_temp_ex[which(rownames(weights_temp_ex) == paste("age",first_age, sep="")):(num_classes+first_age),month] <- fX[,yy] }

# females
weights_F <- weights_temp_ex * IsY[[year_interact]][[mm]]@exploitedStock @numbers $F  / 1000 # to get the grams 
# males
weights_M <- weights_temp_ex * IsY[[year_interact]][[mm]]@exploitedStock @numbers $M  / 1000 # to get the grams
IsY[[year_interact]][[mm]]@exploitedStock @SB $F <- weights_F
IsY[[year_interact]][[mm]]@exploitedStock @SB $M <- weights_M 
 
#weights_F<- Interactionsyear[[1]][[1]]@exploitedStock @SB $F
#weights_M<- Interactionsyear[[1]][[1]]@exploitedStock @SB $M

mean_by_age <- rbind(rowMeans(weights_F), rowMeans(weights_M))
mean_by_age <- colSums(mean_by_age, na.rm=T) 
IsY[[year_interact]][[mm]]@exploitedStock @annual.SB <- sum(mean_by_age, na.rm=T)

# mean weight
mean_weight <- ( sum(weights_temp_ex[,1] * (numbers_temp_ex[,1] *1000)  , na.rm=T) / sum(numbers_temp_ex[,1] *1000, na.rm=T) ) * 1000 # to get the grams  
IsY[[year_interact]][[mm]]@exploitedStock @meanWeight <- mean_weight

# mean length
IsY[[year_interact]][[mm]]@exploitedStock @meanLength <- sum(as.numeric(as.character(numbers_temp_ex[,1])) * mid_lengths, na.rm=T) / sum(as.numeric(as.character(numbers_temp_ex[,1])), na.rm=T)

# L95
IsY[[year_interact]][[mm]]@exploitedStock @L95 <- -1

# SS in numbers
# sum of the products numbers*maturity (by age)
ss_numbers_temp_males <- sum(IsY[[year_interact]][[mm]]@exploitedStock @numbers $M[,1] * populs[[mm]]@maturity.vect[1,], na.rm=T) 
ss_numbers_temp_females <- sum(IsY[[year_interact]][[mm]]@exploitedStock @numbers $F[,1] * populs[[mm]]@maturity.vect[2,], na.rm=T) 
ss_numb <- ss_numbers_temp_males + ss_numbers_temp_females
ss_numbers_temp <- data.frame(matrix(ss_numb, nrow=1, ncol=length(MONTHS)))
colnames(ss_numbers_temp) <- MONTHS
IsY[[year_interact]][[mm]]@exploitedStock @SS.numbers  <- ss_numbers_temp

# SSB
# sum of the products numbers*maturity * mean weight  (by age); to be divided by 1000
ss_weights_temp_males <- sum(IsY[[year_interact]][[mm]]@exploitedStock @numbers $M[,1] * populs[[mm]]@maturity.vect[1,] * weights_temp_ex[,1], na.rm=T)  / 1000
ss_weights_temp_females <- sum(IsY[[year_interact]][[mm]]@exploitedStock @numbers $F[,1] * populs[[mm]]@maturity.vect[2,] * weights_temp_ex[,1], na.rm=T) / 1000 
ss_weights <- ss_weights_temp_males + ss_weights_temp_females
ss_weights_temp <- data.frame(matrix(ss_weights, nrow=1, ncol=length(MONTHS)))
colnames(ss_weights_temp) <- MONTHS
IsY[[year_interact]][[mm]]@exploitedStock @SSB  <- ss_weights_temp
 
 IsY[[year_interact]][[mm]]@exploitedStock @annual.SSB  <- mean(as.numeric(ss_weights_temp))
 
# critical length
# the age corresponding to the maximum SB is selected. Then, the mean length associated to this age (+0.5) is calculated by means of reverse von Bertalanffy function.
biomass <- IsY[[year_interact]][[mm]]@exploitedStock @SB $F[,1] + IsY[[year_interact]][[mm]]@exploitedStock @SB $M[,1]
critical_age <- which(biomass == max(biomass))
IsY[[year_interact]][[mm]]@exploitedStock @criticalLength  <- mid_lengths[critical_age]

# harvest rate
# sum of total weight of all the fleet segment divided by total SB (males and females) of the first month summed up over the ages
total_weight_catches <- 0
for (n_fl_catch in 1:length(associated_fleetsegment_indices)) {
  total_weight_catches <- total_weight_catches + IsY[[year_interact]][[mm]]@interactions[[n_fl_catch]]$catches@totalweight
}
total_SB <- sum(IsY[[year_interact]][[mm]]@exploitedStock @SB$F[,1], na.rm=T) + sum(IsY[[year_interact]][[mm]]@exploitedStock @SB$M[,1], na.rm=T)
IsY[[year_interact]][[mm]]@exploitedStock @harvestRate <- total_weight_catches/total_SB

# exploitation Rate
# mean of harvest (between ages minfbar and maxfbar) divided by mean of (harvest+m) (only values between the ages minfbar and maxfbar) considering the mean over the ages
min_ <- data.frame(matrix(source_obj$ results@range, nrow=1))[1,6]
max_ <- data.frame(matrix(source_obj$ results@range, nrow=1))[1,7]
fX <- data.frame(matrix(data.frame(source_obj$ results@harvest@.Data)[,yy], nrow=1))
colnames(fX) <-  c(paste("age",first_age:(num_classes+first_age-1), sep=""))
mean_harvest <- mean(as.numeric(as.character(fX[which(colnames(fX) == paste("age",min_, sep="")):which(colnames(fX) == paste("age",max_, sep=""))])), na.rm=T)

 IsY[[year_interact]][[mm]]@mortalities$F[length(BMT_FLEETSEGMENTS)+1] <- mean_harvest
#  IsY[[yy]][[mm]]@mortalities$Z[length(BMT_FLEETSEGMENTS)+1] <- mortalities_tbl[yy,8]  ?????


fX <- data.frame(matrix(data.frame(source_obj$ results@m@.Data)[,yy] + data.frame(source_obj$ results@harvest@.Data)[,yy], nrow=1))
colnames(fX) <-  c(paste("age",first_age:(num_classes+first_age-1), sep=""))
mean_harvest_m <-  mean(as.numeric(as.character(fX[which(colnames(fX) == paste("age",min_, sep="")):which(colnames(fX) == paste("age",max_, sep=""))])), na.rm=T)

 IsY[[year_interact]][[mm]]@mortalities$Z[length(BMT_FLEETSEGMENTS)+1] <- mean_harvest_m

IsY[[year_interact]][[mm]]@exploitedStock @exploitationRate <- mean_harvest/mean_harvest_m

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# READ AND ASSIGN STOCK UNEXPLOITED
# ----------------------------------------------------------------------------
# stock in number    
numbers_temp_unex <- data.frame(matrix(NA, nrow=(num_classes+first_age), ncol=length(MONTHS)))
rownames(numbers_temp_unex) <- c(paste("age", c(0:(num_classes+first_age-1)), sep=""))
colnames(numbers_temp_unex) <- MONTHS
IsY[[year_interact]][[mm]]@unexploitedStock @numbers $F <- numbers_temp_unex
IsY[[year_interact]][[mm]]@unexploitedStock @numbers $M <- numbers_temp_unex  

# stock weight in numbers
weights_temp <- data.frame(matrix(NA, nrow=(num_classes+first_age), ncol=length(MONTHS)))
rownames(weights_temp) <- c(paste("age", c(0:(num_classes+first_age-1)), sep=""))
colnames(weights_temp) <- MONTHS
IsY[[year_interact]][[mm]]@unexploitedStock @SB $F <- weights_temp
IsY[[year_interact]][[mm]]@unexploitedStock @SB $M <- weights_temp 
 
# mean weight
IsY[[year_interact]][[mm]]@unexploitedStock @meanWeight <- -1

# mean length
IsY[[year_interact]][[mm]]@unexploitedStock @meanLength <- -1

# L95
IsY[[year_interact]][[mm]]@unexploitedStock @L95 <- -1

# SS in numbers
ss_numbers_temp <- data.frame(matrix(NA, nrow=1, ncol=length(MONTHS)))
colnames(ss_numbers_temp) <- MONTHS
IsY[[year_interact]][[mm]]@unexploitedStock @SS.numbers  <- ss_numbers_temp

# SSB
ss_weights_temp <- data.frame(matrix(NA, nrow=1, ncol=length(MONTHS)))
colnames(ss_weights_temp) <- MONTHS
IsY[[year_interact]][[mm]]@unexploitedStock @SSB  <- ss_weights_temp

# critical length
IsY[[year_interact]][[mm]]@unexploitedStock @criticalLength  <- -1

# harvest rate
IsY[[year_interact]][[mm]]@unexploitedStock @harvestRate  <- 0

# exploitation rate
IsY[[year_interact]][[mm]]@unexploitedStock @exploitationRate  <- 0

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------



# ----------------------------------------------------------------------------
# READ AND ASSIGN VALUES TO THE OTHER VARIABLES OF THE INTERACTION
# ----------------------------------------------------------------------------
# mean length for total catches
#tot_numb_catch <- rep(0, num_classes)
#for (fl_n_int in 1:length(associated_fleetsegment_indices)) {
#tot_numb_catch <- tot_numb_catch + as.numeric(as.character(IsY[[yy]][[mm]]@interactions[[fl_n_int]]$catches@numbers))
#}
# IsY[[yy]][[mm]]@meanLength_catches <- sum(tot_numb_catch * mid_lengths) / sum(tot_numb_catch)
#
## mean weight
## "weighted mean of mean weights by age; to be multiplied by 1000 to get grams"
## mean weight
#fX <- data.frame(source_obj$results@catch.wt@.Data)
#fX <- fX[,yy]
## "weighted mean of mean weights by age; to be multiplied by 1000 to get grams"
#IsY[[yy]][[mm]]@meanWeight_catches <- sum(tot_numb_catch * fX) / sum(tot_numb_catch) * 1000
#
## L95
#IsY[[yy]][[mm]]@L95_catches <- -1

# } # end if doALADYM_simulation

# --------------------------------------------------------------------------
# TOTAL CATCHES
# --------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# READ AND ASSIGN CATCH of Ctype=CATCHES
# ----------------------------------------------------------------------------
# catch in numbers 
IsY[[year_interact]][[mm]]@totalcatch@Ctype <- "CATCHES"  
numbers_temp <-  data.frame(matrix(0, ncol=(num_classes+first_age), nrow=1)) 
colnames(numbers_temp) <- c(paste("age", c(0:(num_classes+first_age-1)), sep="")) 
fX <- data.frame(source_obj$results@catch.n@.Data)[,yy]
numbers_temp[1,which(colnames(numbers_temp) == paste("age",first_age, sep="")):(num_classes+first_age)] <- fX * 1000     # to get the numbers

IsY[[year_interact]][[mm]]@totalcatch@numbers <- data.frame(numbers_temp)

fX <- data.frame(source_obj$results@catch@.Data)[,yy] /1000
IsY[[year_interact]][[mm]]@totalcatch@totalweight <- fX

# mean length
# the mean length associated to each age (+0.5) is calculated by means of von Bertalanffy function. Then a weighted mean of mean lengths by age is calculated.
IsY[[year_interact]][[mm]]@totalcatch@meanLength <- sum(IsY[[year_interact]][[mm]]@totalcatch@numbers * mid_lengths, na.rm=T) / sum(IsY[[year_interact]][[mm]]@totalcatch@numbers , na.rm=T)
IsY[[year_interact]][[mm]]@meanLength_catches <-  IsY[[year_interact]][[mm]]@totalcatch@meanLength 

# mean weight
fX <- data.frame(source_obj$results@catch.wt@.Data)
fX <- fX[,yy]
IsY[[year_interact]][[mm]]@totalcatch@meanWeight <- sum(IsY[[year_interact]][[mm]]@totalcatch@numbers * fX, na.rm=T) / sum(IsY[[year_interact]][[mm]]@totalcatch@numbers , na.rm=T) * 1000
IsY[[year_interact]][[mm]]@meanWeight_catches <- IsY[[year_interact]][[mm]]@totalcatch@meanWeight

# --------------------------------------------------------------------------
# TOTAL LANDINGS
# --------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# READ AND ASSIGN CATCH of Ctype=LANDINGS
# ----------------------------------------------------------------------------
# catch in numbers
IsY[[year_interact]][[mm]]@totallanding@Ctype <- "LANDINGS"    
numbers_temp <-  data.frame(matrix(0, ncol=(num_classes+first_age), nrow=1)) 
colnames(numbers_temp) <- c(paste("age", c(0:(num_classes+first_age-1)), sep="")) 
fX <- data.frame(source_obj$results@landings.n@.Data)[,yy]
numbers_temp[1,which(colnames(numbers_temp) == paste("age",first_age, sep="")):(num_classes+first_age)] <- fX * 1000     # to get the numbers
IsY[[year_interact]][[mm]]@totallanding@numbers <- data.frame(numbers_temp)

fX <- data.frame(source_obj$results@landings@.Data)[,yy] /1000
IsY[[year_interact]][[mm]]@totallanding@totalweight <- fX

# mean length
# the mean length associated to each age (+0.5) is calculated by means of von Bertalanffy function. Then a weighted mean of mean lengths by age is calculated.
IsY[[year_interact]][[mm]]@totallanding@meanLength <- sum(IsY[[year_interact]][[mm]]@totallanding@numbers * mid_lengths, na.rm=T) / sum(IsY[[year_interact]][[mm]]@totallanding@numbers , na.rm=T)
# mean weight
fX <- data.frame(source_obj$results@landings.wt@.Data)
fX <- fX[,yy]
IsY[[year_interact]][[mm]]@totallanding@meanWeight <- sum(IsY[[year_interact]][[mm]]@totallanding@numbers * fX, na.rm=T) / sum(IsY[[year_interact]][[mm]]@totallanding@numbers , na.rm=T) * 1000



# --------------------------------------------------------------------------
# TOTAL DISCARDS
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# READ AND ASSIGN CATCH of Ctype=DISCARDS
# --------------------------------------------------------------------------
# catch in numbers

IsY[[year_interact]][[mm]]@totaldiscard@Ctype <- "DISCARDS"  
numbers_temp <-  data.frame(matrix(0, ncol=(num_classes+first_age), nrow=1)) 
colnames(numbers_temp) <- c(paste("age", c(0:(num_classes+first_age-1)), sep="")) 
fX <- data.frame(source_obj$results@discards.n@.Data)[,yy]
numbers_temp[1,which(colnames(numbers_temp) == paste("age",first_age, sep="")):(num_classes+first_age)] <- fX * 1000     # to get the numbers
IsY[[year_interact]][[mm]]@totaldiscard@numbers <- data.frame(numbers_temp)

fX <- data.frame(source_obj$results@discards@.Data)[,yy] /1000
IsY[[year_interact]][[mm]]@totaldiscard@totalweight <- fX

# mean length
# the mean length associated to each age (+0.5) is calculated by means of von Bertalanffy function. Then a weighted mean of mean lengths by age is calculated.
IsY[[year_interact]][[mm]]@totaldiscard@meanLength <- sum(IsY[[year_interact]][[mm]]@totaldiscard@numbers * mid_lengths, na.rm=T) / sum(IsY[[year_interact]][[mm]]@totaldiscard@numbers , na.rm=T)
# mean weight
fX <- data.frame(source_obj$results@discards.wt@.Data)
fX <- fX[,yy]
IsY[[year_interact]][[mm]]@totaldiscard@meanWeight <- sum(IsY[[year_interact]][[mm]]@totaldiscard@numbers * fX, na.rm=T) / sum(IsY[[year_interact]][[mm]]@totaldiscard@numbers, na.rm=T) * 1000


# ----------------------------------------------------------------------------
# READ AND ASSIGN VALUES TO THE REFERENCE POINTS
# ----------------------------------------------------------------------------

min_ <- data.frame(matrix(source_obj$ results@range, nrow=1))[1,6]
max_ <- data.frame(matrix(source_obj$ results@range, nrow=1))[1,7]
fX <- data.frame(matrix(data.frame(source_obj$ results@harvest@.Data)[,yy],nrow=1))
colnames(fX) <-  c(paste("age",first_age:(num_classes+first_age-1), sep=""))
mean_harvest <- mean(as.numeric(as.character(fX[which(colnames(fX) == paste("age",min_, sep="")):which(colnames(fX) == paste("age",max_, sep=""))])), na.rm=T)


fX <- data.frame(source_obj$ referencepoints)
fXrownames <-  fX[, 1]
fX <- fX[,2:ncol(fX)]
rownames(fX) <- fXrownames
# reference points
# the values are repeated for all the years; the value of factor is calculated as ratio between F and the mean of harvest (between ages minfbar and maxfbar)
 # mean_harvest
#rp_temp_0.1 <-  data.frame(matrix(NA, ncol=7, nrow=1))
#colnames(rp_temp_0.1) <- c("factor", "F", "Y", "Y_R", "B", "B_R", "SSB")
                              # data.frame	values by items [factor, Y, Y/R, B, B/R, SSB]
IsY[[year_interact]][[mm]]@referencePoints@F0.1[2, 1] <- as.numeric(as.character(fX[1,1]))/ mean_harvest # factor
IsY[[year_interact]][[mm]]@referencePoints@F0.1[2, 2] <- as.numeric(as.character(fX[1,1])) # F
IsY[[year_interact]][[mm]]@referencePoints@F0.1[2, 3] <- as.numeric(as.character(fX[1,2])) # Total.yield
IsY[[year_interact]][[mm]]@referencePoints@F0.1[2, 5] <- as.numeric(as.character(fX[1,5])) # Biomass
IsY[[year_interact]][[mm]]@referencePoints@F0.1[2, 7] <- as.numeric(as.character(fX[1,4])) # SSB

#rp_temp_max <-  data.frame(matrix(NA, ncol=7, nrow=1))
#colnames(rp_temp_max) <- c("factor", "F", "Y", "Y_R", "B", "B_R", "SSB")
#                               # data.frame	values by items [factor, Y, Y/R, B, B/R, SSB]
IsY[[year_interact]][[mm]]@referencePoints@Fmax[2, 1] <- as.numeric(as.character(fX[2,1]))/ mean_harvest # factor
IsY[[year_interact]][[mm]]@referencePoints@Fmax[2, 2] <- as.numeric(as.character(fX[2,1])) # F
IsY[[year_interact]][[mm]]@referencePoints@Fmax[2, 3] <- as.numeric(as.character(fX[2,2])) # Total.yield
IsY[[year_interact]][[mm]]@referencePoints@Fmax[2, 5] <- as.numeric(as.character(fX[2,5])) # Biomass
IsY[[year_interact]][[mm]]@referencePoints@Fmax[2, 7] <- as.numeric(as.character(fX[2,4])) # SSB

#rp_temp_FMSY<-  data.frame(matrix(NA, ncol=7, nrow=1))
#colnames(rp_temp_FMSY) <- c("factor", "F", "Y", "Y_R", "B", "B_R", "SSB")
#                               # data.frame	values by items [factor, Y, Y/R, B, B/R, SSB]
IsY[[year_interact]][[mm]]@referencePoints@FMSY[2, 1] <- as.numeric(as.character(fX[4,1]))/ mean_harvest # factor
IsY[[year_interact]][[mm]]@referencePoints@FMSY[2, 2] <- as.numeric(as.character(fX[4,1])) # F
IsY[[year_interact]][[mm]]@referencePoints@FMSY[2, 3] <- as.numeric(as.character(fX[4,2])) # Total.yield
IsY[[year_interact]][[mm]]@referencePoints@FMSY[2, 5] <- as.numeric(as.character(fX[4,5])) # Biomass
IsY[[year_interact]][[mm]]@referencePoints@FMSY[2, 7] <- as.numeric(as.character(fX[4,4])) # SSB  
      
#      IsY[[yy]][[mm]]@referencePoints <- new(Class= "bmtBioreferencepoint", 
#                                                        F0.1 = rp_temp_0.1,
#                                                        FMSY = rp_temp_FMSY,
#                                                        Fmax = rp_temp_max ) 

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------


return(list(populs,IsY) )
}

 
# funzione  von Bertalanffy:
VB <- function(age,Linf,K,t0)
{
   L <- Linf * (1 - exp(-K * (age - t0)))
   return(L)
}