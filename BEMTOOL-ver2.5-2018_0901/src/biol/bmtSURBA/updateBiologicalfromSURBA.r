# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





updateBiologicalfromSURBA <-function(populs, IsY, source_obj, yy, mm, mort_const, year_interact) {
if (FALSE) {
populs <- Populations
source_obj <- SURBARes
IsY <- Interactionsyear
mort_const <- mortality_constant
mm <- m_int
yy <- y_ord
}
      associated_fleetsegment <- as.vector(cfg[rownames(cfg) == paste("casestudy.S", mm, ".associatedFleetsegment", sep=""), ]) 
      associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!="" & associated_fleetsegment!="-"]
      associated_fleetsegment_indices <- which(BMT_FLEETSEGMENTS %in% associated_fleetsegment)
      n_ass_fleet <- length(associated_fleetsegment_indices)
# ----------------------------------------------------------------------------
# READ AND ASSIGN PARAMETERS TO THE POPULATION OF SPECIES mm (ONLY IF yy = 1)
# ---------------------------------------------------------------------------- 
sexratio <- as.numeric(populs[[mm]]@sexratio)
num_classes <- length(source_obj$age_classes)
  
ages_F <- as.numeric(populs[[mm]]@lifespan[2,1])
ages_M <- as.numeric(populs[[mm]]@lifespan[1,1])

num_classes_real <- max(ages_F, ages_M)  

if (yy == 1) {
# update maturity vector
if (exists("fS")) { rm(fS)}
for (cl in 2:ncol(source_obj$maturity)) {
  if (!exists("fS")) { fS <- mean(as.numeric(as.character(source_obj$maturity[,cl])) ) } else { fS <- c(fS, mean(as.numeric(as.character(source_obj$maturity[,cl] )))  ) }
}
#if (sex == "C" ) {

if (num_classes == num_classes_real) {
 populs[[mm]]@maturity.vect[1,] <- fS
 populs[[mm]]@maturity.vect[2,] <- fS
 } else {
 populs[[mm]]@maturity.vect[1,] <- c(fS, rep(fS[num_classes], (num_classes_real - num_classes)))
 populs[[mm]]@maturity.vect[2,] <-  c(fS, rep(fS[num_classes], (num_classes_real - num_classes)))
 }
#}

# natural mortality
# difference between Z and total F by age considering the first value of the natural mortality vector; same values for males and females

if (exists("fS_M")) { rm(fS_M)}
for (cl in 2:ncol(source_obj$natural_mortality)) {
  if (!exists("fS_M")) { fS_M <- mean( as.numeric(as.character(source_obj$natural_mortality[,cl])) ) } else { fS_M <- c(fS_M, mean( as.numeric(as.character(source_obj$natural_mortality[,cl] )) ) ) }
}

if (mort_const) {
 populs[[mm]]@M.cost[1,1] <- fS_M[1]
 populs[[mm]]@M.cost[2,1] <- fS_M[1]
} else {
  for (month in 1:length(MONTHS) ) { 
   if (num_classes == num_classes_real) {
  populs[[mm]]@M.vect$M[,month] <- fS_M 
   } else {
    populs[[mm]]@M.vect$M[,month] <- c(fS_M, rep(fS_M[num_classes], (num_classes_real - num_classes)))  
   }
  }
  for (month in 1:length(MONTHS) ) { 
   if (num_classes == num_classes_real) {
  populs[[mm]]@M.vect$F[,month] <- fS_M 
  } else {
    populs[[mm]]@M.vect$F[,month] <- c(fS_M, rep(fS_M[num_classes], (num_classes_real - num_classes)))
  }
  }
#}
}

}  # end if first year

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

for (cl in 2:ncol(source_obj$total_mortality)) {
  if (!exists("fS")) { fS <- mean( as.numeric(as.character(source_obj$natural_mortality[,cl])))  } else { fS <- c(fS, mean( as.numeric(as.character(source_obj$natural_mortality[,cl] )) ))  }
}

IsY[[year_interact]][[mm]]@mortalities[(length(BMT_FLEETSEGMENTS)+1),1] <- as.numeric(as.character(source_obj$total_mortality[yy,5])) # Z

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------


return( list(populs, IsY) )
}
