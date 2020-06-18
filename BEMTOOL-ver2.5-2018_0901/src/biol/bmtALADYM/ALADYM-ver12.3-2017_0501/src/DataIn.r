# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



#
# Read data from file
#
# This module can be replaced to suit different input styles
#

if (showCompTime)  {
  DataIn_ptm <- proc.time()
}

INP$Year_simulation             <- length(years) + length(years_forecast)
GLO$L_number  <- INP$Time_slice * INP$Year_simulation      # numero di mesi della simulazione     

loca_fileName <- "datain.dat"
                                                                                     
                                                                                     # assegnare il numero corrispondente alla distribuzione scelta
RND$R_fun                  <- which(DISTRIBUTION == new_aldSimulation@R.100runs$distribution[1])# reclutamento IN MIGLIAIA funzione di distribuzione di probabilità e suoi parametri                                                                          
RND$R_min                  <- as.numeric(new_aldSimulation@R.100runs$min[1])
RND$R_max                  <- as.numeric(new_aldSimulation@R.100runs$max[1])
RND$R_a                    <- as.numeric(new_aldSimulation@R.100runs$A[1])
RND$R_b                    <- as.numeric(new_aldSimulation@R.100runs$B[1])  
                                                                                         # distribuzione del K della VBF dei maschi
RND$MGrowth_K_fun           <- which(DISTRIBUTION == data.frame(new_aldPopulation@growth)$distribution[rownames(data.frame(new_aldPopulation@growth)) == "M.K"] )
RND$MGrowth_K_min           <- as.numeric(data.frame(new_aldPopulation@growth)$min[rownames(data.frame(new_aldPopulation@growth)) == "M.K"])
RND$MGrowth_K_max           <- as.numeric(data.frame(new_aldPopulation@growth)$max[rownames(data.frame(new_aldPopulation@growth)) == "M.K"])
RND$MGrowth_K_a             <- as.numeric(data.frame(new_aldPopulation@growth)$A[rownames(data.frame(new_aldPopulation@growth)) == "M.K"])
RND$MGrowth_K_b             <- as.numeric(data.frame(new_aldPopulation@growth)$B[rownames(data.frame(new_aldPopulation@growth)) == "M.K"])

                                                                                         #    distribuzione del Linf della VBF dei maschi
RND$MGrowth_Linf_fun        <- which(DISTRIBUTION == data.frame(new_aldPopulation@growth)$distribution[rownames(data.frame(new_aldPopulation@growth)) == "M.Linf"] )
RND$MGrowth_Linf_min        <- as.numeric(data.frame(new_aldPopulation@growth)$min[rownames(data.frame(new_aldPopulation@growth)) == "M.Linf"])
RND$MGrowth_Linf_max        <- as.numeric(data.frame(new_aldPopulation@growth)$max[rownames(data.frame(new_aldPopulation@growth)) == "M.Linf"])
RND$MGrowth_Linf_a          <- as.numeric(data.frame(new_aldPopulation@growth)$A[rownames(data.frame(new_aldPopulation@growth)) == "M.Linf"])
RND$MGrowth_Linf_b          <- as.numeric(data.frame(new_aldPopulation@growth)$B[rownames(data.frame(new_aldPopulation@growth)) == "M.Linf"])

                                                                                          #distribuz Lm50 maschi
RND$MML50p_fun              <- which(DISTRIBUTION == data.frame(new_aldPopulation@maturityogive)$distribution[rownames(data.frame(new_aldPopulation@maturityogive)) == "M.L50"] )
RND$MML50p_min              <- as.numeric(data.frame(new_aldPopulation@maturityogive)$min[rownames(data.frame(new_aldPopulation@maturityogive)) == "M.L50"])
RND$MML50p_max              <- as.numeric(data.frame(new_aldPopulation@maturityogive)$max[rownames(data.frame(new_aldPopulation@maturityogive)) == "M.L50"])
RND$MML50p_a                <- as.numeric(data.frame(new_aldPopulation@maturityogive)$A[rownames(data.frame(new_aldPopulation@maturityogive)) == "M.L50"])
RND$MML50p_b                <- as.numeric(data.frame(new_aldPopulation@maturityogive)$B[rownames(data.frame(new_aldPopulation@maturityogive)) == "M.L50"])

                                                                                          #distribuz maturity range maschi
RND$MML75pL25p_fun          <- which(DISTRIBUTION == data.frame(new_aldPopulation@maturityogive)$distribution[rownames(data.frame(new_aldPopulation@maturityogive)) == "M.L75L25"] )
RND$MML75pL25p_min          <- as.numeric(data.frame(new_aldPopulation@maturityogive)$min[rownames(data.frame(new_aldPopulation@maturityogive)) == "M.L75L25"])
RND$MML75pL25p_max          <- as.numeric(data.frame(new_aldPopulation@maturityogive)$max[rownames(data.frame(new_aldPopulation@maturityogive)) == "M.L75L25"])
RND$MML75pL25p_a            <- as.numeric(data.frame(new_aldPopulation@maturityogive)$A[rownames(data.frame(new_aldPopulation@maturityogive)) == "M.L75L25"])
RND$MML75pL25p_b            <- as.numeric(data.frame(new_aldPopulation@maturityogive)$B[rownames(data.frame(new_aldPopulation@maturityogive)) == "M.L75L25"])

                                                                                       # distribuz e parametri K della VBF femmine
RND$FGrowth_K_fun           <- which(DISTRIBUTION == data.frame(new_aldPopulation@growth)$distribution[rownames(data.frame(new_aldPopulation@growth)) == "F.K"] )
RND$FGrowth_K_min           <- as.numeric(data.frame(new_aldPopulation@growth)$min[rownames(data.frame(new_aldPopulation@growth)) == "F.K"])
RND$FGrowth_K_max           <- as.numeric(data.frame(new_aldPopulation@growth)$max[rownames(data.frame(new_aldPopulation@growth)) == "F.K"])
RND$FGrowth_K_a             <- as.numeric(data.frame(new_aldPopulation@growth)$A[rownames(data.frame(new_aldPopulation@growth)) == "F.K"])
RND$FGrowth_K_b             <- as.numeric(data.frame(new_aldPopulation@growth)$B[rownames(data.frame(new_aldPopulation@growth)) == "F.K"])

                                                                                          # Linf distribuz e parametri
RND$FGrowth_Linf_fun        <- which(DISTRIBUTION == data.frame(new_aldPopulation@growth)$distribution[rownames(data.frame(new_aldPopulation@growth)) == "F.Linf"] )
RND$FGrowth_Linf_min        <- as.numeric(data.frame(new_aldPopulation@growth)$min[rownames(data.frame(new_aldPopulation@growth)) == "F.Linf"])
RND$FGrowth_Linf_max        <- as.numeric(data.frame(new_aldPopulation@growth)$max[rownames(data.frame(new_aldPopulation@growth)) == "F.Linf"])
RND$FGrowth_Linf_a          <- as.numeric(data.frame(new_aldPopulation@growth)$A[rownames(data.frame(new_aldPopulation@growth)) == "F.Linf"])
RND$FGrowth_Linf_b          <- as.numeric(data.frame(new_aldPopulation@growth)$B[rownames(data.frame(new_aldPopulation@growth)) == "F.Linf"])

                                                                                         #Lm50 distribuz e parametri
RND$FML50p_fun              <- which(DISTRIBUTION == data.frame(new_aldPopulation@maturityogive)$distribution[rownames(data.frame(new_aldPopulation@maturityogive)) == "F.L50"] )
RND$FML50p_min              <- as.numeric(data.frame(new_aldPopulation@maturityogive)$min[rownames(data.frame(new_aldPopulation@maturityogive)) == "F.L50"])
RND$FML50p_max              <- as.numeric(data.frame(new_aldPopulation@maturityogive)$max[rownames(data.frame(new_aldPopulation@maturityogive)) == "F.L50"])
RND$FML50p_a                <- as.numeric(data.frame(new_aldPopulation@maturityogive)$A[rownames(data.frame(new_aldPopulation@maturityogive)) == "F.L50"])
RND$FML50p_b                <- as.numeric(data.frame(new_aldPopulation@maturityogive)$B[rownames(data.frame(new_aldPopulation@maturityogive)) == "F.L50"])

                                                                                       #Maturity range distribuz e parametri
RND$FML75pL25p_fun          <- which(DISTRIBUTION == data.frame(new_aldPopulation@maturityogive)$distribution[rownames(data.frame(new_aldPopulation@maturityogive)) == "F.L75L25"] )
RND$FML75pL25p_min          <- as.numeric(data.frame(new_aldPopulation@maturityogive)$min[rownames(data.frame(new_aldPopulation@maturityogive)) == "F.L75L25"])
RND$FML75pL25p_max          <- as.numeric(data.frame(new_aldPopulation@maturityogive)$max[rownames(data.frame(new_aldPopulation@maturityogive)) == "F.L75L25"])
RND$FML75pL25p_a            <- as.numeric(data.frame(new_aldPopulation@maturityogive)$A[rownames(data.frame(new_aldPopulation@maturityogive)) == "F.L75L25"])
RND$FML75pL25p_b            <- as.numeric(data.frame(new_aldPopulation@maturityogive)$B[rownames(data.frame(new_aldPopulation@maturityogive)) == "F.L75L25"])


#------------------------------------------------------------------
# Stochastic Value Parameters
#------------------------------------------------------------------

# Growth related

INP$MGrowth_Linf_min           <- RND$MGrowth_Linf_min
INP$MGrowth_Linf_max           <- RND$MGrowth_Linf_max
INP$MGrowth_K_min              <- RND$MGrowth_K_min
INP$MGrowth_K_max              <- RND$MGrowth_K_max

                                                                                              #vita media dei maschi
INP$MGrowth_t0_min              <- as.numeric(data.frame(new_aldPopulation@growth)$min[rownames(data.frame(new_aldPopulation@growth)) == "M.t0"])
INP$MGrowth_t0_max              <- as.numeric(data.frame(new_aldPopulation@growth)$max[rownames(data.frame(new_aldPopulation@growth)) == "M.t0"])
INP$MGrowth_tend                <- as.numeric(data.frame(new_aldPopulation@lifespan)$lifespan[rownames(data.frame(new_aldPopulation@lifespan)) == "M"])


INP$FGrowth_Linf_min           <- RND$FGrowth_Linf_min
INP$FGrowth_Linf_max           <- RND$FGrowth_Linf_max
INP$FGrowth_K_min              <- RND$FGrowth_K_min
INP$FGrowth_K_max              <- RND$FGrowth_K_max

                                                                                                #vita media delle femmine
INP$FGrowth_t0_min              <- as.numeric(data.frame(new_aldPopulation@growth)$min[rownames(data.frame(new_aldPopulation@growth)) == "F.t0"])
INP$FGrowth_t0_max              <- as.numeric(data.frame(new_aldPopulation@growth)$max[rownames(data.frame(new_aldPopulation@growth)) == "F.t0"])
INP$FGrowth_tend                <- as.numeric(data.frame(new_aldPopulation@lifespan)$lifespan[rownames(data.frame(new_aldPopulation@lifespan)) == "F"])

INP$Survivability_monthly_vec   <-  rep(as.numeric(as.character(new_aldSimulation@monthlysurvivability)), length(years))

# Maturity ogive related

INP$MML50p_min                 <- RND$MML50p_min
INP$MML50p_max                 <- RND$MML50p_max
INP$MML75pL25p_min             <- RND$MML75pL25p_min
INP$MML75pL25p_max             <- RND$MML75pL25p_max

INP$FML50p_min                 <- RND$FML50p_min
INP$FML50p_max                 <- RND$FML50p_max
INP$FML75pL25p_min             <- RND$FML75pL25p_min
INP$FML75pL25p_max             <- RND$FML75pL25p_max


# Recruitment equation
INP$FRLa                    <- as.numeric(data.frame(new_aldSimulation@stockr.relationship)$a[1])
INP$FRLb                    <- as.numeric(data.frame(new_aldSimulation@stockr.relationship)$b[1])
INP$FRLc                    <- as.numeric(data.frame(new_aldSimulation@stockr.relationship)$c[1])
INP$FRLt                    <- which(SR_TYPE == data.frame(new_aldSimulation@stockr.relationship)$relationship[1])

# unit for Spawners
INP$S_unit                  <- which(SR_UNIT == new_aldSimulation@stockr.unit)


INP$Fertility_Rate             <- vector(mode = "numeric", length = 12)                  # GUI: ciclo sulla lista offspring
for (y in 1:length(MONTHS)) {
  INP$Fertility_Rate[ y] <- data.frame(new_aldSimulation@monthlyoffspring)[1,y]
}
INP$Fertility_Rate_inp<- INP$Fertility_Rate

INP$Fertility_Delay             <- as.numeric(new_aldSimulation@spawners.delayss)    

#
# Rotate to use modulo 12 arithmetic
#
loca_temp <- INP$Fertility_Rate[12]
for(loca_i in (11:1)) {
  INP$Fertility_Rate[loca_i + 1] <- INP$Fertility_Rate[loca_i]         # si tiene conto del ritardo di un mese e si riscrive il vettore delle fertilità, traslando il tutto di un mese
}
INP$Fertility_Rate[1] <- loca_temp


# -------------------------------------------------------------------
#Simulation Parameters
#-------------------------------------------------------------------

INP$Time_slice                  <- as.numeric(new_aldSimulation@time_slice_year)
INP$Year_simulation             <- length(years) + length(years_forecast)

INP$Life_before                 <- as.numeric(new_aldSimulation@presimulation_years)

INP$Fertility_Rate_esteso <- c(mean(INP$Fertility_Rate),rep(INP$Fertility_Rate,INP$Year_simulation))  
 
 INP$tr <- as.numeric(new_aldSimulation@Tr)
 #INP$tr <- tr

GLO$MC_number <- INP$Time_slice * INP$MGrowth_tend + 1 - INP$tr  # numero di mesi di vita 
GLO$FC_number <- INP$Time_slice * INP$FGrowth_tend + 1 - INP$tr

GLO$L_number  <- INP$Time_slice * INP$Year_simulation      # numero di mesi della simulazione 
GLO$P_number  <- INP$Time_slice * INP$Life_before

#-------------------------------------------------------------------
# Variable Length Parameters
#-------------------------------------------------------------------

# Mortality

INP$MOPT_M_TYPE <- which(MORTALITY_TYPE == new_aldSimulation@naturalmortality.M.type)

if (INP$MOPT_M_TYPE == 1) {
   INP$MM_fixed <- as.numeric(new_aldSimulation@naturalmortality.M.constant)
} else if( INP$MOPT_M_TYPE == 3) {
  INP$MM_vector                <- vector(mode = "numeric", length = GLO$MC_number)
  for(loca_i in 1:GLO$MC_number) {
	        INP$MM_vector[loca_i]     <- data.frame(new_aldSimulation@naturalmortality.M.vector)[loca_i, 2]
  }
} else if (INP$MOPT_M_TYPE == 4) {
    INP$MMtmax = as.numeric(as.character(new_aldSimulation@naturalmortality.M.tmax))
    l50M=mean(c(INP$MML50p_min, INP$MML50p_max))
    tmaxM=INP$MGrowth_tend - 1
} else {
    INP$MMtmax=NA
    l50M=NA
    tmaxM=NA
}


INP$FOPT_M_TYPE <- which(MORTALITY_TYPE == new_aldSimulation@naturalmortality.F.type)

if (INP$FOPT_M_TYPE == 1) {
INP$FM_fixed                <- as.numeric(new_aldSimulation@naturalmortality.F.constant)
} else if ( INP$FOPT_M_TYPE == 3) {
  INP$FM_vector                <- vector(mode = "numeric", length = GLO$FC_number)
  for(loca_i in 1:GLO$FC_number) {
	     INP$FM_vector[loca_i]     <- data.frame(new_aldSimulation@naturalmortality.F.vector)[loca_i, 2]
  }
} else if ( INP$FOPT_M_TYPE == 4 ) { # Prodbiom da cancellare una volta che si e inserita la cella nell'interfaccia
    INP$FMtmax = as.numeric(as.character(new_aldSimulation@naturalmortality.F.tmax))
    l50F=mean(c(INP$FML50p_min, INP$FML50p_max))
    tmaxF=INP$FGrowth_tend - 1
} else {
    INP$FMtmax=NA
    l50F=NA
    tmaxF=NA
}

# Fmax
if ( as.character(new_aldSimulation@enteringMortality) == "Z" )  {
INP$OPT_F_TYPE <- 1
} else {
INP$OPT_F_TYPE <- 2
}

n_ages_M <- INP$MGrowth_tend  
n_ages_F <- INP$FGrowth_tend  
first_age_mal <- 0
first_age_fem <- 0


    n_ages_M <- n_ages_M - trunc(INP$tr/12)
    first_age_mal <- trunc(INP$tr/12)
    n_ages_F <- n_ages_F - trunc(INP$tr/12)
    first_age_fem <- trunc(INP$tr/12)

 l_inf_M <- INP$MGrowth_Linf_max 
  l_inf_F <- INP$FGrowth_Linf_max 

l_inf_lens_M <-c(0:(round(l_inf_M,0)+1))
l_inf_lens_F <-c(0:(round(l_inf_F,0)+1))

print(paste("Life span: males", n_ages_M, "females", n_ages_F) ) 
 
nrow_males <- length(years)*n_ages_M        #
nrow_females <- length(years)*n_ages_F       #

nrow_males_lenXsel <- length(years)*length(l_inf_lens_M )       #
nrow_females_lenXsel <- length(years)*length(l_inf_lens_F)       #

INP$Sel_vector <- data.frame(matrix(nrow=(nrow_males + nrow_females), ncol=(3+length(FLEETSEGMENTS_names))))
                              # c(c(first_age_mal:(n_ages_M+first_age_mal-1)) , c(first_age_fem:(n_ages_F+first_age_fem-1)))
INP$Sel_vector_len <- data.frame(matrix(nrow=(nrow_males_lenXsel + nrow_females_lenXsel), ncol=(3+length(FLEETSEGMENTS_names))))						  
							  
INP$Sel_vector[,1] <- c(rep(c(first_age_mal:(n_ages_M+first_age_mal-1)), length(years)), rep(c(first_age_fem:(n_ages_F+first_age_fem-1)), length(years)))  
INP$Sel_vector_len[,1] <- c(rep(l_inf_lens_M, length(years)), rep(l_inf_lens_F, length(years)))

lab_year <- rep(years,n_ages_M)
lab_year <- lab_year[order(lab_year)]  # M
INP$Sel_vector[1:nrow_males,2] <- lab_year

lab_year <- rep(years,n_ages_F)
lab_year <- lab_year[order(lab_year)]  # F
INP$Sel_vector[(nrow_males+1):(nrow_males+nrow_females),2] <- lab_year

lab_year <- rep(years,length(l_inf_lens_M))
lab_year <- lab_year[order(lab_year)]  # M
INP$Sel_vector_len[1:nrow_males_lenXsel,2] <- lab_year

lab_year <- rep(years,length(l_inf_lens_F))
lab_year <- lab_year[order(lab_year)]  # F
INP$Sel_vector_len[(nrow_males_lenXsel+1):(nrow_males_lenXsel+nrow_females_lenXsel),2] <- lab_year


INP$Fm <- data.frame(matrix(nrow=(nrow_males + nrow_females), ncol=(3+length(FLEETSEGMENTS_names))))
                              # c(c(first_age_mal:(n_ages_M+first_age_mal-1)) , c(first_age_fem:(n_ages_F+first_age_fem-1)))						  
							  
INP$Fm[,1] <- c(rep(c(first_age_mal:(n_ages_M+first_age_mal-1)), length(years)), rep(c(first_age_fem:(n_ages_F+first_age_fem-1)), length(years)))

lab_year <- rep(years,n_ages_M)
lab_year <- lab_year[order(lab_year)]  # M
INP$Fm[1:nrow_males,2] <- lab_year

lab_year <- rep(years,n_ages_F)
lab_year <- lab_year[order(lab_year)]  # F
INP$Fm[(nrow_males+1):(nrow_males+nrow_females),2] <- lab_year

INP$Type = vector(mode="character", length=length(FLEETSEGMENTS_names)) # “A” (vettore per età, AGE) o “L” (vettore per lunghezza/LENGTH)

INP$D_Vector_sim <- data.frame(matrix(0, nrow=(nrow_males + nrow_females), ncol=(3+length(FLEETSEGMENTS_names))))
                              # c(c(first_age_mal:(n_ages_M+first_age_mal-1)) , c(first_age_fem:(n_ages_F+first_age_fem-1)))
INP$D_Vector_sim[,1] <- c(rep(c(first_age_mal:(n_ages_M+first_age_mal-1)), length(years)), rep(c(first_age_fem:(n_ages_F+first_age_fem-1)), length(years)))

lab_year <- rep(years,n_ages_M)
lab_year <- lab_year[order(lab_year)]  # M
INP$D_Vector_sim[1:nrow_males,2] <- lab_year

lab_year <- rep(years,n_ages_F)
lab_year <- lab_year[order(lab_year)]  # F
INP$D_Vector_sim[(nrow_males+1):(nrow_males+nrow_females),2] <- lab_year

INP$Disc_Surv_rate_sim_temp <- c()
INP$Disc_Surv_rate_type_sim_temp <- c()
INP$Disc_Surv_rate_constF_sim_temp <- c()
INP$Disc_Surv_rate_constM_sim_temp <- c()
INP$Disc_Surv_rate_ogive_param1_sim_temp <- c()
INP$Disc_Surv_rate_ogive_param2_sim_temp <- c()


INP$Esc_Surv_rate_sim_temp <- c()
INP$Esc_Surv_rate_type_sim_temp <- c()
INP$Esc_Surv_rate_constM_sim_temp <- c()
INP$Esc_Surv_rate_constF_sim_temp <- c()
INP$Esc_Surv_rate_ogive_param1_sim_temp <- c()
INP$Esc_Surv_rate_ogive_param2_sim_temp <- c()
INP$Esc_Surv_rate_vectM_sim_temp <- data.frame(matrix(NA, nrow=length(BAS$MLength),ncol=length(FLEETSEGMENTS_names)))
INP$Esc_Surv_rate_vectF_sim_temp <- data.frame(matrix(NA, nrow=length(BAS$FLength),ncol=length(FLEETSEGMENTS_names)))

INP$Esc_Vector_sim_temp <- data.frame(matrix(NA, nrow=(n_ages_M + n_ages_F),ncol=(length(FLEETSEGMENTS_names)+2)))
colnames(INP$Esc_Vector_sim_temp) <- c("Age", paste("fs", 1:length(FLEETSEGMENTS_names), sep=""), "Sex")
INP$Esc_Vector_sim_temp[,1] <- c(c(first_age_mal:(n_ages_M+first_age_mal-1)), c(first_age_fem:(n_ages_F+first_age_fem-1)))
INP$Esc_Vector_sim_temp[,ncol(INP$Esc_Vector_sim_temp)] <- c(rep("M", n_ages_M), rep("F", n_ages_F))


for (fs in 1:length(FLEETSEGMENTS_names) ) {

if (new_aldSimulation@enteringMortality == "F") {
FishingM_males_fs <-   FleetList_simulation[[fs]]@fishingmortality.M.vector
FishingM_females_fs <-     FleetList_simulation[[fs]]@fishingmortality.F.vector 

if (exists("col_to_add_M")) { rm(col_to_add_M) }
if (exists("col_to_add_F")) { rm(col_to_add_F) }
}

if (nrow(FleetList_simulation[[fs]]@escape.survivability.DOS.ext_vect.M) == 0 ) {
    col_to_add_M_Sur_Vector <- data.frame(matrix(NA, nrow=1, ncol=(n_ages_M)))
#  colnames(Sur_Vector_males_fs) <- c(paste("age", c(first_age_mal:(n_ages_M-1)), sep="") )
} else {
 col_to_add_M_Sur_Vector <-   FleetList_simulation[[fs]]@escape.survivability.DOS.ext_vect.M
}

if (nrow(FleetList_simulation[[fs]]@escape.survivability.DOS.ext_vect.F) == 0 ) {
    col_to_add_F_Sur_Vector <- data.frame(matrix(NA, nrow=1, ncol=(n_ages_F)))
#  colnames(Sur_Vector_females_fs) <- c(paste("age", c(first_age_mal:(n_ages_F-1)), sep="") )
} else {
 col_to_add_F_Sur_Vector <-   FleetList_simulation[[fs]]@escape.survivability.DOS.ext_vect.F
}



if (nrow(FleetList_simulation[[fs]]@discard_extvector.M.vector) == 0 ) {
    D_Vector_males_fs <- data.frame(matrix(NA, nrow=length(years), ncol=(n_ages_M+1)))
  colnames(D_Vector_males_fs) <- c("years", paste("age", c(first_age_mal:(n_ages_M-1)), sep="") )
  D_Vector_males_fs$years <- years
} else {
 D_Vector_males_fs <-   FleetList_simulation[[fs]]@discard_extvector.M.vector
}

if (nrow(FleetList_simulation[[fs]]@discard_extvector.F.vector) == 0 ) {
  D_Vector_females_fs <- data.frame(matrix(NA, nrow=length(years), ncol=(n_ages_F+1)))
  colnames(D_Vector_females_fs) <- c("years", paste("age", c(first_age_fem:(n_ages_F-1)), sep="") )
  D_Vector_females_fs$years <- years
} else {
  D_Vector_females_fs <-   FleetList_simulation[[fs]]@discard_extvector.F.vector
}


if (exists("col_to_add_M_D_Vector")) { rm(col_to_add_M_D_Vector) }
if (exists("col_to_add_F_D_Vector")) { rm(col_to_add_F_D_Vector) }


if (new_aldSimulation@enteringMortality != "F") {
if (FleetList_simulation[[fs]]@selectivity.mode == "age") {
     INP$Type[fs] <- "A"
 } else if (FleetList_simulation[[fs]]@selectivity.mode == "length") {
    INP$Type[fs] <- "L"
 }

    
  if (nrow(FleetList_simulation[[fs]]@SelectivityAge.M.vector) == 0 ) {
    SelAGE_males_fs <- data.frame(matrix(NA, nrow=length(years), ncol=(n_ages_M+1)))
  colnames(SelAGE_males_fs) <- c("years", paste("age", c(first_age_mal:(n_ages_M-1)), sep="") )
  SelAGE_males_fs$years <- years
} else {
 SelAGE_males_fs <-   FleetList_simulation[[fs]]@SelectivityAge.M.vector
}

if (nrow(FleetList_simulation[[fs]]@SelectivityAge.F.vector) == 0 ) {
  SelAGE_females_fs <- data.frame(matrix(NA, nrow=length(years), ncol=(n_ages_F+1)))
  colnames(SelAGE_females_fs) <- c("years", paste("age", c(first_age_fem:(n_ages_F-1)), sep="") )
  SelAGE_females_fs$years <- years
} else {
  SelAGE_females_fs <-   FleetList_simulation[[fs]]@SelectivityAge.F.vector
}
  
   
#   } else if (FleetList_simulation[[fs]]@selectivity.mode == "length") {

   
  if (nrow(FleetList_simulation[[fs]]@SelectivityLength.M.vector) == 0 ) {
    SelLENGTH_males_fs <- data.frame( matrix(NA, nrow=length(l_inf_lens_M), ncol=(length(years)+2) ) )
  colnames(SelLENGTH_males_fs) <- c("Length", years, sep="")
  SelLENGTH_males_fs$Length <- l_inf_lens_M
} else {
 SelLENGTH_males_fs <-   FleetList_simulation[[fs]]@SelectivityLength.M.vector
}

if (nrow(FleetList_simulation[[fs]]@SelectivityLength.F.vector) == 0 ) {
  SelLENGTH_females_fs <- data.frame(matrix(NA, nrow=length(l_inf_lens_F), ncol=(length(years)+2) ) )
  colnames(SelLENGTH_females_fs) <- c("Length", years, sep="")
   SelLENGTH_females_fs$Length <- l_inf_lens_F
} else {
  SelLENGTH_females_fs <-   FleetList_simulation[[fs]]@SelectivityLength.F.vector
}
  
 }
   
#   }

   
 if (exists("col_to_add_M_SelAGE_Vector")) { rm(col_to_add_M_SelAGE_Vector) }
if (exists("col_to_add_F_SelAGE_Vector")) { rm(col_to_add_F_SelAGE_Vector) }  
 
  if (exists("col_to_add_M_SelLENGTH_Vector")) { rm(col_to_add_M_SelLENGTH_Vector) }
if (exists("col_to_add_F_SelLENGTH_Vector")) { rm(col_to_add_F_SelLENGTH_Vector) }   
   
for (yy in 1:length(years) ) {

if (new_aldSimulation@enteringMortality == "F") {
if (!exists("col_to_add_M")) {
col_to_add_M <-  as.numeric(as.character(FishingM_males_fs[FishingM_males_fs$year == years[yy],2:(n_ages_M+1)]))
} else {
col_to_add_M <- c( col_to_add_M, as.numeric(as.character(FishingM_males_fs[FishingM_males_fs$year == years[yy],2:(n_ages_M+1)])) )
}
     
if (!exists("col_to_add_F")) {
col_to_add_F <-  as.numeric(as.character(FishingM_females_fs[FishingM_females_fs$year == years[yy],2:(n_ages_F+1)]))
} else {
col_to_add_F <- c(col_to_add_F,as.numeric(as.character(FishingM_females_fs[FishingM_females_fs$year == years[yy],2:(n_ages_F+1)])) )
}

} 

if (new_aldSimulation@enteringMortality != "F") {
if (FleetList_simulation[[fs]]@selectivity.mode == "age") {
if (!exists("col_to_add_M_SelAGE_Vector")) {
col_to_add_M_SelAGE_Vector <-  as.numeric(as.character(SelAGE_males_fs[SelAGE_males_fs$Year == years[yy],2:(n_ages_M+1)]))
} else {
col_to_add_M_SelAGE_Vector <- c( col_to_add_M_SelAGE_Vector, as.numeric(as.character(SelAGE_males_fs[SelAGE_males_fs$Year == years[yy],2:(n_ages_M+1)])) )
}
     
if (!exists("col_to_add_F_SelAGE_Vector")) {
col_to_add_F_SelAGE_Vector <-  as.numeric(as.character(SelAGE_females_fs[SelAGE_females_fs$Year == years[yy],2:(n_ages_F+1)]))
} else {
col_to_add_F_SelAGE_Vector <- c(col_to_add_F_SelAGE_Vector,as.numeric(as.character(SelAGE_females_fs[SelAGE_females_fs$Year == years[yy],2:(n_ages_F+1)])) )
}

} else if (FleetList_simulation[[fs]]@selectivity.mode == "length") {

if (!exists("col_to_add_M_SelLENGTH_Vector")) {
col_to_add_M_SelLENGTH_Vector <-  as.numeric(as.character(SelLENGTH_males_fs[, yy+1]))
} else {
col_to_add_M_SelLENGTH_Vector <- c( col_to_add_M_SelLENGTH_Vector, as.numeric(as.character(SelLENGTH_males_fs[, yy+1]) ) )
}
     
if (!exists("col_to_add_F_SelLENGTH_Vector")) {
col_to_add_F_SelLENGTH_Vector <-  as.numeric(as.character(SelLENGTH_females_fs[, yy+1]))
} else {
col_to_add_F_SelLENGTH_Vector <- c(col_to_add_F_SelLENGTH_Vector,as.numeric(as.character(SelLENGTH_females_fs[, yy+1])) )
}

}

}


if (!exists("col_to_add_M_D_Vector")) {
col_to_add_M_D_Vector <-  as.numeric(as.character(D_Vector_males_fs[D_Vector_males_fs$year == years[yy],2:(n_ages_M+1)]))
} else {
col_to_add_M_D_Vector <- c( col_to_add_M_D_Vector, as.numeric(as.character(D_Vector_males_fs[D_Vector_males_fs$year == years[yy],2:(n_ages_M+1)])) )
}
     
if (!exists("col_to_add_F_D_Vector")) {
col_to_add_F_D_Vector <-  as.numeric(as.character(D_Vector_females_fs[D_Vector_females_fs$year == years[yy],2:(n_ages_F+1)]))
} else {
col_to_add_F_D_Vector <- c(col_to_add_F_D_Vector,as.numeric(as.character(D_Vector_females_fs[D_Vector_females_fs$year == years[yy],2:(n_ages_F+1)])) )
}


}

if (new_aldSimulation@enteringMortality == "F") {
INP$Fm[1:(nrow_males),(2+fs)] <- col_to_add_M
INP$Fm[(nrow_males+1):(nrow_males+nrow_females),(2+fs)] <- col_to_add_F 
}

if (new_aldSimulation@enteringMortality != "F") {
if (FleetList_simulation[[fs]]@selectivity.mode == "age") {
INP$Sel_vector[1:(nrow_males),(2+fs)] <- col_to_add_M_SelAGE_Vector
INP$Sel_vector[(nrow_males+1):(nrow_males+nrow_females),(2+fs)]  <- col_to_add_F_SelAGE_Vector

} else if (FleetList_simulation[[fs]]@selectivity.mode == "length") {
INP$Sel_vector_len[1:(nrow_males_lenXsel),(2+fs)] <- col_to_add_M_SelLENGTH_Vector
INP$Sel_vector_len[(nrow_males_lenXsel+1):(nrow_males_lenXsel+nrow_females_lenXsel),(2+fs)]  <- col_to_add_F_SelLENGTH_Vector 
}    

}

INP$D_Vector_sim[1:(nrow_males),(2+fs)] <- col_to_add_M_D_Vector
INP$D_Vector_sim[(nrow_males+1):(nrow_males+nrow_females),(2+fs)] <- col_to_add_F_D_Vector

INP$Esc_Vector_sim_temp[1:(n_ages_M),(1+fs)] <- as.numeric(as.character(col_to_add_M_Sur_Vector))
INP$Esc_Vector_sim_temp[(n_ages_M+1):(n_ages_M+n_ages_F),(1+fs)] <- as.numeric(as.character(col_to_add_F_Sur_Vector))

INP$Disc_Surv_rate_sim_temp <- c(INP$Disc_Surv_rate_sim_temp, as.character(  FleetList_simulation[[fs]]@discard.survivability.calculation ))
INP$Esc_Surv_rate_sim_temp <- c(INP$Esc_Surv_rate_sim_temp, as.character( FleetList_simulation[[fs]]@escape.survivability.calculation ))

  if ( length(FleetList_simulation[[fs]]@discard.survivability.datatype) > 0) {
       if (FleetList_simulation[[fs]]@discard.survivability.datatype == "C") {
               INP$Disc_Surv_rate_type_sim_temp <- c(INP$Disc_Surv_rate_type_sim_temp, 1 ) # 1 constant
       } else if (FleetList_simulation[[fs]]@discard.survivability.datatype == "DOS") {
                 INP$Disc_Surv_rate_type_sim_temp <- c(INP$Disc_Surv_rate_type_sim_temp, 2 ) # 2 ogive
       } else {
           INP$Disc_Surv_rate_type_sim_temp <- c(INP$Disc_Surv_rate_type_sim_temp,NA ) # NA
       }
  } else {
     INP$Disc_Surv_rate_type_sim_temp <- c(INP$Disc_Surv_rate_type_sim_temp,NA ) # 1 constant, 2 ogive 
  }

    if ( length(FleetList_simulation[[fs]]@escape.survivability.datatype) > 0) {
if (FleetList_simulation[[fs]]@escape.survivability.datatype == "C") {
          INP$Esc_Surv_rate_type_sim_temp <-  c(INP$Esc_Surv_rate_type_sim_temp, 1 ) # 1 constant
} else if (FleetList_simulation[[fs]]@escape.survivability.datatype == "DOS" ) {    
       if (FleetList_simulation[[fs]]@escape.survivability.DOS.datatype == "O") {
            INP$Esc_Surv_rate_type_sim_temp <-  c(INP$Esc_Surv_rate_type_sim_temp, 2 ) # 2 ogive 
       } else if (FleetList_simulation[[fs]]@escape.survivability.DOS.datatype  == "V") {
             INP$Esc_Surv_rate_type_sim_temp <-  c(INP$Esc_Surv_rate_type_sim_temp, 3 )   # 3 externat vector
       }
} else {
     INP$Esc_Surv_rate_type_sim_temp <-  c(INP$Esc_Surv_rate_type_sim_temp, NA) # NA
} 
} else {
     INP$Esc_Surv_rate_type_sim_temp <-  c(INP$Esc_Surv_rate_type_sim_temp, NA) # NA
}

if (length(FleetList_simulation[[fs]]@discard.survivability.datatype) > 0) {
INP$Disc_Surv_rate_constM_sim_temp <- c(INP$Disc_Surv_rate_constM_sim_temp, ifelse(as.character( FleetList_simulation[[fs]]@discard.survivability.datatype) == "C", as.numeric(as.character(FleetList_simulation[[fs]]@discard.survivability.params$param1_or_M[1])), NA) ) # param 1 / males 
INP$Disc_Surv_rate_constF_sim_temp <- c(INP$Disc_Surv_rate_constF_sim_temp, ifelse(as.character( FleetList_simulation[[fs]]@discard.survivability.datatype) == "C", as.numeric(as.character(FleetList_simulation[[fs]]@discard.survivability.params$param2_or_F[1])), NA) ) # param 1 / males 
} else {
INP$Disc_Surv_rate_constM_sim_temp <- c(INP$Disc_Surv_rate_constM_sim_temp,NA) # param 1 / males 
INP$Disc_Surv_rate_constF_sim_temp <- c(INP$Disc_Surv_rate_constF_sim_temp, NA) # param 1 / males 
}

if ( nrow(FleetList_simulation[[fs]]@escape.survivability.constant) > 0 )  {
     INP$Esc_Surv_rate_constM_sim_temp <- c(INP$Esc_Surv_rate_constM_sim_temp,  as.numeric(as.character(FleetList_simulation[[fs]]@escape.survivability.constant$males[1])))
     INP$Esc_Surv_rate_constF_sim_temp <- c(INP$Esc_Surv_rate_constF_sim_temp,  as.numeric(as.character(FleetList_simulation[[fs]]@escape.survivability.constant$females[1])))
} else {
     INP$Esc_Surv_rate_constM_sim_temp <- c(INP$Esc_Surv_rate_constM_sim_temp, NA)
     INP$Esc_Surv_rate_constF_sim_temp <- c(INP$Esc_Surv_rate_constF_sim_temp, NA)
}

if (nrow(FleetList_simulation[[fs]]@escape.survivability.DOS.ogiveparams) > 0) {
     INP$Esc_Surv_rate_ogive_param1_sim_temp <- c(INP$Esc_Surv_rate_ogive_param1_sim_temp, as.numeric(as.character(FleetList_simulation[[fs]]@escape.survivability.DOS.ogiveparams$param1[1])))
     INP$Esc_Surv_rate_ogive_param2_sim_temp <- c(INP$Esc_Surv_rate_ogive_param2_sim_temp, as.numeric(as.character(FleetList_simulation[[fs]]@escape.survivability.DOS.ogiveparams$param2[1])))
} else {
     INP$Esc_Surv_rate_ogive_param1_sim_temp <- c(INP$Esc_Surv_rate_ogive_param1_sim_temp, NA)
     INP$Esc_Surv_rate_ogive_param2_sim_temp <- c(INP$Esc_Surv_rate_ogive_param2_sim_temp, NA)
}

if (length(FleetList_simulation[[fs]]@discard.survivability.datatype) != 0) {
INP$Disc_Surv_rate_ogive_param1_sim_temp <- c(INP$Disc_Surv_rate_ogive_param1_sim_temp, ifelse(as.character( FleetList_simulation[[fs]]@discard.survivability.datatype) == "DOS", as.numeric(as.character(FleetList_simulation[[fs]]@discard.survivability.params$param1_or_M[1])), NA) ) # param 1
INP$Disc_Surv_rate_ogive_param2_sim_temp <- c(INP$Disc_Surv_rate_ogive_param2_sim_temp, ifelse(as.character( FleetList_simulation[[fs]]@discard.survivability.datatype) == "DOS", as.numeric(as.character(FleetList_simulation[[fs]]@discard.survivability.params$param2_or_F[1])), NA) ) # param 2 
} else {
INP$Disc_Surv_rate_ogive_param1_sim_temp <- c(INP$Disc_Surv_rate_ogive_param1_sim_temp,  NA) # param 1 
INP$Disc_Surv_rate_ogive_param2_sim_temp <- c(INP$Disc_Surv_rate_ogive_param2_sim_temp, NA) # param 2 
}


}



INP$Disc_Surv_rate_sim <- INP$Disc_Surv_rate_sim_temp
INP$Disc_Surv_rate_type_sim <- INP$Disc_Surv_rate_type_sim_temp

INP$Disc_Surv_rate_constF_sim <- INP$Disc_Surv_rate_constF_sim_temp
INP$Disc_Surv_rate_constM_sim <- INP$Disc_Surv_rate_constM_sim_temp

INP$Disc_Surv_rate_ogive_param2_sim <- INP$Disc_Surv_rate_ogive_param2_sim_temp
INP$Disc_Surv_rate_ogive_param1_sim <- INP$Disc_Surv_rate_ogive_param1_sim_temp


INP$Esc_Surv_rate_sim <- INP$Esc_Surv_rate_sim_temp
INP$Esc_Surv_rate_type_sim <- INP$Esc_Surv_rate_type_sim_temp

INP$Esc_Surv_rate_constM_sim <- INP$Esc_Surv_rate_constM_sim_temp
INP$Esc_Surv_rate_constF_sim <- INP$Esc_Surv_rate_constF_sim_temp

INP$Esc_Surv_rate_ogive_param1_sim <-  INP$Esc_Surv_rate_ogive_param1_sim_temp
INP$Esc_Surv_rate_ogive_param2_sim <-  INP$Esc_Surv_rate_ogive_param2_sim_temp

INP$Esc_Vector_sim <- INP$Esc_Vector_sim_temp    # matrix with all the gears
INP$Esc_vectorM_sim <- INP$Esc_Vector_sim[as.character(INP$Esc_Vector_sim$Sex)=="M",]        # by sex
INP$Esc_vectorF_sim <- INP$Esc_Vector_sim[as.character(INP$Esc_Vector_sim$Sex)=="F",]


if (new_aldSimulation@enteringMortality == "F") {
INP$Fm[1:(nrow_males),(3+length(FLEETSEGMENTS_names))] <- "M"
INP$Fm[(nrow_males+1):(nrow_males+nrow_females),(3+length(FLEETSEGMENTS_names))] <- "F"
colnames(INP$Fm) <- c("Age", "Year", paste("fs", c(1:length(FLEETSEGMENTS_names)), sep=""), "Sex")
}

INP$Sel_vector[1:(nrow_males),(3+length(FLEETSEGMENTS_names))] <- "M"
INP$Sel_vector[(nrow_males+1):(nrow_males+nrow_females),(3+length(FLEETSEGMENTS_names))] <- "F"
colnames(INP$Sel_vector) <- c("Age", "Year", paste("fs", c(1:length(FLEETSEGMENTS_names)), sep=""), "Sex")

INP$Sel_vector_len[1:(nrow_males_lenXsel),(3+length(FLEETSEGMENTS_names))] <- "M"
INP$Sel_vector_len[(nrow_males_lenXsel+1):(nrow_males_lenXsel+nrow_females_lenXsel),(3+length(FLEETSEGMENTS_names))] <- "F"
colnames(INP$Sel_vector_len) <- c("Length", "Year", paste("fs", c(1:length(FLEETSEGMENTS_names)), sep=""), "Sex")

INP$D_Vector_sim[1:(nrow_males),(3+length(FLEETSEGMENTS_names))] <- "M"
INP$D_Vector_sim[(nrow_males+1):(nrow_males+nrow_females),(3+length(FLEETSEGMENTS_names))] <- "F"
colnames(INP$D_Vector_sim) <- c("Age", "Year", paste("fs", c(1:length(FLEETSEGMENTS_names)), sep=""), "Sex")


INP$D_vectorM_sim = INP$D_Vector_sim[as.character(INP$D_Vector_sim$Sex)=="M",]
INP$D_vectorF_sim = INP$D_Vector_sim[as.character(INP$D_Vector_sim$Sex)=="F",]


if (IN_BEMTOOL) {
if (new_aldSimulation@enteringMortality == "F") {
write.table(INP$Fm, file= F_BYGEAR_INP_table, sep=";", row.names=F)
}

#D_Vector_sim = D_Vector_sim
write.table(INP$D_Vector_sim, file= DISCARD_BYAGE_EXTERNALVECT_INP_table, sep=";", row.names=F)

}


## Selection Gear
#

#-------------------------------------------------------------------
# Global Parameters
#-------------------------------------------------------------------

                                                                                       #parametri relazione lunghezza peso per maschi e femmine
INP$MWLa                        <- as.numeric(data.frame(new_aldPopulation@lengthweight)$a[rownames(data.frame(new_aldPopulation@lengthweight)) == "M"])
INP$MWLb                        <- as.numeric(data.frame(new_aldPopulation@lengthweight)$b[rownames(data.frame(new_aldPopulation@lengthweight)) == "M"])

INP$FWLa                        <- as.numeric(data.frame(new_aldPopulation@lengthweight)$a[rownames(data.frame(new_aldPopulation@lengthweight)) == "F"])
INP$FWLb                        <- as.numeric(data.frame(new_aldPopulation@lengthweight)$b[rownames(data.frame(new_aldPopulation@lengthweight)) == "F"])

to_write <- "Global"
source( paste(ALADYM_home, "/src/writeDataIn.r", sep="") ) 

#----- eliminata una parte del codice non usata--------

loca_Nsimulation                <- (GLO$L_number + 1)    #numero di mesi da simulare +1

INP$MZ_estimated               <- vector(mode = "numeric", length = loca_Nsimulation)        #inizializzazione dei vettori da stimare a partire da vettori di input
INP$FZ_estimated               <- vector(mode = "numeric", length = loca_Nsimulation)
INP$FMAX_vector                <- vector(mode = "numeric", length = loca_Nsimulation)

# Altri parametri di selettività: inizializzazione -------------------------------------------------------
INP$SL50p                      <- vector(mode = "numeric", length = loca_Nsimulation)
INP$SL75pL25p                  <- vector(mode = "numeric", length = loca_Nsimulation)
INP$SD50p                      <- vector(mode = "numeric", length = loca_Nsimulation)
#--------------------------------------------------------------------------------------

# nb_gears = length(FLEETSEGMENTS_names)
nb_gears = length(FleetList_simulation)

INP$OPT_SG_TYPE                   <- matrix(nrow = loca_Nsimulation, ncol = nb_gears)



INP$RGmin                      <- vector(mode = "numeric", length = loca_Nsimulation)
INP$RGmax                      <- vector(mode = "numeric", length = loca_Nsimulation)
INP$Recruits                   <- vector(mode = "numeric", length = loca_Nsimulation)
INP$Sex_ratio                  <- vector(mode = "numeric", length = loca_Nsimulation)
INP$Fishing_efforts            <- matrix(nrow = loca_Nsimulation, ncol = nb_gears)
INP$Land_obl                       <- matrix(nrow = loca_Nsimulation, ncol = nb_gears)                                                  # <<----------------------------- NEW
# colnames(INP$Fishing_efforts) = FLEETSEGMENTS_names
 colnames(INP$Fishing_efforts)        <- as.character(FLEETSEGMENTS_names)
INP$p_Production            <- matrix(nrow = loca_Nsimulation, ncol = nb_gears)
# colnames(INP$p_Production ) = FLEETSEGMENTS_names
colnames(INP$p_Production)            <- as.character(FLEETSEGMENTS_names)

# INP$MZ_objective               <- vector(mode = "numeric", length = loca_Nsimulation)				# commentato nell'integrazione dell'interfaccia 
# INP$FZ_objective               <- vector(mode = "numeric", length = loca_Nsimulation)
 
 
#  Costruzione della matrice sforzo di pesca per attrezzo
nb_years <- INP$Year_simulation

for (gear in 1:nb_gears){
      # fishing_effort_temp = gear_data[gear_data$Gear == FLEETSEGMENTS_names[gear],]
	   prod_temp <-c()
       for (y in 1:nb_years) {
       row_prod <- FleetList_simulation[[gear]]@production.vector[y, (colnames(FleetList_simulation[[gear]]@production.vector) != "year")]
       row_prod <- as.numeric(as.character(row_prod[row_prod!=""]))
       prod_temp <- c(prod_temp, row_prod)
       }
       
       fishingeffort_temp <-c()
       for (y in 1:nb_years) {
       row_effort <- FleetList_simulation[[gear]]@fishingeffort.vector[y, (colnames(FleetList_simulation[[gear]]@fishingeffort.vector) != "year")]
       row_effort <- as.numeric(as.character(row_effort[row_effort!=""]))
       fishingeffort_temp <- c(fishingeffort_temp, row_effort)
       }
   
#    if (FleetList_simulation[[gear]]@discard.calculation == "YES") {
   	   landobl_temp <- c()
       for (y in 1:nb_years) {
      if (FleetList_simulation[[gear]]@discard.calculation == "YES") {
       row_obl <- as.character(FleetList_simulation[[gear]]@landing.obligation.vector[y, (colnames(FleetList_simulation[[gear]]@landing.obligation.vector) != "year")] )
       if (y==1) {
       	row_obl <- c(row_obl[1], as.character(row_obl[row_obl!=""]) )
       } else {
			 row_obl <- as.character(row_obl[row_obl!=""])
			 }
       } else {
       
       if (y==1) {
       	row_obl <- rep("N", 13)
       } else {
			 row_obl <- rep("N", 12)
			 }
    
       }
       landobl_temp <- c(landobl_temp, row_obl)
       }
#    }   
      
    
			 
      for (loca_i in 1:(loca_Nsimulation)){
#      INP$Fishing_efforts[loca_i,gear]  <- fishing_effort_temp$f_act[loca_i]  
#      INP$OPT_SG_TYPE[loca_i,gear]<- fishing_effort_temp$Type[loca_i]
#      INP$p_Production[loca_i,gear]<- fishing_effort_temp$p_production[loca_i]
        INP$Fishing_efforts[loca_i,gear]  <- fishingeffort_temp[loca_i] 
        
        INP$OPT_SG_TYPE[loca_i,gear]   <- FleetList_simulation[[gear]]@selectivity.vector[loca_i,8]   # tipo di selettività
        
        INP$p_Production[loca_i,gear] <- prod_temp[loca_i]
        INP$Land_obl[loca_i,gear] <- landobl_temp[loca_i]                                                # <<----------------------------- NEW   
      }
 }
 
 
  # array of total mortality males
     Zm_temp <-c()
       for (y in 1:nb_years) {
       row_Zm <- new_aldSimulation@totalmortality.M.vector[y, (colnames(new_aldSimulation@totalmortality.M.vector) != "year")]
       row_Zm <- as.numeric(row_Zm[row_Zm != ""])
       Zm_temp <- c(Zm_temp, row_Zm)
       }

           # array of total mortality females
     Zf_temp <-c()
       for (y in 1:nb_years) {
       row_Zf <- new_aldSimulation@totalmortality.F.vector[y, (colnames(new_aldSimulation@totalmortality.F.vector) != "year")]
       row_Zf <- as.numeric(row_Zf[row_Zf != ""])
       Zf_temp <- c(Zf_temp, row_Zf)
       }

     SR_temp <- c()
      for (y in 1:nb_years) {
       row_SR <- new_aldSimulation@stockr.vector[y, (colnames(new_aldSimulation@stockr.vector) != "year")]
       row_SR <- as.numeric(row_SR[row_SR != ""])
       SR_temp <- c(SR_temp, row_SR)
       }

#if ( new_aldSimulation@CI_calculation & new_aldSimulation@CI_error_source == 2 ) {
#  R_type_sim           <- which(DISTRIBUTION == data.frame(new_aldSimulation@recruitment.noise)$distribution[1])
#  
#  if (as.character(new_aldSimulation@recruitment.noise$distribution[1]) == "Uniform") {
#  R_pam1_sim   <- as.numeric(data.frame(new_aldSimulation@recruitment.noise)$min[1])
#  R_pam2_sim  <- as.numeric(data.frame(new_aldSimulation@recruitment.noise)$max[1])
#  }  else {
#    R_pam1_sim   <- as.numeric(data.frame(new_aldSimulation@recruitment.noise)$A[1])
#  R_pam2_sim  <- as.numeric(data.frame(new_aldSimulation@recruitment.noise)$B[1])
#  }
#  
#   # da verificare
#   R_type_for <- R_type_sim
#  R_pam1_for <-  R_pam1_sim
#   R_pam2_for <-  R_pam2_sim
#  } else {
 R_type_sim  <-  4
 R_pam1_sim   <- 1
  R_pam2_sim  <- 1   
#
# # da verificare
# R_type_for <- R_type_sim
#R_pam1_for <-  R_pam1_sim
#R_pam2_for <-  R_pam2_sim
#  }


  
for(loca_i in 1:loca_Nsimulation) {
    INP$MZ_estimated[loca_i]                  <- Zm_temp[loca_i]            # Z stimato per i maschi
   INP$FZ_estimated[loca_i]                  <- Zf_temp[loca_i]              # Z stimato per le femmine

    if (INP$FRLt == 4) {
     INP$Recruits[loca_i]          <- SR_temp[loca_i]
     }  else {
       INP$Recruits[loca_i]          <- NA
     }
   INP$Sex_ratio[loca_i]         <- as.numeric(new_aldPopulation@sexratio)

}   
  
#-------------------------------------------------------------------
# Miscellaneous Parameters
#-------------------------------------------------------------------

GLO$Nrun                       <- as.numeric(new_aldSimulation@seed_rundomization_runs)     # Number of Run for seed randomization 

INP$param1 = matrix(nrow=(loca_Nsimulation),ncol=nb_gears)
INP$param2 = matrix(nrow=(loca_Nsimulation),ncol=nb_gears)
INP$param3 = matrix(nrow=(loca_Nsimulation),ncol=nb_gears)
INP$param4 = matrix(nrow=(loca_Nsimulation),ncol=nb_gears)
INP$param5 = matrix(nrow=(loca_Nsimulation),ncol=nb_gears)
INP$param6 = matrix(nrow=(loca_Nsimulation),ncol=nb_gears)
INP$param7 = matrix(nrow=(loca_Nsimulation),ncol=nb_gears)
INP$Discard = matrix(nrow=(loca_Nsimulation),ncol=nb_gears)
INP$Type_discard = matrix(nrow=(loca_Nsimulation),ncol=nb_gears)                                              # <<----------------------------- NEW

INP$Forecast_reduction = matrix(nrow=(1),ncol=nb_gears)
Plot_selectivity = matrix(nrow=(loca_Nsimulation),ncol=nb_gears)


SS                                  <- which(SS_TYPE == new_aldSimulation@spawners.ss)  # 1 o 2 

# spezzare i mesi simulation + forecast in tempi diversi

      # il P_production lo calcola solo se c'è entrata con Z                                                           
for (gear in 1:nb_gears){


  for (loca_i in 1:(loca_Nsimulation)){
  
  if (as.numeric(INP$OPT_F_TYPE)==1) {
  
  if (FleetList_simulation[[gear]]@selectivity.mode == "params") {
  
   INP$param1[loca_i,gear]  <-  as.numeric(as.character(FleetList_simulation[[gear]]@selectivity.vector[loca_i, 3] ))
   INP$param2[loca_i,gear]  <-  as.numeric(as.character(FleetList_simulation[[gear]]@selectivity.vector[loca_i, 4]   ))
   INP$param3[loca_i,gear]  <-  as.numeric(as.character(FleetList_simulation[[gear]]@selectivity.vector[loca_i, 5]     ))
   INP$param4[loca_i,gear]  <-  as.numeric(as.character(FleetList_simulation[[gear]]@selectivity.vector[loca_i, 6]  ))
   INP$param5[loca_i,gear]  <-  as.numeric(as.character(FleetList_simulation[[gear]]@selectivity.vector[loca_i, 7]    )) 
   INP$Type[gear] <- "NA"
   
   }  else {
        INP$OPT_SG_TYPE[loca_i,gear] <- 7
   }
# } 
 }
    # Land_obl[loca_i,gear] <- landobl_temp[loca_i] 
    
   if (FleetList_simulation[[gear]]@discard.calculation == "YES") {
        

 if (FleetList_simulation[[gear]]@discard.datatype == "Reverse ogive") {
    INP$Type_discard[loca_i,gear]  <- 1
   INP$param6[loca_i,gear]  <- as.numeric(as.character(FleetList_simulation[[gear]]@discard.vector[loca_i, 3]  ))
   INP$param7[loca_i,gear]  <-  as.numeric(as.character(FleetList_simulation[[gear]]@discard.vector[loca_i, 4])) 
   } else {
     INP$Type_discard[loca_i,gear] <- 2
    INP$param6[loca_i,gear] <- NA
    INP$param7[loca_i,gear] <- NA
   }

      }  
   disc_calc <-   FleetList_simulation[[gear]]@discard.calculation
   INP$Discard[loca_i,gear] <- ifelse(disc_calc == "YES", "Y", ifelse(disc_calc == "0", 0, NA))
   

}

}


Calc_ref_point            <-  ifelse(new_aldSimulation@reference_points_calculation, "Y", "N")             # yes, no (spunta da aggiugere nella scheda della simulazione)

min_ageM                  <- as.numeric(data.frame(new_aldSimulation@fishingmortality)$min[rownames(data.frame(new_aldSimulation@fishingmortality)) == "M"])
max_ageM                  <- as.numeric(data.frame(new_aldSimulation@fishingmortality)$max[rownames(data.frame(new_aldSimulation@fishingmortality)) == "M"])
min_ageF                  <- as.numeric(data.frame(new_aldSimulation@fishingmortality)$min[rownames(data.frame(new_aldSimulation@fishingmortality)) == "F"])
max_ageF                  <- as.numeric(data.frame(new_aldSimulation@fishingmortality)$max[rownames(data.frame(new_aldSimulation@fishingmortality)) == "F"])

INP$min_ageM                  <- as.numeric(data.frame(new_aldSimulation@fishingmortality)$min[rownames(data.frame(new_aldSimulation@fishingmortality)) == "M"])
INP$max_ageM                  <- as.numeric(data.frame(new_aldSimulation@fishingmortality)$max[rownames(data.frame(new_aldSimulation@fishingmortality)) == "M"])
INP$min_ageF                  <- as.numeric(data.frame(new_aldSimulation@fishingmortality)$min[rownames(data.frame(new_aldSimulation@fishingmortality)) == "F"])
INP$max_ageF                  <- as.numeric(data.frame(new_aldSimulation@fishingmortality)$max[rownames(data.frame(new_aldSimulation@fishingmortality)) == "F"])

forecast=length(years)*12+1


write("                       ", file=loca_fileName, append=TRUE)
write("range for output F males            ", file=loca_fileName, append=TRUE)
write(paste(min_ageM,"-",max_ageM), file=loca_fileName, append=TRUE)

write("range for output F females            ", file=loca_fileName, append=TRUE)
write(paste(min_ageF,"-",max_ageF), file=loca_fileName, append=TRUE)


# Numero di anni su cui deve fare le medie per il forecast:      ------------------------------------------------------- FORECAST           

INP$Average_forecast <- new_aldSimulation@yearsForAverage 

if(forecast < GLO$L_number) {
p_prod_temp = data.frame(matrix(0,nrow= (INP$Average_forecast*12+1), ncol=nb_gears))
p_prod_temp = data.frame(INP$p_Production[((forecast-INP$Average_forecast*12+1):forecast) ,] ) # considero i p production degli anni selezionati dall'utente


for (g in 1:nb_gears){
INP$p_Production[(forecast+1):(GLO$L_number + 1) ,g] = mean(as.numeric(as.character(p_prod_temp[,g])) )  # medie degli ultimi x anni (solo per il forecast)
}

}

if (as.logical(data.frame(new_aldSimulation@recruitment.tuning))) {
Cal <- "Y"
INP$Cal_min <-  as.numeric(as.character(data.frame(new_aldSimulation@recruitment.tuning.range$min)))
INP$Cal_max <-   as.numeric(as.character(data.frame(new_aldSimulation@recruitment.tuning.range$max)))
} else {
Cal <- "N"
}



##----# LETTURA MATRICE F - nuova matrice by sex
if (as.numeric(INP$OPT_F_TYPE)==2) { 
#Fm = read.table(name_F_by_gear,sep=";",header=TRUE)      # AGGIUNTO
nb_gears=ncol(INP$Fm)-3
#
INP$Fmales = INP$Fm[as.character(INP$Fm[,ncol(INP$Fm)])=="M",]               # spostato nell'IF (MT)
INP$Fmales=INP$Fmales[,1:(ncol(INP$Fmales)-1)]
INP$Ffemales = INP$Fm[as.character(INP$Fm[,ncol(INP$Fm)])=="F",]
INP$Ffemales=INP$Ffemales[,1:(ncol(INP$Ffemales)-1)]

}

INP$Sel_vectorF <- INP$Sel_vector[INP$Sel_vector[,ncol(INP$Sel_vector)] == "F", ]
INP$Sel_vectorM <- INP$Sel_vector[INP$Sel_vector[,ncol(INP$Sel_vector)] == "M", ]

INP$Sel_vector_lenF <- INP$Sel_vector_len[INP$Sel_vector_len[,ncol(INP$Sel_vector_len)] == "F", ]
INP$Sel_vector_lenM <- INP$Sel_vector_len[INP$Sel_vector_len[,ncol(INP$Sel_vector_len)] == "M", ]

rm(list = ls(pat='loca*'))    #rimuove tutte le variabili locali

if (showCompTime)  {
proc_ <- proc.time()
print(paste("DataIn [time]::::::::::::::::::::::::::::::::", round(as.numeric(proc_[3]-DataIn_ptm[3]),2), "sec" ) , quote=F )   
#  print(proc.time() - DataIn_ptm, quote=F ) 
  rm(DataIn_ptm)
}

#-------------------------------------------------------------------
# End                                                                                   
#-------------------------------------------------------------------
