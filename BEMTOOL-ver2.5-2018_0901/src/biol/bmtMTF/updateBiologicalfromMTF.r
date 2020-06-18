# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





updateBiologicalfromMTF <- function(Inters) { 

if (FALSE) {
  Inters = Interactionsyear
}

sexratio <- as.numeric(Populations[[MTF_spe]]@sexratio)
 
t0 <- as.numeric(Populations[[MTF_spe]]@growth[2,1])
k <-  as.numeric(Populations[[MTF_spe]]@growth[2,2])
linf <-   as.numeric(Populations[[MTF_spe]]@growth[2,3])

maturity_prop <- Populations[[MTF_spe]]@maturity.vect

 SAtool <- as.character(cfg[rownames(cfg) == paste("casestudy.S", MTF_spe, ".StockAssessmentTool", sep=""),1])
# 
if (SAtool != "XSA") {
if (SAtool == "VIT") {
    num_classes_all_years <- c(0)
      for (y_int in 1:simperiod) {
          num_classes_all_years <- c(num_classes_all_years,  length(VITinfo[[MTF_spe]][[y_int]]$results[[1]]$age_classes)  )
      }
      num_classes_all_years <- num_classes_all_years[num_classes_all_years !=0]    
      num_classes <- min(num_classes_all_years)
} else {
num_classes <- as.numeric(as.character(Populations[[MTF_spe]]@lifespan[2,1] ))
}
} else {
num_classes <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", MTF_spe, ".StockAssessmentTool", sep=""),5]))    # only the F life span will be taken
}

if (SAtool == "VIT") {
num_classes_BACKUP <<-  num_classes_all_years
}
life_span <- as.numeric(as.character(Populations[[MTF_spe]]@lifespan[2,1] ))
age_vect <- c(0:(num_classes-1))

mid_ages <-  c(0:(life_span-1)) + 0.5
mid_lengths <- c(0:(life_span-1))
for (ag in 1:length(mid_ages)) { mid_lengths[ag] <- VB(mid_ages[ag],linf,k,t0) }

# ciclo sulle età (per gli anni del forecast)   prendere la terza colonna (50%)   splittare per sex ratio     da moltiplicare per 1000 (in MTF sono migliaia)
mtf_res_stocknumber <-as.data.frame(stock.n(MTFresult))                 
mtf_res_stock_mean_w <- as.data.frame(stock.wt(MTFresult))
mtf_res_ssb <- as.data.frame(ssb(MTFresult))

mtf_res_catchnumber <- as.data.frame(catch.n(MTFresult))
mtf_res_catchweight <- as.data.frame(catch(MTFresult))

mtf_res_landingnumber <- as.data.frame(landings.n(MTFresult))
mtf_res_landingweight <- as.data.frame(landings(MTFresult))

mtf_res_discardnumber <- as.data.frame(discards.n(MTFresult))
mtf_res_discardweight <- as.data.frame(discards(MTFresult))

mtf_res_harvest <-  as.data.frame(harvest(MTFresult))  
mtf_res_m <- as.data.frame(m(MTFresult)) 
mtf_res_range <- range(MTFresult)

for (ye_f in 1:foreperiod) {

ye <- ye_f + simperiod
# ----------------------------------------------------------------------------
# ASSIGN total CATCH 
# ----------------------------------------------------------------------------

catch_numbers <- data.frame(matrix(0, nrow=1, ncol=life_span))
colnames(catch_numbers) <- c(paste("age", c(0:(life_span-1)), sep=""))
rownames(catch_numbers) <- years.forecast[ye_f]

 for (ag_ in 1:num_classes) {   
       catch_n_oneage <- as.data.frame(mtf_res_catchnumber[mtf_res_catchnumber$age == age_vect[ag_],])     
       if (!any(is.na(catch_n_oneage$data)) ) {
       catch_all_years <- t(apply(tapply(catch_n_oneage$data,INDEX=catch_n_oneage$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))
       catch_numbers[1,ag_] <-  catch_all_years[ye,3] * 1000   # to get the numbers from thousands
       }
    }
    
           Inters[[ye]][[MTF_spe]]@totalcatch @Ctype <- "CATCHES" 
           Inters[[ye]][[MTF_spe]]@totalcatch @numbers <- catch_numbers
           
       catch_all_years <- t(apply(tapply(mtf_res_catchweight$data,INDEX=mtf_res_catchweight$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))
       Inters[[ye]][[MTF_spe]]@totalcatch @totalweight <- catch_all_years[ye,3] 
  
   Inters[[ye]][[MTF_spe]]@meanLength_catches <- sum(Inters[[ye]][[MTF_spe]]@totalcatch @numbers  * mid_lengths) / sum(Inters[[ye]][[MTF_spe]]@totalcatch @numbers ) 
   

# ----------------------------------------------------------------------------
# ASSIGN total LANDING 
# ----------------------------------------------------------------------------

landing_numbers <- data.frame(matrix(0, nrow=1, ncol=life_span))
colnames(landing_numbers) <- c(paste("age", c(0:(life_span-1)), sep=""))
rownames(landing_numbers) <- years.forecast[ye_f]

 for (ag_ in 1:num_classes) {   
       landing_n_oneage <- as.data.frame(mtf_res_landingnumber[mtf_res_landingnumber$age == age_vect[ag_],])     
       if (!any(is.na(landing_n_oneage$data)) ) {
       landing_all_years <- t(apply(tapply(landing_n_oneage$data,INDEX=landing_n_oneage$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))
       landing_numbers[1,ag_] <-  landing_all_years[ye,3] * 1000   # to get the numbers from thousands
       }
    }
    
           Inters[[ye]][[MTF_spe]]@totallanding @Ctype <- "LANDINGS" 
           Inters[[ye]][[MTF_spe]]@totallanding @numbers <- landing_numbers
           
      landing_all_years <- t(apply(tapply(mtf_res_landingweight$data,INDEX=mtf_res_landingweight$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))
       Inters[[ye]][[MTF_spe]]@totallanding @totalweight <- landing_all_years[ye,3] 
  
   #Inters[[ye]][[MTF_spe]]@meanLength_catches <- sum(Inters[[ye]][[MTF_spe]]@totalcatch @numbers  * mid_lengths) / sum(Inters[[ye]][[MTF_spe]]@totalcatch @numbers ) 
        
# ----------------------------------------------------------------------------
# ASSIGN total DISCARD 
# ----------------------------------------------------------------------------

discard_numbers <- data.frame(matrix(0, nrow=1, ncol=life_span))
colnames(discard_numbers) <- c(paste("age", c(0:(life_span-1)), sep=""))
rownames(discard_numbers) <- years.forecast[ye_f]

 for (ag_ in 1:num_classes) {   
       discard_n_oneage <- as.data.frame(mtf_res_discardnumber[mtf_res_discardnumber$age == age_vect[ag_],])     
       if (!any(is.na(discard_n_oneage$data)) ) {
       discard_all_years <- t(apply(tapply(discard_n_oneage$data,INDEX=discard_n_oneage$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))
       discard_numbers[1,ag_] <-  discard_all_years[ye,3] * 1000   # to get the numbers from thousands
       }
    }
    
           Inters[[ye]][[MTF_spe]]@totaldiscard @Ctype <- "DISCARDS" 
           Inters[[ye]][[MTF_spe]]@totaldiscard @numbers <- discard_numbers
           
       discard_all_years <- t(apply(tapply(mtf_res_discardweight$data,INDEX=mtf_res_discardweight$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))
       Inters[[ye]][[MTF_spe]]@totaldiscard @totalweight <- discard_all_years[ye,3] 
  
  # Inters[[ye]][[MTF_spe]]@meanLength_catches <- sum(Inters[[ye]][[MTF_spe]]@totalcatch @numbers  * mid_lengths) / sum(Inters[[ye]][[MTF_spe]]@totalcatch @numbers ) 
 
# ----------------------------------------------------------------------------
# ASSIGN STOCKs 
# ----------------------------------------------------------------------------

numbers_ex_all <- data.frame(matrix(0, nrow=life_span, ncol=length(MONTHS)))
rownames(numbers_ex_all) <- c(paste("age", c(0:(life_span-1)), sep=""))
colnames(numbers_ex_all) <- MONTHS

weights_ex_all <- data.frame(matrix(0, nrow=life_span, ncol=length(MONTHS)))
rownames(weights_ex_all) <- c(paste("age", c(0:(life_span-1)), sep=""))
colnames(weights_ex_all) <- MONTHS

    # correction for stock in numbers
     if(SAtool == "VIT") {
     corr_ =data.frame(correction_stock_nb_from_VIT(MTF_spe,num_classes_BACKUP)$average) 
     }    

    for (ag_ in 1:num_classes) {   
       
       stock_n_oneage <- as.data.frame(mtf_res_stocknumber[mtf_res_stocknumber$age == age_vect[ag_],]) 
       stock_all_years <- t(apply(tapply(stock_n_oneage$data,INDEX=stock_n_oneage$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))
      
      if (SAtool == "VIT")  {
       # application of correction
       numbers_ex_all[ag_,] <-  stock_all_years[ye,3] * 1000 * corr_[ag_,]  # to get the numbers from thousands
      } else {
       numbers_ex_all[ag_,] <-  stock_all_years[ye,3] * 1000   # to get the numbers from thousands
      } 

       mean_weight_oneage <- as.data.frame(mtf_res_stock_mean_w[mtf_res_stock_mean_w$age == age_vect[ag_],]) 
       stock_mw_all_years <- t(apply(tapply(mean_weight_oneage$data,INDEX=mean_weight_oneage$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))
       weights_ex_all[ag_,] <- stock_mw_all_years[ye,3]
    }
    

    # mean weight for total catches
       Inters[[ye]][[MTF_spe]]@meanWeight_catches <- sum(Inters[[ye]][[MTF_spe]]@totalcatch @numbers * weights_ex_all[,1]) / sum(Inters[[ye]][[MTF_spe]]@totalcatch @numbers ) * 1000
    # ---------------------------------------  

       numbers_ex_F <- data.frame(numbers_ex_all * sexratio)       
       numbers_ex_M <- data.frame(numbers_ex_all * (1-sexratio))

       Inters[[ye]][[MTF_spe]]@exploitedStock@numbers <- list(F=numbers_ex_F, M=numbers_ex_M) 

       sb_ex_F <- numbers_ex_F * weights_ex_all  /1000
       sb_ex_M <- numbers_ex_M * weights_ex_all  /1000  # to get the tons from kg

       Inters[[ye]][[MTF_spe]]@exploitedStock@SB <- list(F=sb_ex_F, M=sb_ex_M) 
       
       SS_numbers_F <- numbers_ex_F * t(maturity_prop[2,] )
       SS_numbers_M <- numbers_ex_M * t(maturity_prop[1,] )
       SS_number_all <- SS_numbers_F + SS_numbers_M
        SS_number_all_temp <- SS_number_all
       SS_number_all <- data.frame(matrix(colSums(SS_number_all), nrow=1, ncol=length(MONTHS)))
       colnames(SS_number_all) <- MONTHS
        
       Inters[[ye]][[MTF_spe]]@exploitedStock@SS.numbers <-  SS_number_all
       
       
       ssb_allyears <- t(apply(tapply(mtf_res_ssb$data,INDEX=mtf_res_ssb$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))  
       ssb_number <- data.frame(matrix( ssb_allyears[ye,3], nrow=1, ncol=length(MONTHS)))
       colnames(ssb_number) <- MONTHS
       
      # Inters[[ye]][[MTF_spe]]@exploitedStock@SSB <-  ssb_number
      if (SAtool == "VIT") { 
         SSB_value_corr <- data.frame(matrix(colSums(SS_number_all_temp * weights_ex_all/1000), nrow=1, ncol=length(MONTHS)))
         colnames(SSB_value_corr) <- MONTHS
         Inters[[ye]][[MTF_spe]]@exploitedStock@SSB <- SSB_value_corr  
      } else { 
         Inters[[ye]][[MTF_spe]]@exploitedStock@SSB <- ssb_number
      
      }
      
        Inters[[ye]][[MTF_spe]]@exploitedStock@annual.SSB <- mean( as.numeric(as.character(Inters[[ye]][[MTF_spe]]@exploitedStock@SSB)) ) 
     
      # weighted mean of mean weights by age; to be multiplied by 1000 to get grams
       Inters[[ye]][[MTF_spe]]@exploitedStock@meanLength <- sum(numbers_ex_all[,1] * mid_lengths) / sum(numbers_ex_all[,1] )
       mean_weight <- ( sum(weights_ex_all[,1] * (numbers_ex_all[,1] *1000)  ) / sum(numbers_ex_all[,1] *1000) ) * 1000 # to get the grams  
       Inters[[ye]][[MTF_spe]]@exploitedStock@meanWeight <- mean_weight
       
       # critical length
       biomass <- Inters[[ye]][[MTF_spe]]@exploitedStock @SB $F[,1] + Inters[[ye]][[MTF_spe]]@exploitedStock @SB $M[,1]
       critical_age <- which(biomass == max(biomass))
       Inters[[ye]][[MTF_spe]]@exploitedStock @criticalLength  <- mid_lengths[critical_age]
 
# harvest rate
# sum of total weight of all the fleet segment divided by total SB (males and females) of the first month summed up over the ages
total_SB <- sum( Inters[[ye]][[MTF_spe]]@exploitedStock @SB$F[,1]) + sum( Inters[[ye]][[MTF_spe]]@exploitedStock @SB$M[,1])
 Inters[[ye]][[MTF_spe]]@exploitedStock @harvestRate <-  Inters[[ye]][[MTF_spe]]@totalcatch@totalweight /total_SB

     Inters[[ye]][[MTF_spe]]@exploitedStock@annual.SB <- total_SB
 
harvest_all <- data.frame(matrix(NA, ncol=life_span, nrow=1))
colnames(harvest_all) <- c(paste("age", c(0:(life_span-1)), sep=""))

m_all <- data.frame(matrix(0, ncol=life_span, nrow=1))
colnames(m_all) <- c(paste("age", c(0:(life_span-1)), sep=""))

    for (ag_ in 1:num_classes) {   
       harvest_oneage <- as.data.frame(mtf_res_harvest[mtf_res_harvest$age == age_vect[ag_],])     
       harvest_all_years <- t(apply(tapply(harvest_oneage$data,INDEX=harvest_oneage$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))
       harvest_all[1,ag_] <-  harvest_all_years[ye,3]   # to get the numbers from thousands
    
       m_oneage <- as.data.frame(mtf_res_m[mtf_res_m$age == age_vect[ag_],])     
       m_all_years <- t(apply(tapply(m_oneage$data,INDEX=m_oneage$year,FUN=quantile,simplify=FALSE,probs=c(0.05,0.25,0.5,0.75,0.95)),1,unlist))
       m_all[1,ag_] <-  m_all_years[ye,3]   # to get the numbers from thousands
    } 
                                  
# exploitation Rate
# mean of harvest (between ages minfbar and maxfbar) divided by mean of (harvest+m) (only values between the ages minfbar and maxfbar) considering the mean over the ages
min_ <- data.frame(matrix(mtf_res_range, nrow=1))[1,6]
max_ <- data.frame(matrix(mtf_res_range, nrow=1))[1,7]

mean_harvest <- mean(as.numeric(as.character(harvest_all[1, (min_+1):(max_+1)])), na.rm=TRUE)

 Inters[[ye]][[MTF_spe]]@mortalities$F[length(BMT_FLEETSEGMENTS)+1] <- mean_harvest

m_harv <- m_all + harvest_all
mean_harvest_m <-  mean(as.numeric(as.character(m_harv[(min_+1):(max_+1)])))

 Inters[[ye]][[MTF_spe]]@mortalities$Z[length(BMT_FLEETSEGMENTS)+1] <- mean_harvest_m

Inters[[ye]][[MTF_spe]]@exploitedStock @exploitationRate <- mean_harvest/mean_harvest_m

Inters[[ye]][[MTF_spe]]@exploitedStock @L95 <- -1

# ----------------------------------------------------------------------------
# REFERENCE POINTs 
# ----------------------------------------------------------------------------
Inters[[ye]][[MTF_spe]]@referencePoints <- Inters[[1]][[MTF_spe]]@referencePoints
      
} # end years loop   

return(Inters)
}                                                            
