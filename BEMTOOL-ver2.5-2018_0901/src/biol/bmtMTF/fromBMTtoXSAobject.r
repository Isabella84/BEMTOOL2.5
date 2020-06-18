# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





fromBMTtoXSAobject <- function(XSAi) {


if (FALSE) {
XSAi <- XSAinfo
}

for (m_int in 1:length(BMT_SPECIES)) {

      associated_fleetsegment <- as.vector(cfg[rownames(cfg) == paste("casestudy.S", m_int, ".associatedFleetsegment", sep=""), ]) 
      associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!="" & associated_fleetsegment!="-"]
      n_fleet_for_species <- length(associated_fleetsegment)
      SAtool <- as.character(cfg[rownames(cfg) == paste("casestudy.S", m_int, ".StockAssessmentTool", sep=""),1])
      mortality_constant <- as.logical(cfg[rownames(cfg) == paste("casestudy.S", m_int, ".params", sep=""),18])  
      lifespan_F <- as.numeric(Populations[[m_int]]@lifespan[2,1])
      lifespan_M <- as.numeric(Populations[[m_int]]@lifespan[1,1])
      ALADYM_sim <- as.logical(cfg[rownames(cfg) == paste("casestudy.S", m_int, ".AladymSimulation", sep=""),1]) 
      
        if (exists("mat_slot")) { rm(mat_slot) }
        if (exists("mean_weight_slot")) { rm(mean_weight_slot) }
        if (exists("stock_mean_weight_slot")) { rm(stock_mean_weight_slot) }
        if (exists("catch_mean_weight_slot")) { rm(catch_mean_weight_slot) }
        if (exists("catch_n_slot")) {  rm(catch_n_slot)  }
        if (exists("m_slot")) {  rm(m_slot)  }
        if (exists("harvest_slot")) {  rm(harvest_slot)  }
        if (exists("stock_n_slot")) {  rm(stock_n_slot)  }
        if (exists("catch_slot")) {  rm(catch_slot)  }
        if (exists("zero_vector")) {  rm(zero_vector) }
        
        if (exists("mat_slot_M")) { rm(mat_slot_M) }
        if (exists("mean_weight_slot_M")) { rm(mean_weight_slot_M) }
        if (exists("stock_mean_weight_slot_M")) { rm(stock_mean_weight_slot_M) }
        if (exists("catch_mean_weight_slot_M")) { rm(catch_mean_weight_slot_M) }
        if (exists("catch_n_slot_M")) {  rm(catch_n_slot_M)  }
        if (exists("m_slot_M")) {  rm(m_slot_M)  }
        if (exists("harvest_slot_M")) {  rm(harvest_slot_M)  }
        if (exists("stock_n_slot_M")) {  rm(stock_n_slot_M)  }
        if (exists("catch_slot_M")) {  rm(catch_slot_M)  }
        if (exists("zero_vector_M")) {  rm(zero_vector_M) }
      
      
      SAtool_dummy <- ifelse( (SAtool == "none" | SAtool == "NONE"), "" , SAtool) 
          
      if (SAtool_dummy == "VIT") {
      print(paste("Trasforming BMT objects (from VIT assessment) in XSA object [",BMT_SPECIES[m_int],"]...", sep=""), quote=FALSE)
     # print(paste("(stock assessment tool = ",SAtool, ")", sep=""), quote=FALSE)
      minAge <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m_int, ".StockAssessmentTool", sep=""),5]))
      maxAge <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m_int, ".StockAssessmentTool", sep=""),6]))

      num_classes_all_years <- c(0)
      for (y_int in 1:simperiod) {
          num_classes_all_years <- c(num_classes_all_years,  length(VITinfo[[m_int]][[y_int]]$results[[1]]$age_classes)  )
      }
      num_classes_all_years <- num_classes_all_years[num_classes_all_years !=0]
      min_num_classes <- min(num_classes_all_years)

    for (y_int in 1:simperiod) {
       VIT.sex <-	as.logical(cfg[rownames(cfg) == paste("casestudy.S", m_int, ".StockAssessmentTool", sep=""),2])  
      if (!VIT.sex) {
               resultsVit <- VITinfo[[m_int]][[y_int]]$results[[1]]
              #  num_classes <- max(ages_F, ages_M) 
               num_classes <- length(VITinfo[[m_int]][[y_int]]$results[[1]]$age_classes)                 
                # maturity ratio
                   if (!exists("mat_slot")) {  mat_slot <-    as.numeric(as.character(resultsVit$ age_stocks  [1:min_num_classes, 8] ))
                   } else {  mat_slot <-  c(mat_slot,  as.numeric(as.character(resultsVit$ age_stocks  [1:min_num_classes, 8] ))   )   }                   
                   mat_slot[length(mat_slot)] <- mean(as.numeric(as.character(resultsVit$ age_stocks  [min_num_classes:num_classes, 8] )) )    

               # mean weight
                if (!exists("mean_weight_slot")) {  mean_weight_slot <-    as.numeric(as.character(resultsVit$ age_stocks  [1:min_num_classes, 7] )) /1000
               } else {   mean_weight_slot <-  c(mean_weight_slot,  as.numeric(as.character(resultsVit$ age_stocks  [1:min_num_classes, 7] ))/1000   )    }
                  mean_weight_slot[length(mean_weight_slot)] <- mean( as.numeric(as.character(resultsVit$ age_stocks  [min_num_classes:num_classes, 7] ))/1000  )  
              
               # catches in numbers
                if (!exists("catch_n_slot")) {  catch_n_slot <-    as.numeric(as.character(resultsVit$catches_nb[1:min_num_classes, 2])) / 1000
               } else {  catch_n_slot <-  c(catch_n_slot,  as.numeric(as.character(resultsVit$catches_nb[1:min_num_classes, 2])) / 1000   )   }
                catch_n_slot[length(catch_n_slot)] <- sum( as.numeric(as.character(resultsVit$catches_nb  [min_num_classes:num_classes, 2] ))/1000  )  
               
               # m 
               fV_Z <- as.numeric(as.character((resultsVit$VPA_results_mortalities   [1:num_classes, 2]))) 
               fV_F <- as.numeric(as.character((resultsVit$VPA_results_mortalities   [1:num_classes, 3]))) 
               diff_Z_F <-  fV_Z - fV_F
               if (!exists("m_slot")) {  m_slot <-    diff_Z_F[1:min_num_classes]
               } else {  m_slot <-  c(m_slot,  diff_Z_F[1:min_num_classes]   )  }
               m_slot[length(m_slot)] <- mean( diff_Z_F[min_num_classes:num_classes]  )   
               
              # harvest
              if (!exists("harvest_slot")) {  harvest_slot <-    fV_F[1:min_num_classes]
               } else {  harvest_slot <-  c(harvest_slot,  fV_F[1:min_num_classes]   )  }
               harvest_slot[length(harvest_slot)] <- mean( fV_F[min_num_classes:num_classes]  )    

              # stock in number
              if (!exists("stock_n_slot")) {  stock_n_slot <-    as.numeric(as.character(resultsVit$VPA_results_nb  [1:min_num_classes, 2] )) / 1000
               } else {  stock_n_slot <-  c(stock_n_slot,  as.numeric(as.character(resultsVit$VPA_results_nb  [1:min_num_classes, 2] )) / 1000   )  }
              stock_n_slot[length(stock_n_slot)] <- sum( as.numeric(as.character(resultsVit$VPA_results_nb  [min_num_classes:num_classes, 2] )) / 1000 )    
                              
              # catch in weight by year
               if (!exists("catch_slot")) {  catch_slot <- as.numeric(as.character(resultsVit$catches_w[num_classes+1,2]) ) / 1000
               } else {  catch_slot <-  c(catch_slot,  as.numeric(as.character(resultsVit$catches_w[num_classes+1,2]) ) / 1000   )  }
                
               # stock in weight by year
               if (!exists("stock_slot")) {  stock_slot <- as.numeric(as.character(resultsVit$VPA_results_w  [(num_classes+1), 3] ) ) / 1000
               } else {  stock_slot <-  c(stock_slot,  as.numeric(as.character(resultsVit$VPA_results_w  [(num_classes+1), 3] ) ) / 1000   )  }
              
               if (!exists("zero_vector")) {  zero_vector <- rep(0, min_num_classes)
               } else {  zero_vector <-  c(zero_vector,  rep(0, min_num_classes)   )  }
               
         } else {
    # VIT by sex  
      ages_F <- length(VITinfo[[m_int]][[y_int]]$results[[1]]$age_classes)
      ages_M <- length(VITinfo[[m_int]][[y_int]]$results[[2]]$age_classes)
      
        num_classes_all_years <- c(0)
      for (y_int in 1:simperiod) {
          num_classes_all_years <- c(num_classes_all_years,  length(VITinfo[[m_int]][[y_int]]$results[[1]]$age_classes)  )
           num_classes_all_years <- c(num_classes_all_years,  length(VITinfo[[m_int]][[y_int]]$results[[2]]$age_classes)  )
      }
      num_classes_all_years <- num_classes_all_years[num_classes_all_years !=0]
      min_num_classes <- min(num_classes_all_years)
      
      num_classes <- max(ages_F, ages_M) 
      lifespan_MF <- max(lifespan_F, lifespan_M) 
      # results of females
               resultsVit <- VITinfo[[m_int]][[y_int]]$results[[1]]
               resultsVit_M <- VITinfo[[m_int]][[y_int]]$results[[2]]
                # maturity ratio
               if (!exists("mat_slot")) {  mat_slot <-    as.numeric(as.character(resultsVit$ age_stocks  [1:min_num_classes, 8] ))
               } else {  mat_slot <-  c(mat_slot,  as.numeric(as.character(resultsVit$ age_stocks  [1:min_num_classes, 8] ))   )   }                 
               mat_slot[length(mat_slot)] <- mean(as.numeric(as.character(resultsVit$ age_stocks  [min_num_classes:num_classes, 8] )) )    

              if (!exists("mat_slot_M")) {  mat_slot_M <-    as.numeric(as.character(resultsVit_M$ age_stocks  [1:num_classes, 8] ))
               } else {  mat_slot_M <-  c(mat_slot_M,  as.numeric(as.character(resultsVit_M$ age_stocks  [1:num_classes, 8] ))   )   }
             mat_slot_M[length(mat_slot_M)] <- mean(as.numeric(as.character(resultsVit_M$ age_stocks  [min_num_classes:num_classes, 8] )) )    

               # mean weight
                if (!exists("mean_weight_slot")) {  mean_weight_slot <-    as.numeric(as.character(resultsVit$ age_stocks  [1:min_num_classes, 7] )) /1000
               } else {   mean_weight_slot <-  c(mean_weight_slot,  as.numeric(as.character(resultsVit$ age_stocks  [1:min_num_classes, 7] ))/1000   )    }
                mean_weight_slot[length(mean_weight_slot)] <- mean( as.numeric(as.character(resultsVit$ age_stocks  [min_num_classes:num_classes, 7] ))/1000  )  
                   
               if (!exists("mean_weight_slot_M")) {  mean_weight_slot_M <-    as.numeric(as.character(resultsVit_M$ age_stocks  [1:min_num_classes, 7] )) /1000
               } else {   mean_weight_slot_M <-  c(mean_weight_slot_M,  as.numeric(as.character(resultsVit_M$ age_stocks  [1:min_num_classes, 7] ))/1000   )    }
                mean_weight_slot_M[length(mean_weight_slot_M)] <- mean( as.numeric(as.character(resultsVit_M$ age_stocks  [min_num_classes:num_classes, 7] ))/1000  )  
                              
               # catches in numbers
                if (!exists("catch_n_slot")) {  catch_n_slot <-    as.numeric(as.character(resultsVit$catches_nb[1:min_num_classes, 2])) / 1000
               } else {  catch_n_slot <-  c(catch_n_slot,  as.numeric(as.character(resultsVit$catches_nb[1:min_num_classes, 2])) / 1000   )   }
                catch_n_slot[length(catch_n_slot)] <- sum( as.numeric(as.character(resultsVit$catches_nb  [min_num_classes:num_classes, 2] ))/1000  )  

               if (!exists("catch_n_slot_M")) {  catch_n_slot_M <-    as.numeric(as.character(resultsVit_M$catches_nb[1:min_num_classes, 2])) / 1000
               } else {  catch_n_slot_M <-  c(catch_n_slot_M,  as.numeric(as.character(resultsVit_M$catches_nb[1:min_num_classes, 2])) / 1000   )   }
                catch_n_slot_M[length(catch_n_slot_M)] <- sum( as.numeric(as.character(resultsVit_M$catches_nb  [min_num_classes:num_classes, 2] ))/1000  )  
                
               # m 
               fV_Z <- as.numeric(as.character((resultsVit$VPA_results_mortalities   [1:num_classes, 2]))) 
               fV_F <- as.numeric(as.character((resultsVit$VPA_results_mortalities   [1:num_classes, 3]))) 
               diff_Z_F <-  fV_Z - fV_F
               if (!exists("m_slot")) {  m_slot <-    diff_Z_F[1:min_num_classes]
               } else {  m_slot <-  c(m_slot,  diff_Z_F[1:min_num_classes]   )  }
               m_slot[length(m_slot)] <- mean( diff_Z_F[min_num_classes:num_classes]  )   

               fV_Z_M <- as.numeric(as.character((resultsVit_M$VPA_results_mortalities   [1:num_classes, 2]))) 
               fV_F_M <- as.numeric(as.character((resultsVit_M$VPA_results_mortalities   [1:num_classes, 3]))) 
               diff_Z_F_M <-  fV_Z_M - fV_F_M
               if (!exists("m_slot_M")) {  m_slot_M <-    diff_Z_F_M[1:min_num_classes]
               } else {  m_slot_M <-  c(m_slot_M,  diff_Z_F_M[1:min_num_classes]   )  }
              m_slot_M[length(m_slot_M)] <- mean( diff_Z_F_M[min_num_classes:num_classes]  )   
                              
              # harvest
              if (!exists("harvest_slot")) {  harvest_slot <-    fV_F[1:min_num_classes]
               } else {  harvest_slot <-  c(harvest_slot,  fV_F[1:min_num_classes]   )  }
               harvest_slot[length(harvest_slot)] <- mean( fV_F[min_num_classes:num_classes]  )    
              
               if (!exists("harvest_slot_M")) {  harvest_slot_M <-    fV_F_M[1:min_num_classes]
               } else {  harvest_slot_M <-  c(harvest_slot_M,  fV_F_M[1:min_num_classes]   )  }
               harvest_slot_M[length(harvest_slot_M)] <- mean( fV_F_M[min_num_classes:num_classes]  )
                   
              # stock in number
              if (!exists("stock_n_slot")) {  stock_n_slot <-    as.numeric(as.character(resultsVit$VPA_results_nb  [1:(min_num_classes), 2] )) / 1000
               } else {  stock_n_slot <-  c(stock_n_slot,  as.numeric(as.character(resultsVit$VPA_results_nb  [1:(min_num_classes), 2] )) / 1000   )  }
              stock_n_slot[length(stock_n_slot)] <- sum( as.numeric(as.character(resultsVit$VPA_results_nb  [min_num_classes:num_classes, 2] )) / 1000 )    
              
              
              if (!exists("stock_n_slot_M")) {  stock_n_slot_M <-    as.numeric(as.character(resultsVit_M$VPA_results_nb  [1:(min_num_classes), 2] )) / 1000
               } else {  stock_n_slot_M <-  c(stock_n_slot_M,  as.numeric(as.character(resultsVit_M$VPA_results_nb  [1:(min_num_classes), 2] )) / 1000   )  }
              stock_n_slot_M[length(stock_n_slot_M)] <- sum( as.numeric(as.character(resultsVit_M$VPA_results_nb  [min_num_classes:num_classes, 2] )) / 1000 )    
              
              # catch in weight by year
               if (!exists("catch_slot")) {  catch_slot <- as.numeric(as.character(resultsVit$catches_w[num_classes+1,2]) ) / 1000
               } else {  catch_slot <-  c(catch_slot,  as.numeric(as.character(resultsVit$catches_w[num_classes+1,2]) ) / 1000   )  }
               if (!exists("catch_slot_M")) {  catch_slot_M <- as.numeric(as.character(resultsVit_M$catches_w[num_classes+1,2]) ) / 1000
               } else {  catch_slot_M <-  c(catch_slot_M,  as.numeric(as.character(resultsVit_M$catches_w[num_classes+1,2]) ) / 1000   )  }
               # stock in weight by year
               if (!exists("stock_slot")) {  stock_slot <- as.numeric(as.character(resultsVit$VPA_results_w  [(num_classes+1), 3] ) ) / 1000
               } else {  stock_slot <-  c(stock_slot,  as.numeric(as.character(resultsVit$VPA_results_w  [(num_classes+1), 3] ) ) / 1000   )  }
               if (!exists("stock_slot_M")) {  stock_slot_M <- as.numeric(as.character(resultsVit_M$VPA_results_w  [(num_classes+1), 3] ) ) / 1000
               } else {  stock_slot_M <-  c(stock_slot_M,  as.numeric(as.character(resultsVit_M$VPA_results_w  [(num_classes+1), 3] ) ) / 1000   )  }
               
               if (!exists("zero_vector")) {  zero_vector <- rep(0, min_num_classes)
               } else {  zero_vector <-  c(zero_vector,  rep(0, min_num_classes)   )  }
              
           }          
}

      if (VIT.sex) { 
       # lifespan <- lifespan_MF 
        catch_slot <- catch_slot + catch_slot_M
        catch_n_slot <- catch_n_slot + catch_n_slot_M
        mean_weight_slot <- mean(mean_weight_slot, mean_weight_slot_M)
        stock_slot <- stock_slot + stock_slot_M
        stock_n_slot <- stock_n_slot + stock_n_slot_M
        m_slot <- mean(m_slot, m_slot_M )
        mat_slot <- mean(mat_slot, mat_slot_M)
        harvest_slot <- mean(mat_slot, mat_slot_M)
      }

        bmtXSAstock <- new(Class= "FLStock")
        bmtXSAstock@catch <- FLQuant(catch_slot, dimnames=list(age="all", year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@catch.n  <- FLQuant(catch_n_slot, dimnames=list(age=(0:(min_num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@catch.wt  <- FLQuant(mean_weight_slot, dimnames=list(age=(0:(min_num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@discards <- FLQuant(c(0,0,0,0), dimnames=list(age="all", year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@discards.n   <- FLQuant(zero_vector, dimnames=list(age=(0:(min_num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@discards.wt   <- FLQuant(zero_vector, dimnames=list(age=(0:(min_num_classes-1)), year=years[1]:years[simperiod]), units="NA")    
        bmtXSAstock@landings <- FLQuant(catch_slot, dimnames=list(age="all", year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@landings.n   <- FLQuant(catch_n_slot, dimnames=list(age=(0:(min_num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@landings.wt   <- FLQuant(mean_weight_slot, dimnames=list(age=(0:(min_num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@stock <- FLQuant(stock_slot, dimnames=list(age="all", year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@stock.n   <- FLQuant(stock_n_slot, dimnames=list(age=(0:(min_num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@stock.wt   <- FLQuant(mean_weight_slot, dimnames=list(age=(0:(min_num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@m   <-  FLQuant(m_slot, dimnames=list(age=(0:(min_num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@mat   <- FLQuant(mat_slot, dimnames=list(age=(0:(min_num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@harvest   <- FLQuant(harvest_slot, dimnames=list(age=(0:(min_num_classes-1)), year=years[1]:years[simperiod]), units="f")
        bmtXSAstock@harvest.spwn   <- FLQuant(zero_vector, dimnames=list(age=(0:(min_num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@m.spwn   <- FLQuant(zero_vector, dimnames=list(age=(0:(min_num_classes-1)), year=years[1]:years[simperiod]), units="NA")

        bmtXSAstock@name  <- paste("Index File", BMT_SPECIES[m_int], "in GSA", BMT_GSA)
        bmtXSAstock@desc <- paste("Generated by BEMTOOL software",Sys.Date() )

        range_vector <- c(0,(min_num_classes-1),(min_num_classes-1),years[1],years[simperiod],minAge,maxAge)
        names(range_vector) <- c("min", "max", "plusgroup", "minyear", "maxyear", "minfbar", "maxfbar")
        bmtXSAstock@range <-  range_vector

       save_path <- paste(casestudy_path, "/", harvest_rule_id,"/Biological Pressure Impact/MSTF - ", BMT_SPECIES[m_int],"/", casestudy_name, " - ", BMT_SPECIES[m_int], " XSA object FORE", harvest_rule_id,".dat", sep="")     
        dput(bmtXSAstock, file=save_path)

        XSAi[[m_int]]$results <- list(results=bmtXSAstock)
      
      
      } else if (SAtool_dummy == "Report") {
      print(paste("Trasforming BMT objects (from Report assessment) in XSA object [",BMT_SPECIES[m_int],"]...", sep=""), quote=FALSE)
     # print(paste("(stock assessment tool = ",SAtool, ")", sep=""), quote=FALSE)
               num_classes <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m_int, ".StockAssessmentTool", sep=""),5]))
               resultsReport <- ReportINFO[[m_int]]$results
             for (yea in 1:simperiod) {                              
                # maturity ratio
               if (!exists("mat_slot")) {  mat_slot <-    as.numeric(as.character(resultsReport$ maturity  [, yea] ))
               } else {  mat_slot <-  c(mat_slot,  as.numeric(as.character(resultsReport$ maturity  [, yea] ))   )   }
            }
            for (yea in 1:simperiod) {                              
               # mean weight for catch
                if (!exists("catch_mean_weight_slot")) {  catch_mean_weight_slot <-    as.numeric(as.character(resultsReport$ catches_wt  [, yea] ))
               } else {   catch_mean_weight_slot <-  c(catch_mean_weight_slot,  as.numeric(as.character(resultsReport$ catches_wt  [, yea] ))  )    }
            }
            for (yea in 1:simperiod) {                              
               # mean weight for stock
                if (!exists("stock_mean_weight_slot")) {  stock_mean_weight_slot <-    as.numeric(as.character(resultsReport$stock_wt  [, yea] ))
               } else {   stock_mean_weight_slot <-  c(stock_mean_weight_slot,  as.numeric(as.character(resultsReport$stock_wt  [, yea] ))  )    }
            }
           for (yea in 1:simperiod) {                              
               # catch in numbers
                if (!exists("catch_n_slot")) {  catch_n_slot <-    as.numeric(as.character(resultsReport$catches_nb  [, yea] ))
               } else {   catch_n_slot <-  c(catch_n_slot,  as.numeric(as.character(resultsReport$catches_nb  [, yea] ))  )    }
            } 
           for (yea in 1:simperiod) {                              
               # natural mortality
                if (!exists("m_slot")) {  m_slot <-    as.numeric(as.character(resultsReport$natural_mortality  [, yea] ))
               } else {   m_slot <-  c(m_slot,  as.numeric(as.character(resultsReport$natural_mortality  [, yea] ))  )    }
            } 
           for (yea in 1:simperiod) {                              
               # fishing mortality
                if (!exists("harvest_slot")) {  harvest_slot <-    as.numeric(as.character(resultsReport$fishing_mortality  [, yea] ))
               } else {   harvest_slot <-  c(harvest_slot,  as.numeric(as.character(resultsReport$fishing_mortality  [, yea] ))  )    }
            } 
           for (yea in 1:simperiod) {                              
               # # stock in number
                if (!exists("stock_n_slot")) {  stock_n_slot <-    as.numeric(as.character(resultsReport$stock_nb  [, yea] ))
               } else {   stock_n_slot <-  c(stock_n_slot,  as.numeric(as.character(resultsReport$stock_nb  [, yea] ))  )    }
            } 
           for (yea in 1:simperiod) {    
              # catch in weight by year
           if (!exists("catch_slot")) {  catch_slot <- sum(as.numeric(as.character(resultsReport$catches_nb  [, yea] )) * as.numeric(as.character(resultsReport$catches_wt  [, yea] )) ) 
           } else {  catch_slot <-  c(catch_slot,   sum(as.numeric(as.character(resultsReport$catches_nb  [, yea] )) * as.numeric(as.character(resultsReport$catches_wt  [, yea] )) ) ) }
          }
           for (yea in 1:simperiod) {      
               # stock in weight by year
               if (!exists("stock_slot")) {  stock_slot <- sum(as.numeric(as.character(resultsReport$stock_nb  [, yea] )) * as.numeric(as.character(resultsReport$stock_wt  [, yea] )) ) 
               } else {  stock_slot <-  c(stock_slot,   sum(as.numeric(as.character(resultsReport$stock_nb  [, yea] )) * as.numeric(as.character(resultsReport$stock_wt  [, yea] )) ) ) }
          }
           for (yea in 1:simperiod) {      
               if (!exists("zero_vector")) {  zero_vector <- rep(0, num_classes)
               } else {  zero_vector <-  c(zero_vector,  rep(0, num_classes)   )  }
          }
        bmtXSAstock <- new(Class= "FLStock")
        bmtXSAstock@catch <- FLQuant(catch_slot, dimnames=list(age="all", year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@catch.n  <- FLQuant(catch_n_slot, dimnames=list(age=(0:(num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@catch.wt  <- FLQuant(catch_mean_weight_slot, dimnames=list(age=(0:(num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@discards <- FLQuant(c(0,0,0,0), dimnames=list(age="all", year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@discards.n   <- FLQuant(zero_vector, dimnames=list(age=(0:(num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@discards.wt   <- FLQuant(zero_vector, dimnames=list(age=(0:(num_classes-1)), year=years[1]:years[simperiod]), units="NA")    
        bmtXSAstock@landings <- FLQuant(catch_slot, dimnames=list(age="all", year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@landings.n   <- FLQuant(catch_n_slot, dimnames=list(age=(0:(num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@landings.wt   <- FLQuant(catch_mean_weight_slot, dimnames=list(age=(0:(num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@stock <- FLQuant(stock_slot, dimnames=list(age="all", year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@stock.n   <- FLQuant(stock_n_slot, dimnames=list(age=(0:(num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@stock.wt   <- FLQuant(stock_mean_weight_slot, dimnames=list(age=(0:(num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@m   <-  FLQuant(m_slot, dimnames=list(age=(0:(num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@mat   <- FLQuant(mat_slot, dimnames=list(age=(0:(num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@harvest   <- FLQuant(harvest_slot, dimnames=list(age=(0:(num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@harvest.spwn   <- FLQuant(zero_vector, dimnames=list(age=(0:(num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@m.spwn   <- FLQuant(zero_vector, dimnames=list(age=(0:(num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@name  <- paste("Index File", BMT_SPECIES[m_int], "in GSA", BMT_GSA)
        bmtXSAstock@desc <- paste("Generated by BEMTOOL software",Sys.Date() )
        minAge <- as.numeric(as.character(resultsReport$age_rangeF$min)) 
        maxAge <- as.numeric(as.character(resultsReport$age_rangeF$max)) 
        range_vector <- c(0,(num_classes-1),NA,years[1],years[simperiod],minAge,maxAge)
        names(range_vector) <- c("min", "max", "plusgroup", "minyear", "maxyear", "minfbar", "maxfbar")
        bmtXSAstock@range <-  range_vector
        save_path <- paste(casestudy_path, "/", harvest_rule_id,"/Biological Pressure Impact/MSTF - ", BMT_SPECIES[m_int],"/", casestudy_name, " - ", BMT_SPECIES[m_int], " XSA object FORE ", harvest_rule_id,".dat", sep="")   
        dput(bmtXSAstock, file=save_path)
        print(paste("Saving XSA object in", save_path))
        XSAi[[m_int]]$results <- bmtXSAstock 
        
             
      } else if ( (SAtool_dummy == "NONE" | SAtool_dummy == "SURBA" | SAtool_dummy == "") & ALADYM_sim ) {  # bmt to aladym  
      
     phase <<-  "SIMULATION" 
      ALADYM_spe <<- m_int
      source(paste(ALADYM_home, "/src/paths.r", sep=""))
           phase <<-  "FORECAST" 
      
       source(paste(getwd(), "/src/biol/bmtALADYM/reloadEnvSpecies.r", sep=""))  

       ind_weightsM <- data.frame(cbind(BAS$MAge, BAS$MWeight))
       colnames(ind_weightsM) <- c("age_fraction","weight")
       ind_weightsM$age <-  trunc(ind_weightsM$age_fraction)
       weights_M <- aggregate(ind_weightsM$weight, by=list(ind_weightsM$age), FUN="mean")
       
       first_age <- trunc(INP$tr/12)
       num_classes <- max(as.numeric(as.character(Populations[[m_int]]@lifespan$lifespan))) 
       
       weights_M_rep <- rep(NA, num_classes) 
       weights_M_rep[(first_age+1):num_classes] <- weights_M[1:(nrow(weights_M)-1),2] 
       
       weights_M <- weights_M_rep
       
               weights_M[which(is.na(weights_M))] <- weights_M[which(!is.na(weights_M))][1]
      # weights_M <- weights_M[1:(nrow(weights_M)-1),2]

       ind_weightsF <- data.frame(cbind(BAS$FAge, BAS$FWeight))
       colnames(ind_weightsF) <- c("age_fraction","weight")
       ind_weightsF$age <-  trunc(ind_weightsF$age_fraction)
       weights_F <- aggregate(ind_weightsF$weight, by=list(ind_weightsF$age), FUN="mean")
       
       first_age <- trunc(INP$tr/12)
       num_classes <- max(as.numeric(as.character(Populations[[m_int]]@lifespan$lifespan))) 
       
       weights_F_rep <- rep(NA, num_classes) 
       weights_F_rep[(first_age+1):num_classes] <- weights_F[1:(nrow(weights_F)-1),2] 
       
       weights_F <- weights_F_rep

        weights_F[which(is.na(weights_F))] <- weights_F[which(!is.na(weights_F))][1]
       
       # weights_F <- weights_F[1:(nrow(weights_F)-1),2]

       all_weights <- data.frame(rbind(weights_M, weights_F) )
       
       cw_sw <- colMeans(all_weights, na.rm=T)
       
       mortM <- rowMeans(Populations[[m_int]]@M.vect$M, na.rm=T)
       
       mortM[which(is.na(mortM))] <- mortM[which(!is.na(mortM))][1]
       
       mortF <- rowMeans(Populations[[m_int]]@M.vect$F, na.rm=T)
       
        mortF[which(is.na(mortF))] <- mortF[which(!is.na(mortF))][1]

       all_mort <- data.frame(rbind(mortM, mortF) )
       mort_xsa <- colMeans(all_mort, na.rm=T)

       all_F <- read.csv(F_BYGEAR_table, sep=";")

       all_F_m <- all_F[all_F$sex== "M",]
       all_F_f <- all_F[all_F$sex== "F",]
       
       all_F_m[all_F_m == 0] <- NA
       all_F_f[all_F_f == 0] <- NA
 
       all_F_allsex <- all_F_m
       
      for (nro in 1:nrow(all_F_allsex)) {
           mm <- data.frame(rbind(all_F_m[nro,], all_F_f[nro,] ))
          all_F_allsex[nro, colnames(all_F_allsex) != "Year.Age" & colnames(all_F_allsex) != "sex" & colnames(all_F_allsex) != "Gear"] <- colMeans(mm[,(colnames(mm) != "Year.Age" & colnames(mm) != "sex" & colnames(mm) != "Gear")], na.rm=T)
      }  
      
      unique(all_F_allsex$Gear)
      
      all_F_total <-  all_F_allsex[all_F_allsex$Gear ==   unique(all_F_allsex$Gear)[1],]

      for (nro in 1:nrow(all_F_total)) {
           mm <- all_F_allsex[all_F_allsex$Year.Age == all_F_total$Year.Age[nro],colnames(all_F_allsex) != "Year.Age" & colnames(all_F_allsex) != "sex" & colnames(all_F_allsex) != "Gear"]
         all_F_total[nro, colnames(all_F_total) != "Year.Age" & colnames(all_F_total) != "sex" & colnames(all_F_total) != "Gear"] <- colSums(mm[,(colnames(mm) != "Year.Age" & colnames(mm) != "sex" & colnames(mm) != "Gear")], na.rm=T)
      }


       print(paste("Trasforming BMT objects (from ALADYM simulation) in XSA object [",BMT_SPECIES[m_int],"]...", sep=""), quote=FALSE)
            # print(paste("(stock assessment tool = ",SAtool, ")", sep=""), quote=FALSE)
       num_classes <- max(as.numeric(as.character(Populations[[m_int]]@lifespan$lifespan))) 

             for (yea in 1:simperiod) {                                               
                # maturity ratio                                             
               if (!exists("mat_slot")) {  mat_slot <- as.numeric(as.character(colMeans(Populations[[m_int]]@maturity.vect) ))
               } else {  mat_slot <-  c(mat_slot,  as.numeric(as.character(colMeans(Populations[[m_int]]@maturity.vect) ))  )   }
            }
            for (yea in 1:simperiod) {                              
               # mean weight for catch                                                                             rep(Interactionsyear[[yea]][[m_int]]@totalcatch@meanWeight , num_classes)
                if (!exists("catch_mean_weight_slot")) {  catch_mean_weight_slot <- as.numeric(as.character(cw_sw  ))
               } else {   catch_mean_weight_slot <-  c(catch_mean_weight_slot,  as.numeric(as.character(cw_sw ))  )    }
            }
            for (yea in 1:simperiod) {                              
               # mean weight for stock
                if (!exists("stock_mean_weight_slot")) {  stock_mean_weight_slot <-    as.numeric(as.character(cw_sw ))
               } else {   stock_mean_weight_slot <-  c(stock_mean_weight_slot,  as.numeric(as.character(cw_sw ))  )    }
            }
           for (yea in 1:simperiod) {                              
               # catch in numbers
                if (!exists("catch_n_slot")) {  catch_n_slot <-    as.numeric(as.character(Interactionsyear[[yea]][[m_int]]@totalcatch@numbers ))
               } else {   catch_n_slot <-  c(catch_n_slot,  as.numeric(as.character(Interactionsyear[[yea]][[m_int]]@totalcatch@numbers ))  )    }
            } 
           for (yea in 1:simperiod) {                              
               # natural mortality
                if (!exists("m_slot")) {  m_slot <-    as.numeric(as.character(mort_xsa))
               } else {   m_slot <-  c(m_slot,  as.numeric(as.character(mort_xsa))  )    }
            } 
           for (yea in 1:simperiod) {                              
               # fishing mortality
                if (!exists("harvest_slot")) {  harvest_slot <-    as.numeric(as.character(all_F_total[yea, colnames(all_F_total) != "Year.Age" & colnames(all_F_total) != "sex" & colnames(all_F_total) != "Gear"] ))
               } else {   harvest_slot <-  c(harvest_slot,   as.numeric(as.character(all_F_total[yea, colnames(all_F_total) != "Year.Age" & colnames(all_F_total) != "sex" & colnames(all_F_total) != "Gear"] ))  )    }
            } 
           for (yea in 1:simperiod) {                              
               # # stock in number
               
               numb_M <- rowMeans(Interactionsyear[[yea]][[m_int]]@exploitedStock@numbers$M ) 
               numb_F <- rowMeans(Interactionsyear[[yea]][[m_int]]@exploitedStock@numbers$F ) 
               
               all_numb <- data.frame(rbind(numb_M, numb_F) )
               stocknumbers_xsa <- colSums(all_numb, na.rm=T)/1000

                if (!exists("stock_n_slot")) {  stock_n_slot <-    as.numeric(as.character(stocknumbers_xsa ))
               } else {   stock_n_slot <-  c(stock_n_slot,  as.numeric(as.character(stocknumbers_xsa ))  )    }
            } 
            
            stock_slot <- stock_n_slot * stock_mean_weight_slot
            catch_slot <- catch_n_slot/1000 * stock_mean_weight_slot
            

          # for (yea in 1:simperiod) {    
#              # catch in weight by year 
#           if (!exists("catch_slot")) {  catch_slot <- sum(as.numeric(as.character(resultsReport$catches_nb[, yea] )) * as.numeric(as.character(resultsReport$catches_wt[, yea] )) ) 
#          } else {  catch_slot <-  c(catch_slot,   sum(as.numeric(as.character(resultsReport$catches_nb[, yea] )) * as.numeric(as.character(resultsReport$catches_wt[, yea] )) ) ) }
#         }
#           for (yea in 1:simperiod) {      
#               # stock in weight by year
#               if (!exists("stock_slot")) {  stock_slot <- sum(as.numeric(as.character(resultsReport$stock_nb  [, yea] )) * as.numeric(as.character(resultsReport$stock_wt  [, yea] )) ) 
#              } else {  stock_slot <-  c(stock_slot,   sum(as.numeric(as.character(resultsReport$stock_nb  [, yea] )) * as.numeric(as.character(resultsReport$stock_wt  [, yea] )) ) ) }
#        }
 
        
           for (yea in 1:simperiod) {      
               if (!exists("zero_vector")) {  zero_vector <- rep(0, num_classes)
               } else {  zero_vector <-  c(zero_vector,  rep(0, num_classes) )  }
          }
          
        bmtXSAstock <- new(Class= "FLStock")
        bmtXSAstock@catch <- FLQuant(catch_slot, dimnames=list(age="all", year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@catch.n  <- FLQuant(catch_n_slot/1000, dimnames=list(age=(0:(num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@catch.wt  <- FLQuant(catch_mean_weight_slot/1000, dimnames=list(age=(0:(num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@discards <- FLQuant(c(0,0,0,0), dimnames=list(age="all", year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@discards.n   <- FLQuant(zero_vector, dimnames=list(age=(0:(num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@discards.wt   <- FLQuant(zero_vector, dimnames=list(age=(0:(num_classes-1)), year=years[1]:years[simperiod]), units="NA")    
        bmtXSAstock@landings <- FLQuant(catch_slot, dimnames=list(age="all", year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@landings.n   <- FLQuant(catch_n_slot, dimnames=list(age=(0:(num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@landings.wt   <- FLQuant(catch_mean_weight_slot/1000, dimnames=list(age=(0:(num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@stock <- FLQuant(stock_slot, dimnames=list(age="all", year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@stock.n   <- FLQuant(stock_n_slot, dimnames=list(age=(0:(num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@stock.wt   <- FLQuant(stock_mean_weight_slot/1000, dimnames=list(age=(0:(num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@m   <-  FLQuant(m_slot, dimnames=list(age=(0:(num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@mat   <- FLQuant(mat_slot, dimnames=list(age=(0:(num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@harvest   <- FLQuant(harvest_slot, dimnames=list(age=(0:(num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@harvest@units <- "f"
        bmtXSAstock@harvest.spwn   <- FLQuant(zero_vector, dimnames=list(age=(0:(num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@m.spwn   <- FLQuant(zero_vector, dimnames=list(age=(0:(num_classes-1)), year=years[1]:years[simperiod]), units="NA")
        bmtXSAstock@name  <- paste("Index File", BMT_SPECIES[m_int], "in GSA", BMT_GSA)
        bmtXSAstock@desc <- paste("Generated by BEMTOOL software",Sys.Date() )
        minAge <- as.numeric(as.character(max(as.numeric(ALADYM_GUI_simulations[[m_int]]@fishingmortality$min))))  
        maxAge <- as.numeric(as.character(min(as.numeric(ALADYM_GUI_simulations[[m_int]]@fishingmortality$max)))) 
        range_vector <- c(0,(num_classes-1),NA,years[1],years[simperiod],minAge,maxAge)
        names(range_vector) <- c("min", "max", "plusgroup", "minyear", "maxyear", "minfbar", "maxfbar")
        bmtXSAstock@range <-  range_vector
        save_path <- paste(casestudy_path, "/", harvest_rule_id,"/Biological Pressure Impact/MSTF - ", BMT_SPECIES[m_int],"/", casestudy_name, " - ", BMT_SPECIES[m_int], " XSA object FORE ", harvest_rule_id,".dat", sep="")   
        dput(bmtXSAstock, file=save_path)
        print(paste("Saving XSA object in", save_path))
        XSAi[[m_int]]$results <- bmtXSAstock
          
      
      }   # end Report loop    
  

} # end species loop

return(XSAi)
}