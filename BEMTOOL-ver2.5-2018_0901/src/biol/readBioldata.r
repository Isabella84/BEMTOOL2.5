# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

# ---------------------------------------------------------------------------------------------------
# 6.1 STEP: read survey data from files in cfg table according the related parameters for the reading
# --------------------------------------------------------------------------------------------------- 
#         (example of stock assessment tool parameters for VIT)
#[VIT.sex]	     [VIT.file.female]	      [VIT.file.males]	     [VIT.file.combined]	       [VIT.sexratio]	      [VIT.analysis.length]	        [VIT.analysis.discard]
#FALSE	         C:\	                    C:\	                    C:\	                        0.3	                FALSE	                        FALSE

 # for each species call the specified file obtained from VIT, XSA or SURBA according the indication in cfg table  [casestudy.S1.StockAssessmentTool]
      #se VIT --> VIT function
      #se XSA --> XSA function
      #se SURBA --> SURBA function
      
      # da eliminare
#  m_int=1
#  y_int=1

missing_SA <<- data.frame(matrix( NA, ncol=length(years), nrow=length(BMT_SPECIES) ) )
colnames(missing_SA) <- years
rownames(missing_SA) <- BMT_SPECIES   

 for (m_int in 1:length(BMT_SPECIES)) {
 print(paste("Assign values from external stock assessment [",BMT_SPECIES[m_int],"]", sep=""), quote=FALSE)
#      associated_fleetsegment <- as.vector(cfg[rownames(cfg) == paste("casestudy.S", m_int, ".associatedFleetsegment", sep=""), ]) 
#      associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!="" & associated_fleetsegment!="-"]
#    #  n_fleet_for_species <- length(associated_fleetsegment)
                 
      SAtool <- as.character(cfg[rownames(cfg) == paste("casestudy.S", m_int, ".StockAssessmentTool", sep=""),1])
      print(paste("(stock assessment tool = ",SAtool, ")", sep=""), quote=FALSE)
      mortality_constant <- as.logical(cfg[rownames(cfg) == paste("casestudy.S", m_int, ".params", sep=""),18])  
      ALADYM_flag <- as.logical(cfg[rownames(cfg) == paste("casestudy.S", m_int, ".AladymSimulation", sep=""),1])
   # ----------------------------------------------------------------------------
   # READING DATA FROM VIT
   # ----------------------------------------------------------------------------
             
      if (SAtool == "VIT") {
           VIT.sex <-	as.logical(cfg[rownames(cfg) == paste("casestudy.S", m_int, ".StockAssessmentTool", sep=""),2])  
           VIT.analysis.discard <- as.logical(cfg[rownames(cfg) == paste("casestudy.S", m_int, ".StockAssessmentTool", sep=""),4])  
                   
           # if (exists("VITinfo") ) { rm(VITinfo) }
          if (exists("VITinfo_onespecies") ) { rm(VITinfo_onespecies) }
          
             y_ord <- 0
            for (y_int in 1:simperiod) {
            
                filePath_F <- as.character(cfg[rownames(cfg) == paste("casestudy.S", m_int, ".StockAss.fileF", sep=""),y_int] )
                filePath_M <- as.character(cfg[rownames(cfg) == paste("casestudy.S", m_int, ".StockAss.fileM", sep=""),y_int] )
                filePath_C <- as.character(cfg[rownames(cfg) == paste("casestudy.S", m_int, ".StockAss.fileC", sep=""),y_int] )
               
               if ( VIT.sex & filePath_F != "-" & filePath_M != "-") {
                   VIToutput <- readVIT(y_int, m_int, n_fleet_for_species)
                   VITinfo1s <- list(species=m_int, year=y_int, results=VIToutput)
                   missing_SA[m_int, y_int] <- "Y"
               } else if ( !VIT.sex & filePath_C != "-") {
                   VIToutput <- readVIT(y_int, m_int, n_fleet_for_species)
                   VITinfo1s <- list(species=m_int, year=y_int, results=VIToutput)
                   missing_SA[m_int, y_int] <- "Y"
               } else {
                   VITinfo1s <- list(species=m_int, year=y_int, results=NA)
                   missing_SA[m_int, y_int] <- "N"
               }
 
               if (!exists("VITinfo_onespecies")) {
                   VITinfo_onespecies <- list(VITinfo1s)
               } else {
                   VITinfo_onespecies <- c(VITinfo_onespecies, list(VITinfo1s))
               } 
             
             if (missing_SA[m_int, y_int] == "Y") {
             y_ord <- y_ord + 1 
               if (!VIT.sex) {
age_classes_C <- length(VIToutput[[1]]$age_classes)
Interactionsyear <<- updateBiologicalfromVIT(ALADYM_flag,Populations,Interactionsyear,VIToutput[[1]],y_ord,m_int,"C",VIT.analysis.discard,mortality_constant,age_classes_C,y_int)[[2]]
Populations <<- updateBiologicalfromVIT(ALADYM_flag,Populations,Interactionsyear,VIToutput[[1]],y_ord,m_int,"C",VIT.analysis.discard,mortality_constant,age_classes_C,y_int)[[1]]
               } else {
age_classes_MF <- max(length(VIToutput[[1]]$age_classes) , length(VIToutput[[2]]$age_classes))
Interactionsyear<<-updateBiologicalfromVIT(ALADYM_flag,Populations,Interactionsyear,VIToutput[[1]],y_ord,m_int,"F",VIT.analysis.discard,mortality_constant,age_classes_MF,y_int)[[2]]
Populations <<- updateBiologicalfromVIT(ALADYM_flag,Populations,Interactionsyear,VIToutput[[1]],y_ord,m_int,"F",VIT.analysis.discard,mortality_constant,age_classes_MF,y_int)[[1]]
Interactionsyear<<-updateBiologicalfromVIT(ALADYM_flag,Populations,Interactionsyear,VIToutput[[2]],y_ord,m_int,"M",VIT.analysis.discard,mortality_constant,age_classes_MF,y_int)[[2]]
Populations <<- updateBiologicalfromVIT(ALADYM_flag,Populations,Interactionsyear,VIToutput[[2]],y_ord,m_int,"M",VIT.analysis.discard,mortality_constant,age_classes_MF,y_int)[[1]]
               } 
               } else {   
               # update objects with LANDING e NA per i numeri
               print(paste("No external stock assessment available [",BMT_SPECIES[m_int],"] for ",years[y_int], sep=""), quote=FALSE)
               }

         }
                if (!exists("VITinfo")) {
                   VITinfo <<- list(VITinfo_onespecies)
               } else {
                   VITinfo <<- c(VITinfo, list(VITinfo_onespecies))
               }   
Populations[[m_int]]@maturity.vect[1,]  <- as.numeric(as.character(Populations[[m_int]]@maturity.vect[1,] )) / length(which(missing_SA[m_int,] == "Y"))
Populations[[m_int]]@maturity.vect[2,]  <- as.numeric(as.character(Populations[[m_int]]@maturity.vect[2,] )) / length(which(missing_SA[m_int,] == "Y"))

Populations[[m_int]]@M.vect$M <-  Populations[[m_int]]@M.vect$M / length(which(missing_SA[m_int,] == "Y"))
Populations[[m_int]]@M.vect$F <-  Populations[[m_int]]@M.vect$F / length(which(missing_SA[m_int,] == "Y"))

if (!is.na(as.numeric(as.character(Populations[[m_int]]@M.cost[1,])))) {
Populations[[m_int]]@M.cost[1,] <-  as.numeric(as.character(Populations[[m_int]]@M.cost[1,]))  / length(which(missing_SA[m_int,] == "Y"))
Populations[[m_int]]@M.cost[2,] <-  as.numeric(as.character(Populations[[m_int]]@M.cost[2,]))  / length(which(missing_SA[m_int,] == "Y"))
}
         

   # ----------------------------------------------------------------------------
   # READING DATA FROM XSA
   # ----------------------------------------------------------------------------
            
      } else if (SAtool == "XSA") {
                #if (exists("XSAinfo") ) { rm(XSAinfo) }
                if (exists("XSAinfo_onespecies") ) { rm(XSAinfo_onespecies) }

               XSAoutput <- readXSA(m_int, n_fleet_for_species)
               XSAinfo_onespecies <- list(species=m_int, results=XSAoutput)
               # memorizzare gli oggetti di XSA per il forecast
                y_ord <- 0
                for (y_int in 1:simperiod) {
              # print(paste("External stock assessment for the year n.", y_int, sep=""), quote=FALSE)
              if ( years[y_int] %in% as.numeric(colnames(XSAoutput$results@catch@.Data)) ) {
               y_ord <- y_ord +1 
               Interactionsyear <<- updateBiologicalfromXSA(ALADYM_flag, Populations, Interactionsyear, XSAoutput, y_ord, m_int, mortality_constant,y_int)[[2]]
               Populations <<- updateBiologicalfromXSA(ALADYM_flag, Populations, Interactionsyear, XSAoutput, y_ord, m_int, mortality_constant,y_int)[[1]]
               missing_SA[m_int, y_int] <- "Y"
               } else {
                  print(paste("No external stock assessment available [",BMT_SPECIES[m_int],"] for ",years[y_int], sep=""), quote=FALSE)
                  missing_SA[m_int, y_int] <- "N"
               }  
               }  
    
      if (!exists("XSAinfo")) {
                   XSAinfo <<- list(XSAinfo_onespecies)
               } else {
                   XSAinfo <<- c(XSAinfo, list(XSAinfo_onespecies))
               }   
    
   # ----------------------------------------------------------------------------
   # READING DATA FROM SURBA
   # ----------------------------------------------------------------------------
    
      } else if (SAtool == "SURBA") {      
          #  print(paste("Reading stock assessment results from SURBA for [",  BMT_SPECIES[m_int], "]...", sep=""))     
              # if (exists("SURBAinfo") ) { rm(SURBAinfo) }
              if (exists("SURBAinfo_onespecies") ) { rm(SURBAinfo_onespecies) }
      
           # print(paste("Reading stock assessment results from SURBA for [",  BMT_SPECIES[m_int], "]...", sep=""), quote=F) 
          #  ages_C <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m_int, ".StockAssessmentTool", sep=""),5])) 
            filePath <- as.character(cfg[rownames(cfg) == paste("casestudy.S", m_int, ".StockAss.fileC", sep=""),1] )

        SURBARes <- readSURBA(m_int, filePath)
        SURBAinfo_onespecies <- list(species=m_int, results=SURBARes)
        
        if (!exists("SURBAinfo")) {
                   SURBAinfo <<- list(SURBAinfo_onespecies)
               } else {
                   SURBAinfo <<- c(SURBAinfo, list(SURBAinfo_onespecies))
               } 
             y_ord <- 0
           for (y_int in 1:simperiod) {
           if ( years[y_int] %in% as.numeric(as.character(SURBARes$total_mortality$year)) ) { 
            y_ord <- y_ord +1 
        Interactionsyear <<- updateBiologicalfromSURBA( Populations, Interactionsyear, SURBARes, y_ord, m_int, mortality_constant, y_int)[[2]]
        Populations <<- updateBiologicalfromSURBA( Populations, Interactionsyear, SURBARes,  y_ord, m_int, mortality_constant, y_int)[[1]]
            missing_SA[m_int, y_int] <- "Y"
          } else {
          # no assessment
               print(paste("No external stock assessment available [",BMT_SPECIES[m_int],"] for ",years[y_int], sep=""), quote=FALSE)
             missing_SA[m_int, y_int] <- "N"
          }
          }
          
          
   # ----------------------------------------------------------------------------
   # READING DATA FROM Report
   # ----------------------------------------------------------------------------      
      
      } else if (SAtool == "Report") {
         # if (exists("ReportINFO") ) { rm(ReportINFO) }
        if (exists("ReportINFO_onespecies") ) { rm(ReportINFO_onespecies) }
      
           # print(paste("Reading stock assessment results from Report for [",  BMT_SPECIES[m_int], "]...", sep=""), quote=F) 
            ages_C <- as.numeric(as.character(cfg[rownames(cfg) == paste("casestudy.S", m_int, ".StockAssessmentTool", sep=""),5])) 
            filePath <- as.character(cfg[rownames(cfg) == paste("casestudy.S", m_int, ".StockAss.fileC", sep=""),1] )

        ReportRes <- readReport(m_int, filePath, ages_C)
        ReportINFO_onespecies <- list(species=m_int, results=ReportRes)
        
        if (!exists("ReportINFO")) {
                   ReportINFO <<- list(ReportINFO_onespecies)
               } else {
                   ReportINFO <<- c(ReportINFO, list(ReportINFO_onespecies))
               } 
        
          y_ord <- 0
           for (y_int in 1:simperiod) {
         if ( years[y_int] %in%  ReportRes$timeseries ) {
           y_ord <- y_ord +1 
        Interactionsyear <<- updateBiologicalfromReport( Populations, Interactionsyear, ReportRes, y_ord, m_int, mortality_constant, y_int)[[2]]
        Populations <<- updateBiologicalfromReport( Populations, Interactionsyear, ReportRes,  y_ord, m_int, mortality_constant, y_int)[[1]]
            missing_SA[m_int, y_int] <- "Y"
       } else {
                  print(paste("No external stock assessment available [",BMT_SPECIES[m_int],"] for ",years[y_int], sep=""), quote=FALSE)
                  missing_SA[m_int, y_int] <- "N"
               }  
          }
   # ----------------------------------------------------------------------------
   # No reading is executed
   # ----------------------------------------------------------------------------            
      } else if (SAtool == "NONE") {
       print(paste("No assessment results are available for [",  BMT_SPECIES[m_int], "]...", sep=""), quote=F) 
        missing_SA[m_int,] <- "N"
       }      
  
  
       VITinfo_empty <- list(species=m_int, results=NA)
       XSAinfo_empty <- list(species=m_int, results=NA) 
       ReportINFO_empty <- list(species=m_int, results=NA)
       SURBAinfo_empty <- list(species=m_int, results=NA)
       
         
              if (!exists("VITinfo")) {
              if (SAtool != "VIT") {
                   VITinfo <<- list(VITinfo_empty)
                   }
               } else {
                 if (SAtool != "VIT") {
                   VITinfo <<- c(VITinfo, list(VITinfo_empty))
                   }
               } 
  
             if (!exists("XSAinfo")) {
              if (SAtool != "XSA") {
                   XSAinfo <<- list(XSAinfo_empty)
                   }
               } else {
                 if (SAtool != "XSA") {
                   XSAinfo <<- c(XSAinfo, list(XSAinfo_empty))
                   }
               } 
             
              if (!exists("SURBAinfo")) {
              if (SAtool != "SURBA") {
                   SURBAinfo <<- list(SURBAinfo_empty)
                   }
               } else {
                 if (SAtool != "SURBA") {
                   SURBAinfo <<- c(SURBAinfo, list(SURBAinfo_empty))
                   }
               }  
               
               
             if (!exists("ReportINFO")) {
              if (SAtool != "Report") {
                   ReportINFO <<- list(ReportINFO_empty)
                   }
               } else {
                 if (SAtool != "Report") {
                   ReportINFO <<- c(ReportINFO, list(ReportINFO_empty))
                   }
               }  
  
  
 } # end loop for species         

  print("***************************************************************************", quote=FALSE) 
  print("BMT objects successfully updated from STOCK ASSESSMENT results!", quote=FALSE) 
  print("***************************************************************************", quote=FALSE) 

  for (m_int_rp in 1:length(BMT_SPECIES)) {
  
#  print("***************************************************************************", quote=FALSE) 
  print(paste("check reference points for species",BMT_SPECIES[m_int_rp], "..."), quote=FALSE) 
#  print("***************************************************************************", quote=FALSE) 

  
  for (nr in c(1:4)) {

         rp_F.01 <- data.frame(matrix(ncol=ncol(Interactionsyear[[1]][[1]]@referencePoints@F0.1), nrow=length(years)) )
         colnames(rp_F.01) <- colnames(Interactionsyear[[1]][[1]]@referencePoints@F0.1)
         rownames(rp_F.01) <- years
         rp_F.02 <- data.frame(matrix(ncol=ncol(Interactionsyear[[1]][[1]]@referencePoints@F0.2), nrow=length(years)) )
         colnames(rp_F.02) <- colnames(Interactionsyear[[1]][[1]]@referencePoints@F0.2)
         rownames(rp_F.02) <- years
         rp_FMSY <- data.frame(matrix(ncol=ncol(Interactionsyear[[1]][[1]]@referencePoints@FMSY), nrow=length(years)) )
         colnames(rp_FMSY) <- colnames(Interactionsyear[[1]][[1]]@referencePoints@FMSY)
         rownames(rp_FMSY) <- years
         rp_Fmax <- data.frame(matrix(ncol=ncol(Interactionsyear[[1]][[1]]@referencePoints@Fmax), nrow=length(years)) )
         colnames(rp_Fmax) <- colnames(Interactionsyear[[1]][[1]]@referencePoints@Fmax)
         rownames(rp_Fmax) <- years
         
        for (year_rp in 1:length(years)) {
            rp_F.01[year_rp,] <- as.numeric(as.character(Interactionsyear[[year_rp]][[m_int_rp]]@referencePoints@F0.1[nr,]))
            rp_F.02[year_rp,] <- as.numeric(as.character(Interactionsyear[[year_rp]][[m_int_rp]]@referencePoints@F0.2[nr,]))
            rp_FMSY[year_rp,] <- as.numeric(as.character(Interactionsyear[[year_rp]][[m_int_rp]]@referencePoints@FMSY[nr,]))   
            rp_Fmax[year_rp,] <- as.numeric(as.character(Interactionsyear[[year_rp]][[m_int_rp]]@referencePoints@Fmax[nr,])) 
        } 
         
        rp_F.01_means <- colMeans(rp_F.01, na.rm=T)
        rp_F.02_means <- colMeans(rp_F.02, na.rm=T)
        rp_FMSY_means <- colMeans(rp_FMSY, na.rm=T)
        rp_Fmax_means <- colMeans(rp_Fmax, na.rm=T)
        
       # if ( any(is.finite(as.numeric(as.character(rp_F.01_means)))) ) {
#        print("mean for F.01")  
#        print( rp_F.01_means )
#        }
#        if ( any(is.finite(as.numeric(as.character(rp_F.02_means)))) ) {
#        print("mean for F.02")  
#        print(rp_F.02_means)
#        }
#        if ( any(is.finite(as.numeric(as.character(rp_FMSY_means))))) {
#        print("mean for FMSY")
#        print( rp_FMSY_means)
#        }
#        if ( any(is.finite(as.numeric(as.character(rp_Fmax_means))))) {
#        print("mean for Fmax" )
#        print(rp_Fmax_means)
#        }
        
        for (year_rp in 1:length(years)) {
              if (all(!is.finite(as.numeric(as.character(Interactionsyear[[year_rp]][[m_int_rp]]@referencePoints@F0.1[nr,])) ) ) &  any(is.finite(as.numeric(as.character(rp_F.01_means)))) ) {
                   Interactionsyear[[year_rp]][[m_int_rp]]@referencePoints@F0.1[nr,] <-  rp_F.01_means
                   #print(paste("Set mean RP for", BMT_SPECIES[m_int_rp], "in", years[year_rp], "[", rownames(Interactionsyear[[year_rp]][[m_int_rp]]@referencePoints@F0.1[nr,]), "]") )
              }
              if (all(!is.finite(as.numeric(as.character(Interactionsyear[[year_rp]][[m_int_rp]]@referencePoints@F0.2[nr,])) ) ) &  any(is.finite(as.numeric(as.character(rp_F.02_means))))) {
                   Interactionsyear[[year_rp]][[m_int_rp]]@referencePoints@F0.2[nr,] <-  rp_F.02_means
                   #print(paste("Set mean RP for", BMT_SPECIES[m_int_rp], "in", years[year_rp], "[", rownames(Interactionsyear[[year_rp]][[m_int_rp]]@referencePoints@F0.1[nr,]), "]") )
              }
              if (all(!is.finite(as.numeric(as.character(Interactionsyear[[year_rp]][[m_int_rp]]@referencePoints@FMSY[nr,])) ) ) &  any(is.finite(as.numeric(as.character(rp_FMSY_means))))) {
                   Interactionsyear[[year_rp]][[m_int_rp]]@referencePoints@FMSY[nr,] <-  rp_FMSY_means
                   #print(paste("Set mean RP for", BMT_SPECIES[m_int_rp], "in", years[year_rp], "[", rownames(Interactionsyear[[year_rp]][[m_int_rp]]@referencePoints@F0.1[nr,]), "]") )
              }
              if (all(!is.finite(as.numeric(as.character(Interactionsyear[[year_rp]][[m_int_rp]]@referencePoints@Fmax[nr,])) ) ) &  any(is.finite(as.numeric(as.character(rp_Fmax_means))))) {
                   Interactionsyear[[year_rp]][[m_int_rp]]@referencePoints@Fmax[nr,] <- rp_Fmax_means
                   #print(paste("Set mean RP for", BMT_SPECIES[m_int_rp], "in", years[year_rp], "[", rownames(Interactionsyear[[year_rp]][[m_int_rp]]@referencePoints@F0.1[nr,]), "]") )
              }
        }  
        
        }
        }