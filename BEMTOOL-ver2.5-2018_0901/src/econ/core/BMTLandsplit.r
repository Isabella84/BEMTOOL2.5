# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



# BMTLandsplit.R - Bemtool landings split component (for non-integrated approach)
# Author: Paolo Accadia

BMTLandsplit <- function(Flyear, Inters) {
 
for (m_spe in 1:m_stock) {
for (ye_f in 1:foreperiod) {
yy <- ye_f + simperiod
#MTF_home_spe <- paste(MTF_home, "/Results_MTF/", BMT_SPECIES[m_spe], sep="")   
catch_table_path <- paste(casestudy_path, "/", harvest_rule_id,"/Biological Pressure Impact/MSTF - ", BMT_SPECIES[m_spe],"/", casestudy_name, " - catch(F01 in ref_year) ", harvest_rule_id,".csv", sep="")
catch_table <- read.csv(catch_table_path,sep=";") 

    CPUE <- matrix(0,nrow=n_fleet, ncol=1)
    Eff_vect <- matrix(0,nrow=n_fleet, ncol=1)
    
      associated_fleetsegment <- as.vector(cfg[rownames(cfg) == paste("casestudy.S", m_spe, ".associatedFleetsegment", sep=""), ]) 
      associated_fleetsegment <- associated_fleetsegment[!is.na(associated_fleetsegment) & associated_fleetsegment!="" & associated_fleetsegment!="-"]
      associated_fleetsegment_indices <- which(BMT_FLEETSEGMENTS %in% associated_fleetsegment)
    
    fl_ord_this_spe <- 1
    # Distribution of landings among fleet segments
    for (i in 1:length(BMT_FLEETSEGMENTS)) { 
    CPUE[i] <- as.numeric(as.character(Flyear[[simperiod]]@fleetsegments[[i]]@landing.weight[m_spe])) / as.numeric(as.character(Flyear[[simperiod]]@fleetsegments[[i]]@GT.DAYS.annual))
    Eff_vect[i] <- Flyear[[yy]]@fleetsegments[[i]]@GT.DAYS.annual
    }
    for (i in 1:length(BMT_FLEETSEGMENTS)) { 
     # CPUE[i] <- as.numeric(as.character(Flyear[[1]]@fleetsegments[[i]]@landing.weight[m_spe])) / as.numeric(as.character(Flyear[[1]]@fleetsegments[[i]]@GT.DAYS.annual))
     
      if (i %in% associated_fleetsegment_indices) {   
      print("---------------------------------------------")

      if (CPUE[i] != 0) {
       if ( is.na(Flyear[[yy]]@fleetsegments[[i]]@GT.DAYS.annual)) {
       print(paste("GT.DAYS.annual:", Flyear[[yy]]@fleetsegments[[i]]@GT.DAYS.annual))
       }
      if (is.na(Flyear[[yy]]@fleetsegments[[i]]@GT.DAYS.annual/sum(Eff_vect*CPUE/CPUE[i]))) { 
      print(paste("fattore moltiplicativo: ", Flyear[[yy]]@fleetsegments[[i]]@GT.DAYS.annual/sum(Eff_vect*CPUE/CPUE[i]))) 
       }
          land_value <- catch_table[yy,3] * Flyear[[yy]]@fleetsegments[[i]]@GT.DAYS.annual/sum(Eff_vect*CPUE/CPUE[i])
        } else {
          land_value <- 0
        } 
      Flyear[[yy]]@fleetsegments[[i]]@landing.weight[m_spe] <- land_value
      Inters[[yy]][[m_spe]]@interactions[[fl_ord_this_spe]]$landings@totalweight <-  as.numeric(as.character(Flyear[[yy]]@fleetsegments[[i]]@landing.weight[m_spe] )) # / 1000
      print(paste("updating year:",  yy, ", species:",m_spe, ", fleet segment:", i, ", valore:" ,  Inters[[yy]][[m_spe]]@interactions[[fl_ord_this_spe]]$landings@totalweight))
      if (Inters[[yy]][[m_spe]]@totaldiscard@totalweight == 0) {
          Inters[[yy]][[m_spe]]@interactions[[fl_ord_this_spe]]$catches@totalweight <- Inters[[yy]][[m_spe]]@interactions[[fl_ord_this_spe]]$landings@totalweight
      } 
#[1] "fattore moltiplicativo:  NaN"
#[1] "updating year: 6 , species: 1 , fleet segment: 4 , valore: NaN"
#
      
      fl_ord_this_spe <- fl_ord_this_spe+1
      } else {
       Flyear[[yy]]@fleetsegments[[i]]@landing.weight[m_spe] <- 0

      }
    } # end loop fleet segment
} # end year loop
} # end loop species

return(list(Flyear, Inters))
}
