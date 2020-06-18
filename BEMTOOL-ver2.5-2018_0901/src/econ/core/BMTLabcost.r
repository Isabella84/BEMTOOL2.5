# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



# BMTPrice.R - Bemtool labour costs component
# Author: Paolo Accadia

BMTLabcost <- function(option, Flyear, lcmat, currenttime, n_fleet) {

    # All models
    for (i in 1:n_fleet) { 
    
      bmtlabour_crew_share_COL1 <- as.numeric(as.character(lcmat[1,i]))
      bmtlabour_fuelcost_option_COL2 <- as.numeric(as.character(lcmat[2,i])) 
      bmtlabour_commercialcost_option_COL3 <- as.numeric(as.character(lcmat[3,i]))   
      bmtlabour_othervariablecost_option_COL4 <- as.numeric(as.character(lcmat[4,i])) 
      bmtlabour_sorting_coeff_COL6 <- as.numeric(as.character(discard_coeff_mat[i,currenttime-simperiod]))    
      #      bmtlabour_min_national_wage_COL2 <- as.numeric(as.character(lcmat[3,i]))         ???????????????????????????????????'
            
      bmtlabour_curr_revenue <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@total.revenues))
      bmtlabour_curr_fuelcost <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$fuel.cost))
      bmtlabour_curr_commercialcost <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$commercial.cost))
      bmtlabour_curr_othervariablecost <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$other.var.cost))
      bmtlabour_curr_totalvariablecost <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost$tot.var.cost))
            
      # if = 0 (not use)  <->  = vector
      # empty field = Discard rate
      
      if ( bmtlabour_sorting_coeff_COL6 == -1 ) {
      #      if ( is.na(bmtlabour_sorting_coeff_COL6) ) {
             bmtlabour_LANDING_target <-  sum( as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@landing.weight )) , na.rm=T)
            bmtlabour_DISCARD_target <-  sum( as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@discard.weight )) , na.rm=T)
            
            bmtlabour_sorting_coeff_COL6 <-  ifelse(is.na(bmtlabour_DISCARD_target), 0, bmtlabour_DISCARD_target/(bmtlabour_DISCARD_target + bmtlabour_LANDING_target) )    
      }
    
    if (option[2] != 4) {  
     bmtlabour_curr <- max( bmtlabour_crew_share_COL1 * (bmtlabour_curr_revenue -  sum((bmtlabour_fuelcost_option_COL2 * bmtlabour_curr_fuelcost), (bmtlabour_commercialcost_option_COL3 * bmtlabour_curr_commercialcost), (bmtlabour_othervariablecost_option_COL4 * bmtlabour_curr_othervariablecost), na.rm=T)) , 0) 
     } else {
        bmtlabour_curr <- max( bmtlabour_crew_share_COL1 * (bmtlabour_curr_revenue - bmtlabour_curr_totalvariablecost) , 0)   
     } 
     # massimo tra :  lcmat(1) *  [ (total_rev - lcmat(2) * costivar(2)  -  lcmat(3) * costvar(3)  -  lcmat(4)*costvar(4) )   e  0 
            # creew_share(a)   * [ ricavi_totali -   b* fuel  -  c*costi_commerciali  - d* altri_costi_variabili  ]  
            
Flyear[[currenttime]]@fleetsegments[[i]]@labour.cost <-  bmtlabour_curr +  bmtlabour_sorting_coeff_COL6 * bmtlabour_curr # adjustment with the sorting cefficient   
    
    print(paste("LABOUR COSTS ---------- YEAR:", currenttime-simperiod, "FLEET:", i, " coeff ==== ", bmtlabour_sorting_coeff_COL6))
		
		for (PERC in c(1:5)) {
      #      bmtlabour_min_national_wage_COL2 <- as.numeric(as.character(lcmat[3,i]))         ???????????????????????????????????' 
      
      bmtlabour_curr_revenue <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@total.revenues.CI.perc[PERC]))
      bmtlabour_curr_fuelcost <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$fuel.cost[PERC]))
      bmtlabour_curr_commercialcost <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$commercial.cost[PERC]))
      bmtlabour_curr_othervariablecost <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$other.var.cost[PERC]))
      bmtlabour_curr_totalvariablecost <- as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@variable.cost.CI.perc$tot.var.cost[PERC]))
            
      # if = 0 (not use)  <->  = vector
      # empty field = Discard rate
     
      if ( bmtlabour_sorting_coeff_COL6 == -1 ) {
#      if ( is.na(bmtlabour_sorting_coeff_COL6) ) {
             bmtlabour_LANDING_target <-  sum( as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@landing.weight.CI.perc[PERC,] )) , na.rm=T)
            bmtlabour_DISCARD_target <-  sum( as.numeric(as.character(Flyear[[currenttime]]@fleetsegments[[i]]@discard.weight.CI.perc[PERC,] )) , na.rm=T)
            
            bmtlabour_sorting_coeff_COL6 <-  ifelse(is.na(bmtlabour_DISCARD_target), 0, bmtlabour_DISCARD_target/(bmtlabour_DISCARD_target + bmtlabour_LANDING_target) )    
      }
    
    if (option[2] != 4) {  
     bmtlabour_curr <- max( bmtlabour_crew_share_COL1 * (bmtlabour_curr_revenue - ( bmtlabour_fuelcost_option_COL2 * bmtlabour_curr_fuelcost + bmtlabour_commercialcost_option_COL3 * bmtlabour_curr_commercialcost + bmtlabour_othervariablecost_option_COL4 * bmtlabour_curr_othervariablecost)) , 0) 
     } else {
        bmtlabour_curr <- max( bmtlabour_crew_share_COL1 * (bmtlabour_curr_revenue - bmtlabour_curr_totalvariablecost) , 0)   
     } 
     # massimo tra :  lcmat(1) *  [ (total_rev - lcmat(2) * costivar(2)  -  lcmat(3) * costvar(3)  -  lcmat(4)*costvar(4) )   e  0 
            # creew_share(a)   * [ ricavi_totali -   b* fuel  -  c*costi_commerciali  - d* altri_costi_variabili  ]  
            
Flyear[[currenttime]]@fleetsegments[[i]]@labour.cost.CI.perc[PERC] <-  bmtlabour_curr +  bmtlabour_sorting_coeff_COL6 * bmtlabour_curr # adjustment with the sorting cefficient   
		}
		
		
		
		
		
		
		
		
		}
 
  return(Flyear)
}
