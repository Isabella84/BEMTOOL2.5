# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


check_bmt_simulation_economic_ts <- function(w) {

okei <- T
wind <- F

   for (fli in 1:length(BMT_FLEETSEGMENTS)) {
        if (okei) {
        # total variable costs (= commercial + other variable)
        this_cost_mat <- ECONOMICDATA_COSTS_list[[fli]]
        this_cost_variable <- this_cost_mat[, colnames(this_cost_mat) == "casestudy.fuelcosts" | colnames(this_cost_mat) == "casestudy.commercialcosts" |  colnames(this_cost_mat) == "casestudy.othervariablecosts" ]
       
       this_cost_variable$total_variable <- 0 
        for (nrw in 1:nrow(this_cost_variable)) {
           this_cost_variable[nrw, is.na(this_cost_variable[nrw,])] <- 0
           this_cost_variable$total_variable[nrw] <- sum(as.numeric(as.character(this_cost_variable[nrw,])))
        }
        
             # this_cost_variable$total_variable <- sum(as.numeric(as.character(this_cost_variable[,1])) , as.numeric(as.character(this_cost_variable[,2])) , as.numeric(as.character(this_cost_variable[,3])) , na.rm=T)
              
              if ( !all(this_cost_variable$total_variable == 0) ) {
              if (all( round(this_cost_variable$total_variable, 0) ==  round(as.numeric(as.character(this_cost_mat[,colnames(this_cost_mat) == "casestudy.totalvariablecosts"])) , 0) )) {
                   okei <- T  
              } else {
                   #okei <- F
                   okei <- T
#                   if (!wind) {
#                       err_wnd_ts <<- showError("Check consistency in variable costs time series!")
#                       wind <- T
#                   }
               print("-- WARNING! ---------------------------------------------------------", quote=F)    
               print(paste("Total variable costs (sum of fuel, commercial and other variable costs) for", BMT_FLEETSEGMENTS[fli],":"), quote=F) 
               print(this_cost_variable$total_variable, quote=F)   
               print("inconsistent with total costs in input:", quote=F)
               print(as.numeric(as.character(this_cost_mat[,colnames(this_cost_mat) == "casestudy.totalvariablecosts"])), quote=F)
              }
              }
          } else {
              #okei <- F
              okei <- T
          }
   }
#return(okei)
return(T)
}