# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


split_F <- function(mde) {
       
  F_overall_matrix_M_temp <- new_aldSimulation@fishingmortality.overall.M 
    F_overall_matrix_F_temp <- new_aldSimulation@fishingmortality.overall.F 
    
#    print("Fishing mortality OVERALL males")
#    print(F_overall_matrix_M_temp)
#    
#     print("Fishing mortality OVERALL females")
#    print(F_overall_matrix_F_temp)
 
              
if (nrow(F_overall_matrix_M_temp) != 0 & nrow(F_overall_matrix_F_temp) != 0 ) {
          
n_ages_M  <- as.numeric(as.character(gtkEntryGetText(entryVBF_M_lifespan)))  
n_ages_F  <- as.numeric(as.character(gtkEntryGetText(entryVBF_F_lifespan)))

first_age_mal <- 0
first_age_fem <- 0

   n_ages_M <- n_ages_M - trunc(Tr/12)
    first_age_mal <- trunc(Tr/12)
   n_ages_F <- n_ages_F - trunc(Tr/12)
    first_age_fem <- trunc(Tr/12)

all_Fmortalities_O <- data.frame(matrix(nrow=(n_ages_M + n_ages_F), ncol=(2+length(years) ) ))
colnames(all_Fmortalities_O) <- c("Age", years, "Sex") 
        
        all_Fmortalities_O$Age <- c(c(first_age_mal:(n_ages_M+first_age_mal-1)) , c(first_age_fem:(n_ages_F+first_age_fem-1)))
         all_Fmortalities_O$Sex  <- c(rep("M", n_ages_M), rep("F", n_ages_F)) 

         temp_M <- F_overall_matrix_M_temp
         temp_M <- t(temp_M[,2:ncol(temp_M)])
                  temp_F <- F_overall_matrix_F_temp
                     temp_F <- t(temp_F[,2:ncol(temp_F)])
                                                                      

  all_Fmortalities_O[all_Fmortalities_O$Sex == "M", 2:(length(years)+1) ] <- data.frame(temp_M  )
    all_Fmortalities_O[all_Fmortalities_O$Sex == "F", 2:(length(years)+1) ] <- data.frame(temp_F  )
    

#  all_Fmortalities_O <- data.frame(all_Fmortalities_O)

    }



if ( mde == "P" ) {
  
  # ------------------------------------------------------ SPLITTING BY PRODUCTION
   
  production_to_use <- get_production_data_by_year() 
              
Fmortality_splitted <-  splitF_bymeansPRODUCTION(all_Fmortalities_O, production_to_use)

print("Splitted by production")

} else if ( mde == "CAA" ) {

# ------------------------------------------------------ SPLITTING BY CATCH AT AGE
  
     all_catchAtAge <- data.frame(matrix(nrow=(n_ages_M*length(years) + n_ages_F*length(years)), ncol=3+length(FLEETSEGMENTS_names) ))
colnames(all_catchAtAge) <- c("Age", "Year", paste("fs", c(1:length(FLEETSEGMENTS_names)), sep=""), "Sex") 

    vect_year <- data.frame(matrix(nrow=0, ncol=1))
        vect_sex <- data.frame(matrix(nrow=0, ncol=1))
            vect_ages <- data.frame(matrix(nrow=0, ncol=1))

 for (yy in 1:length(years) ) { 
vect_year <- rbind(vect_year, matrix(rep(years[yy], (n_ages_M+n_ages_F)), ncol=1) )
vect_sex <- rbind(vect_sex,   matrix(c(rep("M", n_ages_M) , rep("F", n_ages_F)), ncol=1) )
vect_ages <- rbind(vect_ages,  matrix(c(c(first_age_mal:(n_ages_M+first_age_mal-1)) , c(first_age_fem:(n_ages_F+first_age_fem-1))), ncol=1) )
 }
    
 all_catchAtAge$Age <- vect_ages[,1]
  all_catchAtAge$Year <- vect_year[,1]  
    all_catchAtAge$Sex <- vect_sex[,1] 
        
 for (yy in 1:length(years) ) { 
for (fs in 1:length(FLEETSEGMENTS_names) ) {
fs_object <- FleetList_simulation[[fs]]        
dataframe_F_M <- fs_object@catchAtAge.M.vector
dataframe_F_F <- fs_object@catchAtAge.F.vector
                                                                                        
all_catchAtAge[all_catchAtAge$Year == years[yy] & all_catchAtAge$Sex == "M", (2+fs) ] <- as.numeric(as.character(dataframe_F_M[yy,2:(n_ages_M+1)]))
all_catchAtAge[all_catchAtAge$Year == years[yy] & all_catchAtAge$Sex == "F", (2+fs) ] <- as.numeric(as.character(dataframe_F_F[yy,2:(n_ages_F+1)]))
 }
}
   

Fmortality_splitted  <- splitF_bymeansCATCHatAGE(all_Fmortalities_O, all_catchAtAge)

print("Splitted by catch at age")
}

if (nrow(Fmortality_splitted) != 0) {
        for (fs in 1:length(FLEETSEGMENTS_names) ) {
   F_fs <- Fmortality_splitted[,c(1:2, (fs+2), (length(FLEETSEGMENTS_names)+3))]

   dataframe_F_M <- data.frame(matrix(nrow=0, ncol=(n_ages_M+1)))
   colnames(dataframe_F_M) <-  c("year", paste("age", c(first_age_mal:(n_ages_M+first_age_mal-1)), sep="") )
   dataframe_F_F <- data.frame(matrix(nrow=0, ncol=(n_ages_F+1)))
   colnames(dataframe_F_F) <-  c("year", paste("age", c(first_age_fem:(n_ages_F+first_age_fem-1)), sep="") )
      
   for (yy in 1:length(years) ) {
       F_fs_y <- F_fs[F_fs$Year == years[yy], ]
       to_add <- data.frame(matrix(  F_fs_y[F_fs_y[ncol(F_fs_y)]=="M",3], nrow=1)) 
       to_add <- data.frame(cbind(years[yy], to_add))
       colnames(to_add) <- c("year", paste("age", c(first_age_mal:(n_ages_M+first_age_mal-1)), sep="") )
       dataframe_F_M <- rbind(dataframe_F_M, to_add)
       to_add <- data.frame(matrix(  F_fs_y[F_fs_y[ncol(F_fs_y)]=="F",3], nrow=1)) 
       to_add <- data.frame(cbind(years[yy], to_add) )
       colnames(to_add) <- c("year", paste("age",  c(first_age_fem:(n_ages_F+first_age_fem-1)), sep="") )
       dataframe_F_F <- rbind(dataframe_F_F, to_add) 
   }     # .GlobalEnv$
 FleetList_simulation[[fs]]@fishingmortality.M.vector <<- dataframe_F_M
 FleetList_simulation[[fs]]@fishingmortality.F.vector <<- dataframe_F_F
 
 }

selected <- gtkComboBoxGetActiveText(combo_fleetsegments)
index_to_load <- which(FLEETSEGMENTS_names == selected )  

if (length(index_to_load) != 0 ) {
mortality.Fvector.females <<- FleetList_simulation[[index_to_load]]@fishingmortality.F.vector
mortality.Fvector.males <<- FleetList_simulation[[index_to_load]]@fishingmortality.M.vector

reload_fishingmortalityF()
reload_fishingmortalityM() 
}

}


return(Fmortality_splitted) 
}    




