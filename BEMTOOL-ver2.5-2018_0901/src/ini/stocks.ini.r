# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





if (phase == "SIMULATION") { 
vect_years <- years
index_year <- y
} else {
vect_years <- years.forecast
index_year <- y-simperiod
}


# ------------------------------------------------------------------------------
# Example of object of type Stock with name EXPLOITEDSTOCK_1
# ------------------------------------------------------------------------------
# structure of exploited stock in number data frame by age
# for males
lifespan_M <<-  as.numeric(as.character(Populations[[ALADYM_spe]]@lifespan$lifespan[[1]]))
lifespan_F <<- as.numeric(as.character(Populations[[ALADYM_spe]]@lifespan$lifespan[[2]]))

exploitednum_df_M <- data.frame(matrix(0, nrow=(max(lifespan_M, lifespan_F)+1), ncol=length(MONTHS)))
rownames(exploitednum_df_M) <- c(paste("age", c(0:max(lifespan_M, lifespan_F)), sep=""))
colnames(exploitednum_df_M) <- MONTHS
# for females
exploitednum_df_F <- data.frame(matrix(0, nrow=(max(lifespan_M, lifespan_F)+1), ncol=length(MONTHS)))
rownames(exploitednum_df_F) <- c(paste("age", c(0:max(lifespan_M, lifespan_F)), sep=""))
colnames(exploitednum_df_F) <- MONTHS

# structure of exploited stock in biomass data frame by age
exploitedbiomass_df_M <- data.frame(matrix(0, nrow=(max(lifespan_M, lifespan_F)+1), ncol=length(MONTHS)))
rownames(exploitedbiomass_df_M) <- c(paste("age", c(0:max(lifespan_M, lifespan_F)), sep=""))
colnames(exploitedbiomass_df_M) <- MONTHS
# for females
exploitedbiomass_df_F <- data.frame(matrix(0, nrow=(max(lifespan_M, lifespan_F)+1), ncol=length(MONTHS)))
rownames(exploitedbiomass_df_F) <- c(paste("age", c(0:max(lifespan_M, lifespan_F)), sep=""))
colnames(exploitedbiomass_df_F) <- MONTHS

# structure of SSnum data frame by age
SSnum_df <- data.frame(matrix(0, nrow=1, ncol=max(lifespan_M, lifespan_F)+1))
rownames(SSnum_df) <- vect_years[index_year]
colnames(SSnum_df) <- c(paste("age", c(0:max(lifespan_M, lifespan_F)), sep=""))

# structure of SSbiomass data frame by age
SSbiomass_df <- data.frame(matrix(0, nrow=1, ncol=max(lifespan_M, lifespan_F)+1) )
rownames(SSbiomass_df) <- vect_years[index_year]
colnames(SSbiomass_df) <- c(paste("age", c(0:max(lifespan_M, lifespan_F)), sep=""))

EXPLOITEDSTOCK_1 <- new(Class= "bmtStock",
                        Stype = STOCK_TYPE[1] # exploited
                        #numbers = list(M=exploitednum_df_M, F=exploitednum_df_F),
#                        SB = list(M=exploitedbiomass_df_M, F=exploitedbiomass_df_F),
#                        SS.numbers = SSnum_df,
#                        SSB = SSbiomass_df,
#                        L95 = 0,
#                        meanLength = 0,
#                        meanWeight = 0,
#                        criticalLength = 0,      
#                        harvestRate = 0.5,       # only for exploited stock
#                        exploitationRate = 0.5 
                          ) # only for exploited stock

# ------------------------------------------------------------------------------
# Example of object of type Stock with name UNEXPLOITEDSTOCK_1
# ------------------------------------------------------------------------------
# structure of unexploited stock in number data frame by age
# for males
unexploitednum_df_M <- data.frame(matrix(0, nrow=1, ncol=max(lifespan_M, lifespan_F)+1) )
rownames(unexploitednum_df_M) <- vect_years[index_year]
colnames(unexploitednum_df_M) <- c(paste("age", c(0:max(lifespan_M, lifespan_F)), sep=""))
# for females
unexploitednum_df_F <- data.frame(matrix(0, nrow=1, ncol=max(lifespan_M, lifespan_F)+1) )
rownames(unexploitednum_df_F) <- vect_years[index_year]
colnames(unexploitednum_df_F) <- c(paste("age", c(0:max(lifespan_M, lifespan_F)), sep=""))

# structure of unexploited stock in biomass data frame by age
# for males
unexploitedbiomass_df_M <- data.frame(matrix(0, nrow=1, ncol=max(lifespan_M, lifespan_F)+1))
rownames(unexploitedbiomass_df_M) <- vect_years[index_year]
colnames(unexploitedbiomass_df_M) <- c(paste("age", c(0:max(lifespan_M, lifespan_F)), sep=""))
# for females
unexploitedbiomass_df_F <- data.frame(matrix(0, nrow=1, ncol=max(lifespan_M, lifespan_F)+1))
rownames(unexploitedbiomass_df_F) <- vect_years[index_year]
colnames(unexploitedbiomass_df_F) <- c(paste("age", c(0:max(lifespan_M, lifespan_F)), sep=""))

# structure of unexploited SSnum data frame by age
unexploitedSSnum_df <- data.frame(matrix(0, nrow=1, ncol=max(lifespan_M, lifespan_F)+1) )
rownames(unexploitedSSnum_df) <- vect_years[index_year]
colnames(unexploitedSSnum_df) <- c(paste("age", c(0:max(lifespan_M, lifespan_F)), sep=""))

# structure of unexploited SSbiomass data frame by age
unexploitedSSbiomass_df <- data.frame(matrix(0, nrow=1, ncol=max(lifespan_M, lifespan_F)+1))
rownames(unexploitedSSbiomass_df) <- vect_years[index_year]
colnames(unexploitedSSbiomass_df) <- c(paste("age", c(0:max(lifespan_M, lifespan_F)), sep=""))

UNEXPLOITEDSTOCK_1 <- new(Class= "bmtStock",
                        Stype = STOCK_TYPE[2] # unexploited
                        #numbers = list(M=unexploitednum_df_M, F=unexploitednum_df_F),
                        #SB = list(M=unexploitedbiomass_df_M, F=unexploitedbiomass_df_F),
                        #SS.numbers = unexploitedSSnum_df,
                        #SSB = unexploitedSSbiomass_df,
                        #L95 = 0,
                        #meanLength = 0,
                        #meanWeight = 0,
                        #criticalLength = 0,      
                        #harvestRate = -1,       # only for exploited stock
                        #exploitationRate = -1
                        )  # only for exploited stock
                        
new_Stocks <- list(EXPLOITEDSTOCK_1, UNEXPLOITEDSTOCK_1)

 # print(paste("stocks: ", BMT_SPECIES[m], "in", vect_years[index_year], "successfully created!"), quote=FALSE)
