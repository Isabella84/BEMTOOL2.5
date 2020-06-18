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
# Example of object of type Catch with name LANDINGS_1
# ------------------------------------------------------------------------------
# structure of landings in number data frame by month
landingsnum_df <- data.frame(matrix(0, nrow=1, ncol=max(lifespan_M, lifespan_F)+1))
rownames(landingsnum_df) <- vect_years[index_year]
colnames(landingsnum_df) <- c(paste("age", c(0:max(lifespan_M, lifespan_F)), sep=""))

LANDINGS_1 <- new(Class= "bmtCatch",
                    Ctype = CATCH_TYPE[1] # landings
                    #numbers = landingsnum_df,
                    #totalweight = 0,                                     # in tons
                    #L95 = -1,                                            # in mm
                    #meanLenght = 0,         # in mm
                    #meanWeight = 0
                    )                                      # in kg
# ------------------------------------------------------------------------------
# Example of object of type Catch with name DISCARDS_1
# ------------------------------------------------------------------------------
# structure of discards in number data frame by month
discardsnum_df <- data.frame(matrix(0, nrow=1, ncol=max(lifespan_M, lifespan_F)+1))
rownames(discardsnum_df) <- vect_years[index_year]
colnames(discardsnum_df) <- c(paste("age", c(0:max(lifespan_M, lifespan_F)), sep=""))

## structure of mean length data frame by age
#meanLength_disc_df <- data.frame(matrix(-1, nrow=1, ncol=max(lifespan_M, lifespan_F)+1))
#rownames(meanLength_disc_df) <- c(current_year)
#colnames(meanLength_disc_df) <- c(paste("age", c(0:max(lifespan_M, lifespan_F)), sep=""))

DISCARDS_1 <- new(Class= "bmtCatch",
                    Ctype = CATCH_TYPE[2] # discards
                    #numbers = discardsnum_df,
                    #totalweight = 0,                                     # in tons
                    #L95 = -1,                                        # in mm
                    #meanLenght = 0,    # in mm
                    #meanWeight = -1
                    )    # in kg

# ------------------------------------------------------------------------------
# Example of object of type Catch with name CATCHES_1
# ------------------------------------------------------------------------------
# structure of catches in number data frame by month
catchesnum_df <- data.frame(matrix(0, nrow=1, ncol=max(lifespan_M, lifespan_F)+1) )
rownames(catchesnum_df) <- vect_years[index_year]
colnames(catchesnum_df) <- c(paste("age", c(0:max(lifespan_M, lifespan_F)), sep=""))

## structure of mean length data frame by age
#meanLength_catc_df <- data.frame(matrix(0, nrow=1, ncol=max(lifespan_M, lifespan_F)+1))
#rownames(meanLength_catc_df) <- c(current_year)
#colnames(meanLength_catc_df) <- c(paste("age", c(0:max(lifespan_M, lifespan_F)), sep=""))

CATCHES_1 <- new(Class= "bmtCatch",
                    Ctype = CATCH_TYPE[3] # catches
                    #numbers = catchesnum_df,
                    #totalweight = 0,                                     # in tons
                    #L95 = 0,                                        # in mm
                    #meanLenght = 0,    # in mm
                    #meanWeight = -1
                    )                                # in kg

new_CatchesAssociation <- list(BMT_FLEETSEGMENTS[n_fl], LANDINGS_1, DISCARDS_1, CATCHES_1)  

#print(paste("catches: ", BMT_SPECIES[m], "in", vect_years[index_year], "for", BMT_FLEETSEGMENTS[n_fl], "successfully created!"), quote=FALSE)
