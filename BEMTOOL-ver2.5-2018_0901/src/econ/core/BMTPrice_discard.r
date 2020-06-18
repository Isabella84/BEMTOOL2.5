# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy,
# completeness or appropriateness for any particular purpose.



# BMTPrice.R - Bemtool price component
# Author: Paolo Accadia

BMTPrice_discard<- function(option_discard_price, Flyear, currenttime, n_fleet, m_stock) {

if (FALSE) {
Flyear = Fleetyear
currenttime = 8
i = 1
j = 1
}

# --------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------- OPTION 1
# --------------------------------------------------------------------------------------------------------

  if (option_discard_price == 1) {  # Birdmod
    for (i in 1:n_fleet) {
      for (j in 1:m_stock) {

if ( as.character(Flyear[[currenttime]]@fleetsegments[[i]]@landing_obligation[j]) == "Y") {
          bmtprice_discard_prevyear <- as.numeric(as.character(Flyear[[currenttime - 1]]@fleetsegments[[i]]@price.landed_discard[j] ))
          price_elastity_coeff_disc_COL4 <- as.numeric(as.character( pmat5[j,i] ))

bmtprice_discard_weight_curryear <- as.numeric(as.character( Flyear[[currenttime]]@fleetsegments[[i]]@discard.weight[j] ))
bmtprice_discard_weight_prevyear <- as.numeric(as.character( Flyear[[currenttime - 1]]@fleetsegments[[i]]@discard.weight[j] ))

Flyear[[currenttime]]@fleetsegments[[i]]@price.landed_discard[j] <- bmtprice_discard_prevyear * ( 1 + price_elastity_coeff_disc_COL4 *( (bmtprice_discard_weight_curryear - bmtprice_discard_weight_prevyear) / bmtprice_discard_weight_prevyear) )
      } else {
 Flyear[[currenttime]]@fleetsegments[[i]]@price.landed_discard[j]  <- NA
      }

      for (PERC in c(1:5)) {

if ( as.character(Flyear[[currenttime]]@fleetsegments[[i]]@landing_obligation[j]) == "Y") {
     price_elastity_coeff_disc_COL4 <- as.numeric(as.character( pmat5[j,i] ))
     bmtprice_discard_weight_curryear <- as.numeric(as.character( Flyear[[currenttime]]@fleetsegments[[i]]@discard.weight.CI.perc[PERC, j] ))
     bmtprice_discard_weight_prevyear <- as.numeric(as.character( Flyear[[currenttime - 1]]@fleetsegments[[i]]@discard.weight.CI.perc[PERC,j] ))

      Flyear[[currenttime]]@fleetsegments[[i]]@price.landed_discard.CI.perc[PERC,j] <- bmtprice_discard_prevyear * ( 1 + price_elastity_coeff_disc_COL4 *( (bmtprice_discard_weight_curryear - bmtprice_discard_weight_prevyear) / bmtprice_discard_weight_prevyear) )

      } else {
        Flyear[[currenttime]]@fleetsegments[[i]]@price.landed_discard.CI.perc[PERC,j] <- NA

      }

			}
      }
    }

# --------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------- OPTION 2
# --------------------------------------------------------------------------------------------------------

  } else if (option_discard_price == 2) { #  prices previous year
    for (i in 1:n_fleet) {
      for (j in 1:m_stock) {

       bmtprice_price_discard_prevyear <- as.numeric(as.character( Flyear[[simperiod]]@fleetsegments[[i]]@price.landed_discard[j] ))
        Flyear[[currenttime]]@fleetsegments[[i]]@price.landed_discard[j] <- bmtprice_price_discard_prevyear

        				for (PERC in c(1:5)) {
        		      bmtprice_price_discard_prevyear <- as.numeric(as.character( Flyear[[simperiod]]@fleetsegments[[i]]@price.landed_discard.CI.perc[PERC,j] ))
         Flyear[[currenttime]]@fleetsegments[[i]]@price.landed_discard.CI.perc[PERC,j] <- bmtprice_price_discard_prevyear
        				}

      }
    }
    
# --------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------- OPTION 3
# --------------------------------------------------------------------------------------------------------
    
  } else if (option_discard_price == 3) { #  prices costant
  
      for (i in 1:n_fleet) {
      for (j in 1:m_stock) {

if ( as.character(Flyear[[currenttime]]@fleetsegments[[i]]@landing_obligation[j]) == "Y") {

          price_disc_COL4 <- as.numeric(as.character( pmat5[j,i] ))

Flyear[[currenttime]]@fleetsegments[[i]]@price.landed_discard[j] <- price_disc_COL4
      } else {
 Flyear[[currenttime]]@fleetsegments[[i]]@price.landed_discard[j]  <- NA
      }

      for (PERC in c(1:5)) {

if ( as.character(Flyear[[currenttime]]@fleetsegments[[i]]@landing_obligation[j]) == "Y") {
          price_disc_COL4 <- as.numeric(as.character( pmat5[j,i] ))
          
      Flyear[[currenttime]]@fleetsegments[[i]]@price.landed_discard.CI.perc[PERC,j] <- price_disc_COL4

      } else {
        Flyear[[currenttime]]@fleetsegments[[i]]@price.landed_discard.CI.perc[PERC,j] <- NA

      }

			}
      }
    }
  
  }
  
  

  return(Flyear)
}
