# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




Sample_a_random <- function(para_function, para_min, para_max, para_a, para_b) {

# Available distribution
#
# 1 = lognormal
# 2 = gamma
# 3 = normal
# 4 = uniform
#

  if(para_function == 1) {
    para_qmin <- plnorm(para_min, para_a, para_b)
    para_qmax <- plnorm(para_max, para_a, para_b)

    loca_temp <- qlnorm(runif(1, min = para_qmin, max = para_qmax), para_a, para_b)
  }

  if(para_function == 2) {
    para_qmin <- pgamma(para_min, para_a, scale = para_b)
    para_qmax <- pgamma(para_max, para_a, scale = para_b)

    loca_temp <- qgamma(runif(1, min = para_qmin, max = para_qmax), para_a, scale = para_b)
  }

  if(para_function == 3) {
    para_qmin <- pnorm(para_min, para_a, para_b)
    para_qmax <- pnorm(para_max, para_a, para_b)

    loca_temp <- qnorm(runif(1, min = para_qmin, max = para_qmax), para_a, para_b)
  }

  if(para_function == 4) {
    loca_temp <- runif(1, min = para_min, max = para_max)
  }

  return(loca_temp)
}
