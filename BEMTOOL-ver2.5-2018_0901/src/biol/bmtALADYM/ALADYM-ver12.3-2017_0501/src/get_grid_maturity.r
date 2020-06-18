# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

get_grid_maturity <- function(APPROACH,DIST,params,SDs,nruns, loca_grid) {

GRID=data.frame(ML50=rep(params[1,1],nruns),MR=rep(params[1,2],nruns))
if (APPROACH=="D") {
  if (DIST == "Normal"){
      for (i in 1:nruns){         # riempimento prima colonna (L50)
      to_check=rnorm(1,mean=params[1,1],sd=SDs[1,1])
          if (to_check<0){
                    nruns=nruns+1
                    } else {
          GRID[i,1]=rnorm(1,mean=params[1,1],sd=SDs[1,1])

          }
      }
      for (i in 1:nruns){         # riempimento seconda colonna (MR)
      to_check=rnorm(1,mean=params[1,2],sd=SDs[1,2])
          if (to_check<0){
                    nruns=nruns+1
                    } else {
          GRID[i,2]=rnorm(1,mean=params[1,2],sd=SDs[1,2])

          }
      }
      }
      
      if (DIST == "Uniform"){
      for (i in 1:nruns){         # riempimento prima colonna (L50)
      to_check=runif(1,min=params[1,1],max=SDs[1,1])
          if (to_check<0){
                    nruns=nruns+1
                    } else {
          GRID[i,1]=runif(1,min=params[1,1],max=SDs[1,1])

          }
      }
      for (i in 1:nruns){         # riempimento seconda colonna (MR)
      to_check=runif(1,min=params[1,2],max=SDs[1,2])
          if (to_check<0){
                    nruns=nruns+1
                    } else {
          GRID[i,2]=runif(1,min=params[1,2],max=SDs[1,2])

          }
      }
       }
       
        if (DIST == "Lognormal"){
      for (i in 1:nruns){         # riempimento prima colonna (L50)
      to_check=rlnorm(1,mean=params[1,1],sd=SDs[1,1])
          if (to_check<0){
                    nruns=nruns+1
                    } else {
          GRID[i,1]=rlnorm(1,mean=params[1,1],sd=SDs[1,1])

          }
      }
      for (i in 1:nruns){         # riempimento seconda colonna (MR)
      to_check=rlnorm(1,mean=params[1,2],sd=SDs[1,2])
          if (to_check<0){
                    nruns=nruns+1
                    } else {
          GRID[i,2]=rlnorm(1,mean=params[1,2],sd=SDs[1,2])

          }
      }
      
      }

      } else if (APPROACH=="E"){
  GRID = loca_grid[, 2:3] # data.frame(read.table(path, sep=";", header=T))
  colnames(GRID) = c("ML50", "MR")
  }


return(GRID)
}