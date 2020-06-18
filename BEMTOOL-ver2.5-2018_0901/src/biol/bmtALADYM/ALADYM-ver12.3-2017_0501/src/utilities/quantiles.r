# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




# x è una matrice formata dalla colonna delle lunghezze e la colonna delle frequenze

# x=seq(1,20,1)
# y=rnorm(20,20,0.5)
# x=cbind(x,y)


quantiles <- function(x) {
colnames(x) = c("length","frequencies")
x = data.frame(x)
x = x[with(x, order(frequencies,decreasing=TRUE) ), ]
x = cbind(x,x[,2]/sum(x[,2]))  # frequenze
x = cbind(x,rep(x[1,3],nrow(x)))
    for (i in 2:nrow(x))  {
    x[i,4]=  x[i,3]  + x[i-1,4]
    }
    
Select0_05 = x[(0.025 < x[,4]) &   (x[,4] < 0.075),c(1,4)] [1,1]
Select0_25 = x[(0.225 < x[,4]) &   (x[,4] < 0.275),c(1,4)] [1,1]
Select0_50 = x[(0.475 < x[,4]) &   (x[,4] < 0.525),c(1,4)] [1,1]
Select0_75 = x[(0.725 < x[,4]) &   (x[,4] < 0.775),c(1,4)] [1,1]
Select0_95 = x[(0.925 < x[,4]) &   (x[,4] < 0.975),c(1,4)] [1,1]

return(c(Select0_05,Select0_25,Select0_50,Select0_75,Select0_95))



}
