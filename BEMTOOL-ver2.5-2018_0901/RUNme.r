# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



if (!("FLCore" %in% installed.packages()[,1])) {
    install.packages(repos="http://flr-project.org/R")
}

if (!("ggplot2" %in% installed.packages()[,1])) {
    install.packages("ggplot2")
}

if (!("gridExtra" %in% installed.packages()[,1])) {
    install.packages("gridExtra")
}

if (!("akima" %in% installed.packages()[,1])) {
    install.packages("akima")
}

if (!("stringr" %in% installed.packages()[,1])) {
    install.packages("stringr")
}

if (!("RGtk2" %in% installed.packages()[,1])) {
    install.packages("RGtk2")
}


if (!("Hmisc" %in% installed.packages()[,1])) {
    install.packages("Hmisc")
}

if (!("timeDate" %in% installed.packages()[,1])) {
    install.packages("timeDate")
}

if (!("reshape" %in% installed.packages()[,1])) {
    install.packages("reshape")
}

if (!("scales" %in% installed.packages()[,1])) {
    install.packages("scales")
}

