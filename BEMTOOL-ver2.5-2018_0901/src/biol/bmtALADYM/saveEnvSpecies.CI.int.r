# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.









assign(paste("SRO_", ALADYM_spe, "_run", numero_ciclo, sep=""), new.env())
SRO_runCI <- get(paste("SRO_", ALADYM_spe, "_run", numero_ciclo, sep=""))
for (obj_name in ls(SRO)) {
     assign(obj_name, get(obj_name, envir = SRO), envir=SRO_runCI)     
}

assign(paste("RND_", ALADYM_spe, "_run", numero_ciclo, sep=""), new.env())
RND_runCI <- get(paste("RND_", ALADYM_spe, "_run", numero_ciclo, sep=""))
for (obj_name in ls(RND)) {
     assign(obj_name, get(obj_name, envir = RND), envir=RND_runCI)     
}


assign(paste("BAS_", ALADYM_spe, "_run", numero_ciclo, sep=""), new.env())
BAS_runCI <- get(paste("BAS_", ALADYM_spe, "_run", numero_ciclo, sep=""))
for (obj_name in ls(BAS)) {
     assign(obj_name, get(obj_name, envir = BAS), envir=BAS_runCI)     
}

assign(paste("GLO_", ALADYM_spe, "_run", numero_ciclo, sep=""), new.env())
GLO_runCI <- get(paste("GLO_", ALADYM_spe, "_run", numero_ciclo, sep=""))
for (obj_name in ls(GLO)) {
     assign(obj_name, get(obj_name, envir = GLO), envir=GLO_runCI)     
}

