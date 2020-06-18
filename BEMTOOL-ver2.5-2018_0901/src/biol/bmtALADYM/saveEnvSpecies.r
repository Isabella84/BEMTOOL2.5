# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.


#print(paste("Saving", paste("SRO_", ALADYM_spe, sep=""), "environment..."), quote=F)
assign(paste("SRO_", ALADYM_spe, sep=""), new.env())
SRO_simulation <- get(paste("SRO_", ALADYM_spe, sep=""))
for (obj_name in ls(SRO)) {
     assign(obj_name, get(obj_name, envir = SRO), envir=SRO_simulation)     
}
path_to_save <- paste(casestudy_path, "/",harvest_rule_id, "/working files/SRO_", ALADYM_spe,".Rdata", sep="")
save(list = ls(SRO), envir = SRO, file= path_to_save)

#print(paste("Saving", paste("INP_", ALADYM_spe, sep=""), "environment..."), quote=F)
assign(paste("INP_", ALADYM_spe, sep=""), new.env())
INP_simulation <- get(paste("INP_", ALADYM_spe, sep=""))
for (obj_name in ls(INP)) {
     assign(obj_name, get(obj_name, envir = INP), envir=INP_simulation)
}
path_to_save <- paste(casestudy_path, "/",harvest_rule_id, "/working files/INP_", ALADYM_spe, ".Rdata",sep="")
save(list = ls(INP), envir = INP, file= path_to_save)

#print(paste("Saving", paste("RND_", ALADYM_spe, sep=""), "environment..."), quote=F)
assign(paste("RND_", ALADYM_spe, sep=""), new.env())
RND_simulation <- get(paste("RND_", ALADYM_spe, sep=""))
for (obj_name in ls(RND)) {
     assign(obj_name, get(obj_name, envir = RND), envir=RND_simulation)
}
path_to_save <- paste(casestudy_path, "/",harvest_rule_id, "/working files/RND_", ALADYM_spe,".Rdata", sep="")
save(list = ls(RND), envir = RND, file= path_to_save)


#print(paste("Saving", paste("BAS_", ALADYM_spe, sep=""), "environment..."), quote=F)
assign(paste("BAS_", ALADYM_spe, sep=""), new.env())
BAS_simulation <- get(paste("BAS_", ALADYM_spe, sep=""))
for (obj_name in ls(BAS)) {
     assign(obj_name, get(obj_name, envir = BAS), envir=BAS_simulation)
}
path_to_save <- paste(casestudy_path, "/",harvest_rule_id, "/working files/BAS_", ALADYM_spe,".Rdata", sep="")
save(list = ls(BAS), envir = BAS, file= path_to_save)


#print(paste("Saving", paste("GLO_", ALADYM_spe, sep=""), "environment..."), quote=F)
assign(paste("GLO_", ALADYM_spe, sep=""), new.env())
GLO_simulation <- get(paste("GLO_", ALADYM_spe, sep=""))
for (obj_name in ls(GLO)) {
     assign(obj_name, get(obj_name, envir = GLO), envir=GLO_simulation)
}
path_to_save <- paste(casestudy_path, "/",harvest_rule_id, "/working files/GLO_", ALADYM_spe,".Rdata", sep="")
save(list = ls(GLO), envir = GLO, file= path_to_save)
