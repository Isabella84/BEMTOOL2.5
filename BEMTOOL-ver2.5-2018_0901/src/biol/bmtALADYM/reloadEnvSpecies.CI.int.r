# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





cat("\n\n")
print("***************************************************************************", quote=FALSE)
print(paste("Reading last saved BEMTOOL biological simulation [ALADYM] for species", BMT_SPECIES[ALADYM_spe]), quote=FALSE)
print("***************************************************************************", quote=FALSE)
cat("\n\n")

#INP <- new.env()
#RND <- new.env()
#BAS <- new.env()
#GLO <- new.env()
#SRO <- new.env()
#

if (current_year == 1) {
path_to_save <- paste(casestudy_path, "/Diagnosis/ALADYM/", BMT_SPECIES[ALADYM_spe], "/INP_", ALADYM_spe, ".Rdata", sep="")
no_error <- try( load(file= path_to_save , INP), silent=TRUE)

path_to_save <- paste(casestudy_path, "/Diagnosis/ALADYM/", BMT_SPECIES[ALADYM_spe], "/RND_", ALADYM_spe, ".Rdata", sep="")
load(file= path_to_save , RND)

path_to_save <- paste(casestudy_path, "/Diagnosis/ALADYM/", BMT_SPECIES[ALADYM_spe], "/BAS_", ALADYM_spe, ".Rdata", sep="")
load(file= path_to_save , BAS)

path_to_save <- paste(casestudy_path, "/Diagnosis/ALADYM/", BMT_SPECIES[ALADYM_spe], "/SRO_", ALADYM_spe, ".Rdata", sep="")
load(file= path_to_save , SRO)

path_to_save <- paste(casestudy_path, "/Diagnosis/ALADYM/", BMT_SPECIES[ALADYM_spe], "/GLO_", ALADYM_spe, ".Rdata", sep="")
load(file= path_to_save , GLO)

} else {
path_to_save <-  paste(casestudy_path, "/",harvest_rule_id, "/ALADYM/", BMT_SPECIES[ALADYM_spe], "/INP_", ALADYM_spe,".Rdata", sep="")
no_error <- try( load(file= path_to_save , INP), silent=TRUE)

path_to_save <- paste(casestudy_path, "/",harvest_rule_id, "/ALADYM/", BMT_SPECIES[ALADYM_spe], "/SRO_", ALADYM_spe, ".Rdata", sep="")
no_error <- try( load(file= path_to_save , SRO), silent=TRUE)

path_to_save <- paste(casestudy_path, "/",harvest_rule_id, "/ALADYM/", BMT_SPECIES[ALADYM_spe], "/BAS_", ALADYM_spe, ".Rdata", sep="")
no_error <- try( load(file= path_to_save , BAS), silent=TRUE)

path_to_save <- paste(casestudy_path,  "/",harvest_rule_id, "/ALADYM/", BMT_SPECIES[ALADYM_spe], "/RND_", ALADYM_spe, ".Rdata", sep="")
load(file= path_to_save , RND)

path_to_save <- paste(casestudy_path, "/",harvest_rule_id, "/ALADYM/", BMT_SPECIES[ALADYM_spe], "/GLO_", ALADYM_spe, ".Rdata", sep="")
load(file= path_to_save , GLO)

}

 
 if (class(no_error) !=  "try-error") {
 
 READ_ENV_OK <<- T

#print(paste("Reading", paste("INP_", ALADYM_spe, sep=""), "environment..."), quote=F)
assign(paste("INP_", ALADYM_spe, sep=""), new.env())
INP_simulation <- get(paste("INP_", ALADYM_spe, sep=""))
for (obj_name in ls(INP)) {
     assign(obj_name, get(obj_name, envir = INP), envir=INP_simulation)     
}



#print(paste("Reading", paste("RND_", ALADYM_spe, sep=""), "environment..."), quote=F)
assign(paste("RND_", ALADYM_spe, sep=""), new.env())
RND_simulation <- get(paste("RND_", ALADYM_spe, sep=""))
for (obj_name in ls(RND)) {
     assign(obj_name, get(obj_name, envir = RND), envir=RND_simulation)     
}




#print(paste("Reading", paste("BAS_", ALADYM_spe, sep=""), "environment..."), quote=F)
assign(paste("BAS_", ALADYM_spe, sep=""), new.env())
BAS_simulation <- get(paste("BAS_", ALADYM_spe, sep=""))
for (obj_name in ls(BAS)) {
     assign(obj_name, get(obj_name, envir = BAS), envir=BAS_simulation)     
}


#print(paste("Reading", paste("SRO_", ALADYM_spe, sep=""), "environment..."), quote=F)
assign(paste("SRO_", ALADYM_spe, sep=""), new.env())
SRO_simulation <- get(paste("SRO_", ALADYM_spe, sep=""))
for (obj_name in ls(SRO)) {
     assign(obj_name, get(obj_name, envir = SRO), envir=SRO_simulation)     
}

#print(paste("Reading", paste("GLO_", ALADYM_spe, sep=""), "environment..."), quote=F)
assign(paste("GLO_", ALADYM_spe, sep=""), new.env())
GLO_simulation <- get(paste("GLO_", ALADYM_spe, sep=""))
for (obj_name in ls(GLO)) {
     assign(obj_name, get(obj_name, envir = GLO), envir=GLO_simulation)     
}

} else {   

READ_ENV_OK <<- F

      print(paste("No ALADYM files has been found for species", BMT_SPECIES[ALADYM_spe], ". Impossible skip the simulation!"), quote=F)
      showError(paste("No ALADYM files has been found for species", BMT_SPECIES[ALADYM_spe], ". Impossible execute the forecast!")) 
      ALADYM_stop <<- TRUE
      SKIP_spe <<- FALSE
}