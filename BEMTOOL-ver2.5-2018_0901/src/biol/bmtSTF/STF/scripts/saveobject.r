# BEMTOOL - Bio-Economic Model TOOLs - version 2.5
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# BEMTOOL is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





# ------------------------------------------------------------------------------
# Function for saving objects in file
# ------------------------------------------------------------------------------

saveobject <- function (x,
                   file = "",
                   control = c("keepNA", "keepInteger", "showAttributes")){
    if (is.character(file))
        if (nzchar(file)) {
            file <- file(file, "wt")
            on.exit(close(file))
        }
        else file <- stdout()
    opts <- .deparseOpts(control)
    if (isS4(x)) {
        cat("new(\"", class(x), "\"\n", file = file, sep = "")
        for (n in slotNames(x)) {
            cat("    ,", n, "= ", file = file)
            saveobject(slot(x, n), file = file, control = control)
        }
        cat(")\n", file = file)
        invisible()
    } else if(length(grep('@',capture.output(str(x)))) > 0){
      if(is.list(x)){
        cat("list(\n", file = file, sep = "")
        for (i in 1:length(x)) {
          if(!is.null(names(x))){
            n <- names(x)[i]
            if(n != ''){
              cat("    ,", n, "= ", file = file)
            }
          }
          saveobject(x[[i]], file = file, control = control)
        }
        cat(")\n", file = file)
        invisible()
      } else {
        stop('S4 objects are only handled if they are contained within an S4 object or a list object')
      }
    }
    else .Internal(dput(x, file, opts))
}


#saveobject(SPECIES_1,file=(tempFile <- paste(getwd(), "\\Species_obj.dat", sep="")) )
#saveobject(FLEETSEGMENT_1,file=(tempFile <- paste(getwd(), "\\Fleetsegment_obj.dat", sep="")) )
#saveobject(LANDINGS_1,file=(tempFile <- paste(getwd(), "\\Yield_Landings_obj.dat", sep="")) )
#saveobject(EXPLOITEDSTOCK_1,file=(tempFile <- paste(getwd(), "\\Population_ExploitedStock_obj.dat", sep="")) )
#saveobject(STOCK_1,file=(tempFile <- paste(getwd(), "\\Stock_obj.dat", sep="")) )
## load object from file 
#SPECIES_1_temp <- dget(tempFile) 
## verify the equality
#all.equal(SPECIES_1,SPECIES_1_temp)