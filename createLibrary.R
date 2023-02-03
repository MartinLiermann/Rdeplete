require(devtools)
require(roxygen2)
ghDir <- "C:/Users/martin.liermann/Documents/GitHub"
RDpath <- paste(ghDir,"Rdeplete",sep="/")

document(RDpath)
build(RDpath,binary=T)

# other stuff
# install(RDpath)
# devtools::load_all(".")
# library(Rd2roxygen)
# Rd2roxygen(RDpath)
# devtools::build_vignettes(RDpath)