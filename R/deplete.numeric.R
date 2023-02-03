#' @method deplete numeric
#' @export
deplete.numeric <- 
function(x, method="Carle.and.Strub",alpha=1, beta=1, MCMCsims=F, ...){
	stopifnot(method %in% c("Carle.and.Strub","Mantyniemi"))
	if (method=="Carle.and.Strub"){
		retVal <- carleAndStrub(x,alpha=alpha,beta=beta)
	} else {
		retVal <- mantyniemi(x, MCMCsims)
	}
	retVal
}

