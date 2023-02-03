#' @method deplete data.frame
#' @export
deplete.data.frame <- 
function(x, passes=paste("pass",1:3,sep=""), method="Carle.and.Strub", alpha=1, beta=1, ...) {
	dd <- x
	n <- length(dd[,1])
	stopifnot(length(passes) > 1, passes %in% names(dd), n > 0, method %in% c("Carle.and.Strub","Mantyniemi"))
	outDat <- array(NA,dim=c(n,6))
	outDat <- data.frame(outDat,comment=rep("",n),stringsAsFactors = F)
	names(outDat)[1:6] <- c("N", "N.se","p","p.se","N.95CIL", "N.95CIU")
	calcPasses <- function(x){
		i <- 0
		while (!is.na(x[i+1]) & i <= length(x)) {
			i <- i + 1
		}
		i
	}
	for(i in 1:n){
		passDat <- as.numeric(dd[i, passes])
		k <- calcPasses(passDat)
		if (k > 1) {
			if (method == "Carle.and.Strub"){
			   outDat[i,1:6] <- unlist(deplete(passDat[1:k], method=method, alpha=alpha, beta=beta))
			} else if (method == "Mantyniemi"){
				 outDat[i,1:6] <- unlist(deplete(passDat[1:k], method=method))
			}
			outDat$comment[i] <- paste(method,k,"pass")
		} else {
			outDat$comment[i] <- paste("less than 2 passes")
		}
	}
	outDat
}

