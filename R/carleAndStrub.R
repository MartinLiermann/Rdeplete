#' a helper function used by deplete to calculate the Carle and Strub estimate
#' for a removal experiment
#' 
#' A function used by the deplete function. Not intended for stand alone use.
#' 
#' not intended for stand alone use.
#' 
#' @param x a numeric vector with counts for each pass, or a dataframe that
#' includes the columns listed in passes
#' @param alpha the alpha parameter for the Carle.and.Strub method
#' @param beta the beta parameter for the Carle.and.Strub method
#' @return \item{N}{population size estimate} \item{N.se}{population size
#' estimate standard error} \item{p}{capture probability estimate}
#' \item{p.se}{capture probability estimate standard error}
#' \item{N.CI95L}{lower 95\% CI for population size} \item{N.CI95U}{upper 95\%
#' CI for population size}
#' @author Martin Liermann
#' @keywords deplete
carleAndStrub <- 
function(x,alpha=1,beta=1){
	catch <- x
	k <- length(catch)
	i <- 1:k
	T <- sum(catch)
	X <- sum((k-i)*catch)
	N <- T
	repeat{
		if((N+1)/(N-T+1)*prod((k*N-X-T+alpha+k-i)/(k*N-X+alpha+beta+k-i)) <=1) break()
		N <- N+1
	}
	p <- T/(k*N-X)
	q <- 1-p
	FF <- -(1/N)*(1-q^k)/q^k
	# N.se=sqrt((N*T*(N-T))/(T^2-N*(N-T)*(k*p)^2/(1-p)))
	N.se=sqrt(q*T/(-FF*q*T-(k*p)^2)) # large sample variance from equation 17 in Zippin 1956
	# p.se=sqrt(((q*p)^2*(1-q^k))/(N*(q*(1-q^k)^2-(p*k)^2*q^k)))
	p.se=sqrt(-FF*(q*p)^2/(-FF*q*T - (k*p)^2))  # large sample variance from equation 18 in Zippin 1956
	# eventually save as data.frame even for single site (just like version 1.01)
	list(N=N, N.se=N.se,p=p, p.se=p.se, N.CI95L=N-2*N.se,  N.CI95U=N+2*N.se)
}
