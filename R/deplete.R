#' function to estimate population size based on removal data
#' 
#' Estimates population size based on M pass removal data
#' 
#' The function deplete implements the maximum likelihood weighted estimator as
#' described in Carl and Strub 1978 \cr Note that this estimator assumes that
#' the capture probabily is the same for all passes. \cr Violation of this
#' assumption can lead to substantial bias (see the example below). \cr
#' 
#' @aliases deplete deplete.numeric deplete.data.frame
#' @param x a numeric vector with counts for each pass, or a dataframe that
#' includes the columns listed in passes
#' @param passes a character vector with the names of columns which contain the
#' passess. This should only be included if x is a dataframe
#' @param method the method used to calculate the estimates and standard
#' errors. Currently Carle.and.Strub and Montyniemi available
#' @param alpha the alpha parameter for the Carle.and.Strub method
#' @param beta the beta parameter for the Carle.and.Strub method
#' @param ... additional parameters
#' @return When x is a numeric vector deplete returns a list with
#' \item{N}{population size estimate} \item{N.se}{standard error for population
#' estimate based large sample approximation in Zippen 1956 ,eq. 17}
#' \item{p}{capture probability estimate} \item{p.se}{standard error for
#' capture probability estimate based large sample approximation in Zippen 1956
#' ,eq. 18} \item{N.CI95L}{lower 95\% CI for population size}
#' \item{N.CI95U}{upper 95\% CI for population size}
#' 
#' When x is a dataframe deplete returns a dataframe with columns corresponding
#' \cr to the list above with an additional comment column describing the type
#' of estimate.\cr \cr Confidence intervals are +-2*N.se when method =
#' "Carle.and.Strub" or \cr Bayesian high posterior density interval when
#' method = "Monteyniemi"
#' @note Based on excelent Depletion Vignette on the fishR website by Dr. Derek
#' Ogle \cr and references below
#' http://www.ncfaculty.net/dogle/fishR/gnrlex/gnrlex.html
#' @author Martin Liermann
#' @references Carle, F. L. and M. R. Strub. 1978. A new method for estimating
#' population \cr size from removal data. Biometrics 34:621-630. \cr M?ntyniemi
#' S, Romakkaniemi A, Arjas E. 2005. Bayesian removal estimation of a
#' population size under unequal catchability. Can. J. Fish. Aquat. Sci. 62(2):
#' 291-300 \cr Olsen, E., R. French, A. Ritchey, M. Lambert, M. Jennings, and
#' P. O'Toole. 1996. Hood river and \cr pelton ladder evaluation studies. (bpa
#' report doe/bp-00631-4), Bonneville Power Administration. \cr Zippin, C.
#' 1956. An evaluation of the removal method of estimating animal populations.
#' \cr Biometrics 12:163?169. 10 \cr
#' @keywords fish removal
#' @examples
#' 
#' # Hood River steelhead (Olsen et al. 1996)
#' # 3 passes
#' x <- c(187,77,35)
#' deplete(x)
#' 
#' # constructing estimates for IMW data
#' # reshape passes as columns and change column names to pass1, pass2 ...
#' data(IMWexample)
#' dat <- reshape(IMWexample,timevar="Pass",idvar=c("tagYear","River","RP","Species"),direction="wide")
#' names(dat)[5:7] <- paste("pass",1:3,sep="")
#' # calculate estimates for all rows
#' ests <- deplete(dat)
#' # combine with original data
#' dat <- cbind(dat,ests)
#' head(dat)
#' 
#' # comparing the two methods (big difference!)
#' deplete(c(10,2,1), method="Carle.and.Strub")
#' deplete(c(10,2,1), method="Mantyniemi")
#' 
#' # using the MCMC sample when method = "Mantyniemi" 
#' # data from the Mantyniemie et al. 2004 paper
#' paperDat <- c(62,26,17,8,9,6,4,1,2,1,1,1,1,1,0,1,0,0,1,0)
#' samps <- deplete(paperDat, method="Mantyniemi", MCMCsims=T)$MCMCsims
#' plot(density(samps$n[,1]))
#' 
#' # a simulation demonstrating the effects of non-constant catchability
#' # black 95% CI = Carle and Strub
#' # gray 95% CI = Mantynieme
#' # vertical line = true population size
#' sims <- 10
#' plot(1,1,xlim=c(0,80*2),ylim=c(0,sims+1),type="n",xlab="Population size",ylab="simulations")
#' lines(rep(80,2),c(0,sims+1))
#' for(i in 1:sims){
#' 	catch <- simDat(totalFish=80,passes=3,captureProb=c(0.5,0.3,0.2)) # decreasing catchability
#' 	est <- deplete(catch, method="Mantyniemi")
#' 	lines(c(est$N.CI95L,est$N.CI95U),rep(i,2),lwd=2,col="gray")
#' 	est <- deplete(catch, method="Carle.and.Strub")
#' 	lines(c(est$N.CI95L,est$N.CI95U),rep(i+0.25,2),lwd=2,col="black")
#' }
#' 
#' @export deplete
deplete <-
function(x,...) {
UseMethod("deplete",x)
}
