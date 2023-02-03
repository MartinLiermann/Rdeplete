#' a helper function used by deplete to calculate the Mantyniemi estimate for a
#' removal experiment
#' 
#' A function used by the deplete function. Not intended for stand alone use.
#' 
#' not intended for stand alone use.
#' 
#' @param x a numeric vector with counts for each pass, or a dataframe that
#' includes the columns listed in passes
#' @param MCMCsims if true Monte Carlo Markov Chain samples are returned
#' @return \item{N}{population size estimate} \item{N.se}{population size
#' estimate standard error} \item{p}{capture probability estimate}
#' \item{p.se}{capture probability estimate standard error}
#' \item{N.CI95L}{lower 95\% CI for population size} \item{N.CI95U}{upper 95\%
#' CI for population size} \item{MCMCsims}{MCMC simulations returned if
#' MCMCsims=T}
#' @author Martin Liermann
#' @keywords deplete
mantyniemi <- 
function(x, MCMCsims=F){
	set.seed(Sys.time())
	bdat <- list(C = as.numeric(x), k=length(x))
	calcInitVals <- function() list(mu=0.5,u=log(deplete(bdat$C)$N))
	saveList <- c("q","n","mu","eta")
  jagsFiles <- paste(.libPaths(),"/Rdeplete/jagsMod.txt",sep="")
  if(any(file.exists(jagsFiles))){
    jagsFile <- jagsFiles[file.exists(jagsFiles)][1]
  }
  else {
    stop("Cannot file JAGS model file")
  }
  #jagsFile <- "../Rdeplete/inst/jagsMod.txt"
	m1 <- jags(data=bdat, inits=calcInitVals, parameters.to.save=saveList, model.file=jagsFile,
            n.chains=1, n.iter=11000, n.burnin=1000, n.thin=10, DIC=TRUE)
  x <- m1$BUGSoutput$sims.list
  Nci <- HPDinterval(as.mcmc(x$n[,1]))[1:2]
  pci <- HPDinterval(as.mcmc(x$mu))[1:2]
  if(MCMCsims) {
  	list(N=mean(x$n[,1]), N.se=sd(x$n[,1]), p=NA, p.se=NA, N.CI95L=Nci[1], N.CI95U=Nci[2], MCMCsims=x)
  } else {
  	list(N=mean(x$n[,1]), N.se=sd(x$n[,1]), p=NA, p.se=NA, N.CI95L=Nci[1], N.CI95U=Nci[2])
  }
}
