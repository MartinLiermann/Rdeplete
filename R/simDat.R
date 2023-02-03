#' a function to simulate deplection data
#' 
#' A function to simulate depletion data for testing the depletion function
#' under different assumptions. Specifically it allows the capture probability
#' to vary between passes.
#' 
#' A function to simulate depletion data
#' 
#' @param totalFish total number of fish in the population to be simulated
#' @param passes the number of passes
#' @param captureProb the capture probabilities for the different passes. If
#' this parameter is a single value it is used for all passes
#' @return a numeric vector with the simulated values for each pass
#' @author Martin Liermann
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' x <- simDat(100)
#' deplete(x)
#' 
#' @export simDat
simDat <-
function(totalFish=100,passes=3,captureProb=0.5){
if(length(captureProb)==1){
p <- rep(captureProb,passes)
}else{
p <- captureProb[1:passes]
}
pass <- numeric(passes)
N <- totalFish
for(i in 1:passes){
pass[i] <- rbinom(1,N,p[i])
N <- N - pass[i]
}
pass
}
