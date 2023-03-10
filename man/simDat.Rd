% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simDat.R
\name{simDat}
\alias{simDat}
\title{a function to simulate deplection data}
\usage{
simDat(totalFish = 100, passes = 3, captureProb = 0.5)
}
\arguments{
\item{totalFish}{total number of fish in the population to be simulated}

\item{passes}{the number of passes}

\item{captureProb}{the capture probabilities for the different passes. If
this parameter is a single value it is used for all passes}
}
\value{
a numeric vector with the simulated values for each pass
}
\description{
A function to simulate depletion data for testing the depletion function
under different assumptions. Specifically it allows the capture probability
to vary between passes.
}
\details{
A function to simulate depletion data
}
\examples{

x <- simDat(100)
deplete(x)

}
\author{
Martin Liermann
}
\keyword{~kwd1}
\keyword{~kwd2}
