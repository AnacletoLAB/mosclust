\name{perturb.by.noise}
\alias{perturb.by.noise}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to generate a data set perturbed by noise. }
\description{
  This funtion adds gaussian noise to the data. The mean of the gaussian noise is 0 and the standard deviation is estimated from the data.
The gaussian noise  added to the data has 0 mean and the standard deviation is estimated from the data (it is 
set to a given percentile value of the standard deviations computed for each variable).
}
\usage{
perturb.by.noise(X, perc = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ matrix of data (variables are rows, examples columns) }
  \item{perc}{ percentile of the standard deviation (def: 0.5) }
}
\value{
  matrix of perturbed data (variables are rows, examples columns)
}
\references{
 McShane, L.M., Radmacher, D., Freidlin, B., Yu, R.,  Li, M.C. and Simon, R.,
    Method for assessing reproducibility of clustering patterns observed in analyses of microarray data,
    Bioinformatics, 11(8), pp. 1462-1469, 2002.}
\author{ Giorgio Valentini \email{valentini@di.unimi.it}}
\seealso{ \code{\link{do.similarity.noise}}}
\examples{
library("clusterv")
# Data set generation
M <- generate.sample6 (n=20, m=10, dim=100, d=3, s=0.2);
# generation of a data set perturbed by noise
M.perturbed <- perturb.by.noise(M);
# generation of a data set more perturbed by noise
M.more.perturbed <- perturb.by.noise(M, perc=0.95);
}
\keyword{ cluster }
