\name{Kmeans.sim.noise}
\alias{Kmeans.sim.noise}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to compute similarity indices using noise injection techniques and kmeans clustering. }
\description{
  A vector of similarity measures between pairs of clusterings perturbed with random noise is computed for a given number of clusters. 
The variance of the added gaussian noise, estimated from the data as the perc percentile of the standard deviations of the input variables,
the percentile itself and the similarity measure can be selected. 
}
\usage{
Kmeans.sim.noise(X, c = 2, nnoisy = 100, perc = 0.5, s = sFM, 
                 distance = "euclidean", hmethod = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ matrix of data (variables are rows, examples columns) }
  \item{c}{ number of clusters }
  \item{nnoisy}{ number of pairs of noisy data }
  \item{perc}{ percentile of the standard deviations to be used for the added gaussian noise (def. 0.5) }
  \item{s}{ similarity function to be used. It may be one of the following: 
                                - sFM (Fowlkes and Mallows)
                                - sJaccard (Jaccard)
                                - sM (matching coefficient)
                                  (default Fowlkes and Mallows) }
  \item{distance}{ actually only the euclidean distance is available "euclidean" (default) }
  \item{hmethod}{ parameter used for internal compatibility. }
}
\value{
  vector of the computed similarity measures (length equal to nnoisy)
}
\author{ Giorgio Valentini \email{valentini@di.unimi.it}}
\seealso{ \code{\link{Kmeans.sim.projection}}, \code{\link{Kmeans.sim.resampling}}, \code{\link{perturb.by.noise}} }
\examples{
# Synthetic data set generation
M <- generate.sample6 (n=20, m=10, dim=600, d=3, s=0.2);
# computing a vector of similarity indices with 2 clusters:
v2 <- Kmeans.sim.noise(M, c = 2, nnoisy = 20,  s = sFM)
# computing a vector of similarity indices with 3 clusters:
v3 <- Kmeans.sim.noise(M, c = 3, nnoisy = 20,  s = sFM)
# computing a vector of similarity indices with 2 clusters using the Jaccard index
v2J <- Kmeans.sim.noise(M, c = 2, nnoisy = 20,  s = sJaccard)
# 2 clusters using 0.95 percentile (more noise)
v095 <- Kmeans.sim.noise(M, c = 2, nnoisy = 20,  s = sFM, perc=0.95)
}
\keyword{ cluster }
