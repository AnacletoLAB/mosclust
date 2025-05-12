\name{Kmeans.sim.resampling}
\alias{Kmeans.sim.resampling}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to compute similarity indices using resampling techniques and kmeans clustering. }
\description{
  A vector of similarity measures between pairs of clusterings perturbed with resampling techniques is computed for a given number of clusters,
	using the kmeans algorithm.
  The fraction of the resampled data (without replacement) and the similarity measure can be selected.
}
\usage{
Kmeans.sim.resampling(X, c = 2, nsub = 100, f = 0.8, s = sFM, 
                      distance = "euclidean", hmethod = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ matrix of data (variables are rows, examples columns) }
  \item{c}{ number of clusters }
  \item{nsub}{ number of subsamples }
  \item{f}{ fraction of the data resampled without replacement }
  \item{s}{ similarity function to be used. It may be one of the following: 
                                - sFM (Fowlkes and Mallows)
                                - sJaccard (Jaccard)
                                - sM (matching coefficient)
                                  (default Fowlkes and Mallows) }
  \item{distance}{ actually only the euclidean distance is available "euclidean" (default) }
  \item{hmethod}{ parameter used for internal compatibility. }
}
\value{
  vector of the computed similarity measures (length equal to nsub)
}
\author{ Giorgio Valentini \email{valentini@di.unimi.it}}
\seealso{ \code{\link{Kmeans.sim.projection}}, \code{\link{Kmeans.sim.noise}} }
\examples{
library("clusterv")
# Synthetic data set generation
M <- generate.sample6 (n=20, m=10, dim=600, d=3, s=0.2);
# computing a vector of similarity indices with 2 clusters:
v2 <- Kmeans.sim.resampling(M, c = 2, nsub = 20, f = 0.8, s = sFM)
# computing a vector of similarity indices with 3 clusters:
v3 <- Kmeans.sim.resampling(M, c = 3, nsub = 20, f = 0.8, s = sFM)
# computing a vector of similarity indices with 2 clusters using the Jaccard index
v2J <- Kmeans.sim.resampling(M, c = 2, nsub = 20, f = 0.8, s = sJaccard)
}
\keyword{ cluster }
