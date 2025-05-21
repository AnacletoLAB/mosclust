\name{compute.cumulative.multiple}
\alias{compute.cumulative.multiple}
\alias{cumulative.values}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to compute the empirical cumulative distribution function (ECDF) of the similarity measures. }
\description{
 The function \code{compute.cumulative.multiple} computes the empirical cumulative distribution function (ECDF) of the similarity measures 
 for different number of clusters between clusterings.
The function \code{cumulative.values} returns the values of the empirical cumulative distribution
}
\usage{
compute.cumulative.multiple(sim.matrix)

cumulative.values(Fun)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sim.matrix}{ a matrix that stores the similarity between pairs of clustering across multiple number of clusters
and multiple clusterings. Each row corresponds to a different number of clusters; number of columns
equal to  the number of subsamples considered for each number of clusters. }
\item{Fun}{ Function of class ecdf  that stores the discrete values of the cumulative distribution }
}
\value{
Function  \code{compute.cumulative.multiple}: a list of function of class ecdf.
	
Function  \code{cumulative.values}:	 a list with two elements: the "x" element stores a vector with the values of the random variable for
which the cumulative distribution needs to be computed; the "y" element stores a vector with the corresponding
values of the cumulative distribution (i.e. y = F(x)).
}
\author{ Giorgio Valentini \email{valentini@di.unimi.it}}
\seealso{  \code{\link{plot_cumulative.multiple}}}
\examples{
library("clusterv")
# Data set generation
M <- generate.sample6 (n=20, m=10, dim=1000, d=3, s=0.2);
# generation of multiple similarity measures by resampling
Sr.kmeans.sample6 <- do.similarity.resampling(M, c=10, nsub=20, f=0.8, s=sFM, 
                                      alg.clust.sim=Kmeans.sim.resampling); 
# computation of multiple ecdf (from 2 to 10 clusters)
list.F <- compute.cumulative.multiple(Sr.kmeans.sample6);
# values of the ecdf for 8 clusters 
l <- cumulative.values(list.F[[7]])
}
\keyword{ cluster }
