\name{compute.integral}
\alias{compute.integral}
\alias{compute.integral.from.similarity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Functions to compute the integral of the ecdf of the similarity values}
\description{
  The function \code{compute.integral} computes the integral of the ecdf form the function of 
	class ecdf  that stores the discrete values of the empirical cumulative distribution, while
	the function \code{compute.integral.from.similarity} computes the integral of the ecdf
	exploiting then empirical mean of the similarity values (see the paper cited in the reference section for details).
}
\usage{
compute.integral(F, subdivisions = 1000)

compute.integral.from.similarity(sim.matrix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{F}{ Function of class ecdf  that stores the discrete values of the empirical cumulative distribution }
  \item{subdivisions}{ maximum number of subintervals used by the integration process }
	\item{sim.matrix}{ a matrix that stores the similarity between pairs of clustering across multiple number of clusters
and multiple clusterings performed on subsamples of the original data.
Number or rows equal to the different numbers of clusters considered; number of columns      
equal to  the number of subsamples considered for each number of clusters. }
}
\value{
 The function \code{compute.integral} returns the value of the estimate integral.
 The function \code{compute.integral.from.similarity} returns a vector of the values of the estimate integrals (one for each row of \eqn{sim.matrix}).
}
\references{ 
A.Bertoni, G. Valentini, Discovering significant structures in clustered data through Bernstein inequality, 
             CISI '06, Conferenza Italiana Sistemi Intelligenti, Ancona, Italia, 2006. }
\author{ Giorgio Valentini \email{valentini@di.unimi.it}}
\examples{
library("clusterv")
# Data set generation
M <- generate.sample6 (n=20, m=10, dim=1000, d=3, s=0.2);
# generation of multiple similarity measures by resampling
Sr.kmeans.sample6 <- do.similarity.resampling(M, c=10, nsub=20, f=0.8, s=sFM, 
                                      alg.clust.sim=Kmeans.sim.resampling); 
# computation of multiple ecdf (from 2 to 10 clusters)
list.F <- compute.cumulative.multiple(Sr.kmeans.sample6);
# computation of the integral of the ecdf with 2 clusters
compute.integral(list.F[[1]])
# computation of the integral of the ecdf with 8 clusters
compute.integral(list.F[[7]])
# computation of the integral of the ecdfs from 2 to 10 clusters
compute.integral.from.similarity(Sr.kmeans.sample6)
}
\keyword{ cluster }
