\name{Hypothesis.testing}
\alias{Hypothesis.testing}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to select significant clusterings from a given set of p-values }
\description{
  For a given set of p-values returned from a given hypothesis testing, it returns the items for which there is no significant difference at
	alpha significance level (that is the items for which p > alpha).
}
\usage{
Hypothesis.testing(d, alpha = 0.01)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{ data frame with the p-values returned by a given test of hypothesis (e.g. Bernstein or Chi square-based tests) }
  \item{alpha}{ significance level }
}

\value{
 a data frame corresponding to the clusterings significant at alpha level
}

\author{ Giorgio Valentini \email{valentini@dsi.unimi.it}}
\seealso{ \code{\link{Bernstein.compute.pvalues}} \code{\link{Chi.square.compute.pvalues}}}
\examples{
library("clusterv")
# Synthetic data set generation
M <- generate.sample6 (n=20, m=10, dim=1000, d=3, s=0.2)
nsubsamples <- 10;  # number of pairs od clusterings to be evaluated
max.num.clust <- 6; # maximum number of cluster to be evaluated
fract.resampled <- 0.8; # fraction of samples to subsampled
# building a similarity matrix using resampling methods, considering clusterings 
# from 2 to 10 clusters with the k-means algorithm
Sr.Kmeans.sample6 <- do.similarity.resampling(M, c=max.num.clust, nsub=nsubsamples, 
                     f=fract.resampled, s=sFM, alg.clust.sim=Kmeans.sim.resampling);
# computing p-values according to the chi square-based test
dr.Kmeans.sample6 <- Chi.square.compute.pvalues(Sr.Kmeans.sample6);
# test of hypothesis based on the obtained set of p-values
hr.Kmeans.sample6 <- Hypothesis.testing(dr.Kmeans.sample6, alpha=0.01);
# at the given significance level (0.01) the clustering with 2 clusters is selected:
hr.Kmeans.sample6
}
\keyword{ cluster }
\keyword{ htest }
