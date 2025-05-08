\name{Hierarchical.sim.resampling}
\alias{Hierarchical.sim.resampling}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to compute similarity indices using resampling techniques and hierarchical clustering. }
\description{
 Function to compute similarity indices using resampling techniques and hierarchical clustering.
A vector of similarity measures between pairs of clusterings perturbed with resampling techniques is computed for a given number of clusters. 
The fraction of the resampled data (without replacement), the similarity measure and  the type of hierarchical clustering may be selected.
}
\usage{
Hierarchical.sim.resampling(X, c = 2, nsub = 100, f = 0.8, s = sFM, 
                            distance = "euclidean", hmethod = "ward.D")
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
  \item{distance}{ it must be one of the two: "euclidean" (default) or "pearson" (that is 1 - Pearson correlation) }
  \item{hmethod}{  the agglomeration method to be used. This parameter is used only by the hierarchical clustering algorithm.
                    This should be one of the following:
                    "ward.D", "single", "complete", "average", "mcquitty", "median" or "centroid", according of the hclust
                    method of the package stats. }
}

\value{
  vector of the computed similarity measures (length equal to nsub)
}
\author{ Giorgio Valentini \email{valenti@dsi.unimi.it}}
\seealso{ \code{\link{Hierarchical.sim.projection}}, \code{\link{Hierarchical.sim.noise}} }
\examples{
# Synthetic data set generation
M <- generate.sample6 (n=20, m=10, dim=600, d=3, s=0.2);
# computing a vector of similarity indices with 2 clusters:
v2 <- Hierarchical.sim.resampling(M, c = 2, nsub = 20, f = 0.8, s = sFM)
# computing a vector of similarity indices with 3 clusters:
v3 <- Hierarchical.sim.resampling(M, c = 3, nsub = 20, f = 0.8, s = sFM)
# computing a vector of similarity indices with 2 clusters using the Jaccard index
v2J <- Hierarchical.sim.resampling(M, c = 2, nsub = 20, f = 0.8, s = sJaccard)
#  2 clusters using the Jaccard index and Pearson correlation
v2JP <- Hierarchical.sim.resampling(M, c = 2, nsub = 20, f = 0.8, s = sJaccard, 
                                    distance="pearson")
}
\keyword{ cluster }
