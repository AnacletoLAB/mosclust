\name{do.similarity.resampling}
\alias{do.similarity.resampling}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function that computes sets of similarity indices using resampling techniques. }
\description{
 This function may use different clustering algorithms and different similarity measures to compute similarity indices.
 Subsampling techniques are applied to perturb the data. More precisely pairs of data sets are sampled according to an uniform distribution
 without replacement and then are clustered and the resulting clusterings are compared using similarity indices between pairs of clusterings
 (e.g. Rand Index, Jaccard or Fowlkes and Mallows indices). These indices are computed multiple times for different number of clusters. 
}
\usage{
do.similarity.resampling(X, c = 2, nsub = 100, f = 0.8, s = sFM, 
alg.clust.sim = Hierarchical.sim.resampling, distance = "euclidean", hmethod = "ward.D")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{matrix of data (variables are rows, examples columns)}
  \item{c}{ if it is a vector of length 1, number of clusters from 2 to c are considered; otherwise are considered 
        the number of clusters stored in the vector c. }
  \item{nsub}{ number of pairs of subsamples }
  \item{f}{ fraction of the data resampled without replacement }
  \item{s}{ similarity function to be used. It may be one of the following: 
                                - sFM (Fowlkes and Mallows)
                                - sJaccard (Jaccard)
                                - sM (matching coefficient)
                                  (default Fowlkes and Mallows)}
  \item{alg.clust.sim}{ method that computes the similarity indices using subsampling techniques and a specific clustering algorithm. 
												 It may be one of the following: 
												 - Hierarchical.sim.resampling (hierarchical clustering algorithm, default)
												 - Kmeans.sim.resampling (c - mean algorithm)
												 - PAM.sim.resampling (Prediction Around Medoid algorithm)}
												 - Fuzzy.kmeans.sim.resampling (Fuzzy c-mean)
  \item{distance}{ it must be one of the two: "euclidean" (default) or "pearson" (that is 1 - Pearson correlation) }
  \item{hmethod}{  the agglomeration method to be used. This parameter is used only by the hierarchical clustering algorithm.
                    This should be one of the following:
                    "ward.D", "single", "complete", "average", "mcquitty", "median" or "centroid", according of the hclust
                    method of the package stats. }
}

\value{
  a matrix that stores the similarity between pairs of clustering across multiple number of clusters
  and multiple clusterings performed on subsamples of the original data.
  Number or rows equal to the length of c (number of clusters); number of columns      
  equal to nsub, that is the number of subsamples considered for each number of clusters.
}
\references{ 
 Ben-Hur, A. Ellisseeff, A. and Guyon, I., A stability based method for discovering structure in clustered data,
   In: "Pacific Symposium on Biocomputing", Altman, R.B. et al (eds.), pp, 6-17, 2002.
 }
\author{ Giorgio Valentini \email{valentini@di.unimi.it}}
\seealso{ \code{\link{do.similarity.projection}}, \code{\link{do.similarity.noise}} }
\examples{
library("clusterv")
# Data set generation
M <- generate.sample6 (n=20, m=10, dim=600, d=3, s=0.2);
# computing similarity indices with the fuzzy c-mean algorithm
Sr.Fuzzy.kmeans.sample6 <- do.similarity.resampling(M, c=8, nsub=30, f=0.8, s=sFM, 
                           alg.clust.sim=Fuzzy.kmeans.sim.resampling);
# computing similarity indices using the c-mean algorithm
Sr.Kmeans.sample6 <- do.similarity.resampling(M, c=8, nsub=30, f=0.8, s=sFM, 
                                      alg.clust.sim=Kmeans.sim.resampling)
# computing similarity indices using the hierarchical clustering algorithm
Sr.HC.sample6 <- do.similarity.resampling(M, c=8, nsub=30, f=0.8, s=sFM);
}
\keyword{ cluster }


