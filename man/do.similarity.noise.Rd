\name{do.similarity.noise}
\alias{do.similarity.noise}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function that computes sets of similarity indices using using injection of gaussian noise.}
\description{
  This function may use different clustering algorithms and different similarity measures to compute similarity indices.
Injection of gaussian noise  is applied to perturb the data. 
The gaussian noise  added to the data has 0 mean and the standard deviation is estimated from the data (it is 
set to a given percentile value of the standard deviations computed for each variable).
More precisely pairs of data sets are perturbed with noise 
and then are clustered and the resulting clusterings are compared using similarity indices between pairs of clusterings
 (e.g. Rand Index, Jaccard or Fowlkes and Mallows indices). These indices are computed multiple times for different number of clusters. 
}
\usage{
do.similarity.noise(X, c = 2, nnoisy = 100, perc = 0.5, seed = 100, s = sFM, 
alg.clust.sim = Hierarchical.sim.noise, distance = "euclidean", hmethod = "ward.D")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{matrix of data (variables are rows, examples columns)}
  \item{c}{ if it is a vector of length 1, number of clusters from 2 to c are considered; otherwise are considered 
        the number of clusters stored in the vector c. }
  \item{nnoisy}{ number of pairs of noisy data }
  \item{perc}{ percentile of the standard deviations to be used for the added gaussian noise (default: median) }
  \item{seed}{ numerical seed for the random generator }
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
 McShane, L.M., Radmacher, D., Freidlin, B., Yu, R.,  Li, M.C. and Simon, R.,
    Method for assessing reproducibility of clustering patterns observed in analyses of microarray data,
    Bioinformatics, 11(8), pp. 1462-1469, 2002.
 }
\author{ Giorgio Valentini \email{valentini@di.unimi.it}}
\seealso{ \code{\link{do.similarity.projection}}, \code{\link{do.similarity.resampling}} }
\examples{
# Data set generation
M <- generate.sample6 (n=20, m=10, dim=600, d=3, s=0.2);
# computing similarity indices with the fuzzy c-mean algorithm
Sn.Fuzzy.kmeans.sample6 <- do.similarity.noise(M, c=8, nnoisy=30, perc=0.5, s=sFM, 
                                      alg.clust.sim=Fuzzy.kmeans.sim.noise);
# computing similarity indices using the c-mean algorithm
Sn.Fuzzy.kmeans.sample6 <- do.similarity.noise(M, c=8, nnoisy=30, perc=0.5, s=sFM, 
                                      alg.clust.sim=Fuzzy.kmeans.sim.noise);
# computing similarity indices using the hierarchical clustering algorithm
Sn.HC.sample6 <- do.similarity.noise(M, c=8, nnoisy=30, perc=0.5, s=sFM, 
                                      alg.clust.sim=Hierarchical.sim.noise);
}
\keyword{ cluster }
