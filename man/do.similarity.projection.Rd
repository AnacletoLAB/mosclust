\name{do.similarity.projection}
\alias{do.similarity.projection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function that computes sets of similarity indices using randomized maps. }
\description{
  This function may use different clustering algorithms and different similarity measures to compute similarity indices.
  Random projections techniques are applied to perturb the data. 
	More precisely pairs of data sets are projected into lower dimensional subspaces using randomized maps,
  and then are clustered and the resulting clusterings are compared using similarity indices between pairs of clusterings
 (e.g. Rand Index, Jaccard or Fowlkes and Mallows indices). These indices are computed multiple times for different number of clusters. 
}
\usage{
do.similarity.projection(X, c = 2, nprojections = 100, dim = 2, pmethod = "PMO", 
scale = TRUE, seed = 100, s = sFM, alg.clust.sim = Hierarchical.sim.projection, 
distance = "euclidean", hmethod = "ward")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{matrix of data (variables are rows, examples columns)}
  \item{c}{ if it is a vector of length 1, number of clusters from 2 to c are considered; otherwise are considered 
        the number of clusters stored in the vector c. }
  \item{nprojections}{ number of pairs of projected data }
  \item{dim}{ dimension of the projected data }
  \item{pmethod}{  pmethod : projection method. It must be one of the following:  
									"RS" (random subspace projection)
									"PMO" (Plus Minus One random projection) (default)
									"Norm" (normal random projection)
									"Achlioptas" (Achlioptas random projection)}
  \item{scale}{ if TRUE randomized projections are scaled (default) }
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
                    "ward", "single", "complete", "average", "mcquitty", "median" or "centroid", according of the hclust
                    method of the package stats. }
}
\value{
  a matrix that stores the similarity between pairs of clustering across multiple number of clusters
  and multiple clusterings performed on subsamples of the original data.
  Number or rows equal to the length of c (number of clusters); number of columns      
  equal to nsub, that is the number of subsamples considered for each number of clusters.
}
\references{ 
 A.Bertoni, G. Valentini, Randomized maps for assessing the reliability of patients clusters in DNA microarray data analyses, 
 Artificial Intelligence in Medicine 37(2):85-109  2006}
\author{ Giorgio Valentini \email{valenti@dsi.unimi.it}}
\seealso{ \code{\link{do.similarity.resampling}}, \code{\link{do.similarity.noise}} }
\examples{
# Data set generation
M <- generate.sample6 (n=20, m=10, dim=600, d=3, s=0.2);
# computing similarity indices with the fuzzy c-mean algorithm
Sp.fuzzy.kmeans.sample6 <- do.similarity.projection(M, c=8, nprojections=30, 
   dim=JL.predict.dim(120,0.2), pmethod="PMO", alg.clust.sim=Fuzzy.kmeans.sim.projection);
# computing similarity indices using the c-mean algorithm
Sp.kmeans.sample6 <- do.similarity.projection(M, c=8, nprojections=30, 
   dim=JL.predict.dim(120,0.2), pmethod="PMO", alg.clust.sim=Kmeans.sim.projection);
# computing similarity indices using the hierarchical clustering algorithm
Sp.HC.sample6 <- do.similarity.projection(M, c=8, nprojections=30, 
   dim=JL.predict.dim(120,0.2), pmethod="PMO", alg.clust.sim=Hierarchical.sim.projection);
}
\keyword{ cluster }
