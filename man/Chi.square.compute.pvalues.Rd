\name{Chi.square.compute.pvalues}
\alias{Chi.square.compute.pvalues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to compute the stability indices and the p-values associated to a set of clusterings according to the chi-square test between 
        multiple proportions. }
\description{
  For a given similarity matrix a list of stability indices, sorted by descending order, from the most significant
clustering to the least significant is given. Moreover the corresponding p-values, computed according to a chi-square based test are provided.
}
\usage{
Chi.square.compute.pvalues(sim.matrix, s0 = 0.9)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sim.matrix}{ a matrix that stores the similarity between pairs of clustering across multiple number of clusters
      and multiple clusterings. Rows correspond to the different clusterings; columns to the n repeated clusterings for each number of clusters
       Row 1 correspond to a 2-clustering, row 2 to a 3-clustering, ... row m to a m+1 clustering. }
  \item{s0}{ threshold for the similarity value (default 0.9) }
}
\details{
  The stability index for a given clustering is computed as the mean of the similarity indices between pairs of 
k-clusterings obtained from the perturbed data. The similarity matrix given as input can be obtained from the functions
do.similarity.resampling, do.similarity.projection, do.similarity.noise. For each k-clustering the proportion
of pairs of perturbed clusterings having similarity indices larger than a given threshold (the parameter \eqn{s0}) is computed.
The p-values are obtained according the chi-square test between multiple proportions (each proportion corresponds to a different k-clustering) 
A low p-value means that there is a significant difference between the top sorted and the given k-clustering. 
}
\value{
  a data frame with 4 components:
  \item{ordered.clusterings }{a vector with the number of clusters ordered from the most significant to the least significant}
  \item{p.value }{a vector with the corresponding p-values computed according to chi-square test between multiple proportions
          in descending order (their values correspond to the clusterings of the vector ordered.clusterings)}
	\item{means }{vector with the stability index (mean similarity) for each k-clustering}
  \item{variance }{vector with the variance of the similarity for each k-clustering}
}
\references{ 
A.Bertoni, G. Valentini, Model order selection for clustered bio-molecular data,  
In: Probabilistic Modeling and Machine Learning in Structural and Systems Biology, J. Rousu, S. Kaski and E. Ukkonen (Eds.), 
Tuusula, Finland, 17-18 June,  2006
}
\author{ Giorgio Valentini \email{valentini@dsi.unimi.it}}
\seealso{ \code{\link{Bernstein.compute.pvalues}}, \code{\link{Hypothesis.testing}},

\code{\link{do.similarity.resampling}}, \code{\link{do.similarity.projection}}, \code{\link{do.similarity.noise}}}
\examples{
# Synthetic data set generation
M <- generate.sample6 (n=10, m=15, dim=800, d=3, s=0.2)
nsubsamples <- 10;  # number of pairs od clusterings to be evaluated
max.num.clust <- 6; # maximum number of cluster to be evaluated
fract.resampled <- 0.8; # fraction of samples to subsampled
# building a similarity matrix using resampling methods, considering clusterings 
# from 2 to 10 clusters with the k-means algorithm
Sr.Kmeans.sample6 <- do.similarity.resampling(M, c=max.num.clust, nsub=nsubsamples, 
                     f=fract.resampled, s=sFM, alg.clust.sim=Kmeans.sim.resampling);
# computing p-values according to the chi square-based test
dr.Kmeans.sample6 <- Chi.square.compute.pvalues(Sr.Kmeans.sample6);
# the same, using noise to perturbate the data and  hierarchical clustering algorithm
Sn.HC.sample6 <- do.similarity.noise(M, c=max.num.clust, nnoisy=nsubsamples, perc=0.5, 
                                 s=sFM, alg.clust.sim=Hierarchical.sim.noise);
dn.HC.sample6 <- Chi.square.compute.pvalues(Sn.HC.sample6);
}
\keyword{ cluster }
\keyword{ htest }
