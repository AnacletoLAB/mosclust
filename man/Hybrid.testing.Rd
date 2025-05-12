\name{Hybrid.testing}
\alias{Hybrid.testing}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Statistical test based on stability methods for model order selection. }
\description{
  Statistical test to estimate if there is a significant difference between a set of clustering solutions.
	Given a set of clustering solutions (that is solutions for different number \emph{k} of clusters),
	the statistical test using both the \emph{Bernstein} inequality-based test and the \eqn{\chi^2} based test evaluates
	what are the significant solutions at a given significance level.
}
\usage{
Hybrid.testing(sim.matrix, alpha = 0.01, s0 = 0.9)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sim.matrix}{  a matrix that stores the similarity between pairs of clustering across multiple number of clusters
 and multiple clusterings.}
  \item{alpha}{ significance level (default 0.01) }
  \item{s0}{ threshold for the similarity value used in the \eqn{\chi^2} based test (default 0.9) }
}
\details{
   The function accepts as input  a similarity matrix that stores the similarity measure between multiple pairs of clusterings 
considering different number of clusters. Each row of the matrix corresponds to a k-clustering, each column to different repeated 
measures.
Note that the similarities can be computed using different clustering algorithms, different perturbations methods 
(resampling techniques, random projections or noise-injection methods) and different similarity measures. 
The stability index for a given clustering is computed as the mean of the similarity indices between pairs of 
k-clusterings obtained from the perturbed data. The similarity matrix given as input can be obtained from the functions
do.similarity.resampling, do.similarity.projection, do.similarity.noise.
The clusterings are ranked according to the values of the stability indices and the Bernstein inequality-based test is iteratively performed
between the top ranked and upward from the last ranked clustering until the null hypothesis (that is no significant difference between the clustering
solutions) cannot be rejected. Then, to refine the solutions, the chi square-based test is performed on the remaining top ranked clusterings.
The significant solutions at a given \eqn{\alpha} significance level, as well as the computed p-values are returned.
}
\value{
  a list with 6 elements: 
  \item{n.Bernstein.selected }{number of clusterings selected as  significant by the Bernstein test}
  \item{n.chi.sq.selected }{number of clusterings selected as  significant by chi square test. It may be 
                            equal to 0 if Bernstein test selects only 1 clustering.}
	\item{Bernstein.res }{data frame with the p-values obtained from Bernstein inequality }
	\item{chi.sq.res }{data frame with the p-values obtained from chi square test. If through Bernstein 
               inequality test only 1 clustering is significant this component is NULL}
  \item{selected.res }{data frame with the results relative to the clusterings selected by the overall hybrid test }
  \item{F}{a list of cumulative distribution functions (of class ecdf) (not sorted).}      
}
\references{ 
W. Hoeffding, Probability inequalities for sums of independent random variables, J. Amer. Statist. Assoc. vol.58 pp. 13-30, 1963.
 
A.Bertoni, G. Valentini, Model order selection for clustered bio-molecular data,  
In: Probabilistic Modeling and Machine Learning in Structural and Systems Biology, J. Rousu, S. Kaski and E. Ukkonen (Eds.), 
Tuusula, Finland, 17-18 June,  2006

A.Bertoni, G. Valentini, Discovering significant structures in clustered data through Bernstein inequality, 
             CISI '06, Conferenza Italiana Sistemi Intelligenti, Ancona, Italia, 2006.
}
\author{ Giorgio Valentini \email{valentini@dsi.unimi.it}}
\seealso{ \code{\link{Bernstein.compute.pvalues}}, \code{\link{Chi.square.compute.pvalues}}, 

\code{\link{Hypothesis.testing}}, \code{\link{do.similarity.resampling}}, 

\code{\link{do.similarity.projection}}, \code{\link{do.similarity.noise}}}
\examples{
\donttest{
library("clusterv")
# Generation of a synthetic data set with a three-levels hierarchical structure
M1 <- generate.sample.h2 (n=20, l=20, Delta.h=6, Delta.v=3, sd=0.1)
# building a similarity matrix using resampling methods, considering clusterings 
# from 2 to 15 clusters
S1.HC <- do.similarity.resampling (M1, c=15, nsub=20, f=0.8, s=sFM, 
                                   alg.clust.sim=Hierarchical.sim.resampling)
# Application of the Hybrid statistical test
l1.HC <- Hybrid.testing(S1.HC, alpha=0.01, s0=0.95)
# 3 clusterings are selected, according to the hierarchical structure of the data:
l1.HC$selected.res
}
}
\keyword{ cluster }
\keyword{ htest }
