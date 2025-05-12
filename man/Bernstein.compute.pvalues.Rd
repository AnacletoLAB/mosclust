\name{Bernstein.compute.pvalues}
\alias{Bernstein.compute.pvalues}
\alias{Bernstein.ind.compute.pvalues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to compute the stability indices and the p-values associated to a set of clusterings according to Bernstein inequality. }
\description{
 For a given similarity matrix a list of stability indices, sorted by descending order, from the most significant
clustering to the least significant is given, and the corresponding p-values, computed according to a Bernstein inequality based test are  provided.
}
\usage{
Bernstein.compute.pvalues(sim.matrix)

Bernstein.ind.compute.pvalues(sim.matrix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sim.matrix}{ a matrix that stores the similarity between pairs of clustering across multiple number of clusters
and multiple clusterings.
Rows correspond to the different clusterings; columns to the n repeated clusterings for each number of clusters
Row 1 corresond to a 2-clustering, row 2 to a 3-clustering, ... row m to a m+1 clustering. }
}
\details{
The stability index for a given clustering is computed as the mean of the similarity indices between pairs of 
k-clusterings obtained from the perturbed data. The similarity matrix given as input can be obtained from the functions
do.similarity.resampling, do.similarity.projection, do.similarity.noise. 
A list of p-values, sorted by descending order, from the most significant
clustering to the least significant is given according to a test based on Bernstein inequality. 
The test is based on the distribution of the similarity measures between pairs of clustering performed on perturbed data,
but differently from the chi-square based test (see \code{\link{Chi.square.compute.pvalues}}), no assumptions are made
about the "a priori" distribution of the similarity measures.
The function \code{Bernstein.ind.compute.pvalues} assumes also that the the random variables represented by the means
of the similarities between pairs of clusterings are independent, while, on the contrary, 
the function \code{Bernstein.compute.pvalues} no assumptions are made.
Low p-value mean that there is a significant difference between the 
top sorted and the given clustering. Please, see the papers cited in the reference section for more technical details.
}
\value{
  a list with 4 components:
  \item{ordered.clusterings }{a vector with the number of clusters ordered from the most significant to the least significant}
  \item{p.value }{a vector with the corresponding p-values computed according to Bernstein inequality and Bonferroni correction
          in descending order (theri values correspond to the clusterings of the vector ordered.clusterings)}
	\item{means }{vector with the mean similarity for each clustering}
  \item{variance }{vector with the variance of the similarity for each clustering}
}
\references{ 
W. Hoeffding, Probability inequalities for sums of independent random variables, J. Amer. Statist. Assoc. vol.58 pp. 13-30, 1963.

A.Bertoni, G. Valentini, Discovering significant structures in clustered data through Bernstein inequality, 
             CISI '06, Conferenza Italiana Sistemi Intelligenti, Ancona, Italia, 2006.
}
\author{ Giorgio Valentini \email{valentini@di.unimi.it}}
\seealso{  \code{\link{Chi.square.compute.pvalues}}, \code{\link{Hypothesis.testing}},

\code{\link{do.similarity.resampling}}, \code{\link{do.similarity.projection}}, \code{\link{do.similarity.noise}} }
\examples{
# Computation of the p-values according to Bernstein inequality using 
# resampling techniques and a hierarchical clustering algorithm
M <- generate.sample.h2 (n=20, l=10, Delta.h=4, Delta.v=2, sd=0.15);
S.HC <- do.similarity.resampling (M, c=15, nsub=20, f=0.8, s=sFM, 
                           alg.clust.sim=Hierarchical.sim.resampling);
# Bernstein test with no assumption of independence
Bernstein.compute.pvalues(S.HC)
# Bernstein test with  assumption of independence
Bernstein.ind.compute.pvalues(S.HC)
}
\keyword{ cluster }
\keyword{ htest }
