\name{PAM.sim.projection}
\alias{PAM.sim.projection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to compute similarity indices using random projections and PAM clustering. }
\description{
 A vector of similarity measures between pairs of clusterings perturbed with random projections is computed for a given number of clusters. 
	The dimension of the projected data, the type of randomized map and the similarity measure may be selected.
}
\usage{
PAM.sim.projection(X, c = 2, nprojections = 100, dim = 2, pmethod = "PMO", 
  scale = TRUE, seed = 100, s = sFM, distance = "euclidean", hmethod = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ matrix of data (variables are rows, examples columns) }
  \item{c}{ number of clusters }
  \item{nprojections}{ number of pairs of projected data }
  \item{dim}{ dimension of the projected data }
  \item{pmethod}{ projection method. It must be one of the following:  
									- "RS" (random subspace projection)
									- "PMO" (Plus Minus One random projection)
									- "Norm" (normal random projection)
									- "Achlioptas" (Achlioptas random projection)}
  \item{scale}{ if TRUE randomized projections are scaled (default) }
  \item{seed}{ numerical seed for the random generator }
  \item{s}{ similarity function to be used. It may be one of the following: 
                                - sFM (Fowlkes and Mallows)
                                - sJaccard (Jaccard)
                                - sM (matching coefficient)
                                  (default Fowlkes and Mallows) }
  \item{distance}{ it must be one of the two: "euclidean" (default) or "pearson" (that is 1 - Pearson correlation)}
  \item{hmethod}{ parameter used for internal compatibility. }
}
\value{
  vector of the computed similarity measures (length equal to nprojections)
}
\author{ Giorgio Valentini \email{valentini@di.unimi.it}}
\seealso{ \code{\link{PAM.sim.resampling}}, \code{\link{PAM.sim.noise}} }
\examples{
library("clusterv")
# Synthetic data set generation
M <- generate.sample6 (n=20, m=10, dim=600, d=3, s=0.2);
# computing a vector of similarity indices with 2 clusters:
v2 <- PAM.sim.projection(M, c = 2, nprojections = 20, dim = 200, 
                         pmethod = "PMO", s = sFM)
# computing a vector of similarity indices with 3 clusters:
v3 <- PAM.sim.projection(M, c = 3, nprojections = 20, dim = 200, 
                         pmethod = "PMO", s = sFM)
# computing a vector of similarity indices with 2 clusters using the Jaccard index
v2J <- PAM.sim.projection(M, c = 2, nprojections = 20, dim = 200, 
                          pmethod = "PMO", s = sJaccard)
}
\keyword{ cluster }
