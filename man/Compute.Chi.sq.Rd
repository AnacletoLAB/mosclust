\name{Compute.Chi.sq}
\alias{Compute.Chi.sq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to evaluate if a set of similarity distributions significantly differ using the chi square test. }
\description{
  The set of similarity values for a specific value of k (number of clusters) are subdivided in two groups
choosing a threshold for the similarity value (default 0.9). Then different sets are compared using the chi squared test for multiple proportions.
The number of degrees of freedom are equal to the number of the different sets minus 1.
This function is iteratively used by \code{Chi.square.compute.pvalues}.
}
\usage{
Compute.Chi.sq(M, s0 = 0.9)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M}{ matrix representing the similarity values for different number of clusters. Each row represents similarity values for a
number of clusters. Number of rows ==> how many numbers of clusters are considered; 
number of columns ==> cardinality of the similarity values for a given number of clusters }
  \item{s0}{ threshold for the similarity value (default 0.9) }
}
\value{
  p-value (type I error) associated with the null hypothesis (no difference between the considered set of k-clusterings)
}
\references{ 
A.Bertoni, G. Valentini, Model order selection for clustered bio-molecular data,  
In: Probabilistic Modeling and Machine Learning in Structural and Systems Biology, J. Rousu, S. Kaski and E. Ukkonen (Eds.), 
Tuusula, Finland, 17-18 June,  2006
}
\author{ Giorgio Valentini \email{valentini@dsi.unimi.it}}
\seealso{ \code{\link{Chi.square.compute.pvalues}}}
\examples{
library("clusterv")
# Synthetic data set generation
M <- generate.sample6 (n=10, m=15, dim=800, d=3, s=0.2)
# computing the similarity matrix using random projections and hierarchcial clustering
Sim <- do.similarity.projection(M, c=6, nprojections=20, dim=JL.predict.dim(60,epsilon=0.2))
# Evaluating the p-value for the group of the 5 clusterings (from 2 to 6 clusters)
Compute.Chi.sq(Sim)
# the same, considering only the clusterings wih 2 and 6 clusters:
Compute.Chi.sq(Sim[c(1,5),])
}
\keyword{ cluster }
\keyword{ htest }
