\name{Do.boolean.membership.matrix}
\alias{Do.boolean.membership.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to compute and build up a pairwise boolean membership matrix. }
\description{
  It computes the pairwise membership matrix for a given clustering. The number of rows is equal to the number of columns (the number of examples).
	The element \eqn{m_{ij}} is set to 1 if the examples \eqn{i} and \eqn{j} belong to the same cluster, otherwise to 0. 
	This function may be used also with clusterings that do not define strictly a partition of the data and using
  diferent number of clusters for each clustering.
}
\usage{
Do.boolean.membership.matrix(cl, dim.M, examplelabels)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cl}{ a clustering (list of vectors defining a clustering) }
  \item{dim.M}{ dimension of the similarity matrix (number of examples) }
  \item{examplelabels}{ labels of the examples drawn from the original data }
}
\value{
 the pairwise boolean membership square matrix.
}
\author{ Giorgio Valentini \email{valentini@di.unimi.it}}
\examples{
# Synthetic data set generation (3 clusters with 20 examples for each cluster)
M <- generate.sample3(n=20, m=2)
# k-means clustering with 3 clusters
r<-kmeans(t(M), c=3, iter.max = 1000);
# this function is implemented in the clusterv package:
cl <- Transform.vector.to.list(r$cluster); 
# generation of boolean membership square matrix:
B <- Do.boolean.membership.matrix(cl, 60, 1:60)
}
\keyword{ cluster }

