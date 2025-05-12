\name{Similarity.measures}
\alias{sFM}
\alias{sJaccard}
\alias{sM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Similarity measures between pairs of clusterings }
\description{
  Classical similarity measures between pairs of clusterings are implememted. These measures use the pairwise boolean membership matrix 
	(\code{\link{Do.boolean.membership.matrix}}) to compute the similarity between two clusterings, using the matrix as a vector and computing
	the result as an internal product. It may be shown that the same result may be obtained using contingency matrices and the classical
	definition of Fowlkes and Mallows (implemented with the function \code{sFM}), Jaccard (implemented with the function \code{sJaccard}) 
	and Matching (Rand Index, implemented with the function \code{sM}) coefficients.
	Their values range from 0 to 1 (0 no similarity, 1 identity).
}
\usage{
sFM(M1, M2)
sJaccard(M1, M2)
sM(M1, M2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M1}{ boolean membership matrix representing the first clustering }
  \item{M2}{ boolean membership matrix representing the second clustering }
}
\value{
  similarity measure between the two clusterings according to Fowlkes and Mallows (\code{sFM}), Jaccard (\code{sJaccard}) and
	Matching (\code{sM}) coefficients.
}
\references{ 
Ben-Hur, A. Ellisseeff, A. and Guyon, I., A stability based method for discovering structure in clustered data,
   In: "Pacific Symposium on Biocomputing", Altman, R.B. et al (eds.), pp, 6-17, 2002.}
\author{ Giorgio Valentini \email{valentini@di.unimi.it}}
\seealso{ \code{\link{Do.boolean.membership.matrix}} }
\examples{
# Synthetic data set generation (3 clusters with 20 examples for each cluster)
M <- generate.sample3(n=20, m=2)
# k-means clustering with 3 clusters
r1<-kmeans(t(M), c=3, iter.max = 1000);
# this function is implemented in the clusterv package:
cl1 <- Transform.vector.to.list(r1$cluster); 
# generation of a boolean membership square matrix:
Bkmeans <- Do.boolean.membership.matrix(cl1, 60, 1:60)
# the same as above, using PAM clustering with 3 clusters
d <- dist (t(M));
r2 <- pam (d,3,cluster.only=TRUE);
cl2 <- Transform.vector.to.list(r2);
BPAM <- Do.boolean.membership.matrix(cl2, 60, 1:60)
# computation of the Fowlkes and Mallows index beween the k-means and the PAM clustering:
sFM(Bkmeans, BPAM)
# computation of the Jaccard index beween the k-means and the PAM clustering:
sJaccard(Bkmeans, BPAM)
# computation of the Matching coefficient beween the k-means and the PAM clustering:
sM(Bkmeans, BPAM)
}
\keyword{ cluster }


