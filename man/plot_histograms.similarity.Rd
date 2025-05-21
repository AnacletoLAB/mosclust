\name{plot_histograms.similarity}
\alias{plot_multiple.hist.similarity}
\alias{plot_hist.similarity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plotting histograms of similarity measures between clusterings }
\description{
  These functions plot histograms of a set of similarity measures obtained through perturbation methods.
	In particular \code{plot_hist.similarity} plots a single histogram referred to a specific number of clusters,
	while \code{plot_multiple.hist.similarity} plots multiple histograms referred to different numbers of clusters
	(one for each number of clusters, i.e. one for each row of the matrix \eqn{S} of similarity values).
}
\usage{
plot_hist.similarity(sim, nbins = 25)

plot_multiple.hist.similarity(S, n.col = 3, labels = NULL, nbins = 25)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sim}{ vector of similarity values }
  \item{nbins}{ number of the bins of the histogram }
	\item{S}{ Matrix of similarity values, rows correspond to diferent number of clusters }
  \item{n.col}{ number of columns in the grid of the histograms (default = 3) }
  \item{labels}{ label of the histograms. If NULL (default) the number of clusters from 2 to nrow(S)+1 are used }
}
\value{
  No return value, the function is called for its side-effect of
drawing a plot on the current graphics device.
}
\author{ Giorgio Valentini \email{valentini@di.unimi.it}}
\seealso{ \code{\link{plot_cumulative}},  \code{\link{plot_cumulative.multiple}}}
\examples{
library("clusterv")
# Data set generation
M <- generate.sample6 (n=20, m=10, dim=1000, d=3, s=0.2);
# generation of multiple similarity measures by resampling
Sr.kmeans.sample6 <- do.similarity.resampling(M, c=10, nsub=20, f=0.8, s=sFM, 
                                      alg.clust.sim=Kmeans.sim.resampling); 
# plot of the histograms of similarity measures for clusterings from 2 to 10 clusters:
plot_multiple.hist.similarity (Sr.kmeans.sample6, n.col=3, labels=NULL, nbins=25);
# the same as postrcript file
postscript(file="histograms.eps", horizontal=FALSE, onefile = FALSE);
plot_multiple.hist.similarity (Sr.kmeans.sample6, n.col=3, labels=NULL, nbins=25);
dev.off();
unlink("histograms.eps");
# plot of a single histogram
plot_hist.similarity(Sr.kmeans.sample6[2,], nbins = 25)
}
\keyword{ cluster }
