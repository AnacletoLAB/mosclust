\name{plot_cumulative}
\alias{plot_cumulative}
\alias{plot_cumulative.multiple}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to plot the empirical cumulative distribution function of the similarity values }
\description{
  The function \code{plot_cumulative} plots the ecdf of the similarity values between pairs of clusterings for a specific number of clusters.
  The function \code{plot_cumulative.multiple} plots the graphs of the empirical cumulative distributions corresponding to different number of clusters, 
	using different patterns and/or different colors for each graph. Up to 15  ecdf graphs can be plotted simultaneously.
}
\usage{
plot_cumulative(F)

plot_cumulative.multiple(list.F, labels = NULL, min.x = -1, colors = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{F}{ Function of class ecdf  that stores the discrete values of the cumulative distribution }
	\item{list.F}{ a list of function of class ecdf }
  \item{labels}{ vector of the labels associated to the CDF. If  NULL (default), then a vector of labels from 2 to lenght(list.F)+1
is used.}
  \item{min.x}{ minimum value to be plotted for similarities. If -1 (default) the minimum of the similarity value is obtained
from list.F }
  \item{colors}{ if TRUE (default) different colors are used to plot the different ECDF, otherwise black lines are used }
}
\author{ Giorgio Valentini \email{valenti@dsi.unimi.it}}
\seealso{  \code{\link{compute.cumulative.multiple}}}
\examples{
# Data set generation
M <- generate.sample6 (n=20, m=10, dim=1000, d=3, s=0.2);
# generation of multiple similarity measures by resampling
Sr.kmeans.sample6 <- do.similarity.resampling(M, c=10, nsub=20, f=0.8, s=sFM, 
                                      alg.clust.sim=Kmeans.sim.resampling); 
# computation of multiple ecdf (from 2 to 10 clusters)
list.F <- compute.cumulative.multiple(Sr.kmeans.sample6);
# values of the ecdf for 8 clusters 
l <- cumulative.values(list.F[[7]])
# plot of the ecdf for 8 clusters
plot_cumulative(list.F[[7]])
# plot of the empirical cumulative distributions from 2 to 10 clusters
plot_cumulative.multiple(list.F)
}
\keyword{ cluster }
