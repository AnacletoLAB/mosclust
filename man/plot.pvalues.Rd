\name{plot.pvalues}
\alias{plot.pvalues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to plot p-values for different tests of hypothesis }
\description{
  The p-values corresponding to different k-clusterings according to different hypothesis testing are plotted. A horizontal line corresponding to
a given alpha value (significance) is also plotted. In the x axis is represented the number of clusters sorted according to the value of the stability index,
and in the y axis the corresponding p-value.
In this way the results of different tests of hypothesis can be compared.
}
\usage{
plot.pvalues(l,alpha=1e-02,legendy=0, leg_label=NULL, colors=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{l}{a list of lists. Each component list represents a different test of hypothesis, and it has in turn 4 components: 		 
					 ordered.clusterings : a vector with the number of clusters ordered from the most significant to the least significant;
					  p.value : a vector with the corresponding p-values computed according to chi-square test between multiple proportions
            in descending order (their values correspond to the clusterings of the vector ordered.clusterings);
            means : vector with the mean similarity (stability index) for each clustering;
            variance : vector with the variance of the similarity for each clustering.}
  \item{alpha}{ alpha value for which the straight line is plotted }
  \item{legendy}{ ordinate of the legend. If 0 (def.) no legend is plotted. }
	\item{leg_label}{labels of the legend. If NULL (def.) the text "test 1, test 2, ... test n" for the n tests is printed.
                   Otherwise it is a vector of characters specifying the text to be printed}
	\item{colors}{if TRUE (def.) lines are printed with colors, otherwise using only different line pattern} 
}
\author{ Giorgio Valentini \email{valenti@dsi.unimi.it}}
\seealso{  \code{\link{plot.cumulative.multiple}}}
\examples{
# Data set generation
M <- generate.sample6 (n=20, m=10, dim=1000, d=3, s=0.2);
# generation of multiple similarity measures by resampling
Sr.kmeans.sample6 <- do.similarity.resampling(M, c=10, nsub=20, f=0.8, s=sFM, 
                                      alg.clust.sim=Kmeans.sim.resampling); 
# hypothesis testing using the chi-square based test
d.chi <- Chi.square.compute.pvalues(Sr.kmeans.sample6)
# hypothesis testing using the Bernstein based test
d.Bern <- Bernstein.compute.pvalues(Sr.kmeans.sample6)
# hypothesis testing using the Bernstein based test (with independence assumption)
d.Bern.ind <- Bernstein.ind.compute.pvalues(Sr.kmeans.sample6)
l <- list(d.chi, d.Bern, d.Bern.ind);
# plot of the corresponding computed p-values
plot.pvalues(l, alpha = 1e-05, legendy = 1e-12)
}
\keyword{ cluster }
\keyword{ htest }

