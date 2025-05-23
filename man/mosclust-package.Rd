\name{mosclust-package}
\alias{mosclust-package}
\alias{mosclust}
\docType{package}
\title{
Model order selection for clustering
}
\description{
The \emph{mosclust} R package (that stands for \bold{m}odel \bold{o}rder \bold{s}election for \bold{clust}ering problems)
implements a set of functions to discover significant structures in bio-molecular data.
	Using multiple perturbations of the data the stability of clustering solutions is assessed. Different
	perturbations may be used: resampling techniques, random projections and noise injection. Stability measures
	for the estimate of clustering solutions and statistical tests to assess their significance are provided.
}
\details{
\tabular{ll}{
Package: \tab mosclust\cr
Type: \tab Package\cr
Version: \tab 1.0.2\cr
Date: \tab 2006-09-08\cr
License: \tab GPL (>= 2) \cr
}


Recently, several methods based on the concept of stability have been proposed to estimate the "optimal" number of clusters in
complex bio-molecular data. In this conceptual framework multiple clusterings are obtained by introducing
perturbations into the original data, and a clustering is considered reliable if it is approximately maintained across multiple
perturbations.

Several perturbation techniques have been proposed, ranging form bootstrap techniques, to random projections to lower dimensional
subspaces to noise injection procedures. All these perturbation techniques are implemented in \emph{mosclust}.

The library implements indices of stability/reliability of the clusterings based on the distribution of similarity measures between multiple instances of
clusterings performed on multiple instances of data obtained through a given random perturbation of the original data.

These indices provides a "score" that can be used to compare the reliability of different clusterings. 
Moreover statistical tests based on \eqn{\chi^2} and on the classical Bernstein inequality are implemented in order to assess the statistical
significance of the discovered clustering solutions. By this approach we could also find multiple structures simultaneously present in the data. For instance,
it is possible that data exhibit a hierarchial structure, with subclusters inside other clusters, and using the indices and the statistical tests
implemented in \emph{mosclust} we may detect them at a given significance level.

Summarizing, this package may be used for:
\itemize{ 
\item Assessment of the reliability of a given clustering solution
\item Clustering model order selection: what about the "natural" number of clusters inside the data?
\item Assessment of the statistical significance of a given clustering solution
\item Discovery of multiple structures underlying the data: are there multiple reliable clustering solutions at a given significance level?
} 

The statistical tests implemented in the package have been designed with the theoretical and methodological contribution of
\emph{Alberto Bertoni} (DSI, Università degli Studi di Milano).
}

\author{
Giorgio Valentini <valentini@di.unimi.it>
}
\references{

Bittner, M. et al., Molecular classification of malignant melanoma by gene expression profiling, Nature, 406:536--540, 2000.

Monti, S.,  Tamayo P.,  Mesirov J. and Golub T., Consensus Clustering: A Resampling-based Method for Class Discovery and Visualization of Gene
	 Expression Microarray Data, Machine Learning, 52:91--118, 2003.
	 
Dudoit S. and Fridlyand J., A prediction-based resampling method for estimating the number of clusters in a dataset, Genome Biology,
3(7): 1-21, 2002.

Kerr M.K. and Curchill G.A.,Bootstrapping cluster analysis: assessing the reliability of conclusions from microarray experiments,
      PNAS, 98:8961--8965, 2001.
 
McShane, L.M., Radmacher, D., Freidlin, B., Yu, R.,  Li, M.C. and Simon, R.,
    Method for assessing reproducibility of clustering patterns observed in analyses of microarray data,
    Bioinformatics, 11(8), pp. 1462-1469, 2002.
 
Ben-Hur, A. Ellisseeff, A. and Guyon, I., A stability based method for discovering structure in clustered data,
   In: "Pacific Symposium on Biocomputing", Altman, R.B. et al (eds.), pp, 6-17, 2002.
	 
Smolkin M. and Gosh D., Cluster stability scores for microarray data in cancer studies, BMC Bioinformatics,
    36(4), 2003.
		
W. Hoeffding, Probability inequalities for sums of independent random variables, J. Amer. Statist. Assoc. vol.58 pp. 13-30, 1963.
		
A.Bertoni, G. Valentini, Randomized maps for assessing the reliability of patients clusters in DNA microarray data analyses, 
 Artificial Intelligence in Medicine 37(2):85-109  2006
 
A.Bertoni, G. Valentini, Model order selection for clustered bio-molecular data,  
In: Probabilistic Modeling and Machine Learning in Structural and Systems Biology, J. Rousu, S. Kaski and E. Ukkonen (Eds.), 
Tuusula, Finland, 17-18 June,  2006

A.Bertoni, G. Valentini, Discovering significant structures in clustered data through Bernstein inequality, 
             CISI '06, Conferenza Italiana Sistemi Intelligenti, Ancona, Italia, 2006.
						 
G. Valentini, Clusterv: a tool for assessing the reliability of clusters discovered in DNA microarray data, Bioinformatics,
   22(3):369-370, 2006.
}
\keyword{ package }
\keyword{ cluster }
\keyword{ htest }
\seealso{
\emph{clusterv}
}
