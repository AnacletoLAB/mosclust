\name{Bernstein.p.value}
\alias{Bernstein.p.value}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to compute the p-value according to Bernstein inequality. }
\description{
  The Bernstein inequality gives an upper bound to the probability that the means of two random variables differ by chance, considering also their variance.
	This function implements the Berstein inequality and it is used by the functions \code{Bernstein.compute.pvalues} and
\code{Bernstein.ind.compute.pvalues}
}
\usage{
Bernstein.p.value(n, Delta, v)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ number of observations of the random variable }
  \item{Delta}{ difference between the means }
  \item{v}{ variance of the random variable } 
}
\value{
  a real number that provides an upper bound to the probability that the two means differ by chance
}
\references{ 
W. Hoeffding, Probability inequalities for sums of independent random variables, J. Amer. Statist. Assoc. vol.58 pp. 13-30, 1963.
}
\author{ Giorgio Valentini \email{valenti@dsi.unimi.it}}
\seealso{  \code{\link{Bernstein.compute.pvalues}}, \code{\link{Bernstein.ind.compute.pvalues}}}
\examples{
# Computation of the upper bounds to the probability that the two means differ by chance
Bernstein.p.value(n=100, Delta=0.1, v=0.01)
Bernstein.p.value(n=100, Delta=0.05, v=0.01)
Bernstein.p.value(n=100, Delta=0.05, v=0.1)
Bernstein.p.value(n=1000, Delta=0.05, v=0.1)
}
\keyword{ cluster }
\keyword{ htest }
