\name{Intersect}
\alias{Intersect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to compute the intersection between elements of two vectors }
\description{
  Having as input two sets of elements represented by two vectors, the intersection between the two sets is performed and
	the corresponding vector is returned.
}
\usage{
Intersect(sub1, sub2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sub1}{ first vector representing the first set }
  \item{sub2}{ second vector representing the second set }
}
\value{
 vector that stores the elements common to the two input vectors
}
\author{ Giorgio Valentini \email{valentini@di.unimi.it}}
\examples{
# Intesection between two sets of elements represented by vectors
s1 <- 1:10;
s2 <- 3:12;
Intersect(s1, s2)
}
\keyword{ cluster }
