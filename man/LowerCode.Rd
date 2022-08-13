\name{LowerCode}
\alias{LowerCode}
\title{
Convert to Lower Case
}
\description{
Converts a \acronym{BASIC} program to lower case, except for any string
literals.
}
\usage{
LowerCode(p)
}
\arguments{
  \item{p}{
A \acronym{BASIC} program listing, as a character vector.
}
}
\value{
Returns the converted listing, as a character vector.
}
\seealso{
\code{\link{UpperCode}}
}
\examples{
# Convert a program to lower case.
LowerCode(c('10 LET X=X+1', '20 PRINT "Hello, World!"'))
}
