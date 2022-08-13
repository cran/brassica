\name{UpperCode}
\alias{UpperCode}
\title{
Convert to Upper Case
}
\description{
Converts a \acronym{BASIC} program to upper case, except for any string
literals.
}
\usage{
UpperCode(p)
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
\code{\link{LowerCode}}
}
\examples{
# Convert a program to upper case.
UpperCode(c('10 let x=x+1', '20 print "Hello, World!"'))
}
