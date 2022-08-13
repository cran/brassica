\name{brassica-package}
\alias{brassica-package}
\alias{brassica}
\docType{package}
\title{
BASIC Interpreter
}
\description{
Runs 1970s \acronym{BASIC} programs.
}
\details{
Interprets and executes a subset of early (1975) Altair/Microsoft
\acronym{BASIC}, plus some generalisations and extensions.
Enables various programs of historical interest to be run in their primal form.
A selection of public-domain examples are included.
}
\author{
Mike Lee
}
\seealso{
\code{\link{RUN}}
}
\examples{
require(brassica)

\dontrun{
# Load and run the 'Wumpus' program.
RUN("WUMPUS")
}
}
\keyword{BASIC}
\keyword{interpreter}
\keyword{package}
