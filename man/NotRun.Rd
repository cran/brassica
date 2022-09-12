\name{NotRun}
\alias{NotRun}
\title{
Check BASIC Programs
}
\description{
Returns a data frame of any \acronym{BASIC} program lines that contain one or
more unexecuted statements.
Used with repeated calls of \code{RUN()}, this provides a test of code syntax
and accessibility.
}
\usage{
NotRun(pretty = TRUE)
}
\arguments{
  \item{pretty}{
A single logical value.
If \code{TRUE} (the default), formatted output is printed before the data frame
is returned invisibly.
If \code{FALSE}, the data frame is returned normally (visibly), without any
other output being printed.
}
}
\value{
Returns a data frame of any lines, from the currently-loaded \acronym{BASIC}
program, containing one or more unexecuted statements.
Lines are listed with both their \acronym{BASIC} line numbers and the
corresponding file line numbers of the source script.
A persistent appearance here may, but does not necessarily, signify a problem
with the line.
}
\note{
\acronym{BASIC} \code{ON} and \code{IF-THEN-ELSE} statements contain multiple
alternative clauses.
Each such statement, as a whole, is marked as having been executed once any one
of its clauses has been run without error.
Hence, the absence of such a statement from the \code{NotRun()} frame does not
guarantee that all of its clauses are error-free.
}
\seealso{
\code{\link{RUN}}
}
\examples{
# Peruse which lines have not yet been run.
NotRun()
}
