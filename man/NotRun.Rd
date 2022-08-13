\name{NotRun}
\alias{NotRun}
\title{
Check BASIC Programs
}
\description{
Returns a data frame of any \acronym{BASIC} program lines that have not yet been
run.
Used with repeated calls of \code{RUN()}, this provides a test of code syntax
and accessibility.
}
\usage{
NotRun(pretty = FALSE)
}
\arguments{
  \item{pretty}{
A single logical value.
If \code{TRUE}, formatted output is printed before the data frame is returned
invisibly.
If \code{FALSE} (the default), the data frame is returned normally (visibly),
without any other output being printed.
}
}
\value{
Returns a data frame of any lines from the currently-loaded \acronym{BASIC}
program that have not been successfully run in their entirety.
Lines are listed with both their \acronym{BASIC} line numbers and the
corresponding file line numbers of the source script.
An appearance here might signify a problem with the line, or simply that one or
more statements on it have not yet been executed.
}
\seealso{
\code{\link{RUN}}
}
\examples{
# Peruse which lines have not yet been run.
NotRun()
}
