\name{RUN}
\alias{RUN}
\title{
Run BASIC Programs
}
\description{
Loads and runs \acronym{BASIC} programs.
}
\usage{
RUN(program = NULL, ttx = 0, tty = 0, up = FALSE)
}
\arguments{
  \item{program}{
A character string, containing the name or file path of a \acronym{BASIC}
program.
Omitting this re-runs the currently-loaded program.
}
  \item{ttx}{
Aesthetic teletype option.
A delay, of not more than 200 milliseconds, to be applied after printing each
character.
In practice, the actual delay will be this time plus however long it takes to
flush the console, plus interpreter overheads.
Depending on the model, a real teletype could manage around ten characters per
second.
Making this negative imposes no delay, but flushes the console after each
completed line (not after each character).
}
  \item{tty}{
Aesthetic teletype option.
A delay, of not more than 2000 milliseconds, to be applied after each line of
printing.
Making this negative imposes no delay, but still flushes the console after
each completed line.
}
  \item{up}{
Aesthetic teletype option.
Making this \code{TRUE} forces all \acronym{BASIC} output to upper case.
}
}
\value{
Returns an invisible \code{NULL}, after printing to standard output.
}
\details{
List of bundled example programs (from the references):
\tabular{llr}{\emph{Animal} \tab by Arthur Luehrmann \tab (decision trees)
\cr\emph{Camel} \tab by the Heath Users Group \tab (random walk)
\cr\emph{Chase} \tab by Mac Oglesby \tab (robots)
\cr\emph{Eliza} \tab by Jeff Shrager \tab (language processing)
\cr\emph{Even Wins} \tab by Eric Peters \tab (machine learning)
\cr\emph{Flip} \tab by John S.{} James \tab (machine learning)
\cr\emph{Four in a Row} \tab by James L.{} Murphy \tab (artificial intelligence)
\cr\emph{Guess-It} \tab by Gerard Kierman \tab (bluffing)
\cr\emph{Hammurabi} \tab by David H.{} Ahl \tab (resource management)
\cr\emph{Hexapawn} \tab by R.{} A.{} Kaapke \tab (machine learning)
\cr\emph{Inkblot} \tab by Scott Costello \tab (projection)
\cr\emph{Life} \tab by Clark Baker \tab (cellular automata)
\cr\emph{Maze} \tab by Richard Schaal \tab (depth-first search)
\cr\emph{Not One} \tab by Robert Puopolo \tab (expectation values)
\cr\emph{Sea Battle} \tab by Vincent Erickson \tab (monsters)
\cr\emph{Super Star Trek} \tab by Richard Leedon \tab (vector space)
\cr\emph{Wumpus} \tab by Gregory Yob \tab (graph topology)}
All of these are in the public domain.
}
\note{
Many \acronym{BASIC} programs ask for user input after printing a custom prompt.
When \code{ttx} is greater than zero, some \R sessions may incorrectly position
the cursor at the beginning of the line, or on the next.
Neither occurrence is fatal.
Known to be affected: Windows \R terminal (\file{R} and \file{Rterm}, but the
\file{Rgui} console is fine), Mac \R console (but the Bash terminal is fine),
RStudio.
Additionally, double buffering, or the like, may interfere with the effect at
higher speeds (Mac \R console, again).
}
\references{
\enumerate{
\item David H.{} Ahl & Steve North, \emph{BASIC Computer Games} (1978)
\item David H.{} Ahl & Steve North, \emph{More BASIC Computer Games} (1979)
}
}
\seealso{
\code{\link{LIST}}
}
\examples{
\dontrun{
# Load and run 'Wumpus'.
RUN("wumpus")

# Load and run 'Chase', flushing the
# console after every line of output.
RUN("chase", -1)

# Load and run 'Camel', with full
# retro-style teletypewriter effects.
RUN("camel", 20, 300, TRUE)

# Load and run your program.
RUN("myprogram.bas")

# Re-run the last program
# (without re-loading it).
RUN()
}
}
