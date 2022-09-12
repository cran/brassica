# BASIC programmer's development tools.
# MJL @ Titirangi, 11 August 2022.
# Last edit: 11 September 2022.

################################################################################
# Program listing / checking.

LIST <- function()
{
  # Lists the entire program as a character vector.
  ProgramListing()
}

NotRun <- function(pretty = TRUE)
{
  # Returns a data frame of unchecked (not wholly checked) lines, by file line
  # number and BASIC line number. If the pretty argument is TRUE, the frame is
  # returned invisibly after printing a formatted version to standard output.
  # The unchecked file line numbers, u, are monotonically increasing.
  u <- UncheckedFileLineNumbers()
  i <- UncheckedLineNumbers()
  s <- UncheckedLines()
  a <- data.frame(FileLine = u, Line = i, Listing = s)
  if (!pretty) return(a)
  n <- length(u)
  if (n == 0L) return(cat("Ok\n"))
  i <- floor(log10(u[n])) - floor(log10(u))
  cat(paste0(strrep(" ", i), "[", u, "] ", s, "\n"), sep = "")
  cat("(", n, " unchecked line", strrep("s", sign(n - 1L)), ")\n", sep = "")
  invisible(a)
}

################################################################################
# Case conversion.

ToCase <- function(p, f)
{
  # Apply function f to program listing p, excepting it's string literals.
  h <- gregexpr("\"", p, fixed = TRUE)
  for (i in seq_along(p))
  {
    j <- h[[i]]
    p[i] <- if (!any(j > 0L)) f(p[i]) else
            {
              j <- c(-1L, j[j > 0L], nchar(p[i]) + 1L)
              a <- substring(p[i], j[-length(j)], j[-1L] - 1L)
              a[c(TRUE, FALSE)] <- f(a[c(TRUE, FALSE)])
              paste0(a, collapse = "")
            }
  }
  p
}

LowerCode <- function(p)
{
  # Convert program listing p to lower case, excepting its string literals.
  ToCase(p, tolower)
}

UpperCode <- function(p)
{
  # Convert program listing p to upper case, excepting its string literals.
  ToCase(p, toupper)
}

############################################################################ EOF
