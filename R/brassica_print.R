# Evaluation and printing of PRINT statements (and INPUT prompts).
# MJL @ Titirangi, 11 August 2022.
# Last edit: 4 October 2022.

################################################################################
# Term separators. These symbols delimit individual PRINT terms, expand to
# varying amounts of whitespace when printed, and provide line-continuation by
# instructing BASIC not to append a newline.

separators <- c(";", ",")

################################################################################
# Formatting constants (tab width, floating point precision).

printZoneWidth <- 14L
printPrecision <- 6L

################################################################################
# Top-level print evaluators.

EvalPrintStatement <- function(s)
{
  # Evaluates, formats and prints each term of PRINT statement s.
  if (!nzchar(s)) return(PrintBufferAndNewline())
  a <- list()
  while (Running() && nzchar(s))
  {
    p <- EvalFirstPrintTerm(s)
    if (Terminated()) return(PrintFinalTerm(a))
    a <- append(a, p[-length(p)])
    s <- unname(p[[length(p)]])
    if (InteriorTermHasEnded(a[length(a)], s)) a <- AppendInteriorTerm(a)
  }
  if (Running()) PrintFinalTerm(a)
}

EvalFirstPrintTerm <- function(s)
{
  # Delegates the evaluation of the BASIC PRINT expression in string s to a
  # handler specific to the leading (left-most) term in that expression. Returns
  # the result of evaluating that first term in a list along with any remaining
  # unevaluated portion of the expression.
  if (BeginsWith("\"", s)) return(EvalString(s))
  if (BeginsWithAny(LETTERS, s)) return(EvalName(s))
  if (BeginsWithAny(separators, s)) return(IsolateSeparator(s))
  if (BeginsWith("(", s)) return(EvalGroup(s))
  if (BeginsWithAny(operators, s)) return(IsolateOperator(s))
  if (BeginsWithAny(digits, s)) return(EvalNumber(s))
  Interrupt(SetSyntaxError())
}

################################################################################
# Evaluation assistants.

AppendInteriorTerm <- function(a)
{
  # Evaluates a single PRINT term, formats the result for printing, and appends
  # it to the print buffer. List a comprises the term as a collection of BASIC
  # values and operators, followed by a separator. The collection may be empty,
  # or the separator absent (implied), but not both (list a cannot be empty).
  n <- length(a)
  s <- IsSeparator(a[n])
  if (n - s > 0L)
  {
    b <- if (s) a[-n] else a
    if (IsUnevaluated(b)) b <- EvalUnaryGroups(b)
    if (IsUnevaluated(b)) b <- ApplyBinaryOperators(b)
    if (IsUnevaluated(b)) return(SetSyntaxError())
    AppendToPrintBuffer(FormatPrintable(b))
  }
  if (s) AppendToPrintBuffer(FormatSeparator(a[n]))
  list()
}

FormatPrintable <- function(x)
{
  # Formats a BASIC string or number, x, to a print-ready R string.
  if (IsString(x)) return(Value(x))
  n <- toupper(format(Value(x), digits = printPrecision, decimal.mark = "."))
  if (BeginsWith("0.", n)) n <- substring(n, 2L) else
  if (BeginsWith("-0.", n)) n <- paste0("-", substring(n, 3L))
  paste0(strrep(" ", !BeginsWith("-", n)), n, " ")
}

FormatSeparator <- function(x)
{
  # Converts separation operator x to some number of spaces for printing. This
  # is always zero when using SkipToPrintZone() for comma separators. The
  # alternative function, SpaceToFillPrintZone(), produces variable whitespace,
  # which is subject to teletypewriter character printing delays.
  if (Value(x) == ";") "" else SkipToPrintZone()
}

InteriorTermHasEnded <- function(a, s)
{
  # Returns TRUE on reaching the end of an interior PRINT term. This occurs when
  # the last component of the current term, a, is a separator, a number followed
  # by something other than a binary operator, or a string followed by something
  # other than a string operator. When a is not a separator, and the remaining
  # unevaluated statement, s, is empty, this is a final term (not an interior
  # term). We have to look ahead on s, instead of just evaluating the next term,
  # in case it calls POS(1) (with the current term not yet committed to the
  # buffer).
  if (IsSeparator(a)) return(TRUE)
  if (!nzchar(s)) return(FALSE)
  if (IsString(a)) return(!BeginsWithAny(stringOps, s))
  if (IsNumber(a)) return(!BeginsWithAny(binaries, s))
  FALSE
}

IsolateSeparator <- function(s)
{
  # Detaches a separation operator from the beginning of statement s.
  # The operator is not applied at this time.
  o <- BeginsWithWhich(separators, s)
  Intermediate(AsOperator(o), substring(s, nchar(o) + 1L))
}

IsSeparator <- function(x)
{
  # Returns TRUE if x is a separation operator. Returns FALSE otherwise.
  IsOperator(x) && any(Value(x) == separators, na.rm = TRUE)
}

PrintFinalTerm <- function(a)
{
  # Evaluates a single PRINT term, formats the result for printing, and appends
  # it to the print buffer. List a contains the term as a collection of BASIC
  # data values and operators. The list can be empty, but cannot end with a
  # term separator (comma or semicolon).
  if (length(a) < 1L) return(NULL)
  if (IsUnevaluated(a)) a <- EvalUnaryGroups(a)
  if (IsUnevaluated(a)) a <- ApplyBinaryOperators(a)
  if (IsUnevaluated(a)) return(SetSyntaxError())
  AppendToPrintBuffer(FormatPrintable(a))
  PrintBufferAndNewline()
}

SkipToPrintZone <- function()
{
  # Rapidly advances the cursor to the beginning of the next print zone (tab
  # stop, without teletype print-speed restrictions). Inaccurate when the print
  # buffer contains special or non-printing characters.
  p <- VirtualCursorPosition()
  s <- printZoneWidth - p %% printZoneWidth
  w <- TerminalWidth()
  if ((p + s) < w) AdvancePrintHead(s) else PrintBufferAndNewline()
  ""
}

SpaceToFillPrintZone <- function()
{
  # Returns a character string of however many spaces are required to advance
  # the cursor to the first column of the next print zone (i.e., tab stop).
  # Inaccurate if the print buffer contains special or non-printing characters.
  # These spaces are subject to teletype delays, and this function has been
  # replaced with SkipToPrintZone(). We retain it, in case we want those delays.
  p <- VirtualCursorPosition()
  w <- printZoneWidth - p %% printZoneWidth
  if ((p + w) < TerminalWidth()) return(strrep(" ", w))
  PrintBufferAndNewline()
  ""
}

################################################################################
# Printing utilities.

AdvancePrintHead <- function(n)
{
  # Rapidly advances the print head n spaces; in a single movement and without
  # actually printing anything (i.e., without teletype effects). This has no
  # protection against overshooting the console; wrapping is to be done before
  # calling this function. When printed material on the line extends beyond the
  # current cursor position, we have to re-cat it, so as not to overprint it.
  PrintNonEmptyBuffer()
  k <- GetCursorPosition()
  p <- GetCharactersOnLine()
  z <- substring(p, k + 1L, k + n)
  z <- paste0(z, strrep(" ", max(0L, n - nchar(z))))
  UpdateCharactersOnLine(z)
  cat(z, sep = "")
}

Print <- function(s)
{
  # Concatenates character vector s to standard output.
  # In teletype mode, this goes one character at a time, pausing after each.
  p <- CharacterPause()
  z <- paste0(if (UpperCaseOut()) toupper(s) else s, collapse = "")
  UpdateCharactersOnLine(z)
  if (p == 0) return(cat(z, sep = ""))
  z <- strsplit(z, "", fixed = TRUE)[[1L]]
  for (x in z)
  {
    cat(x, sep = "")
    flush.console()
    Sys.sleep(p)
  }
}

PrintBufferAndNewline <- function()
{
  # Prints the print buffer, empty or not, followed by a newline.
  PrintPrintBuffer()
  PrintNewLine()
}

PrintCarriageReturn <- function()
{
  # Prints a carriage return. Pauses afterward, if in teletype mode.
  cat("\r")
  p <- LinePause()
  if (p != 0) flush.console()
  if (p > 0) Sys.sleep(p)
  ResetCursorPosition()
}

PrintExitMessage <- function()
{
  # Prints the exit message, then immediately deletes it from program memory.
  # For use at the very end of a RUN(), after a program has terminated.
  m <- GetExitMessage()
  if (any(nzchar(m))) {Print(m); PrintNewLine()}
  DelPro(exitTrigger)
}

PrintFinalBuffer <- function()
{
  # Prints anything sitting in the print buffer, then prints a new line if the
  # current line is not empty (these two actions are independent). Called on
  # termination of a BASIC program, just before printing any exit message.
  PrintNonEmptyBuffer()
  if (nzchar(GetCharactersOnLine())) PrintNewLine()
}

PrintNewLine <- function()
{
  # Prints a newline. Pauses afterward, if in teletype mode.
  cat("\n")
  p <- LinePause()
  if (p != 0) flush.console()
  if (p > 0) {Sys.sleep(p)}
  ResetCursorPosition()
  SetNoCharactersOnLine()
}

PrintNonEmptyBuffer <- function()
{
  # Prints the print buffer (without newline), only if the buffer is non-empty.
  if (AnythingInPrintBuffer()) PrintPrintBuffer()
}

PrintNonEmptyLine <- function()
{
  # Prints the print buffer and a newline, only if the buffer is non-empty.
  if (AnythingInPrintBuffer()) PrintBufferAndNewline()
}

PrintPrintBuffer <- function()
{
  # Sends the print buffer (even when empty) to standard output, and immediately
  # deletes its content (after printing).
  Print(GetPrintBuffer())
  ClearPrintBuffer()
}

RetractPrintHead <- function(n)
{
  # Rapidly retracts the print head n spaces toward, but not beyond, the left
  # margin, without teletype effects. We do this the long way, via a carriage
  # return and AdvancePrintHead(), so that R's cursor position is in agreement.
  PrintNonEmptyBuffer()
  k <- GetCursorPosition()
  cat("\r")
  ResetCursorPosition()
  AdvancePrintHead(max(0L, k - n))
}

################################################################################
# Seek user input.

PromptForInput <- function(p)
{
  # Present the user with an input prompt, '?', an addenda prompt, '??', or no
  # prompt, '', as specified by integer p, and accept one line of input.

  a <- switch(p, "? ", "?? ")

  # When a teletype character delay is in effect, we print the buffer first, so
  # that the effect is applied. In some R sessions (*) this may result in the
  # input prompt appearing on the next line (i.e., after a newline), or at the
  # beginning of the current line (with user input overprinting the prompt).
  # While unaesthetic, this is not fatal.
  # (*) Affected: Windows terminal (R & Rterm; but the Rgui console is fine),
  # Mac console (but the terminal is fine; opposite to Windows), RStudio.
  if (CharacterPause() > 0)
  {
    PrintPrintBuffer()
    Print(a)
    a <- trimws(readline())

  # When no teletype character delay is in effect (and need not be applied),
  # we print the buffer via readline's prompt argument. This works in all R
  # sessions (the input prompt appears where we want it), although the prompt
  # will be coloured in some.
  } else
  {
    a <- paste0(c(GetPrintBuffer(), a), collapse = "")
    a <- trimws(readline(if (UpperCaseOut()) toupper(a) else a))
    ClearPrintBuffer()
  }

  ResetCursorPosition()
  SetNoCharactersOnLine()
  a
}

IssueRedoFromStartWarning <- function()
{
  # Advise the user of an issue with their input (too much or wrong type; too
  # little merely results in the addenda prompt), and that we're starting over.
  Print("? Redo from start")
  PrintNewLine()
}

############################################################################ EOF
