# Implements the action of BASIC commands (beginning with a keyword).
# MJL @ Titirangi, 3 August 2022.
# Last edit: 13 September 2022.

################################################################################
# Recognised BASIC keywords (commands).

keywords <- c("CLEAR", "DATA", "DEF", "DELAY", "DIM", "ELSE", "END", "FN",
              "FOR", "GO", "GOSUB", "GOTO", "IF", "INPUT", "LET", "NEXT", "ON",
              "PRINT", "READ", "REM", "RESTORE", "RETURN", "STEP", "STOP",
              "THEN", "TO")

################################################################################
# Named components of composite data structures.

variable  <- "variable"
variables <- "variables"
remainder <- "remainder"

################################################################################
# Top-level statement processor.

Enact <- function(s)
{
  # Top-most BASIC-statement processing function. Identifies the primary command
  # keyword of statement s, and calls the appropriate method for handling it.
  if (Terminated()) return(Exit())
  a <- WhichAction(s)
  eval(call(paste0("Enact", a), Trim(s, if (a == "None") "" else a)))
}

################################################################################
# Utilities for the top-level processor.

Trim <- function(s, w)
{
  # Removes as many characters as are in word w from the beginning of string s.
  substring(s, nchar(w) + 1L)
}

WhichAction <- function(s)
{
  # Returns the leading BASIC keyword (command) of statement s, or 'None' in the
  # absence of such.
  if (length(k <- BeginsWithWhich(keywords, s)) != 0L) k else "None"
}

################################################################################
# Implied actions when no leading keyword is present.

EnactImpliedGOTO <- function(s)
{
  # A string of digits after THEN is an implied GOTO (that line number).
  EnactGOTO(s)
}

EnactImpliedLET <- function(s)
{
  # No keyword at the beginning of a non-blank statement is an implied LET.
  EnactLET(s)
}

EnactNone <- function(s)
{
  # No keyword at the beginning of a statement is either an implied LET
  # assignment (when the statement is non-blank), or simply a do-nothing (when
  # the statement is blank). Although supported here, blank statements are not
  # found in original BASIC.
  if (nzchar(s)) return(EnactImpliedLET(s))
  MarkStatementAsChecked()
  AdvanceToNextStatement()
}

################################################################################
# Actions for each BASIC command (keyword).

EnactCLEAR <- function(s)
{
  # Performs a CLEAR; deletes everything in heap memory (does not affect either
  # the loop or subroutine return-point stacks). In 8K Altair BASIC, CLEAR <N>
  # deletes everything in heap memory and allocates <N> (bytes?) for character
  # string storage. Here, we ignore <N>, so long as it is a positive number.
  if (nzchar(s))
  {
    n <- Eval(s)
    if (!IsNumber(n)) return(SetTypeMismatchError())
    if (Value(n) < 0) return(SetIllegalQuantityError())
  }
  ResetHeapMemory()
  MarkStatementAsChecked()
  AdvanceToNextStatement()
}

EnactDATA <- function(s)
{
  # Actions DATA statements at run-time (when they are are skipped-over).
  MarkStatementAsChecked()
  AdvanceToNextStatement()
}

EnactDEF <- function(s)
{
  # Actions DEF FN definition of custom user-functions. A function must have at
  # least one parameter variable, even if this is just a dummy. Here, we allow
  # multiple parameters of any data type (number, integer, or string). Many
  # original flavours of BASIC insisted on precisely one parameter, of number
  # type only. Parameters cannot have subscripts (no array elements allowed).
  # Here, functions may return numbers or strings, and must have the usual type
  # suffix, $, for the latter. Number-valued functions may possess the integer
  # suffix, %, in which case their output will be appropriately constrained.
  # Many original BASICs only support unconstrained number-valued definitions.
  if (!BeginsWith("FN", s)) return(SetSyntaxError())
  # DetachLeadingVariable() will raise an error on 'FN'.
  a <- DetachLeadingVariable(Trim(s, "FN"))
  v <- a[[variable]]
  if (!IsArray(v)) return(SetSyntaxError())
  # Restore the 'FN' (or 'fn', etc.) to the case-sensitive function name, n.
  n <- paste0(substring(s, 1L, 2L), NameOfVariable(v))
  p <- SubscriptsOfVariable(v)
  # Check all parameter names are singletons (no subscripted array-members).
  if (!all(grepl("[[:alpha:]][[:alnum:]]*[%$]{0,1}$", p)))
    return(SetSyntaxError())
  if (any(duplicated(p))) return(SetDuplicatedParameterError())
  s <- a[[remainder]]
  if (!BeginsWith("=", s)) return(SetSyntaxError())
  s <- substring(s, 2L)
  if (!nzchar(s)) return(SetSyntaxError())
  DefineFunction(n, p, s)
  MarkStatementAsChecked()
  AdvanceToNextStatement()
}

EnactDELAY <- function(s)
{
  # Actions DELAY commands, for pausing execution for some amount of time.
  n <- Eval(s)
  if (!IsNumber(n)) return(SetTypeMismatchError())
  if (Value(n) < 0) return(SetIllegalQuantityError())
  PrintPrintBuffer()
  flush.console()
  Sys.sleep(Value(n))
  MarkStatementAsChecked()
  AdvanceToNextStatement()
}

EnactDIM <- function(s)
{
  # Actions a DIM command, for dimensioning one or more arrays.
  a <- SplitArguments(s)
  if (Terminated()) return(Exit())
  for (b in a)
  {
    v <- DetachLeadingVariable(b)
    if (Terminated()) return(Exit())
    if (nzchar(v[[remainder]])) return(SetSyntaxError())
    DimensionArray(v[[variable]])
    if (Terminated()) return(Exit())
  }
  MarkStatementAsChecked()
  AdvanceToNextStatement()
}

EnactELSE <- function(s)
{
  # It is an error for a statement to begin with ELSE, which should appear only
  # after IF and THEN, as part of an IF - THEN - ELSE statement.
  SetSyntaxError()
}

EnactEND <- function(s)
{
  # Actions END commands. These set a blank exit message to halt program
  # execution and terminate quietly.
  if (nzchar(s)) return(SetSyntaxError())
  MarkStatementAsChecked()
  SetEndMessage()
}

EnactFN <- function(s)
{
  # It is an error for a statement to begin with FN, which should appear only
  # after DEF, or within an arithmetical expression within some other statement.
  SetSyntaxError()
}

EnactFOR <- function(s)
{
  # Implements FOR - TO - STEP commands. These define a looping condition to be
  # checked by NEXT. This function is only called to initialise the loop, not on
  # every iteration. Because the looping condition is not checked at this (FOR)
  # point, BASIC loops always execute at least once (this was the case for early
  # versions of BASIC, but not for later versions). While it would be easier to
  # process the STEP term first, we work in left-to-right order so that the user
  # is notified of the leftmost error (should one occur).
  a <- DetachLeadingVariable(s)
  if (Terminated()) return(Exit())
  v <- a[[variable]]
  s <- a[[remainder]]
  if (!IsNumberValued(v) || IsArray(v)) return(SetSyntaxError())
  if (!BeginsWith("=", s)) return(SetSyntaxError())
  i <- PositionOfFirst("TO", s)
  if (i < 1L) return(SetSyntaxError())
  a <- Eval(substring(s, 2L, i - 1L))
  if (!IsNumber(a)) return(SetTypeMismatchError())
  s <- substring(s, i + 2L)
  i <- PositionOfFirst("STEP", s)
  b <- Eval(if (i < 1L) s else substring(s, 0L, i - 1L))
  if (!IsNumber(b)) return(SetTypeMismatchError())
  if (i < 1L) d <- AsNumber(1) else
  {
    d <- Eval(substring(s, i + 4L))
    if (!IsNumber(d)) return(SetTypeMismatchError())
  }
  SetValue(v, a)
  if (Terminated()) return(Exit())
  InitiateLoop(v, b, d)
  MarkStatementAsChecked()
  AdvanceToNextStatement()
}

EnactGO <- function(s)
{
  # GO on its own is an error. While a reserved word, it should only appear as
  # part of GOTO or GOSUB.
  SetSyntaxError()
}

EnactGOSUB <- function(s)
{
  # Actions GOSUB commands. The subroutine must be given as an integer literal.
  if (!grepl("^[[:digit:]]+$", s)) return(SetUndefinedStatementError())
  i <- which(GetLineNumbers() == as.integer(s))
  if (length(i) != 1L) return(SetUndefinedStatementError())
  PushReturnPoint(GetPoint())
  MarkStatementAsChecked()
  ScopeSubroutineLoops()
  GoToStartOfLine(i)
}

EnactGOTO <- function(s)
{
  # Actions GOTO commands. The destination must be given as an integer literal.
  if (!grepl("^[[:digit:]]+$", s)) return(SetUndefinedStatementError())
  i <- which(GetLineNumbers() == as.integer(s))
  if (length(i) != 1L) return(SetUndefinedStatementError())
  MarkStatementAsChecked()
  GoToStartOfLine(i)
}

EnactIF <- function(s)
{
  # Actions IF - GOTO, IF - THEN, and IF - THEN - ELSE statements. The statement
  # will only be marked as checked after a THEN, ELSE, or GOTO sub-statement has
  # been successfully executed. This admits the possibility that an alternative
  # (THEN or ELSE) sub-statement might still contain an error.
  i <- PositionOfFirst("THEN", s)
  g <- PositionOfFirst("GOTO", s)
  if ((g > 0L) && ((i < 0L) || (g < i))) i <- g
  if (i < 1L) return(SetSyntaxError())
  k <- IsFalse(Eval(substring(s, 0L, i - 1L)))
  if (Terminated()) return(Exit())
  s <- substring(s, i + 4L)
  if (i == g) return(if (k) AdvanceToStartOfNextLine() else EnactGOTO(s))
  j <- PositionOfPairedELSE(s)
  if (k && (j < 0L)) return(AdvanceToStartOfNextLine())
  if (j > 0L) s <- if (k) substring(s, j + 4L) else substring(s, 0L, j - 1L)
  if (grepl("^[[:digit:]]+$", s)) return(EnactImpliedGOTO(s))
  Enact(s)
}

EnactINPUT <- function(s)
{
  # Actions INPUT statements, through which user-input is entered.

  # Specifies the default input prompt.
  p <- 1L

  # The INPUT keyword may be immediately followed by an optional string literal.
  # If so, append it to the print buffer. The string must be followed by either
  # a semicolon (display the default input prompt) or a comma (do not display
  # any input prompt). Set p accordingly.
  i <- as.integer(gregexpr("\"", s, fixed = TRUE)[[1L]])
  if (i[1L] == 1L)
  {
    if (length(i) > 1L) i <- i[2L] else return(SetSyntaxError())
    p <- substring(s, i + 1L, i + 1L)
    if (!any(p == c(",", ";"))) return(SetSyntaxError())
    AppendToPrintBuffer(substring(s, 2L, i - 1L))
    s <- substring(s, i + 2L)
    p <- as.integer(p == ";")
  }

  # The statement should end with a comma-separated list of zero or more
  # variables to be assigned. Separate these.
  v <- list()
  if (nzchar(s))
  {
    v <- DetachLeadingVariables(s)
    if (Terminated()) return(Exit())
    if (nzchar(v[[remainder]])) return(SetSyntaxError())
    v <- v[[variables]]
  }

  # Accept as many comma-separated input items from the user as there are
  # variables to assign (or one item, in the no-variable case). If the user
  # enters too few items, the addendum prompt is displayed and the remainder are
  # sought. If the user enters too many items, or an item of the wrong data type
  # (not-a-number, where a number was expected), a warning is issued and we
  # start over. Human errors here are non-fatal.
  a <- list()
  u <- PromptForInput(p)
  while (length(a) < length(v))
  {
    for (s in ScanData(u))
    {
      if (length(a) >= length(v)) {p <- -1L; break}
      d <- DatumToVariable(v[[length(a) + 1L]], s)
      if (IsError(d)) {p <- -1L; break}
      a <- append(a, list(d))
    }
    if (p < 0L) {IssueRedoFromStartWarning(); a <- list()}
    if (length(a) == length(v)) break
    p <- as.integer(sign(length(a))) + 1L
    u <- PromptForInput(p)
  }

  # Assign all variables (if any), before moving on to the next statement.
  if (length(v) > 0L) mapply(SetValue, v, a)
  MarkStatementAsChecked()
  AdvanceToNextStatement()
}

EnactLET <- function(s)
{
  # Actions LET commands (assigns values to variables). The first = in a LET
  # statement is the assignment operator; any others being comparisons.
  a <- DetachLeadingVariable(s)
  if (Terminated()) return(Exit())
  x <- a[[variable]]
  s <- a[[remainder]]
  if (!BeginsWith("=", s)) return(SetSyntaxError())
  v <- Eval(substring(s, 2L))
  if (Terminated()) return(Exit())
  SetValue(x, v)
  if (Terminated()) return(Exit())
  MarkStatementAsChecked()
  AdvanceToNextStatement()
}

EnactNEXT <- function(s)
{
  # Actions NEXT commands. The statement s can be empty (tests the most recent
  # loop), or a comma-separated list of variable names (each tests that specific
  # loop). Unlike EnactFOR(), this function is called on every iteration. A loop
  # is only broken when the iterator exceeds its goal; merely equalling the goal
  # results in one more iteration. By 'exceed', we mean 'greater than' when the
  # step is non-negative, and 'less than' when the step is negative.
  r <- ""
  v <- NULL
  if (nzchar(s))
  {
    v <- DetachLeadingVariable(s)
    if (Terminated()) return(Exit())
    r <- v[[remainder]]
    v <- v[[variable]]
  }
  p <- GetLoopParameters(v)
  if (Terminated()) return(Exit())
  if (!nzchar(r)) MarkStatementAsChecked()
  v <- p[[variable]]
  g <- Value(p[["goal"]])
  s <- Value(p[["step"]])
  a <- Value(GetValue(v)) + s
  SetValue(v, AsNumber(a))
  s <- if (s < 0) -1 else 1
  if (sign(a - g) != s) return(LoopAgain(p))
  ConcludeLoop(p)
  if (nzchar(r))
  {
    if (!BeginsWith(",", r) || (nchar(r) < 2L)) return(SetSyntaxError())
    return(EnactNEXT(substring(r, 2L)))
  }
  AdvanceToNextStatement()
}

EnactON <- function(s)
{
  # Actions ON - GOTO/GOSUB commands. The switching number, k, between ON and
  # GOTO or GOSUB, cannot be negative. Destinations, after the GOTO or GOSUB,
  # must be integer literals. The program will jump to the k-th destination. If
  # k is zero, or exceeds the number of destinations, the ON command is ignored,
  # and the program moves on to the next statement. It is an error for k to be
  # negative. The statement is not marked as checked until the GOTO or GOSUB has
  # been successfully executed.
  i <- PositionOfFirst("GO", s)
  if (i < 1L) return(SetSyntaxError())
  k <- Eval(substring(s, 0L, i - 1L))
  if (!IsNumber(k)) return(SetTypeMismatchError())
  k <- as.integer(floor(Value(k)))
  if (k < 0L) return(SetIllegalQuantityError())
  if (k == 0L) return(AdvanceToNextStatement())
  r <- substring(s, i, i + 5L)
  g <- if (BeginsWith("GOTO", r)) "GOTO" else
        if (BeginsWith("GOSUB", r)) "GOSUB" else return(SetSyntaxError())
  s <- ScanData(substring(s, i + nchar(g)))
  if (k > length(s)) return(AdvanceToNextStatement())
  eval(call(paste0("Enact", g), s[k]))
}

EnactPRINT <- function(s)
{
  # Evaluates, formats, and prints each term of PRINT statement s.
  EvalPrintStatement(s)
  if (Terminated()) return(PrintNonEmptyLine())
  MarkStatementAsChecked()
  AdvanceToNextStatement()
}

EnactREAD <- function(s)
{
  # Actions READ statements, wherein each variable of a comma-separated list is
  # assigned the next inline DATA item.
  u <- DetachLeadingVariables(s)
  if (Terminated()) return(Exit())
  if (nzchar(u[[remainder]])) return(SetSyntaxError())
  for (v in u[[variables]])
  {
    s <- GetDatum()
    if (Terminated()) return(Exit())
    d <- DatumToVariable(v, s)
    if (IsError(d)) return(SetTypeMismatchError())
    SetValue(v, d)
    if (Terminated()) return(Exit())
  }
  MarkStatementAsChecked()
  AdvanceToNextStatement()
}

EnactREM <- function(s)
{
  # Actions REM remarks. Being comments, these are skipped-over.
  MarkStatementAsChecked()
  AdvanceToNextStatement()
}

EnactRESTORE <- function(s)
{
  # Actions RESTORE commands. The command word may optionally be followed by a
  # number (literal or variable). In the absence of that number, the data
  # pointer resets to the first inline BASIC DATA item. In the presence of that
  # number, the data pointer moves to the first datum on the first line with a
  # BASIC line number not less than the RESTORE number. The original flavours of
  # BASIC had differing capabilities with regard to the RESTORE number (allowed,
  # only literal integer constants allowed, or not supported at all).
  if (!nzchar(s)) GoToFirstDatum() else
  {
    n <- Eval(s)
    if (!IsNumber(n)) return(SetTypeMismatchError())
    SeekData(n)
  }
  MarkStatementAsChecked()
  AdvanceToNextStatement()
}

EnactRETURN <- function(s)
{
  # Actions a RETURN command, to move the program back to the first statement
  # after the last GOSUB.
  if (nzchar(s)) return(SetSyntaxError())
  p <- PopReturnPoint()
  if (Terminated()) return(Exit())
  MarkStatementAsChecked()
  DropSubroutineLoops()
  GoToFirstStatementAfter(p)
}

EnactSTEP <- function(s)
{
  # It is an error for a statement to begin with STEP, which should only appear
  # after FOR and TO, as part of a FOR - TO - STEP statement.
  SetSyntaxError()
}

EnactSTOP <- function(s)
{
  # Actions STOP commands. These terminate the program with a break message.
  if (nzchar(s)) return(SetSyntaxError())
  MarkStatementAsChecked()
  SetBreakMessage()
}

EnactTHEN <- function(s)
{
  # It is an error for a statement to begin with THEN, which should appear only
  # after IF, as part of an IF - THEN statement.
  SetSyntaxError()
}

EnactTO <- function(s)
{
  # It is an error for a statement to begin with TO, which should only appear
  # after FOR, as part of a FOR - TO - <STEP> statement.
  SetSyntaxError()
}

################################################################################
# Parsing of BASIC-variable names.

DetachLeadingVariable <- function(s)
{
  # Extracts the BASIC variable at the beginning of (partial) statement s, and
  # returns that variable, along with the remainder of the statement, in a list.
  # If s does not begin with a variable, a syntax error is raised.
  i <- EndOfLeadingName(s)
  if (i < 1L) return(SetSyntaxError())
  j <- PositionOfClosingParenthesis(substring(s, i + 1L))
  k <- if (j < 1L) character() else
        SplitArguments(substring(s, i + 2L, i + j - 1L))
  v <- Variable(substring(s, 0L, i), k)
  list(variable = v, remainder = substring(s, i + j + 1L))
}

DetachLeadingVariables <- function(s)
{
  # Extracts all of the comma-separated variables from partial statement s, and
  # returns them in a list, along with the remainder of s.
  v <- list()
  while (Running())
  {
    a <- DetachLeadingVariable(s)
    if (Terminated()) return(Exit())
    v <- append(v, unname(a[variable]))
    s <- a[[remainder]]
    if (substring(s, 1L, 1L) != ",") break
    s <- substring(s, 2L)
  }
  list(variables = v, remainder = s)
}

EndOfLeadingName <- function(s)
{
  # Returns the position of the last character of the name at the beginning of
  # string s, or 0 or -1 if no such name is there. A name is one alphabetical
  # character followed by a (possibly empty) string of alphanumerics before up
  # to one type identifier (% or $), and does not include any BASIC reserved
  # word (command keyword, function, or logical operator).
  i <- attr(regexpr("^[[:alpha:]][[:alnum:]]*[%$]{0,1}", s), "match.length")
  j <- PositionOfFirstReservedWord(substring(s, 0L, i))
  if (j <= i) j - 1L else i
}

PositionOfFirst <- function(w, s)
{
  # Returns the position of the first case-insensitive appearance of word w
  # within string s, but not within a string literal. Returns -1 when w does not
  # appear within s.
  i <- Uncontained(toupper(w), toupper(s), "\"")
  if (length(i) < 1L) -1L else i[1L]
}

PositionOfFirstReservedWord <- function(s)
{
  # Returns the position of the first appearance of any reserved BASIC word
  # (keyword, function, or operator) within non-empty string s, which should
  # consist of an alphabetic character followed by zero or more alphanumeric
  # characters and an optional trailing % or $. The magic numbers 2 and 6 come
  # about from all reserved words being between 2 and 7 characters. While it is
  # possible to split s on numeric characters and loop over smaller words, that
  # is slower than matching on one big table (which grows as nchar(s)^2). It is
  # also slower to pre-filter the reserved words (on the letters of s) with
  # regular expressions. While it would be possible to insert a line 'if any(
  # r == w) return(1)' before the match, for faster detection of the whole of s
  # being a reserved word, this ultimately results in faster syntax errors, at
  # the expense of slower normal operations. Returns one more than the number of
  # characters in s when no reserved word is present.
  n <- nchar(s)
  y <- substring(s, n, n)
  m <- n - as.integer(any(y == c("$", "%", seq(0, 9))))
  if (m < 2L) return(n + 1L)
  r <- GetReservedWords(m, y == "$")
  u <- rep(seq(n - 1L), c(rep(6L, max(n - 7L, 0L)), seq(min(n - 1L, 6L), 1L)))
  v <- unlist(mapply(seq, seq(2L, n), mapply(min, seq(7L, n + 5L), n)))
  w <- !is.na(match(substring(toupper(s), u, v), r))
  if (any(w)) return(u[which(w)[1L]])
  n + 1L
}

PositionOfPairedELSE <- function(s)
{
  # String s immediately follows a THEN (not included in s). This function
  # returns the position, within s, of the ELSE paired with that leading THEN.
  # Returns -1 when no such ELSE exists.
  u <- toupper(s)
  k <- PositionsOf("\"", u)
  i <- Unenclosed("ELSE", u, k)
  if (length(i) == 0L) return(-1L)
  j <- Unenclosed("THEN", u, k)
  if (length(j) == 0L) return(i[1L])
  i <- i[(sapply(i, function(a, b) sum(a > b), j) - seq_along(i)) < 0L]
  if (length(i) == 0L) return(-1L)
  i[1L]
}

Uncontained <- function(w, s, z)
{
  # Returns all positions of character (or word) w within string s that do not
  # lie between a pair of delimiting characters (or words, typically quotation
  # marks) z, according to the even/odd delimiter-count rule. Case sensitive.
  u <- PositionsOf(w, s)
  v <- PositionsOf(z, s)
  u[as.logical(sapply(u, function(a, b) sum(a > b) %% 2L == 0L, v))]
}

################################################################################
# Data readers (including user input).

DatumToNumber <- function(s)
{
  # Converts R-string s (being user-input or a DATA item) to a BASIC number.
  # BASIC regards blanks, and decimal points with no digit on either side, as
  # representations of zero. Returns a non-fatal error on failure.
  w <- toupper(gsub("[[:blank:]]+", "", s))
  if (!nzchar(w) || grepl("^[+-]{0,1}\\.(E[+-]{0,1}[[:digit:]]*){0,1}$", w))
    return(AsNumber(0))
  n <- try(suppressWarnings(as.numeric(w)), silent = TRUE)
  if (!inherits(n, "try-error") && is.finite(n)) return(AsNumber(n))
  AsError("Failed Numeric Conversion")
}

DatumToString <- function(s)
{
  # Converts R-string s (being user-input or a DATA item) to a BASIC string, by
  # removing leading and trailing quotes only when a leading quote is present.
  AsString(if (BeginsWith("\"", s)) gsub("^\"|\"$", "", s) else s)
}

DatumToVariable <- function(v, s)
{
  # Converts R-string s (being either user-input or a DATA item) to a BASIC
  # string or number, whichever is appropriate for variable v. Returns a non-
  # fatal error in response to conversion failure.
  if (IsStringValued(v)) DatumToString(s) else DatumToNumber(s)
}

ScanData <- function(s)
{
  # Picks comma-separated values (inline or user-input) out of string s, for use
  # with DATA, INPUT, or ON commands. BASIC takes no action on any keywords in
  # data, so 'DATA 1REM,2,3' scans to three items, rather than one and a remark.
  # Colons still split statements, so 'DATA 1:REM,2' contains one data point. We
  # leave encasing quotes in place, because while 'DATA 5' can be READ to string
  # or number type, 'DATA "5"' can only be READ as a string. Data items with an
  # opening quotation mark need not have a closing one.
  i <- Uncontained(",", s, "\"")
  substring(s, c(1L, i + 1L), c(i - 1L, nchar(s)))
}

################################################################################
# BASIC to R logical truth of value conversion.

IsFalse <- function(x)
{
  # Returns TRUE (FALSE) if BASIC-value x is regarded as logically false (true)
  # in BASIC. Sets a syntax error when x is not a value type.
  if (IsNumber(x)) return(Value(x) == 0)
  if (IsString(x)) return(!nzchar(Value(x)))
  SetSyntaxError()
}

IsTrue <- function(x)
{
  # Returns TRUE (FALSE) if BASIC-value x is regarded as logically true (false)
  # in BASIC. Sets a syntax error when x is not a value type.
  if (IsNumber(x)) return(Value(x) != 0)
  if (IsString(x)) return(nzchar(Value(x)))
  SetSyntaxError()
}

############################################################################ EOF
