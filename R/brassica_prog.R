# BASIC program storage, state, and flow.
# MJL @ Titirangi, 3 August 2022.

################################################################################
# Storage space for one BASIC program and its run-state.

progMemory <- new.env(parent = emptyenv())

################################################################################
# Constant object names.

basicProgram      <- "bp" # Raw program listing, as it appears in the file.
basicStatements   <- "bs" # Program listing, deconstructed to statements.
checkedStatements <- "cs" # Logs which statements have successfully executed.
cursorPosition    <- "cp" # Zero-indexed console column of the print cursor.
dataIndex         <- "di" # Index number of the next data item to read.
dataQueue         <- "dl" # Character vector of all data items in the program.
exitTrigger       <- "et" # The first error or break condition to have occurred.
fileLineNumbers   <- "fn" # Source-file line number of each program line.
lastRandomNumber  <- "lr" # The last pseudo-random variate generated.
lineIndex         <- "li" # Index number of the current program line.
lineNumbers       <- "ln" # The BASIC line number of each program line.
loopPoints        <- "lp" # FOR-loop return points and conditions.
printBuffer       <- "pb" # Character strings, ready to be printed.
printedChars      <- "pc" # Characters already printed on the current line.
reservedWords     <- "rw" # BASIC keywords, functions, and logical operators.
returnPoints      <- "rp" # GOSUB return-point stack (for RETURN commands).
statementIndex    <- "si" # Index of the current statement on the current line.
ttCharDelay       <- "tx" # Teletypewriter delay after printing each character.
ttLineDelay       <- "ty" # Teletypewriter delay after printing each line.
upperCase         <- "uc" # Whether or not to force upper-case printing.

################################################################################
# Clear program memory.

ResetProgMemory <- function()
{
  # Delete the program and its state, leaving only the (constant) reserved words
  # look-up table, and the initial random number, on an otherwise clean slate.
  remove(list = ls(progMemory), pos = progMemory)
  SetReservedWords()
  GetNewRandomNumber()
}

################################################################################
# Low-level setter and getter for objects in program memory.

DelPro <- function(a)
{
  # Deletes the object named a from program memory, if it exists there.
  if (exists(a, progMemory, inherits = FALSE)) rm(list = a, pos = progMemory)
}

GetPro <- function(a, b)
{
  # Retrieves the object named a from program memory, returning default value b
  # if a is not found there.
  get0(a, progMemory, inherits = FALSE, ifnotfound = b)
}

SetPro <- function(a, b)
{
  # Assigns value b to an object named a, stored within program memory.
  assign(a, b, progMemory)
}

################################################################################
# Load-time program pre-processing and inline data extraction.

LineHasStatement <- function(p)
{
  # Returns TRUE for those lines of program listing p that are neither blank nor
  # a comment (hence contain a program statement), and FALSE for those that are
  # (blanks or comments). Comments are lines having # as the first non-blank.
  # (REM remarks are loaded and interpreted, comments are not.)
  !grepl("^[[:blank:]]*(#|$)", p)
}

Load <- function(f)
{
  # Loads a BASIC program from file (named) f. This can either be supplied by
  # the user as an external file (within their file space), or else bundled
  # within the R package's namespace.
  ResetLineIndex()
  p <- ReadExternal(f)
  if (is.null(p)) p <- ReadBundled(f)
  if (is.null(p)) return(SetFileNotFoundError())
  ResetProgMemory()
  RetainProgramVerbatim(p)
  LoadStatements(p)
  LoadData()
}

LoadData <- function()
{
  # Scans the stored program for inline DATA statements, and transcribes all
  # such data to a vector in program-memory for run-time convenience. BASIC
  # does not detect DATA not at the very beginning of its statement (as is the
  # case for, say, 'IF 1=1 THEN DATA 1,2,3'). Note that 'DATA ,,' and 'DATA'
  # (with nothing after it) are both allowed. This is read as an empty string
  # (which becomes a zero if coerced to number-type).
  s <- GetStatements()
  n <- GetLineNumbers()
  n[is.na(n)] <- -1L
  n <- cummax(n)
  d <- character()
  for (i in seq_along(s))
  {
    for (b in s[[i]][BeginsWith("DATA", s[[i]])])
    {
      a <- ScanData(Trim(b, "DATA"))
      names(a)[1L] <- as.character(n[i])
      d <- c(d, a)
    }
  }
  SetData(d)
}

LoadStatements <- function(p)
{
  # Deconstructs BASIC program listing p to its constituent statements, and
  # stores them, along with their BASIC line numbers and source-file line
  # numbers, within program-memory space.

  # Source-file line numbers (fl) and the text on each line (lt).
  fl <- seq_along(p)
  lt <- trimws(p)

  # Remove blank and comment lines, leaving only those containing statements.
  i <- LineHasStatement(lt)
  fl <- fl[i]
  lt <- lt[i]

  # Split BASIC line numbers (bl) from the line text (lt).
  i <- attr(regexpr("^[[:digit:]]+", lt), "match.length")
  bl <- as.integer(substring(lt, 1L, i))
  lt <- trimws(substring(lt, i + 1L))

  # Check BASIC line numbers are unique and monotonically increasing.
  i <- bl[!is.na(bl)]
  if (any(duplicated(i))) return(SetRedefinedLineError())
  if (!all(i == cummax(i))) return(SetLineOutOfSequenceError())

  # Locate colons (statement separators), quotes (string delimiters), DATAs
  # (literal data markers), and REMs (remarks; comment markers). BASIC keywords
  # have precedence over everything else, so, unless a REM is a string literal
  # (within quotes or data), everything after it goes unseen. Hence the line
  # '100 LET A=XREMY:DATA 1,2,3' contains no data, while '100 DATAREM,1:DATA2'
  # contains three data items ('REM', 1, and 2).
  ic <- gregexpr(":", lt, fixed = TRUE)
  iq <- gregexpr("\"", lt, fixed = TRUE)
  ir <- gregexpr("REM", lt, ignore.case = TRUE)
  id <- gregexpr("DATA", lt, ignore.case = TRUE)

  # Split each line into its constituent statements.
  ps <- as.list(lt)
  for (i in seq_along(ps))
  {
    # Locate colons on this line.
    jc <- ic[[i]][ic[[i]] > 0L]
    if (length(jc) == 0L) next

    # Locate quotes, REMs and DATAs on this line.
    jq <- iq[[i]][iq[[i]] > 0L]
    jr <- ir[[i]][ir[[i]] > 0L]
    jd <- id[[i]][id[[i]] > 0L]

    # Disregard markers within string literals, by the even/odd quotes rule.
    jc <- jc[as.logical(sapply(jc, function(a, b) sum(a > b) %% 2L == 0L, jq))]
    jr <- jr[as.logical(sapply(jr, function(a, b) sum(a > b) %% 2L == 0L, jq))]
    jd <- jd[as.logical(sapply(jd, function(a, b) sum(a > b) %% 2L == 0L, jq))]

    # Disregard REMs within DATA statements (treated as string literals).
    if ((length(jc) > 0L) && (length(jr) > 0L) && (length(jd) > 0L))
    {
      kc <- sapply(jr, function(x, y) max(c(-1L, y[y < x])), jc)
      kd <- sapply(jr, function(x, y) max(c(-2L, y[y < x])), jd)
      jr <- jr[kc > kd]
    }

    # Disregard colons within remarks (everything after the first active REM).
    if ((length(jc) > 0L) && (length(jr) > 0L)) jc <- jc[jc < jr[1L]]

    # Split the line into statements.
    if (length(jc) > 0L)
    {
      a <- substring(ps[[i]], c(1L, jc + 1L), c(jc, nchar(ps[[i]])))
      ps[[i]] <- trimws(gsub("^:|:$", "", a))
    }
  }

  # Remove whitespace from each line, excepting that on the interior of string
  # literals and data items (quoted or not). Other than these, BASIC pretty much
  # ignores whitespace, and removing it before run-time makes life easier. The
  # necessity of treating DATA statements differently from others, compels us to
  # perform this action after, not before, reducing the program to statements.
  d <- "^D[[:blank:]]*A[[:blank:]]*T[[:blank:]]*A"
  for (i in seq_along(ps))
  {
    k <- attr(regexpr(d, ps[[i]], ignore.case = TRUE), "match.length")
    for (j in which(k < 0L)) ps[[i]][j] <- SpaceFreeLine(ps[[i]][j])
    for (j in which(k > 0L)) ps[[i]][j] <- SpaceFreeData(ps[[i]][j], k[j])
  }

  # Load the source-file line numbers (fl), the corresponding BASIC program line
  # numbers (bl), and the associated BASIC program statements (ps), into memory.
  SetFileLineNumbers(fl)
  SetLineNumbers(bl)
  SetStatements(ps)
}

ProgramListing <- function()
{
  # Retrieves the original BASIC program listing from memory.
  # This only happens when the user calls LIST() to take a look at it.
  GetPro(basicProgram, character())
}

ReadBundled <- function(f)
{
  # If f is the name of an included BASIC program, return that program as a
  # character vector. Otherwise, return NULL.
  g <- tolower(gsub("[[:blank:]]+", "_", f))
  g <- system.file("BASIC", paste0(g, ".bas"), package = "brassica")
  ReadExternal(g)
}

ReadExternal <- function(f)
{
  # If f is the name of a readable file, return its content as a character
  # vector. Otherwise, return NULL.
  if (!inherits(f, class(character()))
      || (length(f) != 1L)
      || !file.exists(f)
      || (file.access(f, 4L) != 0L)
      || file.info(f)$isdir)
    return(NULL)
  readLines(f)
}

RetainProgramVerbatim <- function(p)
{
  # Keeps an unmodified copy of the original BASIC program listing.
  # This copy is not used anywhere, except when the user requests a LIST().
  SetPro(basicProgram, p)
}

SetAesthetics <- function(ttx, tty, up)
{
  # Sets teletypewriter-effect control parameters: the delay after printing each
  # character, the delay after printing each line (carriage return), and whether
  # or not to convert all output to upper case.
  SetTeletypeCharDelay(ttx)
  SetTeletypeLineDelay(tty)
  SetUpperCaseOut(up)
}

SetData <- function(d)
{
  # Stores the vector of inline BASIC-program DATA, d, in program-memory.
  SetPro(dataQueue, d)
}

SetFileLineNumbers <- function(n)
{
  # Stores the vector of source-file line numbers, n, in program memory.
  SetPro(fileLineNumbers, n)
}

SetLineNumbers <- function(n)
{
  # Stores the vector of BASIC line numbers, n, in program memory.
  SetPro(lineNumbers, n)
}

SetStatements <- function(s)
{
  # Stores a BASIC program as a list of its statements, s, and marks each of
  # those statements as unchecked (not yet successfully executed).
  SetPro(basicStatements, s)
  MarkStatementsAsUnchecked()
}

SpaceFreeData <- function(s, n)
{
  # Removes unnecessary whitespace from BASIC DATA statement s, being everything
  # except that within a string literal or strictly within a datum (whether
  # quoted or not). Integer n marks the end of the DATA keyword.
  a <- gsub("[[:blank:]]+", "", substring(s, 1L, n))
  b <- substring(s, n + 1L)
  i <- c(0L, Uncontained(",", b, "\""), nchar(b) + 1L)
  b <- substring(b, i[-length(i)] + 1L, i[-1L] - 1L)
  paste0(a, paste0(trimws(b), collapse = ","))
}

SpaceFreeLine <- function(s)
{
  # Removes unnecessary whitespace from any BASIC statement, s, besides a DATA
  # statement (for those, use SpaceFreeData()). Unnecessary whitespace in this
  # context is everything except that within string literals.
  i <- c(-1L, 0L, PositionsOf("\"", s))
  i <- c(i, nchar(s) + seq(length(i) %% 2L, 1L) + 1L)
  j <- i + c(0L, 1L)
  k <- i + c(-1L, 0L)
  s <- substring(s, j[-length(j)], k[-1L])
  s[c(FALSE, TRUE)] <- gsub("[[:blank:]]+", "", s[c(FALSE, TRUE)])
  paste0(s, collapse = "")
}

################################################################################
# Reserved-word look-up table.

SetReservedWords <- function()
{
  # Builds a constant look-up table of BASIC's reserved words, at load-time.
  w <- c(keywords, functions, logicals)
  w <- w[order(nchar(w), w)]
  i <- grepl("[$]$", w)
  u <- w[!i]
  v <- w[i]
  j <- nchar(u)
  k <- nchar(v)
  m <- max(j, k - 1L)
  r <- vector(mode(list()), m)
  for (i in seq_along(r))
    r[[i]] <- list(u[j <= i], sort(c(u[j <= i], v[k <= i + 1L])))
  r[[1L]] <- list(m, m)
  SetPro(reservedWords, r)
}

GetReservedWords <- function(n, s)
{
  # Returns a character vector of BASIC's reserved words containing n or fewer
  # alphanumeric characters, during run-time. If s is TRUE, the vector includes
  # words ending with '$' (making n + 1 characters in total). When s is FALSE,
  # those words are omitted. The special case of n = 1 returns the length of the
  # longest reserved word (7 characters, for RESTORE).
  w <- GetPro(reservedWords, NULL)
  w[[min(n, length(w))]][[as.integer(s) + 1L]]
}

################################################################################
# Run-time retrieval of BASIC DATA, for READ commands.

AdvanceToNextDatum <- function()
{
  # Increment the data index, ready to read the next datum in the list.
  SetDataIndex(GetDataIndex() + 1L)
}

GetDataIndex <- function()
{
  # Returns the value of the data index.
  GetPro(dataIndex, 1L)
}

GetDataQueue <- function()
{
  # Returns the entire data list.
  GetPro(dataQueue, character())
}

GetDatum <- function()
{
  # Returns the current datum from the list, and advances to the next.
  d <- GetDataQueue()
  i <- GetDataIndex()
  if (length(d) < i) return(SetOutOfDataError())
  AdvanceToNextDatum()
  unname(d[i])
}

GoToFirstDatum <- function()
{
  # Resets the data index to the first datum in the list.
  SetDataIndex(1L)
}

SeekData <- function(n)
{
  # Moves the data index to the first datum on the first program line numbered
  # not less than BASIC number n. Moves beyond the list when n is too large.
  m <- as.integer(names(GetDataQueue()))
  SetDataIndex(min(which(m >= Value(n)), length(m) + 1L))
}

SetDataIndex <- function(i)
{
  # Moves the data index to position number i (which can be beyond the list).
  SetPro(dataIndex, i)
}

################################################################################
# Program-counter line index.

AdvanceToStartOfNextLine <- function()
{
  # Advances the program counter to the first statement on the next line.
  SetLineIndex(GetLineIndex() + 1L)
  GoToFirstStatementOnLine()
}

CountProgramLines <- function()
{
  # Returns the total number of lines in the stored BASIC program.
  length(GetFileLineNumbers())
}

GetLineIndex <- function()
{
  # Returns the sequential index number of the current program line.
  GetPro(lineIndex, 0L)
}

GoToFirstLine <- function()
{
  # Goes to the first line, without altering the statement index.
  SetLineIndex(1L)
}

GoToStartOfFirstLine <- function()
{
  # Goes to the first statement of the first line (to the start of the program).
  # Does the same thing as GoToFirstStatement().
  GoToFirstLine()
  GoToFirstStatementOnLine()
}

GoToStartOfLine <- function(i)
{
  # Goes to the first statement (the start) of the i-th line (by line index, not
  # by BASIC line number).
  SetLineIndex(i)
  GoToFirstStatementOnLine()
}

ResetLineIndex <- function()
{
  # Moves the line index out-of-program, prior to loading another program when
  # one has been run previously.
  SetLineIndex(0L)
}

SetLineIndex <- function(i)
{
  # Moves to the i-th line (by index), without changing the statement index.
  SetPro(lineIndex, i)
}

WithinProgram <- function()
{
  # Returns TRUE if the line index lies within the range of the stored BASIC
  # program. Returns FALSE otherwise (including when no program is loaded).
  i <- GetLineIndex()
  (i > 0L) && (i <= CountProgramLines())
}

################################################################################
# Program-counter statement index.

AdvanceToNextStatement <- function()
{
  # Advances the program counter to the next statement.
  # This might be on the current line, or the next.
  i <- SetStatementIndex(GetStatementIndex() + 1L)
  if (i > CountStatementsOnLine()) AdvanceToStartOfNextLine()
}

CountStatementsOnLine <- function()
{
  # Returns the number of statements on the current line of the program.
  length(GetStatementsOnLine())
}

GetStatementIndex <- function()
{
  # Returns the index of the current statement on the current line. This is line
  # relative; the second statement on line 100 has index 2, as does the second
  # statement on line 5000.
  GetPro(statementIndex, 1L)
}

GoToFirstStatement <- function()
{
  # Resets the program counter to the first statement of the first line (i.e.,
  # goes to the start of the BASIC program, same as GoToStartOfFirstLine()).
  GoToFirstLine()
  GoToFirstStatementOnLine()
}

GoToFirstStatementOnLine <- function()
{
  # Goes to the first statement on the current line (without changing lines).
  SetStatementIndex(1L)
}

SetStatementIndex <- function(i)
{
  # Moves to the i-th statement on the current line. That statement need not
  # exist (there is no protection against overshooting, here).
  SetPro(statementIndex, i)
}

################################################################################
# Full program-counter setter and getter (for loops and subroutines).

GetPoint <- function()
{
  # Returns the program counter as a (line-index, statement-index) ordered pair.
  # (Returns the current position in the program.)
  c(GetLineIndex(), GetStatementIndex())
}

SetPoint <- function(p)
{
  # Sets the program counter from a (line-index, statement-index) ordered pair.
  # (Goes to that position in the program.)
  SetLineIndex(p[1L])
  SetStatementIndex(p[2L])
}

################################################################################
# GOSUB (subroutine) return-point stack.

GetReturnPoints <- function()
{
  # Retrieves the entire return-point stack.
  GetPro(returnPoints, list())
}

GoToFirstStatementAfter <- function(p)
{
  # Moves to the statement immediately following the GOSUB at point p.
  SetPoint(p)
  AdvanceToNextStatement()
}

PopReturnPoint <- function()
{
  # Pops the last return-point (removes it from the stack before returning it).
  p <- GetReturnPoints()
  if (length(p) < 1L) return(SetReturnWithoutGosubError())
  SetReturnPoints(p[-length(p)])
  p[[length(p)]]
}

PushReturnPoint <- function(p)
{
  # Pushes a return-point, p, onto the top of the stack.
  SetReturnPoints(append(GetReturnPoints(), list(p)))
}

SetReturnPoints <- function(p)
{
  # Saves p as the entire return-point stack, in program memory.
  SetPro(returnPoints, p)
}

################################################################################
# FOR (loop) return-point stack.

FindLoop <- function(p)
{
  # Locates loop p within the loop-stack of the current subroutine scope, and
  # returns its index (or -1, if p does not appear within the loop-stack). A
  # special case is p = NULL, for which the most recent loop is returned.
  u <- GetLoops()
  if (length(u) < 1L) return(-1L)
  if (is.null(p)) return(length(u))
  i <- names(u) == LoopName(p)
  if (!any(i)) return(-1L)
  min(which(i))
}

GetAllLoops <- function()
{
  # Returns the list of loop definitions across all subroutine levels.
  GetPro(loopPoints, list(list()))
}

GetLoops <- function()
{
  # Returns the list of loop definitions for the current subroutine level.
  p <- GetAllLoops()
  p[[length(p)]]
}

LoopName <- function(p)
{
  # Returns the name of loop p (by which it is identified in the loop stack).
  NameForLoop(p[[variable]])
}

NamedLoop <- function(p)
{
  # Embeds loop object p in a named list, for appending to the loop stack.
  a <- list(p)
  names(a) = LoopName(p)
  a
}

NameForLoop <- function(v)
{
  # Returns the (loop stack) name of the loop over variable v.
  # Presently, this is the same as the variable's R storage-object's name.
  NameForObject(v)
}

NewLoop <- function(v, g, s)
{
  # Initiates a new FOR loop, iterating over variable v, in increments of step
  # s, until exceeding goal g, at the current point in the program.
  list(point = GetPoint(), variable = v, goal = g, step = s)
}

SetAllLoops <- function(p)
{
  # Sets the entire list of loop definitions, for all subroutine levels.
  SetPro(loopPoints, p)
}

SetLoops <- function(p)
{
  # Sets the list of loop definitions, for the current subroutine level.
  a <- GetAllLoops()
  a[[length(a)]] <- p
  SetAllLoops(a)
}

################################################################################
# FOR (loop) program flow controls.

ConcludeLoop <- function(p)
{
  # Removes loop p from the stack, after its termination condition has been met,
  # or on beginning a new loop over the same iterator. Concluding loop p also
  # concludes any nested (subsequently initiated) loops. No action is performed
  # when loop p does not appear in the stack.
  i <- FindLoop(p)
  if (i > 0L) SetLoops(GetLoops()[seq(0L, i - 1L)])
}

DropSubroutineLoops <- function()
{
  # On RETURNing from a subroutine, this concludes all loops created within it
  # and drops the scoping level, reverting to the loops of the previous level.
  a <- GetAllLoops()
  SetAllLoops(a[-length(a)])
}

GetLoopParameters <- function(v)
{
  # Returns the parameters (return-point, iterator variable, stopping condition,
  # and iteration increment) of the FOR loop associated with variable v. If such
  # a loop exists, it is unique. If v is NULL, the most recent loop is returned.
  u <- GetLoops()
  if (length(u) < 1L) return(SetNextWithoutForError())
  if (is.null(v)) return(u[[length(u)]])
  if (!IsNumberValued(v) || IsArray(v)) return(SetNextWithoutForError())
  p <- u[[NameForLoop(v)]]
  if (is.null(p)) return(SetNextWithoutForError())
  p
}

InitiateLoop <- function(v, g, s)
{
  # Initiates a new loop, iterating over variable v, in increments of step s,
  # until exceeding goal g, within the current subroutine scoping level, at the
  # current point in the program. If another loop over the same iterator already
  # exists within the same scope, that pre-existing loop is concluded (dropped),
  # along with any nested loops.
  p <- NewLoop(v, g, s)
  ConcludeLoop(p)
  SetLoops(append(GetLoops(), NamedLoop(p)))
}

LoopAgain <- function(p)
{
  # Moves the program counter to the first statement after the top (the defining
  # FOR statement) of loop p.
  SetPoint(p[["point"]])
  AdvanceToNextStatement()
}

ScopeSubroutineLoops <- function()
{
  # Creates a new level of loop scoping, on beginning a new subroutine (GOSUB).
  # Although subroutines use the same variables as the rest of the program (all
  # BASIC variables being global), a subroutine only sees the loops that were
  # created within it.
  a <- GetAllLoops()
  SetAllLoops(append(a, list(list())))
}

################################################################################
# Run-time retrieval of line numbers and BASIC program statements.

GetFileLineNumber <- function()
{
  # Returns the source file line number of the current BASIC line. For example,
  # if the current BASIC line reads '100 print x', and it appears on line 5 of
  # the source file, then this function returns the integer value 5, not 100.
  i <- GetLineIndex()
  n <- GetFileLineNumbers()
  if ((i < 1L) || (i > length(n))) return(NA_integer_)
  n[i]
}

GetFileLineNumbers <- function()
{
  # Returns a vector of all source-file line numbers for those source-file lines
  # containing an executable BASIC line (as opposed to a comment or whitespace).
  GetPro(fileLineNumbers, integer())
}

GetLineNumber <- function()
{
  # Returns the BASIC line number of the current program line.
  i <- GetLineIndex()
  n <- GetLineNumbers()
  if ((i < 1L) || (i > length(n))) return(NA_integer_)
  n[i]
}

GetLineNumbers <- function()
{
  # Returns the BASIC line numbers for every line of the entire program.
  GetPro(lineNumbers, integer())
}

GetStatement <- function()
{
  # Returns the current BASIC statement.
  GetStatementsOnLine()[GetStatementIndex()]
}

GetStatements <- function()
{
  # Returns the complete program listing, deconstructed to statements.
  GetPro(basicStatements, list())
}

GetStatementsOnLine <- function()
{
  # Returns a vector of all statements on the current line. This can be empty
  # when the line index lies beyond the end of the program.
  i <- GetLineIndex()
  s <- GetStatements()
  if ((i < 1L) || (i > length(s))) return(character())
  s[[i]]
}

################################################################################
# Low-level exit-message constructors.

ConstructLocationClause <- function()
{
  # Constructs a location clause, to be appended to an error or break message,
  # when that error or break occurred in run-time (on a specific program line).
  if (!WithinProgram()) return("")
  n <- GetLineNumber()
  if (!is.na(n)) return(paste(" on line", n))
  paste(" on file line", GetFileLineNumber())
}

EmbellishBreakMessage <- function(m)
{
  # Formats message m as a break message with line number (if applicable).
  paste0(m, ConstructLocationClause())
}

EmbellishErrorMessage <- function(m)
{
  # Formats message m as an error message with line number (if applicable).
  paste0("? ", m, " Error", ConstructLocationClause())
}

GetExitMessage <- function()
{
  # Returns the exit message as a character string (or empty character vector
  # when no message has been set).
  Value(GetExitTrigger())
}

GetExitTrigger <- function()
{
  # Returns the exit message as a BASIC break or error object (or as a BASIC
  # string, if no exit has been triggered).
  GetPro(exitTrigger, AsString(character()))
}

SetExitTrigger <- function(m)
{
  # Sets the exit trigger condition to BASIC (break or error) data object m.
  # This will overwrite any pre-existing condition.
  SetPro(exitTrigger, m)
}

SetIfNoExitTriggered <- function(m)
{
  # Sets the exit-trigger to BASIC (break or error) object m, unless some other
  # exit message already exists (in which case, no action is taken). Returns the
  # original exit trigger condition.
  if (Running()) SetExitTrigger(m)
  GetExitTrigger()
}

################################################################################
# High-level exit-message constructors.

SetBreakMessage <- function()
{
  # Thrown by a BASIC STOP command. Will not replace any pre-existing message.
  # Returns the original exit trigger condition.
  SetIfNoExitTriggered(AsBreak(EmbellishBreakMessage("Break")))
}

SetEndMessage <- function()
{
  # Thrown by a BASIC END command (for a silent exit). Will not replace any
  # pre-existing message. Returns the original exit trigger condition.
  SetIfNoExitTriggered(AsBreak(""))
}

SetErrorMessage <- function(m)
{
  # Sets the exit message to an error based on character string m, but will not
  # overwrite an existing message. Returns the original exit trigger condition.
  SetIfNoExitTriggered(AsError(EmbellishErrorMessage(m)))
}

################################################################################
# Higher-level error message constructors.

SetBadSubscriptError <- function()
{
  # Raised when an index to an array lies outside its allowed range.
  SetErrorMessage("Bad Subscript")
}

SetDivisionByZeroError <- function()
{
  # Raised when attempting to divide something by zero.
  SetErrorMessage("Division By Zero")
}

SetDuplicatedParameterError <- function()
{
  # Raised when defining a function of the same parameter twice, e.g., f(x,x).
  SetErrorMessage("Duplicated Parameter")
}

SetFileNotFoundError <- function()
{
  # Raised when attempting to access a file that cannot be read.
  SetErrorMessage("File Not Found")
}

SetIllegalQuantityError <- function()
{
  # Raised when a numerical argument to a function lies outside its permitted
  # range (such as when attempting to take the logarithm of a negative value).
  SetErrorMessage("Illegal Quantity")
}

SetLineOutOfSequenceError <- function()
{
  # Raised when the program listing contains BASIC line numbers that are not
  # monotonically increasing.
  SetErrorMessage("Line Out of Sequence")
}

SetNextWithoutForError <- function()
{
  # Raised when the program encounters an end-of-loop (NEXT), without having
  # begun the loop (without the corresponding FOR).
  SetErrorMessage("Next Without For")
}

SetOutOfDataError <- function()
{
  # Raised on an attempt to READ inline DATA, without there being any left in
  # the queue.
  SetErrorMessage("Out Of Data")
}

SetRedefinedLineError <- function()
{
  # Raised when the program listing contains a duplicated BASIC line number.
  SetErrorMessage("Redefined Line")
}

SetRedimmedArrayError <- function()
{
  # Raised when attempting to DIM an array that has already been DIMmed (even
  # when the new and old dimensions are identical).
  SetErrorMessage("Redimmed Array")
}

SetReturnWithoutGosubError <- function()
{
  # Raised upon meeting an end-of-subroutine (RETURN), without being in one
  # (without the corresponding GOSUB).
  SetErrorMessage("Return Without Gosub")
}

SetSyntaxError <- function()
{
  # Raised when a program statement breaks some or other BASIC syntax rule.
  SetErrorMessage("Syntax")
}

SetTypeMismatchError <- function()
{
  # Raised when a string (or number) was required, but a number (or string) was
  # given.
  SetErrorMessage("Type Mismatch")
}

SetUndefinedFunctionError <- function()
{
  # Raised on attempting to call a user function (FN) without first having
  # defined it (with DEF FN).
  SetErrorMessage("Undefined Function")
}

SetUndefinedStatementError <- function()
{
  # Raised on attempting to GOTO or GOSUB a BASIC line number that does not
  # appear within the program. This includes when attempting to GOTO/GOSUB to
  # a character string, variable, floating-point number, or any other token(s)
  # besides an unquoted literal string of digits.
  SetErrorMessage("Undefined Statement")
}

################################################################################
# Statement checking (for development purposes, only).

AllChecked <- function()
{
  # Returns TRUE when every last statement of the program has been executed
  # successfully at some point. Returns FALSE otherwise.
  (length(UncheckedLineIndices()) == 0L)
}

GenerateUncheckedList <- function()
{
  # Creates a list of 'unchecked' flags, one for each program statement.
  lapply(GetStatements(), function(x) rep(FALSE, length(x)))
}

GetChecks <- function()
{
  # Returns the full list of check-state flags.
  GetPro(checkedStatements, list())
}

MarkStatementsAsUnchecked <- function()
{
  # Flags all program statements as being unchecked.
  SetChecks(GenerateUncheckedList())
}

MarkStatementAsChecked <- function()
{
  # Flags the current statement as being checked. That is, as having been
  # successfully executed by the interpreter (in its entirety, without error).
  s <- GetChecks()
  s[[GetLineIndex()]][GetStatementIndex()] <- TRUE
  SetChecks(s)
}

SetChecks <- function(s)
{
  # Stores check-list s in program memory.
  SetPro(checkedStatements, s)
}

UncheckedFileLineNumbers <- function()
{
  # Returns the source-file line numbers of the unchecked statements.
  GetFileLineNumbers()[UncheckedLineIndices()]
}

UncheckedLineIndices <- function()
{
  # Returns the indices of any BASIC program lines containing at least one
  # unchecked statement.
  which(!sapply(GetChecks(), all))
}

UncheckedLineNumbers <- function()
{
  # Returns the BASIC program line numbers of the unchecked statements.
  GetLineNumbers()[UncheckedLineIndices()]
}

UncheckedLines <- function()
{
  # Returns a listing of the unchecked (not wholly checked) program lines.
  p <- ProgramListing()
  p[LineHasStatement(p)][UncheckedLineIndices()]
}

################################################################################
# Print-buffer management.

AnythingInPrintBuffer <- function()
{
  # Returns TRUE when the print buffer contains at least one character.
  # Returns FALSE otherwise.
  any(nzchar(GetPrintBuffer()))
}

AppendToPrintBuffer <- function(s)
{
  # Appends character string s to the print buffer. If s contains a newline or
  # carriage return, the buffer is printed up to (including) that character.
  # If the buffer is too large for the console, lines are printed until the
  # remainder fits.
  s <- as.character(s)
  if (!nzchar(s)) return(NULL)
  i <- gregexpr("[\n\r]", s)[[1L]][1L]
  while (i > 0L)
  {
    SetPrintBuffer(append(GetPrintBuffer(), substring(s, 0L, i - 1L)))
    ReducePrintBuffer()
    PrintPrintBuffer()
    if (substring(s, i, i) == "\n") PrintNewLine() else PrintCarriageReturn()
    s <- substring(s, i + 1L)
    i <- gregexpr("[\n\r]", s)[[1L]][1L]
  }
  SetPrintBuffer(append(GetPrintBuffer(), s))
  ReducePrintBuffer()
}

CharactersInPrintBuffer <- function()
{
  # Returns the number of characters, printable or otherwise, in the buffer.
  sum(nchar(GetPrintBuffer()))
}

ClearPrintBuffer <- function()
{
  # Empties the print buffer, leaving nothing to be printed.
  SetPro(printBuffer, "")
}

GetPrintBuffer <- function()
{
  # Retrieves the print buffer; a character vector of strings to be printed.
  GetPro(printBuffer, "")
}

ReducePrintBuffer <- function()
{
  # When printing of the buffer would extend beyond the width of the console, we
  # print however much fits and then begin a new line.
  w <- as.integer(options("width"))
  while(VirtualCursorPosition() > w)
  {
    n <- w - GetCursorPosition()
    s <- paste0(GetPrintBuffer(), collapse = "")
    Print(substring(s, 1L, n))
    PrintNewLine()
    SetPrintBuffer(substring(s, n + 1L))
  }
}

SetPrintBuffer <- function(s)
{
  # Sets the print buffer to character vector s.
  SetPro(printBuffer, s)
}

################################################################################
# Cursor position.

AdvanceCursorPosition <- function(n)
{
  # Moves the cursor n spaces to the right (whether or not that's on the page).
  SetCursorPosition(GetCursorPosition() + n)
}

GetCursorPosition <- function()
{
  # Returns the cursor position, that being the (zero-indexed) console column
  # from where the next chunk of printed output will begin. Not accurate when
  # special characters (\a, \t, etc.) have already been printed on the line.
  GetPro(cursorPosition, 0L)
}

ResetCursorPosition <- function()
{
  # Resets the cursor position to the first (zero-indexed) console column.
  SetCursorPosition(0L)
}

SetCursorPosition <- function(n)
{
  # Sets the cursor position to console column number n (zero-indexed).
  SetPro(cursorPosition, n)
}

VirtualCursorPosition <- function()
{
  # The position the cursor would have after printing the buffer without any
  # line break. Not accurate in the presence of special characters; \a, etc.
  GetCursorPosition() + CharactersInPrintBuffer()
}

################################################################################
# Overprinting and print-skipping.

GetCharactersOnLine <- function()
{
  # The character string already printed on the current line (needed in case of
  # a carriage return and subsequent TAB, SPC, or comma over-skipping).
  GetPro(printedChars, "")
}

SetCharactersOnLine <- function(s)
{
  # Sets the character string already printed on the current line.
  SetPro(printedChars, s)
}

SetNoCharactersOnLine <- function()
{
  # Sets the current line to be empty.
  SetCharactersOnLine("")
}

UpdateCharactersOnLine <- function(s)
{
  # Adds string s to the text printed on the current line, at the current cursor
  # position. Text may already extend right of the cursor, in which case some or
  # all of it is overprinted by s. Updates the cursor position accordingly.
  n <- nchar(s)
  k <- GetCursorPosition()
  p <- GetCharactersOnLine()
  u <- paste0(substring(p, 1L, k), s, substring(p, k + n + 1L))
  SetCharactersOnLine(u)
  AdvanceCursorPosition(n)
}

################################################################################
# Teletypewriter-effect controls.

CharacterPause <- function()
{
  # Returns however many seconds are to be dwelt after printing each character.
  GetPro(ttCharDelay, 0)
}

LinePause <- function()
{
  # Returns however many seconds are to be dwelt after printing each line.
  GetPro(ttLineDelay, 0)
}

SetTeletypeCharDelay <- function(m)
{
  # Sets the teletype character-pause aesthetic. The argument, m, is a delay
  # time, in milliseconds, to be dwelt after printing each character. The delay
  # is restricted to between 0 and 0.2 seconds, with zero being the default.
  # Real teletypes could manage around 10 characters per second.
  s <- if (is.numeric(m) && (length(m) == 1L) && is.finite(m)) m / 1000 else 0
  SetPro(ttCharDelay, max(0, min(s, 0.2)))
  if (s < 0) SetTeletypeLineDelay(-1)
}

SetTeletypeLineDelay <- function(m)
{
  # Sets the teletype line-pause aesthetic. The argument, m, is a delay time, in
  # milliseconds, to be dwelt after completing each line of printing. The delay
  # is limited to no more than two seconds, with zero as the default. Negative
  # values impose no delay, but flush the console after every line of printing.
  s <- if (is.numeric(m) && (length(m) == 1L) && is.finite(m)) m / 1000 else 0
  if (s == 0) s <- LinePause()
  SetPro(ttLineDelay, if (s < 0) -1 else min(s, 2))
}

SetUpperCaseOut <- function(u)
{
  # Sets the upper-case aesthetic flag. If u is anything besides TRUE, the flag
  # is set to FALSE (do not convert printed output to upper case).
  SetPro(upperCase, is.logical(u) && (length(u) == 1L) && !is.na(u) && u)
}

UpperCaseOut <- function()
{
  # Returns TRUE when printing is to be done in upper case, or FALSE otherwise.
  GetPro(upperCase, FALSE)
}

################################################################################
# Pseudorandom numbers.

GetLastRandomNumber <- function()
{
  # Returns the most recent previously generated pseudorandom number.
  GetPro(lastRandomNumber, 0)
}

GetNewRandomNumber <- function()
{
  # Generates and returns a new pseudorandom number from the standard uniform
  # distribution. Updates lastRandomNumber in case the value is wanted again.
  SetPro(lastRandomNumber, runif(1L))
}

################################################################################
# Run-state tests, for trapping errors and terminating execution.

Exit <- function()
{
  # Returns the exit trigger, that being a BASIC error or break object. This is
  # used to propagate the original error back up the function call stack.
  GetExitTrigger()
}

Running <- function()
{
  # Returns TRUE if and only if no exit message has been set (whether or not the
  # program-counter lies within the program). Otherwise returns FALSE.
  !exists(exitTrigger, progMemory, inherits = FALSE)
}

Terminated <- function()
{
  # Returns TRUE if and only if an exit message has been set (whether or not the
  # program-counter lies within the program). Otherwise returns FALSE.
  exists(exitTrigger, progMemory, inherits = FALSE)
}

############################################################################ EOF
