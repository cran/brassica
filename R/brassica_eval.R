# Evaluation of BASIC expressions (not containing a command keyword).
# MJL @ Titirangi, 3 August 2022.
# Last edit: 12 September 2022.

################################################################################
# BASIC functions.

functions <- c("ABS", "ASC", "ATN", "CHR$", "COS", "EXP", "INSTR", "INT",
                "LEFT$", "LEN", "LOG", "MID$", "POS", "RND", "RIGHT$", "SGN",
                "SIN", "SPC", "SQR", "STR$", "STRING$", "SYST", "TAB", "TAN",
                "TTW", "VAL")

################################################################################
# BASIC operators and their R-function implementations.

exponentiators  <- list("^" = "EvalRAISE")
arithUnaries    <- list("-" = "EvalNEGATE", "+" = "EvalIDENTITY")
multipliers     <- list("*" = "EvalMULTIPLY", "/" = "EvalDIVIDE")
integerDividers <- list("\\" = "EvalINTEGERDIVISION")
modulos         <- list("MOD" = "EvalMODULO")
adders          <- list("+" = "EvalADD", "-" = "EvalSUBTRACT")
relationals     <- list("=" = "EvalEQUAL", "<>" = "EvalNOTEQUAL",
                        "<" = "EvalLESS", ">=" = "EvalNOTLESS",
                        ">" = "EvalGREATER", "<=" = "EvalNOTGREATER")
logicalUnaries  <- list("NOT" = "EvalNOT")
logicalBinaries <- list("AND" = "EvalAND", "OR" = "EvalOR", "XOR" = "EvalXOR")

################################################################################
# Derived vectors of operator groups (names or symbols only).

# All logical operators: NOT, AND, OR, XOR.
logicals <- c(names(logicalUnaries),
              names(logicalBinaries))

# All symbolic operators: ^, -, *, \, <>, etc.
operators <- c(names(exponentiators), names(multipliers),
               names(integerDividers), names(adders), names(relationals))

# All alphabetic operators: MOD, NOT, AND, OR, XOR.
operatorWords <- c(names(modulos), logicals)

# All binary operators: ^, MOD, +, =, <=, AND, etc.
binaries <- c(operators, names(modulos), names(logicalBinaries))

################################################################################
# BASIC digits (includes .), for detecting numeric literals.

digits <- c(as.character(seq(0L, 9L)), ".")

################################################################################
# Top-level evaluators.

Eval <- function(s)
{
  # Evaluates the BASIC expression supplied in non-empty R character string s.
  if (Terminated()) return(Exit())
  if (IsEvaluated(s)) return(as.list(s))
  a <- list()
  while (nzchar(s))
  {
    p <- EvalFirstTerm(s)
    a <- append(a, p[-length(p)])
    s <- unname(p[[length(p)]])
  }
  if (IsUnevaluated(a)) a <- EvalUnaryGroups(a) else return(a)
  if (IsUnevaluated(a)) a <- ApplyBinaryOperators(a) else return(a)
  if (IsUnevaluated(a)) SetSyntaxError() else a
}

EvalFirstTerm <- function(s)
{
  # Delegates the evaluation of the BASIC expression in string s to a handler
  # specific to the leading (left-most) term in that expression. Returns the
  # result of evaluating that first term in a list along with any remaining
  # unevaluated portion of the expression. The SPC() and TAB() functions are
  # only permitted within PRINT statements (which is not here).
  if (BeginsWithAny(c("SPC", "TAB"), s)) return(SetSyntaxError())
  if (BeginsWithAny(LETTERS, s)) return(EvalName(s))
  if (BeginsWithAny(digits, s)) return(EvalNumber(s))
  if (BeginsWithAny(operators, s)) return(IsolateOperator(s))
  if (BeginsWith("(", s)) return(EvalGroup(s))
  if (BeginsWith("\"", s)) return(EvalString(s))
  Interrupt(SetSyntaxError())
}

################################################################################
# Component-specific evaluators.

EvalArguments <- function(s)
{
  # Evaluates each expression in a comma-separated list supplied as a single
  # string s. Returns a shallow list; list(basicN = 1, basicS = "A", ...).
  EvalExpressions(SplitArguments(s))
}

EvalExpressions <- function(x)
{
  # Evaluates each expression (string) in character vector x. This produces a
  # shallow list, list(basicN = 1, basicS = "A", ...), as opposed to the deep
  # lists from lapply; list(list(basicN = 1), list(basicS = "A"), ...). We use
  # append, rather than assigning to x[i], as append migrates names from Eval(a)
  # to x (i.e., from list members to the list itself), avoiding the need to
  # follow with names(x) <- sapply(x, names); lapply(x, unname).
  v <- list()
  for (a in x) v <- append(v, as.list(Eval(a)))
  v
}

EvalFunction <- function(s)
{
  # Evaluates the BASIC function at the beginning of expression s.
  f <- BeginsWithWhich(functions, s)
  i <- PositionOfClosingParenthesis(s <- Trim(s, f))
  a <- EvalArguments(substring(s, 2L, i - 1L))
  f <- paste0("Eval", toupper(sub("$", "_", f, fixed = TRUE)))
  Intermediate(eval(call(f, a)), substring(s, i + 1L))
}

EvalGroup <- function(s)
{
  # Evaluates the parenthesis-enclosed expression at the beginning of string s.
  i <- PositionOfClosingParenthesis(s)
  if (i < 1L) return(Interrupt(SetSyntaxError()))
  g <- substring(s, 2L, i - 1L)
  if (!nzchar(g)) return(Interrupt(SetSyntaxError()))
  Intermediate(Eval(g), substring(s, i + 1L))
}

EvalName <- function(s)
{
  # Evaluates the alphanumerically named object at the beginning of statement s.
  # This might be a variable, a function, or an operator word.
  if (BeginsWith("FN", s)) return(EvalUserFunction(s))
  if (BeginsWithAny(operatorWords, s)) return(IsolateOperatorWord(s))
  if (BeginsWithAny(functions, s)) return(EvalFunction(s))
  v <- DetachLeadingVariable(s)
  Intermediate(GetValue(v[[variable]]), v[[remainder]])
}

EvalNumber <- function(s)
{
  # Evaluates the character-representation integer or floating point number at
  # the beginning of string s to a numeric data type. In BASIC, spaces are
  # allowed anywhere (these will have been removed), parentheses are not allowed
  # anywhere, and E can only be followed by an (optional, signed) integer (read
  # as 0 in its absence). The actual conversion is done by StringToNumber().
  i <- attr(regexpr("^[[:digit:]]*[.]{0,1}[[:digit:]]*", s), "match.length")
  if (i >= nchar(s)) return(Intermediate(AsNumber(StringToNumber(s)), ""))
  a <- substring(s, i + 1L)
  j <- attr(regexpr("^[eE][+-]{0,1}[[:digit:]]*", a), "match.length")
  if (j > 0L) {i <- i + j; a <- substring(a, j + 1L)}
  Intermediate(AsNumber(StringToNumber(substr(s, 1L, i))), a)
}

EvalString <- function(s)
{
  # Evaluates the BASIC string literal at the beginning of expression string s.
  # (Removes the enclosing quotes.)
  i <- gregexpr("\"", s, fixed = TRUE)[[1L]]
  if (length(i) < 2L) return(Intermediate(AsString(substring(s, 2L)), ""))
  Intermediate(AsString(substring(s, 2L, i[2L] - 1L)), substring(s, i[2L] + 1L))
}

EvalSubscripts <- function(s)
{
  # Evaluates a vector of BASIC array-variable subscripts, s, to R integers, for
  # value getting and setting in heap memory. The subscripts might already be
  # evaluated, or they might be character strings of unevaluated expressions.
  if (is.integer(s)) return(s)
  if (length(s) < 1L) return(integer())
  i <- EvalExpressions(s)
  if (any(Type(i) != basicN)) return(SetTypeMismatchError())
  i <- as.integer(floor(sapply(i, Value)))
  if (any(i < 0L)) return(SetIllegalQuantityError())
  i
}

EvalUnaryGroups <- function(a)
{
  # Evaluates the (implied) groups over which unary operations +, -, and NOT
  # extend, before applying the unary operation to the result. For simplicity,
  # evaluation is performed right to left. Order of precedence is preserved,
  # with unary + applying only to the immediately adjacent element, unary -
  # extending as far as the next operator besides ^ (at position j), and unary
  # NOT extending as far as the first logical binary operator (at position k).
  j <- length(a) + 1L
  k <- length(a) + 1L
  o <- rev(which(Type(a) == basicO))
  for (i in o)
  {
    n <- Value(a[i])
    # When the operator is a unary NOT, evaluate its group, then apply the NOT.
    # With right to left evaluation, groups cannot contain any unary operators,
    # so we need only apply any binaries.
    if (n == "NOT")
    {
      if (k - i < 2L) return(SetSyntaxError())
      b <- ApplyBinaryOperators(a[seq(i + 1L, k - 1L)])
      a <- append(a[-seq(i, k - 1L)], EvalNOT(b), after = i - 1L)
      k <- i + 1L
      j <- k
      next
    }
    # Unary + and - are distinguishable from their binary counterparts by their
    # immediately following either another operator, or nothing (being in the
    # first position). Unary + isn't quite the same as doing nothing, since it
    # raises an error if its operand isn't a number or string.
    if (IsUnevaluated(a[i - 1L]))
    {
      if (n == "-")
      {
        if (j - i < 2L) return(SetSyntaxError())
        b <- ApplyBinaryOperators(a[seq(i + 1L, j - 1L)])
        a <- append(a[-seq(i, j - 1L)], EvalNEGATE(b), after = i - 1L)
        k <- k - j + i + 1L
        j <- i + 1L
        next
      }
      if (n == "+")
      {
        a <- append(a[-c(i, i + 1L)], EvalIDENTITY(a[i + 1L]), after = i - 1L)
        j <- j - 1L
        k <- k - 1L
        next
      }
    }
    # When the operator is binary, reassess the end-points of the extent of any
    # subsequent (leftward) arithmetical (j) or logical (k) unary operators.
    if (n != "^")
    {
      j <- i
      if (any(n == logicals)) k <- i
    }
  }
  a
}

EvalUserFunction <- function(s)
{
  # Evaluates the user-defined BASIC function at the beginning of string s.
  # Note that function parameters, as in f[["parameters"]], are stored as names
  # of the underlying heap-memory R storage-objects, and not as BASIC variables.
  a <- DetachLeadingVariable(Trim(s, "FN"))
  if (Terminated()) return(Interrupt(Exit()))
  v <- a[[variable]]
  n <- paste0(substring(s, 1L, 2L), NameOfVariable(v))
  y <- TypeFromName(n)
  f <- RecallFunction(n)
  p <- f[["parameters"]]
  u <- EvalExpressions(SubscriptsOfVariable(v))
  if (!ArgsMatch(TypeFromName(p), u)) return(Interrupt(Exit()))
  z <- list()
  for (i in seq_along(p))
  {
    w <- if (ObjectExists(p[i])) GetObject(p[i]) else InitialDatum(p[i])
    z <- append(z, list(w))
    v <- if (IsIntegerConstrained(p[i])) floor(Value(u[i])) else Value(u[i])
    SetObject(p[i], v)
  }
  x <- Eval(f[["definition"]])
  for (i in seq_along(p)) SetObject(p[i], z[[i]])
  if (Type(x) != y) return(Interrupt(SetTypeMismatchError()))
  if (IsIntegerConstrained(n)) x <- AsNumber(floor(Value(x)))
  Intermediate(x, a[[remainder]])
}

################################################################################
# Evaluation assistants.

ArgsMatch <- function(a, x)
{
  # Returns TRUE if the evaluated BASIC values in list x exactly match the types
  # named by character vector a. Raises an error and returns FALSE otherwise.
  # This function is only called after complete evaluation of all the arguments
  # in x. Hence all are of BASIC number of string type (no NULLs or operators,
  # or untyped values), otherwise an error would have been raised by Eval().
  if ((length(x) == length(a)) && all(Type(x) == a)) return(TRUE)
  if (length(x) != length(a)) SetSyntaxError() else SetTypeMismatchError()
  FALSE
}

BeginsWith <- function(w, s)
{
  # Returns TRUE where the strings of character vector s begin with word w, and
  # FALSE elsewhere. Alternatively, s can be a string and w a character vector.
  # Case insensitive.
  toupper(w) == toupper(substring(s, 1L, nchar(w)))
}

BeginsWithAny <- function(w, s)
{
  # Returns TRUE if string s begins with any of the strings in character vector
  # w. Alternatively, s can be a character vector and w a character string.
  # Case insensitive.
  any(BeginsWith(w, s))
}

BeginsWithWhich <- function(w, s)
{
  # Returns the longest string from character vector w that appears at the
  # beginning of string s (i.e., the greediest match). Case insensitive. Returns
  # an empty character vector when no string in w matches the beginning of s.
  a <- w[BeginsWith(w, s)]
  a[which.max(nchar(a))]
}

IsEvaluated <- function(x)
{
  # Returns TRUE if x is a fully evaluated BASIC value (a string or number).
  # Otherwise returns FALSE, including if x is NULL or empty.
  !IsUnevaluated(x)
}

IsFunction <- function(s)
{
  # Returns TRUE if s is the name of a BASIC function (ABS, MID$, et cetera).
  # Otherwise returns FALSE, including if s is empty or NULL.
  (length(s) == 1L) && any(functions == toupper(s), na.rm = TRUE)
}

IsolateOperator <- function(s)
{
  # Detaches an arithmetical or relational operator from the beginning of
  # statement s. The operator is not applied at this time.
  o <- BeginsWithWhich(operators, s)
  Intermediate(AsOperator(o), substring(s, nchar(o) + 1L))
}

IsolateOperatorWord <- function(s)
{
  # Detaches an operator word from the beginning of statement s.
  # The operator is not applied at this time.
  o <- BeginsWithWhich(operatorWords, s)
  Intermediate(AsOperator(o), substring(s, nchar(o) + 1L))
}

IsUnevaluated <- function(x)
{
  # Returns FALSE if x is a fully evaluated BASIC value (a string or number).
  # Otherwise returns TRUE, including if x is NULL or empty or non-singleton.
  (length(x) != 1L) || all(Type(x) != c(basicN, basicS), na.rm = TRUE)
}

Intermediate <- function(x, s)
{
  # Bundles the result, x, of evaluating part of a statement, with the remaining
  # (unevaluated) part, s (a character string), into list. While x ought to be a
  # named list, we allow for unnamed, a vector, or NULL. In general, x can be a
  # list of partial results (say, an unapplied operator followed by an evaluated
  # argument).
  if (is.null(Type(x))) x <- AsError(x)
  append(as.list(x), if (length(s) > 0L) unname(s) else "")
}

Interrupt <- function(e)
{
  # A special version of Intermediate() for when an error has occurred. Sets the
  # remaining (unevaluated) expression to the empty string, leaving nothing to
  # evaluate (ends evaluation, even if the error itself doesn't).
  Intermediate(e, "")
}

PositionOfClosingParenthesis <- function(s)
{
  # If the first character in statement s is an opening parenthesis, we return
  # the position (index) of the corresponding closing parenthesis. If that
  # closing parenthesis does not exist within s, then an error is raised and -1L
  # is returned. If the first character in s is not an opening parenthesis, then
  # 0L is returned (no error is raised in this case). While we perhaps ought to
  # check for REMs here (which would hide any parentheses to the right), we get
  # the same syntax error on attempting to evaluate the REM, anyway. In original
  # BASIC, 'MID$("ABC", "D", REM)' and 'MID$("ABC", "D"' are both type mismatch
  # errors (on the "D"), before getting to the REM or missing parenthesis, while
  # we'll get a syntax error as all arguments are evaluated before type checks.
  if (!BeginsWith("(", s)) return(0L)
  iq <- PositionsOf("\"", s)
  io <- Unenclosed("(", s, iq)
  ic <- c(0L, Unenclosed(")", s, iq))
  jc <- (sapply(lapply(ic, ">", io), sum) - seq_along(ic) + 1L)[-1L]
  if (all(jc)) {SetSyntaxError(); return(-1L)}
  ic[which.min(abs(jc)) + 1L]
}

PositionsOf <- function(w, s)
{
  # Returns all positions of word w within string s. Returns an empty vector
  # when w does not appear within s. Case sensitive.
  i <- gregexpr(w, s, fixed = TRUE)[[1L]]
  i[i > 0L]
}

SplitArguments <- function(s)
{
  # Splits a character string, s, of comma-separated (unevaluated) statements
  # into a vector of those same statements. As in PositionOfClosingParenthesis,
  # we do not check for REMs. If one is present, we'll still get a syntax error
  # when we attempt to evaluate it.
  if (!grepl(",", s, fixed = TRUE)) return(s)
  if (!grepl("[\"(]", s))
    return(c(strsplit(s, ",", fixed = TRUE)[[1L]], rep("", grepl(",$", s))))
  iq <- PositionsOf("\"", s)
  io <- Unenclosed("(", s, iq)
  ic <- Unenclosed(")", s, iq)
  id <- c(0L, Unbracketed(Unenclosed(",", s, iq), io, ic), nchar(s) + 1L)
  substring(s, id[-length(id)] + 1L, id[-1L] - 1L)
}

StringToNumber <- function(s)
{
  # Converts the character-string representation of an integer or floating point
  # number, s, to numerical type, without flagging it as being an evaluated
  # BASIC number. Neither R nor BASIC allow parentheses within the string, nor E
  # being followed by a non-integer power, nor E being the first character in
  # the string. Both allow '1E', '1E+', and '1E-' (these are all read as 1E0).
  # BASIC allows blank spaces while R does not, but these will have already been
  # removed. Unlike R, BASIC allows '.', '.E', '.E+4', etc., so we catch this as
  # a special case ('.' without digits is read as floating-point zero).
  if (grepl("^\\.(E|$)", s, ignore.case = TRUE)) return(0)
  n <- try(suppressWarnings(as.numeric(s)), silent = TRUE)
  if (!is.finite(n) || inherits(n, "try-error")) return(SetSyntaxError())
  n
}

Unbracketed <- function(x, a, b)
{
  # Returns the positions, x, along some string, that are not contained between
  # opening and closing brackets at positions a and b, respectively. Any or all
  # of these can be empty. Returns an empty vector when no unbracketed positions
  # appear within x. The 'k' in '(k' is considered to be contained (even in the
  # absence of a closing bracket).
  x[as.logical(sapply(x, function(y, i, j) sum(y > j) >= sum(y > i), a, b))]
}

Unenclosed <- function(w, s, i)
{
  # Returns all positions of word w within string s that are not inside a pair
  # of delimiting characters (or words, typically quotation marks) at positions
  # i along the same string, according to the even/odd delimiter-count rule.
  # Case sensitive. The delimiter-position vector, i, can be empty. Returns an
  # empty vector when there are no unenclosed occurrences of w within s.
  u <- PositionsOf(w, s)
  u[as.logical(sapply(u, function(a, b) sum(a > b) %% 2L == 0L, i))]
}

################################################################################
# Binary-operator applicators.

ApplyAdders <- function(a)
{
  # Applies any addition or subtraction operators appearing within the list of
  # BASIC statement components, a. Addition includes string concatenation.
  ApplyCollection(adders, a)
}

ApplyBinaryOperators <- function(a)
{
  # Applies all BASIC binary operators found within the list of (otherwise)
  # evaluated expression components, a, in order of decreasing precedence.
  # There can be no groupings or unary operators within the list.
  a <- ApplyExponentiators(a); if (IsEvaluated(a)) return(a)
  a <- ApplyMultipliers(a); if (IsEvaluated(a)) return(a)
  a <- ApplyIntegerDividers(a); if (IsEvaluated(a)) return(a)
  a <- ApplyModulos(a); if (IsEvaluated(a)) return(a)
  a <- ApplyAdders(a); if (IsEvaluated(a)) return(a)
  a <- ApplyRelationals(a); if (IsEvaluated(a)) return(a)
  ApplyLogicals(a)
}

ApplyCollection <- function(f, a)
{
  # Applies any of the members of a list of equal-precedence operators, f,
  # wherever they appear within the list of BASIC statement components, a, in
  # left-to-right order. An example of f is list("^" = "EvalRAISE").
  # The Terminated() check is necessary in case of adjacent operators.
  z <- WhichAreOperators(Type(f), a)
  for (h in seq_along(z))
  {
    j <- z[h]; i <- j - 1L; k <- j + 1L
    x <- eval(call(f[[Value(a[j])]], a[i], a[k]))
    if (Terminated()) return(SetSyntaxError())
    a <- append(a[-c(i, j, k)], x, after = i - 1L)
    z <- z - 2L
  }
  a
}

ApplyExponentiators <- function(a)
{
  # Applies any exponentiation (power) operators appearing within the list of
  # BASIC statement components, a, in left-to-right order.
  ApplyCollection(exponentiators, a)
}

ApplyIntegerDividers <- function(a)
{
  # Applies any integer-division operators appearing within the list of BASIC
  # statement components, a, in left-to-right order.
  ApplyCollection(integerDividers, a)
}

ApplyLogicals <- function(a)
{
  # Applies any binary logical operators appearing within the list of BASIC
  # statement components, a, in order of decreasing precedence.
  o <- logicalBinaries
  for (i in seq_along(o)) a <- ApplyCollection(o[i], a)
  a
}

ApplyModulos <- function(a)
{
  # Applies any modulo operators appearing within the list of BASIC statement
  # components, a, in left-to-right order.
  ApplyCollection(modulos, a)
}

ApplyMultipliers <- function(a)
{
  # Applies any multiplication or division operators appearing within the list
  # of BASIC statement components, a.
  ApplyCollection(multipliers, a)
}

ApplyRelationals <- function(a)
{
  # Applies any relational operators appearing within the list of BASIC
  # statement components, a, in left-to-right order.
  ApplyCollection(relationals, a)
}

WhichAreOperators <- function(s, a)
{
  # Returns the position indices, within the list of BASIC statement components,
  # a, of any member of the vector of operator symbols, s.
  i <- which(Type(a) == basicO)
  i[as.character(a[i]) %in% s]
}

################################################################################
# BASIC function evaluators.

EvalABS <- function(x)
{
  # Returns the absolute value of the BASIC number x.
  if (!ArgsMatch(basicN, x)) return(Exit())
  AsNumber(abs(Value(x)))
}

EvalASC <- function(x)
{
  # Returns the ASCII code of the first character of the BASIC string x.
  if (!ArgsMatch(basicS, x)) return(Exit())
  if (!nzchar(Value(x))) return(SetIllegalQuantityError())
  AsNumber(utf8ToInt(substring(Value(x), 1L, 1L)))
}

EvalATN <- function(x)
{
  # Returns the arctangent of the BASIC number x.
  if (!ArgsMatch(basicN, x)) return(Exit())
  AsNumber(atan(Value(x)))
}

EvalCHR_ <- function(x)
{
  # Returns the ASCII character whose code is the greatest integer not greater
  # than the BASIC number x.
  if (!ArgsMatch(basicN, x)) return(Exit())
  if (Value(x) < 0) return(SetIllegalQuantityError())
  AsString(intToUtf8(as.integer(floor(Value(x)))))
}

EvalCOS <- function(x)
{
  # Returns the cosine of the BASIC number x.
  if (!ArgsMatch(basicN, x)) return(Exit())
  AsNumber(cos(Value(x)))
}

EvalEXP <- function(x)
{
  # Returns the exponential of the BASIC number x.
  if (!ArgsMatch(basicN, x)) return(Exit())
  AsNumber(exp(Value(x)))
}

EvalINSTR <- function(x)
{
  # Locates one string within another. List x contains two or three arguments:
  # integer N, string S, and word W, with N being optional (defaults to unity).
  # Returns the position of the first (case-insensitive) occurrence of W within
  # S not before the Nth character. Returns zero when S is empty, or has fewer
  # than N characters, or when W does not appear within S at or after the Nth
  # character. W can be the empty string (matches at N).
  if (length(x) == 2L) x <- append(AsNumber(1), x)
  if (!ArgsMatch(c(basicN, basicS, basicS), x)) return(Exit())
  i <- max(1L, as.integer(floor(x[[1L]])))
  if (i > nchar(x[[2L]])) return(AsNumber(0))
  j <- regexpr(toupper(x[[3L]]), toupper(substring(x[[2L]], i)), fixed = TRUE)
  if (j < 1L) return(AsNumber(0))
  AsNumber(i + j - 1L)
}

EvalINT <- function(x)
{
  # Returns the greatest integer not greater than the BASIC number x.
  if (!ArgsMatch(basicN, x)) return(Exit())
  AsNumber(floor(Value(x)))
}

EvalLEFT_ <- function(x)
{
  # Returns the leftmost x[2] characters of BASIC string x[1], or the whole of
  # x[1] if it has fewer than x[2] characters.
  if (!ArgsMatch(c(basicS, basicN), x)) return(Exit())
  if (x[[2L]] < 0) return(SetIllegalQuantityError())
  AsString(substring(x[[1L]], 0L, as.integer(floor(x[[2L]]))))
}

EvalLEN <- function(x)
{
  # Returns the number of characters in BASIC string x.
  if (!ArgsMatch(basicS, x)) return(Exit())
  AsNumber(nchar(Value(x)))
}

EvalLOG <- function(x)
{
  # Returns the natural logarithm of BASIC number x.
  if (!ArgsMatch(basicN, x)) return(Exit())
  if (Value(x) <= 0) return(SetIllegalQuantityError())
  AsNumber(log(Value(x)))
}

EvalMID_ <- function(x)
{
  # Returns x[3] characters from BASIC string x[1], beginning from the x[2]th
  # character. If x[3] is omitted, or if x[2] + x[3] exceeds the number of
  # characters in x[1], then all characters in x[1], from the x[2]th onward,
  # are returned. If x[2] exceeds the number of characters in x[1], then an
  # empty string is returned.
  if (!ArgsMatch(c(basicS, basicN, basicN)[-length(x) - 1L], x)) return(Exit())
  if ((x[[2L]] < 1) || (x[[length(x)]] < 0)) return(SetIllegalQuantityError())
  i <- as.integer(floor(x[[2L]]))
  j <- if (length(x) == 3L) as.integer(floor(x[[3L]])) else nchar(x[[1L]])
  AsString(substring(x[[1L]], i, i + j - 1L))
}

EvalPOS <- function(x)
{
  # Returns the (virtual) cursor position from the left margin, starting from 0.
  # Not accurate when special characters are on the line (\a, \t, et cetera).
  if (!ArgsMatch(basicN, x)) return(Exit())
  if (Value(x) != 1) return(SetIllegalQuantityError())
  AsNumber(VirtualCursorPosition())
}

EvalRND <- function(x)
{
  # Returns a pseudo-random variate from the uniform distribution over (0, 1).
  # When x is zero, the previous value is returned (rather than a new one).
  # When x is negative, the generator is seeded with as.integer(floor(x)).
  if (!ArgsMatch(basicN, x)) return(Exit())
  v <- Value(x)
  if (v > 0) return(AsNumber(GetNewRandomNumber()))
  if (v == 0) return(AsNumber(GetLastRandomNumber()))
  AsNumber(SeedRandomNumbers(as.integer(floor(v))))
}

EvalRIGHT_ <- function(x)
{
  # Returns the rightmost x[2] characters of BASIC string x[1], or the whole of
  # x[1] if it has fewer than x[2] characters.
  if (!ArgsMatch(c(basicS, basicN), x)) return(Exit())
  if (x[[2L]] < 0) return(SetIllegalQuantityError())
  n <- nchar(x[[1L]])
  AsString(substring(x[[1L]], n - as.integer(floor(x[[2L]])) + 1L, n))
}

EvalSGN <- function(x)
{
  # Returns the signum function of BASIC number x.
  if (!ArgsMatch(basicN, x)) return(Exit())
  AsNumber(sign(Value(x)))
}

EvalSIN <- function(x)
{
  # Returns the sine of BASIC number x.
  if (!ArgsMatch(basicN, x)) return(Exit())
  AsNumber(sin(Value(x)))
}

EvalSPC <- function(x)
{
  # Rapidly moves the cursor x spaces to the right or left, wrapping at the
  # width of the console (which may trigger one or more newlines). Wrapping may
  # be inaccurate when special characters (\t, etc.) are present on the line.
  if (!ArgsMatch(basicN, x)) return(Exit())
  n <- as.integer(floor(Value(x)))
  if (n < 0L) {RetractPrintHead(-n); return(AsOperator(";"))}
  k <- VirtualCursorPosition()
  w <- as.integer(options("width"))
  m <- (k + n) - w
  if (m > 0L) {PrintBufferAndNewline(); n <- m}
  while (n > w) {PrintNewLine(); n <- n - w}
  AdvancePrintHead(n)
  AsOperator(";")
}

EvalSQR <- function(x)
{
  # Returns the square root of BASIC number x.
  if (!ArgsMatch(basicN, x)) return(Exit())
  if (Value(x) < 0) return(SetIllegalQuantityError())
  AsNumber(sqrt(Value(x)))
}

EvalSTR_ <- function(x)
{
  # Returns a character-string representation of BASIC number x.
  if (!ArgsMatch(basicN, x)) return(Exit())
  s <- toupper(format(Value(x), digits = printPrecision))
  AsString(sub("0.", ".", s, fixed = TRUE))
}

EvalSTRING_ <- function(x)
{
  # Returns a BASIC string consisting of x[1] copies of string x[2] (or spaces,
  # if x[2] is omitted).
  if (!ArgsMatch(c(basicN, basicS)[-length(x) - 1L], x)) return(Exit())
  n <- as.integer(floor(x[[1L]]))
  if (n < 0L) return(SetIllegalQuantityError())
  AsString(strrep(if (length(x) == 2L) x[[2L]] else " ", n))
}

EvalSYST <- function(x)
{
  # Returns the current system date-time, in seconds.
  if (!ArgsMatch(basicN, x)) return(Exit())
  if (Value(x) != 1) return(SetIllegalQuantityError())
  AsNumber(Sys.time())
}

EvalTAB <- function(x)
{
  # Positions the cursor such that the next character will be printed in column
  # x. We extend to allow negative values to mean -x spaces from the right-hand
  # margin. If the cursor already lies to the right of the requested position, a
  # new line is begun. Positioning will be inaccurate when special characters
  # (\a, \b, \t, etc.) have been placed on the line.
  if (!ArgsMatch(basicN, x)) return(Exit())
  p <- as.integer(floor(Value(x))) %% as.integer(options("width"))
  k <- VirtualCursorPosition()
  if (p < k) {PrintBufferAndNewline(); k <- 0L}
  AdvancePrintHead(p - k)
  AsOperator(";")
}

EvalTAN <- function(x)
{
  # Returns the tangent of BASIC number x.
  if (!ArgsMatch(basicN, x)) return(Exit())
  AsNumber(tan(Value(x)))
}

EvalTTW <- function(x)
{
  # Returns the current width of the terminal, as a number of characters.
  if (!ArgsMatch(basicN, x)) return(Exit())
  if (Value(x) != 1) return(SetIllegalQuantityError())
  AsNumber(options("width"))
}

EvalVAL <- function(x)
{
  # Treats BASIC string x as the character representation of a numerical value,
  # and returns the corresponding BASIC number. When x cannot be identified as
  # a number, BASIC 0 is returned (no error is raised).
  if (!ArgsMatch(basicS, x)) return(Exit())
  n <- try(suppressWarnings(as.numeric(Value(x))), silent = TRUE)
  if (!is.finite(n) || inherits(n, "try-error")) return(AsNumber(0))
  AsNumber(n)
}

################################################################################
# Arithmetical operations.

EvalADD <- function(x, y)
{
  # Applies the addition operator between two BASIC numbers or two BASIC strings
  # (performs concatenation), x and y.
  if (IsString(x) && IsString(y)) return(AsString(paste0(Value(x), Value(y))))
  if (IsNumber(x) && IsNumber(y)) return(AsNumber(Value(x) + Value(y)))
  if (IsUnevaluated(x) || IsUnevaluated(y)) return(SetSyntaxError())
  SetTypeMismatchError()
}

EvalDIVIDE <- function(x, y)
{
  # Applies the division operator between two BASIC numbers, x (the dividend)
  # and y (the divisor).
  if (IsUnevaluated(x) || IsUnevaluated(y)) return(SetSyntaxError())
  if (!IsNumber(x) || !IsNumber(y)) return(SetTypeMismatchError())
  if (Value(y) == 0) return(SetDivisionByZeroError())
  AsNumber(Value(x) / Value(y))
}

EvalIDENTITY <- function(x)
{
  # Applies the unary identity operation to a BASIC string or number, x.
  # This function is only called after x has been fully evaluated. BASIC allows
  # this operation upon strings (e.g., +"A"). Expressions such as '3+' and '+'
  # are not allowed; the operator must still act on a valid data value.
  if (IsUnevaluated(x)) return(SetSyntaxError())
  x
}

EvalINTEGERDIVISION <- function(x, y)
{
  # Applies the integer-division operator between two BASIC numbers, x (the
  # dividend) and y (the divisor). As in original (extended) Altair BASIC, both
  # operands are truncated to integers before the operation, and the result is
  # rounded toward zero.
  if (IsUnevaluated(x) || IsUnevaluated(y)) return(SetSyntaxError())
  if (!IsNumber(x) || !IsNumber(y)) return(SetTypeMismatchError())
  a <- floor(Value(x))
  b <- floor(Value(y))
  if (b == 0) return(SetDivisionByZeroError())
  v <- a / b
  AsNumber(sign(v) * floor(abs(v)))
}

EvalMODULO <- function(x, y)
{
  # Applies the modulo operator between two BASIC numbers, x (the dividend) and
  # y (the modulus). As in original (extended) Altair BASIC, the operation is
  # defined as A MOD B = A - B * (A\B), with both operands being truncated to
  # integers beforehand.
  if (IsUnevaluated(x) || IsUnevaluated(y)) return(SetSyntaxError())
  if (!IsNumber(x) || !IsNumber(y)) return(SetTypeMismatchError())
  a <- floor(Value(x))
  b <- floor(Value(y))
  if (b == 0) return(SetDivisionByZeroError())
  v <- a / b
  AsNumber(a - b * sign(v) * floor(abs(v)))
}

EvalMULTIPLY <- function(x, y)
{
  # Applies the multiplication operator between two BASIC numbers, x and y.
  if (IsNumber(x) && IsNumber(y)) return(AsNumber(Value(x) * Value(y)))
  if (IsUnevaluated(x) || IsUnevaluated(y)) return(SetSyntaxError())
  SetTypeMismatchError()
}

EvalNEGATE <- function(x)
{
  # Applies the unary negation operator to a BASIC number, x.
  # This function is only called after x has been fully evaluated.
  # BASIC does not allow negation of strings.
  if (IsNumber(x)) return(AsNumber(-Value(x)))
  if (IsUnevaluated(x)) return(SetSyntaxError())
  SetTypeMismatchError()
}

EvalRAISE <- function(x, y)
{
  # Applies the exponentiation operator between two BASIC numbers, x (the base)
  # and y (the power).
  if (IsUnevaluated(x) || IsUnevaluated(y)) return(SetSyntaxError())
  if (!IsNumber(x) || !IsNumber(y)) return(SetTypeMismatchError())
  a <- Value(x); b <- Value(y)
  if ((a < 0) && (b != floor(b))) return(SetIllegalQuantityError())
  if ((a == 0) && (b < 0)) return(SetDivisionByZeroError())
  AsNumber(a ^ b)
}

EvalSUBTRACT <- function(x, y)
{
  # Applies the subtraction operator between two BASIC numbers, x (the
  # subtrahend) and y (the minuend).
  if (IsNumber(x) && IsNumber(y)) return(AsNumber(Value(x) - Value(y)))
  if (IsUnevaluated(x) || IsUnevaluated(y)) return(SetSyntaxError())
  SetTypeMismatchError()
}

################################################################################
# Relational operations.

EvalEQUAL <- function(x, y)
{
  # Returns BASIC -1 (true) if BASIC value x equals BASIC value y.
  # Otherwise, returns BASIC 0 (false). String comparisons are case-insensitive.
  if (IsUnevaluated(x) || IsUnevaluated(y)) return(SetSyntaxError())
  if (Type(x) != Type(y)) return(SetTypeMismatchError())
  AsNumber(-as.numeric(Caseless(x) == Caseless(y)))
}

EvalGREATER <- function(x, y)
{
  # Returns BASIC -1 (true) if BASIC value x is greater than BASIC value y.
  # Otherwise, returns BASIC 0 (false). String comparisons are case-insensitive.
  if (IsUnevaluated(x) || IsUnevaluated(y)) return(SetSyntaxError())
  if (Type(x) != Type(y)) return(SetTypeMismatchError())
  AsNumber(-as.numeric(Caseless(x) > Caseless(y)))
}

EvalLESS <- function(x, y)
{
  # Returns BASIC -1 (true) if BASIC value x is less than BASIC value y.
  # Otherwise, returns BASIC 0 (false). String comparisons are case-insensitive.
  if (IsUnevaluated(x) || IsUnevaluated(y)) return(SetSyntaxError())
  if (Type(x) != Type(y)) return(SetTypeMismatchError())
  AsNumber(-as.numeric(Caseless(x) < Caseless(y)))
}

EvalNOTEQUAL <- function(x, y)
{
  # Returns BASIC -1 (true) if BASIC value x does not equal BASIC value y.
  # Otherwise, returns BASIC 0 (false). String comparisons are case-insensitive.
  if (IsUnevaluated(x) || IsUnevaluated(y)) return(SetSyntaxError())
  if (Type(x) != Type(y)) return(SetTypeMismatchError())
  AsNumber(-as.numeric(Caseless(x) != Caseless(y)))
}

EvalNOTGREATER <- function(x, y)
{
  # Returns BASIC -1 (true) if BASIC value x is less than or equal to BASIC
  # value y. Otherwise, returns BASIC 0 (false).
  # String comparisons are case-insensitive.
  if (IsUnevaluated(x) || IsUnevaluated(y)) return(SetSyntaxError())
  if (Type(x) != Type(y)) return(SetTypeMismatchError())
  AsNumber(-as.numeric(Caseless(x) <= Caseless(y)))
}

EvalNOTLESS <- function(x, y)
{
  # Returns BASIC -1 (true) if BASIC value x is greater than or equal to BASIC
  # value y. Otherwise, returns BASIC 0 (false).
  # String comparisons are case-insensitive.
  if (IsUnevaluated(x) || IsUnevaluated(y)) return(SetSyntaxError())
  if (Type(x) != Type(y)) return(SetTypeMismatchError())
  AsNumber(-as.numeric(Caseless(x) >= Caseless(y)))
}

################################################################################
# Logical bitwise operations.

EvalAND <- function(a, b)
{
  # Performs bitwise logical AND on BASIC numbers a and b, after coercing both
  # to integer format.
  if (IsNumber(a) && IsNumber(b))
    return(AsNumber(bitwAnd(as.integer(Value(a)), as.integer(Value(b)))))
  if (IsUnevaluated(a) || IsUnevaluated(b)) return(SetSyntaxError())
  SetTypeMismatchError()
}

EvalNOT <- function(a)
{
  # Performs bitwise logical NOT on BASIC number a, after coercing it to integer
  # format. Within this format, NOT A = -(A + 1). Hence, NOT NOT A = A,
  # NOT 0 = -1, and NOT -1 = 0, consistent with BASIC's false = 0, true = -1.
  if (IsNumber(a)) return(AsNumber(bitwNot(as.integer(Value(a)))))
  if (IsUnevaluated(a)) return(SetSyntaxError())
  SetTypeMismatchError()
}

EvalOR <- function(a, b)
{
  # Performs bitwise logical OR on BASIC numbers a and b, after coercing both
  # to integer format.
  if (IsNumber(a) && IsNumber(b))
    return(AsNumber(bitwOr(as.integer(Value(a)), as.integer(Value(b)))))
  if (IsUnevaluated(a) || IsUnevaluated(b)) return(SetSyntaxError())
  SetTypeMismatchError()
}

EvalXOR <- function(a, b)
{
  # Performs bitwise logical XOR on BASIC numbers a and b, after coercing both
  # to integer format.
  if (IsNumber(a) && IsNumber(b))
    return(AsNumber(bitwXor(as.integer(Value(a)), as.integer(Value(b)))))
  if (IsUnevaluated(a) || IsUnevaluated(b)) return(SetSyntaxError())
  SetTypeMismatchError()
}

############################################################################ EOF
