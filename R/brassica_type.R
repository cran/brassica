# BASIC data type definitions, casting, and testing.
# MJL @ Titirangi, 3 August 2022.

# BASIC expressions are evaluated to data objects (mainly numbers and strings,
# but also errors and operators), consisting of a value wrapped in a named list
# (the name being that of the data type). Even so, many functions throughout
# will equally handle named, unnamed, or empty lists and vectors, and NULLs.
# Errors return data objects as a simple expediency. Likewise, operators are
# treated as data objects until such time as they are applied.

################################################################################
# BASIC data-type names.

basicB <- "basicB"    # Break message.
basicE <- "basicE"    # Error message.
basicN <- "basicN"    # Numerical value.
basicO <- "basicO"    # Operator.
basicS <- "basicS"    # Character string.

################################################################################
# BASIC-datum type and value extractors.

Caseless <- function(x)
{
  # Returns the underlying R value of BASIC datum x, but raised to upper case
  # when x is a string. Used to make case-insensitive relational comparisons.
  if (IsString(x)) toupper(Value(x)) else Value(x)
}

Type <- function(x)
{
  # Returns the type (name) of BASIC datum x.
  # In general, x can be a list of data, resulting in a vector of types.
  names(x)
}

Value <- function(x)
{
  # Returns the underlying (unnamed) R value of (named) BASIC datum x.
  x[[1L]]
}

################################################################################
# BASIC type casters.

AsBreak <- function(m)
{
  # Flags m as being a break result (message).
  # The unname(unlist()) calls remove any list wrapper and any prior names.
  list(basicB = unname(unlist(m)))
}

AsError <- function(m)
{
  # Flags m as being an error result (message).
  # The unname(unlist()) calls remove any list wrapper and any prior names.
  list(basicE = unname(unlist(m)))
}

AsNumber <- function(n)
{
  # Flags n as being an evaluated BASIC numerical value.
  # The as.numeric() call removes any list wrapper, removes any prior names, and
  # casts R integers to R numerics.
  list(basicN = as.numeric(n))
}

AsOperator <- function(s)
{
  # Flags s as being an unapplied BASIC operator.
  # The as.character() call removes any list wrapper and any prior names.
  list(basicO = as.character(s))
}

AsString <- function(s)
{
  # Flags s as being an evaluated BASIC character string.
  # The as.character() call removes any list wrapper and any prior names.
  list(basicS = as.character(s))
}

################################################################################
# BASIC type checkers.

IsBreak <- function(x)
{
  # Returns TRUE if x is a BASIC break result (message).
  # Otherwise returns FALSE, including if x is NULL or untyped.
  IsType(x, basicB)
}

IsError <- function(x)
{
  # Returns TRUE if x is a BASIC error result (message).
  # Otherwise returns FALSE, including if x is NULL or untyped.
  IsType(x, basicE)
}

IsNumber <- function(x)
{
  # Returns TRUE if x is a fully evaluated BASIC number.
  # Otherwise returns FALSE, including if x is NULL or untyped.
  IsType(x, basicN)
}

IsOperator <- function(x)
{
  # Returns TRUE if x is an unapplied BASIC operator.
  # Otherwise returns FALSE, including if x is NULL or untyped.
  IsType(x, basicO)
}

IsString <- function(x)
{
  # Returns TRUE if x is a fully evaluated BASIC string.
  # Otherwise returns FALSE, including if x is NULL or untyped.
  IsType(x, basicS)
}

IsType <- function(x, y)
{
  # Returns TRUE if and only if x is a BASIC datum of type y.
  # Otherwise returns FALSE, including if x is NULL or untyped.
  (length(x) == 1L) && any(Type(x) == y, na.rm = TRUE)
}

############################################################################ EOF
