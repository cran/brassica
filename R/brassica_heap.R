# Implements storage of BASIC data within an R environment.
# MJL @ Titirangi, 3 August 2022.

################################################################################
# Storage space for BASIC variables, functions, and arrays.

heapMemory <- new.env(parent = emptyenv())

################################################################################
# BASIC-variable initialisation constants.

initialString <- ""     # Initial value for a string-type variable.
initialNumber <- 0      # Initial value for a number-type variable.
defaultExtent <- 10L    # Default extent for each dimension of an array.

################################################################################
# Clear heap memory.

ResetHeapMemory <- function()
{
  # Delete all BASIC data storage objects (un-assigns all variables).
  remove(list = ls(heapMemory), pos = heapMemory)
}

################################################################################
# R-object setter and getter.

GetObject <- function(n)
{
  # Retrieves the storage-object named n, in its entirety.
  get(n, heapMemory)
}

ObjectExists <- function(n)
{
  # Returns TRUE if and only if a storage-object named n exists.
  # Returns FALSE otherwise.
  exists(n, heapMemory, inherits = FALSE)
}

SetObject <- function(n, v)
{
  # Assigns value v to the storage-object named n.
  assign(n, v, heapMemory)
}

################################################################################
# Functions acting upon R storage-object names.

CreateObject <- function(n, d)
{
  # Creates a storage-object of name n and BASIC (array) dimensions d.
  # For a singleton (non-array) object, d should be an empty integer vector.
  v <- InitialDatum(n)
  SetObject(n, if (length(d) == 0L) v else array(v, d + 1L))
}

InitialDatum <- function(n)
{
  # Returns the value to which the storage-object named n is initialised at the
  # time of its creation.
  if (TypeFromName(n) == basicS) initialString else initialNumber
}

################################################################################
# Functions acting upon R BASIC-data storage objects.

AssignVariable <- function(x, v, a)
{
  # Assigns BASIC-value a to the BASIC variable (or array element) referenced by
  # v and stored as R object x (or an element thereof).
  b <- if (IsIntegerValued(v)) floor(Value(a)) else Value(a)
  x[LinearisedIndex(x, v)] <- b
  SetObject(NameForObject(v), x)
}

DimensionalityOfObject <- function(x)
{
  # Returns the integer number of dimensions of storage-object x (zero, in the
  # case of a singleton non-array object).
  length(dim(x))
}

ExtentOfObject <- function(x)
{
  # Returns a vector of the (zero-indexed) BASIC array dimensions spanned by
  # (one-indexed) R storage-object x. That is, a return value of c(2, 3) means
  # x stores a BASIC array of those dimensions, e.g., A(2,3). These differ from
  # the R dimensions of x. Returns an empty vector when x is not an array.
  d <- dim(x)
  if (is.null(d)) integer() else d - 1L
}

LinearisedIndex <- function(x, v)
{
  # Maps an n-dimensional set of BASIC-variable reference (v) subscripts, each
  # starting at zero, to a single R storage-object (x) index, starting from one.
  # Includes zero-dimensional singleton (non-array) objects and variables.
  d <- ExtentOfObject(x)
  sum(cumprod(c(1L, d[-length(d)] + 1L)) * SubscriptsOfVariable(v)) + 1L
}

RetrieveValue <- function(x, v)
{
  # Extracts the BASIC value referenced by variable v from storage-object x.
  # Variable subscript validity checks should already have been performed.
  a <- x[LinearisedIndex(x, v)]
  if (IsStringValued(v)) AsString(a) else AsNumber(a)
}

SubscriptsMatch <- function(x, v)
{
  # Checks whether or not a storage-object, x, and a BASIC variable reference,
  # v, have the same number of dimensions (subscripts) and that x is large
  # enough to encompass the specific subscripts of v. Returns TRUE if and only
  # if this is the case, or FALSE otherwise. Subscripts must be evaluated to
  # integers before this is called.
  ((DimensionalityOfObject(x) == NumberOfSubscripts(v))
    && all(ExtentOfObject(x) >= SubscriptsOfVariable(v)))
}

################################################################################
# BASIC-variable reference-object constructor and getters.

NameOfVariable <- function(v)
{
  # Returns the name of the BASIC variable referenced by v, as a character
  # string. This includes the trailing $ or % in the case of string or integer
  # variables, respectively. Does not include the parenthesis or subscripts of
  # array-element references (only the name of the whole array).
  v[["name"]]
}

SubscriptsOfVariable <- function(v)
{
  # Returns the subscripts of BASIC variable reference v as either an integer
  # vector (of evaluated subscripts) or a character vector (of unevaluated BASIC
  # expressions). This is empty when the variable is dimensionless (has no
  # subscripts).
  v[["subscripts"]]
}

Variable <- function(n, i)
{
  # Constructs a BASIC variable reference object, for a variable of name n and
  # subscripts i. For singleton (non-array) references, i should be empty.
  # The subscripts, i, may be evaluated (to integers) or unevaluated strings
  # of BASIC expressions (ordinarily the latter). Besides the usual data-typed
  # variables, A, A$, etc., we also use these constructs for user-defined
  # functions, FNA(X). These have the same name-subscripts-value description,
  # and are indeed variable, but the subscripts need not be integers and the
  # (string) value is evaluated as code, rather than being taken at face value.
  list(name = unname(n), subscripts = i)
}

################################################################################
# Initialisation, assignment and recall of BASIC variables.

DimensionArray <- function(v)
{
  # Initialises a BASIC array of the name and dimensions in reference v.
  n <- NameForObject(v)
  if (ObjectExists(n)) return(SetRedimmedArrayError())
  i <- EvalSubscripts(SubscriptsOfVariable(v))
  if (Terminated()) return(Exit())
  CreateObject(n, i)
}

GetValue <- function(v)
{
  # Returns the value of the BASIC variable (or array element) referenced by v.
  # A default value will be created if the variable has not yet been assigned.
  v <- Variable(NameOfVariable(v), EvalSubscripts(SubscriptsOfVariable(v)))
  if (Terminated()) return(Exit())
  x <- GetOrCreateObject(v)
  if (!SubscriptsMatch(x, v)) return(SetBadSubscriptError())
  RetrieveValue(x, v)
}

SetValue <- function(v, a)
{
  # Assigns BASIC value a to the BASIC variable (or array element) referenced by
  # v. Returns a.
  v <- Variable(NameOfVariable(v), EvalSubscripts(SubscriptsOfVariable(v)))
  if (Terminated()) return(Exit())
  x <- GetOrCreateObject(v)
  if (!SubscriptsMatch(x, v)) return(SetBadSubscriptError())
  if (!TypesMatch(v, a)) return(SetTypeMismatchError())
  AssignVariable(x, v, a)
  a
}

################################################################################
# Functions acting upon BASIC-variable references.

DefaultExtent <- function(v)
{
  # Returns default BASIC array dimensions for initialising the BASIC variable
  # referenced by v when it has not already been defined by a DIM statement.
  # Returns and empty integer vector when v is a singleton (not an array).
  rep(defaultExtent, NumberOfSubscripts(v))
}

GetOrCreateObject <- function(v)
{
  # Retrieves the entirety of the R storage-object for the BASIC variable or
  # array referenced by v. If the storage-object does not already exist, it will
  # be created (with default dimensions and values) beforehand.
  n <- NameForObject(v)
  if (ObjectExists(n)) GetObject(n) else CreateObject(n, DefaultExtent(v))
}

IsArray <- function(v)
{
  # Returns TRUE if and only if the BASIC variable referenced by v is an element
  # of a BASIC array. Returns FALSE otherwise.
  NumberOfSubscripts(v) != 0L
}

IsIntegerValued <- function(v)
{
  # Returns TRUE if and only if the BASIC variable referenced by v is an integer
  # constrained number. Returns FALSE otherwise.
  IsIntegerConstrained(NameOfVariable(v))
}

IsNumberValued <- function(v)
{
  # Returns TRUE if and only if the BASIC variable referenced by v is a number.
  # Returns FALSE otherwise.
  TypeOfVariable(v) == basicN
}

IsStringValued <- function(v)
{
  # Returns TRUE if and only if the BASIC variable referenced by v is a string.
  # Returns FALSE otherwise.
  TypeOfVariable(v) == basicS
}

NameForObject <- function(v)
{
  # Maps the name of the BASIC variable referenced by v to the name of its R
  # data-storage object. In BASIC, X and X(1) are two different objects. We
  # prepend a marker to array names, rather than appending a '(', in order to
  # keep variable data-type checks simple (these look for a trailing '$').
  if (IsArray(v)) paste0("_", NameOfVariable(v)) else NameOfVariable(v)
}

NumberOfSubscripts <- function(v)
{
  # Returns the number of subscripts (dimensions) of BASIC variable reference v.
  length(SubscriptsOfVariable(v))
}

TypesMatch <- function(v, a)
{
  # Returns TRUE if and only if the BASIC data type of the variable (reference)
  # v matches that of the BASIC value a. Returns FALSE otherwise.
  TypeOfVariable(v) == Type(a)
}

TypeOfVariable <- function(v)
{
  # Returns the BASIC data type (string or number) of variable (reference) v.
  TypeFromName(NameOfVariable(v))
}

################################################################################
# Define and recall user-defined BASIC functions.

DefineFunction <- function(f, p, s)
{
  # Define a function, named f, of parameter variables named p, as the BASIC
  # expression of character string s. The parameters must all be singletons (no
  # arrays), and we store them by the names of their corresponding R-objects.
  # To distinguish the function from data-type objects, its name, f, necessarily
  # begins with 'FN' (or 'fn', etc.).
  n <- sapply(p, function(v) NameForObject(Variable(v, character())))
  SetObject(f, list(parameters = n, definition = s))
}

RecallFunction <- function(f)
{
  # Returns the definition of user-defined function named f. To distinguish the
  # function from data-type objects, its name, f, necessarily begins with 'FN'
  # (or 'fn', etc.).
  if (!ObjectExists(f)) return(SetUndefinedFunctionError())
  GetObject(f)
}

################################################################################
# Type utilities, acting on any name(s) (variable, function, or storage-object).

IsIntegerConstrained <- function(n)
{
  # Returns TRUE when string n names an integer-constrained variable, R storage
  # object, or user-defined function. Returns FALSE otherwise.
  substring(n, nchar(n)) == "%"
}

TypeFromName <- function(n)
{
  # Returns the data-type of a variable, or the return type of a user-defined
  # function, from either its name, or the associated R-object name, n. In
  # general, n can be a vector of names.
  c(basicN, basicS)[as.integer(substring(n, nchar(n)) == "$") + 1L]
}

############################################################################ EOF
