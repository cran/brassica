# BASIC interpreter.
# MJL @ Titirangi, 3 August 2022.

################################################################################
# Primary function.

RUN <- function(program = NULL, ttx = 0, tty = 0, up = FALSE)
{
  # Loads and runs a BASIC program, with optional teletypewriter aesthetics.
  # Args:
  #   program : The file path of a BASIC program listing,
  #               or NULL to re-run a program already loaded.
  #   ttx     : A pause, in milliseconds, after printing each character.
  #   tty     : A pause, in milliseconds, after printing each line.
  #   up      : Whether or not to print exclusively in upper case.
  # Note:
  #   A negative value for ttx or tty does not pause (as for a value of zero),
  #   but flushes the console after each complete line of printed output.
  # Returns:
  #   Invisible NULL. BASIC-program output will appear on standard output.
  if (length(program) > 0L) Load(program)
  SetAesthetics(ttx, tty, up)
  ResetHeapMemory()
  GoToFirstDatum()
  GoToFirstStatement()
  while(Running() && WithinProgram()) Enact(GetStatement())
  PrintExitMessage()
}

############################################################################ EOF
