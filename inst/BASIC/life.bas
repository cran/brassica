# LIFE
# By Clark Baker.
# From David H. Ahl & Steve North, BASIC Computer Games, Workman, 1978.

# Changes:
# - Formatting of some printed output.
# - Added authors to title, and instructions, from line 700 on.
# - Changed line 40 to begin on a blank, instead of on 'done'.
# - Removed line 50, and changed the live cell condition on line 160 from
#   '<>" "' to '="@"' (from 'not a space', to 'is an at sign').
# - Added 'DELAY 0' to line 635 (this flushes the terminal).
# - Inserted lines 639 and 639, asking whether or not to continue.

2 PRINT TAB(25);"LIFE"
4 PRINT TAB(18);"Creative Computing"
6 PRINT TAB(16);"Morristown, New Jersey"
8 GOSUB 700
9 X1=1: Y1=1: X2=24: Y2=70
10 DIM A(24,70),B$(24)
20 C=1
30 INPUT B$(C)
40 IF B$(C)="" THEN 80: REM WAS 'IF B$(C)="DONE" THEN B$(C)="": GOTO 80'
# 50 IF LEFT$(B$(C),1)="." THEN B$(C)=" "+RIGHT$(B$(C),LEN(B$(C))-1)
60 C=C+1
70 GOTO 30
80 C=C-1: L=0
90 FOR X=1 TO C-1
100 IF LEN(B$(X))>L THEN L=LEN(B$(X))
110 NEXT X
120 X1=11-C/2
130 Y1=33-L/2
140 FOR X=1 TO C
150 FOR Y=1 TO LEN(B$(X))
160 IF MID$(B$(X),Y,1)="@" THEN A(X1+X,Y1+Y)=1:P=P+1
170 NEXT Y
180 NEXT X
200 PRINT:PRINT:PRINT
210 PRINT "Generation:";G,"Population:";P;: IF I9 THEN PRINT "INVALID!";
215 X3=24: Y3=70: X4=1: Y4=1: P=0
220 G=G+1
225 FOR X=1 TO X1-1: PRINT: NEXT X
230 FOR X=X1 TO X2
240 PRINT
250 FOR Y=Y1 TO Y2
253 IF A(X,Y)=2 THEN A(X,Y)=0: GOTO 270
256 IF A(X,Y)=3 THEN A(X,Y)=1: GOTO 261
260 IF A(X,Y)<>1 THEN 270
261 PRINT TAB(Y);"@";
262 IF X<X3 THEN X3=X
264 IF X>X4 THEN X4=X
266 IF Y<Y3 THEN Y3=Y
268 IF Y>Y4 THEN Y4=Y
270 NEXT Y
290 NEXT X
295 FOR X=X2+1 TO 24: PRINT: NEXT X
299 X1=X3: X2=X4: Y1=Y3: Y2=Y4
301 IF X1<3 THEN X1=3: I9=-1
303 IF X2>22 THEN X2=22: I9=-1
305 IF Y1<3 THEN Y1=3: I9=-1
307 IF Y2>68 THEN Y2=68: I9=-1
309 P=0
500 FOR X=X1-1 TO X2+1
510 FOR Y=Y1-1 TO Y2+1
520 C=0
530 FOR I=X-1 TO X+1
540 FOR J=Y-1 TO Y+1
550 IF A(I,J)=1 OR A(I,J)=2 THEN C=C+1
560 NEXT J
570 NEXT I
580 IF A(X,Y)=0 THEN 610
590 IF C<3 OR C>4 THEN A(X,Y)=2: GOTO 600
595 P=P+1
600 GOTO 620
610 IF C=3 THEN A(X,Y)=3: P=P+1
620 NEXT Y
630 NEXT X
635 X1=X1-1: Y1=Y1-1: X2=X2+1: Y2=Y2+1: DELAY 0
638 IF G=21 THEN INPUT "Continue to generation 50 (Y/n)";C$: IF C$="N" THEN END
639 IF G=51 THEN INPUT "Continue indefinitely (y/N)";C$: IF C$<>"Y" THEN END
640 GOTO 210
650 END
700 REM AUTHORS & INSTRUCTIONS
710 PRINT
720 PRINT TAB(20);"by Clark Baker"
730 PRINT TAB(12);"after John Conway's game (1970)"
740 PRINT TAB(16);"modified by Steve North"
750 PRINT TAB(25);"c1975"
760 PRINT: PRINT: PRINT
770 PRINT "At the prompt, type '.' for dead cells, and '@' for live ones."
780 PRINT "Enter a blank line to begin.  For example:"
790 PRINT "? .@@"
800 PRINT "? @@."
810 PRINT "? .@."
820 PRINT "?"
830 PRINT "Escape to end the game after generation 50."
840 PRINT
850 PRINT "Enter your pattern:"
860 RETURN

# END
