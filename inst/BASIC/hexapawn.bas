# HEXAPAWN
# By R. A. Kaapke.
# From David H. Ahl & Steve North, BASIC Computer Games, Workman, 1978.

# A greatly simplified chess game, from Martin Gardner, The Unexpected Hanging
# and Other Mathematical Diversions, Simon & Schuster, 1969, Chapter 8: 'A
# Matchbox Game-Learning Machine'. The computer learns by eliminating positions
# that result in a loss. It does not reinforce winning positions. It becomes
# unbeatable after enough games. The original matchbox learning engine, MENACE
# (Matchbox Educable Naughts And Crosses Engine), is due to Donald Michie 'Trial
# and Error' Penguin Science Survey 1961, volume 2.

# Changes:
# - Formatting of some printed output.
# - Moved title to subroutine at 1100, appended author and year.
# - Altered lines 25, 130, 620, 630, 631, and the corresponding instructions at
#   2170 and 2190, to use the numeric keypad layout for I/O. Top to bottom, the
#   original layout was 123 / 456 / 789. This is assumed by the DATA statements
#   for the B and M matrices (and remains in use under the hood).
# - Altered line 121, to quit on a move of 0, 0 (or blank, blank).
# - Added line 2155, to page the instructions.

1 GOSUB 1100
4 REM  HEXAPAWN:  INTERPRETATION OF HEXAPAWN GAME AS PRESENTED IN
5 REM  MARTIN GARDNER'S "THE UNEXPECTED HANGING AND OTHER MATHEMATIC-
6 REM  AL DIVERSIONS", CHAPTER EIGHT:  A MATCHBOX GAME-LEARNING MACHINE
7 REM  ORIGINAL VERSION FOR H-P TIMESHARE SYSTEM BY R.A. KAAPKE 5/5/76
8 REM  INSTRUCTIONS BY JEFF DALTON
9 REM  CONVERSION TO MITS BASIC BY STEVE NORTH
10 DIM B(19,9),M(19,4),S(9),P$(3)
15 W=0: L=0
20 DEF FNR(X)=-3*(X=1)-(X=3)-4*(X=6)-6*(X=4)-7*(X=9)-9*(X=7)+FNS(X)
25 DEF FNS(X)=-X*(X=2 OR X=5 OR X=8): DEF FNK(N)=N+6*((N>6)-(N<4))
30 DEF FNM(Y)=Y-INT(Y/10)*10
35 P$="x.o"
40 FOR I=1 TO 19: FOR J=1 TO 9: READ B(I,J): NEXT J: NEXT I
45 FOR I=1 TO 19: FOR J=1 TO 4: READ M(I,J): NEXT J: NEXT I
50 PRINT "Instructions (y/n)";
60 INPUT A$
70 A$=LEFT$(A$,1)
80 IF A$="Y" THEN 2000
90 IF A$<>"N" THEN 50
100 X=0: Y=0
111 S(4)=0: S(5)=0: S(6)=0
112 S(1)=-1: S(2)=-1: S(3)=-1
113 S(7)=1: S(8)=1: S(9)=1
115 GOSUB 1000
120 PRINT "Your move";
121 INPUT M1,M2: IF M1=0 AND M2=0 THEN END
122 IF M1=INT(M1)AND M2=INT(M2)AND M1>0 AND M1<10 AND M2>0 AND M2<10 THEN 130
123 PRINT "Illegal co-ordinates."
124 GOTO 120
130 M1=FNK(M1): M2=FNK(M2): IF S(M1)=1 THEN 150
140 PRINT "Illegal move.": GOTO 120
150 IF S(M2)=1 THEN 140
160 IF M2-M1<>-3 AND S(M2)<>-1 THEN 140
170 IF M2>M1 THEN 140
180 IF M2-M1=-3 AND (S(M2)<>0) THEN 140
185 IF M2-M1<-4 THEN 140
186 IF M1=7 AND M2=3 THEN 140
190 S(M1)=0
200 S(M2)=1
205 GOSUB 1000
210 IF S(1)=1 OR S(2)=1 OR S(3)=1 THEN 820
220 FOR I=1 TO 9
221 IF S(I)=-1 THEN 230
222 NEXT I
223 GOTO 820
230 FOR I=1 TO 9
240 IF S(I)<>-1 THEN 330
250 IF S(I+3)=0 THEN 350
260 IF FNR(I)=I THEN 320
270 IF I>3 THEN 300
280 IF S(5)=1 THEN 350
290 GOTO 330
300 IF S(8)=1 THEN 350
310 GOTO 330
320 IF S(I+2)=1 OR S(I+4)=1 THEN 350
330 NEXT I
340 GOTO 820
350 FOR I=1 TO 19
360 FOR J=1 TO 3
370 FOR K=3 TO 1 STEP -1
380 T((J-1)*3+K)=B(I,(J-1)*3+4-K)
390 NEXT K
400 NEXT J
410 FOR J=1 TO 9
420 IF S(J)<>B(I,J) THEN 460
430 NEXT J
440 R=0
450 GOTO 540
460 FOR J=1 TO 9
470 IF S(J)<>T(J) THEN 510
480 NEXT J
490 R=1
500 GOTO 540
510 NEXT I
511 REMEMBER THE TERMINATION OF THIS LOOP IS IMPOSSIBLE
512 PRINT "Illegal board pattern."
530 STOP
540 X=I
550 FOR I=1 TO 4
560 IF M(X,I)<>0 THEN 600
570 NEXT I
580 PRINT "I resign."
590 GOTO 820
600 Y=INT(RND(1)*4+1)
601 IF M(X,Y)=0 THEN 600
610 IF R<>0 THEN 630
620 PRINT "I move from";STR$(FNK(INT(M(X,Y)/10)));" to";STR$(FNK(FNM(M(X,Y))))
622 S(INT(M(X,Y)/10))=0
623 S(FNM(M(X,Y)))=-1
624 GOTO 640
630 PRINT "I move from";STR$(FNK(FNR(INT(M(X,Y)/10))));" to";
631 PRINT STR$(FNK(FNR(FNM(M(X,Y)))))
632 S(FNR(INT(M(X,Y)/10)))=0
633 S(FNR(FNM(M(X,Y))))=-1
640 GOSUB 1000
641 IF S(7)=-1 OR S(8)=-1 OR S(9)=-1 THEN 870
650 FOR I=1 TO 9
660 IF S(I)=1 THEN 690
670 NEXT I
680 GOTO 870
690 FOR I=1 TO 9
700 IF S(I)<>1 THEN 790
710 IF S(I-3)=0 THEN 120
720 IF FNR(I)=I THEN 780
730 IF I<7 THEN 760
740 IF S(5)=-1 THEN 120
750 GOTO 790
760 IF S(2)=-1 THEN 120
770 GOTO 790
780 IF S(I-2)=-1 OR S(I-4)=-1 THEN 120
790 NEXT I
800 PRINT "You can't move, so ";
810 GOTO 870
820 PRINT "You win."
830 M(X,Y)=0
840 L=L+1
850 PRINT "I have won";STR$(W);", and you";STR$(L);", out of";L+W;"games."
851 PRINT
860 GOTO 100
870 PRINT "I win."
880 W=W+1
890 GOTO 850
900 DATA -1,-1,-1,1,0,0,0,1,1,-1,-1,-1,0,1,0,1,0,1
905 DATA -1,0,-1,-1,1,0,0,0,1,0,-1,-1,1,-1,0,0,0,1
910 DATA -1,0,-1,1,1,0,0,1,0,-1,-1,0,1,0,1,0,0,1
915 DATA 0,-1,-1,0,-1,1,1,0,0,0,-1,-1,-1,1,1,1,0,0
920 DATA -1,0,-1,-1,0,1,0,1,0,0,-1,-1,0,1,0,0,0,1
925 DATA 0,-1,-1,0,1,0,1,0,0,-1,0,-1,1,0,0,0,0,1
930 DATA 0,0,-1,-1,-1,1,0,0,0,-1,0,0,1,1,1,0,0,0
935 DATA 0,-1,0,-1,1,1,0,0,0,-1,0,0,-1,-1,1,0,0,0
940 DATA 0,0,-1,-1,1,0,0,0,0,0,-1,0,1,-1,0,0,0,0
945 DATA -1,0,0,-1,1,0,0,0,0
950 DATA 24,25,36,0,14,15,36,0,15,35,36,47,36,58,59,0
955 DATA 15,35,36,0,24,25,26,0,26,57,58,0
960 DATA 26,35,0,0,47,48,0,0,35,36,0,0,35,36,0,0
965 DATA 36,0,0,0,47,58,0,0,15,0,0,0
970 DATA 26,47,0,0,47,58,0,0,35,36,47,0,28,58,0,0,15,47,0,0
1000 PRINT
1010 FOR I=1 TO 3
1020 FOR J=1 TO 3
1030 PRINT TAB(10);MID$(P$,S((I-1)*3+J)+2,1);
1040 NEXT J
1050 PRINT
1060 NEXT I
1070 PRINT
1080 RETURN
1100 PRINT TAB(25);"HEXAPAWN"
1110 PRINT TAB(20);"Creative Computing"
1120 PRINT TAB(18);"Morristown, New Jersey"
1130 PRINT
1140 PRINT TAB(21);"by R. A. Kaapke"
1150 PRINT TAB(9);"modified by Jeff Dalton and Steve North"
1160 PRINT TAB(7);"original board game by Martin Gardner (1962)"
1170 PRINT TAB(27);"1976"
1180 PRINT: PRINT: PRINT
1190 RETURN
2000 PRINT: PRINT "This program plays the game of Hexapawn."
2010 PRINT "Hexapawn is played with chess pawns on a 3 by 3 board."
2020 PRINT "The pawns are moved as in chess - one space forward to"
2030 PRINT "an empty space or one space forward and diagonally to"
2040 PRINT "capture an opposing man.  On the board, your pawns"
2050 PRINT "are 'o', the computer's pawns are 'x', and empty "
2060 PRINT "squares are '.'.  To enter a move, type the number of"
2070 PRINT "the square you are moving from, followed by the number"
2080 PRINT "of the square you will move to.  The numbers must be"
2090 PRINT "separated by a comma.": PRINT
2100 PRINT "The computer starts a series of games knowing only when"
2105 PRINT "the game is won (a draw is impossible) and how to move."
2110 PRINT "It has no strategy at first, and just moves randomly."
2120 PRINT "However, it learns from each game.  Thus, winning becomes"
2130 PRINT "more and more difficult.  Also, to help offset your"
2140 PRINT "initial advantage, you will not be told how to win the"
2150 PRINT "game but must learn this by playing."
2155 PRINT: INPUT "[more]",
2160 PRINT: PRINT "The numbering of the board is as follows:": PRINT
2170 PRINT TAB(10);"789": PRINT TAB(10);"456": PRINT TAB(10);"123": PRINT
2180 PRINT "For example, to move your rightmost pawn forward, you"
2190 PRINT "would type 3,6 in response to the question 'Your move?'"
2200 PRINT "Since I'm a good sport, you'll always go first."
2210 PRINT
2220 GOTO 100
9999 END

# END
