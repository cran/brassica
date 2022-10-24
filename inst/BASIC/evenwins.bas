# GAME OF EVEN WINS
# By Eric Peters.
# From David H. Ahl & Steve North, BASIC Computer Games, Workman, 1978.

# A Nim game, where the objective is to finish with an even number of chips.
# The computer learns from both its wins and losses, and improves over time.

# Changes:
# - Formatting of some printed output.
# - Appended author and year to title.
# - Altered line 5 to accept 'N' for 'no'.
# - Altered lines 25, 370, and 420, and added the subroutine at 600, to print a
#   running score of human versus computer.

1 PRINT TAB(24);"EVEN WINS": PRINT TAB(20);"Creative Computing"
2 PRINT TAB(18);"Morristown, New Jersey": PRINT
3 PRINT TAB(22);"by Eric Peters": PRINT TAB(26);"c1973": PRINT: PRINT: PRINT
4 INPUT "Do you want instructions (y/n)";A$:PRINT
5 IF LEFT$(A$,1)="N" THEN 20
6 PRINT "The game is played as follows:":PRINT
7 PRINT "At the beginning of the game, a random number of chips are"
8 PRINT "placed on the board.  The number of chips always starts"
9 PRINT "as an odd number.  On each turn, a player must take one,"
10 PRINT "two, three, or four chips.  The winner is the player who"
11 PRINT "finishes with a total number of chips that is even."
12 PRINT "The computer starts out knowing only the rules of the"
13 PRINT "game.  It gradually learns to play well.  It should be"
14 PRINT "difficult to beat the computer after twenty games in a"
15 PRINT "row.  Try it!!!!": PRINT
16 PRINT "To quit at any time, type a '0' as your move.": PRINT
20 DIM R(1,5)
25 L=0: B=0: C0=0: H0=0
30 FOR I=0 TO 5
40 R(1,I)=4
50 R(0,I)=4
60 NEXT I
70 A=0: B=0
90 P=INT((13*RND(1)+9)/2)*2+1
100 IF P=1 THEN 530
110 PRINT "There are";P;"chips on the board."
120 E1=E
130 L1=L
140 E=(A/2-INT(A/2))*2
150 L=INT((P/6-INT(P/6))*6+.5)
160 IF R(E,L)>=P THEN 320
170 M=R(E,L)
180 IF M<=0 THEN 370
190 P=P-M
200 IF M=1 THEN 510
210 PRINT "Computer takes";M;"chips leaving";P;"... Your move";
220 B=B+M
230 INPUT M
240 M=INT(M)
250 IF M<1 THEN 450
260 IF M>4 THEN 460
270 IF M>P THEN 460
280 IF M=P THEN 360
290 P=P-M
300 A=A+M
310 GOTO 100
320 IF P=1 THEN 550
330 PRINT "Computer takes";P;"chips."
340 R(E,L)=P
350 B=B+P
360 IF B/2=INT(B/2) THEN 420
370 PRINT "Game over ... You win!!!": H0=H0+1: GOSUB 600
390 IF R(E,L)=1 THEN 480
400 R(E,L)=R(E,L)-1
410 GOTO 70
420 PRINT "Game over ... I win!!!": C0=C0+1: GOSUB 600
430 GOTO 70
450 IF M=0 THEN 570
460 PRINT M;"is an illegal move ... Your move";
470 GOTO 230
480 IF R(E1,L1)=1 THEN 70
490 R(E1,L1)=R(E1,L1)-1
500 GOTO 70
510 PRINT "Computer takes 1 chip leaving";P;"... Your move";
520 GOTO 220
530 PRINT "There is 1 chip on the board."
540 GOTO 120
550 PRINT "Computer takes 1 chip."
560 GOTO 340
570 END
600 IF H0>C0 THEN PRINT "Human";STR$(H0);", Computer";STR$(C0);"."
610 IF H0<=C0 THEN PRINT "Computer";STR$(C0);", Human";STR$(H0);"."
620 PRINT: RETURN

# END
