# NOT ONE
# By Robert Puopolo.
# From David H. Ahl & Steve North, More BASIC Computer Games, Workman, 1980.

# A dice game involving levels of risk. The computer makes rational choices
# based on expectation values, and its position relative to the player.

# Changes:
# - This version has been substantially rewritten, without changing the rules.
# - The original computer logic committed to a predetermined number of rolls
#   based on the value of the first roll (specifically: 18 rolls for an initial
#   roll of 2, 3, 11, or 12; 9 rolls for an initial roll of 4, 5, 9, or 10; and
#   6 rolls for an initial roll of 6, 7, or 8). This version continues to roll
#   so long as the expectation value of the next roll remains positive. On
#   average, this produces a higher score than the original logic, but the
#   fluctuations are large, and the difference is small.
# - The original would crash if more than fifty rolls were attempted in one turn
#   (this is not so improbable after an intial roll of 12). This version allows
#   unlimited rolls.
# - In the final round, the computer continues to roll only until it is winning
#   (unless it begins with the win already guaranteed, in which case it rolls as
#   normal to maximise points and twist the knife).
# - Appended author and year to title.

10 PRINT TAB(25);"NOT ONE"
15 PRINT TAB(20);"Creative Computing"
20 PRINT TAB(18);"Morristown, New Jersey"
25 PRINT
30 PRINT TAB(20);"by Robert Puopolo"
35 PRINT TAB(8);"dice game described by John Scarne (1945)"
40 PRINT TAB(27);"1975"
45 PRINT: PRINT: PRINT
50 DIM H(10),C(10)
60 INPUT "Would you like instructions (y/n)";A$
70 IF LEFT$(A$,1)<>"N" THEN GOSUB 400
80 H0=0
90 C0=0
100 FOR T=1 TO 10
110 PRINT: PRINT: PRINT "ROUND";T
120 I=1: PRINT: PRINT "You roll:"
130 R=INT(6*RND(1)+1)+INT(6*RND(1)+1): PRINT R
140 IF I=1 THEN R1=R: H(T)=R: I=0: GOTO 170
150 IF R=R1 THEN H(T)=0: PRINT "You score zero for this round.": GOTO 200
160 H(T)=H(T)+R
170 INPUT "Again";B$
180 IF LEFT$(B$,1)<>"N" THEN 130
190 PRINT "You score";H(T);"for this round.": H0=H0+H(T)
200 I=1: PRINT
210 R=INT(6*RND(1)+1)+INT(6*RND(1)+1): PRINT "Computer rolls";R
220 IF I=1 THEN R1=R: C(T)=R: I=0: GOTO 250
230 IF R<>R1 THEN C(T)=C(T)+R: GOTO 250
240 C(T)=0: PRINT "Computer scores zero for this round!": GOTO 280
250 IF T=10 AND C0<=H0 THEN IF C0+C(T)<=H0 THEN 210 ELSE 270
260 IF 252>=(6-ABS(7-R1))*(R1+C(T)) THEN 210
270 PRINT "Computer scores";C(T);"for this round.": C0=C0+C(T)
280 PRINT
290 IF T=10 THEN PRINT "Final score - "; ELSE PRINT "Score - ";
300 IF H0>C0 THEN PRINT "   You:";H0;"   Computer:";C0: GOTO 320
310 PRINT "   Computer:";C0;"   You:";H0
320 NEXT T
330 PRINT: PRINT: PRINT "Scoring Summary:": PRINT
340 PRINT "Round";TAB(9);"You";TAB(16);"Computer": PRINT
350 FOR T=1 TO 10: PRINT T;TAB(8);H(T);TAB(15);C(T): NEXT
360 PRINT: PRINT "Totals:";TAB(8);H0;TAB(15);C0: PRINT
370 INPUT "Play again";B$
380 IF LEFT$(B$,1)<>"N" THEN 80
390 END
400 PRINT
410 PRINT "The game of Not One (AKA 'Pig') is played with two"
420 PRINT "players and a pair of dice.  There are ten rounds"
430 PRINT "in the game; each round consisting of one turn for"
440 PRINT "each player (yourself and the computer).  Players"
450 PRINT "sum the scores they attain on each round, and the"
460 PRINT "player with the highest score after the ten rounds"
470 PRINT "is the winner."
480 PRINT
490 PRINT "On each turn, the player may roll the two dice from"
500 PRINT "1 to n times.  If t(i) is the total of the dice on"
510 PRINT "the ith roll, then the player's score for the turn"
520 PRINT "is t(1) + t(2) + ... + t(n).  However, and here's"
530 PRINT "the catch, if any t(i > 1) is equal to t(1), then"
540 PRINT "the turn is over and their score for that round is"
550 PRINT "zero.  After each roll not equal to t(1), the"
560 PRINT "player can decide whether to roll again ('yes') or"
570 PRINT "stop ('no') and score the points already obtained."
580 PRINT
590 INPUT "[press enter]",
600 RETURN

# END
