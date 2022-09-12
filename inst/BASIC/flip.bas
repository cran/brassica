# FLIP
# By John S. James.
# From David H. Ahl & Steve North, More BASIC Computer Games, Workman, 1980.

# Builds a 16-parameter profile of the player from their last two guesses and
# whether they were right or wrong. The program then exploits any statistical
# propensity for the last two guesses and their outcomes to determine the next.
# Probabilities far from 0.5 in the profile represent human predictability.
# To retain and refine your profile over multiple games, change 'THEN 240' to
# 'THEN 280' on line 860.

# Changes:
# - Formatting of some printed output.
# - Appended author and year to title.
# - Prints profile after game, as per John's suggestion.

10 PRINT TAB(25);"FLIP"
20 PRINT TAB(18);"Creative Computing"
30 PRINT TAB(16);"Morristown, New Jersey"
32 PRINT
34 PRINT TAB(19);"by John S. James"
36 PRINT TAB(25);"1977"
38 PRINT:PRINT:PRINT
40 B1=50
42 PRINT "Explanation (Y or N)";
44 INPUT T$
46 IF LEFT$(T$,1) <> "Y" THEN 180
48 PRINT
50 PRINT "On each turn, you guess Yes ('Y') or No ('N')."
60 PRINT "Only one is correct, and the program has decided"
70 PRINT "which one before you make your guess. At first"
80 PRINT "your odds are 50%, pure chance. But later, the"
90 PRINT "program will try to take advantage of patterns"
100 PRINT "in your guessing."
110 PRINT
120 PRINT "The game ends after ";B1;" turns; a score of ";
125 PRINT INT(B1/2-1)
130 PRINT "or more is good. The program tells you when you"
140 PRINT "win a turn by typing an asterisk ('*') as the"
150 PRINT "first character of the following line."
160 PRINT
170 REM
180 REM INITIALIZE: 16 PROBABILITIES, 4 RESPONSES (X),
190 REM OLD-MEMORY FACTOR (F1), RANDOMNESS FACTOR (F2),
200 REM SCORES (S1,S2) AND RIGHT-ANSWER FLAG.
210 PRINT
220 PRINT
230 DIM P(16),X(4)
240 PRINT "Begin."
250 FOR I=1 TO 16
260 P(I)=.5
270 NEXT I
280 FOR I=1 TO 4
290 X(I)=0
300 IF RND(1) < .5 THEN 320
310 X(I)=1
320 NEXT I
330 F1=.8
340 F2=.3
350 S1=0
360 S2=0
370 A$=" "
380 REM
390 REM TAKE THE ESTIMATED PROBABILITY (Z1)
400 REM OF THE PERSON GUESSING YES.
410 REM USE AN ADJUSTED PROBABILITY (Z2).
420 I9=8*X(4)+4*X(3)+2*X(2)+X(1)+1
430 Z1=P(I9)
440 Z2=Z1
450 IF Z2 <> .5 THEN 480
460 Z2=RND(1)
470 GOTO 520
480 IF Z2 > .5 THEN 510
490 Z2=Z2*F2+0*(1-F2)
500 GOTO 520
510 Z2=Z2*F2+1*(1-F2)
520 Z5=0
530 IF RND(1) < Z2 THEN 560
540 Z5=1
550 REM
560 REM INTERACT WITH PERSON. GET HIS RESPONSE (Z3).
570 REM UPDATE RESPONSE HISTORY (X), APPROPRIATE PROB. (P(I9)).
580 PRINT A$;
590 Z3=0
600 INPUT H$
610 IF LEFT$(H$,1) = "Y" THEN 650
620 IF LEFT$(H$,1) = "N" THEN 660
630 PRINT "Error; must be Y or N."
640 GOTO 600
650 Z3=1
660 A$=" "
670 S2=S2+1
680 IF Z3 <> Z5 THEN 710
690 A$="*"
700 S1=S1+1
710 REM UPDATE X - THE LAST FOUR CHOICES
720 X(1)=X(3)
730 X(2)=X(4)
740 X(3)=Z3
750 X(4)=Z5
760 REM UPDATE THE PROBABILITY USING OLD I9.
770 P(I9)=F1*P(I9)+(1-F1)*X(3)
780 IF S2 < B1 THEN 380
790 PRINT A$;
800 PRINT
810 PRINT "End of game."
820 PRINT "You got ";S1;" out of ";S2;" correct."
822 PRINT:PRINT "Your profile:"
824 FOR I=0TO3:FOR J=1TO4:PRINT P(4*I+J),:NEXT:PRINT:NEXT
830 PRINT:PRINT
840 PRINT "Play again (Y or N)";
850 INPUT T$
860 IF LEFT$(T$,1)="Y" THEN 240
870 END

# END
