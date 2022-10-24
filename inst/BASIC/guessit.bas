# GUESS-IT
# By Gerard Kierman.
# From David H. Ahl & Steve North, More BASIC Computer Games, Workman, 1980.

# A card game involving bluffing. The computer uses the optimal strategy, given
# by Rufus Isaacs 'A Card Game with Bluffing' The American Mathematical Monthly,
# volume 62, number 2, February 1955, pp 99 - 108, but there is still an element
# of luck.

# Changes:
# - Formatting of some printed output.
# - Appended author and year to title.
# - Altered lines 230, 240, 430, 440, 560, 570, 1100, 1540, and 1550 to accept
#   'Y' and 'N' for 'yes' and 'no'.
# - Altered line 270 to start from J = 1. The original had J = 2; a typo which
#   blocked guessing the first number guessed in the previous game.
# - Altered lines 360 - 380 to print the hand sorted.
# - Added line 605, so that guesses must be integers from {1, 2, ... 11}.
# - Made the PRINT statements on lines 1000 amd 1080 identical, so you can no
#   longer see when the computer is bluffing.
# - Added line 1105, so you must answer Y or N to the computer's question. This
#   necesitated adding line 1015 to prevent testing for a bluff by giving some
#   other (non Y or N) answer (even though the computer ignores that response).
# - Changed line 1580 from 'STOP' to 'END', for a silent exit.
# - Altered line 1950 to wait for the user at the end of the instructions.

1 PRINT TAB(25);"GUESS-IT"
2 PRINT TAB(20);"Creative Computing"
3 PRINT TAB(18);"Morristown, New Jersey"
4 PRINT
5 PRINT TAB(20);"by Gerard Kierman"
6 PRINT TAB(8);"original card game by Rufus Isaacs (1955)"
7 PRINT TAB(26);"c1978"
10 PRINT: PRINT: PRINT
20 G1=0:C1=0
50 A1=RND(1)
70 H=5
80 DIM P(10,10)
90 FOR K=1 TO H
100 P(K,0)=1
110 P(0,K)=1/(K+1)
120 NEXT K
130 FOR I=1 TO H
140 FOR J=I TO H
150 P(I,J)=(1+J*P(J,I-1)*(1-P(J-1,I)))/(1+(J+2)*P(J,I-1))
160 P(J,I)=(1+I*P(I,J-1)*(1-P(I-1,J)))/(1+(I+1)*P(I,J-1))
170 NEXT J
180 NEXT I
190 Z=11
200 DIM U(Z),N(Z)
210 PRINT"Do you want instructions (y/n)";
220 INPUT A$
230 IF LEFT$(A$,1)="Y" THEN 1730
240 IF LEFT$(A$,1)<>"N" THEN 210
250 PRINT
260 G1=G1+1
270 FOR J=1 TO Z
280 U(J)=0
290 NEXT J
300 E=0:T=0:C=0:P=0:L=0
310 GOSUB 1630
320 REM N(1) TO N(H)= COMP HAND N(H+1)=TO N(Z)= OTHER HAND
330 D=(Z)
340 PRINT"Your hand is:"
350 PRINT
360 FOR J=1 TO 11: FOR I=H+1 TO Z-1
370 IF N(I)=J THEN PRINT N(I);
380 NEXT I,J
390 PRINT
400 PRINT
410 PRINT"Do you want to go first";
420 INPUT A$
430 IF LEFT$(A$,1)="Y" THEN 470
440 IF LEFT$(A$,1)<>"N" THEN 410 :REM orginally went to 390
450 K=1
460 GOTO 480
470 K=0
480 K=K+1
490 M=H-C
500 N=H-P
# 510 PRINT
520 IF K=(INT(K/2))*2 THEN 860
530 PRINT
540 PRINT"Do you want to guess the down number";
550 INPUT A$
560 IF LEFT$(A$,1)="Y" THEN 1250
570 IF LEFT$(A$,1)<>"N" THEN 540 :REM orginally went to 530
580 PRINT
590 PRINT"What number do you want to ask about";
600 INPUT E
605 IF E<>INT(E) OR E<1 OR E>11 THEN 590
610 FOR I=1 TO Z
620 IF E=U(I) THEN 650
630 NEXT I
640 GOTO 670
650 PRINT: PRINT E;"was asked before.  Try another."
660 GOTO 580
670 FOR J=1 TO H
680 IF N(J)=E THEN 800
690 NEXT J
700 PRINT
710 PRINT E;"is not in my hand." :REM E omitted in original (typo)
720 IF M=0 THEN 1460
730 IF N=0 THEN 1440
740 Y=((M+1)*P(M,N-1)-M*P(M-1,N))/(1+(M+1)*P(M,N-1))
750 IF RND(1)<Y THEN 1380
760 GOSUB 1220
770 IF (H-P)=1 THEN 1460
780 P=P+1
790 GOTO 480
800 PRINT
810 PRINT E;"is in my hand."
820 C=C+1
830 GOSUB 1220
840 GOTO 480
850 REM COMP SEQ STARTS
860 IF T<>0 THEN 1410
870 IF H-C<>0 THEN 890
880 GOTO 1460
890 IF H-P<>0 THEN910
900 GOTO 1460
910 IF (2*H-2)-(P+C)<>0 THEN 930
920 GOTO 1460
930 REM RND DECISION TO BLUFF OR NOT ON ASKING FOR CARD
940 IF RND(1)>1/(1+(N+1)*P(N,M-1)) THEN 1060
950 PRINT
960 A=INT(H*RND(1))+1
970 FOR J=1 TO Z
980 IF N(A)=U(J) THEN 960
990 NEXT J
1000 PRINT"Do you have";N(A);
1010 INPUT A$
1015 IF LEFT$(A$,1)<>"Y" AND LEFT$(A$,1)<>"N" THEN 1000
1020 C=C+1
1030 E=N(A)
1040 GOSUB 1220
1050 GOTO 480
1060 GOSUB 1170
1070 PRINT
1080 PRINT"Do you have";N(A);
1090 INPUT A$
1100 IF LEFT$(A$,1)="Y" THEN 1130
1105 IF LEFT$(A$,1)<>"N" THEN 1080
1110 T=1
1120 GOTO 480
1130 E=N(A)
1140 P=P+1
1150 GOSUB 1220
1160 GOTO 480
1170 A=INT((H+1)*RND(1))+(H+1)
1180 FOR J=1 TO Z
1190 IF N(A)=U(J) THEN GOTO 1170
1200 NEXT J
1210 RETURN
1220 L=L+1
1230 U(L)=E
1240 RETURN
1250 PRINT
1260 PRINT"What do you think the down number is";
1270 INPUT B
1280 PRINT
1290 PRINT"The down number is";STR$(N(Z));"."
1300 IF B=N(Z) THEN 1360
1310 PRINT
1320 PRINT"Your guess of";B;"is not correct - you lose."
1330 C1=C1+1
1340 GOTO 1520
1350 PRINT
1360 PRINT"Your guess of";B;"is correct - you win."
1370 GOTO 1520
1380 PRINT" I think you were not bluffing when you asked about";STR$(E);"."
1390 G=E
1400 GOTO 1480
1410 PRINTN(A);"was not a bluff."
1420 G=N(A)
1430 GOTO 1480
1440 G=N(Z)
1450 GOTO 1480
1460 GOSUB 1170
1470 G=N(A)
1480 PRINT" I guess the down number is";STR$(G);"."
1490 IF G=N(Z) THEN 1590
1500 PRINT
1510 PRINT"The down number is";STR$(N(Z));".  I was wrong... you win."
1520 PRINT: PRINT"Do you want to play again";
1530 INPUT A$
1540 IF LEFT$(A$,1)="Y" THEN 250
1550 IF LEFT$(A$,1)<>"N" THEN 1520
1560 PRINT
1570 PRINT"We played";G1;"games.  You lost";C1;"and won";STR$(G1-C1);"."
1580 END
1590 PRINT
1600 PRINT"The down number is";STR$(N(Z));".  I was correct... you lose."
1610 C1=C1+1
1620 GOTO 1520
1630 FOR I=1 TO Z
1640 N(I)=I
1650 NEXT I
1660 FOR I=1 TO Z
1670 R=INT(RND(1)*((Z+1)-I))+I
1680 W=N(R)
1690 N(R)=N(I)
1700 N(I)=W
1710 NEXT I
1720 RETURN
1730 PRINT
1740 PRINT"The object of this game is to guess an unknown number"
1750 PRINT"called the 'down number'.  The game is played with the"
1760 PRINT"numbers 1 to";STR$(Z);".  You will be given a hand of";H
1770 PRINT"randomly selected numbers between 1 and";STR$(Z);". The"
1780 PRINT"computer will have a similar hand.  The down number will"
1785 PRINT"always be the number not in either player's hand."
1790 PRINT
1800 PRINT"You alternate moves with the computer.  On any move there"
1810 PRINT"are two options- guess the down number, or ask about some"
1820 PRINT"number."
1830 PRINT
1840 PRINT"When a player guesses the down number, the game stops."
1850 PRINT"If the guess is correct, that player wins."
1860 PRINT"If the guess is not correct, that player loses."
1870 PRINT
1880 PRINT"All questions about numbers in the other player's hand"
1890 PRINT"must be answered truthfully.  A player may 'bluff' by"
1900 PRINT"asking about a number in his own hand.  The computer"
1910 PRINT"will sometimes do this."
1920 PRINT
1930 PRINT"Each number may be asked about only once."
1940 PRINT
1950 PRINT"Good luck...   [press enter]";:INPUT"",
1960 GOTO 250
1970 END

# END
