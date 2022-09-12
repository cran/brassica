# CHASE
# By Mac Oglesby, modified by Bill Cotter, improved by Arnold Loveridge.
# From David H. Ahl & Steve North, More BASIC Computer Games, Workman, 1980.

# Changes:
# - Formatting of some printed output, especially map characters.
# - Appended author and year to title.
# - Inserted 'would you like instructions', mainly to hold the title on-console.
# - Tweaked 'another game' logic, enabling just pressing enter twice.

10 PRINT TAB(26);"CHASE"
20 PRINT TAB(20);"Creative Computing"
30 PRINT TAB(18);"Morristown, New Jersey"
34 PRINT
35 PRINT TAB(22);"by Mac Oglesby"
36 PRINT TAB(4);"with additions by Bill Cotter and Arnold Loveridge"
37 PRINT TAB(26);"c1975"
40 PRINT:PRINT:PRINT
45 PRINT "Would you like instructions (y/n)";
46 INPUT N9$
47 IF N9$ = "N" THEN 180
50 PRINT
51 PRINT "You are within the walls of a high voltage maze."
52 PRINT "There are five security machines trying to destroy you."
60 PRINT "You are the 'o',   the interceptors are the '+'s."
70 PRINT "The areas marked '#' are high voltage."
80 PRINT "Your only chance for survival is to manoeuvre each"
90 PRINT "interceptor into a '#'.  -----GOOD LUCK-----"
100 PRINT "Moves are   7.8.9"
110 PRINT "            4.5.6"
120 PRINT "            1.2.3"
# 130 PRINT
140 PRINT "10 = no move for the rest of the game"
150 PRINT "-1 = give up, situation hopeless"
160 PRINT " 0 = a tremendous (but unfortunately random) leap."
170 PRINT
180 DIM A(10,20),A1(10,20),N(12),L(5),M(5),L1(5),M1(5)
190 REM
210 FOR B=1 TO 10
220 FOR C=1 TO 20
230 X=INT(10*RND(1))
240 IF X=5 THEN 270
250 A(B,C)=ASC(" ")
260 GOTO 280
270 A(B,C)=ASC("#")
280 NEXT C
290 NEXT B
300 FOR D=1 TO 10
310 A(D,1)=ASC("#"):A(D,20)=ASC("#")
320 NEXT D
330 FOR F=1 TO 20
340 A(1,F)=ASC("#"):A(10,F)=ASC("#")
350 NEXT F
360 GOTO 410
370 H=INT(2+8*RND(1))
380 I=INT(2+18*RND(1))
390 IF A(H,I)<>ASC(" ") THEN 370
400 RETURN
410 GOSUB 370
420 A(H,I)=ASC("o")
430 J=H:K=I
440 FOR N9=1 TO 5
450 GOSUB 370
460 A(H,I)=ASC("+")
470 L(N9)=H:M(N9)=I
480 NEXT N9
490 FOR B1=1 TO 10:FOR B2=1 TO 20:A1(B1,B2)=A(B1,B2):NEXT B2:NEXT B1
500 FOR B1=1 TO 5:L1(B1)=L(B1):M1(B1)=M(B1):NEXT B1
520 J1=J:K1=K
530 Y9=0
540 FOR D2=1 TO 10
550 FOR B2=1 TO 20
560 N$=CHR$(A(D2,B2))
570 PRINT N$;
580 NEXT B2
590 PRINT
600 NEXT D2
610 IF Y9 <> 10 THEN 640
620 PRINT
630 GOTO 890
640 INPUT Y9
650 J2=J:K2=K
660 IF Y9=0 THEN 860
670 IF Y9 < 0 THEN 1230
680 IF Y9=10 THEN 1070
690 ON Y9 GOTO 820,800,780,840,890,760,700,720,740
700 J=J-1:K=K-1
710 GOTO 890
720 J=J-1
730 GOTO 890
740 J=J-1:K=K+1
750 GOTO 890
760 K=K+1
770 GOTO 890
780 J=J+1:K=K+1
790 GOTO 890
800 J=J+1
810 GOTO 890
820 J=J+1:K=K-1
830 GOTO 890
840 K=K-1
850 GOTO 890
860 PRINT "$6,000,000 jump!!!"
870 J=INT(2+8*RND(1))
880 K=INT(2+18*RND(1))
890 IF A(J,K)=ASC("#") THEN 1260
900 A(J2,K2)=ASC(" ")
910 A(J,K)=ASC("o")
920 GOTO 1070
930 REM INTERCEPTOR MOVEMENT
940 IF A(X,Y)=ASC("#") THEN 1040
950 X2=X:Y2=Y
960 X=SGN(J-X):Y=SGN(K-Y)
970 X=X+X2:Y=Y+Y2
980 IF A(X,Y)=ASC("o") THEN 1050
990 IF A(X,Y)=ASC(" ") THEN 1020
1000 A(X2,Y2)=ASC(" ")
1010 RETURN
1020 A(X,Y)=ASC("+")
1030 A(X2,Y2)=ASC(" ")
1040 RETURN
1050 G9=99
1060 RETURN
1070 FOR N9=1 TO 5
1080 X=L(N9):Y=M(N9)
1090 G9=0
1100 GOSUB 940
1110 IF G9 <> 0 THEN 1240
1120 L(N9)=X:M(N9)=Y
1130 NEXT N9
1140 FOR N9=1 TO 5
1150 IF A(L(N9),M(N9)) <> ASC(" ") THEN 1170
1160 A(L(N9),M(N9))=ASC("+")
1170 NEXT N9
1180 FOR N9=1 TO 5
1190 IF A(L(N9),M(N9)) <> ASC("#") THEN 540
1200 NEXT N9
1210 PRINT "You have destroyed all your opponents - the game is yours."
1220 GOTO 1290
1230 PRINT "Give up, eh."
1240 PRINT "*** You have been destroyed by a lucky computer. ***"
1250 GOTO 1290
1260 PRINT "High Voltage!!!!!!!!!!"
1270 PRINT "***** ZAP ***** You're dead!!!"
1280 PRINT
1290 PRINT  "Another game (y/n)";
1300 INPUT N9$
1310 IF N9$ = "N" THEN 1400
1320 PRINT "Same setup (y/n)";
1330 INPUT N9$
1340 IF N9$ <> "Y" THEN 190
1350 FOR B1=1 TO 10:FOR B2=1 TO 20:A(B1,B2)=A1(B1,B2):NEXT B2:NEXT B1
1360 FOR B1=1 TO 5:L(B1)=L1(B1):M(B1)=M1(B1):NEXT B1
1380 J=J1:K=K1
1390 GOTO 530
1400 END

# END
