# SEABAT
# By Vincent Erickson.
# From David H. Ahl & Steve North, More BASIC Computer Games, Workman, 1980.

# This program goes by 'Seabattle', 'Sea battle', 'Seabat', and 'Underwater Pie
# Lob'. Vincent's name is variously given as either 'Erikson' or 'Erickson'.
# Development took place on and off from 1977 through 1979.

# Changes:
# - Formatting of much printed output.
# - Added author and year to title, adjusted layout.
# - Appended the instructions (line 8000 onwards).
# - Inserted the prompt for instructions on lines 275-277.
# - Added a default player name (line 295).
# - Changed STOP to END on line 6250, so that the game exits quietly.
# - Added the torpedo suspense delay on line 2395 (and 2420).
# - Added subroutine at line 6500 to map between game course codes (1 through 8,
#   clockwise from north) and the numeric keypad. Altered lines 6100 - 6125 to
#   accept keypad directions from the captain, and line 2150 to report keypad
#   directions to the captain. Altered the instructions to match.
# - Altered lines 5120 - 5165 to use the full range of codes, to use DELAY
#   instead of a delay loop, and to impose a time limit on the response.
# - Modifed line 3320 to state current depth before a change request.
# - Added line 3535, to append current depth to the status report.
# - Added line 4165 to fix bug when sabotaging too close to map edge.
# - Added line 4085 to avoid a potential division-by-zero.
# - Changed 'C1>F OR' to '(C1>F) OR' on line 4540. While MS BASIC doesn't allow
#   whitespace in keywords (apart from GO TO), brassica does and reads 'FOR'.

10 PRINT TAB(26);"SEABAT"
12 PRINT TAB(20);"Creative Computing"
14 PRINT TAB(18);"Morristown, New Jersey"
16 PRINT
18 PRINT TAB(20);"by Vincent Erikson"
22 PRINT TAB(11);"converted to MS BASIC by Steve North"
24 PRINT TAB(27);"1979"
30 PRINT:PRINT:PRINT
40 REM
50 REM  PROGRAM BY VINCENT ERIKSON
60 REM   ORIGINALLY IN H.P. BASIC
70 REM   CONVERTED TO MICROSOFT BASIC BY S.N.
80 REM
90 REM  NOTE THE FOLLOWING ABOUT CONVERSIONS:
100 REM   1) RESTORE <LINE NUMBER> MEANS TO SET THE DATA
110 REM      POINTER TO THE SPECIFIED LINE. THIS IS ONLY
120 REM      PRESENT IN TRS-80 LEVEL II AND CP/M BASIC.
130 REM      FOR OTHERS, IMPROVISE BY USING A RESTORE, AND
140 REM      FOR...NEXT WITH READ STATEMENTS TO SKIP OVER
150 REM      THE DATA THAT SHOULD BE IGNORED.
160 REM
170 REM   2) LOGICAL EXPRESSIONS ARE USED OFTEN.  A TRUE
180 REM      EXPRESSION EVALUATES AS A (-1) AND A FALSE EXPRESSION
190 REM      EVALUATES AS A (0).  THUS IF THE PROGRAM SAYS:
200 REM          X = (D<50)
210 REM      IT MEANS, LET X=0 IF D>=50, AND LET X=-1 IF D<50.
220 REM      AGAIN, IMPROVISE IF YOUR BASIC DOESN'T HAVE THIS
230 REM      (BUT ALL MICROSOFT BASICS DO.)
240 REM
245 REM   The real name of this program is, "Underwater Pie Lob"
250 REM *** PROGRAM FOLLOWS ***
260 REM ***
270 DIM A(20,20),D(9):GOSUB 6500
275 PRINT "Would you like instructions (y/n)";
276 INPUT A$
277 IF LEFT$(A$, 1) = "Y" THEN GOSUB 8000
280 PRINT "What is your name";
290 INPUT N$
295 IF LEN(N$)=0 THEN N$="Captain"
300 PRINT
310 REM *** SET UP AREA ***
320 FOR I=1 TO 20
322 FOR J=1 TO 20
324 A(I,J)=0
326 NEXT J
328 NEXT I
330 REM *** ISLAND ***
340 RESTORE 6300
350 FOR X=7 TO 13
360 FOR Y=7 TO 12
370 READ A(X,Y)
380 NEXT Y
390 NEXT X
400 REM *** SUB ***
410 S1=10: S2=10
420 A(S1,S2)=2
430 REM *** ENEMY SHIPS ***
440 S=INT(RND(1)*16)+15
450 RESTORE 6090
460 FOR X=1 TO (INT(RND(1)*4)+1)*2-1
470 READ D8,D9
480 NEXT X
490 FOR X=1 TO S
500 X1=INT(RND(1)*20)+1
510 X2=INT(RND(1)*20)+1
520 IF A(X1,X2)<>0 THEN 500
530 A(X1,X2)=3
540 NEXT X
550 PRINT "You must destroy";S;"enemy ships to win, ";N$;"."
560 REM *** HEADQUARTERS ***
570 S3=INT(RND(1)*20)+1
580 S4=INT(RND(1)*20)+1
590 IF A(S3,S4)<>0 THEN 570
600 A(S3,S4)=4
610 REM *** UNDERWATER MINES ***
620 FOR X=1 TO INT(RND(1)*8)+8
630 X1=INT(RND(1)*20)+1
640 X2=INT(RND(1)*20)+1
650 IF A(X1,X2)<>0 THEN 630
660 A(X1,X2)=5
670 NEXT X
680 REM *** SEA MONSTERS ***
690 FOR X=1 TO 4
700 X1=INT(RND(1)*18)+2
710 X2=INT(RND(1)*18)+2
720 IF A(X1,X2)<>0 THEN 700
730 A(X1,X2)=6
740 RESTORE 6090
750 FOR Y=1 TO INT(RND(1)*8)+1
760 READ M1,M2
770 NEXT Y
780 NEXT X
790 REM *** SET STARTING VALUES ***
800 FOR I=1 TO 9
802 D(I)=0
804 NEXT I
810 C=30
820 P=6000
830 F=2500
840 T=10
850 M=3
860 D=100
870 D2=2
880 REM *** COMMAND SECTION ***
890 PRINT: PRINT: PRINT "What are your orders, ";N$;
900 INPUT O
910 ON INT(O+1) GOTO 1040,1680,2220,2680,3250,3410,3700,3880,4400,4660
920 PRINT "The commands are:"
930 PRINT "     #0: Navigation"
940 PRINT "     #1: Sonar"
950 PRINT "     #2: Torpedo control"
960 PRINT "     #3: Polaris missile control"
970 PRINT "     #4: Maneuvering"
980 PRINT "     #5: Status/damage report"
990 PRINT "     #6: Headquarters"
1000 PRINT "     #7: Sabotage"
1010 PRINT "     #8: Power conversion"
1020 PRINT "     #9: Surrender"
1030 GOTO 880
1040 REM *** #0: NAVIGATION ***
1050 IF D(1) >= 0 THEN 1080
1060 PRINT "Engines are under repair, ";N$;"."
1070 GOTO 880
1080 IF C>8 THEN 1110
1090 PRINT "Not enough crew to man the engines, ";N$;"."
1100 GOTO 880
1110 D1=1-((.23+RND(1)/10)*(-(D<=50)))
1120 GOSUB 6090
1130 PRINT "Power available = ";STR$(P);". Power to use";
1140 INPUT P1
1150 IF P1<0 OR P1>P THEN 1130
1160 IF P1 <= 1000 THEN 1210
1170 IF RND(1)<.43 THEN 1210
1180 PRINT "Atomic pile goes supercritical, ";N$;"!!! Headquarters"
1190 PRINT "will warn all subs to stay away from radioactive area!!!"
1200 GOTO 6180
1210 X=S1
1220 Y=S2
1230 Q1=1
1240 FOR X2=1 TO INT(INT(P1/100+.5)*D1+.5)
1250 IF X+X1>0 AND X+X1<21 AND Y+Y1>0 AND Y+Y1<21 THEN 1280
1260 PRINT "You can't leave the area, ";N$;"!!"
1270 GOTO 1340
1280 ON A(X+X1,Y+Y1)+1 GOTO 1290,1330,1630,1390,1440,1470,1490
1290 X=X+X1
1300 Y=Y+Y1
1310 P=P-100
1320 GOTO 1520
1330 PRINT "You almost ran aground, ";N$;"!!"
1340 A(X,Y)=2
1350 A(S1,S2)=0
1360 S1=X
1370 S2=Y
1380 GOTO 4690
1390 IF D>50 THEN 1290
1400 PRINT "You rammed a ship!!! You're both sunk, ";N$;"!!"
1410 S=S-1
1420 IF S=0 THEN 6260
1430 GOTO 6180
1440 IF D>50 THEN 1290
1450 PRINT "You rammed headquarters!! You're sunk!!"
1460 GOTO 6180
1470 PRINT "You've been blown up by a mine, ";N$;"!!"
1480 GOTO 6180
1490 IF RND(1)<.21 THEN 1630
1500 PRINT "You were eaten by a sea monster, ";N$;"!!"
1510 GOTO 6180
1520 REM *** CHECK FOR NEARBY SEA MONSTERS ***
1530 FOR X3=X-2 TO X+2
1540 FOR Y3=Y-2 TO Y+2
1550 IF X3<1 OR X3>20 OR Y3<1 OR Y3>20 THEN 1610
1560 IF A(X,Y)<>6 THEN 1610
1570 IF RND(1)<.25 THEN 1500
1580 IF Q1=0 THEN 1610
1590 PRINT "You just had a narrow escape with a sea monster, ";N$;"!!"
1600  Q1=0
1610 NEXT Y3
1620 NEXT X3
1630 NEXT X2
1640 PRINT "Navigation complete. Power left = ";STR$(P);"."
1650 IF P>0 THEN 1340
1660 PRINT "Atomic pile has gone dead!!! Sub sinks, crew suffocates."
1670 GOTO 6180
1680 REM *** #1: SONAR ***
1690 IF D(2) >= 0 THEN 1720
1700 PRINT "Sonar is under repair, ";N$;"."
1710 GOTO 880
1720 IF C>5 THEN 1750
1730 PRINT "Not enough crew to work sonar, ";N$;"."
1740 GOTO 880
1750 PRINT "Option #";
1760 INPUT O
1770 ON INT(O+1) GOTO 1790,2010
1780 GOTO 1750
1790 REM *** PRINT OUT MAP ***
1800 PRINT
1810 FOR X=1 TO 20
1820 FOR Y=1 TO 20
1830 DATA "   ","***","(X)","\S/","!H!"," $ ","-M-"
1840 IF A(X,Y)<>0 THEN 1880
1850 IF X<>1 AND X<>20 AND Y<>1 AND Y<>20 THEN 1880
1860 PRINT " . ";
1870 GOTO 1950
1880 RESTORE 1830
1890 FOR X1=1 TO A(X,Y)+1
1900 READ A$
1910 NEXT X1
1920 IF D<50 AND RND(1)<.23 AND A(X,Y)<>1 AND A(A,Y)<>2 THEN 1860
1930 IF RND(1)<.15 AND A(X,Y)>2 THEN 1860
1940 PRINT A$;
1950 NEXT Y
1960 PRINT
1970 NEXT X
1980 P=P-50
1990 IF P>0 THEN 880
2000 GOTO 1660
2010 REM *** DIRECTIONAL INFORMATION ***
2020 FOR I=1 TO 5
2022 B(I)=0
2024 NEXT I
2030 PRINT "Direction   # of ships    Distances"
2040 RESTORE 6090
2050 FOR X=1 TO 8
2060 READ X1,Y1
2070 X3=0
2080 FOR X4=1 TO 20
2090 IF S1+X1*X4<1 OR S1+X1*X4>20 OR S2+Y1*X4<1 OR S2+Y1*X4>20 THEN 2140
2100 IF A(S1+X1*X4,S2+Y1*X4)<>3 THEN 2130
2110 X3=X3+1
2120 B(X3)=X4
2130 NEXT X4
2140 IF X3=0 THEN 2200
2150 PRINT "   ";KM(2,X),X3,
2160 FOR X4=1 TO X3
2170 PRINT B(X4);
2180 NEXT X4
2190 PRINT
2200 NEXT X
2210 GOTO 1980
2220 REM *** #2: TORPEDO CONTROL ***
2230 IF D(3) >=0 THEN 2260
2240 PRINT "Torpedo tubes are under repair, ";N$;"."
2250 GOTO 880
2260 IF C>=10 THEN 2290
2270 PRINT "Not enough crew to fire torpedo, ";N$;"."
2280 GOTO 880
2290 IF T THEN 2320
2300 PRINT "No torpedoes left, ";N$;"."
2310 GOTO 880
2320 IF D<2000 THEN 2360
2330 IF RND(1)>.5 THEN 2360
2340 PRINT "Pressure implodes upon firing...you're crushed!!"
2350 GOTO 6180
2360 GOSUB 6080
2370 X=S1
2380 Y=S2
2390 FOR X2=1 TO INT(7+5*(-(D>50))-RND(1)*4+.5)
2395 DELAY .5
2400 IF X+X1>0 AND X+X1<21 AND Y+Y1>0 AND Y+Y1<21 THEN 2460
2410 PRINT:PRINT "Torpedo out of sonar range...ineffectual, ";N$;"."
2420 T=T-1:DELAY 0
2430 P=P-150
2440 IF P>0 THEN 4690
2450 GOTO 1660
2460 ON A(X+X1,Y+Y1)+1 GOTO 2470,2510,2650,2540,2580,2610,2630
2470 X=X+X1
2480 Y=Y+Y1
2490 PRINT "..!..";
2500 GOTO 2650
2510 PRINT "Whump!":PRINT "You took out some island, ";N$;"!"
2520 A(X+X1,Y+Y1)=0
2530 GOTO 2420
2540 PRINT "Ouch!!!":PRINT "You got one, ";N$;"!!"
2550 S=S-1
2560 IF S<>0 THEN 2520
2570 GOTO 6260
2580 PRINT "Boom!!!":PRINT "You blew up your headquarters, ";N$;"!!!"
2590 S3=0: S4=0: D2=0
2600 GOTO 2520
2610 PRINT "Blam!!":PRINT "Shot wasted on a mine, ";N$;"!!"
2620 GOTO 2520
2630 PRINT "Crunch.":PRINT "A sea monster had a torpedo for lunch, ";N$;"!!"
2640 GOTO 2420
2650 NEXT X2
2660 PRINT:PRINT "Dud."
2670 GOTO 2420
2680 REM #3: POLARIS MISSILE CONTROL ***
2690 IF D(4) >= 0 THEN 2720
2700 PRINT "Missile silos are under repair, ";N$;"."
2710 GOTO 880
2720 IF C>23 THEN 2750
2730 PRINT "Not enough crew to launch a missile, ";N$;"."
2740 GOTO 880
2750 IF M<>0 THEN 2780
2760 PRINT "No missiles left, ";N$;"."
2770 GOTO 880
2780 IF D>50 AND D<2000 THEN 2850
2790 PRINT "Recommend that you do not fire at this depth...Proceed";
2800 INPUT A$
2810 IF LEFT$(A$,1)="N" THEN 880
2820 IF RND(1)<.5 THEN 2850
2830 PRINT "Missile explodes upon firing, ";N$;"!! You're dead!!"
2840 GOTO 6180
2850 GOSUB 6080
2860 PRINT "Fuel (lbs.)";
2870 INPUT F1
2880 IF F1>0 AND F1 <= F THEN 2910
2890 PRINT "You have";F;"lbs. left, ";N$;"."
2900 GOTO 2860
2910 F2=INT(F1/75+.5)
2920 IF S1+X1*F2>0 AND S1+X1*F2<21 AND S2+Y1*F2>0 AND S2+Y1*F2<21 THEN 2980
2930 PRINT "Missile out of tracking range, ";N$;". Missile lost."
2940 M=M-1
2950 F=F-F1
2960 P=P-300
2970 GOTO 2440
2980 D3=0: D4=0: D5=0: D6=0
2990 FOR X=S1+X1*F2-1 TO S1+X1*F2+1
3000 FOR Y=S2+Y1*F2-1 TO S2+Y1*F2+1
3010 IF X<1 OR X>20 OR Y<1 OR Y>20 THEN 3140
3020 D3=D3-(A(X,Y)=3)
3030 D4=D4-(A(X,Y)=6)
3040 D5=D5-(A(X,Y)=5)
3050 D6=D6-(A(X,Y)=1)
3060 IF A(X,Y)<>4 THEN 3100
3070 PRINT "You've destroyed your headquarters, ";N$;"!!!"
3080 D3=0: S4=0: D2=0
3090 GOTO 3130
3100 IF A(X,Y)<>2 THEN 3130
3110 PRINT "You just destroyed yourself, ";N$;"!!!    Dummy!!"
3120 GOTO 6180
3130 A(X,Y)=0
3140 NEXT Y
3150 NEXT X
3160 IF D6=0 THEN 3180
3170 PRINT "You blew out some island, ";N$;"."
3180 IF D5=0 THEN 3200
3190 PRINT "You destroyed";D5;"mines, ";N$;"."
3200 IF D4=0 THEN 3220
3210 PRINT "You got";D4;"sea monsters, ";N$;"!!!   Good work!!"
3220 PRINT "You destroyed";D3;"enemy ships, ";N$;"!!!"
3230 S=S-D3
3240 GOTO 2940
3250 REM *** MANEUVERING ***
3260 IF D(5)>=0 THEN 3290
3270 PRINT "Ballast controls are being repaired, ";N$;"."
3280 GOTO 890
3290 IF C>12 THEN 3320
3300 PRINT "There are not enough crew to work the controls, ";N$;"."
3310 GOTO 880
3320 PRINT "Current depth = ";STR$(D);". New depth";
3330 INPUT D1
3340 IF D1 >= 0 AND D1<3000 THEN 3370
3350 PRINT "Hull crushed by pressure, ";N$;"!!"
3360 GOTO 6180
3370 P=P-INT(ABS((D-D1)/2+.5))
3380 PRINT "Maneuver complete. Power loss = ";STR$(INT(ABS((D-D1)/2+.5)));"."
3390 D=D1
3400 GOTO 4690
3410 REM *** #5: STATUS / DAMAGE REPORT ***
3420 IF D(6) >= 0 THEN 3450
3430 PRINT "No reports are able to get through, ";N$;"."
3440 GOTO 880
3450 IF C>3 THEN 3480
3460 PRINT "No one left to give the report, ";N$;"."
3470 GOTO 880
3480 PRINT "# of enemy ships left.......";S
3490 PRINT "# of power units left.......";P
3500 PRINT "# of torpedoes left.........";T
3510 PRINT "# of missiles left..........";M
3520 PRINT "# of crewmen left...........";C
3530 PRINT "lbs. of fuel left...........";F
3535 PRINT "ft. deep....................";D
3540 PRINT
3550 PRINT "Want a damage report";
3560 INPUT A$
3570 IF LEFT$(A$,1)="N" THEN 3670
3580 PRINT "Item"," Damage  (+ good, 0 neutral, - bad)"
3590 PRINT "----"," ------"
3600 DATA "Engines","Sonar","Torpedoes","Missiles","Maneuvering"
3610 DATA "Status","Headquarters","Sabotage","Converter"
3620 RESTORE 3600
3630 FOR X=1 TO 9
3640 READ A$
3650 PRINT A$,D(X)
3660 NEXT X
3670 PRINT "You are at location (";S1;",";S2;")."
3680 PRINT
3690 GOTO 880
3700 REM #6: HEADQUARTERS ***
3710 IF D(7) >=0 THEN 3740
3720 PRINT "Headquarters is damaged.  Unable to help, ";N$;"."
3730 GOTO 880
3740 IF D2<>0 THEN 3770
3750 PRINT "Headquarters is deserted, ";N$;"."
3760 GOTO 880
3770 IF SQR((S1-S3)^2+(S2-S4)^2) <= 2 AND D<51 THEN 3800
3780 PRINT "Unable to comply with docking orders, ";N$;"."
3790 GOTO 880
3800 PRINT "Divers from headquarters bring out supplies and men."
3810 P=4000
3820 T=8
3830 M=2
3840 F=1500
3850 C=25
3860 D2=D2-1
3870 GOTO 4690
3880 REM *** #7: SABOTAGE ***
3890 IF D(8)>=0 THEN 3920
3900 PRINT "Hatches inaccessible, ";N$;".  No sabotages possible."
3910 GOTO 880
3920 IF C>10 THEN 3950
3930 PRINT "Not enough crew to go on a mission, ";N$;"."
3940 GOTO 880
3950 D3=0:D4=0
3960 FOR X=S1-2 TO S1+2
3970 FOR Y=S2-2 TO S2+2
3980 IF X<1 OR X>20 OR Y<1 OR Y>20 THEN 4010
3990 D3=D3-(A(X,Y)=3)
4000 D4=D4-(A(X,Y)=6)
4010 NEXT Y
4020 NEXT X
4030 IF D3<>0 THEN 4060
4040 PRINT "No ships in range, ";N$;"."
4050 GOTO 880
4060 PRINT "There are";D3;"ships in range, ";N$;"."
4070 PRINT "How many men are going, ";N$;
4080 INPUT Q1
4085 IF Q1<1 THEN 4070
4090 IF C-Q1 >= 10 THEN 4120
4100 PRINT "You must leave at least 10 men on board, ";N$;"."
4110 GOTO 4070
4120 D5=INT(D3/Q1+.5)
4130 D6=0
4140 FOR X=S1-2 TO S1+2
4150 FOR Y=S2-2 TO S2+2
4160 IF D3/Q1>1-RND(1) AND RND(1)+D3/Q1<.9 THEN 4220
4165 IF X<1 OR X>20 OR Y<1 OR Y>20 THEN 4220
4170 IF A(X,Y)<>3 THEN 4220
4180 D6=D6+1
4190 A(X,Y)=0
4200 S=S-1
4210 IF S=0 THEN 6260
4220 NEXT Y
4230 NEXT X
4240 PRINT D6;"Ships were destroyed, ";N$;"."
4250 D6=0: D7=0
4260 FOR X=1 TO Q1
4270 D7=D7-(RND(1)>.6)
4280 NEXT X
4290 FOR X=1 TO Q1-D7
4300 D6=D6-(RND(1)<.15)
4310 NEXT X
4320 IF D4=0 THEN 4360
4330 PRINT "A sea monster smells the men on the way back!!!"
4340 PRINT D7;"Men were eaten, ";N$;"!!"
4350 C=C-D7
4360 PRINT D6;"Men were lost through accidents, ";N$;"."
4370 C=C-D6
4380 P=P-INT(10*Q1+RND(1)*10)
4390 GOTO 4700
4400 REM *** #8: POWER CONVERTER ***
4410 IF D(9) >= 0 THEN 4440
4420 PRINT "Power converter is damaged, ";N$;"."
4430 GOTO 880
4440 IF C>5 THEN 4470
4450 PRINT "Not enough men to work the converter, ";N$;"."
4460 GOTO 880
4470 PRINT "Option? (1 = fuel to power,  2 = power to fuel)";
4480 INPUT O
4490 ON O GOTO 4510,4580
4500 GOTO 4470
4510 REM *** FUEL TO POWER CONVERSION ***
4520 PRINT "Fuel available = ";STR$(F);". Convert";
4530 INPUT C1
4540 IF (C1>F) OR C1<0 THEN 4520
4550 F=F-C1
4560 P=P+INT(C1/3)
4570 GOTO 4640
4580 REM *** POWER TO FUEL CONVERSION ***
4590 PRINT "Power available = ";STR$(P-1);". Convert";
4600 INPUT C1
4610 IF C1>P-1 OR C1<0 THEN 4590
4620 P=P-C1
4630 F=F+INT(C1*3)
4640 PRINT "Conversion complete. Power = ";STR$(P);". Fuel = ";STR$(F);"."
4650 GOTO 4690
4660 REM *** #9: SURRENDER ***
4670 PRINT "Coward!! You're not very patriotic, ";N$;"!!!"
4680 GOTO 6180
4690 REM *** RETALIATION SECTION ***
4700 Q=0
4710 FOR X=S1-4 TO S1+4
4720 FOR Y=S2-4 TO S2+4
4730 IF X<1 OR X>20 OR Y<1 OR Y>20 THEN 4760
4740 IF A(X,Y)<>3 THEN 4760
4750 Q=Q+(RND(1)/SQR((S1-X)^2+(S2-Y)^2))
4760 NEXT Y
4770 NEXT X
4780 IF Q THEN 4810
4790 PRINT:PRINT "No ships in range to depth charge you, ";N$;"!!"
4800 GOTO 5210
4810 PRINT:PRINT "Depth charges off ";
4820 IF RND(1)>.5 THEN 4850
4830 PRINT "port side, ";N$;"!!!"
4840 GOTO 4860
4850 PRINT "starboard side, ";N$;"!!!"
4860 IF Q>.13 OR RND(1)>.92 THEN 4890
4870 PRINT "No real damage sustained, ";N$;"."
4880 GOTO 5220
4890 IF Q>.36 OR RND(1)>.96 THEN 4940
4900 PRINT "Light superficial damage, ";N$;"."
4910 P=P-50
4920 D(INT(RND(1)*9)+1)=-RND(1)*2
4930 GOTO 5210
4940 IF Q>.6 OR RND(1)>.975 THEN 5020
4950 PRINT "Moderate damage. Repairs needed."
4960 P=P-75+INT(RND(1)*30)
4970 FOR Y=1 TO 2
4980 X=INT(RND(1)*9)+1
4990 D(X)=D(X)-RND(1)*8
5000 NEXT Y
5010 GOTO 5210
5020 IF Q>.9 OR RND(1)>.983 THEN 5100
5030 PRINT "Heavy damage!! Repairs immediate, ";N$;"!!!"
5040 P=P-(200+INT(RND(1)*76))
5050 FOR X=1 TO 4+INT(RND(1)*2)
5060 Y=INT(RND(1)*9)+1
5070 D(Y)=D(Y)-RND(1)*11
5080 NEXT X
5090 GOTO 5210
5100 PRINT "Damage critical!!!!   We need help!!!"
5110 A$="VRAVUKXCNVPCRHFDRSAXQURLQTRHXYACVFZYITLCBSSYYKDQIPCAEGQGPCNOTSIO"
5120 X=INT(RND(1)*(LEN(A$)-3))+1
5130 PRINT "Send 'help' in code. Here is the code: ";
5134 DELAY 0.5:PRINT MID$(A$,X,4);:DELAY 1.5
5136 PRINT CHR$(13);TAB(39);"XXXX";:DELAY 0.25:PRINT CHR$(13);TAB(39);"****"
5140 T0 = SYST(1):INPUT "Enter code";B$
5150 PRINT
5160 IF B$<>MID$(A$,X,4) THEN 5190
5165 IF SYST(1)-T0>5 THEN PRINT "Too slow! Help comes too late!!!":GOTO 6180
5170 PRINT "Fast work, ";N$;"!! Help arrives in time to save you!!!"
5180 GOTO 5040
5190 PRINT "Message garbled, ";N$;"...No help arrives!!!"
5200 GOTO 6180
5210 REM *** MOVE SHIPS / SEA MONSTERS ***
5220 IF D(1)>=0 OR D(3)>=0 OR D(4)>=0 OR D(5)>=0 OR D(6)>=0 OR D(7)>=0 THEN 5260
5230 IF D(8) >= 0 OR D(9) >= 0 THEN 5260
5240 PRINT "Damage too much, ";N$;"!!!   You're sunk!!"
5250 GOTO 6180
5260 REM *** MOVE SHIPS / SEA MONSTERS ***
5270 PRINT: PRINT: PRINT "---*** Result of last enemy maneuver ***---"
5280 FOR X=1 TO 20
5290 FOR Y=1 TO 20
5300 IF A(X,Y)<>3 THEN 5690
5310 REM *** MOVE A SHIP ***
5320 W=D8
5330 V=D9
5340 IF X+W>0 AND X+W<21 AND Y+V>0 AND Y+V<21 THEN 5420
5350 FOR X0=19 TO 1 STEP -1
5360 IF A(X-W*X0,Y-V*X0)<>0 THEN 5400
5370 A(X-W*X0,Y-V*X0)=3
5380 A(X,Y)=0
5390 GOTO 6000
5400 NEXT X0
5410 STOP
5420 ON A(X+W,Y+V)+1 GOTO 5430,5460,5530,5460,5560,5600,5650
5430 A(X+W,Y+V)=3
5440 A(X,Y)=0
5450 GOTO 6000
5460 REM *** CHANGE DIRECTION ***
5470 RESTORE 6090
5480 FOR X0=1 TO INT(RND(1)*8)+1
5490 READ W,V
5500 NEXT X0
5510 IF X+W<1 OR X+W>20 OR Y+V<1 OR Y+V>20 THEN 5470
5520 GOTO 5420
5530 IF D>50 THEN 5460
5540 PRINT "*** You've been rammed by a ship, ";N$;"!!!"
5550 GOTO 6180
5560 IF RND(1)<.15 THEN 5460
5570 PRINT "*** Your headquarters was rammed, ";N$;"!!!"
5580 S3=0: S4=0: D2=0: A(X+W,Y+V)=0
5590 GOTO 5620
5600 IF RND(1)<.7 THEN 5460
5610 PRINT "*** Ship destroyed by a mine, ";N$;"!!!"
5620 S=S-1
5630 IF S<>0 THEN 5440
5640 GOTO 6260
5650 IF RND(1)<.8 THEN 5460
5660 PRINT "*** Ship eaten by a sea monster, ";N$;"!!"
5670 S=S-1
5680 GOTO 5630
5690 REM *** MOVE A SEA MONSTER ***
5700 IF A(X,Y)<>6 THEN 6000
5710 IF X+M1<1 OR X+M1>20 OR Y+M2<1 OR Y+M2>20 THEN 5760
5720 ON A(X+M1,Y+M2)+1 GOTO 5730,5760,5830,5850,5900,5730,5930
5730 A(X+M1,Y+M2)=6
5740 A(X,Y)=0
5750 GOTO 6000
5760 REM *** CHANGE DIRECTION ***
5770 RESTORE 6090
5780 FOR X0=1 TO INT(RND(1)*8)+1
5790 READ M1,M2
5800 NEXT X0
5810 IF X+M1<1 OR X+M1>20 OR Y+M2<1 OR Y+M2>20 THEN 5760
5820 GOTO 5720
5830 PRINT "*** You've been eaten by a sea monster, ";N$;"!!"
5840 GOTO 6180
5850 IF RND(1)>.2 THEN 5760
5860 PRINT "*** Ship eaten by a sea monster, ";N$;"!!"
5870 S=S-1
5880 IF S<>0 THEN 5730
5890 GOTO 6260
5900 PRINT "*** A sea monster ate your headquarters, ";N$;"!!"
5910 S3=0: S4=0: D2=0
5920 GOTO 5730
5930 IF RND(1)<.75 THEN 5760
5940 PRINT "*** A sea monster fight, ";N$;"!!! ";
5950 IF RND(1)<.8 THEN 5980
5960 PRINT "And one dies!!"
5970 GOTO 5730
5980 PRINT "It's a tie!!"
5990 GOTO 5760
6000 NEXT Y
6010 NEXT X
6020 REM *** MAKE REPAIRS ***
6030 FOR Y=1 TO 9
6040 X=INT(RND(1)*9)+1
6050 D(X)=D(X)+(RND(1)*(2+RND(1)*2))*(1+(-(D<51) OR -(D>2000)))*(-(D(X)<3))
6060 NEXT Y
6070 GOTO 880
6080 REM *** GOSUB FOR COURSE / DIRECTION ***
6090 DATA -1,0,-1,1,0,1,1,1,1,0,1,-1,0,-1,-1,-1
6100 PRINT "Course (1-9)";
6110 INPUT C1:C1=INT(C1+.5)
6120 IF C1<1 OR C1>9 OR C1=5 THEN 6100
6125 C1=KM(1,C1)
6130 RESTORE 6090
6140 FOR X9=1 TO INT(C1+.5)
6150 READ X1,Y1
6160 NEXT X9
6170 RETURN
6180 REM *** DESTROYED ? ***
6190 PRINT "There are still";S;"enemy ships left, ";N$;"."
6200 PRINT "You will be demoted to the rank of deck scrubber!!!"
6210 PRINT "Want another game";
6220 INPUT A$
6230 IF LEFT$(A$,1)<>"Y" THEN 6250
6240 GOTO 310
6250 END
6260 PRINT "Good work, ";N$;"!!!  You got them all!!!"
6270 PRINT "Promotion and commendations will be given immediately!!!"
6280 GOTO 6210
6300 DATA 0,1,1,1,0,0,0,1,1,1,1,0,1,1,1,0,1,1,1,1,0,0,0,1
6310 DATA 1,1,0,0,1,1,0,1,1,0,1,0,0,0,1,0,0,0
6320 END
6500 REM *** KEYPAD MAPPING ***
6510 RESTORE 6500: DIM KM(2,9)
6520 FOR I=1TO2: FOR J=1TO9: READ KM(I,J): NEXT J,I
6530 DATA 6,5,4,7,0,3,8,1,2,8,9,6,3,2,1,4,7,0
6540 RETURN
8000 REM   *** INSTRUCTIONS ***
8010 PRINT
8020 PRINT "This is the game of Sea Battle!!! The object of the game is to"
8030 PRINT "destroy all of the enemy ships in your 20 by 20 area with the"
8040 PRINT "various weapons in your submarine's arsenal. You must do this,"
8050 PRINT "however, without letting the enemy destroy you first!!"
8060 PRINT
8070 PRINT "There are several interesting hazards in the game. They include:"
8080 PRINT "  .. depth charges from nearby enemy ships."
8090 PRINT "  .. very hungry sea monsters!!"
8100 PRINT "  .. and hidden underwater mines."
8110 PRINT ""
8120 PRINT "The depth changes are effective to any depth, but they lose"
8130 PRINT "their effectivness over distance, so the farther you are from"
8140 PRINT "any ships, the better!"
8150 PRINT "The sea monsters take a meandering course through your area that"
8160 PRINT "may bring it close enough to attack you. You rarely survive."
8170 PRINT "They also like to eat your torpedoes, but missiles will kill them."
8180 PRINT
8190 PRINT "The enemy ships move on every turn, in a fixed course, unless they"
8200 PRINT "encounter obstacles. They will get blown up by mines, and get"
8210 PRINT "eaten by sea monsters too."
8220 PRINT:INPUT "[press enter to continue]",:PRINT
8230 PRINT "You have ten orders that you may give. they are:"
8240 PRINT
8250 PRINT "    #0: Navigation - This command allows you to move in a"
8260 PRINT "particular direction and distance across your area. The"
8270 PRINT " 7 8 9  direction is determined by the graph at left. There"
8280 PRINT "  \'/   are 8 directions to move in, and they are the same"
8290 PRINT " 4-*-6  anytime you are asked for a course. For example,"
8300 PRINT "  /'\   to move north, you would use course #8. The computer"
8310 PRINT " 1 2 3  will also ask for an amount of power. It takes 100 units"
8320 PRINT "of power to move your sub 1 space. Beware of obstacles!!"
8330 PRINT "If you use more than 1000 units in a turn, there is an overload"
8340 PRINT "danger, so be very careful!!"
8350 INPUT "",
8360 PRINT "    #1: Sonar - This command has two options. Option #1 gives"
8370 PRINT "directional information, showing the directions and distances"
8380 PRINT "in which there are enemy ships. This is useful for shooting at long"
8390 PRINT "ranges, where it is difficult to tell if a ship is in direct line."
8400 PRINT
8410 PRINT "  Option #0 prints out a map of your area in a square."
8420 PRINT "It uses symbols for the map: '*' indicates dry land, '$' is"
8430 PRINT "an underwater mine, '\S/' is an enemy ship. '-M-' is a sea monster,"
8440 PRINT "'!H!' is your headquarters, and finally, '(X)' is you!!!"
8450 PRINT
8460 PRINT "Every so often, a '.' will appear inside the screen. This is"
8470 PRINT "a sonar malfunction, and so the object there isn't identified."
8480 PRINT "If you are above 50 feet, waves will show up as '.'."
8490 INPUT "",
8500 PRINT "    #2: Torpedo control - This command allows you to shoot"
8510 PRINT "1 of your 10 torpedoes at enemy ships. The computer will only"
8520 PRINT "require the direction to shoot, using the indicator above."
8530 PRINT "They have a range of 7-13 spaces. One torpedo gets one ship."
8540 INPUT "",
8550 PRINT "    #3: Polaris missile control - This command allows you to"
8560 PRINT "launch one of your Polaris missiles against the enemy. The"
8570 PRINT "computer will ask for a course and fuel. It takes 75 lbs. of fuel"
8580 PRINT "to boost a missile 1 space. Since they are so much more powerful,"
8590 PRINT "they will completely destroy the space they land on, plus all"
8600 PRINT "of the immediately adjacent ones. Missiles destroy everything!!!"
8610 INPUT "",
8620 PRINT "    #4: Maneuvering - This command allows you to change the"
8630 PRINT "depth you're at. You may want to do this if you are badly"
8640 PRINT "damaged, because repairs go on twice as quickly below 2500 ft."
8650 PRINT "and above 50 ft. than in between. You start the game at 100 ft."
8660 PRINT "You use up about 1 power unit for every 2 ft. you change."
8670 INPUT "",
8680 PRINT "    #5: Status/damage report - This command gives you the"
8690 PRINT "status of your sub. It tells you how much is left in your"
8700 PRINT "arsenal, which items are damaged, and how much."
8710 INPUT "",
8720 PRINT "    #6: Headquarters - This command allows scuba divers from"
8730 PRINT "your headquarters to replenish your supply of weapons and men."
8740 PRINT "You must be at 50 ft. or less, and 2 or fewer spaces away to"
8750 PRINT "do this however, and you can only do it twice."
8760 INPUT "",
8770 PRINT "    #7: Sabotage (scuba) - This command allows you to send"
8780 PRINT "men out on a sabotage mission against enemy ships. You may"
8790 PRINT "only go against ships within 3 spaces of you, and you must"
8800 PRINT "leave at least 10 men on board the sub to run it."
8810 INPUT "",
8820 PRINT "    #8: Power conversion - This command allows you to change fuel"
8830 PRINT "to power, or vice-versa."
8840 INPUT "",
8850 PRINT "    #9: Surrender - This command is only for cowards and traitors!!"
8860 INPUT "",
8870 PRINT
8880 PRINT "You start the game with the following supplies:"
8890 PRINT "   6000 units of power, 2500 lbs. of rocket fuel, 10 torpedoes"
8900 PRINT "   3 missiles, 1 headquarters, and a random number of ships."
8910 PRINT
8920 PRINT "  I left some interesting details out of the instructions,"
8930 PRINT "to make playing the game the first few times more interesting."
8940 PRINT
8950 PRINT "You start the game in the island's lagoon, and it is your duty"
8960 PRINT "to seek out and destroy the enemy at all costs!!!"
8970 PRINT:INPUT "[ends]",:PRINT
8980 RETURN

# END
