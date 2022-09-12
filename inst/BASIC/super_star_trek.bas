# SUPER STAR TREK
# By Bob Leedon & Dave Ahl.
# From David H. Ahl & Steve North, BASIC Computer Games, Workman, 1978.

# Changes:
# - Formatting of some printed output.
# - Appended the instructions from line 10000, and a GOSUB to them from line 20.
#   Changed line 6330 to restart another mission from line 30 (instead of 10),
#   so as not to be prompted for instructions a second time.
# - Added the authors (as I understand them) to the instructions title screen.
# - Added subroutine at 9300, to map between original course directions (1
#   through 9, clockwise from east) and numeric keypad directions, while
#   preserving real-valued directions. Called from line 260. Used on lines
#   2300-2320, 4760-4780, 8310-8330, and 8430-9450. Updated the instructions.
# - Changed lines 1280 & 1300 to use INPUT instead of INP.
# - Removed hardware-hitting (POKE) hardcopy options on lines 7540, 7542, 7850.
# - Added notes in the instructions on movement between sectors (within a
#   quadrant), the matrix (y, x) coordinate system, and how to get tehnicians'
#   repair estimates while docked.
# - Trapped potential division by zero error in the libary-computer's direction
#   calculator, on line 8210.
# - Altered lines 6770 & 6780, so that the name of the quadrant is printed at
#   the top of the short-range scanner display.
# - Altered lines 1560-1570 to blink the RED alert on and off upon encountering
#   dastardly Klingons. Could print CHR$(7) (bell) to make a beep here, too.
# - Split line 3870 to that and 3880, so as to insert a time's-up test. This is
#   a bug fix; there was no check after a normal interquadrant warp (probably
#   had been in earlier versions). Alternatively, the test could be made on line
#   2060, after re-numbering the existing line as 2070.
# - Modified line 6220, and added line 6225, to state time is up when a mission
#   is failed by running out of time (as opposed to just stating the stardate).

10 REM SUPER STARTREK - MAY 16,1978 - REQUIRES 24K MEMORY
20 GOSUB 10000
30 REM
40 REM ****        **** STAR TREK ****        ****
50 REM **** SIMULATION OF A MISSION OF THE STARSHIP ENTERPRISE,
60 REM **** AS SEEN ON THE STAR TREK TV SHOW.
70 REM **** ORIGIONAL PROGRAM BY MIKE MAYFIELD, MODIFIED VERSION
80 REM **** PUBLISHED IN DEC'S "101 BASIC Games", BY DAVE AHL.
90 REM **** MODIFICATIONS TO THE LATTER (PLUS DEBUGGING) BY BOB
100 REM *** LEEDOM - APRIL & DECEMBER 1974,
110 REM *** WITH A LITTLE HELP FROM HIS FRIENDS . . .
120 REM *** COMMENTS, EPITHETS, AND SUGGESTIONS SOLICITED --
130 REM *** SEND TO:  R. C. LEEDOM
140 REM ***           WESTINGHOUSE DEFENSE & ELECTRONICS SYSTEMS CNTR.
150 REM ***           BOX 746, M.S. 338
160 REM ***           BALTIMORE, MD  21203
170 REM ***
180 REM *** CONVERTED TO MICROSOFT 8 K BASIC 3/16/78 BY JOHN GORDERS
190 REM *** LINE NUMBERS FROM VERSION STREK7 OF 1/12/75 PRESERVED AS
200 REM *** MUCH AS POSSIBLE WHILE USING MULTIPLE STATEMENTS PER LINE
205 REM *** SOME LINES ARE LONGER THAN 72 CHARACTERS; THIS WAS DONE
210 REM *** BY USING "?" INSTEAD OF "PRINT" WHEN ENTERING LINES
215 REM ***
220 PRINT:PRINT:PRINT:PRINT:PRINT:PRINT:PRINT:PRINT:PRINT:PRINT:PRINT
221 PRINT"                                    ,------*------,"
222 PRINT"                    ,-------------   '---  ------'"
223 PRINT"                     '-------- --'      / /"
224 PRINT"                         ,---' '-------/ /--,"
225 PRINT"                          '----------------'":PRINT
226 PRINT"                    The USS Enterprise --- NCC-1701"
227 PRINT:PRINT:PRINT:PRINT:PRINT
260 CLEAR 600:GOSUB 9300
270 Z$="                         "
330 DIM G(8,8),C(9,2),K(3,3),N(3),Z(8,8),D(8)
370 T=INT(RND(1)*20+20)*100:T0=T:T9=25+INT(RND(1)*10):D0=0:E=3000:E0=E
440 P=10:P0=P:S9=200:S=0:B9=0:K9=0:X$="":X0$=" is "
470 DEF FND(D)=SQR((K(I,1)-S1)^2+(K(I,2)-S2)^2)
475 DEF FNR(R)=INT(RND(R)*7.98+1.01)
480 REM INITIALIZE ENTERPRIZE'S POSITION
490 Q1=FNR(1):Q2=FNR(1):S1=FNR(1):S2=FNR(1)
530 FORI=1TO9:C(I,1)=0:C(I,2)=0:NEXTI
540 C(3,1)=-1:C(2,1)=-1:C(4,1)=-1:C(4,2)=-1:C(5,2)=-1:C(6,2)=-1
600 C(1,2)=1:C(2,2)=1:C(6,1)=1:C(7,1)=1:C(8,1)=1:C(8,2)=1:C(9,2)=1
670 FORI=1TO8:D(I)=0:NEXTI
710 A1$="NAVSRSLRSPHATORSHEDAMCOMXXX"
810 REM SETUP WHAT EXHISTS IN GALAXY . . .
815 REM K3 = # KLINGONS  B3 = # STARBASES  S3 = # STARS
820 FORI=1TO8:FORJ=1TO8:K3=0:Z(I,J)=0:R1=RND(1)
850 IFR1>.98THENK3=3:K9=K9+3:GOTO980
860 IFR1>.95THENK3=2:K9=K9+2:GOTO980
870 IFR1>.80THENK3=1:K9=K9+1
980 B3=0:IFRND(1)>.96THENB3=1:B9=B9+1
1040 G(I,J)=K3*100+B3*10+FNR(1):NEXTJ:NEXTI:IFK9>T9THENT9=K9+1
1100 IFB9<>0THEN1200
1150 IFG(Q1,Q2)<200THENG(Q1,Q2)=G(Q1,Q2)+100:K9=K9+1
1160 B9=1:G(Q1,Q2)=G(Q1,Q2)+10:Q1=FNR(1):Q2=FNR(1)
1200 K7=K9:IFB9<>1THENX$="s":X0$=" are "
1230 PRINT"Your orders are as follows:"
1240 PRINT"   Destroy the";K9;"Klingon warships which have invaded the"
1252 PRINT"   galaxy before they can attack Federation headquarters"
1260 PRINT"   on stardate ";STR$(T0+T9);".  This gives you";T9;"days.";
1262 PRINT"  There";X0$
1272 PRINT"  ";B9;"starbase";X$;" in the galaxy for resupplying your ship."
1280 PRINT
1300 I=RND(1):INPUT"[acknowledge when ready to accept command]",
1310 REM HERE ANY TIME NEW QUADRANT ENTERED
1320 Z4=Q1:Z5=Q2:K3=0:B3=0:S3=0:G5=0:D4=.5*RND(1):Z(Q1,Q2)=G(Q1,Q2)
1390 IFQ1<1ORQ1>8ORQ2<1ORQ2>8THEN1600
1430 GOSUB 9030:PRINT:IF T0<>T THEN 1490
1460 PRINT"Your mission begins with your starship located"
1470 PRINT"in the galactic quadrant '";G2$;"'.":GOTO 1500
1490 PRINT"Now entering ";G2$;" quadrant . . ."
1500 PRINT:K3=INT(G(Q1,Q2)*.01):B3=INT(G(Q1,Q2)*.1)-10*K3
1540 S3=G(Q1,Q2)-100*K3-10*B3:IFK3=0THEN1590
1560 PRINT"Combat area.     Condition RED."SPC(-1);
1565 FORI=1TO4:DELAY.1:PRINTSPC(-3)"   ";:DELAY.2:PRINTSPC(-3)"RED";:NEXT
1570 PRINT ".":DELAY0:IFS>200THEN1590
1580 PRINT"   Shields dangerously low."
1590 FORI=1TO3:K(I,1)=0:K(I,2)=0:NEXTI
1600 FORI=1TO3:K(I,3)=0:NEXTI:Q$=Z$+Z$+Z$+Z$+Z$+Z$+Z$+LEFT$(Z$,17)
1660 REM POSITION ENTERPRISE IN QUADRANT, THEN PLACE "K3" KLINGONS, &
1670 REM "B3" STARBASES, & "S3" STARS ELSEWHERE.
1680 A$="<*>":Z1=S1:Z2=S2:GOSUB8670:IFK3<1THEN1820
1720 FORI=1TOK3:GOSUB8590:A$="+K+":Z1=R1:Z2=R2
1780 GOSUB8670:K(I,1)=R1:K(I,2)=R2:K(I,3)=S9*(0.5+RND(1)):NEXTI
1820 IFB3<1THEN1910
1880 GOSUB8590:A$=">!<":Z1=R1:B4=R1:Z2=R2:B5=R2:GOSUB8670
1910 FORI=1TOS3:GOSUB8590:A$=" * ":Z1=R1:Z2=R2:GOSUB8670:NEXTI
1980 GOSUB6430
1990 IFS+E>10THENIFE>10ORD(7)=0THEN2060
2020 PRINT:PRINT"** Fatal Error **   You've just stranded your ship in space."
2030 PRINT"You have insufficient maneuvering energy, and shield control"
2040 PRINT"is presently incapable of cross-circuiting to the engine room!!"
2050 GOTO6220
2060 INPUT"Command";A$
2080 FORI=1TO9:IFLEFT$(A$,3)<>MID$(A1$,3*I-2,3)THEN2160
2140 ONIGOTO2300,1980,4000,4260,4700,5530,5690,7290,6270
2160 NEXTI:PRINT"Enter one of the following:"
2180 PRINT"  NAV  (to set course)"
2190 PRINT"  SRS  (for short range sensor scan)"
2200 PRINT"  LRS  (for long range sensor scan)"
2210 PRINT"  PHA  (to fire phasers)"
2220 PRINT"  TOR  (to fire photon torpedoes)"
2230 PRINT"  SHE  (to raise or lower shields)"
2240 PRINT"  DAM  (for damage control reports)"
2250 PRINT"  COM  (to call on library-computer)"
2260 PRINT"  XXX  (to resign your command)":PRINT:GOTO 1990
2290 REM COURSE CONTROL BEGINS HERE
2300 INPUT"Course (1-10)";C1:IFC1=10THENC1=8
2310 IFC1<1ORC1>=10THEN2330
2320 C1=FNKC(C1):IFC1>=1ANDC1<9THEN2350
2330 PRINT"   Lt. Sulu reports  'Incorrect course data, sir!'":GOTO1990
2350 X$="8":IFD(1)<0THENX$="0.2"
2360 PRINT"Warp factor (0-";X$;")";:INPUTW1:IFD(1)<0ANDW1>.2THEN2470
2380 IFW1>0ANDW1<=8THEN2490
2390 IFW1=0THEN1990
2420 PRINT"   Chief Engineer Scott reports  'The engines won't take";
2430 PRINT" warp ";STR$(W1);"!'":GOTO1990
2470 PRINT"Warp engines are damaged.  Maximum speed = warp 0.2.":GOTO1990
2490 N=INT(W1*8+.5):IFE-N>=0THEN2590
2500 PRINT"Engineering reports   'Insufficient energy available"
2510 PRINT"                       for maneuvering at warp ";STR$(W1);"!'"
2530 IFS<N-EORD(7)<0THEN1990
2550 PRINT"Deflector control room acknowledges";S;"units of energy"
2560 PRINT"                         presently deployed to shields."
2570 GOTO1990
2580 REM KLINGONS MOVE/FIRE ON MOVING STARSHIP . . .
2590 FORI=1TOK3:IFK(I,3)=0THEN2700
2610 A$="   ":Z1=K(I,1):Z2=K(I,2):GOSUB8670:GOSUB8590
2660 K(I,1)=Z1:K(I,2)=Z2:A$="+K+":GOSUB8670
2700 NEXTI:GOSUB6000:D1=0:D6=W1:IFW1>=1THEND6=1
2770 FORI=1TO8:IFD(I)>=0THEN2880
2790 D(I)=D(I)+D6:IFD(I)>-.1ANDD(I)<0THEND(I)=-.1:GOTO2880
2800 IFD(I)<0THEN2880
2810 IFD1<>1THEND1=1:PRINT"Damage Control report:  ";
2840 PRINTTAB(8);:R1=I:GOSUB8790:PRINTG2$;" Repair completed."
2880 NEXTI:IFRND(1)>.2THEN3070
2910 R1=FNR(1):IFRND(1)>=.6THEN3000
2930 D(R1)=D(R1)-(RND(1)*5+1):PRINT"Damage Control report:  ";
2960 GOSUB8790:PRINTG2$;" damaged":PRINT:GOTO3070
3000 D(R1)=D(R1)+RND(1)*3+1:PRINT"Damage Control report:  ";
3030 GOSUB8790:PRINTG2$;" state of repair improved.":PRINT
3060 REM BEGIN MOVING STARSHIP
3070 A$="   ":Z1=INT(S1):Z2=INT(S2):GOSUB8670
3110 X1=C(C1,1)+(C(C1+1,1)-C(C1,1))*(C1-INT(C1)):X=S1:Y=S2
3140 X2=C(C1,2)+(C(C1+1,2)-C(C1,2))*(C1-INT(C1)):Q4=Q1:Q5=Q2
3170 FORI=1TON:S1=S1+X1:S2=S2+X2:IFS1<1ORS1>=9ORS2<1ORS2>=9THEN3500
3240 S8=INT(S1)*24+INT(S2)*3-26:IFMID$(Q$,S8,2)="  "THEN3360
3320 S1=INT(S1-X1):S2=INT(S2-X2):PRINT"Warp engines shut down at ";
3350 PRINT"sector";S1;",";S2;"due to bad navigation.":GOTO3370
3360 NEXTI:S1=INT(S1):S2=INT(S2)
3370 A$="<*>":Z1=INT(S1):Z2=INT(S2):GOSUB8670:GOSUB3910:T8=1
3430 IFW1<1THENT8=.1*INT(10*W1)
3450 T=T+T8:IFT>T0+T9THEN6220
3470 REM SEE IF DOCKED, THEN GET COMMAND
3480 GOTO1980
3490 REM EXCEEDED QUADRANT LIMITS
3500 X=8*Q1+X+N*X1:Y=8*Q2+Y+N*X2:Q1=INT(X/8):Q2=INT(Y/8):S1=INT(X-Q1*8)
3550 S2=INT(Y-Q2*8):IFS1=0THENQ1=Q1-1:S1=8
3590 IFS2=0THENQ2=Q2-1:S2=8
3620 X5=0:IFQ1<1THENX5=1:Q1=1:S1=1
3670 IFQ1>8THENX5=1:Q1=8:S1=8
3710 IFQ2<1THENX5=1:Q2=1:S2=1
3750 IFQ2>8THENX5=1:Q2=8:S2=8
3790 IFX5=0THEN3860
3800 PRINT"Lt. Uhura reports message from starfleet command:"
3810 PRINT"  'Permission to attempt crossing of galactic perimeter"
3820 PRINT"  is hereby *denied*.  Shut down your engines.'"
3830 PRINT"Chief Engineer Scott reports  'Warp engines shut down"
3840 PRINT"  at sector";S1;",";S2;"of quadrant";Q1;", ";STR$(Q2);".'"
3850 IFT>T0+T9THEN6220
3860 IF8*Q1+Q2=8*Q4+Q5THEN3370
3870 T=T+1:GOSUB3910:IFT>T0+T9THEN6220
3880 GOTO1320
3900 REM MANEUVER ENERGY S/R **
3910 E=E-N-10:IFE>=0THENRETURN
3930 PRINT"Shield control supplies energy to complete the maneuver."
3940 S=S+E:E=0:IFS<=0THENS=0
3980 RETURN
3990 REM LONG RANGE SENSOR SCAN CODE
4000 IFD(3)<0THENPRINT"Long range sensors are inoperable":GOTO1990
4030 PRINT"Long range scan for quadrant";Q1;",";Q2
4040 O1$="-------------------":PRINTO1$
4060 FORI=Q1-1TOQ1+1:N(1)=-1:N(2)=-2:N(3)=-3:FORJ=Q2-1TOQ2+1
4120 IFI>0ANDI<9ANDJ>0ANDJ<9THENN(J-Q2+2)=G(I,J):Z(I,J)=G(I,J)
4180 NEXTJ:FORL=1TO3:PRINT": ";:IFN(L)<0THENPRINT"*** ";:GOTO4230
4210 PRINTRIGHT$(STR$(N(L)+1000),3);" ";
4230 NEXTL:PRINT":":PRINTO1$:NEXTI:GOTO1990
4250 REM PHASER CONTROL CODE BEGINS HERE
4260 IFD(4)<0THENPRINT"Phasers inoperative.":GOTO1990
4265 IFK3>0THEN4330
4270 PRINT"Science Officer Spock reports  'Sensors show no enemy ships"
4280 PRINT"                                in this quadrant.'":GOTO1990
4330 IFD(8)<0THENPRINT"Computer failure hampers accuracy."
4350 PRINT"Phasers locked on target.  ";
4360 PRINT"Energy available =";E;"units."
4370 INPUT"Number of units to fire";X:IFX<=0THEN1990
4400 IFE-X<0THEN4360
4410 E=E-X:IFD(7)<0THENX=X*RND(1)
4450 H1=INT(X/K3):FORI=1TO3:IFK(I,3)<=0THEN4670
4480 H=INT((H1/FND(0))*(RND(1)+2)):IFH>.15*K(I,3)THEN4530
4500 PRINT"Sensors show no damage to enemy at ";K(I,1);",";K(I,2):GOTO4670
4530 K(I,3)=K(I,3)-H:PRINTH;"unit hit on Klingon at sector";K(I,1);",";
4550 PRINTK(I,2):IFK(I,3)<=0THENPRINT"*** Klingon destroyed ***":GOTO4580
4560 PRINT"   (Sensors show";K(I,3);"units remaining)":GOTO4670
4580 K3=K3-1:K9=K9-1:Z1=K(I,1):Z2=K(I,2):A$="   ":GOSUB8670
4650 K(I,3)=0:G(Q1,Q2)=G(Q1,Q2)-100:Z(Q1,Q2)=G(Q1,Q2):IFK9<=0THEN6370
4670 NEXTI:GOSUB6000:GOTO1990
4690 REM PHOTON TORPEDO CODE BEGINS HERE
4700 IFP<=0THENPRINT"All photon torpedoes expended.":GOTO 1990
4730 IFD(5)<0THENPRINT"Photon tubes are not operational.":GOTO1990
4760 INPUT"Photon torpedo course (1-10)";C1:IFC1=10THENC1=8
4770 IFC1<1ORC1>=10THEN4790
4780 C1=FNKC(C1):IFC1>=1ANDC1<9THEN4850
4790 PRINT"Ensign Chekov reports  'Incorrect course data, sir!'"
4800 GOTO1990
4850 X1=C(C1,1)+(C(C1+1,1)-C(C1,1))*(C1-INT(C1)):E=E-2:P=P-1
4860 X2=C(C1,2)+(C(C1+1,2)-C(C1,2))*(C1-INT(C1)):X=S1:Y=S2
4910 PRINT"Torpedo track:"
4920 X=X+X1:Y=Y+X2:X3=INT(X+.5):Y3=INT(Y+.5)
4960 IFX3<1ORX3>8ORY3<1ORY3>8THEN5490
5000 PRINT"               ";X3;",";Y3:A$="   ":Z1=X:Z2=Y:GOSUB8830
5050 IFZ3<>0THEN4920
5060 A$="+K+":Z1=X:Z2=Y:GOSUB8830:IFZ3=0THEN5210
5110 PRINT"*** Klingon destroyed ***":K3=K3-1:K9=K9-1:IFK9<=0THEN6370
5150 FORI=1TO3:IFX3=K(I,1)ANDY3=K(I,2)THEN5190
5180 NEXTI:I=3
5190 K(I,3)=0:GOTO5430
5210 A$=" * ":Z1=X:Z2=Y:GOSUB8830:IFZ3=0THEN5280
5260 PRINT"Star at";X3;",";Y3;"absorbed torpedo energy.":GOSUB6000:GOTO1990
5280 A$=">!<":Z1=X:Z2=Y:GOSUB8830:IFZ3=0THEN4760
5330 PRINT"*** Starbase destroyed ***":B3=B3-1:B9=B9-1
5360 IFB9>0ORK9>T-T0-T9THEN5400
5370 PRINT"That does it, captain!!  You are hereby relieved of command"
5380 PRINT"and sentenced to 99 stardates at hard labor on Cygnus 12!!"
5390 GOTO 6270
5400 PRINT"Starfleet command reviewing your record to consider"
5410 PRINT"court martial!":D0=0
5430 Z1=X:Z2=Y:A$="   ":GOSUB8670
5470 G(Q1,Q2)=K3*100+B3*10+S3:Z(Q1,Q2)=G(Q1,Q2):GOSUB6000:GOTO1990
5490 PRINT"Torpedo missed.":GOSUB6000:GOTO1990
5520 REM SHIELD CONTROL
5530 IFD(7)<0THENPRINT"Shield control inoperable.":GOTO1990
5560 PRINT"Energy available =";E+S;:INPUT"Number of units to shields";X
5580 IFX<0ORS=XTHENPRINT"<shields unchanged>":GOTO1990
5590 IFX<=E+STHEN5630
5600 PRINT"Shield Control reports  'This is not the Federation treasury.'"
5610 PRINT"<shields unchanged>":GOTO1990
5630 E=E+S-X:S=X:PRINT"Deflector Control Room report:"
5660 PRINT"  'Shields now at";INT(S);"units per your command.'":GOTO1990
5680 REM DAMAGE CONTROL
5690 IFD(6)>=0THEN5910
5700 PRINT"Damage Control report not available.":IFD0=0THEN1990
5720 D3=0:FORI=1TO8:IFD(I)<0THEND3=D3+.1
5760 NEXTI:IFD3=0THEN1990
5780 PRINT:D3=D3+D4:IFD3>=1THEND3=.9
5810 PRINT"Technicians standing by to effect repairs to your ship."
5820 PRINT"Estimated time to repair:";.01*INT(100*D3);"stardates."
5840 INPUT "Will you authorize the repair order (y/n)";A$
5860 IFA$<>"y"THEN 1990
5870 FORI=1TO8:IFD(I)<0THEND(I)=0
5890 NEXTI:T=T+D3+.1
5910 PRINT:PRINT"Device             State of repair":FORR1=1TO8
5920 GOSUB8790:PRINTG2$;LEFT$(Z$,25-LEN(G2$));INT(D(R1)*100)*.01
5950 NEXTR1:PRINT:IFD0<>0THEN5720
5980 GOTO 1990
5990 REM KLINGONS SHOOTING
6000 IFK3<=0THENRETURN
6010 IFD0<>0THENPRINT"Starbase shields protect the Enterprise.":RETURN
6040 FORI=1TO3:IFK(I,3)<=0THEN6200
6060 H=INT((K(I,3)/FND(1))*(2+RND(1))):S=S-H:K(I,3)=K(I,3)/(3+RND(0))
6080 PRINTH;"unit hit on enterprise from sector";K(I,1);",";K(I,2)
6090 IFS<=0THEN6240
6100 PRINT"      <shields down to";S;"units>":IFH<20THEN6200
6120 IFRND(1)>.6ORH/S<=.02THEN6200
6140 R1=FNR(1):D(R1)=D(R1)-H/S-.5*RND(1):GOSUB8790
6170 PRINT"Damage Control reports '";G2$;" damaged by the hit.'"
6200 NEXTI:RETURN
6210 REM END OF GAME
6220 PRINT"It is stardate ";STR$(T);".";:IFT<=T0+T9THENPRINT:GOTO 6270
6225 PRINT"  Time is up.":GOTO 6270
6240 PRINT:PRINT"The Enterprise has been destroyed."
6250 PRINT"The Federation will be conquered.":PRINT:GOTO 6220
6270 PRINT"There were";K9;"Klingon battle cruisers left"
6280 PRINT"at the end of your mission."
6290 PRINT:PRINT:IFB9=0THEN6360
6310 PRINT"The Federation is in need of a new starship commander"
6320 PRINT"for a similar mission -- if there is a volunteer,"
6330 INPUT"let him step forward and enter 'Aye'";A$:IFA$="aye"THEN30
6360 END
6370 PRINT"Congratulations, captain!  The last Klingon battle cruiser"
6380 PRINT"menacing the Federation has been destroyed.":PRINT
6400 PRINT"Your efficiency rating is";1000*(K7/(T-T0))^2:GOTO6290
6420 REM SHORT RANGE SENSOR SCAN & STARTUP SUBROUTINE
6430 FORI=S1-1TOS1+1:FORJ=S2-1TOS2+1
6450 IFINT(I+.5)<1ORINT(I+.5)>8ORINT(J+.5)<1ORINT(J+.5)>8THEN6540
6490 A$=">!<":Z1=I:Z2=J:GOSUB8830:IFZ3=1THEN6580
6540 NEXTJ:NEXTI:D0=0:GOTO6650
6580 D0=1:C$="docked":E=E0:P=P0
6620 PRINT"Shields dropped for docking purposes.":S=0:GOTO6720
6650 IFK3>0THENC$="*RED*":GOTO6720
6660 C$="green":IFE<E0*.1THENC$="Yellow"
6720 IFD(2)>=0THEN6770
6730 PRINT:PRINT"*** Short range sensors are out ***":PRINT:RETURN
6770 O1$="---------------------------------":Z4=Q1:Z5=Q2:G5=0:GOSUB9030
6780 PRINT:PRINT"- "G2$" "RIGHT$(O1$,LEN(O1$)-LEN(G2$)-3):FORI=1TO8
6820 FORJ=(I-1)*24+1TO(I-1)*24+22STEP3:PRINT" ";MID$(Q$,J,3);:NEXTJ
6830 ONIGOTO6850,6900,6960,7020,7070,7120,7180,7240
6850 PRINT"        Stardate          ";INT(T*10)*.1:GOTO7260
6900 PRINT"        Condition          ";C$:GOTO7260
6960 PRINT"        Quadrant          ";Q1;",";Q2:GOTO7260
7020 PRINT"        Sector            ";S1;",";S2:GOTO7260
7070 PRINT"        Photon torpedoes  ";INT(P):GOTO7260
7120 PRINT"        Total energy      ";INT(E+S):GOTO7260
7180 PRINT"        Shields           ";INT(S):GOTO7260
7240 PRINT"        Klingons remaining";INT(K9)
7260 NEXTI:PRINTO1$:RETURN
7280 REM LIBRARY COMPUTER CODE
7290 IFD(8)<0THENPRINT"Computer disabled.":GOTO1990
7320 INPUT"Computer active and awaiting command";A:IFA<0THEN1990
7350 PRINT:H8=1:ONA+1GOTO7530,7890,8060,8500,8150,7390
7360 PRINT"Functions available from library-computer:"
7370 PRINT"   0 = Cumulative galactic record"
7372 PRINT"   1 = Status report"
7374 PRINT"   2 = Photon torpedo data"
7376 PRINT"   3 = Starbase NAV data"
7378 PRINT"   4 = Direction/distance calculator"
7380 PRINT"   5 = Galaxy 'region name' map":PRINT:GOTO7320
7390 REM SETUP TO CHANGE CUM GAL RECORD TO GALAXY MAP
7400 H8=0:G5=1:PRINT"                        The Galaxy":GOTO7550
7530 REM CUM GALACTIC RECORD
7540 REM
7542 REM
7543 PRINT:PRINT"        ";
7544 PRINT"Computer record of galaxy for quadrant";Q1;",";Q2
7546 PRINT
7550 PRINT"       1     2     3     4     5     6     7     8"
7560 O1$="     ----- ----- ----- ----- ----- ----- ----- -----"
7570 PRINTO1$:FORI=1TO8:PRINTI;:IFH8=0THEN7740
7630 FORJ=1TO8:PRINT"   ";:IFZ(I,J)=0THENPRINT"***";:GOTO7720
7700 PRINTRIGHT$(STR$(Z(I,J)+1000),3);
7720 NEXTJ:GOTO7850
7740 Z4=I:Z5=1:GOSUB9030:J0=INT(15-.5*LEN(G2$)):PRINTTAB(J0);G2$;
7800 Z5=5:GOSUB 9030:J0=INT(39-.5*LEN(G2$)):PRINTTAB(J0);G2$;
7850 PRINT:PRINTO1$:NEXTI:PRINT:GOTO1990
7890 REM STATUS REPORT
7900 PRINT "   Status report:":X$="":IFK9>1THENX$="s"
7940 PRINT"Klingon";X$;" left: ";K9
7960 PRINT"Mission must be completed in";.1*INT((T0+T9-T)*10);"stardates."
7970 X$="s":IFB9<2THENX$="":IFB9<1THEN8010
7980 PRINT"The Federation is maintaining";B9;"starbase";X$;" in the galaxy."
7990 GOTO5690
8010 PRINT"Your stupidity has left you on your own in"
8020 PRINT"  the galaxy -- you have no starbases left!":GOTO5690
8060 REM TORPEDO, BASE NAV, D/D CALCULATOR
8070 IFK3<=0THEN4270
8080 X$="":IFK3>1THENX$="s"
8090 PRINT"From Enterprise to Klingon battle cruser";X$
8100 H8=0:FORI=1TO3:IFK(I,3)<=0THEN8480
8110 W1=K(I,1):X=K(I,2)
8120 C1=S1:A=S2:GOTO8220
8150 PRINT"Direction/distance calculator:"
8160 PRINT"You are at quadrant ";Q1;",";Q2;" sector ";S1;",";S2
8170 PRINT"Please enter":INPUT"  Initial coordinates (y,x)";C1,A
8200 INPUT"  Final coordinates (y,x)";W1,X
8205 IF C1<>W1 OR A<>X THEN 8220
8210 PRINT"Division by zero fries ship's computer. You're stranded.":GOTO6250
8220 X=X-A:A=C1-W1:IFX<0THEN8350
8250 IFA<0THEN8410
8260 IFX>0THEN8280
8270 IFA=0THENC1=5:GOTO8290
8280 C1=1
8290 IFABS(A)<=ABS(X)THEN8330
8310 PRINT"Direction =";FNCK(C1+(((ABS(A)-ABS(X))+ABS(A))/ABS(A))):GOTO8460
8330 PRINT"Direction =";FNCK(C1+(ABS(A)/ABS(X))):GOTO8460
8350 IFA>0THENC1=3:GOTO8420
8360 IFX<>0THENC1=5:GOTO8290
8410 C1=7
8420 IFABS(A)>=ABS(X)THEN8450
8430 PRINT"Direction =";FNCK(C1+(((ABS(X)-ABS(A))+ABS(X))/ABS(X))):GOTO8460
8450 PRINT"Direction =";FNCK(C1+(ABS(X)/ABS(A)))
8460 PRINT"Distance =";SQR(X^2+A^2):IFH8=1THEN1990
8480 NEXTI:GOTO1990
8500 IFB3<>0THENPRINT"From enterprise to starbase:":W1=B4:X=B5:GOTO8120
8510 PRINT"Mr. Spock reports  'Sensors show no starbases in this";
8520 PRINT" quadrant.'":GOTO1990
8580 REM FIND EMPTY PLACE IN QUADRANT (FOR THINGS)
8590 R1=FNR(1):R2=FNR(1):A$="   ":Z1=R1:Z2=R2:GOSUB8830:IFZ3=0THEN8590
8600 RETURN
8660 REM INSERT IN STRING ARRAY FOR QUADRANT
8670 S8=INT(Z2-.5)*3+INT(Z1-.5)*24+1
8675 IF LEN(A$)<>3THEN PRINT"ERROR":STOP
8680 IFS8=1THENQ$=A$+RIGHT$(Q$,189):RETURN
8690 IFS8=190THENQ$=LEFT$(Q$,189)+A$:RETURN
8700 Q$=LEFT$(Q$,S8-1)+A$+RIGHT$(Q$,190-S8):RETURN
8780 REM PRINTS DEVICE NAME
8790 ONR1GOTO8792,8794,8796,8798,8800,8802,8804,8806
8792 G2$="Warp engines":RETURN
8794 G2$="Short range sensors":RETURN
8796 G2$="Long range sensors":RETURN
8798 G2$="Phaser control":RETURN
8800 G2$="Photon tubes":RETURN
8802 G2$="Damage control":RETURN
8804 G2$="Shield control":RETURN
8806 G2$="Library-computer":RETURN
8820 REM STRING COMPARISON IN QUADRANT ARRAY
8830 Z1=INT(Z1+.5):Z2=INT(Z2+.5):S8=(Z2-1)*3+(Z1-1)*24+1:Z3=0
8890 IFMID$(Q$,S8,3)<>A$THENRETURN
8900 Z3=1:RETURN
9010 REM QUADRANT NAME IN G2$ FROM Z4,Z5 (=Q1,Q2)
9020 REM CALL WITH G5=1 TO GET REGION NAME ONLY
9030 IFZ5<=4THENONZ4GOTO9040,9050,9060,9070,9080,9090,9100,9110
9035 GOTO9120
9040 G2$="Antares":GOTO9210
9050 G2$="Rigel":GOTO9210
9060 G2$="Procyon":GOTO9210
9070 G2$="Vega":GOTO9210
9080 G2$="Canopus":GOTO9210
9090 G2$="Altair":GOTO9210
9100 G2$="Sagittarius":GOTO9210
9110 G2$="Pollux":GOTO9210
9120 ONZ4GOTO9130,9140,9150,9160,9170,9180,9190,9200
9130 G2$="Sirius":GOTO9210
9140 G2$="Deneb":GOTO9210
9150 G2$="Capella":GOTO9210
9160 G2$="Betelgeuse":GOTO9210
9170 G2$="Aldebaran":GOTO9210
9180 G2$="Regulus":GOTO9210
9190 G2$="Arcturus":GOTO9210
9200 G2$="Spica"
9210 IFG5<>1THENONZ5GOTO9230,9240,9250,9260,9230,9240,9250,9260
9220 RETURN
9230 G2$=G2$+" I":RETURN
9240 G2$=G2$+" II":RETURN
9250 G2$=G2$+" III":RETURN
9260 G2$=G2$+" IV":RETURN
9300 REM KEYPAD <-> COURSE LOOK-UP TABLE
9310 REM KM(0,N) & FNKC ARE KEYPAD-N -> COURSE #
9320 REM KM(1,N) & FNCK ARE COURSE-N -> KEYPAD #
9330 DIM KM(1,9)
9340 KM(0,1)=6:KM(0,2)=7:KM(0,3)=8:KM(0,4)=5:KM(0,5)=0
9345 KM(0,6)=1:KM(0,7)=4:KM(0,8)=3:KM(0,9)=2
9350 KM(1,1)=6:KM(1,2)=9:KM(1,3)=8:KM(1,4)=7:KM(1,5)=4
9355 KM(1,6)=1:KM(1,7)=2:KM(1,8)=3:KM(1,9)=0
9360 DEF FNKC(N)=KM(0,INT(N))+(N-INT(N))
9370 DEF FNCK(N)=KM(1,INT(N))+(N-INT(N))
9380 RETURN
10000 REM INSTRUCTIONS FOR "Super Star Trek"  MAR 5, 1978
10010 FOR I=1 TO 12:PRINT:NEXT I
10012 PRINT TAB(10);"*************************************"
10014 PRINT TAB(10);"*                                   *"
10016 PRINT TAB(10);"*                                   *"
10018 PRINT TAB(10);"*      * * Super Star Trek * *      *"
10020 PRINT TAB(10);"*                                   *"
10022 PRINT TAB(10);"*                                   *"
10024 PRINT TAB(10);"*************************************"
10030 PRINT:PRINT
10040 PRINT" original 'Star Trek' game in HP BASIC by Mike Mayfield (1972)"
10045 PRINT" ported to BASIC-PLUS by David H. Ahl and Mary Cole (1973)"
10050 PRINT" expanded to 'Super Star Trek' by Robert Leedon (1974)"
10055 PRINT" converted to Altair/Microsoft BASIC by David H. Ahl (1975)"
10060 PRINT" converted to Microsoft 8K BASIC by John Gorders (1978)"
10065 FOR I=1 TO 6:PRINT:NEXT I
10070 INPUT "Do you need instructions (y/n)";K$:IF K$="N" THEN RETURN
10080 PRINT
10090 PRINT"      Instructions for 'Super Star Trek'"
10100 PRINT
10110 PRINT"1. When you see \Command ?\ printed, enter one of the legal"
10120 PRINT"     commands (NAV, SRS, LRS, PHA, TOR, SHE, DAM, COM, or XXX)."
10130 PRINT"2. If you should type in an illegal command, you'll get a short"
10140 PRINT"     list of the legal commands printed out."
10150 PRINT"3. Some commands require you to enter data (for example, the"
10160 PRINT"     'NAV' command comes back with 'Course (1-10) ?').  If you"
10170 PRINT"     type in illegal data (like negative numbers), the command"
10180 PRINT"     will be aborted."
10190 PRINT
10270 PRINT"     The galaxy is divided into an 8 x 8 quadrant grid,"
10280 PRINT"and each quadrant is further divided into an 8 x 8 sector grid."
10290 PRINT
10300 PRINT"     You will be assigned a starting point somewhere in the"
10310 PRINT"galaxy to begin a tour of duty as commander of the starship"
10320 PRINT"\Enterprise\. Your mission; to seek and destroy the fleet of"
10330 PRINT"Klingon warships which are menacing the United Federation of"
10340 PRINT"Planets."
10360 PRINT
10370 PRINT"     You have the following commands available to you as captain"
10380 PRINT"of the starship Enterprise:"
10385 PRINT:INPUT"[more]",:PRINT
10390 PRINT"\NAV\ command = Warp engine control            7  8  9"
10400 PRINT"     Course is in a keypad vector               . . ."
10410 PRINT"     arrangement, as shown.  Integer             ..."
10420 PRINT"     and real values may be used, with       4 ---*--- 6"
10430 PRINT"     the fractional component applied            ..."
10440 PRINT"     anticlockwise (thus, 6.5 is half-          . . ."
10450 PRINT"     way between 6 and 9).                     1  2  3"
10460 PRINT"     Values may approach 10.0, which"
10470 PRINT"     is itself equivalent to 8.                Course"
10480 PRINT
10490 PRINT"     One warp factor is the size of one quadrant (8 sectors)."
10500 PRINT"     Therefore, to get from quadrant 6,5 to 5,5, you would use"
10510 PRINT"     course 8, warp factor 1. To maneuver between the sectors"
10512 PRINT"     within a quadrant, use sub-warp speeds (factors below 1)."
10514 PRINT
10520 PRINT"     The Federation uses standard galactic matrix coordinates:"
10522 PRINT"     (Y-downward, X-rightward)."
10530 PRINT:INPUT"[more]",:PRINT
10540 PRINT"\SRS\ command = Short range sensor scan"
10550 PRINT"     Shows you a scan of your present quadrant."
10555 PRINT
10560 PRINT"     Symbology on your sensor screen is as follows:"
10570 PRINT"        <*> = Your starship's position"
10580 PRINT"        +K+ = Klingon battle cruiser"
10590 PRINT"        >!< = Federation starbase (refuel/repair/re-arm here!)"
10600 PRINT"         *  = A star"
10605 PRINT
10610 PRINT"     A condensed 'status report' will also be presented."
10620 PRINT:INPUT"[more]",:PRINT
10640 PRINT"\LRS\ command = Long range sensor scan"
10650 PRINT"     Shows conditions in space for one quadrant on each side"
10660 PRINT"     of the enterprise (which is in the middle of the scan)."
10670 PRINT"     The scan is coded in the form \###\, where the units digit"
10680 PRINT"     is the number of stars, the tens digit is the number of"
10690 PRINT"     starbases, and the hundreds digit is the number of"
10700 PRINT"     Klingons."
10705 PRINT
10706 PRINT"     Example - 207 = 2 Klingons, no starbases, & 7 stars."
10710 PRINT:INPUT"[more]",:PRINT
10720 PRINT"\PHA\ command = Phaser control"
10730 PRINT"     Allows you to destroy the Klingon battle cruisers by "
10740 PRINT"     zapping them with suitably large units of energy to"
10750 PRINT"     deplete their shield power.  (Remember, Klingons have"
10760 PRINT"     phasers too!)"
10770 PRINT:INPUT"[more]",:PRINT
10780 PRINT"\TOR\ command = Photon torpedo control"
10790 PRINT"     Torpedo course is the same as used in warp engine control."
10800 PRINT"     If you hit the Klingon vessel, he is destroyed and"
10810 PRINT"     cannot fire back at you.  If you miss, you are subject to"
10820 PRINT"     his phaser fire.  In either case, you are also subject to "
10825 PRINT"     the phaser fire of all other Klingons in the quadrant."
10830 PRINT
10835 PRINT"     The library-computer (\COM\ command) has an option (#2)"
10840 PRINT"     to compute torpedo trajectories for you."
10850 PRINT:INPUT"[more]",:PRINT
10860 PRINT"\SHE\ command = Shield control"
10870 PRINT"     Defines the number of energy units to be assigned to the"
10880 PRINT"     shields.  Energy is taken from total ship's energy.  Note"
10890 PRINT"     that the status display's 'total energy' includes shield "
10895 PRINT"     energy."
10900 PRINT:INPUT"[more]",:PRINT
10910 PRINT"\DAM\ command = Damage control report"
10920 PRINT"     Gives the state of repair of all devices.  A negative"
10930 PRINT"     'state of repair' shows that the device is temporarily"
10940 PRINT"     damaged.  While docked, this places repair orders."
10950 PRINT:INPUT"[more]",:PRINT
10960 PRINT"\COM\ command = Library-computer"
10970 PRINT"     The library-computer contains six options:"
10980 PRINT"     Option 0 = Cumulative galactic record"
10990 PRINT"        This option shows computer memory of the results of all"
11000 PRINT"        previous short and long range sensor scans."
11010 PRINT"     Option 1 = Status report"
11020 PRINT"        This option shows the number of Klingons, stardates,"
11030 PRINT"        and starbases remaining in the mission."
11040 PRINT"     Option 2 = Photon torpedo data"
11050 PRINT"        Which gives directions and distance from the Enterprise"
11060 PRINT"        to all Klingons in your quadrant."
11070 PRINT"     Option 3 = Starbase NAV data"
11080 PRINT"        This option gives direction and distance to any "
11090 PRINT"        starbase within your quadrant."
11100 PRINT"     Option 4 = Direction/distance calculator"
11110 PRINT"        This option allows you to enter coordinates for"
11120 PRINT"        direction/distance calculations."
11130 PRINT"     Option 5 = Galactic /region name/ map"
11140 PRINT"        This option prints the names of the sixteen major "
11150 PRINT"        galactic regions referred to in the game."
11160 PRINT
11200 REM READY TO BEGIN
11210 PRINT:PRINT"[reverse polarity of the dilithium-cassette triplayer";
11220 INPUT", and hit return]",
11230 RETURN

# END
