# CAMEL
# By (at least, from) the Heath Users Group.
# From David H. Ahl & Steve North, More BASIC Computer Games, Workman, 1980.

# Changes:
# - Formatting of some printed output.
# - Appended author and year to title.
# - Added +1 to the random number on line 640, and +2 on line 710, so that your
#   camel always moves at least a little distance.
# - Added +0.5 on line 380 to increase the pygmies' speed to compensate for the
#   changes on lines 640 and 710. Adding a whole +1 makes you overly reliant on
#   covering a lot of ground in the first couple of turns.
# - Added +1 on line 1120 to lessen the frequency of oases.
# - Bug fix. Nowhere, between lines 890 and 1110, is Q set to 1. Hence the game
#   fails to remember you've been captured by Berbers, and you're instantly free
#   after command 8, whether or not the ransom was paid. However, setting Q = 1
#   at line 895 or 1105 has two consequences. One; you eventually need to drink
#   while waiting (line 1110 went to 340) but are unable to do so (line 980) and
#   will die of thirst. Two; the pygmies are immobile (line 390). Added line
#   1105 and modified line 1110; the Berbers keep you alive, while the pygmies
#   search for you (slower than their normal pursuit).

10 PRINT TAB(26);"CAMEL"
20 PRINT TAB(20);"Creative Computing"
30 PRINT TAB(18);"Morristown, New Jersey"
40 PRINT
50 PRINT TAB(12);"submitted by the Heath Users Group"
60 PRINT TAB(26);"c1978"
90 PRINT:PRINT:PRINT
110 PRINT "Would you like instructions";
120 INPUT D$
130 IF LEFT$(D$,1)="N" THEN 320
140 PRINT:PRINT "   Welcome to CAMEL."
150 PRINT "The object is to travel 200 miles across the great Gobi Desert."
160 PRINT "A tribe of knocked kneed pygmies will be chasing you."
170 PRINT "You will be asked for commands every so often."
180 PRINT
# 190 PRINT
# 200 PRINT
210 PRINT "C O M M A N D S :"
220 PRINT "#1 drink from your canteen"
230 PRINT "#2 ahead moderate speed"
240 PRINT "#3 ahead full speed"
250 PRINT "#4 stop for the night"
260 PRINT "#5 status check"
270 PRINT "#6 hope for help"
275 PRINT
# 276 PRINT
# 277 PRINT
# 278 PRINT
# 279 PRINT
280 PRINT "You have one quart of water, which will last you six drinks."
290 PRINT "You may renew your water supply completely at an oasis."
300 PRINT "You get half a quart if found by help."
310 PRINT "If help does not find you after command six, you lose."
320 PRINT:PRINT "Good luck and good camelling!!"
330 PRINT "You are in the middle of the desert at an oasis."
335 GOSUB 2000
340 IF C>199 THEN 1210
350 Z=Z-1
355 IF Z=1 THEN PRINT "----------W A R N I N G---------- get a drink"
360 IF Z<0 THEN 1630
370 P=P+1
380 X2=INT(10*RND(1)+3.0) :REM Originally read 'X2=INT(10*RND(1)+2.5)'.
390 IF Q>0 THEN 940
400 IF P<4 THEN 470
410 C1=C1+X2
420 IF C1<C THEN 460
430 PRINT "The pygmies have captured you."
440 PRINT "Camel and people soup is their favourite dish!!!!!"
450 GOTO 1560
460 PRINT "The pygmies are "C-C1;" miles behind you."
470 PRINT "You have travelled ";C;" miles altogether."
480 PRINT "What is your command";
490 INPUT Y
495 PRINT
500 ON Y GOTO 830,610,680,760,790
550 T=INT(10*RND(1))
560 IF T<>1 THEN 1200
570 PRINT "Help has found you in a state of unconsciousness."
580 S=3
590 Z=4
600 GOTO 340
610 F=F+1
620 IF F=8 THEN 1190
630 GOSUB 880
640 X1=INT(10*RND(1))+1 :REM Originally read 'X1=INT(10*RND(1))'.
650 C=C+X1
660 PRINT "Your camel likes this pace."
670 GOTO 340
680 F=F+3
690 IF F>7 THEN 1190
700 GOSUB 880
710 X1=2*INT(10*RND(1))+2 :REM Originally read 'X1=2*INT(10*RND(1))'.
720 C=C+X1
730 PRINT "Your camel is burning across the desert sands."
# 740 PRINT
750 GOTO 340
760 PRINT "Your camel thanks you!"
770 F=0
780 GOTO 350
790 PRINT "Your camel has ";7-F;" good days left."
800 PRINT "You have ";S;" drinks left in your canteen."
810 PRINT "You can go ";Z;" commands without drinking."
830 S=S-1
840 IF S<0 THEN 1200
850 PRINT "Better watch for an oasis!"
860 Z=4
870 GOTO 480
880 A=INT(100*RND(1))
890 IF A>5 THEN 1120
900 PRINT "Wild Berbers hidden in the sand have captured you."
910 PRINT "Luckily, the local sheikh has agreed to their ransom-"
920 PRINT "demands.......but........watch for the pygmies!!!"
930 PRINT "You have a new choice of sub-commands:"
940 PRINT "#7 attempt an escape"
950 PRINT "#8 wait for payment"
960 PRINT "Your sub-command ";
970 INPUT X
980 IF X=8 THEN 1060
990 X1=INT(10 * RND(1))
1000 IF X1<5 THEN 1040
1010 PRINT "Congratulations, you successfully escaped!!!!"
1020 Q=0
1030 GOTO 340
1040 PRINT "You were mortally wounded by a pig-stabber while escaping."
1050 GOTO 1410
1060 X1=INT(100*RND(1))
1070 REM
1080 IF X1>24 THEN 1100
1090 PRINT "Your ransom has been paid and you are free to go."
1095 Q=0
1096 GOTO 340
1100 PRINT "The local sultan is collecting.......just wait......."
1105 Z=3:C1=C1+INT(8*RND(1)):IF C1<C THEN 940
1110 GOTO 430
1120 A=INT(10*RND(1))+1 :REM Originally read 'A=INT(10*RND(1))'.
1130 IF A>2 THEN 1240
1140 PRINT "You have arrived at an oasis--------"
1150 PRINT "Your camel is filling your canteen and eating figs."
1160 Z=4
1170 S=6
1180 RETURN
1190 PRINT "You dirty rapscallion! You ran your poor camel to death!!"
1200 GOTO 1410
1210 PRINT "You win, a party is being given in your honour......."
1220 PRINT ".......the pygmies are planning to attend......."
1230 GOTO 1560
1240 X1=INT(100*RND(1))
1250 IF X1>5 THEN 1350
1260 PRINT "You have been caught in a sandstorm.....good luck!"
1270 X5=INT(10*RND(1))
1280 X6=INT(10*RND(1))
1290 IF X6<5 THEN 1320
1300 C=C+X5
1310 GOTO 1330
1320 C=C-X5
1330 PRINT "Your new position is ";C;" miles so far!"
1340 RETURN
1350 X1=INT(100*RND(1))
1360 IF X1>5 THEN RETURN
1370 C1=C1+1
1380 PRINT "Your camel hurt his hump."
1390 PRINT "Luckily the pygmies were footweary!!!"
1400 RETURN
1410 U=INT(10*RND(1))
1420 PRINT "You died in the desert."
1430 IF U>1 THEN 1460
1440 PRINT "The national camel's union is not attending your funeral!!!"
1450 GOTO 1560
1460 IF U>3 THEN 1490
1470 PRINT "Your body was eaten by vultures and imported cannibals!!!"
1480 GOTO 1560
1490 IF U>5 THEN 1520
1500 PRINT "The local sheikh now uses your skull for a change purse!!!"
1510 GOTO 1560
1520 IF U>7 THEN 1550
1530 PRINT "People with little intelligence should stay out of the desert."
1540 GOTO 1560
1550 PRINT "Turkeys should fly, not ride camels!!!!!!!"
1560 PRINT
1570 PRINT
1580 PRINT "Want a new camel and a new game ";
1590 INPUT D$
1600 IF LEFT$(D$,1)="Y" THEN 320
1620 GOTO 1650
1630 PRINT "You ran out of water......sorry chum!!!"
1640 GOTO 1410
1650 PRINT "-----------------"
1655 PRINT "     CHICKEN     "
1657 PRINT "-----------------"
1660 END
2000 Z=4
2010 S=6
2020 C=0
2030 C1=0
2040 Q=0
2050 F=0
2060 P=0
2070 RETURN

# END
