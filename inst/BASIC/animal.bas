# ANIMAL
# By Arthur Luehrmann.
# From David H. Ahl & Steve North, BASIC Computer Games, Workman, 1978.

# Changes:
# - Formatting of some printed output.
# - Appended author and year to title.
# - Inserted line 135, to allow the user to quit.
# - Added lines 145 and 700-810, to print the decision tree.
# - Modified lines 240 and 270, to prohibit void animals and questions.
# - Added lines 274 and 276, to capitalise the first letter of each question.
# - Modified lines 310 and 320 to force A$ to upper case (for tree printing).
# - Rewrote lines 610-645 to better format printing of longer animal names.

10 PRINT TAB(26);"ANIMAL"
20 PRINT TAB(20);"Creative Computing"
30 PRINT TAB(18);"Morristown, New Jersey"
45 PRINT
50 PRINT TAB(20);"by Arthur Luehrmann"
55 PRINT TAB(7);"modified by Nathan Teichholtz and Steve North"
60 PRINT TAB(27);"1975"
65 PRINT:PRINT:PRINT
70 DIM A$(200)
80 FOR I=0 TO 3
90 READ A$(I)
100 NEXT I
# 110 N=VAL(A$(0)) :REM never used
120 REM          MAIN CONTROL SECTION
130 INPUT "Are you thinking of an animal (y/n/list/tree)";A$
135 IF LEFT$(A$,1)="N" THEN 999
140 IF A$="LIST" THEN 600
145 IF A$="TREE" THEN 700
150 IF LEFT$(A$,1)<>"Y" THEN 120
160 K=1
170 GOSUB 390
180 IF LEN(A$(K))=0 THEN 999
190 IF LEFT$(A$(K),2)="\Q" THEN 170
200 PRINT "Is it a ";RIGHT$(A$(K),LEN(A$(K))-2);
210 INPUT A$
220 A$=LEFT$(A$,1)
230 IF LEFT$(A$,1)="Y" THEN PRINT "Why not try another animal?": GOTO 120
240 INPUT "The animal you were thinking of was a ";V$: IF LEN(V$)=0 THEN 240
250 PRINT "Please type in a question that would distinguish a"
260 PRINT V$;" from a ";RIGHT$(A$(K),LEN(A$(K))-2);":"
270 INPUT X$: IF LEN(X$)=0 THEN 250
274 C=ASC(LEFT$(X$,1)): L=(C>=ASC("a"))AND(C<=ASC("z"))
276 IF L THEN X$=CHR$(C-ASC("a")+ASC("A"))+MID$(X$,2)
280 PRINT "For a ";V$;", the answer would be (y/n)";
290 INPUT A$
300 A$=LEFT$(A$,1): IF A$<>"Y" AND A$<>"N" THEN 280
310 IF A$="Y" THEN A$="Y": B$="N"
320 IF A$="N" THEN A$="N": B$="Y"
330 Z1=VAL(A$(0))
340 A$(0)=STR$(Z1+2)
350 A$(Z1)=A$(K)
360 A$(Z1+1)="\A"+V$
370 A$(K)="\Q"+X$+"\"+A$+STR$(Z1+1)+"\"+B$+STR$(Z1)+"\"
380 GOTO 120
390 REM     SUBROUTINE TO PRINT QUESTIONS
400 Q$=A$(K)
410 FOR Z=3 TO LEN(Q$)
415 IF MID$(Q$,Z,1)<>"\" THEN PRINT MID$(Q$,Z,1);: NEXT Z
420 INPUT C$
430 C$=LEFT$(C$,1)
440 IF C$<>"Y" AND C$<>"N" THEN 410
450 T$="\"+C$
455 FOR X=3 TO LEN(Q$)-1
460 IF MID$(Q$,X,2)=T$ THEN 480
470 NEXT X
475 STOP
480 FOR Y=X+1 TO LEN(Q$)
490 IF MID$(Q$,Y,1)="\" THEN 510
500 NEXT Y
505 STOP
510 K=VAL(MID$(Q$,X+2,Y-X-2))
520 RETURN
530 DATA "4","\QDoes it swim\Y2\N3\","\Afish","\Abird"
600 PRINT:PRINT "Animals I already know are:"
605 X=0
610 FOR I=1 TO VAL(A$(0))-1
620 IF LEFT$(A$(I),2)<>"\A" THEN 650
630 Y=INT(LEN(A$(I))/15)
640 IF X>0 AND X+Y>3 THEN X=0: PRINT
645 PRINT TAB(15*X);MID$(A$(I),3);: X=X+Y+1
650 NEXT I
660 PRINT
670 PRINT
680 GOTO 120
700 PRINT:PRINT "Decision tree:"
710 FOR I=1 TO VAL(A$(0))-1
720 Q$=A$(I): X=0: Y=0: PRINT STR$(I);TAB(3)
730 FOR Z=1 TO LEN(Q$)
740 T$=MID$(Q$,Z,1)
750 IF T$<>"\" THEN 780
760 IF Y=1 THEN PRINT "?";: Y=0
770 X=1: PRINT "   ";: GOTO 800
780 IF X=0 THEN PRINT T$;: GOTO 800
790 PRINT T$;": ";: X=0: IF T$="Q" THEN Y=1
800 NEXT Z: PRINT: NEXT I: PRINT
810 GOTO 120
999 END

# END
