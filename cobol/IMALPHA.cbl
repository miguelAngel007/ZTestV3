*     * FO5238 * 06/26/11 PROYECTO REBORN
000100 IDENTIFICATION   DIVISION.
000200 PROGRAM-ID.      IMALPHA.
000300*REMARKS.                                                         IM004
000370*       'IMALPHA' IS AN APPLICATION-WIDE INTERMEDIATE CALLED      IM004
000440*       MODULE THAT PROCESSES THE FIAS ALPHA-NAME FILE.           IM004
000510*                                                                 IM004
000580*       'SIALPMV' IS AN APPLICATION-WIDE I/O VSAM MODULE CALLED   IM004
000650*       BY 'IMALPHA' TO PERFORM THE PHYSICAL READ/WRITE FUNCTION. IM004
000720      SKIP2                                                       IM004
000801*--------------------------------------------------------------*  IMIB01A
000802*               ** HISTORY OF REVISIONS **                     *  IMIB01A
000803* DESCRIPTION                                           CHNGID *  IMIB01A
000804* ____________________________________________________  _______*  IMIB01A
000805*                                                              *  IMIB01A
000806* 16/08/10 REAPPLY CODE FOR DIFF VALUES OF WMS-TAX-CODE IMIB01A*  IMIB01A
000810*                                                              *  IMIB01A
000820*                                                              *  IMIB01A
000999*--------------------------------------------------------------*  IMIB01A
001000 ENVIRONMENT      DIVISION.
001100 DATA             DIVISION.
001200 WORKING-STORAGE  SECTION.
001201 77  SI-MODULE-INFO      PIC X(64)
001202     VALUE 'IMALPHA   -----TSD-             11/21/10  20.31.55'.
001203*    THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG
001204*    TO FIDELITY INFORMATION SERVICES AND IS
001205*    LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,
001206*    USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.
001207*    COPYRIGHT FIDELITY INFORMATION SERVICES
001208*    2010, ALL RIGHTS RESERVED.
001300 77  WS-PROGRAM-ID                   PIC X(08)  VALUE  'IMALPHA'.
001400 77  WS-PROGRAM-CHANGE               PIC X(08)  VALUE  '00/00/00'.
001500 77  WS-PROGRAM-CENTER-CHANGE        PIC X(08)  VALUE  '00/00/00'.
001600     SKIP2
001700 77  WS-SUB-1               COMP     PIC S9(02) VALUE  +000.
001800 77  WS-SUB-2               COMP     PIC S9(02) VALUE  +000.
001900 77  WS-SUB-3               COMP-3   PIC S9(03) VALUE  +000.
002000 77  WS-SUB-4               COMP     PIC S9(02) VALUE  +000.
002100 77  WS-SUB-5               COMP     PIC S9(02) VALUE  +000.
002200     SKIP2
002300 77  WS-COMP-SIX            COMP     PIC S9(01) VALUE  +6.
002400     SKIP2
002500 77  WS-LITT-ONE                     PIC X(01)  VALUE  '1'.
002600 77  WS-LITT-TWO                     PIC X(01)  VALUE  '2'.
002700     SKIP2
002702*    *---------------------------------------------------*        GTN2796
002704*    * SET THE WORKING-STORAGE FIELDS NEEDED BY SIGETNM2 *        GTN2796
002706*    *---------------------------------------------------*        GTN2796
002710 01  WS-COMP-AREA.                                                GTN2796
002720     03  HOLD-SIGNM2-LINE-LNG    PIC S9(4) COMP SYNC VALUE +0040. GTN2796
002730     03  HOLD-SIGNM2-AREA-LNG    PIC S9(4) COMP SYNC VALUE +0280. GTN2796
002740     03  HOLD-SIGNM2-LINE-OFFSET PIC S9(4) COMP SYNC VALUE +0000. GTN2796
002750     03  HOLD-SIGNM2-END-OFFSET  PIC S9(4) COMP SYNC VALUE +0001. GTN2796
002760     03  ERR-COUNT               PIC S9(4) COMP SYNC VALUE ZERO.  GTN2796
002770     03  SUBA                    PIC S9(4) COMP SYNC VALUE ZERO.  GTN2796
002775     03  WS-TAX-CODE-TST         PIC X.                           CIB5664
002776         88  VALID-IB-TAX-CODE   VALUES '1' THRU '8'.             IMIB01A
002780         88  VALID-TAX-CODE      VALUES '0' THRU '4'.             CIB5664
002785         88  VALID-TAX-CODE2     VALUES '1' THRU '4'.             CIB5664
002790         88  BUSINESS-TAX-CODE   VALUES '1' '3'.                  CIB5664
002795         88  PERSON-TAX-CODE     VALUES '2' '4'.                  CIB5664
002800*                                                                 GTN2796
002850     COPY SIWSGNM2.                                               GTN2796
002900     EJECT
003000 01  WS-FILE-AREA.
003100     05  WS-FILE-ALPHA               PIC X(01)  VALUE  'C'.
003200         88  WS-FILE-ALPHA-88-OPEN              VALUE  'O'.
003300         88  WS-FILE-ALPHA-88-CLOSED            VALUE  'C'.
003400     SKIP2
003500 01  WS-HOLD-AREA.
003600     05  WS-HOLD-PARTIAL-KEY.
003700         10  WS-HOLD-PARTIAL-1-18    PIC X(18).
003800         10  FILLER                  PIC X(2).
003900     05  WS-HOLD-NAME-AREA           PIC X(140).
004000     05  FILLER           REDEFINES  WS-HOLD-NAME-AREA.
004100         10  WS-HOLD-NAME            OCCURS 07.
004200             15  FILLER              PIC X(20).
004300     SKIP2
004400 01  WS-ERROR-AREA.
004500     05  WS-ERROR-1.
004600         10  WS-ERROR-1-LITT-1       PIC X(22).
004700         10  FILLER                  PIC X(13)  VALUE
004800             ' WITH ERROR ('.
004900         10  WS-ERROR-1-LITT-2       PIC X(1).
005000         10  FILLER                  PIC X(18)  VALUE
005100             ') AND TRAILER NAME'.
005200         10  WS-ERROR-1-LITT-3       PIC X(20).
005300     SKIP2
005400     COPY  SIWSMESS.
005500     EJECT
005800 01  WS-PARM-NAME-AREA               PIC X(280).
005900 01  FILLER                          REDEFINES WS-PARM-NAME-AREA.
006000     05  WS-PARM-NAME                OCCURS 7 TIMES PIC X(40).
007100     EJECT
007200 01  WS-ALPHA-READ-AREA.
007300     05  WS-ALPHA-READ-KEY.
007400         10  WS-ALPHA-READ-CNTL.
007500             15  WS-ALPHA-READ-BANK  PIC XX.
007600             15  WS-ALPHA-READ-KEY1  PIC X(16).
007700         10  WS-ALPHA-READ-KEY2      PIC S9(03)  COMP-3.
007800         10  FILLER                  PIC X(03).
007900     05  WS-ALPHA-READ-NAME          PIC X(20).
008000     05  WS-ALPHA-READ-ADDRESS       PIC X(20).
008100     05  WS-ALPHA-READ-ACCOUNT       PIC X(32).
008200     SKIP2
008300 01  WS-ALPHA-WRITE-AREA.
008400     05  WS-ALPHA-WRITE-KEY.
008500         10  WS-ALPHA-WRITE-CNTL.
008600             15  WS-ALPHA-WRITE-BANK PIC XX.
008700             15  WS-ALPHA-WRITE-KEY1 PIC X(16).
008800         10  WS-ALPHA-WRITE-KEY2     PIC S9(03)  COMP-3.
008900         10  FILLER                  PIC X(03).
009000     05  WS-ALPHA-WRITE-NAME         PIC X(20).
009100     05  WS-ALPHA-WRITE-ADDRESS      PIC X(20).
009200     05  WS-ALPHA-WRITE-ACCOUNT      PIC X(32).
009300     SKIP2
009400 01  NAME-ADDRESS.
009500     03  WORK-NAME-ADDRESS-TRAILER OCCURS 2 TIMES.
009600       04  WORK-NA-NO-LINES          PIC S9      COMP-3.
009700       04  WORK-NA-LINE                          OCCURS 8 TIMES.  9915845
009800         05  WORK-NAME-ADDR-TYPE     PIC X.
009900         05  WORK-LINE-NO            PIC X.
010000         05  WORK-NAME-ADDRESS       PIC X(40).
010100 01  FILLER REDEFINES NAME-ADDRESS.
010200     03  WORK-NAME-ADDRESS-TRLR-1    PIC X(337).                  9915845
010300     03  WORK-NAME-ADDRESS-TRLR-2    PIC X(337).                  9915845
010400     EJECT
010500 LINKAGE          SECTION.
010600     COPY  SIWSCNTL.
010650     COPY  IMWSENVO.                                              IM003
010700     EJECT
010800     COPY  IMAWKMST.
010900     SKIP3                                                        IM006
010910 01  BCR-ALPHA-FLAG                  PIC X.                       IM006
010920     EJECT                                                        IM006
011000 PROCEDURE  DIVISION          USING  I-O-CONTROL-AREA
011050                                     SI-ENVIRONMENT-AREA          IM003
011100                                     MASTER-AREA                  IM006
011120                                     BCR-ALPHA-FLAG.              IM006
011200 A1000-ENTRY.
011230     IF  SI-88-ENVIRONMENT-NO-ALPHA                               IM003
011260         GO TO A1000-EXIT.                                        IM003
011263     IF  I-O-CONTROL-OPERATOR EQUAL  'O'                          IM006
011264         GO TO A1000-FILE-CONTROL.                                IM006
011265     IF  I-O-CONTROL-OPERATOR EQUAL  'E'                          IM006
011266         GO TO A1000-FILE-CONTROL.                                IM006
011267     IF  BCR-ALPHA-FLAG EQUAL '3'                                 CMJ3787
011268         GO TO A1000-EXIT.                                        IM006
011269     IF  BCR-ALPHA-FLAG EQUAL '2'                                 IM006
011272         IF  I-O-CONTROL-OPERATOR EQUAL  'A'                      IM006
011275         PERFORM  G1500-IM-SSN-ADD  THRU  G1500-IM-SSN-EXIT       IM006
011278         GO TO A1000-EXIT.                                        IM006
011281     IF  BCR-ALPHA-FLAG EQUAL '2'                                 IM006
011284         IF  I-O-CONTROL-OPERATOR EQUAL  'D'                      IM006
011287         PERFORM  G2500-IM-SSN-DEL  THRU  G2500-IM-SSN-EXIT       IM006
011290         GO TO A1000-EXIT.                                        IM006
011295 A1000-FILE-CONTROL.                                              IM006
011300     IF  I-O-CONTROL-OPERATOR EQUAL  'A'
011400         PERFORM  G1000-IM-ALPHA-ADD  THRU  G1000-IM-EXIT
011500     ELSE
011600     IF  I-O-CONTROL-OPERATOR EQUAL  'D'
011700         PERFORM  G2000-IM-ALPHA-DEL   THRU  G2000-IM-EXIT
011800     ELSE
011900     IF  I-O-CONTROL-OPERATOR EQUAL  'E'
012000         IF  WS-FILE-ALPHA-88-OPEN
012100             CALL 'SIALPMV'   USING  I-O-CONTROL-AREA
012200                                     WS-ALPHA-WRITE-AREA
012300             MOVE  'C'           TO  WS-FILE-ALPHA
012400         ELSE
012500             NEXT SENTENCE
012600     ELSE
012700     IF  I-O-CONTROL-OPERATOR EQUAL  'O'
012800         IF  WS-FILE-ALPHA-88-CLOSED
012900             CALL 'SIALPMV'   USING  I-O-CONTROL-AREA
013000                                     WS-ALPHA-WRITE-AREA
013100             MOVE  'O'           TO  WS-FILE-ALPHA
013200         ELSE
013300             NEXT SENTENCE
013400     ELSE
013500         MOVE  WS-PROGRAM-ID     TO  SIMESS-PROGRAM
013600         MOVE  0501              TO  SIMESS-MESS-NO
013700         MOVE  SPACES            TO  SIMESS-OPTIONAL-MESSAGE
013800         CALL  'SIMESS'       USING  SIMESS-AREA.
013900 A1000-EXIT.
014000     GOBACK.
014100     EJECT
014200 G1000-IM-ALPHA-ADD.
014300*    ************************************************************
014400*    **                     IMPACS SYSTEM                      **
014500*    ************************************************************
014600*    ************************************************************
014700*    **  IMPACS NAME/ADDRESS LINE TYPE "1"  IS ACCOUNT NAME    **
014800*    **         NAME/ADDRESS LINE TYPE "2"  IS ACCOUNT ADDRESS **
014900*    ************************************************************
014930     IF  BCR-ALPHA-FLAG EQUAL '1'                                 IM006
014960         PERFORM  G1500-IM-SSN-ADD  THRU  G1500-IM-SSN-EXIT.      IM006
015000     IF  WMS-NME-ADDR-TLRS  EQUAL  ZEROES
015100         GO TO  G1000-IM-EXIT.
015200     MOVE  SPACES                TO  WS-ALPHA-WRITE-AREA.
015300     MOVE  WMS-CONTROL-1         TO  WS-ALPHA-WRITE-BANK.
015400     MOVE  SPACES                TO  WS-ALPHA-WRITE-ADDRESS.
015500     MOVE  WMS-CONTROL-KEY       TO  WS-ALPHA-WRITE-ACCOUNT.
015600     MOVE  +1                    TO  WS-SUB-4.
015700     MOVE SPACES                 TO  NAME-ADDRESS
015800     MOVE WMS-NAME-ADDRESS-TRAILER-1
015900                                 TO  WORK-NAME-ADDRESS-TRLR-1.
016000     IF  WMS-NME-ADDR-TLRS  EQUAL  +2
016100         MOVE WMS-NAME-ADDRESS-TRAILER-2
016200                                 TO  WORK-NAME-ADDRESS-TRLR-2.
016300     SKIP2
016400 G1000-IM-ADDRESS-LOOP-1.
016500     IF  WS-SUB-4  GREATER  THAN  WMS-NME-ADDR-TLRS
016600         GO TO  G1000-IM-EXIT.
016700     MOVE +1                     TO  WS-SUB-5.
016800 G1000-IM-ADDRESS-LOOP-2.
016850     IF  WS-SUB-5  EQUAL TO +9 OR                                 9915845
016900         WS-SUB-5  GREATER  THAN  WORK-NA-NO-LINES (WS-SUB-4)     9915845
017000         GO TO  G1000-IM-GET-NAME.
017100     IF  WORK-NAME-ADDR-TYPE     (WS-SUB-4, WS-SUB-5)
017200         EQUAL  WS-LITT-TWO
017300         MOVE  WORK-NAME-ADDRESS (WS-SUB-4, WS-SUB-5)
017400                                 TO  WS-ALPHA-WRITE-ADDRESS
017500     ELSE
017600         ADD +1 TO WS-SUB-5
017700         GO TO  G1000-IM-ADDRESS-LOOP-2.
017800     SKIP2
017900 G1000-IM-GET-NAME.
018000     MOVE  +1                    TO  WS-SUB-1
018100                                     WS-SUB-2
018200                                     WS-SUB-5.
018300     MOVE  SPACES                TO  WS-PARM-NAME-AREA
018400                                     WS-HOLD-NAME-AREA.
018500     SKIP2
018600 G1000-IM-NAME-LOOP.
018650     IF  WS-SUB-5  EQUAL +9                                       9919486
018700     OR  WS-SUB-5  GREATER  THAN  WORK-NA-NO-LINES (WS-SUB-4)     9919486
018800         GO TO  G1000-IM-CALL-GETNAME.
018900     IF  WORK-NAME-ADDR-TYPE     (WS-SUB-4, WS-SUB-5)
019000         EQUAL  WS-LITT-ONE
019100         MOVE  WORK-NAME-ADDRESS (WS-SUB-4, WS-SUB-5)
019200                                 TO  WS-PARM-NAME (WS-SUB-2)
019300                                     WS-HOLD-NAME (WS-SUB-2)
019400         ADD +1                  TO  WS-SUB-2
019500                                     WS-SUB-5
019600         GO TO  G1000-IM-NAME-LOOP
019700     ELSE
019800         ADD +1                  TO  WS-SUB-5
019900         GO TO  G1000-IM-NAME-LOOP.
020000     SKIP2
020100 G1000-IM-CALL-GETNAME.
020110*    *--------------------------------------------------------*   GTN2796
020120*    * SET UP THE CALL PARAMETERS FOR SIGETNM2.  GENERATE ANY *   GTN2796
020130*    * ERROR MESSAGES IF SIGNM2-NAME-IND IS NOT EQUAL A SPACE *   GTN2796
020140*    *--------------------------------------------------------*   GTN2796
020200     MOVE HOLD-SIGNM2-LINE-LNG     TO SIGNM2-LINE-LNG.            GTN2796
020300     MOVE '0'                      TO SIGNM2-RET-NAME-OPTION (4)  GTN2796
020310                                      SIGNM2-RET-NAME-OPTION (5)  GTN2796
020320                                      SIGNM2-RET-NAME-OPTION (6)  GTN2796
020330                                      SIGNM2-RET-NAME-OPTION (7)  GTN2796
020340                                      SIGNM2-RET-NAME-OPTION (8). GTN2796
020350     MOVE HOLD-SIGNM2-AREA-LNG     TO SIGNM2-AREA-LNG.            GTN2796
020360     MOVE HOLD-SIGNM2-LINE-OFFSET  TO SIGNM2-LINE-OFFSET.         GTN2796
020370     MOVE HOLD-SIGNM2-END-OFFSET   TO SIGNM2-END-LINE-OFFSET.     GTN2796
020400     CALL  'SIGETNM2'  USING       WS-PARM-NAME-AREA              GTN2796
020500                                   SIGETNM2-WORK-AREA.            GTN2796
020550     MOVE +0                       TO SUBA.                       GTN2796
020600*                                                                 GTN2796
020700 G1051-ERROR-SCAN.                                                GTN2796
020710     ADD  +1                       TO SUBA.                       GTN2796
020720     IF  SIGNM2-NAME-IND (SUBA) NOT EQUAL SPACE                   GTN2796
020730         PERFORM G1052-ERROR-LOOP THRU G1053-ERROR-LOOP-EXIT.     GTN2796
020740     IF  SUBA LESS THAN 5                                         GTN2796
020750         GO TO G1051-ERROR-SCAN.                                  GTN2796
020760     GO TO G1054-PREPARE-SUB.                                     GTN2796
020770*                                                                 GTN2796
020800 G1052-ERROR-LOOP.                                                GTN2796
020810     MOVE +0                         TO ERR-COUNT.                GTN2796
020820*                                                                 GTN2796
020900 G1053-ERROR-CONT.                                                GTN2796
020910     ADD +1                          TO ERR-COUNT.                GTN2796
020920     IF  SIGNM2-NAME-FLAG (SUBA, ERR-COUNT) EQUAL '1'             GTN2796
020930         MOVE WS-PROGRAM-ID          TO SIMESS-PROGRAM            GTN2796
020940         MOVE 003                    TO SIMESS-MESS-NO            GTN2796
020950         MOVE WMS-CONTROL-KEY        TO WS-ERROR-1-LITT-1         GTN2796
020960         MOVE SIGNM2-NAME-ERR-MSG (ERR-COUNT)                     GTN2796
020970                                     TO WS-ERROR-1-LITT-2         GTN2796
020980         MOVE WS-HOLD-NAME (1)       TO WS-ERROR-1-LITT-3         GTN2796
020990         MOVE WS-ERROR-1             TO SIMESS-OPTIONAL-MESSAGE   GTN2796
021000         CALL 'SIMESS'            USING SIMESS-AREA.              GTN2796
021100     IF  ERR-COUNT LESS THAN 8                                    GTN2796
021200         GO TO G1053-ERROR-CONT.                                  GTN2796
021300 G1053-ERROR-LOOP-EXIT.                                           GTN2796
021400     EXIT.                                                        GTN2796
021500*                                                                 GTN2796
021600 G1054-PREPARE-SUB.                                               GTN2796
021700     SKIP2
021800     MOVE +1                     TO  WS-SUB-1
021900                                     WS-SUB-2.
021950     MOVE +0                     TO  WS-SUB-3.                    GTN2796
022000     SKIP2
022100 G1000-IM-KEY1-LOOP.
022200     IF  SIGNM2-NAME (WS-SUB-1) EQUAL  SPACES                     GTN2796
022300         ADD +1 TO WS-SUB-4
022400         GO TO G1000-IM-ADDRESS-LOOP-1.
022410*    *--------------------------------------------------------*   GTN2796
022420*    * USE THE NEW DATANAME TO INITIALIZE THE LINE LENGTH FOR *   GTN2796
022430*    * SIKEYGNR                                               *   GTN2796
022440*    *--------------------------------------------------------*   GTN2796
022500     MOVE  HOLD-SIGNM2-LINE-LNG  TO  SIGNM2-LINE-LNG.             GTN2796
022600     CALL  'SIKEYGNR'         USING  SIGNM2-LINE-LNG              GTN2796
022700                                     SIGNM2-NAME (WS-SUB-1)       GTN2796
022800                                     WS-ALPHA-WRITE-KEY1.
022900     SKIP2
022950     MOVE SIGNM2-NAME (WS-SUB-1) TO  WS-ALPHA-WRITE-NAME.         GTN2796
023000     MOVE  +1                    TO  WS-ALPHA-WRITE-KEY2.
023100     MOVE  WS-ALPHA-WRITE-AREA   TO  WS-ALPHA-READ-AREA.
023200     MOVE  'K'                   TO  I-O-CONTROL-OPERATOR.
023300     CALL  'SIALPMV'          USING  I-O-CONTROL-AREA
023400                                     WS-ALPHA-READ-AREA.
023500     SKIP2
023600     MOVE  +001                  TO  WS-SUB-3.
023700     IF  I-O-88-NOT-FOUND
023800         GO TO  G1000-IM-KEY2-INSERT.
023900     SKIP2
024000 G1000-IM-KEY2-LOOP.
024400     MOVE  'R'                   TO  I-O-CONTROL-OPERATOR.
024500     CALL  'SIALPMV'          USING  I-O-CONTROL-AREA
024600                                     WS-ALPHA-READ-AREA.
024700     SKIP2
024800     IF  I-O-88-END-OF-FILE
024900         COMPUTE WS-ALPHA-WRITE-KEY2 = WS-ALPHA-READ-KEY2 + 1
025000         GO TO G1000-IM-KEY2-INSERT.                              9919486
025100     IF  WS-ALPHA-READ-CNTL   NOT EQUAL  WS-ALPHA-WRITE-CNTL
025200         COMPUTE  WS-ALPHA-WRITE-KEY2  EQUAL  (WS-SUB-3  + 001)
025300         GO TO G1000-IM-KEY2-INSERT.                              9919486
025310     IF  WS-SUB-3 EQUAL +999                                      9919486
025320         ADD +1      TO WS-SUB-1                                  9919486
025330         GO TO G1000-IM-KEY1-LOOP.                                9919486
025400     ADD +001        TO WS-SUB-3.                                 9919486
025500     IF  WS-SUB-3 EQUAL WS-ALPHA-READ-KEY2                        9919486
025600         GO TO G1000-IM-KEY2-LOOP
025700     ELSE
025800         MOVE WS-SUB-3 TO WS-ALPHA-WRITE-KEY2.
025900     SKIP2
026000 G1000-IM-KEY2-INSERT.
026800     MOVE  'A'                   TO  I-O-CONTROL-OPERATOR.
026900     CALL  'SIALPMV'          USING  I-O-CONTROL-AREA
027000                                     WS-ALPHA-WRITE-AREA.
027100     ADD +1                      TO  WS-SUB-1
027200                                     WS-SUB-2.
027300     GO TO  G1000-IM-KEY1-LOOP.
027400 G1000-IM-EXIT.
027500     EXIT.
027550     EJECT                                                        GTN2796
027560 G1500-IM-SSN-ADD.                                                GTN2796
027570*    ************************************************************ GTN2796
027580*    **                     IMPACS SYSTEM                      ** GTN2796
027590*    ************************************************************ GTN2796
027595*    ************************************************************ GTN2796
027596*    **  IMPACS SOCIAL SECURITY KEY                            ** GTN2796
027597*    **          --- ADD ---                                   ** GTN2796
027598*    ************************************************************ GTN2796
027600     MOVE WMS-TAX-CODE           TO  WS-TAX-CODE-TST.
027602*    IF  NOT VALID-TAX-CODE2                                      CIB5664
027603     IF  NOT VALID-IB-TAX-CODE                                    IMIB01A
027605         GO TO G1500-IM-SSN-EXIT.                                 GTN2796
027606     IF  WMS-TAX-NO EQUAL SPACES                                  GTN2796
027607         GO TO G1500-IM-SSN-EXIT.                                 GTN2796
027608     MOVE  SPACES                TO  WS-ALPHA-WRITE-AREA.         GTN2796
027609     MOVE  WMS-CONTROL-1         TO  WS-ALPHA-WRITE-BANK.         GTN2796
027610     MOVE  SPACES                TO  WS-ALPHA-WRITE-ADDRESS.      GTN2796
027611     MOVE  WMS-CONTROL-KEY       TO  WS-ALPHA-WRITE-ACCOUNT.      GTN2796
027612     MOVE  SPACES                TO  WS-PARM-NAME-AREA            GTN2796
027613                                     WS-HOLD-NAME-AREA.           GTN2796
027614     MOVE  WMS-TAX-NO            TO  WS-PARM-NAME (1)             GTN2796
027615                                     SIGNM2-NAME  (1).            GTN2796
027618 G1500-IM-CALL-GETNAME.                                           GTN2796
027630     MOVE +0                       TO SUBA.                       GTN2796
027631*                                                                 GTN2796
027632 G1551-ERROR-SCAN.                                                GTN2796
027633     ADD  +1                       TO SUBA.                       GTN2796
027634     IF  SIGNM2-NAME-IND (SUBA) NOT EQUAL SPACE                   GTN2796
027635         PERFORM G1052-ERROR-LOOP THRU G1053-ERROR-LOOP-EXIT.     GTN2796
027636     IF  SUBA LESS THAN 5                                         GTN2796
027637         GO TO G1551-ERROR-SCAN.                                  GTN2796
027638     SKIP2                                                        IM006
027639 G1500-IM-KEY1-LOOP.                                              IM006
027640     MOVE  HOLD-SIGNM2-LINE-LNG  TO  SIGNM2-LINE-LNG.             GTN2796
027641     CALL  'SIKEYGNR'         USING  SIGNM2-LINE-LNG              GTN2796
027642                                     SIGNM2-NAME (WS-SUB-1)       GTN2796
027643                                     WS-ALPHA-WRITE-KEY1.         IM006
027644     SKIP2                                                        IM006
027645     MOVE  +1                    TO  WS-ALPHA-WRITE-KEY2.         IM006
027646     MOVE  WS-ALPHA-WRITE-AREA   TO  WS-ALPHA-READ-AREA.          IM006
027647     MOVE  'K'                   TO  I-O-CONTROL-OPERATOR.        IM006
027648     CALL  'SIALPMV'          USING  I-O-CONTROL-AREA             IM006
027649                                     WS-ALPHA-READ-AREA.          IM006
027650     SKIP2                                                        IM006
027651     IF  I-O-88-NOT-FOUND                                         IM006
027652         GO TO  G1500-IM-KEY2-INSERT.                             IM006
027653     GO  TO  G1500-IM-SSN-EXIT.                                   IM006
027654     SKIP2                                                        IM006
027655 G1500-IM-KEY2-INSERT.                                            IM006
027656     MOVE SIGNM2-NAME (WS-SUB-1) TO WS-ALPHA-WRITE-NAME.          GTN2796
027661     MOVE  'A'                   TO  I-O-CONTROL-OPERATOR.        IM006
027662     CALL  'SIALPMV'          USING  I-O-CONTROL-AREA             IM006
027663                                     WS-ALPHA-WRITE-AREA.         IM006
027664 G1500-IM-SSN-EXIT.                                               IM006
027665     EXIT.                                                        IM006
027666     EJECT                                                        IM006
027700 G2000-IM-ALPHA-DEL.
027800*    ************************************************************
027900*    **                     IMPACS SYSTEM                      **
028000*    ************************************************************
028100*    ************************************************************
028200*    **  NAME/ADDRESS LINE TYPE "1" CONTAINS ACCOUNT NAME/AD-  **
028300*    **  DRESS INFORMATION USED TO BUILD FIAS ALPHA-XREF FILE. **
028400*    ************************************************************
028430     IF  BCR-ALPHA-FLAG EQUAL '1'                                 IM006
028460         PERFORM  G2500-IM-SSN-DEL  THRU  G2500-IM-SSN-EXIT.      IM006
028500     MOVE +1                     TO  WS-SUB-4.
028600     MOVE SPACES                 TO  NAME-ADDRESS.
028700     SKIP2
028800 G2000-IM-NAME-LOOP-1.
028900     IF  WS-SUB-4  GREATER  THAN  WMS-NME-ADDR-TLRS
029000         GO TO  G2000-IM-EXIT.
029100     MOVE WMS-NAME-ADDRESS-TRAILER-1
029200                                 TO  WORK-NAME-ADDRESS-TRLR-1.
029300     IF  WMS-NME-ADDR-TLRS  EQUAL  +2
029400         MOVE WMS-NAME-ADDRESS-TRAILER-2
029500                                 TO  WORK-NAME-ADDRESS-TRLR-2.
029600     MOVE +1                     TO  WS-SUB-2
029700                                     WS-SUB-5.
029800     MOVE  SPACES                TO  WS-PARM-NAME-AREA
029900                                     WS-HOLD-NAME-AREA.
030000     SKIP2
030100 G2000-IM-NAME-LOOP-2.
030200     IF  WS-SUB-5  GREATER  THAN  WORK-NA-NO-LINES (WS-SUB-4)
030300         GO TO  G2000-IM-GETNAME.
030400     IF  WORK-NAME-ADDR-TYPE     (WS-SUB-4, WS-SUB-5)
030500         EQUAL  WS-LITT-ONE
030600         MOVE  WORK-NAME-ADDRESS (WS-SUB-4, WS-SUB-5)
030700                                 TO  WS-PARM-NAME (WS-SUB-2)
030800                                     WS-HOLD-NAME (WS-SUB-2)
030900         ADD  +1                 TO  WS-SUB-2
031000                                     WS-SUB-5
031100         GO TO  G2000-IM-NAME-LOOP-2
031200     ELSE
031300         ADD  +1                 TO  WS-SUB-5
031400         GO TO  G2000-IM-NAME-LOOP-2.
031500     SKIP2
031600 G2000-IM-GETNAME.
031650     MOVE '0'                      TO SIGNM2-RET-NAME-OPTION (4)  GTN2796
031700                                      SIGNM2-RET-NAME-OPTION (5)  GTN2796
031750                                      SIGNM2-RET-NAME-OPTION (6)  GTN2796
031800                                      SIGNM2-RET-NAME-OPTION (7)  GTN2796
031850                                      SIGNM2-RET-NAME-OPTION (8). GTN2796
031900     MOVE HOLD-SIGNM2-AREA-LNG     TO SIGNM2-AREA-LNG.            GTN2796
031950     MOVE HOLD-SIGNM2-LINE-OFFSET  TO SIGNM2-LINE-OFFSET.         GTN2796
032000     MOVE HOLD-SIGNM2-END-OFFSET   TO SIGNM2-END-LINE-OFFSET.     GTN2796
032050     MOVE HOLD-SIGNM2-LINE-LNG     TO SIGNM2-LINE-LNG.            GTN2796
032100     CALL  'SIGETNM2'  USING       WS-PARM-NAME-AREA              GTN2796
032150                                   SIGETNM2-WORK-AREA.            GTN2796
032200     MOVE +0                       TO SUBA.                       GTN2796
032250*                                                                 GTN2796
032300 G2051-ERROR-SCAN.                                                GTN2796
032350     ADD  +1                       TO SUBA.                       GTN2796
032400     IF  SIGNM2-NAME-IND (SUBA) NOT EQUAL SPACE                   GTN2796
032450         PERFORM G1052-ERROR-LOOP THRU G1053-ERROR-LOOP-EXIT.     GTN2796
032500     IF  SUBA LESS THAN 5                                         GTN2796
032550         GO TO G2051-ERROR-SCAN.                                  GTN2796
033100     MOVE +1                     TO  WS-SUB-1.
033200     SKIP2
033300 G2000-IM-KEY1-LOOP.
033400     IF  WS-SUB-1 GREATER THAN 7 OR
033500         WS-PARM-NAME (WS-SUB-1) EQUAL SPACES
033600         ADD +1 TO WS-SUB-4
033700         GO TO  G2000-IM-NAME-LOOP-1.
033800     MOVE  HOLD-SIGNM2-LINE-LNG  TO  SIGNM2-LINE-LNG.             GTN2796
033900     MOVE  LOW-VALUES            TO  WS-ALPHA-WRITE-AREA.
034000     MOVE  WMS-CONTROL-1         TO  WS-ALPHA-WRITE-BANK.
034100     CALL  'SIKEYGNR'         USING  SIGNM2-LINE-LNG              GTN2796
034200                                     SIGNM2-NAME (WS-SUB-1)       GTN2796
034300                                     WS-ALPHA-WRITE-KEY1.
034400     MOVE WS-ALPHA-WRITE-CNTL     TO  WS-HOLD-PARTIAL-1-18.
034500     MOVE  'T'                   TO  I-O-CONTROL-OPERATOR.
034600     CALL  'SIALPMV'          USING  I-O-CONTROL-AREA
034700                                     WS-ALPHA-WRITE-AREA.
034800     IF  I-O-88-NOT-FOUND
034900         ADD  +1                 TO  WS-SUB-1
035000         GO TO  G2000-IM-KEY1-LOOP.
035100     SKIP2
035200 G2000-IM-KEY2-LOOP.
035300     MOVE  'R'                   TO  I-O-CONTROL-OPERATOR.
035400     CALL  'SIALPMV'          USING  I-O-CONTROL-AREA
035500                                     WS-ALPHA-WRITE-AREA.
035600     IF  NOT  I-O-88-END-OF-FILE
035700         IF  WS-ALPHA-WRITE-CNTL   EQUAL  WS-HOLD-PARTIAL-1-18
035800             IF  WS-ALPHA-WRITE-ACCOUNT  EQUAL  WMS-CONTROL-KEY
035900                 MOVE 'D'        TO   I-O-CONTROL-OPERATOR
036000                 CALL 'SIALPMV' USING I-O-CONTROL-AREA
036100                                      WS-ALPHA-WRITE-AREA
036200                 GO TO  G2000-IM-KEY2-LOOP
036300             ELSE
036400                 GO TO  G2000-IM-KEY2-LOOP                        IM006
036600         ELSE
036700             ADD  +1             TO WS-SUB-1
036800             GO TO  G2000-IM-KEY1-LOOP
036900     ELSE
037000         ADD  +1                 TO  WS-SUB-1
037100         GO TO G2000-IM-KEY1-LOOP.
037200 G2000-IM-EXIT.
037300     EXIT.
037320 G2500-IM-SSN-DEL.                                                IM006
037340*    ************************************************************ IM006
038330*    **                     IMPACS SYSTEM                      ** IM006
039320*    ************************************************************ IM006
040310*    ************************************************************ IM006
041300*    **  SOCIAL SECURITY                                       ** IM006
042290*    **                        DELETE                          ** IM006
043280*    ************************************************************ IM006
043282     MOVE WMS-TAX-CODE           TO  WS-TAX-CODE-TST.
043284*    IF  NOT VALID-TAX-CODE2                                      CIB5664
043290     IF  NOT VALID-IB-TAX-CODE                                    IMIB01A
043480         GO TO G2500-IM-SSN-EXIT.                                 IM006
043580     IF  WMS-TAX-NO EQUAL SPACES                                  IM006
043680         GO TO G2500-IM-SSN-EXIT.                                 IM006
044270     MOVE  SPACES                TO  WS-PARM-NAME-AREA            IM006
045260                                     WS-HOLD-NAME-AREA.           IM006
046250     SKIP2                                                        IM006
047240     MOVE  WMS-TAX-NO                                             IM006
048230                                 TO  WS-PARM-NAME (1)             IM006
049220                                     WS-HOLD-NAME (1).            IM006
050210 G2500-IM-GETNAME.                                                IM006
050300     MOVE '0'                      TO SIGNM2-RET-NAME-OPTION (4)  GTN2796
050400                                      SIGNM2-RET-NAME-OPTION (5)  GTN2796
050500                                      SIGNM2-RET-NAME-OPTION (6)  GTN2796
050600                                      SIGNM2-RET-NAME-OPTION (7)  GTN2796
050700                                      SIGNM2-RET-NAME-OPTION (8). GTN2796
050800     MOVE HOLD-SIGNM2-AREA-LNG     TO SIGNM2-AREA-LNG.            GTN2796
050900     MOVE HOLD-SIGNM2-LINE-OFFSET  TO SIGNM2-LINE-OFFSET.         GTN2796
051000     MOVE HOLD-SIGNM2-END-OFFSET   TO SIGNM2-END-LINE-OFFSET.     GTN2796
051100     MOVE HOLD-SIGNM2-LINE-LNG     TO SIGNM2-LINE-LNG.            GTN2796
051200     CALL  'SIGETNM2'  USING       WS-PARM-NAME-AREA              GTN2796
051300                                   SIGETNM2-WORK-AREA.            GTN2796
051400     MOVE +0                       TO SUBA.                       GTN2796
051500                                                                  GTN2796
051600 G2551-ERROR-SCAN.                                                GTN2796
051700     ADD  +1                       TO SUBA.                       GTN2796
051800     IF  SIGNM2-NAME-IND (SUBA) NOT EQUAL SPACE                   GTN2796
051900         PERFORM G1052-ERROR-LOOP THRU G1053-ERROR-LOOP-EXIT.     GTN2796
052000     IF  SUBA LESS THAN 5                                         GTN2796
052100         GO TO G2551-ERROR-SCAN.                                  GTN2796
065060     SKIP2                                                        IM006
066050 G2500-IM-KEY1-LOOP.                                              IM006
067040     IF  WS-PARM-NAME (1) EQUAL SPACES                            IM006
068030         GO TO G2500-IM-SSN-EXIT.                                 IM006
069020     MOVE  HOLD-SIGNM2-LINE-LNG  TO  SIGNM2-LINE-LNG.             GTN2796
070010     MOVE  LOW-VALUES            TO  WS-ALPHA-WRITE-AREA.         IM006
071000     MOVE  WMS-CONTROL-1         TO  WS-ALPHA-WRITE-BANK.         IM006
071990     CALL  'SIKEYGNR'         USING  SIGNM2-LINE-LNG              GTN2796
072980                                     SIGNM2-NAME (WS-SUB-1)       GTN2796
073970                                     WS-ALPHA-WRITE-KEY1.         IM006
074960     MOVE WS-ALPHA-WRITE-CNTL     TO  WS-HOLD-PARTIAL-1-18.       IM006
075950     MOVE  'T'                   TO  I-O-CONTROL-OPERATOR.        IM006
076940     CALL  'SIALPMV'          USING  I-O-CONTROL-AREA             IM006
077930                                     WS-ALPHA-WRITE-AREA.         IM006
078920     IF  I-O-88-NOT-FOUND                                         IM006
079910         GO TO G2500-IM-SSN-EXIT.                                 IM006
080900     SKIP2                                                        IM006
081890 G2500-IM-KEY-DELETE.                                             IM006
082880     MOVE  'R'                   TO  I-O-CONTROL-OPERATOR.        IM006
083870     CALL  'SIALPMV'          USING  I-O-CONTROL-AREA             IM006
084860                                     WS-ALPHA-WRITE-AREA.         IM006
085850     IF  I-O-88-END-OF-FILE                                       IM006
086840         GO TO G2500-IM-SSN-EXIT.                                 IM006
087830     IF  WS-ALPHA-WRITE-CNTL NOT EQUAL  WS-HOLD-PARTIAL-1-18      IM006
088820         GO TO G2500-IM-SSN-EXIT.                                 IM006
089810     IF  WS-ALPHA-WRITE-ACCOUNT NOT EQUAL  WMS-CONTROL-KEY        IM006
090800         GO TO G2500-IM-SSN-EXIT.                                 IM006
091790     MOVE 'D'        TO   I-O-CONTROL-OPERATOR.                   IM006
092780     CALL 'SIALPMV' USING I-O-CONTROL-AREA                        IM006
093770                          WS-ALPHA-WRITE-AREA.                    IM006
094760 G2500-IM-SSN-EXIT.                                               IM006
095750     EXIT.                                                        IM006
096740     EJECT                                                        IM006
