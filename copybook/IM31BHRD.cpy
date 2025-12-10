*     * 512468*28/04/28 JCTE IMPLEMENTAR SPLIT FORMA NATURAL SDO.COMP.
*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*
000200*
000300*--------------------------------------------------------------*
000400*         IMPACS BALANCE HISTORY READ COPYBOOK
000500*--------------------------------------------------------------*
000600*
000700*
000800 C0810-BAL-HIST-READ.
000900*--------------------------------------------------------------*
001000*    BYPASS READING THE BALANCE HISTORY FILE IF                *
001100*      -   BALANCE HISTORY OPTION IS NOT USED AT BCR LEVEL     *
001200*      -   ACCOUNT HEADER RECORD    (WMS-EXC-CODE = 0)         *
001300*      -   NEW ACCOUNT              (SKIP-READ-SW = 1)         *
001400*      -   ONLINE NEW ACCOUNT                                  *
001500*      -   INITIAL READ             (FIRST-READ-SW = 1)        *
001600*          (NO BCR HAS BEEN READ YET)                          *
001700*      -   ACCOUNT DOES NOT USE BALANCE HISTORY                *
001800*    ACCOUNTS WITH HIFI INDICATOR GREATER THAN ZERO AND USING  *
001900*     SPLIT RATE ACCRUALS HAVE A BALANCE HISTORY AGGREGATE     *
002000*     RECORD.                                                  *
002100*--------------------------------------------------------------*
002200     IF  WBC-BAL-HIST-FLAG EQUAL '0'                              0266741
002300     OR  WMS-EXC-CODE     EQUAL '0'
002400     OR  SKIP-READ-SW     EQUAL '1'
002500     OR  FIRST-READ-SW    EQUAL '1'
002550     OR  MST-EOF          EQUAL HIGH-VALUES                       9915857
002600     OR (SI-88-ENVIRONMENT-ONLINE
002700     AND IM-MEMO-OL-NEW-ACCT)                                     1003520
002800         GO TO C0840-EXIT.                                        9915857
002805                                                                  100
002810     IF  MST-1 NOT EQUAL HIGH-VALUES                              1003520
002815         IF  MST-1 NOT EQUAL WBC1-CONTROL-1                       0266741
002820             MOVE '1'            TO NEW-BK-NO-TRN-FLAG            1003520
002825             GO TO C0840-EXIT                                     9915857
002830         ELSE                                                     1003520
002835             IF  ((MST-2 NOT EQUAL WBC1-CONTROL-2)                0266741
002840             AND (WBC-CTL2-FLAG EQUAL '2'))                       0266741
002845             OR  ((MST-3 NOT EQUAL WBC1-CONTROL-3)                0266741
002850             AND (WBC-CTL3-FLAG EQUAL '2'))                       0266741
002855                 MOVE '1'                TO NEW-BK-NO-TRN-FLAG    1003520
002860                 GO TO C0840-EXIT.                                9915857
002900*--------------------------------------------------------------*
003000*    INITIALIZE BALANCE HISTORY RECORDS ON THE ACCOUNTS WHEN   *
003100*    THE BCR BALANCE HISTORY FLAG IS '2' - INITIAL BUILD.      *
003200*--------------------------------------------------------------*
003300     IF  WBC-BAL-HIST-FLAG EQUAL '2'                              0266741
003400         PERFORM INIT-BALANCE-HISTORY THRU INIT-BAL-HIST-EXIT
003500         GO TO C0840-EXIT.                                        9915857
003510*--------------------------------------------------------------*  9915857
003520*  IF THIS ACCOUNT IS AN INTER-BRANCH TRANSFER ON A NON-PROCESS*  9915857
003530*  BANK THEN THE BAL HIST RECS WILL BE WRITTEN WITHOUT ERROR   *  9915857
003540*  CHECING.                                                    *  9915857
003550*--------------------------------------------------------------*  9915857
003560     IF  WBC-PROCESS-FLAG    EQUAL '0'                            0266741
003570         IF  WMS-IBT-FLAG    EQUAL '3'                            9915857
003590             GO TO C0815-BH-CLEAR.                                9915857
003600     IF  WMS-BALANCE-HISTORY   EQUAL '1'
003700     OR (WMS-IOD-RATE-USE EQUAL 'S'                               1003625
003800     AND WMS-HIFI-INDICATOR GREATER THAN +0)                      1003520
003900     OR (WMS-OD-ACCRUAL-TRLR   EQUAL '1'
004000     AND WMS-ODAC-BAL-HIST EQUAL '1')                             1003520
004100     OR (WMS-SAVINGS-TRLR      EQUAL '1'
004200     AND WMS-SAV-BAL-HIST  EQUAL '1')                             1003520
004300         GO TO C0815-BH-CLEAR.
004400     GO TO C0840-EXIT.                                            9915857
004500 C0815-BH-CLEAR.
004600*--------------------------------------------------------------*
004700*  CLEAR THE BALANCE HISTORY WORK AREAS                        *
004800*--------------------------------------------------------------*
004900     MOVE 'FFF'               TO  BAL-HIST-WORK-FLAGS.
005000     MOVE 'F'                 TO  BAL-HIST-WORK-AGGR-FLAGS.
005100     MOVE LOW-VALUES          TO  WORK-BAL-HIST-IOD
005200                                  WORK-BAL-HIST-MMDA
005300                                  WORK-BAL-HIST-ODAC
005400                                  WORK-BAL-HIST-SAV
005410                                  WORK-BAL-HIST-SAV-TIER          0316967
005500                                  WORK-BAL-HIST-MMDA-AGGR.
005600     MOVE ZERO                TO  WBHI-ENTRIES
005700                                  WBHM-ENTRIES
005800                                  WBHO-ENTRIES
005900                                  WBHS-ENTRIES
005910                                  WBHT-ENTRIES                    0316967
006000                                  WBHA-MMDA-ENTRIES.
006100 C0820-BH-READ-RTN.
006200*--------------------------------------------------------------*
006300*  "SKIP-READ-BAL" CONTROLS READING THE BALANCE HISTORY FILE   *
006400*      "0" = NORMAL SEQUENTIAL READ                            *
006500*      "1" = READ PREVIOUSLY READ RECORD                       *
006600*      "E" = END OF FILE                                       *
006700*--------------------------------------------------------------*
006800     IF  SKIP-READ-BAL EQUAL 'E'
006900         GO TO C0830-BH-CHK-ERRORS.
007000     IF  SKIP-READ-BAL NOT EQUAL  '1'
007050         MOVE 'I'             TO  I-O-CONTROL-ACCESS              9915848
007100         MOVE 'R'             TO  I-O-CONTROL-OPERATOR
007200         CALL 'IMBALM'     USING  I-O-CONTROL-AREA                9915848
007300                                  BALANCE-HISTORY-REC
007310                                  BAL-HIST-LENGTHS                9915848
007320                                  SI-ENVIRONMENT-AREA             9915848
007330                                  DDA-WRKBCR-1                    0447266
007400         IF  I-O-88-END-OF-FILE
007500             MOVE HIGH-VALUES TO BH-CONTROLS
007600             MOVE 'E'         TO SKIP-READ-BAL
007700             GO TO C0830-BH-CHK-ERRORS
007800         ELSE
007900             MOVE '0'         TO SKIP-READ-BAL
008000     ELSE
008100         MOVE 'N'             TO  I-O-CONTROL-OPERATOR
008200         MOVE '0'             TO  SKIP-READ-BAL
008300         CALL 'IMBALM'     USING  I-O-CONTROL-AREA                9915848
008400                                  BALANCE-HISTORY-REC             9915848
008410                                  BAL-HIST-LENGTHS                9915848
008420                                  SI-ENVIRONMENT-AREA             0447266
008430                                  DDA-WRKBCR-1.                   0447266
008500     IF  BH-CONTROLS GREATER THAN  MST-CONTROL
008600         MOVE '1'             TO  SKIP-READ-BAL
008700         GO TO C0830-BH-CHK-ERRORS.
008800     IF  BH-CONTROLS   LESS THAN  MST-CONTROL
008900         MOVE 501             TO  SIMESS-MESS-NO
009000         MOVE BH-KEY          TO  SIMESS-OPTIONAL-MESS5
009100         MOVE SIMESS-MESS5    TO  SIMESS-OPTIONAL-MESSAGE
009200         CALL 'SIMESS'     USING  SIMESS-AREA
009300         GO TO C0820-BH-READ-RTN.
009400*--------------------------------------------------------------*
009500* SAVE THE BALANCE HISTORY RECORD IN THE APPROPRIATE WORK AREA *
009600*--------------------------------------------------------------*
009700     IF  BH-IOD
009800         MOVE BALANCE-HISTORY-REC TO WORK-BAL-HIST-IOD
009900     ELSE
010000     IF  BH-MMDA
010100         IF  BH-SUB-TYPE EQUAL LOW-VALUES
010200             MOVE BALANCE-HISTORY-REC TO WORK-BAL-HIST-MMDA
010210             IF WMS-USER-CD-16 = '1'                              512468
010211                IF WMS-IOD-RATE-USE = 'T'                         512468
010212                   MOVE LOW-VALUES      TO WORK-BAL-HIST-MMDA-AGGR512468
010213                   MOVE WMS-CONTROL-KEY TO WBHA-MMDA-CONTROLS     512468
010214                   MOVE 'M'             TO WBHA-MMDA-TYPE         512468
010215                   MOVE 'A'             TO WBHA-MMDA-SUB-TYPE     512468
010216                   MOVE +0              TO WBHA-MMDA-ENTRIES      512468
010217                                                                  512468
010218                   MOVE 'S'             TO WMS-IOD-RATE-USE       512468
010219                END-IF
010220             END-IF
010300         ELSE
010400             MOVE BALANCE-HISTORY-REC TO WORK-BAL-HIST-MMDA-AGGR
010500     ELSE
010600     IF  BH-ODAC
010700         MOVE BALANCE-HISTORY-REC TO WORK-BAL-HIST-ODAC
010800     ELSE
010900     IF  BH-SAV
011000         MOVE BALANCE-HISTORY-REC TO WORK-BAL-HIST-SAV
011100     ELSE
011110     IF  BH-SAVT                                                  0316967
011120         MOVE BALANCE-HISTORY-REC TO WORK-BAL-HIST-SAV-TIER       0316967
011130     ELSE                                                         0316967
011200         MOVE 501            TO  SIMESS-MESS-NO
011300         MOVE BH-KEY         TO  SIMESS-OPTIONAL-MESS6
011400         MOVE SIMESS-MESS6   TO  SIMESS-OPTIONAL-MESSAGE
011500         CALL 'SIMESS'    USING  SIMESS-AREA.
011510     IF  BH-CONTROLS EQUAL MST-CONTROL                            1003520
011520     AND ((BH-CTL1 NOT EQUAL WBC1-CONTROL-1)                      0266741
011530     OR  (WBC-CTL2-FLAG EQUAL '2'                                 0266741
011540     AND BH-CTL2 NOT EQUAL WBC1-CONTROL-2)                        0266741
011550     OR  (WBC-CTL3-FLAG EQUAL '2'                                 0266741
011560     AND BH-CTL3 NOT EQUAL WBC1-CONTROL-3))                       0266741
011570         MOVE '1'            TO  SKIP-READ-BAL                    1003520
011580         GO TO C0840-EXIT.                                        9915857
011600     GO TO C0820-BH-READ-RTN.
011700 C0830-BH-CHK-ERRORS.
011800*--------------------------------------------------------------*
011900* INITIALIZE THE WORK AREA AND REPORT ON SIMESS ANY MISSING    *
012000* RECORDS AS INDICATED BY THE BAL-HIST FLAGS ON THE ACCOUNT.   *
012100*--------------------------------------------------------------*
012200     IF  MST-CONTROL EQUAL HIGH-VALUES
012300         GO TO C0840-EXIT.                                        9915857
012400     IF  WMS-BALANCE-HISTORY EQUAL '0'
012500         IF  WMS-IOD-RATE-USE EQUAL 'S'                           1003625
012600             NEXT SENTENCE
012700         ELSE
012800             GO TO C0835.
012900     IF  (WMS-IOD-RATE-USE EQUAL 'S'                              1003625
013000     AND WMS-HIFI-INDICATOR GREATER THAN +0)
013100         MOVE 'S' TO BHW-MMDA-AGGR-FLAG
013200             IF  WBHA-MMDA-CONTROLS EQUAL MST-CONTROL
013300                 IF  WBHA-S-T-CYCLE-TDY (1) EQUAL '1'
013400                     MOVE ZERO TO WBHA-MMDA-ENTRIES
013500                 ELSE
013600                     NEXT SENTENCE
013700             ELSE
013800                 MOVE MST-CONTROL TO WBHA-MMDA-CONTROLS
013900                 MOVE 'M'         TO SIMESS-OPTIONAL-MESS7-T
014000                                     WBHA-MMDA-TYPE
014100                 MOVE 'A'         TO SIMESS-OPTIONAL-MESS7-S
014200                                     WBHA-MMDA-SUB-TYPE
014300                 GO TO C0840-BH-ERROR.
014400     IF  WMS-BALANCE-HISTORY EQUAL '0'
014500         GO TO C0835.
014600     IF  WMS-HIFI-INDICATOR GREATER THAN ZERO
014700         MOVE 'M'             TO BHW-IOD-FLAG
014800         IF  WBHM-CONTROLS EQUAL MST-CONTROL
014900             GO TO C0835
015000         ELSE
015100             MOVE MST-CONTROL TO WBHM-CONTROLS
015200             MOVE 'M'         TO SIMESS-OPTIONAL-MESS7-T
015300                                 WBHM-TYPE
015400             GO TO C0840-BH-ERROR.
015500     IF  WMS-IOD-RATE-PTR GREATER THAN ZERO
015600         MOVE 'I'             TO BHW-IOD-FLAG
015700         IF  WBHI-CONTROLS EQUAL MST-CONTROL
015800             GO TO C0835
015900         ELSE
016000             MOVE MST-CONTROL TO WBHI-CONTROLS
016100             MOVE 'I'         TO SIMESS-OPTIONAL-MESS7-T
016200                                 WBHI-TYPE
016300             GO TO C0840-BH-ERROR.
016400 C0835.
016500     IF  WMS-OD-ACCRUAL-TRLR  EQUAL '1'
016600     AND WMS-ODAC-BAL-HIST    EQUAL '1'
016700         MOVE 'N'             TO BHW-ODAC-FLAG
016800         IF  WBHO-CONTROLS NOT EQUAL MST-CONTROL
016900             MOVE 'O'         TO SIMESS-OPTIONAL-MESS7-T
017000                                 WBHO-TYPE
017100             MOVE MST-CONTROL TO WBHO-CONTROLS
017200             GO TO C0840-BH-ERROR.
017300     IF  WMS-SAVINGS-TRLR  EQUAL '1'
017400     AND WMS-SAV-BAL-HIST  EQUAL '1'
017410         IF  WMS-SAV-TIER-PTR GREATER +0                          0316967
017500             MOVE 'T'             TO BHW-SAV-FLAG                 0316967
017520             IF  WBHT-CONTROLS NOT EQUAL MST-CONTROL              0316967
017530                 MOVE 'T'         TO SIMESS-OPTIONAL-MESS7-T      0316967
017540                                 WBHT-TYPE                        0316967
017550                 MOVE MST-CONTROL TO WBHT-CONTROLS                0316967
017560                 GO TO C0840-BH-ERROR                             0316967
017565             ELSE                                                 0316967
017570                 NEXT SENTENCE                                    0316967
017575         ELSE                                                     0316967
017580             MOVE 'R'             TO BHW-SAV-FLAG                 0316967
017600             IF  WBHS-CONTROLS NOT EQUAL MST-CONTROL              0316967
017700                 MOVE 'S'         TO SIMESS-OPTIONAL-MESS7-T      0316967
017800                                 WBHS-TYPE
017900                 MOVE MST-CONTROL TO WBHS-CONTROLS                0316967
018000                 GO TO C0840-BH-ERROR.                            0316967
018100     GO TO C0840-EXIT.                                            9915857
018200 C0840-BH-ERROR.
018300     MOVE 501                 TO  SIMESS-MESS-NO.
018400     MOVE MST-CONTROL         TO  SIMESS-OPTIONAL-MESS7.
018500     MOVE SIMESS-MESS7        TO  SIMESS-OPTIONAL-MESSAGE.
018600     CALL 'SIMESS'         USING  SIMESS-AREA.
018700 C0840-EXIT.  EXIT.
018800
018900 C0850-BH-READ-END.
