*     * FO5238 * 06/26/11 PROYECTO REBORN
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.    IMACTM.
000300 DATE-WRITTEN.  JULY 1983.
000400 DATE-COMPILED. TODAY.
000500 ENVIRONMENT    DIVISION.
000600 DATA DIVISION.
000700 WORKING-STORAGE SECTION.
000701 77  SI-MODULE-INFO      PIC X(64)
000702     VALUE 'IMACTM    -----TSD-             02/19/09  04.58.12'.
000705*    THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG
000706*    TO FIDELITY INFORMATION SERVICES AND IS
000707*    LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,
000708*    USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.
000709*    COPYRIGHT FIDELITY INFORMATION SERVICES
000710*    2009, ALL RIGHTS RESERVED.
000800 01  PROGRAM-NAME                    PIC X(8)    VALUE 'IMACTM'.
000900 01  W-HOLD-OPERATOR                 PIC X(1)    VALUE ' '.
001000 01  W-HOLD-VSAM-KEY                 PIC X(22)   VALUE ' '.
001100 01  W-FIRST-EXC-MAST-SWITCH         PIC X(1)    VALUE 'N'.
001200 01  W-FIRST-SEQ-MAST-INPUT-SWT      PIC X(1)    VALUE 'N'.
001300 01  W-FIRST-SEQ-MAST-OUTPUT-SWT     PIC X(1)    VALUE 'N'.
001400 01  INVALID-ENV-MSG.
001500     03  FILLER                      PIC X(38)   VALUE
001600         'ENVIRONMENT SEQ/VSAM CONTROL INVALID ('.
001700     03  INVALID-ENV-LIT             PIC X.
001800     03  FILLER                      PIC X(18)   VALUE
001900         ') PASSED TO MODULE'.
002000 01  NON-SUPPORT-MSG.
002100     03  FILLER                      PIC X(36)   VALUE
002200         'NON-SUPPORTED I-O-CONTROL-OPERATOR ('.
002300     03  NON-SUPPORT-LIT             PIC X.
002400     03  FILLER                      PIC X(18)   VALUE
002500         ') PASSED TO MODULE'.
002600 01  EXC-FILE-HAS-RECORDS-MSG.
002700     03  FILLER                      PIC X(75)   VALUE
002800         'VSAM REPORT EXCEPTION FILE (IMACTMU) CONTAINS RECORDS IT
002900-        ' SHOULD BE EMPTY'.
003000     COPY SIWSMESS.
003100 01  COMPRESSED-MASTER-AREA.
003200     03  CMA-LENGTH                  PIC S9(4)   VALUE +0  COMP.
003300     03  FILLER                      PIC XX      VALUE ' '.
003400     03  COMPRESSED-MASTER-RECORD.
003500         05  CMA-HEADER-AREA.
003600             10  CMA-CONTROL-KEY     PIC X(22)   VALUE ' '.
003700             10  CMA-EXC-CODE        PIC X       VALUE ' '.
003800             10  FILLER              PIC X(61)   VALUE ' '.
003900         05  FILLER                  PIC X(16542) VALUE SPACES.   0930011
004000     EJECT
004100 LINKAGE SECTION.
004200     COPY SIWSCNTL.
004300     EJECT
004400     COPY IMAWKMST.
004500     EJECT
004600     COPY IMWSENVO.                                               IM003
004700     EJECT
004800 PROCEDURE DIVISION  USING  I-O-CONTROL-AREA
004900                            MASTER-AREA
005000                            SI-ENVIRONMENT-AREA.
005100 DRIVER.
005200     IF (SI-88-ENVIRONMENT-SQVS                                   IM004
005260     AND SI-88-ENVIRONMENT-IM31)                                  IM004
005320         GO TO SQVS-LOGIC                                         IM004
005380     ELSE                                                         IM004
005440         IF SI-88-ENVIRONMENT-SEQ                                 IM004
005500             GO TO SEQ-LOGIC                                      IM004
005560         ELSE                                                     IM004
005620             IF SI-88-ENVIRONMENT-VSAM                            IM004
005680                 GO TO VSAM-LOGIC                                 IM004
005740             ELSE                                                 IM004
005800                 MOVE PROGRAM-NAME TO SIMESS-PROGRAM              IM004
005860                 MOVE 501          TO SIMESS-MESS-NO              IM004
005920                 MOVE SI-ENVIRONMENT-VSAM TO INVALID-ENV-LIT      IM004
005980                 MOVE INVALID-ENV-MSG TO SIMESS-OPTIONAL-MESSAGE  IM004
006040                 CALL 'SIMESS' USING SIMESS-AREA                  IM004
006100                 GO TO GOBACK-PARA.                               IM004
006400     EJECT
006500 SEQ-LOGIC.
006600     IF I-O-88-SEQ-READ
006700         PERFORM OPEN-INPUT-FILE THRU OPEN-INPUT-EXIT
006710         MOVE -1  TO I-O-SEND-CODE                                2016547
006800         CALL 'IMACTMS' USING I-O-CONTROL-AREA
006900                              COMPRESSED-MASTER-AREA
007000         IF NOT I-O-88-END-OF-FILE
007100             IF CMA-EXC-CODE NOT EQUAL '0'
007200                 CALL 'IMMSEP' USING MASTER-AREA                  0903291
007300                                       COMPRESSED-MASTER-AREA
007400                 GO TO GOBACK-PARA
007500             ELSE
007600                 MOVE CMA-HEADER-AREA TO DDA-ACCT-MASTER
007700                 MOVE CMA-LENGTH      TO WMS-LENGTH
007800                 MOVE LOW-VALUES      TO WMS-BIN0
007900                 GO TO GOBACK-PARA
008000         ELSE
008100             GO TO GOBACK-PARA.
008200     SKIP1
008300     IF I-O-88-REWRITE OR I-O-88-INSERT
008400         PERFORM OPEN-OUTPUT-FILE THRU OPEN-OUTPUT-EXIT
008500         MOVE 'L' TO I-O-CONTROL-OPERATOR
008600         IF WMS-EXC-CODE NOT EQUAL '0'
008700             CALL 'IMMSCP' USING MASTER-AREA                      0903291
008710             MOVE -1  TO I-O-SEND-CODE                            2016547
008800             CALL 'IMACTMS' USING I-O-CONTROL-AREA
008900                                  MASTER-AREA
009000             GO TO GOBACK-PARA
009100         ELSE
009110             MOVE -1  TO I-O-SEND-CODE                            2016547
009200             CALL 'IMACTMS' USING I-O-CONTROL-AREA
009300                                  MASTER-AREA
009400             GO TO GOBACK-PARA.
009500     SKIP1
009600     IF I-O-88-DELETE
009700         GO TO GOBACK-PARA.
009800     SKIP1
009900     IF I-O-88-REREAD
010000         IF  CMA-EXC-CODE NOT EQUAL '0'                           IM003
010100             CALL 'IMMSEP' USING MASTER-AREA                      0903291
010200                                   COMPRESSED-MASTER-AREA
010300             GO TO GOBACK-PARA
010400         ELSE
010500             MOVE CMA-HEADER-AREA TO DDA-ACCT-MASTER
010600             MOVE CMA-LENGTH      TO WMS-LENGTH
010700             MOVE LOW-VALUES      TO WMS-BIN0
010800             GO TO GOBACK-PARA.
010900     SKIP1
011000     IF  I-O-CONTROL-OPERATOR EQUAL '1'                           IM003
011100         PERFORM INSERT-EXCEPTION-MASTER THRU INSERT-EXC-EXIT
011200         GO TO GOBACK-PARA.
011300     SKIP1
011400     IF  I-O-88-OPEN-SEQ                                          IM003
011500         GO TO GOBACK-PARA.
011600     SKIP1
011700     IF  I-O-88-CLOSE                                             IM003
011800         IF  W-FIRST-EXC-MAST-SWITCH EQUAL 'F'                    IM003
011900             MOVE 'O' TO I-O-CONTROL-ACCESS                       IM003
012000             MOVE 'N' TO W-FIRST-EXC-MAST-SWITCH
012010             MOVE -1  TO I-O-SEND-CODE                            2016547
012100             CALL 'IMEXCMV' USING I-O-CONTROL-AREA                IM003
012200                                  MASTER-AREA.
012300     IF  I-O-88-CLOSE                                             IM003
012400         IF  W-FIRST-SEQ-MAST-INPUT-SWT EQUAL 'F'                 IM003
012500             MOVE 'I' TO I-O-CONTROL-ACCESS
012510             MOVE -1  TO I-O-SEND-CODE                            2016547
012600             CALL 'IMACTMS' USING I-O-CONTROL-AREA
012700                                  MASTER-AREA
012800             MOVE 'N' TO W-FIRST-SEQ-MAST-INPUT-SWT.
012900     IF  I-O-88-CLOSE                                             IM003
013000         IF  W-FIRST-SEQ-MAST-OUTPUT-SWT EQUAL 'F'                IM003
013100             MOVE 'O' TO I-O-CONTROL-ACCESS
013110             MOVE -1  TO I-O-SEND-CODE                            2016547
013200             CALL 'IMACTMS' USING I-O-CONTROL-AREA
013300                                  MASTER-AREA
013400             MOVE 'N' TO W-FIRST-SEQ-MAST-OUTPUT-SWT.
013500     IF  I-O-88-CLOSE                                             IM003
013600         GO TO GOBACK-PARA.
013700     GO TO CONTROL-ERROR-PARA.
013800     EJECT
013900 OPEN-INPUT-FILE.
014000     IF  W-FIRST-SEQ-MAST-INPUT-SWT EQUAL 'N'                     IM003
014100         MOVE I-O-CONTROL-OPERATOR TO W-HOLD-OPERATOR
014200         MOVE 'O' TO I-O-CONTROL-OPERATOR
014300         MOVE 'I' TO I-O-CONTROL-ACCESS
014310         MOVE -1  TO I-O-SEND-CODE                                2016547
014400         CALL 'IMACTMS' USING I-O-CONTROL-AREA
014500                              MASTER-AREA
014600         MOVE 'F' TO W-FIRST-SEQ-MAST-INPUT-SWT
014700         MOVE W-HOLD-OPERATOR TO I-O-CONTROL-OPERATOR.
014800 OPEN-INPUT-EXIT. EXIT.
014900     SKIP3                                                        IM003
015000 OPEN-OUTPUT-FILE.
015100     IF  W-FIRST-SEQ-MAST-OUTPUT-SWT EQUAL 'N'                    IM003
015200         MOVE I-O-CONTROL-OPERATOR TO W-HOLD-OPERATOR
015300         MOVE 'O' TO I-O-CONTROL-OPERATOR
015400         MOVE 'O' TO I-O-CONTROL-ACCESS
015410         MOVE -1  TO I-O-SEND-CODE                                2016547
015500         CALL 'IMACTMS' USING I-O-CONTROL-AREA
015600                              MASTER-AREA
015700         MOVE 'F' TO W-FIRST-SEQ-MAST-OUTPUT-SWT
015800         MOVE W-HOLD-OPERATOR TO I-O-CONTROL-OPERATOR.
015900 OPEN-OUTPUT-EXIT. EXIT.
016000     EJECT
016100 INSERT-EXCEPTION-MASTER.
016200*
016300*    ASSUMING THAT THE MASTER HAS ALREADY BEEN COMPRESSED
016400*
016500     IF  W-FIRST-EXC-MAST-SWITCH EQUAL 'N'                        IM003
016600         MOVE 'F' TO W-FIRST-EXC-MAST-SWITCH
016700         MOVE 'P' TO I-O-CONTROL-OPERATOR
016800         MOVE 'O' TO I-O-CONTROL-ACCESS                           IM003
016810         MOVE -1  TO I-O-SEND-CODE                                2016547
016900         CALL 'IMEXCMV' USING I-O-CONTROL-AREA                    IM003
017000                              MASTER-AREA
017100         IF  I-O-88-NORMAL-RET                                    IM003
017200             MOVE EXC-FILE-HAS-RECORDS-MSG TO
017300                  SIMESS-OPTIONAL-MESSAGE                         IM003
017400             MOVE 501 TO SIMESS-MESS-NO
017500             MOVE PROGRAM-NAME TO SIMESS-PROGRAM
017600             CALL 'SIMESS' USING SIMESS-AREA.
017700     MOVE 'L' TO I-O-CONTROL-OPERATOR.
017710     MOVE -1  TO I-O-SEND-CODE.                                   2016547
017800     CALL 'IMEXCMV' USING I-O-CONTROL-AREA                        IM003
017900                          MASTER-AREA.
018000 INSERT-EXC-EXIT. EXIT.
018100     EJECT
018200 VSAM-LOGIC.
018300     IF  I-O-88-SEQ-READ                                          IM003
018310         MOVE -1  TO I-O-SEND-CODE                                2016547
018400         CALL 'IMACTMV' USING I-O-CONTROL-AREA
018500                              COMPRESSED-MASTER-AREA
018600         IF  NOT I-O-88-END-OF-FILE                               IM003
018700             MOVE CMA-CONTROL-KEY TO W-HOLD-VSAM-KEY
018800             IF  CMA-EXC-CODE NOT EQUAL '0'                       IM003
018900                 CALL 'IMMSEP' USING MASTER-AREA                  0903291
019000                                       COMPRESSED-MASTER-AREA
019100                 GO TO GOBACK-PARA
019200             ELSE
019300                 MOVE CMA-HEADER-AREA TO DDA-ACCT-MASTER
019400                 MOVE CMA-LENGTH      TO WMS-LENGTH
019500                 MOVE LOW-VALUES      TO WMS-BIN0
019600                 GO TO GOBACK-PARA
019700         ELSE
019800             GO TO GOBACK-PARA.
019900     SKIP1
020000     IF  I-O-88-REWRITE OR I-O-88-INSERT                          IM003
020100         IF WMS-EXC-CODE NOT EQUAL '0'
020200             CALL 'IMMSCP' USING MASTER-AREA                      0903291
020210             MOVE -1  TO I-O-SEND-CODE                            2016547
020300             CALL 'IMACTMV' USING I-O-CONTROL-AREA
020400                                  MASTER-AREA
020500             GO TO GOBACK-PARA
020600         ELSE
020610             MOVE -1  TO I-O-SEND-CODE                            2016547
020700             CALL 'IMACTMV' USING I-O-CONTROL-AREA
020800                                  MASTER-AREA
020900             GO TO GOBACK-PARA.
021000     SKIP1
021100     IF  I-O-88-RBA-DIRRET                                        IM005
021190         MOVE WMS-CONTROL-KEY TO CMA-CONTROL-KEY                  IM005
021195         MOVE -1  TO I-O-SEND-CODE                                2016547
021280         CALL 'IMACTMV' USING I-O-CONTROL-AREA                    IM005
021370                              COMPRESSED-MASTER-AREA              IM005
021460         IF  WMS-CONTROL-KEY EQUAL CMA-CONTROL-KEY                IM005
021550             MOVE WMS-CONTROL-KEY TO W-HOLD-VSAM-KEY              IM005
021640             IF  CMA-EXC-CODE NOT EQUAL '0'                       IM005
021730                 CALL 'IMMSEP' USING MASTER-AREA                  0903291
021820                                       COMPRESSED-MASTER-AREA     IM005
021910                 GO TO GOBACK-PARA                                IM005
022000             ELSE                                                 IM005
022090                 MOVE CMA-HEADER-AREA TO DDA-ACCT-MASTER          IM005
022180                 MOVE CMA-LENGTH      TO WMS-LENGTH               IM005
022270                 MOVE LOW-VALUES      TO WMS-BIN0                 IM005
022360                 GO TO GOBACK-PARA                                IM005
022430         ELSE                                                     IM005
022500             GO TO GOBACK-PARA.                                   IM005
022700     IF  I-O-88-KEYED-DIRECT                                      IM003
022800         MOVE WMS-CONTROL-KEY TO CMA-CONTROL-KEY                  IM003
022810         MOVE -1  TO I-O-SEND-CODE                                2016547
022900         CALL 'IMACTMV' USING I-O-CONTROL-AREA                    IM003
023000                              COMPRESSED-MASTER-AREA              IM003
023100         IF  NOT I-O-88-NOT-FOUND                                 IM003
023200             MOVE WMS-CONTROL-KEY TO W-HOLD-VSAM-KEY              IM003
023300             IF  CMA-EXC-CODE NOT EQUAL '0'                       IM003
023400                 CALL 'IMMSEP' USING MASTER-AREA                  0903291
023500                                       COMPRESSED-MASTER-AREA     IM003
023600                 GO TO GOBACK-PARA                                IM003
023700             ELSE                                                 IM003
023800                 MOVE CMA-HEADER-AREA TO DDA-ACCT-MASTER          IM003
023900                 MOVE CMA-LENGTH      TO WMS-LENGTH               IM003
024000                 MOVE LOW-VALUES      TO WMS-BIN0                 IM003
024100                 GO TO GOBACK-PARA                                IM003
024200         ELSE                                                     IM003
024300             GO TO GOBACK-PARA.                                   IM003
024400     SKIP1                                                        IM003
024500     IF I-O-CONTROL-OPERATOR EQUAL '1'                            IM003
024600         PERFORM INSERT-EXCEPTION-MASTER THRU INSERT-EXC-EXIT     IM003
024700         GO TO GOBACK-PARA.                                       IM003
024800     SKIP1                                                        IM003
024900     IF I-O-88-DELETE                                             IM003
024910         MOVE -1  TO I-O-SEND-CODE                                2016547
025000         CALL 'IMACTMV' USING I-O-CONTROL-AREA                    IM003
025100                              MASTER-AREA                         IM003
025200         GO TO GOBACK-PARA.                                       IM003
025300     SKIP1                                                        IM003
025400     IF I-O-88-REREAD                                             IM003
025500         MOVE W-HOLD-VSAM-KEY TO CMA-CONTROL-KEY                  IM003
025600         MOVE 'S' TO I-O-CONTROL-OPERATOR                         IM003
025610         MOVE -1  TO I-O-SEND-CODE                                2016547
025700         CALL 'IMACTMV' USING I-O-CONTROL-AREA                    IM003
025800                              COMPRESSED-MASTER-AREA              IM003
025900         IF I-O-88-NORMAL-RET                                     IM003
026000             MOVE 'R' TO I-O-CONTROL-OPERATOR                     IM003
026010             MOVE -1  TO I-O-SEND-CODE                            2016547
026100             CALL 'IMACTMV' USING I-O-CONTROL-AREA                IM003
026200                                  COMPRESSED-MASTER-AREA          IM003
026300             IF CMA-EXC-CODE NOT EQUAL '0'                        IM003
026400                 CALL 'IMMSEP' USING MASTER-AREA                  0903291
026500                                       COMPRESSED-MASTER-AREA     IM003
026600                 GO TO GOBACK-PARA                                IM003
026700             ELSE                                                 IM003
026800                 MOVE CMA-HEADER-AREA TO DDA-ACCT-MASTER          IM003
026900                 MOVE CMA-LENGTH      TO WMS-LENGTH               IM003
027000                 MOVE LOW-VALUES      TO WMS-BIN0                 IM003
027100                 GO TO GOBACK-PARA                                IM003
027200         ELSE                                                     IM003
027300             MOVE PROGRAM-NAME TO SIMESS-PROGRAM                  IM003
027400             MOVE 501          TO SIMESS-MESS-NO                  IM003
027500             MOVE 'RE-READ FOR NON-EXISTENT VSAM RECORD' TO       IM003
027600                 SIMESS-OPTIONAL-MESSAGE                          IM003
027700             CALL 'SIMESS' USING SIMESS-AREA.                     IM003
027800     SKIP1                                                        IM003
027900     IF I-O-88-OPEN-KSDS OR I-O-88-OPEN-ESDS                      IM003
027910         MOVE -1  TO I-O-SEND-CODE                                2016547
028000         CALL 'IMACTMV' USING I-O-CONTROL-AREA                    IM003
028100                              COMPRESSED-MASTER-AREA              IM003
028200         GO TO GOBACK-PARA.                                       IM005
028800     IF I-O-88-CLOSE                                              IM003
028810         MOVE -1  TO I-O-SEND-CODE                                2016547
028900         CALL 'IMACTMV' USING I-O-CONTROL-AREA                    IM003
029000                              COMPRESSED-MASTER-AREA.             IM005
029400     IF I-O-88-CLOSE                                              IM003
029500         IF W-FIRST-EXC-MAST-SWITCH EQUAL 'F'                     IM003
029600             MOVE 'O' TO I-O-CONTROL-ACCESS                       IM003
029700             MOVE 'N' TO W-FIRST-EXC-MAST-SWITCH                  IM003
029710             MOVE -1  TO I-O-SEND-CODE                            2016547
029800             CALL 'IMEXCMV' USING I-O-CONTROL-AREA                IM003
029900                                  MASTER-AREA                     IM003
030000             GO TO GOBACK-PARA                                    IM003
030100         ELSE                                                     IM003
030200             GO TO GOBACK-PARA.                                   IM003
030300     GO TO CONTROL-ERROR-PARA.                                    IM003
030400     EJECT                                                        IM003
030500 CONTROL-ERROR-PARA.                                              IM003
030600     MOVE PROGRAM-NAME TO SIMESS-PROGRAM.                         IM003
030700     MOVE 501          TO SIMESS-MESS-NO.                         IM003
030800     MOVE I-O-CONTROL-OPERATOR TO NON-SUPPORT-LIT.                IM003
030900     MOVE NON-SUPPORT-MSG TO SIMESS-OPTIONAL-MESSAGE.             IM003
031000     CALL 'SIMESS' USING SIMESS-AREA.                             IM003
031100 GOBACK-PARA.                                                     IM003
031200     GOBACK.                                                      IM003
032200 SQVS-LOGIC.                                                      IM004
033200     IF  I-O-88-SEQ-READ                                          IM004
033210         MOVE -1  TO I-O-SEND-CODE                                2016547
034200         CALL 'IMACTMS' USING I-O-CONTROL-AREA                    IM004
035200                              COMPRESSED-MASTER-AREA              IM004
036200         IF  NOT I-O-88-END-OF-FILE                               IM004
037200             IF  CMA-EXC-CODE NOT EQUAL '0'                       IM004
038200                 CALL 'IMMSEP' USING MASTER-AREA                  0903291
039200                                       COMPRESSED-MASTER-AREA     IM004
040200                 GO TO GOBACK-PARA                                IM004
041200             ELSE                                                 IM004
042200                 MOVE CMA-HEADER-AREA TO DDA-ACCT-MASTER          IM004
043200                 MOVE CMA-LENGTH      TO WMS-LENGTH               IM004
044200                 MOVE LOW-VALUES      TO WMS-BIN0                 IM004
045200                 GO TO GOBACK-PARA                                IM004
046200         ELSE                                                     IM004
047200             GO TO GOBACK-PARA.                                   IM004
048200     SKIP1                                                        IM004
049200     IF  I-O-88-REWRITE OR I-O-88-INSERT                          IM004
050200         MOVE 'L' TO I-O-CONTROL-OPERATOR                         IM004
051200         IF WMS-EXC-CODE NOT EQUAL '0'                            IM004
052200             CALL 'IMMSCP' USING MASTER-AREA                      0903291
052210             MOVE -1  TO I-O-SEND-CODE                            2016547
053200             CALL 'IMACTMV' USING I-O-CONTROL-AREA                IM004
054200                                  MASTER-AREA                     IM004
055200             GO TO GOBACK-PARA                                    IM004
056200         ELSE                                                     IM004
056210             MOVE -1  TO I-O-SEND-CODE                            2016547
057200             CALL 'IMACTMV' USING I-O-CONTROL-AREA                IM004
058200                                  MASTER-AREA                     IM004
059200             GO TO GOBACK-PARA.                                   IM004
060200     SKIP1                                                        IM004
061200     IF  I-O-CONTROL-OPERATOR EQUAL '1'                           IM004
062200         PERFORM INSERT-EXCEPTION-MASTER THRU INSERT-EXC-EXIT     IM004
063200         GO TO GOBACK-PARA.                                       IM004
064200     SKIP1                                                        IM004
065200     IF I-O-88-DELETE                                             IM004
066200         GO TO GOBACK-PARA.                                       IM004
067200     SKIP1                                                        IM004
068200     IF I-O-88-REREAD                                             IM004
069200         IF  CMA-EXC-CODE NOT EQUAL '0'                           IM004
070200             CALL 'IMMSEP' USING MASTER-AREA                      0903291
071200                                   COMPRESSED-MASTER-AREA         IM004
072200             GO TO GOBACK-PARA                                    IM004
073200         ELSE                                                     IM004
074200             MOVE CMA-HEADER-AREA TO DDA-ACCT-MASTER              IM004
075200             MOVE CMA-LENGTH      TO WMS-LENGTH                   IM004
076200             MOVE LOW-VALUES      TO WMS-BIN0                     IM004
077200             GO TO GOBACK-PARA.                                   IM004
078200     SKIP1                                                        IM004
079200     IF  I-O-88-OPEN-SEQ                                          IM004
080200         MOVE 'O' TO I-O-CONTROL-OPERATOR                         IM004
081200         MOVE 'I' TO I-O-CONTROL-ACCESS                           IM004
081210         MOVE -1  TO I-O-SEND-CODE                                2016547
082200         CALL 'IMACTMS' USING I-O-CONTROL-AREA                    IM004
083200                              MASTER-AREA                         IM004
084200         MOVE 'O' TO I-O-CONTROL-OPERATOR                         IM004
085200         MOVE 'O' TO I-O-CONTROL-ACCESS                           IM004
085210         MOVE -1  TO I-O-SEND-CODE                                2016547
086200         CALL 'IMACTMV' USING I-O-CONTROL-AREA                    IM004
087200                              MASTER-AREA                         IM004
088200         GO TO GOBACK-PARA.                                       IM004
089200     SKIP1                                                        IM004
090200     IF  I-O-88-CLOSE                                             IM004
091200         MOVE 'I' TO I-O-CONTROL-ACCESS                           IM004
091210         MOVE -1  TO I-O-SEND-CODE                                2016547
092200         CALL 'IMACTMS' USING I-O-CONTROL-AREA                    IM004
093200                              MASTER-AREA                         IM004
094200         MOVE 'O' TO I-O-CONTROL-ACCESS                           IM004
094210         MOVE -1  TO I-O-SEND-CODE                                2016547
095200         CALL 'IMACTMV' USING I-O-CONTROL-AREA                    IM004
096200                              COMPRESSED-MASTER-AREA              IM004
097200         IF  W-FIRST-EXC-MAST-SWITCH EQUAL 'F'                    IM004
098200             MOVE 'O' TO I-O-CONTROL-ACCESS                       IM004
099200             MOVE 'N' TO W-FIRST-EXC-MAST-SWITCH                  IM004
099210             MOVE -1  TO I-O-SEND-CODE                            2016547
100200             CALL 'IMEXCMV' USING I-O-CONTROL-AREA                IM004
101200                                  MASTER-AREA.                    IM004
102200     GO TO GOBACK-PARA.                                           IM004
