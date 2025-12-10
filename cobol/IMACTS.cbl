*     * FO5238 * 06/26/11 PROYECTO REBORN
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.    IMACTS.
000300
000400 ENVIRONMENT    DIVISION.
000500 DATA DIVISION.
000600 WORKING-STORAGE SECTION.
000601 77  SI-MODULE-INFO      PIC X(64)
000602     VALUE 'IMACTS    -----TSD-             02/19/09  04.58.38'.
000605*    THIS PROGRAM CONTAINS TRADE SECRETS THAT BELONG
000606*    TO FIDELITY INFORMATION SERVICES AND IS
000607*    LICENSED BY AN AGREEMENT.  ANY UNAUTHORIZED ACCESS,
000608*    USE, DUPLICATION, OR DISCLOSURE IS UNLAWFUL.
000609*    COPYRIGHT FIDELITY INFORMATION SERVICES
000610*    2009, ALL RIGHTS RESERVED.
000700 77  PROGRAM-NAME                    PIC X(8)    VALUE 'IMACTS'.
000800 77  W-HOLD-OPERATOR                 PIC X(1)    VALUE ' '.
000900 77  W-HOLD-VSAM-KEY                 PIC X(22)   VALUE ' '.
001000 77  W-FIRST-EXC-MAST-SWITCH         PIC X(1)    VALUE 'N'.
001100 77  W-FIRST-SEQ-MAST-INPUT-SWT      PIC X(1)    VALUE 'N'.
001200 77  W-FIRST-SEQ-MAST-OUTPUT-SWT     PIC X(1)    VALUE 'N'.
001300 01  INVALID-ENV-MSG.
001400     03  FILLER                      PIC X(38)   VALUE
001500         'ENVIRONMENT SEQ/VSAM CONTROL INVALID ('.
001600     03  INVALID-ENV-LIT             PIC X.
001700     03  FILLER                      PIC X(18)   VALUE
001800         ') PASSED TO MODULE'.
001900 01  NON-SUPPORT-MSG.
002000     03  FILLER                      PIC X(36)   VALUE
002100         'NON-SUPPORTED I-O-CONTROL-OPERATOR ('.
002200     03  NON-SUPPORT-LIT             PIC X.
002300     03  FILLER                      PIC X(18)   VALUE
002400         ') PASSED TO MODULE'.
002500 01  EXC-FILE-HAS-RECORDS-MSG.
002600     03  FILLER                      PIC X(53)   VALUE
002700         'VSAM REPORT EXCEPTION FILE (IMACTMU) CONTAINS RECORDS'.
002800     03  FILLER                      PIC X(22)   VALUE
002900         ' - IT SHOULD BE EMPTY'.
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
004300
004400     COPY IMAWKMST.
004500
004600     COPY IMWSENVO.
004700
004800     COPY SIWSSIOB.
004900
005000 01  SIWSBL-STACK-TABLE.
005100     03  SIWSBL-STACK-NO             PIC X(2)
005200                                     OCCURS 25 TIMES.
005300     EJECT
005400 PROCEDURE DIVISION                USING I-O-CONTROL-AREA
005500                                         MASTER-AREA
005600                                         SI-ENVIRONMENT-AREA
005700                                         I-O-BASE
005800                                         SIWSBL-STACK-TABLE.
005900 DRIVER.
006000*
006100*  MOVE I-O PROTOTYPE PARAMETERS
006200*
006300     MOVE 'IMACTM'                    TO I-O-BASE-PROTOTYPE-NAME.
006400     MOVE 'IM'                        TO I-O-BASE-APPL.
006500     MOVE 'AC'                        TO I-O-BASE-FILE.
006600     MOVE 'V'                         TO I-O-BASE-PROTOTYPE-S-V
006700                                         I-O-BASE-V-S.
006800
006900     IF  (SI-88-ENVIRONMENT-SQVS
007000     AND SI-88-ENVIRONMENT-IM31)
007100         GO TO SQVS-LOGIC
007200     ELSE
007300         IF  SI-88-ENVIRONMENT-VSAM
007400             GO TO VSAM-LOGIC
007500         ELSE
007600             MOVE PROGRAM-NAME        TO SIMESS-PROGRAM
007700             MOVE 501                 TO SIMESS-MESS-NO
007800             MOVE SI-ENVIRONMENT-VSAM TO INVALID-ENV-LIT
007900             MOVE INVALID-ENV-MSG     TO SIMESS-OPTIONAL-MESSAGE
008000             CALL 'SIMESS'         USING SIMESS-AREA
008100             GO TO GOBACK-PARA.
008200
008300 VSAM-LOGIC.
008400     IF  I-O-88-SEQ-READ
008500         CALL 'SISSRTN'            USING I-O-BASE
008600                                         I-O-CONTROL-AREA
008700                                         COMPRESSED-MASTER-AREA
008800         IF  NOT I-O-88-END-OF-FILE
008900             MOVE CMA-CONTROL-KEY     TO W-HOLD-VSAM-KEY
009000             IF  CMA-EXC-CODE NOT EQUAL '0'
009100                 CALL 'IMMSEP'     USING MASTER-AREA
009200                                         COMPRESSED-MASTER-AREA
009300                 GO TO GOBACK-PARA
009400             ELSE
009500                 MOVE CMA-HEADER-AREA TO DDA-ACCT-MASTER
009600                 MOVE CMA-LENGTH      TO WMS-LENGTH
009700                 MOVE LOW-VALUES      TO WMS-BIN0
009800                 GO TO GOBACK-PARA
009900         ELSE
010000             GO TO GOBACK-PARA.
010100
010200     IF  I-O-88-RBA-DIRRET
010300         MOVE WMS-CONTROL-KEY         TO CMA-CONTROL-KEY
010400         CALL 'SISSRTN'            USING I-O-BASE
010500                                         I-O-CONTROL-AREA
010600                                         COMPRESSED-MASTER-AREA
010700         IF  WMS-CONTROL-KEY EQUAL CMA-CONTROL-KEY
010800             MOVE WMS-CONTROL-KEY     TO W-HOLD-VSAM-KEY
010900             IF  CMA-EXC-CODE NOT EQUAL '0'
011000                 CALL 'IMMSEP'     USING MASTER-AREA
011100                                         COMPRESSED-MASTER-AREA
011200                 GO TO GOBACK-PARA
011300             ELSE
011400                 MOVE CMA-HEADER-AREA TO DDA-ACCT-MASTER
011500                 MOVE CMA-LENGTH      TO WMS-LENGTH
011600                 MOVE LOW-VALUES      TO WMS-BIN0
011700                 GO TO GOBACK-PARA
011800         ELSE
011900             GO TO GOBACK-PARA.
012000
012100     IF  I-O-88-KEYED-DIRECT
012200         MOVE WMS-CONTROL-KEY         TO CMA-CONTROL-KEY
012300         CALL 'SISSRTN'            USING I-O-BASE
012400                                         I-O-CONTROL-AREA
012500                                         COMPRESSED-MASTER-AREA
012600         IF  NOT I-O-88-NOT-FOUND
012700             MOVE WMS-CONTROL-KEY     TO W-HOLD-VSAM-KEY
012800             IF  CMA-EXC-CODE NOT EQUAL '0'
012900                 CALL 'IMMSEP'     USING MASTER-AREA
013000                                         COMPRESSED-MASTER-AREA
013100                 GO TO GOBACK-PARA
013200             ELSE
013300                 MOVE CMA-HEADER-AREA TO DDA-ACCT-MASTER
013400                 MOVE CMA-LENGTH      TO WMS-LENGTH
013500                 MOVE LOW-VALUES      TO WMS-BIN0
013600                 GO TO GOBACK-PARA
013700         ELSE
013800             GO TO GOBACK-PARA.
013900
014000     IF  I-O-88-REREAD
014100         MOVE W-HOLD-VSAM-KEY         TO CMA-CONTROL-KEY
014200         MOVE 'S'                     TO I-O-CONTROL-OPERATOR
014300         CALL 'SISSRTN'            USING I-O-BASE
014400                                         I-O-CONTROL-AREA
014500                                         COMPRESSED-MASTER-AREA
014600         IF  I-O-88-NORMAL-RET
014700             MOVE 'R'                 TO I-O-CONTROL-OPERATOR
014710             MOVE -1                  TO I-O-SEND-CODE            2016547
014800             CALL 'SISSRTN'        USING I-O-BASE
014900                                         I-O-CONTROL-AREA
015000                                         COMPRESSED-MASTER-AREA
015100             IF  CMA-EXC-CODE NOT EQUAL '0'
015200                 CALL 'IMMSEP'     USING MASTER-AREA
015300                                         COMPRESSED-MASTER-AREA
015400                 GO TO GOBACK-PARA
015500             ELSE
015600                 MOVE CMA-HEADER-AREA TO DDA-ACCT-MASTER
015700                 MOVE CMA-LENGTH      TO WMS-LENGTH
015800                 MOVE LOW-VALUES      TO WMS-BIN0
015900                 GO TO GOBACK-PARA
016000         ELSE
016100             MOVE PROGRAM-NAME        TO SIMESS-PROGRAM
016200             MOVE 501                 TO SIMESS-MESS-NO
016300             MOVE 'RE-READ FOR NON-EXISTENT VSAM RECORD'
016400                                      TO SIMESS-OPTIONAL-MESSAGE
016500             CALL 'SIMESS'         USING SIMESS-AREA.
016600
016700     IF  I-O-88-OPEN-KSDS
016800     OR  I-O-88-OPEN-ESDS
016900         CALL 'SISSLOP'            USING SIWSBL-STACK-TABLE
017000                                         I-O-BASE
017100                                         I-O-CONTROL-AREA
017200                                         COMPRESSED-MASTER-AREA
017300         GO TO GOBACK-PARA.
017400
017500     IF  I-O-88-CLOSE
017600         CALL 'SISSLOP'            USING SIWSBL-STACK-TABLE
017700                                         I-O-BASE
017800                                         I-O-CONTROL-AREA
017900                                         COMPRESSED-MASTER-AREA.
018000
018100     IF  I-O-88-CLOSE
018200         GO TO GOBACK-PARA.
018300
018400 CONTROL-ERROR-PARA.
018500     MOVE PROGRAM-NAME                TO SIMESS-PROGRAM.
018600     MOVE 501                         TO SIMESS-MESS-NO.
018700     MOVE I-O-CONTROL-OPERATOR        TO NON-SUPPORT-LIT.
018800     MOVE NON-SUPPORT-MSG             TO SIMESS-OPTIONAL-MESSAGE.
018900     CALL 'SIMESS'                 USING SIMESS-AREA.
019000
019100 GOBACK-PARA.
019200     GOBACK.
019300
019400 SQVS-LOGIC.
019500     IF  I-O-88-REWRITE
019600     OR  I-O-88-INSERT
019700         MOVE 'L'                     TO I-O-CONTROL-OPERATOR
019800         IF  WMS-EXC-CODE NOT EQUAL '0'
019900             CALL 'IMMSCP'         USING MASTER-AREA
020000             CALL 'SISSRTN'        USING I-O-BASE
020100                                         I-O-CONTROL-AREA
020200                                         MASTER-AREA
020210             MOVE -1                  TO I-O-SEND-CODE            2016547
020300             CALL 'IMACTMS'        USING I-O-CONTROL-AREA
020400                                         MASTER-AREA
020500             GO TO GOBACK-PARA
020600         ELSE
020700             CALL 'SISSLOP'        USING SIWSBL-STACK-TABLE
020800                                         I-O-BASE
020900                                         I-O-CONTROL-AREA
021000                                         MASTER-AREA
021010             MOVE -1                  TO I-O-SEND-CODE            2016547
021100             CALL 'IMACTMS'        USING I-O-CONTROL-AREA
021200                                         MASTER-AREA
021300             GO TO GOBACK-PARA.
021400
021500     IF  I-O-88-DELETE
021600         GO TO GOBACK-PARA.
021700
021800     IF  I-O-88-OPEN-SEQ
021900         MOVE 'O'                     TO I-O-CONTROL-OPERATOR
022000         MOVE 'I'                     TO I-O-CONTROL-ACCESS
022010         MOVE -1                      TO I-O-SEND-CODE            2016547
022100         CALL 'IMACTMS'            USING I-O-CONTROL-AREA
022200                                         MASTER-AREA
022300         MOVE 'O'                     TO I-O-CONTROL-OPERATOR
022400         MOVE 'O'                     TO I-O-CONTROL-ACCESS
022500         CALL 'SISSLOP'            USING SIWSBL-STACK-TABLE
022600                                         I-O-BASE
022700                                         I-O-CONTROL-AREA
022800                                         MASTER-AREA
022810         MOVE -1                      TO I-O-SEND-CODE            2016547
022900         CALL 'IMACTMS'            USING I-O-CONTROL-AREA
023000                                         MASTER-AREA
023100         GO TO GOBACK-PARA.
023200
023300     IF  I-O-88-CLOSE
023400         MOVE 'I'                     TO I-O-CONTROL-ACCESS
023410         MOVE -1                      TO I-O-SEND-CODE            2016547
023500         CALL 'IMACTMS'            USING I-O-CONTROL-AREA
023600                                         MASTER-AREA
023700         MOVE 'O'                     TO I-O-CONTROL-ACCESS
023800         CALL 'SISSLOP'            USING SIWSBL-STACK-TABLE
023900                                         I-O-BASE
024000                                         I-O-CONTROL-AREA
024100                                         MASTER-AREA
024110         MOVE -1                      TO I-O-SEND-CODE            2016547
024200         CALL 'IMACTMS'            USING I-O-CONTROL-AREA
024300                                         MASTER-AREA
024400         IF  W-FIRST-EXC-MAST-SWITCH EQUAL 'F'
024500             MOVE 'O'                 TO I-O-CONTROL-ACCESS
024600             MOVE 'N'                 TO W-FIRST-EXC-MAST-SWITCH
024700             CALL 'IMEXCMV'        USING I-O-CONTROL-AREA
024800                                         MASTER-AREA.
024900     GO TO GOBACK-PARA.
