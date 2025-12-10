*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*
000200*
000300*--------------------------------------------------------------*
000400*    IMPACS EXTENDED FLOAT FILE READ COPYBOOK                  *
000500*--------------------------------------------------------------*
000600*
000700*
000800 C0700.
000900     MOVE 'F'                    TO AFF-FLAG.
001000     MOVE +0                     TO AFF-MAX-ENTRY.
001100*--------------------------------------------------------------*
001200*    BYPASS READING THE EXTENDED FLOAT FILE IF                 *
001300*    - EXTENDED FLOAT OPTION IS NOT USED AT THE BCR LEVEL      *
001400*    - ACCOUNT HEADER RECORD      (WMS-EXC-CODE = 0)           *
001500*    - NEW ACCOUNT                (SKIP-READ-SW = 1)           *
001600*    - ONLINE NEW ACCOUNT                                      *
001700*    - INITIAL READ               (FIRST-READ-SW = 1)          *
001800*      (NO BCR HAS BEEN READ YET)                              *
001900*    - ACCOUNT DOES NOT HAVE AN EXTENDED FLOAT RECORD          *
002000*--------------------------------------------------------------*
002100     IF  WBC-EXTEND-FLOAT-OPT EQUAL '0'
002200     OR  WMS-EXC-CODE         EQUAL '0'
002300     OR  SKIP-READ-SW         EQUAL '1'
002400     OR  FIRST-READ-SW        EQUAL '1'
002500     OR  MST-EOF              EQUAL HIGH-VALUES
002600     OR (SI-88-ENVIRONMENT-ONLINE
002700     AND IM-MEMO-OL-NEW-ACCT)
002800         GO TO C0750-EXIT.
002900
003000     IF  MST-1            NOT EQUAL HIGH-VALUES
003100         IF  MST-1        NOT EQUAL WBC1-CONTROL-1
003200             MOVE '1'            TO NEW-BK-NO-TRN-FLAG
003300             GO TO C0750-EXIT
003400         ELSE
003500             IF  ((MST-2     NOT EQUAL WBC1-CONTROL-2)
003600             AND (WBC-CTL2-FLAG  EQUAL '2'))
003700             OR  ((MST-3     NOT EQUAL WBC1-CONTROL-3)
003800             AND (WBC-CTL3-FLAG  EQUAL '2'))
003900                 MOVE '1'        TO NEW-BK-NO-TRN-FLAG
004000                 GO TO C0750-EXIT.
004100
004200*--------------------------------------------------------------*
004300*    INITIALIZE EXTENDED FLOAT RECORDS ON THE ACCOUNTS WHEN    *
004400*    THE BCR EXTEND FLOAT OPTION IS '2' - INITIAL BUILD.       *
004500*--------------------------------------------------------------*
004600     IF  WBC-EXTEND-FLOAT-OPT EQUAL '2'
004700         MOVE '0'                TO WMS-EXTEND-FLOAT
004800         GO TO C0750-EXIT.
004900
005000*--------------------------------------------------------------*
005100*    IF THIS ACCOUNT IS AN INTER-BRANCH TRANSFER ON A NON-     *
005200*    PROCESS BANK THEN THE EXTENDED FLOAT RECORDS WILL BE      *
005300*    REWRITTEN WITHOUT ERROR CHECKING.                         *
005400*--------------------------------------------------------------*
005500     IF  WBC-PROCESS-FLAG NOT EQUAL '1'
005600         IF  WMS-IBT-FLAG     EQUAL '3'
005700             GO TO C0710-FF-CLEAR.
005800
005900*--------------------------------------------------------------*
006000*    IF THE BANK USES THE EXTENDED FLOAT FILE AND THE ACCOUNT  *
006100*    HAS AN EXTENDED FLOAT RECORD ON FILE                      *
006200*    CLEAR THE WORK AREA AND READ THE EXTENDED FLOAT RECORD    *
006300*--------------------------------------------------------------*
006400     IF  WMS-EXTEND-FLOAT NOT EQUAL '1'
006500         GO TO C0750-EXIT.
006600
006700 C0710-FF-CLEAR.
006800*--------------------------------------------------------------*
006900*    CLEAR THE EXTENDED FLOAT FILE WORK AREAS                  *
007000*--------------------------------------------------------------*
007100     MOVE CLEAR-EXTENDED-FLOAT-REC
007200                                 TO WFF-EXTENDED-FLOAT-REC.
007300
007400 C0720-FF-READ-RTN.
007500*--------------------------------------------------------------*
007600*    AFF-SKIP-READ CONTROLS READING THE EXTENDED FLOAT FILE    *
007700*      0 = NORMAL SEQUENTIAL READ                              *
007800*      1 = READ PREVIOUSLY READ RECORD                         *
007900*      E = END OF FILE                                         *
008000*--------------------------------------------------------------*
008100     IF  AFF-SKIP-READ        EQUAL 'E'
008200         GO TO C0730-FF-CHK-ERRORS.
008300
008400     IF  AFF-SKIP-READ    NOT EQUAL '1'
008500         MOVE CLEAR-EXTENDED-FLOAT-REC
008600                                 TO EXTENDED-FLOAT-REC
008700         MOVE 'I'                TO I-O-CONTROL-ACCESS
008800         MOVE 'R'                TO I-O-CONTROL-OPERATOR
008900         CALL 'IMAFFMS'       USING I-O-CONTROL-AREA
009000                                    EXTENDED-FLOAT-REC
009100         IF  I-O-88-END-OF-FILE
009200             MOVE HIGH-VALUES    TO FF-KEY
009300             MOVE 'E'            TO AFF-SKIP-READ
009400             GO TO C0730-FF-CHK-ERRORS
009500         ELSE
009600             MOVE '0'            TO AFF-SKIP-READ
009700     ELSE
009800         MOVE CLEAR-EXTENDED-FLOAT-REC
009900                                 TO EXTENDED-FLOAT-REC
010000         MOVE 'N'                TO I-O-CONTROL-OPERATOR
010100         MOVE '0'                TO AFF-SKIP-READ
010200         CALL 'IMAFFMS'       USING I-O-CONTROL-AREA
010300                                    EXTENDED-FLOAT-REC.
010400
010500     IF  FF-KEY        GREATER THAN MST-CONTROL
010600         MOVE '1'                TO AFF-SKIP-READ
010700         GO TO C0730-FF-CHK-ERRORS.
010800
010900     IF  FF-KEY           LESS THAN MST-CONTROL
011000         MOVE 501                TO SIMESS-MESS-NO
011100         MOVE FF-KEY             TO SIMESS-OPTIONAL-MESS15
011200         MOVE SIMESS-MESS15      TO SIMESS-OPTIONAL-MESSAGE
011300         CALL 'SIMESS'        USING SIMESS-AREA
011400         GO TO C0720-FF-READ-RTN.
011500
011600*--------------------------------------------------------------*
011700*    SAVE THE EXTENDED FLOAT RECORD IN THE WORK AREA           *
011800*--------------------------------------------------------------*
011900     MOVE CLEAR-EXTENDED-FLOAT-REC
012000                                 TO WFF-EXTENDED-FLOAT-REC.
012100     MOVE EXTENDED-FLOAT-REC     TO WFF-EXTENDED-FLOAT-REC.
012200     IF  FF-KEY               EQUAL MST-CONTROL
012300     AND ((FF-CTL1        NOT EQUAL WBC1-CONTROL-1)
012400     OR  (WBC-CTL2-FLAG       EQUAL '2'
012500     AND FF-CTL2          NOT EQUAL WBC1-CONTROL-2)
012600     OR  (WBC-CTL3-FLAG       EQUAL '2'
012700     AND FF-CTL3          NOT EQUAL WBC1-CONTROL-3))
012800         MOVE '1'                TO AFF-SKIP-READ
012900         GO TO C0750-EXIT.
013000
013100     GO TO C0720-FF-READ-RTN.
013200
013300 C0730-FF-CHK-ERRORS.
013400*--------------------------------------------------------------*
013500*    INITIALIZE THE WORK AREA.                                 *
013600*    REPORT ON SIMESS ANY MISSING RECORDS AS INDICATED BY THE  *
013700*    EXTENDED FLOAT FLAG ON THE ACCOUNT.                       *
013800*--------------------------------------------------------------*
013900     IF  MST-CONTROL          EQUAL HIGH-VALUES
014000         GO TO C0750-EXIT.
014100
014200     IF  WMS-EXTEND-FLOAT     EQUAL '1'
014300         MOVE 'N'                TO AFF-FLAG
014400         MOVE WFF-ENTRIES        TO AFF-MAX-ENTRY
014500         IF  WFF-KEY      NOT EQUAL MST-CONTROL
014600             MOVE MST-CONTROL    TO WFF-KEY
014700             GO TO C0740-FF-ERROR.
014800
014900*--------------------------------------------------------------*
015000*    WRITE FLOAT RECORD FOR NEW IBT ACCOUNT ON NON-PROCESS DAY *
015100*--------------------------------------------------------------*
015200     IF  WBC-PROCESS-FLAG NOT EQUAL '1'
015300         IF  WFF-KEY          EQUAL MST-CONTROL
015400         AND WMS-IBT-FLAG     EQUAL '3'
015500             MOVE WFF-EXTENDED-FLOAT-REC
015600                                 TO EXTENDED-FLOAT-REC
015700             PERFORM WRITE-EXT-FLOAT-REC
015800                THRU WRITE-EXT-FLOAT-EXIT.
015900
016000     GO TO C0750-EXIT.
016100
016200 C0740-FF-ERROR.
016300     MOVE 501                    TO SIMESS-MESS-NO.
016400     MOVE MST-CONTROL            TO SIMESS-OPTIONAL-MESS17.
016500     MOVE SIMESS-MESS17          TO SIMESS-OPTIONAL-MESSAGE.
016600     CALL 'SIMESS'            USING SIMESS-AREA.
016700
016800 C0750-EXIT.
016900     EXIT.
017000
