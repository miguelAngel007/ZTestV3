*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*
000200*--------------------------------------------------------------*
000300*    REPORT EXCEPTIONS FOR EXTERNAL INVESTMENTS - DAILY        *
000400*--------------------------------------------------------------*
000500 R9100.
000600*--------------------------------------------------------------*
000700*    QI   - INCOMPLETE EXTERNAL INVESTMENT LINK                *
000800*--------------------------------------------------------------*
000900*
001000***  CREATE INCOMPLETE LINK REPORT EXCEPTION QI DAILY IF
001100***  ADDED THROUGH IMPACS ONLY OR EXTERNAL INVESTMENT APPL ONLY
001200*
001300     IF  (WMS-XINV-LINK-IND   EQUAL  'A' OR 'I')
001400         MOVE 'QI'               TO  IMEX-CODE-1
001500         MOVE '15'               TO  IMEX-REC-NO
001600         MOVE WMS-XINV-LINK-IND  TO  WXI-FM-NEW
001700         MOVE '2005'             TO  WXI-FM-FIELD
001800         MOVE DMT-TRAN-TRAILER (2005)
001900                                 TO  WXI-FM-MAINT
002000         MOVE SPACES             TO  WXI-FM-REQ-BY
002100         MOVE EXT-INV-LINK-IND(SIWS-LANG-SUB)                     0266741
002200                                 TO  WXI-FM-DESC
002300         MOVE WXI-FILE-MAINT     TO  EX15-FM-TRAN
002400         MOVE WXI-LINK-IND-IN    TO  WXI-FM-OLD
002500                                     EX15-MST-OLD-DATA
002600         MOVE WMS-DATE-LAST-MAINT
002700                                 TO  EX15-DATE-LAST-MAINT
002800         PERFORM WRITE-EX-MST  THRU  WRITE-EX-EXIT.
002900*
003000*--------------------------------------------------------------*
003100*    QL   - EXTERNAL INVESTMENT LINKED ACCOUNT                 *
003200*--------------------------------------------------------------*
003300     IF  WMS-XINV-LINK-IND    EQUAL  'L'
003400         MOVE '17'               TO  IMEX-REC-NO
003500         MOVE 'QL'               TO  IMEX-CODE-1
003600         PERFORM WRITE-EX-MST  THRU  WRITE-EX-EXIT.
003700*
003800*--------------------------------------------------------------*
003900*    QP   - EXTERNAL INVESTMENT BANK POSITION ACCOUNT          *
004000*--------------------------------------------------------------*
004100     IF  WMS-XINV-LINK-IND    EQUAL  'P'
004200         MOVE '17'               TO  IMEX-REC-NO
004300         MOVE 'QP'               TO  IMEX-CODE-1
004400         PERFORM WRITE-EX-MST  THRU  WRITE-EX-EXIT.
004500*
004600     GO TO R9199-EXIT.
004700*
004800*--------------------------------------------------------------*
004900*    REPORT EXCEPTIONS FOR EXTERNAL INVESTMENTS - PERFORMED    *
005000*--------------------------------------------------------------*
005100*
005200*--------------------------------------------------------------*
005300*    QN   - POSTED EXTERNAL ADJUSTMENT TO EXTERNAL INVESTMENT  *
005400*           BALANCE (TRAN 60/61 TO FIELD NUMBER 52)            *
005500*--------------------------------------------------------------*
005600 R9110-QN.
005700     MOVE '04'                   TO  IMEX-REC-NO.
005800     MOVE 'QN'                   TO  IMEX-CODE-1.
005900     MOVE TR-MT-AMT              TO  EX04-AMT.
006000     MOVE TR-CLASS               TO  EX04-TR-CODE.
006100     MOVE TR-MT-FLD              TO  EX04-FIELD-P.
006200     IF  TR-MT-REF-NO NUMERIC
006300         MOVE TR-MT-REF-NO       TO  IM-EXC-REF-NUM
006400     ELSE
006500         MOVE ZEROES             TO  IM-EXC-REF-NUM.
006600     MOVE TR-SOURCE              TO  EX04-SOURCE.
006700     MOVE TR-BATCH               TO  EX04-BATCH.
006800     MOVE TR-SEQ                 TO  EX04-SEQ.
006900     MOVE WMS-DATE-LAST-MAINT    TO  EX04-DATE-LAST-MAINT.
007000*
007100     PERFORM WRITE-EXCEPT      THRU  WRITE-EX-EXIT.
007200     GO TO R9199-EXIT.
007300*
007400*--------------------------------------------------------------*
007500*    Q5   - POSTED EXTERNAL INVESTMENT MONETARY TRANSACTION    *
007600*--------------------------------------------------------------*
007700 R9120-Q5.
007800     MOVE '13'                   TO  IMEX-REC-NO.
007900     MOVE 'Q5'                   TO  IMEX-CODE-1.
008000     MOVE TR-TRAN80-A            TO  IMEX-REC-13A.
008100     MOVE TR-AMOUNT              TO  EX13-AMT.
008200     MOVE TR-USER-CODE           TO  EX13-USER-CODE.
008300     MOVE TR-TRAN-TYPE           TO  EX13-TYPE.
008400     MOVE TR-TRAN80-B            TO  IMEX-REC-13B.
008500     MOVE DATE-LAST-ACTIVE       TO  EX13-DATE-LAST-ACTIVE.
008600     IF  TR-UNIV-DESC-FLAG    EQUAL  '1'
008700     OR  TR-CURRENCY-FLAG     EQUAL  '1'
008800         MOVE '1'                TO  SRC-INFO-LENGTH-FLG
008900         MOVE '22'               TO  IMEX-REC-NO
009000         SUBTRACT +273         FROM  TF-LENGTH
009100                             GIVING  EX-22
009200         ADD EX-13               TO  EX-22
009300         IF  TR-CURRENCY-FLAG EQUAL  '1'
009400             MOVE TR-CURRENCY-INFO
009500                                 TO  EX22-CURRENCY-INFO
009600         ELSE
009700             MOVE SPACES         TO  EX22-CURRENCY-INFO.
009800     IF  TR-UNIV-DESC-FLAG    EQUAL  '1'
009900         MOVE TR-UNIV-DESC-TRAN  TO  EX22-RMDR.
010000     IF  ACCT-NOT-FOUND       EQUAL  '1'
010100         MOVE ZERO               TO  EX13-AMT-AVAIL
010200     ELSE
010300         MOVE CUR-BAL            TO  EX13-AMT-AVAIL
010400         IF  SAVINGS-PRESENT  EQUAL  '1'
010500             ADD WMS-INT-CUR-BAL TO  EX13-AMT-AVAIL.
010600*
010700     PERFORM WRITE-EXCEPT      THRU  WRITE-EX-EXIT.
010800     GO TO R9199-EXIT.
010900*
011000*--------------------------------------------------------------*
011100*    Q7   - UNPOSTED EXTERNAL INVESTMENT MONETARY TRANSACTION  *
011200*--------------------------------------------------------------*
011300 R9130-Q7.
011400*
011500***  SAVE ORIGINAL EXCEPTION'S INFORMATION FOR LATER PROCESSING
011600*
011700     MOVE EX-INFO                TO  WXI-EX-INFO.
011800*
011900     MOVE '13'                   TO  RECNO.
012000     MOVE 'Q7'                   TO  EXCODE-1.
012100     MOVE TR-TRAN80-A            TO  IMEX-REC-13A.
012200     MOVE TR-AMOUNT              TO  EX13-AMT.
012300     MOVE TR-USER-CODE           TO  EX13-USER-CODE.
012400     MOVE TR-TRAN-TYPE           TO  EX13-TYPE.
012500     MOVE TR-TRAN80-B            TO  IMEX-REC-13B.
012600     MOVE DATE-LAST-ACTIVE       TO  EX13-DATE-LAST-ACTIVE.
012700     IF  TR-UNIV-DESC-FLAG    EQUAL  '1'
012800     OR  TR-CURRENCY-FLAG     EQUAL  '1'
012900         MOVE '22'               TO  IMEX-REC-NO
013000         SUBTRACT +273         FROM  TF-LENGTH
013100                             GIVING  EX-22
013200         ADD EX-13               TO  EX-22
013300         IF  TR-CURRENCY-FLAG EQUAL  '1'
013400             MOVE TR-CURRENCY-INFO
013500                                 TO  EX22-CURRENCY-INFO
013600         ELSE
013700             MOVE SPACES         TO  EX22-CURRENCY-INFO.
013800     IF  TR-UNIV-DESC-FLAG    EQUAL  '1'
013900         MOVE TR-UNIV-DESC-TRAN  TO  EX22-RMDR.
014000     IF  ACCT-NOT-FOUND       EQUAL  '1'
014100         MOVE ZERO               TO  EX13-AMT-AVAIL
014200     ELSE
014300         MOVE CUR-BAL            TO  EX13-AMT-AVAIL
014400         IF  SAVINGS-PRESENT  EQUAL  '1'
014500             ADD WMS-INT-CUR-BAL TO  EX13-AMT-AVAIL.
014600*
014700     PERFORM UNPOST-EXCEPT     THRU  WRITE-EX-EXIT.
014800*
014900***  RESTORE ORIGINAL EXCEPTION'S INFORMATION FOR PROCESSING
015000*
015100     MOVE WXI-EX-INFO            TO  EX-INFO.
015200     MOVE SPACES                 TO  WXI-EX-INFO.
015300     GO TO R9199-EXIT.
015400*
015500 R9199-EXIT.
015600     EXIT.
015700*
