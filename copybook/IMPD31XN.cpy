*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*
000200*--------------------------------------------------------------*
000300*    EDIT ACCOUNTS LINKED TO AN EXTERNAL INVESTMENT            *
000400*--------------------------------------------------------------*
000500 R4700-EDIT-XINV.
000600     IF  WBC-XINV-INSTALLED   EQUAL  '0'                          0266741
000700         GO TO R4799-EXIT.
000800*
000900*--------------------------------------------------------------*
001000*    CHECK FOR ACCOUNT DELINKED TODAY AND REDEEM ALL FUNDS     *
001100*--------------------------------------------------------------*
001200     IF  WMS-XINV-PROCESS-IND EQUAL  '2'
001300     AND WMS-XINV-LINK-IND    EQUAL  'D'
001400     AND WXI-LINK-IND-IN      EQUAL  'L'
001500         MOVE WMS-XINV-BALANCE   TO  HOLD-AMT15
001600         MOVE 'C'                TO  WXI-DR-CR
001700         GO TO R4745-REDEEM-OR-DELINK.
001800*
001900     IF  WMS-XINV-LINK-IND NOT EQUAL 'L'
002000         GO TO R4799-EXIT.
002100*
002200*--------------------------------------------------------------*
002300*    FOR LINKED ACCOUNT, PROCESS INDICATOR MUST BE SET         *
002400*--------------------------------------------------------------*
002500     IF  WMS-XINV-PROCESS-IND EQUAL  '0'
002600         GO TO R4710-ERR.
002700*
002800*--------------------------------------------------------------*
002900*    FOR SAME DAY SWEEP, BCR MUST HAVE SAME DAY APPL INSTALLED *
003000*--------------------------------------------------------------*
003100     IF  WMS-XINV-PROCESS-IND EQUAL  '1'
003200         IF  WBC-XINV-INSTALLED EQUAL '0' OR '2'                  0266741
003300             GO TO R4710-ERR.
003400*
003500*--------------------------------------------------------------*
003600*    FOR NEXT DAY SWEEP, BCR MUST HAVE NEXT DAY APPL INSTALLED *
003700*--------------------------------------------------------------*
003800     IF  WMS-XINV-PROCESS-IND EQUAL  '2'
003900         IF  WBC-XINV-INSTALLED EQUAL '0' OR '1'                  0266741
004000             GO TO R4710-ERR.
004100*
004200*--------------------------------------------------------------*
004300*    DDA STATUS MUST BE ACTIVE OR INACTIVE                     *
004400*--------------------------------------------------------------*
004500     IF  WMS-STATUS       NOT EQUAL  '00' AND '07'
004600         GO TO R4710-ERR.
004700*
004800*--------------------------------------------------------------*
004900*    CANNOT BE FUNDED CHILD ACCOUNT                            *
005000*--------------------------------------------------------------*
005100     IF  WMS-FUNDING-FLAG NOT EQUAL  ' ' AND '0' AND 'P'
005200         GO TO R4710-ERR.
005300*
005400*--------------------------------------------------------------*
005500*    CANNOT BE ZBA CHILD ACCOUNT                               *
005600*--------------------------------------------------------------*
005700     IF  WMS-TARGET-AMT-TRLR  EQUAL '1'
005800         IF  WMS-NB-ZB-PAR-CHILD EQUAL 'B' OR 'C'
005900         OR  WMS-NB-ZB-LVL-NBR   GREATER THAN '1'
006000             GO TO R4710-ERR.
006100*
006200*--------------------------------------------------------------*
006300*    CANNOT BE A CASH MANAGEMENT ACCOUNT                       *
006400*--------------------------------------------------------------*
006500     IF  WMS-SAVINGS-TRLR     EQUAL  '1'
006600     AND (WMS-CMA-INDICATOR NOT EQUAL '0')
006700         GO TO R4710-ERR.
006800*
006900*--------------------------------------------------------------*
007000*    CANNOT HAVE SAVINGS TRAILER WITH MAXIMUM DDA BALANCE      *
007100*--------------------------------------------------------------*
007200     IF  WMS-SAVINGS-TRLR     EQUAL  '1'
007300     AND (WMS-DDA-MAX-FLAG NOT EQUAL '0')
007400         GO TO R4710-ERR.
007500*
007600*--------------------------------------------------------------*
007700*    CANNOT BE AN INVESTMENT ACCOUNT                           *
007800*--------------------------------------------------------------*
007900     IF  WMS-SAVINGS-TRLR     EQUAL  '1'
008000     AND WMS-INVESTMENT-TRLR  EQUAL  '1'
008100         GO TO R4710-ERR.
008200*
008300*--------------------------------------------------------------*
008400*    CANNOT BE A MONEY MARKET/SAVINGS ACCOUNT                  *  0437111
008500*--------------------------------------------------------------*
008600     IF  WMS-MMDA-INDICATOR   EQUAL  '1' OR '2' OR '3'            0437111
008700         GO TO R4710-ERR.
008800*
008900*--------------------------------------------------------------*
009000*    MUST ALLOW ACH DEBITS AND CREDITS                         *
009100*    ONLY SET RESTRICTION INDICATOR FOR NEXT DAY SWEEPS        *
009200*--------------------------------------------------------------*
009300     IF  WMS-ACH-FLAG     NOT EQUAL  '3'
009400         GO TO R4710-ERR.
009500*
009600*--------------------------------------------------------------*
009700*    SAME DAY CANNOT HAVE LOAN TRAILER WITH AUTOMATIC PAYMENTS *
009800*--------------------------------------------------------------*
009900     IF  WMS-XINV-PROCESS-IND EQUAL  '1'
010000     AND WMS-LOAN-TRLR        EQUAL  '1'
010100         IF  (WMS-LOAN-STATUS EQUAL  '0' OR '1' OR '5')
010200         AND (WMS-AUTO-PYMT   EQUAL  '1')
010300             GO TO R4710-ERR.
010400*
010500*--------------------------------------------------------------*
010600*    IF NO ERROR, SAME DAY EXITS EXTERNAL INVESTMENT EDITS     *
010700*    IF NO ERROR, NEXT DAY CALCULATES SWEEP BALANCE            *
010800*--------------------------------------------------------------*
010900*
011000     IF  WMS-XINV-PROCESS-IND EQUAL  '1'
011100         GO TO R4799-EXIT
011200     ELSE
011300         GO TO R4720-XINV.
011400*
011500*--------------------------------------------------------------*
011600*    IF THERE ARE ERRORS, TURN RESTRICTION FLAG ON AND         *
011700*    WRITE QV REPORT EXCEPTION                                 *
011800*--------------------------------------------------------------*
011900 R4710-ERR.
012000     IF  WMS-XINV-RESTRICT    EQUAL  ' ' OR 'R'
012100         MOVE 'A'                TO  WMS-XINV-RESTRICT.
012200     MOVE 'QV'                   TO  IMEX-CODE-1.
012300     MOVE '17'                   TO  IMEX-REC-NO.
012400     PERFORM WRITE-EX-MST      THRU  WRITE-EX-EXIT.
012500     GO TO R4799-EXIT.
012600*
012700*--------------------------------------------------------------*
012800*    NEXT DAY EXTERNAL INVESTMENT PROCESSING                   *
012900*--------------------------------------------------------------*
013000 R4720-XINV.
013100*
013200*--------------------------------------------------------------*
013300*    CHECK RESTRICTION INDICATOR FOR RESTRICT ALL (VALUE "A")  *
013400*--------------------------------------------------------------*
013500     IF  WMS-XINV-RESTRICT    EQUAL  'A'
013600         GO TO R4799-EXIT.
013700*
013800*--------------------------------------------------------------*
013900*    CALCULATE DDA BALANCE AVAILABLE FOR INVESTMENT            *
014000*--------------------------------------------------------------*
014100     MOVE ZERO                   TO  HOLD-AMT15.
014200     MOVE WMS-DDA-BAL            TO  CUR-BAL.
014300*
014400*--------------------------------------------------------------*
014500*    WMS-MIN-BAL-CALC IS USED FOR EXTERNAL INVESTMENTS USING A *
014600*    COLLECTED BALANCE (MAY BE CHANGED HERE TO USE OTHER FIELD)*
014700*--------------------------------------------------------------*
014800     IF  WMS-XINV-BAL-CALC    EQUAL  'C'
014900         MOVE WMS-MIN-BAL-CALC   TO  CALC-FLAG
015000         PERFORM K0104         THRU  K0105.
015100*
015200     IF  WMS-MIN-BAL          EQUAL  CUR-BAL
015300         GO TO R4799-EXIT.
015400*
015500     IF  CUR-BAL       GREATER THAN  WMS-MIN-BAL
015600         GO TO R4730-PURCHASE
015700     ELSE
015800         GO TO R4740-REDEEM.
015900*
016000*--------------------------------------------------------------*
016100*    DDA BALANCE IS GREATER THAN PEG, PURCHASE INVESTMENT FUNDS*
016200*--------------------------------------------------------------*
016300 R4730-PURCHASE.
016400*
016500*--------------------------------------------------------------*
016600*    CHECK RESTRICTION INDICATOR FOR RESTRICT PURCHASES        *
016700*--------------------------------------------------------------*
016800     IF  WMS-XINV-RESTRICT    EQUAL  'P'
016900         GO TO R4799-EXIT.
017000*
017100     MOVE 'D'                    TO  WXI-DR-CR.
017200     SUBTRACT WMS-MIN-BAL      FROM  CUR-BAL
017300                             GIVING  HOLD-AMT15.
017400*
017500     SUBTRACT HOLD-AMT15       FROM  WMS-DDA-BAL
017600                                     WMS-CURR-BAL.
017700     ADD +1                      TO  T3-XINV-PUR-GEN-NO.
017800     ADD HOLD-AMT15              TO  WMS-XINV-BALANCE
017900                                     T3-XINV-PUR-GEN-AMT
018000                                     T3-XINV-ND-PUR-AMT
018100                                     WS-GL-XINV-ND-PUR-AMT.
018200*
018300     IF  WMS-IOD-RATE-PTR   GREATER THAN ZERO
018400     OR  WMS-HIFI-INDICATOR GREATER THAN ZERO
018500         ADD +1                  TO  IOD-DB-NO
018600                                     WS-GL-IOD-DB-NO
018700         ADD HOLD-AMT15          TO  IOD-DB-AMT
018800                                     WS-GL-IOD-DB-AMT
018900     ELSE
019000         ADD +1                  TO  DDA-DB-NO
019100                                     WS-GL-DDA-DB-NO
019200         ADD HOLD-AMT15          TO  DDA-DB-AMT
019300                                     WS-GL-DDA-DB-AMT.
019400*
019500     PERFORM R4750-GT30        THRU  R4750-EXIT.
019600*
019700     ADD +1                      TO  FS-DB
019800                                     WMS-STMT-NO-DEBITS.
019900     ADD HOLD-AMT15              TO  WMS-STMT-DR-AMT.
020000     PERFORM K7500             THRU  K8500.
020100*
020200     GO TO R4799-EXIT.
020300*
020400*--------------------------------------------------------------*
020500*    PEG IS GREATER THAN DDA BALANCE, REDEEM INVESTMENT FUNDS  *
020600*--------------------------------------------------------------*
020700 R4740-REDEEM.
020800*
020900*--------------------------------------------------------------*
021000*    CHECK RESTRICTION INDICATOR FOR RESTRICT REDEMPTIONS      *
021100*--------------------------------------------------------------*
021200     IF  WMS-XINV-RESTRICT    EQUAL  'R'
021300         GO TO R4799-EXIT.
021400*
021500     MOVE 'C'                    TO  WXI-DR-CR.
021600     SUBTRACT CUR-BAL          FROM  WMS-MIN-BAL
021700                             GIVING  HOLD-AMT15.
021800*
021900     IF  WMS-XINV-BALANCE LESS THAN  HOLD-AMT15
022000         MOVE WMS-XINV-BALANCE   TO  HOLD-AMT15.
022100*
022200     IF  HOLD-AMT15           EQUAL  +0
022300         GO TO R4799-EXIT.
022400*
022500*--------------------------------------------------------------*
022600*    EXECUTE THE SAME CODING FOR REDEMPTION AND DELINKING      *
022700*--------------------------------------------------------------*
022800 R4745-REDEEM-OR-DELINK.
022900*
023000     ADD +1                      TO  T3-XINV-RDM-GEN-NO.
023100     ADD HOLD-AMT15              TO  WMS-DDA-BAL
023200                                     WMS-CURR-BAL
023300                                     T3-XINV-RDM-GEN-AMT
023400                                     T3-XINV-ND-RDM-AMT
023500                                     WS-GL-XINV-ND-RDM-AMT.
023600*
023700     SUBTRACT HOLD-AMT15       FROM  WMS-XINV-BALANCE.
023800     IF  WMS-IOD-RATE-PTR   GREATER  THAN ZERO
023900     OR  WMS-HIFI-INDICATOR GREATER  THAN ZERO
024000         ADD +1                  TO  IOD-CR-NO
024100                                     WS-GL-IOD-CR-NO
024200         ADD HOLD-AMT15          TO  IOD-CR-AMT
024300                                     WS-GL-IOD-CR-AMT
024400     ELSE
024500         ADD +1                  TO  DDA-CR-NO
024600                                     WS-GL-DDA-CR-NO
024700         ADD HOLD-AMT15          TO  DDA-CR-AMT
024800                                     WS-GL-DDA-CR-AMT.
024900*
025000     PERFORM R4750-GT30        THRU  R4750-EXIT.
025100*
025200     ADD +1                      TO  FS-CR
025300                                     WMS-STMT-NO-CREDITS.
025400     ADD HOLD-AMT15              TO  WMS-STMT-CR-AMT.
025500     PERFORM K7500             THRU  K8500.
025600*
025700     GO TO R4799-EXIT.
025800*
025900*--------------------------------------------------------------*
026000*    CREATE GENERATED INVESTMENT TRAN GT30 (GTGQ REPORT EXCEPT)*
026100*--------------------------------------------------------------*
026200 R4750-GT30.
026300     MOVE 'GQ'                   TO  IMEX-CODE-2
026400     MOVE WBC-GENERATED-TRANS (30)                                0266741
026500                                 TO  GEN-TRAN-OPTION-HOLD
026600     MOVE '30'                   TO  EX01-INT-CODE.
026700     IF  WXI-DR-CR EQUAL 'D'
026800         MOVE '4'                TO  EX01-TYPE
026900     ELSE
027000         MOVE '2'                TO  EX01-TYPE.
027100     MOVE ZERO                   TO  EX01-NUMBER.
027200     MOVE HOLD-AMT15             TO  HOLD-AMT.
027300     PERFORM WRITE-GEN-TRANS   THRU  WRITE-GEN-EXIT.
027400*
027500 R4750-EXIT.
027600     EXIT.
027700*
027800 R4799-EXIT.
027900     EXIT.
028000*
