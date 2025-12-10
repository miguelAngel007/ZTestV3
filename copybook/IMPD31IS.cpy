*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*--------------------------------------------------------------*
000200*     IMPD31IS                                                 *
000300*     DETERMINES WHETHER AN IOD, SAVINGS PAYMENT OR OD CHARGE  *
000400*     CYCLE IS SCHEDULED FOR TODAY.                            *
000500*--------------------------------------------------------------*
000600
000700     IF  WMS-IOD-INT-PAY-CYCLE NOT EQUAL
000800         ('B' AND 'C' AND 'E' AND 'L' AND 'W')
000900         GO TO R2505.
001000     MOVE WMS-IOD-INT-PAY-CYCLE    TO INT-CYCLE.
001100     MOVE WMS-IOD-INT-INCR         TO INT-INCR.
001200     MOVE WMS-IOD-INT-DAY          TO INT-DAY.
001300     MOVE WMS-IOD-INT-MONTH1       TO INT-MONTH1.
001400     MOVE SPACE                    TO INT-CYCLE-DAYX.
001500     PERFORM R2510 THRU R2599.
001600     MOVE INT-CYCLE-DAY            TO IOD-CYCLE-DAY.
001700     MOVE INT-PAY-TODAY            TO IOD-PAY-TODAY.
001800
001900 R2505.
002000     IF  WMS-SAVINGS-TRLR EQUAL ZERO
002100         GO TO R2506.
002200     IF  WMS-SAV-INT-PAY-CYCLE NOT EQUAL
002300         ('B' AND 'C' AND 'E' AND 'L' AND 'W')
002400         GO TO R2506.
002500     MOVE WMS-SAV-INT-PAY-CYCLE    TO INT-CYCLE.
002600     MOVE WMS-SAV-INT-INCR         TO INT-INCR.
002700     MOVE WMS-SAV-INT-DAY          TO INT-DAY.
002800     MOVE WMS-SAV-INT-MONTH1       TO INT-MONTH1.
002900     MOVE SPACE                    TO INT-CYCLE-DAYX.
003000     PERFORM R2510 THRU R2599.
003100     MOVE INT-CYCLE-DAY            TO SAV-CYCLE-DAY.
003200     MOVE INT-PAY-TODAY            TO SAV-PAY-TODAY.
003300
003400 R2506.
003500     IF  WMS-OD-ACCRUAL-TRLR EQUAL ZERO
003600         GO TO R2599.
003700     IF  WMS-OD-INT-SCHED NOT EQUAL
003800         ('B' AND 'C' AND 'E' AND 'L' AND 'W')
003900         GO TO R2599.
004000     MOVE WMS-OD-INT-SCHED         TO INT-CYCLE.
004100     MOVE WMS-OD-INT-INCR          TO INT-INCR.
004200     MOVE WMS-OD-INT-DAY           TO INT-DAY.
004300     MOVE WMS-OD-INT-MONTH1        TO INT-MONTH1.
004400     MOVE SPACE                    TO INT-CYCLE-DAYX.
004500     PERFORM R2510 THRU R2599.
004600     MOVE INT-CYCLE-DAY            TO OD-CYCLE-DAY.
004700     MOVE INT-PAY-TODAY            TO OD-CHG-TODAY.
004800     GO TO R2599.
004900 R2510.
005000     MOVE '0' TO INT-PAY-TODAY.
005100     IF  INT-CYCLE   EQUAL 'E'
005200     AND INT-INCR    EQUAL '0'
005300     AND WBC-YEAR-END EQUAL '1'                                   0266741
005400         MOVE WBC-ACCRUAL-DAYS     TO INT-CYCLE-DAY               0266741
005500         GO TO R2590.
005600     IF  INT-CYCLE EQUAL 'E'
005700         IF  WBC-MONTH-END EQUAL '1'                              0266741
005800             MOVE WBC-ACCRUAL-DAYS TO INT-CYCLE-DAY               0266741
005900             GO TO R2570
006000         ELSE
006100             GO TO R2599.
006200     IF  WBC-YEAR-END EQUAL '1'                                   0266741
006300     AND INT-CYCLE   EQUAL 'L'
006400     AND INT-INCR    EQUAL '0'
006600         MOVE '1'                 TO INT-CYCLE-DAYX
006700         GO TO R2590.
006800     IF  INT-CYCLE EQUAL 'L'
006900         IF  WBC-MONTH-END EQUAL '1'                              0266741
007000             MOVE '1'             TO INT-CYCLE-DAYX
007100             GO TO R2570
007200         ELSE
007300             GO TO R2599.
007400     IF  INT-CYCLE NOT EQUAL 'C'
007500         GO TO R2550.
007600     MOVE +1 TO X.
007700
007800 R2540.
007900     IF  INT-DAY EQUAL WBC-STMT-CAL (X)                           0266741
008000         IF  X LESS THAN WBC-ACCRUAL-DAYS                         0266741
008100             MOVE X                  TO INT-CYCLE-DAY
008200             GO  TO  R2570
008300         ELSE
008400             MOVE WBC-ACCRUAL-DAYS   TO INT-CYCLE-DAY             0266741
008500             GO  TO  R2570.
008600     ADD +1 TO X.
008700     IF  X GREATER THAN +8
008800         GO  TO  R2599.
008900     GO TO R2540.
009000
009100 R2550.
009200     IF  INT-CYCLE EQUAL 'B'
009300         IF  INT-DAY  EQUAL WBC-STMT-BANK (1)                     0266741
009400                         OR WBC-STMT-BANK (2)                     0266741
009500                         OR WBC-STMT-BANK (3)                     0266741
009600                         OR WBC-STMT-BANK (4)                     0266741
009700                         OR WBC-STMT-BANK (5)                     0266741
009800                         OR WBC-STMT-BANK (6)                     0266741
009900             MOVE '1'    TO INT-CYCLE-DAYX
010000             GO TO R2570
010100         ELSE
010200             GO TO R2599.
010300*--------------------------------------------------------------*
010400* DETERMINES IF A SPECIFIC DAY OF THE WEEK OCCURS IN TODAY'S   *
010500* PROCESSING.                                                  *
010600*--------------------------------------------------------------*
010700     IF  (INT-CYCLE EQUAL 'W'
010800     AND INT-DAY1 EQUAL '0')
010900         IF  INT-DAY2  EQUAL WBC-STMT-WK-DAY (1)                  0266741
011000                          OR WBC-STMT-WK-DAY (2)                  0266741
011100                          OR WBC-STMT-WK-DAY (3)                  0266741
011200                          OR WBC-STMT-WK-DAY (4)                  0266741
011300                          OR WBC-STMT-WK-DAY (5)                  0266741
011400                          OR WBC-STMT-WK-DAY (6)                  0266741
011500             MOVE '1' TO INT-CYCLE-DAYX
011600             GO TO R2590.
011700*--------------------------------------------------------------*
011800* DETERMINES IF A FIXED DAY AND WEEK WITHIN MONTH OCCURS IN    *
011900* TODAY'S PROCESSING.                                          *
012000*--------------------------------------------------------------*
012100     IF  INT-CYCLE EQUAL 'W'
012200         IF  INT-DAY  EQUAL WBC-STMT-WEEK (1)                     0266741
012300                         OR WBC-STMT-WEEK (2)                     0266741
012400                         OR WBC-STMT-WEEK (3)                     0266741
012500                         OR WBC-STMT-WEEK (4)                     0266741
012600                         OR WBC-STMT-WEEK (5)                     0266741
012700                         OR WBC-STMT-WEEK (6)                     0266741
012800             MOVE '1' TO INT-CYCLE-DAYX
012900             GO TO R2570.
013000     GO TO R2599.
013100
013200 R2570.
013300     IF  INT-INCR EQUAL '1'
013400         GO TO R2590.
013500     MOVE INT-MONTH1 TO HOLD-9.
013600     IF  INT-CYCLE EQUAL 'C'
013700     AND WBC-MONTH-END EQUAL TO '1'                               0266741
013800     AND INT-DAY LESS THAN WBC-LAST-PROC-DA                       0266741
013900         GO  TO  R2585.
014400
014500 R2580.
014600     IF  WBC-PROC-THRU-MO EQUAL HOLD-X                            0266741
014700         GO TO R2590.
014800     IF  INT-INCR-9 EQUAL +0
014900         GO TO R2599.
015000     ADD INT-INCR-9 TO HOLD-9.
015100     IF  HOLD-X GREATER THAN '12'
015200         GO TO R2599.
015300     GO TO R2580.
015400
015500 R2585.
015600     IF  WBC-STMT-MO EQUAL HOLD-X                                 0266741
015700         GO TO R2590.
015800     IF  INT-INCR-9 EQUAL +0
015900         GO TO R2599.
016000     ADD INT-INCR-9 TO HOLD-9.
016100     IF  HOLD-X GREATER THAN '12'
016200         GO TO R2599.
016300     GO TO R2585.
016400
016500 R2590.
016600     MOVE '1' TO INT-PAY-TODAY.
016700
016800 R2599.
016900     EXIT.
017000
