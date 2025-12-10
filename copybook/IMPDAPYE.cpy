*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100******************************************************************
000200*    APYE AND INTEREST EARNED FIGURES ARE REPORTED WHEN THE      *
000300*    INTEREST PAYMENT OCCURS ON THE STATEMENT FOR ACCOUNTS THAT  *
000400*    CALCULATE INTEREST USING AN AVERAGE DAILY BALANCE AND SEND  *
000500*    STATEMENTS MORE FREQUENTLY THAN INTEREST IS CREDITED TO THE *
000600*    ACCOUNT.  THE FOLLOWING LIST IDENTIFIES THESE ACCOUNTS:     *
000700*       -INTEREST IS TIED TO THE SERVICE CHARGE                  *
000800*       -SERVICE CHARGE IS NOT TIED TO THE STATEMENT             *
000900*       -SERVICE CHARGE LESS FREQUENTLY THAN MONTHLY             *
001000*       -INTEREST NOT PAID THIS STATEMENT CYCLE                  *
001100******************************************************************
001200 1000-APYE-SPEC-RULE.
001300     MOVE 'N' TO WS-SKIP-APYE.
001310     IF  WMS-IOD-CYC-PAID-INT GREATER THAN +0.00                  9915851
001320         GO TO 9999-APYE-EXIT.                                    9915851
001330     IF  WMS-IOD-INT-PAY-CYCLE EQUAL 'B' OR 'C' OR 'E' OR         9915851
001340                                     'L' OR 'W'                   9915851
001350         GO TO 4000-APYE-INDEP-CYCLE.                             9915851
001352     IF  WMS-IOD-INT-PAY-CYCLE EQUAL '1'                          0827731
001354     AND WMS-SC-CYCLE EQUAL 'Y'                                   0827731
001356         MOVE 'Y' TO WS-SKIP-APYE                                 0827731
001358         GO TO 9999-APYE-EXIT.                                    0827731
001400     IF  (WMS-IOD-INT-PAY-CYCLE EQUAL '2' OR '4')                 9915845
001500     OR  (WMS-SC-CYCLE EQUAL '0')
001600     OR  (WMS-SC-INCR LESS THAN +2)
001800         GO TO 9999-APYE-EXIT.
001900     IF  WMS-STMT-SUPP EQUAL '4'
002000         IF  WMS-STMT-FLD3 NOT LESS WMS-SC-INCR
002100             GO TO 9999-APYE-EXIT.
002200     MOVE 'Y' TO WS-SKIP-APYE.
002300     GO TO 9999-APYE-EXIT.
002400
002500 2000-APYE-SPEC-RULE.
002600     MOVE 'N' TO WS-SKIP-APYE.
002610     IF  WMS-IOD-INT-PAY-CYCLE EQUAL 'B' OR 'C' OR 'E' OR         9915851
002620                                     'L' OR 'W'                   9915851
002630         GO TO 4000-APYE-INDEP-CYCLE.                             9915851
002632     IF  WMS-IOD-INT-PAY-CYCLE EQUAL '1'                          0827731
002634     AND WMS-SC-CYCLE EQUAL 'Y'                                   0827731
002636         MOVE 'Y' TO WS-SKIP-APYE                                 0827731
002638         GO TO 9999-APYE-EXIT.                                    0827731
002700     IF  (WMS-IOD-INT-PAY-CYCLE EQUAL '2' OR '4')                 9915845
002800     OR  (WMS-SC-CYCLE EQUAL '0')
002900     OR  (WMS-SC-INCR LESS THAN +2)
003000         GO TO 9999-APYE-EXIT.
003100     IF  WMS-STMT-SUPP EQUAL '4'
003200         IF  WMS-STMT-FLD3 NOT LESS WMS-SC-INCR
003300             GO TO 9999-APYE-EXIT.
003400     MOVE 'Y' TO WS-SKIP-APYE.
003500     GO TO 9999-APYE-EXIT.
003600
003700 3000-APYE-SPEC-RULE.
003800     MOVE 'N' TO WS-SKIP-APYE.
003810     IF  WMS-SAV-INT-PAY-CYCLE EQUAL 'B' OR 'C' OR 'E' OR         9915851
003820                                     'L' OR 'W'                   9915851
003830         GO TO 5000-APYE-INDEP-CYCLE.                             9915851
003832     IF  WMS-SAV-INT-PAY-CYCLE EQUAL '1'                          0827731
003834     AND WMS-SC-CYCLE EQUAL 'Y'                                   0827731
003836         MOVE 'Y' TO WS-SKIP-APYE                                 0827731
003838         GO TO 9999-APYE-EXIT.                                    0827731
003900     IF  (WMS-SAV-INT-PAY-CYCLE EQUAL '2' OR '3')                 9915851
004000     OR  (WMS-SC-CYCLE EQUAL '0')
004100     OR  (WMS-SC-INCR LESS THAN +2)
004200         GO TO 9999-APYE-EXIT.
004300     IF  WMS-STMT-SUPP EQUAL '4'
004400         IF  WMS-STMT-FLD3 NOT LESS WMS-SC-INCR
004500             GO TO 9999-APYE-EXIT.
004600     MOVE 'Y' TO WS-SKIP-APYE.
004700     GO TO 9999-APYE-EXIT.
004800
004900 4000-APYE-INDEP-CYCLE.                                           9915851
005000     IF  WMS-IOD-INT-PAY-CYCLE EQUAL 'E' OR 'L'                   9915851
005100         IF  WMS-IOD-INT-INCR EQUAL '0'                           9915851
005200             GO TO 9999-APYE-EXIT.                                9915851
005300     IF  WMS-STMT-SUPP EQUAL '4'                                  9915851
005400         IF  WMS-STMT-FLD3 NOT LESS WMS-IOD-INT-INCR              9915851
005500             GO TO 9999-APYE-EXIT.                                9915851
005600     MOVE 'Y' TO WS-SKIP-APYE.                                    9915851
005700     GO TO 9999-APYE-EXIT.                                        9915851
005800                                                                  9915851
005900 5000-APYE-INDEP-CYCLE.                                           9915851
006000     IF  WMS-SAV-INT-PAY-CYCLE EQUAL 'E' OR 'L'                   9915851
006100         IF  WMS-SAV-INT-INCR EQUAL '0'                           9915851
006200             GO TO 9999-APYE-EXIT.                                9915851
006300     IF  WMS-STMT-SUPP EQUAL '4'                                  9915851
006400         IF  WMS-STMT-FLD3 NOT LESS WMS-SAV-INT-INCR              9915851
006500             GO TO 9999-APYE-EXIT.                                9915851
006600     MOVE 'Y' TO WS-SKIP-APYE.                                    9915851
006700     GO TO 9999-APYE-EXIT.                                        9915851
006800                                                                  9915851
006900 9999-APYE-EXIT.                                                  9915851
007000     EXIT.                                                        9915851
