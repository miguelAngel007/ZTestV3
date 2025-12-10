*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000010     MOVE ZERO TO TAX-CALC-AMOUNT                                 2012254
000020                  TAX-CALC-AMOUNT-ST                              2012254
000030                  TAX-CALC-AMOUNT-LOC                             2012254
000040                  WORK-TAX-RATE.                                  2012254
000050                                                                  2012254
000100     IF  TAX-CALC-PAYMENT NOT GREATER THAN ZERO                   2012254
000200         GO TO TAX-CALC-EXIT.                                     2012254
000300                                                                  2012254
000400     IF  WMS-RATE-TRLR EQUAL 0                                    2012254
000500         GO TO TAX-CALC-EXIT.                                     2012254
000501                                                                  2012254
000600                                                                  2012254
001100*                                                                 2012254
001200* CALCULATE FEDERAL TAX                                           2012254
001300*                                                                 2012254
001400     IF  WMS-BKUP-WTHLD-FLAG   GREATER THAN ZERO                  2012254
001500         IF WMS-FED-TAX-RATE-FLAG EQUAL 'Y'                       2012254
001600             MOVE WMS-FED-TAX-RATES TO WORK-TAX-RATES             2012254
001700             PERFORM TXCL-00-CHK-RATES                            2012254
001800                                    THRU TXCL-99-CHK-RATES-EXIT   2012254
001900             MULTIPLY TAX-CALC-PAYMENT BY WORK-TAX-RATE           2012254
002000                 GIVING TAX-CALC-AMOUNT ROUNDED.                  2012254
002100*                                                                 2012254
002200* CALCULATE STATE TAX                                             2012254
002300*                                                                 2012254
002400     IF  WMS-STATE-LOCAL-TX-CD EQUAL '1' OR '2'                   2012254
002500         IF WMS-ST-TAX-RATE-FLAG EQUAL 'Y'                        2012254
002600             MOVE WMS-ST-TAX-RATES  TO WORK-TAX-RATES             2012254
002700             PERFORM TXCL-00-CHK-RATES                            2012254
002800                                    THRU TXCL-99-CHK-RATES-EXIT   2012254
002900             MULTIPLY TAX-CALC-PAYMENT BY WORK-TAX-RATE           2012254
003000                 GIVING TAX-CALC-AMOUNT-ST ROUNDED.               2012254
003100*                                                                 2012254
003200* CALCULATE LOCAL TAX                                             2012254
003300*                                                                 2012254
003400     IF  WMS-STATE-LOCAL-TX-CD EQUAL '1' OR '3'                   2012254
003500         IF WMS-LOC-TAX-RATE-FLAG EQUAL 'Y'                       2012254
003600             MOVE WMS-LOC-TAX-RATES TO WORK-TAX-RATES             2012254
003700             PERFORM TXCL-00-CHK-RATES                            2012254
003800                                    THRU TXCL-99-CHK-RATES-EXIT   2012254
003900             MULTIPLY TAX-CALC-PAYMENT BY WORK-TAX-RATE           2012254
004000                 GIVING TAX-CALC-AMOUNT-LOC ROUNDED.              2012254
004010     GO TO TAX-CALC-EXIT.                                         2012254
004100*                                                                 2012254
004400                                                                  2012254
004500 TXCL-00-CHK-RATES.                                               2012254
004600     MOVE +1 TO Y.                                                2012254
004700                                                                  2012254
004800 TXCL-01-FIND-RATE.                                               2012254
004900     IF  Y EQUAL BIN-9                                            2012254
005000     OR  TAX-CALC-PAYMENT  LESS THAN WORK-TAX-CUR-LMT (Y)         2012254
005100         MOVE WORK-TAX-CUR-ANN (Y)  TO WORK-TAX-RATE              2012254
005200         GO TO TXCL-99-CHK-RATES-EXIT.                            2012254
005300     ADD +1 TO Y.                                                 2012254
005400     GO TO TXCL-01-FIND-RATE.                                     2012254
005500                                                                  2012254
005600 TXCL-99-CHK-RATES-EXIT.                                          2012254
005700     EXIT.                                                        2012254
005800*                                                                 2012254
005900 TAX-CALC-EXIT.                                                   2012254
006000     EXIT.                                                        2012254
