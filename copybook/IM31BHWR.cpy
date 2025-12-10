*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100******************************************************************
000200*    THIS COPY USED IN THE FOLLOWING PROGRAMS                    *
000300*                                                                *
000400*    IM31                                                        *
000500*                                                                *
000600*    PLEASE ADD NEW PROGRAMS TO THE ABOVE LIST                   *
000700*                                                                *
000800******************************************************************
000900 WRITE-BALANCE-HISTORY.
001000*--------------------------------------------------------------*
001100*    BYPASS WRITING THE BALANCE HISTORY FILE IF                *
001200*      -   BALANCE HISTORY OPTION IS NOT USED AT BCR LEVEL     *
001300*      -   ACCOUNT HEADER RECORD    (WMS-EXC-CODE = 0)         *
001400*      -   ACCOUNT DOES NOT USE BALANCE HISTORY                *
001500*--------------------------------------------------------------*
001600     IF  WBC-BAL-HIST-FLAG EQUAL '0'                              0266741
001700     OR  WMS-EXC-CODE     EQUAL '0'
001800         GO TO WRITE-BAL-IBT-EXIT.                                9915857
001900
002000     IF  WMS-BALANCE-HISTORY EQUAL '0'
002100         IF  WMS-IOD-RATE-USE NOT EQUAL 'S'                       100
002200             GO TO WRITE-BAL-HIST-CONT.
002300
002400     IF  WMS-BALANCE-HISTORY EQUAL '1'
002500         MOVE WMS-BAL-HIST-RET        TO BHW-MAX-RET
002600         IF  WMS-HIFI-INDICATOR GREATER THAN ZERO
002700             MOVE WORK-BAL-HIST-MMDA  TO BALANCE-HISTORY-REC
002800             MOVE BHW-LEN-MMDA        TO BHW-LEN
002900             PERFORM WRITE-BAL-HIST-REC THRU WRITE-BAL-HIST-EXIT
003000         ELSE
003100             IF  WMS-IOD-RATE-PTR GREATER THAN ZERO
003200                 MOVE WORK-BAL-HIST-IOD TO BALANCE-HISTORY-REC
003300                 MOVE BHW-LEN-IOD     TO BHW-LEN
003400                 PERFORM WRITE-BAL-HIST-REC THRU
003500                                        WRITE-BAL-HIST-EXIT.
003510     IF  WMS-BALANCE-HISTORY EQUAL '1'                            9915857
003520     AND IBT-96-FLAG EQUAL '1'                                    9915857
003530     AND (WMS-HIFI-INDICATOR GREATER THAN ZERO                    9915857
003540          OR  WMS-IOD-RATE-PTR GREATER THAN ZERO)                 9915857
003550         MOVE HOLD-WORK-ACCOUNT       TO BH-CONTROLS              9915857
003560         PERFORM WRITE-BAL-HIST-IBT   THRU WRITE-BAL-IBT-EXIT.    9915857
003600     IF  WMS-IOD-RATE-USE EQUAL 'S'                               100
003700     AND WMS-HIFI-INDICATOR GREATER THAN +0
003800         MOVE WORK-BAL-HIST-MMDA-AGGR TO BALANCE-HISTORY-REC
003900         MOVE BHW-LEN-MMDA-AGGR       TO BHW-LEN
004000         MOVE +60                     TO BHW-MAX-RET
004100         PERFORM WRITE-BAL-HIST-REC THRU WRITE-BAL-HIST-EXIT      9915857
004110         IF  IBT-96-FLAG EQUAL '1'                                9915857
004120             MOVE HOLD-WORK-ACCOUNT       TO BH-CONTROLS          9915857
004130             PERFORM WRITE-BAL-HIST-IBT   THRU WRITE-BAL-IBT-EXIT.9915857
004200 WRITE-BAL-HIST-CONT.
004300     IF  WMS-OD-ACCRUAL-TRLR EQUAL '1'
004400     AND WMS-ODAC-BAL-HIST   EQUAL '1'
004500         MOVE WORK-BAL-HIST-ODAC      TO BALANCE-HISTORY-REC
004600         MOVE BHW-LEN-ODAC            TO BHW-LEN
004700         MOVE WMS-ODAC-BAL-HIST-RET   TO BHW-MAX-RET
004800         PERFORM WRITE-BAL-HIST-REC THRU WRITE-BAL-HIST-EXIT      9915857
004810         IF  IBT-96-FLAG EQUAL '1'                                9915857
004820             MOVE HOLD-WORK-ACCOUNT       TO BH-CONTROLS          9915857
004830             PERFORM WRITE-BAL-HIST-IBT THRU WRITE-BAL-IBT-EXIT.  9915857
004900
005000     IF  WMS-SAVINGS-TRLR  EQUAL '1'
005100     AND WMS-SAV-BAL-HIST  EQUAL '1'
005105         IF  WMS-SAV-TIER-PTR GREATER +0                          0316967
005110             MOVE WORK-BAL-HIST-SAV-TIER  TO BALANCE-HISTORY-REC  0316967
005115             MOVE BHW-LEN-SAVT            TO BHW-LEN              0316967
005120             MOVE WMS-SAV-BAL-HIST-RET    TO BHW-MAX-RET          0316967
005125             PERFORM WRITE-BAL-HIST-REC THRU WRITE-BAL-HIST-EXIT  0316967
005130             IF  IBT-96-FLAG EQUAL '1'                            0316967
005135               MOVE HOLD-WORK-ACCOUNT     TO BH-CONTROLS          0316967
005140               PERFORM WRITE-BAL-HIST-IBT THRU WRITE-BAL-IBT-EXIT 0316967
005145             ELSE                                                 0316967
005150               NEXT SENTENCE                                      0316967
005155         ELSE                                                     0316967
005200             MOVE WORK-BAL-HIST-SAV       TO BALANCE-HISTORY-REC  0316967
005300             MOVE BHW-LEN-SAV             TO BHW-LEN              0316967
005400             MOVE WMS-SAV-BAL-HIST-RET    TO BHW-MAX-RET          0316967
005500             PERFORM WRITE-BAL-HIST-REC THRU WRITE-BAL-HIST-EXIT  0316967
005510             IF  IBT-96-FLAG EQUAL '1'                            0316967
005520               MOVE HOLD-WORK-ACCOUNT     TO BH-CONTROLS          0316967
005530               PERFORM WRITE-BAL-HIST-IBT THRU WRITE-BAL-IBT-EXIT.0316967
005600*--------------------------------------------------------------*
005700*    CLEAR THE BALANCE HISTORY WORK FIELDS.                    *
005800*--------------------------------------------------------------*
005900     MOVE 'FFF'                      TO BAL-HIST-WORK-FLAGS.
006000     MOVE 'F'                        TO BAL-HIST-WORK-AGGR-FLAGS.
006100     MOVE LOW-VALUES                 TO WORK-BAL-HIST-IOD
006200                                        WORK-BAL-HIST-MMDA
006300                                        WORK-BAL-HIST-ODAC
006400                                        WORK-BAL-HIST-SAV
006410                                        WORK-BAL-HIST-SAV-TIER    0316967
006500                                        WORK-BAL-HIST-MMDA-AGGR.
006600     GO TO WRITE-BAL-HIST-END.
006700 WRITE-BAL-HIST-REC.
006800*--------------------------------------------------------------*
006900*    CALCULATE THE LENGTH BASED ON THE NUMBER OF ENTRIES.      *
007000*--------------------------------------------------------------*
007100     IF  BH-ENTRIES GREATER THAN  BHW-MAX-RET
007200         MOVE BHW-MAX-RET     TO  BH-ENTRIES.
007300     COMPUTE BH-LENGTH = BHW-LEN-FIXED + (BHW-LEN * BH-ENTRIES).
007400     MOVE 'O'                 TO  I-O-CONTROL-ACCESS.
007500     MOVE 'L'                 TO  I-O-CONTROL-OPERATOR.
007600     CALL 'IMBALM'         USING  I-O-CONTROL-AREA                9915848
007700                                  BALANCE-HISTORY-REC             9915848
007710                                  BAL-HIST-LENGTHS                9915848
007720                                  SI-ENVIRONMENT-AREA             0447266
007730                                  DDA-WRKBCR-1.                   0447266
007800 WRITE-BAL-HIST-EXIT.  EXIT.
007900
008000 WRITE-BAL-HIST-IBT.                                              9915857
008100     MOVE 'O'                 TO  I-O-CONTROL-ACCESS.             9915857
008200     MOVE 'L'                 TO  I-O-CONTROL-OPERATOR.           9915857
008300     CALL 'IMIBTTS'        USING  I-O-CONTROL-AREA                9915857
008400                                  BALANCE-HISTORY-REC.            9915857
008500 WRITE-BAL-IBT-EXIT.  EXIT.                                       9915857
008600                                                                  9915857
008700 WRITE-BAL-HIST-END.                                              9915857
