*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*
000200*         IM31 PROCEDURE COPYBOOK - INSERT IN PAR R6600          *
000300*----------------------------------------------------------------*
000400     IF WMS-DATE-LAST-CUST-ACTIVITY EQUAL WBC-CAPTURE-DATE        IMUP006
000500        MOVE '00'            TO WMS-STATUS                        IMUP006
000600        MOVE '00   '         TO WMS-GL-COST-CTR (5)               IMUP006
000700        MOVE '51'            TO WMS-REASON-CLOSED                 IMUP006
000800        MOVE WRK-DC-DSC-DATE TO WMS-DC-DATE-STATUS-CHANGE         IMUP006
000900        MOVE ZEROS           TO WMS-DATE-INACT-DORM               IMUP006
001000        MOVE 'SU'            TO IMEX-CODE-1                       IMUP006
001100        MOVE 'SD'            TO IMEX-CODE-2                       IMUP006
001200        MOVE '09'            TO IMEX-REC-NO                       IMUP006
001300        MOVE '51'            TO EX09-GEN-REASON                   IMUP006
001400        MOVE WK-IM-STATUS-IN TO EX09-STATUS-IN                    IMUP006
001500        MOVE '00'            TO EX09-STATUS-OUT, WK-IM-STATUS-IN  IMUP006
001600        MOVE WMS-CURR-BAL    TO EX09-CUR-BAL                      IMUP006
001700        MOVE '*'             TO EX09-GEN-STAT                     IMUP006
001800        MOVE ' DDA-AUTOMATICALLY CHANGED INACTIVE'                IMUP006
001900                             TO EX09-GEN-DESC                     IMUP006
002000        PERFORM WRITE-EX-MST THRU WRITE-EX-EXIT                   IMUP006
002100        GO TO R7000                                               IMUP006
002200     END-IF.                                                      IMUP006
002300                                                                  IMUP006
002400     IF WMS-CURR-BAL EQUAL TO ZERO                                IMUP006
002500     AND WMS-CLS-OVRRIDE EQUAL TO '0'                             IMUP006
002600     AND ((WMS-IOD-CYC-ACC-INT EQUAL ZERO)                        IMUP006
002700     OR (WMS-IOD-CYC-ACC-INT GREATER THAN ZERO                    IMUP006
002800     AND WMS-IOD-CYC-ACC-INT LESS THAN WMS-MIN-CR-INT))           IMUP006
002900        IF WMS-LOAN-TRLR EQUAL '1'                                IMUP006
003000           IF (WMS-PRIN-BAL EQUAL ZERO AND                        IMUP006
003100               WMS-INTRST-BAL EQUAL ZERO AND                      IMUP006
003200               WMS-OTHER-CHGS EQUAL ZERO)                         IMUP006
003300               IF WMS-DAYS-ZERO-BAL + 1 LESS THAN                 0702079
003400                                             WMS-0BAL-GRACE-DAYS  0702079
003500                  GO TO R7000                                     IMI002
003600               ELSE                                               IMUP006
003700                  GO TO R6050                                     IMUP006
003800               END-IF                                             IMI002
003900            ELSE                                                  IMUP006
004000               GO TO R7000                                        IMI002
004100            END-IF                                                IMI002
004200        ELSE                                                      IMI002
004300           IF WMS-DAYS-ZERO-BAL + 1 LESS THAN WMS-0BAL-GRACE-DAYS 0702079
004400              GO TO R7000                                         IMI002
004500           ELSE                                                   IMUP006
004600              GO TO R6050                                         IMUP006
004700           END-IF                                                 IMI002
004800        END-IF                                                    IMI002
004900     END-IF.                                                      IMI002
