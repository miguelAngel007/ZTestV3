*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100******************************************************************
000200*    THIS COPY USED IN THE FOLLOWING PROGRAMS                    *
000300*                                                                *
000400*    IM31                                                        *
000500*                                                                *
000600*    PLEASE ADD NEW PROGRAMS TO THE ABOVE LIST                   *
000700*                                                                *
000800******************************************************************
000900 K5311-BALANCE-HISTORY.
001000*--------------------------------------------------------------*
001100*  MAINTENANCE TO THE FOLLOWING FIELDS MAY CAUSE A BALANCE     *
001200*  HISTORY RECORD TO BE ADDED/CHANGED.  DELETIONS WILL OCCUR   *
001300*  AT THE TIME OF OUPUT PROCESSING.                            *
001400*         FLD-NO    MASTER FIELD                               *
001500*          0008  =  WMS-STATUS
001600*          0495  =  WMS-INT-STATUS
001700*          0660  =  WMS-IOD-RATE-PTR                           *
001800*          0689  =  WMS-HIFI-INDICATOR                         *
001900*          0716  =  WMS-BALANCE-HISTORY                        *
002000*          0875  =  WMS-BAL-HIST-RET                           *
002100*          1087  =  WMS-SAV-BAL-HIST                           *
002200*          1088  =  WMS-SAV-BAL-HIST-RET                       *
002300*          1089  =  WMS-ODAC-BAL-HIST                          *
002400*          1090  =  WMS-ODAC-BAL-HIST-RET                      *
002500*          1099  =  WMS-INDEX-RATE-USE
002510*          2039  =  WMS-SAV-TIER-PTR                              0316967
002600*--------------------------------------------------------------*
002700     IF  WBC-BAL-HIST-FLAG EQUAL '0'                              0266741
002800         GO TO K5315.
002900     IF  WMS-BALANCE-HISTORY EQUAL '0'
003000     OR  TR-FM-FLD-NO NOT EQUAL '0008'
003100         GO TO K5312.
003200*--------------------------------------------------------------*
003300*  IF RETURNING FROM TERMINATED OR PURGED STATUS, CLEAR THE    *
003400*  BALANCE HISTORY FILE.                                       *
003500*--------------------------------------------------------------*
003600     IF  STATUS-IN NOT EQUAL '05' AND '08' AND '99'
003700         GO TO K5315.
003800     IF  WMS-STATUS NOT EQUAL '05' AND '08' AND '99'
003900         MOVE ZERO TO WBHI-ENTRIES
004000                      WBHM-ENTRIES.
004100     GO TO K5315.
004200 K5312.
004300     IF  TR-FM-FLD-NO EQUAL '0660'
004500                         OR '0716'
004600         PERFORM INIT-BH-IOD THRU INIT-BH-IOD-EXIT
004700         GO TO K5315.
004710     IF  TR-FM-FLD-NO EQUAL '0689'                                9915475
004720         PERFORM INIT-BH-IOD THRU INIT-BH-IOD-EXIT.               9915475
004800     IF  TR-FM-FLD-NO EQUAL '0689' OR '1099'
004900         PERFORM INIT-BH-SUB-TYPE THRU INIT-BH-SUB-TYPE-EXIT
005000         GO TO K5315.
005100     IF  TR-FM-FLD-NO NOT EQUAL '0875'
005200         GO TO K5313-SAV.
005300     IF  WMS-BALANCE-HISTORY EQUAL '0'
005400             GO TO K5315.
005500     IF  WMS-HIFI-INDICATOR GREATER THAN ZERO
005600         IF  WMS-BAL-HIST-RET LESS THAN WBHM-ENTRIES
005700             MOVE WMS-BAL-HIST-RET   TO WBHM-ENTRIES
005800             GO TO K5315
005900         ELSE
006000             GO TO K5315.
006100     IF  WMS-IOD-RATE-PTR GREATER THAN ZERO
006200         IF  WMS-BAL-HIST-RET LESS THAN WBHI-ENTRIES
006300             MOVE WMS-BAL-HIST-RET   TO WBHI-ENTRIES
006400             GO TO K5315
006500         ELSE
006600             GO TO K5315.
006700     GO TO K5315.
006800 K5313-SAV.
006900     IF  WMS-SAVINGS-TRLR EQUAL '0'
007000         GO TO K5314-ODAC.
007100     IF  WMS-SAV-BAL-HIST EQUAL '0'
007200     OR  TR-FM-FLD-NO NOT EQUAL '0495'
007300         GO TO K5313-HIST.
007400*--------------------------------------------------------------*
007500*  IF RETURNING FROM TERMINATED OR PURGED STATUS, CLEAR THE    *
007600*  SAVINGS BALANCE HISTORY FILE.                               *
007700*--------------------------------------------------------------*
007800     IF  SAVINGS-STATUS-IN GREATER THAN '6'
007900     AND WMS-INT-STATUS LESS THAN '7'
007910         IF  WMS-SAV-TIER-PTR GREATER +0                          0316967
007920             MOVE ZERO TO WBHT-ENTRIES                            0316967
007930         ELSE                                                     0316967
008000             MOVE ZERO TO WBHS-ENTRIES.                           0316967
008100     GO TO K5315.
008200 K5313-HIST.
008300     IF  TR-FM-FLD-NO EQUAL '1087'
008310     OR  TR-FM-FLD-NO EQUAL '2039'                                0316967
008400         PERFORM INIT-BH-SAV THRU INIT-BH-SAV-EXIT
008500         GO TO K5315.
008600     IF  TR-FM-FLD-NO EQUAL '1088'
008700     AND WMS-SAV-BAL-HIST EQUAL '1'
008710         IF  WMS-SAV-TIER-PTR GREATER +0                          0316967
008720             IF  WMS-SAV-BAL-HIST-RET LESS THAN WBHT-ENTRIES      0316967
008730                 MOVE WMS-SAV-BAL-HIST-RET   TO WBHT-ENTRIES      0316967
008800                 GO TO K5315                                      0316967
008900             ELSE                                                 0316967
009000                 GO TO K5315                                      0316967
009100         ELSE
009110             IF  WMS-SAV-BAL-HIST-RET LESS THAN WBHS-ENTRIES      0316967
009120                 MOVE WMS-SAV-BAL-HIST-RET   TO WBHS-ENTRIES      0316967
009130                 GO TO K5315                                      0316967
009140             ELSE                                                 0316967
009200                 GO TO K5315.                                     0316967
009300 K5314-ODAC.
009400     IF  WMS-OD-ACCRUAL-TRLR EQUAL '0'
009500         GO TO K5315.
009600     IF  TR-FM-FLD-NO EQUAL '1089'
009700         PERFORM INIT-BH-ODAC THRU INIT-BH-ODAC-EXIT
009800         GO TO K5315.
009900     IF  TR-FM-FLD-NO EQUAL '1090'
010000     AND WMS-ODAC-BAL-HIST EQUAL '1'
010100         IF  WMS-ODAC-BAL-HIST-RET LESS THAN WBHO-ENTRIES
010200             MOVE WMS-ODAC-BAL-HIST-RET   TO WBHO-ENTRIES
010300             GO TO K5315
010400         ELSE
010500             GO TO K5315.
010600 K5315.
010700
