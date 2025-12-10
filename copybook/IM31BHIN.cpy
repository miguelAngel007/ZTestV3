*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100******************************************************************
000200*    THIS COPY USED IN THE FOLLOWING PROGRAMS                    *
000300*                                                                *
000400*    IM31                                                        *
000500*                                                                *
000600*    PLEASE ADD NEW PROGRAMS TO THE ABOVE LIST                   *
000700*                                                                *
000800******************************************************************
000900 INIT-BALANCE-HISTORY.
001000*--------------------------------------------------------------*
001100*    BYPASS INITIALIZING BALANCE HISTORY RECORDS IF            *
001200*      -   BALANCE HISTORY OPTION IS NOT USED AT BCR LEVEL     *
001300*      -   ACCOUNT DOES NOT USE BALANCE HISTORY                *
001400*      -   TRAILER DOES NOT EXIST                              *
001500*      -   RECORD ALREADY EXISTS FOR ACCOUNT (MAINTENANCE ONLY)*
001600*    MAINTENANCE TO THE BALANCE HISTORY FIELDS MAY CAUSE       *
001700*    RECORDS TO BE INITIALIZED, IF THE RECORD DID NOT EXIST    *
001800*    PREVIOUSLY.                                               *
001900*--------------------------------------------------------------*
002000     MOVE 'FFF'                      TO BAL-HIST-WORK-FLAGS.
002100     MOVE 'F'                        TO BAL-HIST-WORK-AGGR-FLAGS.
002200     MOVE LOW-VALUES                 TO WORK-BAL-HIST-IOD
002300                                        WORK-BAL-HIST-MMDA
002400                                        WORK-BAL-HIST-ODAC
002500                                        WORK-BAL-HIST-SAV
002510                                        WORK-BAL-HIST-SAV-TIER    0316967
002600                                        WORK-BAL-HIST-MMDA-AGGR.
002700     IF  WBC-BAL-HIST-FLAG EQUAL '0'                              0266741
002800         GO TO INIT-BAL-HIST-EXIT.
002900 INIT-BH-SUB-TYPE.
003000     MOVE 'F' TO BHW-MMDA-AGGR-FLAG.
003100     IF  WMS-IOD-RATE-USE NOT EQUAL 'S'                           100
003200         GO TO INIT-BH-SUB-TYPE-EXIT.
003300     IF  WMS-HIFI-INDICATOR EQUAL +0
003400         GO TO INIT-BH-SUB-TYPE-EXIT.
003500     MOVE 'S' TO BHW-MMDA-AGGR-FLAG.
003600     IF  WMS-CONTROL-KEY EQUAL WBHA-MMDA-CONTROLS
003700         GO TO INIT-BH-SUB-TYPE-EXIT
003800     ELSE
003900         MOVE LOW-VALUES      TO WORK-BAL-HIST-MMDA-AGGR
004000         MOVE WMS-CONTROL-KEY TO WBHA-MMDA-CONTROLS
004100         MOVE 'M'             TO WBHA-MMDA-TYPE
004200         MOVE 'A'             TO WBHA-MMDA-SUB-TYPE
004300         MOVE +0              TO WBHA-MMDA-ENTRIES
004400         MOVE +60             TO BHW-MAX-RET.
004500 INIT-BH-SUB-TYPE-EXIT.
004600     EXIT.
004700 INIT-BH-IOD.
004800     MOVE 'F'                        TO BHW-IOD-FLAG.
004900     IF  WMS-BALANCE-HISTORY EQUAL '0'
005000         GO TO INIT-BH-IOD-EXIT.
005100
005200     IF  WMS-HIFI-INDICATOR GREATER THAN ZERO
005300         MOVE 'M'                    TO BHW-IOD-FLAG
005400                                        BHW-TYPE
005500         IF  WMS-CONTROL-KEY  EQUAL WBHM-CONTROLS
005600             GO TO INIT-BH-IOD-EXIT
005700         ELSE
005800             MOVE LOW-VALUES         TO WORK-BAL-HIST-MMDA
005900             MOVE WMS-CONTROL-KEY    TO WBHM-CONTROLS
006000             MOVE 'M'                TO WBHM-TYPE
006100             MOVE ZERO               TO WBHM-ENTRIES
006200             MOVE WMS-BAL-HIST-RET   TO BHW-MAX-RET
006300             PERFORM INIT-BAL-HIST-BACKDATE THRU INIT-BH-BKDT-EXIT
006400             GO TO INIT-BH-IOD-EXIT.
006500
006600     IF  WMS-IOD-RATE-PTR  GREATER THAN ZERO
006700         MOVE 'I'                    TO BHW-IOD-FLAG
006800                                        BHW-TYPE
006900         IF  WMS-CONTROL-KEY EQUAL WBHI-CONTROLS
007000             GO TO INIT-BH-IOD-EXIT
007100         ELSE
007200             MOVE LOW-VALUES         TO WORK-BAL-HIST-IOD
007300             MOVE WMS-CONTROL-KEY    TO WBHI-CONTROLS
007400             MOVE 'I'                TO WBHI-TYPE
007500             MOVE ZERO               TO WBHI-ENTRIES
007600             MOVE WMS-BAL-HIST-RET   TO BHW-MAX-RET
007700             PERFORM INIT-BAL-HIST-BACKDATE THRU INIT-BH-BKDT-EXIT
007800             GO TO INIT-BH-IOD-EXIT.
007900 INIT-BH-IOD-EXIT.                                                9915845
008000     EXIT.                                                        9915845
008100
008200 INIT-BH-ODAC.
008300     MOVE 'F'                 TO BHW-ODAC-FLAG.
008400     IF  WMS-OD-ACCRUAL-TRLR EQUAL '0'
008500     OR  WMS-ODAC-BAL-HIST   EQUAL '0'
008600         GO TO INIT-BH-ODAC-EXIT.
008700     MOVE 'N'                 TO BHW-ODAC-FLAG.
008800     IF  WMS-CONTROL-KEY   EQUAL WBHO-CONTROLS
008900         GO TO INIT-BH-ODAC-EXIT.
009000     MOVE 'O'                 TO BHW-TYPE.
009100     MOVE LOW-VALUES          TO WORK-BAL-HIST-ODAC.
009200     MOVE WMS-CONTROL-KEY     TO WBHO-CONTROLS.
009300     MOVE 'O'                 TO WBHO-TYPE.
009400     MOVE ZERO                TO WBHO-ENTRIES.
009500     MOVE WMS-ODAC-BAL-HIST-RET  TO BHW-MAX-RET.
009600     PERFORM INIT-BAL-HIST-BACKDATE THRU INIT-BH-BKDT-EXIT.
009700 INIT-BH-ODAC-EXIT.                                               9915845
009800     EXIT.                                                        9915845
009900 INIT-BH-SAV.
010000     MOVE 'F'                 TO BHW-SAV-FLAG.
010100     IF  WMS-SAVINGS-TRLR EQUAL '0'
010200     OR  WMS-SAV-BAL-HIST EQUAL '0'
010300         GO TO INIT-BH-SAV-EXIT.
010305     IF  WMS-SAV-TIER-PTR GREATER +0                              0316967
010310         MOVE 'T'                 TO BHW-SAV-FLAG                 0316967
010315         IF  WMS-CONTROL-KEY  EQUAL  WBHT-CONTROLS                0316967
010317             GO TO INIT-BH-SAV-EXIT                               0316967
010320         ELSE                                                     0316967
010325             MOVE 'T'                 TO BHW-TYPE                 0316967
010330             MOVE LOW-VALUES          TO WORK-BAL-HIST-SAV        0316967
010335             MOVE WMS-CONTROL-KEY     TO WBHT-CONTROLS            0316967
010340             MOVE 'T'                 TO WBHT-TYPE                0316967
010345             MOVE ZERO                TO WBHT-ENTRIES             0316967
010350             MOVE WMS-SAV-BAL-HIST-RET TO BHW-MAX-RET             0316967
010355             PERFORM INIT-BAL-HIST-BACKDATE THRU INIT-BH-BKDT-EXIT0316967
010360             GO TO INIT-BH-SAV-EXIT.                              0316967
010400     MOVE 'R'                 TO BHW-SAV-FLAG                     0316967
010500     IF  WMS-CONTROL-KEY  EQUAL  WBHS-CONTROLS
010600         GO TO INIT-BH-SAV-EXIT.
010700     MOVE 'S'                 TO BHW-TYPE.
010800     MOVE LOW-VALUES          TO WORK-BAL-HIST-SAV.
010900     MOVE WMS-CONTROL-KEY     TO WBHS-CONTROLS.
011000     MOVE 'S'                 TO WBHS-TYPE.
011100     MOVE ZERO                TO WBHS-ENTRIES.
011200     MOVE WMS-SAV-BAL-HIST-RET   TO BHW-MAX-RET.
011300     PERFORM INIT-BAL-HIST-BACKDATE THRU INIT-BH-BKDT-EXIT.
011400 INIT-BH-SAV-EXIT.                                                9915845
011500     EXIT.                                                        9915845
011600 INIT-BH-END.
011700     GO TO INIT-BAL-HIST-EXIT.
011800
011900 INIT-BAL-HIST-BACKDATE.
012000*--------------------------------------------------------------*
012100*    BACKDATED NEW ACCOUNTS WILL CAUSE THE BALANCE HISTORY     *
012200*    RECORDS TO BE INITIALIZED BACK TO THE OPENING DATE        *
012300*    (NOT TO EXCEED THE MAXIMUM RETENTION DAYS).               *
012400*--------------------------------------------------------------*
012500     IF  NEW-MASTER-SW NOT EQUAL '1'
012600         GO TO INIT-BH-BKDT-EXIT.
012700     IF  WMS-DATE-OPENED EQUAL WBC-CAPTURE-DATE                   0266741
012800                            OR SPACES
012900                            OR ZEROS
013000         GO TO INIT-BH-BKDT-EXIT.
013100     IF  BAL-HIST-WORK-FLAGS EQUAL 'FFF'
013200         GO TO INIT-BH-BKDT-EXIT.
013300     MOVE SPACES              TO HOLD-BH-DATE.
013400     MOVE ZERO                TO ACCRUAL-BALANCE.
013500     MOVE WBC-CAPTURE-DATE    TO DT-HIGH-DATE.                    0266741
013600     MOVE WMS-DATE-OPENED     TO DT-LOW-DATE.
013700     CALL 'SIDIF1' USING DATE-AREA.
013800     IF  RET-DAYS EQUAL ZERO
013900         GO TO INIT-BH-BKDT-EXIT.
014000     MOVE RET-DAYS            TO HOLD-BKDT-DAYS.
014100     IF  BHW-MAX-RET LESS THAN   RET-DAYS
014200         MOVE BHW-MAX-RET     TO RET-DAYS
014300                                 HOLD-BKDT-DAYS                   0727455
014400         CALL 'SIDLO'  USING     DATE-AREA.
014500     MOVE DT-LOW-DATE         TO HOLD-BKDT-DATE.
014600     MOVE '2'                 TO INTEREST-ADJUST-CODE.
014700*  BACKDATED IOD/MMDA ACCOUNT
014800     IF  BHW-TYPE EQUAL 'I' OR 'M'
014900         MOVE 'N'             TO IOD-FLAG
015000         MOVE 'I'             TO INTEREST-ADJ-SUBCODE
015100         PERFORM ADJUST-ACCRUED-INTEREST THRU END-INT-ADJUST
015200         MOVE 'F'             TO IOD-FLAG
015300         MOVE 'N'             TO INTEREST-ADJ-SUBCODE.
015400*  BACKDATED ODAC ACCOUNT
015500     IF  BHW-TYPE EQUAL 'O'
015600         MOVE 'F'             TO IOD-FLAG
015700         MOVE 'I'             TO INTEREST-ADJ-SUBCODE
015800         PERFORM X0900-TRAN-ADJUST THRU X0900-RHR-OD-EXIT
015900         MOVE 'N'             TO INTEREST-ADJ-SUBCODE.
016000*  BACKDATED SAV ACCOUNT
016100     IF  BHW-TYPE EQUAL 'S' OR 'T'                                0316967
016200         MOVE 'F'             TO IOD-FLAG
016300         MOVE 'I'             TO INTEREST-ADJ-SUBCODE
016400         PERFORM ADJUST-ACCRUED-INTEREST THRU END-INT-ADJUST
016500         MOVE 'N'             TO INTEREST-ADJ-SUBCODE.
016600     MOVE SPACE               TO BHW-TYPE.
016700 INIT-BH-BKDT-EXIT.                                               9915845
016800     EXIT.                                                        9915845
016900 INIT-BAL-HIST-EXIT.                                              9915845
017000     EXIT.                                                        9915845
