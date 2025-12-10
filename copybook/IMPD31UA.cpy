*     * FO5238 * 06/26/11 PROYECTO REBORN
000001*----------------------------------------------------------------*
000002*         IM31 PROCEDURE COPYBOOK - INSERT AFTER LINE 815800     *
000003*                                                                *
000004*  NOTE:  DO NOT RENUMBER/RESEQUENCE!! CODE TAKEN FROM IM31      *
000005*----------------------------------------------------------------*
000006*                **  HISTORY OF REVISION **                      *
000007*  DATE     DESCRIPTION                                  CHNGID  *
000008*  -------- -------------------------------------------  --------*
000009*                                                                *
000010* 06/01/12 CODE TRANSFERRED FROM IM31 DUE TO LIBRARIAN   U06ENE  *
000011*          ISSUE WHICH CAN HANDLE MAX 32,000 LINES               *
000012*                                                                *
000099*----------------------------------------------------------------*
702101 S3050-WRITE-MEMO-RECORD.                                         1003624
702102     IF  MASTER-GROUPING-SW EQUAL 'N'                             1003624
702103         GO TO S3050-WRITE-MEMO.                                  1003624
702104*                                                                 1003624
702105     IF  WMS-EXC-CODE NOT EQUAL '0'                               1003624
702106         GO TO S3050-WRITE-GROUP-OL-MEMO.                         1003624
702107*                                                                 1003624
702108     MOVE 'L'          TO I-O-CONTROL-OPERATOR.                   1003624
702109     MOVE 'IMMEMMV'    TO I-O-BASE-PROTOTYPE                      1003624
702110                          I-O-BASE-FILE-NAME.                     1003624
702111     CALL 'SISSLOP' USING SIWSLK-STACK-TABLE                      1003624
702112                          I-O-BASE                                1003624
702113                          I-O-CONTROL-AREA                        1003624
702114                          IM-MEMO-OL-AREA.                        1003624
702115     GO TO S3050-EXIT.                                            1003624
702116*                                                                 1003624
702117 S3050-WRITE-GROUP-OL-MEMO.                                       1003624
702118     IF  SKIP-READ-MEMO EQUAL '0'                                 1003624
702119         MOVE 'L' TO I-O-CONTROL-OPERATOR                         1003624
702120     ELSE                                                         1003624
702121         MOVE 'L' TO I-O-CONTROL-OPERATOR.                        1003624
702122     MOVE 'IMMEMMV'          TO I-O-BASE-PROTOTYPE                1003624
702123                                I-O-BASE-FILE-NAME.               1003624
702124     MOVE SV-MASTER-GROUP-NO TO I-O-BASE-SEG-NO.                  1003624
702125     CALL 'SISSRTN' USING I-O-BASE                                1003624
702126                          I-O-CONTROL-AREA                        1003624
702127                          IM-MEMO-OL-AREA.                        1003624
702128 S3050-WRITE-GROUP-MEMO-END.                                      1003624
702129     GO TO S3050-EXIT.                                            1003624
702130 S3050-WRITE-MEMO.                                                1003624
702131     IF  SKIP-READ-MEMO EQUAL '0'                                 1003624
702132         MOVE 'L' TO I-O-CONTROL-OPERATOR                         1003624
702133     ELSE                                                         1003624
702134         MOVE 'L' TO I-O-CONTROL-OPERATOR.                        1003624
702135     CALL 'IMMEMMV' USING I-O-CONTROL-AREA                        1003624
702136                          IM-MEMO-OL-AREA.                        1003624
702137 S3050-EXIT. EXIT.                                                1003624
702150     SKIP3
702151*----------------------------------------------------------------*IM008
702152*    S3100 WILL SAVE INFORMATION TO THE EXCEPTION MASTER FILE    *IM008
702153*    FOR ANY INFORMATION THAT IS PRESENTLY REQUIRED IN THE       *IM008
702154*    STATEMENT PROGRAM.  THE VALUES FOR SAVE-MASTER-FLAG ARE:    *IM008
702155*       0 = FIRST TIME THRU.  CHECK CONDITIONS FOR MASTER TO     *IM008
702156*           SAVED.                                               *IM008
702157*       1 = INFORMATION REQUIRED IN STATEMENTS IS NEEDED         *IM008
702158*           AND THE MASTER AREA WILL BE SAVED (UNCOMPRESSED).    *IM008
702159*       2 = NO TRAILER INFORMATION IS NEEDED.  SAVE SERVICE      *IM008
702160*           CHARGE INFORMATION TO THE FIXED PORTION OF THE       *IM008
702161*           MASTER RECORD (COMPRESSED).                          *IM008
702162*       3 = HEADER RECORD                                        *1004470
702163*----------------------------------------------------------------*1004470
702164 S3100.                                                           1004470
702165     IF  SAVE-MASTER-FLAG EQUAL '3'                               1004470
702166         GO TO S3199-EXIT.                                        1004470
702167     IF  SAVE-MASTER-FLAG EQUAL '1'                               1004470
702168         GO TO S3120-RESTORE.                                     1004470
702169     IF  SAVE-MASTER-FLAG EQUAL '2'                               1004470
702170         GO TO S3130-SERV-CHG.                                    1004470
702171     IF  SVCH-TODAY EQUAL '1'                                     1004470
702172         IF  WMS-SAVINGS-TRLR EQUAL '1'                           1004470
702173         OR  WMS-EXT-SC-DATA-TRLR EQUAL '1'                       1004470
702174         OR  WMS-OD-ACCRUAL-TRLR EQUAL '1'                        1004470
702175             GO TO S3110-MOVE.                                    1004470
702176     IF  PRIME-RATE-CHG EQUAL '1'                                 1004470
702177         GO TO S3110-MOVE.                                        1004470
702178     MOVE '2' TO SAVE-MASTER-FLAG.                                1004470
702179     GO TO S3199-EXIT.                                            1004470
702180 S3110-MOVE.                                                      1004470
702181     MOVE MASTER-AREA TO SAVE-MASTER-AREA.                        1004470
702182     MOVE  '1' TO SAVE-MASTER-FLAG.                               1004470
702183     IF  PRIME-RATE-CHG EQUAL '1'                                 1004470
702184         MOVE CYCL-DATE-LAST-BILLING TO WMS-DATE-LAST-BILLING     1004470
702185         MOVE CYCL-LN-BILL-CY-DAYS   TO WMS-LN-BILL-CY-DAYS.      1004470
702186     GO TO S3199-EXIT.                                            1003343
702187 S3120-RESTORE.                                                   1003343
702188     MOVE '0' TO SAVE-CMA-IND SAVE-CMA-USE.                       9916431
702189     MOVE SAVE-MASTER-AREA TO MASTER-AREA.                        9916431
702190     IF  SVCH-TODAY EQUAL '1'                                     9916431
702191     AND WMS-EXT-SC-DATA-TRLR EQUAL '1'                           9916431
702192         MOVE SAVE-EXT-SC-REC TO WMS-EXTENDED-SERV-CHG-TRLR.      9916431
702193     IF  SVCH-TODAY EQUAL '1' AND WMS-SAVINGS-TRLR EQUAL '1'      2016653
702194         MOVE WMS-CMA-INDICATOR   TO SAVE-CMA-IND                 2016653
702195         MOVE WMS-CMA-BALANCE-USE TO SAVE-CMA-USE                 2016653
702196         MOVE SAVE-AGGR-DAYS      TO WMS-INT-AGGR-DAYS            2016653
702197         MOVE SAVE-SC-AGGR-DAYS   TO WMS-INT-SC-AGGR-DAYS         2016653
702198         MOVE SAVE-INT-AGGR-BAL   TO WMS-INT-SC-AGGR-BAL          9916431
702199         MOVE SAVE-INT-AGGR-COLL  TO WMS-INT-SC-AGGR-COLL         9916431
702200         MOVE SAVE-CMA-AGGR-DAYS  TO CMA-AGGR-DAYS                2016683
702201         IF  WMS-CMA-INDICATOR EQUAL '1'                          2016683
702202             PERFORM R4650.                                       9916431
702203     IF  ODAC-CHG-TODAY EQUAL '1'                                 9916431
702204     AND WMS-OD-ACCRUAL-TRLR EQUAL '1'                            9916431
702205         MOVE SAVE-ODAC-DAYS TO WMS-OD-ACR-AGGR-DAYS-CTD          9916431
702206         MOVE SAVE-ODAC-AGGR-BAL TO WMS-OD-ACR-AGGR-CTD.          9916431
702207     IF  PRIME-RATE-CHG EQUAL '1'                                 9916431
702208         MOVE SAVE-LOAN-STMT-CLEAR TO WMS-LOAN-STMT-CLEAR         9916431
702209         MOVE SAVE-LOAN-STATEMENT-INFO TO WMS-LOAN-STATEMENT-INFO 9916431
702210         MOVE SAVE-ANNUAL-RATE     TO WMS-ANNUAL-RATE             9916431
702211         MOVE SAVE-DAILY-RATE      TO WMS-DAILY-RATE              9916431
702212         MOVE SAVE-PRIME-RATE      TO WMS-PRIME-RATE              9916431
702213         MOVE SAVE-BEGIN-INT       TO WMS-BEGIN-INT               9916431
702214         MOVE SAVE-DATE-LAST-BILLING TO WMS-DATE-LAST-BILLING.    9916431
702215     IF  YEAR-CHANGE EQUAL '1'                                    9915949
702216         MOVE SAVE-RATE-TRAILER  TO WMS-RATE-TRAILER.             9915949
702217     CALL 'IMMSCP' USING MASTER-AREA.                             9915949
702218 S3130-SERV-CHG.                                                  9915949
702219     IF  SVCH-TODAY EQUAL '1'                                     9915949
702220         MOVE SAVE-EXC-14-REC TO WMS-SERVICE-CHARGE-DATA          9915949
702221         IF  WMS-SAVINGS-TRLR EQUAL '1'                           9915949
702222         AND SAVE-CMA-IND     EQUAL '1'                           9915949
702223             COMPUTE WMS-SC-AGGR-DDA = WMS-SC-AGGR-DDA +          2016683
702224                     (WMS-DDA-BAL * CMA-AGGR-DAYS) +              2016683
702225                     (CMA-DDA-PAST-BAL * CMA-PAST-AGGR-DAYS).     2016683
702226 S3199-EXIT.                                                      9915949
702227     EXIT.                                                        9915949
702231                                                                  IM008
702236 ADJ-OD-AGGR.                                                     IM008
702250     MOVE HOLD-BKDT-DATE TO DT-LOW-DATE.
702300     MOVE WBC-CAPTURE-DATE TO DT-HIGH-DATE.                       0266741
702350     PERFORM AGGR-CALC THRU AGGR-CALC-EXIT.
702360     IF  OD-BKDT-DAYS-FLAG EQUAL '1'                              0903490
702363         MOVE '0' TO OD-BKDT-DAYS-FLAG                            0903490
702370         MOVE RET-DAYS TO WMS-OD-ACR-AGGR-DAYS-CTD.               0903491
702400     IF  RET-DAYS GREATER THAN WMS-OD-ACR-AGGR-DAYS-CTD
702450         MULTIPLY TR-AMOUNT BY WMS-OD-ACR-AGGR-DAYS-CTD
702500             GIVING SAVE-AGGR
702520         IF  TR-TRAN-TYPE EQUAL '3' OR '4' OR '5' OR '8'          9915845
702550             SUBTRACT SAVE-AGGR FROM WMS-OD-ACR-AGGR-CTD          0902380
702570         ELSE                                                     0902380
702590             ADD SAVE-AGGR TO WMS-OD-ACR-AGGR-CTD                 0902380
702600     ELSE
702650         ADD AGG-AMT   TO WMS-OD-ACR-AGGR-CTD.
702700     MOVE AGG-AMT TO SAVE-AGGR.
702750     IF DT-L-MO NOT EQUAL WBC-CAPTURE-MO                          0266741
702800         MOVE '01' TO DT-L-DA
702850         MOVE WBC-CAPTURE-MO TO DT-L-MO                           0266741
702900         MOVE WBC-CAPTURE-YR TO DT-L-YR                           0266741
702950         PERFORM AGGR-CALC THRU AGGR-CALC-EXIT.
702960     IF  RET-DAYS GREATER THAN WMS-OD-ACR-AGGR-DAYS-MTD           0903491
702970         MOVE RET-DAYS TO WMS-OD-ACR-AGGR-DAYS-MTD.               0903491
703000     ADD AGG-AMT TO WMS-OD-ACR-AGGR-MTD.
703050     IF  WMS-OD-ACR-AGGR-CTD GREATER THAN +0.00
703100         MOVE ZERO TO WMS-OD-ACR-AGGR-CTD
703150                      WMS-OD-ACR-AGGR-DAYS-CTD.
703200     IF  WMS-OD-ACR-AGGR-MTD GREATER THAN +0.00
703250         MOVE ZERO TO WMS-OD-ACR-AGGR-MTD
703300                      WMS-OD-ACR-AGGR-DAYS-MTD.
703350 ADJ-OD-AGGR-EXIT.
703400     EXIT.
703450     SKIP3
703453 ADJ-KITE-AGGR.                                                   IM006
703456     MOVE HOLD-BKDT-DATE TO DT-LOW-DATE.                          IM006
703459     MOVE WBC-CAPTURE-DATE TO DT-HIGH-DATE.                       0266741
703462     CALL 'SIDIF1' USING DATE-AREA.                               IM006
703463     IF (RET-DAYS EQUAL TO ZERO) OR                               IM006
703464        (RET-DAYS LESS THAN ZERO)                                 IM006
703465         GO TO ADJ-KITE-AGGR-EXIT.                                IM006
703469     MOVE WBC-WEEK-DAY TO HOLD-WEEK-DAY.                          0266741
703470     SUBTRACT HOLD-WEEK-DAY FROM RET-DAYS GIVING HOLD-DAYS.       IM006
703471     IF  HOLD-DAYS NOT GREATER THAN ZERO                          IM006
703472         PERFORM CHECK-HOLIDAYS THRU CHECK-HOLIDAYS-EXIT          IM006
703473     ELSE                                                         IM006
703474         PERFORM CHECK-SUN THRU CHECK-HOLIDAYS-EXIT.              IM006
703475     IF (RET-DAYS EQUAL TO ZERO) OR                               IM006
703476        (RET-DAYS LESS THAN ZERO)                                 IM006
703477         GO TO ADJ-KITE-AGGR-EXIT.                                IM006
703479     IF  RET-DAYS GREATER THAN +6                                 IM006
703480         MOVE +6 TO RET-DAYS.                                     IM006
703481     MOVE +0 TO X.                                                IM006
703482     SUBTRACT +7 FROM RET-DAYS GIVING X.                          IM006
703484     MULTIPLY -1 BY X.                                            IM006
703489     IF  TR-80-TYPE EQUAL '06'                                    IM006
703490         IF  TR-TRAN-TYPE EQUAL '1' OR '2'                        9915845
703491             SUBTRACT TR-ITEM-COUNT FROM WMS-KITE-TODAY-DR-NO (X) IM006
703492             SUBTRACT TR-AMOUNT FROM WMS-KITE-TODAY-DR-AMT (X)    IM006
703493             GO TO ADJ-KITE-AGGR-EXIT                             IM006
703494         ELSE                                                     IM006
703495             SUBTRACT TR-ITEM-COUNT FROM WMS-KITE-TODAY-CR-NO (X) IM006
703496             SUBTRACT TR-AMOUNT FROM WMS-KITE-TODAY-CR-AMT (X)    IM006
703497             GO TO ADJ-KITE-AGGR-EXIT.                            IM006
703498     IF  TR-TRAN-TYPE EQUAL '1' OR '2'                            9915845
703499         ADD TR-ITEM-COUNT TO WMS-KITE-TODAY-CR-NO (X)            IM006
703500         ADD TR-AMOUNT TO WMS-KITE-TODAY-CR-AMT (X)               IM006
703501     ELSE                                                         IM006
703502         ADD TR-ITEM-COUNT TO WMS-KITE-TODAY-DR-NO (X)            IM006
703503         ADD TR-AMOUNT TO WMS-KITE-TODAY-DR-AMT (X).              IM006
703504 ADJ-KITE-AGGR-EXIT.                                              IM006
703505     EXIT.                                                        IM006
703506     SKIP3                                                        IM006
703507 ADJ-SC-AGGR.                                                     IM006
703550     IF  TR-TRAN-TYPE EQUAL '5' OR '6'                            9915845
703600         MOVE TR-LOAN-ORIG-DATE TO DT-LOW-DATE
703650     ELSE
703700         MOVE HOLD-BKDT-DATE TO DT-LOW-DATE.
703750     MOVE WBC-CAPTURE-DATE TO DT-HIGH-DATE.                       0266741
703800     PERFORM AGGR-CALC THRU AGGR-CALC-EXIT.
703805******************************************************************IM007
703810***  THE "BACKDATE DAYS FLAGS" ARE SET TO '1' FOR BACKDATED NEW  *9715519
703815***  ACCOUNTS/SAVINGS TRAILERS.  THE FIRST BACKDATED DDA/SAV     *9715519
703820***  TRANSACTION SETS THE SERVICE CHARGE, MONTH-TO-DATE, AND     *9715519
703825***  YEAR-TO-DATE AGGREGATES.                                    *9715519
703830******************************************************************IM007
703832     IF  TR-TRAN-TYPE EQUAL '1' OR '2' OR '3' OR '4'              9915845
703835         IF  BKDT-DAYS-FLAG EQUAL '1'                             9715519
703845             MOVE RET-DAYS TO WMS-SC-AGGR-DAYS                    9715519
703850             IF  BKDT-DAYS-FLAG EQUAL '1'                         9715519
703856             AND WMS-MARKET-TRLR EQUAL '1'                        9715519
703862                 MOVE RET-DAYS TO WMS-HST-AGGR-DAYS (1).          9715519
703865     IF  TR-TRAN-TYPE EQUAL '7' OR '8'                            9915845
703868         IF  SAV-BKDT-DAYS-FLAG EQUAL '1'                         9715519
703874         AND WMS-SAVINGS-TRLR EQUAL '1'                           9715519
703880             MOVE RET-DAYS TO WMS-INT-AGGR-DAYS                   9916070
703883                              WMS-INT-SC-AGGR-DAYS.               9916070
703886     IF  TR-TRAN-TYPE EQUAL '5'                                   9915845
703900         IF  RET-DAYS GREATER THAN WMS-LOAN-AGGR-DAYS
703950             MULTIPLY TR-AMOUNT BY WMS-LOAN-AGGR-DAYS
704000                 GIVING SAVE-AGGR
704050             SUBTRACT SAVE-AGGR FROM WMS-CURR-AGGR1               0902380
704100         ELSE
704150             ADD AGG-AMT TO WMS-CURR-AGGR1.                       0902380
704160     IF  TR-TRAN-TYPE EQUAL '6'                                   9915845
704170         IF  RET-DAYS GREATER THAN WMS-LOAN-AGGR-DAYS             0902380
704180             MULTIPLY TR-AMOUNT BY WMS-LOAN-AGGR-DAYS             0902380
704190                 GIVING SAVE-AGGR                                 0902380
704200             ADD SAVE-AGGR TO WMS-CURR-AGGR1                      0902380
704210         ELSE                                                     0902380
704220             ADD AGG-AMT TO WMS-CURR-AGGR1.                       0902380
704230     IF  TR-TRAN-TYPE EQUAL '5' OR '6'                            9915845
704240         MOVE AGG-AMT TO SAVE-AGGR                                0902380
704250         MOVE RET-DAYS TO SAVE-AGGR-DAYS                          0902380
704260             GO TO ADJ-MTD-AGGR.                                  0902380
704270     IF  RET-DAYS GREATER THAN WMS-SC-AGGR-DAYS                   0902380
704280         MULTIPLY TR-AMOUNT BY WMS-SC-AGGR-DAYS                   0902380
704290             GIVING SAVE-AGGR                                     0902380
704300         IF  TR-TRAN-TYPE EQUAL '3' OR '4' OR '8'                 9915845
704310             SUBTRACT SAVE-AGGR FROM WMS-SC-AGGR-BAL              0902380
704320                                     WMS-SC-AGGR-COLL             0902380
704330                                     WMS-SC-AGGR-CUST-COLL        0902380
704340         ELSE                                                     0902380
704350             ADD SAVE-AGGR TO WMS-SC-AGGR-BAL                     0902380
704360                              WMS-SC-AGGR-COLL                    0902380
704370                              WMS-SC-AGGR-CUST-COLL               0902380
704380     ELSE                                                         0902380
704390         ADD AGG-AMT TO WMS-SC-AGGR-BAL                           0902380
704400                        WMS-SC-AGGR-COLL                          0902380
704450                        WMS-SC-AGGR-CUST-COLL.                    0902380
704452     IF  TR-TRAN-TYPE EQUAL '1' OR '2' OR '3' OR '4'              9916070
704454         IF  RET-DAYS GREATER THAN WMS-SC-AGGR-DAYS               9916070
704456             IF  TR-TRAN-TYPE EQUAL '3' OR '4'                    9916070
704458                 SUBTRACT SAVE-AGGR FROM WMS-SC-AGGR-DDA          9916070
704460             ELSE                                                 9916070
704462                 ADD SAVE-AGGR TO WMS-SC-AGGR-DDA                 9916070
704464         ELSE                                                     9916070
704466             ADD AGG-AMT TO WMS-SC-AGGR-DDA.                      9916070
704470     IF  TR-TRAN-TYPE EQUAL '7' OR '8'                            9915845
704500         PERFORM CALC-SAV-AGGR THRU CALC-SAV-AGGR-EXIT.           0903490
704550     MOVE AGG-AMT TO SAVE-AGGR.
704570     MOVE RET-DAYS TO SAVE-AGGR-DAYS.                             IM007
704572     IF  TR-TRAN-TYPE EQUAL '7' OR '8'                            9916070
704574         IF  RET-DAYS GREATER THAN WMS-INT-SC-AGGR-DAYS           9916070
704576             MULTIPLY TR-AMOUNT BY WMS-INT-SC-AGGR-DAYS           9916070
704578                 GIVING SAVE-AGGR                                 9916070
704580             IF  TR-TRAN-TYPE EQUAL '8'                           9916070
704582                 SUBTRACT SAVE-AGGR FROM WMS-INT-SC-AGGR-BAL      9916070
704584                                         WMS-INT-SC-AGGR-COLL     9916070
704586             ELSE                                                 9916070
704588                 ADD SAVE-AGGR TO WMS-INT-SC-AGGR-BAL             9916070
704590                                  WMS-INT-SC-AGGR-COLL            9916070
704592         ELSE                                                     9916070
704594             ADD AGG-AMT TO WMS-INT-SC-AGGR-BAL                   9916070
704596                            WMS-INT-SC-AGGR-COLL.                 9916070
704600 ADJ-MTD-AGGR.
704650     IF DT-L-MO NOT EQUAL WBC-CAPTURE-MO                          0266741
704700         MOVE '01' TO DT-L-DA
704750         MOVE WBC-CAPTURE-MO TO DT-L-MO                           0266741
704800         MOVE WBC-CAPTURE-YR TO DT-L-YR                           0266741
704850         PERFORM AGGR-CALC THRU AGGR-CALC-EXIT.
704855     IF  TR-TRAN-TYPE EQUAL '1' OR '2' OR '3' OR '4'              9915845
704860         IF  BKDT-DAYS-FLAG EQUAL '1'                             9715519
704880             MOVE RET-DAYS TO WMS-MTD-AGGR-DAYS.                  9715519
704900     IF  TR-TRAN-TYPE EQUAL '5' OR '6'                            9915845
704950         ADD AGG-AMT TO WMS-MTD-AGGR1
704960         COMPUTE WMS-LOAN-YTD-AGGR ROUNDED = WMS-LOAN-YTD-AGGR    IM006
704970                                           + AGG-AMT              IM006
705000     ELSE
705050         ADD AGG-AMT TO WMS-MTD-AGGR-BAL WMS-MTD-AGGR-COLL.
705055     IF  TR-TRAN-TYPE EQUAL '7' OR '8'                            9915845
705060     AND SAV-BKDT-DAYS-FLAG EQUAL '1'                             9715519
705070     AND WMS-SAVINGS-TRLR EQUAL '1'                               0903490
705080         MOVE RET-DAYS TO WMS-INT-MTD-ACCR-DAYS.                  0903490
705100     IF  TR-TRAN-TYPE EQUAL '7' OR '8'                            9915845
705110         ADD AGG-AMT TO WMS-INT-MTD-AGGR                          IM003
705120         IF  WMS-MARKET-TRLR EQUAL '1'                            IM003
705130         AND (WMS-HISTORY-OPT-BAL EQUAL '2' OR '3')               IM003
705140             ADD AGG-AMT TO WMS-HST-AGGR-BAL.                     IM003
705150     IF  TR-TRAN-TYPE LESS THAN '5'                               9915845
705160     AND WMS-MARKET-TRLR EQUAL '1'                                IM003
705170     AND (WMS-HISTORY-OPT-BAL EQUAL '1' OR '3')                   IM003
705180         ADD AGG-AMT TO WMS-HST-AGGR-BAL.                         IM003
705185     IF  TR-TRAN-TYPE NOT EQUAL '5' AND '6'                       9915845
705190         ADD AGG-AMT TO WMS-MTD-AGGR-CUST-COLL.                   IM003
705198 ADJ-MTD-AGGR-EXIT.                                               9915845
705199     EXIT.                                                        9915845
705240 ADJ-YTD-AGGR.                                                    0727675
705250     IF  HOLD-BKDT-YR EQUAL WBC-CAPTURE-YR                        0266741
705270         MOVE SAVE-AGGR-DAYS TO RET-DAYS                          IM007
705300         MOVE SAVE-AGGR TO AGG-AMT.
705350     ADD AGG-AMT TO WMS-YTD-AGGR-BAL.
705360     IF  (BKDT-DAYS-FLAG EQUAL '1')                               2016472
705365     AND (TR-TRAN-TYPE EQUAL '1' OR '2' OR '3' OR '4')            2016472
705390         MOVE RET-DAYS TO WMS-YTD-AGGR-DAYS.                      IM007
705391     IF  TR-TRAN-TYPE EQUAL '7' OR '8'                            9915845
705392         ADD AGG-AMT TO WMS-INT-YTD-AGGR-BALS.                    0903490
705393     IF  (SAV-BKDT-DAYS-FLAG EQUAL '1')                           2016472
705394     AND (TR-TRAN-TYPE EQUAL '7' OR '8')                          2016472
705395     AND (WMS-SAVINGS-TRLR EQUAL '1')                             2016472
705396         MOVE RET-DAYS TO WMS-INT-YTD-AGGR-DAYS.                  9715519
705397     PERFORM CALC-TIS-AGGR THRU CALC-TIS-AGGR-EXIT.               1004015
705398     IF  TR-TRAN-TYPE EQUAL '1' OR '2' OR '3' OR '4'              9915845
705399         MOVE '0' TO BKDT-DAYS-FLAG.                              9715519
705400     IF  TR-TRAN-TYPE EQUAL '7' OR '8'                            9915845
705401         MOVE '0' TO SAV-BKDT-DAYS-FLAG.                          9715519
705402 AGGR-CALC.                                                       9715519
705450     CALL 'SIDIF1' USING DATE-AREA.
705500     IF WBC-AGGREGATE EQUAL 'C' OR                                0266741
705550         TR-TRAN-TYPE EQUAL '5' OR '6'                            9915845
705600         GO TO AGGR-AMT.
705650     IF RET-DAYS NOT GREATER THAN ZERO
705700         MOVE ZERO TO AGG-AMT
705750         GO TO AGGR-CALC-EXIT.
705800     MOVE WBC-WEEK-DAY TO HOLD-WEEK-DAY.                          0266741
705850     IF  RET-DAYS NOT LESS HOLD-WEEK-DAY                          IM007
705858         SUBTRACT HOLD-WEEK-DAY FROM RET-DAYS GIVING HOLD-DAYS    IM007
705866     ELSE                                                         IM007
705874         MOVE RET-DAYS TO HOLD-DAYS                               IM007
705882         GO TO CHECK-HOLIDAYS.                                    IM007
705900     IF HOLD-DAYS NOT GREATER THAN ZERO
705950         GO TO CHECK-HOLIDAYS.
706000 CHECK-SUN.
706050     SUBTRACT +1 FROM HOLD-DAYS.
706100     IF  HOLD-DAYS LESS THAN ZERO
706150         GO TO CHECK-EXIT.
706200     DIVIDE HOLD-DAYS BY +7 GIVING TOT-NO-WKEND.
706250     ADD +1 TO TOT-NO-WKEND.
706300     SUBTRACT TOT-NO-WKEND FROM RET-DAYS.
706350 CHECK-EXIT. EXIT.
706400 CHECK-SAT.
706450     MOVE ZERO TO TOT-NO-WKEND.
706500     PERFORM CHECK-SUN THRU CHECK-EXIT.                           IM007
706600 CHECK-HOLIDAYS.
706650     SUBTRACT HOLD-BKDT-HOL FROM RET-DAYS.
706660 CHECK-HOLIDAYS-EXIT.                                             IM006
706670     EXIT.                                                        IM006
706700 AGGR-AMT.
706750     IF RET-DAYS GREATER THAN +0
706800         MULTIPLY TR-AMOUNT BY RET-DAYS GIVING AGG-AMT
706850     ELSE MOVE ZERO TO AGG-AMT.
706900     IF  TR-TRAN-TYPE EQUAL '3' OR '4' OR '5' OR '8'              9915845
706910         MULTIPLY -1 BY AGG-AMT.                                  1004795
706920 AGGR-CALC-EXIT.                                                  1004795
706930     EXIT.                                                        1004795
706940                                                                  1004795
706950*--------------------------------------------------------------*  1004795
706960*    CALCULATE TRUTH-IN-SAVINGS AGGREGATES BY CALENDAR DAYS    *  1004795
706970*--------------------------------------------------------------*  1004795
706980 CALC-TIS-AGGR.                                                   1004795
706981*----------------------------------------------------------------*1105504
706982*    BYPASS ALL AGGREGATE CALCULATIONS DURING STATIC RUN.        *1105504
706983*----------------------------------------------------------------*1105504
706984     IF  WBC-RUN-FLAG EQUAL 'I'                                   0266741
706985     AND WBC-ACCRUAL-DAYS EQUAL ZERO                              0266741
706986         GO TO CALC-TIS-AGGR-EXIT.                                1105504
706990     IF  TR-TRAN-TYPE EQUAL '5' OR '6'                            9915845
706995         GO TO CALC-TIS-AGGR-EXIT.                                1004795
707000     IF  WMS-STATUS EQUAL '04'                                    1004795
707005         PERFORM CALC-TIS-DAYS THRU CALC-TIS-DAYS-EXIT.           1004795
707009     MOVE HOLD-BKDT-DATE TO DT-LOW-DATE.                          1004015
707010     MOVE WBC-CAPTURE-DATE TO DT-HIGH-DATE.                       0266741
707011     CALL 'SIDIF1' USING DATE-AREA.                               1004015
707012     PERFORM AGGR-AMT THRU AGGR-CALC-EXIT.                        1004015
707013     IF  TR-TRAN-TYPE EQUAL '7' OR '8'                            9915845
707014         GO TO CALC-TIS-SAV-AGGR.                                 1004511
707015     IF  BKDT-DAYS-FLAG EQUAL '1'                                 1004511
707016     AND (WMS-IOD-RATE-PTR GREATER THAN +0                        1004511
707017     OR  WMS-HIFI-INDICATOR GREATER THAN +0)                      1004511
707018         MOVE RET-DAYS TO WMS-IOD-TIS-AGGR-DAYS.                  1004511
707019     IF  WMS-IOD-RATE-PTR EQUAL +0                                1004511
707020     AND WMS-HIFI-INDICATOR EQUAL +0                              1004511
707021         GO TO CALC-TIS-AGGR-EXIT.                                1004511
707022     IF  RET-DAYS GREATER THAN WMS-IOD-TIS-AGGR-DAYS              1004511
707023         MULTIPLY TR-AMOUNT BY WMS-IOD-TIS-AGGR-DAYS              1004511
707024                  GIVING SAVE-AGGR                                1004511
707025         IF  TR-TRAN-TYPE EQUAL '3' OR '4'                        9915845
707026             SUBTRACT SAVE-AGGR FROM WMS-IOD-TIS-AGGR-BAL         1004511
707027         ELSE                                                     1004511
707028             ADD SAVE-AGGR TO WMS-IOD-TIS-AGGR-BAL                1004511
707029     ELSE                                                         1004511
707030         ADD AGG-AMT TO WMS-IOD-TIS-AGGR-BAL.                     1004511
707031     GO TO CALC-TIS-AGGR-EXIT.                                    1004511
707032                                                                  1004511
707033 CALC-TIS-SAV-AGGR.                                               1004511
707034     IF  SAV-BKDT-DAYS-FLAG EQUAL '1'                             9715519
707035         IF  WMS-SAVINGS-TRLR EQUAL '1'                           1004511
707036             MOVE RET-DAYS TO WMS-INT-TIS-AGGR-DAYS.              1004511
707037     IF  RET-DAYS GREATER THAN WMS-INT-TIS-AGGR-DAYS              1004015
707038         MULTIPLY TR-AMOUNT BY WMS-INT-TIS-AGGR-DAYS              1004015
707039                  GIVING SAVE-AGGR                                1004015
707040         IF  TR-TRAN-TYPE EQUAL '8'                               9915845
707041             SUBTRACT SAVE-AGGR FROM WMS-INT-TIS-AGGR-BAL         1004015
707042         ELSE                                                     1004015
707043             ADD SAVE-AGGR TO WMS-INT-TIS-AGGR-BAL                1004015
707044     ELSE                                                         1004015
707045         ADD AGG-AMT TO WMS-INT-TIS-AGGR-BAL.                     1004015
707046 CALC-TIS-AGGR-EXIT.                                              1004015
707047     EXIT.                                                        1004015
707053                                                                  0903490
707056 CALC-SAV-AGGR.                                                   0903490
707059     IF  RET-DAYS GREATER THAN WMS-INT-AGGR-DAYS                  0903490
707062         MULTIPLY TR-AMOUNT BY WMS-INT-AGGR-DAYS                  0903490
707065         GIVING SAVE-AGGR                                         0903490
707068         IF  TR-TRAN-TYPE EQUAL '8'                               9915845
707071             SUBTRACT SAVE-AGGR FROM WMS-INT-AGGR-BAL             0903490
707074         ELSE                                                     0903490
707077             ADD SAVE-AGGR TO WMS-INT-AGGR-BAL                    0903490
707080     ELSE                                                         0903490
707083         ADD AGG-AMT TO WMS-INT-AGGR-BAL.                         0903490
707086 CALC-SAV-AGGR-EXIT.                                              0903490
707089     EXIT.                                                        0903490
707092                                                                  0903490
707095 CALC-TIS-DAYS.                                                   1004795
707098*                                                                 1004795
707101*  ADJUST TISA AGGREGATE DAYS IF ACCOUNT WAS IN CLOSED STATUS  *  1004795
707104*  AND A BACKDATED TRANSACTION REACTIVATES THE ACCOUNT.        *  1004795
707107*                                                                 1004795
707110     MOVE HOLD-BKDT-DATE         TO DT-HIGH-DATE.                 1004795
707113     MOVE WMS-DATE-LAST-STMT     TO DT-LOW-DATE.                  1004795
707116                                                                  1004795
707119     CALL 'SIDIF1'            USING DATE-AREA.                    1004795
707122                                                                  1004795
707125     IF  RET-DAYS GREATER THAN ZERO                               1004795
707128         GO TO CALC-TIS-COMPARE-CLOSED-DATE.                      1004795
707131                                                                  1004795
707134     GO TO CALC-TIS-DAYS-SINCE-LAST-STMT.                         1004795
707137                                                                  1004795
707140 CALC-TIS-COMPARE-CLOSED-DATE.                                    1004795
707143*                                                                 1004795
707146*  IF BACKDATE IS AFTER LAST STATEMENT DATE COMPARE THE BACKDATE* 1004795
707149*  DATE TO THE DATE THE ACCOUNT CLOSED.                         * 1004795
707152*                                                                 1004795
707155     MOVE HOLD-BKDT-DATE         TO DT-HIGH-DATE.                 1004795
707158     MOVE WMS-CLOSED-DA          TO DT-L-DA.                      1004795
707161     MOVE WMS-CLOSED-MO          TO DT-L-MO.                      1004795
707164     MOVE WMS-CLOSED-YR          TO DT-L-YR.                      1004795
707167                                                                  1004795
707170     CALL 'SIDIF1'            USING DATE-AREA.                    1004795
707173                                                                  1004795
707176     IF  RET-DAYS GREATER THAN ZERO                               1004795
707179         GO TO CALC-TIS-ADJUST-DAYS.                              1004795
707182                                                                  1004795
707185 CALC-TIS-DAYS-SINCE-LAST-STMT.                                   1004795
707188*                                                                 1004795
707191*--------------------------------------------------------------*  1004795
707194*  IF THE BACKDATE DATE IS PRIOR TO OR ON THE LAST STATEMENT   *  1004795
707197*  DATE OR PRIOR TO OR ON THE DATE THE ACCOUNT CLOSED THEN TISA*  1004795
707200*  DAYS SHOULD BE THE NUMBER OF DAYS SINCE THE LAST STATEMENT  *  1004795
707203*  FROM TODAY'S PROCESSING DATE.                               *  1004795
707206*--------------------------------------------------------------*  1004795
707209*                                                                 1004795
707212     MOVE WMS-DATE-LAST-STMT     TO DT-LOW-DATE                   1004795
707215     MOVE WBC-CAPTURE-DATE       TO DT-HIGH-DATE                  0266741
707218     CALL 'SIDIF1'            USING DATE-AREA                     1004795
707221                                                                  1004795
707224     SUBTRACT +1 FROM RET-DAYS.                                   1004795
707227                                                                  1004795
707230     IF  TR-TRAN-TYPE EQUAL '7' OR '8'                            9915845
707233         MOVE RET-DAYS           TO WMS-INT-TIS-AGGR-DAYS         1004795
707236     ELSE                                                         1004795
707239         MOVE RET-DAYS           TO WMS-IOD-TIS-AGGR-DAYS.        1004795
707242                                                                  1004795
707245     GO TO CALC-TIS-DAYS-EXIT.                                    1004795
707248                                                                  1004795
707251 CALC-TIS-ADJUST-DAYS.                                            1004795
707254*                                                                 1004795
707257*--------------------------------------------------------------*  1004795
707260*  ADJUST TISA AGGREGATE DAYS BY THE NUMBER OF DAYS FROM THE   *  1004795
707263*  BACKDATE DATE TO THE CURRENT PROCESS DATE.  THIS IS DONE IF *  1004795
707266*  THE BACKDATE DATE IS AFTER THE LAST STATEMENT DATE AND AFTER*  1004795
707269*  THE DATE THE ACCOUNT CLOSED.                                *  1004795
707272*--------------------------------------------------------------*  1004795
707275*                                                                 1004795
707278     MOVE HOLD-BKDT-DATE         TO DT-LOW-DATE.                  1004795
707281     MOVE WBC-CAPTURE-DATE       TO DT-HIGH-DATE                  0266741
707284     CALL 'SIDIF1'            USING DATE-AREA                     1004795
707287                                                                  1004795
707296     IF  TR-TRAN-TYPE EQUAL '7' OR '8'                            9915845
707299         ADD  RET-DAYS           TO WMS-INT-TIS-AGGR-DAYS         1004795
707302     ELSE                                                         1004795
707305         ADD  RET-DAYS           TO WMS-IOD-TIS-AGGR-DAYS.        1004795
707308                                                                  1004795
707311 CALC-TIS-DAYS-EXIT.                                              1004795
707314     EXIT.                                                        1004795
707315                                                                  1004795
707317 UNPOST-EXCEPT.                                                   1004795
707320     MOVE '31' TO REJPROG.                                        1004795
707323     IF TR-REJ-PROG NOT EQUAL TO '28'                             1004795
707326         MOVE '31' TO TR-REJ-PROG.                                1004795
707329     IF  TR-CLASS EQUAL '80'                                      1004795
707332         IF  TR-POST-PRIORITY EQUAL '3A' OR '0A'                  1004795
707335             MOVE REJRESN TO TR-REJ-RESN                          1004795
707338         ELSE                                                     1004795
707341             MOVE REJRESN TO WK-REJ-REASON                        1004795
707344             PERFORM X3000-REJ-REASON THRU X3000-EXIT             1004795
707347     ELSE                                                         1004795
707350         MOVE REJRESN TO TR-REJ-RESN.                             1004795
707355     MOVE EX-INFO TO IMEX-INFO.                                   1004795
707400     MOVE SPACES TO EX-INFO.
707450 WRITE-EXCEPT.
707500     MOVE TR-KEY TO IMEX-KEY.
707550     IF  TR-SRC-INFO-FLAG IS GREATER THAN '0'                     IM006
707551     AND TR-SRC-INFO NOT EQUAL SPACES                             IM006
707552         GO TO WRITE-EX-WITH-SRC.                                 IM006
707553     IF  IMEX-REC-NO EQUAL '02'                                   IM006
707554         MOVE '0' TO EX02-SRC-FLAG.                               IM006
707555     IF  IMEX-REC-NO EQUAL '03'                                   IM006
707556         MOVE '0' TO EX03-SRC-FLAG.                               IM006
707557     IF  IMEX-REC-NO EQUAL '04'                                   IM006
707558         MOVE '0' TO EX04-SRC-FLAG.                               IM006
707559     IF  IMEX-REC-NO EQUAL '05'                                   IM006
707560         MOVE '0' TO EX05-SRC-FLAG.                               IM006
707561     IF  IMEX-REC-NO EQUAL '07'                                   IM006
707562         MOVE '0' TO EX07-SRC-FLAG.                               IM006
707563     IF  IMEX-REC-NO EQUAL '09'                                   IM006
707564         MOVE '0' TO EX09-SRC-FLAG.                               IM006
707565     IF  IMEX-REC-NO EQUAL '11'                                   IM006
707566         MOVE '0' TO EX11-SRC-FLAG.                               IM006
707567     IF  IMEX-REC-NO EQUAL '12'                                   IM006
707568         MOVE '0' TO EX12-SRC-FLAG.                               IM006
707569     IF  IMEX-REC-NO EQUAL '13'                                   IM006
707570         MOVE '0' TO EX13-SRC-FLAG.                               IM006
707571     IF  IMEX-REC-NO EQUAL '15'                                   IM006
707572         MOVE '0' TO EX15-SRC-FLAG.                               IM006
707573     IF  IMEX-REC-NO EQUAL '17'                                   IM006
707574         MOVE '0' TO EX17-SRC-FLAG.                               IM006
707575     IF  IMEX-REC-NO EQUAL '18'                                   IM006
707576         MOVE '0' TO EX18-SRC-FLAG.                               IM006
707577     IF  IMEX-REC-NO EQUAL '22'                                   IM006
707578         MOVE '0' TO EX13-SRC-FLAG.                               IM006
707579     IF  IMEX-REC-NO EQUAL '23'                                   IM006
707580         MOVE '0' TO EX23-SRC-FLAG.                               IM006
707581     IF  IMEX-REC-NO EQUAL '29'                                   0617360
707582         MOVE '0' TO EX29-SRC-FLAG.                               0617360
707583     GO TO CALL-IM31RDWR.                                         0617360
707584 WRITE-EX-WITH-SRC.                                               0617360
707585     IF  IMEX-REC-NO EQUAL '01' OR '06' OR '08' OR '10'           0617360
707587                        OR '14' OR '16' OR '19' OR '20'           9915845
707591                        OR '21' OR '24' OR '44' OR '45'           9915845
707593         GO TO CALL-IM31RDWR.                                     IM006
707594     MOVE '1' TO SRC-INFO-LENGTH-FLG.                             IM006
707595     IF  IMEX-REC-NO EQUAL '02'                                   IM006
707596         MOVE TR-SRC-INFORMATION TO EX02-SRC-INFORMATION.         IM006
707597     IF  IMEX-REC-NO EQUAL '03'                                   IM006
707598         MOVE TR-SRC-INFORMATION TO EX03-SRC-INFORMATION.         IM006
707599     IF  IMEX-REC-NO EQUAL '04'                                   IM006
707600         MOVE TR-SRC-INFORMATION TO EX04-SRC-INFORMATION.         IM006
707601     IF  IMEX-REC-NO EQUAL '05'                                   IM006
707602         MOVE TR-SRC-INFORMATION TO EX05-SRC-INFORMATION.         IM006
707603     IF  IMEX-REC-NO EQUAL '07'                                   IM006
707604         MOVE TR-SRC-INFORMATION TO EX07-SRC-INFORMATION.         IM006
707605     IF   IMEX-REC-NO EQUAL '09'                                  IM006
707606         MOVE TR-SRC-INFORMATION TO EX09-SRC-INFORMATION.         IM006
707607     IF  IMEX-REC-NO EQUAL '11'                                   IM006
707608         MOVE TR-SRC-INFORMATION TO EX11-SRC-INFORMATION.         IM006
707609     IF  IMEX-REC-NO EQUAL '12'                                   IM006
707610         MOVE TR-SRC-INFORMATION TO EX12-SRC-INFORMATION.         IM006
707611     IF  IMEX-REC-NO EQUAL '13'                                   IM006
707612         MOVE TR-SRC-INFORMATION TO EX13-SRC-INFORMATION.         IM006
707613     IF  IMEX-REC-NO EQUAL '15'                                   IM006
707614         MOVE TR-SRC-INFORMATION TO EX15-SRC-INFORMATION.         IM006
707615     IF  IMEX-REC-NO EQUAL '17'                                   IM006
707616         MOVE TR-SRC-INFORMATION TO EX17-SRC-INFORMATION.         IM006
707617     IF  IMEX-REC-NO EQUAL '18'                                   IM006
707618         MOVE TR-SRC-INFORMATION TO EX18-SRC-INFORMATION.         IM006
707619     IF  IMEX-REC-NO EQUAL '22'                                   IM006
707620         MOVE TR-SRC-INFORMATION TO EX13-SRC-INFORMATION.         IM006
707621     IF  IMEX-REC-NO EQUAL '23'                                   IM006
707622         MOVE TR-SRC-INFORMATION TO EX23-SRC-INFORMATION.         IM006
707623     IF  IMEX-REC-NO EQUAL '29'                                   0617360
707624         MOVE TR-SRC-INFORMATION TO EX29-SRC-INFORMATION.         0617360
707637     GO TO CALL-IM31RDWR.                                         IM006
707640 WRITE-EX-MST.                                                    IM006
707641*----------------------------------------------------------------*1105504
707642*    ADD 15-DIGIT REFERENCE NUMBER TO EXCEPTION RECORDS.         *1105504
707643*----------------------------------------------------------------*1105504
707649     MOVE WMS-CONTROL-KEY TO IMEX-KEY.                            1105504
707650     IF  IMEX-REC-NO EQUAL '02'                                   1105504
707651         MOVE '0' TO EX02-SRC-FLAG.                               1105504
707652     IF  IMEX-REC-NO EQUAL '03'                                   1105504
707653         MOVE '0' TO EX03-SRC-FLAG.                               1105504
707654     IF  IMEX-REC-NO EQUAL '04'                                   1105504
707655         MOVE '0' TO EX04-SRC-FLAG.                               1105504
707656     IF  IMEX-REC-NO EQUAL '05'                                   1105504
707657         MOVE '0' TO EX05-SRC-FLAG.                               1105504
707658     IF  IMEX-REC-NO EQUAL '07'                                   1105504
707659         MOVE '0' TO EX07-SRC-FLAG.                               1105504
707660     IF  IMEX-REC-NO EQUAL '09'                                   1105504
707661         MOVE WS-REF-NO TO EX09-REF-NUM                           9915749
707662         MOVE '0' TO EX09-SRC-FLAG.                               IM006
707663     IF  IMEX-REC-NO EQUAL '11'                                   IM006
707664         MOVE '0' TO EX11-SRC-FLAG.                               IM006
707665     IF  IMEX-REC-NO EQUAL '12'                                   IM006
707666         MOVE '0' TO EX12-SRC-FLAG.                               IM006
707667     IF  IMEX-REC-NO EQUAL '13'                                   IM006
707668         MOVE '0' TO EX13-SRC-FLAG.                               IM006
707669     IF  IMEX-REC-NO EQUAL '15'                                   IM006
707670         MOVE '0' TO EX15-SRC-FLAG.                               IM006
707671     IF  IMEX-REC-NO EQUAL '17'                                   IM006
707672         MOVE '0' TO EX17-SRC-FLAG.                               IM006
707673     IF  IMEX-REC-NO EQUAL '18'                                   IM006
707674         MOVE '0' TO EX18-SRC-FLAG.                               IM006
707675     IF  IMEX-REC-NO EQUAL '22'                                   IM006
707676         MOVE '0' TO EX13-SRC-FLAG.                               IM006
707677     IF  IMEX-REC-NO EQUAL '23'                                   IM006
707678         MOVE '0' TO EX23-SRC-FLAG.                               IM006
707679     IF  IMEX-REC-NO EQUAL '29'                                   0617360
707680         MOVE '0' TO EX29-SRC-FLAG.                               0617360
707700 CALL-IM31RDWR.
707705******************************************************************IM005
707710***  THE FOLLOWING USER EXIT MAY BE USED TO CHANGE THE EXCEPTION  IM005
707715***  RECORDS PRIOR TO THEIR BEING WRITTEN BY IMTAGTS.             IM005
707720***                                                               IM005
707725***  COPY IM31UEXC.                                               IM005
707730***                                                               IM005
707735***  END OF USER EXIT.                                            IM005
707740******************************************************************IM005
707750     MOVE EX-REC-LENGTH (IMEX-REC-NO9) TO IMEX-LENGTH.
707760     IF  SRC-INFO-LENGTH-FLG EQUAL '1'                            IM006
707770         ADD EX-SRC-INFO-LENGTH TO IMEX-LENGTH.                   IM006
707800     MOVE LOW-VALUES TO IMEX-BIN0.
707850     ADD +1 TO EX-SEQ-CTR.
707900     MOVE EX-SEQ-CTR TO IMEX-SEQUENCE.
707950     IF  ACCT-NOT-FOUND EQUAL TO '1'                              IM006
708000         MOVE HIGH-VALUES TO IMEX-CODE-2.
708010     MOVE '1' TO EX-MST-CTL.                                      0427139
708015     IF  IMEX-REC-NO EQUAL '13' OR '22'                           0427139
708020         PERFORM X0780-REJECT-TYPE THRU X0780-REJECT-TYPE-EXIT.   0427139
708021     IF  (IMEX-REC-NO EQUAL '13' OR '22')                         1020116
708022         IF  (TR-80-TYPE EQUAL '09' OR '13')                      1020116
708023             IF  (WS-REGE-EFF EQUAL 'A' OR 'Y')                   1020116
708024                 MOVE WMS-OD-REGE-OPT-CODE TO EX13-ATM-OPT-CD.    1020116
708025     IF  ((IMEX-REC-NO EQUAL '06' OR '07')                        0427139
708030     AND (WMS-PASSBOOK-ACCT EQUAL '1')                            0427139
708035     AND (IMEX-CODE-1 NOT EQUAL 'PB'))                            0427139
708040         GO TO WRITE-EXIT1.                                       0427139
708045     IF  ((IMEX-REC-NO EQUAL '03')                                0427139
708050     AND (WMS-PASSBOOK-ACCT EQUAL '1')                            0427139
708051     AND (IMEX-CODE-1 EQUAL 'I2'))                                0617492
708053         GO TO WRITE-EXIT1.                                       0617492
708055*--------------------------------------------------------------*  0617492
708056*    FOR CREDIT CARD/LOC FUNDING, IF THE POSTING PRIORITY WAS  *  0617492
708057*    SET IN IM28 TO A '99' AND TRANSACTION IS A NSF, OD OR DAU *  0617492
708058*    THEN SET EX13-XC-LOC-LIMIT-FLAG TO A 'Y' FOR POSSIBLE USE *  0617492
708059*    BY IM41 PHASES THAT USE EXCEPTION 13 RECORD.              *  0617492
708060*--------------------------------------------------------------*  0617492
708061     IF  WBC-FUND-XC-LOC-LIMIT NUMERIC                            0617492
708062     IF  ((WBC-FUND-XC-LOC-LIMIT GREATER THAN ZERO)               0617492
708063     AND (IMEX-REC-NO EQUAL '13')                                 0617492
708064     AND (TR-POST-PRIORITY EQUAL '99')                            0617492
708065     AND (IMEX-CODE-2 EQUAL 'A1' OR IMEX-CODE-1 EQUAL 'HJ' OR     0617492
708066          IMEX-CODE-1 EQUAL 'H2'))                                0617492
708067         MOVE 'Y'            TO EX13-XC-LOC-LIMIT-FLAG.           0617492
708070****************************************************************  0326947
708071*    CALL MAINTENANCE HISTORY PHASE TO FLAG THE EXCEPTION TAG  *  0326947
708072*    RECORDS FOR TRANSACTIONS TO BE WRITTEN TO THE MAINTENANCE *  0326947
708073*    HISTORY FILE (IMOMHM).                                       0326947
708074****************************************************************  0326947
708076     MOVE '0'                TO IMEX-MAINT-HIST.                  0326947
708078     MOVE +0                 TO IMEX-MAINT-HIST-RET.              0326947
708080     IF  WBC-MAINT-HIST-OPT  GREATER THAN '0'                     0326947
708082     AND WBC-MAINT-EXC-PHASE NOT EQUAL SPACES                     0326947
708084         MOVE WBC-MAINT-EXC-PHASE TO IM31-PH2                     0326947
708086         CALL 'SILINK' USING IM31-PHASE                           0326947
708088                             EXCEPTION-AREA                       0326947
708090                             MASTER-AREA                          0326947
708092                             DDA-WRKBCR-1.                        0326947
708093     IF  WBC-IMSA41-USER-PHASE NOT EQUAL SPACES                   0447267
708094         GO TO WRITE-IMTAGT.                                      0447267
708103     IF  IMEX-CODE-1 EQUAL  'DY' OR 'EF' OR 'E0' OR 'E1' OR 'FS'  0447267
708105         OR 'GT' OR 'HA' OR 'HZ' OR 'IB' OR 'II' OR 'IS' OR 'IZ'  0447267
708107         OR 'I1' OR 'I2' OR 'I8' OR 'KJ' OR 'KN' OR 'K0' OR 'K1'  0447267
708109         OR 'K2' OR 'K3' OR 'K5' OR 'K6' OR 'K7' OR 'K8' OR 'K9'  0447267
708111         OR 'PB' OR 'RS' OR 'ST' OR 'SX' OR 'PA'                  0617360
708115         GO TO WRITE-IMTAGT.                                      0447267
708117     MOVE 1 TO WK-BISCH-SEQ.                                      0447267
708119     MOVE IMEX-CODE-1 TO WK-BISCH-EXC.                            0447267
708121     CALL 'SIBINSRH' USING RPT-EXC-TBL-AREA                       0447267
708123                           WK-BIN-SRCH-AREA.                      0447267
708125     IF  WK-BISCH-CODE EQUAL TO '1'                               0447267
708127         GO TO WRITE-IMTAGT.                                      0447267
708129     MOVE 1 TO WK-BISCH-SEQ.                                      0447267
708131     MOVE IMEX-CODE-2 TO WK-BISCH-EXC.                            0447267
708133     CALL 'SIBINSRH' USING RPT-EXC-TBL-AREA                       0447267
708135                           WK-BIN-SRCH-AREA.                      0447267
708137     IF  WK-BISCH-CODE EQUAL TO '0'                               0447267
708138         MOVE '0' TO  SRC-INFO-LENGTH-FLG                         0827820
708139         GO TO WRITE-EXIT1.                                       0447267
708143 WRITE-IMTAGT.                                                    0447267
708145     MOVE 'L' TO I-O-CONTROL-OPERATOR.                            0447267
708150     CALL 'IMTAGTS' USING I-O-CONTROL-AREA,
708200                          EXCEPTION-AREA.
708220     MOVE '0' TO  SRC-INFO-LENGTH-FLG.                            IM006
708250 WRITE-EXIT1.  EXIT.
708299 END-EXCEPT.                                                      0617360
708300     GO TO CLEAR-IMEX1                                            0617360
708301           CLEAR-IMEX2                                            0617360
708302           CLEAR-IMEX3                                            0617360
708303           CLEAR-IMEX4                                            0617360
708304           CLEAR-IMEX5                                            0617360
708305           CLEAR-IMEX6                                            0617360
708306           CLEAR-IMEX7                                            0617360
708307           CLEAR-IMEX8                                            0617360
708308           CLEAR-IMEX9                                            0617360
708309           CLEAR-IMEX10                                           0617360
708310           CLEAR-IMEX11                                           0617360
708311           CLEAR-IMEX12                                           0617360
708312           CLEAR-IMEX13                                           0617360
708313           CLEAR-IMEX14                                           0617360
708314           CLEAR-IMEX15                                           0617360
708315           CLEAR-IMEX16                                           0617360
708316           CLEAR-IMEX17                                           0617360
708317           CLEAR-IMEX18                                           0617360
708318           CLEAR-IMEX19                                           0617360
708319           CLEAR-IMEX20                                           0617360
708320           CLEAR-IMEX21                                           0617360
708321           CLEAR-IMEX22                                           0617360
708322           CLEAR-IMEX23                                           0617360
708323           CLEAR-IMEX24                                           0617360
708324           CLEAR-IMEX25                                           0617360
708325           CLEAR-IMEX26                                           0617360
708326           CLEAR-IMEX29                                           0617360
708327           DEPENDING ON IMEX-REC-NO9.                             0527319
708328     GO TO END-EXCEPT1.                                           0527319
708329                                                                  0527319
708330 CLEAR-IMEX1.                                                     0527319
708331     MOVE SPACES TO IMEX-INFO1.                                   0527319
708332     GO TO WRITE-EX-EXIT.                                         0527319
708333                                                                  0527319
708334 CLEAR-IMEX2.                                                     0527319
708335     MOVE SPACES TO IMEX-INFO2.                                   0527319
708336     GO TO WRITE-EX-EXIT.                                         0527319
708337                                                                  0527319
708338 CLEAR-IMEX3.                                                     0527319
708339     MOVE SPACES TO IMEX-INFO3.                                   0527319
708340     GO TO WRITE-EX-EXIT.                                         0527319
708341                                                                  0527319
708342 CLEAR-IMEX4.                                                     0527319
708343     MOVE SPACES TO IMEX-INFO4.                                   0527319
708344     GO TO WRITE-EX-EXIT.                                         0527319
708345                                                                  0527319
708346 CLEAR-IMEX5.                                                     0527319
708347     MOVE SPACES TO IMEX-INFO5.                                   0527319
708348     GO TO WRITE-EX-EXIT.                                         0527319
708349                                                                  0527319
708350 CLEAR-IMEX6.                                                     0527319
708351     MOVE SPACES TO IMEX-INFO6.                                   0527319
708352     GO TO WRITE-EX-EXIT.                                         0527319
708353                                                                  0527319
708354 CLEAR-IMEX7.                                                     0527319
708355     MOVE SPACES TO IMEX-INFO7.                                   0527319
708356     GO TO WRITE-EX-EXIT.                                         0527319
708357                                                                  0527319
708358 CLEAR-IMEX8.                                                     0527319
708359     MOVE SPACES TO IMEX-INFO8.                                   0527319
708360     GO TO WRITE-EX-EXIT.                                         0527319
708361                                                                  0527319
708362 CLEAR-IMEX9.                                                     0527319
708363     MOVE SPACES TO IMEX-INFO9.                                   0527319
708364     GO TO WRITE-EX-EXIT.                                         0527319
708365                                                                  0527319
708366 CLEAR-IMEX10.                                                    0527319
708367     MOVE SPACES TO IMEX-INFO10.                                  0527319
708368     GO TO WRITE-EX-EXIT.                                         0527319
708369                                                                  0527319
708370 CLEAR-IMEX11.                                                    0527319
708371     MOVE SPACES TO IMEX-INFO11.                                  0527319
708372     GO TO WRITE-EX-EXIT.                                         0527319
708373                                                                  0527319
708374 CLEAR-IMEX12.                                                    0527319
708375     MOVE SPACES TO IMEX-INFO12.                                  0527319
708376     GO TO WRITE-EX-EXIT.                                         0527319
708377                                                                  0527319
708378 CLEAR-IMEX13.                                                    0527319
708379     MOVE SPACES TO IMEX-INFO13.                                  0527319
708380     GO TO WRITE-EX-EXIT.                                         0527319
708381                                                                  0527319
708382 CLEAR-IMEX14.                                                    0527319
708383     MOVE SPACES TO IMEX-INFO14.                                  0527319
708384     GO TO WRITE-EX-EXIT.                                         0527319
708385                                                                  0527319
708386 CLEAR-IMEX15.                                                    0527319
708387     MOVE SPACES TO IMEX-INFO15                                   0527319
708388                    EX15-DATE-LAST-MAINT                          0527319
708389                    EX15-SRC-INFORMATION.                         0527319
708390     GO TO WRITE-EX-EXIT.                                         0527319
708391                                                                  0527319
708392 CLEAR-IMEX16.                                                    0527319
708393     MOVE SPACES TO IMEX-RECORD.                                  0527319
708394     GO TO WRITE-EX-EXIT.                                         0527319
708395                                                                  0527319
708396 CLEAR-IMEX17.                                                    0527319
708397     MOVE SPACES TO IMEX-INFO17.                                  0527319
708398     GO TO WRITE-EX-EXIT.                                         0527319
708399                                                                  0527319
708400 CLEAR-IMEX18.                                                    0527319
708401     MOVE SPACES TO IMEX-INFO18.                                  0527319
708402     GO TO WRITE-EX-EXIT.                                         0527319
708403                                                                  0527319
708404 CLEAR-IMEX19.                                                    0527319
708405     MOVE SPACES TO IMEX-INFO19.                                  0527319
708406     GO TO WRITE-EX-EXIT.                                         0527319
708407                                                                  0527319
708408 CLEAR-IMEX20.                                                    0527319
708409     MOVE SPACES TO IMEX-RECORD, IMEX-EX-CODE.                    0920021
708410     GO TO WRITE-EX-EXIT.                                         0527319
708411                                                                  0527319
708412 CLEAR-IMEX21.                                                    0527319
708413     MOVE SPACES TO IMEX-INFO21.                                  0527319
708414     GO TO WRITE-EX-EXIT.                                         0527319
708415                                                                  0527319
708416 CLEAR-IMEX22.                                                    0527319
708417     MOVE SPACES TO IMEX-INFO22.                                  0527319
708418     GO TO WRITE-EX-EXIT.                                         0527319
708419                                                                  0527319
708420 CLEAR-IMEX23.                                                    0527319
708421     MOVE SPACES TO IMEX-INFO23.                                  0527319
708422     GO TO WRITE-EX-EXIT.                                         0527319
708423                                                                  0527319
708424 CLEAR-IMEX24.                                                    0527319
708425     MOVE SPACES TO IMEX-INFO24.                                  0527319
708426     GO TO WRITE-EX-EXIT.                                         0527319
708427                                                                  0527319
708428 CLEAR-IMEX25.                                                    0527319
708429     MOVE SPACES TO IMEX-INFO.                                    0527319
708430     GO TO WRITE-EX-EXIT.                                         0527319
708431                                                                  0527319
708432 CLEAR-IMEX26.                                                    0527319
708433     MOVE SPACES TO IMEX-INFO26.                                  0527319
708434     GO TO WRITE-EX-EXIT.                                         0527319
708435                                                                  0527319
708436 CLEAR-IMEX29.                                                    0617360
708437     MOVE SPACES TO IMEX-INFO29.                                  0617360
708438     GO TO WRITE-EX-EXIT.                                         0617360
708439                                                                  0617360
708440 END-EXCEPT1.                                                     0617360
708441     IF  IMEX-REC-NO EQUAL '44'                                   0617360
708442         MOVE SPACES TO IMEX-INFO44                               0617360
708443         GO TO WRITE-EX-EXIT.                                     0617360
708444     IF  IMEX-REC-NO EQUAL '45'                                   0617360
708445         MOVE SPACES TO IMEX-INFO45                               0617360
708446         GO TO WRITE-EX-EXIT.                                     0617360
708447     IF  IMEX-REC-NO EQUAL '46'                                   0617360
708448         MOVE SPACES TO IMEX-INFO46                               0617360
708449         GO TO WRITE-EX-EXIT.                                     0617360
708450     IF  IMEX-REC-NO EQUAL '47'                                   0617360
708451         MOVE SPACES TO IMEX-INFO47                               0617360
708452         GO TO WRITE-EX-EXIT.                                     0617360
708453     MOVE SPACES TO IMEX-INFO.                                    0617360
708454 WRITE-EX-EXIT.  EXIT.                                            0617360
708455 END-UNPOST.                                                      0617360
708500     IF ACCT-NOT-FOUND EQUAL TO '1'
708550         MOVE ZERO TO EX-MST-CTL
708600                      UNPOST-TODAY.
708650     MOVE SPACES TO EX-INFO.
708700     MOVE '1' TO TRAN-REWRITE-SW.                                 IM002
708710     GO TO Q4600.                                                 IM002
708750 CALC-AVAIL-BAL.
708752*----------------------------------------------------------------*9915749
708754*    COMPARE ABM FLAG TO DETERMINE AUTO BORROW ELIGIBLE          *9915749
708756*    ACCOUNTS. RECALCULATE OUTSTANDING PRINCIPLE BALANCE         *9915749
708758*    USING CREDIT LIMIT MINUS AVAILABLE.                         *9915749
708760*----------------------------------------------------------------*9915749
708800     IF WMS-CR-LIMIT-PTR EQUAL TO ZERO
708850         MOVE WMS-CREDIT-LIMIT TO LOAN-LIM
708900     ELSE MOVE WBC-CR-LIMIT (WMS-CR-LIMIT-PTR) TO LOAN-LIM.       0266741
708950     ADD WMS-ADDITIONAL-CREDIT TO LOAN-LIM.
709000     ADD WMS-PRIN-BAL, WMS-INTRST-BAL, WMS-OTHER-CHGS
709050         GIVING LOAN-BAL.
709100     SUBTRACT LOAN-BAL FROM LOAN-LIM GIVING HOLD-AMT9.
709150 CALC-1. EXIT.
709200 CALC-2.
709210     IF  WMS-NB-DC-ABM EQUAL TO '1'                               9915749
709220         GO TO CALC-3.                                            9915749
709250     MOVE HOLD-AMT9 TO WMS-AVAIL-BAL.
709300     IF  WMS-AVAIL-BAL LESS THAN ZERO
709350         MOVE '1' TO OVER-LINE
709400         MOVE ZERO TO WMS-AVAIL-BAL
709450         GO TO CALC-3
709500     ELSE
709550         MOVE '0' TO WMS-OVERLIMIT, OVER-LINE.
709600     IF  WMS-LOAN-STATUS GREATER THAN '1' AND NOT EQUAL '5'
709650     OR  WMS-LOAN-BAD-ACCT GREATER THAN '0'
709700         MOVE ZERO TO WMS-AVAIL-BAL
709750         SUBTRACT HOLD-AMT9 FROM LOAN-LIM.
709800 CALC-3. EXIT.
709850 CALC-4.
709900     IF LOAN-BAL GREATER THAN WMS-LOAN-HIGH-BAL
709950         MOVE LOAN-BAL TO WMS-LOAN-HIGH-BAL.
710000     IF LOAN-BAL GREATER THAN WMS-HIGH-BAL-THIS-CYCLE
710050         MOVE LOAN-BAL TO WMS-HIGH-BAL-THIS-CYCLE.
710100     MOVE '1' TO WMS-LOAN-CALC-CODE.
710150 CALC-AVAIL-EXIT.  EXIT.
710200 EJECT
710250 WRITE-MEMO-UPDATE.
710300                                                                  9915749
710400     COPY IMPDMEM1.                                               9915749
710500                                                                  9915749
713100 WRITE-MEMO-EXIT.
713150     EJECT
713200 WRITE-GEN-TRANS.
713205******************************************************************IM006
713210***  THE FOLLOWING USER EXIT MAY BE USED TO CHANGE THE GENERATED  IM006
713215***  TRANSACTIONS PRIOR TO THEIR BEING WRITTEN BY IMTAGTS.        IM006
713220***                                                               IM006
713225***  COPY IM31UGEN.                                               IM006
713230***                                                               IM006
713235***  END OF USER EXIT.                                            IM006
713240******************************************************************IM006
713250     MOVE 'GT' TO IMEX-CODE-1.
713300 WGT-1.
713350     MOVE '01' TO IMEX-REC-NO.
713400     MOVE GT-USER-CODE TO EX01-USER-CODE.
713450     MOVE GT-MNEUMONIC TO EX01-STMT-SYM.
713500     MOVE HOLD-AMT TO EX01-AMT.
713550     ADD +1 TO SEQ-CTR.
713600     MOVE SEQ-CTR TO EX01-SEQ-NO.
713650     MOVE ZERO TO EX01-TRACE.
713700     MOVE GT-ACCUM1 TO EX01-ACCM-IND1.
713710     MOVE GT-ACCUM2 TO EX01-ACCM-IND2.                            0617360
713725     IF  WMS-PLAN-TRLR NOT EQUAL '1'                              0617360
713730         GO TO WGT-1-CONT.                                        0617360
713745     IF  (EX01-TYPE     EQUAL '5' OR '6' OR '07' OR '08')         0617360
713750     OR  (EX01-INT-CODE EQUAL '02')                               0617360
713755         GO TO WGT-1-CONT.                                        0617360
713756     IF  EX01-TYPE EQUAL '1' OR '2'                               0617360
713757         ADD EX01-AMT    TO T3-PLAN-CRD-AMT                       0617360
713758                            WS-GL-PLAN-CRD-AMT.                   0617360
713759     IF  EX01-TYPE EQUAL '3' OR '4'                               0617360
713760         ADD EX01-AMT    TO T3-PLAN-DEB-AMT                       0617360
713761                            WS-GL-PLAN-DEB-AMT.                   0617360
713762     MOVE SPACES TO EX01-COMB-PLN-CODES                           0617360
713763                    EX01-PLN-CAL-YR                               0617360
713764                    EX01-PLN-TAX-YR.                              0617360
713765     IF  EX01-INT-CODE EQUAL '06' OR '21' OR '25'                 0617360
713766         NEXT SENTENCE                                            0617360
713767     ELSE                                                         0617360
713768         GO TO WGT-1-CONT.                                        0617360
713769     IF  EX01-INT-CODE EQUAL '25'                                 0617360
713770         MOVE WMS-IOD-DIST-PL-CD TO EX01-PLN-CODE                 0617360
713771                                    WS-WORK-PLN-CODE              0617360
713772         MOVE SPACES             TO WS-WORK-PLN-CODE2             0617360
713773     ELSE                                                         0617360
713774         MOVE '21'    TO EX01-PLN-CODE                            0617360
713775                         WS-WORK-PLN-CODE                         0617360
713776         MOVE SPACES  TO WS-WORK-PLN-CODE2.                       0617360
713777     MOVE '0'      TO EX01-PLN-CAL-YR                             0617360
713778                      WS-WORK-CAL-YR.                             0617360
713779     MOVE '0'      TO EX01-PLN-TAX-YR                             0617360
713780                      WS-WORK-TAX-YR.                             0617360
713781     MOVE EX01-AMT TO WS-WORK-CTDT-AMT.                           0617360
713782     IF  EX01-INT-CODE EQUAL '06' OR '25'                         0617360
713783         MOVE '0'  TO WS-WORK-TAX-CODE                            0617360
713784     ELSE                                                         0617360
713785         MOVE EX01-TAX-TYPE TO WS-WORK-TAX-CODE.                  0617360
713786     PERFORM UPDATE-PLN-CTDT THRU UIC-END.                        0617360
713795 WGT-1-CONT.                                                      0617360
713800     MOVE WBC-CAPTURE-DATE TO EX01-EFFECTIVE-DATE.                0266741
713806     IF  EX01-TYPE EQUAL '5' OR '6'                               IM008
713812         IF  LOAN-ACCRUAL-DAY GREATER THAN +1                     IM008
713818             ADD LOAN-ACCRUAL-DAY TO EX01-EFFECT-DA               IM008
713824             SUBTRACT +1 FROM EX01-EFFECT-DA                      IM008
713830             GO TO WGT-2                                          IM008
713836         ELSE                                                     IM008
713842             GO TO WGT-2.                                         IM008
713850     IF  ACCRUAL-DAY GREATER THAN +1
713900         ADD ACCRUAL-DAY TO EX01-EFFECT-DA
713950         SUBTRACT +1 FROM EX01-EFFECT-DA.
713960                                                                  IM008
713970 WGT-2.                                                           IM008
713975     IF  WBC-GEN-TRAN-PHASE NOT EQUAL SPACES                      0266741
713980         MOVE WBC-GEN-TRAN-PHASE TO IM31-PH2                      0266741
713985         CALL 'SILINK' USING IM31-PHASE                           9915858
713990                             EXCEPTION-AREA                       9915858
713995                             MASTER-AREA.                         9915858
714000     IF  WBC-TRANS-PRINT NOT EQUAL TO ZERO AND                    0266741
714050         IMEX-CODE-1 EQUAL 'GT'
714100         MOVE '2' TO TRIAL-TRANS
714150         PERFORM CALL-TRIAL-BAL THRU CALL-TRIAL-END.
714200     MOVE EX01-TYPE TO SAVE-TYPE.
714210     IF  WMS-INVESTMENT-TRLR EQUAL '1'                            1004554
714230         MOVE '6' TO WK-INVEST-FUNCTION                           1004554
714240         PERFORM CALL-INVEST-PHASE THRU CALL-INVEST-EXIT.         1004554
714250     PERFORM WRITE-EX-MST THRU WRITE-EX-EXIT.
714300 WGT-3.  EXIT.                                                    IM008
714350 WGT-4.                                                           IM008
714400     IF  GT-ACCUM1 GREATER THAN ZERO AND
714450         WMS-EXT-SC-DATA-TRLR EQUAL '1'
714500         MOVE GT-ACCUM1 TO X
714550         IF  WMS-SC-MISC-CNTR-FLG (X) EQUAL '1' OR '3' OR '5'     IM008
714600             ADD HOLD-AMT TO WMS-SC-MISC-AMTS (X)
714650         ELSE
714700             ADD +1 TO WMS-SC-MISC-NOS (X).
714750     IF  GT-ACCUM2 GREATER THAN ZERO AND
714800         WMS-EXT-SC-DATA-TRLR EQUAL '1'
714850         MOVE GT-ACCUM2 TO X
714900         IF  WMS-SC-MISC-CNTR-FLG (X) EQUAL '1' OR '3' OR '5'     IM008
714950             ADD HOLD-AMT TO WMS-SC-MISC-AMTS (X)
715000         ELSE
715050             ADD +1 TO WMS-SC-MISC-NOS (X).
715100     MOVE GT-OPTION9 (4) TO HOLD-OPT.
715150     IF HOLD-OPT GREATER THAN +3
715200         SUBTRACT +4 FROM HOLD-OPT
715250         ADD +1 TO WMS-SC-DEBITS, WMS-MTD-SVC-CHG-DR.
715260     IF  WMS-CHK-TRUNC-FLAG EQUAL TO 'N' OR 'I' OR 'R' OR 'U'     0417110
715262                                  OR 'E'                          0417110
715300         IF  HOLD-OPT GREATER THAN +1                             1105504
715350             SUBTRACT +2 FROM HOLD-OPT.                           1105504
715400     IF HOLD-OPT GREATER THAN +0
715450         ADD +1 TO WMS-STMT-ITEMS.
715500     MOVE GT-OPTION9 (5) TO HOLD-OPT.
715550     IF  HOLD-OPT EQUAL +1 OR +3
715600         ADD +1 TO WMS-SC-CREDITS, WMS-MTD-SVC-CHG-CR.
715650     IF  SAVE-TYPE EQUAL '5' OR '6'
715700         MOVE WBC-CAPTURE-DATE TO WMS-DATE-LAST-LOAN-ACTIVITY     0266741
715750     ELSE
715800         MOVE WBC-CAPTURE-DATE TO WMS-DATE-LAST-MONETARY-ACT.     0266741
715850     IF  STATUS-IN EQUAL '06' OR WMS-STATUS EQUAL '06'
715900         MOVE 'H0' TO IMEX-CODE-1
715950         PERFORM WGT-5 THRU WGT-5-EXIT.                           IM008
716000     IF  STATUS-IN EQUAL '07' OR WMS-STATUS EQUAL '07'
716050         MOVE 'H1' TO IMEX-CODE-1
716100         PERFORM WGT-5 THRU WGT-5-EXIT.                           IM008
716120     GO  TO  WGT-6.                                               IM008
716200 WGT-5.                                                           IM008
716250     MOVE '13' TO IMEX-REC-NO.
716300     MOVE ZEROES TO EX13-BATCH, EX13-SEQ, EX13-ITEM-CNT.
716350     MOVE SAVE-TYPE TO EX13-TYPE.
716400     MOVE DATE-LAST-ACTIVE TO EX13-DATE-LAST-ACTIVE.
716450     MOVE HOLD-AMT TO EX13-AMT.
716500     MOVE GT-USER-CODE TO EX13-UCODE2.
716550     MOVE 'GT' TO EX13-UCODE1, EX13-TR-TYPE.
716600     PERFORM WRITE-EX-MST THRU WRITE-EX-EXIT.
716650 WGT-5-EXIT. EXIT.                                                IM008
716700 WGT-6.                                                           IM008
716750     IF  WMS-CURR-BAL EQUAL ZEROES AND
716800         SAVE-TYPE NOT EQUAL '5' AND
716850         SAVE-TYPE NOT EQUAL '6'
716900         MOVE SAVE-TYPE TO EX01-TYPE
716950         MOVE 'H5' TO IMEX-CODE-1
717000         PERFORM WGT-5 THRU WGT-5-EXIT.                           IM008
717050     IF GT-OPTION (1) GREATER THAN '0'
717100         MOVE SAVE-TYPE TO EX01-TYPE
717150         MOVE 'N' TO HOLD-X1
717200         MOVE GT-OPTION (1) TO HOLD-X2
717250         MOVE HOLD-X TO IMEX-CODE-1
717300         PERFORM WGT-1 THRU WGT-3.                                IM008
717350 WRITE-GEN-EXIT.  EXIT.
717400     SKIP3
717450 INTRST-SECTION-C    SECTION 21.
717500     SKIP3
717550 TRAN-ADJUST.
717600     MOVE '2' TO INTEREST-ADJUST-CODE.
717650     IF  TR-TRAN-TYPE EQUAL '1' OR '2' OR '7'                     9915845
718250         MOVE TR-AMOUNT TO ACCRUAL-BALANCE
718300         MOVE +.005 TO ACCRUAL-ROUND
718350     ELSE
718400         MOVE ZERO TO ACCRUAL-BALANCE
718450         MOVE -.005 TO ACCRUAL-ROUND
718500         SUBTRACT TR-AMOUNT FROM ACCRUAL-BALANCE.
718550     MOVE HOLD-BKDT-DAYS TO ACCRUAL-DAYS.
718600 ADJUST-ACCRUED-BAL-HIST.                                         0902213
718601*--------------------------------------------------------------*  0902213
718602*  WHEN BALANCE HISTORY IS BEING USED:                         *  0902213
718603*  HISTORY MUST EXIST FOR THE REQUIRED NUMBER OF BACKDATE DAYS,*  0902213
718604*  OR A PARTIAL ADJUSTMENT WILL BE MADE.  CREATE AN EXCEPTION  *  0902213
718605*  TO INDICATE THE NUMBER OF DAYS REQUIRING MANUAL ADJUSTMENTS.*  0902213
718606*--------------------------------------------------------------*  0902213
718607     MOVE SPACES TO HOLD-BH-DATE.                                 0902213
718608     IF  IOD-FLAG EQUAL 'N'                                       0902213
718609     AND BHW-IOD-FLAG EQUAL 'I'                                   0902213
718610     AND HOLD-BKDT-DAYS GREATER THAN WBHI-ENTRIES                 0902213
718611         MOVE WBHI-ENTRIES TO ACCRUAL-DAYS                        0902213
718612         IF  WBHI-ENTRIES GREATER THAN ZERO                       0902213
718613             MOVE WBHI-IOD-DATE (WBHI-ENTRIES) TO HOLD-BH-DATE.   0902213
718614     IF  IOD-FLAG EQUAL 'N'                                       0902213
718615     AND BHW-IOD-FLAG EQUAL 'M'                                   0902213
718616     AND HOLD-BKDT-DAYS GREATER THAN WBHM-ENTRIES                 0902213
718617         MOVE WBHM-ENTRIES TO ACCRUAL-DAYS                        0902213
718618         IF  WBHM-ENTRIES GREATER THAN  ZERO                      0902213
718619             MOVE WBHM-MMDA-DATE (WBHM-ENTRIES) TO HOLD-BH-DATE.  0902213
718620     IF  IOD-FLAG EQUAL 'F'                                       0902213
718621     AND BHW-SAV-FLAG EQUAL 'R'                                   0316967
718622     AND HOLD-BKDT-DAYS GREATER THAN WBHS-ENTRIES                 0902213
718623         MOVE WBHS-ENTRIES TO ACCRUAL-DAYS                        0902213
718624         IF  WBHS-ENTRIES GREATER THAN ZERO                       0902213
718625             MOVE WBHS-SAV-DATE (WBHS-ENTRIES) TO HOLD-BH-DATE.   0902213
718626     IF  IOD-FLAG EQUAL 'F'                                       0316967
718627     AND BHW-SAV-FLAG EQUAL 'T'                                   0316967
718628     AND HOLD-BKDT-DAYS GREATER THAN WBHT-ENTRIES                 0316967
718629         MOVE WBHT-ENTRIES TO ACCRUAL-DAYS                        0316967
718630         IF  WBHT-ENTRIES GREATER THAN ZERO                       0316967
718631             MOVE WBHT-SAVT-DATE (WBHT-ENTRIES) TO HOLD-BH-DATE.  0316967
718632     IF  ACCRUAL-DAYS EQUAL ZERO                                  0316967
718633         MOVE '1' TO EX13-REASON-OR-OPT                           0316967
718634         PERFORM X0800-RHR-EXC-K THRU X0880-RHR-EXIT              0316967
718635         GO TO END-INT-ADJUST                                     0316967
718636     ELSE                                                         0316967
718637         IF  HOLD-BKDT-DAYS GREATER THAN ACCRUAL-DAYS             0316967
718638             MOVE '9' TO EX13-REASON-OR-OPT                       0316967
718639             PERFORM X0800-RHR-EXC-K THRU X0880-RHR-EXIT.         0316967
718640     IF  INTEREST-ADJ-SUBCODE EQUAL 'F' OR 'U'                    0316967
718641         GO TO ADJUST-ACCRUED-INTEREST.                           0316967
718642     MOVE 'N' TO INTEREST-ADJ-SUBCODE.                            0316967
718643     IF  IOD-FLAG EQUAL 'N'                                       0316967
718644     AND (BHW-IOD-FLAG EQUAL 'I' OR 'M')                          0316967
718645         MOVE 'T' TO INTEREST-ADJ-SUBCODE.                        0316967
718646     IF (IOD-FLAG NOT EQUAL 'N')                                  0316967
718647     AND (BHW-SAV-FLAG EQUAL 'R' OR 'T')                          0316967
718648         MOVE 'T' TO INTEREST-ADJ-SUBCODE.                        0316967
718649 ADJUST-ACCRUED-INTEREST.                                         0316967
718650     IF IOD-FLAG EQUAL 'N'
718700         MOVE WMS-IOD-CYC-ACC-INT TO ACCRUAL-AMOUNT
718750     ELSE MOVE WMS-ACCR-INT TO ACCRUAL-AMOUNT.
718760     MOVE ACCRUAL-AMOUNT TO HOLD-INTEREST.                        0902213
718800     IF ACCRUAL-AMOUNT GREATER THAN ZERO
718850         ADD +.005 TO ACCRUAL-AMOUNT
718900     ELSE
718950         ADD -.005 TO ACCRUAL-AMOUNT.
719000     MOVE ACCRUAL-AMOUNT TO HOLD-ACCR2.
719002     MOVE ZERO TO HOLD-SVC-FEE1                                   1004554
719004                  HOLD-SVC-FEE2.                                  1004554
719006     IF  IOD-FLAG EQUAL 'F'                                       1004554
719008         IF  WMS-INVESTMENT-TRLR EQUAL '1'                        1004554
719010             MOVE WMS-INV-SVC-ACCRD-CYC TO ACCRUAL-SVC-AMT        1004554
719012             ADD +.005  ACCRUAL-SVC-AMT GIVING HOLD-SVC-FEE2.     1004554
719050     IF  INTEREST-ADJUST-CODE EQUAL '2'
719100         PERFORM X0200-RATE-HIST-RETRIEVAL THRU X0880-RHR-EXIT    IM008
719130         MOVE 'N' TO INTEREST-ADJ-SUBCODE                         0902213
719170         GO TO CALL-ACCRUAL-END.                                  IM008
719350     IF  INTEREST-ADJUST-CODE EQUAL '4' OR '5' OR '6'
719400         PERFORM X0300-RHR-ACCRUAL THRU X0320-RHR-ACCRUAL-EXIT
719450         GO TO CALL-ACCRUAL-END.
719500     IF  PAST-ACCR EQUAL '1'                                      IM008
719510     AND IOD-FLAG EQUAL 'N'                                       1003625
719515         IF  IOD-CHG-FLG EQUAL 'Y'                                9915575
719520         AND (WS-RWF-DDA-CHG (IOD-SUB) EQUAL '1'                  9915575
719540         OR   WS-RWF-DDA-CHG (IOD-SUB) EQUAL '2')                 9915575
719650         AND ACCR-NO-HOLD GREATER THAN RHW-DAYS
719700             MOVE '3' TO INTEREST-ADJUST-CODE                     9915575
719701         ELSE                                                     9915575
719702             IF  TIER-CHG-FLG EQUAL 'Y'                           9915575
719703             AND (WS-RWF-TIER-CHG (TIER-SUB) EQUAL '1'            9915575
719704             OR   WS-RWF-TIER-CHG (TIER-SUB) EQUAL '2')           9915575
719705             AND ACCR-NO-HOLD GREATER THAN RHW-DAYS               9915575
719706                 MOVE '3' TO INTEREST-ADJUST-CODE.                9915575
719710     IF  PAST-ACCR EQUAL '1'                                      1003625
719715     AND IOD-FLAG EQUAL 'F'                                       1003625
719717         IF  SAV-CHG-FLG EQUAL 'Y'                                0316967
719720         AND (WS-RWF-SAV-CHG (SAV-SUB) EQUAL '1'                  0316967
719725         OR   WS-RWF-SAV-CHG (SAV-SUB) EQUAL '2')                 2015901
719730         AND ACCR-NO-HOLD GREATER THAN RHW-DAYS                   1003625
719735             MOVE '3' TO INTEREST-ADJUST-CODE                     0316967
719737         ELSE                                                     0316967
719739             IF  SAV-TIER-CHG-FLG EQUAL 'Y'                       0316967
719740             AND (WS-RWF-SAVT-CHG (SAV-TIER-SUB) EQUAL '1'        0316967
719741             OR   WS-RWF-SAVT-CHG (SAV-TIER-SUB) EQUAL '2')       0316967
719743             AND ACCR-NO-HOLD GREATER THAN RHW-DAYS               0316967
719745                 MOVE '3' TO INTEREST-ADJUST-CODE.                0316967
719750     IF  INTEREST-ADJUST-CODE EQUAL '0'
719800         MOVE ZERO TO RHW-ACCRUAL-AMOUNT
719850         GO TO ADD-DAILY-ACCRUAL.
719900 CALL-ACCRUAL-PHASE.
719910*----------------------------------------------------------------*1105504
719911*    BYPASS THE ACCRUAL PHASE CALL IF ACCOUNT STATUS IS          *1105504
719912*    CLOSING (03) OR IF THIS IS A STATIC RUN.                    *1105504
719913*----------------------------------------------------------------*1105504
719920     IF  WMS-STATUS EQUAL '03'                                    1105504
719922         MOVE ZERO TO ACCRUAL-AMOUNT                              1105504
719923                      TISA-ACCRUAL-AMOUNT                         2013076
719924         GO TO CALL-ACCRUAL-END.                                  1105504
719926     IF  WBC-RUN-FLAG EQUAL 'I'                                   0266741
719928         IF  WBC-ACCRUAL-DAYS EQUAL ZERO                          0266741
719930             MOVE ZERO TO ACCRUAL-AMOUNT                          1105504
719932             GO TO CALL-ACCRUAL-END.                              1105504
719950     IF  IOD-FLAG EQUAL 'N'
720000         IF  WMS-HIFI-INDICATOR GREATER THAN +0
720050             MOVE +0 TO ACCRUAL-ANNUAL-RATE, ACCRUAL-DAILY-RATE
720100         ELSE
720150             IF  INTEREST-ADJUST-CODE EQUAL '3'
720160                 MOVE WMS-IOD-PREV-ANN                            9715504
720170                      TO ACCRUAL-ANNUAL-RATE                      9715504
720200                 MOVE WMS-IOD-PREV-DAF                            1003625
720220                      TO ACCRUAL-DAILY-RATE                       IM008
720300             ELSE
720350                 MOVE WMS-IOD-CUR-ANN                             1003625
720400                      TO ACCRUAL-ANNUAL-RATE                      IM008
720450                 MOVE WMS-IOD-CUR-DAF                             1003625
720500                      TO ACCRUAL-DAILY-RATE.                      IM008
720502     IF  IOD-FLAG EQUAL 'F'                                       0316967
720504     AND WMS-SAV-TIER-PTR GREATER THAN +0                         0316967
720505         IF  WMS-DAILY-RATE-CODE EQUAL '3'                        0817657
720506             MOVE +365 TO DAF-DAYS                                0627539
720507         ELSE                                                     0627539
720508         IF  WMS-DAILY-RATE-CODE EQUAL '1'                        0817657
720509             MOVE +360 TO DAF-DAYS                                0817657
720510         ELSE                                                     0316967
720511         IF  WMS-LEAP-YEAR-FLG EQUAL '1'                          0817657
720512             MOVE +366 TO DAF-DAYS                                0817657
720513         ELSE                                                     0817657
720514             MOVE +365 TO DAF-DAYS.                               0817657
720520     IF  IOD-FLAG EQUAL 'F'                                       0316967
720521         IF  WMS-SAV-TIER-PTR GREATER THAN +0                     0316967
720522             MOVE +0 TO ACCRUAL-ANNUAL-RATE, ACCRUAL-DAILY-RATE   0316967
720523             MOVE '3' TO WS-RATE-KEY-LEVEL                        0316967
720524             PERFORM READ-RATE-SAV-TIER THRU READ-RATE-END        0316967
720526             IF  WS-RATE-NOT-FOUND = 'Y'                          0316967
720527                 PERFORM R0015-RATE-REJECT THRU R0015-EXIT        0316967
720528                 PERFORM WRITE-EX-MST THRU WRITE-EX-EXIT          0316967
720530                 MOVE ZERO TO ACCRUAL-AMOUNT                      0316967
720532                 GO TO CALL-ACCRUAL-END                           0316967
720540             ELSE                                                 0316967
720554                 MOVE WS-RATE-KEY-LEVEL TO WMX-TIER-RATE-KEY-LEVEL0316967
720555                 MOVE RMA-CUR-TIERED-DT     TO WMX-TIER-CUR-DATE  0316967
720556                 MOVE RMA-CUR-TIERED-ANN(1) TO WMX-TIER-CUR-ANN(1)0316967
720557                 MOVE RMA-CUR-TIERED-ANN(2) TO WMX-TIER-CUR-ANN(2)0316967
720558                 MOVE RMA-CUR-TIERED-ANN(3) TO WMX-TIER-CUR-ANN(3)0316967
720559                 MOVE RMA-CUR-TIERED-ANN(4) TO WMX-TIER-CUR-ANN(4)0316967
720560                 MOVE RMA-CUR-TIERED-ANN(5) TO WMX-TIER-CUR-ANN(5)0316967
720561                 MOVE RMA-CUR-TIERED-ANN(6) TO WMX-TIER-CUR-ANN(6)0316967
720562                 MOVE RMA-CUR-TIERED-ANN(7) TO WMX-TIER-CUR-ANN(7)0316967
720563                 MOVE RMA-CUR-TIERED-ANN(8) TO WMX-TIER-CUR-ANN(8)0316967
720564                 MOVE RMA-CUR-TIERED-ANN(9) TO WMX-TIER-CUR-ANN(9)0316967
720565                 MOVE RMA-CUR-TIERED-LMT(1) TO WMX-TIER-CUR-LMT(1)0316967
720566                 MOVE RMA-CUR-TIERED-LMT(2) TO WMX-TIER-CUR-LMT(2)0316967
720567                 MOVE RMA-CUR-TIERED-LMT(3) TO WMX-TIER-CUR-LMT(3)0316967
720568                 MOVE RMA-CUR-TIERED-LMT(4) TO WMX-TIER-CUR-LMT(4)0316967
720569                 MOVE RMA-CUR-TIERED-LMT(5) TO WMX-TIER-CUR-LMT(5)0316967
720570                 MOVE RMA-CUR-TIERED-LMT(6) TO WMX-TIER-CUR-LMT(6)0316967
720571                 MOVE RMA-CUR-TIERED-LMT(7) TO WMX-TIER-CUR-LMT(7)0316967
720572                 MOVE RMA-CUR-TIERED-LMT(8) TO WMX-TIER-CUR-LMT(8)0316967
720573                 MOVE RMA-PREV-TIERED-DT    TO WMX-TIER-PREV-DATE 0316967
720574               MOVE RMA-PREV-TIERED-ANN(1) TO WMX-TIER-PREV-ANN(1)0316967
720575               MOVE RMA-PREV-TIERED-ANN(2) TO WMX-TIER-PREV-ANN(2)0316967
720576               MOVE RMA-PREV-TIERED-ANN(3) TO WMX-TIER-PREV-ANN(3)0316967
720577               MOVE RMA-PREV-TIERED-ANN(4) TO WMX-TIER-PREV-ANN(4)0316967
720578               MOVE RMA-PREV-TIERED-ANN(5) TO WMX-TIER-PREV-ANN(5)0316967
720579               MOVE RMA-PREV-TIERED-ANN(6) TO WMX-TIER-PREV-ANN(6)0316967
720580               MOVE RMA-PREV-TIERED-ANN(7) TO WMX-TIER-PREV-ANN(7)0316967
720581               MOVE RMA-PREV-TIERED-ANN(8) TO WMX-TIER-PREV-ANN(8)0316967
720582               MOVE RMA-PREV-TIERED-ANN(9) TO WMX-TIER-PREV-ANN(9)0316967
720583               MOVE RMA-PREV-TIERED-LMT(1) TO WMX-TIER-PREV-LMT(1)0316967
720584               MOVE RMA-PREV-TIERED-LMT(2) TO WMX-TIER-PREV-LMT(2)0316967
720585               MOVE RMA-PREV-TIERED-LMT(3) TO WMX-TIER-PREV-LMT(3)0316967
720586               MOVE RMA-PREV-TIERED-LMT(4) TO WMX-TIER-PREV-LMT(4)0316967
720587               MOVE RMA-PREV-TIERED-LMT(5) TO WMX-TIER-PREV-LMT(5)0316967
720588               MOVE RMA-PREV-TIERED-LMT(6) TO WMX-TIER-PREV-LMT(6)0316967
720589               MOVE RMA-PREV-TIERED-LMT(7) TO WMX-TIER-PREV-LMT(7)0316967
720590               MOVE RMA-PREV-TIERED-LMT(8) TO WMX-TIER-PREV-LMT(8)0316967
720591               PERFORM CALC-SAVT-CUR-DAF                          0316967
720592                  THRU CALC-SAVT-PREV-DAF-EXIT                    0316967
720593               PERFORM VERIFY-YR-BOUNDRY-FOR-SAVT                 0316967
720594                  THRU VERIFY-SAVT-EXIT                           0316967
720595         ELSE                                                     0316967
720600         IF  INTEREST-ADJUST-CODE EQUAL '3'
720610         AND WMS-RATE-TRLR EQUAL '1'                              0527330
720620             MOVE WMS-SAV-PREV-ANN                                9715504
720630                 TO ACCRUAL-ANNUAL-RATE                           9715504
720650             MOVE WMS-SAV-PREV-DAF TO ACCRUAL-DAILY-RATE          1003625
720700         ELSE
720710         IF WMS-RATE-TRLR EQUAL '1'                               0527330
720750             MOVE WMS-SAV-CUR-ANN                                 1003625
720800                 TO ACCRUAL-ANNUAL-RATE
720850             MOVE WMS-SAV-CUR-DAF                                 1003625
720900                 TO ACCRUAL-DAILY-RATE.
720910     IF  WMS-INVESTMENT-TRLR EQUAL '1'                            1004554
720920         MOVE WMS-INT-INV-SVC-RT    TO ACCRUAL-SVC-RT             1004554
720930         MOVE WMS-INV-SVC-ACCRD-CYC TO ACCRUED-SVC-ACCR.          1004554
720950     IF  WBC-ACCRUAL-PHASE EQUAL SPACES                           0266741
721000         MOVE ZERO TO ACCRUAL-AMOUNT                              0902509
721050         GO TO CALL-ACCRUAL-END.                                  0902509
721300     MOVE WBC-ACCRUAL-PHASE TO IM31-PH2.                          0266741
721350     IF IOD-FLAG EQUAL 'N'
721400         MOVE WMS-IOD-CYC-ACC-INT TO ACCRUED-TO-DATE
721450     ELSE MOVE WMS-ACCR-INT TO ACCRUED-TO-DATE.
721500     IF  SAVINGS-MINIMUM EQUAL '1'
721550         ADD WMS-MIN-AMT-FOR-SUPER TO WMS-INT-CUR-BAL
721600                                  WMS-CURR-BAL
721650                                  CUR-BAL.
721700     CALL 'SILINK'   USING IM31-PHASE
721750                           INTEREST-ACCRUAL-PARAMETERS
721800                           IOD-FLAG
721850                           DDA-WRKBCR-1                           0266741
721900                           DDA-WRKBCR-4                           0266741
721950                           MASTER-AREA
721955                           RATE-WORK-FLAGS                        1003625
721960                           WS-RWF-TIER-TABLE                      1003625
721965                           WS-RWF-DDA-TABLE                       1003625
721970                           WS-RWF-SAV-TABLE                       9715519
721975                           WS-RWF-SAVT-TABLE                      0316967
722000                           RATE-HIST-HOLD
722150                           RATE-HIST-ACCRUAL                      IM002
722160                           RATE-HIST-WORK                         0902213
722168                           WORK-BAL-HIST-IOD                      0902509
722176                           WORK-BAL-HIST-MMDA                     0902509
722184                           WORK-BAL-HIST-SAV                      0902509
722186                           WORK-BAL-HIST-SAV-TIER                 0316967
722192                           WORK-BAL-HIST-MMDA-AGGR                1004795
722195                           TISA-BACKDATE-ACCRUAL-PARMS            0316967
722197                           WMX-TIER-RATES.                        0316967
722200     IF  SAVINGS-MINIMUM EQUAL '1'
722250         SUBTRACT WMS-MIN-AMT-FOR-SUPER FROM WMS-INT-CUR-BAL
722300                                         WMS-CURR-BAL
722350                                         CUR-BAL.
722400     IF  HIFI-INEL-FLAG EQUAL '1'
722450         MOVE '1' TO HIFI-RPT-INEL-FLAG.
722500 CALL-ACCRUAL-END.   EXIT.
722550     SKIP3
722600 ADJUST-OR-ACCRUE.
722650     MOVE ACCRUAL-AMOUNT TO RHW-ACCRUAL-AMOUNT.
722700     IF  INTEREST-ADJUST-CODE NOT EQUAL '2'
722750         GO TO MOVE-DAILY-ACCRUAL.
722753     IF  NEG-ACCR-FLAG EQUAL '1'                                  0902493
722756         GO TO END-INT-A.                                         0902493
722760     IF  ACCRUAL-AMOUNT EQUAL ZERO                                IM003
722770         GO TO END-INT-A.                                         IM003
722800     IF  ACCRUAL-AMOUNT + HOLD-INTEREST NOT LESS THAN +0.00       0902213
722806         GO TO ADJ-OR-ACCR-CONT.                                  0902213
722812*--------------------------------------------------------------*  0902213
722818*  CHECK THE BCR NEGATIVE ACCRUAL OPTIONS                      *  0902213
722824*--------------------------------------------------------------*  0902213
722830     IF (IOD-FLAG EQUAL 'N'                                       0902213
722836         AND WBC-IOD-NEG-ACCRUAL EQUAL '1')                       0266741
722842     OR (IOD-FLAG EQUAL 'F'                                       0902213
722848         AND WBC-SAV-NEG-ACCRUAL EQUAL '1')                       0266741
722854         GO TO ADJ-OR-ACCR-CONT.                                  0902213
722890     GO TO END-INT-A.                                             0902213
722896 ADJ-OR-ACCR-CONT.                                                0902213
722910     MOVE ACCRUAL-AMOUNT TO BKDT-ACCR-BAL.                        1010055
722912     IF  BKDT-ACCR-BAL GREATER THAN ZERO                          9915255
722914         ADD +.005 TO BKDT-ACCR-BAL                               9915255
722916     ELSE                                                         9915255
722918         ADD -.005 TO BKDT-ACCR-BAL.                              9915255
722920     MOVE BKDT-ACCR-BAL TO HOLD-ACCR1.                            9915255
722950     MOVE HOLD-ACCR1 TO EX02-BAL.                                 9915255
723000     MOVE '02' TO IMEX-REC-NO.
723050     IF  IOD-FLAG EQUAL 'N'
723100         ADD ACCRUAL-AMOUNT TO WMS-IOD-CYC-ACC-INT                0902213
723200         MOVE 'K8' TO IMEX-CODE-1
723210         ADD +1 TO SEQ-CTR                                        IM004
723220         MOVE SEQ-CTR TO EX02-SEQ-NO                              IM004
723250         PERFORM WRITE-EX-MST THRU WRITE-EX-EXIT
723300     ELSE
723350         ADD ACCRUAL-AMOUNT TO WMS-ACCR-INT                       0902213
723450         MOVE 'K6' TO IMEX-CODE-1
723460         ADD +1 TO SEQ-CTR                                        IM004
723470         MOVE SEQ-CTR TO EX02-SEQ-NO                              IM004
723500         PERFORM WRITE-EX-MST THRU WRITE-EX-EXIT                  1004554
723510         IF  WMS-INVESTMENT-TRLR EQUAL '1'                        1004554
723520             ADD ACCRUAL-SVC-AMT TO WMS-INV-SVC-ACCRD-CYC.        1004554
723550     GO TO CALC-INT-CHANGE.
723600 MOVE-DAILY-ACCRUAL.
723650     IF IOD-FLAG EQUAL 'N'
723700         MOVE ACCRUAL-AMOUNT TO WMS-IOD-DAILY-ACC
723750     ELSE
723800         MOVE ACCRUAL-AMOUNT TO WMS-ACCRUAL-AMT.
723850 ADD-DAILY-ACCRUAL.
723900     IF IOD-FLAG EQUAL 'N'
723950         ADD WMS-IOD-DAILY-ACC TO WMS-IOD-CYC-ACC-INT
724000     ELSE
724050         ADD WMS-ACCRUAL-AMT TO WMS-ACCR-INT                      1004554
724060         IF  WMS-INVESTMENT-TRLR EQUAL '1'                        1004554
724070             ADD ACCRUAL-SVC-AMT TO WMS-INV-SVC-ACCRD-CYC.        1004554
724100 CALC-INT-CHANGE.
724105                                                                  1004795
724110     IF  INTEREST-ADJUST-CODE EQUAL '0' OR '1' OR '3'             1004795
724115         IF  IOD-FLAG EQUAL 'N'                                   1004795
724120             MOVE WMS-IOD-DAILY-ACC                               1004795
724125                                 TO TISA-ACCRUAL-AMOUNT           1004795
724130         ELSE                                                     1004795
724135             MOVE WMS-ACCRUAL-AMT                                 1004795
724140                                 TO TISA-ACCRUAL-AMOUNT.          1004795
724145                                                                  1004795
724150     IF IOD-FLAG EQUAL 'N'
724200         MOVE WMS-IOD-CYC-ACC-INT TO ACCRUAL-AMOUNT
724250     ELSE MOVE WMS-ACCR-INT TO ACCRUAL-AMOUNT.
724300     IF ACCRUAL-AMOUNT GREATER THAN ZERO
724350         ADD +.005 TO ACCRUAL-AMOUNT
724400     ELSE
724450         ADD -.005 TO ACCRUAL-AMOUNT.
724500     MOVE ACCRUAL-AMOUNT TO HOLD-ACCR1.
724550     SUBTRACT HOLD-ACCR2 FROM HOLD-ACCR1.
724560     IF  (WMS-IOD-ACCRUAL-TYPE EQUAL 'C' OR 'D')                  1105232
724570         PERFORM 2000-APYE-SPEC-RULE THRU 9999-APYE-EXIT          1105232
724575     ELSE                                                         1105232
724580         MOVE 'N' TO WS-SKIP-APYE.                                1105232
724600     IF  IOD-FLAG EQUAL 'N'                                       1004795
724620         ADD  HOLD-ACCR1         TO WMS-IOD-MTD-ACC-INT           1004795
724630                                    WMS-IOD-YTD-ACC-INT           1105132
724650         ADD  TISA-ACCRUAL-AMOUNT                                 1004795
724655                                 TO WMS-IOD-TIS-EARN-INT          1004795
724660         IF  (WS-SKIP-APYE EQUAL 'Y')                             1105232
724665         AND (TISA-ACCRUAL-AMOUNT NOT EQUAL HOLD-ACCR1)           1105232
724667         AND (INTEREST-ADJUST-CODE EQUAL '2')                     1105232
724670             COMPUTE WMS-IOD-TIS-CYC-BEGIN-ACCR ROUNDED EQUAL     1105232
724675                     WMS-IOD-TIS-CYC-BEGIN-ACCR +                 1105232
724680                     TISA-ACCRUAL-AMOUNT - HOLD-ACCR1             1105232
724685         ELSE                                                     1105232
724690             NEXT SENTENCE                                        1105232
724695                                                                  1105232
724700     ELSE
724715         ADD  HOLD-ACCR1         TO WMS-MTD-ACCR-INT              1004795
724716                                    WMS-YTD-ACCR-INT              1105132
724720         ADD  TISA-ACCRUAL-AMOUNT                                 1004795
724730                                 TO WMS-INT-TIS-EARN-INT          1105232
724735         IF  (WS-SKIP-APYE EQUAL 'Y')                             1105232
724740         AND (TISA-ACCRUAL-AMOUNT NOT EQUAL HOLD-ACCR1)           1105232
724743         AND (INTEREST-ADJUST-CODE EQUAL '2')                     1105232
724745             COMPUTE WMS-INT-TIS-CYC-BEGIN-ACCR ROUNDED EQUAL     1105232
724750                     WMS-INT-TIS-CYC-BEGIN-ACCR +                 1105232
724755                     TISA-ACCRUAL-AMOUNT - HOLD-ACCR1.            1105232
724760     IF  WMS-IOD-TIS-EARN-INT LESS THAN ZERO                      1004015
724765         MOVE ZERO TO WMS-IOD-TIS-EARN-INT.                       1004015
724770     IF  WMS-SAVINGS-TRLR EQUAL '1'                               1004015
724775     AND WMS-INT-TIS-EARN-INT LESS THAN ZERO                      1004015
724780         MOVE ZERO TO WMS-INT-TIS-EARN-INT.                       1004015
724782     IF  IOD-FLAG EQUAL 'F'                                       1004554
724784         IF  WMS-INVESTMENT-TRLR EQUAL '1'                        1004554
724786             MOVE WMS-INV-SVC-ACCRD-CYC TO ACCRUAL-SVC-AMT        1004554
724788             ADD +.005 ACCRUAL-SVC-AMT GIVING HOLD-SVC-FEE1       1004554
724790             SUBTRACT HOLD-SVC-FEE2 FROM HOLD-SVC-FEE1            1004554
724791             ADD HOLD-SVC-FEE1 TO WMS-INV-SVC-ACCRD-MTD           1004554
724792             IF  INTEREST-ADJUST-CODE EQUAL '2'                   1004554
724793                 ADD HOLD-SVC-FEE1 TO INV-SVC-ADJ-INTL            1004554
724794                                      WS-GL-INV-SVC-ADJ-INTL      1004554
724795             ELSE                                                 1004554
724796                 ADD HOLD-SVC-FEE1 TO WMS-INV-SVC-ACCRD-TDY.      1004554
724800     IF  INTEREST-ADJUST-CODE NOT EQUAL '2'
724850         IF IOD-FLAG EQUAL 'N'
724900             ADD HOLD-ACCR1 TO IOD-INT-ACC-TODAY
724901                               WS-GL-IOD-INT-ACC-TODAY            0902970
724950                               WMS-IOD-ACCRD-TODAY
724960             IF  ACCRUAL-BALANCE GREATER THAN ZERO                1003110
724970                 MOVE ACCRUAL-BALANCE TO WMS-IOD-ACCRUAL-BAL      1003110
724980             ELSE                                                 1003110
724990                 MOVE +0              TO WMS-IOD-ACCRUAL-BAL      1003110
725000         ELSE
725005             ADD HOLD-ACCR1 TO WMS-INT-ACCRD-TODAY                1004554
725010             IF  WMS-INVESTMENT-TRLR EQUAL '1'                    1004554
725015                 ADD HOLD-ACCR1 TO INV-DIV-ACCRD-TDY              1004554
725020                                   WS-GL-INV-DIV-ACCRD-TDY        1004554
725025             ELSE                                                 1004554
725050                 ADD HOLD-ACCR1 TO DDA-INT-ACC-TODAY              1004554
725051                                   WS-GL-DDA-INT-ACC-TODAY.       1004554
725070     IF  INTEREST-ADJUST-CODE NOT EQUAL '2'                       1003110
725080     AND IOD-FLAG NOT EQUAL 'N'                                   1003110
725110         IF  ACCRUAL-BALANCE GREATER THAN ZERO                    1003110
725120             MOVE ACCRUAL-BALANCE TO WMS-INT-ACCRUAL-BAL          1003110
725130         ELSE                                                     1003110
725140             MOVE +0              TO WMS-INT-ACCRUAL-BAL.         1003110
725150     IF  INTEREST-ADJUST-CODE EQUAL '2'                           0902213
725157         IF IOD-FLAG EQUAL 'N'                                    0902213
725164             ADD HOLD-ACCR1 TO IOD-INT-ACC-ADJ-INT                0902213
725165                               WS-GL-IOD-INT-ACC-ADJ-INT          0902970
725171         ELSE                                                     0902213
725172             IF  WMS-INVESTMENT-TRLR EQUAL '1'                    1004554
725173                 ADD HOLD-ACCR1 TO INV-DIV-INTL-ADJ               1004554
725174                                   WS-GL-INV-DIV-INTL-ADJ         1004554
725175             ELSE                                                 1004554
725178                 ADD HOLD-ACCR1 TO DDA-INT-ACC-ADJ-INT            1004554
725179                                   WS-GL-DDA-INT-ACC-ADJ-INT.     1004554
725185     MOVE RHW-ACCRUAL-AMOUNT TO ACCRUAL-AMOUNT.                   0902213
725200 END-INT-A.
725250     MOVE 'F' TO IOD-FLAG.
725270     MOVE '0' TO NEG-ACCR-FLAG.                                   0902493
725300 END-INT-ADJUST.
725350     EXIT.
725400 PROJECT-UNAVAIL.                                                 IM007
725450     MOVE WK-CUST-AVAIL-2-12 TO WORK-CUST-AVAIL-TRLR.             IM007
725460     MOVE ZEROS TO WK-CUST-AVAIL-FUNDS (10).                      IM007
725520     ADD +1 TO W.                                                 IM002
725550     SUBTRACT WK-CUST-AVAIL-FUNDS (1)                             IM007
725580              WK-CUST-AVAIL-FUNDS (2)                             IM007
725610              WK-CUST-AVAIL-FUNDS (3)                             IM007
725640              WK-CUST-AVAIL-FUNDS (4)                             IM007
725670              WK-CUST-AVAIL-FUNDS (5)                             IM007
725700              WK-CUST-AVAIL-FUNDS (6)                             IM007
725730              WK-CUST-AVAIL-FUNDS (7)                             IM007
725760              WK-CUST-AVAIL-FUNDS (8)                             IM007
725790              WK-CUST-AVAIL-FUNDS (9) FROM CUR-BAL.               IM007
726000     MOVE CUR-BAL TO ACCRUAL-BALANCE.
726010     MOVE ZERO TO WK-CUST-AVAIL-FUNDS (W).                        IM007
726020     MOVE HOLD-CUR-BAL TO CUR-BAL.                                IM002
726050     GO  TO  PROJECT-ACCRUALS.
726100 PROJECT-FLOAT.
726150     MOVE WK-BANK-AVAIL-2-7  TO WORK-BANK-AVAIL-TRLR.             IM007
726200     MOVE ZEROS TO WK-BANK-AVAIL-AMT (7).                         IM007
726220     ADD +1 TO W.                                                 IM002
726250     SUBTRACT WK-BANK-AVAIL-AMT (1)                               IM007
726270              WK-BANK-AVAIL-AMT (2)                               IM007
726290              WK-BANK-AVAIL-AMT (3)                               IM007
726310              WK-BANK-AVAIL-AMT (4)                               IM007
726330              WK-BANK-AVAIL-AMT (5)                               IM007
726350              WK-BANK-AVAIL-AMT (6) FROM CUR-BAL.                 IM007
726505     MOVE CUR-BAL TO ACCRUAL-BALANCE.                             IM004
726510     MOVE ZERO TO WK-BANK-AVAIL-AMT (W).                          IM007
726520     MOVE HOLD-CUR-BAL TO CUR-BAL.                                IM002
726600 PROJECT-ACCRUALS.
726650     IF  ACCRUAL-BALANCE NOT GREATER THAN ZERO                    1001295
726660         GO TO PROJECT-ACCRUALS-END.                              1001295
726670     MOVE 'P' TO INTEREST-ADJ-SUBCODE.                            1001295
726672     IF IOD-FLAG EQUAL 'N'                                        1001295
726674         MOVE WMS-IOD-CYC-ACC-INT TO ACCRUED-TO-DATE              1001295
726676     ELSE MOVE WMS-ACCR-INT TO ACCRUED-TO-DATE.                   1001295
726680     IF  YEAR-CHANGE EQUAL '0'                                    1001295
726700         PERFORM CALL-ACCRUAL-PHASE THRU CALL-ACCRUAL-END
726705     ELSE                                                         1001295
726710         PERFORM X0300-RHR-ACCRUAL THRU X0320-RHR-ACCRUAL-EXIT    1001295
726720         IF  HIFI-INEL-FLAG EQUAL '1'                             1001295
726730             MOVE '1' TO HIFI-RPT-INEL-FLAG.                      1001295
726740     MOVE 'N' TO INTEREST-ADJ-SUBCODE.                            1001295
726750     ADD ACCRUAL-AMOUNT TO ACCR-AMT1.                             1001295
726752     IF IOD-FLAG EQUAL 'F'                                        1004554
726754        IF  WMS-INVESTMENT-TRLR EQUAL '1'                         1004554
726756            ADD ACCRUAL-SVC-AMT TO ACCR-SVC-AMT.                  1004554
726758 PROJECT-ACCRUALS-END.                                            1004554
726760     EXIT.                                                        1004554
726762                                                                  1004554
726764 CALL-INVEST-PHASE.                                               1004554
726765     IF  WBC-IM31-INVEST-PH EQUAL SPACES                          0627557
726766         MOVE PROGRAM-NAME TO SIMESS-PROGRAM                      0627557
726767         MOVE 505 TO SIMESS-MESS-NO                               0627557
726768         MOVE 'NO INVESTMENT PHASE' TO SIMESS-OPTIONAL-MESSAGE    0627557
726769         CALL 'SIMESS' USING SIMESS-AREA                          0627557
726770         GO TO Z9900.                                             0627557
726771     MOVE WBC-IM31-INVEST-PH TO IM31-PH2.                         0627557
726772     CALL 'SILINK' USING IM31-PHASE                               0627557
726773                         MASTER-AREA                              0627557
726774                         DDA-WRKBCR-1                             0627557
726775                         DDA-WRKBCR-4                             0627557
726776                         TRAN-FILE                                0627557
726777                         EXCEPTION-AREA                           0627557
726778                         EXCEPTION-LENGTH-TABLE                   1004554
726780                         EX-SEQ-CTR                               1004554
726782                         EX-MST-CTL                               1004554
726784                         INTEREST-ACCRUAL-PARAMETERS              1004554
726786                         TODAYS-INTRST-PYMT                       1004554
726788                         DATE-LAST-ACTIVE.                        1004554
726790 CALL-INVEST-EXIT.                                                1004554
726792     EXIT.                                                        1004554
726800     SKIP3
726801 X0100-6-TO-8-LO.                                                 IM008
726802     MOVE DT-L-YR TO C-LOW-YR.                                    IM008
726803     IF  DT-L-YR LESS THAN WBC-NEXT-CENT-YR                       0266741
726804         MOVE '20' TO C-LOW-CENT                                  IM008
726805     ELSE                                                         IM008
726806         MOVE '19' TO C-LOW-CENT.                                 IM008
726807     MOVE DT-L-MO TO C-LOW-MO.                                    IM008
726808     MOVE DT-L-DA TO C-LOW-DA.                                    IM008
726809                                                                  IM008
726810 X0110-6-TO-8-HI.                                                 IM008
726811     MOVE DT-H-YR TO C-HIGH-YR.                                   IM008
726812     IF  DT-H-YR LESS THAN WBC-NEXT-CENT-YR                       0266741
726813         MOVE '20' TO C-HIGH-CENT                                 IM008
726814     ELSE                                                         IM008
726815         MOVE '19' TO C-HIGH-CENT.                                IM008
726816     MOVE DT-H-MO TO C-HIGH-MO.                                   IM008
726817     MOVE DT-H-DA TO C-HIGH-DA.                                   IM008
726818                                                                  IM008
726819 X0120-8-TO-6-LO.                                                 IM008
726820     MOVE C-LOW-MO TO DT-L-MO.                                    IM008
726821     MOVE C-LOW-DA TO DT-L-DA.                                    IM008
726822     MOVE C-LOW-YR TO DT-L-YR.                                    IM008
726823                                                                  IM008
726824 X0130-8-TO-6-HI.                                                 IM008
726825     MOVE C-HIGH-MO TO DT-H-MO.                                   IM008
726826     MOVE C-HIGH-DA TO DT-H-DA.                                   IM008
726827     MOVE C-HIGH-YR TO DT-H-YR.                                   IM008
726828                                                                  IM008
726830 X0140-BUILD-EDITED-ACCT.                                         0417078
726832     MOVE WBC-ACCT-EDIT-MASK TO ACCT-WK-AREA.                     0417078
726834     CALL 'IMACTEDT'      USING WMS-CTL4-ACCT                     0417078
726836                                ACCT-WK-AREA                      0417078
726838                                WBC-ACCT-DISPLACEMENT             0417078
726840                                WBC-CTL4-FLAG.                    0417078
726842     MOVE WK-ED-ACCT         TO WMS-EDITED-ACCT.                  0417078
726844 X0140-EXIT.                                                      0417078
726846     EXIT.                                                        0417078
726850     SKIP3
726900 X0200-RATE-HIST-RETRIEVAL.
726950     MOVE +0 TO RHW-NO-DAYS
727000                RHA-OLD-AMOUNT
727020                TISA-RHA-NEW-AMOUNT                               1004795
727030                TISA-ACCRUAL-AMOUNT                               1004795
727040                TISA-ACCRUAL-DAYS                                 1004795
727050                RHA-NEW-AMOUNT
727060                RHA-NEW-SVC-AMT                                   1004554
727070                ACCRUAL-SVC-AMT                                   1004554
727100                ACCRUAL-AMOUNT.
727120     MOVE '0' TO NEG-ACCR-FLAG.                                   0902493
727150     MOVE HOLD-BKDT-MO TO RHW-TRAN-MO.
727200     MOVE HOLD-BKDT-DA TO RHW-TRAN-DA.
727250     MOVE HOLD-BKDT-YR TO RHW-TRAN-YR.
727300     IF  HOLD-BKDT-YR GREATER THAN WBC-NEXT-CENT-YR               0266741
727350         MOVE '19' TO RHW-TRAN-CENT
727400     ELSE
727450         MOVE '20' TO RHW-TRAN-CENT.
727453     IF  HOLD-BH-DATE NOT EQUAL SPACES                            1004795
727456         MOVE HOLD-BH-DATE TO RHW-TRAN-DATE                       1004795
727459         MOVE SPACES TO HOLD-BH-DATE.                             1004795
727462                                                                  1004795
727465 X0200-BUILD-TIS-BKDT-DATE.                                       1004795
727468*                                                                 1004795
727471*--------------------------------------------------------------*  1004795
727474*  BUILD THE TISA BACKDATE DATE.  THIS DATE IS THE GREATER     *  1004795
727477*  OF THE TRAN BACKDATE DATE AND THE DATE OF THE LAST          *  1004795
727480*  STATEMENT PLUS 1.  THIS DATE IS ONLY CALCULATED FOR IOD AND *  1004795
727483*  SAVINGS BACKDATES.   THE TISA BACKDATE WILL BE USED TO      *  1004795
727486*  DETERMINE THE NUMBER OF DAYS TO ACCRUE INTEREST FOR THE     *  1004795
727489*  TISA INTEREST EARNED FIELD.                                 *  1004795
727492*--------------------------------------------------------------*  1004795
727495*                                                                 1004795
727498     IF  RHW-LOAN EQUAL 'N'                                       1004795
727501         GO TO X0200-BUILD-TIS-BKDT-DT-EXIT.                      1004795
727504                                                                  1004795
727507     IF  WMS-DATE-LAST-STMT EQUAL SPACES                          1004795
727510         MOVE RHW-TRAN-DATE      TO TISA-RHW-TRAN-DATE            1004795
727513         GO TO X0200-BUILD-TIS-BKDT-DT-EXIT.                      1004795
727516                                                                  1004795
727519     MOVE WMS-LAST-STMT-YR       TO TISA-RHW-TRAN-YR.             1004795
727522     MOVE WMS-LAST-STMT-DA       TO TISA-RHW-TRAN-DA.             1004795
727525     MOVE WMS-LAST-STMT-MO       TO TISA-RHW-TRAN-MO.             1004795
727528                                                                  1004795
727531     IF  WMS-LAST-STMT-YR GREATER THAN WBC-NEXT-CENT-YR           0266741
727534         MOVE '19'               TO TISA-RHW-TRAN-CENT            1004795
727537     ELSE                                                         1004795
727540         MOVE '20'               TO TISA-RHW-TRAN-CENT.           1004795
727543                                                                  1004795
727546     IF  RHW-TRAN-DATE GREATER THAN TISA-RHW-TRAN-DATE            1004795
727549         MOVE RHW-TRAN-DATE      TO TISA-RHW-TRAN-DATE            1004795
727552         GO TO X0200-BUILD-TIS-BKDT-DT-EXIT.                      1004795
727555*                                                                 1004795
727558*  THE TISA BACKDATE DATE IS CALCULATED TO BE THE DAY AFTER    *  1004795
727561*  THE LAST STATEMENT DATE IN THE FOLLOWING CODE.              *  1004795
727564*                                                                 1004795
727567     MOVE TISA-RHW-TRAN-DATE     TO C-LOW-DATE.                   1004795
727570     MOVE +1                     TO C-NO-DAYS.                    1004795
727573                                                                  1004795
727576     CALL 'SIDCHI'            USING C-DATE-WORK.                  1004795
727579                                                                  1004795
727582     MOVE C-HIGH-DATE            TO TISA-RHW-TRAN-DATE.           1004795
727585                                                                  1004795
727588 X0200-BUILD-TIS-BKDT-DT-EXIT.                                    1004795
727591                                                                  1004795
727600     IF  RHW-LOAN EQUAL 'N'                                       1004795
727610         IF  RHW-NON-HIST EQUAL 'N'                               1004795
727620             MOVE RHW-TRAN-DATE TO RNX-SPLT-DT                    1004795
727630                                   C-LOW-DATE                     1004795
727640             GO TO X0280-RHR-CALC-DAYS                            1004795
727650         ELSE                                                     1004795
727700             GO TO X0230-RHR-START.                               1004795
727750     IF  IOD-FLAG EQUAL 'N'
727800         MOVE WMS-IOD-CYC-ACC-INT TO ACCRUED-TO-DATE
727810         MOVE WMS-IOD-TIS-EARN-INT TO TISA-ACCRUED-TO-DATE        1004795
727850     ELSE
727900         MOVE WMS-ACCR-INT TO ACCRUED-TO-DATE
727940         MOVE WMS-INT-TIS-EARN-INT TO TISA-ACCRUED-TO-DATE        1004795
727942         IF  WMS-SAV-TIER-PTR GREATER +0                          0316967
727943             MOVE WMS-SAV-TIER-PTR TO RHW-IND                     0316967
727944             MOVE 'T' TO RHW-TYPE                                 0316967
727945             ADD RHA-OLD-SAV-BAL ACCRUAL-BALANCE                  0316967
727946                                 GIVING RHA-NEW-SAV-BAL           0316967
727947             ADD RHA-OLD-SAV-BAL ACCRUAL-BALANCE                  0316967
727948                                 GIVING RHA-NEW-BALANCE           0316967
727949             GO TO X0230-RHR-START                                0316967
727950         ELSE                                                     0316967
727952             MOVE WMS-INT-RATE TO RHW-IND                         0316967
727954             MOVE 'I' TO RHW-TYPE                                 0316967
727956             GO TO X0230-RHR-START.                               0316967
728100     IF  WMS-HIFI-INDICATOR GREATER THAN +0
728150         MOVE WMS-HIFI-INDICATOR TO RHW-IND                       1003625
728155         MOVE 'T' TO RHW-TYPE                                     1003625
728250     ELSE
728300         IF  WMS-IOD-RATE-PTR GREATER THAN +0
728350             MOVE WMS-IOD-RATE-PTR TO RHW-IND
728355             MOVE 'I' TO RHW-TYPE                                 1003625
728500         ELSE
728550             GO TO X0880-RHR-EXIT.
728600     MOVE RHW-DATE-LAST-ACT TO DT-HIGH-DATE.
728650     MOVE HOLD-BKDT-DATE TO DT-LOW-DATE.
728660     IF  WBC-ALLOW-BCKDT-HIFI EQUAL '1'                           0266741
728665     OR  BHW-IOD-FLAG NOT EQUAL 'F'                               0902509
728670         GO TO X0202-RHR-SET-RHA.                                 0902493
728700     IF  WMS-DATE-LAST-MONETARY-ACT EQUAL HOLD-BKDT-DATE
728750         IF  HOLD-BKDT-DATE EQUAL WMS-DATE-LAST-SERV-CHG
728800         OR  HOLD-BKDT-DATE EQUAL WMS-DATE-LAST-STMT
728850             MOVE '1' TO EX13-REASON-OR-OPT
728900             GO TO X0800-RHR-EXC-K.
728950     CALL 'SIDIF1' USING DATE-AREA.
729000     IF  RET-DAYS GREATER THAN ZERO
729050         MOVE '1' TO EX13-REASON-OR-OPT
729100         GO TO X0800-RHR-EXC-K.
729120                                                                  0902493
729121 X0202-RHR-SET-RHA.                                               0902493
729122     MOVE CUR-BAL TO HOLD-CUR-BAL.                                0902493
729123     MOVE RHA-OLD-DDA-BAL TO CUR-BAL.                             0902493
729124     MOVE WMS-IOD-CALC TO CALC-FLAG.                              0902493
729125     PERFORM K0104 THRU K0105.                                    0902493
729126     ADD RHA-OLD-DDA-BAL ACCRUAL-BALANCE GIVING RHA-NEW-DDA-BAL.  0902493
729127     MOVE CUR-BAL TO RHA-OLD-BALANCE.                             0902493
729128     ADD CUR-BAL ACCRUAL-BALANCE GIVING RHA-NEW-BALANCE.          0902493
729129     MOVE WMS-IOD-CYC-ACC-INT TO RHA-OLD-TO-DATE.                 0902493
729130     MOVE ZERO TO RHA-NEW-TO-DATE.                                0902493
729131     MOVE HOLD-CUR-BAL TO CUR-BAL.                                0902493
729132                                                                  0902493
729133*----------------------------------------------------------------*0902493
729134*THE FOLLOWING TABLE LISTS THE POSSIBLE BALANCE COMBINATIONS AND *0902493
729135*THE ACTION THAT WILL RESULT DEPENDING ON THE TRAN AMOUNT.       *0902493
729136*THIS ACTION WILL ONLY OCCUR IF THE FOLLOWING IS TRUE:           *1003507
729137*                                                                *0902493
729139*  1)  BALANCE HISTORY DOES NOT EXIST FOR THIS ACCOUNT.          *1003507
729140*                                                                *0902493
729141* OLD BAL   NEW BAL  TRAN AMOUNT   ACTION/ACCRUAL BALANCE        *0902493
729142*--------  --------  -----------   ------------------------------*0902493
729143*POSITIVE  POSITIVE                USE TRAN AMOUNT TO EITHER     *0902493
729144*                                  DEBIT OR CREDIT ACCRUALS.     *0902493
729145*     500       600         100+    100+                         *9915845
729146*     500       400         100-    100-                         *9915845
729147*                                                                *0902493
729148*NEGATIVE  NEGATIVE                USE TRAN AMOUNT TO CALCULATE  *0902493
729149*                                  ACCRUAL BUT DO NOT POST.      *0902493
729150*                                  GENERATE A 'NEGATIVE ACCRUAL' *0902493
729151*                                  EXCEPTION ON BACKDATE REPORT  *0902493
729152*                                  (IM41NG).                     *0902493
729153*     500-      400-        100+    100+                         *9915845
729154*     500-      600-        100-    100-                         *9915845
729155*                                                                *0902493
729156*POSITIVE  NEGATIVE                REDUCE TRAN AMOUNT BY AMOUNT  *0902493
729157*                                  OF NEW BALANCE. CALCULATE BY  *0902493
729158*                                  SUBTRACTING NEW BALANCE FROM  *0902493
729159*                                  TRAN AMOUNT.                  *0902493
729160*     500       500-       1000-    500-                         *9915845
729161*     500       100-        600-    500-                         *9915845
729162*                                                                *0902493
729163*NEGATIVE  POSITIVE                ACCRUE ON NEW BALANCE AMOUNT  *0902493
729164*                                  INSTEAD OF TRANSACTION AMOUNT.*0902493
729165*     500-      500        1000+    500+                         *9915845
729166*     500-      100         600+    100+                         *9915845
729167*----------------------------------------------------------------*0902493
729173     IF  WMS-BALANCE-HISTORY NOT EQUAL '0'                        0902493
729174         GO TO X0205-HIFI-BKDT.                                   0902493
729175     IF  RHA-OLD-BALANCE NOT LESS ZERO                            0902493
729176         IF  RHA-NEW-BALANCE NOT LESS ZERO                        0902493
729177             GO TO X0205-HIFI-BKDT.                               0902493
729178     IF  RHA-OLD-BALANCE NOT GREATER ZERO                         0902493
729179         IF  RHA-NEW-BALANCE NOT GREATER ZERO                     0902493
729180             MOVE '1' TO NEG-ACCR-FLAG                            0902493
729181             GO TO X0205-HIFI-BKDT.                               0902493
729182     IF  RHA-OLD-BALANCE NOT LESS ZERO                            0902493
729183         IF  RHA-NEW-BALANCE NOT GREATER ZERO                     0902493
729184             SUBTRACT RHA-NEW-BALANCE FROM ACCRUAL-BALANCE.       0902493
729185     IF  RHA-OLD-BALANCE NOT GREATER ZERO                         0902493
729186         IF  RHA-NEW-BALANCE NOT LESS ZERO                        0902493
729187             MOVE RHA-NEW-BALANCE TO ACCRUAL-BALANCE.             0902493
729189                                                                  0902493
729190 X0205-HIFI-BKDT.                                                 0902493
729191     IF  WMS-HIFI-INDICATOR EQUAL +0                              0902493
729192         GO TO X0215-IOD-BKDT.                                    0902493
729193     MOVE WMS-TIER-CUR-DATE TO C-HIGH-DATE.                       1003625
729194     PERFORM X0130-8-TO-6-HI.                                     0902493
729200     CALL 'SIDIF1' USING DATE-AREA.
730000*                                                                 9915581
730350 X0210-RHR-SET-PRV.
730890     MOVE WMS-HIFI-INDICATOR TO RHW-IND.                          1003625
730895     MOVE 'T' TO RHW-TYPE                                         1003625
730900     GO TO X0230-RHR-START.                                       0902493
730910 X0215-IOD-BKDT.                                                  0902509
730920     MOVE WMS-IOD-CUR-DATE TO C-HIGH-DATE.                        1003625
730930     PERFORM X0130-8-TO-6-HI.                                     0902509
730940     CALL 'SIDIF1' USING DATE-AREA.                               0902509
731100*                                                                 9915581
731700 X0230-RHR-START.
731750     MOVE 'F' TO RHW-SW.
731755     MOVE RHW-GEN-KEY   TO RMA-KEY.                               1003625
731835 X0235-RHR-RESTART.                                               IM008
731840     MOVE 'S' TO I-O-CONTROL-OPERATOR.                            IM008
732000     CALL 'IMRTEMV' USING I-O-CONTROL-AREA                        1003625
732050                          RATE-MASTER-AREA.                       1003625
732080     IF  I-O-88-NOT-FOUND                                         IM008
732110         IF  RMA-PRODUCT EQUAL SPACES                             1003625
732112         AND RMA-REGION EQUAL SPACES                              1003625
732120             MOVE +2 TO SIMESS-MESS-NO                            IM008
732130             MOVE 'NO DEFAULT RATE MASTER RECORD FOUND'           1003625
732140                   TO SIMESS-OPTIONAL-MESSAGE                     IM008
732150             CALL 'SIMESS' USING SIMESS-AREA                      IM008
732160             MOVE '3' TO EX13-REASON-OR-OPT                       1003625
732165             GO TO X0800-RHR-EXC-K                                1003625
732170         ELSE                                                     IM008
732172         IF  RMA-PRODUCT NOT EQUAL SPACES                         1003625
732180             MOVE SPACES TO RMA-PRODUCT                           1003625
732190             GO TO X0235-RHR-RESTART                              1003625
732192         ELSE                                                     1003625
732194         IF  RMA-REGION  NOT EQUAL SPACES                         1003625
732196             MOVE SPACES TO RMA-REGION                            1003625
732198             GO TO X0235-RHR-RESTART.                             1003625
732400 X0240-RHR-READ.
732450     MOVE 'R' TO I-O-CONTROL-OPERATOR.
732500     CALL 'IMRTEMV' USING I-O-CONTROL-AREA                        1003625
732550                          RATE-MASTER-AREA.                       1003625
732560     MOVE RMA-RET-OLD TO HOLD-RTE-RET.                            1003625
732570     MOVE BIN-1 TO X.                                             IM008
732580                                                                  IM008
732590 X0250-RHR-RATE-CHK.                                              IM008
732600     IF  RMA-IOD-SAV                                              1003625
732610     OR  RMA-LOANS                                                1003625
732620     OR  RMA-OD-ACCR                                              1003625
732630         NEXT SENTENCE                                            IM008
732640     ELSE                                                         IM008
732650        GO TO X0250-RHR-MMDA-CHK.                                 IM008
732658     IF  RMA-RET-OLD GREATER 0                                    9915845
732659         MOVE RMA-RET-OLD TO HX                                   9916139
732660         PERFORM X0250-RHR-ILO-MOVE                               9915845
732662             VARYING HR FROM 1 BY 1                               9915845
732664             UNTIL HR GREATER THAN RMA-RET-OLD.                   9915845
732670     IF  RMA-PREV-ILO-DT NOT EQUAL SPACES AND ZEROES              1003625
732680         ADD +1 TO HOLD-RTE-RET                                   1003625
732682         MOVE HOLD-RTE-RET TO HR                                  9915845
732690         MOVE RMA-PREV-ILO-DT     TO RHH-PTR-DT (HR)              9915845
732692         MOVE RMA-PREV-ILO-ANN    TO RHH-PTR-ANN (HR)             9915845
732700         ADD +1 TO HOLD-RTE-RET HR                                9915845
732702         MOVE RMA-CUR-ILO-DT      TO RHH-PTR-DT (HR)              9915845
732704         MOVE RMA-CUR-ILO-ANN     TO RHH-PTR-ANN (HR)             9915845
732720     ELSE                                                         IM008
732730         ADD +1 TO HOLD-RTE-RET                                   1003625
732732         MOVE HOLD-RTE-RET TO HR                                  9915845
732734         MOVE RMA-CUR-ILO-DT      TO RHH-PTR-DT (HR)              9915845
732736         MOVE RMA-CUR-ILO-ANN     TO RHH-PTR-ANN (HR).            9915845
732740     PERFORM LEAP-CHECK-RATE    THRU LEAP-CHECK-EXIT.             9915577
732750     IF  RHH-PTR-DT (X) GREATER THAN RHW-TRAN-DATE                IM008
732753     AND (INTEREST-ADJ-SUBCODE NOT EQUAL 'T' AND 'F'              0902213
732756     AND 'U' AND 'I')                                             0902213
732760         MOVE '3' TO EX13-REASON-OR-OPT                           IM008
732770         GO TO X0800-RHR-EXC-K                                    IM008
732780     ELSE                                                         IM008
732790         MOVE RHH-PTR-DATA (X) TO RNX-PTR-DATA                    IM008
732795         PERFORM CALC-RNX-DAF THRU CALC-RNX-DAF-EXIT              1003625
732800         GO TO X0260-RHR-PROCESS-HIST.                            IM008
732801                                                                  9915845
732802 X0250-RHR-ILO-MOVE.                                              9915845
732803     IF  RMA-OLD-ILO-ANN(HX) NUMERIC                              9916139
732804         MOVE RMA-OLD-ILO-DT(HX)   TO RHH-PTR-DT (HR)             9916139
732805         MOVE RMA-OLD-ILO-ANN(HX)  TO RHH-PTR-ANN (HR).           9916139
732806     SUBTRACT +1                 FROM HX.                         9916139
732810                                                                  IM008
732820 X0250-RHR-MMDA-CHK.                                              IM008
732830     IF  RMA-TIERED                                               1003625
732840         NEXT SENTENCE                                            IM008
732850     ELSE                                                         IM008
732860         GO TO X0250-RHR-SPLT-CHK.                                IM008
732868     IF  RMA-RET-OLD GREATER 0                                    9915845
732869         MOVE RMA-RET-OLD TO HX                                   9916139
732870         PERFORM X0250-RHR-TIER-MOVE                              9915845
732872             VARYING HR FROM 1 BY 1                               9915845
732874             UNTIL HR GREATER THAN RMA-RET-OLD.                   9915845
732880     IF  RMA-PREV-TIERED-DT NOT EQUAL SPACES AND ZEROES           1003625
732890         ADD +1 TO HOLD-RTE-RET                                   1003625
732892         MOVE HOLD-RTE-RET TO HR                                  9915845
732893         MOVE RMA-PREV-TIERED-DT    TO RHH-FUND-DT (HR)           9915845
732894         MOVE RMA-PREV-TIERED-ANN(1) TO RHH-FND-ANN (HR 1)        9915845
732895         MOVE RMA-PREV-TIERED-ANN(2) TO RHH-FND-ANN (HR 2)        9915845
732896         MOVE RMA-PREV-TIERED-ANN(3) TO RHH-FND-ANN (HR 3)        9915845
732897         MOVE RMA-PREV-TIERED-ANN(4) TO RHH-FND-ANN (HR 4)        9915845
732898         MOVE RMA-PREV-TIERED-ANN(5) TO RHH-FND-ANN (HR 5)        9915845
732899         MOVE RMA-PREV-TIERED-ANN(6) TO RHH-FND-ANN (HR 6)        9915845
732900         MOVE RMA-PREV-TIERED-ANN(7) TO RHH-FND-ANN (HR 7)        9915845
732901         MOVE RMA-PREV-TIERED-ANN(8) TO RHH-FND-ANN (HR 8)        9915845
732902         MOVE RMA-PREV-TIERED-ANN(9) TO RHH-FND-ANN (HR 9)        9915845
732903         MOVE RMA-PREV-TIERED-LMT(1) TO RHH-FND-LMT (HR 1)        9915845
732904         MOVE RMA-PREV-TIERED-LMT(2) TO RHH-FND-LMT (HR 2)        9915845
732905         MOVE RMA-PREV-TIERED-LMT(3) TO RHH-FND-LMT (HR 3)        9915845
732906         MOVE RMA-PREV-TIERED-LMT(4) TO RHH-FND-LMT (HR 4)        9915845
732907         MOVE RMA-PREV-TIERED-LMT(5) TO RHH-FND-LMT (HR 5)        9915845
732908         MOVE RMA-PREV-TIERED-LMT(6) TO RHH-FND-LMT (HR 6)        9915845
732909         MOVE RMA-PREV-TIERED-LMT(7) TO RHH-FND-LMT (HR 7)        9915845
732910         MOVE RMA-PREV-TIERED-LMT(8) TO RHH-FND-LMT (HR 8)        9915845
732911         ADD +1 TO HOLD-RTE-RET HR                                9915845
732912         MOVE RMA-CUR-TIERED-DT     TO RHH-FUND-DT (HR)           9915845
732913         MOVE RMA-CUR-TIERED-ANN(1) TO RHH-FND-ANN (HR 1)         9915845
732914         MOVE RMA-CUR-TIERED-ANN(2) TO RHH-FND-ANN (HR 2)         9915845
732915         MOVE RMA-CUR-TIERED-ANN(3) TO RHH-FND-ANN (HR 3)         9915845
732916         MOVE RMA-CUR-TIERED-ANN(4) TO RHH-FND-ANN (HR 4)         9915845
732917         MOVE RMA-CUR-TIERED-ANN(5) TO RHH-FND-ANN (HR 5)         9915845
732918         MOVE RMA-CUR-TIERED-ANN(6) TO RHH-FND-ANN (HR 6)         9915845
732919         MOVE RMA-CUR-TIERED-ANN(7) TO RHH-FND-ANN (HR 7)         9915845
732920         MOVE RMA-CUR-TIERED-ANN(8) TO RHH-FND-ANN (HR 8)         9915845
732921         MOVE RMA-CUR-TIERED-ANN(9) TO RHH-FND-ANN (HR 9)         9915845
732922         MOVE RMA-CUR-TIERED-LMT(1) TO RHH-FND-LMT (HR 1)         9915845
732923         MOVE RMA-CUR-TIERED-LMT(2) TO RHH-FND-LMT (HR 2)         9915845
732924         MOVE RMA-CUR-TIERED-LMT(3) TO RHH-FND-LMT (HR 3)         9915845
732925         MOVE RMA-CUR-TIERED-LMT(4) TO RHH-FND-LMT (HR 4)         9915845
732926         MOVE RMA-CUR-TIERED-LMT(5) TO RHH-FND-LMT (HR 5)         9915845
732927         MOVE RMA-CUR-TIERED-LMT(6) TO RHH-FND-LMT (HR 6)         9915845
732928         MOVE RMA-CUR-TIERED-LMT(7) TO RHH-FND-LMT (HR 7)         9915845
732939         MOVE RMA-CUR-TIERED-LMT(8) TO RHH-FND-LMT (HR 8)         9915845
732940     ELSE                                                         9915845
732941         ADD +1 TO HOLD-RTE-RET                                   9915845
732942         MOVE HOLD-RTE-RET TO HR                                  9915845
732943         MOVE RMA-CUR-TIERED-DT     TO RHH-FUND-DT (HR)           9915845
732944         MOVE RMA-CUR-TIERED-ANN(1) TO RHH-FND-ANN (HR 1)         9915845
732945         MOVE RMA-CUR-TIERED-ANN(2) TO RHH-FND-ANN (HR 2)         9915845
732946         MOVE RMA-CUR-TIERED-ANN(3) TO RHH-FND-ANN (HR 3)         9915845
732947         MOVE RMA-CUR-TIERED-ANN(4) TO RHH-FND-ANN (HR 4)         9915845
732948         MOVE RMA-CUR-TIERED-ANN(5) TO RHH-FND-ANN (HR 5)         9915845
732949         MOVE RMA-CUR-TIERED-ANN(6) TO RHH-FND-ANN (HR 6)         9915845
732950         MOVE RMA-CUR-TIERED-ANN(7) TO RHH-FND-ANN (HR 7)         9915845
732951         MOVE RMA-CUR-TIERED-ANN(8) TO RHH-FND-ANN (HR 8)         9915845
732952         MOVE RMA-CUR-TIERED-ANN(9) TO RHH-FND-ANN (HR 9)         9915845
732953         MOVE RMA-CUR-TIERED-LMT(1) TO RHH-FND-LMT (HR 1)         9915845
732954         MOVE RMA-CUR-TIERED-LMT(2) TO RHH-FND-LMT (HR 2)         9915845
732955         MOVE RMA-CUR-TIERED-LMT(3) TO RHH-FND-LMT (HR 3)         9915845
732956         MOVE RMA-CUR-TIERED-LMT(4) TO RHH-FND-LMT (HR 4)         9915845
732957         MOVE RMA-CUR-TIERED-LMT(5) TO RHH-FND-LMT (HR 5)         9915845
732958         MOVE RMA-CUR-TIERED-LMT(6) TO RHH-FND-LMT (HR 6)         9915845
732959         MOVE RMA-CUR-TIERED-LMT(7) TO RHH-FND-LMT (HR 7)         9915845
732960         MOVE RMA-CUR-TIERED-LMT(8) TO RHH-FND-LMT (HR 8).        9915845
732961     PERFORM LEAP-CHECK-RATE      THRU LEAP-CHECK-EXIT.           9915577
732962     IF  RHH-FUND-DT (X) GREATER THAN RHW-TRAN-DATE               9915577
732963     AND (INTEREST-ADJ-SUBCODE NOT EQUAL 'T' AND 'F'              0902213
732966     AND 'U' AND 'I')                                             0902213
732970         MOVE '3' TO EX13-REASON-OR-OPT                           IM008
732980         GO TO X0800-RHR-EXC-K                                    IM008
732990     ELSE                                                         IM008
733000         MOVE RHH-FUND-DATA (X) TO RNX-FUND-DATA                  IM008
733005         PERFORM CALC-RNX-DAF THRU CALC-RNX-DAF-EXIT              1003625
733010         GO TO X0260-RHR-PROCESS-HIST.                            IM008
733012                                                                  9915845
733013 X0250-RHR-TIER-MOVE.                                             9915845
733014     IF  RMA-OLD-TIERED-ANN(HX 1) NUMERIC                         9916139
733015         MOVE RMA-OLD-TIERED-DT(HX)    TO RHH-FUND-DT (HR)        9916139
733016         MOVE RMA-OLD-TIERED-ANN(HX 1) TO RHH-FND-ANN (HR 1)      9916139
733017         MOVE RMA-OLD-TIERED-ANN(HX 2) TO RHH-FND-ANN (HR 2)      9916139
733018         MOVE RMA-OLD-TIERED-ANN(HX 3) TO RHH-FND-ANN (HR 3)      9916139
733019         MOVE RMA-OLD-TIERED-ANN(HX 4) TO RHH-FND-ANN (HR 4)      9916139
733020         MOVE RMA-OLD-TIERED-ANN(HX 5) TO RHH-FND-ANN (HR 5)      9916139
733021         MOVE RMA-OLD-TIERED-ANN(HX 6) TO RHH-FND-ANN (HR 6)      9916139
733022         MOVE RMA-OLD-TIERED-ANN(HX 7) TO RHH-FND-ANN (HR 7)      9916139
733023         MOVE RMA-OLD-TIERED-ANN(HX 8) TO RHH-FND-ANN (HR 8)      9916139
733024         MOVE RMA-OLD-TIERED-ANN(HX 9) TO RHH-FND-ANN (HR 9)      9916139
733025         MOVE RMA-OLD-TIERED-LMT(HX 1) TO RHH-FND-LMT (HR 1)      9916139
733026         MOVE RMA-OLD-TIERED-LMT(HX 2) TO RHH-FND-LMT (HR 2)      9916139
733027         MOVE RMA-OLD-TIERED-LMT(HX 3) TO RHH-FND-LMT (HR 3)      9916139
733028         MOVE RMA-OLD-TIERED-LMT(HX 4) TO RHH-FND-LMT (HR 4)      9916139
733029         MOVE RMA-OLD-TIERED-LMT(HX 5) TO RHH-FND-LMT (HR 5)      9916139
733030         MOVE RMA-OLD-TIERED-LMT(HX 6) TO RHH-FND-LMT (HR 6)      9916139
733031         MOVE RMA-OLD-TIERED-LMT(HX 7) TO RHH-FND-LMT (HR 7)      9916139
733032         MOVE RMA-OLD-TIERED-LMT(HX 8) TO RHH-FND-LMT (HR 8).     9916139
733033     SUBTRACT +1                     FROM HX.                     9916139
733034                                                                  9916139
733035 X0250-RHR-SPLT-CHK.                                              9915845
733040     IF  RMA-SPLIT-LOANS                                          1003625
733050         NEXT SENTENCE                                            IM008
733060     ELSE                                                         IM008
733070         GO TO X0250-RHR-PRIME-CHK.                               IM008
733078     IF  RMA-RET-OLD GREATER 0                                    9915845
733079         MOVE RMA-RET-OLD TO HX                                   9916139
733080         PERFORM X0250-RHR-SPLT-MOVE                              9915845
733082             VARYING HR FROM 1 BY 1                               9915845
733084             UNTIL HR GREATER THAN RMA-RET-OLD.                   9915845
733090     IF  RMA-PREV-SPLIT-DT NOT EQUAL SPACES AND ZEROES            1003625
733100         ADD +1 TO HOLD-RTE-RET                                   1003625
733102         MOVE HOLD-RTE-RET TO HR                                  9915845
733103         MOVE RMA-PREV-SPLIT-DT     TO RHH-SPLT-DT (HR)           9915845
733104         MOVE RMA-PREV-SPLIT-ANN(1) TO RHH-SPL-ANN (HR 1)         9915845
733105         MOVE RMA-PREV-SPLIT-ANN(2) TO RHH-SPL-ANN (HR 2)         9915845
733106         MOVE RMA-PREV-SPLIT-ANN(3) TO RHH-SPL-ANN (HR 3)         9915845
733107         MOVE RMA-PREV-SPLIT-LMT(1) TO RHH-SPL-LMT (HR 1)         9915845
733108         MOVE RMA-PREV-SPLIT-LMT(2) TO RHH-SPL-LMT (HR 2)         9915845
733120         ADD +1 TO HOLD-RTE-RET HR                                9915845
733130         MOVE RMA-CUR-SPLIT-DT      TO RHH-SPLT-DT (HR)           9915845
733131         MOVE RMA-CUR-SPLIT-ANN(1)  TO RHH-SPL-ANN (HR 1)         9915845
733132         MOVE RMA-CUR-SPLIT-ANN(2)  TO RHH-SPL-ANN (HR 2)         9915845
733133         MOVE RMA-CUR-SPLIT-ANN(3)  TO RHH-SPL-ANN (HR 3)         9915845
733134         MOVE RMA-CUR-SPLIT-LMT(1)  TO RHH-SPL-LMT (HR 1)         9915845
733135         MOVE RMA-CUR-SPLIT-LMT(2)  TO RHH-SPL-LMT (HR 2)         9915845
733140     ELSE                                                         IM008
733150         ADD +1 TO HOLD-RTE-RET                                   1003625
733151         MOVE HOLD-RTE-RET TO HR                                  9915845
733153         MOVE RMA-CUR-SPLIT-DT      TO RHH-SPLT-DT (HR)           9915845
733154         MOVE RMA-CUR-SPLIT-ANN(1)  TO RHH-SPL-ANN (HR 1)         9915845
733155         MOVE RMA-CUR-SPLIT-ANN(2)  TO RHH-SPL-ANN (HR 2)         9915845
733156         MOVE RMA-CUR-SPLIT-ANN(3)  TO RHH-SPL-ANN (HR 3)         9915845
733157         MOVE RMA-CUR-SPLIT-LMT(1)  TO RHH-SPL-LMT (HR 1)         9915845
733158         MOVE RMA-CUR-SPLIT-LMT(2)  TO RHH-SPL-LMT (HR 2).        9916139
733160     PERFORM LEAP-CHECK-RATE      THRU LEAP-CHECK-EXIT.           9915577
733170     IF  RHH-SPLT-DT (X) GREATER THAN RHW-TRAN-DATE               IM008
733180         MOVE '3' TO EX13-REASON-OR-OPT                           IM008
733190         GO TO X0800-RHR-EXC-K                                    IM008
733200     ELSE                                                         IM008
733210         MOVE RHH-SPLT-DATA (X) TO RNX-SPLT-DATA                  IM008
733215         PERFORM CALC-RNX-DAF THRU CALC-RNX-DAF-EXIT              1003625
733220         GO TO X0260-RHR-PROCESS-HIST.                            IM008
733230                                                                  IM008
733231 X0250-RHR-SPLT-MOVE.                                             9915845
733232     IF  RMA-OLD-SPLIT-ANN(HX 1) NUMERIC                          9916139
733233         MOVE RMA-OLD-SPLIT-DT(HX)    TO RHH-SPLT-DT (HR)         9916139
733234         MOVE RMA-OLD-SPLIT-ANN(HX 1) TO RHH-SPL-ANN (HR 1)       9916139
733235         MOVE RMA-OLD-SPLIT-ANN(HX 2) TO RHH-SPL-ANN (HR 2)       9916139
733236         MOVE RMA-OLD-SPLIT-ANN(HX 3) TO RHH-SPL-ANN (HR 3)       9916139
733237         MOVE RMA-OLD-SPLIT-LMT(HX 1) TO RHH-SPL-LMT (HR 1)       9916139
733238         MOVE RMA-OLD-SPLIT-LMT(HX 2) TO RHH-SPL-LMT (HR 2).      9916139
733239     SUBTRACT +1                    FROM HX.                      9916139
733240                                                                  9916139
733241 X0250-RHR-PRIME-CHK.                                             9916139
733250     IF  RMA-PRIME                                                1003625
733260         NEXT SENTENCE                                            IM008
733270     ELSE                                                         IM008
733280         MOVE '3' TO EX13-REASON-OR-OPT                           IM008
733290         GO TO X0800-RHR-EXC-K.                                   IM008
733298     IF  RMA-RET-OLD GREATER 0                                    9915845
733299         MOVE RMA-RET-OLD TO HX                                   9916139
733300         PERFORM X0260-RHR-PRIME-MOVE                             9915845
733302             VARYING HR FROM 1 BY 1                               9915845
733304             UNTIL HR GREATER THAN RMA-RET-OLD.                   9915845
733310     IF  RMA-PREV-PRIME-DT NOT EQUAL SPACES AND ZEROES            1003625
733320         ADD +1 TO HOLD-RTE-RET                                   1003625
733321         MOVE HOLD-RTE-RET TO HR                                  9915845
733323         MOVE RMA-PREV-PRIME-DT       TO RHH-PRM-DT (HR)          9915845
733324         MOVE RMA-PREV-PRIME-ANN      TO RHH-PRM-ANN (HR)         9915845
733325         MOVE RMA-PREV-PRIME-ADJ-CODE TO RHH-ADJ-CODE (HR)        9915845
733326         MOVE RMA-PREV-PRIME-BAL-CALC TO RHH-BAL-CALC (HR)        9915845
733327         MOVE RMA-PREV-PRIME-ADJ(1)   TO RHH-PRM-ADJ (HR 1)       9915845
733328         MOVE RMA-PREV-PRIME-ADJ(2)   TO RHH-PRM-ADJ (HR 2)       9915845
733329         MOVE RMA-PREV-PRIME-ADJ(3)   TO RHH-PRM-ADJ (HR 3)       9915845
733330         MOVE RMA-PREV-PRIME-ADJ(4)   TO RHH-PRM-ADJ (HR 4)       9915845
733331         MOVE RMA-PREV-PRIME-ADJ(5)   TO RHH-PRM-ADJ (HR 5)       9915845
733332         MOVE RMA-PREV-PRIME-LMT(1)   TO RHH-PRM-LMT (HR 1)       9915845
733333         MOVE RMA-PREV-PRIME-LMT(2)   TO RHH-PRM-LMT (HR 2)       9915845
733334         MOVE RMA-PREV-PRIME-LMT(3)   TO RHH-PRM-LMT (HR 3)       9915845
733335         MOVE RMA-PREV-PRIME-LMT(4)   TO RHH-PRM-LMT (HR 4)       9915845
733340         ADD +1 TO HOLD-RTE-RET HR                                9915845
733343         MOVE RMA-CUR-PRIME-DT        TO RHH-PRM-DT (HR)          9915845
733344         MOVE RMA-CUR-PRIME-ANN       TO RHH-PRM-ANN (HR)         9915845
733345         MOVE RMA-CUR-PRIME-ADJ-CODE  TO RHH-ADJ-CODE (HR)        9915845
733346         MOVE RMA-CUR-PRIME-BAL-CALC  TO RHH-BAL-CALC (HR)        9915845
733347         MOVE RMA-CUR-PRIME-ADJ(1)    TO RHH-PRM-ADJ (HR 1)       9915845
733348         MOVE RMA-CUR-PRIME-ADJ(2)    TO RHH-PRM-ADJ (HR 2)       9915845
733349         MOVE RMA-CUR-PRIME-ADJ(3)    TO RHH-PRM-ADJ (HR 3)       9915845
733350         MOVE RMA-CUR-PRIME-ADJ(4)    TO RHH-PRM-ADJ (HR 4)       9915845
733351         MOVE RMA-CUR-PRIME-ADJ(5)    TO RHH-PRM-ADJ (HR 5)       9915845
733352         MOVE RMA-CUR-PRIME-LMT(1)    TO RHH-PRM-LMT (HR 1)       9915845
733353         MOVE RMA-CUR-PRIME-LMT(2)    TO RHH-PRM-LMT (HR 2)       9915845
733354         MOVE RMA-CUR-PRIME-LMT(3)    TO RHH-PRM-LMT (HR 3)       9915845
733355         MOVE RMA-CUR-PRIME-LMT(4)    TO RHH-PRM-LMT (HR 4)       9915845
733360     ELSE                                                         IM008
733370         ADD +1 TO HOLD-RTE-RET                                   1003625
733371         MOVE HOLD-RTE-RET TO HR                                  9915845
733373         MOVE RMA-CUR-PRIME-DT        TO RHH-PRM-DT (HR)          9915845
733374         MOVE RMA-CUR-PRIME-ANN       TO RHH-PRM-ANN (HR)         9915845
733375         MOVE RMA-CUR-PRIME-ADJ-CODE  TO RHH-ADJ-CODE (HR)        9915845
733376         MOVE RMA-CUR-PRIME-BAL-CALC  TO RHH-BAL-CALC (HR)        9915845
733377         MOVE RMA-CUR-PRIME-ADJ(1)    TO RHH-PRM-ADJ (HR 1)       9915845
733378         MOVE RMA-CUR-PRIME-ADJ(2)    TO RHH-PRM-ADJ (HR 2)       9915845
733379         MOVE RMA-CUR-PRIME-ADJ(3)    TO RHH-PRM-ADJ (HR 3)       9915845
733380         MOVE RMA-CUR-PRIME-ADJ(4)    TO RHH-PRM-ADJ (HR 4)       9915845
733381         MOVE RMA-CUR-PRIME-ADJ(5)    TO RHH-PRM-ADJ (HR 5)       9915845
733382         MOVE RMA-CUR-PRIME-LMT(1)    TO RHH-PRM-LMT (HR 1)       9915845
733383         MOVE RMA-CUR-PRIME-LMT(2)    TO RHH-PRM-LMT (HR 2)       9915845
733384         MOVE RMA-CUR-PRIME-LMT(3)    TO RHH-PRM-LMT (HR 3)       9915845
733385         MOVE RMA-CUR-PRIME-LMT(4)    TO RHH-PRM-LMT (HR 4).      9915845
733388     PERFORM LEAP-CHECK-RATE        THRU LEAP-CHECK-EXIT.         9915577
733390     IF  RHH-PRM-DT (X) GREATER THAN RHW-TRAN-DATE                IM008
733400         MOVE '3' TO EX13-REASON-OR-OPT                           IM008
733410         GO TO X0800-RHR-EXC-K                                    IM008
733420     ELSE                                                         IM008
733430         MOVE RHH-PRM-DATA (X) TO RNX-PRM-DATA                    IM008
733435         PERFORM CALC-RNX-DAF THRU CALC-RNX-DAF-EXIT              1003625
733440         GO TO X0260-RHR-PROCESS-HIST.                            IM008
733442                                                                  9915845
733443 X0260-RHR-PRIME-MOVE.                                            9915845
733444     IF  RMA-OLD-PRIME-ANN(HX) NUMERIC                            9916139
733445         MOVE RMA-OLD-PRIME-DT(HX)    TO RHH-PRM-DT (HR)          9916139
733446         MOVE RMA-OLD-PRIME-ANN(HX)   TO RHH-PRM-ANN (HR)         9916139
733447         MOVE RMA-OLD-PRIME-ADJ-CODE(HX) TO RHH-ADJ-CODE (HR)     9916139
733448         MOVE RMA-OLD-PRIME-BAL-CALC(HX) TO RHH-BAL-CALC (HR)     9916139
733449         MOVE RMA-OLD-PRIME-ADJ(HX 1) TO RHH-PRM-ADJ (HR 1)       9916139
733450         MOVE RMA-OLD-PRIME-ADJ(HX 2) TO RHH-PRM-ADJ (HR 2)       9916139
733451         MOVE RMA-OLD-PRIME-ADJ(HX 3) TO RHH-PRM-ADJ (HR 3)       9916139
733452         MOVE RMA-OLD-PRIME-ADJ(HX 4) TO RHH-PRM-ADJ (HR 4)       9916139
733453         MOVE RMA-OLD-PRIME-ADJ(HX 5) TO RHH-PRM-ADJ (HR 5)       9916139
733454         MOVE RMA-OLD-PRIME-LMT(HX 1) TO RHH-PRM-LMT (HR 1)       9916139
733455         MOVE RMA-OLD-PRIME-LMT(HX 2) TO RHH-PRM-LMT (HR 2)       9916139
733456         MOVE RMA-OLD-PRIME-LMT(HX 3) TO RHH-PRM-LMT (HR 3)       9916139
733457         MOVE RMA-OLD-PRIME-LMT(HX 4) TO RHH-PRM-LMT (HR 4).      9916139
733458     SUBTRACT +1                    FROM HX.                      9916139
733459                                                                  9916139
733460 X0260-RHR-PROCESS-HIST.                                          IM008
733470     IF  RHW-SW EQUAL 'N'                                         IM008
733480         GO TO X0270-RHR-CHECK-DATES.                             IM008
733490     MOVE 'N' TO RHW-SW.                                          IM008
733500     IF  RMA-IOD-SAV                                              1003625
733510     OR  RMA-LOANS                                                1003625
733520     OR  RMA-OD-ACCR                                              1003625
733530         IF  RNX-PTR-DT LESS THAN RHW-TRAN-DATE                   IM008
733540             MOVE RHW-TRAN-DATE TO RNX-PTR-DT.                    IM008
733550     IF  RMA-TIERED                                               1003625
733560         IF  RNX-FUND-DT LESS THAN RHW-TRAN-DATE                  IM008
733570             MOVE RHW-TRAN-DATE TO RNX-FUND-DT.                   IM008
733580     IF  RMA-SPLIT-LOANS                                          1003625
733590         IF  RNX-SPLT-DT LESS THAN RHW-TRAN-DATE                  IM008
733600             MOVE RHW-TRAN-DATE TO RNX-SPLT-DT.                   IM008
733610     IF  RMA-PRIME                                                1003625
733620         IF  RNX-PRM-DT LESS THAN RHW-TRAN-DATE                   IM008
733630             MOVE RHW-TRAN-DATE TO RNX-PRM-DT.                    IM008
733640*________________________________________________________________*0902213
733641*    INITIALIZE BALANCE HISTORY TO THE ACCOUNT OPENED DATE       *0902213
733642*    (RHW-TRAN-DATE) OR TO THE OLDEST RATE HISTORY DATE          *0902213
733643*    (RNX-PTR-DATE) IF THE OPENED DATE IS PRIOR TO THE OLDEST    *0902213
733644*    HISTORY RETAINED.                                           *0902213
733645*________________________________________________________________*0902213
733646     IF  RMA-IOD-SAV                                              1003625
733647     OR  RMA-OD-ACCR                                              1003625
733648         IF  INTEREST-ADJ-SUBCODE EQUAL 'I'                       0902213
733649         AND RHW-TRAN-DATE LESS THAN RNX-PTR-DT                   0902213
733650             MOVE RNX-PTR-DT TO RHW-TRAN-DATE.                    0902213
733651     IF  RMA-TIERED                                               1003625
733652         IF  INTEREST-ADJ-SUBCODE EQUAL 'I'                       0902213
733653         AND RHW-TRAN-DATE LESS THAN RNX-FUND-DT                  0902213
733654             MOVE RNX-FUND-DT TO RHW-TRAN-DATE.                   0902213
733655*________________________________________________________________*0902213
733656*    CALL THE ACCRUAL ROUTINE ONCE IF USING BALANCE HISTORY AND  *0902213
733657*    THIS IS A BACKDATED MONETARY OR BANK/CUST UNAVAIL TRAN.     *0902213
733658*________________________________________________________________*0902213
733659     IF  INTEREST-ADJUST-CODE EQUAL '2'                           0902213
733660     AND (INTEREST-ADJ-SUBCODE EQUAL 'T' OR 'F' OR 'U')           0902213
733661         MOVE 'E' TO RHW-SW                                       0902213
733662         GO TO X0280-RHR-CALC-DAYS.                               0902213
733663                                                                  0902213
733664 X0270-RHR-CHECK-DATES.                                           0902213
733665     ADD +1 TO X.                                                 0902213
733670     IF  X GREATER THAN HOLD-RTE-RET                              1003625
733680         MOVE 'E' TO RHW-SW                                       IM008
733690         GO TO X0280-RHR-CALC-DAYS.                               IM008
733700     IF  RMA-IOD-SAV                                              1003625
733710     OR  RMA-LOANS                                                1003625
733720     OR  RMA-OD-ACCR                                              1003625
733730         IF  RHH-PTR-DT (X)  NOT GREATER THAN RHW-TRAN-DATE       IM008
733740             MOVE RHH-PTR-DATA (X) TO RNX-PTR-DATA                IM008
733750             MOVE RHW-TRAN-DATE TO RNX-PTR-DT                     IM008
733755             PERFORM CALC-RNX-DAF THRU CALC-RNX-DAF-EXIT          1003625
733760             GO TO X0270-RHR-CHECK-DATES.                         IM008
733770     IF  RMA-TIERED                                               1003625
733780         IF  RHH-FUND-DT (X) NOT GREATER THAN RHW-TRAN-DATE       IM008
733790             MOVE RHH-FUND-DATA (X) TO RNX-FUND-DATA              IM008
733800             MOVE RHW-TRAN-DATE TO RNX-FUND-DT                    IM008
733805             PERFORM CALC-RNX-DAF THRU CALC-RNX-DAF-EXIT          1003625
733810             GO TO X0270-RHR-CHECK-DATES.                         IM008
733820     IF  RMA-SPLIT-LOANS                                          1003625
733830         IF  RHH-SPLT-DT (X) NOT GREATER THAN RHW-TRAN-DATE       IM008
733840             MOVE RHH-SPLT-DATA (X) TO RNX-SPLT-DATA              IM008
733850             MOVE RHW-TRAN-DATE TO RNX-SPLT-DT                    IM008
733855             PERFORM CALC-RNX-DAF THRU CALC-RNX-DAF-EXIT          1003625
733860             GO TO X0270-RHR-CHECK-DATES.                         IM008
733870     IF  RMA-PRIME                                                1003625
733880         IF  RHH-PRM-DT (X)  NOT GREATER THAN RHW-TRAN-DATE       IM008
733890             MOVE RHH-PRM-DATA (X) TO RNX-PRM-DATA                IM008
733900             MOVE RHW-TRAN-DATE TO RNX-PRM-DT                     IM008
733905             PERFORM CALC-RNX-DAF THRU CALC-RNX-DAF-EXIT          1003625
733910             GO TO X0270-RHR-CHECK-DATES.                         IM008
733920     IF  RMA-IOD-SAV                                              1003625
733930     OR  RMA-LOANS                                                1003625
733940     OR  RMA-OD-ACCR                                              1003625
733950         MOVE RHH-PTR-DT (X) TO C-HIGH-DATE                       IM008
733960         IF  RNX-PTR-DT LESS THAN RHW-TRAN-DATE                   IM008
733970             MOVE RHW-TRAN-DATE TO RNX-PTR-DT.                    IM008
733980     IF  RMA-TIERED                                               1003625
733990         MOVE RHH-FUND-DT (X) TO C-HIGH-DATE                      IM008
734000         IF  RNX-FUND-DT LESS THAN RHW-TRAN-DATE                  IM008
734010             MOVE RHW-TRAN-DATE TO RNX-FUND-DT.                   IM008
734020     IF  RMA-SPLIT-LOANS                                          1003625
734030         MOVE RHH-SPLT-DT (X) TO C-HIGH-DATE                      IM008
734040         IF  RNX-SPLT-DT LESS THAN RHW-TRAN-DATE                  IM008
734050             MOVE RHW-TRAN-DATE TO RNX-SPLT-DT.                   IM008
734060     IF  RMA-PRIME                                                1003625
734070         MOVE RHH-PRM-DT (X) TO C-HIGH-DATE                       IM008
734080         IF  RNX-PRM-DT LESS THAN RHW-TRAN-DATE                   IM008
734090             MOVE RHW-TRAN-DATE TO RNX-PRM-DT.                    IM008
734100                                                                  IM008
734110 X0280-RHR-CALC-DAYS.                                             IM008
734120     IF  RHW-SW EQUAL 'E'                                         IM008
734130         MOVE WBC-CAPTURE-MO TO C-HIGH-MO                         0266741
734140         MOVE WBC-CAPTURE-DA TO C-HIGH-DA                         0266741
734150         MOVE WBC-CAPTURE-YR TO C-HIGH-YR                         0266741
734160         IF  WBC-CAPTURE-YR GREATER THAN WBC-NEXT-CENT-YR         0266741
734170             MOVE '19' TO C-HIGH-CENT                             IM008
734180         ELSE                                                     IM008
734190             MOVE '20' TO C-HIGH-CENT.                            IM008
734200     IF  RHW-NON-HIST EQUAL 'N'                                   IM008
734210         GO TO X0285-RHR-CALC-DAYS.                               IM008
734220     IF  RHW-TYPE EQUAL 'I'                                       1105490
734230     OR  RHW-TYPE EQUAL 'L'                                       1105490
734240     OR  RHW-TYPE EQUAL 'O'                                       1105490
734250         MOVE RNX-PTR-DT TO C-LOW-DATE.                           IM008
734290     IF  RHW-TYPE EQUAL 'T'                                       1105490
734300         MOVE RNX-FUND-DT TO C-LOW-DATE.                          IM008
734340     IF  RHW-TYPE EQUAL 'S'                                       1105490
734350         MOVE RNX-SPLT-DT TO C-LOW-DATE.                          IM008
734390     IF  RHW-TYPE EQUAL 'P'                                       1105490
734400         MOVE RNX-PRM-DT TO C-LOW-DATE.                           IM008
734402                                                                  9915845
734404 X0285-RHR-CALC-DAYS.                                             9915845
734406                                                                  9915845
734408     CALL 'SIDIFC1' USING C-DATE-WORK.                            9915845
734410     ADD C-NO-DAYS TO RHW-NO-DAYS.                                9915845
734412     MOVE C-NO-DAYS TO ACCRUAL-DAYS.                              9915845
734413                                                                  0326947
734414     IF  HOLD-BKDT-DAYS NOT NUMERIC                               0326947
734415         MOVE +0           TO HOLD-BKDT-DAYS.                     0326947
734416     IF  IOD-FLAG EQUAL 'N'                                       9915845
734420     AND BHW-IOD-FLAG EQUAL 'I'                                   9915845
734422     AND HOLD-BKDT-DAYS GREATER THAN WBHI-ENTRIES                 9915845
734423     AND INTEREST-ADJ-SUBCODE NOT EQUAL 'I'                       0727455
734424         MOVE WBHI-ENTRIES TO ACCRUAL-DAYS.                       9915845
734426     IF  IOD-FLAG EQUAL 'N'                                       9915845
734428     AND BHW-IOD-FLAG EQUAL 'M'                                   9915845
734430     AND HOLD-BKDT-DAYS GREATER THAN WBHM-ENTRIES                 9915845
734431     AND INTEREST-ADJ-SUBCODE NOT EQUAL 'I'                       0727455
734432         MOVE WBHM-ENTRIES TO ACCRUAL-DAYS.                       9915845
734434     IF  IOD-FLAG EQUAL 'F'                                       9915845
734436     AND BHW-SAV-FLAG EQUAL 'R'                                   0316967
734438     AND HOLD-BKDT-DAYS GREATER THAN WBHS-ENTRIES                 9915845
734439     AND INTEREST-ADJ-SUBCODE NOT EQUAL 'I'                       0727455
734440         MOVE WBHS-ENTRIES TO ACCRUAL-DAYS.                       9915845
734442     IF  IOD-FLAG EQUAL 'F'                                       0316967
734444     AND BHW-SAV-FLAG EQUAL 'T'                                   0316967
734446     AND HOLD-BKDT-DAYS GREATER THAN WBHT-ENTRIES                 0316967
734447     AND INTEREST-ADJ-SUBCODE NOT EQUAL 'I'                       0727455
734448         MOVE WBHT-ENTRIES TO ACCRUAL-DAYS.                       0316967
734450                                                                  1004795
734458 X0285-RHR-CALC-DAYS-TISA.                                        1004795
734459*                                                                 1004795
734460*--------------------------------------------------------------*  1004795
734462*  CALCULATE DAYS TO ACCRUE FOR TISA.  WILL BE THE SAME AS FOR *  1004795
734464*  ACCRUALS UNLESS BACKDATE DATE IS PRIOR TO THE LAST STATEMENT*  1004795
734466*  DATE.                                                       *  1004795
734468*--------------------------------------------------------------*  1004795
734470*                                                                 1004795
734472     IF  INTEREST-ADJUST-CODE NOT EQUAL '2'                       1004795
734474     OR  RHW-LOAN EQUAL 'N'                                       1004795
734475     OR  TISA-RHW-TRAN-DATE GREATER THAN C-HIGH-DATE              1004795
734476         MOVE ZERO               TO TISA-ACCRUAL-DAYS             1004795
734478         GO TO X0285-CALC-TISA-EXIT.                              1004795
734480                                                                  1004795
734482     IF  C-LOW-DATE EQUAL TISA-RHW-TRAN-DATE                      1004795
734484         MOVE C-NO-DAYS          TO TISA-ACCRUAL-DAYS             1004795
734485         MOVE C-HIGH-DATE        TO TISA-RHW-TRAN-DATE            1004795
734486         GO TO X0285-CALC-TISA-EXIT.                              1004795
734488                                                                  1004795
734490     MOVE TISA-RHW-TRAN-DATE      TO C-LOW-DATE.                  1004795
734492                                                                  1004795
734494     CALL 'SIDIFC1'           USING C-DATE-WORK.                  1004795
734496                                                                  1004795
734498     MOVE C-NO-DAYS              TO TISA-ACCRUAL-DAYS.            1004795
734500     MOVE C-HIGH-DATE            TO TISA-RHW-TRAN-DATE.           1004795
734504 X0285-CALC-TISA-EXIT.                                            1004795
734506                                                                  1004795
734508     IF  INTEREST-ADJ-SUBCODE EQUAL 'I'                           1004795
734510     AND ACCRUAL-DAYS EQUAL ZERO                                  1004795
734512         MOVE 'E' TO RHW-SW                                       1004795
734514         GO TO X0880-RHR-EXIT.                                    1004795
734516     IF  RHW-OD EQUAL 'N'                                         1004795
734518         IF  RHW-OD-INT-PTR EQUAL +6                              1004795
734520             PERFORM X0910-RHR-CALC-PRIME THRU X0910-RHR-CALC-EXIT1004795
734525             PERFORM X0900-CALL-OD-PHASE THRU X0900-CALL-ACR-END  1004795
734530             ADD ACCRUAL-AMOUNT TO RHA-NEW-AMOUNT                 IM008
734540             GO TO X0290-RHR-MOVE-REC                             IM008
734550         ELSE                                                     IM008
734560             MOVE RNX-PTR-DAF TO ACCRUAL-DAILY-RATE               IM008
734580             PERFORM X0900-CALL-OD-PHASE THRU X0900-CALL-ACR-END  IM008
734590             ADD ACCRUAL-AMOUNT TO RHA-NEW-AMOUNT                 IM008
734600             GO TO X0290-RHR-MOVE-REC.                            IM008
734610     IF  RHW-LOAN EQUAL 'N'                                       IM008
734620         PERFORM X0500-RHR-LOAN-ACCRUAL THRU X0500-RHR-LOAN-EXIT  IM008
734630         ADD ACCR-AMT1 TO RHA-NEW-AMOUNT                          IM008
734640         GO TO X0290-RHR-MOVE-REC.                                IM008
734650     IF  INTEREST-ADJUST-CODE EQUAL '2'                           IM008
734660         IF  RHW-TYPE EQUAL 'T'                                   1003625
734670             PERFORM X0300-RHR-ACCRUAL THRU X0320-RHR-ACCRUAL-EXITIM008
734680             ADD ACCRUAL-AMOUNT TO RHA-NEW-AMOUNT                 IM008
734685             ADD TISA-ACCRUAL-AMOUNT TO TISA-RHA-NEW-AMOUNT       1004795
734690         ELSE                                                     IM008
734700             MOVE RNX-PTR-ANN TO ACCRUAL-ANNUAL-RATE              IM008
734706             MOVE RNX-PTR-DAF TO ACCRUAL-DAILY-RATE               IM008
734720             PERFORM X0300-RHR-ACCRUAL THRU X0320-RHR-ACCRUAL-EXITIM008
734725             ADD ACCRUAL-SVC-AMT TO RHA-NEW-SVC-AMT               1004554
734730             ADD ACCRUAL-AMOUNT TO RHA-NEW-AMOUNT                 1004795
734735             ADD TISA-ACCRUAL-AMOUNT TO TISA-RHA-NEW-AMOUNT.      1004795
734740     IF  RHA-EXCEPTION EQUAL 'KO'                                 IM008
734750         MOVE SPACES TO RHA-EXCEPTION                             IM008
734760         MOVE '2' TO EX13-REASON-OR-OPT                           IM008
734770         GO TO X0800-RHR-EXC-K.                                   IM008
734810 X0290-RHR-MOVE-REC.                                              IM008
734820     IF  RHW-SW EQUAL 'E'                                         IM008
734830         IF  RHW-OD EQUAL 'N'                                     IM008
734835         OR INTEREST-ADJ-SUBCODE EQUAL 'I'                        0902213
734840             GO TO X0880-RHR-EXIT                                 IM008
734850         ELSE                                                     IM008
734860             GO TO X0820-RHR-EOF.                                 IM008
734870     IF  RMA-IOD-SAV                                              1003625
734880     OR  RMA-LOANS                                                1003625
734890     OR  RMA-OD-ACCR                                              1003625
734900         MOVE RHH-PTR-DATA (X) TO RNX-PTR-DATA                    1003625
734905         PERFORM CALC-RNX-DAF THRU CALC-RNX-DAF-EXIT.             1003625
734910     IF  RMA-TIERED                                               1003625
734920         MOVE RHH-FUND-DATA (X) TO RNX-FUND-DATA                  1003625
734925         PERFORM CALC-RNX-DAF THRU CALC-RNX-DAF-EXIT.             1003625
734930     IF  RMA-SPLIT-LOANS                                          1003625
734940         MOVE RHH-SPLT-DATA (X) TO RNX-SPLT-DATA                  1003625
734945         PERFORM CALC-RNX-DAF THRU CALC-RNX-DAF-EXIT.             1003625
734950     IF  RMA-PRIME                                                1003625
734960         MOVE RHH-PRM-DATA (X) TO RNX-PRM-DATA                    1003625
734965         PERFORM CALC-RNX-DAF THRU CALC-RNX-DAF-EXIT.             1003625
734970     GO TO X0270-RHR-CHECK-DATES.                                 IM008
734980                                                                  IM008
738200 X0300-RHR-ACCRUAL.
738210     IF  IOD-FLAG EQUAL 'N'                                       1105595
738220         MOVE WMS-IOD-TIS-EARN-INT TO TISA-ACCRUED-TO-DATE        1105595
738230     ELSE                                                         1105595
738240         MOVE WMS-INT-TIS-EARN-INT TO TISA-ACCRUED-TO-DATE.       1105595
738250     MOVE +0 TO ACCRUAL-AMOUNT                                    1004795
738252                TISA-ACCRUAL-AMOUNT.                              1004795
738255     MOVE +0 TO ACCRUAL-SVC-AMT.                                  1004554
738260     IF  WMS-INVESTMENT-TRLR EQUAL '1'                            1004554
738265         MOVE WMS-INT-INV-SVC-RT    TO ACCRUAL-SVC-RT             1004554
738270         MOVE WMS-INV-SVC-ACCRD-CYC TO ACCRUED-SVC-ACCR.          1004554
738300     IF  WBC-ACCRUAL-PHASE EQUAL SPACES                           0266741
738350         GO TO X0320-RHR-ACCRUAL-EXIT.                            0902509
738400     IF  IOD-FLAG EQUAL 'F'                                       0817705
738402     AND WMS-SAV-TIER-PTR GREATER THAN +0                         0817705
738404         IF  WMS-DAILY-RATE-CODE EQUAL '3'                        0817705
738406             MOVE +365 TO DAF-DAYS                                0817705
738408         ELSE                                                     0817705
738410         IF  WMS-DAILY-RATE-CODE EQUAL '1'                        0817705
738412             MOVE +360 TO DAF-DAYS                                0817705
738414         ELSE                                                     0817705
738416         IF  WMS-LEAP-YEAR-FLG EQUAL '1'                          0817705
738418             MOVE +366 TO DAF-DAYS                                0817705
738420         ELSE                                                     0817705
738422             MOVE +365 TO DAF-DAYS.                               0817705
738424     IF  IOD-FLAG EQUAL 'F'                                       0817705
738426         IF  WMS-SAV-TIER-PTR GREATER THAN +0                     0817705
738428             MOVE +0 TO ACCRUAL-ANNUAL-RATE, ACCRUAL-DAILY-RATE   0817705
738430             MOVE '3' TO WS-RATE-KEY-LEVEL                        0817705
738432             PERFORM READ-RATE-SAV-TIER THRU READ-RATE-END        0817705
738434             IF  WS-RATE-NOT-FOUND = 'Y'                          0817705
738436                 PERFORM R0015-RATE-REJECT THRU R0015-EXIT        0817705
738438                 PERFORM WRITE-EX-MST THRU WRITE-EX-EXIT          0817705
738440                 MOVE ZERO TO ACCRUAL-AMOUNT                      0817705
738442                 GO TO X0320-RHR-ACCRUAL-EXIT                     0817705
738444             ELSE                                                 0817705
738446                 MOVE WS-RATE-KEY-LEVEL TO WMX-TIER-RATE-KEY-LEVEL0817705
738448                 MOVE RMA-CUR-TIERED-DT     TO WMX-TIER-CUR-DATE  0817705
738450                 MOVE RMA-CUR-TIERED-ANN(1) TO WMX-TIER-CUR-ANN(1)0817705
738452                 MOVE RMA-CUR-TIERED-ANN(2) TO WMX-TIER-CUR-ANN(2)0817705
738454                 MOVE RMA-CUR-TIERED-ANN(3) TO WMX-TIER-CUR-ANN(3)0817705
738456                 MOVE RMA-CUR-TIERED-ANN(4) TO WMX-TIER-CUR-ANN(4)0817705
738458                 MOVE RMA-CUR-TIERED-ANN(5) TO WMX-TIER-CUR-ANN(5)0817705
738460                 MOVE RMA-CUR-TIERED-ANN(6) TO WMX-TIER-CUR-ANN(6)0817705
738462                 MOVE RMA-CUR-TIERED-ANN(7) TO WMX-TIER-CUR-ANN(7)0817705
738464                 MOVE RMA-CUR-TIERED-ANN(8) TO WMX-TIER-CUR-ANN(8)0817705
738466                 MOVE RMA-CUR-TIERED-ANN(9) TO WMX-TIER-CUR-ANN(9)0817705
738468                 MOVE RMA-CUR-TIERED-LMT(1) TO WMX-TIER-CUR-LMT(1)0817705
738470                 MOVE RMA-CUR-TIERED-LMT(2) TO WMX-TIER-CUR-LMT(2)0817705
738472                 MOVE RMA-CUR-TIERED-LMT(3) TO WMX-TIER-CUR-LMT(3)0817705
738474                 MOVE RMA-CUR-TIERED-LMT(4) TO WMX-TIER-CUR-LMT(4)0817705
738476                 MOVE RMA-CUR-TIERED-LMT(5) TO WMX-TIER-CUR-LMT(5)0817705
738478                 MOVE RMA-CUR-TIERED-LMT(6) TO WMX-TIER-CUR-LMT(6)0817705
738480                 MOVE RMA-CUR-TIERED-LMT(7) TO WMX-TIER-CUR-LMT(7)0817705
738482                 MOVE RMA-CUR-TIERED-LMT(8) TO WMX-TIER-CUR-LMT(8)0817705
738484                 MOVE RMA-PREV-TIERED-DT    TO WMX-TIER-PREV-DATE 0817705
738486               MOVE RMA-PREV-TIERED-ANN(1) TO WMX-TIER-PREV-ANN(1)0817705
738488               MOVE RMA-PREV-TIERED-ANN(2) TO WMX-TIER-PREV-ANN(2)0817705
738490               MOVE RMA-PREV-TIERED-ANN(3) TO WMX-TIER-PREV-ANN(3)0817705
738492               MOVE RMA-PREV-TIERED-ANN(4) TO WMX-TIER-PREV-ANN(4)0817705
738494               MOVE RMA-PREV-TIERED-ANN(5) TO WMX-TIER-PREV-ANN(5)0817705
738496               MOVE RMA-PREV-TIERED-ANN(6) TO WMX-TIER-PREV-ANN(6)0817705
738498               MOVE RMA-PREV-TIERED-ANN(7) TO WMX-TIER-PREV-ANN(7)0817705
738500               MOVE RMA-PREV-TIERED-ANN(8) TO WMX-TIER-PREV-ANN(8)0817705
738502               MOVE RMA-PREV-TIERED-ANN(9) TO WMX-TIER-PREV-ANN(9)0817705
738504               MOVE RMA-PREV-TIERED-LMT(1) TO WMX-TIER-PREV-LMT(1)0817705
738506               MOVE RMA-PREV-TIERED-LMT(2) TO WMX-TIER-PREV-LMT(2)0817705
738508               MOVE RMA-PREV-TIERED-LMT(3) TO WMX-TIER-PREV-LMT(3)0817705
738510               MOVE RMA-PREV-TIERED-LMT(4) TO WMX-TIER-PREV-LMT(4)0817705
738512               MOVE RMA-PREV-TIERED-LMT(5) TO WMX-TIER-PREV-LMT(5)0817705
738514               MOVE RMA-PREV-TIERED-LMT(6) TO WMX-TIER-PREV-LMT(6)0817705
738516               MOVE RMA-PREV-TIERED-LMT(7) TO WMX-TIER-PREV-LMT(7)0817705
738518               MOVE RMA-PREV-TIERED-LMT(8) TO WMX-TIER-PREV-LMT(8)0817705
738520               PERFORM CALC-SAVT-CUR-DAF                          0817705
738522                  THRU CALC-SAVT-PREV-DAF-EXIT                    0817705
738524               PERFORM VERIFY-YR-BOUNDRY-FOR-SAVT                 0817705
738526                  THRU VERIFY-SAVT-EXIT.                          0817705
738528                                                                  0817705
741050 X0310-RHR-ACCRUAL-PHASE.
741100     MOVE WBC-ACCRUAL-PHASE TO IM31-PH2.                          0266741
741150     IF  SAVINGS-MINIMUM EQUAL '1'
741200         ADD WMS-MIN-AMT-FOR-SUPER TO WMS-INT-CUR-BAL
741250                                  WMS-CURR-BAL
741300                                  CUR-BAL.
741350     CALL 'SILINK'   USING IM31-PHASE
741400                           INTEREST-ACCRUAL-PARAMETERS
741450                           IOD-FLAG
741500                           DDA-WRKBCR-1                           0266741
741550                           DDA-WRKBCR-4                           0266741
741600                           MASTER-AREA
741610                           RATE-WORK-FLAGS                        1003625
741620                           WS-RWF-TIER-TABLE                      1003625
741630                           WS-RWF-DDA-TABLE                       1003625
741640                           WS-RWF-SAV-TABLE                       9715519
741645                           WS-RWF-SAVT-TABLE                      0316967
741650                           RATE-HIST-HOLD
741800                           RATE-HIST-ACCRUAL                      IM002
741810                           RATE-HIST-WORK                         0902213
741818                           WORK-BAL-HIST-IOD                      0902509
741826                           WORK-BAL-HIST-MMDA                     0902509
741834                           WORK-BAL-HIST-SAV                      0902509
741836                           WORK-BAL-HIST-SAV-TIER                 0316967
741842                           WORK-BAL-HIST-MMDA-AGGR                1004795
741845                           TISA-BACKDATE-ACCRUAL-PARMS            0316967
741847                           WMX-TIER-RATES.                        0316967
741850     IF  SAVINGS-MINIMUM EQUAL '1'
741900         SUBTRACT WMS-MIN-AMT-FOR-SUPER FROM WMS-INT-CUR-BAL
741950                                         WMS-CURR-BAL
742000                                         CUR-BAL.
742050 X0320-RHR-ACCRUAL-EXIT.
742100     EXIT.
742150 X0500-RHR-LOAN-ACCRUAL.
742200**** PUT LOAN ACCRUAL CODE FOR BACKDATED LOAN TRANSACTIONS HERE **
742250     IF  WMS-INT-RATE-PTR GREATER THAN +5
742300         IF  PRIME-CALC-FLAG EQUAL '2' OR '3'
742350         OR  WMS-PRIME-CHNG-RTE NOT EQUAL '1'
742400         IF  INTEREST-ADJUST-CODE EQUAL '2'
742450             MOVE '5' TO EX13-REASON-OR-OPT
742500             PERFORM X0800-RHR-EXC-K THRU X0860-RHR-EXC-WRITE
742550             GO  TO  X0500-RHR-LOAN-EXIT
742600         ELSE
742650             MOVE 'KS' TO IMEX-CODE-2
742700             MOVE 'KP' TO IMEX-CODE-1
742750             MOVE '2' TO EX21-REJ-RSN
742800             MOVE ZERO TO ACCRUAL-AMOUNT
742850             PERFORM C2309-EXC
742900             GO  TO  X0500-RHR-LOAN-EXIT.
742950     IF  INTEREST-ADJUST-CODE EQUAL '2', GO  TO  X0520-RHR-TRN.
743000     IF  INTEREST-ADJUST-CODE EQUAL '3', GO  TO  X0530-RHR-PAST.
743050     IF  INTEREST-ADJUST-CODE EQUAL '4', GO  TO  X0540-RHR-BKDT.
743100     IF  INTEREST-ADJUST-CODE EQUAL '5', GO  TO  X0550-RHR-CORR.
743150     IF  INTEREST-ADJUST-CODE EQUAL '6', GO  TO  X0550-RHR-CORR.
743200     GO  TO  X0500-RHR-LOAN-EXIT.
743250 X0520-RHR-TRN.
743300*****    BACKDATED TRANSACTIONS.                             *****
743350     IF  RHW-SW NOT EQUAL 'E'
743400         GO  TO  X0521.
743450     IF  RHW-TRAN-MO EQUAL WBC-CAPTURE-MO AND                     0266741
743500         RHW-TRAN-YR EQUAL WBC-CAPTURE-YR                         0266741
743550         MOVE RHW-TRAN-DATE TO C-LOW-DATE
743600     ELSE
743650         MOVE C-HIGH-DATE TO C-LOW-DATE
743700         MOVE '01'        TO C-LOW-DA.
743750     CALL 'SIDIFC1' USING C-DATE-WORK.
743800     MOVE C-NO-DAYS TO RHW-MTD-DAYS.
743806     IF   RHW-TRAN-YR   EQUAL  WBC-CAPTURE-YR                     0266741
743812         MOVE RHW-TRAN-DATE   TO  C-LOW-DATE                      IM006
743818     ELSE                                                         IM006
743824         MOVE  C-HIGH-DATE    TO  C-LOW-DATE                      IM006
743830         MOVE  '01'           TO  C-LOW-DA,  C-LOW-MO.            IM006
743836     CALL 'SIDIFC1' USING C-DATE-WORK.                            IM006
743842     MOVE C-NO-DAYS TO RHW-YTD-DAYS.                              IM006
743850     IF  WMS-DATE-LAST-STMT NOT EQUAL SPACES
743900         MOVE WMS-LAST-STMT-MO TO SV-DT8-MO
743950         MOVE WMS-LAST-STMT-DA TO SV-DT8-DA
744000         MOVE WMS-LAST-STMT-YR TO SV-DT8-YR
744050     ELSE
744100         MOVE WMS-OPENED-MO TO SV-DT8-MO
744150         MOVE WMS-OPENED-DA TO SV-DT8-DA
744200         MOVE WMS-OPENED-YR TO SV-DT8-YR.
744250     IF  SV-DT8-YR GREATER THAN WBC-NEXT-CENT-YR                  0266741
744300         MOVE '19' TO SV-DT8-CENT
744350     ELSE
744400         MOVE '20' TO SV-DT8-CENT.
744450     IF  RHW-TRAN-DATE GREATER THAN SAVE-DATE-8
744500         MOVE RHW-TRAN-DATE TO C-LOW-DATE
744550     ELSE
744600         MOVE SAVE-DATE-8 TO C-LOW-DATE.
744650     CALL 'SIDIFC1' USING C-DATE-WORK.
744700     MOVE C-NO-DAYS TO RHW-CY-DAYS.
744750 X0521.
744800     IF  RATE-TYPE EQUAL 'N' AND
744850         WMS-INT-RATE-PTR EQUAL ZERO
744900     OR  RATE-TYPE EQUAL 'O' AND
744950         WMS-PREV-INT-PTR EQUAL ZERO
745000         GO TO X0523-ACCR-SPLIT.                                  IM008
745050*****    BACKDATE LOAN TRANSACTIONS FOR BCR AND PRIME RATES  *****
745100     IF  WMS-INT-RATE-PTR EQUAL +6 AND
745150         WMS-PRIME-ADJ NOT EQUAL ZERO
745200         MOVE RNX-PRM-YR TO HOLD-DAY                              IM008
745220         MOVE RNX-PRM-ANN TO HOLD-ANN-RTE                         IM008
745300         PERFORM X0522-ACCR-PRIME THRU X0522-EXIT
745350         MULTIPLY ACCRUAL-BALANCE BY HOLD-RATE GIVING ACCR-AMT1
745400         GO  TO  X0521-A.
745450     MOVE RNX-PTR-DAF TO HOLD-RATE.                               IM008
745454     MOVE RNX-PTR-ANN TO HOLD-ANN-RTE.                            IM008
745464     IF  WMS-MAX-LOAN-INT GREATER THAN +0                         IM008
745471         MOVE RNX-PTR-YR TO HOLD-YEAR                             IM008
745478         PERFORM MAX-RATE-HIST THRU MAX-EXIT.                     IM008
745485     MULTIPLY ACCRUAL-BALANCE BY HOLD-RATE GIVING ACCR-AMT1.      IM008
745500 X0521-A.
745550     MULTIPLY ACCRUAL-DAYS   BY ACCR-AMT1.
745600     IF  TR-TRAN-TYPE EQUAL '5'                                   9915845
745650         PERFORM X0580-SUBTRACT-ACCR THRU X0580-EXIT
745700     ELSE
745750         PERFORM X0570-ADD-ACCR THRU X0570-EXIT.
745800     IF  RHW-SW EQUAL 'E'
745850         MULTIPLY ACCRUAL-BALANCE BY RHW-CY-DAYS
745900             GIVING HOLD-AMT11-CY
745950         MULTIPLY ACCRUAL-BALANCE BY RHW-MTD-DAYS
746000             GIVING HOLD-AMT11-MTD
746010         MULTIPLY ACCRUAL-BALANCE BY RHW-YTD-DAYS                 IM006
746020             GIVING HOLD-AMT11-YTD                                IM006
746050     IF  TR-TRAN-TYPE EQUAL '5'                                   9915845
746060         COMPUTE WMS-LOAN-YTD-AGGR ROUNDED = WMS-LOAN-YTD-AGGR    IM006
746070                                           - HOLD-AMT11-YTD       IM006
746100         SUBTRACT HOLD-AMT11-CY  FROM WMS-CURR-AGGR1
746150         SUBTRACT HOLD-AMT11-MTD FROM WMS-MTD-AGGR1
746200     ELSE
746210         COMPUTE WMS-LOAN-YTD-AGGR ROUNDED = WMS-LOAN-YTD-AGGR    IM006
746220                                           + HOLD-AMT11-YTD       IM006
746250         ADD HOLD-AMT11-CY  TO WMS-CURR-AGGR1
746300         ADD HOLD-AMT11-MTD TO WMS-MTD-AGGR1.
746302     MOVE '02' TO IMEX-REC-NO.                                    0417193
746304     MOVE 'K0K6' TO IMEX-EX-CODE.                                 0417193
746306     SUBTRACT HOLD-SUB-ACCR FROM HOLD-ADD-ACCR GIVING EX02-BAL.   0417193
746308     ADD +1 TO SEQ-CTR.                                           0417193
746310     MOVE SEQ-CTR TO EX02-SEQ-NO.                                 0417193
746312     PERFORM WRITE-EX-MST THRU WRITE-EX-EXIT.                     0417193
746314     MOVE ZERO TO HOLD-SUB-ACCR, HOLD-ADD-ACCR.                   0417193
746350     GO  TO X0500-RHR-LOAN-EXIT.
746400*****    BACKDATE LOAN TRANSACTION ACCRUAL FOR PRIME RATES   *****
746450 X0522-ACCR-PRIME.
746500     IF  WMS-PRIME-ADJ-CODE EQUAL '1'
746550         ADD WMS-PRIME-ADJ TO HOLD-ANN-RTE
746600     ELSE
746650     IF  WMS-PRIME-ADJ-CODE EQUAL '2'
746700         SUBTRACT WMS-PRIME-ADJ FROM HOLD-ANN-RTE
746750     ELSE
746800         MULTIPLY WMS-PRIME-ADJ BY HOLD-ANN-RTE.
746850     IF  HOLD-ANN-RTE GREATER THAN WS-RMA-MAX-LOAN-INT            1003625
746900         MOVE WS-RMA-MAX-LOAN-INT TO HOLD-ANN-RTE.                1003625
746910     IF  WMS-MAX-LOAN-INT GREATER THAN +0                         IM008
746920         IF  HOLD-ANN-RTE GREATER THAN WMS-MAX-LOAN-INT           IM008
746930             MOVE WMS-MAX-LOAN-INT TO HOLD-ANN-RTE.               IM008
746950 X0522-AP-A.
747000     IF  WMS-LOAN-360-DAY-FACTOR EQUAL '1'                        0817657
747050         MOVE +360 TO HOLD-NO3
747100     ELSE
747102     IF  WMS-LOAN-360-DAY-FACTOR EQUAL '2'                        0817657
747104         MOVE +365 TO HOLD-NO3                                    0627547
747106     ELSE                                                         0627547
747150     IF  HOLD-DAY EQUAL WBC-CAPTURE-YR                            0266741
747200         IF  WBC-LEAP-YEAR EQUAL '1'                              0266741
747250             MOVE +366 TO HOLD-NO3
747300         ELSE
747350             MOVE +365 TO HOLD-NO3
747400     ELSE
747450         MOVE ZERO TO HOLD-REMAINDER
747500         DIVIDE +4 INTO HOLD-DAY9 GIVING HOLD-DIVIDEND
747550                               REMAINDER HOLD-REMAINDER
747600         IF  HOLD-REMAINDER EQUAL ZERO
747650             MOVE +366 TO HOLD-NO3
747700         ELSE
747750             MOVE +365 TO HOLD-NO3.
747800     DIVIDE HOLD-NO3 INTO HOLD-ANN-RTE GIVING HOLD-RATE.
747850 X0522-EXIT. EXIT.
747900 X0523-ACCR-SPLIT.
747950*****    BACKDATE LOAN TRANSACTION ACCRUAL FOR SPLIT RATES   *****
748000     IF  RATE-TYPE EQUAL 'N'
748050         SUBTRACT LOAN-PREV-BAL-IN FROM LOAN-BAL-IN
748100             GIVING HOLD-AMT9
748150         IF  WMS-SPLIT-RATE-SCHD NOT EQUAL ZERO
748200             IF  RHW-NON-HIST EQUAL 'F'
748205                 MOVE RNX-SPL-ANN (1) TO SR-ANNL-RATE1            1003625
748210                 MOVE RNX-SPL-ANN (2) TO SR-ANNL-RATE2            1003625
748215                 MOVE RNX-SPL-ANN (3) TO SR-ANNL-RATE3            1003625
748220                 MOVE RNX-SPL-DAF (1) TO SR-DAILY-RATE1           1003625
748225                 MOVE RNX-SPL-DAF (2) TO SR-DAILY-RATE2           1003625
748230                 MOVE RNX-SPL-DAF (3) TO SR-DAILY-RATE3           1003625
748235                 MOVE RNX-SPL-LMT (1) TO SR-LIMIT1                1003625
748240                 MOVE RNX-SPL-LMT (2) TO SR-LIMIT2                1003625
748260                 MOVE RNX-SPL-YR TO HOLD-YEAR                     IM008
748280                 PERFORM SPLIT-MAX-HIST THRU SPLIT-MAX-EXIT       IM006
748300             ELSE
748350                 PERFORM SPLIT-RATE-MOVE                          1003625
748360                    THRU SPLIT-RATE-MOVE-EXIT                     1003625
748420                 PERFORM SPLIT-MAX-CALC THRU SPLIT-MAX-EXIT       IM006
748450         ELSE
748500             MOVE WMS-LOAN-SPLIT-RATE-SCHEDULE TO SPLIT-RATE-HOLD
748550     ELSE
748600         MOVE LOAN-PREV-BAL-IN TO HOLD-AMT9
748650         IF  WMS-PREV-SPLIT-RATE-PTR NOT EQUAL ZERO
748700             IF  RHW-NON-HIST EQUAL 'F'
748705                 MOVE RNX-SPL-ANN (1) TO SR-ANNL-RATE1            1003625
748710                 MOVE RNX-SPL-ANN (2) TO SR-ANNL-RATE2            1003625
748715                 MOVE RNX-SPL-ANN (3) TO SR-ANNL-RATE3            1003625
748720                 MOVE RNX-SPL-DAF (1) TO SR-DAILY-RATE1           1003625
748725                 MOVE RNX-SPL-DAF (2) TO SR-DAILY-RATE2           1003625
748730                 MOVE RNX-SPL-DAF (3) TO SR-DAILY-RATE3           1003625
748735                 MOVE RNX-SPL-LMT (1) TO SR-LIMIT1                1003625
748740                 MOVE RNX-SPL-LMT (2) TO SR-LIMIT2                1003625
748760                 MOVE RNX-SPL-YR TO HOLD-YEAR                     IM008
748780                 PERFORM SPLIT-MAX-HIST THRU SPLIT-MAX-EXIT       IM006
748800             ELSE
748850                 PERFORM PREV-SPLIT-RATE-MOVE                     1003625
748860                    THRU PREV-SPLIT-RATE-MOVE-EXIT                1003625
748920                 PERFORM SPLIT-MAX-CALC THRU SPLIT-MAX-EXIT       IM006
748950         ELSE
749000             MOVE WMS-LOAN-SPLIT-RATE-SCHEDULE TO SPLIT-RATE-HOLD.
749050     IF  RHW-NON-HIST NOT EQUAL 'N'
749100         GO TO X0528-BACKOUT.                                     IM008
749120     MOVE SPLIT-RATE-HOLD TO RNX-SPLIT.                           IM008
749200     IF  WMS-LOAN-360-DAY-FACTOR EQUAL '0' AND                    0817657
749250         WBC-CAPTURE-YR NOT EQUAL RHW-TRAN-YR                     0266741
749300         NEXT SENTENCE
749350     ELSE
749400         GO TO X0528-BACKOUT.                                     IM008
749450     MOVE ACCRUAL-DAYS TO ACCR-NO-HOLD.
749500     MOVE HOLD-AMT9    TO CALC-AMT1.
749550     MOVE +1 TO ACCRUAL-DAYS.
749600     MOVE 'N' TO RHW-SW.
749650 X0524-AS.
749700     MOVE RHW-TRAN-YR TO HOLD-DAY.
749750     MOVE SR-ANNL-RATE1 TO HOLD-ANN-RTE.
749800     PERFORM X0522-AP-A THRU X0522-EXIT.
749850     MOVE HOLD-RATE TO SR-DAILY-RATE1.
749900     DIVIDE HOLD-NO3 INTO SR-ANNL-RATE2 GIVING SR-DAILY-RATE2.
749950     DIVIDE HOLD-NO3 INTO SR-ANNL-RATE3 GIVING SR-DAILY-RATE3.
750000     IF  ACCR-NO-HOLD EQUAL +1, MOVE 'E' TO RHW-SW.
750050     PERFORM X0528-BACKOUT THRU X0500-RHR-LOAN-EXIT.
750100     SUBTRACT +1 FROM ACCR-NO-HOLD.
750150     IF  ACCR-NO-HOLD EQUAL ZERO
750200         GO  TO  X0500-RHR-LOAN-EXIT.
750250     MOVE CALC-AMT1 TO HOLD-AMT9.
750300     MOVE +1 TO C-NO-DAYS.
750350     MOVE RHW-TRAN-DATE TO C-LOW-DATE.
750400     CALL 'SIDCHI' USING C-DATE-WORK.
750450     MOVE C-HIGH-DATE TO RHW-TRAN-DATE.
750500     GO  TO  X0524-AS.
750550 X0528-BACKOUT.
750600*****    BACKOUT SPLIT RATE ACCRUALS & AGGREGATES            *****
750650     MOVE ZERO TO WMS-LOAN-ACCR-AMT.
750700     PERFORM SPLIT-RATE-CALC THRU SPLIT-CALC-END.
750750     MULTIPLY WMS-LOAN-ACCR-AMT BY ACCRUAL-DAYS GIVING ACCR-AMT1.
750752     MOVE ACCR-AMT1 TO ACCR-AMT2.                                 0817707
750800     PERFORM X0580-SUBTRACT-ACCR THRU X0580-EXIT.
750850*****    BACKOUT AGGREGATES FOR SPLITS                       *****
750900     IF  RHW-SW NOT EQUAL 'E'
750950         GO  TO  X0529-REACCRUE.
750960     MULTIPLY  HOLD-AMT9 BY RHW-YTD-DAYS GIVING HOLD-AMT11-YTD.   IM006
750970     COMPUTE WMS-LOAN-YTD-AGGR ROUNDED = WMS-LOAN-YTD-AGGR        IM006
750980                                       - HOLD-AMT11-YTD.          IM006
751000     IF  HOLD-AMT9 GREATER THAN SR-LIMIT1
751050         MULTIPLY SR-LIMIT1 BY RHW-CY-DAYS  GIVING HOLD-AMT11-CY
751100         MULTIPLY SR-LIMIT1 BY RHW-MTD-DAYS GIVING HOLD-AMT11-MTD
751150         SUBTRACT HOLD-AMT11-CY  FROM WMS-CURR-AGGR1
751200         SUBTRACT HOLD-AMT11-MTD FROM WMS-MTD-AGGR1
751250     ELSE
751300         MULTIPLY HOLD-AMT9 BY RHW-CY-DAYS  GIVING HOLD-AMT11-CY
751350         MULTIPLY HOLD-AMT9 BY RHW-MTD-DAYS GIVING HOLD-AMT11-MTD
751400         SUBTRACT HOLD-AMT11-CY  FROM WMS-CURR-AGGR1
751450         SUBTRACT HOLD-AMT11-MTD FROM WMS-MTD-AGGR1
751500         GO  TO  X0529-REACCRUE.
751550     IF  HOLD-AMT9 GREATER THAN SR-LIMIT2
751600         SUBTRACT SR-LIMIT1 FROM SR-LIMIT2 GIVING SR-LMT-WK
751650         MULTIPLY SR-LMT-WK BY RHW-CY-DAYS  GIVING HOLD-AMT11-CY
751700         MULTIPLY SR-LMT-WK BY RHW-MTD-DAYS GIVING HOLD-AMT11-MTD
751750         SUBTRACT HOLD-AMT11-CY  FROM WMS-CURR-AGGR2
751800         SUBTRACT HOLD-AMT11-MTD FROM WMS-MTD-AGGR2
751850     ELSE
751900         SUBTRACT SR-LIMIT1 FROM HOLD-AMT9 GIVING SR-LMT-WK
751950         MULTIPLY SR-LMT-WK BY RHW-CY-DAYS  GIVING HOLD-AMT11-CY
752000         MULTIPLY SR-LMT-WK BY RHW-MTD-DAYS GIVING HOLD-AMT11-MTD
752050         SUBTRACT HOLD-AMT11-CY  FROM WMS-CURR-AGGR2
752100         SUBTRACT HOLD-AMT11-MTD FROM WMS-MTD-AGGR2
752150         GO  TO  X0529-REACCRUE.
752200     SUBTRACT SR-LIMIT2 FROM HOLD-AMT9 GIVING SR-LMT-WK.
752250     MULTIPLY SR-LMT-WK BY RHW-CY-DAYS  GIVING HOLD-AMT11-CY.
752300     MULTIPLY SR-LMT-WK BY RHW-MTD-DAYS GIVING HOLD-AMT11-MTD.
752350     SUBTRACT HOLD-AMT11-CY  FROM WMS-CURR-AGGR3.
752400     SUBTRACT HOLD-AMT11-MTD FROM WMS-MTD-AGGR3.
752450 X0529-REACCRUE.
752500*****    REACCRUE FOR PREVIOUSLY BACKED OUT SPLIT RATES      *****
752550*****  SINCE THE BALANCE HAS CHANGED WITH A BACKDATED TRANS  *****
752600*****  AND HAS PLACED THE NEW BALANCE INTO A DIFFERENT RATE. *****
752650     IF  TR-TRAN-TYPE EQUAL '5'                                   9915845
752700         SUBTRACT ACCRUAL-BALANCE FROM HOLD-AMT9
752750     ELSE
752800         ADD ACCRUAL-BALANCE TO HOLD-AMT9.
752850     MOVE ZERO TO WMS-LOAN-ACCR-AMT.
752900     PERFORM SPLIT-RATE-CALC THRU SPLIT-CALC-END.
752950*****    ACCRUALS
753000     MULTIPLY WMS-LOAN-ACCR-AMT BY ACCRUAL-DAYS GIVING ACCR-AMT1.
753050     PERFORM X0570-ADD-ACCR THRU X0570-EXIT.
753052     MOVE '02' TO IMEX-REC-NO.                                    0417193
753054     MOVE 'K0K6' TO IMEX-EX-CODE.                                 0417193
753056     SUBTRACT HOLD-SUB-ACCR FROM HOLD-ADD-ACCR GIVING EX02-BAL.   0417193
753058     ADD +1 TO SEQ-CTR.                                           0417193
753060     MOVE SEQ-CTR TO EX02-SEQ-NO.                                 0417193
753062     PERFORM WRITE-EX-MST THRU WRITE-EX-EXIT.                     0417193
753064     MOVE ZERO TO HOLD-SUB-ACCR, HOLD-ADD-ACCR.                   0417193
753066     SUBTRACT ACCR-AMT2 FROM ACCR-AMT1.                           0817707
753100*****    AGGREGATES FOR SPLITS
753150     IF  RHW-SW NOT EQUAL 'E'
753200         GO  TO  X0500-RHR-LOAN-EXIT.
753250 X0529-RE-A.
753260     MULTIPLY  HOLD-AMT9 BY RHW-YTD-DAYS GIVING HOLD-AMT11-YTD.   IM006
753270     COMPUTE WMS-LOAN-YTD-AGGR ROUNDED = WMS-LOAN-YTD-AGGR        IM006
753280                                       + HOLD-AMT11-YTD.          IM006
753300     IF  HOLD-AMT9 GREATER THAN SR-LIMIT1
753350         MULTIPLY SR-LIMIT1 BY RHW-CY-DAYS  GIVING HOLD-AMT11-CY
753400         MULTIPLY SR-LIMIT1 BY RHW-MTD-DAYS GIVING HOLD-AMT11-MTD
753450         ADD HOLD-AMT11-CY  TO WMS-CURR-AGGR1
753500         ADD HOLD-AMT11-MTD TO WMS-MTD-AGGR1
753550     ELSE
753600         MULTIPLY HOLD-AMT9 BY RHW-CY-DAYS  GIVING HOLD-AMT11-CY
753650         MULTIPLY HOLD-AMT9 BY RHW-MTD-DAYS GIVING HOLD-AMT11-MTD
753700         ADD HOLD-AMT11-CY  TO WMS-CURR-AGGR1
753750         ADD HOLD-AMT11-MTD TO WMS-MTD-AGGR1
753800         GO  TO X0500-RHR-LOAN-EXIT.
753850     IF  HOLD-AMT9 GREATER THAN SR-LIMIT2
753900         SUBTRACT SR-LIMIT1 FROM SR-LIMIT2 GIVING SR-LMT-WK
753950         MULTIPLY SR-LMT-WK BY RHW-CY-DAYS  GIVING HOLD-AMT11-CY
754000         MULTIPLY SR-LMT-WK BY RHW-MTD-DAYS GIVING HOLD-AMT11-MTD
754050         ADD HOLD-AMT11-CY  TO WMS-CURR-AGGR2
754100         ADD HOLD-AMT11-MTD TO WMS-MTD-AGGR2
754150     ELSE
754200         SUBTRACT SR-LIMIT1 FROM HOLD-AMT9 GIVING SR-LMT-WK
754250         MULTIPLY SR-LMT-WK BY RHW-CY-DAYS  GIVING HOLD-AMT11-CY
754300         MULTIPLY SR-LMT-WK BY RHW-MTD-DAYS GIVING HOLD-AMT11-MTD
754350         ADD HOLD-AMT11-CY  TO WMS-CURR-AGGR2
754400         ADD HOLD-AMT11-MTD TO WMS-MTD-AGGR2
754450         GO  TO X0500-RHR-LOAN-EXIT.
754500     SUBTRACT SR-LIMIT2 FROM HOLD-AMT9 GIVING SR-LMT-WK.
754550     MULTIPLY SR-LMT-WK BY RHW-CY-DAYS  GIVING HOLD-AMT11-CY.
754600     MULTIPLY SR-LMT-WK BY RHW-MTD-DAYS GIVING HOLD-AMT11-MTD.
754650     ADD HOLD-AMT11-CY  TO WMS-CURR-AGGR3.
754700     ADD HOLD-AMT11-MTD TO WMS-MTD-AGGR3.
754750     GO  TO X0500-RHR-LOAN-EXIT.
754800 X0530-RHR-PAST.
754850*****    ACCRUE PAST ACCRUALS AT THE PREVIOUS RATE WHEN THE  *****
754900*****  EFFECTIVE DATE IS LESS THAN CURRENT WITH PAST ACCRUALS*****
754950*    MOVE 'P' TO FIND-RATE-FLAG.
754955     MOVE 'C' TO FIND-RATE-FLAG.                                  IMUP006
755000     PERFORM X0600-FIND-RATE THRU X0699-EXIT.
755050     MOVE WMS-LOAN-ACCR-AMT TO ACCR-AMT1.
755100     ADD +1 TO WMS-LOAN-AGGR-DAYS, WMS-LOAN-MTD-AGGR-DAYS,        IM006
755110                                   WMS-LOAN-YTD-AGGR-DAYS.        IM006
755150     IF  WMS-INT-RATE-PTR GREATER THAN +0
755200         ADD HOLD-AMT9 TO WMS-CURR-AGGR1, WMS-MTD-AGGR1
755210         COMPUTE WMS-LOAN-YTD-AGGR ROUNDED = WMS-LOAN-YTD-AGGR    IM006
755220                                           + HOLD-AMT9            IM006
755250     ELSE
755300         MOVE +1 TO RHW-CY-DAYS, RHW-MTD-DAYS, RHW-YTD-DAYS       IM006
755350         PERFORM X0529-RE-A THRU X0500-RHR-LOAN-EXIT.
755400     MOVE '0' TO LOAN-ACCR-RECALC.
755450     MOVE '1' TO WMS-LOAN-CALC-CODE.
755500     PERFORM C2384 THRU C2400.
755550     GO  TO X0500-RHR-LOAN-EXIT.
755600 X0540-RHR-BKDT.
755650*****    BACKOUT OLD ACCRUALS AND REACCRUE AT NEW RATE FOR   *****
755700*****    A BACKDATED RATE CHANGE.                            *****
755750     MULTIPLY WMS-LOAN-ACCR-AMT BY RHA-PRV-DAYS GIVING ACCR-AMT1.
755800     PERFORM X0580-SUBTRACT-ACCR THRU X0580-EXIT.
755850     MOVE 'C' TO FIND-RATE-FLAG.
755900     PERFORM X0600-FIND-RATE THRU X0699-EXIT.
755950     MULTIPLY WMS-LOAN-ACCR-AMT BY RHA-PRV-DAYS GIVING ACCR-AMT1.
756000     PERFORM X0570-ADD-ACCR THRU X0570-EXIT.
756002     MOVE '02' TO IMEX-REC-NO.                                    0417193
756004     MOVE 'K0K6' TO IMEX-EX-CODE.                                 0417193
756006     SUBTRACT HOLD-SUB-ACCR FROM HOLD-ADD-ACCR GIVING EX02-BAL.   0417193
756008     ADD +1 TO SEQ-CTR.                                           0417193
756010     MOVE SEQ-CTR TO EX02-SEQ-NO.                                 0417193
756012     PERFORM WRITE-EX-MST THRU WRITE-EX-EXIT.                     0417193
756014     MOVE ZERO TO HOLD-SUB-ACCR, HOLD-ADD-ACCR.                   0417193
756050     GO  TO X0500-RHR-LOAN-EXIT.
756100 X0550-RHR-CORR.
756150*****    BACKOUT OLD ACCRUALS AND REACCRUE AT NEW RATE FOR   *****
756200*****    A CORRECTED RATE.                                   *****
756250*****        ADJUST CODE = 5 - RATE IS EFFECTIVE PRIOR TO    *****
756300*****                          CURRENT RATE                  *****
756350*****        ADJUST CODE = 6 - RATE IS EFFECTIVE AFTER       *****
756400*****                          CURRENT RATE                  *****
756450     MULTIPLY WMS-LOAN-ACCR-AMT BY RHA-DEL-DAYS GIVING ACCR-AMT1.
756500     PERFORM X0580-SUBTRACT-ACCR THRU X0580-EXIT.
756550*    MOVE 'P' TO FIND-RATE-FLAG.
756555     MOVE 'C' TO FIND-RATE-FLAG.                                  IMUP006
756600     PERFORM X0600-FIND-RATE THRU X0699-EXIT.
756650     IF  INTEREST-ADJUST-CODE EQUAL '6'
756700         SUBTRACT RHA-PRV-DAYS FROM RHA-DEL-DAYS GIVING HOLD-DAYS
756750         MULTIPLY WMS-LOAN-ACCR-AMT BY HOLD-DAYS GIVING ACCR-AMT1
756800         PERFORM X0570-ADD-ACCR THRU X0570-EXIT
756850     ELSE
756900         SUBTRACT RHA-DEL-DAYS FROM RHA-PRV-DAYS GIVING HOLD-DAYS
756950         MULTIPLY WMS-LOAN-ACCR-AMT BY HOLD-DAYS GIVING ACCR-AMT1
757000         PERFORM X0580-SUBTRACT-ACCR THRU X0580-EXIT.
757050     MOVE 'C' TO FIND-RATE-FLAG.
757100     PERFORM X0600-FIND-RATE THRU X0699-EXIT.
757150     MULTIPLY WMS-LOAN-ACCR-AMT BY RHA-PRV-DAYS GIVING ACCR-AMT1.
757200     PERFORM X0570-ADD-ACCR THRU X0570-EXIT.
757202     MOVE '02' TO IMEX-REC-NO.                                    0417193
757204     MOVE 'K0K6' TO IMEX-EX-CODE.                                 0417193
757206     SUBTRACT HOLD-SUB-ACCR FROM HOLD-ADD-ACCR GIVING EX02-BAL.   0417193
757208     ADD +1 TO SEQ-CTR.                                           0417193
757210     MOVE SEQ-CTR TO EX02-SEQ-NO.                                 0417193
757212     PERFORM WRITE-EX-MST THRU WRITE-EX-EXIT.                     0417193
757214     MOVE ZERO TO HOLD-SUB-ACCR, HOLD-ADD-ACCR.                   0417193
757250     GO  TO  X0500-RHR-LOAN-EXIT.
757300 X0570-ADD-ACCR.
757350     ADD +.005, WMS-LOAN-ACCR-INT GIVING HOLD-ACCR2.
757400     ADD ACCR-AMT1 TO WMS-LOAN-ACCR-INT.
757450     ADD +.005, WMS-LOAN-ACCR-INT GIVING HOLD-ACCR1.
757500     SUBTRACT HOLD-ACCR2 FROM HOLD-ACCR1.
757550     ADD HOLD-ACCR1 TO WMS-EARNED-INT, LOAN-INT-ACC-ADJ-INT       0735951
757551                                       WS-GL-LOAN-INT-ACC-ADJ-INT 0735951
757552                                       WMS-LOAN-YTD-ACC-INT       1105132
757600                                       WMS-LOAN-ACCR-TODAY.       0902970
757610     ADD  HOLD-ACCR1     TO HOLD-ADD-ACCR.                        0417193
757650 X0570-EXIT. EXIT.
757700 X0580-SUBTRACT-ACCR.
757750     ADD +.005, WMS-LOAN-ACCR-INT GIVING HOLD-ACCR2.
757800     SUBTRACT ACCR-AMT1 FROM WMS-LOAN-ACCR-INT.
757850     ADD +.005, WMS-LOAN-ACCR-INT GIVING HOLD-ACCR1.
757900     SUBTRACT HOLD-ACCR1 FROM HOLD-ACCR2.
757950     SUBTRACT HOLD-ACCR2 FROM WMS-EARNED-INT, LOAN-INT-ACC-ADJ-INT0735951
757951                                        WS-GL-LOAN-INT-ACC-ADJ-INT0735951
757952                                              WMS-LOAN-YTD-ACC-INT1105132
758000                                              WMS-LOAN-ACCR-TODAY.0902970
758010     ADD  HOLD-ACCR2     TO HOLD-SUB-ACCR.                        0417193
758050 X0580-EXIT. EXIT.
758100 X0500-RHR-LOAN-EXIT.
758150     EXIT.
758200     SKIP3
758250 X0600-FIND-RATE.
758300     MOVE WMS-PRIN-BAL TO HOLD-AMT9.
758350     SUBTRACT WMS-PREV-BAL FROM HOLD-AMT9.
758400     MOVE ZERO TO WMS-LOAN-ACCR-AMT.
758450     IF HOLD-AMT9 NOT GREATER THAN ZERO, GO TO X0690.
758500     IF WMS-INT-RATE-PTR EQUAL +6
758550         IF  FIND-RATE-FLAG EQUAL 'P'
758600             MOVE WMS-PRM-PREV-ANN  TO HOLD-ANN-RTE               1003625
758620             MOVE WMS-PRM-PREV-DATE TO C-LOW-DATE                 1003625
758700             MOVE C-LOW-YR         TO HOLD-DAY
758750             PERFORM X0522-ACCR-PRIME THRU X0522-EXIT
758800             GO TO X0670
758850         ELSE                                                     IM008
758860             MOVE WMS-PRM-CUR-ANN TO WMS-PRIME-RATE               1003625
758900             PERFORM X0610-PRIME-RATE-CALC THRU X0620-PRIME-EXIT
758950             GO TO X0670.
759000     IF  WMS-INT-RATE-PTR EQUAL ZERO                              IM006
759030         GO TO X0605.                                             IM006
759060     IF  FIND-RATE-FLAG EQUAL 'P'                                 IM006
759070         MOVE WMS-LN-PREV-DATE TO WORK-AREA3                      1003625
759080         MOVE WMS-LN-PREV-DAF  TO HOLD-RATE                       1003625
759120     ELSE                                                         IM006
759130         MOVE WBC-CAPTURE-YR TO HOLD-YEAR                         0266741
759150         MOVE WMS-LN-CUR-DAF TO HOLD-RATE.                        1003625
759180     IF  WMS-MAX-LOAN-INT GREATER THAN +0                         IM006
759210         PERFORM MAX-RATE-HIST THRU MAX-EXIT.                     IM006
759240     GO TO X0670.                                                 IM006
759250 X0605.                                                           IM006
759270     IF  WMS-SPLIT-RATE-SCHD EQUAL ZERO                           IM006
759300         GO TO X0670.                                             IM006
759330     IF  FIND-RATE-FLAG EQUAL 'P'                                 IM006
759360         MOVE WMS-SPL-PREV-DAF (1) TO SR-DAILY-RATE1              1003625
759380         MOVE WMS-SPL-PREV-DAF (2) TO SR-DAILY-RATE2              1003625
759400         MOVE WMS-SPL-PREV-DAF (3) TO SR-DAILY-RATE3              1003625
759420         MOVE WMS-SPL-PREV-LMT (1) TO SR-LIMIT1                   1003625
759440         MOVE WMS-SPL-PREV-LMT (2) TO SR-LIMIT2                   1003625
759460         MOVE WMS-SPL-PREV-DATE TO WORK-AREA3                     1003625
759660         MOVE '1' TO SR-HOLD-FLAG                                 IM006
759690     ELSE                                                         IM006
759720         PERFORM SPLIT-RATE-MOVE THRU SPLIT-RATE-MOVE-EXIT        1003625
759760         MOVE WBC-CAPTURE-YR TO HOLD-YEAR                         0266741
759780         MOVE '1' TO SR-HOLD-FLAG.                                IM006
759810     IF  WMS-MAX-LOAN-INT GREATER THAN +0                         IM006
759840         PERFORM SPLIT-MAX-HIST THRU SPLIT-MAX-EXIT.              IM006
759870     GO TO X0630-SPLIT-RATE-CALC.                                 IM006
760250 X0610-PRIME-RATE-CALC.
760300     MOVE WMS-PRIME-ADJ TO HOLD-ADJ.
760350     MOVE WS-RMA-MAX-LOAN-INT TO HOLD-MAX.                        1003625
760360     IF  WMS-MAX-LOAN-INT GREATER THAN +0                         IM006
760370         MOVE WMS-MAX-LOAN-INT TO HOLD-MAX.                       IM006
760400     MOVE WMS-PRIME-ADJ-CODE TO HOLD-ADJ-CODE.
760450     IF HOLD-ADJ-CODE EQUAL '1'
760500         ADD HOLD-ADJ WMS-PRIME-RATE GIVING HOLD-ANN-RTE.
760550     IF HOLD-ADJ-CODE EQUAL '2'
760600         SUBTRACT HOLD-ADJ FROM WMS-PRIME-RATE
760650             GIVING HOLD-ANN-RTE.
760700     IF HOLD-ADJ-CODE EQUAL '3'
760750         MULTIPLY HOLD-ADJ BY WMS-PRIME-RATE GIVING HOLD-ANN-RTE.
760800     IF HOLD-ANN-RTE GREATER THAN HOLD-MAX
760850         MOVE HOLD-MAX TO HOLD-ANN-RTE.
760900     MOVE HOLD-ANN-RTE TO WMS-ANNUAL-RATE.
760950     MOVE WMS-DATE-LAST-RATE-CHANGE TO SAVE-DATE.
761000     PERFORM C2302 THRU C2304-EXIT.                               IM008
761050     MOVE WMS-DAILY-RATE TO HOLD-RATE.
761100 X0620-PRIME-EXIT.
761150     EXIT.
761200 X0630-SPLIT-RATE-CALC.
761250     IF HOLD-AMT9 GREATER THAN SR-LIMIT1
761300         MOVE SR-LIMIT1 TO ACCR-BAL
761350         MULTIPLY ACCR-BAL BY SR-DAILY-RATE1 GIVING ACCR-AMT1
761400         ADD ACCR-AMT1 TO WMS-LOAN-ACCR-AMT
761450     ELSE
761500         MOVE HOLD-AMT9 TO ACCR-BAL
761550         MULTIPLY ACCR-BAL BY SR-DAILY-RATE1 GIVING ACCR-AMT1
761600         GO TO X0640-SPLIT-CALC-2.
761650     IF HOLD-AMT9 GREATER THAN SR-LIMIT2
761700         SUBTRACT SR-LIMIT1 FROM SR-LIMIT2 GIVING ACCR-BAL
761750         MULTIPLY ACCR-BAL BY SR-DAILY-RATE2 GIVING ACCR-AMT1
761800         ADD ACCR-AMT1 TO WMS-LOAN-ACCR-AMT
761850     ELSE
761900         SUBTRACT SR-LIMIT1 FROM HOLD-AMT9 GIVING ACCR-BAL
761950         MULTIPLY ACCR-BAL BY SR-DAILY-RATE2 GIVING ACCR-AMT1
762000         GO TO X0640-SPLIT-CALC-2.
762050     SUBTRACT SR-LIMIT2 FROM HOLD-AMT9 GIVING ACCR-BAL.
762100     MULTIPLY ACCR-BAL BY SR-DAILY-RATE3 GIVING ACCR-AMT1.
762150 X0640-SPLIT-CALC-2.
762200     ADD ACCR-AMT1 TO WMS-LOAN-ACCR-AMT.
762250 X0650-SPLIT-CALC-END.  EXIT.
762300 X0660.
762350     GO TO X0690.
762400 X0670.
762450     MOVE HOLD-AMT9 TO ACCR-BAL.
762500     MULTIPLY ACCR-BAL BY HOLD-RATE GIVING ACCR-AMT1.
762550     ADD ACCR-AMT1 TO WMS-LOAN-ACCR-AMT.
762600 X0680.  EXIT.
762650 X0690.
762700     IF WMS-PREV-BAL NOT GREATER THAN ZERO, GO TO X0699-EXIT.
762750     IF  INTEREST-ADJUST-CODE EQUAL '3',  GO  TO  X0699-EXIT.
762800     MOVE WMS-PREV-BAL TO HOLD-AMT9.
762850     IF  WMS-PREV-INT-PTR NOT EQUAL TO ZERO
762900         MOVE WMS-LN-OLD-CUR-DAF TO HOLD-RATE                     1003625
762950         IF  WMS-MAX-LOAN-INT GREATER THAN +0                     IM006
762960             PERFORM MAX-RATE-CALC THRU MAX-EXIT                  IM006
762970             GO TO X0695                                          IM006
762980         ELSE                                                     IM006
762990             GO TO X0695.                                         IM006
762992     IF  WMS-PREV-INT-PTR EQUAL TO ZERO                           0725989
762994         MOVE WMS-PREV-DAILY TO HOLD-RATE.                        0725989
763000     IF  WMS-PREV-SPLIT-RATE-PTR EQUAL ZERO                       IM006
763010         GO TO X0695.                                             IM006
763020     PERFORM SPLIT-RATE-MOVE THRU SPLIT-RATE-MOVE-EXIT.           1003625
763040     MOVE '1' TO SR-HOLD-FLAG.                                    IM006
763050     IF  WMS-MAX-LOAN-INT GREATER THAN +0                         IM006
763060         PERFORM SPLIT-MAX-CALC THRU SPLIT-MAX-EXIT.              IM006
763070     PERFORM X0630-SPLIT-RATE-CALC THRU X0650-SPLIT-CALC-END.     IM006
763080     GO TO X0699-EXIT.                                            IM006
763300 X0695.
763325     PERFORM X0670 THRU X0680.                                    0527323
763330 X0699-EXIT.  EXIT.                                               0527323
763350*----------------------------------------------------------------*0527323
763351*    X0700-SCORE-ACCT CALLS THE SCORING PHASE AND RECALCULATES   *0527323
763352*    AN ACCOUNT'S OD LIMIT.  IF THE AMOUNT IS DIFFERENT FROM THE *0527323
763353*    EXISTING AMOUNT, THE NEW AMOUNT REPLACES THE OLD AMOUNT AND *0527323
763354*    AN EXCEPTION RECORD IS CREATED.  IF THE MATRIX DOES NOT     *0527323
763355*    EXIST FOR AN ACCOUNT, AN EXCEPTION IS CREATED AND THE       *0527323
763356*    ACCOUNT CONTINUES TO PROCESS WITH THE EXISTING OD LIMIT.    *0527323
763357*    SCORING WILL NOT OCCUR IF THE BCR SCORING OPTION IS OFF OR  *0527323
763358*    IF THE BCR SCORING PHASE NAME IS SPACES.                    *0527323
763359*----------------------------------------------------------------*0527323
763360 X0700-SCORE-ACCT.                                                0527323
763362     IF (WBC-IM31-OD-SCORING NOT EQUAL '1')                       0527323
763364     OR  WBC-IM31-OD-SCORING-PHASE EQUAL SPACES                   0527323
763366         GO TO X0700-SCORE-EXIT.                                  0527323
763410     MOVE WBC-IM31-OD-SCORING-PHASE TO IM31-PH2.                  0527323
763411     CALL 'SILINK'   USING IM31-PHASE                             0527323
763412                           MASTER-AREA                            9915749
763413                           DDA-WRKBCR-1                           0266741
763414                           NEW-OD-LIMIT                           9915749
763415                           ODSM-NOT-FOUND.                        9915749
763416     IF  ODSM-NOT-FOUND EQUAL 'Y' OR 'C'                          0547276
763417         MOVE 'D0'   TO IMEX-CODE-1                               0547276
763418         MOVE '064'  TO EX09-REJ-RESN                             0920015
763419         MOVE '31'   TO EX09-REJ-PROG                             0547276
763420         MOVE '09'   TO IMEX-REC-NO                               0547276
763421         MOVE '31'   TO EX09-SOURCE                               0547276
763422         MOVE ZERO   TO EX09-BATCH EX09-TRACE EX09-SEQ            0547276
763423         MOVE SPACES TO EX09-REMAINDER                            0547276
763424         MOVE '95'   TO EX09-TR-CODE                              0547276
763427         IF  ODSM-NOT-FOUND EQUAL 'C'                             0547276
763428             MOVE OD-SCORE-TYP-NF-LIT(SIWS-LANG-SUB)              0547276
763429                     TO EX09-GEN-DESC.                            0547276
763430                                                                  0547276
763431     IF  ODSM-NOT-FOUND EQUAL 'Y' OR 'C'                          0547276
763432         MOVE WMS-CONTROL-KEY TO IMEX-KEY                         0547276
763433         MOVE '0'    TO EX09-SRC-FLAG                             0547276
763434         MOVE ZERO   TO EX09-REF-NUM                              0547276
763435         PERFORM CALL-IM31RDWR THRU WRITE-EX-EXIT                 0547276
763436         GO TO X0700-SCORE-EXIT.                                  0547276
763437                                                                  0547276
763440     IF  NEW-OD-LIMIT EQUAL TO WMS-OD-LIMIT-AMT                   0266741
763441         GO TO X0700-SCORE-EXIT.                                  0266741
763442     MOVE '15'             TO IMEX-REC-NO.                        0266741
763443     MOVE 'B0'             TO IMEX-CODE-1.                        0266741
763444     MOVE NEW-OD-LIMIT     TO NB-EXC15-NEW-FM-DATA.               0266741
763445     MOVE WMS-OD-LIMIT-AMT TO NB-EXC15-MASTER-DATA.               0266741
763446     MOVE NB-EXC15-FM-TRAN TO IMEX-REC-15.                        0266741
763447     MOVE NEW-OD-LIMIT     TO WMS-OD-LIMIT-AMT.                   0266741
763448     PERFORM WRITE-EX-MST THRU WRITE-EX-EXIT.                     0266741
763449 X0700-SCORE-EXIT.                                                0266741
763450     EXIT.                                                        0266741
763451*----------------------------------------------------------------*0266741
763452*    CREATE EXCEPTION 13 RECORDS FOR STOP SUSPECTS.              *0266741
763453*----------------------------------------------------------------*0266741
763454 X0710-STOP-SUSPECTS.                                             0266741
763455     MOVE '0'  TO STOP-HIT-FLAG.                                  0266741
763458     MOVE 'P'  TO EX13-NB-PEND-DEC-IND.                           0266741
763459     MOVE 'SU ' TO REJRESN.                                       0717565
763460     MOVE 'AA' TO EXCODE-1.                                       0527333
763462     IF  TR-POST-PRIORITY EQUAL '59'                              0266741
763463         MOVE 'Y' TO EX13-CAPITAL-MKT-IND                         0266741
763464     ELSE                                                         0266741
763465         MOVE ' ' TO EX13-CAPITAL-MKT-IND.                        0266741
763466     MOVE '13' TO RECNO.                                          0266741
763467     MOVE TR-TRAN80-A      TO IMEX-REC-13A.                       0266741
763468     MOVE TR-AMOUNT        TO EX13-AMT.                           0266741
763469     MOVE TR-USER-CODE     TO EX13-USER-CODE.                     0266741
763470     MOVE TR-TRAN-TYPE           TO EX13-TYPE.                    0266741
763471     MOVE TR-TRAN80-B      TO IMEX-REC-13B.                       0266741
763472     MOVE DATE-LAST-ACTIVE TO EX13-DATE-LAST-ACTIVE.              0266741
763473     IF  TR-UNIV-DESC-FLAG EQUAL '1'                              0266741
763474     OR  TR-CURRENCY-FLAG EQUAL '1'                               0266741
763475         MOVE '1'          TO SRC-INFO-LENGTH-FLG                 0266741
763476         MOVE '22'         TO RECNO                               0266741
763477         SUBTRACT +273 FROM TF-LENGTH GIVING EX-22                0266741
763478         ADD EX-13         TO EX-22                               0266741
763479         IF  TR-CURRENCY-FLAG EQUAL '1'                           0266741
763480             MOVE TR-CURRENCY-INFO TO EX22-CURRENCY-INFO          0266741
763481         ELSE                                                     0266741
763482             MOVE SPACES           TO EX22-CURRENCY-INFO.         0266741
763483     IF  TR-UNIV-DESC-FLAG EQUAL '1'                              0266741
763484         MOVE TR-UNIV-DESC-TRAN TO EX22-RMDR.                     0266741
763485     IF  TR-CLASS EQUAL '80'                                      0266741
763486         IF  TR-POST-PRIORITY EQUAL '3A' OR '0A'                  0266741
763487             MOVE REJRESN  TO TR-REJ-RESN                         0266741
763488         ELSE                                                     0266741
763489             MOVE REJRESN  TO WK-REJ-REASON                       0266741
763490             PERFORM X3000-REJ-REASON THRU X3000-EXIT.            0266741
763491     MOVE EX-INFO          TO IMEX-INFO.                          0266741
763492     MOVE SPACES           TO EX-INFO.                            0266741
763493     IF  ACCT-NOT-FOUND EQUAL '1'                                 0266741
763494         MOVE ZERO         TO EX13-AMT-AVAIL                      0266741
763495     ELSE                                                         0527333
763496         MOVE CUR-BAL      TO EX13-AMT-AVAIL.                     0527333
763497         IF  SAVINGS-PRESENT EQUAL '1'                            0527333
763498             ADD WMS-INT-CUR-BAL TO EX13-AMT-AVAIL.               0527333
763499     PERFORM WRITE-EXCEPT THRU WRITE-EX-EXIT.                     0527333
763500 X0710-EXIT.                                                      0266741
763501     EXIT.                                                        0266741
763502*----------------------------------------------------------------*0266741
763503*    DETERMINE THE POSITION OF THE REFERENCE NUMBER              *0266741
763504*    FOR EXCEPTION REPORTING AND STORE.                          *0266741
763505*----------------------------------------------------------------*0266741
763506 X0750.                                                           0266741
763507     MOVE ZERO TO WS-REF-NO.                                      0266741
763508     IF  TR-CLASS EQUAL '80'                                      0266741
763509         IF  (TR-80-TYPE EQUAL '03' OR '10' OR '12' OR '14')      0266741
763510         AND TR-ACH-REF-NO NUMERIC                                0266741
763511             MOVE TR-ACH-REF-NO TO WS-REF-NO                      0266741
763512         ELSE                                                     0266741
763513         IF  (TR-80-TYPE EQUAL '09' OR '13')                      0266741
763514         AND TR-ATM-REF-NO NUMERIC                                0266741
763515             MOVE TR-ATM-REF-NO TO WS-REF-NO.                     0266741
763516     IF  TR-CLASS EQUAL '20' OR '60' OR '61' OR '95'              0266741
763517         MOVE ZERO TO WS-REF-NO.                                  0266741
763518     IF  TR-CLASS GREATER THAN '49' AND LESS THAN '58'            0266741
763519         MOVE ZERO TO WS-REF-NO.                                  0266741
763520 X0750-EXIT.                                                      0266741
763521     EXIT.                                                        0266741
763522 X0770-REP-FUNDING.                                               0266741
763523     IF  FUNDING-AMT EQUAL TO 0.00                                0266741
763524         GO TO X0770-EXIT.                                        0266741
763527     COMPUTE FUNDING-AMT-WK = (BATCH-RUN-BALANCE - FUNDING-AMT)   0266741
763528     IF  TR-POST-PRIORITY NOT EQUAL '3A' AND '0A'                 0266741
763529         MOVE TR-AMOUNT TO WS-FUND-TR                             0266741
763530     ELSE                                                         0266741
763531         MOVE TR-LP-DB-AMT TO WS-FUND-TR.                         0266741
763532     IF  FUNDING-AMT-WK GREATER THAN WS-FUND-TR                   0266741
763533         GO TO X0770-EXIT.                                        0266741
763534     IF  FUNDING-AMT-WK NOT LESS THAN +0.00                       0266741
763535         SUBTRACT FUNDING-AMT-WK FROM WS-FUND-TR.                 0266741
763536     ADD WS-FUND-TR TO WS-FUNDING-USED.                           0266741
763537     MOVE ' '  TO TR-PEND-DECISION.                               0266741
763538     MOVE ' '  TO EX13-NB-PEND-DEC-IND.                           0266741
763539     MOVE 'FT' TO IMEX-CODE-1.                                    0266741
763540     MOVE 'F4' TO IMEX-CODE-2.                                    0266741
763541     PERFORM K0330 THRU K0339.                                    0266741
763542     IF  WS-FUNDING-USED NOT LESS THAN FUNDING-AMT                0266741
763543         MOVE +0.00 TO FUNDING-AMT.                               0266741
763544 X0770-EXIT.                                                      0266741
763545     EXIT.                                                        0266741
763546     COPY IMPD31RJ.                                               0266741
763547 X0800-RHR-EXC-K.                                                 0266741
763548     IF  TR-CLASS EQUAL '58' OR '59'                              0266741
763550         GO TO X0880-RHR-EXIT.
763600     IF  TR-80-TYPE EQUAL '06'
763650         ADD +1 TO NP-REV-CTR
763700         MOVE 'KN' TO IMEX-CODE-1
763750     ELSE
763800         MOVE 'KO' TO IMEX-CODE-1.
763850     IF  EX13-REASON-OR-OPT NOT EQUAL '7'
763900         MOVE ZERO TO ACCRUAL-AMOUNT.
763950     GO TO X0860-RHR-EXC-WRITE.
764000 X0820-RHR-EOF.
764050     IF  RHW-OD EQUAL 'N'
764100         IF  TR-TRAN-TYPE EQUAL '1' OR '2'                        9915845
764150             MULTIPLY -1 BY RHA-NEW-AMOUNT.
764200     IF  INTEREST-ADJUST-CODE EQUAL '2'
764205         MOVE TISA-RHA-NEW-AMOUNT TO TISA-ACCRUAL-AMOUNT          1004795
764210         MOVE RHA-NEW-SVC-AMT TO ACCRUAL-SVC-AMT                  1004554
764250         MOVE RHA-NEW-AMOUNT TO ACCRUAL-AMOUNT.                   IM008
764300 X0840-RHR-EXC-H.
764350     IF  TR-CLASS EQUAL '58' OR '59'                              IM007
764400         GO TO X0880-RHR-EXIT.
764450     IF  IOD-FLAG EQUAL 'N'
764550         MOVE RHA-NEW-DDA-BAL TO RHA-OLD-DDA-BAL
764590         MOVE HOLD-BKDT-DATE TO RHW-DATE-LAST-ACT.                0316967
764591     IF  IOD-FLAG EQUAL 'F' AND WMS-SAVINGS-TRLR EQUAL '1'        0316967
764592         IF  WMS-SAV-TIER-PTR GREATER +0                          0316967
764593             ADD RHA-OLD-SAV-BAL ACCRUAL-BALANCE                  0316967
764594                                 GIVING RHA-NEW-SAV-BAL           0316967
764596             MOVE RHA-NEW-SAV-BAL TO RHA-OLD-SAV-BAL              0316967
764598             MOVE HOLD-BKDT-DATE TO RHW-DATE-LAST-ACT.            0316967
764602     IF  NEG-ACCR-FLAG EQUAL '1'                                  0902493
764604         MOVE '7' TO EX13-REASON-OR-OPT                           0902493
764606         GO TO X0800-RHR-EXC-K.                                   0902493
764610     IF (INTEREST-ADJUST-CODE NOT EQUAL '2')                      0902213
764614     OR (ACCRUAL-AMOUNT + HOLD-INTEREST NOT LESS THAN +0.00)      0902213
764618         GO TO X0850.                                             0902213
764622*--------------------------------------------------------------*  0902213
764626*  CHECK THE BCR NEGATIVE ACCRUAL OPTIONS FOR BACKDATED TRANS  *  0902213
764630*  WHICH WOULD CAUSE THE ACCRUED INTEREST TO BE NEGATIVE.      *  0902213
764634*--------------------------------------------------------------*  0902213
764638* IOD NEGATIVE ACCRUAL OPTION                                     0902213
764642     IF  IOD-FLAG EQUAL 'N'                                       0902213
764646         IF  WBC-IOD-NEG-ACCRUAL EQUAL '0'                        0266741
764650             MOVE '7' TO EX13-REASON-OR-OPT                       0902213
764654             GO TO X0800-RHR-EXC-K                                0902213
764658         ELSE                                                     0902213
764662             GO TO X0850.                                         0902213
764666* SAV NEGATIVE ACCRUAL OPTION                                     0902213
764670     IF  IOD-FLAG EQUAL 'F'                                       0902213
764674         IF  WBC-SAV-NEG-ACCRUAL EQUAL '0'                        0266741
764678             MOVE '7' TO EX13-REASON-OR-OPT                       0902213
764682             GO TO X0800-RHR-EXC-K.                               0902213
764686 X0850.                                                           0902213
764690     IF  TR-80-TYPE EQUAL '06'                                    0902213
764700         ADD +1 TO NP-REV-CTR
764750         MOVE 'HA' TO IMEX-CODE-1
764800     ELSE
764850         MOVE 'HB' TO IMEX-CODE-1.
764900     MOVE '0' TO EX13-REASON-OR-OPT.
764950 X0860-RHR-EXC-WRITE.
765000     IF  RHW-OD EQUAL 'N'
765050         MOVE 'KM' TO IMEX-CODE-2.
765100     MOVE '13' TO IMEX-REC-NO.                                    IM002
765110     MOVE TR-TRAN80-A TO IMEX-REC-13A.                            IM002
765112     MOVE TR-USER-CODE TO EX13-USER-CODE.                         IM006
765114     MOVE TR-AMOUNT   TO EX13-AMT.                                IM006
765116     MOVE TR-TRAN-TYPE           TO EX13-TYPE.                    9915845
765120     MOVE TR-TRAN80-B TO IMEX-REC-13B.                            IM002
765130     IF  TR-UNIV-DESC-FLAG EQUAL '1'                              IM002
765132     OR  TR-CURRENCY-FLAG EQUAL '1'                               9916381
765135         MOVE '1'  TO SRC-INFO-LENGTH-FLG                         IM006
765140         MOVE '22' TO IMEX-REC-NO                                 IM002
765150         SUBTRACT +273 FROM TF-LENGTH GIVING EX-22                2011293
765160         ADD EX-13 TO EX-22                                       IM002
765162         IF  TR-CURRENCY-FLAG EQUAL '1'                           9916381
765164             MOVE TR-CURRENCY-INFO TO EX22-CURRENCY-INFO          9916381
765166         ELSE                                                     9916381
765168             MOVE SPACES           TO EX22-CURRENCY-INFO.         9916381
765170     IF  TR-UNIV-DESC-TRAN EQUAL '1'                              9916381
765172         MOVE TR-UNIV-DESC-TRAN TO EX22-RMDR.                     9916381
765250     IF  RHW-LOAN EQUAL 'F'
765300         MOVE RHW-DATE-LAST-ACT TO EX13-DATE-LAST-ACTIVE
765350     ELSE
765400         MOVE WMS-DATE-LAST-LOAN-ACTIVITY
765450             TO EX13-DATE-LAST-ACTIVE.
765500     IF EX13-REASON-OR-OPT EQUAL '9'                              0902213
765508         SUBTRACT ACCRUAL-DAYS FROM HOLD-BKDT-DAYS GIVING         0902213
765516                                    EX13-MANUAL-ADJ-DAYS          0902213
765524     ELSE                                                         0902213
765532         MOVE ACCRUAL-AMOUNT TO EX13-ACCRUAL-AMOUNT.              0902213
765540     MOVE RHW-TYPE TO EX13-RATE-TYPE.                             1003625
765550     MOVE RHW-IND  TO EX13-RATE-PTR.                              1003625
765552     IF  RHW-TYPE EQUAL 'L'                                       0367229
765554       IF  (TR-80-TYPE EQUAL '05' OR '06')                        0367229
765556         AND WMS-LOAN-TRLR EQUAL 1                                0367229
765557           CONTINUE                                               0367229
765558       ELSE                                                       0367229
765559             MOVE SPACE TO EX13-RATE-TYPE                         0367229
765560             MOVE ZERO  TO EX13-RATE-PTR.                         0367229
765650     PERFORM WRITE-EXCEPT THRU WRITE-EX-EXIT.
765700 X0880-RHR-EXIT.
765750     EXIT.
765800     SKIP3
765850 X0900-TRAN-ADJUST.
765900     MOVE '2' TO INTEREST-ADJUST-CODE.
765950     MOVE '3' TO OD-ACR-PAY-FLAG.
766000     MOVE 'N' TO RHW-OD.
766050     MOVE ZERO TO RHW-IND.                                        IM008
766100     MOVE RHA-OLD-OD-BAL TO OD-BAL.                               IM008
766150     MOVE WMS-OD-CALC-FLAG TO CALC-FLAG.
766200     PERFORM R3910 THRU R3910-EXIT.
766350     MULTIPLY OD-BAL BY -1 GIVING HOLD-OD-BAL.
766400     MOVE OD-BAL TO RHA-NEW-OD-BAL.                               IM008
766401     IF  INTEREST-ADJ-SUBCODE EQUAL 'I'                           0902213
766402         MOVE ZERO TO HOLD-TR-AMOUNT                              0902213
766403                      ACCRUAL-BALANCE                             0902213
766404         GO TO X0900-TRAN-005                                     0902213
766405     ELSE                                                         0902213
766406         IF  INTEREST-ADJ-SUBCODE EQUAL 'F' OR 'U'                0902213
766407             MULTIPLY -1 BY ACCRUAL-BALANCE                       0902213
766408             GO TO X0900-TRAN-005.                                0902213
766409     MOVE ZERO TO ACCRUAL-BALANCE.                                0902213
766410     IF  TR-TRAN-TYPE EQUAL '3' OR '4'                            9915845
766450         ADD TR-AMOUNT TO HOLD-OD-BAL
766455                          ACCRUAL-BALANCE                         0902213
766460         SUBTRACT TR-AMOUNT FROM RHA-NEW-OD-BAL                   IM008
766470                                 RHA-OLD-OD-BAL                   IM008
766500         MOVE +.005 TO ACCRUAL-ROUND
766550     ELSE
766555         SUBTRACT TR-AMOUNT FROM ACCRUAL-BALANCE                  0902213
766560         ADD TR-AMOUNT TO RHA-NEW-OD-BAL                          IM008
766570                          RHA-OLD-OD-BAL                          IM008
766600         MOVE -.005 TO ACCRUAL-ROUND.
766650     IF  (OD-BAL         LESS THAN ZERO)                          IM008
766652     AND (RHA-NEW-OD-BAL NOT LESS THAN ZERO)                      IM008
766656         MOVE HOLD-OD-BAL TO HOLD-TR-AMOUNT.                      IM008
766659     IF  (OD-BAL         NOT LESS THAN ZERO)                      IM008
766661     AND (RHA-NEW-OD-BAL LESS THAN ZERO)                          IM008
766665         MULTIPLY -1 BY RHA-NEW-OD-BAL GIVING HOLD-TR-AMOUNT.     IM008
766668     IF  (OD-BAL         LESS THAN ZERO)                          IM008
766670     AND (RHA-NEW-OD-BAL LESS THAN ZERO)                          IM008
766674         MOVE TR-AMOUNT TO HOLD-TR-AMOUNT.                        IM008
766677     IF  (OD-BAL         NOT LESS THAN ZERO)                      IM008
766679     AND (RHA-NEW-OD-BAL NOT LESS THAN ZERO)                      IM008
766683         MOVE ZERO TO HOLD-TR-AMOUNT.                             IM008
766692     IF  HOLD-TR-AMOUNT NOT GREATER THAN ZERO                     IM008
766696     AND BHW-ODAC-FLAG EQUAL 'F'                                  0902213
766700         GO TO X0900-RHR-OD-FINISH.
766750     MOVE TR-AMOUNT TO SAVE-AMT.                                  IM008
766760     MOVE HOLD-TR-AMOUNT TO TR-AMOUNT.                            IM008
766770     PERFORM ADJ-OD-AGGR THRU ADJ-OD-AGGR-EXIT.                   IM008
766780     MOVE SAVE-AMT TO TR-AMOUNT.                                  IM008
766790 X0900-TRAN-005.                                                  0902213
766800     PERFORM X0900-RHR-OD-ACCRUAL.
766850     MOVE +0 TO RHW-NO-DAYS
766900                RHA-OLD-AMOUNT
766950                RHA-NEW-AMOUNT
766960                RHA-NEW-SVC-AMT                                   1004554
766970                ACCRUAL-SVC-AMT                                   1004554
767000                ACCRUAL-AMOUNT.
767050     MOVE HOLD-BKDT-MO TO RHW-TRAN-MO.
767100     MOVE HOLD-BKDT-DA TO RHW-TRAN-DA.
767150     MOVE HOLD-BKDT-YR TO RHW-TRAN-YR.
767200     IF  HOLD-BKDT-YR GREATER THAN WBC-NEXT-CENT-YR               0266741
767250         MOVE '19' TO RHW-TRAN-CENT
767300     ELSE
767350         MOVE '20' TO RHW-TRAN-CENT.
767400     MOVE RHW-DATE-LAST-ACT TO DT-HIGH-DATE.
767450     MOVE HOLD-BKDT-DATE TO DT-LOW-DATE.
767500     IF  WMS-DATE-LAST-MONETARY-ACT EQUAL HOLD-BKDT-DATE
767520     AND BHW-ODAC-FLAG EQUAL 'F'                                  0902213
767550         IF  HOLD-BKDT-DATE EQUAL WMS-DATE-LAST-SERV-CHG
767600         OR  HOLD-BKDT-DATE EQUAL WMS-DATE-LAST-STMT
767650             MOVE '1' TO EX13-REASON-OR-OPT
767700             PERFORM X0800-RHR-EXC-K THRU X0880-RHR-EXIT
767750             GO TO X0900-RHR-OD-FINISH.
767800     CALL 'SIDIF1' USING DATE-AREA.
767850     IF  RET-DAYS GREATER THAN ZERO
767870     AND BHW-ODAC-FLAG EQUAL 'F'                                  0902213
767900         MOVE '1' TO EX13-REASON-OR-OPT
767950         PERFORM X0800-RHR-EXC-K THRU X0880-RHR-EXIT
768000         GO TO X0900-RHR-OD-FINISH.
768040     MOVE +09 TO U.                                               9915845
768050     MOVE +10 TO V.                                               9915845
768052     IF BHW-ODAC-FLAG EQUAL 'F'                                   0902213
768054     OR INTEREST-ADJ-SUBCODE EQUAL 'I'                            0902213
768056        GO TO X0900-TRAN-010.                                     0902213
768058*--------------------------------------------------------------*  0902213
768060*  WHEN BALANCE HISTORY IS BEING USED:                         *  0902213
768062*  HISTORY MUST EXIST FOR THE REQUIRED NUMBER OF BACKDATE DAYS,*  0902213
768064*  OR A PARTIAL ADJUSTMENT WILL BE MADE.  CREATE AN EXCEPTION  *  0902213
768066*  TO INDICATE THE NUMBER OF DAYS REQUIRING MANUAL ADJUSTMENTS.*  0902213
768068*  CALL THE ACCRUAL ROUTINE 1 TIME FOR DOLLAR AND BANK/CUST    *  9915845
768069*  UNAVAIL.                                                    *  9915845
768070*--------------------------------------------------------------*  0902213
768072     MOVE HOLD-BKDT-DAYS TO ACCRUAL-DAYS.                         0902213
768074     IF  HOLD-BKDT-DAYS GREATER THAN WBHO-ENTRIES                 0902213
768076         MOVE WBHO-ENTRIES TO ACCRUAL-DAYS.                       0902213
768078     IF  ACCRUAL-DAYS EQUAL ZERO                                  0902213
768080         MOVE '1' TO EX13-REASON-OR-OPT                           0902213
768082         PERFORM X0800-RHR-EXC-K THRU X0880-RHR-EXIT              0902213
768084         GO TO X0900-RHR-OD-FINISH                                0902213
768086     ELSE                                                         0902213
768088         IF  HOLD-BKDT-DAYS GREATER THAN ACCRUAL-DAYS             0902213
768090             MOVE '9' TO EX13-REASON-OR-OPT                       0902213
768092             PERFORM X0800-RHR-EXC-K THRU X0880-RHR-EXIT          0902213
768094             MOVE WBHO-ODAC-DATE (WBHO-ENTRIES) TO RHW-TRAN-DATE  0902213
768096                                                   HOLD-BH-DATE.  0902213
768098     GO TO X0900-CALL-OD-PHASE.                                   0902213
768100 X0900-TRAN-010.
768150     MOVE 'F' TO RHW-SW.
768200     SUBTRACT +1 FROM V                                           9915845
768205                      U.                                          9915845
768210     MOVE V   TO OD-SPLIT-IND.                                    0902213
768220     IF  V EQUAL +0                                               0902213
768230     AND INTEREST-ADJ-SUBCODE EQUAL 'I'                           0902213
768240         GO TO X0900-RHR-OD-FINISH.                               0902213
768250     IF  V EQUAL +0
768300         PERFORM X0820-RHR-EOF       THRU X0880-RHR-EXIT
768350         MOVE '0'                    TO OD-ACR-PAY-FLAG
768400         GO                          TO X0900-ADJUST-ACCRUE.
768500     MOVE WMS-OD-INT-PTR (V)         TO RHW-OD-INT-PTR.           9915845
768550     MOVE WMS-OD-PRIME-ADJ-FLAG (V)  TO HOLD-OD-ADJ-FLAG.         9915845
768600     MOVE WMS-OD-PRIME-ADJ (V)       TO HOLD-OD-PRIME-ADJ.        9915845
768650     IF  V EQUAL +1                                               9915845
768700         MOVE +0.00 TO HOLD-OD-LIMIT                              9915845
768800     ELSE                                                         9915845
768900         IF  WMS-OD-LIMIT-1-TO-8 (U) EQUAL SPACE                  9915845
769000             GO TO X0900-TRAN-010                                 9915845
769100         ELSE                                                     9915845
769200         IF  WMS-OD-LIMIT-1-TO-8N (U) EQUAL '0' OR 'S' OR 'L'     9915845
769300             MOVE WMS-OD-LIMIT-AMT-1-TO-8 (U) TO HOLD-OD-LIMIT    9915845
769400         ELSE                                                     9915845
769500             MOVE WMS-OD-LIMIT-1-TO-8 (U)     TO T                9915845
769600             MOVE WBC-OD-LIMIT (T)            TO HOLD-OD-LIMIT.   0266741
769760     IF  RHW-OD-INT-PTR EQUAL +0                                  IM008
769770         GO TO X0900-TRAN-010.                                    IM008
769800     PERFORM X0900-TRAN-100 THRU X0900-100-EXIT.
769850     CALL 'SIDIF1' USING DATE-AREA.
769900     IF  RET-DAYS EQUAL ZERO
769950         MOVE RHW-TRAN-DATE TO RNX-PTR-DT                         IM008
770050         MOVE 'E' TO RHW-SW
770100         IF  RHW-OD-INT-PTR EQUAL +6
770150             MOVE WMS-OD-PRM-CUR-ANN TO RNX-PRM-ANN               1003625
770200         ELSE
770250             MOVE WMS-OD-CUR-DAF (RHW-OD-INT-PTR) TO RNX-PTR-DAF. 1003625
770600     IF  ACCRUAL-BALANCE NOT GREATER +0.00
770620     AND INTEREST-ADJ-SUBCODE NOT EQUAL 'I'                       0902213
770650         GO TO X0900-TRAN-010.
770700     IF  RHW-SW EQUAL 'E'
770710         MOVE RHW-OD-INT-PTR TO RMA-PTR                           1003625
770750         PERFORM X0280-RHR-CALC-DAYS THRU X0880-RHR-EXIT
770800     ELSE
770850         PERFORM X0230-RHR-START     THRU X0880-RHR-EXIT.
770900     GO TO X0900-TRAN-010.
770950     SKIP1
771000 X0900-TRAN-100.
771050     MOVE +0.00 TO ACCRUAL-BALANCE.
771100     IF  HOLD-TR-AMOUNT EQUAL +0.00
771120     AND INTEREST-ADJ-SUBCODE NOT EQUAL 'I'                       0902213
771150         MOVE +1 TO V
771200         GO TO X0900-100-EXIT.
771250     IF  RHW-OD-INT-PTR EQUAL +6
771300         MOVE 001 TO RHW-IND                                      1003625
771305         MOVE 'P' TO RHW-TYPE                                     1003625
771310         MOVE WMS-PRM-CUR-DATE TO C-HIGH-DATE                     1003625
771320         PERFORM X0130-8-TO-6-HI                                  IM008
771400     ELSE
771450         IF  RHW-OD-INT-PTR GREATER THAN +0 AND LESS THAN +6
771500             MOVE RHW-OD-INT-PTR TO RHW-IND                       1003625
771510             MOVE 'O' TO RHW-TYPE                                 1003625
771520             MOVE WMS-OD-CUR-DATE (RHW-OD-INT-PTR) TO C-HIGH-DATE 1003625
771540             PERFORM X0130-8-TO-6-HI                              IM008
771650         ELSE
771700             GO TO X0900-100-EXIT.
771710     IF  INTEREST-ADJ-SUBCODE EQUAL 'I'                           0902213
771720         GO TO X0900-100-EXIT.                                    0902213
771750     IF  HOLD-OD-BAL GREATER THAN HOLD-OD-LIMIT
771800         SUBTRACT HOLD-OD-LIMIT FROM HOLD-OD-BAL
771850             GIVING ACCRUAL-BALANCE
771900         MOVE HOLD-OD-LIMIT TO HOLD-OD-BAL
771950     ELSE
772000         GO TO X0900-100-EXIT.
772050     IF  HOLD-TR-AMOUNT GREATER THAN ACCRUAL-BALANCE
772100         SUBTRACT ACCRUAL-BALANCE FROM HOLD-TR-AMOUNT
772150     ELSE
772200         MOVE HOLD-TR-AMOUNT TO ACCRUAL-BALANCE
772250         MOVE +1             TO V
772300         MOVE +0.00          TO HOLD-TR-AMOUNT.
772350 X0900-100-EXIT.
772400     EXIT.
772450     SKIP1
772500 X0900-RHR-OD-ACCRUAL.
772550     MOVE WMS-OD-CYC-ACR TO ACCRUAL-AMOUNT.
772560     MOVE ACCRUAL-AMOUNT TO HOLD-INTEREST.                        0902213
772600     IF ACCRUAL-AMOUNT GREATER THAN ZERO
772650         ADD +.005 TO ACCRUAL-AMOUNT
772700     ELSE
772750         ADD -.005 TO ACCRUAL-AMOUNT.
772800     MOVE ACCRUAL-AMOUNT TO HOLD-ACCR2.
772850     MOVE ZERO TO ACCRUAL-AMOUNT.
772900 X0900-CALL-OD-PHASE.
772950     IF  WMS-OD-STATUS NOT EQUAL '0'
773000         MOVE ZERO TO WMS-OD-ACCRL-AMT
773050         GO TO X0900-CALL-ACR-END.
773100     IF  WBC-OD-INT-PHASE EQUAL SPACES                            0266741
773150         MOVE ZERO TO WMS-OD-ACCRL-AMT
773200         GO TO X0900-CALL-ACR-END.
773250     MOVE WBC-OD-INT-PHASE TO IM31-PH2.                           0266741
773300     MOVE WMS-OD-CYC-ACR TO ACCRUED-TO-DATE.
773350     CALL 'SILINK'   USING IM31-PHASE
773400                           MASTER-AREA
773450                           OD-CHARGE-PARAMETERS
773500                           DDA-WRKBCR-1                           0266741
773550                           DDA-WRKBCR-4                           0266741
773600                           INTEREST-ACCRUAL-PARAMETERS
773610                           RATE-WORK-FLAGS                        1003625
773620                           WS-RWF-OD-TABLE                        1003625
773625                           WS-RWF-PRIME-TABLE                     1003625
773650                           RATE-HIST-HOLD
773800                           RATE-HIST-ACCRUAL
773850                           EXCEPTION-AREA
773900                           EX-SEQ-CTR                             0902213
773910                           WORK-BAL-HIST-ODAC.                    0902213
773950 X0900-CALL-ACR-END.   EXIT.
774000     SKIP3
774050 X0900-ADJUST-ACCRUE.
774100*--------------------------------------------------------------*  0902213
774110*  CHECK THE BCR NEGATIVE ACCRUAL OPTION FOR BACKDATED TRANS   *  0902213
774120*  WHICH WOULD CAUSE THE ACCRUED INTEREST TO BE NEGATIVE.      *  0902213
774130*--------------------------------------------------------------*  0902213
774140* OD NEGATIVE ACCRUAL OPTION                                      0902213
774150     IF  INTEREST-ADJUST-CODE EQUAL '2'
774160     AND WBC-OD-NEG-ACCRUAL EQUAL '0'                             0266741
774170         IF  ACCRUAL-AMOUNT + WMS-OD-CYC-ACR LESS THAN +0.00      0902213
774180             MOVE '7' TO EX13-REASON-OR-OPT                       0902213
774190             PERFORM X0800-RHR-EXC-K THRU X0880-RHR-EXIT          0902213
774200             MOVE ZERO TO ACCRUAL-AMOUNT                          0902213
774210             GO TO X0900-RHR-OD-FINISH.                           0902213
774220     IF  BHW-ODAC-FLAG EQUAL 'N'                                  0902213
774230     AND (INTEREST-ADJ-SUBCODE EQUAL 'T' OR 'F' OR 'U')           9915845
774240         PERFORM X0840-RHR-EXC-H THRU X0880-RHR-EXIT              0902213
774250         MOVE '0' TO OD-ACR-PAY-FLAG.                             0902213
774260     MOVE ACCRUAL-AMOUNT TO RHW-ACCRUAL-AMOUNT.                   0902213
774270     IF  INTEREST-ADJUST-CODE EQUAL '2'                           0902213
774280         IF  ACCRUAL-AMOUNT GREATER THAN +0                       0902213
774290             ADD +.005 ACCRUAL-AMOUNT GIVING WK-HOLD-AMT13        9915845
774300         ELSE                                                     0902213
774310             ADD -.005 ACCRUAL-AMOUNT GIVING WK-HOLD-AMT13.       9915845
774311     IF  INTEREST-ADJUST-CODE EQUAL '2'                           9915255
774312     AND ACCRUAL-AMOUNT NOT EQUAL ZERO                            9915255
774313         MOVE WK-HOLD-AMT13 TO HOLD-ACCR1.                        1010055
774320     IF  INTEREST-ADJUST-CODE EQUAL '2'                           0902213
774325         IF  ACCRUAL-AMOUNT NOT EQUAL ZERO                        9915845
774330             ADD ACCRUAL-AMOUNT TO WMS-OD-CYC-ACR                 9915845
774350             MOVE 'K3' TO IMEX-CODE-1                             9915845
774400             MOVE '02' TO IMEX-REC-NO                             9915845
774450             MOVE HOLD-ACCR1 TO EX02-BAL                          9915255
774460             ADD +1 TO SEQ-CTR                                    9915845
774470             MOVE SEQ-CTR TO EX02-SEQ-NO                          9915845
774500             PERFORM WRITE-EX-MST THRU WRITE-EX-EXIT              9915845
774550             GO TO X0900-CALC-INT-CHG                             9915845
774560         ELSE                                                     9915845
774570             GO TO X0900-CALC-INT-CHG.                            9915845
774600     MOVE ACCRUAL-AMOUNT TO WMS-OD-ACCRL-AMT.
774650 X0900-ADD-DAILY.
774700     ADD WMS-OD-ACCRL-AMT TO WMS-OD-CYC-ACR
774750                             WMS-OD-ACCRD-TDY.
774800 X0900-CALC-INT-CHG.
774850     MOVE WMS-OD-CYC-ACR TO ACCRUAL-AMOUNT.
774900     IF ACCRUAL-AMOUNT GREATER THAN ZERO
774950         ADD +.005 TO ACCRUAL-AMOUNT
775000     ELSE
775050         ADD -.005 TO ACCRUAL-AMOUNT.
775100     MOVE ACCRUAL-AMOUNT TO HOLD-ACCR1.
775150     SUBTRACT HOLD-ACCR2 FROM HOLD-ACCR1.
775200     ADD HOLD-ACCR1 TO WMS-OD-MTD-ACR                             1105132
775210                       WMS-OD-YTD-ACR.                            1105132
775250     IF  INTEREST-ADJUST-CODE NOT EQUAL '2'
775300         ADD HOLD-ACCR1 TO OD-ACCRD-TODAY                         0902213
775301                           WS-GL-OD-ACCRD-TODAY                   0902970
775310     ELSE                                                         0902213
775320         ADD HOLD-ACCR1 TO OD-ACCRL-ADJ-INTRNL                    0902970
775321                           WS-GL-OD-ACCRL-ADJ-INTRNL.             0902970
775350     MOVE RHW-ACCRUAL-AMOUNT TO ACCRUAL-AMOUNT.
775400 X0900-RHR-OD-FINISH.
775450     MOVE 'F' TO RHW-OD.
775500 X0900-RHR-OD-EXIT.
775520     EXIT.                                                        IM008
775600     SKIP3
775650 X0910-RHR-CALC-PRIME.
775700     MOVE RNX-PRM-ANN TO ACCRUAL-ANNUAL-RATE.                     IM008
775750     IF  HOLD-OD-ADJ-FLAG EQUAL 'A'
775800         ADD HOLD-OD-PRIME-ADJ TO ACCRUAL-ANNUAL-RATE
775850     ELSE
775900         SUBTRACT HOLD-OD-PRIME-ADJ FROM ACCRUAL-ANNUAL-RATE.
775950     PERFORM C2302 THRU C2302-EXIT.                               IM008
775960     DIVIDE HOLD-NO3 INTO ACCRUAL-ANNUAL-RATE GIVING              IM008
775970         ACCRUAL-DAILY-RATE.                                      9915852
775980 X0910-RHR-CALC-EXIT.                                             9915852
775990     EXIT.                                                        9915852
776000 X0950-CAPITALIZE-INTEREST.                                       9915852
776001     IF  WMS-CAPITALIZATION  NOT EQUAL '1'                        9915852
776002         GO TO  X0950-EXIT.                                       9915852
776003     MOVE WMS-INTRST-BAL     TO HOLD-AMT.                         9915852
776004     ADD  WMS-INTRST-BAL     TO WMS-PRIN-BAL                      9915852
776006                                WMS-COLLECT-INT                   9915852
776008                                WMS-LOAN-YTD-INT                  9915852
776010                                WMS-LN-STMT-DB-AMT                9915852
776012                                WMS-LN-STMT-CR-AMT                9915852
776014                                WMS-YTD-INT-CAP                   9915852
776016                                WMS-INT-CAP                       9915852
776018                                LOAN-INT-PAID                     9915852
776020                                LOAN-DB-AMT                       9915852
776022                                LOAN-CR-AMT                       9915852
776024                                LOAN-DR-PRIN                      9915852
776026                                LOAN-CR-INT                       9915852
776027                                LOAN-TODAY-AMT                    9915852
776028                                AMT-LN-GEN-DR-CAP-INT             9915852
776029                                AMT-LN-GEN-CR-CAP-INT.            9915852
776030     ADD +1                  TO WMS-LN-STMT-DB-NO                 9915852
776032                                WMS-LN-STMT-CR-NO                 9915852
776034                                LOAN-DB-NO                        9915852
776036                                LOAN-CR-NO                        9915852
776037                                LOAN-TODAY-NO                     9915852
776038                                NO-LN-GEN-DR-CAP-INT              9915852
776039                                NO-LN-GEN-CR-CAP-INT.             9915852
776040     MOVE +0 TO WMS-INTRST-BAL.                                   9915845
776042     MOVE +0 TO PRIN-OLD-DISB  ADDL-CR-DISB  PRIN-DISB  INT-DISB  9915845
776043                OTH-DISB.                                         9915845
776044     MOVE WBC-GENERATED-TRANS (28) TO GEN-TRAN-OPTION-HOLD.       0266741
776046     MOVE HOLD-AMT                TO PRIN-DISB.                   9915852
776048     MOVE EX-LOAN-DISB            TO EX01-LOAN-DISB.              9915852
776050     MOVE 'G5' TO IMEX-CODE-2.                                    9915845
776052     MOVE '28' TO EX01-INT-CODE.                                  9915845
776054     MOVE '6' TO EX01-TYPE.                                       9915845
776056     MOVE ZERO TO EX01-NUMBER.                                    9915845
776058     PERFORM WRITE-GEN-TRANS THRU WRITE-GEN-EXIT.                 9915845
776060     MOVE +0 TO PRIN-OLD-DISB  ADDL-CR-DISB  PRIN-DISB  INT-DISB  9915845
776061                OTH-DISB.                                         9915845
776062     MOVE WBC-GENERATED-TRANS (28) TO GEN-TRAN-OPTION-HOLD.       0266741
776064     MOVE HOLD-AMT TO INT-DISB.                                   9915845
776066     MOVE EX-LOAN-DISB TO EX01-LOAN-DISB.                         9915845
776068     MOVE 'G5' TO IMEX-CODE-2.                                    9915845
776070     MOVE '28' TO EX01-INT-CODE.                                  9915845
776072     MOVE '5' TO EX01-TYPE.                                       9915845
776074     MOVE ZERO TO EX01-NUMBER.                                    9915845
776076     PERFORM WRITE-GEN-TRANS THRU WRITE-GEN-EXIT.                 9915845
776078     MOVE '1' TO LOAN-POST-TODAY.                                 9915845
776082     MOVE WBC-CAPTURE-DATE TO WMS-DATE-LAST-BAL-CHANGE            0266741
776084                             WMS-DATE-LAST-LOAN-ACTIVITY          9915845
776086                             WMS-DATE-LAST-ADVANCE.               9915845
776088     MOVE HOLD-AMT TO WMS-AMT-LAST-ADVANCE.                       9915845
776090     MOVE WMS-PRIN-BAL TO WMS-PRIN-LAST-ADVANCE.                  9915845
776091     MOVE WMS-INTRST-BAL TO WMS-INT-LAST-ADVANCE.                 9916091
776092     MOVE WMS-OTHER-CHGS TO WMS-CHGS-LAST-ADVANCE.                9916091
776093     PERFORM CALC-AVAIL-BAL THRU CALC-AVAIL-EXIT.                 9916091
776094 X0950-EXIT.                                                      9916091
776095       EXIT.                                                      9916091
776096*--------------------------------------------------------------*  9916091
776097***  THE FOLLOWING EXIT IS FOR INTERBRANCH TRANSFER PROCESSING *  9916091
776098***                                                            *  9916091
776099     COPY IMPD31IB.                                               9916091
776100***                                                            *  9915845
776101***  END OF IBT EXIT.                                          *  9916091
776102*--------------------------------------------------------------*  9916091
776103 X2100-00-RESET-RATES.                                            9916091
776104     IF  RMA-PRODUCT EQUAL WMS-ACCT-TYPE                          9916091
776105         GO TO X2100-99-EXIT.                                     9916091
776106     IF  WMS-LOAN-TRLR NOT EQUAL '1'                              9916091
776107         GO TO X2100-99-EXIT.                                     IM008
776108     IF  WMS-INT-RATE-PTR GREATER THAN +5                         9916091
776109         MOVE '1' TO WMS-LOAN-CALC-CODE                           9916091
776110         PERFORM PRIME-RATE-CHANGE THRU PRIME-CHANGE-EXIT         9916091
776111         GO TO X2100-99-EXIT.                                     9916091
776112     IF (WMS-INT-RATE-PTR GREATER THAN +0)                        9916091
776113     OR (WMS-SPLIT-RATE-SCHD GREATER THAN +0)                     9916091
776114         MOVE '1' TO WMS-LOAN-CALC-CODE                           9916091
776115         MOVE WBC-CAPTURE-DATE TO WMS-DATE-LAST-RATE-CHANGE.      0266741
776116 X2100-99-EXIT.                                                   9916091
776117     EXIT.                                                        9916091
776118                                                                  9916091
776119 X3000-REJ-REASON.                                                9916091
776120     IF  TR-REJ-RESN EQUAL WK-REJ-REASON                          9916091
776122         GO TO X3000-EXIT.                                        9916091
776124     IF  TR-REJ-RESN EQUAL SPACES                                 9916091
776125         MOVE WK-REJ-REASON TO TR-REJ-RESN                        9916091
776126         GO TO X3000-EXIT.                                        9916091
776127     MOVE +0 TO V.                                                9916091
776128 X3000-10.                                                        9916091
776129     ADD +1 TO V.                                                 9916091
776130     IF  V GREATER THAN +3                                        9916091
776131         GO TO X3000-20.                                          0717520
776132     IF TR-REJ-REASONS (V) EQUAL WK-REJ-REASON                    9916091
776133         GO TO X3000-20.                                          0717520
776135     IF  TR-REJ-REASONS (V) EQUAL SPACES                          IM008
776136         MOVE WK-REJ-REASON TO TR-REJ-REASONS (V)                 IM008
776137         GO TO X3000-20.                                          0717520
776138     GO TO X3000-10.                                              IM008
776139 X3000-20.                                                        0717520
776140     IF  TR-REJ-RESN NUMERIC                                      0717520
776141         GO TO X3000-EXIT.                                        0717520
776142     MOVE TR-REJ-RESN TO WK-REJ-REASON2.                          0717520
776143     MOVE +0 TO V.                                                0717520
776144 X3000-30.                                                        0717520
776145     ADD +1 TO V.                                                 0717520
776146     IF  V GREATER THAN +3                                        0717520
776147         GO TO X3000-EXIT.                                        0717520
776148     IF  TR-REJ-REASONS (V) NUMERIC                               0717520
776149         MOVE TR-REJ-REASONS (V) TO TR-REJ-RESN                   0717520
776150         MOVE WK-REJ-REASON2 TO TR-REJ-REASONS (V)                0717520
776151         GO TO X3000-EXIT                                         0717520
776152     ELSE                                                         0717520
776153         GO TO X3000-30.                                          0717520
776154 X3000-EXIT.                                                      0717520
776155     EXIT.                                                        0717520
776156     SKIP3                                                        0717520
776157 X4000-ROLL-CHECK.                                                0717520
776160     IF  WMS-SAVINGS-TRLR EQUAL '1' AND                           IM004
776165         WMS-CMA-INDICATOR EQUAL '1' AND                          IM004
776170         WMS-CMA-BALANCE-USE EQUAL '1'                            IM004
776175         NEXT SENTENCE                                            IM004
776180     ELSE                                                         IM004
776185         GO  TO  X4045-EXIT.                                      IM008
776188 X4000-ROLL-PREV-AVAIL.                                           IM008
776191     IF  WMS-BANK-AVAIL-TRLR EQUAL TO '0', GO TO X4035-EXIT.      IM008
776194     IF  SAVE-BANK-AMT EQUAL ZEROES, GO TO X4035-EXIT.            IM008
776203     MOVE SAVE-BANK-2-7 TO SAVE-BANK-TRLR.                        IM007
776204     MOVE ZERO TO SAVE-BANK-TR-AMT (7).                           IM007
776206 X4035.                                                           IM008
776207     ADD SAVE-BANK-TR-AMT (1)                                     IM007
776208         SAVE-BANK-TR-AMT (2)                                     IM007
776209         SAVE-BANK-TR-AMT (3)                                     IM007
776210         SAVE-BANK-TR-AMT (4)                                     IM007
776211         SAVE-BANK-TR-AMT (5)                                     IM007
776212         SAVE-BANK-TR-AMT (6) GIVING SAVE-BANK-AMT.               IM007
776213 X4035-EXIT. EXIT.                                                IM008
776214 X4040.                                                           IM008
776215     IF  WMS-CUST-AVAIL-TRLR EQUAL TO '0', GO TO X4045-EXIT.      IM008
776216     IF  SAVE-CUST-AMT EQUAL ZEROES, GO TO X4045-EXIT.            IM008
776217     MOVE SAVE-CUST-2-12 TO SAVE-CUST-TRLR.                       IM007
776218     MOVE ZERO TO SAVE-CUST-TR-AMT (12).                          IM007
776219 X4045.                                                           IM008
776221     ADD  SAVE-CUST-TR-AMT (1)                                    IM007
776222          SAVE-CUST-TR-AMT (2)                                    IM007
776223          SAVE-CUST-TR-AMT (3)                                    IM007
776224          SAVE-CUST-TR-AMT (4)                                    IM007
776225          SAVE-CUST-TR-AMT (5)                                    IM007
776226          SAVE-CUST-TR-AMT (6)                                    IM007
776227          SAVE-CUST-TR-AMT (7)                                    IM007
776228          SAVE-CUST-TR-AMT (8)                                    IM007
776229          SAVE-CUST-TR-AMT (9)                                    IM007
776231          SAVE-CUST-TR-AMT (10)                                   IM007
776233          SAVE-CUST-TR-AMT (11) GIVING SAVE-CUST-AMT.             IM007
776239 X4045-EXIT. EXIT.                                                IM008
776241     SKIP3                                                        IM004
776243 X4050-CALC-ACCR-PREV.                                            IM008
776245     IF CALC-FLAG EQUAL TO '2'                                    IM004
776247         SUBTRACT SAVE-HOLD-AMT FROM CUR-BAL                      IM004
776249         GO TO X4060-EXIT.                                        IM008
776251     IF CALC-FLAG EQUAL TO '3'                                    IM004
776253         SUBTRACT SAVE-BANK-AMT FROM CUR-BAL                      IM007
776255         GO TO X4060-EXIT.                                        IM008
776257     IF CALC-FLAG EQUAL TO '4'                                    IM004
776259         SUBTRACT SAVE-CUST-AMT FROM CUR-BAL                      IM007
776261         GO TO X4060-EXIT.                                        IM008
776263     IF CALC-FLAG EQUAL TO '5'                                    IM004
776265         SUBTRACT SAVE-HOLD-AMT FROM CUR-BAL                      IM004
776267         SUBTRACT SAVE-BANK-AMT FROM CUR-BAL                      IM007
776269         GO TO X4060-EXIT.                                        IM008
776271     IF CALC-FLAG EQUAL TO '6'                                    IM004
776273         SUBTRACT SAVE-HOLD-AMT FROM CUR-BAL                      IM004
776275         SUBTRACT SAVE-CUST-AMT FROM CUR-BAL.                     IM007
776277 X4060-EXIT.  EXIT.                                               IM008
776279     SKIP3                                                        IM004
776281 X5000-TARGET-TOTALS.                                             IM008
776283*    FORCE TRANSACTIONS TO PARENT IF TRAILER DOESN'T EXIST.       IM006
776285     IF  WMS-TARGET-AMT-TRLR NOT EQUAL '1'                        IM006
776287         IF  TR-TRAN-TYPE LESS THAN '3'                           9915845
776289             ADD +1 TO DDA-TARGET-PARNT-NO-CR                     IM006
776290                       WS-GL-DDA-TARGET-PARNT-NO-CR               0902970
776291             ADD TR-AMOUNT TO DDA-TARGET-PARNT-AMT-CR             IM006
776292                              WS-GL-DDA-TARGET-PARNT-AMT-CR       0902970
776293             GO TO X5000-EXIT                                     IM008
776295         ELSE                                                     IM006
776297             ADD +1 TO DDA-TARGET-PARNT-NO-DB                     IM006
776298                       WS-GL-DDA-TARGET-PARNT-NO-DB               0902970
776299             ADD TR-AMOUNT TO DDA-TARGET-PARNT-AMT-DB             IM006
776300                              WS-GL-DDA-TARGET-PARNT-AMT-DB       0902970
776301             GO TO X5000-EXIT.                                    IM008
776303*    FORCE TRANSACTIONS TO PARENT IF TARGET-DELETE EQUALS '2'.    IM006
776305     IF  WMS-TARGET-DELETE EQUAL '2'                              IM006
776307         IF  TR-TRAN-TYPE LESS THAN '3'                           9915845
776309             ADD +1 TO DDA-TARGET-PARNT-NO-CR                     IM006
776310                       WS-GL-DDA-TARGET-PARNT-NO-CR               0902970
776311             ADD TR-AMOUNT TO DDA-TARGET-PARNT-AMT-CR             IM006
776312                              WS-GL-DDA-TARGET-PARNT-AMT-CR       0902970
776313             GO TO X5000-EXIT                                     IM008
776315         ELSE                                                     IM006
776317             ADD +1 TO DDA-TARGET-PARNT-NO-DB                     IM006
776318                       WS-GL-DDA-TARGET-PARNT-NO-DB               0902970
776319             ADD TR-AMOUNT TO DDA-TARGET-PARNT-AMT-DB             IM006
776320                              WS-GL-DDA-TARGET-PARNT-AMT-DB       0902970
776321             GO TO X5000-EXIT.                                    IM008
776323*    DEFAULT TRANSACTIONS TO SUB-ACCOUNT TOTALS.                  IM006
776325     IF  TR-TRAN-TYPE LESS THAN '3'                               9915845
776327         ADD +1 TO DDA-TARGET-SUBAC-NO-CR                         IM006
776328                   WS-GL-DDA-TARGET-SUBAC-NO-CR                   0902970
776329         ADD TR-AMOUNT TO DDA-TARGET-SUBAC-AMT-CR                 IM006
776330                          WS-GL-DDA-TARGET-SUBAC-AMT-CR           0902970
776331     ELSE                                                         IM006
776333         ADD +1 TO DDA-TARGET-SUBAC-NO-DB                         IM006
776334                   WS-GL-DDA-TARGET-SUBAC-NO-DB                   0902970
776335         ADD TR-AMOUNT TO DDA-TARGET-SUBAC-AMT-DB,                0902970
776336                          WS-GL-DDA-TARGET-SUBAC-AMT-DB.          0902970
776337 X5000-EXIT. EXIT.                                                IM008
776339     SKIP3                                                        IM006
776340*----------------------------------------------------------------*1004015
776341*    CALCULATE AVERAGES AND CLEAR AGGREGATES FOR REG DD          *1004015
776342*----------------------------------------------------------------*1004015
776343 X6000-TIS-CALC-APY.                                              1004015
776344     IF  FULL-SHEET-TODAY EQUAL '3'                               1004015
776345         GO TO X6000-TIS-EXIT.                                    1004015
776346     IF  WMS-IOD-TIS-INT-RECALC EQUAL 'Y'                         1004015
776347         GO TO X6000-TIS-IOD-CLEAR.                               1004015
776348     IF  IOD-TIS-AGGR-DAY (STMT-CYCLE-DAY) GREATER THAN ZERO      1004015
776349     AND IOD-TIS-AGGR-BAL (STMT-CYCLE-DAY) GREATER THAN ZERO      1004015
776350         DIVIDE IOD-TIS-AGGR-DAY (STMT-CYCLE-DAY)                 1004015
776351           INTO IOD-TIS-AGGR-BAL (STMT-CYCLE-DAY)                 1004015
776352         GIVING WMS-IOD-TIS-AVG-BAL                               1004015
776353     ELSE                                                         1004015
776354         MOVE ZERO TO WMS-IOD-TIS-AVG-BAL.                        1004015
776355     IF  (WMS-IOD-ACCRUAL-TYPE EQUAL 'C' OR 'D')                  1105232
776356         PERFORM 2000-APYE-SPEC-RULE THRU 9999-APYE-EXIT          1105232
776357     ELSE   MOVE 'N' TO WS-SKIP-APYE.                             1105232
776358     IF  WS-SKIP-APYE EQUAL 'Y'                                   1105232
776359         ADD WMS-IOD-TIS-CYC-BEGIN-ACCR TO WMS-IOD-TIS-AVG-BAL.   1105232
776360     MOVE IOD-TIS-AGGR-DAY (STMT-CYCLE-DAY)                       1105232
776361                            TO WMS-IOD-TIS-AVG-DAYS.              1105232
776362     ADD +0.005, IOD-TIS-EARN-INT (STMT-CYCLE-DAY)                1105232
776363         GIVING WMS-IOD-TIS-REPT-INT.                             1105232
776364 X6000-TIS-IOD-CLEAR.                                             1105232
776365     IF  WMS-STMT-CLEAR EQUAL '1'                                 1105232
776366     OR  FULL-SHEET-TODAY EQUAL '2'                               1105232
776367         NEXT SENTENCE                                            1105232
776368     ELSE                                                         1105232
776369         GO TO X6000-TIS-SAV-APY.                                 1105232
776370     IF TIS-STMT-SW EQUAL '1' GO TO X6000-TIS-SAV-APY.            1105232
776371     SUBTRACT IOD-TIS-AGGR-DAY (STMT-CYCLE-DAY)                   1105232
776372         FROM WMS-IOD-TIS-AGGR-DAYS.                              1105232
776373     SUBTRACT IOD-TIS-AGGR-BAL (STMT-CYCLE-DAY)                   1105232
776374         FROM WMS-IOD-TIS-AGGR-BAL.                               1105232
776375     SUBTRACT IOD-TIS-EARN-INT (STMT-CYCLE-DAY)                   1105232
776376         FROM WMS-IOD-TIS-EARN-INT.                               1105232
776377     MOVE '1' TO TIS-STMT-SW.                                     1105232
776378     IF  WS-SKIP-APYE EQUAL 'Y'                                   1105232
776379         IF  WS-ZERO-CYC-BEG-ACCR EQUAL 'Y'                       1105232
776380             MOVE ZERO TO WMS-IOD-TIS-CYC-BEGIN-ACCR              1105232
776381         ELSE                                                     1105232
776382             MOVE WS-IOD-TIS-HOLD-CYC-BEG-ACCR TO                 1105232
776383                                  WMS-IOD-TIS-CYC-BEGIN-ACCR.     1105232
776384 X6000-TIS-SAV-APY.                                               1105232
776385     IF  WMS-SAVINGS-TRLR EQUAL '0'                               1105232
776386         GO TO X6000-TIS-EXIT.                                    1105232
776387     IF  SAV-TIS-AGGR-DAY (STMT-CYCLE-DAY) GREATER THAN ZERO      1105232
776388     AND SAV-TIS-AGGR-BAL (STMT-CYCLE-DAY) GREATER THAN ZERO      1105232
776389         DIVIDE SAV-TIS-AGGR-DAY (STMT-CYCLE-DAY)                 1105232
776390           INTO SAV-TIS-AGGR-BAL (STMT-CYCLE-DAY)                 1105232
776391         GIVING WMS-INT-TIS-AVG-BAL                               1105232
776392     ELSE MOVE ZERO TO WMS-INT-TIS-AVG-BAL.                       1105232
776393     MOVE 'N' TO WS-SKIP-APYE.                                    1105232
776394     IF  (WMS-IOD-ACCRUAL-TYPE EQUAL 'C' OR 'D')                  1105232
776395         PERFORM 3000-APYE-SPEC-RULE THRU 9999-APYE-EXIT          1105232
776396         IF  WS-SKIP-APYE EQUAL 'Y'                               1105232
776397             ADD WMS-INT-TIS-CYC-BEGIN-ACCR TO                    1105232
776398                                         WMS-INT-TIS-AVG-BAL.     1105232
776399     MOVE SAV-TIS-AGGR-DAY (STMT-CYCLE-DAY)                       1105232
776400                                     TO  WMS-INT-TIS-AVG-DAYS.    1105232
776401     ADD SAV-TIS-EARN-INT (STMT-CYCLE-DAY)                        1105232
776402         +0.005                  GIVING  WMS-INT-TIS-REPT-INT.    1105232
776403     IF  WMS-STMT-CLEAR EQUAL '1'                                 1105232
776404     OR  FULL-SHEET-TODAY EQUAL '2'                               1105232
776405         NEXT SENTENCE                                            1105232
776406     ELSE                                                         1105232
776407         GO TO X6000-TIS-EXIT.                                    1105232
776408     IF  TIS-SV-STMT-SW EQUAL '1'                                 1105232
776409         GO TO X6000-TIS-EXIT.                                    1105232
776410     SUBTRACT SAV-TIS-AGGR-DAY (STMT-CYCLE-DAY)                   1105232
776411                                   FROM  WMS-INT-TIS-AGGR-DAYS.   1105232
776412     SUBTRACT SAV-TIS-AGGR-BAL (STMT-CYCLE-DAY)                   1105232
776413                                   FROM  WMS-INT-TIS-AGGR-BAL.    1105232
776414     SUBTRACT SAV-TIS-EARN-INT (STMT-CYCLE-DAY)                   1105232
776415                                   FROM  WMS-INT-TIS-EARN-INT.    1105232
776416     MOVE '1'                        TO  TIS-SV-STMT-SW.          1105232
776417     IF  WS-SKIP-APYE EQUAL 'Y'                                   1105232
776418         IF  WS-ZERO-CYC-BEG-ACCR EQUAL 'Y'                       1105232
776419             MOVE ZERO TO WMS-INT-TIS-CYC-BEGIN-ACCR              1105232
776420         ELSE                                                     1105232
776421             MOVE WS-INT-TIS-HOLD-CYC-BEG-ACCR                    1105232
776422                                    TO WMS-INT-TIS-CYC-BEGIN-ACCR.1105232
776423 X6000-TIS-EXIT.                                                  1105232
776424     EXIT.                                                        1105232
776425                                                                  1105232
776426 BCR-SECTION         SECTION 48.                                  1105232
776427     SKIP3                                                        1105232
776428 Y0100.                                                           1105232
776429     IF  WBC-PROCESS-FLAG NOT EQUAL '1'                           0266741
776430         GO TO Y0200.                                             1105232
776432     IF  WBC-MONTH-END EQUAL '1'                                  0417181
776433     OR  WBC-PREV-MONTH-END EQUAL '1'                             0417181
776434         PERFORM Y5000-40-TFRCSI-AVERAGE THRU Y5000-40-EXIT.      0417181
776436     IF  WK-RCK06A0-NO NOT EQUAL ZEROS                            1004773
776437         DIVIDE WK-RCK06A0-NO INTO WK-RCK06A0-AMT GIVING          1004773
776438           WK-RCK06A0-AVG                                         1004773
776439     ELSE                                                         1004773
776440         MOVE ZEROS TO WK-RCK06A0-AVG.                            1004773
776441     ADD WK-RCK06A0-AMT           TO WBC-REG-CALL-AMT (15).       0266741
776442     IF  WK-RCK06D0-NO NOT EQUAL ZEROS                            1004773
776443         DIVIDE WK-RCK06D0-NO INTO WK-RCK06D0-AMT GIVING          1004773
776444           WK-RCK06D0-AVG                                         1004773
776445     ELSE                                                         1004773
776446         MOVE ZEROS TO WK-RCK06D0-AVG.                            1004773
776447     ADD WK-RCK06D0-AMT           TO WBC-REG-CALL-AMT (16).       0266741
776448     IF  WK-RCK06E0-NO NOT EQUAL ZEROS                            1004773
776449         DIVIDE WK-RCK06E0-NO INTO WK-RCK06E0-AMT GIVING          1004773
776450           WK-RCK06E0-AVG                                         1004773
776451     ELSE                                                         2010312
776452         MOVE ZEROS TO WK-RCK06E0-AVG.                            2010312
776453     ADD WK-RCK06E0-AMT           TO WBC-REG-CALL-AMT (17).       0266741
776454     IF  WK-RCK1000-NO NOT EQUAL ZEROS                            2010312
776455         DIVIDE WK-RCK1000-NO INTO WK-RCK1000-AMT GIVING          2010312
776456           WK-RCK1000-AVG                                         2010312
776457     ELSE                                                         2010312
776458         MOVE ZEROS TO WK-RCK1000-AVG.                            2010312
776459     ADD WK-RCK1000-AMT           TO WBC-REG-CALL-AMT (18).       0266741
776460     IF  WK-RCK11A0-NO NOT EQUAL ZEROS                            2010312
776461         DIVIDE WK-RCK11A0-NO INTO WK-RCK11A0-AMT GIVING          2010312
776462           WK-RCK11A0-AVG                                         2010312
776463     ELSE                                                         2010312
776464        MOVE ZEROS TO WK-RCK11A0-AVG.                             2010312
776465     ADD WK-RCK11A0-AMT           TO WBC-REG-CALL-AMT (19).       0266741
776466     IF  WBC-PROC-THRU-YR LESS THAN WBC-NEXT-CENT-YR              0266741
776467         MOVE '20'            TO C-HIGH-CENT                      2010312
776468     ELSE                                                         2010312
776469         MOVE '19'            TO C-HIGH-CENT.                     2010312
776470     IF  WBC-QUARTER-END EQUAL '1'                                0266741
776471         MOVE 3                   TO C-NO-DAYS                    2010312
776472         MOVE WBC-PROC-THRU-MO    TO C-HIGH-MO                    0266741
776473         MOVE '31'                TO C-HIGH-DA                    2010312
776474         MOVE WBC-PROC-THRU-YR    TO C-HIGH-YR                    0266741
776475         CALL 'SIDCMLO' USING C-DATE-WORK                         2010312
776476         MOVE WBC-PROC-THRU-DA    TO C-HIGH-DA                    0266741
776477         MOVE ZERO TO C-NO-DAYS                                   2016539
776478         CALL 'SIDIFC1' USING C-DATE-WORK                         2016539
776479         IF  C-NO-DAYS GREATER THAN ZERO                          2016539
776480             DIVIDE WBC-REG-CALL-AMT (15) BY C-NO-DAYS            0266741
776481                                 GIVING WBC-REG-CALL-AMT (15)     0266741
776482             DIVIDE WBC-REG-CALL-AMT (16) BY C-NO-DAYS            0266741
776483                                 GIVING WBC-REG-CALL-AMT (16)     0266741
776484             DIVIDE WBC-REG-CALL-AMT (17) BY C-NO-DAYS            0266741
776485                                 GIVING WBC-REG-CALL-AMT (17)     0266741
776486             DIVIDE WBC-REG-CALL-AMT (18) BY C-NO-DAYS            0266741
776487                                 GIVING WBC-REG-CALL-AMT (18)     0266741
776488             DIVIDE WBC-REG-CALL-AMT (19) BY C-NO-DAYS            0266741
776489                                 GIVING WBC-REG-CALL-AMT (19).    0266741
776490     PERFORM CLEAR-TFR-TOTALS THRU CLEAR-TFR-EXIT.                2010312
776491     PERFORM CALL-TRIAL-BAL THRU CALL-TRIAL-END.                  2010312
776492     MOVE 'W' TO I-O-CONTROL-OPERATOR.                            2010312
776493     CALL 'IMBCRM' USING I-O-CONTROL-AREA                         2010312
776494                         DDA-WRKBCR-7.                            0266741
776495     IF  NOT I-O-88-NORMAL-RET                                    2010312
776496             GO TO Z2000.                                         2010312
776500 Y0200.                                                           IM008
776550     IF  LOW-EOF EQUAL HIGH-VALUE
776600         GO TO Z0010.
776650 Y0300.                                                           IM008
776700     MOVE '0' TO NEW-BK-NO-TRN-FLAG.
776750*   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
776800*    IF YOU ARE PROCESSING A NEW BANK WITH NO TRAN-FILE
776850*    AND NO MASTER-FILE, THE NEW-BK-NO-TRN-FLAG IS SET TO 1
776900*    TO ALLOW A MASTER HEADER RECORD TO BE BUILT FOR THAT BANK.
776950*   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
777000 Y1100.
777050     MOVE 'R' TO I-O-CONTROL-OPERATOR.
777100     CALL 'IMBCRM' USING I-O-CONTROL-AREA
777150                         DDA-WRKBCR-4.                            0266741
777200     IF I-O-88-END-OF-FILE
777250         GO TO Z2000.
777300     IF WBC4-RECORD-ID EQUAL TO 'B1'                              0266741
777350         MOVE DDA-WRKBCR-4 TO DDA-WRKBCR-1                        0266741
777360         MOVE WBC-DEFAULT-LANGUAGE TO SIWS-LANG-CODE              0266741
777361                                      WSSPLEX-HDG-LANG-CODE       0266741
777362         PERFORM 0000-00-SEARCH-LANG-TABLE                        0266741
777363            THRU 0000-00-SEARCH-LANG-EXIT                         0266741
777370         MOVE CB-SHORT-NAME-LIT(SIWS-LANG-SUB)                    0266741
777371                                             TO SHORT-NAME-LIT    0266741
777380         MOVE CB-COST-CENTER-LIT(SIWS-LANG-SUB)                   0266741
777381                                             TO COST-CENTER-LIT   0266741
777390         MOVE CB-OD-LIMIT-AMT-LIT(SIWS-LANG-SUB)                  0266741
777391                                             TO OD-LIMIT-AMT-LIT  0266741
777392         MOVE CB-BAL-HIST-CHK-LIT(SIWS-LANG-SUB)                  0266741
777393                                             TO BAL-HIST-CHK.     0266741
777400     IF WBC4-CONTROL-1 NOT EQUAL TO LOW-1                         0266741
777450     OR WBC-CTL2-FLAG EQUAL '2' AND                               0266741
777500       WBC4-CONTROL-2 NOT EQUAL TO LOW-2                          0266741
777550     OR WBC-CTL3-FLAG EQUAL '2' AND                               0266741
777600       WBC4-CONTROL-3 NOT EQUAL TO LOW-3                          0266741
777650         IF WBC4-RECORD-ID EQUAL 'B4' AND                         0266741
777700             WBC-PROCESS-FLAG EQUAL '1' AND                       0266741
777750             WBC-RUN-FLAG EQUAL 'I'                               0266741
777800                 MOVE '1' TO NEW-BK-NO-TRN-FLAG
777850         ELSE GO TO Y1100.
777900     MOVE WBC4-CONTROL-KEY TO HOLD-CONTROL.                       0266741
777902     MOVE WBC-EXCTBL-PHASE TO RPT-EXC-PHASE.                      0447267
777904     MOVE 'K' TO I-O-CONTROL-OPERATOR.                            0447267
777906     CALL 'SITBLMV' USING I-O-CONTROL-AREA,                       0447267
777908                          RPT-EXC-TABLE-AREA.                     0447267
777910     IF  NOT I-O-88-NORMAL-RET                                    0447267
777912         MOVE PROGRAM-NAME TO SIMESS-PROGRAM                      0447267
777914         MOVE 503 TO SIMESS-MESS-NO                               0447267
777916         MOVE 'REPORT EXCEPTION TABLE' TO SIMESS-OPTIONAL-MESSAGE 0447267
777918         CALL 'SIMESS' USING SIMESS-AREA.                         0447267
777920     MOVE +72 TO RPTEX-TBL-DISPLACEMENT.                          0447267
777922     MOVE +8  TO RPTEX-TBL-ENTRY-SIZE.                            0447267
777924     MOVE +4  TO RPTEX-TBL-KEY.                                   0447267
777926     MOVE +0  TO RPTEX-TBL-CONSTANT-0.                            0447267
777928     MOVE RPT-EXC-TBL-ENTRY (1) TO WK-BISCH-ENTRY.                0447267
777930     MULTIPLY WK-BISCH-LOCATION BY RPTEX-TBL-ENTRY-SIZE GIVING    0447267
777932         RPTEX-TBL-SIZE.                                          0447267
777934     SUBTRACT RPTEX-TBL-ENTRY-SIZE FROM RPTEX-TBL-SIZE.           0447267
777950     IF WBC4-RECORD-ID EQUAL TO 'B2'                              0266741
778000         MOVE DDA-WRKBCR-4 TO DDA-WRKBCR-2                        0266741
778050         GO TO Y1200.
778100     IF WBC4-RECORD-ID EQUAL 'B3'                                 0266741
778150         MOVE DDA-WRKBCR-4 TO DDA-WRKBC-3                         0266741
778200         GO TO Y1100.
778250     IF WBC4-RECORD-ID NOT EQUAL TO 'B4', GO TO Y1100.            0266741
778255     MOVE WBC-CAPTURE-YR TO WK-LEAP-YR.                           0266741
778260     MOVE WBC-LEAP-YEAR TO WK-LEAP-YEAR.                          0266741
778300     MOVE ZERO TO DDA-AN-DATA.
778350     MOVE WBC-TRIAL-PHASE TO IM31-PH2.                            0266741
778370     MOVE WBC-CURR-DEC TO WSSPLEX-CURR-DECIMALS.                  0266741
778400     MOVE IM31-PHASE TO TRIAL-PHASE.
778450     IF WBC-TEMP-TRIAL-SPOOL EQUAL TO SPACE                       0266741
778500         MOVE WBC-IM31-SPOOL TO TRIAL-SPOOL                       0266741
778550     ELSE MOVE WBC-TEMP-TRIAL-SPOOL TO TRIAL-SPOOL.               0266741
778600     IF WBC-RUN-FLAG EQUAL TO 'I' AND                             0266741
778610        WBC-PROCESS-FLAG EQUAL TO '1'                             0266741
778650         MOVE '1' TO NEW-BANK-SW.
778700     MOVE '1' TO BCR-BRK-SW.
778705     MOVE '0' TO YEAR-CHANGE.                                     1001295
778708     IF  WBC-YEAR-END EQUAL '1'                                   0266741
778710         MOVE '12'                 TO DT-L-MO                     1001295
778712         MOVE '31'                 TO DT-L-DA                     1001295
778714         MOVE WBC-NEXT-CAPTURE-YR  TO DT-L-YR                     0266741
778716         CALL 'SIDJUL' USING DATE-AREA                            1001295
778718         IF  WBC-LEAP-YEAR EQUAL '1'                              0266741
778720             MOVE '1'              TO YEAR-CHANGE                 1001295
778722             MOVE DT-JUL-DA        TO NEXT-DAYS                   1001295
778724         ELSE                                                     1001295
778726             IF  DT-JUL-DA EQUAL +366                             1001295
778728                 MOVE '1'          TO YEAR-CHANGE                 1001295
778730                 MOVE DT-JUL-DA    TO NEXT-DAYS                   9915845
778732         ELSE                                                     9915845
778734                 MOVE DT-JUL-DA    TO NEXT-DAYS.                  9915845
778740     IF  WBC-EXTEND-FLOAT-OPT EQUAL '1' OR '2' OR '3'             0927817
778745         PERFORM Y1350 THRU Y1350-EXIT.                           0927817
778750     IF  WBC-BAL-HIST-FLAG NOT EQUAL '0'                          0266741
778800         PERFORM Y1400 THRU Y1400-EXIT.                           0902213
778810     MOVE WBC-WEEK-DAY TO HOLD-WEEK-DAY                           0266741
778820                         HOLD-WEEK-DAY2.                          9915682
778830     ADD +1 TO HOLD-WEEK-DAY2.                                    9915682
778840     IF  HOLD-WEEK-DAY2 GREATER THAN +7                           9915682
778850         MOVE +1 TO HOLD-WEEK-DAY2.                               9915682
778860     IF  WBC-WEEKDAY-BK (HOLD-WEEK-DAY) EQUAL '1'                 0266741
778870         MOVE +1 TO LR-DAY.                                       9915682
778875     IF  WBC-WEEKDAY-BK (HOLD-WEEK-DAY) EQUAL '2' AND             0266741
778880         WBC-WEEKDAY-BK (HOLD-WEEK-DAY2) EQUAL '0' OR '2'         0266741
778890         MOVE +2 TO LR-DAY.                                       9915682
778900     MOVE WBC-CAPTURE-MO TO C-HIGH-MO.                            0266741
778905     MOVE WBC-CAPTURE-DA TO C-HIGH-DA.                            0266741
778910     MOVE WBC-CAPTURE-YR TO C-HIGH-YR                             0266741
778915                           RHA-BC-CAPTURE-YR                      1003625
778916                           WS-NY-YR.                              1003625
778920     IF  WBC-CAPTURE-YR GREATER THAN WBC-NEXT-CENT-YR             0266741
778925         MOVE '19'      TO C-HIGH-CENT                            IM008
778926                           WS-NY-CC                               1003625
778930     ELSE                                                         IM008
778932         MOVE '20'      TO C-HIGH-CENT                            1105504
778934                           WS-NY-CC.                              1105504
778935*----------------------------------------------------------------*1105504
778936*    CREATE INCORE TABLES FOR SERVICE CHARGE ROUTINES.           *1105504
778937*----------------------------------------------------------------*1105504
778938     IF  WBC-PROCESS-FLAG EQUAL '1'                               0266741
778939         PERFORM Y2000-BUILD-FEE-XREF THRU Y2000-BUILD-FEE-EXIT   9915749
778940         PERFORM Y3000-BUILD-PRD-TBLS THRU Y3000-BUILD-PRD-EXIT   9915749
778942         PERFORM READ-RATE-WORK THRU RATE-WORK-END                1003625
778943         PERFORM Y1500-CALC-RATE-WORK-DAF THRU Y1500-END          1003625
778944         MOVE ZEROES TO RMA-KEY                                   1003625
778945         MOVE 'K' TO I-O-CONTROL-OPERATOR                         1003625
778946         CALL 'IMRTEMV' USING I-O-CONTROL-AREA                    1003625
778947                              RATE-MASTER-AREA                    1003625
778948         IF  RMA-KEY EQUAL ZEROES                                 1003625
778949             MOVE WBC1-CONTROL-1 TO BANK-SUB                      0266741
778950             MOVE RMA-PROC-THRU-TBL (BANK-SUB)                    1003625
778951               TO WS-RMA-ZERO-KEY-REC.                            1003625
778954     IF  WBC-MONTH-END NOT EQUAL '1'                              0266741
778958         GO TO Y1140.                                             IM008
778959     PERFORM Y1310 THRU Y1310-EXIT.                               2016374
778960     MOVE HOLD-780-MONTHS TO C-NO-DAYS.                           IM005
778962     MOVE WBC-CAPTURE-MO TO C-HIGH-MO.                            0266741
778966     MOVE WBC-CAPTURE-DA TO C-HIGH-DA.                            0266741
778970     MOVE WBC-CAPTURE-YR TO C-HIGH-YR.                            0266741
778974     IF  WBC-CAPTURE-YR GREATER THAN WBC-NEXT-CENT-YR             0266741
778978         MOVE '19' TO C-HIGH-CENT                                 IM005
778982     ELSE                                                         IM005
778986         MOVE '20' TO C-HIGH-CENT.                                IM005
778989     CALL 'SIDCMLO' USING C-DATE-WORK.                            1003625
778990     MOVE C-LOW-DATE TO HOLD-65-YEARS.                            1003625
778991                                                                  1003625
778992 Y1140.                                                           1003625
778993     MOVE 'N' TO WS-LEAP-YR-SW.                                   1003625
778994     IF WBC-LEAP-YEAR EQUAL TO '1'                                0266741
778995         MOVE +366 TO SAVE-NO3                                    1003625
778996         MOVE 'Y' TO WS-LEAP-YR-SW                                1003625
778997     ELSE                                                         1003625
778998         MOVE +365 TO SAVE-NO3                                    1003625
778999*DETERMINE IF LAST YEAR WAS A LEAP YEAR.                          1003625
779000         MOVE '12'          TO DT-L-MO                            1003625
779001         MOVE '31'          TO DT-L-DA                            1003625
779002         MOVE WBC-CAPTURE-YR TO DT-L-YR                           0266741
779003         PERFORM Y1300 THRU Y1300-EXIT.                           9916150
779005     IF WBC-LOAN-360-DAY-FACTOR EQUAL TO '1'                      0627547
779006         MOVE +360 TO SAVE-NO3.                                   0627547
779007     IF WBC-LOAN-360-DAY-FACTOR EQUAL TO '2'                      0627547
779008         MOVE +365 TO SAVE-NO3.                                   0627547
779009     IF  WBC-PREV-YEAR-END EQUAL '1'                              0266741
779010     AND WBC-LOAN-360-DAY-FACTOR EQUAL '0'                        0266741
779011         NEXT SENTENCE                                            IM008
779012     ELSE                                                         IM008
779013         GO TO Y1150.                                             IM008
779014     MOVE WBC-LAST-CAPTURE-DATE TO DT-LOW-DATE.                   0266741
779015     MOVE '02' TO DT-L-MO.                                        IM008
779016     MOVE '29' TO DT-L-DA.                                        IM008
779017     CALL 'SIEDITDT' USING DATE-AREA.                             IM008
779018     IF  DATE-FLAG EQUAL '0'                                      IM008
779019         MOVE +366 TO SAVE-NO3.                                   IM008
779020                                                                  IM008
779021 Y1150.                                                           IM008
779022     MOVE 'R' TO I-O-CONTROL-OPERATOR.                            IM008
779023     CALL 'IMBCRM' USING I-O-CONTROL-AREA                         IM008
779024                         DDA-WRKBCR-5.                            0266741
779025     IF I-O-88-END-OF-FILE                                        IM008
779026         GO TO Z2000.                                             IM008
779027     IF WBC5-RECORD-ID EQUAL 'B5'                                 0266741
779028         NEXT SENTENCE                                            IM008
779029     ELSE                                                         IM008
779030         GO TO Y1150.                                             IM008
779031     MOVE 'R' TO I-O-CONTROL-OPERATOR.                            IM008
779032     CALL 'IMBCRM' USING I-O-CONTROL-AREA                         IM008
779033                         DDA-WRKBCR-6.                            0266741
779034     IF  I-O-88-END-OF-FILE                                       IM008
779035         GO TO Z2000.                                             IM008
779036     IF WBC6-RECORD-ID EQUAL 'B6'                                 0266741
779037         GO TO Y1160                                              0902970
779038     ELSE                                                         IM008
779039         GO TO Y1150.                                             IM008
779040 Y1160.                                                           0902970
779041     MOVE 'R' TO I-O-CONTROL-OPERATOR.                            0902970
779042     CALL 'IMBCRM' USING I-O-CONTROL-AREA                         0902970
779043                         DDA-WRKBCR-7.                            0266741
779044     IF I-O-88-END-OF-FILE                                        0902970
779045         GO TO Z2000.                                             0902970
779046     IF WBC7-RECORD-ID EQUAL 'B7'                                 0266741
779047         GO TO C1020                                              0902970
779048     ELSE                                                         0902970
779049         GO TO Y1160.                                             0902970
779060 Y1200.                                                           0902970
779062     IF  WBC-RW-FIELD-NBR (1) NOT EQUAL SPACES AND ZEROES         0817741
779064         MOVE 'Y' TO RW-SW.                                       0817741
779070     MOVE WBC-SHORT-NAME-PHASE TO WK-GEN-PHASE.                   0266741
779100     MOVE SPACES TO TODAYS-REPORT-TABLE
779150     MOVE +1 TO X.
779200     MOVE +25 TO Y.
779250 Y1220.
779300     IF  WBC-EXC-CODE (Y) EQUAL '1'                               0266741
779350         MOVE Y TO RPT9 (X)
779400         ADD +1 TO X.
779450     IF  Y GREATER THAN +98
779500         GO TO Y1100.
779550     ADD +1 TO Y.
779600     GO TO Y1220.
779650                                                                  IM008
779700 Y1300.                                                           9916150
779705     IF  WBC-CAPTURE-YR EQUAL '00'                                0266741
779710         MOVE '99' TO DT-L-YR                                     9916150
779715     ELSE                                                         9916150
779720         SUBTRACT 1 FROM DT-L-YR9.                                9916150
779725     CALL 'SIDJUL' USING DATE-AREA.                               9916150
779730     IF  DT-JUL-DA EQUAL +366                                     9916150
779735         MOVE 'Y' TO WS-LEAP-YR-SW.                               9916150
779740 Y1300-EXIT.                                                      9916150
779745     EXIT.                                                        9916150
779750                                                                  9916150
779755 Y1310.                                                           2016374
779760     INITIALIZE WORK-BRT-AREA                                     2016374
779765         REPLACING ALPHANUMERIC BY SPACES NUMERIC BY ZEROS.       2016374
779770     MOVE +0 TO BRT-CNT.                                          2016374
779775                                                                  2016374
779780 Y1310-CONT.                                                      2016374
779785     IF  BRT-EOF-FLAG EQUAL 'Y'                                   2016374
779790         GO TO Y1310-EXIT.                                        2016374
779795     MOVE 'R'          TO I-O-CONTROL-OPERATOR.                   2016374
779800     CALL 'IMBRTMS' USING I-O-CONTROL-AREA                        2016374
779805                          BONUS-RATE-AREA.                        2016374
779810     IF  I-O-88-END-OF-FILE                                       2016374
779815         MOVE 'Y' TO BRT-EOF-FLAG                                 2016374
779820         GO TO Y1310-EXIT.                                        2016374
779825     IF WBC1-CONTROL-KEY GREATER THAN IMBR-CONTROLS               0266741
779830         GO TO Y1310-CONT.                                        2016374
779835     IF WBC1-CONTROL-KEY LESS THAN IMBR-CONTROLS                  0266741
779840         INITIALIZE WORK-BRT-AREA                                 2016374
779845             REPLACING ALPHANUMERIC BY SPACES NUMERIC BY ZEROS.   2016374
779850 Y1310-EXIT.                                                      2016374
779855     EXIT.                                                        2016374
779860                                                                  2016374
779862 Y1350.                                                           0927817
779864*--------------------------------------------------------------*  0927817
779866*    OPEN EXTENDED FLOAT FILE                                  *  0927817
779868*--------------------------------------------------------------*  0927817
779870     IF  AFF-OPEN             EQUAL 'F'                           0927817
779872         MOVE 'I'                TO I-O-CONTROL-ACCESS            0927817
779874         MOVE 'O'                TO I-O-CONTROL-OPERATOR          0927817
779876         CALL 'IMAFFMS'       USING I-O-CONTROL-AREA              0927817
779878                                    EXTENDED-FLOAT-REC            0927817
779880         MOVE 'O'                TO I-O-CONTROL-ACCESS            0927817
779882         MOVE 'O'                TO I-O-CONTROL-OPERATOR          0927817
779884         CALL 'IMAFFMV'       USING I-O-CONTROL-AREA              0927817
779886                                    WFF-EXTENDED-FLOAT-REC        0927817
779888         MOVE 'N'                TO AFF-OPEN.                     0927817
779890                                                                  0927817
779892*--------------------------------------------------------------*  0927817
779894*    CREATE HEADER RECORD WHEN NONE EXISTS                     *  0927817
779896*--------------------------------------------------------------*  0927817
779898     IF  WBC-EXTEND-FLOAT-OPT EQUAL '2' OR '3'                    0927817
779900         MOVE SPACES             TO EXTENDED-FLOAT-REC            0927817
779902         MOVE LOW-VALUES         TO FF-BIN0                       0927817
779904         MOVE HOLD-CONTROL       TO FFH-KEY                       0927817
779906         MOVE 'H'                TO FFH-REC-TYPE                  0927817
779908         IF  MST-CONTROL      EQUAL HIGH-VALUES                   0927817
779910             MOVE 'E'            TO AFF-SKIP-READ.                0927817
779912*                                                                 0927817
779914     IF  WBC-EXTEND-FLOAT-OPT   NOT EQUAL '1'                     0927817
779916         GO TO Y1350-EXIT.                                        0927817
779918*                                                                 0927817
779920 Y1350-READ.                                                      0927817
779922*--------------------------------------------------------------*  0927817
779924*    READ FOR HEADER RECORD MATCHING THE BCR                   *  0927817
779926*--------------------------------------------------------------*  0927817
779928     IF  AFF-SKIP-READ        EQUAL '0'                           0927817
779930         MOVE 'I'                TO I-O-CONTROL-ACCESS            0927817
779932         MOVE 'R'                TO I-O-CONTROL-OPERATOR          0927817
779934         CALL 'IMAFFMS'       USING I-O-CONTROL-AREA              0927817
779936                                    EXTENDED-FLOAT-REC            0927817
779938         IF  I-O-88-END-OF-FILE                                   0927817
779940             MOVE HIGH-VALUES    TO FF-KEY                        0927817
779942             MOVE 'E'            TO AFF-SKIP-READ.                0927817
779944*                                                                 0927817
779946     IF  AFF-SKIP-READ        EQUAL '1'                           0927817
779948         MOVE 'I'                TO I-O-CONTROL-ACCESS            0927817
779950         MOVE 'N'                TO I-O-CONTROL-OPERATOR          0927817
779952         MOVE '0'                TO AFF-SKIP-READ                 0927817
779954         CALL 'IMAFFMS'       USING I-O-CONTROL-AREA              0927817
779956                                    EXTENDED-FLOAT-REC.           0927817
779958*                                                                 0927817
779960     IF  AFF-SKIP-READ        EQUAL 'E'                           0927817
779962         MOVE 501                TO SIMESS-MESS-NO                0927817
779964         MOVE WBC1-CONTROL-KEY   TO SIMESS-OPTIONAL-MESS18        0927817
779966         MOVE SIMESS-MESS18      TO SIMESS-OPTIONAL-MESSAGE       0927817
779968         CALL 'SIMESS'        USING SIMESS-AREA.                  0927817
779970*                                                                 0927817
779972     MOVE FF-CONTROLS            TO AFF-CTL1-3.                   0927817
779974     IF  AFF-CTL1-3    GREATER THAN WBC1-CONTROL-KEY              0927817
779976         MOVE 501                TO SIMESS-MESS-NO                0927817
779978         MOVE WBC1-CONTROL-KEY   TO SIMESS-OPTIONAL-MESS18        0927817
779980         MOVE SIMESS-MESS18      TO SIMESS-OPTIONAL-MESSAGE       0927817
779982         CALL 'SIMESS'        USING SIMESS-AREA.                  0927817
779984*                                                                 0927817
779986     IF  AFF-CTL1-3       LESS THAN WBC1-CONTROL-KEY              0927817
779988         GO TO Y1350-READ.                                        0927817
779990                                                                  0927817
779992     IF  FFH-REC-TYPE     NOT EQUAL 'H'                           0927817
779994         MOVE 501                TO SIMESS-MESS-NO                0927817
779996         MOVE WBC1-CONTROL-KEY   TO SIMESS-OPTIONAL-MESS18        0927817
779998         MOVE SIMESS-MESS18      TO SIMESS-OPTIONAL-MESSAGE       0927817
780000         CALL 'SIMESS'        USING SIMESS-AREA.                  0927817
780002*                                                                 0927817
780004 Y1350-EXIT.                                                      0927817
780006     EXIT.                                                        0927817
780500*                                                                 0927817
780650 Y1400.                                                           0902213
781650     IF  BAL-HIST-OPEN EQUAL 'F'                                  0902213
782600         MOVE 'U'             TO  I-O-CONTROL-ACCESS              9915848
783900         MOVE 'O'             TO  I-O-CONTROL-OPERATOR            9915845
784600         CALL 'IMBALM'  USING I-O-CONTROL-AREA                    9915848
785600                              BALANCE-HISTORY-REC                 9915848
785700                              BAL-HIST-LENGTHS                    9915848
785800                              SI-ENVIRONMENT-AREA                 9915848
785900                              DDA-WRKBCR-1                        0447266
790650         MOVE 'N'             TO  BAL-HIST-OPEN.                  0902213
790675     IF  WBC-BAL-HIST-FLAG EQUAL '2' OR '3'                       0266741
790725         MOVE SPACES          TO BALANCE-HISTORY-HEADER           0902213
790750         MOVE LOW-VALUES      TO BH-BIN0                          0902213
790775         MOVE HOLD-CONTROL    TO BHH-KEY                          0902213
790800         MOVE '0'             TO BHH-RECORD-ID                    0902213
790804         IF  MST-CONTROL EQUAL  HIGH-VALUES                       0902213
790808             MOVE 'E'         TO SKIP-READ-BAL.                   0903444
790820*--------------------------------------------------------------*  0902213
790840*          VERIFY THAT A BALANCE HISTORY HEADER EXISTS            0902213
790860*--------------------------------------------------------------*  0902213
790880     IF  WBC-BAL-HIST-FLAG EQUAL '1'                              0266741
790900         NEXT SENTENCE                                            0902213
790920     ELSE                                                         0902213
790940         GO TO Y1400-EXIT.                                        0902213
790950 Y1410.                                                           0903151
790960     IF  SKIP-READ-BAL EQUAL  '0'                                 0902213
790970         MOVE 'I'             TO  I-O-CONTROL-ACCESS              9915848
790980         MOVE 'R'             TO  I-O-CONTROL-OPERATOR            0902213
791000         CALL 'IMBALM'  USING I-O-CONTROL-AREA                    9915848
791010                              BALANCE-HISTORY-REC                 9915848
791020                              BAL-HIST-LENGTHS                    9915848
791030                              SI-ENVIRONMENT-AREA                 9915848
791035                              DDA-WRKBCR-1                        0447266
791040         IF  I-O-88-END-OF-FILE                                   0902213
791060             MOVE HIGH-VALUES TO BH-CONTROLS                      0902213
791080             MOVE 'E'         TO SKIP-READ-BAL.                   0902213
791100                                                                  0902213
791120     IF  SKIP-READ-BAL EQUAL '1'                                  0902213
791130         MOVE 'I'             TO  I-O-CONTROL-ACCESS              9915845
791140         MOVE 'N'             TO  I-O-CONTROL-OPERATOR            0902213
791160         MOVE '0'             TO  SKIP-READ-BAL                   0902213
791170         CALL 'IMBALM'  USING I-O-CONTROL-AREA                    9915848
791180                              BALANCE-HISTORY-REC                 9915848
791190                              BAL-HIST-LENGTHS                    9915848
791200                              SI-ENVIRONMENT-AREA                 0447266
791205                              DDA-WRKBCR-1.                       0447266
791210                                                                  9915845
791220     IF  SKIP-READ-BAL EQUAL 'E'                                  0902213
791240         MOVE 501             TO SIMESS-MESS-NO                   0902213
791260         MOVE WBC1-CONTROL-KEY TO SIMESS-OPTIONAL-MESS8           0266741
791280         MOVE SIMESS-MESS8    TO SIMESS-OPTIONAL-MESSAGE          0902213
791300         CALL 'SIMESS'     USING SIMESS-AREA.                     0902213
791320     IF  BHH-CTL1-3 GREATER THAN WBC1-CONTROL-KEY                 0266741
791340         MOVE 501             TO SIMESS-MESS-NO                   0902213
791360         MOVE WBC1-CONTROL-KEY TO SIMESS-OPTIONAL-MESS8           0266741
791380         MOVE SIMESS-MESS8    TO SIMESS-OPTIONAL-MESSAGE          0902213
791400         CALL 'SIMESS'     USING SIMESS-AREA.                     0902213
791420     IF  BHH-CTL1-3 LESS THAN WBC1-CONTROL-KEY                    0266741
791440* READ UNTIL A BALANCE HISTORY RECORD GREATER THAN OR EQUAL TO    0903151
791460* THE BCR IS FOUND.                                               0903151
791500         GO TO Y1410.                                             0903151
791520     IF  BHH-RECORD-ID NOT EQUAL '0'                              0902213
791540         MOVE 501                 TO SIMESS-MESS-NO               0902213
791560         MOVE WBC1-CONTROL-KEY     TO SIMESS-OPTIONAL-MESS8       0266741
791580         MOVE SIMESS-MESS8        TO SIMESS-OPTIONAL-MESSAGE      0902213
791600         CALL 'SIMESS'         USING SIMESS-AREA.                 0902213
791650 Y1400-EXIT.  EXIT.                                               0902213
791655                                                                  1003625
791660 Y1500-CALC-RATE-WORK-DAF.                                        1003625
791665 Y1500-DDA.                                                       1003625
791690     MOVE +360 TO DAF-DAYS.                                       0817657
791700     MOVE +0 TO WORK-SUB.                                         1003625
791705 Y1500-DDA-LOOP.                                                  1003625
791710     ADD +1 TO WORK-SUB.                                          1003625
791715     IF  WS-RWF-DDA-KEY (WORK-SUB) EQUAL HIGH-VALUES              1003625
791720     OR  WORK-SUB GREATER THAN +1000                              2016664
791725         GO TO Y1500-DDA-LOOP-EXIT.                               9715519
791730     DIVIDE DAF-DAYS INTO WS-RWF-DDA-ANN (WORK-SUB)               1003625
791735                   GIVING WS-RWF-DDA-DAF (WORK-SUB).              1003625
791736     IF  WS-RWF-DDA-CHG (WORK-SUB) EQUAL '1' OR '2'               1105510
791737         MOVE WS-RWF-DDA-DATE (WORK-SUB) TO C-LOW-DATE            1105510
791738         CALL 'SIDIFC1'               USING C-DATE-WORK           1105510
791739         MOVE C-NO-DAYS TO WS-RWF-DDA-PRV-DAYS (WORK-SUB).        1105510
791740     IF  WS-RWF-DDA-DEL-DATE (WORK-SUB) EQUAL ZEROES OR SPACES    1003625
791742     OR  HIGH-VALUES                                              1003625
791743         MOVE ZEROES TO WS-RWF-DDA-DEL-DAF (WORK-SUB)             1003625
791745         GO TO Y1500-DDA-LOOP.                                    1003625
791750     DIVIDE DAF-DAYS INTO WS-RWF-DDA-DEL-ANN (WORK-SUB)           1003625
791755                   GIVING WS-RWF-DDA-DEL-DAF (WORK-SUB).          1003625
791756     IF  WS-RWF-DDA-CHG (WORK-SUB) EQUAL '4'                      1105510
791757         MOVE WS-RWF-DDA-DATE (WORK-SUB) TO C-LOW-DATE            1105510
791758         CALL 'SIDIFC1'               USING C-DATE-WORK           1105510
791759         MOVE C-NO-DAYS TO WS-RWF-DDA-PRV-DAYS (WORK-SUB)         1105510
791760         MOVE WS-RWF-DDA-DEL-DATE (WORK-SUB) TO C-LOW-DATE        1105510
791761         CALL 'SIDIFC1'                   USING C-DATE-WORK       1105510
791762         MOVE C-NO-DAYS TO WS-RWF-DDA-DEL-DAYS (WORK-SUB)         1105510
791763         IF  WS-RWF-DDA-DEL-DATE (WORK-SUB) NOT LESS THAN         1105510
791764             WS-RWF-DDA-DATE (WORK-SUB)                           1105510
791765             MOVE '5' TO WS-RWF-DDA-INT-ADJ (WORK-SUB)            1105510
791766         ELSE                                                     1105510
791767             MOVE '6' TO WS-RWF-DDA-INT-ADJ (WORK-SUB).           1105510
791768     GO TO Y1500-DDA-LOOP.                                        1105510
791769                                                                  1105510
791770 Y1500-DDA-LOOP-EXIT.                                             9715519
791771     EXIT.                                                        9715519
791772                                                                  9715519
791773 Y1500-DDA-RATE-CHECK.                                            0817657
791774     MOVE WS-RWF-DDA-TABLE TO WS-RWF-SAV-TABLE.                   9715519
791775     MOVE WS-RWF-DDA-TABLE TO WS-360-DDA-TABLE                    0817657
791776                              WS-360-SAV-TABLE.                   0817657
791777     MOVE +365 TO DAF-DAYS.                                       0817657
791778     MOVE +0 TO WORK-SUB.                                         0817657
791779     PERFORM Y1500-DDA-LOOP THRU Y1500-DDA-LOOP-EXIT.             0817657
791780     MOVE WS-RWF-DDA-TABLE TO WS-365-DDA-TABLE                    0817657
791781                              WS-365-SAV-TABLE.                   0817657
791782     MOVE +366 TO DAF-DAYS.                                       0817657
791783     MOVE +0 TO WORK-SUB.                                         0817657
791784     PERFORM Y1500-DDA-LOOP THRU Y1500-DDA-LOOP-EXIT.             0817657
791785     MOVE WS-RWF-DDA-TABLE TO WS-366-DDA-TABLE                    0817657
791786                              WS-366-SAV-TABLE.                   0817657
791787                                                                  0817657
791788 Y1500-LN.                                                        0817657
791800     MOVE +360 TO DAF-DAYS.                                       0817657
791810     MOVE +0 TO WORK-SUB.                                         1003625
791815 Y1500-LN-LOOP.                                                   1003625
791820     ADD +1 TO WORK-SUB.                                          1003625
791825     IF  WS-RWF-LN-KEY (WORK-SUB) EQUAL HIGH-VALUES               1003625
791830     OR  WORK-SUB GREATER THAN +1000                              2016664
791835         GO TO Y1500-LN-LOOP-EXIT.                                0817657
791840     DIVIDE DAF-DAYS INTO WS-RWF-LN-ANN (WORK-SUB)                1003625
791845                   GIVING WS-RWF-LN-DAF (WORK-SUB).               1003625
791846     IF  WS-RWF-LN-CHG (WORK-SUB) EQUAL '1' OR '2'                1105510
791847         MOVE WS-RWF-LN-DATE (WORK-SUB) TO C-LOW-DATE             1105510
791848         CALL 'SIDIFC1'              USING C-DATE-WORK            1105510
791849         MOVE C-NO-DAYS TO WS-RWF-LN-PRV-DAYS (WORK-SUB).         1105510
791850     IF  WS-RWF-LN-DEL-DATE (WORK-SUB) EQUAL ZEROES OR SPACES     1003625
791852     OR  HIGH-VALUES                                              1003625
791853         MOVE ZEROES TO WS-RWF-LN-DEL-DAF (WORK-SUB)              1003625
791855         GO TO Y1500-LN-LOOP.                                     1003625
791860     DIVIDE DAF-DAYS INTO WS-RWF-LN-DEL-ANN (WORK-SUB)            1003625
791865                   GIVING WS-RWF-LN-DEL-DAF (WORK-SUB).           1003625
791866     IF  WS-RWF-LN-CHG (WORK-SUB) EQUAL '4'                       1105510
791867         MOVE WS-RWF-LN-DATE (WORK-SUB) TO C-LOW-DATE             1105510
791868         CALL 'SIDIFC1'              USING C-DATE-WORK            1105510
791869         MOVE C-NO-DAYS TO WS-RWF-LN-PRV-DAYS (WORK-SUB)          1105510
791870         MOVE WS-RWF-LN-DEL-DATE (WORK-SUB) TO C-LOW-DATE         1105510
791871         CALL 'SIDIFC1'                  USING C-DATE-WORK        1105510
791872         MOVE C-NO-DAYS TO WS-RWF-LN-DEL-DAYS (WORK-SUB)          1105510
791873         IF  WS-RWF-LN-DEL-DATE (WORK-SUB) NOT LESS THAN          1105510
791874             WS-RWF-LN-DATE (WORK-SUB)                            1105510
791875             MOVE '5' TO WS-RWF-LN-INT-ADJ (WORK-SUB)             1105510
791876         ELSE                                                     1105510
791877             MOVE '6' TO WS-RWF-LN-INT-ADJ (WORK-SUB).            1105510
791878     GO TO Y1500-LN-LOOP.                                         1105510
791879                                                                  1105510
791880 Y1500-LN-LOOP-EXIT.                                              0817657
791881     EXIT.                                                        0817657
791882                                                                  0817657
791883 Y1500-LN-RATE-CHECK.                                             0817657
791884     MOVE WS-RWF-LOAN-TABLE TO WS-360-LOAN-TABLE.                 0817657
791885     MOVE +365 TO DAF-DAYS.                                       0817657
791886     MOVE +0 TO WORK-SUB.                                         0817657
791887     PERFORM Y1500-LN-LOOP THRU Y1500-LN-LOOP-EXIT.               0817657
791888     MOVE WS-RWF-LOAN-TABLE TO WS-365-LOAN-TABLE.                 0817657
791889     MOVE +366 TO DAF-DAYS.                                       0817657
791890     MOVE +0 TO WORK-SUB.                                         0817657
791891     PERFORM Y1500-LN-LOOP THRU Y1500-LN-LOOP-EXIT.               0817657
791892     MOVE WS-RWF-LOAN-TABLE TO WS-366-LOAN-TABLE.                 0817657
791893                                                                  0817657
791895     MOVE +360 TO DAF-DAYS.                                       0817657
791896     MOVE +0 TO WORK-SUB.                                         0817657
791897 Y1500-OD-LOOP.                                                   0817657
791898     ADD +1 TO WORK-SUB.                                          0817657
791900     IF  WS-RWF-OD-KEY (WORK-SUB) EQUAL HIGH-VALUES               1003625
791905     OR  WORK-SUB GREATER THAN +1000                              2016664
791910         GO TO Y1500-OD-LOOP-EXIT.                                0817657
791915     DIVIDE DAF-DAYS INTO WS-RWF-OD-ANN (WORK-SUB)                1003625
791920                   GIVING WS-RWF-OD-DAF (WORK-SUB).               1003625
791921     IF  WS-RWF-OD-CHG (WORK-SUB) EQUAL '1' OR '2'                1105510
791922         MOVE WS-RWF-OD-DATE (WORK-SUB) TO C-LOW-DATE             1105510
791923         CALL 'SIDIFC1'              USING C-DATE-WORK            1105510
791924         MOVE C-NO-DAYS TO WS-RWF-OD-PRV-DAYS (WORK-SUB).         1105510
791925     IF  WS-RWF-OD-DEL-DATE (WORK-SUB) EQUAL ZEROES OR SPACES     1003625
791927     OR  HIGH-VALUES                                              1003625
791928         MOVE ZEROES TO WS-RWF-OD-DEL-DAF (WORK-SUB)              1003625
791930         GO TO Y1500-OD-LOOP.                                     1003625
791935     DIVIDE DAF-DAYS INTO WS-RWF-OD-DEL-ANN (WORK-SUB)            1003625
791940                   GIVING WS-RWF-OD-DEL-DAF (WORK-SUB).           1003625
791941     IF  WS-RWF-OD-CHG (WORK-SUB) EQUAL '4'                       1105510
791942         MOVE WS-RWF-OD-DATE (WORK-SUB) TO C-LOW-DATE             1105510
791943         CALL 'SIDIFC1'              USING C-DATE-WORK            1105510
791944         MOVE C-NO-DAYS TO WS-RWF-OD-PRV-DAYS (WORK-SUB)          1105510
791945         MOVE WS-RWF-OD-DEL-DATE (WORK-SUB) TO C-LOW-DATE         1105510
791946         CALL 'SIDIFC1'                  USING C-DATE-WORK        1105510
791947         MOVE C-NO-DAYS TO WS-RWF-OD-DEL-DAYS (WORK-SUB)          1105510
791948         IF  WS-RWF-OD-DEL-DATE (WORK-SUB) NOT LESS THAN          1105510
791949             WS-RWF-OD-DATE (WORK-SUB)                            1105510
791950             MOVE '5' TO WS-RWF-OD-INT-ADJ (WORK-SUB)             1105510
791951         ELSE                                                     1105510
791952             MOVE '6' TO WS-RWF-OD-INT-ADJ (WORK-SUB).            1105510
791953     GO TO Y1500-OD-LOOP.                                         1105510
791954                                                                  1105510
791955 Y1500-OD-LOOP-EXIT.                                              0817657
791956     EXIT.                                                        0817657
791957                                                                  0817657
791958 Y1500-OD-RATE-CHECK.                                             0817657
791959     MOVE WS-RWF-OD-TABLE TO WS-360-OD-TABLE.                     0817657
791960     MOVE +365 TO DAF-DAYS.                                       0817657
791961     MOVE +0 TO WORK-SUB.                                         0817657
791962     PERFORM Y1500-OD-LOOP THRU Y1500-OD-LOOP-EXIT.               0817657
791963     MOVE WS-RWF-OD-TABLE TO WS-365-OD-TABLE.                     0817657
791964     MOVE +366 TO DAF-DAYS.                                       0817657
791965     MOVE +0 TO WORK-SUB.                                         0817657
791966     PERFORM Y1500-OD-LOOP THRU Y1500-OD-LOOP-EXIT.               0817657
791967     MOVE WS-RWF-OD-TABLE TO WS-366-OD-TABLE.                     0817657
791968                                                                  0817657
791969 Y1500-PRM.                                                       0817657
791970     MOVE +360 TO DAF-DAYS.                                       0817657
791971     MOVE +0 TO WORK-SUB.                                         0817657
791972 Y1500-PRM-LOOP.                                                  0817657
791973     ADD +1 TO WORK-SUB.                                          0817657
791975     IF  WS-RWF-PRM-KEY (WORK-SUB) EQUAL HIGH-VALUES              1003625
791980     OR  WORK-SUB GREATER THAN +1000                              2016664
791985         GO TO Y1500-PRM-LOOP-EXIT.                               0817657
791990     DIVIDE DAF-DAYS INTO WS-RWF-PRM-ANN (WORK-SUB)               1003625
791995                   GIVING WS-RWF-PRM-DAF (WORK-SUB).              1003625
791996     IF  WS-RWF-PRM-CHG (WORK-SUB) EQUAL '1' OR '2'               1105510
791997         MOVE WS-RWF-PRM-DATE (WORK-SUB) TO C-LOW-DATE            1105510
791998         CALL 'SIDIFC1'               USING C-DATE-WORK           1105510
791999         MOVE C-NO-DAYS TO WS-RWF-PRM-PRV-DAYS (WORK-SUB).        1105510
792000     IF  WS-RWF-PRM-DEL-DATE (WORK-SUB) EQUAL ZEROES OR SPACES    1003625
792002     OR  HIGH-VALUES                                              1003625
792003         MOVE ZEROES TO WS-RWF-PRM-DEL-DAF (WORK-SUB)             1003625
792005         GO TO Y1500-PRM-LOOP.                                    1003625
792010     DIVIDE DAF-DAYS INTO WS-RWF-PRM-DEL-ANN (WORK-SUB)           1003625
792015                   GIVING WS-RWF-PRM-DEL-DAF (WORK-SUB).          1003625
792016     IF  WS-RWF-PRM-CHG (WORK-SUB) EQUAL '4'                      1105510
792017         MOVE WS-RWF-PRM-DATE (WORK-SUB) TO C-LOW-DATE            1105510
792018         CALL 'SIDIFC1'               USING C-DATE-WORK           1105510
792019         MOVE C-NO-DAYS TO WS-RWF-PRM-PRV-DAYS (WORK-SUB)         1105510
792020         MOVE WS-RWF-PRM-DEL-DATE (WORK-SUB) TO C-LOW-DATE        1105510
792021         CALL 'SIDIFC1'                   USING C-DATE-WORK       1105510
792022         MOVE C-NO-DAYS TO WS-RWF-PRM-DEL-DAYS (WORK-SUB)         1105510
792023         IF  WS-RWF-PRM-DEL-DATE (WORK-SUB) NOT LESS THAN         1105510
792024             WS-RWF-PRM-DATE (WORK-SUB)                           1105510
792025             MOVE '5' TO WS-RWF-PRM-INT-ADJ (WORK-SUB)            1105510
792026         ELSE                                                     1105510
792027             MOVE '6' TO WS-RWF-PRM-INT-ADJ (WORK-SUB).           1105510
792028     GO TO Y1500-PRM-LOOP.                                        1105510
792029                                                                  1105510
792030 Y1500-PRM-LOOP-EXIT.                                             0817657
792031     EXIT.                                                        0817657
792032                                                                  0817657
792033 Y1500-PRM-RATE-CHECK.                                            0817657
792034     MOVE WS-RWF-PRIME-TABLE TO WS-360-PRIME-TABLE.               0817657
792035     MOVE +365 TO DAF-DAYS.                                       0817657
792036     MOVE +0 TO WORK-SUB.                                         0817657
792037     PERFORM Y1500-PRM-LOOP THRU Y1500-PRM-LOOP-EXIT.             0817657
792038     MOVE WS-RWF-PRIME-TABLE TO WS-365-PRIME-TABLE.               0817657
792039     MOVE +366 TO DAF-DAYS.                                       0817657
792040     MOVE +0 TO WORK-SUB.                                         0817657
792041     PERFORM Y1500-PRM-LOOP THRU Y1500-PRM-LOOP-EXIT.             0817657
792042     MOVE WS-RWF-PRIME-TABLE TO WS-366-PRIME-TABLE.               0817657
792043                                                                  0817657
792044 Y1500-SPL.                                                       0817657
792045     MOVE +360 TO DAF-DAYS.                                       0817657
792046     MOVE +0 TO WORK-SUB.                                         0817657
792047 Y1500-SPL-LOOP.                                                  0817657
792048     ADD +1 TO WORK-SUB.                                          0817657
792050     IF  WS-RWF-SPL-KEY (WORK-SUB) EQUAL HIGH-VALUES              1003625
792055     OR  WORK-SUB GREATER THAN +1000                              2016664
792060         GO TO Y1500-SPL-LOOP-EXIT.                               0817657
792065     DIVIDE DAF-DAYS INTO WS-RWF-SPL-ANN (WORK-SUB 1)             1003625
792070                   GIVING WS-RWF-SPL-DAF (WORK-SUB 1).            1003625
792075     DIVIDE DAF-DAYS INTO WS-RWF-SPL-ANN (WORK-SUB 2)             1003625
792080                   GIVING WS-RWF-SPL-DAF (WORK-SUB 2).            1003625
792085     DIVIDE DAF-DAYS INTO WS-RWF-SPL-ANN (WORK-SUB 3)             1003625
792090                   GIVING WS-RWF-SPL-DAF (WORK-SUB 3).            1003625
792091     IF  WS-RWF-SPL-CHG (WORK-SUB) EQUAL '1' OR '2'               1105510
792092         MOVE WS-RWF-SPL-DATE (WORK-SUB) TO C-LOW-DATE            1105510
792093         CALL 'SIDIFC1'               USING C-DATE-WORK           1105510
792094         MOVE C-NO-DAYS TO WS-RWF-SPL-PRV-DAYS (WORK-SUB).        1105510
792095     IF  WS-RWF-SPL-DEL-DATE (WORK-SUB) EQUAL ZEROES OR SPACES    1003625
792097     OR  HIGH-VALUES                                              1003625
792098         MOVE ZEROES TO WS-RWF-SPL-DEL-DAF (WORK-SUB 1)           1003625
792099         MOVE ZEROES TO WS-RWF-SPL-DEL-DAF (WORK-SUB 2)           1003625
792100         MOVE ZEROES TO WS-RWF-SPL-DEL-DAF (WORK-SUB 3)           1003625
792101         GO TO Y1500-SPL-LOOP.                                    1003625
792105     DIVIDE DAF-DAYS INTO WS-RWF-SPL-DEL-ANN (WORK-SUB 1)         1003625
792110                   GIVING WS-RWF-SPL-DEL-DAF (WORK-SUB 1).        1003625
792115     DIVIDE DAF-DAYS INTO WS-RWF-SPL-DEL-ANN (WORK-SUB 2)         1003625
792120                   GIVING WS-RWF-SPL-DEL-DAF (WORK-SUB 2).        1003625
792125     DIVIDE DAF-DAYS INTO WS-RWF-SPL-DEL-ANN (WORK-SUB 3)         1003625
792130                   GIVING WS-RWF-SPL-DEL-DAF (WORK-SUB 3).        1003625
792131     IF  WS-RWF-SPL-CHG (WORK-SUB) EQUAL '4'                      1105510
792132         MOVE WS-RWF-SPL-DATE (WORK-SUB) TO C-LOW-DATE            1105510
792133         CALL 'SIDIFC1'               USING C-DATE-WORK           1105510
792134         MOVE C-NO-DAYS TO WS-RWF-SPL-PRV-DAYS (WORK-SUB)         1105510
792135         MOVE WS-RWF-SPL-DEL-DATE (WORK-SUB) TO C-LOW-DATE        1105510
792136         CALL 'SIDIFC1'                   USING C-DATE-WORK       1105510
792137         MOVE C-NO-DAYS TO WS-RWF-SPL-DEL-DAYS (WORK-SUB)         1105510
792138         IF  WS-RWF-SPL-DEL-DATE (WORK-SUB) NOT LESS THAN         1105510
792139             WS-RWF-SPL-DATE (WORK-SUB)                           1105510
792140             MOVE '5' TO WS-RWF-SPL-INT-ADJ (WORK-SUB)            1105510
792141         ELSE                                                     1105510
792142             MOVE '6' TO WS-RWF-SPL-INT-ADJ (WORK-SUB).           1105510
792143     GO TO Y1500-SPL-LOOP.                                        1105510
792144                                                                  1105510
792145 Y1500-SPL-LOOP-EXIT.                                             0817657
792146     EXIT.                                                        0817657
792147                                                                  0817657
792148 Y1500-SPL-RATE-CHECK.                                            0817657
792149     MOVE WS-RWF-SPLIT-TABLE TO WS-360-SPLIT-TABLE.               0817657
792150     MOVE +365 TO DAF-DAYS.                                       0817657
792151     MOVE +0 TO WORK-SUB.                                         0817657
792152     PERFORM Y1500-SPL-LOOP THRU Y1500-SPL-LOOP-EXIT.             0817657
792153     MOVE WS-RWF-SPLIT-TABLE TO WS-365-SPLIT-TABLE.               0817657
792154     MOVE +366 TO DAF-DAYS.                                       0817657
792155     MOVE +0 TO WORK-SUB.                                         0817657
792156     PERFORM Y1500-SPL-LOOP THRU Y1500-SPL-LOOP-EXIT.             0817657
792157     MOVE WS-RWF-SPLIT-TABLE TO WS-366-SPLIT-TABLE.               0817657
792158                                                                  0817657
792159 Y1500-TIER.                                                      0817657
792160     MOVE +0 TO WORK-SUB.                                         0817657
792162     MOVE +360 TO DAF-DAYS.                                       0817657
792175 Y1500-TIER-LOOP.                                                 1003625
792180     ADD +1 TO WORK-SUB.                                          1003625
792185     IF  WS-RWF-TIER-KEY (WORK-SUB) EQUAL HIGH-VALUES             1003625
792190     OR  WORK-SUB GREATER THAN +800                               2016664
792195         GO TO Y1500-TIER-LOOP-EXIT.                              9715519
792200     DIVIDE DAF-DAYS INTO WS-RWF-TIER-ANN (WORK-SUB 1)            1003625
792205                   GIVING WS-RWF-TIER-DAF (WORK-SUB 1).           1003625
792210     DIVIDE DAF-DAYS INTO WS-RWF-TIER-ANN (WORK-SUB 2)            1003625
792215                   GIVING WS-RWF-TIER-DAF (WORK-SUB 2).           1003625
792220     DIVIDE DAF-DAYS INTO WS-RWF-TIER-ANN (WORK-SUB 3)            1003625
792225                   GIVING WS-RWF-TIER-DAF (WORK-SUB 3).           1003625
792230     DIVIDE DAF-DAYS INTO WS-RWF-TIER-ANN (WORK-SUB 4)            1003625
792235                   GIVING WS-RWF-TIER-DAF (WORK-SUB 4).           1003625
792240     DIVIDE DAF-DAYS INTO WS-RWF-TIER-ANN (WORK-SUB 5)            1003625
792245                   GIVING WS-RWF-TIER-DAF (WORK-SUB 5).           1003625
792250     DIVIDE DAF-DAYS INTO WS-RWF-TIER-ANN (WORK-SUB 6)            1003625
792255                   GIVING WS-RWF-TIER-DAF (WORK-SUB 6).           1003625
792260     DIVIDE DAF-DAYS INTO WS-RWF-TIER-ANN (WORK-SUB 7)            1003625
792265                   GIVING WS-RWF-TIER-DAF (WORK-SUB 7).           1003625
792270     DIVIDE DAF-DAYS INTO WS-RWF-TIER-ANN (WORK-SUB 8)            1003625
792275                   GIVING WS-RWF-TIER-DAF (WORK-SUB 8).           1003625
792280     DIVIDE DAF-DAYS INTO WS-RWF-TIER-ANN (WORK-SUB 9)            1003625
792285                   GIVING WS-RWF-TIER-DAF (WORK-SUB 9).           1003625
792286     IF  WS-RWF-TIER-CHG (WORK-SUB) EQUAL '1' OR '2'              1105510
792287         MOVE WS-RWF-TIER-DATE (WORK-SUB) TO C-LOW-DATE           1105510
792288         CALL 'SIDIFC1'                USING C-DATE-WORK          1105510
792289         MOVE C-NO-DAYS TO WS-RWF-TIER-PRV-DAYS (WORK-SUB).       1105510
792290     IF  WS-RWF-TIER-DEL-DATE (WORK-SUB) EQUAL ZEROES OR SPACES   1003625
792292     OR  HIGH-VALUES                                              1003625
792293         MOVE ZEROES TO WS-RWF-TIER-DEL-DAF (WORK-SUB 1)          1003625
792294                        WS-RWF-TIER-DEL-DAF (WORK-SUB 2)          1003625
792295                        WS-RWF-TIER-DEL-DAF (WORK-SUB 3)          1003625
792296                        WS-RWF-TIER-DEL-DAF (WORK-SUB 4)          1003625
792297                        WS-RWF-TIER-DEL-DAF (WORK-SUB 5)          1003625
792298                        WS-RWF-TIER-DEL-DAF (WORK-SUB 6)          1003625
792299                        WS-RWF-TIER-DEL-DAF (WORK-SUB 7)          1003625
792300                        WS-RWF-TIER-DEL-DAF (WORK-SUB 8)          1003625
792301                        WS-RWF-TIER-DEL-DAF (WORK-SUB 9)          1003625
792302         GO TO Y1500-TIER-LOOP.                                   1003625
792303     DIVIDE DAF-DAYS INTO WS-RWF-TIER-DEL-ANN (WORK-SUB 1)        1003625
792305                   GIVING WS-RWF-TIER-DEL-DAF (WORK-SUB 1).       1003625
792310     DIVIDE DAF-DAYS INTO WS-RWF-TIER-DEL-ANN (WORK-SUB 2)        1003625
792315                   GIVING WS-RWF-TIER-DEL-DAF (WORK-SUB 2).       1003625
792320     DIVIDE DAF-DAYS INTO WS-RWF-TIER-DEL-ANN (WORK-SUB 3)        1003625
792325                   GIVING WS-RWF-TIER-DEL-DAF (WORK-SUB 3).       1003625
792330     DIVIDE DAF-DAYS INTO WS-RWF-TIER-DEL-ANN (WORK-SUB 4)        1003625
792335                   GIVING WS-RWF-TIER-DEL-DAF (WORK-SUB 4).       1003625
792340     DIVIDE DAF-DAYS INTO WS-RWF-TIER-DEL-ANN (WORK-SUB 5)        1003625
792345                   GIVING WS-RWF-TIER-DEL-DAF (WORK-SUB 5).       1003625
792350     DIVIDE DAF-DAYS INTO WS-RWF-TIER-DEL-ANN (WORK-SUB 6)        1003625
792355                   GIVING WS-RWF-TIER-DEL-DAF (WORK-SUB 6).       1003625
792360     DIVIDE DAF-DAYS INTO WS-RWF-TIER-DEL-ANN (WORK-SUB 7)        1003625
792365                   GIVING WS-RWF-TIER-DEL-DAF (WORK-SUB 7).       1003625
792370     DIVIDE DAF-DAYS INTO WS-RWF-TIER-DEL-ANN (WORK-SUB 8)        1003625
792375                   GIVING WS-RWF-TIER-DEL-DAF (WORK-SUB 8).       1003625
792380     DIVIDE DAF-DAYS INTO WS-RWF-TIER-DEL-ANN (WORK-SUB 9)        1003625
792385                   GIVING WS-RWF-TIER-DEL-DAF (WORK-SUB 9).       1003625
792386     IF  WS-RWF-TIER-CHG (WORK-SUB) EQUAL '4'                     1105510
792387         MOVE WS-RWF-TIER-DATE (WORK-SUB) TO C-LOW-DATE           1105510
792388         CALL 'SIDIFC1'                USING C-DATE-WORK          1105510
792389         MOVE C-NO-DAYS TO WS-RWF-TIER-PRV-DAYS (WORK-SUB)        1105510
792390         MOVE WS-RWF-TIER-DEL-DATE (WORK-SUB) TO C-LOW-DATE       1105510
792391         CALL 'SIDIFC1'                    USING C-DATE-WORK      1105510
792392         MOVE C-NO-DAYS TO WS-RWF-TIER-DEL-DAYS (WORK-SUB)        1105510
792393         IF  WS-RWF-TIER-DEL-DATE (WORK-SUB) NOT LESS THAN        1105510
792394             WS-RWF-TIER-DATE (WORK-SUB)                          1105510
792395             MOVE '5' TO WS-RWF-TIER-INT-ADJ (WORK-SUB)           1105510
792396         ELSE                                                     1105510
792397             MOVE '6' TO WS-RWF-TIER-INT-ADJ (WORK-SUB).          1105510
792398     GO TO Y1500-TIER-LOOP.                                       1105510
792399                                                                  9715519
792400 Y1500-TIER-LOOP-EXIT.                                            9715519
792401     EXIT.                                                        9715519
792402                                                                  9715519
792403 Y1500-TIER-RATE-CHECK.                                           0817657
792404     MOVE WS-RWF-TIER-TABLE TO WS-RWF-SAVT-TABLE.                 0316967
792405     MOVE WS-RWF-TIER-TABLE TO WS-360-TIER-TABLE                  0817657
792406                               WS-360-SAVT-TABLE.                 0817657
792407     MOVE +365 TO DAF-DAYS.                                       0817657
792408     MOVE +0 TO WORK-SUB.                                         0817657
792409     PERFORM Y1500-TIER-LOOP THRU Y1500-TIER-LOOP-EXIT.           0817657
792410     MOVE WS-RWF-TIER-TABLE TO WS-365-TIER-TABLE                  0817657
792411                               WS-365-SAVT-TABLE.                 0817657
792412     MOVE +366 TO DAF-DAYS.                                       0817657
792413     MOVE +0 TO WORK-SUB.                                         0817657
792414     PERFORM Y1500-TIER-LOOP THRU Y1500-TIER-LOOP-EXIT.           0817657
792415     MOVE WS-RWF-TIER-TABLE TO WS-366-TIER-TABLE                  0817657
792416                               WS-366-SAVT-TABLE.                 0817657
792420 Y1500-END.                                                       9715519
792425     EXIT.                                                        9715519
792430*                                                                 9715519
792600 YE-SPLIT-RATE-MOVE.                                              1003625
792602     MOVE YE-SPL-CUR-ANN (1) TO WMS-SPL-CUR-ANN (1).              1003625
792604     MOVE YE-SPL-CUR-DAF (1) TO WMS-SPL-CUR-DAF (1).              1003625
792606     MOVE YE-SPL-CUR-ANN (2) TO WMS-SPL-CUR-ANN (2).              1003625
792608     MOVE YE-SPL-CUR-DAF (2) TO WMS-SPL-CUR-DAF (2).              1003625
792610     MOVE YE-SPL-CUR-ANN (3) TO WMS-SPL-CUR-ANN (3).              1003625
792612     MOVE YE-SPL-CUR-DAF (3) TO WMS-SPL-CUR-DAF (3).              1003625
792614     MOVE YE-SPL-CUR-LMT (1) TO WMS-SPL-CUR-LMT (1).              1003625
792616     MOVE YE-SPL-CUR-LMT (2) TO WMS-SPL-CUR-LMT (2).              1003625
792618 YE-SPLIT-RATE-MOVE-EXIT.                                         1003625
792620     EXIT.                                                        1003625
792622                                                                  1003625
792624 SPLIT-RATE-MOVE-YE.                                              1003625
792626     MOVE WMS-SPL-CUR-ANN (1) TO YE-SPL-CUR-ANN (1).              1003625
792628     MOVE WMS-SPL-CUR-DAF (1) TO YE-SPL-CUR-DAF (1).              1003625
792630     MOVE WMS-SPL-CUR-ANN (2) TO YE-SPL-CUR-ANN (2).              1003625
792632     MOVE WMS-SPL-CUR-DAF (2) TO YE-SPL-CUR-DAF (2).              1003625
792634     MOVE WMS-SPL-CUR-ANN (3) TO YE-SPL-CUR-ANN (3).              1003625
792636     MOVE WMS-SPL-CUR-DAF (3) TO YE-SPL-CUR-DAF (3).              1003625
792638     MOVE WMS-SPL-CUR-LMT (1) TO YE-SPL-CUR-LMT (1).              1003625
792640     MOVE WMS-SPL-CUR-LMT (2) TO YE-SPL-CUR-LMT (2).              1003625
792642 SPLIT-RATE-MOVE-YE-EXIT.                                         1003625
792644     EXIT.                                                        1003625
792646                                                                  1003625
792648 YE-OLD-SPLIT-RATE-MOVE.                                          1003625
792650     MOVE YE-SPL-CUR-ANN (1) TO WMS-SPL-OLD-CUR-ANN (1).          1003625
792652     MOVE YE-SPL-CUR-DAF (1) TO WMS-SPL-OLD-CUR-DAF (1).          1003625
792654     MOVE YE-SPL-CUR-ANN (2) TO WMS-SPL-OLD-CUR-ANN (2).          1003625
792656     MOVE YE-SPL-CUR-DAF (2) TO WMS-SPL-OLD-CUR-DAF (2).          1003625
792658     MOVE YE-SPL-CUR-ANN (3) TO WMS-SPL-OLD-CUR-ANN (3).          1003625
792660     MOVE YE-SPL-CUR-DAF (3) TO WMS-SPL-OLD-CUR-DAF (3).          1003625
792662     MOVE YE-SPL-CUR-LMT (1) TO WMS-SPL-OLD-CUR-LMT (1).          1003625
792664     MOVE YE-SPL-CUR-LMT (2) TO WMS-SPL-OLD-CUR-LMT (2).          1003625
792666 YE-OLD-SPLIT-RATE-MOVE-EXIT.                                     1003625
792668     EXIT.                                                        1003625
792670                                                                  1003625
792672 OLD-SPLIT-RATE-MOVE-YE.                                          1003625
792674     MOVE WMS-SPL-OLD-CUR-ANN (1) TO YE-SPL-CUR-ANN (1).          1003625
792676     MOVE WMS-SPL-OLD-CUR-DAF (1) TO YE-SPL-CUR-DAF (1).          1003625
792678     MOVE WMS-SPL-OLD-CUR-ANN (2) TO YE-SPL-CUR-ANN (2).          1003625
792680     MOVE WMS-SPL-OLD-CUR-DAF (2) TO YE-SPL-CUR-DAF (2).          1003625
792682     MOVE WMS-SPL-OLD-CUR-ANN (3) TO YE-SPL-CUR-ANN (3).          1003625
792684     MOVE WMS-SPL-OLD-CUR-DAF (3) TO YE-SPL-CUR-DAF (3).          1003625
792686     MOVE WMS-SPL-OLD-CUR-LMT (1) TO YE-SPL-CUR-LMT (1).          1003625
792688     MOVE WMS-SPL-OLD-CUR-LMT (2) TO YE-SPL-CUR-LMT (2).          1003625
792690 OLD-SPLIT-RATE-MOVE-YE-EXIT.                                     1003625
792692     EXIT.                                                        1003625
792694                                                                  1003625
792700 SPLIT-RATE-MOVE.                                                 1003625
792702     MOVE WMS-SPL-CUR-ANN (1) TO SR-ANNL-RATE1.                   1003625
792704     MOVE WMS-SPL-CUR-DAF (1) TO SR-DAILY-RATE1.                  1003625
792706     MOVE WMS-SPL-CUR-ANN (2) TO SR-ANNL-RATE2.                   1003625
792708     MOVE WMS-SPL-CUR-DAF (2) TO SR-DAILY-RATE2.                  1003625
792710     MOVE WMS-SPL-CUR-ANN (3) TO SR-ANNL-RATE3.                   1003625
792712     MOVE WMS-SPL-CUR-DAF (3) TO SR-DAILY-RATE3.                  1003625
792714     MOVE WMS-SPL-CUR-LMT (1) TO SR-LIMIT1.                       1003625
792716     MOVE WMS-SPL-CUR-LMT (2) TO SR-LIMIT2.                       1003625
792718 SPLIT-RATE-MOVE-EXIT.                                            1003625
792720     EXIT.                                                        1003625
792722                                                                  1003625
792724 PREV-SPLIT-RATE-MOVE.                                            1003625
792726     MOVE WMS-SPL-PREV-ANN (1) TO SR-ANNL-RATE1.                  1003625
792728     MOVE WMS-SPL-PREV-DAF (1) TO SR-DAILY-RATE1.                 1003625
792730     MOVE WMS-SPL-PREV-ANN (2) TO SR-ANNL-RATE2.                  1003625
792732     MOVE WMS-SPL-PREV-DAF (2) TO SR-DAILY-RATE2.                 1003625
792734     MOVE WMS-SPL-PREV-ANN (3) TO SR-ANNL-RATE3.                  1003625
792736     MOVE WMS-SPL-PREV-DAF (3) TO SR-DAILY-RATE3.                 1003625
792738     MOVE WMS-SPL-PREV-LMT (1) TO SR-LIMIT1.                      1003625
792740     MOVE WMS-SPL-PREV-LMT (2) TO SR-LIMIT2.                      1003625
792742 PREV-SPLIT-RATE-MOVE-EXIT.                                       1003625
792744     EXIT.                                                        1003625
792746 OLD-SPLIT-RATE-MOVE.                                             1003625
792748     MOVE WMS-SPL-OLD-CUR-ANN (1) TO SR-ANNL-RATE1.               1003625
792750     MOVE WMS-SPL-OLD-CUR-DAF (1) TO SR-DAILY-RATE1.              1003625
792752     MOVE WMS-SPL-OLD-CUR-ANN (2) TO SR-ANNL-RATE2.               1003625
792754     MOVE WMS-SPL-OLD-CUR-DAF (2) TO SR-DAILY-RATE2.              1003625
792756     MOVE WMS-SPL-OLD-CUR-ANN (3) TO SR-ANNL-RATE3.               1003625
792758     MOVE WMS-SPL-OLD-CUR-DAF (3) TO SR-DAILY-RATE3.              1003625
792760     MOVE WMS-SPL-OLD-CUR-LMT (1) TO SR-LIMIT1.                   1003625
792762     MOVE WMS-SPL-OLD-CUR-LMT (2) TO SR-LIMIT2.                   1003625
792764 OLD-SPLIT-RATE-MOVE-EXIT.                                        1003625
792766     EXIT.                                                        1003625
792768                                                                  1003625
792770 COPY IMPDRTWR.                                                   1003625
792771*                                                                 1003625
792772 COPY IMPDRTMR.                                                   1003625
792774                                                                  1003625
792775 ROLL-FUTURE-RATES.                                               2012254
792776     IF  TIER-FUT EQUAL ZERO                                      2012254
792777     AND IOD-FUT EQUAL ZERO                                       2012254
792778     AND DFLT-IOD-FUT EQUAL ZERO                                  2012254
792779     AND SAV-FUT EQUAL ZERO                                       2012254
792780     AND FED-TAX-FUT EQUAL ZERO                                   2012254
792781     AND ST-TAX-FUT EQUAL ZERO                                    2012254
792782     AND LOC-TAX-FUT EQUAL ZERO                                   2012254
792785     AND OD-TRLR-FUT (1) EQUAL ZERO                               9915856
792786     AND OD-TRLR-FUT (2) EQUAL ZERO                               9915856
792787     AND OD-TRLR-FUT (3) EQUAL ZERO                               9915856
792788     AND OD-TRLR-FUT (4) EQUAL ZERO                               9915856
792789     AND OD-TRLR-FUT (5) EQUAL ZERO                               9915856
792790     AND OD-TRLR-FUT (6) EQUAL ZERO                               9915856
792791     AND OD-TRLR-FUT (7) EQUAL ZERO                               9915856
792792     AND OD-TRLR-FUT (8) EQUAL ZERO                               9915856
792793     AND OD-TRLR-FUT (9) EQUAL ZERO                               9915856
792794     AND PRIME-FUT EQUAL ZERO                                     1003625
792795     AND SAV-TIER-FUT EQUAL ZERO                                  0316967
792796         GO TO FUTURE-RATE-EXIT.                                  0316967
792797*                                                                 0316967
792798 FUTURE-TIER-RATE.                                                1003625
792799     PERFORM C2100-CHECK-RATE THRU C2100-CHECK-RATE-EXIT.         0817657
792800     IF  WMS-HIFI-INDICATOR GREATER THAN +0                       1003625
792802         IF  TIER-FUT EQUAL ZERO                                  1003625
792804             GO TO FUTURE-DFLT-IOD-RATE                           1003625
792806         ELSE                                                     1003625
792808             IF  ACCRUAL-DATE EQUAL WS-RWF-TIER-DATE (TIER-FUT)   1003625
792810                 MOVE WMS-TIER-CUR-DATA TO WMS-TIER-PREV-DATA     1003625
792812                 MOVE WMS-DFLT-IOD-CUR-DATA                       1003625
792814                   TO WMS-DFLT-IOD-PREV-DATA                      1003625
792816                 MOVE WS-RWF-TIER-DATA (TIER-FUT)                 1003625
792818                   TO WMS-TIER-CUR-DATA                           1003625
792820                 GO TO FUTURE-DFLT-IOD-RATE.                      1003625
792822 FUTURE-IOD-RATE.                                                 1003625
792824     IF  WMS-IOD-RATE-PTR GREATER THAN +0                         1003625
792826         IF  IOD-FUT EQUAL ZERO                                   1003625
792828             GO TO FUTURE-DFLT-IOD-RATE                           1003625
792830         ELSE                                                     1003625
792832             IF  ACCRUAL-DATE EQUAL WS-RWF-DDA-DATE (IOD-FUT)     1003625
792834                 MOVE WMS-IOD-CUR-DATA TO WMS-IOD-PREV-DATA       1003625
792836                 MOVE WS-RWF-DDA-DATA (IOD-FUT)                   1003625
792838                   TO WMS-IOD-CUR-DATA.                           1003625
792840 FUTURE-DFLT-IOD-RATE.                                            1003625
792842* SET FUTURE DFLT IOD RATE.                                       2012254
792844     IF  WMS-HIFI-INDICATOR GREATER THAN +0                       2012254
792846     OR  WMS-IOD-RATE-PTR GREATER THAN +0                         2012254
792848         IF  DFLT-IOD-FUT EQUAL ZERO                              2012254
792850             GO TO FUTURE-SAV-RATE                                2012254
792852         ELSE                                                     2012254
792854             IF  ACCRUAL-DATE EQUAL WS-RWF-DDA-DATE (DFLT-IOD-FUT)2012254
792856                 MOVE WMS-DFLT-IOD-CUR-DATA                       2012254
792858                   TO WMS-DFLT-IOD-PREV-DATA                      2012254
792860                 MOVE WS-RWF-DDA-DATA (DFLT-IOD-FUT)              2012254
792862                   TO WMS-DFLT-IOD-CUR-DATA.                      2012254
792864 FUTURE-SAV-RATE.                                                 2012254
792865     IF  WMS-SAVINGS-TRLR EQUAL '0'                               2012254
792866         GO TO FUTURE-FED-TAX-RATE.                               2012254
792867     IF  WMS-INT-RATE GREATER THAN +0                             2012254
792868     AND WMS-SAV-TIER-PTR EQUAL +0                                0316967
792869         IF  SAV-FUT EQUAL ZERO                                   0316967
792870             GO TO FUTURE-FED-TAX-RATE                            2012254
792872         ELSE                                                     2012254
792874             IF  ACCRUAL-DATE EQUAL WS-RWF-SAV-DATE (SAV-FUT)     2012254
792876                 MOVE WMS-SAV-CUR-DATA TO WMS-SAV-PREV-DATA       2012254
792878                 MOVE WS-RWF-SAV-DATA (SAV-FUT)                   2012254
792879                   TO WMS-SAV-CUR-DATA.                           2012254
792880 FUTURE-FED-TAX-RATE.                                             2012254
792881     IF  WMS-TAX-FED-PTR GREATER THAN +0                          2012254
792882     AND FED-TAX-FUT GREATER ZERO                                 2012254
792883         IF  ACCRUAL-DATE EQUAL WS-RWF-TAX-DATE (FED-TAX-FUT)     2012254
792884             MOVE WS-RWF-TAX-DATA (FED-TAX-FUT)                   2012254
792885               TO WMS-FED-TAX-CUR-DATA.                           2012254
792886 FUTURE-ST-TAX-RATE.                                              2012254
792887     IF  WMS-TAX-STATE-PTR GREATER THAN +0                        2012254
792888     AND ST-TAX-FUT GREATER ZERO                                  2012254
792889         IF  ACCRUAL-DATE EQUAL WS-RWF-TAX-DATE (ST-TAX-FUT)      2012254
792890             MOVE WS-RWF-TAX-DATA (ST-TAX-FUT)                    2012254
792891               TO WMS-ST-TAX-CUR-DATA.                            2012254
792892 FUTURE-LOC-TAX-RATE.                                             2012254
792893     IF  WMS-TAX-LOCAL-PTR GREATER THAN +0                        2012254
792894     AND LOC-TAX-FUT GREATER ZERO                                 2012254
792895         IF  ACCRUAL-DATE EQUAL WS-RWF-TAX-DATE (LOC-TAX-FUT)     2012254
792896             MOVE WS-RWF-TAX-DATA (LOC-TAX-FUT)                   2012254
792897               TO WMS-LOC-TAX-CUR-DATA.                           2012254
792898 FUTURE-OD-RATE.                                                  2012254
792899*                                                                 2012254
792900     COPY IMPDODFR.                                               2012254
792901*                                                                 2012254
792920 FUTURE-SAV-TIER-RATE.                                            0316967
792921     IF  WMS-SAVINGS-TRLR EQUAL '0'                               0316967
792922         GO TO FUTURE-RATE-EXIT.                                  0316967
792923     IF  WMS-SAV-TIER-PTR GREATER THAN +0                         0316967
792924         IF  SAV-TIER-FUT EQUAL ZERO                              0316967
792925             GO TO FUTURE-RATE-EXIT                               0316967
792926         ELSE                                                     0316967
792927             IF  ACCRUAL-DATE EQUAL WS-RWF-SAV-DATE (SAV-TIER-FUT)0316967
792928                 MOVE WMS-SAV-CUR-DATA TO WMS-SAV-PREV-DATA       0316967
792929                 MOVE WS-RWF-SAV-DATA (SAV-TIER-FUT)              0316967
792930                   TO WMS-SAV-CUR-DATA.                           0316967
792940*                                                                 0316967
792945 FUTURE-RATE-EXIT.                                                0316967
792950     EXIT.                                                        0316967
792964 ROLL-FUTURE-LOAN-RATE.                                           1003625
792965     PERFORM ADJ-FUT-LN-ACCR-DT THRU ADJ-FUT-LN-ACCR-EXIT.        1003625
792966 FUTURE-PRIME-RATE.                                               1003625
792968     IF  WMS-INT-RATE-PTR GREATER THAN +5                         1003625
792970         IF  PRIME-FUT EQUAL ZERO                                 1003625
792972             GO TO FUTURE-PREV-LOAN-RATE                          1003625
792974         ELSE                                                     1003625
792975         IF  RATE-CHG-SW EQUAL '1'                                1020057
792976         AND WS-RWF-PRM-ANN (PRIME-FUT) GREATER THAN              1020057
792977                                        WMS-PRM-CUR-ANN           1020057
792978             GO TO FUTURE-PREV-LOAN-RATE                          1020057
792979         ELSE                                                     1020057
792980             IF  ACCRUAL-DATE-WK EQUAL WS-RWF-PRM-DATE (PRIME-FUT)1020057
792981                 MOVE WMS-PRM-CUR-DATA TO WMS-PRM-PREV-DATA       1020057
792982                 MOVE WS-RWF-PRM-DATA (PRIME-FUT)                 1020057
792983                                       TO WMS-PRM-CUR-DATA        1020057
792996                 IF  WMS-PRIME-CHNG-RTE EQUAL '1'                 1003625
792998                     MOVE WS-RWF-PRM-ANN (PRIME-FUT)              1003625
793000                                       TO WMS-PRIME-RATE          1003625
793002                     PERFORM PRIME-RATE-CALC THRU PRIME-EXIT      1003625
793004                     MOVE ACCR-DATE-MMDDYY                        1003625
793006                       TO WMS-DATE-LAST-RATE-CHANGE               1003625
793008                     GO TO FUTURE-PREV-LOAN-RATE                  1003625
793010                 ELSE                                             1003625
793012                     GO TO FUTURE-PREV-LOAN-RATE.                 1003625
793014 FUTURE-LOAN-RATE.                                                1003625
793016     IF  WMS-INT-RATE-PTR GREATER THAN +0                         1003625
793018         IF  LOAN-FUT EQUAL ZERO                                  1003625
793020             GO TO FUTURE-PREV-LOAN-RATE                          1003625
793022         ELSE                                                     1003625
793024         IF  RATE-CHG-SW EQUAL '1'                                1020057
793025         AND WS-RWF-LN-ANN (LOAN-FUT) GREATER THAN                1020057
793026                                      WMS-ANNUAL-RATE             1020057
793027             GO TO FUTURE-PREV-LOAN-RATE                          1020057
793028         ELSE                                                     1020057
793029             IF  ACCRUAL-DATE-WK EQUAL WS-RWF-LN-DATE (LOAN-FUT)  1020057
793030                 MOVE WMS-LN-CUR-DATA TO WMS-LN-PREV-DATA         1020057
793031                 MOVE WS-RWF-LN-DATA (LOAN-FUT)                   1020057
793032                                      TO WMS-LN-CUR-DATA          1020057
793033                 MOVE WS-RWF-LN-ANN (LOAN-FUT)                    1020057
793034                                      TO WMS-ANNUAL-RATE          1003625
793036                 MOVE WS-RWF-LN-DAF (LOAN-FUT)                    1003625
793038                                      TO WMS-DAILY-RATE           1003625
793040                 PERFORM C2302 THRU C2304-EXIT                    1003625
793042                 MOVE ACCR-DATE-MMDDYY                            1003625
793043                   TO WMS-DATE-LAST-RATE-CHANGE                   1003625
793044                 GO TO FUTURE-PREV-LOAN-RATE.                     1003625
793046 FUTURE-SPLIT-RATE.                                               1003625
793048     IF  WMS-SPLIT-RATE-SCHD GREATER THAN +0                      1003625
793050         IF  SPLIT-FUT EQUAL ZERO                                 1003625
793052             GO TO FUTURE-PREV-LOAN-RATE                          1003625
793054         ELSE                                                     1003625
793055         IF  RATE-CHG-SW EQUAL '1'                                1020057
793056         AND (WS-RWF-SPL-ANN (SPLIT-FUT 1) GREATER THAN           1020057
793057                                       WMS-RATE1-ANNUAL           1020057
793058         OR  WS-RWF-SPL-ANN (SPLIT-FUT 2) GREATER THAN            1020057
793059                                       WMS-RATE2-ANNUAL           1020057
793060         OR  WS-RWF-SPL-ANN (SPLIT-FUT 3) GREATER THAN            1020057
793061                                       WMS-RATE3-ANNUAL)          1020057
793062             GO TO FUTURE-PREV-LOAN-RATE                          1020057
793063         ELSE                                                     1020057
793064             IF  ACCRUAL-DATE-WK EQUAL WS-RWF-SPL-DATE (SPLIT-FUT)1020057
793065                 MOVE WMS-SPL-CUR-DATA TO WMS-SPL-PREV-DATA       1020057
793066                 MOVE WS-RWF-SPL-DATA (SPLIT-FUT)                 1020057
793067                                       TO WMS-SPL-CUR-DATA        1020057
793068                 MOVE WS-RWF-SPL-ANN (SPLIT-FUT 1)                1020057
793069                                       TO WMS-RATE1-ANNUAL        1020057
793070                 MOVE WS-RWF-SPL-DAF (SPLIT-FUT 1)                1020057
793071                                       TO WMS-RATE1-DAILY         1020057
793072                 MOVE WS-RWF-SPL-ANN (SPLIT-FUT 2)                1003625
793074                                       TO WMS-RATE2-ANNUAL        1003625
793076                 MOVE WS-RWF-SPL-DAF (SPLIT-FUT 2)                1003625
793078                                       TO WMS-RATE2-DAILY         1003625
793080                 MOVE WS-RWF-SPL-ANN (SPLIT-FUT 3)                1003625
793082                                       TO WMS-RATE3-ANNUAL        1003625
793084                 MOVE WS-RWF-SPL-DAF (SPLIT-FUT 3)                1003625
793086                                       TO WMS-RATE3-DAILY         1003625
793088                 MOVE WS-RWF-SPL-LMT (SPLIT-FUT 1)                1003625
793090                                       TO WMS-RATE1-LIMIT         1003625
793092                 MOVE WS-RWF-SPL-LMT (SPLIT-FUT 2)                1003625
793094                                       TO WMS-RATE2-LIMIT         1003625
793096                 PERFORM C2302 THRU C2302-EXIT                    1003625
793098                 PERFORM C2306 THRU C2306-EXIT                    1003625
793100                 MOVE ACCR-DATE-MMDDYY                            1003625
793101                   TO WMS-DATE-LAST-RATE-CHANGE                   1003625
793102                 GO TO FUTURE-PREV-LOAN-RATE.                     1003625
793104*                                                                 1003625
793106 FUTURE-PREV-LOAN-RATE.                                           1003625
793108     IF  WMS-PREV-INT-PTR GREATER THAN +0                         1003625
793110         IF  PREV-LOAN-FUT EQUAL ZERO                             1003625
793112             GO TO FUTURE-LOAN-RATE-EXIT                          1003625
793114         ELSE                                                     1003625
793115         IF  RATE-CHG-SW EQUAL '1'                                1020057
793116         AND WS-RWF-LN-ANN (PREV-LOAN-FUT) GREATER THAN           1020057
793117                                      WMS-PREV-ANNUAL             1020057
793118             GO TO FUTURE-LOAN-RATE-EXIT                          1020057
793119         ELSE                                                     1020057
793120             IF  ACCRUAL-DATE-WK EQUAL                            1020057
793121                 WS-RWF-LN-DATE (PREV-LOAN-FUT)                   1020057
793122                 MOVE WMS-LN-OLD-CUR-DATA TO WMS-LN-OLD-PREV-DATA 1020057
793123                 MOVE WS-RWF-LN-DATA (PREV-LOAN-FUT)              1020057
793124                                          TO WMS-LN-OLD-CUR-DATA  1020057
793125                 MOVE WS-RWF-LN-ANN (PREV-LOAN-FUT)               1020057
793126                                          TO WMS-PREV-ANNUAL      1003625
793128                 MOVE WS-RWF-LN-DAF (PREV-LOAN-FUT)               1003625
793130                                          TO WMS-PREV-DAILY       1003625
793131                 MOVE ACCR-DATE-MMDDYY                            1020057
793132                   TO WMS-DATE-LAST-RATE-CHANGE                   1020057
793133                 GO TO FUTURE-LOAN-RATE-EXIT.                     1020057
793134                                                                  1020057
793135 FUTURE-PREV-SPLIT-RATE.                                          1020057
793136     IF  WMS-PREV-SPLIT-RATE-PTR GREATER THAN +0                  1020057
793137         IF  PREV-SPLIT-FUT EQUAL ZERO                            1020057
793138             GO TO FUTURE-LOAN-RATE-EXIT                          1020057
793139         ELSE                                                     1020057
793140         IF  RATE-CHG-SW EQUAL '1'                                1020057
793141         AND (WS-RWF-SPL-ANN (PREV-SPLIT-FUT 1) GREATER THAN      1020057
793142                                       WMS-SPL-OLD-CUR-ANN(1)     1020057
793143         OR  WS-RWF-SPL-ANN (SPLIT-FUT 2) GREATER THAN            1020057
793144                                       WMS-SPL-OLD-CUR-ANN(2)     1020057
793145         OR  WS-RWF-SPL-ANN (SPLIT-FUT 3) GREATER THAN            1020057
793146                                       WMS-SPL-OLD-CUR-ANN(3))    1020057
793147             GO TO FUTURE-PREV-LOAN-RATE                          1020057
793148         ELSE                                                     1020057
793149             IF  ACCRUAL-DATE-WK EQUAL                            1020057
793150                 WS-RWF-SPL-DATE (PREV-SPLIT-FUT)                 1003625
793151                 MOVE WMS-SPL-OLD-CUR-DATA                        1003625
793152                   TO WMS-SPL-OLD-PREV-DATA                       1003625
793153                 MOVE WS-RWF-SPL-DATA (PREV-SPLIT-FUT)            1003625
793154                   TO WMS-SPL-OLD-CUR-DATA                        1003625
793155                 MOVE ACCR-DATE-MMDDYY                            1003625
793156                   TO WMS-DATE-LAST-RATE-CHANGE.                  1003625
793157*                                                                 1003625
793158 FUTURE-LOAN-RATE-EXIT.                                           1003625
793159     EXIT.                                                        1003625
793160 ADJ-FUT-LN-ACCR-DT.                                              1003625
793161     IF  LOAN-ACCRUAL-DAY GREATER THAN +1                         1003625
793162         MOVE +1                 TO C-NO-DAYS                     1003625
793163         MOVE ACCRUAL-DATE-WK    TO C-LOW-DATE                    1003625
793164         CALL 'SIDCHI'           USING C-DATE-WORK                1003625
793165         MOVE C-HIGH-DATE        TO ACCRUAL-DATE-WK               1003625
793166     ELSE                                                         1003625
793167         MOVE ACCRUAL-DATE       TO ACCRUAL-DATE-WK.              1003625
793168     MOVE ADW-MM                 TO ACCR-DATE-MM.                 1003625
793169     MOVE ADW-DD                 TO ACCR-DATE-DD.                 1003625
793170     MOVE ADW-YY                 TO ACCR-DATE-YY.                 1003625
793171 ADJ-FUT-LN-ACCR-EXIT.                                            1003625
793172     EXIT.                                                        1003625
793173*                                                                 1003625
793174 YEAR-CHANGE-RATE-CHANGE.                                         1003625
793175     MOVE WBC-NEXT-CAPTURE-YR TO WS-NY-YR.                        0266741
793176     IF  WBC-NEXT-CAPTURE-YR LESS THAN WBC-NEXT-CENT-YR           0266741
793177         MOVE '20' TO WS-NY-CC                                    9715519
793178     ELSE                                                         9715519
793179         MOVE '19' TO WS-NY-CC.                                   9715519
793180     IF  NEXT-DAYS EQUAL +365                                     9715519
793181     AND HOLD-DDA-LEAP-FLG EQUAL '0'                              9715519
793182         GO TO CALC-IOD-CHANGE.                                   9715519
793184     IF  WMS-HIFI-INDICATOR EQUAL +0                              1003625
793185     OR (WMS-DAILY-RATE-CODE EQUAL '1' OR '3')                    0817657
793186         GO TO CALC-IOD-CHANGE.                                   1003625
793188     MOVE WMS-TIER-CUR-DATA TO WMS-TIER-PREV-DATA.                1003625
793190     MOVE WS-NEW-YEAR-DATE  TO WMS-TIER-CUR-DATE.                 1003625
793192     DIVIDE NEXT-DAYS INTO WMS-TIER-CUR-ANN (1)                   1003625
793194                    GIVING WMS-TIER-CUR-DAF (1).                  1003625
793196     DIVIDE NEXT-DAYS INTO WMS-TIER-CUR-ANN (2)                   1003625
793198                    GIVING WMS-TIER-CUR-DAF (2).                  1003625
793200     DIVIDE NEXT-DAYS INTO WMS-TIER-CUR-ANN (3)                   1003625
793202                    GIVING WMS-TIER-CUR-DAF (3).                  1003625
793204     DIVIDE NEXT-DAYS INTO WMS-TIER-CUR-ANN (4)                   1003625
793206                    GIVING WMS-TIER-CUR-DAF (4).                  1003625
793208     DIVIDE NEXT-DAYS INTO WMS-TIER-CUR-ANN (5)                   1003625
793210                    GIVING WMS-TIER-CUR-DAF (5).                  1003625
793212     DIVIDE NEXT-DAYS INTO WMS-TIER-CUR-ANN (6)                   1003625
793214                    GIVING WMS-TIER-CUR-DAF (6).                  1003625
793216     DIVIDE NEXT-DAYS INTO WMS-TIER-CUR-ANN (7)                   1003625
793218                    GIVING WMS-TIER-CUR-DAF (7).                  1003625
793220     DIVIDE NEXT-DAYS INTO WMS-TIER-CUR-ANN (8)                   1003625
793222                    GIVING WMS-TIER-CUR-DAF (8).                  1003625
793224     DIVIDE NEXT-DAYS INTO WMS-TIER-CUR-ANN (9)                   1003625
793226                    GIVING WMS-TIER-CUR-DAF (9).                  1003625
793232     MOVE WMS-DFLT-IOD-CUR-DATA TO WMS-DFLT-IOD-PREV-DATA.        1003625
793234     MOVE WS-NEW-YEAR-DATE      TO WMS-DFLT-IOD-CUR-DATE.         1003625
793236     IF  WMS-DAILY-RATE-CODE EQUAL '0'                            0817657
793237     OR (WMS-DAILY-RATE-CODE EQUAL '2'                            0817657
793238     AND WBC-DAILY-RATE-PHASE EQUAL SPACES)                       0817657
793239         DIVIDE NEXT-DAYS INTO WMS-DFLT-IOD-CUR-ANN               0817657
793240                        GIVING WMS-DFLT-IOD-CUR-DAF               1003625
793242     ELSE                                                         1003625
793244         IF  WMS-DAILY-RATE-CODE EQUAL '2'                        0817657
793246             MOVE WMS-DFLT-IOD-CUR-ANN TO WK-LIR-ANNUAL           1003625
793248             MOVE WBC-DAILY-RATE-PHASE TO IM31-PH2                0266741
793250             CALL 'SILINK' USING IM31-PHASE                       1003625
793254                                 RATE-WORK-AREA                   1003625
793256             MOVE WK-LIR-DAILY TO WMS-DFLT-IOD-CUR-DAF.           1003625
793258     GO TO CALC-SAV-CHANGE.                                       1003625
793260*                                                                 1003625
793262 CALC-IOD-CHANGE.                                                 1003625
793263     IF  NEXT-DAYS EQUAL +365                                     9715519
793264     AND HOLD-DDA-LEAP-FLG EQUAL '0'                              9715519
793265     AND WMS-DAILY-RATE-CODE EQUAL '0'                            0817657
793266         GO TO CALC-SAV-CHANGE.                                   9715519
793267     IF  (WMS-IOD-RATE-PTR EQUAL +0)                              9715519
793268     OR  (WMS-DAILY-RATE-CODE EQUAL '1' OR '3')                   0817657
793269         GO TO CALC-SAV-CHANGE.                                   9715519
793270     MOVE WMS-IOD-CUR-DATA TO WMS-IOD-PREV-DATA.                  1003625
793271     MOVE WS-NEW-YEAR-DATE TO WMS-IOD-CUR-DATE.                   9915578
793272     MOVE WMS-DFLT-IOD-CUR-DATA TO WMS-DFLT-IOD-PREV-DATA.        9915578
793273     MOVE WS-NEW-YEAR-DATE      TO WMS-DFLT-IOD-CUR-DATE.         9915578
793274     IF  WMS-DAILY-RATE-CODE EQUAL '0'                            0817657
793275     OR (WMS-DAILY-RATE-CODE EQUAL '2'                            0817657
793276     AND WBC-DAILY-RATE-PHASE EQUAL SPACES)                       0817657
793277         DIVIDE NEXT-DAYS INTO WMS-IOD-CUR-ANN                    0817657
793278                        GIVING WMS-IOD-CUR-DAF                    0817657
793279         DIVIDE NEXT-DAYS INTO WMS-DFLT-IOD-CUR-ANN               0817657
793280                        GIVING WMS-DFLT-IOD-CUR-DAF               0817657
793281     ELSE                                                         0817657
793282         IF  WMS-DAILY-RATE-CODE EQUAL '2'                        0817657
793284             MOVE WMS-IOD-CUR-ANN TO WK-LIR-ANNUAL                1003625
793286             MOVE WBC-DAILY-RATE-PHASE TO IM31-PH2                0266741
793288             CALL 'SILINK' USING IM31-PHASE                       1003625
793290                                 RATE-WORK-AREA                   9915578
793292             MOVE WK-LIR-DAILY TO WMS-IOD-CUR-DAF                 9915578
793293             MOVE WMS-DFLT-IOD-CUR-ANN TO WK-LIR-ANNUAL           9915578
793294             CALL 'SILINK' USING IM31-PHASE                       9915578
793295                                 RATE-WORK-AREA                   9915578
793296             MOVE WK-LIR-DAILY TO WMS-DFLT-IOD-CUR-DAF.           9915578
793297*                                                                 9915578
793298 CALC-SAV-CHANGE.                                                 1003625
793299     IF  NEXT-DAYS EQUAL +365                                     9715519
793300     AND HOLD-SAV-LEAP-FLG EQUAL '0'                              9715519
793301     AND WMS-DAILY-RATE-CODE EQUAL '0'                            0817657
793302         GO TO CALC-OD-ACCR-CHANGE.                               9715519
793303     IF  (WMS-SAVINGS-TRLR EQUAL '0')                             9715519
793304     OR  (WMS-DAILY-RATE-CODE EQUAL '1' OR '3')                   0817657
793305         GO TO CALC-OD-ACCR-CHANGE.                               9715519
793306     MOVE WMS-SAV-CUR-DATA TO WMS-SAV-PREV-DATA.                  1003625
793308     MOVE WS-NEW-YEAR-DATE TO WMS-SAV-CUR-DATE.                   1003625
793310     IF  WMS-DAILY-RATE-CODE EQUAL '0'                            0817657
793311     OR (WMS-DAILY-RATE-CODE EQUAL '2'                            0817657
793312     AND WBC-DAILY-RATE-PHASE EQUAL SPACES)                       0817657
793313         DIVIDE NEXT-DAYS INTO WMS-SAV-CUR-ANN                    0817657
793314                        GIVING WMS-SAV-CUR-DAF                    1003625
793316     ELSE                                                         1003625
793318         IF  WMS-DAILY-RATE-CODE EQUAL '2'                        0817657
793320             MOVE WMS-SAV-CUR-ANN TO WK-LIR-ANNUAL                1003625
793322             MOVE WBC-DAILY-RATE-PHASE TO IM31-PH2                0266741
793324             CALL 'SILINK' USING IM31-PHASE                       1003625
793328                                 RATE-WORK-AREA                   1003625
793330             MOVE WK-LIR-DAILY TO WMS-SAV-CUR-DAF.                1003625
793332*                                                                 1003625
793334 CALC-OD-ACCR-CHANGE.                                             1003625
793350     COPY IMPDODAC.                                               9915856
793386*                                                                 1003625
793388 CALC-LOAN-CHANGE.                                                1003625
793390     IF  (WMS-LOAN-TRLR EQUAL +0)                                 1003625
793392     OR  (WMS-LOAN-360-DAY-FACTOR EQUAL TO '1' OR '2')            0817657
793394         GO TO YEAR-CHANGE-RATE-CHANGE-EXIT.                      1003625
793395*                                                                 2016597
793396     MOVE WS-NY-MO             TO WMS-RATE-CHANGE-MO.             2016597
793397     MOVE WS-NY-DA             TO WMS-RATE-CHANGE-DA.             2016597
793398     MOVE WS-NY-YR             TO WMS-RATE-CHANGE-YR.             2016597
793399*                                                                 2016597
793400     IF  WMS-ANNUAL-RATE NUMERIC                                  2016597
793401     AND WMS-ANNUAL-RATE GREATER THAN +0                          2016597
793402         DIVIDE NEXT-DAYS INTO WMS-ANNUAL-RATE                    2016597
793403                        GIVING WMS-DAILY-RATE.                    2016597
793404*                                                                 2016597
793405     IF  WMS-RATE1-ANNUAL NUMERIC                                 2016597
793406     AND WMS-RATE1-ANNUAL GREATER THAN +0                         2016597
793407         DIVIDE NEXT-DAYS INTO WMS-RATE1-ANNUAL                   2016597
793408                        GIVING WMS-RATE1-DAILY                    2016597
793409         IF  WMS-RATE2-ANNUAL NUMERIC                             2016597
793410         AND WMS-RATE2-ANNUAL GREATER THAN +0                     2016597
793411             DIVIDE NEXT-DAYS INTO WMS-RATE2-ANNUAL               2016597
793412                            GIVING WMS-RATE2-DAILY                2016597
793413             IF  WMS-RATE3-ANNUAL NUMERIC                         2016597
793414             AND WMS-RATE3-ANNUAL GREATER THAN +0                 2016597
793415                 DIVIDE NEXT-DAYS INTO WMS-RATE3-ANNUAL           2016597
793416                                GIVING WMS-RATE3-DAILY.           2016597
793417*                                                                 2016597
793418     IF  WMS-INT-RATE-PTR GREATER THAN +5                         2016597
793419         MOVE WMS-PRM-CUR-DATA TO WMS-PRM-PREV-DATA               2016597
793420         MOVE WS-NEW-YEAR-DATE TO WMS-PRM-CUR-DATE                2016597
793421         DIVIDE NEXT-DAYS INTO WMS-PRM-CUR-ANN                    2016597
793422                        GIVING WMS-PRM-CUR-DAF                    2016597
793423         GO TO CALC-PREV-LOAN-CHANGE.                             2016597
793424*                                                                 2016597
793425     IF  WMS-INT-RATE-PTR GREATER THAN +0 AND LESS THAN +6        2016597
793426         MOVE WMS-LN-CUR-DATA  TO WMS-LN-PREV-DATA                2016597
793427         MOVE WS-NEW-YEAR-DATE TO WMS-LN-CUR-DATE                 2016597
793428         DIVIDE NEXT-DAYS INTO WMS-LN-CUR-ANN                     2016597
793429                        GIVING WMS-LN-CUR-DAF                     2016597
793430         GO TO CALC-PREV-LOAN-CHANGE.                             2016597
793431*                                                                 2016597
793432     IF  WMS-SPLIT-RATE-SCHD GREATER THAN +0                      2016597
793433         MOVE WMS-SPL-CUR-DATA TO WMS-SPL-PREV-DATA               2016597
793434         MOVE WS-NEW-YEAR-DATE TO WMS-SPL-CUR-DATE                2016597
793435         DIVIDE NEXT-DAYS INTO WMS-SPL-CUR-ANN (1)                2016597
793436                        GIVING WMS-SPL-CUR-DAF (1)                2016597
793437         DIVIDE NEXT-DAYS INTO WMS-SPL-CUR-ANN (2)                2016597
793438                        GIVING WMS-SPL-CUR-DAF (2)                2016597
793439         DIVIDE NEXT-DAYS INTO WMS-SPL-CUR-ANN (3)                2016597
793440                        GIVING WMS-SPL-CUR-DAF (3)                2016597
793441         GO TO CALC-PREV-LOAN-CHANGE.                             2016597
793442*                                                                 2016597
793443 CALC-PREV-LOAN-CHANGE.                                           2016597
793444     IF  WMS-PREV-ANNUAL NUMERIC                                  2016597
793445     AND WMS-PREV-ANNUAL GREATER THAN +0                          2016597
793446         DIVIDE NEXT-DAYS INTO WMS-PREV-ANNUAL                    2016597
793447                        GIVING WMS-PREV-DAILY.                    2016597
793448*                                                                 2016597
793449     IF  WMS-PREV-INT-PTR GREATER THAN +0                         2016597
793450         MOVE WMS-LN-OLD-CUR-DATA TO WMS-LN-OLD-PREV-DATA         2016597
793451         MOVE WS-NEW-YEAR-DATE    TO WMS-LN-OLD-CUR-DATE          2016597
793452         DIVIDE NEXT-DAYS INTO WMS-LN-OLD-CUR-ANN                 2016597
793453                        GIVING WMS-LN-OLD-CUR-DAF                 2016597
793454         GO TO YEAR-CHANGE-RATE-CHANGE-EXIT.                      1003625
793455*                                                                 2016597
793456     IF  WMS-PREV-SPLIT-RATE-PTR GREATER THAN +0                  1003625
793458         MOVE WMS-SPL-OLD-CUR-DATA TO WMS-SPL-OLD-PREV-DATA       1003625
793460         MOVE WS-NEW-YEAR-DATE     TO WMS-SPL-OLD-CUR-DATE        1003625
793462         DIVIDE NEXT-DAYS INTO WMS-SPL-OLD-CUR-ANN (1)            1003625
793464                        GIVING WMS-SPL-OLD-CUR-DAF (1)            1003625
793466         DIVIDE NEXT-DAYS INTO WMS-SPL-OLD-CUR-ANN (2)            1003625
793468                        GIVING WMS-SPL-OLD-CUR-DAF (2)            1003625
793470         DIVIDE NEXT-DAYS INTO WMS-SPL-OLD-CUR-ANN (3)            1003625
793472                        GIVING WMS-SPL-OLD-CUR-DAF (3)            1003625
793474         GO TO YEAR-CHANGE-RATE-CHANGE-EXIT.                      1003625
793476*                                                                 1003625
793478 YEAR-CHANGE-RATE-CHANGE-EXIT.                                    1003625
793480     EXIT.                                                        1003625
793500*                                                                 1003625
793502*--------------------------------------------------------------*  1003625
793504*CALCULATE THE DAILY FACTORS FOR CURRENT AND PREVIOUS RATES       1003625
793506*FOR NEW ACCOUNTS AND POINTERS THAT HAVE BEEN MAINTENANCED.       1003625
793508*--------------------------------------------------------------*  1003625
793510 CALC-TIER-CUR-DAF.                                               1003625
793512     DIVIDE DAF-DAYS INTO WMS-TIER-CUR-ANN (1)                    1003625
793514                   GIVING WMS-TIER-CUR-DAF (1).                   1003625
793516     DIVIDE DAF-DAYS INTO WMS-TIER-CUR-ANN (2)                    1003625
793518                   GIVING WMS-TIER-CUR-DAF (2).                   1003625
793520     DIVIDE DAF-DAYS INTO WMS-TIER-CUR-ANN (3)                    1003625
793522                   GIVING WMS-TIER-CUR-DAF (3).                   1003625
793524     DIVIDE DAF-DAYS INTO WMS-TIER-CUR-ANN (4)                    1003625
793526                   GIVING WMS-TIER-CUR-DAF (4).                   1003625
793528     DIVIDE DAF-DAYS INTO WMS-TIER-CUR-ANN (5)                    1003625
793530                   GIVING WMS-TIER-CUR-DAF (5).                   1003625
793532     DIVIDE DAF-DAYS INTO WMS-TIER-CUR-ANN (6)                    1003625
793534                   GIVING WMS-TIER-CUR-DAF (6).                   1003625
793536     DIVIDE DAF-DAYS INTO WMS-TIER-CUR-ANN (7)                    1003625
793538                   GIVING WMS-TIER-CUR-DAF (7).                   1003625
793540     DIVIDE DAF-DAYS INTO WMS-TIER-CUR-ANN (8)                    1003625
793542                   GIVING WMS-TIER-CUR-DAF (8).                   1003625
793544     DIVIDE DAF-DAYS INTO WMS-TIER-CUR-ANN (9)                    1003625
793546                   GIVING WMS-TIER-CUR-DAF (9).                   1003625
793548 CALC-TIER-CUR-DAF-EXIT. EXIT.                                    1003625
793550 CALC-TIER-PREV-DAF.                                              1003625
793552     DIVIDE DAF-DAYS INTO WMS-TIER-PREV-ANN (1)                   1003625
793554                   GIVING WMS-TIER-PREV-DAF (1).                  1003625
793556     DIVIDE DAF-DAYS INTO WMS-TIER-PREV-ANN (2)                   1003625
793558                   GIVING WMS-TIER-PREV-DAF (2).                  1003625
793560     DIVIDE DAF-DAYS INTO WMS-TIER-PREV-ANN (3)                   1003625
793562                   GIVING WMS-TIER-PREV-DAF (3).                  1003625
793564     DIVIDE DAF-DAYS INTO WMS-TIER-PREV-ANN (4)                   1003625
793566                   GIVING WMS-TIER-PREV-DAF (4).                  1003625
793568     DIVIDE DAF-DAYS INTO WMS-TIER-PREV-ANN (5)                   1003625
793570                   GIVING WMS-TIER-PREV-DAF (5).                  1003625
793572     DIVIDE DAF-DAYS INTO WMS-TIER-PREV-ANN (6)                   1003625
793574                   GIVING WMS-TIER-PREV-DAF (6).                  1003625
793576     DIVIDE DAF-DAYS INTO WMS-TIER-PREV-ANN (7)                   1003625
793578                   GIVING WMS-TIER-PREV-DAF (7).                  1003625
793580     DIVIDE DAF-DAYS INTO WMS-TIER-PREV-ANN (8)                   1003625
793582                   GIVING WMS-TIER-PREV-DAF (8).                  1003625
793584     DIVIDE DAF-DAYS INTO WMS-TIER-PREV-ANN (9)                   1003625
793586                   GIVING WMS-TIER-PREV-DAF (9).                  1003625
793588 CALC-TIER-PREV-DAF-EXIT. EXIT.                                   1003625
793590                                                                  1003625
793592 CALC-DFLT-IOD-CUR-DAF.                                           1003625
793594     IF  WMS-DAILY-RATE-CODE EQUAL '2'                            0817657
793595     AND WBC-DAILY-RATE-PHASE GREATER THAN SPACES                 0817657
793596         MOVE WMS-DFLT-IOD-CUR-ANN TO WK-LIR-ANNUAL               1003625
793598         MOVE WBC-DAILY-RATE-PHASE TO IM31-PH2                    0266741
793600         CALL 'SILINK' USING IM31-PHASE                           1003625
793602                             RATE-WORK-AREA                       1003625
793604         MOVE WK-LIR-DAILY TO WMS-DFLT-IOD-CUR-DAF                1003625
793606     ELSE                                                         1003625
793608         DIVIDE DAF-DAYS INTO WMS-DFLT-IOD-CUR-ANN                1003625
793610                       GIVING WMS-DFLT-IOD-CUR-DAF.               1003625
793612 CALC-DFLT-IOD-CUR-DAF-EXIT. EXIT.                                1003625
793614 CALC-DFLT-IOD-PREV-DAF.                                          1003625
793616     IF  WMS-DAILY-RATE-CODE EQUAL '2'                            0817657
793617     AND WBC-DAILY-RATE-PHASE GREATER THAN SPACES                 0817657
793618         MOVE WMS-DFLT-IOD-PREV-ANN TO WK-LIR-ANNUAL              1003625
793620         MOVE WBC-DAILY-RATE-PHASE TO IM31-PH2                    0266741
793622         CALL 'SILINK' USING IM31-PHASE                           1003625
793624                             RATE-WORK-AREA                       1003625
793626         MOVE WK-LIR-DAILY TO WMS-DFLT-IOD-PREV-DAF               1003625
793628     ELSE                                                         1003625
793630         DIVIDE DAF-DAYS INTO WMS-DFLT-IOD-PREV-ANN               1003625
793632                       GIVING WMS-DFLT-IOD-PREV-DAF.              1003625
793634 CALC-DFLT-IOD-PREV-DAF-EXIT. EXIT.                               1003625
793636*                                                                 1003625
793638 CALC-IOD-CUR-DAF.                                                1003625
793640     IF  WMS-DAILY-RATE-CODE EQUAL '2'                            0817657
793641     AND WBC-DAILY-RATE-PHASE GREATER THAN SPACES                 0817657
793642         MOVE WMS-IOD-CUR-ANN TO WK-LIR-ANNUAL                    1003625
793644         MOVE WBC-DAILY-RATE-PHASE TO IM31-PH2                    0266741
793646         CALL 'SILINK' USING IM31-PHASE                           1003625
793648                             RATE-WORK-AREA                       1003625
793650         MOVE WK-LIR-DAILY TO WMS-IOD-CUR-DAF                     1003625
793652     ELSE                                                         1003625
793654         DIVIDE DAF-DAYS INTO WMS-IOD-CUR-ANN                     1003625
793656                       GIVING WMS-IOD-CUR-DAF.                    1003625
793658 CALC-IOD-CUR-DAF-EXIT. EXIT.                                     1003625
793660 CALC-IOD-PREV-DAF.                                               1003625
793662     IF  WMS-DAILY-RATE-CODE EQUAL '2'                            0817657
793663     AND WBC-DAILY-RATE-PHASE GREATER THAN SPACES                 0817657
793664         MOVE WMS-IOD-PREV-ANN TO WK-LIR-ANNUAL                   1003625
793666         MOVE WBC-DAILY-RATE-PHASE TO IM31-PH2                    0266741
793668         CALL 'SILINK' USING IM31-PHASE                           1003625
793670                             RATE-WORK-AREA                       1003625
793672         MOVE WK-LIR-DAILY TO WMS-IOD-PREV-DAF                    1003625
793674     ELSE                                                         1003625
793676         DIVIDE DAF-DAYS INTO WMS-IOD-PREV-ANN                    1003625
793677                       GIVING WMS-IOD-PREV-DAF.                   0817657
793678 CALC-IOD-PREV-DAF-EXIT. EXIT.                                    0817657
793679 CALC-SAV-CUR-DAF.                                                0817657
793680     IF  WMS-DAILY-RATE-CODE EQUAL '2'                            0817657
793681     AND WBC-DAILY-RATE-PHASE GREATER THAN SPACES                 0817657
793682         MOVE WMS-SAV-CUR-ANN TO WK-LIR-ANNUAL                    0817657
793683         MOVE WBC-DAILY-RATE-PHASE TO IM31-PH2                    0817657
793684         CALL 'SILINK' USING IM31-PHASE                           0817657
793685                             RATE-WORK-AREA                       0817657
793686         MOVE WK-LIR-DAILY TO WMS-SAV-CUR-DAF                     0817657
793687     ELSE                                                         0817657
793688         DIVIDE DAF-DAYS INTO WMS-SAV-CUR-ANN                     0817657
793689                       GIVING WMS-SAV-CUR-DAF.                    0817657
793690 CALC-SAV-CUR-DAF-EXIT. EXIT.                                     0817657
793691 CALC-SAV-PREV-DAF.                                               0817657
793692     IF  WMS-DAILY-RATE-CODE EQUAL '2'                            0817657
793693     AND WBC-DAILY-RATE-PHASE GREATER THAN SPACES                 0817657
793694         MOVE WMS-SAV-PREV-ANN TO WK-LIR-ANNUAL                   0316967
793695         MOVE WBC-DAILY-RATE-PHASE TO IM31-PH2                    0316967
793696         CALL 'SILINK' USING IM31-PHASE                           0316967
793697                             RATE-WORK-AREA                       0316967
793698         MOVE WK-LIR-DAILY TO WMS-SAV-PREV-DAF                    0316967
793699     ELSE                                                         0316967
793700         DIVIDE DAF-DAYS INTO WMS-SAV-PREV-ANN                    0316967
793701                       GIVING WMS-SAV-PREV-DAF.                   0316967
793702 CALC-SAV-PREV-DAF-EXIT. EXIT.                                    0316967
793703 CALC-SAVT-CUR-DAF.                                               0316967
793704     DIVIDE DAF-DAYS INTO WMX-TIER-CUR-ANN (1)                    0316967
793705                   GIVING WMX-TIER-CUR-DAF (1).                   0316967
793706     DIVIDE DAF-DAYS INTO WMX-TIER-CUR-ANN (2)                    0316967
793707                   GIVING WMX-TIER-CUR-DAF (2).                   0316967
793708     DIVIDE DAF-DAYS INTO WMX-TIER-CUR-ANN (3)                    0316967
793709                   GIVING WMX-TIER-CUR-DAF (3).                   0316967
793710     DIVIDE DAF-DAYS INTO WMX-TIER-CUR-ANN (4)                    0316967
793711                   GIVING WMX-TIER-CUR-DAF (4).                   0316967
793712     DIVIDE DAF-DAYS INTO WMX-TIER-CUR-ANN (5)                    0316967
793713                   GIVING WMX-TIER-CUR-DAF (5).                   0316967
793714     DIVIDE DAF-DAYS INTO WMX-TIER-CUR-ANN (6)                    0316967
793715                   GIVING WMX-TIER-CUR-DAF (6).                   0316967
793716     DIVIDE DAF-DAYS INTO WMX-TIER-CUR-ANN (7)                    0316967
793717                   GIVING WMX-TIER-CUR-DAF (7).                   0316967
793718     DIVIDE DAF-DAYS INTO WMX-TIER-CUR-ANN (8)                    0316967
793719                   GIVING WMX-TIER-CUR-DAF (8).                   0316967
793720     DIVIDE DAF-DAYS INTO WMX-TIER-CUR-ANN (9)                    0316967
793721                   GIVING WMX-TIER-CUR-DAF (9).                   0316967
793722 CALC-SAVT-CUR-DAF-EXIT. EXIT.                                    0316967
793723 CALC-SAVT-PREV-DAF.                                              0316967
793724     DIVIDE DAF-DAYS INTO WMX-TIER-PREV-ANN (1)                   0316967
793725                   GIVING WMX-TIER-PREV-DAF (1).                  0316967
793726     DIVIDE DAF-DAYS INTO WMX-TIER-PREV-ANN (2)                   0316967
793727                   GIVING WMX-TIER-PREV-DAF (2).                  0316967
793728     DIVIDE DAF-DAYS INTO WMX-TIER-PREV-ANN (3)                   0316967
793729                   GIVING WMX-TIER-PREV-DAF (3).                  0316967
793730     DIVIDE DAF-DAYS INTO WMX-TIER-PREV-ANN (4)                   0316967
793731                   GIVING WMX-TIER-PREV-DAF (4).                  0316967
793732     DIVIDE DAF-DAYS INTO WMX-TIER-PREV-ANN (5)                   0316967
793733                   GIVING WMX-TIER-PREV-DAF (5).                  0316967
793734     DIVIDE DAF-DAYS INTO WMX-TIER-PREV-ANN (6)                   0316967
793735                   GIVING WMX-TIER-PREV-DAF (6).                  0316967
793736     DIVIDE DAF-DAYS INTO WMX-TIER-PREV-ANN (7)                   0316967
793737                   GIVING WMX-TIER-PREV-DAF (7).                  0316967
793738     DIVIDE DAF-DAYS INTO WMX-TIER-PREV-ANN (8)                   0316967
793739                   GIVING WMX-TIER-PREV-DAF (8).                  0316967
793740     DIVIDE DAF-DAYS INTO WMX-TIER-PREV-ANN (9)                   0316967
793741                   GIVING WMX-TIER-PREV-DAF (9).                  0316967
793742 CALC-SAVT-PREV-DAF-EXIT. EXIT.                                   0316967
793743 CALC-DAF-DAYS-DDA.                                               0316967
793744     IF  WBC-LEAP-YEAR EQUAL '1'                                  0316967
793745         MOVE +366 TO DAF-DAYS                                    0316967
793746     ELSE                                                         0316967
793747         MOVE +365 TO DAF-DAYS.                                   0316967
793749     IF  WMS-DAILY-RATE-CODE EQUAL '1'                            0817657
793750         MOVE +360 TO DAF-DAYS.                                   0316967
793751     IF  WBC-LEAP-YEAR EQUAL '1'                                  0316967
793752     AND WMS-DAILY-RATE-CODE EQUAL '0'                            0817657
793753     AND WMS-LEAP-YEAR-FLG EQUAL '0'                              0316967
793754         MOVE +365 TO DAF-DAYS.                                   0316967
793755     IF  WMS-DAILY-RATE-CODE EQUAL '3'                            0817657
793756         MOVE +365 TO DAF-DAYS.                                   0627539
793757 CALC-DAF-DAYS-DDA-EXIT. EXIT.                                    0627539
793758 CALC-OD-ACCR-CUR-DAF.                                            0627539
793759     DIVIDE DAF-DAYS           INTO WMS-OD-CUR-ANN (V)            0627539
793760                             GIVING WMS-OD-CUR-DAF (V).           0627539
793761 CALC-OD-ACCR-CUR-DAF-EXIT.                                       0627539
793762     EXIT.                                                        0627539
793763*                                                                 0627539
793764 CALC-OD-ACCR-PREV-DAF.                                           9915856
793766     DIVIDE DAF-DAYS           INTO WMS-OD-PREV-ANN (V)           9915856
793768                             GIVING WMS-OD-PREV-DAF (V).          9915856
793770 CALC-OD-ACCR-PREV-DAF-EXIT.                                      9915856
793772     EXIT.                                                        9915856
793774*                                                                 9915856
793804 CALC-OD-PRM-CUR-DAF.                                             1003625
793806     DIVIDE DAF-DAYS INTO WMS-OD-PRM-CUR-ANN                      1003625
793808                   GIVING WMS-OD-PRM-CUR-DAF.                     1003625
793810 CALC-OD-PRM-CUR-DAF-EXIT. EXIT.                                  1003625
793812 CALC-OD-PRM-PREV-DAF.                                            1003625
793814     DIVIDE DAF-DAYS INTO WMS-OD-PRM-PREV-ANN                     1003625
793816                   GIVING WMS-OD-PRM-PREV-DAF.                    1003625
793818 CALC-OD-PRM-PREV-DAF-EXIT. EXIT.                                 1003625
793820*                                                                 1003625
793822 CALC-LOAN-CUR-DAF.                                               1003625
793824     IF  WMS-INT-RATE-PTR GREATER THAN +5                         1003625
793826         DIVIDE DAF-DAYS INTO WMS-PRM-CUR-ANN                     1003625
793828                       GIVING WMS-PRM-CUR-DAF                     1003625
793830     ELSE                                                         1003625
793832     IF  WMS-INT-RATE-PTR GREATER THAN +0 AND LESS THAN +6        1003625
793834         DIVIDE DAF-DAYS INTO WMS-LN-CUR-ANN                      1003625
793836                       GIVING WMS-LN-CUR-DAF                      1003625
793838     ELSE                                                         1003625
793840     IF  WMS-SPLIT-RATE-SCHD GREATER THAN +0                      1003625
793842         DIVIDE DAF-DAYS INTO WMS-SPL-CUR-ANN (1)                 1003625
793844                       GIVING WMS-SPL-CUR-DAF (1)                 1003625
793846         DIVIDE DAF-DAYS INTO WMS-SPL-CUR-ANN (2)                 1003625
793848                       GIVING WMS-SPL-CUR-DAF (2)                 1003625
793850         DIVIDE DAF-DAYS INTO WMS-SPL-CUR-ANN (3)                 1003625
793852                       GIVING WMS-SPL-CUR-DAF (3).                1003625
793854 CALC-LOAN-CUR-DAF-EXIT. EXIT.                                    1003625
793856 CALC-LOAN-PREV-DAF.                                              1003625
793858     IF  WMS-INT-RATE-PTR GREATER THAN +5                         1003625
793860         DIVIDE DAF-DAYS INTO WMS-PRM-PREV-ANN                    1003625
793862                       GIVING WMS-PRM-PREV-DAF                    1003625
793864     ELSE                                                         1003625
793866     IF  WMS-INT-RATE-PTR GREATER THAN +0 AND LESS THAN +6        1003625
793868         DIVIDE DAF-DAYS INTO WMS-LN-PREV-ANN                     1003625
793870                       GIVING WMS-LN-PREV-DAF                     1003625
793872     ELSE                                                         1003625
793874     IF  WMS-SPLIT-RATE-SCHD GREATER THAN +0                      1003625
793876         DIVIDE DAF-DAYS INTO WMS-SPL-PREV-ANN (1)                1003625
793878                       GIVING WMS-SPL-PREV-DAF (1)                1003625
793880         DIVIDE DAF-DAYS INTO WMS-SPL-PREV-ANN (2)                1003625
793882                       GIVING WMS-SPL-PREV-DAF (2)                1003625
793884         DIVIDE DAF-DAYS INTO WMS-SPL-PREV-ANN (3)                1003625
793886                       GIVING WMS-SPL-PREV-DAF (3).               1003625
793888 CALC-LOAN-PREV-DAF-EXIT. EXIT.                                   1003625
793890*                                                                 1003625
793892 CALC-LOAN-OLD-CUR-DAF.                                           1003625
793894     IF  WMS-PREV-INT-PTR GREATER THAN +0                         1003625
793896         DIVIDE DAF-DAYS INTO WMS-LN-OLD-CUR-ANN                  1003625
793898                       GIVING WMS-LN-OLD-CUR-DAF                  1003625
793900     ELSE                                                         1003625
793902     IF  WMS-PREV-SPLIT-RATE-PTR GREATER THAN +0                  1003625
793904         DIVIDE DAF-DAYS INTO WMS-SPL-OLD-CUR-ANN (1)             1003625
793906                       GIVING WMS-SPL-OLD-CUR-DAF (1)             1003625
793908         DIVIDE DAF-DAYS INTO WMS-SPL-OLD-CUR-ANN (2)             1003625
793910                       GIVING WMS-SPL-OLD-CUR-DAF (2)             1003625
793912         DIVIDE DAF-DAYS INTO WMS-SPL-OLD-CUR-ANN (3)             1003625
793914                       GIVING WMS-SPL-OLD-CUR-DAF (3).            1003625
793916 CALC-LOAN-OLD-CUR-DAF-EXIT. EXIT.                                1003625
793918 CALC-LOAN-OLD-PREV-DAF.                                          1003625
793920     IF  WMS-PREV-INT-PTR GREATER THAN +0                         1003625
793922         DIVIDE DAF-DAYS INTO WMS-LN-OLD-PREV-ANN                 1003625
793924                       GIVING WMS-LN-OLD-PREV-DAF                 1003625
793926     ELSE                                                         1003625
793928     IF  WMS-PREV-SPLIT-RATE-PTR GREATER THAN +0                  1003625
793930         DIVIDE DAF-DAYS INTO WMS-SPL-OLD-PREV-ANN (1)            1003625
793932                       GIVING WMS-SPL-OLD-PREV-DAF (1)            1003625
793934         DIVIDE DAF-DAYS INTO WMS-SPL-OLD-PREV-ANN (2)            1003625
793936                       GIVING WMS-SPL-OLD-PREV-DAF (2)            1003625
793938         DIVIDE DAF-DAYS INTO WMS-SPL-OLD-PREV-ANN (3)            1003625
793940                       GIVING WMS-SPL-OLD-PREV-DAF (3).           1003625
793942 CALC-LOAN-OLD-PREV-DAF-EXIT. EXIT.                               1003625
793944*                                                                 1003625
793946 CALC-DAF-DAYS-LOAN.                                              1003625
793948     IF  WBC-LEAP-YEAR EQUAL '1'                                  0266741
793950         MOVE +366 TO DAF-DAYS                                    1003625
793952     ELSE                                                         1003625
793954         MOVE +365 TO DAF-DAYS.                                   1003625
793956     IF  WMS-LOAN-360-DAY-FACTOR EQUAL '1'                        0817657
793957         MOVE +360 TO DAF-DAYS.                                   0627547
793958     IF  WMS-LOAN-360-DAY-FACTOR EQUAL '2'                        0817657
793959         MOVE +365 TO DAF-DAYS.                                   0627547
793960 CALC-DAF-DAYS-LOAN-EXIT. EXIT.                                   1003625
793962*                                                                 1003625
793963 CALC-DAF-DAYS-SAV.                                               9715519
793964     IF  WBC-LEAP-YEAR EQUAL '1'                                  0266741
793965         MOVE +366 TO DAF-DAYS                                    9715519
793966     ELSE                                                         9715519
793967         MOVE +365 TO DAF-DAYS.                                   9715519
793969     IF  WMS-DAILY-RATE-CODE EQUAL '1'                            0817657
793970         MOVE +360 TO DAF-DAYS.                                   9715519
793971     IF  WBC-LEAP-YEAR EQUAL '1'                                  0266741
793972     AND WMS-DAILY-RATE-CODE EQUAL '0'                            0817657
793973     AND WMS-INT-LEAP-YEAR-FLG EQUAL '0'                          9715519
793974         MOVE +365 TO DAF-DAYS.                                   9715519
793975     IF  WMS-DAILY-RATE-CODE EQUAL '3'                            0817657
793976         MOVE +365 TO DAF-DAYS.                                   0627539
793977 CALC-DAF-DAYS-SAV-EXIT. EXIT.                                    0627539
793978*                                                                 0627539
794012 CALC-RNX-DAF.                                                    1003625
794014*--------------------------------------------------------------*  1003625
794016* DETERMINE NUMBER OF DAYS TO COMPUTE DAILY FACTOR, ALSO          1003625
794018* IF BACKDATING HAS GONE PAST A YEAR BOUNDRY DETERMINE NUMBER     1003625
794020* OF DAYS FOR LAST YEAR.                                          1003625
794022*--------------------------------------------------------------*  1003625
794023     IF  WBC-LEAP-YEAR EQUAL '1'                                  0817657
794024         MOVE +366 TO DAF-DAYS                                    0817657
794025     ELSE                                                         0817657
794026         MOVE +365 TO DAF-DAYS.                                   0817657
794027     IF  RMA-TIERED                                               0817657
794028         IF  WMS-LEAP-YEAR-FLG EQUAL '0'                          0817657
794029         OR  WMS-DAILY-RATE-CODE EQUAL '3'                        0817657
794030             MOVE +365 TO DAF-DAYS.                               0817657
794031     IF  RMA-TIERED                                               0817657
794032         IF  WMS-DAILY-RATE-CODE EQUAL '1'                        0817657
794033             MOVE +360 TO DAF-DAYS.                               0817657
794034     IF  RMA-IOD-SAV                                              9715519
794035     AND WMS-DAILY-RATE-CODE EQUAL '0'                            0817657
794036     AND IOD-FLAG EQUAL 'N'                                       9715519
794037         IF  WMS-LEAP-YEAR-FLG EQUAL '0'                          9715519
794038             MOVE +365 TO DAF-DAYS.                               9715519
794039     IF  RMA-IOD-SAV                                              9715519
794040     AND WMS-DAILY-RATE-CODE EQUAL '0'                            0817657
794041     AND IOD-FLAG EQUAL 'F'                                       9715519
794042         IF  WMS-INT-LEAP-YEAR-FLG EQUAL '0'                      9715519
794043             MOVE +365 TO DAF-DAYS.                               9715519
794044     IF  RMA-IOD-SAV                                              9715519
794045         IF  WMS-DAILY-RATE-CODE EQUAL '1'                        0817657
794046             MOVE +360 TO DAF-DAYS                                9715519
794047             GO TO CALC-RNX-CONT                                  9715519
794048         ELSE                                                     9715519
794049         IF  WMS-DAILY-RATE-CODE EQUAL '3'                        0817657
794050             MOVE +365 TO DAF-DAYS                                0627539
794051             GO TO CALC-RNX-CONT                                  0627539
794052         ELSE                                                     0627539
794053             IF (WMS-DAILY-RATE-CODE EQUAL '2'                    0817657
794054             AND WBC-DAILY-RATE-PHASE GREATER THAN SPACES)        0817657
794055                 MOVE RNX-PTR-ANN  TO WK-LIR-ANNUAL               0817657
794056                 MOVE WBC-DAILY-RATE-PHASE TO IM31-PH2            0817657
794057                 CALL 'SILINK' USING IM31-PHASE                   0817657
794058                                     RATE-WORK-AREA               0817657
794059                 MOVE WK-LIR-DAILY TO RNX-PTR-DAF                 0817657
794060                 GO TO CALC-RNX-DAF-EXIT.                         0817657
794061     IF  RMA-LOANS                                                0817657
794062     OR  RMA-OD-ACCR                                              0817657
794063     OR  RMA-SPLIT-LOANS                                          0817657
794064     OR  RMA-PRIME                                                0817657
794065         IF  WMS-LOAN-360-DAY-FACTOR EQUAL '1'                    0817657
794066             MOVE +360 TO DAF-DAYS                                0817657
794067             GO TO CALC-RNX-CONT                                  0817657
794068         ELSE                                                     0817657
794069         IF  WMS-LOAN-360-DAY-FACTOR EQUAL '2'                    0817657
794070             MOVE +365 TO DAF-DAYS                                0817657
794071             GO TO CALC-RNX-CONT.                                 0817657
794072     IF  RMA-LOANS                                                0817657
794073     OR  RMA-OD-ACCR                                              0817657
794074     OR  RMA-IOD-SAV                                              1003625
794076         MOVE RNX-PTR-YR TO RNX-WS-YR.                            1003625
794078     IF  RMA-PRIME                                                1003625
794080         MOVE RNX-PRM-YR TO RNX-WS-YR.                            1003625
794082     IF  RMA-SPLIT-LOANS                                          1003625
794084         MOVE RNX-SPL-YR TO RNX-WS-YR.                            1003625
794086     IF  RMA-TIERED                                               1003625
794088         MOVE RNX-FND-YR TO RNX-WS-YR.                            1003625
794090     IF  RNX-WS-YR NOT EQUAL WBC-CAPTURE-YR                       0266741
794092         MOVE '12' TO DT-L-MO                                     1003625
794094         MOVE '31' TO DT-L-DA                                     1003625
794096         MOVE RNX-WS-YR TO DT-L-YR                                1003625
794098         CALL 'SIDJUL' USING DATE-AREA                            1003625
794100         MOVE DT-JUL-DA TO DAF-DAYS                               9715519
794101         IF  DAF-DAYS EQUAL +366                                  9715519
794102             PERFORM CALC-RNX-LEAP THRU CALC-RNX-LEAP-EXIT.       9715519
794103 CALC-RNX-CONT.                                                   9715519
794104     IF  RMA-LOANS                                                1003625
794106     OR  RMA-OD-ACCR                                              1003625
794108     OR  RMA-IOD-SAV                                              1003625
794110         DIVIDE DAF-DAYS INTO RNX-PTR-ANN                         1003625
794112                       GIVING RNX-PTR-DAF.                        1003625
794114     IF  RMA-PRIME                                                1003625
794116         DIVIDE DAF-DAYS INTO RNX-PRM-ANN                         1003625
794118                       GIVING RNX-PRM-DAF.                        1003625
794120     IF  RMA-SPLIT-LOANS                                          1003625
794122         DIVIDE DAF-DAYS INTO RNX-SPL-ANN (1)                     1003625
794124                       GIVING RNX-SPL-DAF (1)                     1003625
794126         DIVIDE DAF-DAYS INTO RNX-SPL-ANN (2)                     1003625
794128                       GIVING RNX-SPL-DAF (2)                     1003625
794130         DIVIDE DAF-DAYS INTO RNX-SPL-ANN (3)                     1003625
794132                       GIVING RNX-SPL-DAF (3).                    1003625
794134     IF  RMA-TIERED                                               1003625
794136         DIVIDE DAF-DAYS INTO RNX-FND-ANN (1)                     1003625
794138                       GIVING RNX-FND-DAF (1)                     1003625
794140         DIVIDE DAF-DAYS INTO RNX-FND-ANN (2)                     1003625
794142                       GIVING RNX-FND-DAF (2)                     1003625
794144         DIVIDE DAF-DAYS INTO RNX-FND-ANN (3)                     1003625
794146                       GIVING RNX-FND-DAF (3)                     1003625
794148         DIVIDE DAF-DAYS INTO RNX-FND-ANN (4)                     1003625
794150                       GIVING RNX-FND-DAF (4)                     1003625
794152         DIVIDE DAF-DAYS INTO RNX-FND-ANN (5)                     1003625
794154                       GIVING RNX-FND-DAF (5)                     1003625
794156         DIVIDE DAF-DAYS INTO RNX-FND-ANN (6)                     1003625
794158                       GIVING RNX-FND-DAF (6)                     1003625
794160         DIVIDE DAF-DAYS INTO RNX-FND-ANN (7)                     1003625
794162                       GIVING RNX-FND-DAF (7)                     1003625
794164         DIVIDE DAF-DAYS INTO RNX-FND-ANN (8)                     1003625
794166                       GIVING RNX-FND-DAF (8)                     1003625
794168         DIVIDE DAF-DAYS INTO RNX-FND-ANN (9)                     1003625
794170                       GIVING RNX-FND-DAF (9).                    1003625
794172 CALC-RNX-DAF-EXIT. EXIT.                                         1003625
794174                                                                  1003625
794175 CALC-RNX-LEAP.                                                   9715519
794176     IF  RMA-IOD-SAV                                              9715519
794177     OR  RMA-TIERED                                               9715519
794178         NEXT SENTENCE                                            9715519
794179     ELSE                                                         9715519
794180         GO TO CALC-RNX-LEAP-EXIT.                                9715519
794181     IF  IOD-FLAG EQUAL 'N'                                       9715519
794182         MOVE WMS-DATE-OPENED TO WK-OPEN-DATE                     9715519
794183     ELSE                                                         9715519
794184         MOVE WMS-INT-DATE-OPENED TO WK-OPEN-DATE.                9715519
794185     IF  RNX-WS-YR NOT EQUAL WK-OPEN-YR                           9715519
794186         GO TO CALC-RNX-LEAP-EXIT.                                9715519
794187     MOVE RNX-WS-YR TO WK-LEAP-YR.                                9715519
794188     MOVE '1' TO WK-LEAP-YEAR.                                    9715519
794189     PERFORM CHECK-366 THRU C366-EXIT.                            9715519
794190     IF  WK-RETURN-FLG EQUAL '0'                                  9715519
794191         MOVE +365 TO DAF-DAYS.                                   9715519
794192     MOVE WBC-CAPTURE-YR TO WK-LEAP-YR.                           0266741
794193     MOVE WBC-LEAP-YEAR TO WK-LEAP-YEAR.                          0266741
794194                                                                  9715519
794195 CALC-RNX-LEAP-EXIT.                                              9715519
794196     EXIT.                                                        9715519
794197                                                                  9715519
794198     COPY IMPDLPYR.                                               9715519
794199*                                                                 1105232
794200     COPY IMPDAPYE.                                               1105232
794201*                                                                 1105232
794202     COPY IMPD31LP.                                               9915577
794203*                                                                 9915577
794204*CALCULATE A NEW DAF IF THE RATE BEING ADDED TO AN ACCOUNT HAS    9915577
794205*CROSSED A YEAR BOUNDRY.                                          9915577
794206*                                                                 1003625
794208 VERIFY-YR-BOUNDRY-FOR-TIER.                                      1003625
794209     IF (WMS-DAILY-RATE-CODE EQUAL '1' OR '3')                    0817657
794210         GO TO VERIFY-TIER-EXIT.                                  0627539
794211     IF  RMA-CUR-TIERED-YR NOT EQUAL WBC-CAPTURE-YR               0627539
794212         IF  INTO-OR-OUTOF-LEAP-YR                                1003625
794214             MOVE WMS-TIER-CUR-DATA TO WMS-TIER-PREV-DATA         1003625
794216             MOVE WS-NEW-YEAR-DATE TO WMS-TIER-CUR-DATE           1003625
794218             PERFORM CALC-TIER-CUR-DAF THRU CALC-TIER-CUR-DAF-EXIT1003625
794220             GO TO VERIFY-TIER-EXIT                               1003625
794222         ELSE                                                     1003625
794224             GO TO VERIFY-TIER-EXIT.                              1003625
794226     IF  RMA-PREV-TIERED-YR NOT EQUAL WBC-CAPTURE-YR              0266741
794227     AND RMA-PREV-TIERED-YR NOT EQUAL SPACES                      9715504
794228         IF  INTO-OR-OUTOF-LEAP-YR                                1003625
794230             MOVE WS-NEW-YEAR-DATE TO WMS-TIER-PREV-DATE          1003625
794232             PERFORM CALC-TIER-PREV-DAF                           1003625
794234                THRU CALC-TIER-PREV-DAF-EXIT.                     1003625
794236 VERIFY-TIER-EXIT.                                                1003625
794238     EXIT.                                                        1003625
794240                                                                  1003625
794242 VERIFY-YR-BOUNDRY-FOR-DFLT-IOD.                                  1003625
794244     IF (WMS-DAILY-RATE-CODE EQUAL '1' OR '3')                    0817657
794246         GO TO VERIFY-DFLT-IOD-EXIT.                              1003625
794248     IF  RMA-CUR-ILO-YR NOT EQUAL WBC-CAPTURE-YR                  0266741
794250         IF  INTO-OR-OUTOF-LEAP-YR                                1003625
794252             MOVE WMS-DFLT-IOD-CUR-DATA TO WMS-DFLT-IOD-PREV-DATA 1003625
794254             MOVE WS-NEW-YEAR-DATE TO WMS-DFLT-IOD-CUR-DATE       1003625
794256             PERFORM CALC-DFLT-IOD-CUR-DAF                        1003625
794258                THRU CALC-DFLT-IOD-CUR-DAF-EXIT                   1003625
794260             GO TO VERIFY-DFLT-IOD-EXIT                           1003625
794262         ELSE                                                     1003625
794264             GO TO VERIFY-DFLT-IOD-EXIT.                          1003625
794266     IF  RMA-PREV-ILO-YR NOT EQUAL WBC-CAPTURE-YR                 0266741
794267     AND RMA-PREV-ILO-YR NOT EQUAL SPACES                         9715504
794268         IF  INTO-OR-OUTOF-LEAP-YR                                1003625
794270             MOVE WS-NEW-YEAR-DATE TO WMS-DFLT-IOD-PREV-DATE      1003625
794272             PERFORM CALC-DFLT-IOD-PREV-DAF                       1003625
794274                THRU CALC-DFLT-IOD-PREV-DAF-EXIT.                 1003625
794276 VERIFY-DFLT-IOD-EXIT.                                            1003625
794278     EXIT.                                                        1003625
794280                                                                  1003625
794282 VERIFY-YR-BOUNDRY-FOR-IOD.                                       1003625
794284     IF (WMS-DAILY-RATE-CODE EQUAL '1' OR '3')                    0817657
794286         GO TO VERIFY-IOD-EXIT.                                   1003625
794288     IF  RMA-CUR-ILO-YR NOT EQUAL WBC-CAPTURE-YR                  0266741
794290         IF  INTO-OR-OUTOF-LEAP-YR                                1003625
794292             MOVE WMS-IOD-CUR-DATA TO WMS-IOD-PREV-DATA           1003625
794294             MOVE WS-NEW-YEAR-DATE TO WMS-IOD-CUR-DATE            1003625
794296             PERFORM CALC-IOD-CUR-DAF THRU CALC-IOD-CUR-DAF-EXIT  1003625
794298             GO TO VERIFY-IOD-EXIT                                1003625
794300         ELSE                                                     1003625
794302             GO TO VERIFY-IOD-EXIT.                               1003625
794304     IF  RMA-PREV-ILO-YR NOT EQUAL WBC-CAPTURE-YR                 0266741
794305     AND RMA-PREV-ILO-YR NOT EQUAL SPACES                         9715504
794306         IF  INTO-OR-OUTOF-LEAP-YR                                1003625
794308             MOVE WS-NEW-YEAR-DATE TO WMS-IOD-PREV-DATE           1003625
794310             PERFORM CALC-IOD-PREV-DAF                            1003625
794312                THRU CALC-IOD-PREV-DAF-EXIT.                      1003625
794314 VERIFY-IOD-EXIT.                                                 1003625
794316     EXIT.                                                        1003625
794317                                                                  0316967
794318 VERIFY-YR-BOUNDRY-FOR-SAV.                                       0316967
794319     IF (WMS-DAILY-RATE-CODE EQUAL '1' OR '3')                    0817657
794320         GO TO VERIFY-SAV-EXIT.                                   0316967
794321     IF  RMA-CUR-ILO-YR NOT EQUAL WBC-CAPTURE-YR                  0316967
794322         IF  INTO-OR-OUTOF-LEAP-YR                                0316967
794323             MOVE WMS-SAV-CUR-DATA TO WMS-SAV-PREV-DATA           0316967
794324             MOVE WS-NEW-YEAR-DATE TO WMS-SAV-CUR-DATE            0316967
794325             PERFORM CALC-SAV-CUR-DAF THRU CALC-SAV-CUR-DAF-EXIT  0316967
794326             GO TO VERIFY-SAV-EXIT                                0316967
794327         ELSE                                                     0316967
794328             GO TO VERIFY-SAV-EXIT.                               0316967
794329     IF  RMA-PREV-ILO-YR NOT EQUAL WBC-CAPTURE-YR                 0316967
794330     AND RMA-PREV-ILO-YR NOT EQUAL SPACES                         0316967
794331         IF  INTO-OR-OUTOF-LEAP-YR                                0316967
794332             MOVE WS-NEW-YEAR-DATE TO WMS-SAV-PREV-DATE           0316967
794333             PERFORM CALC-SAV-PREV-DAF                            0316967
794334                THRU CALC-SAV-PREV-DAF-EXIT.                      0316967
794335 VERIFY-SAV-EXIT.                                                 0316967
794336     EXIT.                                                        0316967
794337                                                                  0316967
794338 VERIFY-YR-BOUNDRY-FOR-SAVT.                                      0316967
794339     IF (WMS-DAILY-RATE-CODE EQUAL '1' OR '3')                    0817657
794340         GO TO VERIFY-SAVT-EXIT.                                  0627539
794341     IF  RMA-CUR-TIERED-YR NOT EQUAL WBC-CAPTURE-YR               0627539
794342         IF  INTO-OR-OUTOF-LEAP-YR                                0627539
794343             MOVE WMX-TIER-CUR-DATA TO WMX-TIER-PREV-DATA         0627539
794344             MOVE WS-NEW-YEAR-DATE TO WMX-TIER-CUR-DATE           0627539
794345             PERFORM CALC-SAVT-CUR-DAF THRU CALC-SAVT-CUR-DAF-EXIT0627539
794346             GO TO VERIFY-SAVT-EXIT                               0627539
794347         ELSE                                                     0627539
794348             GO TO VERIFY-SAVT-EXIT.                              0627539
794349     IF  RMA-PREV-TIERED-YR NOT EQUAL WBC-CAPTURE-YR              0627539
794350     AND RMA-PREV-TIERED-YR NOT EQUAL SPACES                      0627539
794351         IF  INTO-OR-OUTOF-LEAP-YR                                0627539
794352             MOVE WS-NEW-YEAR-DATE TO WMX-TIER-PREV-DATE          0627539
794353             PERFORM CALC-SAVT-PREV-DAF                           0627539
794354                THRU CALC-SAVT-PREV-DAF-EXIT.                     0627539
794355 VERIFY-SAVT-EXIT.                                                0627539
794356     EXIT.                                                        0627539
794357                                                                  0627539
794358 VERIFY-YR-BOUNDRY-FOR-LOAN.                                      1003625
794360     IF (WMS-LOAN-360-DAY-FACTOR EQUAL '1' OR '2')                0817657
794362         GO TO VERIFY-LOAN-EXIT.                                  1003625
794364     IF  RMA-CUR-ILO-YR NOT EQUAL WBC-CAPTURE-YR                  0266741
794366         IF  INTO-OR-OUTOF-LEAP-YR                                1003625
794368             MOVE WMS-LN-CUR-DATA  TO WMS-LN-PREV-DATA            1003625
794370             MOVE WS-NEW-YEAR-DATE TO WMS-LN-CUR-DATE             1003625
794372             PERFORM CALC-LOAN-CUR-DAF                            1003625
794374                THRU CALC-LOAN-CUR-DAF-EXIT                       1003625
794376             GO TO VERIFY-LOAN-EXIT                               1003625
794378         ELSE                                                     1003625
794380             GO TO VERIFY-LOAN-EXIT.                              1003625
794382     IF  RMA-PREV-ILO-YR NOT EQUAL WBC-CAPTURE-YR                 0266741
794383     AND RMA-PREV-ILO-YR NOT EQUAL SPACES                         9715504
794384         IF  INTO-OR-OUTOF-LEAP-YR                                1003625
794386             MOVE WS-NEW-YEAR-DATE TO WMS-LN-PREV-DATE            1003625
794388             PERFORM CALC-LOAN-PREV-DAF                           1003625
794390                THRU CALC-LOAN-PREV-DAF-EXIT.                     1003625
794392 VERIFY-LOAN-EXIT.                                                1003625
794394     EXIT.                                                        1003625
794396                                                                  1003625
794398 VERIFY-YR-BOUNDRY-FOR-PRIME.                                     1003625
794400     IF (WMS-LOAN-360-DAY-FACTOR EQUAL '1' OR '2')                0817657
794402         GO TO VERIFY-PRIME-EXIT.                                 1003625
794404     IF  RMA-CUR-PRIME-YR NOT EQUAL WBC-CAPTURE-YR                0266741
794406         IF  INTO-OR-OUTOF-LEAP-YR                                1003625
794408             MOVE WMS-PRM-CUR-DATA TO WMS-PRM-PREV-DATA           1003625
794410             MOVE WS-NEW-YEAR-DATE TO WMS-PRM-CUR-DATE            1003625
794412             PERFORM CALC-LOAN-CUR-DAF                            1003625
794414                THRU CALC-LOAN-CUR-DAF-EXIT                       1003625
794416             GO TO VERIFY-PRIME-EXIT                              1003625
794418         ELSE                                                     1003625
794420             GO TO VERIFY-PRIME-EXIT.                             1003625
794422     IF  RMA-PREV-PRIME-YR NOT EQUAL WBC-CAPTURE-YR               0266741
794423     AND RMA-PREV-PRIME-YR NOT EQUAL SPACES                       9715504
794424         IF  INTO-OR-OUTOF-LEAP-YR                                1003625
794426             MOVE WS-NEW-YEAR-DATE TO WMS-PRM-PREV-DATE           1003625
794428             PERFORM CALC-LOAN-PREV-DAF                           1003625
794430                THRU CALC-LOAN-PREV-DAF-EXIT.                     1003625
794432 VERIFY-PRIME-EXIT.                                               1003625
794434     EXIT.                                                        1003625
794436                                                                  1003625
794438 VERIFY-YR-BOUNDRY-FOR-SPLIT.                                     1003625
794440     IF (WMS-LOAN-360-DAY-FACTOR EQUAL '1' OR '2')                0817657
794442         GO TO VERIFY-SPLIT-EXIT.                                 1003625
794444     IF  RMA-CUR-SPLIT-YR NOT EQUAL WBC-CAPTURE-YR                0266741
794446         IF  INTO-OR-OUTOF-LEAP-YR                                1003625
794448             MOVE WMS-SPL-CUR-DATA TO WMS-SPL-PREV-DATA           1003625
794450             MOVE WS-NEW-YEAR-DATE TO WMS-SPL-CUR-DATE            1003625
794452             PERFORM CALC-LOAN-CUR-DAF                            1003625
794454                THRU CALC-LOAN-CUR-DAF-EXIT                       1003625
794456             GO TO VERIFY-SPLIT-EXIT                              1003625
794458         ELSE                                                     1003625
794460             GO TO VERIFY-SPLIT-EXIT.                             1003625
794462     IF  RMA-PREV-SPLIT-YR NOT EQUAL WBC-CAPTURE-YR               0266741
794463     AND RMA-PREV-SPLIT-YR NOT EQUAL SPACES                       9715504
794464         IF  INTO-OR-OUTOF-LEAP-YR                                1003625
794466             MOVE WS-NEW-YEAR-DATE TO WMS-SPL-PREV-DATE           1003625
794468             PERFORM CALC-LOAN-PREV-DAF                           1003625
794470                THRU CALC-LOAN-PREV-DAF-EXIT.                     1003625
794472 VERIFY-SPLIT-EXIT.                                               1003625
794474     EXIT.                                                        1003625
794476                                                                  1003625
794478 VERIFY-YR-BOUNDRY-FOR-OLD-LOAN.                                  1003625
794480     IF (WMS-LOAN-360-DAY-FACTOR EQUAL '1' OR '2')                0817657
794482         GO TO VERIFY-OLD-LOAN-EXIT.                              1003625
794484     IF  RMA-CUR-ILO-YR NOT EQUAL WBC-CAPTURE-YR                  0266741
794486         IF  INTO-OR-OUTOF-LEAP-YR                                1003625
794488             MOVE WMS-LN-OLD-CUR-DATA TO WMS-LN-OLD-PREV-DATA     1003625
794490             MOVE WS-NEW-YEAR-DATE    TO WMS-LN-OLD-CUR-DATE      1003625
794492             PERFORM CALC-LOAN-OLD-CUR-DAF                        1003625
794494                THRU CALC-LOAN-OLD-CUR-DAF-EXIT                   1003625
794496             GO TO VERIFY-OLD-LOAN-EXIT                           1003625
794498         ELSE                                                     1003625
794500             GO TO VERIFY-OLD-LOAN-EXIT.                          1003625
794502     IF  RMA-PREV-ILO-YR NOT EQUAL WBC-CAPTURE-YR                 0266741
794503     AND RMA-PREV-ILO-YR NOT EQUAL SPACES                         9715504
794504         IF  INTO-OR-OUTOF-LEAP-YR                                1003625
794506             MOVE WS-NEW-YEAR-DATE TO WMS-LN-OLD-PREV-DATE        1003625
794508             PERFORM CALC-LOAN-OLD-PREV-DAF                       1003625
794510                THRU CALC-LOAN-OLD-PREV-DAF-EXIT.                 1003625
794512 VERIFY-OLD-LOAN-EXIT.                                            1003625
794514     EXIT.                                                        1003625
794516                                                                  1003625
794518 VERIFY-YR-BOUNDRY-FOR-OLD-SPL.                                   1003625
794520     IF (WMS-LOAN-360-DAY-FACTOR EQUAL '1' OR '2')                0817657
794522         GO TO VERIFY-OLD-SPLIT-EXIT.                             1003625
794524     IF  RMA-CUR-SPLIT-YR NOT EQUAL WBC-CAPTURE-YR                0266741
794526         IF  INTO-OR-OUTOF-LEAP-YR                                1003625
794528             MOVE WMS-SPL-CUR-DATA TO WMS-SPL-PREV-DATA           1003625
794530             MOVE WS-NEW-YEAR-DATE TO WMS-SPL-CUR-DATE            1003625
794532             PERFORM CALC-LOAN-OLD-CUR-DAF                        1003625
794534                THRU CALC-LOAN-OLD-CUR-DAF-EXIT                   1003625
794536             GO TO VERIFY-OLD-SPLIT-EXIT                          1003625
794538         ELSE                                                     1003625
794540             GO TO VERIFY-OLD-SPLIT-EXIT.                         1003625
794542     IF  RMA-PREV-SPLIT-YR NOT EQUAL WBC-CAPTURE-YR               0266741
794543     AND RMA-PREV-SPLIT-YR NOT EQUAL SPACES                       9715504
794544         IF  INTO-OR-OUTOF-LEAP-YR                                1003625
794546             MOVE WS-NEW-YEAR-DATE TO WMS-SPL-PREV-DATE           1003625
794548             PERFORM CALC-LOAN-OLD-PREV-DAF                       1003625
794550                THRU CALC-LOAN-OLD-PREV-DAF-EXIT.                 1003625
794552 VERIFY-OLD-SPLIT-EXIT.                                           1003625
794554     EXIT.                                                        1003625
794556*                                                                 1003625
794558 VERIFY-YR-BOUNDRY-FOR-OD-ACCR.                                   9915856
794560     IF (WMS-LOAN-360-DAY-FACTOR EQUAL '1' OR '2')                0817657
794562         GO TO VERIFY-OD-ACCR-EXIT.                               9915856
794563*                                                                 9915856
794564     IF  RMA-CUR-ILO-YR NOT EQUAL WBC-CAPTURE-YR                  0266741
794566         IF  INTO-OR-OUTOF-LEAP-YR                                1003625
794568             MOVE WMS-OD-CUR-DATA (V)  TO WMS-OD-PREV-DATA (V)    9915856
794570             MOVE WS-NEW-YEAR-DATE     TO WMS-OD-CUR-DATE (V)     9915856
794572             PERFORM CALC-OD-ACCR-CUR-DAF                         9915856
794574                THRU CALC-OD-ACCR-CUR-DAF-EXIT                    9915856
794576             GO TO VERIFY-OD-ACCR-EXIT                            9915856
794578         ELSE                                                     1003625
794580             GO TO VERIFY-OD-ACCR-EXIT.                           9915856
794581*                                                                 9915856
794582     IF  RMA-PREV-ILO-YR NOT EQUAL WBC-CAPTURE-YR                 0266741
794583     AND RMA-PREV-ILO-YR NOT EQUAL SPACES                         9715504
794584         IF  INTO-OR-OUTOF-LEAP-YR                                1003625
794586             MOVE WS-NEW-YEAR-DATE TO WMS-OD-PREV-DATE (V)        9915856
794588             PERFORM CALC-OD-ACCR-PREV-DAF                        9915856
794590                THRU CALC-OD-ACCR-PREV-DAF-EXIT.                  9915856
794592 VERIFY-OD-ACCR-EXIT.                                             9915856
794594     EXIT.                                                        1003625
794596*                                                                 9915856
794678 VERIFY-YR-BOUNDRY-FOR-OD-PRIME.                                  1003625
794680     IF (WMS-LOAN-360-DAY-FACTOR EQUAL '1' OR '2')                0817657
794682         GO TO VERIFY-OD-PRIME-EXIT.                              1003625
794684     IF  RMA-CUR-PRIME-YR NOT EQUAL WBC-CAPTURE-YR                0266741
794686         IF  INTO-OR-OUTOF-LEAP-YR                                1003625
794688             MOVE WMS-OD-PRM-CUR-DATA  TO WMS-OD-PRM-PREV-DATA    1003625
794690             MOVE WS-NEW-YEAR-DATE     TO WMS-OD-PRM-CUR-DATE     1003625
794692             PERFORM CALC-OD-PRM-CUR-DAF                          1003625
794694                THRU CALC-OD-PRM-CUR-DAF-EXIT                     1003625
794696             GO TO VERIFY-OD-PRIME-EXIT                           1003625
794698         ELSE                                                     1003625
794700             GO TO VERIFY-OD-PRIME-EXIT.                          1003625
794702     IF  RMA-PREV-PRIME-YR NOT EQUAL WBC-CAPTURE-YR               0266741
794703     AND RMA-PREV-PRIME-YR NOT EQUAL SPACES                       2016602
794704         IF  INTO-OR-OUTOF-LEAP-YR                                1003625
794706             MOVE WS-NEW-YEAR-DATE TO WMS-OD-PRM-PREV-DATE        1003625
794708             PERFORM CALC-OD-PRM-PREV-DAF                         1003625
794710                THRU CALC-OD-PRM-PREV-DAF-EXIT.                   1003625
794712 VERIFY-OD-PRIME-EXIT.                                            1003625
794714     EXIT.                                                        1003625
794716*                                                                 9915828
794720 CHECK-W8-STATUS.                                                 9915828
794722***** CERT-NAME-W8-STATUS:                                        9915828
794724*****  0 - CITIZEN                                                9915828
794726*****  1 - NON RESIDENT ALIEN WITH CERTIFIED W8                   9915828
794728*****  2 - NON RESIDENT ALIEN UNCERTIFIED                         9915828
794730     IF  WMS-TIN-CERTIFICATION NOT EQUAL 8                        9915828
794731         MOVE '0' TO CERT-NAME-W8-STATUS                          9915828
794732         GO TO CHECK-W8-EXIT.                                     9915828
794733     IF  WMS-INFO-TRLR NOT EQUAL '1'                              9915828
794734         MOVE '2' TO CERT-NAME-W8-STATUS                          9915828
794735         GO TO CHECK-W8-EXIT.                                     9915828
794736     IF  (WMS-NRA-CERT-NAME1 EQUAL '1' OR '3')                    0817699
794740         MOVE '1' TO CERT-NAME-W8-STATUS                          0817699
794742     ELSE                                                         0817699
794744         MOVE '2' TO CERT-NAME-W8-STATUS.                         0817699
794764 CHECK-W8-EXIT.                                                   9915828
794766     EXIT.                                                        9915828
794768                                                                  9915828
794770 GET-BONUS-RATES.                                                 2016374
794772     IF  BRT-CNT EQUAL +100                                       2016374
794774         MOVE +0 TO BRT-CNT                                       2016374
794776         GO TO GET-BONUS-EXIT.                                    2016374
794778     ADD +1 TO BRT-CNT.                                           2016374
794780     IF  IMBR-PRODUCT-TYPE (BRT-CNT) EQUAL SPACES                 2016374
794782         MOVE +0 TO BRT-CNT                                       2016374
794784         GO TO GET-BONUS-EXIT.                                    2016374
794786     IF  IMBR-PRODUCT-TYPE (BRT-CNT) EQUAL WMS-ACCT-TYPE          2016374
794788         MOVE IMBR-ENTRY (BRT-CNT) TO WORK-BRT-AREA               2016374
794790         GO TO GET-BONUS-EXIT.                                    2016374
794792     GO TO GET-BONUS-RATES.                                       2016374
794794 GET-BONUS-EXIT.  EXIT.                                           2016374
794800*----------------------------------------------------------------*9915749
794810*    CREATE INCORE TABLES FOR SERVICE CHARGE ROUTINES.           *9915749
794820*----------------------------------------------------------------*9915749
794830 Y2000-BUILD-FEE-XREF.                                            9915749
794835     MOVE +0                     TO WK-DEP-ACCM.                  9915749
794840     SET NBR-INX TO 1.                                            9915749
794850     SET FEE-INX TO 1.                                            9915749
794860     PERFORM Y2000-INIT-LOOP 200 TIMES.                           9915749
794870     MOVE 0 TO FEE-NBR-MAX.                                       9915749
794880     MOVE 0 TO FEE-ACCUM-MAX.                                     9915749
794890     SET NBR-INX TO 1.                                            9915749
794900     SET NBR-INX DOWN BY 1.                                       9915749
794905*                                                                 9915749
794910 Y2001-BUILD-FEE-XREF.                                            9915749
794915     IF  IMFTC-1ST-RD-SW EQUAL '1'                                9916298
794920         MOVE '0' TO IMFTC-1ST-RD-SW                              9916298
794925         PERFORM Y2000-READ-FTC.                                  9916298
794930*                                                                 9916298
794935     IF  IMFTC-EOF-SW EQUAL '1'                                   9916298
794940         GO TO Y2000-BUILD-FEE-EXIT.                              9915749
794945*                                                                 9915749
794950     IF  FTC-CONTROL-1 LESS THAN HOLD-1                           9915749
794953         PERFORM Y2000-READ-FTC                                   9916298
794955         GO TO Y2001-BUILD-FEE-XREF.                              9915749
794960*                                                                 9915749
794965     IF  FTC-CONTROL-1 GREATER THAN HOLD-1                        9915749
794970         GO TO Y2000-BUILD-FEE-EXIT.                              9915749
794975*                                                                 9915749
794980     IF  FTC-CONTROL-2 LESS THAN HOLD-2                           9915749
794983         PERFORM Y2000-READ-FTC                                   9916298
794985         GO TO Y2001-BUILD-FEE-XREF.                              9915749
794990*                                                                 9915749
794995     IF  FTC-CONTROL-2 GREATER THAN HOLD-2                        9915749
795000         GO TO Y2000-BUILD-FEE-EXIT.                              9915749
795005*                                                                 9915749
795010     IF  FTC-CONTROL-3 LESS THAN HOLD-3                           9915749
795013         PERFORM Y2000-READ-FTC                                   9916298
795015         GO TO Y2001-BUILD-FEE-XREF.                              9915749
795020*                                                                 9915749
795025     IF  FTC-CONTROL-3 GREATER THAN HOLD-3                        9915749
795030         GO TO Y2000-BUILD-FEE-EXIT.                              9915749
795035*                                                                 9915749
795040     IF  FTC-SRV-CHG-ACC-1 NOT NUMERIC                            9915749
795043         PERFORM Y2000-READ-FTC                                   9916298
795045         GO TO Y2001-BUILD-FEE-XREF.                              9915749
795050*                                                                 9915749
795055     MOVE FTC-SRV-CHG-ACC-1      TO WS-SRV-CHG-ACC.               9915749
795060     IF  FEE-ACCUM-MAX LESS THAN WS-SRV-CHG-ACC                   9915749
795065         MOVE WS-SRV-CHG-ACC     TO FEE-ACCUM-MAX.                9915749
795070*                                                                 9915749
795075     SET FEE-INX                 TO WS-SRV-CHG-ACC.               9915749
795077     IF  FTC-SRV-CHG-ACC-1 NOT EQUAL SPACES AND ZEROS             0827811
795080         MOVE FTC-FEE-TYPE       TO FEE-ACCUM-TYPE (FEE-INX)      0827811
795085         MOVE FTC-FEE-NUMBER     TO FEE-ACCUM-NBR (FEE-INX)       0827811
795090         MOVE FTC-SRV-CHG-ACC-2  TO FEE-ACCUM-2 (FEE-INX)         0827811
795095         MOVE ' '                TO FEE-ACCUM-COMPL-IND (FEE-INX).0827811
795100     MOVE WBC-NB-SC-DEPOSIT-FEE  TO HOLD-FEE-TYPE-NBR.            0266741
795105     IF  FTC-FEE-NUMBER EQUAL HOLD-FEE-NBR                        9915749
795110         MOVE FTC-SRV-CHG-ACC-1  TO WK-DEP-ACCM.                  9915749
795125*                                                                 9915749
795130     IF  FTC-SRV-CHG-ACC-1 EQUAL SPACES OR ZEROS                  9915749
795133         PERFORM Y2000-READ-FTC                                   9916298
795135         GO TO Y2001-BUILD-FEE-XREF.                              9915749
795140*                                                                 9915749
795145     IF  FEE-NBR-MAX GREATER THAN 0                               9915749
795150         IF  FTC-FEE-NUMBER EQUAL FEE-NUMBER-NBR (NBR-INX)        9915749
795153             PERFORM Y2000-READ-FTC                               9916298
795155             GO TO Y2001-BUILD-FEE-XREF.                          9915749
795160*                                                                 9915749
795165     ADD 1                       TO FEE-NBR-MAX.                  9915749
795170     SET NBR-INX UP BY 1.                                         9915749
795175     MOVE FTC-FEE-NUMBER         TO FEE-NUMBER-NBR (NBR-INX).     9915749
795180     MOVE FTC-SRV-CHG-ACC-1      TO FEE-NUMBER-ACCUM (NBR-INX).   9915749
795183     PERFORM Y2000-READ-FTC.                                      9916298
795185     GO TO Y2001-BUILD-FEE-XREF.                                  9915749
795190*                                                                 9915749
795195 Y2000-READ-FTC.                                                  9916298
795200     MOVE 'R'                    TO I-O-CONTROL-OPERATOR.         9916298
795205     MOVE 'I'                    TO I-O-CONTROL-ACCESS.           9916298
795210     CALL 'IMFTCMS'           USING I-O-CONTROL-AREA              9916298
795215                                    FTC-RECORD.                   9916298
795220     IF  I-O-88-END-OF-FILE                                       9916298
795225         MOVE '1'                TO IMFTC-EOF-SW.                 9916298
795310 Y2000-FEE-ERROR.                                                 9915749
795320*--- ERROR ON FEE TABLE FILE - RECORD MISSING FOR CONTROLS ------*9915749
795330     MOVE PROGRAM-NAME TO SIMESS-PROGRAM.                         9915749
795340     MOVE 505 TO SIMESS-MESS-NO.                                  9915749
795350     MOVE HOLD-1 TO E99-C1.                                       9915749
795360     MOVE HOLD-2 TO E99-C2.                                       9915749
795370     MOVE HOLD-3 TO E99-C3.                                       9915749
795380     MOVE EMSG-99 TO SIMESS-OPTIONAL-MESSAGE.                     9915749
795390     CALL 'SIMESS' USING SIMESS-AREA.                             9915749
795400     GO TO Z9900.                                                 9915749
795410 Y2000-INIT-LOOP.                                                 9915749
795420     MOVE SPACES TO FEE-ACCUM-TYPE (FEE-INX).                     9915749
795430     MOVE ZEROS  TO FEE-ACCUM-NBR (FEE-INX)                       9915749
795440                    FEE-ACCUM-2 (FEE-INX)                         9915749
795450                    FEE-NUMBER-NBR (NBR-INX)                      9915749
795460                    FEE-NUMBER-ACCUM (NBR-INX).                   9915749
795470     SET FEE-INX UP BY 1.                                         9915749
795480     SET NBR-INX UP BY 1.                                         9915749
795490 Y2000-BUILD-FEE-EXIT.                                            9915749
795500     EXIT.                                                        9915749
795510 Y3000-BUILD-PRD-TBLS.                                            9915749
795520     SET PRD-INX TO 1.                                            9915749
795530     PERFORM Y3000-INIT-LOOP 50 TIMES.                            9915749
795540     MOVE 0 TO PRD-NBR-MAX.                                       9915749
795545     IF  WBC-PRODUCT-CTL-FLAG EQUAL '0'                           0266741
795550         GO TO Y3000-BUILD-PRD-EXIT.                              9915749
795555     IF  PROD-OPEN-SW EQUAL '0'                                   9915749
795560         MOVE '1' TO PROD-OPEN-SW                                 9915749
795565         MOVE 'I' TO I-O-CONTROL-ACCESS                           9915749
795570         MOVE 'O' TO I-O-CONTROL-OPERATOR                         9915749
795575         CALL 'IMPRDMV' USING I-O-CONTROL-AREA                    9915749
795580                              PRODUCT-REC                         9915749
795585         MOVE 'I' TO I-O-CONTROL-ACCESS                           9915749
795590         MOVE 'R' TO I-O-CONTROL-OPERATOR                         9915749
795595         CALL 'IMPRDMV' USING I-O-CONTROL-AREA                    9915749
795600                              PRODUCT-REC.                        9915749
795620     SET PRD-INX TO 1.                                            9915749
795630 Y3001-BUILD-PRD-TBLS.                                            9915749
795640     IF  PF-CONTROLS LESS THAN WBC1-CONTROL-KEY                   0266741
795650         GO TO Y3001-READ-IMPRDM.                                 9915749
795660     IF  PF-CONTROLS GREATER THAN WBC1-CONTROL-KEY                0266741
795670         GO TO Y3000-BUILD-PRD-EXIT.                              9915749
795680     MOVE PF-TYPE TO PRD-TYPE (PRD-INX).                          9915749
795690     MOVE PFF-NB-RSRVE-RT TO PRD-RSRVE-RT (PRD-INX).              9915749
795700     MOVE PFF-NB-THRESHOLD-DEFAULT TO PRD-THRESHOLD-AMT (PRD-INX).9915749
795705     MOVE PFF-XINV-ALLOWED       TO PRD-XINV-ALLOWED (PRD-INX).   2016639
795710     ADD 1 TO PRD-NBR-MAX.                                        9915749
795712     IF  PRD-NBR-MAX EQUAL TO +50                                 9915749
795714         GO TO Y3000-BUILD-PRD-EXIT.                              9915749
795720     SET PRD-INX UP BY 1.                                         9915749
795730 Y3001-READ-IMPRDM.                                               9915749
795740     MOVE 'R' TO I-O-CONTROL-OPERATOR.                            9915749
795750     MOVE 'I' TO I-O-CONTROL-ACCESS.                              9915749
795760     CALL 'IMPRDMV' USING I-O-CONTROL-AREA                        9915749
795770                          PRODUCT-REC.                            9915749
795780     IF  I-O-88-END-OF-FILE                                       9915749
795790         GO TO Y3000-BUILD-PRD-EXIT.                              9915749
795800     GO TO Y3001-BUILD-PRD-TBLS.                                  9915749
795810 Y3000-INIT-LOOP.                                                 9915749
795820     MOVE SPACES TO PRD-TYPE (PRD-INX).                           9915749
795830     MOVE ZEROS  TO PRD-RSRVE-RT (PRD-INX)                        9915749
795840                    PRD-THRESHOLD-AMT (PRD-INX).                  9915749
795845     MOVE '0'    TO PRD-XINV-ALLOWED  (PRD-INX).                  2016639
795850     SET PRD-INX UP BY 1.                                         9915749
795860 Y3000-BUILD-PRD-EXIT.                                            9915749
795870     EXIT.                                                        9915749
795900*                                                                 1105504
795915 Y4000-RESET-SC-FIELDS.                                           1105504
795920     MOVE ZERO                       TO  SC-FLAG                  1105504
795925                                         SC-AMT-SERV              1105504
795930                                         SC-AMT-ACH               1105504
795935                                         SC-AMT-ATM               1105504
795940                                         SC-AMT-OTH               1105504
795945                                         SC-AMT-EC                1105504
795950                                         SC-CTR-OTH               1105504
795955                                         SC-DISC-WVE-AMT          1105504
795960                                         SC-REG-WVE-AMT           1105504
795965                                         SC-TOT-EC                1105504
795970                                         SC-RESV-AMT              1105504
795975                                         SC-EC-BAL                1105504
795980                                         SC-EC-RATE               1105504
795985                                         SC-SERV-WAIVE            1105504
795990                                         SC-ACH-WAIVE             1105504
795995                                         SC-ATM-WAIVE             1105504
796000                                         SC-OTH-WAIVE             1105504
796005                                         CMA-FEE-WAIVE            1105504
796010                                         CMA-MGMT-FEES            1105504
796015                                         WK-HOLD-EC-AMT           1105504
796020                                         WMS-SC-TRAN-CHG.         1105504
796025     MOVE SPACES                     TO  SC-STMT-SYM-SERV         1105504
796030                                         SC-STMT-SYM-ACH          1105504
796035                                         SC-STMT-SYM-ATM          1105504
796040                                         SC-STMT-SYM-OTH          1105504
796045                                         CMA-MGMT-FEE-SYM.        1105504
796050 Y4000-EXIT.                                                      1105504
796055       EXIT.                                                      1105504
796060*                                                                 1105504
796100 Y5000-10-TFRCSI-CLEAR-TOTALS.                                    2015719
796105     MOVE ZERO TO WK-CSI-TOT-ASSET-AMT                            2015719
796110                  WK-CSI-INT-ASSET-AMT                            2015719
796115                  WK-CSI-INT-LIAB-AMT.                            2015719
796120 Y5000-10-EXIT.                                                   2015719
796125     EXIT.                                                        2015719
796175*--------------------------------------------------------------*  0417181
796180*  ACCUMULATE TFR AVERAGE BALANCE SHEET DATA.                  *  0417181
796185*    LIABILITIES:  (DEPOSITS)                                  *  0417181
796190*    ASSETS:       (NONMORTGAGE LOANS AND ACCRUED RECEIVABLE)  *  0417181
796195*--------------------------------------------------------------*  0417181
796200 Y5000-30-TFRCSI-ACCUM.                                           2015719
796201*- LIABILITIES ------------------------------------------------*  0417181
796205     IF  WMS-SAVINGS-TRLR EQUAL '1'                               2015719
796210     AND WMS-INT-INV-IND EQUAL 'N'                                2015719
796215     AND WMS-CMA-INDICATOR EQUAL '0'                              2015719
796220         MOVE WMS-INT-CUR-BAL TO WK-TFR-PRIN-BAL                  2015719
796225     ELSE                                                         2015719
796230         MOVE ZERO TO WK-TFR-PRIN-BAL.                            2015719
796235     IF  WMS-DDA-BAL GREATER THAN ZERO                            2015719
796236     AND (WMS-IOD-RATE-PTR GREATER THAN 0 OR                      0417181
796237         WMS-HIFI-INDICATOR GREATER THAN 0)                       0417181
796240         ADD WMS-DDA-BAL TO WK-TFR-PRIN-BAL.                      2015719
796245*-TOTAL INTEREST EARNING DEPOSITS-*                               0417181
796250     ADD WK-TFR-PRIN-BAL TO WK-CSI-INT-LIAB-AMT.                  0417181
796255*- ASSETS -----------------------------------------------------*  0417181
796260     MOVE ZERO                   TO WK-TFR-PRIN-BAL.              0417181
796265     IF  WMS-DDA-BAL LESS THAN ZERO                               0417181
796270         MOVE WMS-SYS-TYPE       TO SYS-TYPE-TEST                 0417181
796275         IF  SYS-TYPE-TFR                                         0417181
796280             SUBTRACT WMS-DDA-BAL FROM WK-TFR-PRIN-BAL.           0417181
796285     IF  WMS-LOAN-TRLR EQUAL '0'                                  0417181
796290         GO TO Y5000-30-CONT.                                     0417181
796295     MOVE WMS-LOAN-REG-CODE      TO REG-CODE-TEST.                0417181
796300     IF  NOT REG-CODE-TFR                                         0417181
796305         GO TO Y5000-30-CONT.                                     0417181
796310     IF  WMS-PRIN-BAL NOT LESS THAN WMS-COSTS-UNEARNED            0417181
796315         ADD WMS-PRIN-BAL TO WK-TFR-PRIN-BAL                      0417181
796320         SUBTRACT WMS-COSTS-UNEARNED FROM WK-TFR-PRIN-BAL.        0417181
796325 Y5000-30-CONT.                                                   0417181
796330*-TOTAL NONMORTGAGE LOANS-*                                       0417181
796335     ADD WK-TFR-PRIN-BAL         TO WK-CSI-INT-ASSET-AMT.         0417181
796340*- TOTAL ASSETS -----------------------------------------------*  0417181
796345     IF  WMS-OD-ACCRUAL-TRLR EQUAL '1'                            0417181
796350         ADD WMS-OD-CYC-ACR +.005 GIVING HOLD-ACCR2               0417181
796355         ADD HOLD-ACCR2          TO WK-TFR-PRIN-BAL.              0417181
796360     IF  WMS-LOAN-TRLR EQUAL '0'                                  0417181
796365         GO TO Y5000-30-TOT-ASSETS.                               0417181
796370     IF  NOT REG-CODE-TFR                                         0417181
796375         GO TO Y5000-30-TOT-ASSETS.                               0417181
796380     IF  WMS-LOAN-STATUS GREATER THAN '2' AND NOT EQUAL '5'       0417181
796382         GO TO Y5000-30-TOT-ASSETS.                               0417181
796384     ADD WMS-INTRST-BAL          TO WK-TFR-PRIN-BAL.              0417181
796386     ADD WMS-LOAN-ACCR-INT +.005 GIVING HOLD-ACCR2.               0417181
796388     ADD HOLD-ACCR2              TO WK-TFR-PRIN-BAL.              0417181
796390 Y5000-30-TOT-ASSETS.                                             0417181
796392*-TOTAL ASSETS-*                                                  0417181
796394     ADD WK-TFR-PRIN-BAL         TO WK-CSI-TOT-ASSET-AMT.         0417181
796396 Y5000-30-EXIT.                                                   0417181
796398     EXIT.                                                        0417181
796400 Y5000-40-TFRCSI-AVERAGE.                                         2015719
796405     ADD WK-CSI-TOT-ASSET-AMT TO WBC-TFR-AVG-TOTAL-ASSETS.        0266741
796410                                                                  2015719
796415     ADD WK-CSI-INT-ASSET-AMT TO WBC-TFR-AVG-ASSETS.              0417181
796420                                                                  2015719
796425     ADD WK-CSI-INT-LIAB-AMT  TO WBC-TFR-AVG-LIABILITIES.         0417181
796430                                                                  2015719
796435*--------------------------------------------------------------*  0417181
796440*  DIVIDE BY 6 FOR QUARTERLY AVERAGE OF BEGINNING/ENDING       *  0417181
796445*  AGGREGATE MONTHLY BALANCES.                                 *  0417181
796450*--------------------------------------------------------------*  0417181
796455     IF  WBC-QUARTER-END EQUAL '1'                                0266741
796460         MOVE 6                   TO C-NO-DAYS                    0417181
796505         DIVIDE WBC-TFR-AVG-TOTAL-ASSETS BY C-NO-DAYS             0417181
796510                          GIVING WBC-TFR-AVG-TOTAL-ASSETS         0266741
796515         DIVIDE WBC-TFR-AVG-ASSETS BY C-NO-DAYS                   0417181
796520                          GIVING WBC-TFR-AVG-ASSETS               0417181
796525         DIVIDE WBC-TFR-AVG-LIABILITIES BY C-NO-DAYS              0417181
796530                          GIVING WBC-TFR-AVG-LIABILITIES.         0417181
796535 Y5000-40-EXIT.                                                   2015719
796540     EXIT.                                                        2015719
796545*                                                                 2015719
796550 Y5000-50-REGD.                                                   0266776
796555     IF  SVCH-TODAY EQUAL '1'                                     0266776
796560         GO TO Y5000-50-REGD-EXIT.                                0266776
796565     IF  WMS-SC-CHECK-ITEMS EQUAL IN-MMDA-CHECKS             AND  0266776
796570         WMS-SC-AMTTRNSFR-NO EQUAL IN-MMDA-XFERS                  0266776
796575         GO TO Y5000-50-REGD-EXIT.                                0266776
796580     COMPUTE IN-MMDA-COMB = WMS-SC-CHECK-ITEMS +                  0266776
796585                            WMS-SC-AMTTRNSFR-NO.                  0266776
796586     IF  IN-MMDA-COMB GREATER THAN WMS-MAX-TRAN-FOR-HIFI          0266776
796587         GO TO Y5000-50-NOTICE.                                   0266776
796600     GO TO Y5000-50-REGD-EXIT.                                    0920056
796601 Y5000-50-NOTICE.                                                 0266776
796605     MOVE '14' TO IMEX-REC-NO.                                    0266776
796610     MOVE 'ZF' TO IMEX-CODE-1.                                    0266776
796612     MOVE '  ' TO IMEX-CODE-2.                                    0266776
796615     MOVE WMS-SERVICE-CHARGE-DATA TO EX14-SVC-CHG-INFO            0266776
796620                                     SAVE-EXC-14-REC.             0266776
796625     PERFORM WRITE-EX-MST THRU WRITE-EX-EXIT.                     0266776
796630 Y5000-50-REGD-EXIT.                                              0266776
796635     EXIT.                                                        0266776
796640*                                                                 0266776
800100 FINAL-SECTION       SECTION 49.
800150     SKIP3
800200 Z0010.
800250     MOVE 'R' TO I-O-CONTROL-OPERATOR.
800300     CALL 'IMBCRM' USING I-O-CONTROL-AREA,
800350                          DDA-WRKBCR-1.                           0266741
800400     IF I-O-88-END-OF-FILE
800450         GO TO Z0020.
800500     IF WBC1-RECORD-ID NOT EQUAL TO 'B1'                          0266741
800550         GO TO Z0010.
800600     IF WBC1-CONTROL-1 NOT EQUAL TO LOW-1                         0266741
800650         IF WBC1-RECORD-ID EQUAL 'B1' AND                         0266741
800700             WBC-PROCESS-FLAG EQUAL '1' AND                       0266741
800750             WBC-RUN-FLAG EQUAL 'I'                               0266741
800800                 MOVE WBC1-CONTROL-KEY TO HOLD-CONTROL IMEX-KEY   0266741
800830                 PERFORM Y1400 THRU Y1400-EXIT                    0902213
800850                 PERFORM C1004 THRU C1009-EXIT                    0902970
800860                 MOVE SPACES   TO GL-CURRENT-COST-CTR             0902970
800900                 PERFORM C2001 THRU C2005-EXIT
800950                 GO TO Z0010
801000         ELSE GO TO Z0010.
801050 Z0020.
801051*--------------------------------------------------------------*  9915845
801052***  THE FOLLOWING USER EXIT MAY BE USED TO CHANGE FIELDS OR   *  9915845
801053***  PERFORM FUNCTIONS FOR SPECIAL END OF JOB FUNCTIONS        *  9915845
801054***                                                            *  9915845
801055     COPY IMPD31EN.                                               9915845
801056***                                                            *  9915845
801057***  END OF USER EXIT.                                         *  9915845
801058*--------------------------------------------------------------*  9915845
801100     MOVE 'U' TO I-O-CONTROL-ACCESS.
801150     MOVE 'E' TO I-O-CONTROL-OPERATOR.
801200     CALL 'IMBCRM' USING I-O-CONTROL-AREA,
801250                          DDA-WRKBCR-1.                           0266741
801300     MOVE 'I' TO I-O-CONTROL-ACCESS.                              0447268
801350     MOVE 'E' TO I-O-CONTROL-OPERATOR.
801400     CALL 'IMPOSTS' USING I-O-CONTROL-AREA,
801450                          TRAN-FILE.
801460     MOVE 'O' TO I-O-CONTROL-ACCESS.                              0447268
801470     MOVE 'E' TO I-O-CONTROL-OPERATOR.                            0447268
801480     CALL 'IMPOSTS' USING I-O-CONTROL-AREA,                       0447268
801490                          TRAN-FILE.                              0447268
801500     MOVE 'U' TO I-O-CONTROL-ACCESS.
801550     MOVE 'E' TO I-O-CONTROL-OPERATOR.
801560     IF  MASTER-GROUPING-SW EQUAL 'N'                             1003624
801600         CALL 'IMACTM' USING I-O-CONTROL-AREA                     1003624
801650                             MASTER-AREA                          1003624
801700                             SI-ENVIRONMENT-AREA                  1003624
801701     ELSE                                                         1003624
801702         CALL 'IMACTS' USING I-O-CONTROL-AREA                     1003624
801703                             MASTER-AREA                          1003624
801704                             SI-ENVIRONMENT-AREA                  1003624
801705                             I-O-BASE                             1003624
801706                             SV-MASTER-GROUPS.                    1003624
801707     MOVE '0' TO SI-ENVIRONMENT-PSW.                              1003624
801710     MOVE 'E'                    TO  I-O-CONTROL-OPERATOR.        1004962
801718     MOVE 'I'                    TO  I-O-CONTROL-ACCESS.          1004962
801726     CALL 'IMSVCM'            USING  I-O-CONTROL-AREA             1004962
801730                                     DDA-WRKBCR-1                 0266741
801734                                     IM-SC-ACCESS                 9715464
801738                                     DUMMY-RECORD.                1004962
801750     MOVE 'O' TO I-O-CONTROL-ACCESS.
801800     MOVE 'E' TO I-O-CONTROL-OPERATOR.
801850     CALL 'IMTAGTS' USING I-O-CONTROL-AREA,
801900                          EXCEPTION-AREA.
801910     MOVE 'O' TO I-O-CONTROL-ACCESS.                              0902970
801920     MOVE 'E' TO I-O-CONTROL-OPERATOR.                            0902970
801930     CALL 'IMGLTTS' USING I-O-CONTROL-AREA,                       0902970
801940                          GL-EXTRACT-RECORD.                      0902970
801945     MOVE 'I' TO I-O-CONTROL-ACCESS.                              2016374
801950     MOVE 'E' TO I-O-CONTROL-OPERATOR.                            2016374
801955     CALL 'IMBRTMS' USING I-O-CONTROL-AREA,                       2016374
801960                          BONUS-RATE-AREA.                        2016374
802070     MOVE 'I' TO I-O-CONTROL-ACCESS.                              2016374
802080     MOVE 'E' TO I-O-CONTROL-OPERATOR.                            2016374
802090     CALL 'SITBLMV' USING I-O-CONTROL-AREA,                       2016374
802100                          SI-ENVIRONMENT-AREA.
802101     MOVE 'I' TO I-O-CONTROL-ACCESS.                              9915845
802102     MOVE 'E' TO I-O-CONTROL-OPERATOR.                            9915845
802103     CALL 'IMLKPMV' USING I-O-CONTROL-AREA,                       9915845
802104                          DUMMY-RECORD.                           9915845
802105     MOVE 'I' TO I-O-CONTROL-ACCESS.                              9915845
802106     MOVE 'E' TO I-O-CONTROL-OPERATOR.                            IM008
802109     CALL 'IMRTEMV' USING I-O-CONTROL-AREA                        1003625
802112                          RATE-MASTER-AREA.                       1003625
802115     MOVE 'I' TO I-O-CONTROL-ACCESS.                              IM008
802118     MOVE 'E' TO I-O-CONTROL-OPERATOR.                            IM008
802121     CALL 'IMRTEWV' USING I-O-CONTROL-AREA                        1003625
802124                          RATE-WORK-FILE.                         1003625
802126                                                                  IMIB056
802128     COPY IMPD31U3.                                               IMIB056
802130                                                                  IMIB056
802150     IF  SI-88-ENVIRONMENT-ONLINE
802200         MOVE 'O'   TO  I-O-CONTROL-ACCESS                        1003624
802250         MOVE 'E'   TO  I-O-CONTROL-OPERATOR
802260         IF  MASTER-GROUPING-SW EQUAL 'N'                         1003624
802300             CALL 'IMMEMMV'     USING I-O-CONTROL-AREA            1003624
802350                                      IM-MEMO-OL-AREA             1003624
802355         ELSE                                                     1003624
802360             MOVE 'O'          TO I-O-CONTROL-ACCESS              1003624
802365             MOVE 'IMMEMMV'    TO I-O-BASE-PROTOTYPE              1003624
802370                                  I-O-BASE-FILE-NAME              1003624
802375             CALL 'SISSLOP' USING SV-MASTER-GROUPS                1003624
802380                                  I-O-BASE                        1003624
802385                                  I-O-CONTROL-AREA                1003624
802390                                  IM-MEMO-OL-AREA.                1003624
802395     IF  SI-88-ENVIRONMENT-ONLINE                                 1003624
802400         MOVE 'U'   TO  I-O-CONTROL-ACCESS
802450         MOVE 'E'   TO  I-O-CONTROL-OPERATOR
802500         CALL 'IMALPHA'    USING I-O-CONTROL-AREA
802520                                  SI-ENVIRONMENT-AREA             IM003
802550                                 MASTER-AREA                      IM006
802559                                 WBC-ONLINE-ALPHA-RECORD.         0266741
802560     MOVE 'I' TO I-O-CONTROL-ACCESS.                              9915749
802561     MOVE 'E' TO I-O-CONTROL-OPERATOR.                            9915749
802562     CALL 'IMODSMV' USING I-O-CONTROL-AREA,                       9915749
802563                          DUMMY-RECORD.                           9915749
802565     MOVE 'I' TO I-O-CONTROL-ACCESS.                              9915749
802566     MOVE 'E' TO I-O-CONTROL-OPERATOR.                            9915749
802567     CALL 'IMODLMV' USING I-O-CONTROL-AREA,                       9915749
802568                          TRANSFER-BALANCE-RECORD.                9915749
802570     MOVE 'O' TO I-O-CONTROL-ACCESS.                              9915749
802571     MOVE 'E' TO I-O-CONTROL-OPERATOR.                            9915749
802572     CALL 'IMBRRMS' USING I-O-CONTROL-AREA,                       9915749
802573                          BANK-REPORTING-RECORD.                  9915749
802575     IF  PROD-OPEN-SW EQUAL '1'                                   9915749
802576         MOVE 'E' TO I-O-CONTROL-OPERATOR                         9915749
802577         MOVE 'I' TO I-O-CONTROL-ACCESS                           9915749
802578         CALL 'IMPRDMV' USING I-O-CONTROL-AREA                    9915749
802579                              PRODUCT-REC.                        9915749
802584     MOVE 'I' TO I-O-CONTROL-ACCESS.                              9915749
802585     MOVE 'E' TO I-O-CONTROL-OPERATOR.                            9915749
802586     CALL 'IMFEEMV' USING I-O-CONTROL-AREA                        9915749
802587                          MULTIPLE-PRICE-FEE-TABLE.               9915749
802588     MOVE 'I' TO I-O-CONTROL-ACCESS.                              9915749
802589     MOVE 'E' TO I-O-CONTROL-OPERATOR.                            9915749
802590     CALL 'IMFTCMS' USING I-O-CONTROL-AREA                        9915749
802591                          FTC-RECORD.                             9915749
802592     MOVE 'I' TO I-O-CONTROL-ACCESS.                              9915868
802593     MOVE 'E' TO I-O-CONTROL-OPERATOR.                            9915749
802594     CALL 'IMNMAMV' USING I-O-CONTROL-AREA                        9915868
802595                          IMN-MASTER-RECORD.                      9915868
802596     MOVE 'O' TO I-O-CONTROL-ACCESS.                              9915749
802597     MOVE 'E' TO I-O-CONTROL-OPERATOR.                            9915749
802598     CALL 'IMACMTS' USING I-O-CONTROL-AREA                        9915868
802599                          IM-ACM-ABM-MONETARY.                    9915868
802600     MOVE 'O' TO I-O-CONTROL-ACCESS.                              9915868
802601     MOVE 'E' TO I-O-CONTROL-OPERATOR.                            9915868
802602     CALL 'IMACDTS' USING I-O-CONTROL-AREA                        9915868
802603                          IM-ACM-ABM-TRAN.                        9915868
802604     MOVE '9' TO WSSPLAR-SPL-CD.                                  9915868
802650     CALL 'SISPOOL'  USING SIWS-STANDARD-HEADING
802700                           WS-SPOOL-AREA
802750                           PRINT-AREA                             9915845
802760                           WSSPLEX-EX-SPOOL-AREA.                 9915845
802761     MOVE 'I'    TO I-O-CONTROL-ACCESS.                           9915855
802763     MOVE 'E'    TO I-O-CONTROL-OPERATOR.                         9915855
802765     CALL 'IMLMTMS' USING I-O-CONTROL-AREA                        9915855
802767                          OD-LIMIT-REC.                           9915855
802771     MOVE 'O'    TO I-O-CONTROL-ACCESS.                           9915855
802773     MOVE 'E'    TO I-O-CONTROL-OPERATOR.                         9915855
802775     CALL 'IMLMTMV' USING I-O-CONTROL-AREA                        9915855
802777                          OD-LIMIT-REC-OUT.                       9915855
802780     MOVE 'O'             TO  I-O-CONTROL-ACCESS.                 9915855
802782     MOVE 'E'             TO  I-O-CONTROL-OPERATOR.               9915855
802784     CALL 'IMIBTTS'    USING  I-O-CONTROL-AREA                    9915855
802786                          BALANCE-HISTORY-REC.                    9915855
802790     MOVE SPACES TO WSSPLEX-LANGUAGE-CODE.                        9915855
802800 Z0040.
802850     MOVE 'U' TO I-O-CONTROL-ACCESS.
802900     MOVE 'O' TO I-O-CONTROL-OPERATOR.
802950     CALL 'IMBCRM' USING I-O-CONTROL-AREA,
803000                          DDA-WRKBCR-1.                           0266741
803050 Z0050.
803100     MOVE 'R' TO I-O-CONTROL-OPERATOR.
803150     CALL 'IMBCRM' USING I-O-CONTROL-AREA,
803200                          DDA-WRKBCR-1.                           0266741
803250     IF I-O-88-END-OF-FILE
803300         GO TO Z0060.
803310     IF WBC1-RECORD-ID EQUAL TO 'B7'                              0266741
803320         MOVE DDA-WRKBCR-1     TO DDA-WRKBCR-7                    0266741
803330         MOVE WBC-SEGMENT-NO TO SV-SEGMENT-NO.                    0266741
803350     IF WBC1-RECORD-ID NOT EQUAL TO 'B1', GO TO Z0050.            0266741
803400     IF WBC-PROCESS-FLAG NOT EQUAL TO '1', GO TO Z0050.           0266741
803410     PERFORM UO-TABLE-LOAD THRU UO-TABLE-LOAD-EXIT.               2010732
803450     MOVE POST-COMPL TO WBC-FM-POST-COMP.                         0266741
803500     MOVE 'W' TO I-O-CONTROL-OPERATOR.
803550     CALL 'IMBCRM' USING I-O-CONTROL-AREA,
803600                          DDA-WRKBCR-1.                           0266741
803650     GO TO Z0050.
803700 Z0060.  EXIT.
803750 Z0070.
803800     GO TO Z9900.
803803                                                                  IM008
803850 Z2000.
803900*  --ERROR ON BANK CONTROL MATCHING
803950     MOVE LOW-1 TO E01-C1.
804000     MOVE LOW-2 TO E01-C2.
804050     MOVE LOW-3 TO E01-C3.
804100     MOVE PROGRAM-NAME TO SIMESS-PROGRAM.
804150     MOVE 502 TO SIMESS-MESS-NO.
804200     MOVE EMSG-01 TO SIMESS-OPTIONAL-MESSAGE
804250     CALL 'SIMESS' USING SIMESS-AREA.
804300     GO TO Z9900.
804304 Z2200.                                                           IM002
804308*  --ERROR OCCURING WHEN BCR NOT UPDATED WITH PRODUCT CHANGES--   IM002
804312     MOVE LOW-1 TO E06-C1.                                        9915868
804316     MOVE LOW-2 TO E06-C2.                                        9915868
804320     MOVE LOW-3 TO E06-C3.                                        9915868
804324     MOVE PROGRAM-NAME TO SIMESS-PROGRAM                          IM002
804328     MOVE 002 TO SIMESS-MESS-NO.                                  IM002
804332     MOVE EMSG-06 TO SIMESS-OPTIONAL-MESSAGE.                     9915868
804336     CALL 'SIMESS' USING SIMESS-AREA.                             IM002
804340 Z2200-EXIT. EXIT.                                                IM002
804350 Z2400.
804400*  --ERROR ON MASTER HEADER RECORD PROCESSING
804450     MOVE PROGRAM-NAME TO SIMESS-PROGRAM.
804500     MOVE 505 TO SIMESS-MESS-NO.
804550     MOVE HDR-FILE-SEQ     TO E02-C1.
804600     MOVE HOLD-NO5         TO E02-C2.
804650     MOVE EMSG-02 TO SIMESS-OPTIONAL-MESSAGE.
804700     CALL 'SIMESS' USING SIMESS-AREA.
804750     GO TO Z9900.
804800 Z2600.
804850*  --ERROR OCCURING WHEN PREVIOUS PROGRAM (IM28) NOT RUN
804900     MOVE LOW-1 TO E03-C1.
804950     MOVE LOW-2 TO E03-C2.
805000     MOVE LOW-3 TO E03-C3.
805050     MOVE PROGRAM-NAME TO SIMESS-PROGRAM
805100     MOVE 501 TO SIMESS-MESS-NO.
805150     MOVE EMSG-03 TO SIMESS-OPTIONAL-MESSAGE.
805200     CALL 'SIMESS' USING SIMESS-AREA.
805250     GO TO Z9900.
805253                                                                  IM008
805255 Z2700.                                                           0902213
805260*  --ERROR ON BALANCE HISTORY MATCHING                            0902213
805265     MOVE 505 TO SIMESS-MESS-NO.                                  0902213
805270     MOVE BHH-FILE-SEQ TO SIMESS-MESS9-BH-SEQ.                    0902213
805275     MOVE HOLD-NO5     TO SIMESS-MESS9-BC-SEQ.                    0902213
805280     MOVE SIMESS-MESS9 TO SIMESS-OPTIONAL-MESSAGE.                0902213
805285     CALL 'SIMESS' USING SIMESS-AREA.                             0902213
805290     GO TO Z9900.                                                 0902213
805291 Z2750.                                                           9915855
805292*  --ERROR ON OD LIMIT FILE MATCHING                              9915855
805293     MOVE 505 TO SIMESS-MESS-NO.                                  9915855
805294     MOVE OD-LIMIT-HDR-FILE-SEQ TO SIMESS-MESS14-BH-SEQ.          9915855
805295     MOVE HOLD-NO5              TO SIMESS-MESS14-BC-SEQ.          9915855
805296     MOVE SIMESS-MESS14         TO SIMESS-OPTIONAL-MESSAGE.       9915855
805297     CALL 'SIMESS' USING SIMESS-AREA.                             9915855
805298     GO TO Z9900.                                                 9915855
805300 Z2760.                                                           0927817
805305*  --ERROR ON EXTENDED FLOAT FILE MATCHING                        0927817
805310     MOVE 505                TO SIMESS-MESS-NO.                   0927817
805315     MOVE FFH-FILE-SEQ       TO SIMESS-MESS16-FF-SEQ.             0927817
805320     MOVE HOLD-NO5           TO SIMESS-MESS16-BC-SEQ.             0927817
805325     MOVE SIMESS-MESS16      TO SIMESS-OPTIONAL-MESSAGE.          0927817
805330     CALL 'SIMESS'        USING SIMESS-AREA.                      0927817
805335     GO TO Z9900.                                                 0927817
805340 Z9900.                                                           0927817
805350     MOVE 'U' TO I-O-CONTROL-ACCESS.
805400     MOVE 'E' TO I-O-CONTROL-OPERATOR.
805450     CALL 'IMBCRM' USING I-O-CONTROL-AREA,
805500                          DDA-WRKBCR-1.                           0266741
805530     IF  BAL-HIST-OPEN EQUAL 'N'                                  0902213
805540         MOVE '2'             TO  SI-ENVIRONMENT-PSW              9915845
805560         MOVE 'U'             TO  I-O-CONTROL-ACCESS              9915848
805590         MOVE 'E'             TO  I-O-CONTROL-OPERATOR            0902213
805620         CALL 'IMBALM'  USING I-O-CONTROL-AREA                    9915848
805650                              BALANCE-HISTORY-REC                 9915848
805660                              BAL-HIST-LENGTHS                    9915845
805670                              SI-ENVIRONMENT-AREA                 9915845
805675                              DDA-WRKBCR-1                        0447266
805680         MOVE '0'             TO  SI-ENVIRONMENT-PSW.             9915855
805685     IF  AFF-OPEN          EQUAL  'N'                             0927817
805690         MOVE 'I'             TO  I-O-CONTROL-ACCESS              0927817
805695         MOVE 'E'             TO  I-O-CONTROL-OPERATOR            0927817
805700         CALL 'IMAFFMS' USING I-O-CONTROL-AREA                    0927817
805705                              EXTENDED-FLOAT-REC                  0927817
805710         MOVE 'O'             TO  I-O-CONTROL-ACCESS              0927817
805715         MOVE 'E'             TO  I-O-CONTROL-OPERATOR            0927817
805720         CALL 'IMAFFMV' USING I-O-CONTROL-AREA                    0927817
805725                              WFF-EXTENDED-FLOAT-REC.             0927817
805800     MOVE HIGH-VALUES TO SIMESS-PROGRAM.
805850     CALL 'SIMESS' USING SIMESS-AREA.
805900     STOP RUN.
805910     COPY IMUOLOAD.                                               2010732
805920     COPY IM31UP.                                                 2010732
805930     COPY SIPDSTML.                                               0266741
805990                                                                  0617360
806000     COPY IMPDUIC.                                                0617360
806100                                                                  0617360
806200 UIC-DIST-DATE.                                                   0617360
806300     IF  WS-WORK-PLN-CODE GREATER THAN '49'                       0617360
806400         GO TO UIC-END.                                           0617360
806500     IF  WK-LST-YY LESS THAN WBC-CAPTURE-YR                       0617360
806600         GO TO UIC-END.                                           0617360
806700     IF  WS-WORK-CTDT-AMT LESS THAN ZERO                          0617360
806800         GO TO UIC-END.                                           0617360
806900     MOVE WS-WORK-PLN-CODE TO WMS-PLN-TRLR-LAST-DIST-CODE.        0617360
807000     MOVE WS-WORK-DATE8    TO WMS-PLN-TRLR-DATE-LAST-DIST.        0617360
807100                                                                  0617360
807200 UIC-END.                                                         0617360
807300     EXIT.                                                        0617360
807400                                                                  0617360
807500 UIC-ERROR.                                                       0617360
807600     MOVE 'G0'                  TO IMEX-CODE-1.                   0617360
807700     MOVE 'G1'                  TO IMEX-CODE-2.                   0617360
807800     MOVE '13'                  TO IMEX-REC-NO.                   0617360
807900     MOVE WS-WORK-CTDT-AMT      TO EX13-AMT.                      0617360
808000     MOVE WS-WORK-CONT-DIST-KEY TO EXC13-02-DESC.                 0617360
808100     PERFORM WRITE-EXCEPT     THRU WRITE-EX-EXIT.                 0617360
808200                                                                  0617360
808300 UIC-ERROR-END.                                                   0617360
808400     EXIT.                                                        0617360
808500                                                                  0617360
808600     COPY IMPD31FM.                                               0617360
808700                                                                  0617360
808800 CHECK-PLAN-REV.                                                  0617360
808900     IF ((TR-TRAN-TYPE EQUAL '1' OR '2')                          0617360
809000     AND (WS-WORK-PLN-CODE LESS THAN '50'))                       0617360
809100     OR ((TR-TRAN-TYPE EQUAL '3' OR '4')                          0617360
809200     AND (WS-WORK-PLN-CODE GREATER THAN '49'))                    0617360
809300         COMPUTE WS-WORK-CTDT-AMT = WS-WORK-CTDT-AMT * -1.        0617360
809400                                                                  0617360
809500 CPR-END.                                                         0617360
809600     EXIT.                                                        0617360
809700                                                                  0617360
809800 BACKOUT-ACC-CONT-LMT.                                            0617360
809900*                                                                 0617360
809910     IF (WMS-PLN-TRLR-SNG-FAM-FLG EQUAL 'N' OR 'U')               0827778
809920         GO TO BACKOUT-ACC-CONT-LMT-EXIT.                         0617360
809930*                                                                 0617360
809940     IF  (WBC-CAPTURE-DA EQUAL '01')                              0617360
809950     AND (TR-FM-FLD-NO EQUAL '2219')                              0617360
809960     AND (TR-FM-NEW-DATA (2:1) EQUAL WMS-PLN-TRLR-MAX-CNT-PNT-B)  0617360
809970         GO TO BACKOUT-ACC-CONT-LMT-EXIT.                         0617360
809980*                                                                 0617360
810000     MOVE +0 TO HOLD-AMT13.                                       0617360
810100     IF  WMS-PLN-TRLR-MAX-CNT-PNT EQUAL ZERO                      0617360
810200         MOVE WMS-PLN-TRLR-MAX-CNT-LIM TO HOLD-AMT13              0617360
810300     ELSE                                                         0617360
810400         SET WBC-PL-LMT-IND      TO WMS-PLN-TRLR-MAX-CNT-PNT      0617360
810500         IF  WMS-PLN-TRLR-STATUS-CD GREATER THAN '1'              0617360
810600             IF  WBC-NEXT-YEAR-END EQUAL '1'                      0617360
810700                 IF  WBC-PL-PY-CU-MAX-CONT (WBC-PL-LMT-IND) > +0  0617360
810800                     MOVE WBC-PL-PY-CU-MAX-CONT (WBC-PL-LMT-IND)  0617360
810900                          TO HOLD-AMT13                           0617360
811000                 ELSE                                             0617360
811100                     MOVE WBC-PL-PY-MAX-CONT (WBC-PL-LMT-IND)     0617360
811200                          TO HOLD-AMT13                           0617360
811300             ELSE                                                 0617360
811400                 IF  WBC-PL-CY-CU-MAX-CONT (WBC-PL-LMT-IND) > +0  0617360
811500                     MOVE WBC-PL-CY-CU-MAX-CONT (WBC-PL-LMT-IND)  0617360
811600                          TO HOLD-AMT13                           0617360
811700                 ELSE                                             0617360
811800                     MOVE WBC-PL-CY-MAX-CONT (WBC-PL-LMT-IND)     0617360
811900                          TO HOLD-AMT13                           0617360
812000         ELSE                                                     0617360
812100             IF  WBC-NEXT-YEAR-END EQUAL '1'                      0617360
812200                 MOVE WBC-PL-PY-MAX-CONT (WBC-PL-LMT-IND)         0617360
812300                      TO HOLD-AMT13                               0617360
812400             ELSE                                                 0617360
812500                 MOVE WBC-PL-CY-MAX-CONT (WBC-PL-LMT-IND)         0617360
812600                      TO HOLD-AMT13.                              0617360
812700*                                                                 0617360
812800     IF  HOLD-AMT13 GREATER THAN +0                               0617360
812900         COMPUTE HOLD-AMT13 = HOLD-AMT13 / +12                    0617360
813000     ELSE                                                         0617360
813100         MOVE +0 TO HOLD-AMT13.                                   0617360
813200*                                                                 0617360
813300     COMPUTE WMS-PLN-TRLR-ACC-CNT-LIM = WMS-PLN-TRLR-ACC-CNT-LIM -0617360
813400                                        HOLD-AMT13.               0617360
813500*                                                                 0617360
813600 BACKOUT-ACC-CONT-LMT-EXIT.                                       0617360
813700     EXIT.                                                        0617360
