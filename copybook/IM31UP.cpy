*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000010*****************************************************************
000015*   BELOW IS WHERE YOU WILL PUT THE INDIVIDUAL OPTION           *
000020*   CODE THAT IS TO BE PLACED WITHIN THE PROGRAM THAT           *
000025*   THIS COPYBOOK IS ASSOCIATED WITH.                           *
000030*                                                               *
000035*   STANDARD PARA NAMES.                                        *
000040*      UO-OPTXXXX-A.    UO-OPTXXXX-A-EXIT.                      *
000045*      PREFIX ANY PARAGRAPH REFERENCED IN THIS SECTION          *
000050*      WITH THE UO-OPTXXXX-A SO THAT WE DO NOT HAVE             *
000055*      DUPLICATES.                                              *
000060*                                                               *
000065*   STANDARD LINE NUMBERS.                                      *
000070*      THE FIRST THREE NUMBERS LINE NUMBER SHOULD MATCH         *
000075*      THE OPTION NUMBER (OOO999 OOO = OPTION NUMBER).          *
000080*      EXAMPLE: OPTION 0001 LINE NUMBERS 001000 THRU 001999     *
000085*                                                               *
000090*****************************************************************
000095*   CHANGE COMMENTS WILL BE PLACED HERE WITH DETAIL             *
000100*   COMMENTS TO BE PLACED JUST ABOVE THE PARAGRAPH              *
000105*   THAT YOU ARE ADDING.                                        *
000110*TAG   PROGRAMMER  USER OPTION #    DATE      CHANGE ID         *
000115*          DESCRIPTION                                          *
000120*****************************************************************
000125*U0001 J. JANTON      0001       01/03/2000    &UO00011         * &UO00011
000130*          INTERFACE USER TRAN CODES FOR NSF/OD FEES TO THE     * &UO00011
000131*          GENERAQL LEDGER.                                     * &UO00011
000135*****************************************************************
000136*U0001                                                          * &UO00011
000137*U0001 OPTION  0001 DESCRIPTION:                                * &UO00011
000138*U0001     OPTION 001 IS A MODIFICATION TO IM31 TO ALLOW NSF/OD * &UO00011
000139*U0001     FEES TO BE ADDED TO COUNTERS FOR INTERFACING TO GL.  * &UO00011
000140*U0001 ASSOCIATED PROGRAMS: IM31 (264752-264766)                * &UO00011
000141*U0001 MIGRATION HISTORY:                                       * &UO00011
001100***************************************************************** &UO00011
001110 UO-OPT0001-A.                                                    &UO00011
001120 UO-OPT0001-A-EXIT.                                               &UO00011
001130     EXIT.                                                        &UO00011
001135***************************************************************** 2015774
001136*U0003                                                          * 2015774
001137*U0003 OPTION  0003 DESCRIPTION:                                * 2015774
001138*U0003     OPTION 003 IS A MODIFICATION TO IM31 TO ALLOW        * 2015774
001139*U0003     INCOMING(USER TRAN 1068) AND OUTGOING(USER TRAN 1069)* 2015774
001140*U0003     FEES TO BE ADDED TO COUNTERS FOR INTERFACING TO GL.  * 2015774
001141*U0003 ASSOCIATED PROGRAMS: IM31                                * 2015774
001142*U0003 MIGRATION HISTORY:                                       * 2015774
001143***************************************************************** 2015774
001144 UO-OPT0003-A.                                                    2015774
001145     IF  TR-USER-CODE EQUAL UB-FLD1-4 (3 1)                       2015774
001146         ADD +1 TO WS-GL-USER-COUNT-2                             2015774
001147         ADD TR-AMOUNT TO WS-GL-USER-AMOUNT-2                     2015774
001148         GO TO UO-OPT0003-A-EXIT.                                 2015774
001149*                                                                 2015774
001150     IF  TR-USER-CODE EQUAL UB-FLD1-4 (3 2)                       2015774
001151         ADD +1 TO WS-GL-USER-COUNT-3                             2015774
001152         ADD TR-AMOUNT TO WS-GL-USER-AMOUNT-3.                    2015774
001153 UO-OPT0003-A-EXIT.                                               2015774
001154*                                                                 2015774
001155***************************************************************** 2015774
001156*U0004                                                          * 2015774
001157*U0004 OPTION  0004 DESCRIPTION:                                * 2015774
001158*U0004     OPTION 004 IS A MODIFICATION TO IM31 TO ALLOW        * 2015774
001159*U0004     TELEPHONE TRANSFER(USER TRAN 1003) AND               * 2015774
001160*U0004     INTERNATIONAL WIRE(USER TRAN 1067) FEES              * 2015774
001161*U0004     TO BE ADDED TO COUNTERS FOR INTERFACING TO GL.       * 2015774
001162*U0004 ASSOCIATED PROGRAMS: IM31                                * 2015774
001163*U0004 MIGRATION HISTORY:                                       * 2015774
001164***************************************************************** 2015774
001165*                                                                 2015774
001166 UO-OPT0004-A.                                                    2015774
001167     IF  TR-USER-CODE EQUAL UB-FLD1-4 (4 1)                       2015774
001168         ADD +1 TO WS-GL-USER-COUNT-4                             2015774
001169         ADD TR-AMOUNT TO WS-GL-USER-AMOUNT-4                     2015774
001170         GO TO UO-OPT0004-A-EXIT.                                 2015774
001171*                                                                 2015774
001172     IF  TR-USER-CODE EQUAL UB-FLD1-4 (4 2)                       2015774
001173         ADD +1 TO WS-GL-USER-COUNT-5                             2015774
001174         ADD TR-AMOUNT TO WS-GL-USER-AMOUNT-5.                    2015774
001175 UO-OPT0004-A-EXIT.                                               2015774
001176     EXIT.                                                        2015774
001300*--------------------------------------------------------------*  0417235
001301*  USER OPTION 0018                                            *  0417235
001302*    THIS OPTION ALLOWS IM31 TO GENERATE A FEE FOR ADVANCING   *  0417235
001303*    FUNDS FROM THE LOAN TRAILER                               *  0417235
001304*--------------------------------------------------------------*  0417235
001309*                                                                 0417235
001310 UO-OPT0018-A.                                                    0417235
001320     MOVE 'N'           TO WS-U0018-FEE-CHARGED.                  0417235
001330*    IF  WMS-USER-CD-13 NOT EQUAL '1'                             1020057
001340         GO TO UO-OPT0018-A-EXIT.                                 0417235
001350     MOVE UB-FLD1-4 (0018, 1) TO WS-U0018-FEE-ENTRY.              0417235
001360     MOVE UB-FLD1-2 (0018, 4) TO WS-U0018-GT-NO.                  0417235
001370     IF  WS-U0018-FEE-AP NOT EQUAL 'A' AND 'P'                    0417235
001380         GO TO UO-OPT0018-A-EXIT.                                 0417235
001390     IF  WS-U0018-FEE-NO NOT NUMERIC                              0417235
001400         GO TO UO-OPT0018-A-EXIT.                                 0417235
001410     IF  WS-U0018-FEE-NO9 EQUAL ZERO                              0417235
001430         GO TO UO-OPT0018-A-EXIT.                                 0417235
001440     IF  WS-U0018-GT-NO NOT NUMERIC                               0417235
001450         GO TO UO-OPT0018-A-EXIT.                                 0417235
001460**   IF  WS-U0018-GT-NO9 LESS 38                                  0417235
001470**       GO TO UO-OPT0018-A-EXIT.                                 0417235
001480     MOVE WMS-CONTROL-1       TO WMF-CONTROL-1.                   0417235
001490     MOVE WMS-CURR-CODE       TO WMF-CURRENCY.                    0417235
001500     MOVE WMS-SVC-CHRG-REGION TO WMF-SC-REGION.                   0417235
001510     MOVE WS-U0018-FEE-AP     TO WMF-FEE-TYPE.                    0417235
001520     IF  WS-U0018-FEE-AP EQUAL 'A'                                0417235
001530         MOVE WMS-NB-ANC-TYPE TO WMF-CHG-TYPE                     0417235
001540     ELSE                                                         0417235
001550         MOVE WMS-SC-TYPE     TO WMF-CHG-TYPE.                    0417235
001560     MOVE 'I'                 TO I-O-CONTROL-ACCESS.              0417235
001570     MOVE 'K'                 TO I-O-CONTROL-OPERATOR.            0417235
001580     CALL 'IMFEEMV'        USING I-O-CONTROL-AREA                 0417235
001590                                 MULTIPLE-PRICE-FEE-TABLE.        0417235
001600     IF  I-O-88-NOT-FOUND                                         0417235
001610         GO TO UO-OPT0018-A-EXIT.                                 0417235
001620     MOVE ZERO                TO WS-U0018-COUNT.                  0417235
001630     MOVE MULTIPLE-PRICE-FEE-TABLE TO WS-U0018-FEE-TABLE.         0417235
001640 UO-OPT0018-A-LOOP.                                               0417235
001650     ADD +1                   TO WS-U0018-COUNT.                  0417235
001660     IF  WS-U0018-COUNT GREATER +200                              0417235
001670         GO TO UO-OPT0018-A-EXIT.                                 0417235
001680     IF  WS-U0018-FEE-TBL-NO (WS-U0018-COUNT) NOT EQUAL           0417235
001690         WS-U0018-FEE-NO                                          0417235
001700         GO TO UO-OPT0018-A-LOOP.                                 0417235
001710     MOVE WS-U0018-FEE-TBL-AMT1 (WS-U0018-COUNT)                  0417235
001713                                TO WS-U0018-FEE-AMOUNT.           0417235
001720*------------------*                                              0417235
001730*UPDATE BALANCES   *                                              0417235
001740*------------------*                                              0417235
001750     IF  CUR-BAL LESS WS-U0018-FEE-AMOUNT                         0417235
001760         COMPUTE WS-U0018-HOLD-AMT = WS-U0018-FEE-AMOUNT -        0417235
001770                                     CUR-BAL                      0417235
001780         IF  WMS-AVAIL-BAL NOT LESS WS-U0018-HOLD-AMT             0417235
001790             IF  WMS-LOAN-INCR-PTR NOT GREATER ZERO               0417235
001800                 ADD WS-U0018-HOLD-AMT TO CUR-BAL, LOAN-TODAY,    0417235
001810                     WMS-PRIN-BAL, CUST-BAL, LOAN-MIN-BAL         0417235
001820             ELSE                                                 0417235
001830                 IF  WMS-AVAIL-BAL NOT LESS                       0417235
001840                     WBC-LOAN-INCR(WMS-LOAN-INCR-PTR)             0417235
001850                     ADD WBC-LOAN-INCR (WMS-LOAN-INCR-PTR) TO     0417235
001860                      CUR-BAL, LOAN-TODAY, WMS-PRIN-BAL, CUST-BAL,0417235
001862                      LOAN-MIN-BAL                                0417235
001870                 ELSE                                             0417235
001880                     ADD WMS-AVAIL-BAL TO CUR-BAL, LOAN-TODAY,    0417235
001890                         WMS-PRIN-BAL, CUST-BAL, LOAN-MIN-BAL     0417235
001900                 END-IF                                           0417235
001910             END-IF                                               0417235
001920         ELSE                                                     0417235
001930             IF  WMS-AVAIL-BAL GREATER ZERO                       0417235
001940                 ADD WMS-AVAIL-BAL TO CUR-BAL, LOAN-TODAY,        0417235
001950                      WMS-PRIN-BAL, CUST-BAL, LOAN-MIN-BAL        0417235
001960             END-IF                                               0417235
001970         END-IF                                                   0417235
001980     END-IF.                                                      0417235
001990     SUBTRACT WS-U0018-FEE-AMOUNT FROM WMS-CURR-BAL, CUR-BAL      0417235
002000                           WMS-DDA-BAL, CUST-BAL.                 0417235
002010     ADD +1                  TO WMS-STMT-NO-DEBITS.               0417235
002020     ADD WS-U0018-FEE-AMOUNT TO WMS-STMT-DR-AMT.                  0417235
002030     MOVE WS-U0018-FEE-AMOUNT TO HOLD-AMT.                        0417235
002040     IF  WMS-IOD-RATE-PTR GREATER THAN ZERO                       0417235
002050     OR  WMS-HIFI-INDICATOR GREATER THAN ZERO                     0417235
002060         ADD +1       TO IOD-DB-NO, WS-GL-IOD-DB-NO               0417235
002070         ADD HOLD-AMT TO IOD-DB-AMT, WS-GL-IOD-DB-AMT,            0417235
002080     ELSE                                                         0417235
002090         ADD +1       TO DDA-DB-NO, WS-GL-DDA-DB-NO               0417235
002100         ADD HOLD-AMT TO DDA-DB-AMT, WS-GL-DDA-DB-AMT.            0417235
002105     ADD HOLD-AMT     TO WS-GL-USER-AMOUNT-2.                     0417235
002110     MOVE 'Y'                TO WS-U0018-FEE-CHARGED.             0417235
002120     MOVE WBC-GENERATED-TRANS (1) TO GEN-TRAN-OPTION-HOLD.        0417235
002130     MOVE 'U1'               TO IMEX-CODE-2.                      0417235
002140     MOVE WS-U0018-GT-NO         TO EX01-INT-CODE.                0417235
002142     MOVE WS-U0018-GT-NO         TO GT-USER-CODE.                 0417235
002150     MOVE '4'                    TO EX01-TYPE.                    0417235
002160     MOVE 'XF'                   TO GT-MNEUMONIC.                 0417235
002170     MOVE ZERO                   TO EX01-NUMBER.                  0417235
002180     PERFORM WRITE-GEN-TRANS THRU WRITE-GEN-EXIT.                 0417235
002190 UO-OPT0018-A-EXIT.                                               0417235
002200     EXIT.                                                        0417235
002210 UO-OPT0018-B.                                                    0417235
002220     IF  WS-U0018-FEE-CHARGED EQUAL 'N'                           0417235
002230         GO TO UO-OPT0018-B-EXIT.                                 0417235
002240     MOVE EX-01                  TO WS-U0018-ORIG-01LEN.          0417235
002250     ADD +8                      TO EX-01.                        0417235
002260     COMPUTE WS-U0018-BEGIN-POS =                                 0417235
002270       WS-U0018-ORIG-01LEN + 1.                                   0417235
002280     MOVE WS-U0018-FEE-AMOUNT-X TO                                0417235
002290          EXCEPTION-AREA (WS-U0018-BEGIN-POS:8).                  0417235
002300 UO-OPT0018-B-EXIT.                                               0417235
002310     EXIT.                                                        0417235
002320 UO-OPT0018-C.                                                    0417235
002330     IF  WS-U0018-FEE-CHARGED EQUAL 'N'                           0417235
002340         GO TO UO-OPT0018-C-EXIT.                                 0417235
002350     MOVE WS-U0018-ORIG-01LEN    TO EX-01.                        0417235
002360 UO-OPT0018-C-EXIT.                                               0417235
002370     EXIT.                                                        0417235
002400                                                                  0527332
002500*----------------------------------------------------------------*0527332
002600*U0019                                                           *0527332
002700*U0019 OPTION 0019 DESCRIPTION:                                  *0527332
002800*U0019     OPTION 019 IS A MODIFICATION TO IM31 TO ALLOW         *0527332
002900*U0019     A MAXIMUM THRESHOLD AMOUNT TO TRANSFER TO THE         *0527332
003000*U0019     SAVINGS TRAILER BEYOND THE MAXIMUM DDA BALANCE.       *0527332
003100*U0019 ASSOCIATED PROGRAMS: IM31                                 *0527332
003200*U0019 MIGRATION HISTORY:                                         0527332
003300*----------------------------------------------------------------*0527332
003400 UO-OPT0019-A.                                                    0527332
003500     IF  WMS-INT-USER-CODE NOT EQUAL 'P'                          0527332
003600         GO TO UO-OPT0019-A-END.                                  0527332
003700     IF  WMS-DC-TRLR NOT EQUAL '1'                                0527332
003800         GO TO UO-OPT0019-A-END.                                  0527332
003900     MOVE UB-EXECUTION-OPT(0019) TO WS-U0019-EXEC-DATA.           0527332
004000     IF  WS-U0019-PGM-NAME NOT GREATER SPACES                     0527332
004100         GO TO UO-OPT0019-A-END.                                  0527332
004200     CALL 'SICHKMOD' USING WS-U0019-PGM-NAME                      0527332
004300                           WS-U0019-CAP-ERROR.                    0527332
004400     IF  WS-U0019-CAP-ERROR EQUAL '0'                             0527332
004500         GO TO UO-OPT0019-A-END.                                  0527332
004600     MOVE SPACES TO WS-U0019-CAP-ERROR.                           0527332
004700     CALL WS-U0019-PGM-NAME USING WMS-DC-TRAILER                  0527332
004710                                  HOLD-AMT                        0527332
004800                                  WS-U0019-CAP-AMT                0527332
004900                                  WS-U0019-CAP-ERROR.             0527332
005000     IF  WS-U0019-CAP-ERROR NOT EQUAL SPACES                      0527332
005100         GO TO UO-OPT0019-A-END.                                  0527332
005200     IF  WS-U0019-CAP-AMT NOT GREATER ZERO                        0527332
005300         GO TO UO-OPT0019-A-END.                                  0527332
005400     IF  HOLD-AMT GREATER WS-U0019-CAP-AMT                        0527332
005500         MOVE WS-U0019-CAP-AMT TO HOLD-AMT.                       0527332
005600                                                                  0527332
005700 UO-OPT0019-A-END.                                                0527332
005800     EXIT.                                                        0527332
005900                                                                  0527332
006000                                                                  0627537
006100*----------------------------------------------------------------*0627537
006200*U0020                                                           *0627537
006300*U0020 OPTION 0020 DESCRIPTION:                                  *0627537
006400*U0020                                                           *0817657
006500*U0020                                                           *0817657
006600*U0020                                                           *0817657
006700*U0020                                                           *0817657
006800*U0020 MIGRATION HISTORY:                                         0627537
006900*----------------------------------------------------------------*0627537
007000 UO-OPT0020-A.                                                    0627537
008400                                                                  0627537
008500 UO-OPT0020-A-END.                                                0627537
008600     EXIT.                                                        0627537
008700                                                                  0627537
008800***************************************************************** 0627531
008900*U0021 OPTION  0021 DESCRIPTION:                                * 0627531
009000*U0021     OPTION 0021 IS A MODIFICATION TO IM31 TO ALLOW       * 0627531
009100*U0021     MAINTENANCE TO THE PRODUCT TYPE WHEN THE ACCOUNT     * 0627531
009200*U0021     HAS AN INVESTMENT TRAILER.                           * 0627531
009300*U0021 ASSOCIATED PROGRAMS: IM31, TSIM2E01                      * 0627531
009400*U0021 MIGRATION HISTORY:                                       * 0627531
009500***************************************************************** 0627531
009600 UO-OPT0021-A.                                                    0627531
009700 UO-OPT0021-A-EXIT.                                               0627531
009800     EXIT.                                                        0627531
011000***************************************************************** 1020095
011100*U0024 OPTION  0024 DESCRIPTION:                                * 1020095
011200*U0024     OPTION 0024 IS A MODIFICATION TO IM31 TO CONTROL     * 1020095
011300*U0024     WHETHER OR NOT AN ACCOUNT WILL BE PLACED INTO        * 1020095
011400*U0024     NONACCRUAL STATUS.                                     1020095
011500*U0024 ASSOCIATED PROGRAMS: IM31                                * 1020095
011600*U0024 MIGRATION HISTORY:                                       * 1020095
011700***************************************************************** 1020095
011800 UO-OPT0024-A.                                                    1020095
011900     MOVE UB-EXECUTION-OPT(0024) TO WS-U0024-EXEC-DATA.           1020095
012000     IF  WS-U0024-PGM-NAME NOT GREATER SPACES                     1020095
012100         GO TO UO-OPT0024-A-EXIT.                                 1020095
012200     CALL 'SICHKMOD' USING WS-U0024-PGM-NAME                      1020095
012300                           WS-U0024-ERROR.                        1020095
012400     IF  WS-U0024-ERROR EQUAL '0'                                 1020095
012500         GO TO UO-OPT0024-A-EXIT.                                 1020095
012600     MOVE SPACES TO WS-U0024-ERROR.                               1020095
012700     CALL WS-U0024-PGM-NAME USING MASTER-AREA                     1020095
012800                                  WS-U0024-FLAG                   1020095
012900                                  WS-U0024-ERROR.                 1020095
013000     IF  WS-U0024-ERROR NOT EQUAL SPACES                          1020095
013100         GO TO UO-OPT0024-A-EXIT.                                 1020095
013200 UO-OPT0024-A-EXIT.                                               1020095
013300     EXIT.                                                        1020095
999000****************************************************************
999005*  COMMON CLOSE OPTIONAL FILE ROUTINE                          *
999010****************************************************************
999015 UO-TABLE-CLOSE.
999020     IF UO-TABLE-OPEN = 'Y'
999025         MOVE 'E' TO I-O-CONTROL-OPERATOR
999030         PERFORM UO-IO-RTN THRU UO-IO-RTN-X
999035         MOVE 'N' TO  UO-TABLE-OPEN.
999040*
999045* SAMPLE OF CLOSING OPTION 0001 FILE
999050*
999055*    IF OPT0001-FILE-SW  EQUAL 'Y'
999060*       PERFORM UO-OPT0001-G THRU UO-OPT0001-G-EXIT
999065*       MOVE 'N' TO OPT0001-FILE-SW.
999070*
999075 UO-TABLE-CLOSE-EXIT.
999080     EXIT.
