*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*0902557
000500*            IMPACS WORK BCR CARD 4 COPYBOOK                     *0902557
001800*----------------------------------------------------------------*0902557
001900 01  DDA-WRKBCR-4.
002000     03  WBC4-CONTROL-KEY.
002100         05  WBC4-CONTROL-1  PIC XX.
002200         05  WBC4-CONTROL-2  PIC XXX.
002300         05  WBC4-CONTROL-3  PIC XXX.
002400     03  WBC4-RECORD-ID      PIC XX.
002450     03  FILLER              PIC X(6).                            0627561
002452     03  WBC-IM31-TRAN-PH    PIC X(4).                            0627561
002500     03  WBC-SC-STOP-HOLD    PIC X.                               IM003
002600     03  WBC-POST-DORM       PIC X.
002700     03  WBC-POST-INACTIVE   PIC X.
002800     03  WBC-EMPL-TRIAL      PIC X.
002900     03  WBC-AGGREGATE       PIC X.
003000     03  WBC-NONSEQ-TRANS-PHASE      PIC X(4).
003100     03  WBC-CLOSE-OPT       PIC X.                               0827766
003110     03  WBC-LOAN-ATF-PHASE  PIC X(4).                            IM008
003150     03  WBC-IM31-ESCHEAT-PH PIC X(4).                            0902504
003200     03  WBC-IM31-INVEST-PH  PIC X(4).                            0627557
003300     03  WBC-OVERDRAFT-LIMITS.
003400         05  WBC-OD-LIMIT    PIC S9(13)V99 COMP-3                 9915845
003500                 OCCURS 9 TIMES.
003600     03  WBC-BALANCE-CHANGES.
003700         05  WBC-BAL-CHNG    PIC S9(13)V99 COMP-3                 9915845
003800                 OCCURS 9 TIMES.
003900     03  WBC-LARGE-ITEMS.
004000         05  WBC-LRG-ITEM    PIC S9(13)V99 COMP-3                 9915845
004100                 OCCURS 9 TIMES.
004200     03  WBC-MIN-TRANSFER    PIC S9(13)V99 COMP-3.                9915845
004300     03  WBC-LARGE-BAL       PIC S9(13)V99 COMP-3.                9915845
004400     03  WBC-CLOSE-SUSPECT   COMP-3.
004500         05  WBC-CLOSE-BALANCE   PIC S9(13)V99 COMP-3.            9915845
004600         05  WBC-CLOSE-DAYS      PIC S999.
004700     03  WBC-PAYOFF-TOLERANCE.
004800         05  WBC-PAYOFF-TOL-FLAG     PIC X.
004900         05  WBC-PAYOFF-TOL-AMT      PIC S9(13)V99   COMP-3.      9915845
005000         05  WBC-PAYOFF-TOL-NO-DAYS  PIC S9(3)       COMP-3.
005100     03  WBC-LOAN-FLAG       PIC X.
005200     03  WBC-LOAN-ACCR-RECALC PIC X.
005230     03  WBC-PAST-DUE-BOUNDARIES             COMP-3.              1002953
005235         05  WBC-PD-BDRY-A   PIC S9(3).                           1002953
005240         05  WBC-PD-BDRY-B   PIC S9(3).                           1002953
005245         05  WBC-PD-BDRY-C   PIC S9(3).                           1002953
005250         05  WBC-PD-BDRY-D   PIC S9(3).                           1002953
005255         05  WBC-PD-BDRY-E   PIC S9(3).                           1002953
005260         05  WBC-PD-BDRY-F   PIC S9(3).                           1002953
005265     03  FILLER           REDEFINES WBC-PAST-DUE-BOUNDARIES       1002953
005270                          OCCURS 6 TIMES.                         1002953
005275         05  WBC-PD-BDRY     PIC S9(3)       COMP-3.              1002953
005280     03  WBC-STMT-LEAD-DAYS  PIC S9(3)       COMP-3.              1010074
005300     03  FILLER              PIC X(34).                           1010074
005400     03  WBC-OVRLIM-INCR     PIC X.
005500     03  WBC-IM31-USER-PHASE PIC X(4).
005600     03  WBC-LATE-CHG-PHASE  PIC X(4).
005700     03  WBC-CREDIT-LIMITS.
005800         05  WBC-CR-LIMIT    PIC S9(13)V99   COMP-3               9915845
005900                 OCCURS 5 TIMES.
006000     03  WBC-LINE-PCT        PIC SV999       COMP-3.
006010     03  WBC-LINE-PCT-R REDEFINES WBC-LINE-PCT                    IM008
006020                             PIC S99V9       COMP-3.              IM008
006100     03  WBC-LOAN-INCR-TABLE COMP-3.
006200         05  WBC-LOAN-INCR   PIC S9(13)V99   OCCURS 5 TIMES.      9915845
006300     03  WBC-CMA-INCR-TABLE  COMP-3.                              IM008
006400         05  WBC-CMA-INCR    PIC S9(13)V99   OCCURS 5 TIMES.      9915845
006500     03  FILLER                  PIC X(5).                        0817704
006501     03  WBC-OD-XFER-DR-ST       PIC X(4).                        0817704
006502     03  WBC-OD-XFER-DR-AM       PIC X(4).                        0817704
006503     03  WBC-OD-XFER-CR-IM       PIC X(4).                        0817704
006504     03  WBC-OD-XFER-DR-IM       PIC X(4).                        0817704
006505     03  WBC-LOC-PYMT-DR-CODE    PIC X(4).                        0817704
006506     03  WBC-LOC-PYMT-CR-CODE    PIC X(4).                        0817704
006507     03  WBC-FUND-XC-LOC-LIMIT   PIC S9(13)V99 COMP-3.            0817704
006600     03  WBC-FUND-SYS-CODE       PIC X.                           0902557
006700     03  WBC-FUND-COVER-DAYS     PIC S999    COMP-3.              0902557
006800     03  WBC-FUND-IM-DR-CODE     PIC X(4).                        0902557
006900     03  WBC-FUND-IM-CR-CODE     PIC X(4).                        0902557
007000     03  WBC-FUND-ST-DR-CODE     PIC X(4).                        0902557
007100     03  WBC-FUND-ST-CR-CODE     PIC X(4).                        0902557
007200     03  WBC-IM28-FUND-USER-PH   PIC X(4).                        0902557
007300     03  WBC-IM30-IM-TRAN-PH     PIC X(4).                        0902557
007400     03  WBC-IM30-ST-TRAN-PH     PIC X(4).                        0902557
007500     03  WBC-IM30-FMS-TRAN-PH    PIC X(4).                        0902557
007510     03  WBC-IM30-CL-TRAN-PH     PIC X(4).                        2015171
007520     03  WBC-FUND-CL-DR-CODE     PIC X(4).                        2015171
007530     03  WBC-FUND-CL-CR-CODE     PIC X(4).                        2015171
007540     03  WBC-FUND-NSF-FROM-AM    PIC X(1).                        2016612
007550     03  WBC-FUND-NSF-FROM-CL    PIC X(1).                        2016612
007560     03  WBC-FUND-NSF-FROM-IM    PIC X(1).                        2016612
007570     03  WBC-FUND-NSF-FROM-ST    PIC X(1).                        2016612
007575     03  WBC-XINV-DDA-DR-CODE    PIC X(4).                        2016639
007580     03  WBC-XINV-DDA-CR-CODE    PIC X(4).                        2016639
007585     03  WBC-FUND-WL-TRAN-PH     PIC X(4).                        0327005
007590     03  WBC-FUND-WL-DR-CODE     PIC X(4).                        0327005
007595     03  WBC-FUND-WL-CR-CODE     PIC X(4).                        0327005
007600     03  WBC-FUND-NSF-FROM-WL    PIC X(1).                        0327005
007601     03  WBC-FUND-NSF-FROM-XC    PIC X(1).                        0617492
007602     03  WBC-IM30-XC-TRAN-PH     PIC X(4).                        0617492
007605     03  FILLER                  PIC X(07).                       0617492
007610     03  WBC-DFLT-FEE-SOURCE-NO  PIC XXX.                         9715504
007700     03  WBC-NB-FUNDING-OPTIONS.                                  9715504
007800         05  WBC-NB-AL-TRAN-PH         PIC X(04).                 9715504
007900         05  WBC-NB-FUND-AL-CR-CODE    PIC X(04).                 9715504
008000         05  WBC-NB-FUND-AL-DR-CODE    PIC X(04).                 9715504
008100         05  WBC-NB-FUND-INCR          PIC S9(05)     COMP-3.     9715504
008200         05  FILLER                    PIC X(01).                 2016612
008300         05  WBC-NB-OD-FEE-TR-CODE     PIC X(04).                 9715504
009000     03  WBC-PYMT-APPL           PIC X.
009100     03  WBC-PYMT-PHASE          PIC X(4)     OCCURS 3 TIMES.
009200     03  WBC-LOAN-AGGR-ACCR      PIC X.
009300     03  WBC-TRIAL-OPTIONS.
009400         05  WBC-TRANS-PRINT     PIC S9  COMP-3.
009500         05  FILLER              PIC X.                           IM008
009600         05  WBC-TRIAL-SPEC-INST PIC X.
009700         05  FILLER              PIC X.                           IM008
009800     03  WBC-TRIAL-PHASE         PIC X(4).
009900     03  WBC-INT-PAY             PIC X.
009920     03  WBC-IMSA41-USER-PHASE   PIC XX.                          9915749
009930     03  WBC-POSTAL-PHASE        PIC X(4).                        9915845
009940     03  WBC-TAX-ID-PHASE        PIC X(4).                        9915845
009950     03  WBC-TAX-CALC-PHASE      PIC X(4).                        9915845
009960     03  WBC-GEN-TRAN-PHASE      PIC X(4).                        9915845
009970     03  WBC-MAINT-EXC-PHASE     PIC X(4).                        0326947
009980     03  WBC-PROD-MAINT-PHASE    PIC X(4).                        0437214
009982     03  WBC-IM16-FEE-ANALYSIS-PHASE PIC X(4).                    0527323
009984     03  WBC-IM16-FEE-WAIVE-PHASE    PIC X(4).                    0527323
009986     03  WBC-IM16-GL-RECAP-PHASE     PIC X(4).                    0527323
009988     03  WBC-IM16-GL-TOTAL-PHASE     PIC X(4).                    0527323
009990     03  WBC-IM31-OD-SCORING-PHASE   PIC X(4).                    0527323
009992     03  WBC-IM31-OD-SCORING         PIC X.                       0527323
009994     03  WBC-IM22-DC-INIT-PHASE      PIC X(4).                    0527323
010000     03  FILLER                  PIC X(7).                        0527323
010300     03  WBC-TRANSFER-INCRMENTS OCCURS 5 TIMES       COMP-3.
010400         05  WBC-TRAN-INCR           PIC S9(13)V99.               9915845
010500     03  WBC-DAILY-RATE-CODE         PIC X.
010600     03  WBC-DAILY-RATE-PHASE        PIC XXXX.
010700     03  WBC-ACCRUAL-CODE            PIC X.
010800     03  WBC-ACCRUAL-PHASE           PIC XXXX.
010900     03  WBC-INTRST-PAY-PHASE    PIC X(4)        OCCURS 3 TIMES.
011000     03  WBC-ACCR-RECALC         PIC X.                           IM008
011100     03  WBC-RECALC-RATES        PIC X.
011200     03  WBC-OD-PRIORITY             PIC X.
011300     03  WBC-ACCRUAL-CALC            PIC X.
012000     03  FILLER                      PIC X(30).                   IM008
012210     03  WBC-NST-REPORT-NO           PIC XX.                      IM005
012300     03  WBC-NONSEQ-TOT-TRANS-PRINT.
012400         05  WBC-NST-SCHEDULE        PIC X.
012500         05  WBC-NST-TRANS-PRT       PIC X.
012600     03  WBC-LOAN-360-DAY-FACTOR     PIC X.
012700     03  WBC-LOAN-USER-OPTION-FLAG   PIC X.
012800     03  WBC-LOAN-OVER-PMT           PIC X.
012900     03  WBC-LOAN-DAILY-ACCRUAL      PIC X.
012910     03  WBC-LOAN-CAP-ALLOW          PIC X.                       9915845
012920     03  WBC-LOAN-STOP-ACCR-DAYS     PIC S9(3) COMP-3.            0627535
013000     03  FILLER                      PIC X(82).                   0627535
013075     03  WBC-NB-MISC-AREA.                                        9715504
013150         05  WBC-NB-ACCM-BUCKETS.                                 9715504
013225             07  WBC-NB-MAINT-ACCM        PIC S9(03) COMP-3.      9715504
013300             07  WBC-NB-FDIC-ACCM         PIC S9(03) COMP-3.      9715504
013375             07  WBC-NB-CB-ACCM           PIC S9(03) COMP-3.      9715504
013450         05  WBC-NB-HIGH-VOL-ITM          PIC 9(02).              9715504
013525         05  WBC-NB-ABANDON-AREA.                                 9715504
013600             07  WBC-NB-DDA-ABNDN-MTHS     PIC S9(03)     COMP-3. 9715504
013675             07  WBC-NB-SAV-ABNDN-MTHS     PIC S9(03)     COMP-3. 9715504
013750             07  WBC-NB-SAV-ABND-HIBAL-AMT PIC S9(13)V99  COMP-3. 9915845
013825             07  WBC-NB-SAV-ABND-HIBAL-FLG PIC X(01).             9715504
013900             07  WBC-NB-SAV-ABND-HIBAL-MTH PIC S9(03)     COMP-3. 9715504
013975         05  WBC-NB-DORMANT-AREA.                                 9715504
014050             07  WBC-NB-DDA-DORM-FEE-AMT   PIC S9(13)V99  COMP-3. 9915845
014125             07  WBC-NB-DDA-DORM-FEE-FREQ  PIC X(01).             9715504
014200             07  WBC-NB-DDA-DORM-STMT-CNT  PIC S9(03)     COMP-3. 9715504
014275             07  WBC-NB-DORM-INT-ACCRLS    PIC X(01).             9715504
014350             07  WBC-NB-DORM-MAINT-CHRGS   PIC X(01).             9715504
014425             07  WBC-NB-SAV-DORM-FEE-AMT   PIC S9(13)V99  COMP-3. 9915845
014500             07  WBC-NB-SAV-DORM-FEE-FREQ  PIC X(01).             9715504
014575             07  WBC-NB-SAV-DORM-STMT-CNT  PIC S9(03)     COMP-3. 9715504
014650         05  WBC-NB-DUE-DILIGENCE-DT.                             9715504
014725             07  WBC-NB-DUE-DILIGENCE-MO   PIC X(02).             9715504
014800             07  WBC-NB-DUE-DILIGENCE-DA   PIC X(02).             9715504
014875         05  WBC-NB-ESCHEAT-AREA.                                 9715504
014950             07  WBC-NB-ESCHT-DUE-DT.                             9715504
015025                 09  WBC-NB-ESCHT-DUE-MO   PIC X(02).             9715504
015100                 09  WBC-NB-ESCHT-DUE-DA   PIC X(02).             9715504
015175             07  WBC-NB-ESCHT-FLG-DT.                             9715504
015250                 09  WBC-NB-ESCHT-FLG-MO   PIC X(02).             9715504
015325                 09  WBC-NB-ESCHT-FLG-DA   PIC X(02).             9715504
015400         05  WBC-NB-ZBA-TBA-AREA.                                 9715504
015475             07  WBC-NB-ZBA-CR-CODE               PIC X(04).      9715504
015550             07  WBC-NB-ZBA-DR-CODE               PIC X(04).      9715504
015625         05  WBC-NB-MISC-FIELDS.                                  9715504
015700             07  WBC-NB-POST-ALL-PAY-TO-LIM       PIC X(01).      9715504
015775             07  WBC-NB-PROCESS-WHSE              PIC X(01).      9715504
015850             07  WBC-NB-TXN-SORT-OPTION           PIC X(01).      9715504
015925             07  WBC-NB-DAYS-AFT-3REGD-VIO        PIC X(03).      9715504
016000         05  WBC-NB-MISC-FEE-NOS.                                 9715504
016075             07  WBC-NB-SC-BAL-RGE-FEE     PIC X(04).             9715504
016150             07  WBC-NB-SC-FDIC-FEE        PIC X(04).             9715504
016225             07  WBC-NB-SC-MISC-FEE        PIC X(04).             9715504
016300             07  WBC-NB-SC-DRCRD-FEE       PIC X(04).             9715504
016375             07  WBC-NB-SC-MAINT-FEE       PIC X(04).             9715504
016450             07  WBC-NB-SC-ODP-FEE         PIC X(04).             9715504
016525             07  WBC-NB-SC-REGD-CK-FEE     PIC X(04).             9715504
016600             07  WBC-NB-SC-REGD-TR-FEE     PIC X(04).             9715504
016675             07  WBC-NB-SC-DIRDEP-FEE      PIC X(04).             9715504
016750             07  WBC-NB-SC-DEPOSIT-FEE     PIC X(04).             9715504
016825             07  WBC-NB-SC-AUTO-DR-FEE     PIC X(04).             9715504
016900             07  WBC-NB-SC-TRUNC-FEE       PIC X(04).             9715504
016975             07  WBC-NB-SC-NSF-FEE         PIC X(04).             9715504
017050             07  WBC-NB-SC-OD-FEE          PIC X(04).             9715504
017125             07  WBC-NB-SC-DAU-FEE         PIC X(04).             9715504
017200             07  WBC-NB-SC-CUST-SAV-TYP    PIC X(03).             9715504
017275             07  WBC-NB-SC-SS-CHKG-TYP     PIC X(03).             9715504
017350         05  WBC-NB-GL-CLEARING.                                  9715504
017425             07  WBC-NB-GL-CLR-CC          PIC X(10).             9715504
017500             07  WBC-NB-GL-CLR-ACCT        PIC X(10).             9715504
017575         05  WBC-NB-GL-UNPOSTED.                                  9715504
017650             07  WBC-NB-GL-UNP-CC          PIC X(10).             9715504
017725             07  WBC-NB-GL-UNP-ACCT        PIC X(10).             9715504
018400     03  WBC-OD-INT-PHASE        PIC XXXX.
018500     03  WBC-SC-STMT-SYMBOLS.
018600         05  WBC-SC-STMT-SYM     PIC XX    OCCURS 35 TIMES.
018700     03  FILLER                  PIC X(12).                       1003625
019300     03  WBC-OD-MIN-CHG-AMT      PIC S9(13)V99  COMP-3.           9915845
019400     03  WBC-NB-ADDL-AREA.                                        9715504
019500         05  WBC-NB-SEAS-ACCM          PIC S9(03) COMP-3.         9715504
019600         05  WBC-NB-ALS-WIP-INFO.                                 9715504
019700             07  WBC-NB-ALS-WIP-CC     PIC X(10).                 9715504
019800             07  WBC-NB-ALS-WIP-ACCT   PIC X(10).                 9715504
019900         05  WBC-NB-SC-OD-INT-FEE      PIC X(04).                 9715504
020000         05  WBC-NB-SC-TRAN-CR-FEE     PIC X(04).                 9715504
020100         05  WBC-NB-SC-TRAN-DR-FEE     PIC X(04).                 9715504
020200         05  WBC-NB-SC-TRAN-ITM-FEE    PIC X(04).                 9715504
020300         05  WBC-NB-SC-ST-PAY-CUST-FEE PIC X(04).                 9715504
020400         05  WBC-NB-SC-ST-PAY-BANK-FEE PIC X(04).                 9715504
020500         05  WBC-IMAGE-STMT-FEE        PIC X(04).                 0417110
020600         05  WBC-CD-ROM-STMT-FEE       PIC X(04).                 0417110
020700         05  WBC-ELECTRONIC-STMT-FEE   PIC X(04).                 0417110
020800     03  WBC-OD-CONSEC-DAYS1           PIC S9(3)      COMP-3.     0627540
020900     03  WBC-OD-CONSEC-DAYS2           PIC S9(3)      COMP-3.     0627540
021000     03  WBC-OD-CONSEC-BAL             PIC S9(13)V99  COMP-3.     0627540
021100     03  FILLER                        PIC X(467).                0627540
