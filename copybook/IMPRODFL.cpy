*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000010*----------------------------------------------------------------*0900620
000020*        PRODUCT FILE RECORD LAYOUT COPYBOOK                     *0900620
000030*----------------------------------------------------------------*0900620
000100 01  PRODUCT-REC.
000150   02  FILLER.                                                    1004741
000200     03  PF-KEY.
000300         05  PF-CONTROLS.
000400             07  PF-CTL1             PIC XX.
000500             07  PF-CTL2             PIC XXX.
000600             07  PF-CTL3             PIC XXX.
000700         05  PF-TYPE                 PIC XXX.
000800     03  FILLER                      PIC X(10).
000900   02  PF-FIXED-MASTER.
001000     03  FILLER                      PIC X(6).                    IM008
001400     03  PFF-MISC-FLAGS.
001500         05  PFF-LARGE-ITEM          PIC S9          COMP-3.      IM004
001600         05  PFF-CLOSE-OVERRIDE      PIC X.
001700         05  PFF-BAL-CHANGE          PIC S9          COMP-3.      IM004
001800         05  PFF-LIST-POST           PIC X.
001900         05  PFF-NSF-CALC            PIC X.
002000         05  PFF-COLL-CALC           PIC X.
002100         05  PFF-CUST-COLL           PIC X.
002110         05  PFF-OD-CALC             PIC X.                       9915845
002200         05  PFF-BANK-AVAIL-FLAG     PIC X.                       IM007
002300         05  PFF-CUST-AVAIL-FLAG     PIC X.                       IM007
002400         05  PFF-ARP                 PIC X.
002500         05  PFF-ARP-OTHER           PIC X.
002600         05  PFF-ARP-GEN             PIC X.
002700         05  PFF-ARP-STMT            PIC X.
002800         05  PFF-ARP-N-A             PIC X.
002900         05  PFF-ARP-ISSUE           PIC X.                       IM004
003000         05  PFF-SPECIAL-REPORT      PIC X.
003100         05  PFF-ACH-AUTHORIZED      PIC X.
003200         05  PFF-ANALYSIS            PIC X.
003300         05  PFF-CHARGE-CARD         PIC X.
003400         05  PFF-BANK-AVAIL-EXCEPTION PIC X.                      IM007
003500         05  PFF-BULK-FILE           PIC X.
003600         05  PFF-TRANSFER-PRIORITY   PIC X.
003610         05  PFF-STOP-HOLD-WAIVE     PIC X.                       IM003
003620         05  PFF-EXT-DEP-TRLR        PIC X.                       IM004
003630         05  PFF-NON-TAXABLE         PIC X.                       IM005
003640         05  PFF-CHK-TRUNC-FLAG      PIC X.                       IM006
003650         05  PFF-KITING-FLAG         PIC X.                       IM006
003660         05  PFF-CASH-AVAIL-FLAG     PIC X.                       IM007
003670         05  PFF-CHG-TERMS-FLAG      PIC X.                       1004015
003680         05  PFF-DUAL-YEAR-FLAG      PIC X.                       9915858
003690         05  PFF-PASSBOOK            PIC X.                       9915845
003695         05  PFF-MIN-BAL-CALC        PIC X.                       9916072
003700         05  PFF-OL-CALC.                                         9916157
003702             07  PFF-OL-CALC1        PIC X.                       9916157
003704             07  PFF-OL-CALC2        PIC X.                       9916157
003710         05  PFF-STATE-LOCAL-TX-CD   PIC X.                       2012254
003715         05  PFF-TAX-FED-PTR         PIC S999   COMP-3.           2012254
003720         05  PFF-TAX-STATE-PTR       PIC S999   COMP-3.           2012254
003725         05  PFF-TAX-LOCAL-PTR       PIC S999   COMP-3.           2012254
003750     03  PFF-DAILY-RATE-CODE         PIC X.                       0817657
003752     03  PFF-LOAN-360-DAY-FACTOR     PIC X.                       0817657
003754     03  PFF-ARCH-HIST-RET-DAYS      PIC S999   COMP-3.           0927050
003780     03  FILLER                      PIC X(5).                    0927050
003788     03  PFF-FORCE-POST              PIC X.                       0827769
003790     03  PFF-SOURCE-OF-FUNDS         PIC X(3).                    9915845
003800     03  PFF-SYSTEM-TYPE             PIC XXX.
003900     03  PFF-LEDGER                  PIC XXX.
004000     03  PFF-GEN-LEDGER.
004100         05  PFF-GL-CODE             PIC XX.
004200         05  PFF-GL-USER             PIC X(10).
004300     03  PFF-USER-CODES.
004400         05  PFF-USER-1              PIC XXX.
004500         05  PFF-USER-2              PIC XXX.
004600         05  PFF-USER-3              PIC XXX.
004700         05  PFF-USER-4              PIC XXX.
004800         05  PFF-USER-5              PIC XXX.
004900         05  PFF-USER-6              PIC XXX.
005000     03  PFF-USER-BYTES  REDEFINES  PFF-USER-CODES
005100                                     PIC X       OCCURS 18 TIMES.
005200     03  PFF-CLUB-ACCOUNT            PIC XX.
005300     03  PFF-BRT-TERM-OPT            PIC X.                       0417066
005302     03  PFF-BRT-TERM-LMT            PIC S999        COMP-3.      0417066
005500     03  PFF-BALANCE-HISTORY         PIC X.                       0902213
005600     03  PFF-BAL-HIST-RET            PIC S999        COMP-3.      0902213
005610     03  PFF-0BAL-GRACE-DAYS         PIC S999        COMP-3.      0902079
005620     03  PFF-TIS-CONSUMER-FLAG       PIC X.                       1004382
005630     03  PFF-TISA-AGGR-BAL-CD        PIC X.                       1004595
005700     03  PFF-NB-THRESHOLD-DEFAULT    PIC S9(13)V99   COMP-3.      9915845
005800     03  PFF-OD-NSF-TRLR             PIC X.
005900     03  PFF-OVERDRAFT.
006000         05  PFF-OD-LMT-CODE         PIC X.
006100         05  PFF-OD-LMT-AMT          PIC S9(13)V99   COMP-3.      9915845
006200         05  FILLER                  PIC X.
006300         05  PFF-OD-CHARGING.
006400             07  PFF-OD-CHG-CODE     PIC X.
006500             07  PFF-OD-CHG-AMT      PIC S9(13)V99   COMP-3.      9915845
006600             07  PFF-OD-SUPPRESS-NTC PIC X.                       0900620
006700             07  PFF-OD-CHG-EXC-CODE PIC X.
006800             07  PFF-OD-CHG-MINIMUM  PIC S9(13)V99   COMP-3.      9915845
007000             07  PFF-OD-CHG-MAXIMUM  PIC S9(13)V99   COMP-3.      9915845
007200             07  PFF-OD-CHG-WAIVE    PIC X.
007300         05  PFF-OD-FOLLOW-UP        PIC S999        COMP-3.
007400     03  PFF-NSF-CHARGE-DATA.
007500         05  PFF-NSF-CHG-CODE        PIC X.
007600         05  PFF-NSF-CHG-AMT         PIC S9(13)V99   COMP-3.      9915845
007700         05  PFF-NSF-SUPPRESS-NTC    PIC X.                       0900620
007800         05  PFF-NSF-CHG-MAX         PIC S9(13)V99   COMP-3.      9915845
008000         05  PFF-NSF-CHG-WAIVE       PIC X.
008100     03  PFF-SERVICE-CHARGE-DATA.
008200         05  PFF-SC-TYPE             PIC XXX.                     1003625
008300         05  PFF-SC-ROUTINE          PIC S9          COMP-3.
008400         05  PFF-SC-WAIVE            PIC X.
008500         05  PFF-SC-WAIVE-RSN        PIC XX.
008600         05  PFF-SC-SAV-BAL          PIC X.
008700         05  PFF-SC-CYCLE-CODE       PIC X.
008800         05  PFF-SC-CYCLE-FREQ       PIC X.
008900         05  PFF-SC-CYCLE-MONTH      PIC X.
009000         05  PFF-SC-CYCLE-DAY        PIC XX.
009100         05  PFF-SC-CHECK-CHG-CODE   PIC X.
009200         05  PFF-SC-CHECK-CHG-LMT    PIC S999        COMP-3.
009300         05  PFF-SC-CHECK-CHG-AMT    PIC S9(12)V999  COMP-3.      9915845
009302     03  PFF-OD-MIN-CHG              PIC S9(9)V99    COMP-3.      0827728
009304     03  PFF-OD-REGE-OPT-CODE        PIC X.                       1020116
009320     03  PFF-OD-REGE-OPT-REASON      PIC XX.                      1020116
009400     03  PFF-DAU-MIN-AMT             PIC S9(13)V99   COMP-3.      9915845
009420     03  PFF-DAU-MAX-CHG             PIC S9(13)V99   COMP-3.      9915845
009500     03  PFF-MIN-DDA-BAL-TO-MAINTAIN PIC S9(13)V99   COMP-3.      9915845
009600     03  PFF-CUST-AVAIL-CODE         PIC X.                       IM007
009700     03  PFF-CUST-AVAIL-CHARGE       PIC S9(13)V99   COMP-3.      9915845
009800     03  PFF-DAU-WAIVE               PIC X.                       IM003
009900     03  PFF-STATEMENT-DATA.
010000         05  PFF-ST-PULL-CODE        PIC X.
010100         05  PFF-ST-SUPP-CODE        PIC X.
010200         05  PFF-ST-CYCLE            PIC XX.
010300         05  PFF-ST-MESSAGE          PIC X.
010400         05  PFF-ST-FULL-SHEET       PIC X.
010500         05  PFF-ST-FORMAT           PIC S9          COMP-3.
010600         05  PFF-ST-SPECIAL-HANDLING PIC X.
010700         05  PFF-ST-NUM-STMTS        PIC S9          COMP-3.
010800     03  PFF-COMBINED-STMT-ACCT      PIC X.
010900     03  PFF-COMB-STMT-SEP-FILE      PIC X.
010910     03  PFF-CUST-STMT-PRICE         PIC X.                       9715504
010920     03  PFF-IMAGES-PER-PAGE         PIC S9(3)       COMP-3.      0417110
010922     03  PFF-SIMPLEX-DUPLEX          PIC X(1).                    0417110
011000     03  PFF-MIN-CR-INT              PIC S9(9)V99    COMP-3.      0827727
011100     03  PFF-IOD-DATA.
011200         05  PFF-IOD-CALC-CODE       PIC X.
011300         05  PFF-IOD-RATE-PTR        PIC S999        COMP-3.      1003625
011310         05  PFF-IOD-MIN-RATE        PIC S9V9(8)     COMP-3.      9915845
011320         05  PFF-IOD-MAX-RATE        PIC S9V9(8)     COMP-3.      9915845
011400         05  PFF-IOD-PAY-PHS-PTR     PIC S9          COMP-3.
011500         05  PFF-IOD-HIFI-IND        PIC S999        COMP-3.      1003625
011600         05  PFF-IOD-TAX-WITHHOLD    PIC X.
011700         05  PFF-IOD-TAX-EXEMPT      PIC X.
011800         05  PFF-IOD-TIN-CERT        PIC X.
012000         05  PFF-IOD-ACCRUAL-TYPE    PIC X.                       0902509
012100         05  PFF-IOD-MIN-HIFI-AMT    PIC S9(13)V99   COMP-3.      9915845
012200         05  PFF-IOD-RATE-USE        PIC X.                       1003625
012250         05  PFF-MMDA-INDICATOR      PIC X.                       0902509
012300         05  PFF-IOD-MAX-HIFI-TRANS  PIC S999        COMP-3.
012310     03  PFF-IOD-DISTRIBUTION-INFO.                               IM006
012320         05  PFF-IOD-DIST-CD         PIC X.                       IM006
012330         05  PFF-IOD-DIST-DR-TC      PIC X(4).                    IM006
012340         05  PFF-IOD-DIST-CR-TC      PIC X(4).                    IM006
012350     03  PFF-IOD-INT-CYCLE.                                       9915845
012360         05  PFF-IOD-PAY-CYCLE       PIC X.                       9915845
012370         05  PFF-IOD-INT-INCR        PIC X.                       9915845
012380         05  PFF-IOD-INT-MONTH1      PIC X(02).                   9915845
012390         05  PFF-IOD-INT-DAY         PIC X(02).                   9915845
012395         05  PFF-IOD-INT-DAY2        PIC X(02).                   9915845
012400     03  PFF-EXTERNAL-INVESTMENT.                                 2016639
012405         05  PFF-XINV-ALLOWED        PIC X.                       2016639
012410         05  PFF-XINV-PROCESS-IND    PIC X.                       2016639
012415         05  PFF-XINV-BAL-CALC       PIC X.                       2016639
012480     03  PFF-TRUST-CODE              PIC X.                       1010086
012481     03  PFC-BRT-TERM-CNV            PIC X(3).                    0417066
012482     03  PFC-MIN-INAC-CNV            PIC X(3).                    0417069
012490     03  PFC-STOP-HIT-OPTION         PIC X.                       0266778
012492     03  PFF-SUSPECT-LIMIT-RPT       PIC X.                       0266778
012494     03  PFF-SUSPECT-LIMIT           PIC S9(13)V99   COMP-3.      0266778
012496     03  PFC-MULTI-STOP-HIT          PIC X.                       0266778
012500     03  PFF-DORMANT-OPT             PIC X.
012600     03  PFF-DORMANT-DAYS            PIC S999        COMP-3.
012700     03  PFF-HIFI-DORMANT-OPT        PIC X.
012800     03  PFF-HIFI-DORMANT-DAYS       PIC S999        COMP-3.
012900     03  PFF-INACTIVE-OPT            PIC X.
013000     03  PFF-INACTIVE-DAYS           PIC S999        COMP-3.
013100     03  PFF-PURGE-OPT               PIC X.
013200     03  PFF-PURGE-DAYS              PIC S999        COMP-3.
013210     03  PFF-USER-FLD1               PIC X(05).                   0903293
013220     03  PFF-USER-FLD2               PIC X(05).                   0903293
013230     03  PFF-CITIZEN-COUNTRY         PIC XX.                      9915845
013240     03  PFF-OD-PER-CHRG-FLAG        PIC X.                       9916073
013250     03  PFF-OD-PER-GRACE-DAYS       PIC S999        COMP-3.      9916073
013260     03  PFF-OD-PER-CHRG-DAYS        PIC S999        COMP-3.      9916073
013270     03  PFF-OD-PER-MIN-AMT          PIC S9(13)V99   COMP-3.      9916073
013280     03  PFF-OD-PER-CHRG-AMT         PIC S9(13)V99   COMP-3.      9916073
013282     03  PFF-CLOSE-CHG-INFO.                                      0527322
013284         05  PFF-CLOSE-DAYS          PIC S9(3)       COMP-3.      0527322
013286         05  PFF-CLOSE-AMT           PIC S9(13)V99   COMP-3.      0527322
013288         05  PFF-CLOSE-WVE           PIC X.                       0527322
013300     03  FILLER                      PIC X(39).                   0527322
013350   02  FILLER                        PIC X(50).                   0930011
013550   02  PF-LOAN-DATA.                                              IM003
013600     03  PFL-SET-UP-FLAG             PIC X.
013700     03  PFL-CREDIT-LIMIT-DATA.
013800         05  PFL-CR-LMT-POINTER      PIC S9          COMP-3.
013900         05  PFL-CR-LMT-AMT          PIC S9(13)V99   COMP-3.      9915845
014000     03  PFL-CRED-CARD-IND           PIC X.                       1020057
014100     03  PFL-LOAN-INCREMENT          PIC S9          COMP-3.
014200     03  PFL-PAYMENT-DATA.
014300         05  PFL-FIXED-PYMT-CODE     PIC X.
014400         05  PFL-FIXED-PYMT-AMT      PIC S9(13)V99   COMP-3.      9915845
014500         05  FILLER                  PIC XX.
014600         05  PFL-PYMT-TYPE           PIC X.
014700         05  PFL-CALC-PHASE          PIC S9          COMP-3.
014800         05  PFL-CALC-CODE           PIC X.
014900         05  PFL-CALC-DAY            PIC XX.
015000         05  PFL-DUE-CODE            PIC X.
015100         05  PFL-DUE-DAYS            PIC XX.                      IM004
015200         05  PFL-AUTO-PAYMENT        PIC X.
015250     03  PFL-PDUE-NTC-DAYS           PIC S999        COMP-3.      0901937
015300     03  PFL-REG-CODE                PIC 9(3).                    1004773
015310     03  PFL-HOME-EQUITY-CODE        PIC X.                       IM007
015320     03  PFL-COMPT-CALL-CODE         PIC X(4).                    IM007
015400     03  PFL-PARTIAL-PYMT-FLAG       PIC X.
015500     03  PFL-PARTIAL-PYMT-MIN        PIC S9(13)V99   COMP-3.      9915845
015600     03  PFL-PAST-DUE-DAYS           PIC S999        COMP-3.
015700     03  PFL-PAST-DUE-POST-PRI       PIC XX.
015800     03  PFL-CMA-CREDIT-LINE-PERCENT PIC S9V99       COMP-3.
015900     03  PFL-CREDIT-REVIEW-FREQ      PIC X.
016000     03  PFL-AUTO-OVER-LIMIT-1INCR   PIC X.
016100     03  PFL-LOAN-TYPE               PIC XX.
016200     03  PFL-CAPITALIZATION          PIC X.
016300     03  PFL-BILL-CALCED-FLAG        PIC X.
016310     03  FILLER                      PIC X(6).                    9915845
016400     03  PFL-INTEREST-RATE-DATA.
016500         05  PFL-INT-PTR             PIC S999        COMP-3.      1003625
016600         05  PFL-INT-RATE            PIC S9V9(8)     COMP-3.      9915845
016610         05  PFL-INT-RATE-9      REDEFINES PFL-INT-RATE           9715504
016620                                     PIC S999V9(6)   COMP-3.      9915845
016650         05  PFL-MAX-LOAN-INT        PIC S9V9(8)     COMP-3.      9915845
016655         05  PFL-MAX-LOAN-INT-9  REDEFINES PFL-MAX-LOAN-INT       9715504
016660                                     PIC S999V9(6)   COMP-3.      9915845
016700     03  FILLER                      PIC X(6).                    9715504
016800     03  PFL-SPLIT-RATE-DATA.
016900         05  PFL-SR-POINTER          PIC S999        COMP-3.      1003625
017000         05  PFL-SR-INDICATOR        PIC X.
017100         05  PFL-SR-RATE-1           PIC S9V9(8)     COMP-3.      9915845
017110         05  PFL-SR-RATE-1-9     REDEFINES PFL-SR-RATE-1          9715504
017120                                     PIC S999V9(6)   COMP-3.      9915845
017200         05  PFL-SR-LIMIT-1          PIC S9(13)V99   COMP-3.      9915845
017300         05  PFL-SR-RATE-2           PIC S9V9(8)     COMP-3.      9915845
017310         05  PFL-SR-RATE-2-9     REDEFINES PFL-SR-RATE-2          9715504
017320                                     PIC S999V9(6)   COMP-3.      9915845
017400         05  PFL-SR-LIMIT-2          PIC S9(13)V99   COMP-3.      9915845
017500         05  PFL-SR-RATE-3           PIC S9V9(8)     COMP-3.      9915845
017510         05  PFL-SR-RATE-3-9     REDEFINES PFL-SR-RATE-3          9715504
017520                                     PIC S999V9(6)   COMP-3.      9915845
017600     03  PFL-PRIME-RATE-DATA.
017700         05  PFL-PR-ADJ-CODE         PIC X.
017800         05  PFL-PR-ADJ-RATE         PIC S9V9(8)     COMP-3.      9915845
017810         05  PFL-PR-ADJ-RATE-9   REDEFINES PFL-PR-ADJ-RATE        9715504
017820                                     PIC S999V9(6)   COMP-3.      9915845
017900         05  PFL-PR-CHANGE-FLAG      PIC X.
018000         05  PFL-PR-BAL-CALC         PIC X.
018100         05  PFL-PR-RATE             PIC S9V9(8)     COMP-3.      9915845
018110         05  PFL-PR-RATE-9       REDEFINES PFL-PR-RATE            9715504
018120                                     PIC S999V9(6)   COMP-3.      9915845
018200     03  FILLER                      PIC X(9).                    1003625
018300     03  PFL-INSURANCE-DATA.
018400         05  PFL-INS1-FLAG           PIC X.
018500         05  PFL-INS1-COMPANY        PIC XXX.
018600         05  PFL-INS1-RATE           PIC S9V9(8)     COMP-3.      9915845
018700         05  PFL-INS2-FLAG           PIC X.
018800         05  PFL-INS2-COMPANY        PIC XXX.
018900         05  PFL-INS2-RATE           PIC S9V9(8)     COMP-3.      9915845
019000     03  PFL-PENALTY-DATA.
019100         05  PFL-PNLTY-CODE          PIC X.
019200         05  PFL-PNLTY-CHARGE        PIC S9(13)V99   COMP-3.      9915845
019300         05  PFL-PNLTY-GRACE-DAYS    PIC S999        COMP-3.
019400         05  PFL-PNLTY-WAIVE-FLAG    PIC X.
019500     03  PFL-AUTO-REACTIVATE         PIC X.
019510     03  PFL-ORIG-COSTS-DATA.                                     IM006
019520         05  PFL-COSTS-CODE          PIC X.                       IM006
019530         05  PFL-COSTS-DAY           PIC XX.                      IM006
019540         05  PFL-COSTS-RECALC        PIC X.                       IM006
019550         05  PFL-COSTS-TERM          PIC S999        COMP-3.      IM006
019600     03  FILLER                      PIC X(20).                   9915845
019700   02  PF-MARKETING-DATA.
019800     03  PFM-SET-UP-FLAG             PIC X.
019900     03  PFM-KEY-ACCOUNT             PIC X.
020000     03  PFM-LARGE-ITEM-POINTER      PIC S9          COMP-3.
020100     03  PFM-BAL-CHANGE-POINTER      PIC S9          COMP-3.
020200     03  PFM-CALL-SCHEDULE.
020300         05  PFM-CS-CODE             PIC X.
020400         05  PFM-CS-TYPE             PIC X.
020500         05  PFM-CS-MAX-PERIOD       PIC S999        COMP-3.
020600         05  PFM-CS-SCHED-NUMBER     PIC S999        COMP-3.
020700     03  PFM-REPORTABLE-FLUCTUATION.
020800         05  PFM-RF-PERCENT-DOWN     PIC S9V99       COMP-3.
020900         05  PFM-RF-AMOUNT-DOWN      PIC S9(13)V99   COMP-3.      9915845
021000         05  PFM-RF-PERCENT-UP       PIC S9V99       COMP-3.
021100         05  PFM-RF-AMOUNT-UP        PIC S9(13)V99   COMP-3.      9915845
021200         05  PFM-RF-LOW-LIMIT        PIC S9(13)V99   COMP-3.      9915845
021300         05  PFM-RF-HIGH-LIMIT       PIC S9(13)V99   COMP-3.      9915845
021400         05  PFM-RF-LARGE-LIMIT      PIC S9(13)V99   COMP-3.      9915845
021500     03  PFM-BAL-OPT                 PIC X.
021600     03  PFM-BAL-CALC                PIC X.
021650     03  PFM-ANALYSIS-CODE           PIC XX.                      IM006
021660     03  FILLER                      PIC X(12).                   9915845
021700   02  PF-FUNDS-AVAILABILITY.                                     IM007
021705     03  PFA-EFA-SET-UP-FLAG         PIC X.                       IM007
021710     03  PFA-EFA-CALC-FLAG           PIC X.                       IM007
021715     03  PFA-EFA-LARGE-DEPOSIT-AMT   PIC S9(13)V99   COMP-3.      9915845
021720     03  PFA-EFA-CASH-SCHEDULE.                                   IM007
021725         05  PFA-EFA-NEXT-PTR-1      PIC X.                       IM007
021730         05  PFA-EFA-NEXT-PTR9-1 REDEFINES PFA-EFA-NEXT-PTR-1     IM007
021735                                     PIC 9.                       IM007
021740         05  PFA-EFA-NEXT-AMT-1      PIC S9(13)V99   COMP-3.      9915845
021745         05  PFA-EFA-SPLT-PTR-1      PIC X.                       IM007
021750         05  PFA-EFA-SPLT-PTR9-1 REDEFINES PFA-EFA-SPLT-PTR-1     IM007
021755                                     PIC 9.                       IM007
021760         05  PFA-EFA-SPLT-AMT-1      PIC S9(13)V99   COMP-3.      9915845
021765         05  PFA-EFA-PERCENT-1       PIC S9V99       COMP-3.      IM007
021820     03  PFA-EFA-OD-ALL-AMT          PIC S9(13)V99   COMP-3.      9915845
021830     03  FILLER                      PIC X(04).                   9916072
021850   02  PF-SAVINGS-DATA.                                           IM007
021860     03  PFS-CMA-FEE-MIN-AMT         PIC S9(13)V99   COMP-3.      9916072
021900     03  PFS-SET-UP-FLAG             PIC X.
022000     03  PFS-USER-CODE               PIC X.
022100     03  PFS-INTEREST-POINTER        PIC S999        COMP-3.      1003625
022110     03  PFS-SAV-MIN-RATE            PIC S9V9(8)     COMP-3.      9915845
022120     03  PFS-SAV-MAX-RATE            PIC S9V9(8)     COMP-3.      9915845
022200     03  PFS-TRANSFER-INCREMENT      PIC S9          COMP-3.
022300     03  PFS-PAY-PHASE-POINTER       PIC S9          COMP-3.
022500     03  PFS-DORMANT-OPT             PIC X.
022600     03  PFS-DORMANT-DAYS            PIC S999        COMP-3.
022700     03  PFS-MAX-DDA-BAL-CODE        PIC X.
022800     03  PFS-MAX-DDA-BAL-AMT         PIC S9(13)V99   COMP-3.      9915845
022810     03  PFS-SAV-BAL-HIST            PIC X.                       0902213
022820     03  PFS-SAV-BAL-HIST-RET        PIC S9(3)       COMP-3.      0902213
022830     03  PFS-INV-IND                 PIC X.                       1004554
022840     03  PFS-INV-TYPE                PIC X(3).                    1004554
022850     03  PFS-INV-SVC-RT              PIC S9V9(8)     COMP-3.      9915845
022860     03  PFS-INV-CONFIRM             PIC X.                       1004554
023000     03  PFS-CMA-DATA.
023100         05  PFS-CMA-IND             PIC X.
023200         05  PFS-CMA-SWEEP-FREQ      PIC X.
023300         05  PFS-CMA-SWEEP-INCR      PIC S999        COMP-3.
023400         05  PFS-CMA-BAL-CALC        PIC X.
023500         05  PFS-CMA-BAL-USE         PIC X.
023550         05  PFS-CMA-INCR-PTR        PIC S9          COMP-3.      IM008
023600     03  PFS-SUPER-ELIGIBILITY-DATA.
023700         05  PFS-SUPER-MAX-TRANS     PIC S999        COMP-3.
023800         05  PFS-SUPER-MIN-BAL       PIC S9(13)V99   COMP-3.      9915845
023810     03  PFS-SAV-DISTRIBUTION-INFO.                               IM006
023820         05  PFS-SAV-DIST-CD         PIC X.                       IM006
023830         05  PFS-SAV-DIST-DR-TC      PIC X(4).                    IM006
023840         05  PFS-SAV-DIST-CR-TC      PIC X(4).                    IM006
023850     03  PFS-SAV-INT-CYCLE.                                       9915845
023860         05  PFS-SAV-INT-PAY-CYCLE   PIC X.                       9915845
023870         05  PFS-SAV-INT-INCR        PIC X.                       9915845
023880         05  PFS-SAV-INT-MONTH1      PIC X(02).                   9915845
023890         05  PFS-SAV-INT-DAY         PIC X(02).                   9915845
023895         05  PFS-SAV-INT-DAY2        PIC X(02).                   9915845
023896     03  PFS-CMA-SWEEP-IND           PIC X.                       9916072
023897     03  PFS-CMA-FEE-WAIVE-IND       PIC X.                       9916072
023898     03  PFS-COLL-CALC               PIC X.                       9916070
023900     03  PFS-SAV-TIER-PTR            PIC S999        COMP-3.      0316967
023910     03  PFS-SAV-RATE-USE            PIC X.                       0316967
023950     03  FILLER                      PIC X(03).                   0316967
024000   02  PF-OD-ACCRUAL.
024100     03  PFO-SET-UP-FLAG             PIC X.
024200     03  PFO-OD-ACCR-BAL-CALC        PIC X.
024400     03  PFO-OD-ACCR-INT-WAIVE       PIC X.
024410     03  PFO-OD-MIN-RATE             PIC S9V9(8)     COMP-3.      9915845
024420     03  PFO-OD-MAX-RATE             PIC S9V9(8)     COMP-3.      9915845
024430     03  FILLER                      PIC X(10).                   9915845
024500     03  PFO-OD-ACCR-RATE-AREA.
024610         05  PFO-OD-RATE1-PTR        PIC S999        COMP-3.      9915845
024620         05  PFO-OD-RATE2-PTR        PIC S999        COMP-3.      9915845
024630         05  PFO-OD-RATE3-PTR        PIC S999        COMP-3.      9915845
024640         05  PFO-OD-RATE4-PTR        PIC S999        COMP-3.      9915845
024650         05  PFO-OD-RATE5-PTR        PIC S999        COMP-3.      9915845
024660         05  PFO-OD-RATE6-PTR        PIC S999        COMP-3.      9915845
024670         05  PFO-OD-RATE7-PTR        PIC S999        COMP-3.      9915845
024680         05  PFO-OD-RATE8-PTR        PIC S999        COMP-3.      9915845
024690         05  PFO-OD-RATE9-PTR        PIC S999        COMP-3.      9915845
024700         05  PFO-OD-LMT-1-CODE       PIC X.                       9915845
024710         05  PFO-OD-LMT-1-CODE9 REDEFINES PFO-OD-LMT-1-CODE PIC 9.9915845
024720         05  PFO-OD-LMT-1-AMT        PIC S9(13)V99   COMP-3.      9915845
024730         05  PFO-OD-LMT-2-CODE       PIC X.                       9915845
024740         05  PFO-OD-LMT-2-CODE9 REDEFINES PFO-OD-LMT-2-CODE PIC 9.9915845
024750         05  PFO-OD-LMT-2-AMT        PIC S9(13)V99   COMP-3.      9915845
024760         05  PFO-OD-LMT-3-CODE       PIC X.                       9915845
024770         05  PFO-OD-LMT-3-CODE9 REDEFINES PFO-OD-LMT-3-CODE PIC 9.9915845
024780         05  PFO-OD-LMT-3-AMT        PIC S9(13)V99   COMP-3.      9915845
024790         05  PFO-OD-LMT-4-CODE       PIC X.                       9915845
024800         05  PFO-OD-LMT-4-CODE9 REDEFINES PFO-OD-LMT-4-CODE PIC 9.9915845
024810         05  PFO-OD-LMT-4-AMT        PIC S9(13)V99   COMP-3.      9915845
024820         05  PFO-OD-LMT-5-CODE       PIC X.                       9915845
024830         05  PFO-OD-LMT-5-CODE9 REDEFINES PFO-OD-LMT-5-CODE PIC 9.9915845
024840         05  PFO-OD-LMT-5-AMT        PIC S9(13)V99   COMP-3.      9915845
024850         05  PFO-OD-LMT-6-CODE       PIC X.                       9915845
024860         05  PFO-OD-LMT-6-CODE9 REDEFINES PFO-OD-LMT-6-CODE PIC 9.9915845
024870         05  PFO-OD-LMT-6-AMT        PIC S9(13)V99   COMP-3.      9915845
024880         05  PFO-OD-LMT-7-CODE       PIC X.                       9915845
024890         05  PFO-OD-LMT-7-CODE9 REDEFINES PFO-OD-LMT-7-CODE PIC 9.9915845
024900         05  PFO-OD-LMT-7-AMT        PIC S9(13)V99   COMP-3.      9915845
024910         05  PFO-OD-LMT-8-CODE       PIC X.                       9915845
024920         05  PFO-OD-LMT-8-CODE9 REDEFINES PFO-OD-LMT-8-CODE PIC 9.9915845
024930         05  PFO-OD-LMT-8-AMT        PIC S9(13)V99   COMP-3.      9915845
024940     03  PFO-OD-ACCR-PRIME-AREA.                                  9915845
024950         05  PFO-OD-PRIME-ADJ1       PIC S9V9(8)     COMP-3.      9915845
024960         05  PFO-OD-PRIME-ADJ2       PIC S9V9(8)     COMP-3.      9915845
024970         05  PFO-OD-PRIME-ADJ3       PIC S9V9(8)     COMP-3.      9915845
024980         05  PFO-OD-PRIME-ADJ4       PIC S9V9(8)     COMP-3.      9915845
024990         05  PFO-OD-PRIME-ADJ5       PIC S9V9(8)     COMP-3.      9915845
025000         05  PFO-OD-PRIME-ADJ6       PIC S9V9(8)     COMP-3.      9915845
025010         05  PFO-OD-PRIME-ADJ7       PIC S9V9(8)     COMP-3.      9915845
025020         05  PFO-OD-PRIME-ADJ8       PIC S9V9(8)     COMP-3.      9915845
025030         05  PFO-OD-PRIME-ADJ9       PIC S9V9(8)     COMP-3.      9915845
025040         05  PFO-OD-ADJ-FLAG1        PIC X.                       9915845
025050         05  PFO-OD-ADJ-FLAG2        PIC X.                       9915845
025060         05  PFO-OD-ADJ-FLAG3        PIC X.                       9915845
025070         05  PFO-OD-ADJ-FLAG4        PIC X.                       9915845
025080         05  PFO-OD-ADJ-FLAG5        PIC X.                       9915845
025090         05  PFO-OD-ADJ-FLAG6        PIC X.                       9915845
025100         05  PFO-OD-ADJ-FLAG7        PIC X.                       9915845
025110         05  PFO-OD-ADJ-FLAG8        PIC X.                       9915845
025120         05  PFO-OD-ADJ-FLAG9        PIC X.                       9915845
025200     03  PFO-ODAC-BAL-HIST           PIC X.                       9915845
025300     03  PFO-ODAC-BAL-HIST-RET       PIC S9(3)       COMP-3.      9915845
025400     03  PFO-OD-INT-RATE-USE         PIC X.                       9915845
025500     03  PFO-OD-INT-CYCLE.                                        9915845
025510         05  PFO-OD-ACCR-INT-SCHED   PIC X.                       9915845
025600         05  PFO-OD-INT-INCR         PIC X.                       9915845
025700         05  PFO-OD-INT-MONTH1       PIC X(02).                   9915845
025800         05  PFO-OD-INT-DAY          PIC X(02).                   9915845
025900         05  PFO-OD-INT-DAY2         PIC X(02).                   9915845
026000     03  FILLER                      PIC X(2).                    0827778
026002   02  PF-PLAN-INFO.                                              0617360
026003     03  PFP-RECERT-REQD             PIC X.                       0827778
026004     03  PFP-SET-UP-FLAG             PIC X.                       0617360
026006     03  PFP-PLN-TRLR-TYPE           PIC XX.                      0617360
026008     03  PFP-PLN-TRLR-SUBTYPE        PIC XXX.                     0617360
026010     03  PFP-PLN-TRLR-WITHHOLDING    PIC X.                       0617360
026012     03  PFP-VALID-PLNS.                                          0617360
026014         05  PFP-VALID-PLN1          PIC XX.                      0617360
026016         05  PFP-VALID-PLN2          PIC XX.                      0617360
026018         05  PFP-VALID-PLN3          PIC XX.                      0617360
026020         05  PFP-VALID-PLN4          PIC XX.                      0617360
026022         05  PFP-VALID-PLN5          PIC XX.                      0617360
026024     03  PFP-VALID-PLN REDEFINES PFP-VALID-PLNS                   0617360
026026                           OCCURS 5 TIMES                         0617360
026028                                     PIC XX.                      0617360
026030     03  FILLER                      PIC X.                       0617360
026100   02  PF-EXT-SER-CHG.                                            IM003
026200     03  PFE-SET-UP-FLAG             PIC X.                       IM003
026300     03  PFE-SC-MISC-CNTRS.                                       IM003
026315         05  PFE-SC-MISC-CNTR-FLG    PIC X       OCCURS 200 TIMES.9715504
026330     03  FILLER                      PIC X(49).                   9915845
026345   02  PF-NB-FIXED-DATA.                                          9715504
026360     03  PFF-NB-LOB-IND              PIC 9(02).                   9715504
026375     03  PFF-NB-VEC-SEG-CD           PIC X(03).                   9715504
026390     03  PFF-NB-OD-LIMIT-SCOR-TYPE   PIC X(01).                   9715504
026400     03  PFF-NB-DDA-SAV-TYPE         PIC X(01).                   9715504
026420     03  PFF-NB-MIN-FND-BAL          PIC S9(13)V99   COMP-3.      9915845
026435     03  PFF-NB-ANC-TYPE             PIC X(03).                   9715504
026450     03  PFF-NB-ANC-CHARGE           PIC X(01).                   9715504
026465     03  PFF-NB-ANC-WVE-RSN          PIC X(02).                   9715504
026480     03  PFF-NB-ANC-WV-EX-DT         PIC X(08).                   9715504
026495     03  PFF-NB-WVE-EXP-DT           PIC X(08).                   9715504
026510     03  PFF-NB-SS-RT-IND            PIC X(01).                   9715504
026525     03  PFF-NB-SS-RT-ADJ            PIC S9V9(8)     COMP-3.      9915845
026540     03  PFF-NB-RSRVE-RT             PIC S9V9(8)     COMP-3.      9915845
026555     03  PFF-NB-REGD-MAX-CHKS        PIC S9(03)      COMP-3.      9715504
026570   02  PF-NB-CONTROL-DATA.                                        9715504
026575     03  PFC-NB-REGD-PRD-CNV         PIC X(03).                   9715504
026580     03  PFC-NB-STU-TYPE             PIC X(03).                   9715504
026585     03  PFC-NB-SEAS-MIN-BAL         PIC S9(13)V99   COMP-3.      9915845
026590   02  PF-BONUS-RATE-AREA.                                        2016374
026600     03  PFF-BONUS-RATE-AREA.                                     2016374
026605         05  PFF-BIR-FLAG            PIC X.                       2016374
026610         05  PFF-BIR-INIT-WD-NBR     PIC S9(7)       COMP-3.      2016374
026615         05  PFF-BIR-WD-NBR          PIC S9(7)       COMP-3.      2016374
026620         05  PFF-BIR-WD-AMT          PIC S9(13)V99   COMP-3.      2016374
026625         05  PFF-BIR-DP-NBR          PIC S9(7)       COMP-3.      2016374
026630         05  PFF-BIR-DP-AMT          PIC S9(13)V99   COMP-3.      2016374
026635         05  PFF-BIR-PYMT-METH       PIC X.                       2016374
026640         05  PFF-BIR-GRACE-MOS       PIC S9          COMP-3.      2016374
028000   02  PF-INTL-FILLER.                                            9915845
028100     03  FILLER                      PIC X(19).                   2016374
028200   02  PF-DATA-CENTER-DATA.                                       0930011
028300     03  PFD-SET-UP-FLAG             PIC X.                       0930011
028400     03  PFD-DC-LENGTH               PIC S999        COMP-3.      0930011
028500     03  PFD-DATA-CENTER             PIC X(297).                  0930011
040000   02  PF-LAST-MAINT.                                             9915845
040100     03  PFF-LAST-MAINT.                                          9915845
040200         05  PFF-LM-ONLINE-RBA       PIC X(4).                    9915845
040300         05  PFF-LM-OPER-ID.                                      9915845
040400             07  PFF-LM-TS-TELLER    PIC X(5).                    9915845
040500             07  FILLER              PIC X(3).                    9915845
040600         05  PFF-LM-BRANCH           PIC X(3).                    9915845
040700         05  PFF-LM-TERM-ID          PIC X(4).                    9915845
040800         05  PFF-LM-DATE.                                         9915845
040900             07  PFF-LM-DT-CC        PIC XX.                      9915845
041000             07  PFF-LM-DT.                                       9915845
041100                 09  PFF-LM-DT-YY    PIC XX.                      9915845
041200                 09  PFF-LM-DT-MM    PIC XX.                      9915845
041300                 09  PFF-LM-DT-DD    PIC XX.                      9915845
041400         05  PFF-LM-TIME.                                         9915845
041500             07  PFF-LM-HH-MM-SS     PIC S9(7)   COMP-3.          9915845
