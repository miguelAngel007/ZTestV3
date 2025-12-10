*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100 01  TRANSFER-BALANCE-RECORD.
000200     03  TB-CONTROL.
000300         05  TB-CNTL-1                   PIC XX.
000400         05  TB-CNTL-2                   PIC XXX.
000500         05  TB-CNTL-3                   PIC XXX.
000510         05  TB-CTL4-ACCT.                                        0417078
000600             07  TB-CNTL-4               PIC XXXX.                0417078
000700             07  TB-ACCOUNT-NBR          PIC X(10).               0417078
000800         05  FILLER                      PIC XXXX.                9715504
000900     03  TB-CONTROL-ST REDEFINES TB-CONTROL.
001000         05  TB-CNTL-ST-1                PIC XX.
001100         05  TB-CNTL-ST-2                PIC XXX.
001200         05  TB-CNTL-ST-3                PIC XXX.
001300         05  TB-CNTL-ST-4                PIC XXX.
001400         05  TB-ACCT-NBR-ST              PIC X(14).
001410         05  FILLER                      PIC X.                   9715504
001420     03  TB-CONTROL-ALS REDEFINES TB-CONTROL.                     9715504
001430         05  TB-CNTL-ALS-1               PIC XX.                  9715504
001440         05  TB-CNTL-ALS-2               PIC XXX.                 9715504
001450         05  TB-CNTL-ALS-3               PIC XXX.                 9715504
001460         05  TB-CNTL-ALS-4               PIC XXXX.                9715504
001470         05  TB-ACCT-NBR-ALS             PIC X(14).               9715504
001500     03  TB-ACCOUNT-STATUS               PIC XX.
001600     03  TB-NSF-CALC                     PIC X.
001620     03  TB-DAYS-BELOW-MIN-BAL           PIC S9(3)      COMP-3.
001630     03  TB-PREAUTH-TRNFS-REM            PIC S9(3)      COMP-3.
001640     03  TB-AVAIL-FOR-TRNF               PIC X.
001650     03  TB-FUNDING-PRI.
001670         05  FILLER                      PIC X.
001680         05  TB-FUNDING-FLAG             PIC X.
001690     03  TB-EXC-CODE                     PIC X.                   2016639
001692     03  TB-HOLD-ALL-FUNDS               PIC X(1).                0427202
001700     03  FILLER                          PIC X(10).               0427202
001800     03  TB-DDA-BAL                      PIC S9(13)V99  COMP-3.   9915845
001900     03  TB-LOAN-AVAIL-BAL               PIC S9(13)V99  COMP-3.   9915845
002000     03  TB-SAV-AVAIL-BAL                PIC S9(13)V99  COMP-3.   9915845
002100     03  TB-OD-LIMIT                     PIC S9(13)V99  COMP-3.   9915845
002200     03  TB-MINIMUM-DDA-BAL              PIC S9(13)V99  COMP-3.   9915845
002300     03  TB-MAXIMUM-DDA-BAL              PIC S9(13)V99  COMP-3.   9915845
002400     03  TB-TOTAL-HOLDS                  PIC S9(13)V99  COMP-3.   9915845
002500     03  TB-BANK-UNAVAILABLE             PIC S9(13)V99  COMP-3.   9915845
002600     03  TB-CUSTOMER-UNAVAILABLE         PIC S9(13)V99  COMP-3.   9915845
002700     03  TB-TOTAL-DEBITS                 PIC S9(13)V99  COMP-3.   9915845
002800     03  TB-TOTAL-CREDITS                PIC S9(13)V99  COMP-3.   9915845
002900     03  TB-ENDING-BAL                   PIC S9(13)V99  COMP-3.   9915845
003000     03  FILLER                          PIC X(10).               9715504
003010     03  TB-NEW-BASE-FIELDS.                                      9715504
003020         05  TB-NB-MIN-FUND-BAL          PIC S9(13)V99   COMP-3.  9915845
003025     03  TB-CURR-INFO.                                            2016639
003030         05  TB-CURR-CODE                PIC XXX.                 2016639
003035         05  TB-CURR-DEC                 PIC X.                   2016639
003040     03  TB-XINV-FIELDS.                                          2016639
003045         05  TB-XINV-LINK-IND            PIC X.                   2016639
003050         05  TB-XINV-PROCESS-IND         PIC X.                   2016639
003055         05  TB-XINV-BAL-CALC            PIC X.                   2016639
003060         05  TB-XINV-RESTRICT            PIC X.                   2016639
003065         05  TB-XINV-FUND.                                        2016639
003070             07  TB-XINV-FUND-GROUP      PIC X.                   2016639
003075             07  TB-XINV-FUND-NBR        PIC XXX.                 2016639
003080         05  TB-XINV-ACCT-NBR            PIC X(15).               2016639
003085         05  TB-XINV-BALANCE             PIC S9(13)V99   COMP-3.  2016639
003090         05  TB-XINV-PURCHASE            PIC S9(13)V99   COMP-3.  2016639
003095         05  TB-XINV-REDEMPTION          PIC S9(13)V99   COMP-3.  2016639
003100         05  TB-XINV-BAL-ADJ-UP          PIC S9(13)V99   COMP-3.  2016639
003105         05  TB-XINV-BAL-ADJ-DN          PIC S9(13)V99   COMP-3.  2016639
003110         05  TB-XINV-END-INV-BAL         PIC S9(13)V99   COMP-3.  2016639
003115         05  TB-XINV-END-DDA-BAL         PIC S9(13)V99   COMP-3.  2016639
003120     03  TB-GL-COST-CTR                  PIC X(30).               0517228
003180     03  FILLER                          PIC X(24).               0517228
003190     03  TB-AFF-MAX-TFR-SYSTEM-TYPE  PIC XX.                      2016639
003200     03  TB-AFF-MAX-TFR-CONTROL-NBR  PIC X(35).
003300     03  FILLER REDEFINES TB-AFF-MAX-TFR-CONTROL-NBR.
003400         05  TB-AFF-MAX-TFR-DDA-ACCT     PIC X(22).
003500         05  FILLER                      PIC X(13).
003600     03  FILLER REDEFINES TB-AFF-MAX-TFR-CONTROL-NBR.
003700         05  TB-AFF-MAX-TFR-ST-ACCT      PIC X(25).
003800         05  FILLER                      PIC X(10).
003810     03  FILLER REDEFINES TB-AFF-MAX-TFR-CONTROL-NBR.             2015171
003820         05  TB-AFF-MAX-TFR-CL-ACCT      PIC X(31).               2015171
003830         05  FILLER                      PIC X(04).               2015171
003840     03  FILLER REDEFINES TB-AFF-MAX-TFR-CONTROL-NBR.             0266738
003850         05  TB-AFF-MAX-TFR-AM-ACCT      PIC X(26).               0266738
003860         05  FILLER                      PIC X(09).               0266738
003870     03  FILLER REDEFINES TB-AFF-MAX-TFR-CONTROL-NBR.             0327005
003880         05  TB-AFF-MAX-TFR-WL-ACCT      PIC X(21).               0327005
003890         05  FILLER                      PIC X(14).               0327005
003900     03  TB-AFF-ACCT        OCCURS 9 TIMES.
004000         05  TB-AFF-SYSTEM-TYPE              PIC XX.
004100         05  TB-AFF-CONTROL-NBR              PIC X(35).
004200         05  FILLER REDEFINES TB-AFF-CONTROL-NBR.
004300             07  TB-AFF-DDA-ACCT             PIC X(22).
004400             07  FILLER                      PIC X(13).
004500         05  FILLER REDEFINES TB-AFF-CONTROL-NBR.
004600             07  TB-AFF-ST-ACCT              PIC X(25).
004700             07  FILLER                      PIC X(10).
004710         05  FILLER REDEFINES TB-AFF-CONTROL-NBR.                 9715504
004720             07  TB-AFF-AM-ACCT              PIC X(26).           9715504
004730             07  FILLER                      PIC X(9).            9715504
004740         05  FILLER REDEFINES TB-AFF-CONTROL-NBR.                 2015171
004750             07  TB-AFF-CL-ACCT              PIC X(31).           2015171
004760             07  FILLER                      PIC X(4).            2015171
004770         05  FILLER REDEFINES TB-AFF-CONTROL-NBR.                 0617492
004772             07  TB-AFF-XC-ACCT              PIC X(16).           0617492
004774             07  FILLER                      PIC X(19).           0617492
004800         05  TB-AFF-ODP-PRD-CD               PIC X(03).           9715504
004900     03  TB-ACCOUNT-INDICATORS.                                   9715504
005000         05  TB-NB-LOB-IND                   PIC X(02).           9715504
005100         05  TB-MTD-ANALYSIS                 PIC X(01).           9715504
005200         05  TB-OD-LIMIT-FLAG                PIC X(01).           9715504
005300     03  TB-SERVICE-CHARGE-AREA.                                  9715504
005400         05  TB-STATUS                       PIC X(02).           9715504
005500         05  TB-SVC-CHRG-REGION              PIC X(10).           9715504
005600         05  TB-CURR-BAL                     PIC S9(13)V99 COMP-3.9715504
005700         05  TB-DATE-OPENED.                                      9715504
005800             07  TB-OPENED-MO                PIC X(02).           9715504
005900             07  TB-OPENED-DA                PIC X(02).           9715504
006000             07  TB-OPENED-YR                PIC X(02).           9715504
006100         05  TB-BIRTHDATE.                                        9715504
006200             07  TB-BIRTH-MO                 PIC X(02).           9715504
006300             07  TB-BIRTH-DA                 PIC X(02).           9715504
006400             07  TB-BIRTH-YEAR.                                   9715504
006500                 09  TB-BIRTH-CENT           PIC X(02).           9715504
006600                 09  TB-BIRTH-YR             PIC X(02).           9715504
006700         05  TB-NEW-THIS-CYCLE               PIC X(01).           9715504
006800         05  TB-SC-CHARGE                    PIC X(01).           9715504
006900         05  TB-SC-WAIVE-REASON              PIC X(02).           9715504
007000         05  TB-SC-DEBITS                    PIC S9(07)    COMP-3.9715504
007100         05  TB-SC-TYPE                      PIC X(03).           9715504
007200         05  TB-NB-WVE-EXP-DT                PIC X(08).           9715504
007300         05  TB-NB-ANC-TYPE                  PIC X(03).           9715504
007400         05  TB-NB-ANC-CHARGE                PIC X(01).           9715504
007500         05  TB-NB-ANC-WVE-RSN               PIC X(02).           9715504
007600         05  TB-NB-ANC-WV-EX-DT              PIC X(08).           9715504
007700         05  TB-NB-DISC-FEES              OCCURS 5 TIMES.         9715504
007800             07  TB-NB-DISC-FEE-TYP          PIC X(01).           9715504
007900             07  TB-NB-DISC-FEE-NBR          PIC X(03).           9715504
008000         05  TB-NB-FEE-WV-EX-DT              PIC X(08).           9715504
008100         05  TB-NB-FEE-WVE-RSN               PIC X(02).           9715504
008200         05  TB-NB-SEAS-WVE-DT               PIC X(08).           9715504
008300     03  FILLER                              PIC X(13).           0917815
008900     03  TB-SC-WAIVE-OPTIONS.                                     9715504
009000         05  TB-SC-WAIVE-NEWACCT             PIC X(01).           9715504
009100         05  TB-SC-WAIVE-FROZEN              PIC X(01).           9715504
009200         05  TB-SC-WAIVE-ZEROBAL             PIC X(01).           9715504
009300         05  TB-SC-WAIVE-NOACTIVITY          PIC X(01).           9715504
009400         05  TB-SC-WAIVE-DORMANT             PIC X(01).           9715504
009500         05  TB-SC-WAIVE-INACTIVE            PIC X(01).           9715504
009600         05  TB-SC-WAIVE-TYPE                PIC X(01).           9715504
009700         05  TB-SC-WAIVE-CUST-AGE            PIC X(01).           9715504
009800         05  TB-SC-WAIVE-NBRDAYS             PIC 9(02).           9715504
009900         05  TB-SC-WAIVE-NEW-CYCLE         REDEFINES              9715504
010000                                           TB-SC-WAIVE-NBRDAYS    9715504
010100                                             PIC X(02).           9715504
010200         05  TB-SC-WAIVE-UPPER-AGE-LIM       PIC 9(02).           9715504
010300         05  TB-SC-WAIVE-LOWER-AGE-LIM       PIC 9(02).           9715504
010400         05  TB-SC-WAIVE-CLOSING             PIC X(01).           9715504
010500         05  TB-SC-DORMANT-BAL-NOCHG         PIC S9(13)V99 COMP-3.9915845
010600     03  TB-GENERAL-LEDGER-AREA.                                  9715504
010700         05  TB-GL-CODE                      PIC X(02).           9715504
010800         05  TB-GL-COST-CENTER               PIC X(07).           9715504
010900         05  TB-GL-FEE-INCOME-ACCT.                               9715504
011000             07  TB-GL-FEE-INC-ACCT-NO       PIC X(10).           9715504
011100             07  TB-GL-FEE-INC-CC            PIC X(07).           9715504
011200         05  TB-GL-FEE-WAIVE-ACCT.                                9715504
011300             07  TB-GL-FEE-WVE-ACCT-NO       PIC X(10).           9715504
011400             07  TB-GL-FEE-WVE-CC            PIC X(07).           9715504
011402     03  TB-CALCULATED-FEE-FIELDS.                                0917815
011404         05  TB-FEE-PROC-IND                 PIC X(01).           0917815
011406         05  TB-FEE-WAIVE                    PIC X(01).           0917815
011408         05  TB-FEE-ACCUM                    PIC S999      COMP-3.0917815
011410         05  TB-FEE-WVE-RSN                  PIC X(02).           0917815
011412         05  TB-FEE-AMTS                     COMP-3.              0917815
011414             07  TB-FEE-AMT-DFLT             PIC S9(11)V99.       0917815
011416             07  TB-FEE-AMT-AM               PIC S9(11)V99.       0917815
011418             07  TB-FEE-AMT-CL               PIC S9(11)V99.       0917815
011420             07  TB-FEE-AMT-IM               PIC S9(11)V99.       0917815
011422             07  TB-FEE-AMT-ST               PIC S9(11)V99.       0917815
011424             07  TB-FEE-AMT-WL               PIC S9(11)V99.       0917815
011426             07  TB-FEE-AMT-XC               PIC S9(11)V99.       0917815
011428         05  FILLER REDEFINES TB-FEE-AMTS    COMP-3.              0917815
011430             07  TB-FEE-AMT                  PIC S9(11)V99        0917815
011432                                             OCCURS 7 TIMES.      0917815
011500     03  FILLER                              PIC X(61).           0917815
011600*                                                                 2016639
011700 01  TRANSFER-BALANCE-HEADER REDEFINES TRANSFER-BALANCE-RECORD.   2016639
011800     03  TBH-CNTL1-3                         PIC X(8).            2016639
011900     03  FILLER                              PIC X(28).           2016639
012000     03  TBH-EXC-CODE                        PIC X.               2016639
012100     03  TBH-FILE-SEQ                        PIC S9(5)     COMP-3.2016639
012200     03  FILLER                              PIC X(960).          0917815
