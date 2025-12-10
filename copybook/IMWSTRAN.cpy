*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*
000200*----------------------------------------------------------------*
000300*               IMPACS POSTING TRANSACTIONS RECORD               *
000400*----------------------------------------------------------------*
000500*
000600 01  TRAN-FILE.
000700   02 TF-LENGTH                  PIC S9999       COMP.
000800   02 FILLER                     PIC XX.
000900   02 TRAN-REC                   PIC X(3099).                     9915239
001000   02 TRAN-NEW-ACCT REDEFINES TRAN-REC.
001100     03  FILLER                  PIC X(96).                       0717565
001200     03  TR-NEW-MST              PIC X(2974).
001300*
001400     03  TR-FIXED-PORTION REDEFINES TR-NEW-MST.
001500         05  TR-EX-CODE          PIC X.
001600         05  FILLER              PIC X(14).
001700         05  TR-REGION           PIC XXX.
001800         05  TR-BRANCH           PIC X(5).                        0427044
001900         05  FILLER              PIC X.                           0427044
002000         05  TR-LEDGER           PIC XXX.
002100         05  TR-OFFICER          PIC X(5).
002200         05  FILLER              PIC X(172).
002300         05  TR-ACCT-TYPE        PIC XXX.
002400         05  FILLER              PIC X(1837).
002500         05  TR-LANG-CODE        PIC XX.
002600         05  FILLER              PIC X(34).
002700         05  TR-IBT-FLAG         PIC X.
002800         05  TR-IBT-REROUTE      PIC X.
002900         05  TR-IBT-RETEN-DAYS   PIC S999   COMP-3.
003000         05  TR-IBT-OLD-NEW-ACCT PIC X(20).
003100         05  FILLER              PIC X(870).
003200     03  TR-TRAILER REDEFINES TR-NEW-MST.
003300       04  TR-TRLR-TYPE          PIC X.
003400       04  TR-SS-TRLR.
003500         05  TR-TRLR-SEQ         PIC 99.
003600         05  TR-TRLR.
003700             07  FILLER          PIC X(38).
003800             07  TR-SS-FOREIGN-ADDR
003900                                 PIC X.
003920             07  TR-SS-COUNTRY-CODE                               9916034
003940                                 PIC XX.                          9916034
004000             07  FILLER          PIC X(201).                      9916034
004100             07  TR-NA-ZIP       PIC X(10).
004200             07  FILLER          PIC X(348).
004300       04  FILLER                PIC X(2371).
004400     03  TR-NAME-ADD-TRLR REDEFINES TR-TRAILER.
004500       04  TR-NA-NO-LINES-1      PIC S9          COMP-3.
004600       04  TR-NA-COUNTRY         PIC XX.
004700       04  TR-NA-LINE                            OCCURS 8 TIMES.
004800         05  TR-NAME-ADDR-TYPE   PIC X.
004900         05  TR-LINE-NO          PIC X.
005000         05  TR-NAME-ADDRESS     PIC X(40).
005100       04  FILLER                PIC X(2635).
005200     03  FILLER REDEFINES TR-TRAILER.
005300       04  FILLER                PIC S9          COMP-3.
005400       04  FILLER                PIC XX.
005500       04  FILLER                                OCCURS 8 TIMES.
005600         05  FILLER              PIC XX.
005700         05  TR-CITY-STATE       PIC X(30).
005800         05  TR-ZIP-CODE         PIC X(10).
005900       04  FILLER                PIC X(2635).
006000     03  FILLER REDEFINES TR-TRAILER.
006100       04  FILLER                PIC S9          COMP-3.
006200       04  FILLER                PIC XX.
006300       04  TR-NA-ADDR-LINES      PIC X(210).
006400       04  FILLER                PIC X(126).
006500       04  FILLER                PIC X(2635).
006600     03  FILLER REDEFINES TR-TRAILER.
006700       04  FILLER                PIC S9          COMP-3.
006800       04  FILLER                PIC XX.
006900       04  FILLER                                OCCURS 8 TIMES.
007000         05  FILLER              PIC XX.
007100         05  TR-NA-SPEC-INSTR    PIC X(40).
007200       04  FILLER                PIC X(2635).
007300     03  FILLER REDEFINES TR-TRAILER.
007400       04  FILLER                PIC S9          COMP-3.
007500       04  FILLER                PIC XX.
007600       04  FILLER                                OCCURS 8 TIMES.
007700         05  FILLER              PIC XX.
007800         05  FILLER.
007900             07  TR-NA-EFFECTIVE-DATES-1.
008000                 09  TR-NA-START-DATE-1.
008100                     11  TR-NA-START-MO-DA-1.
008200                         13  TR-NA-START-MO-1
008300                                 PIC XX.
008400                         13  TR-NA-START-DA-1
008500                                 PIC XX.
008600                     11  TR-NA-START-YR-1
008700                                 PIC XX.
008800                 09  TR-NA-REMAIN-DAYS-1
008900                                 PIC S999        COMP-3.
009000             07  TR-NA-NO-STMTS-1
009100                                 PIC S999        COMP-3.
009200             07  TR-NA-USE-CODE-1
009300                                 PIC X.
009400             07  TR-NA-PRI-SEC-1 PIC X.
009500             07  TR-NA-SOC-SEC-1.
009600                 09  TR-NA-SSN-PRE-1
009700                                 PIC X.
009800                 09  TR-NA-SSN-NO-1
009900                                 PIC X(9).
010000                 09  TR-NA-SSN-SUF-1
010100                                 PIC X.
010200             07  TR-NA-PARTNER-PCT-1
010300                                 PIC SV999       COMP-3.
010400             07  TR-NA-USER-1    PIC X.
010500             07  TR-NA-PHONE-1.
010600                 09  TR-NA-AREA-CODE-1
010700                                 PIC XXX.
010800                 09  TR-NA-PHONE-NO-1
010900                                 PIC X(7).
011000                 09  TR-NA-EXTEN-NO-1
011100                                 PIC X(4).
011200       04  FILLER                PIC X(2635).
011300     03  FILLER REDEFINES TR-TRAILER.
011400         05  TR-DCD-NA-PTR       PIC S9          COMP-3.
011500         05  FILLER              PIC X(2973).
011510     03  FILLER REDEFINES TR-TRAILER.                             2016639
011520         05  FILLER              PIC X(959).                      2016639
011540         05  TR-AUTO-PYMT        PIC X.                           2016639
011560         05  FILLER              PIC X(2014).                     2016639
011600     03  FILLER REDEFINES TR-TRAILER.
011620         05  FILLER              PIC X(060).                      2016639
011640         05  TR-CMA-INDICATOR    PIC X.                           2016639
011700         05  FILLER              PIC X(206).                      2016639
011800         05  TR-INT-INV-IND      PIC X.
011900         05  FILLER              PIC X(207).                      2016639
011995         05  TR-DDA-MAX-FLAG     PIC X.                           2016639
012000         05  TR-DDA-MAX-BAL      PIC S9(13)V99   COMP-3.
012100         05  FILLER              PIC X(2490).                     9916070
012200     03  FILLER REDEFINES TR-TRAILER.
012300         05  FILLER              PIC X.
012400         05  TR-TARGET-AMT-CNTL  PIC X(22).
012500         05  FILLER              PIC X(2951).
012600     03  FILLER REDEFINES TR-TRAILER.
012700         05  TR-96-IBT-NEW-ACCT  PIC X(20).
012800         05  FILLER              PIC X(21).
012900         05  TR-96-IBT-STMT-REQ  PIC X.
013000         05  TR-96-IBT-RET-DAYS  PIC S999        COMP-3.
013100         05  TR-96-IBT-PASS-TRANS
013200                                 PIC X.
013300         05  FILLER              PIC X(2929).
013400     03  TR-01-IBT-CONFRIM REDEFINES TR-TRAILER.
013500         05  TR-01-IBT-ACCT      PIC X(22).
013600         05  FILLER              PIC X(2952).
013700   02 TRAN-0 REDEFINES TRAN-REC.
013800     03  TR-KEY.
013900         05  TR-KEY-PART-1.
014000             07  TR-CTL1         PIC XX.
014100             07  TR-CTL2         PIC XXX.
014200             07  TR-CTL3         PIC XXX.
014210             07  TR-CTL4-ACCT.                                    0417078
014300                 09  TR-CTL4     PIC X(4).                        0417078
014400                 09  TR-ACCT     PIC X(10).                       0417078
014500                 09  FILLER REDEFINES TR-ACCT.                    0417078
014600                     11  FILLER      PIC X(7).                    0417078
014700                     11  TR-PROD-TYPE                             0417078
014800                                     PIC X(3).                    0417078
014900     03  TR-TRAN-INFO.
015000         05  TR-TRAN-CODE        PIC XX.
015100             88  MONETARY-TXN    VALUE '80'.
015200         05  TR-CLASS-CODE REDEFINES TR-TRAN-CODE.
015300             07  TR-CLASS.
015400                 09  TR-CLASS1   PIC X.
015500                 09  TR-CLASS2   PIC X.
015600             07  TR-CLASS9 REDEFINES TR-CLASS
015700                                 PIC 99.
015800         05  TR-SUB-CLASS.
015900             07  TR-SCLASS1      PIC X.
016000             07  TR-SCLASS2      PIC X.
016100         05  TR-POST-PRIORITY REDEFINES TR-SUB-CLASS
016200                                 PIC XX.
016300     03  TR-NO-AMT               PIC S9(13)V99   COMP-3.
016400     03  TR-REJ-PROG             PIC XX.
016500     03  TR-REJ-RESN             PIC XXX.                         0717565
016600     03  TR-SOURCE               PIC XX.
016700         88  FROM-TURNAROUND-TAPE  VALUE '04'.
016800     03  FILLER REDEFINES TR-SOURCE.
016900         05  TR-SOURCE1          PIC X.
017000         05  TR-SOURCE2          PIC X.
017100     03  TR-BAT-SEQ.
017200         05  TR-BATCH            PIC S9(5)       COMP-3.
017300         05  TR-SEQ              PIC S9(5)       COMP-3.
017400     03  TR-TRACE                PIC S9(7)       COMP-3.
017500     03  TR-SRC-INFORMATION.
017600         05  TR-SRC-INFO-FLAG    PIC X.
017700         05  TR-SRC-INFO         PIC X(43).                       9915239
017710         05  FILLER REDEFINES TR-SRC-INFO.                        IVIB97
017720             07  FILLER          PIC X(08).                       IVIB97
017730             07  TR-OFIPRO       PIC X(03).                       IVIB97
017733             07  FILLER          PIC X(24).                       IVIB97
017736             07  TR-ORIGIN       PIC X(02).                       IVIB97
017740             07  FILLER          PIC X(06).                       IVIB97
017800         05  TR-IBT-TRANSFER-REC PIC X.
017900     03  FILLER REDEFINES TR-SRC-INFORMATION.
018000         05  TR-SOURCE-MISC1         PIC X(24).                   0427139
018100         05  TR-SOURCE-DATE          PIC X(08).
018200         05  TR-SOURCE-MISC2         PIC X(04).                   0427139
018210         05  TR-SOURCE-ORIGIN        PIC XX.                      0427139
018220         05  TR-SOURCE-MISC3         PIC X(7).                    0427139
018300     03  TR-AMOUNT               PIC S9(13)V99   COMP-3.
018400     03  TR-AMOUNTX REDEFINES TR-AMOUNT
018500                                 PIC X(8).
018600     03  TR-NUMBER REDEFINES TR-AMOUNT
018700                                 PIC S9(15)      COMP-3.
018800     03  TR-USER-CODE            PIC XXXX.
018900     03  TR-TRAN-TYPE            PIC X.
019000         88  DDA-DEPOSIT           VALUE '1'.
019100         88  DDA-CREDIT            VALUE '2'.
019200         88  DDA-CHECK             VALUE '3'.
019300         88  DDA-DEBIT             VALUE '4'.
019400     03  TR-POST-OPTIONS.
019500         05  TR-OPTION           PIC X           OCCURS 28 TIMES. 0637566
019600     03  FILLER REDEFINES TR-POST-OPTIONS.
019700         05  TR-OPTION9          PIC 9           OCCURS 28 TIMES. 0637566
019712     03  TR-COMB-PLN-CODES.                                       0617360
019714         05  TR-PLN-CODE         PIC XX.                          0617360
019716         05  TR-PLN-CODE2        PIC XX.                          0617360
019718     03  TR-PLN-CAL-YR           PIC X.                           0617360
019720     03  TR-PLN-TAX-YR           PIC X.                           0617360
019800     03  TR-STMT-SYM             PIC XX.
019900     03  TR-ACCUM-PTR-1          PIC S999        COMP-3.
020000     03  TR-ACCUM-PTR-1-DEC      PIC X.
020100     03  TR-ACCUM-PTR-2          PIC S999        COMP-3.
020200     03  TR-ACCUM-PTR-2-DEC      PIC X.
020210     03  TR-BAI-CODE-TT          PIC X(5).                        0517286
020220     03  TR-ARP-CODE             PIC XX.                          0517286
020230     03  TR-REV-TRAN-CD          PIC X(04).                       0517286
020300     03  TR-BULK-FILE-INFORMATION.
020400         05  TR-STMT-PULL        PIC X.
020500         05  TR-STMT-CYCLE       PIC XX.
020600         05  TR-BULK-FILE        PIC X.
020700         05  TR-REJ-REASONS      PIC XXX         OCCURS 3 TIMES.  0717565
020800         05  TR-PEND-DECISION    PIC X.
020900     03  TR-80-PSBK-FLAG         PIC X.                           0427139
021000     03  TR-CURRENCY-FLAG        PIC X.
021100     03  TR-CHK-TRUNC-FLAG       PIC X.
021200     03  TR-UNIV-DESC-FLAG       PIC X.
021300     03  TR-80-TRAN-TYPE         PIC XX.
021400         88  MISC-DR-CR            VALUE '01'.
021500         88  BACKDATED-TXN         VALUES '08' '11' '12'
021600                                            '13' '14'.
021700     03  TR-80-TYPE REDEFINES TR-80-TRAN-TYPE.
021800         05  TR-80-TYPE1         PIC X.
021900         05  TR-80-TYPE2         PIC X.
022000     03  TR-80-RMDR              PIC X(120).                      2011293
022100     03  TR-AUTO-CLRNG-HOUSE REDEFINES TR-80-RMDR.
022200         05  TR-COMP-NAME        PIC X(16).
022300         05  TR-ENTRY-DESC       PIC X(10).
022400         05  TR-ENTRY-DATE       PIC X(6).
022500         05  TR-ID               PIC X(15).
022510         05  TR-ACH-CHK-NO       PIC X(10).                       2011293
022600         05  TR-ACH-BKDT-DATE.
022700             07  TR-ACH-BKDT-MO  PIC XX.
022800             07  TR-ACH-BKDT-DA  PIC XX.
022900             07  TR-ACH-BKDT-YR  PIC XX.
023000         05  TR-ACH-BKDT-DAYS    PIC S999        COMP-3.
023100         05  TR-ACH-BKDT-HOL     PIC S9          COMP-3.
023200         05  TR-ACH-BAI-CODE     PIC X(5).
023300         05  TR-ACH-REF-NO       PIC S9(15)      COMP-3.
023400     03  TR-ACH-ADDENDA REDEFINES TR-80-RMDR.
023500         05  FILLER              PIC X(103).                      2011293
023600         05  TR-ADDEND-BKDT-DATE.
023700             07  TR-ADDEND-BD-MO PIC XX.
023800             07  TR-ADDEND-BD-DA PIC XX.
023900             07  TR-ADDEND-BD-YR PIC XX.
024000         05  TR-ADDEND-BKDT-DAYS PIC S999        COMP-3.
024100         05  TR-ADDEND-BKDT-HOL  PIC S9          COMP-3.
024110         05  TR-ADDEND-REF-NO    PIC S9(15)      COMP-3.          2011293
024200     03  TR-ATM REDEFINES TR-80-RMDR.
024300         05  TR-ATM-TERM-CD      PIC X(4).
024400         05  TR-ATM-DESC-CD      PIC X(18).
024500         05  TR-ATM-DATE-CD      PIC X(6).
024510         05  TR-ATM-POS-RECUR    PIC X.                           1020116
024600         05  TR-ATM-BKDT-DATE.
024700             07  TR-ATM-BKDT-MO  PIC XX.
024800             07  TR-ATM-BKDT-DA  PIC XX.
024900             07  TR-ATM-BKDT-YR  PIC XX.
025000         05  TR-ATM-BKDT-DAYS    PIC S999        COMP-3.
025100         05  TR-ATM-BKDT-HOL     PIC S9          COMP-3.
025200         05  TR-ATM-BAI-CODE     PIC X(05).
025300         05  TR-ATM-REF-NO       PIC S9(15)      COMP-3.
025400     03  TR-ACH-ADA REDEFINES TR-80-RMDR.
025500         05  TR-CMPY-NM-ADA      PIC X(16).
025600         05  TR-DESC-ADA         PIC X(10).
025700         05  TR-ENTRY-DT-ADA     PIC X(6).
025800         05  TR-ID-ADA           PIC X(15).
025810         05  TR-CHK-NO-ADA       PIC X(10).                       2011293
025900         05  TR-TYPE-ADA         PIC XX.
026000         05  TR-INFORM-ADA       PIC X(44).
026100     03  TR-ADA-BKDT REDEFINES TR-80-RMDR.
026200         05  FILLER              PIC X(103).                      2011293
026300         05  TR-ADA-BKDT-DATE    PIC X(6).
026400         05  TR-ADA-BKDT-DAYS    PIC S999   COMP-3.
026500         05  TR-ADA-BKDT-HOL     PIC S9     COMP-3.
026600     03  TR-NP-REVERSAL REDEFINES TR-80-RMDR.
026700         05  TR-NP-ORIG-DATE     PIC X(6).
026800         05  TR-NP-ORIG-TR-CD    PIC X(4).
026900         05  TR-NP-ORIG-BATCH    PIC S9(5)       COMP-3.
027000         05  TR-NP-ORIG-SEQ      PIC S9(5)       COMP-3.
027100         05  TR-NP-ITEM-COUNT    PIC S9(5)       COMP-3.
027200         05  TR-NP-DAYS          PIC S999        COMP-3.
027300         05  TR-NP-HOL           PIC S9          COMP-3.
027400     03  TR-TYPE-01 REDEFINES TR-80-RMDR.
027500         05  TR-01-ITEM-COUNT    PIC S9(9)       COMP-3.
027600         05  TR-CHECK-NO         PIC X(10).
027700         05  FILLER              PIC X(8).                        9916070
027800         05  TR-01-BAI           PIC X(05).
027900         05  TR-01-REF-NO        PIC S9(15)      COMP-3.
028000     03  TR-CUST-DESC1  REDEFINES TR-80-RMDR.
028100         05  TR-CUST-DESC        PIC X(16).
028110         05  TR-02-ITEM-COUNT    PIC S9(5)       COMP-3.          0326987
028200         05  FILLER              PIC X(4).                        0326987
028300         05  TR-BAI-CODE         PIC X(05).
028400         05  TR-02-REF-NO        PIC S9(15)      COMP-3.
028500     03  TR-LOAN-FIELDS REDEFINES TR-80-RMDR
028600                                 PIC X(48).                       0326958
028700     03  TR-LOAN-TRANS  REDEFINES TR-80-RMDR.
028800         05  TR-LOAN-OTHER-DIST.
028900             07  TR-LOAN-INS1    PIC S9(13)V99   COMP-3.
029000             07  TR-LOAN-NEW-FIRST REDEFINES TR-LOAN-INS1
029100                                 PIC X.
029200             07  TR-LOAN-INS2    PIC S9(13)V99   COMP-3.
029300             07  TR-LOAN-FEES    PIC S9(13)V99   COMP-3.
029400         05  FILLER REDEFINES TR-LOAN-OTHER-DIST.
029500             07  TR-LOAN-OLD-PRIN
029600                                 PIC S9(13)V99   COMP-3.
029700             07  TR-LOAN-ADDL-CR PIC S9(13)V99   COMP-3.
029800         05  TR-LOAN-PYMT-DIST                   COMP-3.
029900             07  TR-LOAN-PRIN    PIC S9(13)V99.
030000             07  TR-LOAN-INT     PIC S9(13)V99.
030100             07  TR-LOAN-OTHER   PIC S9(13)V99.
030200         05  TR-LOAN-ORIG-DATE.
030300             07  TR-LN-ORIG-MO   PIC XX.
030400             07  TR-LN-ORIG-DA   PIC XX.
030500             07  TR-LN-ORIG-YR   PIC XX.
030600         05  TR-LN-BK-DAYS       PIC S999        COMP-3.
030700     03  TR-BACKDATED-SAVINGS REDEFINES TR-80-RMDR.
030800         05  TR-SAVINGS-DATE     PIC X(6).
030900         05  TR-SAVINGS-DAYS     PIC S999        COMP-3.
031000         05  FILLER              PIC X(13).
031100     03  TR-BACKDATED-IOD REDEFINES TR-80-RMDR.
031200         05  TR-IOD-DATE.
031300             07  TR-IOD-MO       PIC XX.
031400             07  TR-IOD-DA       PIC XX.
031500             07  TR-IOD-YR       PIC XX.
031600         05  TR-IOD-DAYS         PIC S999        COMP-3.
031700         05  TR-IOD-CHECK        PIC X(10).
031800         05  TR-IOD-ITEMS        PIC S9(5)       COMP-3.
031900         05  TR-IOD-HOL          PIC S9          COMP-3.
032000         05  TR-IOD-DESC         PIC X(16).
032100         05  TR-IOD-BAI          PIC X(5).
032200         05  TR-IOD-REF-NO       PIC S9(15)      COMP-3.
032300     03  TR-CURRENCY-INFO.
032400         05  TR-CURR-CODE        PIC XXX.
032500         05  TR-CURR-DEC         PIC X.
032600         05  TR-CURR-AMT         PIC S9(15)V99   COMP-3.
032700         05  TR-CURR-RATE        PIC S9(7)V9(8)  COMP-3.
032800         05  TR-CURR-OVRD        PIC X.
032900         05  TR-CURR-FILLER      PIC XXX.
033000     03  TR-UNIV-DESC-TRAN.
033100         05  TR-SEG-OCCURS       PIC XX.
033200         05  TR-SEG-LENGTH       PIC XX.
033300         05  TR-UNIT-DESC        PIC X(790).                      0447137
033400     SKIP1
033500   02 TRAN-1 REDEFINES TRAN-REC.
033600     03  FILLER                  PIC X(96).                       0717565
033700     03  TR-BANK-CUST-AVAILABLE.
033800         05  TR-BCA-AMOUNTS                      COMP-3.
033900             07  TR-BCA-AMT      PIC S9(13)V99   OCCURS 13 TIMES.
034000         05  TR-BCA-REASON       PIC X.
034100         05  TR-BCA-REF-NO       PIC S9(15)      COMP-3.
034110**** EXTENDED FLOAT FOR TRAN CODE 53 - NOT REG CC COMPLIANT       0927817
034120     03  FILLER REDEFINES TR-BANK-CUST-AVAILABLE.                 0927817
034130         05  TR-FLOAT-DAY        PIC S999        COMP-3.          0927817
034140         05  TR-FLOAT-AMT        PIC S9(13)V99   COMP-3.          0927817
034150         05  TR-FLOAT-RSN        PIC XXX.                         0927817
034160         05  FILLER              PIC X(92).                       0927817
034170         05  TR-FLOAT-REF-NO     PIC S9(15)      COMP-3.          0927817
034200     03  FILLER REDEFINES TR-BANK-CUST-AVAILABLE.
034300         05  TR-FLT-AMOUNTS                      COMP-3.
034400             07  TR-FT-AMT       PIC S9(13)V99   OCCURS 8 TIMES.
034500         05  TR-FLT-REF-NO       PIC S9(15)      COMP-3.
034600     03  TR-NON-PAR REDEFINES TR-BANK-CUST-AVAILABLE
034700                                                 COMP-3.
034800         05  TR-NP-LOCAL-NO      PIC S9(7).
034900         05  TR-NP-FOREIGN-NO    PIC S9(7).
035000         05  TR-NP-NON-PAR-NO    PIC S9(7).
035100         05  TR-NP-NON-PAR-AMT   PIC S9(13)V99.
035200         05  TR-NP-LOCK-BOX-NO   PIC S9(7).
035300         05  TR-NP-CASH-DEP-AMT  PIC S9(13)V99.
035400     03  TR-BCA-ADJUST REDEFINES TR-BANK-CUST-AVAILABLE.
035500         05  TR-BCA-ADJ-DATE.
035600             07  TR-BCA-MO       PIC XX.
035700             07  TR-BCA-DA       PIC XX.
035800             07  TR-BCA-YR       PIC XX.
035900         05  TR-BCA-ADJ-DA-WK    PIC X.
036000         05  TR-BCA-ADJ-HOL      PIC X.
036100         05  TR-BCA-ADJ-TYPE     PIC X.
036200         05  TR-BCA-DAYS         PIC S999        COMP-3.
036300         05  TR-BCA-ADJUSTMENTS                  OCCURS 2 TIMES
036400                                                 COMP-3.
036500             07  TR-BCA-ADJ-AMT  PIC S9(13)V99.
036600             07  TR-BCA-ADJ-BUC  PIC S99.
036700             07  TR-BCA-ADJ-MTD  PIC S9(15)V99.
036800         05  TR-BCA-ADJ-REF-NO   PIC S9(15)      COMP-3.
036900         05  TR-MTD-AGGR-DAYS    PIC S9(3)       COMP-3.
037000     03  TR-LIST-POST REDEFINES TR-BANK-CUST-AVAILABLE.           0827768
037100       04  TR-LIST-POST-3A                       COMP-3.          0827768
037200         05  TR-LP-TOT-NO        PIC S9(7).
037300         05  TR-LP-TOT-AMT       PIC S9(13)V99.
037400         05  TR-LP-DB-NO         PIC S9(7).
037500         05  TR-LP-DB-AMT        PIC S9(13)V99.
037600         05  TR-LP-CR-NO         PIC S9(7).
037700         05  TR-LP-CR-AMT        PIC S9(13)V99.
037800         05  TR-LP-LOAN-DB-NO    PIC S9(7).
037900         05  TR-LP-LOAN-DB-AMT   PIC S9(13)V99.
038000         05  TR-LP-LOAN-CR-NO    PIC S9(7).
038100         05  TR-LP-LOAN-CR-AMT   PIC S9(13)V99.
038200         05  TR-LP-SAV-DR-NO     PIC S9(7).
038300         05  TR-LP-SAV-DR-AMT    PIC S9(13)V99.
038400         05  TR-LP-SAV-CR-NO     PIC S9(7).
038500         05  TR-LP-SAV-CR-AMT    PIC S9(13)V99.
038510         05  TR-LP-TOT-NO-CHK    PIC S9(7).                       0827768
038520         05  TR-LP-TOT-AMT-CHK   PIC S9(13)V99.                   0827768
038530       04  TR-LP-DR-ACCT         PIC X.                           0827768
038600     03  TR-LIST-POST-CR REDEFINES TR-BANK-CUST-AVAILABLE.        0827768
038700       04  TR-LIST-POST-0A                       COMP-3.          0827768
038800         05  TR-LP-TOT-NO-CR     PIC S9(7).
038900         05  TR-LP-TOT-AMT-CR    PIC S9(13)V99.
038910         05  TR-LP-TOT-NO-DEP    PIC S9(7).                       0827768
038920         05  TR-LP-TOT-AMT-DEP   PIC S9(13)V99.                   0827768
038930       04  TR-LP-CR-ACCT         PIC X.                           0827768
039000     03  TR-STOP-HOLD REDEFINES TR-BANK-CUST-AVAILABLE.
039100         05  TR-SH-NEW           PIC X.
039200         05  TR-HOLD-AMT         PIC S9(13)V99   COMP-3.
039300         05  TR-HOLD-NO          PIC S9(7)       COMP-3.
039400         05  TR-STOP-NO          PIC S9(7)       COMP-3.
039500         05  TR-DROP-AMT         PIC S9(13)V99   COMP-3.
039600         05  TR-CUST-UNAVAIL-HOLDS
039700                                 PIC S9(13)V99   COMP-3.
039800         05  TR-OTHER-HOLDS      PIC S9(13)V99   COMP-3.
039900     03  TR-STOP-HOLD REDEFINES TR-BANK-CUST-AVAILABLE.
040000         05  TR-ALL-FUNDS-FLAG   PIC X.
040100     03  TR-SPECIAL-INSTR REDEFINES TR-BANK-CUST-AVAILABLE.
040200         05  FILLER              PIC X.
040300         05  TR-SPEC-INSTR       PIC X(46).
040400     SKIP1
040500   02 TRAN-2 REDEFINES TRAN-REC.
040600     03  FILLER                  PIC X(96).                       0717565
040700     03  TR-FILE-MAINT.
040800         05  TR-FM-NEW-DATA      PIC X(35).
040900         05  TR-FM-NEW-BYTE REDEFINES TR-FM-NEW-DATA
041000                                 PIC X           OCCURS 35 TIMES.
041100         05  FILLER REDEFINES TR-FM-NEW-DATA.
041200             07  TR-FM-NEW-STATUS
041300                                 PIC XX.
041310             07  TR-FM-NEW-REASON PIC XX.                         IMIB004
041400*            07  FILLER          PIC X(33).
041410             07  FILLER           PIC X(31).                      IMIB004
041500         05  FILLER REDEFINES TR-FM-NEW-DATA.
041600             07  TR-FM-NEW-COMP  PIC S9          COMP-3.
041700             07  FILLER          PIC X(34).
041800         05  FILLER REDEFINES TR-FM-NEW-DATA.
041900             07  TR-FM-NEW-COMP-3
042000                                 PIC S999        COMP-3.
042100             07  FILLER          PIC X(33).
042200         05  FILLER REDEFINES TR-FM-NEW-DATA.
042300             07  TR-FM-NEW-AMT   PIC S9(13)V99   COMP-3.
042400             07  FILLER          PIC X(27).
042410         05  FILLER REDEFINES TR-FM-NEW-DATA.                     0617360
042420             07  TR-FM-NEW-AMT9  PIC S9(7)V99   COMP-3.           0617360
042430             07  FILLER          PIC X(30).                       0617360
042500         05  FILLER REDEFINES TR-FM-NEW-DATA.
042600             07  TR-FM-NEW-DATE  PIC X(6).
042700             07  FILLER          PIC X(29).
042702         05  FILLER REDEFINES TR-FM-NEW-DATA.                     1020057
042704             07  TR-FM-NEW-RATE  PIC S9V9(8)     COMP-3.          1020057
042706             07  FILLER          PIC X(30).                       1020057
042800         05  TR-FM-FLD-NO        PIC XXXX.
042900         05  TR-FM-FLD-START     PIC S99999.
043000         05  TR-FM-FLD-LENGTH    PIC S99.
043100         05  TR-FM-DATA-TYPE     PIC X.
043200         05  TR-FM-OLD-DATA-REQD PIC X.
043300         05  TR-FM-ADD-SUB       PIC X.                           0817665
043400         05  TR-FM-CHANGE-CODE   PIC X.
043500         05  TR-FM-REQ-BY        PIC XXX.
043600         05  TR-FM-FLD-DESC      PIC X(17).
043602         05  TR-FM-NEG-IND       PIC X.                           0817665
043604         05  TR-FM-IM2E-TYPE     PIC X.                           0817665
043700         05  TR-FM-OLD-DATA      PIC X(35).
043800         05  FILLER REDEFINES TR-FM-OLD-DATA.
043900             07  TR-FM-IBT-IND   PIC X(03).
044000             07  FILLER          PIC X(32).
044100         05  TR-FM-MAINT-CODE    PIC X.
044200     03  TR-STMT-REQUEST REDEFINES TR-FILE-MAINT.
044300         05  TR-SR-HI-CTL        PIC X(22).
044400         05  TR-SR-INIT          PIC XXX.
044500         05  TR-SR-AUDIT         PIC X.
044600         05  TR-SR-TYPE          PIC X.
044605     03  TR-PSBK-UPDATE REDEFINES TR-FILE-MAINT.                  0427139
044610         05  TR-PSBK-DATE        PIC X(8).                        0427139
044615         05  TR-PSBK-AMOUNT      PIC S9(13)V99   COMP-3.          0427139
044620         05  TR-PSBK-BAL-CHG     PIC X.                           0427139
044625         05  TR-OFF-INIT         PIC XXX.                         0427139
044700     03  TR-UPLINE-REJ REDEFINES TR-FILE-MAINT
044800                                 PIC X(70).
044900     03  TR-LOAN-EXTENSION REDEFINES TR-FILE-MAINT.
045000         05  TR-LOAN-EXT-FLAG    PIC X.
045100         05  TR-LOAN-EXT-CY      PIC S999        COMP-3.
045200         05  TR-LOAN-WAIVE-FLG   PIC X.
045300         05  TR-LOAN-WAIVE-AMT   PIC S9(13)V99   COMP-3.
045400         05  TR-LOAN-EXT-CHARGE  PIC S9(13)V99   COMP-3.
045402     03  TR-PLAN-ADJUSTMENT REDEFINES TR-FILE-MAINT.              0617360
045404         05  TR-PLAN-CHG-CD      PIC X.                           0617360
045406         05  TR-PLAN-CAL-YR      PIC X.                           0617360
045408         05  TR-PLAN-TAX-YR      PIC X.                           0617360
045410         05  TR-PLAN-ADJ-TYPE    PIC X.                           0617360
045412         05  TR-PLAN-PLN-CODE    PIC XX.                          0617360
045414         05  TR-PLAN-PLN-CODE2   PIC XX.                          0617360
045416         05  TR-PLAN-ADJ-AMT     PIC S9(13)V99   COMP-3.          0617360
045418         05  TR-PLAN-OFF-INIT    PIC XXX.                         0617360
045500     03  TR-SSN-NA REDEFINES TR-FILE-MAINT
045600                                 PIC X(50).
045700     03  TR-GL-FM REDEFINES TR-FILE-MAINT.
045800         05  TR-NEW-GL.
045900             07  TR-FM-NEW-GL    PIC XX.
046000             07  TR-FM-NEW-USER-GL
046100                                 PIC X(10).
046200         05  TR-OLD-GL.
046300             07  TR-FM-OLD-GL    PIC XX.
046400             07  TR-FM-OLD-USER-GL
046500                                 PIC X(10).
046600     SKIP1
046700   02 TRAN-3 REDEFINES TRAN-REC.
046800     03  FILLER                  PIC X(24).
046900     03  TR-MEMO-TRANS.
047000         05  TR-MT-FLD           PIC S999        COMP-3.
047100         05  FILLER              PIC X(70).                       0717565
047200         05  TR-MT-AMT           PIC S9(13)V99   COMP-3.
047300         05  TR-MT-NO      REDEFINES TR-MT-AMT
047400                                 PIC S9(15)      COMP-3.
047500         05  TR-MT-CUST-GEN      PIC X.
047600         05  TR-MT-REF-NO        PIC S9(15)      COMP-3.
047700         05  FILLER              PIC X(45).                       9915239
047800     SKIP1
047900   02 TRAN-TOTAL REDEFINES TRAN-REC.
048000     03  FILLER                  PIC X(22).
048100     03  TR-TOT-ID               PIC X(4).
048200     03  FILLER                  PIC X(13).                       0717565
048300     03  TR-SYS-TOT              PIC X(400).
048400     03  FILLER REDEFINES TR-SYS-TOT.
048500         05  TR-PROB-TOT         PIC S9(7)       COMP-3.
