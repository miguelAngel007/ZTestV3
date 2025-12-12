*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*0902557
000500*               IMPACS BCR CARD 1 COPYBOOK                       *0902557
001000*----------------------------------------------------------------*0902557
001700 01  DDA-BCR-1.
001800     03  BC1-CONTROL-KEY.
001900         05  BC1-CONTROL-1           PIC XX.                      IM008
002000         05  BC1-CONTROL-2           PIC XXX.                     IM008
002100         05  BC1-CONTROL-3           PIC XXX.                     IM008
002200     03  BC1-RECORD-ID               PIC XX.                      IM008
002300     03  FILLER                      PIC X(10).                   9915845
002400     03  BC-GROUP                    PIC X.                       IM008
002500     03  BC-DATA-CENTER-ID           PIC X(4).                    IM008
002600     03  BC-CONTROL1-NAME            PIC X(10).                   IM008
002700     03  BC-CTL2-FLAG                PIC X.                       IM008
002800     03  BC-CONTROL2-NAME            PIC X(10).                   IM008
002900     03  BC-CTL3-FLAG                PIC X.                       IM008
003000     03  BC-CONTROL3-NAME            PIC X(10).                   IM008
003100     03  BC-CTL4-FLAG                PIC X.                       IM008
003200     03  BC-CONTROL4-NAME            PIC X(10).                   IM008
003300     03  BC-REPT-NAME                PIC X(30).                   IM008
003400     03  BC-PROCESS-FLAG             PIC X.                       IM008
003500     03  BC-RUN-FLAG                 PIC X.                       IM008
003600     03  BC-SYSTEM-DATE.
003700         05  BC-SYSTEM-MO            PIC XX.                      IM008
003800         05  FILLER                  PIC X.                       IM008
003900         05  BC-SYTEM-DA             PIC XX.                      IM008
004000         05  FILLER                  PIC X.                       IM008
004100         05  BC-SYSTEM-YR            PIC XX.                      IM008
004200     03  BC-SYSTEM-TIME.
004300         05  BC-SYSTEM-HR            PIC XX.                      IM008
004400         05  BC-SYSTEM-MIN           PIC XX.                      IM008
004500         05  BC-SYSTEM-SEC           PIC XX.                      IM008
004600     03  BC-TODAYS-PROCESS-DATES.
004700         05  BC-CAPTURE-DATE.
004800             07  BC-CAPTURE-MO       PIC XX.
004900             07  BC-CAPTURE-DA       PIC XX.
005000             07  BC-CAPTURE-YR       PIC XX.
005100         05  BC-PROCESS-THRU-DATE.
005200             07  BC-PROC-THRU-MO     PIC XX.
005300             07  BC-PROC-THRU-DA     PIC XX.
005400             07  BC-PROC-THRU-YR     PIC XX.
005500         05  BC-STMT-THRU-DATE.
005600             07  BC-STMT-MO          PIC XX.
005700             07  BC-STMT-DA          PIC XX.
005800             07  BC-STMT-YR          PIC XX.
005900         05  BC-REPT-THRU-DATE.
006000             07  BC-REPT-MO          PIC XX.
006100             07  BC-REPT-DA          PIC XX.
006200             07  BC-REPT-YR          PIC XX.
006300     03  BC-CURRENT-DAYS.
006400         05  BC-PAST-ACCR-DAYS       PIC S999    COMP-3.          IM008
006500         05  BC-ACCRUAL-DAYS         PIC S999    COMP-3.          IM008
006600         05  BC-WEEK-DAY             PIC X.
006700         05  BC-BANKING-DAYS         PIC S999    COMP-3.          IM008
006800         05  BC-MONTH-END            PIC X.
006900         05  BC-QUARTER-END          PIC X.
007000         05  BC-YEAR-END             PIC X.
007010         05  BC-ALT-DUAL-YEAR-END    PIC X.                       9915858
007100         05  BC-BUSINESS-DAYS        PIC S999    COMP-3.          IM008
007110         05  FILLER                  PIC X.                       9915845
007200     03  BC-LAST-PROCESS-DATES.
007300         05  BC-LAST-CAPTURE-DATE.
007400             07  BC-LAST-CAPTURE-MO  PIC XX.
007500             07  BC-LAST-CAPTURE-DA  PIC XX.
007600             07  BC-LAST-CAPTURE-YR  PIC XX.
007700         05  BC-LAST-PROC-THRU-DATE.
007800             07  BC-LAST-PROC-MO     PIC XX.
007900             07  BC-LAST-PROC-DA     PIC XX.
008000             07  BC-LAST-PROC-YR     PIC XX.
008100         05  BC-LAST-STMT-DATE.
008200             07  BC-LAST-STMT-MO     PIC XX.
008300             07  BC-LAST-STMT-DA     PIC XX.
008400             07  BC-LAST-STMT-YR     PIC XX.
008500         05  BC-LAST-REPT-DATE.
008600             07  BC-LAST-REPT-MO     PIC XX.
008700             07  BC-LAST-REPT-DA     PIC XX.
008800             07  BC-LAST-REPT-YR     PIC XX.
008900     03  BC-PREV-DAYS.
009000         05  BC-PREV-PAST-ACCR-DAYS  PIC S999    COMP-3.          IM008
009100         05  BC-PREV-ACCR-DAYS       PIC S999    COMP-3.          IM008
009200         05  BC-PREV-WEEK-DAY        PIC X.
009300         05  BC-PREV-BANK-DAYS       PIC S999    COMP-3.          IM008
009400         05  BC-PREV-MONTH-END       PIC X.
009500         05  BC-PREV-QUARTER-END     PIC X.
009600         05  BC-PREV-YEAR-END        PIC X.
009610         05  BC-PREV-ALT-DUAL-YEAR-END                            9915858
009620                                     PIC X.                       9915858
009700         05  BC-PREV-BUS-DAYS        PIC S999    COMP-3.          IM008
009710         05  FILLER                  PIC X.                       9915845
009800     03  BC-NEXT-PROCESS-DATES.
009900         05  BC-NEXT-CAPTURE-DATE.
010000             07  BC-NEXT-CAPTURE-MO  PIC XX.
010100             07  BC-NEXT-CAPTURE-DA  PIC XX.
010200             07  BC-NEXT-CAPTURE-YR  PIC XX.
010300         05  BC-NEXT-PROC-THRU-DATE.
010400             07  BC-NEXT-PROC-MO     PIC XX.
010500             07  BC-NEXT-PROC-DA     PIC XX.
010600             07  BC-NEXT-PROC-YR     PIC XX.
010700         05  BC-NEXT-STMT-DATE.
010800             07  BC-NEXT-STMT-MO     PIC XX.
010900             07  BC-NEXT-STMT-DA     PIC XX.
011000             07  BC-NEXT-STMT-YR     PIC XX.
011100         05  BC-NEXT-REPT-DATE.
011200             07  BC-NEXT-REPT-MO     PIC XX.                      IM008
011300             07  BC-NEXT-REPT-DA     PIC XX.                      IM008
011400             07  BC-NEXT-REPT-YR     PIC XX.                      IM008
011500     03  BC-NEXT-DAYS.
011600         05  BC-NEXT-PAST-ACCR-DAYS  PIC S999    COMP-3.          IM008
011700         05  BC-NEXT-ACCR-DAYS       PIC S999    COMP-3.          IM008
011800         05  BC-NEXT-WEEK-DAY        PIC X.
011900         05  BC-NEXT-BANK-DAYS       PIC S999    COMP-3.          IM008
012000         05  BC-NEXT-MONTH-END       PIC X.
012100         05  BC-NEXT-QUARTER-END     PIC X.
012200         05  BC-NEXT-YEAR-END        PIC X.
012210         05  BC-NEXT-ALT-DUAL-YEAR-END                            9915858
012220                                     PIC X.                       9915858
012300         05  BC-NEXT-BUS-DAYS        PIC S999    COMP-3.          IM008
012310         05  FILLER                  PIC X.                       9915845
012350     03  BC-NEXT-CENT-YR             PIC XX.                      9915845
012400     03  BC-OCCURRENCE               PIC S9      COMP-3           IM008
012500                                                 OCCURS 7 TIMES.  9915845
012510     03  BC-WEEKDAY-BANK.                                         9915845
012520         05  BC-MONDAY-BK            PIC X.                       9915845
012530         05  BC-TUESDAY-BK           PIC X.                       9915845
012540         05  BC-WEDNESDAY-BK         PIC X.                       9915845
012550         05  BC-THURSDAY-BK          PIC X.                       9915845
012560         05  BC-FRIDAY-BK            PIC X.                       9915845
012570         05  BC-SATURDAY-BK          PIC X.                       9915845
012580         05  BC-SUNDAY-BK            PIC X.                       9915845
012582     03  FILLER REDEFINES BC-WEEKDAY-BANK.                        9915682
012584         05  BC-WEEKDAY-BK           PIC X       OCCURS 7 TIMES.  9915682
012590     03  FILLER                      PIC X(7).                    9915845
012600     03  BC-FILE-SEQUENCE-NOS.                                    9915845
012700         05  BC-CURR-FILE-SEQ        PIC S9(5)   COMP-3.          IM008
012800         05  BC-PREV-FILE-SEQ        PIC S9(5)   COMP-3.          IM008
012900     03  BC-RUN-COMPLETE-FLAGS.
013000         05  BC-EDIT1-COMP           PIC X.                       IM008
013100         05  BC-EDIT2-COMP           PIC X.                       IM008
013200         05  BC-IM25-ENTRY-COMP      PIC X.                       IM008
013300         05  BC-IM26-COMP            PIC X.                       IM008
013400         05  BC-STOP-HOLD-MAINT      PIC X.                       IM008
013500         05  BC-IM28-COMP            PIC X.                       IM008
013600         05  BC-FM-POST-COMP         PIC X.                       IM008
013700         05  BC-FILE-SPLIT-COMP      PIC X.                       IM008
013800         05  BC-TRAN-MERGE-COMP      PIC X.                       IM008
013810         05  BC-IM19-COMP            PIC X.                       IM008
013820         05  BC-MAINT-MERGE-COMP     PIC X.                       0326947
013900         05  FILLER                  PIC X(6).                    0326947
014200     03  BC-HOLIDAYS-THIS-YR.
014300         05  BC-HOLIDAYS OCCURS 36 TIMES INDEXED BY BC-HOL-IND.   0817725
014350             07  BC-HOLIDAY-DATE.                                 IM007
014400                 09  BC-HOLIDAY-YEAR.                             IM007
014500                     11  BC-HOLIDAY-CENT PIC XX.                  IM007
014600                     11  BC-HOLIDAY-YR   PIC XX.                  IM007
014700                 09  BC-HOLIDAY-MO       PIC XX.                  IM007
014800                 09  BC-HOLIDAY-DA       PIC XX.                  IM007
014900             07  BC-FED-RES-HOL          PIC X.                   IM007
015000     03  BC-SERVICE-CHG-CODE         PIC X.                       IM008
015100     03  BC-FULL-PAGE                PIC S999    COMP-3.          IM008
015200     03  BC-ARP-FLAG                 PIC X.                       IM008
015300     03  BC-LOAN-SYS-NAME            PIC X(30).                   IM008
015400     03  BC-FLOAT-FLAG               PIC X.                       IM008
015500     03  BC-LEAP-YEAR                PIC X.                       IM008
015600     03  BC-AUTO-RELEASE             PIC X.                       IM008
015700     03  BC-HOLD-SUSPECT             PIC X.                       IM008
015800     03  BC-LIST-POST-NO             PIC S999    COMP-3.          IM008
015900     03  BC-POST-AMT-SEQ             PIC X.                       IM008
016000     03  BC-ANALYSIS-PHASE           PIC X(4).                    IM008
016100     03  BC-TRANEDT-PHASE            PIC X(4).                    IM008
016200     03  BC-DLCA-FLAG                PIC X.                       9916049
016300     03  BC-SELF-CHK-PHASE           PIC X(4).                    IM008
016400     03  BC-MONTH-DAYS               PIC S999    COMP-3.          IM008
016500     03  BC-LIST-POST-DETAIL         PIC X.                       IM008
016600     03  BC-LIST-POST-OD             PIC X.                       IM008
016700     03  BC-TRANSTBL-PHASE           PIC X(6).                    0517286
016710     03  FILLER                      PIC X(2).                    0517286
016800     03  BC-REPNAME-PHASE            PIC X(8).                    IM008
016900     03  BC-REPTBL-PHASE             PIC X(8).                    IM008
017000     03  BC-EXCTBL-PHASE             PIC X(8).                    IM008
017100     03  BC-ACCUM-TRAN-RET           PIC S999    COMP-3.
017200     03  BC-NON-DOL-RETENTION        PIC S999    COMP-3.
017210     03  BC-IOD-NEG-ACCRUAL          PIC X.                       0902213
017220     03  BC-SAV-NEG-ACCRUAL          PIC X.                       0902213
017230     03  BC-OD-NEG-ACCRUAL           PIC X.                       0902213
017500     03  BC-BACKDATE-LIMIT           PIC S999    COMP-3.
017600     03  BC-QUARTER-DAYS             PIC S999    COMP-3.          9715132
017700     03  BC-BAL-HIST-FLAG            PIC X.                       0902213
017800     03  BC-ALLOW-BCKDT-HIFI         PIC X.
017900     03  BC-RPT-DAILY-NSF-OD-TRNS    PIC X.
018000     03  BC-BAL-CHNG-BY-DB-CR        PIC X.
018100     03  BC-GL-CONTROL-LEVEL         PIC X.
018200     03  BC-GEN-DB-ON-NEG-ACCRUAL    PIC X.
018300     03  BC-YEAR-END-INT-CODE        PIC X.                       IM006
018350     03  BC-MEMO-CLEAR               PIC X.                       IM004
018400     03  BC-ACCRUE-AHEAD             PIC X.
018500     03  BC-PURGE-ON-SCHED           PIC X.
018510     03  BC-INT-DIST-CD              PIC X.                       IM006
018520     03  BC-INT-DIST-AMT             PIC S9(13)V99    COMP-3.     9915845
018530     03  BC-ONLINE-OD-FLAG           PIC X.                       IM006
018540     03  BC-ONLINE-LOC-FLAG          PIC X.                       IM006
018550     03  BC-ONLINE-HOLD-FLAG         PIC X.                       IM006
018560     03  BC-ONLINE-NSF-OD-CHRG       PIC X.                       0901891
018570     03  BC-ONLINE-SAV-FLAG          PIC X.                       IM006
018580     03  BC-ONLINE-ALPHA-RECORD      PIC X.                       IM006
018582     03  BC-ARCH-HIST                PIC X.                       0927050
018585     03  FILLER                      PIC X.                       0927050
018586     03  BC-NONPOST-HIST-OPT         PIC X.                       0737697
018588     03  BC-ACCT-EDIT-REBUILD        PIC X.                       0417078
018590     03  BC-STMT-PULL-CODE           PIC X.                       IM007
018595     03  BC-ACCT-EDIT-MASK           PIC X(13).                   IM007
018596     03  BC-STOP-HIT-OPTION          PIC X.                       0266778
018597     03  BC-MULTI-STOP-HIT           PIC X.                       0266778
018598     03  BC-SPEC-INSTR-SUSPECT       PIC X.                       0627534
018599     03  BC-EXTEND-FLOAT-OPT         PIC X.                       0927817
018600     03  BC-EXTEND-FLOAT-DAYS        PIC S999    COMP-3.          0927817
018605     03  FILLER                      PIC X(6).                    0927817
018608     03  BC-PAST-BUS-DAYS                        COMP-3.          0927817
018610         05  BC-PREV-PAST-BUS-DAYS   PIC S999.                    IM007
018620         05  BC-CURR-PAST-BUS-DAYS   PIC S999.                    IM007
018630         05  BC-NEXT-PAST-BUS-DAYS   PIC S999.                    IM007
018650     03  BC-ACCUM-TRAN-SRC           PIC X.                       IM006
018700     03  BC-ABA-NUMBER               PIC X(9)    OCCURS 4 TIMES.  IM008
018800     03  BC-PRODUCT-CTL-FLAG         PIC X.                       IM008
018810     03  BC-LOOKUP-HEADING           PIC X.                       9915845
018820     03  BC-MAINT-HIST-OPT           PIC X.                       0326947
018835     03  BC-PASSBOOKS-ALLOWED        PIC X.                       0427139
018840     03  BC-BAL-HIST-COMPRESS        PIC X.                       0447266
018842     03  BC-OL-FUND-IND              PIC X.                       0517279
018850     03  BC-MASK-ID-PHASE            PIC X(4).                    0817719
018860     03  BC-MASK-ID-MAINLINE.                                     0817719
018862         05  BC-MASK-ID-IM05         PIC X.                       0817719
018864         05  BC-MASK-ID-IM12         PIC X.                       0817719
018866         05  BC-MASK-ID-IM14         PIC X.                       0817719
018868         05  BC-MASK-ID-IM21         PIC X.                       0817719
018870         05  BC-MASK-ID-IM22         PIC X.                       0817719
018872         05  BC-MASK-ID-IM30         PIC X.                       0817719
018874         05  BC-MASK-ID-IM7A         PIC X.                       0817719
018876         05  BC-MASK-ID-IM78         PIC X.                       0817719
018878         05  BC-MASK-ID-IM79         PIC X.                       0817719
018880         05  FILLER                  PIC X.                       0817719
018890     03  FILLER                      PIC X(5).                    0817719
018900     03  BC-TOTAL-LEVEL-FLAGS.
019000         05  BC-IM21-TOT-LVL         PIC X.                       IM008
019100         05  BC-IM22-TOT-LVL         PIC X.                       IM008
019200         05  BC-IM25-TOT-LVL         PIC X.                       IM008
019300         05  BC-IM26-TOT-LVL         PIC X.                       IM008
019400         05  BC-IM27-TOT-LVL         PIC X.                       IM008
019500         05  BC-IM28-TOT-LVL         PIC X.                       IM008
019600         05  BC-IM31-TOT-LVL         PIC X.                       IM008
019700         05  BC-IM32-TOT-LVL         PIC X.                       IM008
019800         05  BC-IM34-TOT-LVL         PIC X.                       IM008
019810         05  BC-IM34OM-TOT-LVL       PIC X.                       0326947
019900         05  FILLER                  PIC X(7).                    0326947
020000     03  BC-RESET-PAGE-CNT-FLAGS.
020100         05  BC-IM21-RESET-PG        PIC X.                       IM008
020200         05  BC-IM22-RESET-PG        PIC X.                       IM008
020300         05  BC-IM25-RESET-PG        PIC X.                       IM008
020400         05  BC-IM26-RESET-PG        PIC X.                       IM008
020500         05  BC-IM27-RESET-PG        PIC X.                       IM008
020600         05  BC-IM28-RESET-PG        PIC X.                       IM008
020700         05  BC-IM31-RESET-PG        PIC X.                       IM008
020800         05  BC-IM32-RESET-PG        PIC X.                       IM008
020900         05  BC-IM34-RESET-PG        PIC X.                       IM008
020910         05  BC-IM34OM-RESET-PG      PIC X.                       0326947
021000         05  FILLER                  PIC X(7).                    0326947
021100     03  BC-DEFAULT-DR               PIC X(4).                    IM008
021200     03  BC-PRIORITY-DR              PIC X(4).                    IM008
021300     03  BC-DEFAULT-CR               PIC X(4).                    IM008
021400     03  BC-ANALYSIS-MOS             PIC S999    COMP-3.          IM008
021500     03  BC-ANALYSIS-FACTORS                     COMP-3.          IM008
021600         05  BC-ANALYSIS-EARN-RATE   PIC S9V9(8).                 9915845
021610         05  BC-ANALYSIS-EARN-RATE1  REDEFINES                    9915845
021620             BC-ANALYSIS-EARN-RATE   PIC S9(3)V9(6).              9915845
021700         05  BC-ANALYSIS-RES-REQ     PIC S9V9(8).                 9915845
021710         05  BC-ANALYSIS-RES-REQ1    REDEFINES                    9915845
021720             BC-ANALYSIS-RES-REQ     PIC S9(3)V9(6).              9915845
021800         05  BC-ANALYSIS-DR-COST     PIC S99V999.                 9915845
021900         05  BC-ANALYSIS-CR-COST     PIC S99V999.                 9915845
022000         05  BC-ANALYSIS-LOCAL-RATE  PIC S99V999.                 9915845
022100         05  BC-ANALYSIS-FOR-RATE    PIC S99V999.                 9915845
022200         05  BC-ANALYSIS-MAINT-CHG   PIC S9(9)V99.                9915845
022300         05  BC-ANALYSIS-NEG-EARN    PIC S9V9(8).                 9915845
022310         05  BC-ANALYSIS-NEG-EARN1   REDEFINES                    9915845
022320             BC-ANALYSIS-NEG-EARN    PIC S9(3)V9(6).              9915845
022400         05  BC-ANALYSIS-CASH-COST   PIC S9V9(8).                 9915845
022410         05  BC-ANALYSIS-CASH-COST1  REDEFINES                    9915845
022420             BC-ANALYSIS-CASH-COST   PIC S9(3)V9(6).              9915845
022500         05  BC-ANALYSIS-ALT-EARN    PIC S9V9(8).                 9915845
022510         05  BC-ANALYSIS-ALT-EARN1   REDEFINES                    9915845
022520             BC-ANALYSIS-ALT-EARN    PIC S9(3)V9(6).              9915845
022600         05  BC-ANALYSIS-NONPAR-ITM  PIC S99V999.                 9915845
022700         05  BC-ANALYSIS-MISC-CNTRS  PIC S99V999  OCCURS 25 TIMES.9915845
022800     03  BC-ANALYSIS-FLOAT           PIC X.                       IM008
022900     03  BC-RATE-TABLE-PHASE         PIC X(8).                    IM008
023000     03  BC-DESC-TABLE-PHASE         PIC X(8).                    IM008
023100     03  BC-BILL-STAT-FLAG           PIC X.                       IM008
023200     03  BC-REP-RCP-FLAG             PIC X.                       IM008
023300     03  BC-CLOSING-TRAN-PAYOFF.                                  IM008
023400         05  BC-CLOSING-TRAN-WAIVE   PIC X.                       IM008
023500         05  BC-CLOSING-TRAN-TOLER   PIC S9(13)V99   COMP-3.      9915845
023550     03  BC-FILE-STAT-PRINT.                                      IM008
023600         05  BC-BCRM-PRINT-OPTION    PIC X.                       IM008
023700         05  BC-LKPM-PRINT-OPTION    PIC X.                       IM008
023750         05  BC-PRDM-PRINT-OPTION    PIC X.                       IM008
023800         05  BC-MSGM-PRINT-OPTION    PIC X.                       IM008
023850         05  BC-TBLM-PRINT-OPTION    PIC X.                       IM008
023860         05  FILLER                  PIC X(4).                    9915845
023900     03  BC-FILE-PRINT-SPOOL.                                     IM008
023950         05  BC-BCRM-PRINT-SPOOL     PIC X.                       IM008
024050         05  BC-LKPM-PRINT-SPOOL     PIC X.                       IM008
024100         05  BC-PRDM-PRINT-SPOOL     PIC X.                       IM008
024150         05  BC-MSGM-PRINT-SPOOL     PIC X.                       IM008
024200         05  BC-TBLM-PRINT-SPOOL     PIC X.                       IM008
024210         05  FILLER                  PIC X(4).                    9915845
024250     03  BC-FILE-PRINT-FORM.                                      IM008
024300         05  BC-BCRM-PRINT-FORM      PIC X.                       IM008
024400         05  BC-LKPM-PRINT-FORM      PIC X.                       IM008
024450         05  BC-PRDM-PRINT-FORM      PIC X.                       IM008
024500         05  BC-MSGM-PRINT-FORM      PIC X.                       IM008
024550         05  BC-TBLM-PRINT-FORM      PIC X.                       IM008
024560         05  FILLER                  PIC X(4).                    9915845
024600     03  BC-SPOOL-CODES.                                          IM008
024650         05  BC-IM05-SPOOL           PIC X.                       IM008
024700         05  BC-IM12-SPOOL           PIC X.                       IM008
024750         05  BC-IM13-SPOOL           PIC X.                       IM008
024800         05  BC-IM21-SPOOL           PIC X.                       IM008
024850         05  BC-IM22-SPOOL           PIC X.                       IM008
024900         05  BC-IM25-SPOOL           PIC X.                       IM008
024950         05  BC-IM26-SPOOL           PIC X.                       IM008
025000         05  BC-IM27-SPOOL           PIC X.                       IM008
025050         05  BC-IM28-SPOOL           PIC X.                       IM008
025100         05  BC-IM31-SPOOL           PIC X.                       IM008
025150         05  BC-IM32-SPOOL           PIC X.                       IM008
025200         05  BC-IM33-SPOOL           PIC X.                       IM008
025250         05  BC-IM34-SPOOL           PIC X.                       IM008
025300         05  BC-IM43-SPOOL           PIC X.                       IM008
025350         05  BC-IM44-SPOOL           PIC X.                       IM008
025400         05  BC-IM45-SPOOL           PIC X.                       IM008
025450         05  BC-IM61-SPOOL           PIC X.                       IM008
025500         05  BC-IM67-SPOOL           PIC X.                       IM008
025550         05  BC-IM73-SPOOL           PIC X.                       1004773
025600         05  BC-IM74-SPOOL           PIC X.                       1004773
025650         05  BC-IM75-SPOOL           PIC X.                       IM008
025700         05  BC-IM78-SPOOL           PIC X.                       IM008
025750         05  BC-IM91-SPOOL           PIC X.                       IM008
025800         05  BC-IM92-SPOOL           PIC X.                       IM008
025825         05  BC-IM30-SPOOL           PIC X.                       0902557
025850         05  BC-IM29ZBA-SPOOL        PIC X.                       9715504
025855         05  BC-IM20XIM-SPOOL        PIC X.                       2016639
025860         05  BC-IM30XIB-SPOOL        PIC X.                       2016639
025865         05  BC-IM30XIT-SPOOL        PIC X.                       2016639
025870         05  BC-IM34OM-SPOOL         PIC X.                       0326947
025900         05  FILLER                  PIC X(4).                    0326947
025950     03  BC-FORM-CODES.                                           IM008
026000         05  BC-IM05-FORM            PIC X.                       IM008
026050         05  BC-IM12-FORM            PIC X.                       IM008
026100         05  BC-IM13-FORM            PIC X.                       IM008
026150         05  BC-IM21-FORM            PIC X.                       IM008
026200         05  BC-IM22-FORM            PIC X.                       IM008
026250         05  BC-IM25-FORM            PIC X.                       IM008
026300         05  BC-IM26-FORM            PIC X.                       IM008
026350         05  BC-IM27-FORM            PIC X.                       IM008
026400         05  BC-IM28-FORM            PIC X.                       IM008
026450         05  BC-IM31-FORM            PIC X.                       IM008
026500         05  BC-IM32-FORM            PIC X.                       IM008
026550         05  BC-IM33-FORM            PIC X.                       IM008
026600         05  BC-IM34-FORM            PIC X.                       IM008
026650         05  BC-IM43-FORM            PIC X.                       IM008
026700         05  BC-IM44-FORM            PIC X.                       IM008
026750         05  BC-IM45-FORM            PIC X.                       IM008
026800         05  BC-IM61-FORM            PIC X.                       IM008
026850         05  BC-IM67-FORM            PIC X.                       IM008
026900         05  BC-IM73-FORM            PIC X.                       1004773
026950         05  BC-IM74-FORM            PIC X.                       1004773
027000         05  BC-IM75-FORM            PIC X.                       IM008
027050         05  BC-IM78-FORM            PIC X.                       IM008
027100         05  BC-IM91-FORM            PIC X.                       IM008
027150         05  BC-IM92-FORM            PIC X.                       IM008
027175         05  BC-IM30-FORM            PIC X.                       0902557
027200         05  BC-IM29ZBA-FORM         PIC X.                       9715504
027210         05  BC-IM20XIM-FORM         PIC X.                       2016639
027220         05  BC-IM30XIB-FORM         PIC X.                       2016639
027230         05  BC-IM30XIT-FORM         PIC X.                       2016639
027240         05  BC-IM34OM-FORM          PIC X.                       0326947
027300         05  FILLER                  PIC X(4).                    0326947
027400     03  FILLER                      PIC X(6).                    IM008
027500     03  BC-BK-IRS-NO                PIC X(10).                   IM008
027510     03  BC-ISO-COUNTRY              PIC XX.                      9915845
027520     03  BC-ISO-IRS-TAX              PIC X.                       9915845
027600     03  BC-USER-REPT-SCHEDULE       OCCURS 10 TIMES              IM008
027700                                     INDEXED BY BC-USER-INDEX.    IM008
027800         05  BC-USER-CODE            PIC X.                       IM008
027900         05  BC-USER-SCHD-TYPE       PIC X.                       IM008
028000         05  BC-USER-DAY1            PIC XX.                      IM008
028100         05  BC-USER-DAY2            PIC XX.                      IM008
028200         05  BC-USER-REPT-FREQ       PIC X.                       IM008
028300         05  BC-USER-MONTH           PIC X.                       IM008
028400     03  BC-SOC-SEC-DATA.
028500         05  BC-SOC-SEC-CODE         PIC X.                       IM008
028600         05  BC-SOC-SEC-TRANSMIT     PIC X(5).                    IM008
028700     03  BC-ACCT-DISPLACEMENT        PIC X.                       IM008
028800     03  BC-CONS-STAT                PIC X.                       IM008
028900     03  BC-IRREG-STMT-CYCLES                    OCCURS 10 TIMES. IM008
029000         05  BC-IRR-STMT-CYCLE       PIC XX.
029100         05  BC-IRR-STMT-INCRM       PIC X.
029200         05  BC-IRR-STMT-BEGIN-MO    PIC X.
029300     03  BC-STMT-FORM-SORT           PIC X.                       IM008
029400     03  FILLER                      PIC X(26).                   0930011
029500     03  BC-SC-INSTALLED             PIC X.                       1004962
029510     03  BC-NX-INSTALLED             PIC X.                       9915845
029520     03  BC-XINV-INSTALLED           PIC X.                       2016639
029600     03  FILLER                      PIC X(16).                   2016639
032200     03  BC-TRANSFER-INFORMATION.                                 IM006
032300         05  BC-TRANSFER-FLG-CL      PIC X.                       IM008
032400         05  BC-TRANSFER-FLG-AL      PIC X.                       IM008
032500         05  BC-TRANSFER-FLG-IC      PIC X.                       IM008
032600         05  BC-TRANSFER-FLG-IM      PIC X.                       IM008
032700         05  BC-TRANSFER-FLG-ST      PIC X.                       IM008
032800         05  BC-TRANSFER-FLG-RE      PIC X.                       IM008
032900         05  BC-TRANSFER-IT32        PIC X.                       IM008
033000     03  FILLER                      PIC X(9).                    IM008
033100     03  BC-LANGUAGE-AREA.                                        9915845
033200         05  BC-MULTI-LANGUAGE-FLAG  PIC X.                       9915845
033300         05  BC-DEFAULT-LANGUAGE     PIC XX.                      9915845
033400     03  FILLER                      PIC X(8).                    9915845
033500     03  BC-CURR-INFO.                                            9915845
033600         05  BC-CURR-CTL-LEVEL       PIC X.                       9915845
033700         05  BC-CURR-CODE            PIC XXX.                     9915845
033800         05  BC-CURR-DEC             PIC X.                       9915845
033900         05  BC-CURR-BASE-CODE       PIC XXX.                     9915845
034000         05  BC-CURR-BASE-DEC        PIC X.                       9915845
034100         05  BC-CURR-CTL1-CODE       PIC XXX.                     9915845
034200         05  BC-CURR-CTL1-DEC        PIC X.                       9915845
034300         05  BC-CURR-QUAL            PIC X(4).                    9915845
034400     03  FILLER                      PIC X(8).                    9915845
034500     03  BC-ALT-DUAL-YEAR-END-INFO.                               9915858
034600         05  BC-ALT-DUAL-YR-OPTION       PIC X.                   9915858
034700         05  BC-ALT-DUAL-YR-DATE.                                 9915858
034800             07  BC-ALT-DUAL-YR-CENT     PIC XX.                  9915858
034900             07  BC-ALT-DUAL-YR-WO-CENT.                          9915858
035000                 09  BC-ALT-DUAL-YR-YR   PIC XX.                  9915858
035100                 09  BC-ALT-DUAL-YR-MO   PIC XX.                  9915858
035200                 09  BC-ALT-DUAL-YR-DA   PIC XX.                  9915858
035300     03  BC-ACCT-PURGE-DATE.                                      9915845
035400         05  BC-ACCT-PURGE-MO            PIC XX.                  9915845
035500         05  BC-ACCT-PURGE-DA            PIC XX.                  9915845
035600     03  BC-ACCT-PURGE-FLAG              PIC X.                   9915845
035700     03  BC-REG-COMPLIANCE-FLAGS.                                 9915845
035800         05  BC-REG-E                    PIC X.                   9915845
035900         05  BC-REG-CC                   PIC X.                   9915845
036000         05  BC-REG-DD                   PIC X.                   9915845
036100         05  BC-REG-Z                    PIC X.                   9915845
036200         05  BC-REG-1042                 PIC X.                   9916034
036300         05  BC-REG-06                   PIC X.                   9915845
036400         05  BC-REG-07                   PIC X.                   9915845
036500         05  BC-REG-08                   PIC X.                   9915845
036600         05  BC-REG-09                   PIC X.                   9915845
036700         05  BC-REG-10                   PIC X.                   9915845
036800         05  BC-REG-11                   PIC X.                   9915845
037900         05  BC-REG-12                   PIC X.                   9915845
038100         05  BC-REG-13                   PIC X.                   9915845
038200         05  BC-REG-14                   PIC X.                   9915845
038300         05  FILLER                      PIC X(10).               9915845
040000     03  BC-INTL-FILLER                  PIC X(50).               9915845
041000     03  BC-PLANFILE-ONLINE-EDIT-INFO.                            0617360
041100         05  BC-PL-XREF-EDIT             PIC X.                   0617360
041200         05  BC-PL-PRIOR-YR-CUTOFF-FLAG PIC X.                    0617360
041300         05  BC-PL-PRIOR-YR-CUTOFF-DATE.                          0617360
041400             09  BC-PL-CUTOFF-MONTH      PIC XX.                  0617360
041500             09  BC-PL-CUTOFF-DAY        PIC XX.                  0617360
041600         05  FILLER                      PIC X(4).                0617360
041800     03  BC-PL-LIMIT-TABLE.                                       0617360
041900         05  BC-PL-CONT-LIMITS     OCCURS 5 TIMES                 0617360
042000                                   INDEXED BY BC-PL-LMT-IND.      0617360
042100             07  BC-PL-CY-MAX-CONT       PIC S9(7)V99 COMP-3.     0617360
042200             07  BC-PL-PY-MAX-CONT       PIC S9(7)V99 COMP-3.     0617360
042300             07  BC-PL-CY-CU-MAX-CONT    PIC S9(7)V99 COMP-3.     0617360
042400             07  BC-PL-PY-CU-MAX-CONT    PIC S9(7)V99 COMP-3.     0617360
042500     03  FILLER                          PIC X(20).               0617360
042600     03  BC-PL-TYPE-MAX-CNT-PTR-TABLE    OCCURS 5 TIMES.          0617360
042700         05  BC-PL-TYPE-MAX-CNT-PTR1     PIC S9(4) COMP.          0617360
042800         05  BC-PL-TYPE-MAX-CNT-PTR2     PIC S9(4) COMP.          0617360
042900         05  BC-PL-PLAN-TYPE             PIC XX.                  0617360
043000     03  FILLER                          PIC X(6).                0617360
043100     03  BC-PL-CROSS-REF-CTLS.                                    0617360
043200         05  BC-PL-XREF-CTL2             PIC X.                   0617360
043300         05  BC-PL-XREF-CTL3             PIC X.                   0617360
043400         05  BC-PL-XREF-CTL4             PIC X.                   0617360
043500     03  BC-PL-FILE-IN-USE               PIC X.                   0617360
043600     03  BC-PHASE-PLAN-KEY-GEN           PIC X(4).                0617360
043700     03  BC-PHASE-PLAN-KEY-EDIT          PIC X(4).                0617360
043800     03  BC-PL-RECERT-REQD               PIC X.                   0827778
060000     03  FILLER                          PIC X(43).               0827778
080000     03  BC-LAST-MAINT.                                           9915845
080100         05  BC-LM-ONLINE-RBA        PIC X(4).                    9915845
080200         05  BC-LM-OPER-ID.                                       9915845
080300             07  BC-LM-TS-TELLER     PIC X(5).                    9915845
080400             07  FILLER              PIC X(3).                    9915845
080500         05  BC-LM-BRANCH            PIC X(3).                    9915845
080600         05  BC-LM-TERM-ID           PIC X(4).                    9915845
080700         05  BC-LM-DATE.                                          9915845
080800             07  BC-LM-DT-CC         PIC XX.                      9915845
080900             07  BC-LM-DT.                                        9915845
081000                 09  BC-LM-DT-YY     PIC XX.                      9915845
081100                 09  BC-LM-DT-MM     PIC XX.                      9915845
081200                 09  BC-LM-DT-DD     PIC XX.                      9915845
081300         05  BC-LM-TIME.                                          9915845
081400             07  BC-LM-HH-MM-SS      PIC S9(7)   COMP-3.          9915845
