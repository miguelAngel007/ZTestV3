*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*0902557
000200*              IMPACS WORK BCR CARD 1                            *0902557
000300*----------------------------------------------------------------*0902557
002800 01  DDA-WRKBCR-1.
002900     03  WBC1-CONTROL-KEY.
003000         05  WBC1-CONTROL-1          PIC XX.                      IM008
003100         05  WBC1-CONTROL-2          PIC XXX.                     IM008
003200         05  WBC1-CONTROL-3          PIC XXX.                     IM008
003300     03  WBC1-RECORD-ID              PIC XX.                      IM008
003400     03  FILLER                      PIC X(10).                   9915845
003500     03  WBC-GROUP                   PIC X.                       IM008
003600     03  WBC-DATA-CENTER-ID          PIC X(4).                    IM008
003700     03  WBC-CONTROL1-NAME           PIC X(10).                   IM008
003800     03  WBC-CTL2-FLAG               PIC X.                       IM008
003900     03  WBC-CONTROL2-NAME           PIC X(10).                   IM008
004000     03  WBC-CTL3-FLAG               PIC X.                       IM008
004100     03  WBC-CONTROL3-NAME           PIC X(10).                   IM008
004200     03  WBC-CTL4-FLAG               PIC X.                       IM008
004300     03  WBC-CONTROL4-NAME           PIC X(10).                   IM008
004400     03  WBC-REPT-NAME               PIC X(30).                   IM008
004500     03  WBC-PROCESS-FLAG            PIC X.                       IM008
004600     03  WBC-RUN-FLAG                PIC X.                       IM008
004700     03  WBC-SYSTEM-DATE.
004800         05  WBC-SYSTEM-MO           PIC XX.                      IM008
004900         05  FILLER                  PIC X.                       IM008
005000         05  WBC-SYSTEM-DA           PIC XX.                      IM008
005100         05  FILLER                  PIC X.                       IM008
005200         05  WBC-SYSTEM-YR           PIC XX.                      IM008
005300     03  WBC-SYSTEM-TIME.
005400         05  WBC-SYSTEM-HR           PIC XX.                      IM008
005500         05  WBC-SYSTEM-MIN          PIC XX.                      IM008
005600         05  WBC-SYSTEM-SEC          PIC XX.                      IM008
005700     03  WBC-TODAYS-PROCESS-DATES.
005800         05  WBC-CAPTURE-DATE.
005900             07  WBC-CAPTURE-MO      PIC XX.
006000             07  WBC-CAPTURE-DA      PIC XX.
006100             07  WBC-CAPTURE-YR      PIC XX.
006200         05  WBC-PROCESS-THRU-DATE.
006300             07  WBC-PROC-THRU-MO    PIC XX.
006400             07  WBC-PROC-THRU-DA    PIC XX.
006500             07  WBC-PROC-THRU-YR    PIC XX.
006600         05  WBC-STMT-THRU-DATE.
006700             07  WBC-STMT-MO         PIC XX.
006800             07  WBC-STMT-DA         PIC XX.
006900             07  WBC-STMT-YR         PIC XX.
007000         05  WBC-REPT-THRU-DATE.
007100             07  WBC-REPT-MO         PIC XX.
007200             07  WBC-REPT-DA         PIC XX.
007300             07  WBC-REPT-YR         PIC XX.
007400     03  WBC-CURRENT-DAYS.
007500         05  WBC-PAST-ACCR-DAYS      PIC S999    COMP-3.          IM008
007600         05  WBC-ACCRUAL-DAYS        PIC S999    COMP-3.          IM008
007700         05  WBC-WEEK-DAY            PIC X.
007800         05  WBC-BANKING-DAYS        PIC S999    COMP-3.          IM008
007900         05  WBC-MONTH-END           PIC X.
008000         05  WBC-QUARTER-END         PIC X.
008100         05  WBC-YEAR-END            PIC X.
008150         05  WBC-ALT-DUAL-YEAR-END   PIC X.                       9915858
008200         05  WBC-BUSINESS-DAYS       PIC S999    COMP-3.          IM008
008250         05  FILLER                  PIC X.                       9915845
008300     03  WBC-LAST-PROCESS-DATES.
008400         05  WBC-LAST-CAPTURE-DATE.
008500             07  WBC-LAST-CAPTURE-MO PIC XX.
008600             07  WBC-LAST-CAPTURE-DA PIC XX.
008700             07  WBC-LAST-CAPTURE-YR PIC XX.
008800         05  WBC-LAST-PROC-THRU-DATE.
008900             07  WBC-LAST-PROC-MO    PIC XX.
009000             07  WBC-LAST-PROC-DA    PIC XX.
009100             07  WBC-LAST-PROC-YR    PIC XX.
009200         05  WBC-LAST-STMT-DATE.
009300             07  WBC-LAST-STMT-MO    PIC XX.
009400             07  WBC-LAST-STMT-DA    PIC XX.
009500             07  WBC-LAST-STMT-YR    PIC XX.
009600         05  WBC-LAST-REPT-DATE.
009700             07  WBC-LAST-REPT-MO    PIC XX.
009800             07  WBC-LAST-REPT-DA    PIC XX.
009900             07  WBC-LAST-REPT-YR    PIC XX.
010000     03  WBC-PREV-DAYS.
010100         05  WBC-PREV-PAST-ACCR-DAYS PIC S999    COMP-3.          IM008
010200         05  WBC-PREV-ACCR-DAYS      PIC S999    COMP-3.          IM008
010300         05  WBC-PREV-WEEK-DAY       PIC X.
010400         05  WBC-PREV-BANK-DAYS      PIC S999    COMP-3.          IM008
010500         05  WBC-PREV-MONTH-END      PIC X.
010600         05  WBC-PREV-QUARTER-END    PIC X.
010700         05  WBC-PREV-YEAR-END       PIC X.
010750         05  WBC-PREV-ALT-DUAL-YEAR-END PIC X.                    9915858
010800         05  WBC-PREV-BUS-DAYS       PIC S999    COMP-3.          IM008
010850         05  FILLER                  PIC X.                       9915845
010900     03  WBC-NEXT-PROCESS-DATES.
011000         05  WBC-NEXT-CAPTURE-DATE.
011100             07  WBC-NEXT-CAPTURE-MO PIC XX.
011200             07  WBC-NEXT-CAPTURE-DA PIC XX.
011300             07  WBC-NEXT-CAPTURE-YR PIC XX.
011400         05  WBC-NEXT-PROC-THRU-DATE.
011500             07  WBC-NEXT-PROC-MO    PIC XX.
011600             07  WBC-NEXT-PROC-DA    PIC XX.
011700             07  WBC-NEXT-PROC-YR    PIC XX.
011800         05  WBC-NEXT-STMT-DATE.
011900             07  WBC-NEXT-STMT-MO    PIC XX.
012000             07  WBC-NEXT-STMT-DA    PIC XX.
012100             07  WBC-NEXT-STMT-YR    PIC XX.
012200         05  WBC-NEXT-REPT-DATE.
012300             07  WBC-NEXT-REPT-MO    PIC XX.
012400             07  WBC-NEXT-REPT-DA    PIC XX.
012500             07  WBC-NEXT-REPT-YR    PIC XX.
012600     03  WBC-NEXT-DAYS.
012700         05  WBC-NEXT-PAST-ACCR-DAYS PIC S999    COMP-3.          IM008
012800         05  WBC-NEXT-ACCR-DAYS      PIC S999    COMP-3.          IM008
012900         05  WBC-NEXT-WEEK-DAY       PIC X.
013000         05  WBC-NEXT-BANK-DAYS      PIC S999    COMP-3.          IM008
013100         05  WBC-NEXT-MONTH-END      PIC X.
013200         05  WBC-NEXT-QUARTER-END    PIC X.
013300         05  WBC-NEXT-YEAR-END       PIC X.
013350         05  WBC-NEXT-ALT-DUAL-YEAR-END PIC X.                    9915858
013400         05  WBC-NEXT-BUS-DAYS       PIC S999    COMP-3.          IM007
013430         05  FILLER                  PIC X.                       9915845
013460     03  WBC-NEXT-CENT-YR            PIC XX.                      9915845
013500     03  WBC-OCCUR.
013600         05  WBC-OCCURRENCE          PIC S9      COMP-3
013700                 OCCURS 7 TIMES.                                  9915845
013710     03  WBC-WEEKDAY-BANK.                                        9915845
013720         05  WBC-MONDAY-BK           PIC X.                       9915845
013730         05  WBC-TUESDAY-BK          PIC X.                       9915845
013740         05  WBC-WEDNESDAY-BK        PIC X.                       9915845
013750         05  WBC-THURSDAY-BK         PIC X.                       9915845
013760         05  WBC-FRIDAY-BK           PIC X.                       9915845
013770         05  WBC-SATURDAY-BK         PIC X.                       9915845
013780         05  WBC-SUNDAY-BK           PIC X.                       9915845
013782     03  FILLER REDEFINES WBC-WEEKDAY-BANK.                       9915682
013784         05  WBC-WEEKDAY-BK          PIC X       OCCURS 7 TIMES.  9915682
013790     03  FILLER                      PIC X(7).                    9915845
013800     03  WBC-FILE-SEQUENCE-NOS.                                   9915845
013900         05  WBC-CURR-FILE-SEQ       PIC S9(5)   COMP-3.          IM008
014000         05  WBC-PREV-FILE-SEQ       PIC S9(5)   COMP-3.          IM008
014100     03  WBC-COMPLETE-FLAGS.
014200         05  WBC-EDIT1-COMP          PIC X.
014300         05  WBC-EDIT2-COMP          PIC X.
014400         05  WBC-IM25-ENTRY-COMP     PIC X.
014500         05  WBC-IM26-COMP           PIC X.                       IM008
014600         05  WBC-STOP-HOLD-MAINT     PIC X.
014700         05  WBC-IM28-COMP           PIC X.                       IM008
014800         05  WBC-FM-POST-COMP        PIC X.
014900         05  WBC-FILE-SPLIT-COMP     PIC X.
015000         05  WBC-TRAN-MERGE-COMP     PIC X.
015010         05  WBC-IM19-COMP           PIC X.                       IM008
015020         05  WBC-MAINT-MERGE-COMP    PIC X.                       0326947
015100         05  FILLER                  PIC X(6).                    0326947
015400     03  WBC-HOLIDAYS-THIS-YR.
015500         05  WBC-HOLIDAYS OCCURS 36 TIMES INDEXED BY WBC-HOL-IND. 0817725
015550             07  WBC-HOLIDAY-DATE.                                IM007
015600                 09  WBC-HOLIDAY-YEAR.                            IM007
015700                     11  WBC-HOLIDAY-CENT    PIC XX.              IM007
015800                     11  WBC-HOLIDAY-YR      PIC XX.              IM007
015900                 09  WBC-HOLIDAY-MO          PIC XX.              IM007
016000                 09  WBC-HOLIDAY-DA          PIC XX.              IM007
016010             07  FILLER REDEFINES WBC-HOLIDAY-DATE.               IM008
016020                 09  FILLER                  PIC XX.              IM008
016030                 09  WBC-HOL-DATE            PIC X(6).            IM008
016100             07  WBC-FED-RES-HOL             PIC X.               IM007
016200     03  WBC-SERVICE-CHG-CODE        PIC X.                       IM008
016300     03  WBC-FULL-PAGE               PIC S999    COMP-3.          IM008
016400     03  WBC-ARP-FLAG                PIC X.                       IM008
016500     03  WBC-LOAN-SYS-NAME           PIC X(30).                   IM008
016600     03  WBC-FLOAT-FLAG              PIC X.                       IM008
016700     03  WBC-LEAP-YEAR               PIC X.                       IM008
016800     03  WBC-AUTO-RELEASE            PIC X.
016900     03  WBC-HOLD-SUSPECT            PIC X.
017000     03  WBC-LIST-POST-NO            PIC S999    COMP-3.          IM008
017100     03  WBC-POST-AMT-SEQ            PIC X.                       IM008
017200     03  WBC-ANALYSIS-PHASE          PIC X(4).                    IM008
017300     03  WBC-TRANEDT-PHASE           PIC X(4).                    IM008
017400     03  WBC-DLCA-FLAG               PIC X.                       9916049
017500     03  WBC-SELF-CHK-PHASE          PIC X(4).                    IM008
017600     03  WBC-MONTH-DAYS              PIC S999    COMP-3.          IM008
017700     03  WBC-LIST-POST-DETAIL        PIC X.                       IM008
017800     03  WBC-LIST-POST-OD            PIC X.                       IM008
017900     03  WBC-TRANSTBL-PHASE          PIC X(6).                    0517286
017910     03  FILLER                      PIC XX.                      0517286
018000     03  WBC-REPNAME-PHASE           PIC X(8).                    IM008
018100     03  WBC-REPTBL-PHASE            PIC X(8).                    IM008
018200     03  WBC-EXCTBL-PHASE            PIC X(8).                    IM008
018300     03  WBC-ACCUM-TRAN-RET          PIC S999    COMP-3.
018400     03  WBC-NON-DOL-RETENTION       PIC S999    COMP-3.
018410     03  WBC-IOD-NEG-ACCRUAL         PIC X.                       0902213
018420     03  WBC-SAV-NEG-ACCRUAL         PIC X.                       0902213
018430     03  WBC-OD-NEG-ACCRUAL          PIC X.                       0902213
018700     03  WBC-BACKDATE-LIMIT          PIC S999    COMP-3.
018800     03  WBC-QUARTER-DAYS            PIC S999    COMP-3.          9715132
018900     03  WBC-BAL-HIST-FLAG           PIC X.                       0902213
019000     03  WBC-ALLOW-BCKDT-HIFI        PIC X.
019100     03  WBC-RPT-DAILY-NSF-OD-TRNS   PIC X.
019200     03  WBC-BAL-CHNG-BY-DB-CR       PIC X.
019300     03  WBC-GL-CONTROL-LEVEL        PIC X.
019400     03  WBC-GEN-DB-ON-NEG-ACCRUAL   PIC X.
019500     03  WBC-YEAR-END-INT-CODE       PIC X.                       IM006
019550     03  WBC-MEMO-CLEAR              PIC X.                       IM004
019600     03  WBC-ACCRUE-AHEAD            PIC X.
019700     03  WBC-PURGE-ON-SCHED          PIC X.
019710     03  WBC-INT-DIST-CD             PIC X.                       IM006
019720     03  WBC-INT-DIST-AMT            PIC S9(13)V99 COMP-3.        9915845
019730     03  WBC-ONLINE-OD-FLAG          PIC X.                       IM006
019740     03  WBC-ONLINE-LOC-FLAG         PIC X.                       IM006
019750     03  WBC-ONLINE-HOLD-FLAG        PIC X.                       IM006
019760     03  WBC-ONLINE-NSF-OD-CHRG      PIC X.                       0901891
019770     03  WBC-ONLINE-SAV-FLAG         PIC X.                       IM006
019780     03  WBC-ONLINE-ALPHA-RECORD     PIC X.                       IM006
019782     03  WBC-ARCH-HIST               PIC X.                       0927050
019785     03  FILLER                      PIC X.                       0927050
019786     03  WBC-NONPOST-HIST-OPT        PIC X.                       0737697
019788     03  WBC-ACCT-EDIT-REBUILD       PIC X.                       0417078
019790     03  WBC-STMT-PULL-CODE          PIC X.                       IM007
019793     03  WBC-ACCT-EDIT-MASK          PIC X(13).                   9915845
019794     03  WBC-STOP-HIT-OPTION         PIC X.                       0266778
019795     03  WBC-MULTI-STOP-HIT          PIC X.                       0266778
019796     03  WBC-SPEC-INSTR-SUSPECT      PIC X.                       0627534
019797     03  WBC-EXTEND-FLOAT-OPT        PIC X.                       0927817
019798     03  WBC-EXTEND-FLOAT-DAYS       PIC S999    COMP-3.          0927817
019800     03  FILLER                      PIC X(6).                    0927817
019808     03  WBC-PAST-BUS-DAYS           COMP-3.                      0927817
019810         05  WBC-PREV-PAST-BUS-DAYS PIC S999.                     9915845
019820         05  WBC-CURR-PAST-BUS-DAYS PIC S999.                     9915845
019830         05  WBC-NEXT-PAST-BUS-DAYS PIC S999.                     9915845
019850     03  WBC-ACCUM-TRAN-SRC          PIC X.                       IM006
019900     03  WBC-ABA-NUMBER              PIC X(9)    OCCURS 4 TIMES.  IM008
020000     03  WBC-PRODUCT-CTL-FLAG        PIC X.                       IM008
020010     03  WBC-LOOKUP-HEADING          PIC X.                       9915845
020020     03  WBC-MAINT-HIST-OPT          PIC X.                       0326947
020025     03  WBC-PASSBOOKS-ALLOWED       PIC X.                       0427139
020030     03  WBC-BAL-HIST-COMPRESS       PIC X.                       0447266
020032     03  WBC-OL-FUND-IND             PIC X.                       0517279
020050     03  WBC-MASK-ID-PHASE           PIC X(4).                    0817719
020060     03  WBC-MASK-ID-MAINLINE.                                    0817719
020062         05  WBC-MASK-ID-IM05        PIC X.                       0817719
020064         05  WBC-MASK-ID-IM12        PIC X.                       0817719
020066         05  WBC-MASK-ID-IM14        PIC X.                       0817719
020068         05  WBC-MASK-ID-IM21        PIC X.                       0817719
020070         05  WBC-MASK-ID-IM22        PIC X.                       0817719
020072         05  WBC-MASK-ID-IM30        PIC X.                       0817719
020074         05  WBC-MASK-ID-IM7A        PIC X.                       0817719
020076         05  WBC-MASK-ID-IM78        PIC X.                       0817719
020078         05  WBC-MASK-ID-IM79        PIC X.                       0817719
020080         05  FILLER                  PIC X.                       0817719
020090     03  FILLER                      PIC X(5).                    0817719
020100     03  WBC-TOTAL-LEVEL-FLAGS.
020200         05  WBC-IM21-TOT-LVL        PIC X.                       IM008
020300         05  WBC-IM22-TOT-LVL        PIC X.                       IM008
020400         05  WBC-IM25-TOT-LVL        PIC X.                       IM008
020500         05  WBC-IM26-TOT-LVL        PIC X.                       IM008
020600         05  WBC-IM27-TOT-LVL        PIC X.                       IM008
020700         05  WBC-IM28-TOT-LVL        PIC X.                       IM008
020800         05  WBC-IM31-TOT-LVL        PIC X.                       IM008
020900         05  WBC-IM32-TOT-LVL        PIC X.                       IM008
021000         05  WBC-IM34-TOT-LVL        PIC X.                       IM008
021010         05  WBC-IM34OM-TOT-LVL      PIC X.                       0326947
021100         05  FILLER                  PIC X(7).                    0326947
021200     03  WBC-RESET-PAGE-CNT-FLAGS.
021300         05  WBC-IM21-RESET-PG       PIC X.                       IM008
021400         05  WBC-IM22-RESET-PG       PIC X.                       IM008
021500         05  WBC-IM25-RESET-PG       PIC X.                       IM008
021600         05  WBC-IM26-RESET-PG       PIC X.                       IM008
021700         05  WBC-IM27-RESET-PG       PIC X.                       IM008
021800         05  WBC-IM28-RESET-PG       PIC X.                       IM008
021900         05  WBC-IM31-RESET-PG       PIC X.                       IM008
022000         05  WBC-IM32-RESET-PG       PIC X.                       IM008
022100         05  WBC-IM34-RESET-PG       PIC X.                       IM008
022110         05  WBC-IM34OM-RESET-PG     PIC X.                       0326947
022200         05  FILLER                  PIC X(7).                    0326947
022300     03  WBC-DEFAULT-DR              PIC XXXX.                    IM004
022400     03  WBC-PRIORITY-DR             PIC XXXX.                    IM004
022500     03  WBC-DEFAULT-CR              PIC XXXX.                    IM004
022600     03  WBC-ANALYSIS-MOS            PIC S999    COMP-3.          IM004
022700     03  WBC-ANALYSIS-FACTORS                    COMP-3.          IM008
022800         05  WBC-ANALYSIS-EARN-RATE  PIC S9V9(8).                 9915845
022810         05  WBC-ANALYSIS-EARN-RATE1  REDEFINES                   9915845
022820                 WBC-ANALYSIS-EARN-RATE  PIC S9(3)V9(6).          9915845
022900         05  WBC-ANALYSIS-RES-REQ    PIC S9V9(8).                 9915845
022910         05  WBC-ANALYSIS-RES-REQ1    REDEFINES                   9915845
022920                 WBC-ANALYSIS-RES-REQ    PIC S9(3)V9(6).          9915845
023000         05  WBC-ANALYSIS-DR-COST    PIC S99V999.                 9915845
023100         05  WBC-ANALYSIS-CR-COST    PIC S99V999.                 9915845
023200         05  WBC-ANALYSIS-LOCAL-RATE PIC S99V999.                 9915845
023300         05  WBC-ANALYSIS-FOR-RATE   PIC S99V999.                 9915845
023400         05  WBC-ANALYSIS-MAINT-CHG  PIC S9(9)V99.                9915845
023500         05  WBC-ANALYSIS-NEG-EARN   PIC S9V9(8).                 9915845
023510         05  WBC-ANALYSIS-NEG-EARN1   REDEFINES                   9915845
023520                 WBC-ANALYSIS-NEG-EARN   PIC S9(3)V9(6).          9915845
023600         05  WBC-ANALYSIS-CASH-COST  PIC S9V9(8).                 9915845
023610         05  WBC-ANALYSIS-CASH-COST1  REDEFINES                   9915845
023620                 WBC-ANALYSIS-CASH-COST  PIC S9(3)V9(6).          9915845
023700         05  WBC-ANALYSIS-ALT-EARN   PIC S9V9(8).                 9915845
023710         05  WBC-ANALYSIS-ALT-EARN1   REDEFINES                   9915845
023720                 WBC-ANALYSIS-ALT-EARN   PIC S9(3)V9(6).          9915845
023800         05  WBC-ANALYSIS-NONPAR-ITM PIC S99V999.                 9915845
023900         05  WBC-ANALYSIS-MISC-CNTRS PIC S99V999  OCCURS 25 TIMES.9915845
024000     03  WBC-ANALYSIS-FLOAT          PIC X.                       IM008
024100     03  WBC-RATE-TABLE-PHASE        PIC X(8).                    IM008
024200     03  WBC-DESC-TABLE-PHASE        PIC X(8).                    IM008
024300     03  WBC-BILL-STAT-FLAG          PIC X.                       IM008
024310     03  WBC-REP-RCP-FLAG            PIC X.                       IM008
024320     03  WBC-CLOSING-TRAN-PAYOFF.                                 IM008
024330         05  WBC-CLOSING-TRAN-WAIVE PIC X.                        9915845
024340         05  WBC-CLOSING-TRAN-TOLER PIC S9(13)V99    COMP-3.      9915845
024350     03  WBC-FILE-STAT-PRINT.                                     IM008
024360         05  WBC-BCRM-PRINT-OPTION PIC X.                         9915845
024400         05  WBC-LKPM-PRINT-OPTION   PIC X.                       IM008
024450         05  WBC-PRDM-PRINT-OPTION   PIC X.                       IM008
024500         05  WBC-MSGM-PRINT-OPTION   PIC X.                       IM008
024550         05  WBC-TBLM-PRINT-OPTION   PIC X.                       IM008
024560         05  FILLER                  PIC X(4).                    9915845
024600     03  WBC-FILE-PRINT-SPOOL.                                    IM008
024650         05  WBC-BCRM-PRINT-SPOOL    PIC X.                       IM008
024750         05  WBC-LKPM-PRINT-SPOOL    PIC X.                       IM008
024800         05  WBC-PRDM-PRINT-SPOOL    PIC X.                       IM008
024850         05  WBC-MSGM-PRINT-SPOOL    PIC X.                       IM008
024900         05  WBC-TBLM-PRINT-SPOOL    PIC X.                       IM008
024910         05  FILLER                  PIC X(4).                    9915845
024950     03  WBC-FILE-PRINT-FORM.                                     IM008
025600         05  WBC-BCRM-PRINT-FORM     PIC X.                       9915845
025700         05  WBC-LKPM-PRINT-FORM     PIC X.                       IM008
025750         05  WBC-PRDM-PRINT-FORM     PIC X.                       IM008
025800         05  WBC-MSGM-PRINT-FORM     PIC X.                       IM008
025850         05  WBC-TBLM-PRINT-FORM     PIC X.                       IM008
025860         05  FILLER                  PIC X(4).                    9915845
025900     03  WBC-SPOOL-CODES.
025950         05  WBC-IM05-SPOOL          PIC X.                       IM008
026000         05  WBC-IM12-SPOOL          PIC X.                       IM008
026050         05  WBC-IM13-SPOOL          PIC X.                       IM008
026100         05  WBC-IM21-SPOOL          PIC X.                       IM008
026150         05  WBC-IM22-SPOOL          PIC X.                       IM008
026200         05  WBC-IM25-SPOOL          PIC X.                       IM008
026250         05  WBC-IM26-SPOOL          PIC X.                       IM008
026300         05  WBC-IM27-SPOOL          PIC X.                       IM008
026350         05  WBC-IM28-SPOOL          PIC X.                       IM008
026400         05  WBC-IM31-SPOOL          PIC X.                       IM008
026450         05  WBC-IM32-SPOOL          PIC X.                       IM008
026500         05  WBC-IM33-SPOOL          PIC X.                       IM008
026550         05  WBC-IM34-SPOOL          PIC X.                       IM008
026600         05  WBC-IM43-SPOOL          PIC X.                       IM008
026650         05  WBC-IM44-SPOOL          PIC X.                       IM008
026700         05  WBC-IM45-SPOOL          PIC X.                       IM008
026750         05  WBC-IM61-SPOOL          PIC X.                       IM008
026800         05  WBC-IM67-SPOOL          PIC X.                       IM008
026850         05  WBC-IM73-SPOOL          PIC X.                       1004773
026900         05  WBC-IM74-SPOOL          PIC X.                       1004773
026950         05  WBC-IM75-SPOOL          PIC X.                       IM008
027000         05  WBC-IM78-SPOOL          PIC X.                       IM008
027050         05  WBC-IM91-SPOOL          PIC X.                       IM008
027100         05  WBC-IM92-SPOOL          PIC X.                       IM008
027125         05  WBC-IM30-SPOOL          PIC X.                       0902557
027150         05  WBC-IM29ZBA-SPOOL       PIC X.                       9715504
027155         05  WBC-IM20XIM-SPOOL       PIC X.                       2016639
027160         05  WBC-IM30XIB-SPOOL       PIC X.                       2016639
027165         05  WBC-IM30XIT-SPOOL       PIC X.                       2016639
027170         05  WBC-IM34OM-SPOOL        PIC X.                       0326947
027200         05  FILLER                  PIC X(4).                    0326947
027250     03  WBC-FORM-CODES.                                          IM008
027300         05  WBC-IM05-FORM           PIC X.                       IM008
027350         05  WBC-IM12-FORM           PIC X.                       IM008
027400         05  WBC-IM13-FORM           PIC X.                       IM008
027450         05  WBC-IM21-FORM           PIC X.                       IM008
027500         05  WBC-IM22-FORM           PIC X.                       IM008
027550         05  WBC-IM25-FORM           PIC X.                       IM008
027600         05  WBC-IM26-FORM           PIC X.                       IM008
027650         05  WBC-IM27-FORM           PIC X.                       IM008
027700         05  WBC-IM28-FORM           PIC X.                       IM008
027750         05  WBC-IM31-FORM           PIC X.                       IM008
027800         05  WBC-IM32-FORM           PIC X.                       IM008
027850         05  WBC-IM33-FORM           PIC X.                       IM008
027900         05  WBC-IM34-FORM           PIC X.                       IM008
027950         05  WBC-IM43-FORM           PIC X.                       IM008
028000         05  WBC-IM44-FORM           PIC X.                       IM008
028050         05  WBC-IM45-FORM           PIC X.                       IM008
028100         05  WBC-IM61-FORM           PIC X.                       IM008
028150         05  WBC-IM67-FORM           PIC X.                       IM008
028200         05  WBC-IM73-FORM           PIC X.                       1004773
028250         05  WBC-IM74-FORM           PIC X.                       1004773
028300         05  WBC-IM75-FORM           PIC X.                       IM008
028350         05  WBC-IM78-FORM           PIC X.                       IM008
028400         05  WBC-IM91-FORM           PIC X.                       IM008
028450         05  WBC-IM92-FORM           PIC X.                       IM008
028470         05  WBC-IM30-FORM           PIC X.                       0902557
028500         05  WBC-IM29ZBA-FORM        PIC X.                       9715504
028505         05  WBC-IM20XIM-FORM        PIC X.                       2016639
028510         05  WBC-IM30XIB-FORM        PIC X.                       2016639
028515         05  WBC-IM30XIT-FORM        PIC X.                       2016639
028520         05  WBC-IM34OM-FORM         PIC X.                       0326947
028550         05  FILLER                  PIC X(4).                    0326947
028600     03  FILLER                      PIC X(6).                    IM008
028700     03  WBC-BK-IRS-NO               PIC X(10).                   IM008
028730     03  WBC-ISO-COUNTRY             PIC XX.                      9915845
028760     03  WBC-ISO-IRS-TAX             PIC X.                       9915845
028800     03  WBC-USER-REPT-SCHEDULE                  OCCURS 10 TIMES  IM008
028900             INDEXED BY WBC-USER-INDEX.
029000         05  WBC-USER-CODE           PIC X.                       IM008
029100         05  WBC-USER-SCHD-TYPE      PIC X.                       IM008
029200         05  WBC-USER-DAY1           PIC XX.                      IM008
029300         05  WBC-USER-DAY2           PIC XX.                      IM008
029400         05  WBC-USER-REPT-FREQ      PIC X.                       IM008
029500         05  WBC-USER-MONTH          PIC X.                       IM008
029600     03  WBC-SOC-SEC-DATA.
029700         05  WBC-SOC-SEC-CODE        PIC X.                       IM008
029800         05  WBC-SOC-SEC-TRANSMIT    PIC X(5).
029900     03  WBC-ACCT-DISPLACEMENT       PIC X.                       IM008
030000     03  WBC-CONS-STAT               PIC X.                       IM008
030100     03  WBC-IRREG-STMT-CYCLES                   OCCURS 10 TIMES. IM008
030200         05  WBC-IRR-STMT-CYCLE      PIC XX.
030300         05  WBC-IRR-STMT-INCRM      PIC X.
030400         05  WBC-IRR-STMT-BEGIN-MO   PIC X.
030500     03  WBC-STMT-FORM-SORT          PIC X.                       IM008
030600     03  FILLER                      PIC X(26).                   0930011
030700     03  WBC-SC-INSTALLED            PIC X.                       1004962
030710     03  WBC-NX-INSTALLED            PIC X.                       9915845
030720     03  WBC-XINV-INSTALLED          PIC X.                       2016639
030800     03  FILLER                      PIC X(16).                   2016639
033400     03  WBC-TRANSFER-INFORMATION.                                IM006
033500         05  WBC-TRANSFER-FLG-CL     PIC X.                       IM008
033600         05  WBC-TRANSFER-FLG-AL     PIC X.                       IM008
033700         05  WBC-TRANSFER-FLG-IC     PIC X.                       IM008
033800         05  WBC-TRANSFER-FLG-IM     PIC X.                       IM008
033900         05  WBC-TRANSFER-FLG-ST     PIC X.                       IM008
034000         05  WBC-TRANSFER-FLG-RE     PIC X.                       IM008
034100         05  WBC-TRANSFER-IT32       PIC X.                       IM008
034200     03  FILLER                      PIC X(9).                    IM008
034202     03  WBC-LANGUAGE-AREA.                                       9915845
034204         05  WBC-MULTI-LANGUAGE-FLAG PIC X.                       9915845
034206         05  WBC-DEFAULT-LANGUAGE    PIC XX.                      9915845
034208     03  FILLER                      PIC X(8).                    9915845
034210     03  WBC-CURR-INFO.                                           9915845
034212         05  WBC-CURR-CTL-LEVEL      PIC X.                       9915845
034214         05  WBC-CURR-CODE           PIC XXX.                     9915845
034216         05  WBC-CURR-DEC            PIC X.                       9915845
034218         05  WBC-CURR-BASE-CODE      PIC XXX.                     9915845
034220         05  WBC-CURR-BASE-DEC       PIC X.                       9915845
034222         05  WBC-CURR-CTL1-CODE      PIC XXX.                     9915845
034224         05  WBC-CURR-CTL1-DEC       PIC X.                       9915845
034226         05  WBC-CURR-QUAL           PIC X(4).                    9915845
034228     03  FILLER                      PIC X(8).                    9915845
034230     03  WBC-ALT-DUAL-YEAR-END-INFO.                              9915858
034232         05  WBC-ALT-DUAL-YR-OPTION      PIC X.                   9915858
034234         05  WBC-ALT-DUAL-YR-DATE.                                9915858
034236             07  WBC-ALT-DUAL-YR-CENT        PIC XX.              9915858
034238             07  WBC-ALT-DUAL-YR-WO-CENT.                         9915858
034240                 09  WBC-ALT-DUAL-YR-YR      PIC XX.              9915858
034242                 09  WBC-ALT-DUAL-YR-MO      PIC XX.              9915858
034244                 09  WBC-ALT-DUAL-YR-DA      PIC XX.              9915858
034246     03  WBC-ACCT-PURGE-DATE.                                     9915845
034248         05  WBC-ACCT-PURGE-MO           PIC XX.                  9915845
034250         05  WBC-ACCT-PURGE-DA           PIC XX.                  9915845
034252     03  WBC-ACCT-PURGE-FLAG             PIC X.                   9915845
034254     03  WBC-REG-COMPLIANCE-FLAGS.                                9915845
034256         05  WBC-REG-E                   PIC X.                   9915845
034258         05  WBC-REG-CC                  PIC X.                   9915845
034260         05  WBC-REG-DD                  PIC X.                   9915845
034262         05  WBC-REG-Z                   PIC X.                   9915845
034264         05  WBC-REG-1042                PIC X.                   9916034
034266         05  WBC-REG-06                  PIC X.                   9915845
034268         05  WBC-REG-07                  PIC X.                   9915845
034270         05  WBC-REG-08                  PIC X.                   9915845
034272         05  WBC-REG-09                  PIC X.                   9915845
034274         05  WBC-REG-10                  PIC X.                   9915845
034276         05  WBC-REG-11                  PIC X.                   9915845
034278         05  WBC-REG-12                  PIC X.                   9915845
034280         05  WBC-REG-13                  PIC X.                   9915845
034282         05  WBC-REG-14                  PIC X.                   9915845
034284         05  FILLER                      PIC X(10).               9915845
034286     03  WBC-INTL-FILLER                 PIC X(50).               9915845
034288     03  WBC-PLANFILE-ONLINE-EDIT-INFO.                           0617360
034289         05  WBC-PL-XREF-EDIT            PIC X.                   0617360
034290         05  WBC-PL-PRIOR-YR-CUTOFF-FLAG PIC X.                   0617360
034291         05  WBC-PL-PRIOR-YR-CUTOFF-DATE.                         0617360
034292             09  WBC-PL-CUTOFF-MONTH     PIC XX.                  0617360
034293             09  WBC-PL-CUTOFF-DAY       PIC XX.                  0617360
034294         05  FILLER                      PIC X(4).                0617360
034296     03  WBC-PL-LIMIT-TABLE.                                      0617360
034297         05  WBC-PL-CONT-LIMITS    OCCURS 5 TIMES                 0617360
034298                                   INDEXED BY WBC-PL-LMT-IND.     0617360
034299             07  WBC-PL-CY-MAX-CONT      PIC S9(7)V99 COMP-3.     0617360
034300             07  WBC-PL-PY-MAX-CONT      PIC S9(7)V99 COMP-3.     0617360
034301             07  WBC-PL-CY-CU-MAX-CONT   PIC S9(7)V99 COMP-3.     0617360
034302             07  WBC-PL-PY-CU-MAX-CONT   PIC S9(7)V99 COMP-3.     0617360
034303     03  FILLER                          PIC X(20).               0617360
034304     03  WBC-PL-TYPE-MAX-CNT-PTR-TABLE   OCCURS 5 TIMES.          0617360
034305         05  WBC-PL-TYPE-MAX-CNT-PTR1    PIC S9(4) COMP.          0617360
034306         05  WBC-PL-TYPE-MAX-CNT-PTR2    PIC S9(4) COMP.          0617360
034307         05  WBC-PL-PLAN-TYPE            PIC XX.                  0617360
034308     03  FILLER                          PIC X(6).                0617360
034309     03  WBC-PL-CROSS-REF-CTLS.                                   0617360
034310         05  WBC-PL-XREF-CTL2            PIC X.                   0617360
034311         05  WBC-PL-XREF-CTL3            PIC X.                   0617360
034312         05  WBC-PL-XREF-CTL4            PIC X.                   0617360
034313     03  WBC-PL-FILE-IN-USE              PIC X.                   0617360
034314     03  WBC-PHASE-PLAN-KEY-GEN          PIC X(4).                0617360
034315     03  WBC-PHASE-PLAN-KEY-EDIT         PIC X(4).                0617360
034316     03  WBC-PL-RECERT-REQD              PIC X.                   0827778
034317     03  FILLER                          PIC X(43).               0827778
034395     03  WBC-LAST-MAINT.                                          0617360
034400         05  WBC-LM-ONLINE-RBA       PIC X(4).                    IM008
034500         05  WBC-LM-OPER-ID.                                      IM008
034600             07  WBC-LM-TS-TELLER    PIC X(5).                    IM008
034700             07  FILLER              PIC X(3).                    IM008
034800         05  WBC-LM-BRANCH           PIC X(3).                    IM008
034900         05  WBC-LM-TERM-ID          PIC X(4).                    IM008
035000         05  WBC-LM-DATE.                                         IM008
035100             07  WBC-LM-DT-CC        PIC XX.                      IM008
035200             07  WBC-LM-DT.                                       IM008
035300                 09  WBC-LM-DT-YY    PIC XX.                      IM008
035400                 09  WBC-LM-DT-MM    PIC XX.                      IM008
035500                 09  WBC-LM-DT-DD    PIC XX.                      IM008
035600         05  WBC-LM-TIME.                                         IM008
035700             07  WBC-LM-HH-MM-SS     PIC S9(7)   COMP-3.          IM008
