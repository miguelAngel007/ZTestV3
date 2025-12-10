*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*9715253
000200*            IMPACS WORK BCR CARD 3 COPYBOOK                     *9715253
000300*----------------------------------------------------------------*9715253
001300 01  DDA-WRKBC-3.
001400     03  WBC3-CONTROL-KEY.
001500         05  WBC3-CONTROL-1  PIC XX.
001600         05  WBC3-CONTROL-2  PIC XXX.
001700         05  WBC3-CONTROL-3  PIC XXX.
001800     03  WBC3-RECORD-ID      PIC XX.
001900     03  FILLER              PIC X(10).                           9915845
002000     03  WBC-CONTROL2-TABLE.
002100         05  FILLER OCCURS 5 TIMES.
002200             07  WBC-CTL2-LOW        PIC XXX.
002300             07  WBC-CTL2-HIGH       PIC XXX.
002400     03  WBC-CONTROL3-TABLE.
002500         05  FILLER OCCURS 5 TIMES.
002600             07  WBC-CTL3-LOW        PIC XXX.
002700             07  WBC-CTL3-HIGH       PIC XXX.
002800     03  WBC-CONTROL4-TABLE.
002900         05  FILLER OCCURS 5 TIMES.
003000             07  WBC-CTL4-LOW        PIC XXXX.
003100             07  WBC-CTL4-HIGH       PIC XXXX.
003200     03  FILLER                  PIC X(20).                       1003625
003500     03  WBC-VALID-STATEMENT.
003600         05  FILLER OCCURS 6 TIMES.
003700             07  WBC-VALID-STMT-FORMAT   PIC X.
003800             07  WBC-FORMAT-PROTECT      PIC X.
003900     03  WBC-STATEMENT-CYCLES.
004000         05  WBC-VALID-CYCLE     OCCURS 50 TIMES
004100                 INDEXED BY WBC3-STMT-INDEX.
004200             07  WBC-STMT-CYCL       PIC X.
004300             07  WBC-STMT-PROTECT    PIC X.
004400     03  WBC-STMT-ON-CLOSED          PIC X.
004500     03  WBC-STMT-ON-OD              PIC X.
004600     03  WBC-STMT-ON-TERMINATED      PIC X.
004700     03  WBC-STMT-ON-DORMANT         PIC X.
004800     03  WBC-STATEMENT-OPTIONS   OCCURS 6 TIMES.
004900         05  WBC-LINE-DEBITS COMP-3.
005000             07  WBC-LINE-DR-NORM    PIC S9.
005100             07  WBC-LINE-DR-DESC1   PIC S9.
005200             07  WBC-LINE-DR-DESC2   PIC S9.
005300         05  WBC-LINE-CREDITS    COMP-3.
005400             07  WBC-LINE-CR-NORM    PIC S9.
005500             07  WBC-LINE-CR-DESC1   PIC S9.
005600             07  WBC-LINE-CR-DESC2   PIC S9.
005700         05  WBC-PAGE-LINES      PIC S999        COMP-3.
005800         05  WBC-MIN-TRANS       PIC S9          COMP-3.
005900         05  WBC-ZIP-SORT        PIC X.
006000         05  WBC-LOAN-LINES      PIC S9          COMP-3.
006100         05  WBC-RATE-LINES      PIC S9          COMP-3.
006200         05  WBC-MSG-LINES       PIC S9          COMP-3.
006300         05  WBC-DR-HDR-LINES    PIC S9          COMP-3.
006400         05  WBC-CR-HDR-LINES    PIC S9          COMP-3.
006500         05  WBC-ACH-LINES       PIC S9          COMP-3.
006600         05  WBC-CHECK-LINES     PIC S9          COMP-3.
006700         05  WBC-DEPOSIT-LINES   PIC S9          COMP-3.
006710         05  WBC-LOAN-BILL-FORMAT  PIC X.                         1002512
006720         05  WBC-CLSD-ACCT-STMT  PIC X.                           1002512
006800     03  WBC-SNAPSHOT-FORMAT     PIC S9  COMP-3.
006900     03  WBC-STMT-PRENOTES       PIC X.                           0903034
007000     03  WBC-FORCE-STMT-DAYS     PIC S999        COMP-3.
007100     03  WBC-STMT-FORCE      PIC S9(13)V99       COMP-3.          9915845
007200     03  WBC-SERIAL-FORMAT-INDICATORS.
007300         05  WBC-SERIAL-FORMAT       PIC X       OCCURS 6 TIMES.
007400     03  WBC-DAILY-BAL-INDICATORS.
007500         05  WBC-DAYBAL-FORMAT       PIC X   OCCURS 6 TIMES.
007600     03  WBC-SAVBAL-FORMAT-INDICATORS.
007700         05  WBC-SAVBAL-FORMAT       PIC X   OCCURS 6 TIMES.
007800     03  WBC-CITY-STATE-CONSTANT OCCURS 5 TIMES.
007900         05  WBC-CITY-STATE-CON  PIC X(30).
008000     03  WBC-GENERATED-TRANS OCCURS 36 TIMES                      9915845
008100             INDEXED BY WBC-GEN-INDEX.
008200         05  WBC-GEN-CODE        PIC XX.
008300         05  WBC-GEN-MNEUMONIC   PIC XX.
008400         05  WBC-GEN-ACCUM1      PIC S999 COMP-3.                 9715504
008500         05  WBC-GEN-ACCUM2      PIC S999 COMP-3.                 9715504
008600         05  WBC-GEN-EXC-OPT     PIC 9.
008700         05  WBC-GEN-OPT1        PIC 9.
008800         05  WBC-GEN-OPT2        PIC 9.
008900         05  WBC-GEN-OPT3        PIC 9.
009000         05  WBC-GEN-OPT4        PIC 9.
009100         05  WBC-GEN-OPT5        PIC 9.
009150     03  FILLER                           PIC X(56).              9915845
009200     03  WBC-VALIDATE-SC-TYPE-FILE        PIC X.                  9715504
009300     03  FILLER                           PIC X.                  9915845
009400     03  WBC-RATE-REGION.                                         9715504
009500         05  WBC-RT-REG-FLD1              PIC X(4).               9715504
009600         05  WBC-RT-REG-FLD2              PIC X(4).               9715504
009700         05  WBC-RT-REG-FLD3              PIC X(4).               9715504
009800     03  WBC-SVCH-REGION.                                         9715504
009900         05  WBC-SC-REG-FLD1              PIC X(4).               9715504
010000         05  WBC-SC-REG-FLD2              PIC X(4).               9715504
010100         05  WBC-SC-REG-FLD3              PIC X(4).               9715504
010200     03  WBC-EFT-STMT-ON-CMA              PIC X.                  0627533
010300     03  WBC-FDIC-CTL-OPT                 PIC X.                  0927850
010400     03  WBC-FDIC-CTL-DATE.                                       0927850
010500         05  WBC-FDIC-CTL-CENT            PIC XX.                 0927850
010600         05  WBC-FDIC-CTL-WO-CENT.                                0927850
010700             07  WBC-FDIC-CTL-YR          PIC XX.                 0927850
010800             07  WBC-FDIC-CTL-MO          PIC XX.                 0927850
010900             07  WBC-FDIC-CTL-DA          PIC XX.                 0927850
011000     03  WBC-FDIC-CR-CODE                 PIC X(4).               0927850
011100     03  WBC-FDIC-DR-CODE                 PIC X(4).               0927850
011200     03  WBC-FDIC-HOLD-PHASE              PIC X(4).               0927850
011300     03  WBC-FDIC-HOLD-TABLE.                                     0927850
011400         05  WBC-FDIC-HOLD-AMTS           OCCURS 10               0927850
011500             INDEXED BY WBC-FDIC-HOLD-IND.                        0927850
011600             07  WBC-FDIC-BAL-THRESHOLD   PIC S9(9)V99 COMP-3.    1010094
011700             07  WBC-FDIC-HOLD-PCT        PIC S9V9999 COMP-3.     0927850
011800             07  WBC-FDIC-HOLD-PCTR REDEFINES                     0927850
011900                 WBC-FDIC-HOLD-PCT        PIC S999V99 COMP-3.     0927850
011910     03  WBC-DATA-CENTER-FILLER           PIC X(300).             0930011
012000     03  FILLER                           PIC X(146).             1010094
