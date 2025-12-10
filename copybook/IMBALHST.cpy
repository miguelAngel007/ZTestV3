*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*-----------------------------------------------------------------
000200*    BALANCE HISTORY RECORD COPYBOOK
000300*-----------------------------------------------------------------
000400 01  BALANCE-HISTORY-REC.
000500   02  BH-LENGTH                         PIC S9999       COMP.
000600   02  BH-BIN0                           PIC XX.
000700   02  BH-RECORD.
000800     03  BH-KEY.
000900         05  BH-CONTROLS.
001000             07  BH-CTL1                 PIC XX.
001100             07  BH-CTL2                 PIC XXX.
001200             07  BH-CTL3                 PIC XXX.
001210             07  BH-CTL4-ACCT.                                    0417078
001300                 09  BH-CTL4             PIC XXXX.                0417078
001400                 09  BH-ACCT             PIC X(10).               0417078
001500         05  BH-TYPE                     PIC X.
001600             88  BH-IOD                              VALUE 'I'.
001700             88  BH-MMDA                             VALUE 'M'.
001800             88  BH-SAV                              VALUE 'S'.
001900             88  BH-ODAC                             VALUE 'O'.
001910             88  BH-SAVT                             VALUE 'T'.   0316967
002000         05  BH-SUB-TYPE                 PIC X.
002100     03  FILLER                          PIC X.
002200     03  BH-ENTRIES                      PIC S999        COMP-3.
002300     03  BH-AREA                         PIC X(15300).            9915845
002400     03  FILLER REDEFINES BH-AREA.
002500         05  BHM-MMDA-AREA OCCURS 60 TIMES.
002600             07  BHM-MMDA-DATE.
002700                 09  BHM-CC              PIC XX.
002800                 09  BHM-YY              PIC XX.
002900                 09  BHM-MM              PIC XX.
003000                 09  BHM-DD              PIC XX.
003100             07  BHM-MMDA-PROD           PIC XXX.
003200             07  BHM-MMDA-BAL            PIC S9(13)V99   COMP-3.
003300             07  BHM-MMDA-CALC           PIC X.
003400             07  BHM-MMDA-CYCLE-TDY      PIC X.
003500             07  BHM-MMDA-TRNSFR-NO      PIC S9(7)       COMP-3.
003600             07  BHM-MMDA-CHK-ITEMS      PIC S9(7)       COMP-3.
003700             07  BHM-MMDA-ACCRUAL-TYPE   PIC X.
003800             07  BHM-MMDA-RATE-USE       PIC X.                   1003625
003900             07  BHM-MMDA-MMDA-INDICATOR PIC X.
004000             07  BHM-MMDA-INELIG-RATE    PIC SVP9(15)    COMP-3.  9915845
004100             07  BHM-MMDA-PTR            PIC S999        COMP-3.  1003625
004200             07  BHM-MMDA-ACCR-TDY       PIC S9(11)V9(6) COMP-3.
004300             07  BHM-MMDA-ACCR-CTD       PIC S9(11)V9(6) COMP-3.
004400             07  BHM-MMDA-ACCR-ADJ       PIC S9(11)V9(6) COMP-3.
004500             07  BHM-MMDA-LIMIT1         PIC S9(13)V99   COMP-3.  9915845
004600             07  BHM-MMDA-LIMIT2         PIC S9(13)V99   COMP-3.  9915845
004700             07  BHM-MMDA-LIMIT3         PIC S9(13)V99   COMP-3.  9915845
004800             07  BHM-MMDA-LIMIT4         PIC S9(13)V99   COMP-3.  9915845
004900             07  BHM-MMDA-LIMIT5         PIC S9(13)V99   COMP-3.  9915845
005000             07  BHM-MMDA-LIMIT6         PIC S9(13)V99   COMP-3.  9915845
005100             07  BHM-MMDA-LIMIT7         PIC S9(13)V99   COMP-3.  9915845
005200             07  BHM-MMDA-LIMIT8         PIC S9(13)V99   COMP-3.  9915845
005300             07  BHM-MMDA-DLY-RATE1      PIC SVP9(15)    COMP-3.  9915845
005400             07  BHM-MMDA-DLY-RATE2      PIC SVP9(15)    COMP-3.  9915845
005500             07  BHM-MMDA-DLY-RATE3      PIC SVP9(15)    COMP-3.  9915845
005600             07  BHM-MMDA-DLY-RATE4      PIC SVP9(15)    COMP-3.  9915845
005700             07  BHM-MMDA-DLY-RATE5      PIC SVP9(15)    COMP-3.  9915845
005800             07  BHM-MMDA-DLY-RATE6      PIC SVP9(15)    COMP-3.  9915845
005900             07  BHM-MMDA-DLY-RATE7      PIC SVP9(15)    COMP-3.  9915845
006000             07  BHM-MMDA-DLY-RATE8      PIC SVP9(15)    COMP-3.  9915845
006100             07  BHM-MMDA-DLY-RATE9      PIC SVP9(15)    COMP-3.  9915845
006110         05  FILLER                      PIC X(3000).             9915845
006200     03  FILLER REDEFINES BH-AREA.
006300         05  BHI-IOD-AREA OCCURS 60 TIMES.
006400             07  BHI-IOD-DATE.
006500                 09  BHI-CC              PIC XX.
006600                 09  BHI-YY              PIC XX.
006700                 09  BHI-MM              PIC XX.
006800                 09  BHI-DD              PIC XX.
006900             07  BHI-IOD-PROD            PIC XXX.
007000             07  BHI-IOD-BAL             PIC S9(13)V99   COMP-3.
007100             07  BHI-IOD-CALC            PIC X.
007200             07  BHI-IOD-CYCLE-TDY       PIC X.
007300             07  BHI-IOD-TRNSFR-NO       PIC S9(7)       COMP-3.
007400             07  BHI-IOD-CHK-ITEMS       PIC S9(7)       COMP-3.
007500             07  BHI-IOD-ACCRUAL-TYPE    PIC X.
007600             07  BHI-IOD-RATE-USE        PIC X.                   1003625
007700             07  BHI-IOD-MMDA-INDICATOR  PIC X.
007800             07  BHI-IOD-INELIG-RATE     PIC SVP9(15)    COMP-3.  9915845
007900             07  BHI-IOD-PTR             PIC S999        COMP-3.  1003625
008000             07  BHI-IOD-ACCR-TDY        PIC S9(11)V9(6) COMP-3.
008100             07  BHI-IOD-ACCR-CTD        PIC S9(11)V9(6) COMP-3.
008200             07  BHI-IOD-ACCR-ADJ        PIC S9(11)V9(6) COMP-3.
008300             07  BHI-IOD-DLY-RATE        PIC SVP9(15)    COMP-3.  9915845
008310         05  FILLER                      PIC X(10680).            9915845
008400
008500     03  FILLER REDEFINES BH-AREA.
008600         05  BHS-SAV-AREA OCCURS 60 TIMES.
008700             07  BHS-SAV-DATE.
008800                 09  BHS-CC              PIC XX.
008900                 09  BHS-YY              PIC XX.
009000                 09  BHS-MM              PIC XX.
009100                 09  BHS-DD              PIC XX.
009200             07  BHS-SAV-PROD            PIC XXX.
009300             07  BHS-SAV-BAL             PIC S9(13)V99   COMP-3.
009400             07  BHS-SAV-CALC-FLAG       PIC X.
009500             07  BHS-SAV-CMA-USE         PIC X.
009600             07  BHS-SAV-CYCLE-TDY       PIC X.
009700             07  BHS-SAV-INT-TRAN-NO     PIC S9(7)       COMP-3.
009800             07  BHS-SAV-INELIG-RATE     PIC SVP9(15)    COMP-3.  9915845
009900             07  BHS-SAV-PTR             PIC S999        COMP-3.  1003625
010000             07  BHS-SAV-ACCR-TDY        PIC S9(11)V9(6) COMP-3.
010100             07  BHS-SAV-ACCR-CTD        PIC S9(11)V9(6) COMP-3.
010200             07  BHS-SAV-ACCR-ADJ        PIC S9(11)V9(6) COMP-3.
010300             07  BHS-SAV-DLY-RATE        PIC SVP9(15)    COMP-3.  9915845
010310         05  FILLER                      PIC X(11040).            9915845
010320                                                                  1004524
010400     03  FILLER REDEFINES BH-AREA.
010500         05  BHO-ODAC-AREA OCCURS 60 TIMES.
010600             07  BHO-ODAC-DATE.
010700                 09  BHO-CC              PIC XX.
010800                 09  BHO-YY              PIC XX.
010900                 09  BHO-MM              PIC XX.
011000                 09  BHO-DD              PIC XX.
011100*****        07  BHO-ODAC-PROD           PIC XXX.                 9915845
011110             07  BHO-FILLER              PIC XX.                  9915845
011200             07  BHO-ODAC-CALC-FLAG      PIC X.
011210             07  BHO-ODAC-RATE-USE       PIC X.                   9915845
011300             07  BHO-ODAC-BAL            PIC S9(13)V99   COMP-3.
011400             07  BHO-ODAC-INT-PTR1       PIC S999        COMP-3.  1003625
011500             07  BHO-ODAC-INT-PTR2       PIC S999        COMP-3.  1003625
011600             07  BHO-ODAC-INT-PTR3       PIC S999        COMP-3.  1003625
011610             07  BHO-ODAC-INT-PTR4       PIC S999        COMP-3.  9915845
011620             07  BHO-ODAC-INT-PTR5       PIC S999        COMP-3.  9915845
011630             07  BHO-ODAC-INT-PTR6       PIC S999        COMP-3.  9915845
011640             07  BHO-ODAC-INT-PTR7       PIC S999        COMP-3.  9915845
011650             07  BHO-ODAC-INT-PTR8       PIC S999        COMP-3.  9915845
011660             07  BHO-ODAC-INT-PTR9       PIC S999        COMP-3.  9915845
011700             07  BHO-ODAC-ACCR-TDY       PIC S9(11)V9(6) COMP-3.
011800             07  BHO-ODAC-ACCR-CTD       PIC S9(11)V9(6) COMP-3.
011900             07  BHO-ODAC-ACCR-ADJ       PIC S9(11)V9(6) COMP-3.
012000             07  BHO-ODAC-LIMIT1         PIC S9(13)V99   COMP-3.  9915845
012100             07  BHO-ODAC-LIMIT2         PIC S9(13)V99   COMP-3.  9915845
012110             07  BHO-ODAC-LIMIT3         PIC S9(13)V99   COMP-3.  9915845
012120             07  BHO-ODAC-LIMIT4         PIC S9(13)V99   COMP-3.  9915845
012130             07  BHO-ODAC-LIMIT5         PIC S9(13)V99   COMP-3.  9915845
012140             07  BHO-ODAC-LIMIT6         PIC S9(13)V99   COMP-3.  9915845
012150             07  BHO-ODAC-LIMIT7         PIC S9(13)V99   COMP-3.  9915845
012160             07  BHO-ODAC-LIMIT8         PIC S9(13)V99   COMP-3.  9915845
012200             07  BHO-ODAC-RATE1          PIC SVP9(15)    COMP-3.  9915845
012300             07  BHO-ODAC-RATE2          PIC SVP9(15)    COMP-3.  9915845
012400             07  BHO-ODAC-RATE3          PIC SVP9(15)    COMP-3.  9915845
012410             07  BHO-ODAC-RATE4          PIC SVP9(15)    COMP-3.  9915845
012420             07  BHO-ODAC-RATE5          PIC SVP9(15)    COMP-3.  9915845
012430             07  BHO-ODAC-RATE6          PIC SVP9(15)    COMP-3.  9915845
012440             07  BHO-ODAC-RATE7          PIC SVP9(15)    COMP-3.  9915845
012450             07  BHO-ODAC-RATE8          PIC SVP9(15)    COMP-3.  9915845
012460             07  BHO-ODAC-RATE9          PIC SVP9(15)    COMP-3.  9915845
012500             07  BHO-ODAC-PRIME-ADJ1     PIC S9V9(8)     COMP-3.
012600             07  BHO-ODAC-PRIME-ADJ2     PIC S9V9(8)     COMP-3.
012700             07  BHO-ODAC-PRIME-ADJ3     PIC S9V9(8)     COMP-3.
012710             07  BHO-ODAC-PRIME-ADJ4     PIC S9V9(8)     COMP-3.  9915845
012720             07  BHO-ODAC-PRIME-ADJ5     PIC S9V9(8)     COMP-3.  9915845
012730             07  BHO-ODAC-PRIME-ADJ6     PIC S9V9(8)     COMP-3.  9915845
012740             07  BHO-ODAC-PRIME-ADJ7     PIC S9V9(8)     COMP-3.  9915845
012750             07  BHO-ODAC-PRIME-ADJ8     PIC S9V9(8)     COMP-3.  9915845
012760             07  BHO-ODAC-PRIME-ADJ9     PIC S9V9(8)     COMP-3.  9915845
012800             07  BHO-ODAC-PRIME-FLAG1    PIC X.
012900             07  BHO-ODAC-PRIME-FLAG2    PIC X.
013000             07  BHO-ODAC-PRIME-FLAG3    PIC X.
013010             07  BHO-ODAC-PRIME-FLAG4    PIC X.                   9915845
013020             07  BHO-ODAC-PRIME-FLAG5    PIC X.                   9915845
013030             07  BHO-ODAC-PRIME-FLAG6    PIC X.                   9915845
013040             07  BHO-ODAC-PRIME-FLAG7    PIC X.                   9915845
013050             07  BHO-ODAC-PRIME-FLAG8    PIC X.                   9915845
013060             07  BHO-ODAC-PRIME-FLAG9    PIC X.                   9915845
013100
013110     03  FILLER REDEFINES BH-AREA.                                0316967
013112         05  BHT-SAVT-AREA OCCURS 60 TIMES.                       0316967
013114             07  BHT-SAVT-DATE.                                   0316967
013116                 09  BHT-CC              PIC XX.                  0316967
013118                 09  BHT-YY              PIC XX.                  0316967
013120                 09  BHT-MM              PIC XX.                  0316967
013122                 09  BHT-DD              PIC XX.                  0316967
013124             07  BHT-SAVT-PROD           PIC XXX.                 0316967
013126             07  BHT-SAVT-BAL            PIC S9(13)V99   COMP-3.  0316967
013128             07  BHT-SAVT-CALC-FLAG      PIC X.                   0316967
013130             07  BHT-SAVT-CMA-USE        PIC X.                   0316967
013132             07  BHT-SAVT-CYCLE-TDY      PIC X.                   0316967
013134             07  BHT-SAVT-RATE-USE       PIC X.                   0316967
013136             07  BHT-SAVT-INT-TRAN-NO    PIC S9(7)       COMP-3.  0316967
013138             07  BHT-SAVT-PTR            PIC S999        COMP-3.  0316967
013140             07  BHT-SAVT-ACCR-TDY       PIC S9(11)V9(6) COMP-3.  0316967
013142             07  BHT-SAVT-ACCR-CTD       PIC S9(11)V9(6) COMP-3.  0316967
013144             07  BHT-SAVT-ACCR-ADJ       PIC S9(11)V9(6) COMP-3.  0316967
013146             07  BHT-SAVT-LIMIT1         PIC S9(13)V99   COMP-3.  0316967
013148             07  BHT-SAVT-LIMIT2         PIC S9(13)V99   COMP-3.  0316967
013150             07  BHT-SAVT-LIMIT3         PIC S9(13)V99   COMP-3.  0316967
013152             07  BHT-SAVT-LIMIT4         PIC S9(13)V99   COMP-3.  0316967
013154             07  BHT-SAVT-LIMIT5         PIC S9(13)V99   COMP-3.  0316967
013156             07  BHT-SAVT-LIMIT6         PIC S9(13)V99   COMP-3.  0316967
013158             07  BHT-SAVT-LIMIT7         PIC S9(13)V99   COMP-3.  0316967
013160             07  BHT-SAVT-LIMIT8         PIC S9(13)V99   COMP-3.  0316967
013162             07  BHT-SAVT-DLY-RATE1      PIC SVP9(15)    COMP-3.  0316967
013164             07  BHT-SAVT-DLY-RATE2      PIC SVP9(15)    COMP-3.  0316967
013166             07  BHT-SAVT-DLY-RATE3      PIC SVP9(15)    COMP-3.  0316967
013168             07  BHT-SAVT-DLY-RATE4      PIC SVP9(15)    COMP-3.  0316967
013170             07  BHT-SAVT-DLY-RATE5      PIC SVP9(15)    COMP-3.  0316967
013172             07  BHT-SAVT-DLY-RATE6      PIC SVP9(15)    COMP-3.  0316967
013174             07  BHT-SAVT-DLY-RATE7      PIC SVP9(15)    COMP-3.  0316967
013176             07  BHT-SAVT-DLY-RATE8      PIC SVP9(15)    COMP-3.  0316967
013178             07  BHT-SAVT-DLY-RATE9      PIC SVP9(15)    COMP-3.  0316967
013180         05  FILLER                      PIC X(3780).             0316967
013190                                                                  0316967
013200 01  BALANCE-HISTORY-AGGR-REC  REDEFINES BALANCE-HISTORY-REC.
013300     03  FILLER                          PIC X(31).
013400     03  BHA-AREA                        PIC X(15300).            9915845
013500     03  FILLER REDEFINES BHA-AREA.
013600         05  BHA-SPLIT-TIER-AGGR-AREA OCCURS 60 TIMES.
013700             07  BHA-S-T-CYCLE-TDY       PIC X.
013800             07  BHA-S-T-RATE-USE        PIC X.
013900             07  BHA-S-T-ACCRUAL-TYPE    PIC X.
014000             07  BHA-S-T-EFF-DATE.
014100                 09  BHA-S-T-EFF-CC      PIC XX.
014200                 09  BHA-S-T-EFF-YY      PIC XX.
014300                 09  BHA-S-T-EFF-MM      PIC XX.
014400                 09  BHA-S-T-EFF-DD      PIC XX.
014500             07  FILLER                  PIC X(10).
014600             07  BHA-S-T-PTR             PIC S999        COMP-3.  1003625
014700             07  BHA-AGGR-DAYS           PIC S9(3)       COMP-3.
014800             07  BHA-S-T-AMT1            PIC S9(15)V99   COMP-3.  9915845
014900             07  BHA-S-T-AMT2            PIC S9(15)V99   COMP-3.  9915845
015000             07  BHA-S-T-AMT3            PIC S9(15)V99   COMP-3.  9915845
015100             07  BHA-S-T-AMT4            PIC S9(15)V99   COMP-3.  9915845
015200             07  BHA-S-T-AMT5            PIC S9(15)V99   COMP-3.  9915845
015300             07  BHA-S-T-AMT6            PIC S9(15)V99   COMP-3.  9915845
015400             07  BHA-S-T-AMT7            PIC S9(15)V99   COMP-3.  9915845
015500             07  BHA-S-T-AMT8            PIC S9(15)V99   COMP-3.  9915845
015600             07  BHA-S-T-AMT9            PIC S9(15)V99   COMP-3.  9915845
015650             07  BHA-S-T-INEL-AMT        PIC S9(15)V99   COMP-3.  9915845
015660         05  FILLER                      PIC X(8400).             9915845
015700
015800     03  FILLER REDEFINES BHA-AREA.
015900         05  BHA-DAILY-RATE-AGGR-AREA OCCURS 60 TIMES.
016000             07  BHA-DLY-CYCLE-TDY       PIC X.
016100             07  BHA-DLY-RATE-USE        PIC X.
016200             07  BHA-DLY-ACCRUAL-TYPE    PIC X.
016300             07  BHA-DLY-EFF-DATE.
016400                 09  BHA-DLY-EFF-CC      PIC XX.
016500                 09  BHA-DLY-EFF-YY      PIC XX.
016600                 09  BHA-DLY-EFF-MM      PIC XX.
016700                 09  BHA-DLY-EFF-DD      PIC XX.
016800             07  FILLER                  PIC X(10).
016900             07  BHA-DLY-PTR             PIC S999        COMP-3.  1003625
017000             07  BHA-DLY-DAYS            PIC S9(3)       COMP-3.
017100             07  BHA-DLY-AMT1            PIC S9(15)V99   COMP-3.  9915845
017150             07  BHA-DLY-INEL-AMT1       PIC S9(15)V99   COMP-3.  9915845
017160         05  FILLER                      PIC X(12720).            9915845
017200
017300     03  FILLER REDEFINES BHA-AREA.
017400         05  BHA-OD-AGGR-AREA OCCURS 60 TIMES.
017500             07  BHA-OD-CYCLE-TDY        PIC X.
017510             07  BHA-OD-RATE-USE         PIC X.                   9915845
017600             07  FILLER                  PIC XX.
017700             07  BHA-OD-EFF-DATE.
017800                 09  BHA-OD-EFF-CC       PIC XX.
017900                 09  BHA-OD-EFF-YY       PIC XX.
018000                 09  BHA-OD-EFF-MM       PIC XX.
018100                 09  BHA-OD-EFF-DD       PIC XX.
018200             07  FILLER                  PIC X(10).
018300             07  BHA-OD-INT-PTR1         PIC S999        COMP-3.  1003625
018400             07  BHA-OD-INT-PTR2         PIC S999        COMP-3.  1003625
018500             07  BHA-OD-INT-PTR3         PIC S999        COMP-3.  1003625
018510             07  BHA-OD-INT-PTR4         PIC S999        COMP-3.  9915845
018520             07  BHA-OD-INT-PTR5         PIC S999        COMP-3.  9915845
018530             07  BHA-OD-INT-PTR6         PIC S999        COMP-3.  9915845
018540             07  BHA-OD-INT-PTR7         PIC S999        COMP-3.  9915845
018550             07  BHA-OD-INT-PTR8         PIC S999        COMP-3.  9915845
018560             07  BHA-OD-INT-PTR9         PIC S999        COMP-3.  9915845
018600             07  BHA-OD-DAYS             PIC S9(3)       COMP-3.
018700             07  BHA-OD-AMT1             PIC S9(15)V99   COMP-3.  9915845
018800             07  BHA-OD-AMT2             PIC S9(15)V99   COMP-3.  9915845
018900             07  BHA-OD-AMT3             PIC S9(15)V99   COMP-3.  9915845
018910             07  BHA-OD-AMT4             PIC S9(15)V99   COMP-3.  9915845
018920             07  BHA-OD-AMT5             PIC S9(15)V99   COMP-3.  9915845
018930             07  BHA-OD-AMT6             PIC S9(15)V99   COMP-3.  9915845
018940             07  BHA-OD-AMT7             PIC S9(15)V99   COMP-3.  9915845
018950             07  BHA-OD-AMT8             PIC S9(15)V99   COMP-3.  9915845
018960             07  BHA-OD-AMT9             PIC S9(15)V99   COMP-3.  9915845
019000             07  BHA-OD-LIMIT1           PIC S9(13)V99   COMP-3.  9915845
019100             07  BHA-OD-LIMIT2           PIC S9(13)V99   COMP-3.  9915845
019110             07  BHA-OD-LIMIT3           PIC S9(13)V99   COMP-3.  9915845
019120             07  BHA-OD-LIMIT4           PIC S9(13)V99   COMP-3.  9915845
019130             07  BHA-OD-LIMIT5           PIC S9(13)V99   COMP-3.  9915845
019140             07  BHA-OD-LIMIT6           PIC S9(13)V99   COMP-3.  9915845
019150             07  BHA-OD-LIMIT7           PIC S9(13)V99   COMP-3.  9915845
019160             07  BHA-OD-LIMIT8           PIC S9(13)V99   COMP-3.  9915845
019200             07  BHA-OD-PRIME-ADJ1       PIC S9V9(8)     COMP-3.
019300             07  BHA-OD-PRIME-ADJ2       PIC S9V9(8)     COMP-3.
019400             07  BHA-OD-PRIME-ADJ3       PIC S9V9(8)     COMP-3.
019410             07  BHA-OD-PRIME-ADJ4       PIC S9V9(8)     COMP-3.  9915845
019420             07  BHA-OD-PRIME-ADJ5       PIC S9V9(8)     COMP-3.  9915845
019430             07  BHA-OD-PRIME-ADJ6       PIC S9V9(8)     COMP-3.  9915845
019440             07  BHA-OD-PRIME-ADJ7       PIC S9V9(8)     COMP-3.  9915845
019450             07  BHA-OD-PRIME-ADJ8       PIC S9V9(8)     COMP-3.  9915845
019460             07  BHA-OD-PRIME-ADJ9       PIC S9V9(8)     COMP-3.  9915845
019500             07  BHA-OD-PRIME-FLAG1      PIC X.
019600             07  BHA-OD-PRIME-FLAG2      PIC X.
019700             07  BHA-OD-PRIME-FLAG3      PIC X.
019710             07  BHA-OD-PRIME-FLAG4      PIC X.                   9915845
019720             07  BHA-OD-PRIME-FLAG5      PIC X.                   9915845
019730             07  BHA-OD-PRIME-FLAG6      PIC X.                   9915845
019740             07  BHA-OD-PRIME-FLAG7      PIC X.                   9915845
019750             07  BHA-OD-PRIME-FLAG8      PIC X.                   9915845
019760             07  BHA-OD-PRIME-FLAG9      PIC X.                   9915845
019790         05  FILLER                      PIC X(840).              9915845
019800
019900 01  BALANCE-HISTORY-HEADER REDEFINES BALANCE-HISTORY-REC.
020000     03  FILLER                          PIC X(4).
020100     03  BHH-KEY.
020200         05  BHH-CTL1-3                  PIC X(8).
020300         05  FILLER                      PIC X(14).
020400         05  BHH-RECORD-ID               PIC X.
020500     03  BHH-FILE-SEQ                    PIC S9(5)       COMP-3.
020600     03  BHH-FILLER                      PIC X.                   1004524
020700     03  FILLER                          PIC X(15269).            9915845
020800                                                                  9915845
040000 01  BAL-HIST-LENGTHS.                                            9915845
040100**** MOVE +15331 TO BHL-MAX-RECORD IN ANY PROGRAM USING BALANCE   9915845
040200**** HISTORY EXPAND OR COMPRESS ROUTINES.                         9915845
040300**** BHL-MAX-RECORD REFLECTS THE FIXED PORTION PLUS THE VARIABLE  9915845
040400****                OCCURRENCES OF DATA PLUS THE RECORD LENGTH    9915845
040500**** BHL-FIXED REFLECTS THE FIXED AREA WITHOUT THE RECORD LENGTH  9915845
040600     03  BHL-MAX-RECORD          PIC S9999 COMP SYNC VALUE +0000. 9915845
040700     03  BHL-FIXED               PIC S9999 COMP SYNC VALUE +0027. 9915845
040800                                                                  9915845
