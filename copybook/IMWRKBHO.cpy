*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*                                                                 9915845
000200*----------------------------------------------------------------*9915845
000300*           BALANCE HISTORY WORK RECORD -- O/D ACCRUAL           *9915845
000400*----------------------------------------------------------------*9915845
000500*                                                                 9915845
001000 01  WORK-BAL-HIST-ODAC.
001100   02  WBHO-LENGTH                   PIC S9999       COMP.
001200   02  WBHO-BIN0                     PIC XX.
001300   02  WBHO-RECORD.
001400     03  WBHO-KEY.
001500         05  WBHO-CONTROLS.
001600             07  WBHO-CTL1           PIC XX.
001700             07  WBHO-CTL2           PIC XXX.
001800             07  WBHO-CTL3           PIC XXX.
001810             07  WBHO-CTL4-ACCT.                                  0417078
001900                 09  WBHO-CTL4       PIC XXXX.                    0417078
002000                 09  WBHO-ACCT       PIC X(10).                   0417078
002100         05  WBHO-TYPE               PIC X.
002200     03  FILLER                      PIC XX.
002300     03  WBHO-ENTRIES                PIC S999        COMP-3.
002400     03  WBHO-AREA                   PIC X(15300).                9915845
002500     03  FILLER REDEFINES WBHO-AREA.
002600         05  WBHO-ODAC-AREA OCCURS 60 TIMES.
002700             07  WBHO-ODAC-DATE.
002800                 09  WBHO-CC         PIC XX.
002900                 09  WBHO-YY         PIC XX.
003000                 09  WBHO-MM         PIC XX.
003100                 09  WBHO-DD         PIC XX.
003200*****        07  WBHO-ODAC-PROD      PIC XXX.                     9915845
003210             07  WBHO-FILLER         PIC XX.                      9915845
003300             07  WBHO-ODAC-CALC-FLAG PIC X.
003310             07  WBHO-ODAC-RATE-USE  PIC X.                       9915845
003400             07  WBHO-ODAC-BAL       PIC S9(13)V99   COMP-3.
003500             07  WBHO-ODAC-INT-PTR1  PIC S999        COMP-3.      1003625
003600             07  WBHO-ODAC-INT-PTR2  PIC S999        COMP-3.      1003625
003700             07  WBHO-ODAC-INT-PTR3  PIC S999        COMP-3.      1003625
003710             07  WBHO-ODAC-INT-PTR4  PIC S999        COMP-3.      9915845
003720             07  WBHO-ODAC-INT-PTR5  PIC S999        COMP-3.      9915845
003730             07  WBHO-ODAC-INT-PTR6  PIC S999        COMP-3.      9915845
003740             07  WBHO-ODAC-INT-PTR7  PIC S999        COMP-3.      9915845
003750             07  WBHO-ODAC-INT-PTR8  PIC S999        COMP-3.      9915845
003760             07  WBHO-ODAC-INT-PTR9  PIC S999        COMP-3.      9915845
003800             07  WBHO-ODAC-ACCR-TDY  PIC S9(11)V9(6) COMP-3.
003900             07  WBHO-ODAC-ACCR-CTD  PIC S9(11)V9(6) COMP-3.
004000             07  WBHO-ODAC-ACCR-ADJ  PIC S9(11)V9(6) COMP-3.
004100             07  WBHO-ODAC-LIMIT1    PIC S9(13)V99   COMP-3.      9915845
004200             07  WBHO-ODAC-LIMIT2    PIC S9(13)V99   COMP-3.      9915845
004210             07  WBHO-ODAC-LIMIT3    PIC S9(13)V99   COMP-3.      9915845
004220             07  WBHO-ODAC-LIMIT4    PIC S9(13)V99   COMP-3.      9915845
004230             07  WBHO-ODAC-LIMIT5    PIC S9(13)V99   COMP-3.      9915845
004240             07  WBHO-ODAC-LIMIT6    PIC S9(13)V99   COMP-3.      9915845
004250             07  WBHO-ODAC-LIMIT7    PIC S9(13)V99   COMP-3.      9915845
004260             07  WBHO-ODAC-LIMIT8    PIC S9(13)V99   COMP-3.      9915845
004300             07  WBHO-ODAC-RATE1     PIC SVP9(15)    COMP-3.      9915845
004400             07  WBHO-ODAC-RATE2     PIC SVP9(15)    COMP-3.      9915845
004500             07  WBHO-ODAC-RATE3     PIC SVP9(15)    COMP-3.      9915845
004510             07  WBHO-ODAC-RATE4     PIC SVP9(15)    COMP-3.      9915845
004520             07  WBHO-ODAC-RATE5     PIC SVP9(15)    COMP-3.      9915845
004530             07  WBHO-ODAC-RATE6     PIC SVP9(15)    COMP-3.      9915845
004540             07  WBHO-ODAC-RATE7     PIC SVP9(15)    COMP-3.      9915845
004550             07  WBHO-ODAC-RATE8     PIC SVP9(15)    COMP-3.      9915845
004560             07  WBHO-ODAC-RATE9     PIC SVP9(15)    COMP-3.      9915845
004600             07  WBHO-ODAC-PRIME-ADJ1    PIC S9V9(8) COMP-3.
004700             07  WBHO-ODAC-PRIME-ADJ2    PIC S9V9(8) COMP-3.
004800             07  WBHO-ODAC-PRIME-ADJ3    PIC S9V9(8) COMP-3.
004810             07  WBHO-ODAC-PRIME-ADJ4    PIC S9V9(8) COMP-3.      9915845
004820             07  WBHO-ODAC-PRIME-ADJ5    PIC S9V9(8) COMP-3.      9915845
004830             07  WBHO-ODAC-PRIME-ADJ6    PIC S9V9(8) COMP-3.      9915845
004840             07  WBHO-ODAC-PRIME-ADJ7    PIC S9V9(8) COMP-3.      9915845
004850             07  WBHO-ODAC-PRIME-ADJ8    PIC S9V9(8) COMP-3.      9915845
004860             07  WBHO-ODAC-PRIME-ADJ9    PIC S9V9(8) COMP-3.      9915845
004900             07  WBHO-ODAC-PRIME-FLAG1   PIC X.
005000             07  WBHO-ODAC-PRIME-FLAG2   PIC X.
005100             07  WBHO-ODAC-PRIME-FLAG3   PIC X.
005110             07  WBHO-ODAC-PRIME-FLAG4   PIC X.                   9915845
005120             07  WBHO-ODAC-PRIME-FLAG5   PIC X.                   9915845
005130             07  WBHO-ODAC-PRIME-FLAG6   PIC X.                   9915845
005140             07  WBHO-ODAC-PRIME-FLAG7   PIC X.                   9915845
005150             07  WBHO-ODAC-PRIME-FLAG8   PIC X.                   9915845
005160             07  WBHO-ODAC-PRIME-FLAG9   PIC X.                   9915845
005200     03  FILLER REDEFINES WBHO-AREA.
005300         05  WBHO-ODAC-OCC-1         PIC X(255).                  9915845
005400         05  WBHO-ODAC-OCC-2-60      PIC X(15045).                9915845
005500     03  FILLER REDEFINES WBHO-AREA.
005600         05  WBHO-ODAC-OCC-1-59      PIC X(15045).                9915845
005700         05  WBHO-ODAC-OCC-60        PIC X(255).                  9915845
