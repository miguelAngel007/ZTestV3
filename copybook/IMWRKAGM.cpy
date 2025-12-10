*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*-----------------------------------------------------------------
000200*    BALANCE HISTORY RECORD COPYBOOK
000300*          MMDA (SPLIT OR TIER) AGGREGATES
000400*-----------------------------------------------------------------
000500 01  WORK-BAL-HIST-MMDA-AGGR.
000600     03  WBHA-MMDA-LENGTH                PIC S9999       COMP.    100
000700     03  WBHA-MMDA-BIN0                  PIC XX.                  100
000800     03  WBHA-MMDA-KEY.
000900         05  WBHA-MMDA-CONTROLS.
001000             07  WBHA-MMDA-CTL1          PIC XX.                  100
001100             07  WBHA-MMDA-CTL2          PIC XXX.                 100
001200             07  WBHA-MMDA-CTL3          PIC XXX.                 100
001210             07  WBHA-MMDA-CTL4-ACCT.                             0417078
001300                 09  WBHA-MMDA-CTL4      PIC XXXX.                0417078
001400                 09  WBHA-MMDA-ACCT      PIC X(10).               0417078
001500         05  WBHA-MMDA-TYPE              PIC X.                   100
001600         05  WBHA-MMDA-SUB-TYPE          PIC X.                   100
001700     03  FILLER                          PIC X.                   100
001800     03  WBHA-MMDA-ENTRIES               PIC S999        COMP-3.  100
001900     03  WBHA-MMDA-AREA                  PIC X(6900).             9915845
002000     03  FILLER REDEFINES WBHA-MMDA-AREA.
002100         05  WBHA-SPLIT-TIER-AGGR-AREA OCCURS 60 TIMES.
002200             07  WBHA-S-T-CYCLE-TDY      PIC X.
002300             07  WBHA-S-T-RATE-USE       PIC X.
002400             07  WBHA-S-T-ACCRUAL-TYPE   PIC X.
002500             07  WBHA-S-T-EFF-DATE.
002600                 09  WBHA-S-T-EFF-CC     PIC XX.
002700                 09  WBHA-S-T-EFF-YY     PIC XX.
002800                 09  WBHA-S-T-EFF-MM     PIC XX.
002900                 09  WBHA-S-T-EFF-DD     PIC XX.
003000             07  FILLER                  PIC X(10).
003100             07  WBHA-S-T-PTR            PIC S999        COMP-3.  1003625
003200             07  WBHA-S-T-AGGR-DAYS      PIC S9(3)       COMP-3.
003300             07  WBHA-S-T-AMOUNTS.
003400                 09  WBHA-S-T-AMT1       PIC S9(15)V99   COMP-3.  9915845
003500                 09  WBHA-S-T-AMT2       PIC S9(15)V99   COMP-3.  9915845
003600                 09  WBHA-S-T-AMT3       PIC S9(15)V99   COMP-3.  9915845
003700                 09  WBHA-S-T-AMT4       PIC S9(15)V99   COMP-3.  9915845
003800                 09  WBHA-S-T-AMT5       PIC S9(15)V99   COMP-3.  9915845
003900                 09  WBHA-S-T-AMT6       PIC S9(15)V99   COMP-3.  9915845
004000                 09  WBHA-S-T-AMT7       PIC S9(15)V99   COMP-3.  9915845
004100                 09  WBHA-S-T-AMT8       PIC S9(15)V99   COMP-3.  9915845
004200                 09  WBHA-S-T-AMT9       PIC S9(15)V99   COMP-3.  9915845
004300             07  FILLER REDEFINES WBHA-S-T-AMOUNTS.
004400                 09  WBHA-S-T-AMT        PIC S9(15)V99   COMP-3   9915845
004500                           OCCURS 9 TIMES.
004550             07  WBHA-S-T-INEL-AMT       PIC S9(15)V99   COMP-3.  9915845
004600     03  FILLER REDEFINES WBHA-MMDA-AREA.
004700         05  WBHA-MMDA-OCC-1             PIC X(115).              9915845
004800         05  WBHA-MMDA-OCC-2-60          PIC X(6785).             9915845
004900     03  FILLER REDEFINES WBHA-MMDA-AREA.
005000         05  WBHA-MMDA-OCC-1-59          PIC X(6785).             9915845
005100         05  WBHA-MMDA-OCC-60            PIC X(115).              9915845
