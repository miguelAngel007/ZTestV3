*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100******************************************************************
000200*  BALANCE HISTORY WORK COPYBOOK                                 *
000300*       SAVINGS TIERED HISTORY RECORD                            *
000400******************************************************************
000500 01  WORK-BAL-HIST-SAV-TIER.
000600   02  WBHT-LENGTH                   PIC S9999       COMP.
000700   02  WBHT-BIN0                     PIC XX.
000800   02  WBHT-RECORD.
000900     03  WBHT-KEY.
001000         05  WBHT-CONTROLS.
001100             07  WBHT-CTL1           PIC XX.
001200             07  WBHT-CTL2           PIC XXX.
001300             07  WBHT-CTL3           PIC XXX.
001310             07  WBHT-CTL4-ACCT.                                  0417078
001400                 09  WBHT-CTL4       PIC XXXX.                    0417078
001500                 09  WBHT-ACCT       PIC X(10).                   0417078
001600         05  WBHT-TYPE               PIC X.
001700         05  WBHT-SUB-TYPE           PIC X.
001800     03  FILLER                      PIC X.
001900     03  WBHT-ENTRIES                PIC S999        COMP-3.
002000     03  WBHT-AREA                   PIC X(11520).
002100     03  FILLER REDEFINES WBHT-AREA.
002200         05  WBHT-SAVT-AREA OCCURS 60 TIMES.
002300             07  WBHT-SAVT-DATE.
002400                 09  WBHT-CC         PIC XX.
002500                 09  WBHT-YY         PIC XX.
002600                 09  WBHT-MM         PIC XX.
002700                 09  WBHT-DD         PIC XX.
002800             07  WBHT-SAVT-PROD      PIC XXX.
002900             07  WBHT-SAVT-BAL       PIC S9(13)V99   COMP-3.
003000             07  WBHT-SAVT-CALC-FLAG PIC X.
003100             07  WBHT-SAVT-CMA-USE   PIC X.
003200             07  WBHT-SAVT-CYCLE-TDY PIC X.
003300             07  WBHT-SAVT-RATE-USE  PIC X.
003400             07  WBHT-SAVT-INT-TRAN-NO PIC S9(7)     COMP-3.
003500             07  WBHT-SAVT-PTR       PIC S999        COMP-3.
003600             07  WBHT-SAVT-ACCR-TDY  PIC S9(11)V9(6) COMP-3.
003700             07  WBHT-SAVT-ACCR-CTD  PIC S9(11)V9(6) COMP-3.
003800             07  WBHT-SAVT-ACCR-ADJ  PIC S9(11)V9(6) COMP-3.
003900             07  WBHT-SAVT-LIMITS.
004000                 09  WBHT-SAVT-LIMIT1 PIC S9(13)V99   COMP-3.
004100                 09  WBHT-SAVT-LIMIT2 PIC S9(13)V99   COMP-3.
004200                 09  WBHT-SAVT-LIMIT3 PIC S9(13)V99   COMP-3.
004300                 09  WBHT-SAVT-LIMIT4 PIC S9(13)V99   COMP-3.
004400                 09  WBHT-SAVT-LIMIT5 PIC S9(13)V99   COMP-3.
004500                 09  WBHT-SAVT-LIMIT6 PIC S9(13)V99   COMP-3.
004600                 09  WBHT-SAVT-LIMIT7 PIC S9(13)V99   COMP-3.
004700                 09  WBHT-SAVT-LIMIT8 PIC S9(13)V99   COMP-3.
004800             07  FILLER REDEFINES WBHT-SAVT-LIMITS.
004900                 09 WBHT-SAVT-LIMIT   PIC S9(13)V99   COMP-3
005000                                OCCURS 8 TIMES.
005100             07  WBHT-SAVT-DLY-RATES.
005200                 09  WBHT-SAVT-DLY-RATE1 PIC SVP9(15)  COMP-3.
005300                 09  WBHT-SAVT-DLY-RATE2 PIC SVP9(15)  COMP-3.
005400                 09  WBHT-SAVT-DLY-RATE3 PIC SVP9(15)  COMP-3.
005500                 09  WBHT-SAVT-DLY-RATE4 PIC SVP9(15)  COMP-3.
005600                 09  WBHT-SAVT-DLY-RATE5 PIC SVP9(15)  COMP-3.
005700                 09  WBHT-SAVT-DLY-RATE6 PIC SVP9(15)  COMP-3.
005800                 09  WBHT-SAVT-DLY-RATE7 PIC SVP9(15)  COMP-3.
005900                 09  WBHT-SAVT-DLY-RATE8 PIC SVP9(15)  COMP-3.
006000                 09  WBHT-SAVT-DLY-RATE9 PIC SVP9(15)  COMP-3.
006100             07  FILLER REDEFINES WBHT-SAVT-DLY-RATES.
006200                 09 WBHT-SAVT-DLY-RATE   PIC SVP9(15)  COMP-3
006300                                OCCURS 9 TIMES.
006400     03  FILLER REDEFINES WBHT-AREA.
006500         05  WBHT-SAVT-OCC-1          PIC X(192).
006600         05  WBHT-SAVT-OCC-2-60       PIC X(11328).
006700     03  FILLER REDEFINES WBHT-AREA.
006800         05  WBHT-SAVT-OCC-1-59       PIC X(11328).
006900         05  WBHT-SAVT-OCC-60         PIC X(192).
