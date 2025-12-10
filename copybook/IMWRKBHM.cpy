*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100******************************************************************
000200*  BALANCE HISTORY WORK COPYBOOK                                 *
000300*       MMDA HISTORY RECORD                                      *
000400******************************************************************
000500 01  WORK-BAL-HIST-MMDA.
000600   02  WBHM-LENGTH                   PIC S9999       COMP.
000700   02  WBHM-BIN0                     PIC XX.
000800   02  WBHM-RECORD.
000900     03  WBHM-KEY.
001000         05  WBHM-CONTROLS.
001100             07  WBHM-CTL1           PIC XX.
001200             07  WBHM-CTL2           PIC XXX.
001300             07  WBHM-CTL3           PIC XXX.
001310             07  WBHM-CTL4-ACCT.                                  0417078
001400                 09  WBHM-CTL4       PIC XXXX.                    0417078
001500                 09  WBHM-ACCT       PIC X(10).                   0417078
001600         05  WBHM-TYPE               PIC X.
001700         05  WBHM-SUB-TYPE           PIC X.
001800     03  FILLER                      PIC X.
001900     03  WBHM-ENTRIES                PIC S999        COMP-3.
002000     03  WBHM-AREA                   PIC X(12300).                9915845
002100     03  FILLER REDEFINES WBHM-AREA.
002200         05  WBHM-MMDA-AREA OCCURS 60 TIMES.
002300             07  WBHM-MMDA-DATE.
002400                 09  WBHM-CC         PIC XX.
002500                 09  WBHM-YY         PIC XX.
002600                 09  WBHM-MM         PIC XX.
002700                 09  WBHM-DD         PIC XX.
002800             07  WBHM-MMDA-PROD      PIC XXX.
002900             07  WBHM-MMDA-BAL       PIC S9(13)V99   COMP-3.
003000             07  WBHM-MMDA-CALC      PIC X.
003100             07  WBHM-MMDA-CYCLE-TDY PIC X.
003200             07  WBHM-MMDA-TRNSFR-NO PIC S9(7)       COMP-3.
003300             07  WBHM-MMDA-CHK-ITEMS PIC S9(7)       COMP-3.
003400             07  WBHM-MMDA-ACCRUAL-TYPE
003500                                     PIC X.
003600             07  WBHM-MMDA-RATE-USE  PIC X.                       1003625
003800             07  WBHM-MMDA-MMDA-INDICATOR
003900                                     PIC X.
004000             07  WBHM-MMDA-INELIG-RATE
004100                                     PIC SVP9(15)    COMP-3.      9915845
004200             07  WBHM-MMDA-PTR       PIC S999        COMP-3.      1003625
004300             07  WBHM-MMDA-ACCR-TDY  PIC S9(11)V9(6) COMP-3.
004400             07  WBHM-MMDA-ACCR-CTD  PIC S9(11)V9(6) COMP-3.
004500             07  WBHM-MMDA-ACCR-ADJ  PIC S9(11)V9(6) COMP-3.
004600             07  WBHM-MMDA-LIMITS.
004700                 09  WBHM-MMDA-LIMIT1 PIC S9(13)V99   COMP-3.     9915845
004800                 09  WBHM-MMDA-LIMIT2 PIC S9(13)V99   COMP-3.     9915845
004900                 09  WBHM-MMDA-LIMIT3 PIC S9(13)V99   COMP-3.     9915845
005000                 09  WBHM-MMDA-LIMIT4 PIC S9(13)V99   COMP-3.     9915845
005100                 09  WBHM-MMDA-LIMIT5 PIC S9(13)V99   COMP-3.     9915845
005200                 09  WBHM-MMDA-LIMIT6 PIC S9(13)V99   COMP-3.     9915845
005300                 09  WBHM-MMDA-LIMIT7 PIC S9(13)V99   COMP-3.     9915845
005400                 09  WBHM-MMDA-LIMIT8 PIC S9(13)V99   COMP-3.     9915845
005500             07  FILLER REDEFINES WBHM-MMDA-LIMITS.
005600                 09 WBHM-MMDA-LIMIT  PIC S9(13)V99   COMP-3       9915845
005700                                OCCURS 8 TIMES.
005800             07  WBHM-MMDA-DLY-RATES.
005900                 09  WBHM-MMDA-DLY-RATE1 PIC SVP9(15)  COMP-3.    9915845
006000                 09  WBHM-MMDA-DLY-RATE2 PIC SVP9(15)  COMP-3.    9915845
006100                 09  WBHM-MMDA-DLY-RATE3 PIC SVP9(15)  COMP-3.    9915845
006200                 09  WBHM-MMDA-DLY-RATE4 PIC SVP9(15)  COMP-3.    9915845
006300                 09  WBHM-MMDA-DLY-RATE5 PIC SVP9(15)  COMP-3.    9915845
006400                 09  WBHM-MMDA-DLY-RATE6 PIC SVP9(15)  COMP-3.    9915845
006500                 09  WBHM-MMDA-DLY-RATE7 PIC SVP9(15)  COMP-3.    9915845
006600                 09  WBHM-MMDA-DLY-RATE8 PIC SVP9(15)  COMP-3.    9915845
006700                 09  WBHM-MMDA-DLY-RATE9 PIC SVP9(15)  COMP-3.    9915845
006800             07  FILLER REDEFINES WBHM-MMDA-DLY-RATES.
006900                 09 WBHM-MMDA-DLY-RATE   PIC SVP9(15)  COMP-3     9915845
007000                                OCCURS 9 TIMES.
007100     03  FILLER REDEFINES WBHM-AREA.
007200         05  WBHM-MMDA-OCC-1          PIC X(205).                 9915845
007300         05  WBHM-MMDA-OCC-2-60       PIC X(12095).               9915845
007400     03  FILLER REDEFINES WBHM-AREA.
007500         05  WBHM-MMDA-OCC-1-59       PIC X(12095).               9915845
007600         05  WBHM-MMDA-OCC-60         PIC X(205).                 9915845
