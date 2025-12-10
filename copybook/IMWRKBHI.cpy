*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100******************************************************************
000200*  BALANCE HISTORY WORK COPYBOOK                                 *
000300*        IOD HISTORY RECORD                                      *
000400******************************************************************
000500 01  WORK-BAL-HIST-IOD.
000600   02  WBHI-LENGTH                   PIC S9999       COMP.
000700   02  WBHI-BIN0                     PIC XX.
000800   02  WBHI-RECORD.
000900     03  WBHI-KEY.
001000         05  WBHI-CONTROLS.
001100             07  WBHI-CTL1           PIC XX.
001200             07  WBHI-CTL2           PIC XXX.
001300             07  WBHI-CTL3           PIC XXX.
001310             07  WBHI-CTL4-ACCT.                                  0417078
001400                 09  WBHI-CTL4       PIC XXXX.                    0417078
001500                 09  WBHI-ACCT       PIC X(10).                   0417078
001600         05  WBHI-TYPE               PIC X.
001700         05  WBHI-SUB-TYPE           PIC X.
001800     03  FILLER                      PIC X.
001900     03  WBHI-ENTRIES                PIC S999        COMP-3.
002000     03  WBHI-AREA                   PIC X(4620).                 9915845
002100     03  FILLER REDEFINES WBHI-AREA.
002200         05  WBHI-IOD-AREA OCCURS 60 TIMES.
002300             07  WBHI-IOD-DATE.
002400                 09  WBHI-CC         PIC XX.
002500                 09  WBHI-YY         PIC XX.
002600                 09  WBHI-MM         PIC XX.
002700                 09  WBHI-DD         PIC XX.
002800             07  WBHI-IOD-PROD       PIC XXX.
002900             07  WBHI-IOD-BAL        PIC S9(13)V99   COMP-3.
003000             07  WBHI-IOD-CALC       PIC X.
003100             07  WBHI-IOD-CYCLE-TDY  PIC X.
003200             07  WBHI-IOD-TRNSFR-NO  PIC S9(7)       COMP-3.
003300             07  WBHI-IOD-CHK-ITEMS  PIC S9(7)       COMP-3.
003400             07  WBHI-IOD-ACCRUAL-TYPE
003500                                     PIC X.
003600             07  WBHI-IOD-RATE-USE   PIC X.                       1003625
003800             07  WBHI-IOD-MMDA-INDICATOR
003900                                     PIC X.
004000             07  WBHI-IOD-INELIG-RATE
004100                                     PIC SVP9(15)    COMP-3.      9915845
004200             07  WBHI-IOD-PTR        PIC S999        COMP-3.      1003625
004300             07  WBHI-IOD-ACCR-TDY   PIC S9(11)V9(6) COMP-3.
004400             07  WBHI-IOD-ACCR-CTD   PIC S9(11)V9(6) COMP-3.
004500             07  WBHI-IOD-ACCR-ADJ   PIC S9(11)V9(6) COMP-3.
004600             07  WBHI-IOD-DLY-RATE   PIC SVP9(15)    COMP-3.      9915845
004700     03  FILLER REDEFINES WBHI-AREA.
004800         05  WBHI-IOD-OCC-1          PIC X(77).                   9915845
004900         05  WBHI-IOD-OCC-2-60       PIC X(4543).                 9915845
005000     03  FILLER REDEFINES WBHI-AREA.
005100         05  WBHI-IOD-OCC-1-59       PIC X(4543).                 9915845
005200         05  WBHI-IOD-OCC-60         PIC X(77).                   9915845
