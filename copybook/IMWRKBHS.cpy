*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100******************************************************************
000200*        THIS COPY USED IN THE FOLLOWING PROGRAMS                *
000300*                                                                *
000400*   IM31      IM31CACR  IM31TACR  IM78                           *
000500*   IM41N0    IM78B1                                             *
000600*                                                                *
000700*    PLEASE ADD NEW PROGRAMS TO THE ABOVE LIST                   *
000800*                                                                *
000900******************************************************************
001000 01  WORK-BAL-HIST-SAV.
001100   02  WBHS-LENGTH                   PIC S9999       COMP.
001200   02  WBHS-BIN0                     PIC XX.
001300   02  WBHS-RECORD.
001400     03  WBHS-KEY.
001500         05  WBHS-CONTROLS.
001600             07  WBHS-CTL1           PIC XX.
001700             07  WBHS-CTL2           PIC XXX.
001800             07  WBHS-CTL3           PIC XXX.
001810             07  WBHS-CTL4-ACCT.                                  0417078
001900                 09  WBHS-CTL4       PIC XXXX.                    0417078
002000                 09  WBHS-ACCT       PIC X(10).                   0417078
002100         05  WBHS-TYPE               PIC X.
002200     03  FILLER                      PIC XX.
002300     03  WBHS-ENTRIES                PIC S999        COMP-3.
002400     03  WBHS-AREA                   PIC X(4260).                 9915845
002500     03  FILLER REDEFINES WBHS-AREA.
002600         05  WBHS-SAV-AREA OCCURS 60 TIMES.
002700             07  WBHS-SAV-DATE.
002800                 09  WBHS-CC         PIC XX.
002900                 09  WBHS-YY         PIC XX.
003000                 09  WBHS-MM         PIC XX.
003100                 09  WBHS-DD         PIC XX.
003200             07  WBHS-SAV-PROD       PIC XXX.
003300             07  WBHS-SAV-BAL        PIC S9(13)V99   COMP-3.
003400             07  WBHS-SAV-CALC-FLAG  PIC X.
003500             07  WBHS-SAV-CMA-USE    PIC X.
003600             07  WBHS-SAV-CYCLE-TDY  PIC X.
003700             07  WBHS-SAV-INT-TRAN-NO
003800                                     PIC S9(7)       COMP-3.
003900             07  WBHS-SAV-INELIG-RATE
004000                                     PIC SVP9(15)    COMP-3.      9915845
004100             07  WBHS-SAV-PTR        PIC S999        COMP-3.      1003625
004200             07  WBHS-SAV-ACCR-TDY   PIC S9(11)V9(6) COMP-3.
004300             07  WBHS-SAV-ACCR-CTD   PIC S9(11)V9(6) COMP-3.
004400             07  WBHS-SAV-ACCR-ADJ   PIC S9(11)V9(6) COMP-3.
004500             07  WBHS-SAV-DLY-RATE   PIC SVP9(15)    COMP-3.      9915845
004600     03  FILLER REDEFINES WBHS-AREA.
004700         05  WBHS-SAV-OCC-1          PIC X(71).                   9915845
004800         05  WBHS-SAV-OCC-2-60       PIC X(4189).                 9915845
004900     03  FILLER REDEFINES WBHS-AREA.
005000         05  WBHS-SAV-OCC-1-59       PIC X(4189).                 9915845
005100         05  WBHS-SAV-OCC-60         PIC X(71).                   9915845
