*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*--------------------------------------------------------------*
000200*    WORK EXTENDED FLOAT RECORD COPYBOOK                       *
000300*--------------------------------------------------------------*
000400 01  WFF-EXTENDED-FLOAT-REC.
000500     05  WFF-LENGTH              PIC S9999       COMP.
000600     05  WFF-BIN0                PIC XX.
000700
000800     05  WFF-RECORD.
000900         10  WFF-KEY.
001000             15  WFF-CONTROLS.
001100                 20  WFF-CTL1    PIC XX.
001200                 20  WFF-CTL2    PIC XXX.
001300                 20  WFF-CTL3    PIC XXX.
001400                 20  WFF-CTL4    PIC XXXX.
001500             15  WFF-ACCT        PIC X(10).
001600         10  FILLER REDEFINES WFF-KEY.
001700             15  WFF-CTL1-CTL3   PIC X(08).
001800             15  WFF-CTL4-ACCT.
001900                 20  FILLER      PIC XX.
002000                 20  WFF-ACCT12  PIC X(12).
002100         10  WFF-REC-TYPE        PIC X.
002200         10  WFF-ENTRIES         PIC S999        COMP-3.
002300         10  WFF-FILLER          PIC X(21).
002400         10  WFF-TOTAL-13-99     PIC S9(13)V99   COMP-3.
002500         10  WFF-FLOAT-AREA      PIC X(792).
002600
002700         10  FILLER    REDEFINES WFF-FLOAT-AREA.
002800             15  WFF-FLOAT-AMT   OCCURS 99 TIMES
002900                                 PIC S9(13)V99   COMP-3.
003000
003100         10  FILLER    REDEFINES WFF-FLOAT-AREA.
003200             15  WFF-FLOAT-01    PIC S9(13)V99   COMP-3.
003300             15  WFF-FLOAT-02-99 PIC X(784).
003400
003500         10  FILLER    REDEFINES WFF-FLOAT-AREA.
003600             15  WFF-FLOAT-01-12.
003700                 20  WFF-FLOAT-01-11 PIC X(88).
003800                 20  WFF-FLOAT-12    PIC S9(13)V99  COMP-3.
003900             15  WFF-FLOAT-13-99.
004000                 20  WFF-FLOAT-13    PIC S9(13)V99   COMP-3.
004100                 20  WFF-FLOAT-14-99 PIC X(688).
004200
