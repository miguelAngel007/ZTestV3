*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*--------------------------------------------------------------*
000200*    CLEAR EXTENDED FLOAT RECORD COPYBOOK (IMAFF FILE)         *
000300*--------------------------------------------------------------*
000400 01  CLEAR-EXTENDED-FLOAT-REC.
000500     05  CFF-LENGTH              PIC S9999 COMP  VALUE +0.
000600     05  CFF-BIN0                PIC XX          VALUE LOW-VALUE.
000700
000800     05  CFF-RECORD.
000900         10  CFF-KEY                             VALUE ZEROES.
001000             15  CFF-CONTROLS.
001100                 20  CFF-CTL1    PIC XX.
001200                 20  CFF-CTL2    PIC XXX.
001300                 20  CFF-CTL3    PIC XXX.
001400                 20  CFF-CTL4    PIC XXXX.
001500             15  CFF-ACCT        PIC X(10).
001600         10  FILLER REDEFINES CFF-KEY.
001700             15  CFF-CTL1-CTL3   PIC X(08).
001800             15  CFF-CTL4-ACCT.
001900                 20  FILLER      PIC XX.
002000                 20  CFF-ACCT12  PIC X(12).
002100         10  CFF-REC-TYPE        PIC X           VALUE 'F'.
002200         10  CFF-ENTRIES         PIC S999        COMP-3
002300                                                 VALUE +0.
002400         10  CFF-FILLER          PIC X(21)       VALUE SPACES.
002500         10  CFF-TOTAL-13-99     PIC S9(13)V99   COMP-3
002600                                                 VALUE +0.
002700         10  CFF-FLOAT-AREA.
002800             15  CFF-FLOAT-AMT   OCCURS 99 TIMES
002900                                 PIC S9(13)V99   COMP-3
003000                                                 VALUE +0.
003100
003200         10  FILLER    REDEFINES CFF-FLOAT-AREA.
003300             15  CFF-FLOAT-01-12.
003400                 20  CFF-FLOAT-01-11  PIC X(88).
003500                 20  CFF-FLOAT-12     PIC S9(13)V99   COMP-3.
003600             15  CFF-FLOAT-13-99      PIC X(696).
003700
