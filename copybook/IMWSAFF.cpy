*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*--------------------------------------------------------------*
000200*    EXTENDED FLOAT RECORD COPYBOOK (IMAFF FILE)               *
000300*--------------------------------------------------------------*
000400 01  EXTENDED-FLOAT-REC.
000500     05  FF-LENGTH               PIC S9999       COMP.
000600     05  FF-BIN0                 PIC XX.
000700
000800     05  FF-RECORD.
000900         10  FF-KEY.
001000             15  FF-CONTROLS.
001100                 20  FF-CTL1     PIC XX.
001200                 20  FF-CTL2     PIC XXX.
001300                 20  FF-CTL3     PIC XXX.
001400                 20  FF-CTL4     PIC XXXX.
001500             15  FF-ACCT         PIC X(10).
001600         10  FILLER REDEFINES FF-KEY.
001700             15  FF-CTL1-CTL3    PIC X(08).
001800             15  FF-CTL4-ACCT.
001900                 20  FILLER      PIC XX.
002000                 20  FF-ACCT12   PIC X(12).
002100         10  FF-REC-TYPE         PIC X.
002200             88  FF-TYPE-HDR                     VALUE 'H'.
002300             88  FF-TYPE-FLT                     VALUE 'F'.
002400         10  FF-ENTRIES          PIC S999        COMP-3.
002500         10  FF-FILLER           PIC X(21).
002600         10  FF-TOTAL-13-99      PIC S9(13)V99   COMP-3.
002700         10  FF-FLOAT-AREA       PIC X(792).
002800
002900         10  FILLER    REDEFINES FF-FLOAT-AREA.
003000             15  FF-FLOAT-AMT    OCCURS 99 TIMES
003100                                 PIC S9(13)V99   COMP-3.
003200
003300         10  FILLER    REDEFINES FF-FLOAT-AREA.
003400             15  FF-FLOAT-01     PIC S9(13)V99   COMP-3.
003500             15  FF-FLOAT-02-99  PIC X(784).
003600
003700         10  FILLER    REDEFINES FF-FLOAT-AREA.
003800             15  FF-FLOAT-01-12.
003900                 20  FF-FLOAT-01-11  PIC X(88).
004000                 20  FF-FLOAT-12     PIC S9(13)V99   COMP-3.
004100             15  FF-FLOAT-13-99.
004200                 20  FF-FLOAT-13     PIC S9(13)V99   COMP-3.
004300                 20  FF-FLOAT-14-99  PIC X(688).
004400
004500     05  FFH-HEADER    REDEFINES FF-RECORD.
004600         10  FFH-KEY             PIC X(22).
004700         10  FFH-REC-TYPE        PIC X.
004800         10  FFH-FILE-SEQ        PIC S9(5)       COMP-3.
004900         10  FILLER              PIC X(820).
005000
