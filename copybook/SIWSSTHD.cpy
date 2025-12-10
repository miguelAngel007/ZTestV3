*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
001000 01  SIWS-STANDARD-HEADING.
001100     03  SIWS-HDG1.
001200         05  SIWS-SH1-PRTCL              PIC X     VALUE '1'.
001300         05  SIWS-SH1-CTL1-DESC          PIC X(10) VALUE SPACES.
001400         05  FILLER                      PIC X     VALUE SPACE.
001500         05  SIWS-SH1-CTL1-AREA.
001600             10  SIWS-SH1-CTL1           PIC XX    VALUE SPACES.
001700             10  FILLER                  PIC X(3)  VALUE SPACE.
001800             10  SIWS-SH1-CTL1-LUF       PIC X(30) VALUE SPACES.
001900         05  FILLER                      PIC X(4)  VALUE SPACES.
002000         05  SIWS-SH1-SYS-NAME           PIC X(32)
002100                     VALUE 'SYSTEMATICS SYSTEM NAME         '.
002200         05  FILLER                      PIC X(14) VALUE SPACES.
002300         05  FILLER                      PIC X     VALUE '*'.
002400         05  SIWS-SH1-PHASE              PIC X(8)  VALUE SPACES.
002500         05  FILLER                      PIC X(4)  VALUE '*'.     2602675
002600         05  SIWS-SH1-PAGENO-LIT         PIC X(18) VALUE          2602675
002700                                         'PAGE NO'.
002800         05  SIWS-SH1-PAGENO             PIC Z(4)9.
002900     03  SIWS-HDG2.
003000         05  SIWS-SH2-PRTCL              PIC X     VALUE '1'.
003100         05  SIWS-SH2-CTL2-DESC          PIC X(10) VALUE SPACES.
003200         05  FILLER                      PIC X     VALUE SPACE.
003300         05  SIWS-SH2-CTL2-AREA.
003400             10  SIWS-SH2-CTL2           PIC X(3)  VALUE SPACES.
003500             10  FILLER                  PIC X(2)  VALUE SPACE.
003600             10  SIWS-SH2-CTL2-LUF       PIC X(30) VALUE SPACES.
003700         05  FILLER                      PIC X(4)  VALUE SPACES.
003800         05  SIWS-SH2-BANK-NAME          PIC X(40) VALUE SPACES.
003900         05  FILLER                      PIC X(6)  VALUE SPACES.
004000         05  SIWS-SH2-WEEKDAY            PIC X(9)  VALUE SPACES.
004100         05  FILLER                      PIC X(4)  VALUE SPACES.  2602675
004200         05  SIWS-SH2-PROC-SYS-DATE-LIT  PIC X(12) VALUE
004300             'PROCESS DATE'.
004400         05  FILLER                      PIC X     VALUE SPACE.
004500         05  SIWS-SH2-PROC-SYS-DATE.
004600             07  SIWS-SH2-PROC-SYS-MO    PIC XX    VALUE SPACES.
004700             07  FILLER                  PIC X     VALUE '/'.
004800             07  SIWS-SH2-PROC-SYS-DA    PIC XX    VALUE SPACES.
004900             07  FILLER                  PIC X     VALUE '/'.
005000             07  SIWS-SH2-PROC-SYS-YR    PIC XXXX  VALUE SPACES.  2602675
005010             07  FILLER REDEFINES SIWS-SH2-PROC-SYS-YR.           9713304
005020                 09  SIWS-SH2-PROC-SYS-CC                         9713304
005030                                         PIC XX.                  9713304
005040                 09  SIWS-SH2-PROC-SYS-YY                         9713304
005050                                         PIC XX.                  9713304
005100     03  SIWS-HDG3.
005200         05  SIWS-SH3-PRTCL              PIC X     VALUE '1'.
005300         05  SIWS-SH3-CTL3-DESC          PIC X(10) VALUE SPACES.
005400         05  FILLER                      PIC X     VALUE SPACE.
005500         05  SIWS-SH3-CTL3-AREA.
005600             10  SIWS-SH3-CTL3           PIC X(3)  VALUE SPACES.
005700             10  FILLER                  PIC X(2)  VALUE SPACE.
005800             10  SIWS-SH3-CTL3-LUF       PIC X(30) VALUE SPACES.
005900         05  FILLER                      PIC X(4)  VALUE SPACE.
006000         05  SIWS-SH3-RPT-NAME           PIC X(40) VALUE SPACES.
006100         05  FILLER                      PIC XX    VALUE SPACES.
006200         05  SIWS-SH3-APPL-NAME                    VALUE SPACE.
006300             07  FILLER                  PIC X(4).
006400             07  SIWS-SH3-AST1           PIC X(1).
006500             07  SIWS-SH3-RPT-ID.
006600                 09  SIWS-SH3-RPT-APPL   PIC X(2).
006700                 09  SIWS-SH3-RPT-SUB-APPL  PIC X(2).
006800                 09  FILLER              PIC X(1).
006900                 09  SIWS-SH3-RPT-NBR    PIC 9(3).
007000             07  SIWS-SH3-AST2           PIC X(1).
007100             07  FILLER                  PIC X(2).
007200         05  FILLER                      PIC X     VALUE SPACES.  2602675
007300         05  SIWS-SH3-DATE-TIME-LIT      PIC X(12) VALUE
007400             'PROCESS THRU'.
007500         05  FILLER                      PIC X     VALUE SPACE.
007600         05  SIWS-SH3-DATE-TIME.
007700             07  SIWS-SH3-MO-HR          PIC XX    VALUE SPACES.
007800             07  SIWS-SH3-SL1-PER1       PIC X     VALUE '/'.
007900             07  SIWS-SH3-DA-MIN         PIC XX    VALUE SPACES.
008000             07  SIWS-SH3-SL2-PER2       PIC X     VALUE '/'.
008100             07  SIWS-SH3-YR-SEC         PIC XXXX  VALUE SPACES.  2602675
008110             07  FILLER REDEFINES SIWS-SH3-YR-SEC.                9713304
008120                 09  SIWS-SH3-YR-SEC-CC  PIC XX.                  9713304
008130                 09  SIWS-SH3-YR-SEC-YY  PIC XX.                  9713304
008200     03  SIWS-HDG4.
008300         05  SIWS-SH4-PRTCL              PIC X     VALUE '2'.
008400         05  SIWS-SH4-CTL4-DESC          PIC X(10) VALUE SPACES.
008500         05  FILLER                      PIC X     VALUE SPACE.
008600         05  SIWS-SH4-CTL4-AREA.
008700             10  SIWS-SH4-CTL4           PIC X(4)  VALUE SPACES.
008800             10  FILLER                  PIC X     VALUE SPACE.
008900             10  SIWS-SH4-CTL4-LUF       PIC X(30) VALUE SPACES.
009000         05  FILLER                      PIC X(63) VALUE SPACES.  2602675
009100         05  SIWS-SH4-DATE-TIME-LIT      PIC X(12) VALUE SPACES.
009200         05  FILLER                      PIC X     VALUE SPACE.
009300         05  SIWS-SH4-DATE-TIME.
009400             07  SIWS-SH4-MO-HR          PIC XX    VALUE SPACES.
009500             07  SIWS-SH4-SL1-PER1       PIC X     VALUE '/'.
009600             07  SIWS-SH4-DA-MIN         PIC XX    VALUE SPACES.
009700             07  SIWS-SH4-SL2-PER2       PIC X     VALUE '/'.
009800             07  SIWS-SH4-YR-SEC         PIC XXXX  VALUE SPACES.  2602675
009810             07  FILLER REDEFINES SIWS-SH4-YR-SEC.                9713304
009820                 09  SIWS-SH4-YR-SEC-CC  PIC XX.                  9713304
009830                 09  SIWS-SH4-YR-SEC-YY  PIC XX.                  9713304
009900     03  SIWS-HEADING-CONTROL-AREA.
010000         05  SIWS-BCR-FULL-PAGE        PIC S9(3) COMP-3 VALUE +56.
010100         05  SIWS-SPL-PAGE-CNTR        PIC S9(5) COMP-3 VALUE +0.
010200         05  SIWS-SPL-LINE-CNTR        PIC S9(7) COMP-3 VALUE +0.
010300         05  SIWS-SPL-HDR-BREAK        PIC X            VALUE 'X'.
010400             88  SIWS-DO-NOT-PRODUCE-HDRS               VALUE ' '.
010500             88  SIWS-PRODUCE-HDRS                      VALUE 'X'.
010600         05  SIWS-SPL-FORCE-2NDARY     PIC X            VALUE ' '.
010700             88  SIWS-FORCE-2NDARY                      VALUE 'X'.
010800         05  SIWS-START-2NDARY-HD      PIC S9(3) COMP-3 VALUE +1.
010900         05  SIWS-NO-2NDARY-HD         PIC S9(3) COMP-3 VALUE +0.
011000         05  FILLER                    PIC X(10)        VALUE ' '.
011100         05  SIWS-2NDARY-HEADINGS.
011200             10  SIWS-2NDARY-HEAD      OCCURS 10 TIMES
011300                                       PIC X(133).
