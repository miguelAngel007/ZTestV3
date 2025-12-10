*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*
000200*    IMPACS OD LIMIT RECORD LAYOUT (IMLMTM)                      *
000300*----------------------------------------------------------------*
000400 01  OD-LIMIT-REC.
000500   02  OD-LENGTH                     PIC S9(4)        COMP.
000600   02  FILLER                        PIC XX.
000700   02  OD-LIMIT-HEADER-REC.
000800     03  OD-LIMIT-HDR-KEY            PIC X(22).
000900     03  OD-LIMIT-HDR-ID             PIC X.
001000     03  OD-LIMIT-HDR-DATA-CENTR-ID  PIC XXXX.
001100     03  OD-LIMIT-HDR-GROUP          PIC X.
001200     03  OD-LIMIT-HDR-FILE-SEQ       PIC S9(5)        COMP-3.
001300     03  OD-LIMIT-HDR-SYSTEM-DATE    PIC X(8).
001400     03  OD-LIMIT-HDR-SYSTEM-TIME    PIC X(6).
001500     03  OD-LIMIT-HDR-EOM            PIC X.
001600     03  OD-LIMIT-HDR-EOQ            PIC X.
001700     03  OD-LIMIT-HDR-EOY            PIC X.
001800     03  OD-LIMIT-HDR-PROC-THRU      PIC X(6).
001900     03  OD-LIMIT-HDR-EOAY           PIC X.
002000     03  OD-LIMIT-HDR-FILLER         PIC X(25).
002100     03  FILLER                      PIC X(541).
002200   02  OD-LIMIT-DETAIL-REC REDEFINES OD-LIMIT-HEADER-REC.
002300     03  OD-LIMIT-KEY.
002400         05  OD-LIMIT-CONTROLS.
002500             07  OD-LIMIT-CTL1       PIC XX.
002600             07  OD-LIMIT-CTL2       PIC XXX.
002700             07  OD-LIMIT-CTL3       PIC XXX.
002800             07  OD-LIMIT-CTL4       PIC XXXX.
002900         05  OD-LIMIT-ACCT           PIC X(10).
002910     03  FILLER REDEFINES OD-LIMIT-KEY.                           0417078
002920         05  OD-LIMIT-CTL1-CTL3          PIC X(08).               0417078
002930         05  OD-LIMIT-CTL4-ACCT          PIC X(14).               0417078
003000     03  OD-LIMIT-FIXED-FLAGS.
003100         05  OD-LIMIT-REC-ID         PIC X.
003200         05  OD-LIMIT-DELETE         PIC X.
003300             88 OD-REC-DELETE        VALUE 'Y'.
003400             88 OD-REC-RETAIN-ONEDAY VALUE 'A'.
003500         05  OD-LIMIT-REC-CHG-TODAY  PIC X.
003600             88 OD-REC-NO-CHG        VALUE '0'.
003700             88 OD-REC-CHG-ONLINE    VALUE '1'.
003800             88 OD-REC-CHG-BATCH     VALUE '2'.
003900             88 OD-REC-CHG-OL-BATCH  VALUE '3'.
004000             88 OD-REC-ADDED-TODAY   VALUE '4'.
004100         05  OD-LIMIT-REPORT-TODAY   PIC X.
004200         05  OD-LIMIT-AUTH           PIC X.
004300         05  OD-LIMIT-DOCUMENT-IND   PIC X.
004400         05  FILLER                  PIC X(3).
004500     03  OD-LIMIT-FIXED-AMTS.
004600         05  OD-LIMIT-BATCH-TOTAL    PIC S9(13)V99    COMP-3.
004700         05  OD-LIMIT-ONLINE-TOTAL   PIC S9(13)V99    COMP-3.
004800         05  FILLER                  PIC X(16).
004900     03  OD-LIMIT-EXP-RPT-LEAD-DAYS  PIC S999         COMP-3.
005000     03  OD-LIMIT-REV-RPT-LEAD-DAYS  PIC S999         COMP-3.
005100     03  OD-LIMIT-OCCURRENCES        PIC S999         COMP-3.
005200***
005300     03  OD-LIMIT-1-12               OCCURS 12 TIMES.
005400         05  OD-LIMIT-AMT            PIC S9(13)V99    COMP-3.
005500         05  OD-LIMIT-AMT-RED REDEFINES OD-LIMIT-AMT
005600                                     PIC X(8).
005700         05  OD-LIMIT-EFF-DATE.
005800             07  OD-EFF-CC           PIC XX.
005900             07  OD-EFF-DATE.
006000                 09  OD-EFF-YY       PIC XX.
006100                 09  OD-EFF-MM       PIC XX.
006200                 09  OD-EFF-DD       PIC XX.
006300         05  OD-LIMIT-EXP-DATE.
006400             07  OD-EXP-CC           PIC XX.
006500             07  OD-EXP-DATE.
006600                 09  OD-EXP-YY       PIC XX.
006700                 09  OD-EXP-MM       PIC XX.
006800                 09  OD-EXP-DD       PIC XX.
006900         05  OD-LIMIT-REV-DATE.
007000             07  OD-REV-CC           PIC XX.
007100             07  OD-REV-DATE.
007200                 09  OD-REV-YY       PIC XX.
007300                 09  OD-REV-MM       PIC XX.
007400                 09  OD-REV-DD       PIC XX.
007500         05  OD-LIMIT-TYPE.
007600             07  OD-LIMIT-TYPE1      PIC X.
007700             07  OD-LIMIT-TYPE2      PIC X.
007800             07  OD-LIMIT-TYPE3      PIC X.
007900         05  OD-LIMIT-STATUS         PIC X.
008000             88  OD-LIMIT-NOT-YET-ACTIVE VALUE '0'.
008100             88  OD-LIMIT-ACTIVE         VALUE 'A'.
008200             88  OD-LIMIT-EXPIRED        VALUE 'E'.
008300         05  OD-LIMIT-EXP-ACTION     PIC X.
008400             88  OD-EXP-DELETE       VALUE '0'.
008500             88  OD-EXP-FLAG-ADD     VALUE '1'.
008600             88  OD-EXP-FLAG-NO-ADD  VALUE '2'.
008700         05  OD-LIMIT-OCC-CHG-TODAY  PIC X.
008800             88 OD-OCC-NO-CHG        VALUE '0'.
008900             88 OD-OCC-CHG-ONLINE    VALUE '1'.
009000             88 OD-OCC-CHG-BATCH     VALUE '2'.
009100             88 OD-OCC-CHG-OL-BATCH  VALUE '3'.
009200             88 OD-OCC-ADDED-TODAY   VALUE '4'.
009300         05  OD-LIMIT-OCC-DEL-TODAY  PIC X.
009400         05  FILLER                  PIC X(7).
