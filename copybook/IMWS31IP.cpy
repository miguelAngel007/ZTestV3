*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*--------------------------------------------------------------*
000200*    COPYBOOK CONTAINS THE INDEP. INTEREST CYCLE WORK FIELDS.  *
000300*--------------------------------------------------------------*
000400 01  INT-WORK-FIELDS.
000500     03  WRK-INT-LAST-PAID-DATE.
000600         05  WORK-INT-MM                   PIC XX.
000700         05  WORK-INT-DD                   PIC XX.
000800         05  WORK-INT-YY                   PIC XX.
000900
001000     03  INT-CYCLE-WS.
001100         05  INT-CYCLE-DAYX                PIC X.
001200         05  INT-CYCLE-DAY
001300             REDEFINES INT-CYCLE-DAYX      PIC 9.
001400         05  INT-CYCLE                     PIC X    VALUE SPACE.
001500         05  INT-DAY.
001600             07  INT-DAY1                  PIC X    VALUE SPACE.
001700             07  INT-DAY2                  PIC X    VALUE SPACE.
001800         05  INT-INCR                      PIC X    VALUE SPACE.
001900         05  INT-INCR-9
002000             REDEFINES INT-INCR            PIC 9.
002100         05  INT-MONTH1                    PIC XX   VALUE SPACE.
002200         05  INT-PAY-TODAY                 PIC X    VALUE SPACE.
002300
002400     03  IOD-CYCLE-WK.
002500         05  IOD-CYCLE-DAYX                PIC X.
002600         05  IOD-CYCLE-DAY
002700             REDEFINES IOD-CYCLE-DAYX      PIC 9.
002800         05  IOD-PAY-TODAY                 PIC X    VALUE '0'.
002900
003000     03  SAV-CYCLE-WK.
003100         05  SAV-CYCLE-DAYX                PIC X.
003200         05  SAV-CYCLE-DAY
003300             REDEFINES SAV-CYCLE-DAYX      PIC 9.
003400         05  SAV-PAY-TODAY                 PIC X    VALUE '0'.
003500
003600     03  OD-CYCLE-WS.
003700         05  OD-CYCLE-DAYX                 PIC X.
003800         05  OD-CYCLE-DAY
003900             REDEFINES OD-CYCLE-DAYX       PIC 9.
004000         05  OD-CHG-TODAY                  PIC X.
004100
004200*--------------------------------------------------------------*
004300*    WKPS  - WORK FIELDS FOR IOD/OD INTEREST PAYMENT SCHEDULES *
004400*--------------------------------------------------------------*
004500 01  WK-FM-WORK-FIELDS.
004600     03  WKPS-FM-IOD-CYC-FLG               PIC X    VALUE '0'.
004700     03  WKPS-FM-IOD-INC-FLG               PIC X    VALUE '0'.
004800     03  WKPS-FM-IOD-MTH-FLG               PIC X    VALUE '0'.
004900     03  WKPS-FM-IOD-DAY-FLG               PIC X    VALUE '0'.
005000     03  WKPS-FM-SAV-CYC-FLG               PIC X    VALUE '0'.
005100     03  WKPS-FM-SAV-INC-FLG               PIC X    VALUE '0'.
005200     03  WKPS-FM-SAV-MTH-FLG               PIC X    VALUE '0'.
005300     03  WKPS-FM-SAV-DAY-FLG               PIC X    VALUE '0'.
005400     03  WKPS-FM-OD-CYC-FLG                PIC X    VALUE '0'.
005500     03  WKPS-FM-OD-INC-FLG                PIC X    VALUE '0'.
005600     03  WKPS-FM-OD-MTH-FLG                PIC X    VALUE '0'.
005700     03  WKPS-FM-OD-DAY-FLG                PIC X    VALUE '0'.
005800
005900     03  WKPS-SAVE-IOD-SCHD.
006000         05  WKPS-IOD-CYCLE                PIC X.
006100         05  WKPS-IOD-INCR                 PIC X.
006200         05  WKPS-IOD-MONTH1               PIC XX.
006300         05  WKPS-IOD-DAY                  PIC XX.
006400     03  WKPS-SAVE-SAV-SCHD.
006500         05  WKPS-SAV-CYCLE                PIC X.
006600         05  WKPS-SAV-INCR                 PIC X.
006700         05  WKPS-SAV-MONTH1               PIC XX.
006800         05  WKPS-SAV-DAY                  PIC XX.
006900     03  WKPS-SAVE-OD-SCHD.
007000         05  WKPS-OD-CYCLE                 PIC X.
007100         05  WKPS-OD-INCR                  PIC X.
007200         05  WKPS-OD-MONTH1                PIC XX.
007300         05  WKPS-OD-DAY                   PIC XX.
007400
007500         05  WKPS-HOLD-DAY.
007600             10  WKPS-HOLD-DAY-X-1         PIC X.
007700             10  WKPS-HOLD-DAY-X-2         PIC X.
007800         05  WKPS-HOLD-MM.
007900             10  WKPS-HOLD-MM-X-1          PIC X.
008000             10  WKPS-HOLD-MM-X-2          PIC X.
008100
008200     03  WKPS-FM-IOD-CYC-REC.
008300         05  FILLER                        PIC X(22).
008400         05  WKPS-FM-IOD-CYC-CLASS         PIC XX.
008500         05  FILLER                        PIC X(15).             0920022
008600         05  WKPS-FM-IOD-CYC-SOURCE        PIC XX.
008700         05  WKPS-FM-IOD-CYC-BAT           PIC S9(5)  COMP-3.
008800         05  WKPS-FM-IOD-CYC-SEQ           PIC S9(5)  COMP-3.
008900         05  FILLER                        PIC X(39).
009000         05  WKPS-FM-IOD-CYC-REMAINDER     PIC X(105).
009100     03  WKPS-FM-IOD-INC-REC.
009200         05  FILLER                        PIC X(22).
009300         05  WKPS-FM-IOD-INC-CLASS         PIC XX.
009400         05  FILLER                        PIC X(15).             0920022
009500         05  WKPS-FM-IOD-INC-SOURCE        PIC XX.
009600         05  WKPS-FM-IOD-INC-BAT           PIC S9(5)  COMP-3.
009700         05  WKPS-FM-IOD-INC-SEQ           PIC S9(5)  COMP-3.
009800         05  FILLER                        PIC X(39).
009900         05  WKPS-FM-IOD-INC-REMAINDER     PIC X(105).
010000     03  WKPS-FM-IOD-MTH-REC.
010100         05  FILLER                        PIC X(22).
010200         05  WKPS-FM-IOD-MTH-CLASS         PIC XX.
010300         05  FILLER                        PIC X(15).             0920022
010400         05  WKPS-FM-IOD-MTH-SOURCE        PIC XX.
010500         05  WKPS-FM-IOD-MTH-BAT           PIC S9(5)  COMP-3.
010600         05  WKPS-FM-IOD-MTH-SEQ           PIC S9(5)  COMP-3.
010700         05  FILLER                        PIC X(39).
010800         05  WKPS-FM-IOD-MTH-REMAINDER     PIC X(105).
010900     03  WKPS-FM-IOD-DAY-REC.
011000         05  FILLER                        PIC X(22).
011100         05  WKPS-FM-IOD-DAY-CLASS         PIC XX.
011200         05  FILLER                        PIC X(15).             0920022
011300         05  WKPS-FM-IOD-DAY-SOURCE        PIC XX.
011400         05  WKPS-FM-IOD-DAY-BAT           PIC S9(5)  COMP-3.
011500         05  WKPS-FM-IOD-DAY-SEQ           PIC S9(5)  COMP-3.
011600         05  FILLER                        PIC X(39).
011700         05  WKPS-FM-IOD-DAY-REMAINDER     PIC X(105).
011800     03  WKPS-FM-SAV-CYC-REC.
011900         05  FILLER                        PIC X(22).
012000         05  WKPS-FM-SAV-CYC-CLASS         PIC XX.
012100         05  FILLER                        PIC X(15).             0920022
012200         05  WKPS-FM-SAV-CYC-SOURCE        PIC XX.
012300         05  WKPS-FM-SAV-CYC-BAT           PIC S9(5)  COMP-3.
012400         05  WKPS-FM-SAV-CYC-SEQ           PIC S9(5)  COMP-3.
012500         05  FILLER                        PIC X(39).
012600         05  WKPS-FM-SAV-CYC-REMAINDER     PIC X(105).
012700     03  WKPS-FM-SAV-INC-REC.
012800         05  FILLER                        PIC X(22).
012900         05  WKPS-FM-SAV-INC-CLASS         PIC XX.
013000         05  FILLER                        PIC X(15).             0920022
013100         05  WKPS-FM-SAV-INC-SOURCE        PIC XX.
013200         05  WKPS-FM-SAV-INC-BAT           PIC S9(5)  COMP-3.
013300         05  WKPS-FM-SAV-INC-SEQ           PIC S9(5)  COMP-3.
013400         05  FILLER                        PIC X(39).
013500         05  WKPS-FM-SAV-INC-REMAINDER     PIC X(105).
013600     03  WKPS-FM-SAV-MTH-REC.
013700         05  FILLER                        PIC X(22).
013800         05  WKPS-FM-SAV-MTH-CLASS         PIC XX.
013900         05  FILLER                        PIC X(15).             0920022
014000         05  WKPS-FM-SAV-MTH-SOURCE        PIC XX.
014100         05  WKPS-FM-SAV-MTH-BAT           PIC S9(5)  COMP-3.
014200         05  WKPS-FM-SAV-MTH-SEQ           PIC S9(5)  COMP-3.
014300         05  FILLER                        PIC X(39).
014400         05  WKPS-FM-SAV-MTH-REMAINDER     PIC X(105).
014500     03  WKPS-FM-SAV-DAY-REC.
014600         05  FILLER                        PIC X(22).
014700         05  WKPS-FM-SAV-DAY-CLASS         PIC XX.
014800         05  FILLER                        PIC X(15).             0920022
014900         05  WKPS-FM-SAV-DAY-SOURCE        PIC XX.
015000         05  WKPS-FM-SAV-DAY-BAT           PIC S9(5)  COMP-3.
015100         05  WKPS-FM-SAV-DAY-SEQ           PIC S9(5)  COMP-3.
015200         05  FILLER                        PIC X(39).
015300         05  WKPS-FM-SAV-DAY-REMAINDER     PIC X(105).
015400     03  WKPS-FM-OD-CYC-REC.
015500         05  FILLER                        PIC X(22).
015600         05  WKPS-FM-OD-CYC-CLASS          PIC XX.
015700         05  FILLER                        PIC X(15).             0920022
015800         05  WKPS-FM-OD-CYC-SOURCE         PIC XX.
015900         05  WKPS-FM-OD-CYC-BAT            PIC S9(5)  COMP-3.
016000         05  WKPS-FM-OD-CYC-SEQ            PIC S9(5)  COMP-3.
016100         05  FILLER                        PIC X(39).
016200         05  WKPS-FM-OD-CYC-REMAINDER      PIC X(105).
016300     03  WKPS-FM-OD-INC-REC.
016400         05  FILLER                        PIC X(22).
016500         05  WKPS-FM-OD-INC-CLASS          PIC XX.
016600         05  FILLER                        PIC X(15).             0920022
016700         05  WKPS-FM-OD-INC-SOURCE         PIC XX.
016800         05  WKPS-FM-OD-INC-BAT            PIC S9(5)  COMP-3.
016900         05  WKPS-FM-OD-INC-SEQ            PIC S9(5)  COMP-3.
017000         05  FILLER                        PIC X(39).
017100         05  WKPS-FM-OD-INC-REMAINDER      PIC X(105).
017200     03  WKPS-FM-OD-MTH-REC.
017300         05  FILLER                        PIC X(22).
017400         05  WKPS-FM-OD-MTH-CLASS          PIC XX.
017500         05  FILLER                        PIC X(15).             0920022
017600         05  WKPS-FM-OD-MTH-SOURCE         PIC XX.
017700         05  WKPS-FM-OD-MTH-BAT            PIC S9(5)  COMP-3.
017800         05  WKPS-FM-OD-MTH-SEQ            PIC S9(5)  COMP-3.
017900         05  FILLER                        PIC X(39).
018000         05  WKPS-FM-OD-MTH-REMAINDER      PIC X(105).
018100     03  WKPS-FM-OD-DAY-REC.
018200         05  FILLER                        PIC X(22).
018300         05  WKPS-FM-OD-DAY-CLASS          PIC XX.
018400         05  FILLER                        PIC X(15).             0920022
018500         05  WKPS-FM-OD-DAY-SOURCE         PIC XX.
018600         05  WKPS-FM-OD-DAY-BAT            PIC S9(5)  COMP-3.
018700         05  WKPS-FM-OD-DAY-SEQ            PIC S9(5)  COMP-3.
018800         05  FILLER                        PIC X(39).
018900         05  WKPS-FM-OD-DAY-REMAINDER      PIC X(105).
019000
