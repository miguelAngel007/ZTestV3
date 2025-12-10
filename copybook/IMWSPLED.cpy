*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*--------------------------------------------------------------*
000200* THIS COPYBOOK CONTAINS THE FIELDS USED BY THE PLAN EDITS     *
000300*--------------------------------------------------------------*
000400 01  WK-PLAN-EDITS.
000500     03  WK-PE-PL-CODES.
000600         05  WK-PE-PL-CD              PIC XX.
000700         05  WK-PE-PL-CD2             PIC XX.
000800     03  WK-PE-PGM                    PIC X(4).
001100     03  WK-PE-MASTER                 PIC X.
001200     03  WK-PE-PL-TYPE.
001300         05  WK-PE-PL-TYPEN           PIC 99.
001400     03  WK-PE-PART-EXC-FLG           PIC X.
001500     03  WK-PE-BENE-FLG               PIC X.
001600     03  WK-PE-VALID-CONT             PIC X.
001700     03  WK-PE-SNG-FAM-FLG            PIC X.
001800     03  WK-PE-EARLY-DIST             PIC X.
001900     03  WK-PE-STATUS-CD              PIC X.
002000     03  WK-PE-DEATH-DT.
002100         05  WK-PE-DEATH-YR           PIC X(4).
002200         05  FILLER                   PIC X(4).
002300     03  WK-PE-DT-NXT-PAY             PIC X(8).                   0937740
002400     03  WK-PE-RET-MIN-DATE           PIC X(8).                   0937740
002500     03  WK-PE-RUN-DATE.                                          0937740
002510         05  WK-PE-RUN-YEAR.                                      0937740
002520             07  WK-PE-RUN-CC         PIC XX.                     0937740
002530             07  WK-PE-RUN-YY         PIC XX.                     0937740
002540         05  FILLER                   PIC X(4).                   0937740
002600     03  WK-PE-ADJ-TYPE               PIC X.                      0937740
002700     03  WK-PE-TR-INFO                PIC X.                      0937740
002800     03  WK-PE-TR-TYPE                PIC X.                      0937740
002900     03  WK-PE-OPT-2                  PIC X.                      0937740
003000     03  WK-PE-OPT-6                  PIC X.                      0937740
003100     03  WK-PE-OPT-9                  PIC X.                      0937740
003200     03  WK-PE-OPT-11                 PIC X.                      0937740
003300     03  WK-PE-OPT-12                 PIC X.                      0937740
003400     03  WK-PE-OPT-13                 PIC X.                      0937740
003500     03  WK-PE-OPT-16                 PIC X.                      0937740
003600     03  WK-PE-OPT-18                 PIC X.                      0937740
004000     03  WK-PE-ERR-CD-TBL.                                        0937740
004100         05  WK-PE-ERR-CDS            OCCURS 10 TIMES             0937740
004200                                      INDEXED BY PE-IDX.          0937740
004300             07  WK-PE-ERR-CD         PIC S9(4) COMP.             0937740
004400             07  WK-PE-ERR-PC         PIC X.                      0937740
