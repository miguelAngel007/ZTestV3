*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
000010*---------------------------------------------------------------* 2602409
000020*                        SIWSSPAR                               * 2602409
000030*                       SISPOOL AREA                            * 2602409
000040*---------------------------------------------------------------* 2602409
000050*---------------------------------------------------------------* 2602409
000060*               ** HISTORY OF REVISIONS **                      * 2602409
000093* 06/24/05 ADD VALUES FOR HTML ENABLEMENT               GN5297  * 0615297
000095* 08/09/01 ADDED THE $ PRINT INDICATOR                  ~~~4346 * 2024346
000096* 06/22/95 TOOK OUT PRINT TRANSLATE FLAG                2602409 * 2602409
000097*          ADDED "L" FOR EXTENDED AREA TO PASS 4TH      2602409 * 2602409
000098*          PARAMETER TO SISPOOL                         2602409 * 2602409
000099*---------------------------------------------------------------* 2602409
000100 01  WS-SPOOL-AREA.
000200     05  WSSPLAR-SPL-CD            PIC X        VALUE '1'.
000300         88  WSSPLAR-SPL-CDE-PRINT VALUE '1' '3' '5' '7'
000400                                         'A' 'C' 'E' 'G'          0615297
000405                                         'I' 'K' 'M' 'O'          0615297
000410                                         'Q' 'S' 'U' 'W'.         0615297
000500         88  WSSPLAR-SPL-CDE-FICHE VALUE '2' '3' '6' '7'
000600                                         'B' 'C' 'F' 'G'          0615297
000605                                         'J' 'K' 'N' 'O'          0615297
000610                                         'R' 'S' 'V' 'W'.         0615297
000700         88  WSSPLAR-SPL-CDE-XMIT  VALUE '4' '5' '6' '7'
000800                                         'D' 'E' 'F' 'G'          0615297
000805                                         'L' 'M' 'N' 'O'          0615297
000810                                         'T' 'U' 'V' 'W'.         0615297
000900         88  WSSPLAR-SPL-CDE-INQ   VALUE '8' 'A' 'B' 'C'
001000                                         'D' 'E' 'F' 'G'          0615297
001005                                         'P' 'Q' 'R' 'S'          0615297
001010                                         'T' 'U' 'V' 'W'.         0615297
001100         88  WSSPLAR-SPL-CDE-CLOSE VALUE '9'.
001105         88  WSSPLAR-SPL-CDE-HTML  VALUE 'H' 'I' 'J' 'K'          0615297
001110                                         'L' 'M' 'N' 'O'          0615297
001115                                         'P' 'Q' 'R' 'S'          0615297
001120                                         'T' 'U' 'V' 'W'.         0615297
001200     05  WSSPLAR-SPL-FM            PIC X        VALUE 'A'.
001300     05  WSSPLAR-SPL-PGM                        VALUE SPACES.
001400         10  WSSPLAR-SPL-APPL      PIC XX.
001500         10  WSSPLAR-SPL-MAIN      PIC XX.
001600         10  WSSPLAR-SPL-USER      PIC XXXX.
001700     05  WSSPLAR-SPL-RPT-NUM       PIC 999      VALUE 000.
001800     05  WSSPLAR-SPL-BCR-BREAK-SW  PIC X        VALUE SPACE.
001900         88  WSSPLAR-SPL-BCR-BREAK              VALUE 'X'.
002000     05  WSSPLAR-BCR-CTL-AREA.
002100         10  WSSPLAR-BCR-CTL-1-4.
002200             15  WSSPLAR-BCR-CTLS.
002300                 20  WSSPLAR-BCR-CTL1 PIC XX    VALUE ZEROS.
002400                 20  WSSPLAR-BCR-CTL2 PIC XXX   VALUE ZEROS.
002500                 20  WSSPLAR-BCR-CTL3 PIC XXX   VALUE ZEROS.
002600             15  WSSPLAR-BCR-CTL4     PIC XXXX  VALUE ZEROS.      SI0007
002700         10  FILLER                   PIC X(10) VALUE ZEROS.      SI0007
002800     05  FILLER                REDEFINES WSSPLAR-BCR-CTL-AREA.
002900         10  WSSPLAR-BCR-CCR          PIC XXXX.
003000         10  WSSPLAR-BCR-RCCR.
003100             15  WSSPLAR-BCR-RC       OCCURS 9 TIMES PIC XX.
003110         10  FILLER REDEFINES WSSPLAR-BCR-RCCR.                   2500563
003120             15  WSSPLAR-BCR-RC2      PIC X(10).                  2500563
003130             15  FILLER               PIC X(8).                   2500563
003200     05  WSSPLAR-RPT-BREAK-SW      PIC X.
003300         88  WSSPLAR-RPT-BREAK                  VALUE 'X'.
003400     05  WSSPLAR-RPT-CTL-AREA.
003500         10  WSSPLAR-RPT-CTL-1-4.
003600             15  WSSPLAR-RPT-CTLS.
003700                 20  WSSPLAR-RPT-CTL1 PIC XX    VALUE ZEROS.
003800                 20  WSSPLAR-RPT-CTL2 PIC XXX   VALUE ZEROS.
003900                 20  WSSPLAR-RPT-CTL3 PIC XXX   VALUE ZEROS.
004000             15  WSSPLAR-RPT-CTL4     PIC XXXX  VALUE ZEROS.      SI0007
004100         10  FILLER                   PIC X(10) VALUE ZEROS.      SI0007
004200     05  FILLER                REDEFINES WSSPLAR-RPT-CTL-AREA.
004300         10  WSSPLAR-RPT-CCR          PIC XXXX.
004400         10  WSSPLAR-RPT-RCCR.
004500             15  WSSPLAR-RPT-RC       OCCURS 9 TIMES PIC XX.
004510         10  FILLER REDEFINES WSSPLAR-RPT-RCCR.                   2500563
004520             15  WSSPLAR-RPT-RC2      PIC X(10).                  2500563
004530             15  FILLER               PIC X(8).                   2500563
004600     05  WSSPLAR-BILLSTAT-RPTRCP   PIC X        VALUE '0'.
004700         88  WSSPLAR-NO-BILLRCP                 VALUE '0'.
004800         88  WSSPLAR-BILLSTAT                   VALUE '1' '2'.
004900         88  WSSPLAR-BILLSTAT-RCP               VALUE '2'.
005000         88  WSSPLAR-RCAP                       VALUE '2' '3'.
005100     05  WSSPLAR-SPL-LINE-SEQ      PIC S9(7)    VALUE +0   COMP-3.
005200     05  WSSPLAR-SPL-LINE-TYPE     PIC X        VALUE SPACE.
005300     05  WSSPLAR-SPL-SUB-TYPE      PIC X        VALUE SPACE.
005400     05  WSSPLAR-SPL-RECAP-LEVEL   PIC X        VALUE '0'.
005500         88  WSSPLAR-88-RECAP-OFF               VALUE '0'.
005600         88  WSSPLAR-88-RECAP-CTL1              VALUE '1'.
005700         88  WSSPLAR-88-RECAP-CTL2              VALUE '2'.
005800         88  WSSPLAR-88-RECAP-CTL3              VALUE '3'.
005900         88  WSSPLAR-88-RECAP-CTL4              VALUE '4'.
006000     05  WSSPLAR-PRT-ID            PIC X        VALUE SPACE.
006100     05  WSSPLAR-3800-DATA.
006200         10  WSSPLAR-EXT-FLAG      PIC X        VALUE SPACE.
006250             88  WSSPLAR-EXT-AREA               VALUE 'L'.        2602409
006300             88  WSSPLAR-TRC-FLAG               VALUE 'T'.
006400             88  WSSPLAR-EXT-PRT                VALUE 'X'.
006500         10  WSSPLAR-EXT-LNG       PIC S9999    VALUE +133 COMP.
006600     05  WSSPLAR-PRINT-DOLLAR-IND  PIC X        VALUE SPACE.      2024346
006610*                   Y = YES, LEAVE THE $ IN PRINT LINE            2024346
006620*          BLANK OR N = NO,  REMOVE THE $ FROM PRINT LINE         2024346
006710     05  WSSPLAR-SUB-APPLID        PIC X(2)     VALUE SPACE.
006720     05  WSSPLAR-RPT-SKRPTID       PIC S9(4)    VALUE +0 COMP.
006800     05  WSSPLAR-FORM-CHG-VER      PIC X        VALUE SPACE.      2500563
