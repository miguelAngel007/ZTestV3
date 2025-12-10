*     * 802262 12/11/20 PROYECTO UPGRADE GN172 EDVR
000100*
000200*    SIDHIBD CALCULATION PARAMETERS AREA
000300*
000302*--------------------------------------------------------------*  0925754
000304*               ** HISTORY OF REVISIONS **                     *  0925754
000306* DESCRIPTION                                           CHNGID *  0925754
000308* ____________________________________________________  _______*  0925754
000310*                                                              *  0925754
000396* 11/17/08 MODIFIED TO CHANGE THE OCCURENCES            GN5754 *  0925754
000398*--------------------------------------------------------------*  0925754
000400 01  SIDHIBD-CALC-PARMS.
000500     03  BD-BUSINESS-DAYS  PIC S9(5) COMP-3.
000600     03  BD-ACTUAL-DAYS    PIC S9(5) COMP-3.
000700     03  BD-RETURN-CODE    PIC S999  COMP-3.
000800     03  BD-DAY-OF-WEEK    PIC 9.
000900     03  BD-DATE-FORMAT    PIC X     VALUE 'C'.
001000         88  CCYYMMDD-FORMAT         VALUE 'C'.
001100         88  YYMMDD-FORMAT           VALUE 'Y'.
001200         88  MMDDYY-FORMAT           VALUE 'M'.
001205     03  BD-MONDAY-PROC    PIC X     VALUE 'Y'.                   2020220
001210     03  BD-TUESDAY-PROC   PIC X     VALUE 'Y'.                   2020220
001215     03  BD-WEDNESDAY-PROC PIC X     VALUE 'Y'.                   2020220
001220     03  BD-THURSDAY-PROC  PIC X     VALUE 'Y'.                   2020220
001225     03  BD-FRIDAY-PROC    PIC X     VALUE 'Y'.                   2020220
001300     03  BD-SATURDAY-PROC  PIC X     VALUE 'N'.
001400     03  BD-SUNDAY-PROC    PIC X     VALUE 'N'.
001500     03  BD-INCLUDE-BEGIN  PIC X     VALUE 'N'.
001600     03  BD-INCLUDE-END    PIC X     VALUE 'Y'.
001700     03  FILLER            PIC X(8)  VALUE LOW-VALUES.
001800     03  BD-NO-HOLIDAYS    PIC S999  COMP-3 VALUE +0.
001900     03  BD-HOLIDAY-TABLE.
002000         05  BD-HOLIDAY        OCCURS 036 TIMES.                  0925754
002100             07  BD-CENTYEAR.
002200                 09  BD-CENT   PIC XX.
002300                 09  BD-YEAR   PIC XX.
002400             07  BD-MONTH      PIC XX.
002500             07  BD-DAY        PIC XX.
002600     03  BD-BASE-YEAR          PIC 9(2) VALUE 50.                 2602675
