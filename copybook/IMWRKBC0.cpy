*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100******************************************************************
000200*    THIS COPY USED IN THE FOLLOWING PROGRAMS                    *
000300*                                                                *
000400*    IM13      IM19      IMPCINIT                                *
000500*                                                                *
000600*                                                                *
000700*                                                                *
000800*    PLEASE ADD NEW PROGRAMS TO ABOVE LIST                       *
000900*                                                                *
001000******************************************************************
001100 01  DDA-WRKBCR-0.
001200     03  WBC0-CONTROL-KEY.
001300         05  WBC0-CONTROL-1              PIC XX.
001400         05  WBC0-CONTROL-2              PIC XXX.
001500         05  WBC0-CONTROL-3              PIC XXX.
001600     03  WBC0-RECORD-ID                  PIC XX.
001650     03  FILLER                          PIC X(10).               9915845
001700     03  WBC-SYSTEM-PROCESS-DATES.
001800         05  WBC-SYS-PREV-PROC-DATE.
001900             07  WBC-SYS-PREV-PROC-MO    PIC XX.
002000             07  WBC-SYS-PREV-PROC-DA    PIC XX.
002100             07  WBC-SYS-PREV-PROC-YR    PIC XX.
002200         05  WBC-SYS-CURR-PROC-DATE.
002300             07  WBC-SYS-CURR-PROC-MO    PIC XX.
002400             07  WBC-SYS-CURR-PROC-DA    PIC XX.
002500             07  WBC-SYS-CURR-PROC-YR    PIC XX.
002600         05  WBC-SYS-NEXT-PROC-DATE.
002700             07  WBC-SYS-NEXT-PROC-MO    PIC XX.
002800             07  WBC-SYS-NEXT-PROC-DA    PIC XX.
002900             07  WBC-SYS-NEXT-PROC-YR    PIC XX.
003000     03  FILLER                          PIC X(6).
003100     03  WBC-SYSTEM-WEEK-DAYS.
003200         05  WBC-SYS-CURR-WEEK-DAY       PIC X.
003300         05  WBC-SYS-PREV-WEEK-DAY       PIC X.
003400         05  WBC-SYS-NEXT-WEEK-DAY       PIC X.
003500     03  WBC-SYSTEM-SCHEDULE-DATA.
003600         05  WBC-SYS-SCHEDULE            PIC X OCCURS 7 TIMES.
003700     03  WBC-SYSTEM-HOLIDAYS-THIS-YR.
003800         05  WBC-SYS-HOLIDAYS            OCCURS 36 TIMES INDEXED  0817725
003900                                         BY WBC-SYS-HOL-IND.
004000             07  WBC-SYS-HOLIDAY-DATE.
004100                 09  WBC-SYS-HOLIDAY-YEAR.
004200                     11  WBC-SYS-HOL-CENT PIC XX.
004300                     11  WBC-SYS-HOL-YR  PIC XX.
004400                 09  WBC-SYS-HOL-MO      PIC XX.
004500                 09  WBC-SYS-HOL-DA      PIC XX.
004600             07  FILLER REDEFINES WBC-SYS-HOLIDAY-DATE.
004700                 09  FILLER              PIC XX.
004800                 09  WBC-SYS-HOL-DATE    PIC X(6).
004900     03  FILLER                          PIC X(1327).             0817725
005000     03  WBC-SYSTEM-LAST-MAINT.
005100         05  WBC-SYS-LM-ONLINE-RBA       PIC X(4).
005200         05  WBC-SYS-LM-OPER-ID.
005300             07  WBC-SYS-LM-TS-TELLER    PIC X(5).
005400             07  FILLER                  PIC X(3).
005500         05  WBC-SYS-LM-BRANCH           PIC X(3).
005600         05  WBC-SYS-LM-TERM-ID          PIC X(4).
005700         05  WBC-SYS-LM-DATE.
005800             07  WBC-SYS-LM-DT-CC        PIC XX.
005900             07  WBC-SYS-LM-DT.
006000                 09  WBC-SYS-LM-DT-YY    PIC XX.
006100                 09  WBC-SYS-LM-DT-MM    PIC XX.
006200                 09  WBC-SYS-LM-DT-DD    PIC XX.
006300         05  WBC-SYS-LM-TIME.
006400             07  WBC-SYS-LM-HH-MM-SS     PIC S9(7)   COMP-3.
