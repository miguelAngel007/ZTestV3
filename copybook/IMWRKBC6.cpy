*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100******************************************************************
000200*    THIS COPY USED IN THE FOLLOWING PROGRAMS                    *
000250*                                                                *IM005
000300*    FIIMNNS1  FIIMNOD1  FIIMNOD2  FIIMNOD3  IMBCRFIX  IMSVCGTS  *IM005
000400*    IM003B    IM004B    IM005B    IM03      IM05      IM06      *IM005
000500*    IM12      IM13      IM14BA    IM14BB    IM14BC    IM14BD    *IM008
000600*    IM14BE    IM14BF    IM14BH    IM19      IM41      IM41NH    *IM008
000700*    IM41NJ    IM41NL    IM41NM    IM41NR    IM41OD    IM41OV    *IM008
000800*    IM41OX    IM41OZ    IM41O1    IM41PC    IM41PF    IM41PJ    *IM008
000850*    IM41PM    IM41P6    IM41P7    IM41P9    IM41Q3    IM42      *IM008
000860*    IM43      IM45      IM67      IM67FMS   IM92                *IM008
000875*                                                                *IM008
000900*    PLEASE ADD NEW PROGRAMS TO THE ABOVE LIST                   *
001000*                                                                *
001100******************************************************************
001200 01  DDA-WRKBCR-6.
001300     03  WBC6-CONTROL-KEY.
001400         05  WBC6-CONTROL-1  PIC XX.
001500         05  WBC6-CONTROL-2  PIC XXX.
001600         05  WBC6-CONTROL-3  PIC XXX.
001700     03  WBC6-RECORD-ID      PIC XX.
001800     03  FILLER              PIC X(10).                           9915845
001900     03  WBC-DEFAULT-SPOOL-OPT   PIC X.
002000     03  WBC6-REPORT-TABLE   OCCURS 99 TIMES
002100             INDEXED BY WBC6-REPT-INDEX.
002200         05  WBC-RPT-PHASE   PIC X(4).
002300         05  WBC-RPT-PRINT   PIC X.
002400         05  WBC-RPT-SPOOL-OPT   PIC X.
002500         05  WBC-RPT-BILLABLE    PIC 9.
002510         05  WBC-RPT-MASK-ID     PIC X.                           0817719
002520     03  FILLER                  PIC X(601).                      0817719
002600     03  WBC-STATEMENT-PHASES    OCCURS 6 TIMES.
002700         05  WBC-STMT-TYPE           PIC X.
002800         05  WBC-STMT-SPOOL-OPT  PIC X.
002900         05  WBC-STMT-PHASE          PIC X(4).
003000     03  WBC-STATEMENT-FORMAT    OCCURS 6 TIMES.
003100         05  WBC-STAT-KIND.
003200             07  WBC-STAT-KIND1      PIC X.
003300             07  WBC-STAT-COMBINED   PIC X.
003400         05  WBC-STAT-FORM       PIC S9      COMP-3.
003500         05  WBC-STAT-MSG        PIC X.
003600         05  WBC-STAT-FIELD      PIC XX.
003700     03  WBC-STMT-FORM-NAMES.
003800         05  WBC-FORM-NAME   PIC X(6)        OCCURS 6 TIMES.
003900     03  WBC-LOAN-TRANS-PRINT    PIC X.
004000     03  FILLER                  PIC X(12).                       IM008
004400     03  WBC-IM34-SORT-PHASE     PIC XXXX.
004500     03  WBC-IM34-COMB-STMT-PHS  PIC XXXX.
004600     03  WBC-IM34-ARP-PHASE      PIC XXXX.
004700     03  WBC-BANK-NAME-ADDRESS.
004800         05  WBC-BK-NAME         PIC X(40).
004900         05  WBC-BK-ADDRESS      PIC X(40).
005000         05  WBC-CITY-STATE      PIC X(30).
005100         05  WBC-BK-ZIP          PIC X(10).
005200     03  WBC-BANK-TELEPHONE.                                      IM008
005210         05  WBC-TELEPHONE-AREA  PIC X(3).                        IM008
005220         05  WBC-TELEPHONE-NUM   PIC X(11).                       IM008
005300     03  FILLER                  PIC X(8).                        9915845
005400     03  FILLER                  PIC X(11).                       9915845
