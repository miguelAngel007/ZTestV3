*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*
000200*         IMPACS ALTERNATE NAME AND ADDRESS FILE                 *
000300*----------------------------------------------------------------*
000400 01  IMN-MASTER-RECORD.
000500     05  IMN-CONTROL-KEY.
000600         10  IMN-CONTROLS-WO-USE.
000700             15  IMN-CONTROL-1   PIC XX.
000800             15  IMN-CONTROL-2   PIC XXX.
000900             15  IMN-CONTROL-3   PIC XXX.
000910             15  IMN-CTL4-ACCT.                                   0417078
001000                 20  IMN-CONTROL-4   PIC X(4).                    0417078
001100                 20  IMN-ACCOUNT-NO  PIC X(10).                   0417078
001200         10  IMN-USE-CODE        PIC XX.
001300             88  IMN-USE-CODE-ST                VALUE 'ST'.
001400     05  IMN-DATA.
001500         10  IMN-NBR-ADDRS       PIC S9(3)      COMP-3.
001600         10  IMN-ADDR-DATA  OCCURS 9 TIMES.
001700             15  IMN-LN-NO-LINES PIC S9         COMP-3.
001800             15  IMN-LN-COUNTRY  PIC XX.
001900             15  IMN-LN-LINE   OCCURS 6 TIMES.
002000                 20  IMN-LN-TYPE PIC X.
002100                 20  IMN-LN-TYP9 REDEFINES IMN-LN-TYPE
002200                                 PIC 9.
002300                 20  IMN-LN-NO   PIC X.
002400                 20  IMN-LN-NO9  REDEFINES IMN-LN-NO
002500                                 PIC 9.
002600                 20  IMN-LN-NAME-ADDR
002700                                 PIC X(40).
002800                 20  FILLER  REDEFINES IMN-LN-NAME-ADDR.
002900                     25  IMN-LN-CITY-STATE
003000                                 PIC X(30).
003100                     25  IMN-LN-ZIP-CODE
003200                                 PIC X(10).
003300         10  IMN-LAST-MAINT.
003400             15  IMN-LM-OPER-ID  PIC X(5).
003500             15  IMN-LM-DATE.
003600                 20  IMN-LM-DT-CC
003700                                 PIC XX.
003800                 20  IMN-LM-DT.
003900                     25  IMN-LM-DT-YY
004000                                 PIC XX.
004100                     25  IMN-LM-DT-MM
004200                                 PIC XX.
004300                     25  IMN-LM-DT-DD
004400                                 PIC XX.
004500             15  IMN-LM-TIME     PIC S9(7)      COMP-3.
004600     05  FILLER                  PIC X(81).
