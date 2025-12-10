*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100***************************************************************** IM005
000200*         THIS COPY USED IN THE FOLLOWING PROGRAMS              * IM005
000300*                                                               * IM005
000400*    FIIMEN01  FIIMLOAD  IMACTM    IMALPHA   IMFDIC    IMSVCGTS * IM008
000500*    IM006ACT  IM0061AC  IM007ACT  IM008M    IM03      IM27     * IM008
000600*    IM31      IM32      IM33      IM34      IM36      IM41     * IM008
000700*    IM41PF    IM41PG    IM42      IM44      IM45      IM61     * IM008
000800*    IM78      IM91                                             * IM008
000900*    PLEASE ADD NEW PROGRAMS AS NEEDED TO THE ABOVE LIST        * IM008
001000***************************************************************** IM008
001100 01  SI-ENVIRONMENT-AREA.                                         IM008
001200     03  SI-ENVIRONMENT-LENGTH       PIC S9(4)   COMP.            IM008
001300     03  FILLER                      PIC X(02).                   IM008
001400     03  SI-ENVIRONMENT-KEY          PIC X(08)   VALUE ZEROES.    IM008
001500     03  SI-ENVIRONMENT-REC-TYPE     PIC X.                       IM008
001600     03  SI-LAST-MAINT.                                           IM008
001700         05  SI-LM-ONLINE-RBA        PIC X(4).                    IM008
001800         05  SI-LM-OPER-ID.                                       IM008
001900             07  SI-LM-TS-TELLER     PIC X(5).                    IM008
002000             07  FILLER              PIC X(3).                    IM008
002100         05  SI-LM-BRANCH            PIC X(3).                    IM008
002200         05  SI-LM-TERM-ID           PIC X(4).                    IM008
002300         05  SI-LM-DATE.                                          IM008
002400             07  SI-LM-DT-CC         PIC XX.                      IM008
002500             07  SI-LM-DT.                                        IM008
002600                 09  SI-LM-DT-YY     PIC XX.                      IM008
002700                 09  SI-LM-DT-MM     PIC XX.                      IM008
002800                 09  SI-LM-DT-DD     PIC XX.                      IM008
002900         05  SI-LM-TIME.                                          IM008
003000             07  SI-LM-HH-MM-SS      PIC S9(7)   COMP-3.          IM008
003100     03  SI-ENVIRONMENT-DATA.                                     IM008
003200         05  SI-ENVIRONMENT-CICS     PIC X(01).                   IM008
003300             88  SI-88-ENVIRONMENT-NOT-ONLINE    VALUE '0'.       IM008
003400             88  SI-88-ENVIRONMENT-ONLINE        VALUE '1'.       IM008
003500         05  SI-ENVIRONMENT-VSAM     PIC X(01).                   IM008
003600             88  SI-88-ENVIRONMENT-SEQ           VALUE '0'.       IM008
003700             88  SI-88-ENVIRONMENT-VSAM          VALUE '1'.       IM008
003800         05  SI-ENVIRONMENT-ALPH     PIC X(01).                   IM008
003900             88  SI-88-ENVIRONMENT-NO-ALPHA      VALUE '1'.       IM008
004000         05  SI-ENVIRONMENT-EXCM     PIC X(01).                   IM008
004100             88  SI-88-ENVIRONMENT-EXCM          VALUE '1'.       IM008
004200         05  SI-ENVIRONMENT-STOP     PIC X(01).                   IM008
004300             88  SI-88-ENVIRONMENT-STOPS         VALUE '1'.       IM008
004400         05  SI-ENVIRONMENT-HIST     PIC X(01).                   IM008
004500             88  SI-88-ENVIRONMENT-HISTORY       VALUE '1'.       IM008
004600         05  SI-ENVIRONMENT-CHKS     PIC X(01).                   IM008
004700             88  SI-88-ENVIRONMENT-CONSOLE       VALUE '0'.       IM008
004800             88  SI-88-ENVIRONMENT-0-CHECK       VALUE '1'.       IM008
004900         05  SI-ENVIRONMENT-PSW      PIC X(01).                   IM008
005000             88  SI-88-ENVIRONMENT-PSW           VALUE '1'.       IM008
005100             88  SI-88-ENVIRONMENT-IM31          VALUE '2'.       IM008
005200         05  SI-ENVIRONMENT-SQVS     PIC X(01).                   IM008
005300             88  SI-88-ENVIRONMENT-SQVS          VALUE '1'.       IM008
