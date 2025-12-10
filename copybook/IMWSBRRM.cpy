*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*
000200*    BALANCE REPORTING INTERFACE FILE FOR ENCORE+                *
000300*----------------------------------------------------------------*
000400 01  BANK-REPORTING-RECORD.
000500     05  BRR-CONTROL-KEY.
000600         10  BRR-CTL-1-3.
000700             20  BRR-CTL-1          PIC X(02).
000800             20  BRR-CTL-2          PIC X(03).
000900             20  BRR-CTL-3          PIC X(03).
000910         10  BRR-CTL4-ACCT.                                       0417078
001000             20  BRR-CTL-4          PIC X(04).                    0417078
001100             20  BRR-ACCT-NO        PIC X(10).                    0417078
001200     05  BRR-EXC-CODE               PIC X(01).
001300     05  BRR-RESERVE-RECLASS-DATA.                                9916105
001305         10  BRR-REGION             PIC X(03).                    9916105
001310         10  BRR-OFFICER            PIC X(05).                    9916105
001315         10  BRR-STATUS             PIC X(02).                    9916105
001320         10  BRR-DDA-BAL            PIC S9(13)V99  COMP-3.        9916105
001325         10  BRR-SC-AGGR-DAYS       PIC S9(03)     COMP-3.        9916105
001330         10  BRR-SC-AGGR-COLL       PIC S9(15)V99  COMP-3.        9916105
001335         10  BRR-GL-KEY.                                          9916105
001340             15  BRR-GL-CODE        PIC  X(02).                   9916105
001345             15  BRR-GL-USER-DEF    PIC  X(10).                   9916105
001350             15  BRR-GL-COST-CENTER PIC  X(30).                   9916105
001355             15  FILLER             REDEFINES BRR-GL-COST-CENTER. 9916105
001360                 20  BRR-GL-COST-CTR PIC X(05)  OCCURS 6 TIMES.   9916105
001365         10  BRR-DATE-LAST-SERV-CHG PIC  9(06).                   9916105
001370         10  BRR-NB-THRESHOLD       PIC S9(13)V99    COMP-3.      9916105
001375         10  BRR-IOD-ACCRD-TODAY    PIC S9(11)V9(06) COMP-3.      9916105
001380         10  BRR-COLL-BAL           PIC S9(13)V99    COMP-3.      9916105
001385     05  BRR-OTHER-RECORD-DATA.                                   9916105
001400         10  BRR-NB-DC-TMI-AREA.
001500             15  BRR-NB-DC-ABM          PIC X(02).                9916105
001600             15  BRR-NB-DC-ACH-ORIG     PIC X(02).                9916105
001700             15  BRR-NB-DC-CDA          PIC X(02).                9916105
001800             15  BRR-NB-DC-EDI          PIC X(02).                9916105
001900             15  BRR-NB-DC-LOCKBOX      PIC X(02).                9916105
002000             15  BRR-NB-DC-NBW-BAL-CURR PIC X(02).
002100             15  BRR-NB-DC-NBW-BAL-PRV  PIC X(02).                9916105
002200             15  BRR-NB-DC-NBW-INQ      PIC X(02).                9916105
002300             15  BRR-NB-DC-NBW-ST-PAY   PIC X(02).                9916105
002400             15  BRR-NB-DC-WIRE         PIC X(02).                9916105
002500             15  BRR-NB-DC-BUS-EXPR     PIC X(02).                9916105
002600             15  BRR-NB-DC-VLT-SVCS     PIC X(02).                9916105
002700             15  BRR-NB-DC-DEP-PLUS     PIC X(02).                9916105
002800             15  BRR-NB-DC-RSVE4        PIC X(02).                9916105
002900             15  BRR-NB-DC-RSVE5        PIC X(02).                9916105
003000         10  BRR-ARP-CODE           PIC X(02).
003100         10  BRR-NB-PTD-IND         PIC X(02).
003200         10  BRR-NB-ZB-ZBA-TBA-OPT  PIC X(02).
003300         10  BRR-NB-TFR-IND         PIC X(02).
003700         10  BRR-SPECIAL-ROUTINE    PIC S9(03)     COMP-3.
003800         10  BRR-DDA-BALANCE-IN     PIC S9(13)V99  COMP-3.        9915845
004000         10  BRR-CURR-BAL           PIC S9(13)V99  COMP-3.        9915845
005000         10  BRR-DATE-LAST-MAINT    PIC  9(06).
005300         10  BRR-BANK-AVAIL-AMT     PIC S9(13)V99  COMP-3.        9915845
005400         10  BRR-COLL-AVAIL-BAL     PIC S9(13)V99  COMP-3.        9915845
005600         10  BRR-AVG-COLL-BAL       PIC S9(13)V99  COMP-3.
005800         10  BRR-TODAYS-DEBITS      PIC S9(13)V99  COMP-3.        9915845
005900         10  BRR-TODAYS-CREDITS     PIC S9(13)V99  COMP-3.        9915845
006000         10  BRR-BANK-AVAIL-TR-AMT1 PIC S9(13)V99  COMP-3.        9915845
006100         10  BRR-BANK-AVAIL-TR-AMT2 PIC S9(13)V99  COMP-3.        9915845
006200         10  BRR-BANK-AVAIL-TR-AMT3 PIC S9(13)V99  COMP-3.        9915845
006300         10  BRR-BANK-AVAIL-TR-AMT4 PIC S9(13)V99  COMP-3.        9915845
006400         10  BRR-BANK-AVAIL-TR-AMT5 PIC S9(13)V99  COMP-3.        9915845
006500         10  BRR-BANK-AVAIL-TR-AMT6 PIC S9(13)V99  COMP-3.        9915845
006600         10  BRR-BANK-AVAIL-TR-AMT7 PIC S9(13)V99  COMP-3.        9915845
006700         10  BRR-MTD-AGGR-DAYS      PIC S9(3)      COMP-3.        9915845
006800         10  BRR-AVG-BAL            PIC S9(13)V99  COMP-3.
006900         10  BRR-INT-TRANS-ACCT-IND PIC  X.                       2010312
007000         10  FILLER                 PIC  X.                       2010312
