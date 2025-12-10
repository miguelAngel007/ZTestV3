*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*--------------------------------------------------------------*
000200*                                                              *
000300*   SYSTEM TOTALS EXCEPTION RECORD 1 - CREATED BY IM31         *
000400*                                                              *
000500*--------------------------------------------------------------*
000600
000700       08  DDA-TOT-REC-GL            PIC 99.
000800       08  DDA-TOT-REC-ID            PIC X.
000900       08  DDA-COMP-DATA                             COMP-3.
001000         09  DDA-END-NO-ACCTS        PIC S9(9).                   9915845
001100         09  DDA-END-BAL             PIC S9(15)V99.               9915845
001200         09  DDA-END-ZERO-BAL        PIC S9(9).                   9915845
001300         09  DDA-END-CLOSED          PIC S9(9).                   9915845
001400         09  DDA-END-TERMIN          PIC S9(9).                   9915845
001500         09  DDA-END-DORM-NO         PIC S9(9).                   9915845
001600         09  DDA-END-DORM-BAL        PIC S9(15)V99.               9915845
001700         09  DDA-END-INACT-NO        PIC S9(9).                   9915845
001800         09  DDA-END-INACT-BAL       PIC S9(15)V99.               9915845
001900         09  DDA-END-MKT-NO          PIC S9(9).                   9915845
002000         09  DDA-END-MKT-BAL         PIC S9(15)V99.               9915845
002100         09  DDA-END-INT-NO          PIC S9(9).                   9915845
002200         09  DDA-END-INT-BAL         PIC S9(15)V99.               9915845
002300         09  DDA-END-BANK-AVAIL      PIC S9(15)V99.               9915845
002400         09  DDA-END-CUST-AVAIL      PIC S9(15)V99.               9915845
002500         09  DDA-ANALYSIS-NO         PIC S9(9).                   9915845
002600         09  DDA-ARP-NO              PIC S9(9).                   9915845
002700         09  DDA-NEW-ACCTS           PIC S9(9).                   9915845
002800         09  DDA-CLSD-ACCTS          PIC S9(9).                   9915845
002900         09  DDA-TERM-ACCTS          PIC S9(9).                   9915845
003000         09  DDA-DROP-ACCTS          PIC S9(9).                   9915845
003100         09  DDA-DB-NO               PIC S9(9).                   9915845
003200         09  DDA-DB-AMT              PIC S9(15)V99.               9915845
003300         09  DDA-UP-DB-NO            PIC S9(9).                   9915845
003400         09  DDA-UP-DB-AMT           PIC S9(15)V99.               9915845
003500         09  DDA-CR-NO               PIC S9(9).
003600         09  DDA-CR-AMT              PIC S9(15)V99.               9915845
003700         09  DDA-UP-CR-NO            PIC S9(9).                   9915845
003800         09  DDA-UP-CR-AMT           PIC S9(15)V99.               9915845
003900         09  DDA-SVC-CHG-NO          PIC S9(9).                   9915845
004000         09  DDA-SVC-CHG-AMT         PIC S9(15)V99.               9915845
004100         09  DDA-CHK-CHG-NO          PIC S9(9).                   9915845
004200         09  DDA-CHK-CHG-AMT         PIC S9(15)V99.               9915845
004300         09  DDA-LIMIT-TRAN-NO       PIC S9(9).                   9915845
004400         09  DDA-LIMIT-TRAN-AMT      PIC S9(15)V99.               9915845
004500         09  DDA-OD-CHG-NO           PIC S9(9).                   9915845
004600         09  DDA-OD-CHG-AMT          PIC S9(15)V99.               9915845
004700         09  DDA-NSF-CHG-NO          PIC S9(9).                   9915845
004800         09  DDA-NSF-CHG-AMT         PIC S9(15)V99.               9915845
004900         09  DDA-INT-PAID-NO         PIC S9(9).                   9915845
005000         09  DDA-INT-PAID-AMT        PIC S9(15)V99.               9915845
005100         09  DDA-LOAN-NO             PIC S9(9).                   9915845
005200         09  DDA-LOAN-AMT            PIC S9(15)V99.               9915845
005300         09  DDA-LOAN-OVRPMT-NO      PIC S9(9).                   9915845
005400         09  DDA-LOAN-OVRPMT-AMT     PIC S9(15)V99.               9915845
005500         09  DDA-LOAN-PMT-NO         PIC S9(9).                   9915845
005600         09  DDA-LOAN-PMT-AMT        PIC S9(15)V99.               9915845
005700         09  DDA-MAINT-ACC           PIC S9(9).                   9915845
005800         09  DDA-MAINT-REJ           PIC S9(9).                   9915845
005900         09  DDA-INT-ACC-TODAY       PIC S9(15)V99.               9915845
006000         09  DDA-INT-PAID-TODAY      PIC S9(15)V99.               9915845
006100         09  DDA-INT-ACC-MTD         PIC S9(15)V99.               9915845
006200         09  DDA-INT-PAID-MTD        PIC S9(15)V99.               9915845
006300         09  DDA-INT-ACC-CY          PIC S9(15)V99.               9915845
006400         09  DDA-INT-PAID-YTD        PIC S9(15)V99.               9915845
006500         09  DDA-INT-RPTD-YTD        PIC S9(15)V99.               9915845
006600         09  DDA-INT-ACC-ADJ-EXT     PIC S9(15)V99.               9915845
006700         09  DDA-INT-ACC-ADJ-INT     PIC S9(15)V99.               9915845
006800         09  DDA-INT-YTD-ADJ         PIC S9(15)V99.               9915845
006900         09  DDA-BANK-AVAIL-AMT      PIC S9(15)V99.               9915845
007000         09  DDA-BANK-AVAIL-BUMP     PIC S9(15)V99.               9915845
007100         09  DDA-CUST-AVAIL-AMT      PIC S9(15)V99.               9915845
007200         09  DDA-CUST-AVAIL-BUMP     PIC S9(15)V99.               9915845
007300         09  DDA-BANK-AVAIL-ADJ      PIC S9(15)V99.               9915845
007400         09  DDA-CUST-AVAIL-ADJ      PIC S9(15)V99.               9915845
007500         09  DDA-NO-ACTIVE           PIC S9(9).
007600         09  DDA-TARGET-PARNT-NO-DB  PIC S9(9).                   9915845
007700         09  DDA-TARGET-PARNT-AMT-DB PIC S9(15)V99.               9915845
007800         09  DDA-TARGET-PARNT-NO-CR  PIC S9(9).                   9915845
007900         09  DDA-TARGET-PARNT-AMT-CR PIC S9(15)V99.               9915845
008000         09  DDA-TARGET-SUBAC-NO-DB  PIC S9(9).                   9915845
008100         09  DDA-TARGET-SUBAC-AMT-DB PIC S9(15)V99.               9915845
008200         09  DDA-TARGET-SUBAC-NO-CR  PIC S9(9).                   9915845
008300         09  DDA-TARGET-SUBAC-AMT-CR PIC S9(15)V99.               9915845
008400         09  DDA-STMT-PAGES.
008500             11  DDA-STMT-FORMAT     PIC S9(9)   OCCURS 6 TIMES.  9915845
008600       08  DDA-AN-DATA                               DISPLAY.
008700         09  DDA-STMT-CYCLE-PROT.
008800             11  DDA-CYCLE           PIC X       OCCURS 50 TIMES.
008900         09  DDA-STMT-FORM-PROT.
009000             11  DDA-FORMAT          PIC X       OCCURS 6 TIMES.
009100       08  DDA-COMP-DATA2                            COMP-3.
009200         09  DDA-SVC-CHG-WAV-NO      PIC S9(9).                   9915845
009300         09  DDA-SVC-CHG-WAV-AMT     PIC S9(15)V99.               9915845
009400         09  DDA-BEG-OD-NO           PIC S9(9).                   9915845
009500         09  DDA-BEG-OD-AMT          PIC S9(15)V99.               9915845
009600         09  DDA-END-OD-NO           PIC S9(9).                   9915845
009700         09  DDA-END-OD-AMT          PIC S9(15)V99.               9915845
009800*  IOD-DATA
009900         09  IOD-INT-ACC-TODAY       PIC S9(15)V99.               9915845
010000         09  IOD-INT-PAID-TODAY      PIC S9(15)V99.               9915845
010100         09  IOD-INT-ACC-MTD         PIC S9(15)V99.               9915845
010200         09  IOD-INT-PAID-MTD        PIC S9(15)V99.               9915845
010300         09  IOD-INT-ACC-CY          PIC S9(15)V99.               9915845
010400         09  IOD-INT-PAID-YTD        PIC S9(15)V99.               9915845
010500         09  IOD-INT-RPTD-YTD        PIC S9(15)V99.               9915845
010600         09  IOD-INT-ACC-ADJ-EXT     PIC S9(15)V99.               9915845
010700         09  IOD-INT-ACC-ADJ-INT     PIC S9(15)V99.               9915845
010800         09  IOD-INT-YTD-ADJ         PIC S9(15)V99.               9915845
010900         09  IOD-END-NO-ACCTS        PIC S9(9).                   9915845
011000         09  IOD-INT-PAID-NO         PIC S9(9).                   9915845
011100         09  IOD-INT-PAID-AMT        PIC S9(15)V99.               9915845
011200         09  IOD-TRANSFER-TO-NO      PIC S9(9).                   9915845
011300         09  IOD-TRANSFER-FROM-NO    PIC S9(9).                   9915845
011400         09  IOD-TRANSFER-TO-AMT     PIC S9(15)V99.               9915845
011500         09  IOD-TRANSFER-FROM-AMT   PIC S9(15)V99.               9915845
011600         09  IOD-DB-NO               PIC S9(9).                   9915845
011700         09  IOD-CR-NO               PIC S9(9).                   9915845
011800         09  IOD-DB-AMT              PIC S9(15)V99.               9915845
011900         09  IOD-CR-AMT              PIC S9(15)V99.               9915845
012000         09  IOD-END-BAL             PIC S9(15)V99.               9915845
012100         09  IOD-NEW-ACCTS           PIC S9(9).                   9915845
012200         09  IOD-DROP-ACCTS          PIC S9(9).                   9915845
012300         09  IOD-END-INT-BAL         PIC S9(15)V99.               9915845
012400         09  IOD-END-INT-NO          PIC S9(9).                   9915845
012500*
012600         09  DDA-GEN-CLS-DR-NO       PIC S9(9).                   9915845
012700         09  DDA-GEN-CLS-DR-AMT      PIC S9(15)V99.               9915845
012800         09  DDA-GEN-CLS-CR-NO       PIC S9(9).                   9915845
012900         09  DDA-GEN-CLS-CR-AMT      PIC S9(15)V99.               9915845
013000*
013100         09  DDA-END-CASH-AVAIL      PIC S9(15)V99.               9915845
013200         09  DDA-CASH-AVAIL-AMT      PIC S9(15)V99.               9915845
013300         09  DDA-CASH-AVAIL-BUMP     PIC S9(15)V99.               9915845
013400         09  DDA-CASH-AVAIL-ADJ      PIC S9(15)V99.               9915845
013410         09  IOD-INT-ACC-YTD         PIC S9(15)V99.               9915845
013420         09  DDA-INT-ACC-YTD         PIC S9(15)V99.               9915845
013430         09  INV-DIV-ACCRD-YTD       PIC S9(15)V99.               9915845
013440         09  LOAN-END-YTD-ERND       PIC S9(15)V99.               9915845
013450         09  OD-ACCRUED-YTD          PIC S9(15)V99.               9915845
013500         09  FILLER                  PIC S9(9).                   0337109
013600         09  FILLER                  PIC S9(15)V99.               0337109
013700         09  SERV-CHG-DORM-NO        PIC S9(9).                   9915845
013800         09  SERV-CHG-DORM-AMT       PIC S9(15)V99.               9915845
013900         09  NO-LN-GEN-DR-CAP-INT   PIC S9(9).                    9915852
014000         09  AMT-LN-GEN-DR-CAP-INT  PIC S9(15)V99.                9915852
014100         09  NO-LN-GEN-CR-CAP-INT   PIC S9(9).                    9915852
014200         09  AMT-LN-GEN-CR-CAP-INT  PIC S9(15)V99.                9915852
014300         09  DDA-CMA-FEES-NBR       PIC S9(9).                    9916072
014400         09  DDA-CMA-FEES-AMT       PIC S9(15)V99.                9916072
014500         09  DDA-CMA-FEES-NBR-WV    PIC S9(9).                    9916072
014600         09  DDA-CMA-FEES-AMT-WV    PIC S9(15)V99.                9916072
014610         09  DUAL-SV-INT-PAID-YTD   PIC S9(15)V99.                9915858
014620         09  DUAL-SV-INT-YTD-ADJ    PIC S9(15)V99.                9915858
014630         09  DUAL-IO-INT-PAID-YTD   PIC S9(15)V99.                9915858
014640         09  DUAL-IO-INT-YTD-ADJ    PIC S9(15)V99.                9915858
014700       08  FILLER                   PIC X(1919).                  0427139
