*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000010******************************************************************0326947
000011* IM31 WRITES THE IMTAGT FILE USING THIS COPYBOOK. IM32 READS THE 0326947
000012* IMTAGT RECORD USING A HARD CODED AREA CALLED                    0326947
000013* ARP-RPT-EXC-REC-AREA.  ANY CHANGES TO THIS COPY BOOK NEED TO BE 0326947
000015* MADE IN IM32, IMEXTRAN, AND DAG IMTAG. ALSO NEED TO CHECK IM33  0326947
000016* AND IM34 AND A HARD CODED AREA CALLED WS-REPORT-RECORD-LAYOUTS. 0326947
000017******************************************************************0326947
000100 01  EXCEPTION-AREA.
000200   02  IMEX-LENGTH                   PIC S9999   COMP SYNC.
000300   02  IMEX-BIN0                     PIC XX.
000400   02  DDA-EXCEPT-RECORD.
000500     03  IMEX-KEY.
000600         05  IMEX-CTL1               PIC XX.
000700         05  IMEX-CTL2               PIC XXX.
000800         05  IMEX-CTL3               PIC XXX.
000810         05  IMEX-CTL4-ACCT.                                      0417078
000900             07  IMEX-CTL4           PIC XXXX.                    0417078
001000             07  IMEX-ACCT-NO        PIC X(10).                   0417078
001100     03  IMEX-REC-NO                 PIC XX.
001200     03  IMEX-REC-NO9 REDEFINES IMEX-REC-NO          PIC 99.
001300     03  IMEX-EX-CODE.
001400         05  IMEX-CODE-1             PIC XX.
001500         05  IMEX-CODE-1N REDEFINES IMEX-CODE-1      PIC 99.
001600         05  IMEX-CODE-2             PIC XX.
001700     03  IMEX-SEQUENCE               PIC S999        COMP-3.
001710     03  IMEX-MAINT-HIST             PIC X.                       0326947
001720     03  IMEX-MAINT-HIST-RET         PIC S999        COMP-3.      0326947
001750     03  FILLER                      PIC X(8).                    0717565
001800     03  IMEX-RECORD                 PIC X(3007).                 0326947
001900     03  IMEX-NA-TRLR REDEFINES IMEX-RECORD          PIC X(336).
002000     03  IMEX-REC-01 REDEFINES IMEX-RECORD.
002100         05  EX01-AMT                PIC S9(13)V99    COMP-3.
002200         05  EX01-INT-CODE           PIC XX.
002300         05  EX01-USER-CODE          PIC XX.
002400         05  EX01-TYPE               PIC X.
002500         05  EX01-STMT-SYM           PIC XX.
002600         05  EX01-ACCM-IND1          PIC S9(3)       COMP-3.
002700         05  EX01-ACCM-IND2          PIC S9(3)       COMP-3.
002800         05  EX01-SEQ-NO             PIC S9(5)       COMP-3.
002900         05  EX01-NUMBER             PIC S9(5)       COMP-3.
003000         05  EX01-TRACE              PIC S9(7)       COMP-3.
003100         05  EX01-LOAN-DISB          PIC X(48).
003200         05  FILLER REDEFINES EX01-LOAN-DISB.
003300             07  EX01-OD-CYC-ACCR    PIC S9(11)V9(6) COMP-3.
003400             07  EX01-SC-CTR-OTH     PIC S9(7)       COMP-3.
003500             07  EX01-SC-TOT-EC      PIC S9(13)V99   COMP-3.
003600             07  EX01-SC-RESV-AMT    PIC S9(13)V99   COMP-3.
003700             07  EX01-SC-EC-BAL      PIC S9(13)V99   COMP-3.
003800             07  EX01-SC-EC-RATE     PIC S9V9(8)     COMP-3.
003900             07  FILLER              PIC X(6).
004000         05  EX01-EFFECTIVE-DATE.
004100             07  EX01-EFFECT-MO      PIC XX.
004200             07  EX01-EFFECT-DA      PIC 99.
004300             07  EX01-EFFECT-YR      PIC XX.
004310         05  EX01-TAX-TYPE           PIC X.                       2012254
004320         05  EX01-COMB-PLN-CODES.                                 0617360
004322             07  EX01-PLN-CODE       PIC XX.                      0617360
004324             07  EX01-PLN-CODE2      PIC XX.                      0617360
004326         05  EX01-PLN-CAL-YR         PIC X.                       0617360
004328         05  EX01-PLN-TAX-YR         PIC X.                       0617360
004330         05  FILLER                  PIC X(2917).                 0617360
004400     03  IMEX-REC-02 REDEFINES IMEX-RECORD.
004500         05  EX02-BAL                PIC S9(13)V99    COMP-3.
004600         05  EX02-BAL-ACCR1 REDEFINES EX02-BAL
004700                                     PIC S9(9)V9(6)   COMP-3.
005000         05  EX02-DATE-LAST REDEFINES EX02-BAL   PIC X(6).        9915845
005100         05  EX02-SEQ-NO             PIC S9(5)        COMP-3.     9915845
005200         05  EX02-SRC-INFORMATION.                                9915845
005300             07  EX02-SRC-FLAG       PIC X.                       9915845
005400             07  EX02-SRC-INFO       PIC X(44).                   9915239
005410         05  FILLER                  PIC X(2951).                 0326947
005500     03  IMEX-REC-03 REDEFINES IMEX-RECORD.                       9915845
005600         05  EX03-TYP-NO.                                         9915845
005700             07  EX03-TYP            PIC X.                       9915845
005800             07  EX03-NO             PIC 99.                      9915845
005900             07  FILLER              PIC XX.                      9915845
006000         05  FILLER REDEFINES EX03-TYP-NO.                        9915845
006100             07  EX03-IBT-OLD-DDA-STATUS  PIC XX.                 9915845
006200             07  EX03-IBT-OLD-LOAN-STATUS PIC X.                  9915845
006300             07  EX03-IBT-OLD-SAV-STATUS  PIC X.                  9915845
006400             07  EX03-IBT-OLD-OD-STATUS   PIC X.                  9915845
006500         05  EX03-OLD-PRIME REDEFINES EX03-TYP-NO                 9915845
006600                                     PIC S9V9(8) COMP-3.          9915845
006700         05  EX03-CODE               PIC X.                       9915845
006800         05  EX03-RESN               PIC X.                       9915845
006900         05  EX03-SRC-INFORMATION.                                9915845
007000             07  EX03-SRC-FLAG       PIC X.                       9915845
007100             07  EX03-SRC-INFO       PIC X(44).                   9915239
007105         05  FILLER                  PIC X(2955).                 0326947
007200     03  IMEX-REC-04 REDEFINES IMEX-RECORD.                       9915845
007300         05  EX04-AMT                PIC S9(13)V99   COMP-3.      9915845
007400         05  EX04-AMT-ACCR1    REDEFINES EX04-AMT                 9915845
007500                                     PIC S9(9)V9(6)  COMP-3.      9915845
007800         05  EX04-NO REDEFINES EX04-AMT  PIC S9(15)  COMP-3.      9915845
007900         05  EX04-DATA-1.                                         9915845
008000             07  EX04-TR-CODE        PIC XX.                      9915845
008100             07  EX04-FIELD          PIC XX.                      9915845
008200             07  EX04-FIELD-9  REDEFINES EX04-FIELD  PIC 99.      9915845
008300             07  EX04-FIELD-X  REDEFINES EX04-FIELD.              9915845
008400                 09  EX04-FIELD-P    PIC S9(03)      COMP-3.      9915845
008500             07  FILLER              PIC XX.                      9915845
008600         05  EX04-RATE-DATA REDEFINES EX04-DATA-1.                9915845
008700             07  EX04-TYPE           PIC X.                       9915845
008800             07  EX04-INT-RATE       PIC S9V9(8)     COMP-3.      9915845
008900       04  EX05-TR50.                                             9915845
009000         05  EX04-SOURCE             PIC XX.                      9915845
009100         05  EX04-BATCH              PIC S9(5)       COMP-3.      9915845
009200         05  EX04-SEQ                PIC S9(5)       COMP-3.      9915845
009300         05  EX04-TRACE              PIC S9(7)       COMP-3.      9915845
009400         05  EX04-NEW-AMT            PIC S9(13)V99   COMP-3.      9915845
009500         05  EX04-NEW-AMTX REDEFINES EX04-NEW-AMT    PIC X(8).    9915845
009600         05  EX04-NEW-NO REDEFINES EX04-NEW-AMT                   9915845
009700                                     PIC S9(15)      COMP-3.      9915845
009800         05  IM-EXC-REF-NUM          PIC S9(15)      COMP-3.      9915845
009810         05  EX04-DATE-LAST-MAINT.                                9916299
009820             07  EX04-LST-MAINT-MO   PIC XX.                      9916299
009830             07  EX04-LST-MAINT-DA   PIC XX.                      9916299
009840             07  EX04-LST-MAINT-YR   PIC XX.                      9916299
009900         05  EX04-SRC-INFORMATION.                                9915845
010000             07  EX04-SRC-FLAG       PIC X.                       9915845
010100             07  EX04-SRC-INFO       PIC X(44).                   9915239
010110         05  FILLER                   PIC X(2914).                0326947
010200     03  IMEX-REC-05 REDEFINES IMEX-RECORD.                       9915845
010300         05  EX05-EX04               PIC X(34).                   0326947
010400         05  EX05-ORIG-DATE          PIC X(6).                    9915845
010500         05  FILLER                  PIC X.                       9915845
010600         05  EX05-ORIG-DA-WK         PIC X.                       9915845
010700         05  EX05-ORIG-HOL           PIC X.                       9915845
010800         05  EX05-TYPE               PIC X.                       9915845
010900         05  EX05-ADJ-AMT            PIC S9(13)V99    COMP-3.     9915845
011000         05  EX05-ANALYSIS           PIC X.                       9915845
011100         05  EX05-REF-NUM            PIC S9(15)      COMP-3.      9915845
011110         05  EX05-DATE-LAST-MAINT.                                0266748
011112             07  EX05-LST-MAINT-MO   PIC XX.                      0266748
011114             07  EX05-LST-MAINT-DA   PIC XX.                      0266748
011116             07  EX05-LST-MAINT-YR   PIC XX.                      0266748
011200         05  EX05-SRC-INFORMATION.                                9915845
011300             07  EX05-SRC-FLAG       PIC X.                       9915845
011400             07  EX05-SRC-INFO       PIC X(44).                   9915239
011410         05  FILLER                   PIC X(2895).                0326947
011500     03  IMEX-REC-06 REDEFINES IMEX-RECORD.                       9915845
011600         05  FILLER                  PIC X(61).                   9915845
011700         05  EX06-BEG-BAL            PIC S9(13)V99   COMP-3.      9915845
011800         05  EX06-CR-NO              PIC S9(7)       COMP-3.      9915845
011900         05  EX06-CR-AMT             PIC S9(13)V99   COMP-3.      9915845
012000         05  EX06-DB-NO              PIC S9(7)       COMP-3.      9915845
012100         05  EX06-DB-AMT             PIC S9(13)V99   COMP-3.      9915845
012200         05  EX06-SVC-CHG            PIC S9(13)V99   COMP-3.      9915845
012300         05  EX06-ITEMS              PIC S9(7)       COMP-3.      9915845
012400         05  EX06-LINES              PIC S9(3)       COMP-3.      9915845
012500         05  EX06-PAGES              PIC S9(3)       COMP-3.      9915845
012600         05  EX06-END-BAL            PIC S9(13)V99   COMP-3.      9915845
012700         05  EX06-LAST-STMT          PIC X(6).                    9915845
012800         05  EX06-TR-SEQ             PIC S9(7)       COMP-3.      9915845
012900         05  EX06-FORMAT             PIC S9          COMP-3.      9915845
013000         05  EX06-STMT-LINES         PIC S999        COMP-3.      9915845
013010         05  EX06-WAIVED-SC-RP       PIC S9(11)V99   COMP-3.      0347045
013100         05  EX06-DDA-BAL            PIC S9(13)V99   COMP-3.      9915845
013200         05  EX06-SAV-BAL            PIC S9(13)V99   COMP-3.      9915845
013300         05  EX06-LOAN-BAL           PIC S9(13)V99   COMP-3.      9915845
013400         05  EX06-LOAN-PRIN-BAL      PIC S9(13)V99   COMP-3.      9915845
013500         05  EX06-LOAN-PREV-BAL      PIC S9(13)V99   COMP-3.      9915845
013510         05  FILLER                  PIC X(2830).                 0347045
013600     03  IMEX-REC-07 REDEFINES IMEX-RECORD.                       9915845
013610       04  EX07-PSBK-DATA.                                        0427139
013700         05  EX07-COMB-FILE          PIC X.                       9915845
013800         05  EX07-ZIP-CODE           PIC X(10).                   9915845
013900         05  EX07-PRIMARY-ACCT.                                   9915845
014000             07  EX07-PRIM-CTL1      PIC XX.                      9915845
014100             07  EX07-PRIM-CTL2      PIC XXX.                     9915845
014200             07  EX07-PRIM-CTL3      PIC XXX.                     9915845
014210             07  EX07-PRIM-CTL4-ACCT.                             0417078
014300                 09  EX07-PRIM-CTL4  PIC XXXX.                    0417078
014400                 09  EX07-PRIM-ACCT  PIC X(10).                   0417078
014500         05  EX07-ADDR-PTR           PIC S999        COMP-3.      9915845
014510         05  EX07-NA-COUNTRY         PIC XX.                      9915845
014600         05  EX07-LN-TRAILER         PIC X.                       9915845
014700         05  EX07-SV-TRAILER         PIC X.                       9915845
014800         05  EX07-COMB-ONLY          PIC X.                       9915845
014900         05  EX07-IOD-PTR            PIC S999        COMP-3.      9915845
015000         05  EX07-HIFI-IND           PIC S999        COMP-3.      9915845
015100         05  EX07-SV-RATE-PTR        PIC S999        COMP-3.      9915845
015200         05  EX07-LN-RATE-PTR        PIC S999        COMP-3.      9915845
015300         05  EX07-LN-SPLIT-SCHED     PIC S999        COMP-3.      9915845
015400         05  EX07-LN-BILL-CY-DAYS    PIC S999        COMP-3.      9915845
015500         05  EX07-LN-BILL-CY-DATE    PIC X(6).                    9915845
015600         05  EX07-LN-STMT-CY-DAYS    PIC S999        COMP-3.      9915845
015700         05  EX07-SV-INV-IND         PIC X.                       9915845
015800         05  EX07-REQ-BY             PIC XXX.                     9915845
015900         05  EX07-RESET              PIC X.                       9915845
016000         05  EX07-FORMAT             PIC S9          COMP-3.      9915845
016100         05  EX07-SERIAL             PIC X.                       9915845
016200         05  EX07-CMB-ST-TRLRS       PIC S999        COMP-3.      9915845
016300         05  EX07-STMT-PAGES         PIC S999        COMP-3.      9915845
016400         05  EX07-BEG-BAL            PIC S9(13)V99   COMP-3.      9915845
016500         05  EX07-BEG-SAV-BAL        PIC S9(13)V99   COMP-3.      9915845
016600         05  EX07-STMT-LINES         PIC S999        COMP-3.      9915845
016700         05  EX07-STMT-PG-LINES      PIC S999        COMP-3.      9915845
016800         05  EX07-STMT-DATE.                                      9915845
016900             07  EX07-STMT-MO        PIC XX.                      9915845
017000             07  EX07-STMT-DA        PIC 99.                      9915845
017100             07  EX07-STMT-YR        PIC XX.                      9915845
017200         05  EX07-DDA-BAL            PIC S9(13)V99   COMP-3.      9915845
017300         05  EX07-SAV-BAL            PIC S9(13)V99   COMP-3.      9915845
017400         05  EX07-LOAN-BAL           PIC S9(13)V99   COMP-3.      9915845
017500         05  EX07-LOAN-PRIN-BAL      PIC S9(13)V99   COMP-3.      9915845
017600         05  EX07-LOAN-PREV-BAL      PIC S9(13)V99   COMP-3.      9915845
017700       04  EX07-SRC-INFORMATION.                                  0427139
017800             07  EX07-SRC-FLAG       PIC X.                       9915845
017900             07  EX07-SRC-INFO       PIC X(44).                   9915239
017910       04  FILLER                    PIC X(2825).                 0427139
018000     03  IMEX-REC-08 REDEFINES IMEX-RECORD.                       9915845
018100         05  EX08-SPEC-INSTR         PIC X(52).                   9915845
018200         05  FILLER REDEFINES EX08-SPEC-INSTR.                    9915845
018300             07  EX08-CNTL           PIC X(35).                   9915845
018400             07  EX08-END-BAL        PIC S9(13)V99    COMP-3.     9915845
018500             07  EX08-FL-TO-PRNT     PIC X.                       9915845
018600             07  EX08-DR-CR-IND      PIC X.                       9915845
018700             07  EX08-ZB-LEV-NO      PIC X.                       9915845
018800             07  EX08-ZB-REL-NO      PIC X(6).                    9915845
018810         05  EX08-ZBA-PARENT-INFO.                                0326947
018820             07  EX08-ZBA-PRNT-TARGET-BAL   PIC S9(13)V99 COMP-3. 0326947
018830             07  EX08-ZBA-PRNT-SUMM-OPT     PIC X(01).            0326947
018840             07  EX08-ZBA-PRNT-TRAN-NOTIFY  PIC X(01).            0326947
018850             07  EX08-ZBA-PRNT-FRCED-REF-NO PIC X(10).            0326947
018852         07  EX08-ZBA-ULTMT-PRNT-INFO.                            0436790
018853             09  EX08-ZBA-ULTMT-PRNT-CTL PIC X(22).               0436790
018860         05  FILLER                         PIC X(2913).          0436790
018900     03  IMEX-REC-09 REDEFINES IMEX-RECORD.                       9915845
019000         05  EX09-REJ-PROG           PIC XX.                      9915845
019100         05  EX09-STATUS-IN REDEFINES EX09-REJ-PROG  PIC XX.      9915845
019200         05  EX09-REJ-RESN           PIC XXX.                     0717565
019300         05  EX09-STATUS-OUT REDEFINES EX09-REJ-RESN PIC XXX.     0717565
019400         05  EX09-SOURCE             PIC XX.                      9915845
019500         05  EX09-BATCH              PIC S9(5)       COMP-3.      9915845
019600         05  EX09-SEQ                PIC S9(5)       COMP-3.      9915845
019700         05  EX09-TRACE              PIC S9(7)       COMP-3.      9915845
019800         05  EX09-TR-CODE            PIC XX.                      9915845
019900         05  EX09-REMAINDER          PIC X(81).                   9915845
020000         05  FILLER REDEFINES EX09-REMAINDER.                     9915845
020100             07  EX09-CUR-BAL    PIC S9(13)V99       COMP-3.      9915845
020200             07  EX09-GEN-STAT   PIC X.                           9915845
020300             07  EX09-GEN-DESC   PIC X(51).                       9915845
020305             07  EX09-GEN-REASON PIC X(02).                       IMIB004
020310*            07  FILLER          PIC X(21).                       0326947
020315             07  FILLER          PIC X(19).                       IMIB004
020400         05  EX09-REF-NUM            PIC S9(15)      COMP-3.      9915845
020500         05  EX09-SRC-INFORMATION.                                9915845
020600             07  EX09-SRC-FLAG       PIC X.                       9915845
020700             07  EX09-SRC-INFO       PIC X(44).                   9915239
020710         05  FILLER                  PIC X(2854).                 0717565
020800     03  IMEX-REC-10 REDEFINES IMEX-RECORD.                       9915845
020900         05  EX10-PMT-AMT            PIC S9(13)V99   COMP-3.      9915845
021000         05  EX10-DISTRIBUTION       PIC X(48).                   9915845
021100         05  EX10-REASON             PIC X.                       9915845
021110         05  FILLER                  PIC X(2950).                 0326947
021200     03  IMEX-REC-11 REDEFINES IMEX-RECORD.                       9915845
021300         05  EX11-TYP-NO.                                         9915845
021400             07  EX11-TYP            PIC X.                       9915845
021500             07  EX11-NO             PIC 99.                      9915845
021600             07  EX11-NOX REDEFINES EX11-NO  PIC XX.              9915845
021700         05  EX11-TRLR               PIC X(85).                   9915845
021800         05  EX11-CODE               PIC X.                       9915845
021802         05  EX11-DATE-LAST-MAINT.                                0266748
021804             07  EX11-LST-MAINT-MO   PIC XX.                      0266748
021806             07  EX11-LST-MAINT-DA   PIC XX.                      0266748
021808             07  EX11-LST-MAINT-YR   PIC XX.                      0266748
021900         05  EX11-SRC-INFORMATION.                                9915845
022000             07  EX11-SRC-FLAG       PIC X.                       9915845
022100             07  EX11-SRC-INFO       PIC X(44).                   9915239
022110         05  FILLER                  PIC X(2867).                 0326947
022200     03  IMEX-REC-12 REDEFINES IMEX-RECORD.                       9915845
022300         05  EX12-NA-LINE            PIC X(42)   OCCURS 8 TIMES.  9915845
022400         05  EX12-NA-PTR             PIC S9          COMP-3.      9915845
022402         05  EX12-DATE-LAST-MAINT.                                0266748
022404             07  EX12-LST-MAINT-MO   PIC XX.                      0266748
022406             07  EX12-LST-MAINT-DA   PIC XX.                      0266748
022408             07  EX12-LST-MAINT-YR   PIC XX.                      0266748
022410         05  EX12-NA-COUNTRY         PIC XX.                      9915845
022500         05  EX12-OLD-LANG-CODE      PIC XX.                      9915845
022600         05  EX12-LANG-CODE-CHNG     PIC X.                       9915845
022700         05  EX12-SRC-INFORMATION.                                9915845
022800             07  EX12-SRC-FLAG       PIC X.                       9915845
022900             07  EX12-SRC-INFO       PIC X(44).                   9915239
022910         05  FILLER                  PIC X(2614).                 0326947
023000     03  IMEX-REC-13 REDEFINES IMEX-RECORD.                       9915845
023100         05  EX13-REJ-PROG           PIC XX.                      9915845
023200         05  EX13-REJ-RESN           PIC XXX.                     0717565
023300         05  EX13-SOURCE             PIC XX.                      9915845
023400         05  EX13-BATCH              PIC S9(5)       COMP-3.      9915845
023500         05  EX13-SEQ                PIC S9(5)       COMP-3.      9915845
023600         05  EX13-TRACE              PIC S9(7)       COMP-3.      9915845
023700         05  EX13-AMT                PIC S9(13)V99   COMP-3.      9915845
023800         05  EX13-USER-CODE.                                      9915845
023900             07  EX13-UCODE1         PIC XX.                      9915845
024000             07  EX13-UCODE2         PIC XX.                      9915845
024100         05  EX13-TYPE               PIC X.                       9915845
024200         05  EX13-NB-PEND-DEC-IND    PIC X.                       9915845
024300         05  EX13-NB-FORCE-PAY-IND   PIC X.                       9915845
024400         05  EX13-CAPITAL-MKT-IND    PIC X.                       9915845
024402         05  EX13-COMB-PLN-CODES.                                 0617360
024404             07  EX13-PLN-CODE       PIC XX.                      0617360
024406             07  EX13-PLN-CODE2      PIC XX.                      0617360
024408         05  EX13-PLN-CAL-YR         PIC X.                       0617360
024410         05  EX13-PLN-TAX-YR         PIC X.                       0617360
024420         05  EX13-XC-LOC-LIMIT-FLAG  PIC X.                       0617492
024500         05  EX13-TR-TYPE            PIC XX.                      9915845
024600         05  EX13-RMDR               PIC X(120).                  2011293
024700         05  FILLER REDEFINES EX13-RMDR.                          9915845
024800             07  EX13-DESC.                                       9915845
024900                 09  EX13-ITEM-CNT   PIC S9(9)       COMP-3.      9915845
025000                 09  EX13-CHK-NO     PIC X(10).                   9915845
025100                 09  EX13-OD-TRAN-FEE PIC S9(13)V99  COMP-3.      0326947
025200                 09  EX13-BAI-CODE   PIC X(05).                   9915845
025300                 09  EX13-REF-NUM    PIC S9(15)      COMP-3.      9915845
025310                 09  FILLER          PIC X(84).                   0326947
025350         05  EXC13-MISC REDEFINES EX13-RMDR.                      0326987
025355             07  EXC13-02-DESC       PIC X(16).                   0326987
025360             07  EXC13-02-ITEM-COUNT PIC S9(5)       COMP-3.      0326987
025365             07  FILLER              PIC X(04).                   0326987
025370             07  EXC13-02-BAI        PIC X(05).                   0326987
025375             07  EXC13-02-REF-NO     PIC S9(15)      COMP-3.      0326987
025380             07  FILLER              PIC X(84).                   0326987
025400         05  EX13-NP-INFO REDEFINES EX13-RMDR.                    9915845
025500             07  EX13-NP-ORIG-DATE.                               9915845
025600                 09  EX13-NP-ORIG-MO PIC XX.                      9915845
025700                 09  EX13-NP-ORIG-DA PIC XX.                      9915845
025800                 09  EX13-NP-ORIG-YR PIC XX.                      9915845
025900             07  EX13-NP-ORIG-CODE   PIC XXXX.                    9915845
026000             07  EX13-NP-ORIG-BATCH  PIC S9(5)   COMP-3.          9915845
026100             07  EX13-NP-ORIG-SEQ    PIC S9(5)   COMP-3.          9915845
026200             07  EX13-NP-ITEMS       PIC S9(5)   COMP-3.          9915845
026300             07  EX13-NP-DAYS        PIC S999    COMP-3.          9915845
026400             07  EX13-NP-HOL         PIC S9      COMP-3.          9915845
026500             07  IM-EXC13-ORIG-REF-NUM PIC S9(15)  COMP-3.        9915845
026600             07  FILLER              PIC X(90).                   0326947
026700         05  EX13-BACKDATE REDEFINES EX13-RMDR.                   9915845
026800             07  EX13-BD-DATE.                                    9915845
026900                 09  EX13-BD-MO      PIC XX.                      9915845
027000                 09  EX13-BD-DA      PIC XX.                      9915845
027100                 09  EX13-BD-YR      PIC XX.                      9915845
027200             07  EX13-BD-DAYS        PIC S999    COMP-3.          9915845
027300             07  EX13-BD-CHECK       PIC X(10).                   9915845
027400             07  EX13-BD-ITEMS       PIC S9(5)   COMP-3.          9915845
027500             07  EX13-BD-HOL         PIC S9      COMP-3.          9915845
027510             07  EX13-BD-DESC        PIC X(16).                   0326947
027520             07  EX13-BD-BAI-CD      PIC X(05).                   0326947
027530             07  EX13-BD-REF-NO      PIC S9(15)  COMP-3.          0326947
027540             07  FILLER              PIC X(69).                   0326947
027600         05  EX13-ACH-BKDT REDEFINES EX13-RMDR.                   9915845
027700             07  EX13-ACH-COMPANY    PIC X(16).                   9915845
027800             07  EX13-ACH-DESC       PIC X(10).                   9915845
027900             07  EX13-ACH-DATE       PIC X(6).                    9915845
028000             07  EX13-ACH-ID         PIC X(15).                   9915845
028010             07  EX13-ACH-CHK-NO     PIC X(10).                   2011293
028100             07  EX13-ACH-BKDT-DATE  PIC X(6).                    9915845
028200             07  EX13-ACH-BKDT-DAYS  PIC S999    COMP-3.          9915845
028300             07  EX13-ACH-BKDT-HOL   PIC S9      COMP-3.          9915845
028305             07  EX13-ACH-BAI-CD     PIC X(5).                    0326947
028310             07  EX13-ACH-REF-NO     PIC S9(15)  COMP-3.          2011293
028320             07  FILLER              PIC X(41).                   0326947
028400         05  EX13-ATM-BKDT REDEFINES EX13-RMDR.                   9915845
028500             07  EX13-ATM-TERM       PIC X(4).                    9915845
028600             07  EX13-ATM-DESC       PIC X(18).                   9915845
028700             07  EX13-ATM-DATE       PIC X(6).                    9915845
028710             07  EX13-ATM-POS-RECUR  PIC X.                       1020116
028800             07  EX13-ATM-BKDT-DATE  PIC X(6).                    9915845
028900             07  EX13-ATM-BKDT-DAYS  PIC S999    COMP-3.          9915845
029000             07  EX13-ATM-BKDT-HOL   PIC S9      COMP-3.          9915845
029010             07  EX13-ATM-BAI-CD     PIC X(5).                    0326947
029020             07  EX13-ATM-REF-NO     PIC S9(15)  COMP-3.          0326947
029030             07  EX13-ATM-OPT-CD     PIC X.                       1020116
029040             07  FILLER              PIC X(68).                   1020116
029100         05  EX13-ADDENDA-BKDT REDEFINES EX13-RMDR.               9915845
029200             07  FILLER              PIC X(57).                   2011293
029300             07  EX13-ADA-TYPE       PIC XX.                      9915845
029400             07  EX13-ADA-INFO       PIC X(44).                   9915845
029500             07  EX13-ADA-BKDT-DATE  PIC X(6).                    9915845
029600             07  EX13-ADA-BKDT-DAYS  PIC S999    COMP-3.          9915845
029700             07  EX13-ADA-BKDT-HOL   PIC S9      COMP-3.          9915845
029710             07  EX13-ADA-REF-NO     PIC S9(15)  COMP-3.          2011293
029800         05  EX13-LOAN-DISB REDEFINES EX13-RMDR.                  9915845
029900             07  EX13-OTH-DIST                       COMP-3.      9915845
030000                 09  EX13-INS1       PIC S9(13)V99.               9915845
030100                 09  EX13-INS2       PIC S9(13)V99.               9915845
030200                 09  EX13-FEES       PIC S9(13)V99.               9915845
030300             07  EX13-BAL-ADJ REDEFINES EX13-OTH-DIST.            0326947
030400                 09  EX13-PRIN-OLD   PIC S9(13)V99   COMP-3.      0326947
030500                 09  EX13-ADDL-CR    PIC S9(13)V99   COMP-3.      0326947
030510                 09  FILLER          PIC X(8).                    0326947
030600             07  EX13-PYMT-DIST                      COMP-3.      9915845
030700                 09  EX13-PRIN       PIC S9(13)V99.               9915845
030800                 09  EX13-INT        PIC S9(13)V99.               9915845
030900                 09  EX13-OTH        PIC S9(13)V99.               9915845
031000             07  EX13-LN-ORIG-DATE.                               9915845
031100                 09  EX13-LN-BD-MO   PIC XX.                      9915845
031200                 09  EX13-LN-BD-DA   PIC XX.                      9915845
031300                 09  EX13-LN-BD-YR   PIC XX.                      9915845
031400             07  EX13-LN-BD-DAYS     PIC S999    COMP-3.          9915845
031410             07  FILLER              PIC X(64).                   0326947
031500         05  EX13-DATE-LAST-ACTIVE   PIC X(6).                    9915845
031600         05  EX13-REASON-OR-OPT      PIC X.                       9915845
031700         05  EX13-BKDT-TRAN.                                      9915845
031800             07  EX13-ACCRUAL-AMOUNT PIC S9(11)V9(6)  COMP-3.     9915845
031900             07  FILLER REDEFINES EX13-ACCRUAL-AMOUNT.            9915845
032000                 09  EX13-MANUAL-ADJ-DAYS                         9915845
032100                                         PIC S999        COMP-3.  9915845
032200                 09  FILLER              PIC X(7).                9915845
032300             07  EX13-RATE-TYPE      PIC X.                       9915845
032400             07  EX13-RATE-PTR       PIC 999.                     9915845
032500         05  FILLER REDEFINES EX13-BKDT-TRAN.                     9915845
032600             07  EX13-AMT-AVAIL      PIC S9(13)V99    COMP-3.     9915845
032700             07  FILLER              PIC X(5).                    0326947
032800         05  EX13-SRC-INFORMATION.                                9915845
032900             07  EX13-SRC-FLAG       PIC X.                       9915845
033000             07  EX13-SRC-INFO       PIC X(44).                   9915239
033100         05  EX22-CURRENCY-INFO.                                  9915845
033200             07  EX22-CURR-CODE      PIC XXX.                     9915845
033300             07  EX22-CURR-DEC       PIC X.                       9915845
033400             07  EX22-CURR-AMT       PIC S9(15)V99    COMP-3.     9915845
033500             07  EX22-CURR-RATE      PIC S9(7)V9(8)   COMP-3.     9915845
033600             07  EX22-CURR-OVRD      PIC X.                       9915845
033700             07  EX22-CURR-FILLER    PIC XXX.                     9915845
033800         05  EX22-RMDR.                                           9915845
033900             07  EX22-SEG-OCCURS PIC XX.                          9915845
034000             07  EX22-SEG-LENGTH PIC XX.                          9915845
034100             07  EX22-UNIV-DESC  PIC X(790).                      0447137
034110         05  FILLER              PIC X(1961).                     0717565
034200     03  IMEX-REC-14 REDEFINES IMEX-RECORD.                       9915845
034300         05  EX14-SVC-CHG-INFO       PIC X(248).                  9916070
034400         05  EX14-SVC-RD-CHG         PIC S9(13)V99    COMP-3.     9915845
034405         05  EX14-SVC-ANC-CHG        PIC X.                       0516691
034410         05  EX14-SVC-FEE-TYPE       PIC X.                       0516691
034415         05  FILLER                  PIC X(2749).                 0516691
034500     03  IMEX-REC-15 REDEFINES IMEX-RECORD.                       9915845
034600         05  EX15-FM-TRAN            PIC X(70).                   9915845
034700         05  EX15-MST-OLD-DATA       PIC X(35).                   9915845
034800         05  EX15-OLD-BYTE REDEFINES EX15-MST-OLD-DATA            9915845
034900                                     PIC X   OCCURS 35 TIMES.     9915845
034902         05  EX15-FM-IM2E-TYPE       PIC X.                       0817665
034904         05  EX15-FM-AFT-POST        PIC X.                       0817665
035000         05  EX15-DATE-LAST-MAINT.                                9915845
035100             07  EX15-LST-MAINT-MO   PIC XX.                      9915845
035200             07  EX15-LST-MAINT-DA   PIC XX.                      9915845
035300             07  EX15-LST-MAINT-YR   PIC XX.                      9915845
035400         05  EX15-SRC-INFORMATION.                                9915845
035500             07  EX15-SRC-FLAG       PIC X.                       9915845
035600             07  EX15-SRC-INFO       PIC X(44).                   9915239
035610         05  FILLER                  PIC X(2849).                 0817665
035700     03  IMEX-REC-17 REDEFINES IMEX-RECORD.                       9915845
035800         05  EX17-SRC-INFORMATION.                                9915845
035900             07  EX17-SRC-FLAG       PIC X.                       9915845
036000             07  EX17-SRC-INFO       PIC X(44).                   9915239
036010         05  FILLER                  PIC X(2962).                 0326947
036100     03  IMEX-REC-18 REDEFINES IMEX-RECORD.                       9915845
036200         05  EX18-FLAG               PIC X.                       9915845
036300         05  EX18-NO-CYC             PIC S999        COMP-3.      9915845
036400         05  EX18-PREV-EXT.                                       9915845
036500             07  EX18-PE-DATE        PIC X(6).                    9915845
036600             07  EX18-PE-FLAG        PIC X.                       9915845
036700             07  EX18-PE-REM-CYC     PIC S999        COMP-3.      9915845
036800         05  EX18-WAIVE-FLG          PIC X.                       9915845
036900         05  EX18-WAIVE-AMT          PIC S9(13)V99    COMP-3.     9915845
037000         05  EX18-EXT-CHARGE         PIC S9(13)V99    COMP-3.     9915845
037100         05  EX18-PAST-DUE-FLAG      PIC  X.                      9915845
037200         05  EX18-PAST-DUE-AMT       PIC S9(13)V99    COMP-3.     9915845
037300         05  EX18-PYMT-DUE-AMT       PIC S9(13)V99    COMP-3.     9915845
037400         05  EX18-BILLING-TRAILERS   OCCURS 7 TIMES.              9915845
037500             07  EX18-BT-DUE-DATE.                                9915845
037600                 09  EX18-BT-DUE-CENT PIC XX.                     9915845
037700                 09  EX18-BT-DUE-YR  PIC XX.                      9915845
037800                 09  EX18-BT-DUE-MO  PIC XX.                      9915845
037900                 09  EX18-BT-DUE-DA  PIC XX.                      9915845
038000             07  EX18-BT-DUE-AMT     PIC S9(13)V99    COMP-3.     9915845
038100             07  EX18-BT-DUE-STAT    PIC X.                       9915845
038200         05  EX18-PREV-LN-STAT       PIC X.                       9915845
038210         05  EX18-DATE-LAST-MAINT.                                0266748
038212             07  EX18-LST-MAINT-MO   PIC XX.                      0266748
038214             07  EX18-LST-MAINT-DA   PIC XX.                      0266748
038216             07  EX18-LST-MAINT-YR   PIC XX.                      0266748
038300         05  EX18-SRC-INFORMATION.                                9915845
038400             07  EX18-SRC-FLAG       PIC X.                       9915845
038500             07  EX18-SRC-INFO       PIC X(44).                   9915239
038510         05  FILLER                  PIC X(2790).                 0326947
038600     03  IMEX-REC-19 REDEFINES IMEX-RECORD.                       9915845
038700         05  EX19-LOAN-AGGR-DATA     PIC X(57).                   9915845
038800         05  EX19-LOAN-ACCR-INT      PIC S9(13)V99    COMP-3.     9915845
038810         05  FILLER                  PIC X(2942).                 0326947
038900     03  IMEX-REC-21  REDEFINES  IMEX-RECORD.                     9915845
039000         05  EX21-MONETARY-DATE      PIC X(6).                    9915845
039100         05  EX21-ACCRUAL-AMOUNT     PIC S9(11)V9(6)  COMP-3.     9915845
039200         05  EX21-RATE-DATE          PIC X(8).                    9915845
039300         05  EX21-RATE-TYPE          PIC X.                       9915845
039400         05  EX21-RATE-PTR           PIC 999.                     9915845
039500         05  EX21-REJ-RSN            PIC X.                       9915845
039510         05  FILLER                  PIC X(2979).                 0326947
039512     03  IMEX-REC-21-PSBK REDEFINES IMEX-RECORD.                  0427139
039514         05  EX21-PSBK-DATE          PIC X(8).                    0427139
039516         05  FILLER                  PIC X(2999).                 0427139
039600     03  IMEX-REC-23 REDEFINES  IMEX-RECORD.                      9915845
039700         05  FILLER                  PIC X(30).                   9915845
039800         05  EX23-CLOSING    COMP-3.                              9915845
039900             07  EX23-ADJUSTMENT     PIC S9(13)V99.               9915845
040000             07  EX23-IOD-INT-PAY    PIC S9(13)V99.               9915845
040100             07  EX23-SAV-INT-PAY    PIC S9(13)V99.               9915845
040200             07  EX23-OD-INT-CHG     PIC S9(13)V99.               9915845
040300             07  EX23-IOD-INT-WAV    PIC S9(13)V99.               9915845
040400             07  EX23-SAV-INT-WAV    PIC S9(13)V99.               9915845
040500             07  EX23-OD-INT-WAV     PIC S9(13)V99.               9915845
040600             07  EX23-IOD-TAX        PIC S9(13)V99.               9915845
040700             07  EX23-SAV-TAX        PIC S9(13)V99.               9915845
040710             07  EX23-CLOSING-CHG    PIC S9(13)V99.               0527322
040800         05  EX23-SRC-INFORMATION.                                9915845
040900             07  EX23-SRC-FLAG       PIC X.                       9915845
041000             07  EX23-SRC-INFO       PIC X(44).                   9915239
041010         05  FILLER                  PIC X(2852).                 0527322
041100     03  IMEX-REC-24 REDEFINES  IMEX-RECORD.                      9915845
041200         05  EX24-COSTS-AMORTIZED-TDY    PIC S9(13)V99 COMP-3.    9915845
041300         05  EX24-COSTS-TERM-CHG-TDY     PIC X.                   9915845
041310         05  FILLER                  PIC X(2998).                 0326947
041400     03  IMEX-REC-26 REDEFINES  IMEX-RECORD.                      9915845
041500         05  EX26-RATE-AREA          PIC X(139).                  9915845
041510         05  FILLER                  PIC X(2868).                 0326947
041550     03  IMEX-REC-29 REDEFINES  IMEX-RECORD.                      0617360
041552         05  EX29-PLN-CTDT.                                       0617360
041554             07  EX29-CAL-YR         PIC X.                       0617360
041556             07  EX29-TAX-YR         PIC X.                       0617360
041558             07  EX29-COMB-PLN-CODES.                             0617360
041560                 09  EX29-PLN-CODE   PIC XX.                      0617360
041562                 09  EX29-PLN-CODE2  PIC XX.                      0617360
041564             07  EX29-TAX-CODE       PIC X.                       0617360
041566             07  EX29-ADJ-AMT        PIC S9(13)V99 COMP-3.        0617360
041568         05  EX29-CHG-CODE           PIC X.                       0617360
041570         05  EX29-OLD-AMT            PIC S9(13)V99 COMP-3.        0617360
041572         05  EX29-NEW-AMT            PIC S9(13)V99 COMP-3.        0617360
041574         05  EX29-SOURCE             PIC XX.                      0617360
041576         05  EX29-BATCH              PIC S9(5)     COMP-3.        0617360
041578         05  EX29-SEQ                PIC S9(5)     COMP-3.        0617360
041580         05  EX29-SRC-INFORMATION.                                0617360
041582             07  EX29-SRC-FLAG       PIC X.                       0617360
041584             07  EX29-SRC-INFO       PIC X(44).                   0617360
041586         05  FILLER                  PIC X(2922).                 0617360
041600     03  IMEX-REC-44 REDEFINES  IMEX-RECORD.                      9915845
041700         05  EX44-IBT-OLD-NEW-ACCT   PIC X(22).                   9915845
041800         05  EX44-CLASS              PIC X(02).                   9915845
041900         05  EX44-SOURCE             PIC X(02).                   9915845
042000         05  EX44-BATCH              PIC S9(5)      COMP-3.       9915845
042100         05  EX44-SEQ                PIC S9(5)      COMP-3.       9915845
042200         05  EX44-TRACE              PIC S9(7)      COMP-3.       9915845
042210         05  FILLER                  PIC X(2971).                 0326947
042300     03  IMEX-REC-45 REDEFINES  IMEX-RECORD.                      9915845
042400         05  EX45-TYPE               PIC X.                       9915845
042500         05  EX45-NEW-ACCT           PIC X(22).                   9915845
042600         05  EX45-SEGMENT            PIC X(2984).                 0326947
042700     03  IMEX-REC-46 REDEFINES  IMEX-RECORD.                      9915855
042800         05  EX46-DATA.                                           9915855
042900             07  EX46-OD-LMT-DELETE      PIC X.                   9915855
043000             07  EX46-OD-LMT-CHG-TDY     PIC X.                   9915855
043100             07  EX46-OD-LMT-RPT-TDY     PIC X.                   9915855
043200             07  EX46-OD-LMT-AUTH        PIC X.                   9915855
043300             07  EX46-OD-LMT-DOC-IND     PIC X.                   9915855
043400             07  EX46-FILLER             PIC X(3).                9915855
043500             07  EX46-OD-LMT-BATCH-AMT   PIC S9(13)V99   COMP-3.  9915855
043600             07  EX46-OD-LMT-ONLINE-AMT  PIC S9(13)V99   COMP-3.  9915855
043700             07  EX46-OD-LMT-FILLER      PIC X(16).               9915855
043800             07  EX46-OD-LMT-EXP-DAYS    PIC S999        COMP-3.  9915855
043900             07  EX46-OD-LMT-REV-DAYS    PIC S999        COMP-3.  9915855
044000             07  EX46-OD-LMT-OCCURRENCES PIC S999        COMP-3.  9915855
044010         05  FILLER                  PIC X(2961).                 0326947
044100     03  IMEX-REC-47 REDEFINES  IMEX-RECORD.                      9915855
044120         05  EX47-OD-LMT-OCCUR           PIC S999        COMP-3.  9915855
044200         05  EX47-DATA.                                           9915855
044300             07  EX47-OD-LMT-AMT         PIC S9(13)V99   COMP-3.  9915855
044400             07  EX47-OD-LIMIT-EFF-DATE.                          9915855
044500                 09  EX47-OD-EFF-CC       PIC XX.                 9915855
044600                 09  EX47-OD-EFF-DATE.                            9915855
044700                     11  EX47-OD-EFF-YY   PIC XX.                 9915855
044800                     11  EX47-OD-EFF-MM   PIC XX.                 9915855
044900                     11  EX47-OD-EFF-DD   PIC XX.                 9915855
045000             07  EX47-OD-LIMIT-EXP-DATE.                          9915855
045100                 09  EX47-OD-EXP-CC       PIC XX.                 9915855
045200                 09  EX47-OD-EXP-DATE.                            9915855
045300                     11  EX47-OD-EXP-YY   PIC XX.                 9915855
045400                     11  EX47-OD-EXP-MM   PIC XX.                 9915855
045500                     11  EX47-OD-EXP-DD   PIC XX.                 9915855
045600             07  EX47-OD-LIMIT-REV-DATE.                          9915855
045700                 09  EX47-OD-REV-CC       PIC XX.                 9915855
045800                 09  EX47-OD-REV-DATE.                            9915855
045900                     11  EX47-OD-REV-YY   PIC XX.                 9915855
046000                     11  EX47-OD-REV-MM   PIC XX.                 9915855
046100                     11  EX47-OD-REV-DD   PIC XX.                 9915855
046200             07  EX47-OD-LIMIT-TYPE.                              9915855
046300                 09  EX47-OD-LIMIT-TYPE1  PIC X.                  9915855
046400                 09  EX47-OD-LIMIT-TYPE2  PIC X.                  9915855
046500                 09  EX47-OD-LIMIT-TYPE3  PIC X.                  9915855
046600             07  EX47-OD-LIMIT-EXPIRED    PIC X.                  9915855
046700             07  EX47-OD-LIMIT-EXP-ACTION PIC X.                  9915855
046800             07  EX47-OD-LIMIT-OCC-CHG-TODAY PIC X.               9915855
046900             07  EX47-OD-LIMIT-OCC-DEL-TODAY PIC X.               9915855
047000             07  EX47-FILLER              PIC X(7).               9915855
047100         05  FILLER                       PIC X(2959).            0326947
