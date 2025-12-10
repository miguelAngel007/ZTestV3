*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*1003624
000200*            IMPACS WWBCR 7 CARD COPYBOOK                        *9915845
000300*----------------------------------------------------------------*1003624
001000 01  DDA-WRKBCR-7.                                                9915845
001100     03  WBC7-CONTROL-KEY.
001200         05  WBC7-CONTROL-1  PIC XX.
001300         05  WBC7-CONTROL-2  PIC XXX.
001400         05  WBC7-CONTROL-3  PIC XXX.
001500     03  WBC7-RECORD-ID      PIC XX.
001600     03  FILLER              PIC X(10).                           9915845
001700     03  WBC-GL-PHASE        PIC X(4).                            0902970
001710     03  WBC-GL-RC-PHASE     PIC X(4).                            0902970
001800     03  WBC-BILLING-PHASE   PIC X(8).
001850     03  WBC-FMS-CUST-NO     PIC X(4).                            IM006
001860     03  WBC-GL-OR-REGION-CHANGE         PIC X(1).                9915845
001870     03  WBC-SEGMENT-NO      PIC X(2).                            9915845
001900     03  FILLER              PIC X(3).                            9915845
001910   02  WBC-FILE-TOTALS.                                           9915845
002000     03  WBC-DDA-BEGINNING-TOTALS.
002100         05  WBC-DDA-BEGIN-TOT   COMP-3.
002200             07  WBC-BEG-NO-ACCTS        PIC S9(9).               9915845
002300             07  WBC-BEG-BAL             PIC S9(15)V99.           9915845
002400             07  WBC-BEG-ZERO-BAL        PIC S9(9).               9915845
002500             07  WBC-BEG-CLSD-ACCTS      PIC S9(9).               9915845
002600             07  WBC-BEG-TERM-ACCTS      PIC S9(9).               9915845
002700             07  WBC-BEG-NO-DORM         PIC S9(9).               9915845
002800             07  WBC-BEG-DORM-AMT        PIC S9(15)V99.           9915845
002900             07  WBC-BEG-NO-INACT        PIC S9(9).               9915845
003000             07  WBC-BEG-INACT-AMT       PIC S9(15)V99.           9915845
003100             07  WBC-BEG-MRKT-ACCTS      PIC S9(9).               9915845
003200             07  WBC-BEG-MRKT-AMT        PIC S9(15)V99.           9915845
003300             07  WBC-BEG-DDA-FLOAT       PIC S9(15)V99.           9915845
003400             07  WBC-BEG-UNCOLL-FUNDS    PIC S9(15)V99.           9915845
003500             07  WBC-BEG-OD-ACCTS        PIC S9(9).               9915845
003600             07  WBC-BEG-OD-AMT          PIC S9(15)V99.           9915845
003700             07  WBC-BEG-OD-ACCR-ACCTS   PIC S9(9).               9915845
003800             07  WBC-BEG-OD-ACCR-BAL     PIC S9(15)V99.           9915845
003900             07  WBC-BEG-OD-ACCRD-INT    PIC S9(15)V99.           9915845
004000             07  WBC-BEG-OD-INT-MTD      PIC S9(15)V99.           9915845
004100             07  WBC-BEG-OD-INT-YTD      PIC S9(15)V99.           9915845
004300     03  WBC-DDA-ENDING-TOTALS.
004400         05  WBC-DDA-END-TOT COMP-3.
004500             07  WBC-END-NO-ACCTS        PIC S9(9).               9915845
004600             07  WBC-END-BAL             PIC S9(15)V99.           9915845
004700             07  WBC-END-ZERO-BAL        PIC S9(9).               9915845
004800             07  WBC-END-CLSD-ACCTS      PIC S9(9).               9915845
004900             07  WBC-END-TERM-ACCTS      PIC S9(9).               9915845
005000             07  WBC-END-NO-DORM         PIC S9(9).               9915845
005100             07  WBC-END-DORM-AMT        PIC S9(15)V99.           9915845
005200             07  WBC-END-NO-INACT        PIC S9(9).               9915845
005300             07  WBC-END-INACT-AMT       PIC S9(15)V99.           9915845
005400             07  WBC-END-MRKT-ACCTS      PIC S9(9).               9915845
005500             07  WBC-END-MRKT-AMT        PIC S9(15)V99.           9915845
005600             07  WBC-END-DDA-FLOAT       PIC S9(15)V99.           9915845
005700             07  WBC-END-UNCOLL-FUNDS    PIC S9(15)V99.           9915845
005800             07  WBC-END-OD-ACCTS        PIC S9(9).               9915845
005900             07  WBC-END-OD-AMT          PIC S9(15)V99.           9915845
006000             07  WBC-END-OD-ACCR-ACCTS   PIC S9(9).               9915845
006100             07  WBC-END-OD-ACCR-BAL     PIC S9(15)V99.           9915845
006200             07  WBC-END-OD-ACCRD-INT    PIC S9(15)V99.           9915845
006300             07  WBC-END-OD-INT-MTD      PIC S9(15)V99.           9915845
006400             07  WBC-END-OD-INT-YTD      PIC S9(15)V99.           9915845
006600     03  WBC-ACCUM-TRANS-IN  PIC S9(11)      COMP-3.              9915845
006700     03  WBC-ACCUM-TRANS-OUT PIC S9(11)      COMP-3.              9915845
006800     03  WBC-STOP-HOLD-IN    PIC S9(9)       COMP-3.              9915845
006900     03  WBC-STOP-HOLD-OUT   PIC S9(9)       COMP-3.              9915845
007000     03  WBC-ANALYSIS-IN     PIC S9(9)       COMP-3.              9915845
007100     03  WBC-ANALYSIS-OUT    PIC S9(9)       COMP-3.              9915845
007200     03  WBC-BEGINNING-LOAN-BALANCES.
007300         05  WBC-BEGIN-LOAN-BAL  COMP-3.
007400             07  WBC-BEG-TOT-NO          PIC S9(9).               9915845
007500             07  WBC-BEG-TOT-AMT         PIC S9(15)V99.           9915845
007600             07  WBC-BEG-INT-BAL         PIC S9(15)V99.           9915845
007700             07  WBC-BEG-OTHER-BAL       PIC S9(15)V99.           9915845
007800             07  WBC-BEG-OTHER-BREAKDOWN.
007900                 09  WBC-BEG-INS1        PIC S9(15)V99.           9915845
008000                 09  WBC-BEG-INS2        PIC S9(15)V99.           9915845
008100                 09  WBC-BEG-FEES        PIC S9(15)V99.           9915845
008200         05  WBC-BEGIN-ADD-LOAN      COMP-3.
008300             07  WBC-BEG-ACCR-INT        PIC S9(15)V99.           9915845
008400             07  WBC-BEG-DDA-BAL         PIC S9(15)V99.           9915845
008500             07  WBC-BEG-PAST-DUE.
008600                 09  WBC-BEG-PAST-NO     PIC S9(9).               9915845
008700                 09  WBC-BEG-PAST-AMT    PIC S9(15)V99.           9915845
008800             07  WBC-BEG-NO-OVRLINE      PIC S9(9).               9915845
008900             07  WBC-BEG-OVRLINE-AMT     PIC S9(15)V99.           9915845
009000             07  WBC-BEG-APPRV-CREDIT    PIC S9(15)V99.           9915845
009100             07  WBC-BEG-AVAIL-CREDIT    PIC S9(15)V99.           9915845
009200             07  WBC-BEG-MTD-INTEREST.
009300                 09  WBC-BEG-INT-EARNED  PIC S9(15)V99.           9915845
009400                 09  WBC-BEG-INT-COLL    PIC S9(15)V99.           9915845
009500             07  WBC-BEG-YTD-INTEREST    PIC S9(15)V99.           9915845
009600             07  WBC-BEG-ORIG-COSTS      PIC S9(15)V99.           9915845
009700     03  WBC-ENDING-LOAN-BALANCES.
009800         05  WBC-END-LOAN-BAL COMP-3.                             9915845
009900             07  WBC-END-TOT-NO          PIC S9(9).               9915845
010000             07  WBC-END-TOT-AMT         PIC S9(15)V99.           9915845
010100             07  WBC-END-INT-BAL         PIC S9(15)V99.           9915845
010200             07  WBC-END-OTHER-BAL       PIC S9(15)V99.           9915845
010300             07  WBC-END-OTHER-BREAKDOWN.
010400                 09  WBC-END-INS1        PIC S9(15)V99.           9915845
010500                 09  WBC-END-INS2        PIC S9(15)V99.           9915845
010600                 09  WBC-END-FEES        PIC S9(15)V99.           9915845
010700         05  WBC-END-ADD-LOAN        COMP-3.
010800             07  WBC-END-ACCR-INT        PIC S9(15)V99.           9915845
010900             07  WBC-END-DDA-BAL         PIC S9(15)V99.           9915845
011000             07  WBC-END-PAST-DUE.
011100                 09  WBC-END-PAST-NO     PIC S9(9).               9915845
011200                 09  WBC-END-PAST-AMT    PIC S9(15)V99.           9915845
011300             07  WBC-END-NO-OVRLINE      PIC S9(9).               9915845
011400             07  WBC-END-OVRLINE-AMT     PIC S9(15)V99.           9915845
011500             07  WBC-END-APPRV-CREDIT    PIC S9(15)V99.           9915845
011600             07  WBC-END-AVAIL-CREDIT    PIC S9(15)V99.           9915845
011700             07  WBC-END-MTD-INTEREST.
011800                 09  WBC-END-INT-EARNED  PIC S9(15)V99.           9915845
011900                 09  WBC-END-INT-COLL    PIC S9(15)V99.           9915845
012000             07  WBC-END-YTD-INTEREST    PIC S9(15)V99.           9915845
012100             07  WBC-END-ORIG-COSTS      PIC S9(15)V99.           9915845
012200     03  WBC-BEGINNING-INTRST-TOTALS.
012300         05  WBC-BEG-INTRST-TOT  COMP-3.
012400             07  WBC-BEG-NO-INT-PAY      PIC S9(9).               9915845
012500             07  WBC-BEG-INT-PAY-BAL     PIC S9(15)V99.           9915845
012600             07  WBC-BEG-INTRST-MTD.
012700                 09  WBC-BEG-MTD-ACCR    PIC S9(15)V99.           9915845
012800                 09  WBC-BEG-MTD-PAID    PIC S9(15)V99.           9915845
012900             07  WBC-BEG-YTD-INT-PD      PIC S9(15)V99.           9915845
013000             07  WBC-BEG-YTD-TAX-PD      PIC S9(15)V99.           9915845
013010             07  WBC-BEG-YTD-STATE-TAX-PD PIC S9(15)V99.          9915845
013020             07  WBC-BEG-YTD-LOCAL-TAX-PD PIC S9(15)V99.          9915845
013100             07  WBC-BEG-ACCR-INTRST     PIC S9(15)V99.           9915845
013300     03  WBC-ENDING-INTRST-TOTALS.
013400         05  WBC-END-INTRST-TOT  COMP-3.
013500             07  WBC-END-NO-INT-PAY      PIC S9(9).               9915845
013600             07  WBC-END-INT-PAY-BAL     PIC S9(15)V99.           9915845
013700             07  WBC-END-INTRST-MTD.
013800                 09  WBC-END-MTD-ACCR    PIC S9(15)V99.           9915845
013900                 09  WBC-END-MTD-PAID    PIC S9(15)V99.           9915845
014000             07  WBC-END-YTD-INT-PD      PIC S9(15)V99.           9915845
014100             07  WBC-END-YTD-TAX-PD      PIC S9(15)V99.           9915845
014110             07  WBC-END-YTD-STATE-TAX-PD PIC S9(15)V99.          9915845
014120             07  WBC-END-YTD-LOCAL-TAX-PD PIC S9(15)V99.          9915845
014200             07  WBC-END-ACCR-INTRST     PIC S9(15)V99.           9915845
014400     03  WBC-BEGINNING-BILLING-TOTALS.
014500         05  WBC-BEGIN-BILL-TOT  COMP-3.                          9915845
014600             07  WBC-BEG-NO-TRANS        PIC S9(11).              9915845
014700             07  WBC-BEG-NEW-ACCTS       PIC S9(9).               9915845
014800             07  WBC-BEG-NEW-APPRV-LOANS PIC S9(9).               9915845
014900             07  WBC-BEG-MAINT-TRANS     PIC S9(11).
015000             07  WBC-BEG-SPECL-RPTS      PIC S9(9).               9915845
015200     03  WBC-ENDING-BILLING-TOTALS.
015300         05  WBC-END-BILL-TOT    COMP-3.
015400             07  WBC-END-NO-TRANS        PIC S9(11).
015500             07  WBC-END-NEW-ACCTS       PIC S9(9).               9915845
015600             07  WBC-END-NEW-APPRV-LOANS PIC S9(9).               9915845
015700             07  WBC-END-MAINT-TRANS     PIC S9(11).
015800             07  WBC-END-SPECL-RPTS      PIC S9(9).               9915845
016000     03  WBC-TODAYS-MERGE-TOTALS.
016100         05  WBC-MERGE-TOT       COMP-3.
016200             07  WBC-MERGE-IN            PIC S9(11).              9915845
016300             07  WBC-MERGE-ADD           PIC S9(9).               9915845
016400             07  WBC-MERGE-DROP          PIC S9(9).               9915845
016500             07  WBC-MERGE-OUT           PIC S9(11).              9915845
016600     03  WBC-BEGINNING-IOD-TOTALS.
016700         05  WBC-BEG-IOD-INTRST-TOT COMP-3.
016800             07  WBC-BEG-IOD-NO-INT-PAY  PIC S9(9).               9915845
016900             07  WBC-BEG-IOD-INT-PAY-BAL PIC S9(15)V99.           9915845
017000             07  WBC-BEG-IOD-INTRST-MTD.
017100                 09  WBC-BEG-IOD-MTD-ACCR    PIC S9(15)V99.       9915845
017200                 09  WBC-BEG-IOD-MTD-INT-PD  PIC S9(15)V99.       9915845
017300             07  WBC-BEG-IOD-YTD-INT-PD      PIC S9(15)V99.       9915845
017400             07  WBC-BEG-IOD-YTD-TAX-PD      PIC S9(15)V99.       9915845
017410             07  WBC-BEG-IOD-YTD-STATE-TAX-PD PIC S9(15)V99.      9915845
017420             07  WBC-BEG-IOD-YTD-LOCAL-TAX-PD PIC S9(15)V99.      9915845
017500             07  WBC-BEG-IOD-ACCR-INTRST     PIC S9(15)V99.       9915845
017600     03  WBC-ENDING-IOD-TOTALS.
017700         05  WBC-END-IOD-INTRST-TOT COMP-3.
017800             07  WBC-END-IOD-NO-INT-PAY  PIC S9(9).               9915845
017900             07  WBC-END-IOD-INT-PAY-BAL PIC S9(15)V99.           9915845
018000             07  WBC-END-IOD-INTRST-MTD.
018100                 09  WBC-END-IOD-MTD-ACCR    PIC S9(15)V99.       9915845
018200                 09  WBC-END-IOD-MTD-INT-PD  PIC S9(15)V99.       9915845
018300             07  WBC-END-IOD-YTD-INT-PD      PIC S9(15)V99.       9915845
018400             07  WBC-END-IOD-YTD-TAX-PD      PIC S9(15)V99.       9915845
018410             07  WBC-END-IOD-YTD-STATE-TAX-PD PIC S9(15)V99.      9915845
018420             07  WBC-END-IOD-YTD-LOCAL-TAX-PD PIC S9(15)V99.      9915845
018500             07  WBC-END-IOD-ACCR-INTRST     PIC S9(15)V99.       9915845
018600     03  WBC-REGULATORY-TOTALS.                                   9915845
018700         05  WBC-REG-ACCUMULATORS    PIC  X(189).                 9915845
018800         05  WBC-REG-TFR-ACCUMULATORS                             9915845
018900                                     REDEFINES                    9915845
019000                                     WBC-REG-ACCUMULATORS         9915845
019100                                     OCCURS 21 TIMES              9915845
019200                                     INDEXED BY WBC-REG-TFR-IDX.  9915845
019300             07  WBC-REG-TFR-AMT     PIC S9(15)V99 COMP-3.        9915845
019400         05  WBC-REG-CALL-ACCUMULATORS                            9915845
019500                                     REDEFINES                    9915845
019600                                     WBC-REG-ACCUMULATORS         9915845
019700                                     OCCURS 21 TIMES              9915845
019800                                     INDEXED BY WBC-REG-CALL-IDX. 9915845
019900             07  WBC-REG-CALL-AMT    PIC S9(15)V99 COMP-3.        9915845
019910     03  WBC-TFR-SPC900-NO           PIC S9(9)     COMP-3.        9915845
020100     03  WBC-NB-BEG-END-ABNDN-TOTALS.                             9915845
020200         05  WBC-NB-BEG-ABNDN-AMT PIC S9(15)V99 COMP-3.           9915845
020300         05  WBC-NB-BEG-ABNDN-NO     PIC S9(09)    COMP-3.        9915845
020400         05  WBC-NB-END-ABNDN-AMT    PIC S9(15)V99 COMP-3.        9915845
020500         05  WBC-NB-END-ABNDN-NO     PIC S9(09)    COMP-3.        9915845
020600     03  WBC-BEGINNING-DUAL-YR-TOTALS.                            9915858
020700         05  WBC-BEG-IOD-DUAL-YR-TOT  COMP-3.                     9915858
020800             07  WBC-BEG-IO-DUAL-YTD-INT-PD     PIC S9(15)V99.    9915858
020900             07  WBC-BEG-IO-DUAL-YTD-TAX-PD     PIC S9(15)V99.    9915858
021000             07  WBC-BEG-IO-DUAL-YTD-ST-TAX-PD  PIC S9(15)V99.    9915858
021100             07  WBC-BEG-IO-DUAL-YTD-LOC-TAX-PD PIC S9(15)V99.    9915858
021200         05  WBC-BEG-SAV-DUAL-YR-TOT  COMP-3.                     9915858
021300             07  WBC-BEG-SV-DUAL-YTD-INT-PD     PIC S9(15)V99.    9915858
021400             07  WBC-BEG-SV-DUAL-YTD-TAX-PD     PIC S9(15)V99.    9915858
021500             07  WBC-BEG-SV-DUAL-YTD-ST-TAX-PD  PIC S9(15)V99.    9915858
021600             07  WBC-BEG-SV-DUAL-YTD-LOC-TAX-PD PIC S9(15)V99.    9915858
021700     03  WBC-ENDING-DUAL-YR-TOTALS.                               9915858
021800         05  WBC-END-IOD-DUAL-YR-TOT  COMP-3.                     9915858
021900             07  WBC-END-IO-DUAL-YTD-INT-PD     PIC S9(15)V99.    9915858
022000             07  WBC-END-IO-DUAL-YTD-TAX-PD     PIC S9(15)V99.    9915858
022100             07  WBC-END-IO-DUAL-YTD-ST-TAX-PD  PIC S9(15)V99.    9915858
022200             07  WBC-END-IO-DUAL-YTD-LOC-TAX-PD PIC S9(15)V99.    9915858
022300         05  WBC-END-SAV-DUAL-YR-TOT  COMP-3.                     9915858
022400             07  WBC-END-SV-DUAL-YTD-INT-PD     PIC S9(15)V99.    9915858
022500             07  WBC-END-SV-DUAL-YTD-TAX-PD     PIC S9(15)V99.    9915858
022600             07  WBC-END-SV-DUAL-YTD-ST-TAX-PD  PIC S9(15)V99.    9915858
022700             07  WBC-END-SV-DUAL-YTD-LOC-TAX-PD PIC S9(15)V99.    9915858
022800     03  WBC-REG-RECLASS-TOTALS.                                  2010312
022810         05  WBC-REG-RECLASS-ACCUMULATORS            COMP-3.      2010312
022820             07  WBC-REG-RECLASS-ACCUM1      PIC S9(15)V99        2010312
022830                                                    VALUE +0.     2010312
022840             07  WBC-REG-RECLASS-ACCUM2      PIC S9(15)V99        2010312
022850                                                    VALUE +0.     2010312
022900         05  WBC-REG-MMA-ACCUMULATORS                             2010312
022910                                      REDEFINES                   2010312
022920                                      WBC-REG-RECLASS-ACCUMULATORS2010312
022930                                      OCCURS 2 TIMES              2010312
022940                                      INDEXED BY WBC-REG-MMA-IDX. 2010312
022950             07  WBC-REG-MMA-AMT      PIC S9(15)V99  COMP-3.      2010312
022960     03  WBC-TFR-AVG-TOTAL-ASSETS    PIC S9(15)V99 COMP-3.        2015719
022970     03  WBC-TFR-AVG-ASSETS          PIC S9(15)V99 COMP-3.        0417181
022980     03  WBC-TFR-AVG-LIABILITIES     PIC S9(15)V99 COMP-3.        0417181
023000     03  WBC-ENDING-XINV-TOTALS.                                  2016639
023100         05  WBC-END-XINV-SD-NO      PIC S9(9)       COMP-3.      2016639
023200         05  WBC-END-XINV-SD-AMT     PIC S9(15)V99   COMP-3.      2016639
023300         05  WBC-END-XINV-ND-NO      PIC S9(9)       COMP-3.      2016639
023400         05  WBC-END-XINV-ND-AMT     PIC S9(15)V99   COMP-3.      2016639
023500     03  WBC-TODAYS-MAINT-HIST-TOTALS.                            0326947
023600         05  WBC-MAINT-HIST-IN       PIC S9(11)      COMP-3.      0326947
023700         05  WBC-MAINT-HIST-ADD      PIC S9(9)       COMP-3.      0326947
023800         05  WBC-MAINT-HIST-DROP     PIC S9(9)       COMP-3.      0326947
023900         05  WBC-MAINT-HIST-OUT      PIC S9(11)      COMP-3.      0326947
030000     03  WBC-REG-NONACCR-AMT         PIC S9(15)V99   COMP-3.      0637525
030100     03  FILLER                      PIC X(162).                  0637525
