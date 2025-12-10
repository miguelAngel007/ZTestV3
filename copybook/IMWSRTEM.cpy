*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*
000200*         RATE MASTER FILE                                       *
000300*----------------------------------------------------------------*
000400 01  RATE-MASTER-AREA.
000500   05  RMA-AREA.
000600     10  RMA-FIXED-HDR.
000700         15  RMA-LENGTH                PIC S9999 COMP.
000800         15  RMA-FILLER                PIC XX.
000900         15  RMA-KEY.
001000             20  RMA-CONTROLS.
001100                 25  RMA-CTL1          PIC XX.
001110                 25  RMA-CURRENCY      PIC XXX.                   9915845
001200                 25  RMA-REGION        PIC X(10).
001300             20  RMA-PRODUCT           PIC XXX.
001400             20  RMA-RATE-TYPE         PIC X.
001500                 88  RMA-IOD-SAV                 VALUE 'I'.
001600                 88  RMA-LOANS                   VALUE 'L'.
001700                 88  RMA-OD-ACCR                 VALUE 'O'.
001800                 88  RMA-PRIME                   VALUE 'P'.
001900                 88  RMA-SPLIT-LOANS             VALUE 'S'.
002000                 88  RMA-TIERED                  VALUE 'T'.
002010                 88  RMA-TAX                     VALUE 'X'.       9915845
002100             20  RMA-PTR               PIC 999.
002200
002300         15  RMA-LAST-MAINT.
002400             20  RMA-LM-ONLINE-RBA     PIC X(4).
002500             20  RMA-LM-OPER-ID.
002600                 25  RMA-LM-TS-TELLER  PIC X(5).
002700                 25  FILLER            PIC X(3).
002800             20  RMA-LM-BRANCH         PIC X(3).
002900             20  RMA-LM-TERM-ID        PIC X(4).
003000             20  RMA-LM-DATE.
003100                 25  RMA-LM-DT-CENT    PIC XX.
003200                 25  RMA-LM-DT.
003300                     30  RMA-LM-DT-YY  PIC XX.
003400                     30  RMA-LM-DT-MM  PIC XX.
003500                     30  RMA-LM-DT-DD  PIC XX.
003600             20  RMA-LM-TIME.
003700                 25  RMA-LM-HH-MM-SS   PIC S9(7) COMP-3.
003800
003900         15  RMA-DEL-FLAG              PIC X.
004000         15  RMA-RECALC-RATE           PIC X.
004100         15  RMA-RET-OLD               PIC S999  COMP-3.
004200         15  RMA-DESC                  PIC X(30).
004300
004400*----------------------------------------------------------------*
004500*     THIS AREA IS USED WHEN RMA-IND = 'T' TIERED/SPLIT RATES    *
004510*                                      'X' TAX RATES             *9915845
004600*----------------------------------------------------------------*
004700     10  RMA-TIERED-RATES.
004800         15  RMA-CUR-TIERED-CHG-FLAG             PIC X.
004900*            NOCH-TDY = '0', CHNG-TDY = '1',
005000*            BKDT-TDY = '2', CORR-TDY = '4'
005100         15  RMA-CUR-TIERED-DATA.
005200             20  RMA-CUR-TIERED-DT.
005300                 25  RMA-CUR-TIERED-CENT         PIC XX.
005310                 25  RMA-CUR-TIERED-D4.                           9915845
005400                     30  RMA-CUR-TIERED-YR       PIC XX.          9915845
005500                     30  RMA-CUR-TIERED-MO       PIC XX.          9915845
005600                     30  RMA-CUR-TIERED-DA       PIC XX.          9915845
005700             20  RMA-CUR-TIERED-RATES            COMP-3.
005800                 25  RMA-CUR-TIERED-RT           OCCURS 9 TIMES.
005900                     30  RMA-CUR-TIERED-ANN      PIC S9V9(8).     9915845
006000                     30  RMA-CUR-TIERED-ANN-R    REDEFINES
006100                         RMA-CUR-TIERED-ANN      PIC S999V9(6).   9915845
006300                 25  RMA-CUR-TIERED-LMT          PIC S9(13)V99    9915845
006400                                                 OCCURS 8 TIMES.
006500         15  RMA-PREV-TIERED-DATA.
006600             20  RMA-PREV-TIERED-DT.
006700                 25  RMA-PREV-TIERED-CENT        PIC XX.
006710                 25  RMA-PREV-TIERED-D4.                          9915845
006800                     30  RMA-PREV-TIERED-YR      PIC XX.          9915845
006900                     30  RMA-PREV-TIERED-MO      PIC XX.          9915845
007000                     30  RMA-PREV-TIERED-DA      PIC XX.          9915845
007100             20  RMA-PREV-TIERED-RATES           COMP-3.
007200                 25  RMA-PREV-TIERED-RT          OCCURS 9 TIMES.
007300                     30  RMA-PREV-TIERED-ANN     PIC S9V9(8).     9915845
007400                     30  RMA-PREV-TIERED-ANN-R   REDEFINES
007500                         RMA-PREV-TIERED-ANN     PIC S999V9(6).   9915845
007700                 25  RMA-PREV-TIERED-LMT         PIC S9(13)V99    9915845
007800                                                 OCCURS 8 TIMES.
007900         15  RMA-FUT-TIERED-CHG-FLAG             PIC X.
008000*            NOCH-TDY = '0', CHNG-TDY = '1'
008100         15  RMA-FUT-TIERED-DATA.
008200             20  RMA-FUT-TIERED-DT.
008300                 25  RMA-FUT-TIERED-CENT         PIC XX.
008310                 25  RMA-FUT-TIERED-D4.                           9915845
008400                     30  RMA-FUT-TIERED-YR       PIC XX.          9915845
008500                     30  RMA-FUT-TIERED-MO       PIC XX.          9915845
008600                     30  RMA-FUT-TIERED-DA       PIC XX.          9915845
008700             20  RMA-FUT-TIERED-RATES            COMP-3.
008800                 25  RMA-FUT-TIERED-RT           OCCURS 9 TIMES.
008900                     30  RMA-FUT-TIERED-ANN      PIC S9V9(8).     9915845
009000                     30  RMA-FUT-TIERED-ANN-R    REDEFINES
009100                         RMA-FUT-TIERED-ANN      PIC S999V9(6).   9915845
009300                 25  RMA-FUT-TIERED-LMT          PIC S9(13)V99    9915845
009400                                                 OCCURS 8 TIMES.
009500         15  RMA-DEL-TIERED-DATA.
009600             20  RMA-DEL-TIERED-DT.
009700                 25  RMA-DEL-TIERED-CENT         PIC XX.
009710                 25  RMA-DEL-TIERED-D4.                           9915845
009800                     30  RMA-DEL-TIERED-YR       PIC XX.          9915845
009900                     30  RMA-DEL-TIERED-MO       PIC XX.          9915845
010000                     30  RMA-DEL-TIERED-DA       PIC XX.          9915845
010100             20  RMA-DEL-TIERED-RATES            COMP-3.
010200                 25  RMA-DEL-TIERED-RT           OCCURS 9 TIMES.
010300                     30  RMA-DEL-TIERED-ANN      PIC S9V9(8).     9915845
010400                     30  RMA-DEL-TIERED-ANN-R    REDEFINES
010500                         RMA-DEL-TIERED-ANN      PIC S999V9(6).   9915845
010700                 25  RMA-DEL-TIERED-LMT          PIC S9(13)V99    9915845
010800                                                 OCCURS 8 TIMES.
010900         15  RMA-OLD-TIERED-INFO.
011000             20  RMA-OLD-TIERED-DATA             OCCURS 96 TIMES
011100                                      INDEXED BY RMA-TIERED-IND.
011200                 25  RMA-OLD-TIERED-DT.
011300                     30  RMA-OLD-TIERED-CENT     PIC XX.
011310                     30  RMA-OLD-TIERED-D4.                       9915845
011400                         35  RMA-OLD-TIERED-YR   PIC XX.          9915845
011500                         35  RMA-OLD-TIERED-MO   PIC XX.          9915845
011600                         35  RMA-OLD-TIERED-DA   PIC XX.          9915845
011700                 25  RMA-OLD-TIERED-RATES        COMP-3.
011800                     30  RMA-OLD-TIERED-RT       OCCURS 9 TIMES.
011900                         35  RMA-OLD-TIERED-ANN  PIC S9V9(8).     9915845
012000                         35  RMA-OLD-TIERED-ANN-R    REDEFINES
012100                             RMA-OLD-TIERED-ANN  PIC S999V9(6).   9915845
012300                     30  RMA-OLD-TIERED-LMT      PIC S9(13)V99    9915845
012400                                                 OCCURS 8 TIMES.
012500         15  FILLER REDEFINES RMA-OLD-TIERED-INFO.
012600             20  FILLER                          PIC X(117).      9915845
012700             20  RMA-OLD-TIERED-2-96.
012800                 25  RMA-OLD-TIERED-OCCURS       PIC X(117)       9915845
012900                                                 OCCURS 95 TIMES.
013000*----------------------------------------------------------------*
013100*     THIS AREA IS USED WHEN RMA-IND = 'I' IOD/SAVINGS RATES     *
013200*                                      'L' LOAN RATES            *
013300*                                      'O' OVERDRAFT ACCRUAL     *
013400*----------------------------------------------------------------*
013500     10  RMA-ILO-RATES               REDEFINES RMA-TIERED-RATES.
013600         15  RMA-CUR-ILO-CHG-FLAG        PIC X.
013700*            NOCH-TDY = '0', CHNG-TDY = '1',
013800*            BKDT-TDY = '2', CORR-TDY = '4'
013900         15  RMA-CUR-ILO-DATA.
014000             20  RMA-CUR-ILO-DT.
014100                 25  RMA-CUR-ILO-CENT    PIC XX.
014110                 25  RMA-CUR-ILO-D4.                              9915845
014200                     30  RMA-CUR-ILO-YR  PIC XX.                  9915845
014300                     30  RMA-CUR-ILO-MO  PIC XX.                  9915845
014400                     30  RMA-CUR-ILO-DA  PIC XX.                  9915845
014500             20  RMA-CUR-ILO-RATES       COMP-3.
014600                 25  RMA-CUR-ILO-ANN     PIC S9V9(8).             9915845
014700                 25  RMA-CUR-ILO-ANN-R   REDEFINES
014800                     RMA-CUR-ILO-ANN     PIC S999V9(6).           9915845
015000         15  RMA-PREV-ILO-DATA.
015100             20  RMA-PREV-ILO-DT.
015200                 25  RMA-PREV-ILO-CENT   PIC XX.
015210                 25  RMA-PREV-ILO-D4.                             9915845
015300                     30  RMA-PREV-ILO-YR PIC XX.                  9915845
015400                     30  RMA-PREV-ILO-MO PIC XX.                  9915845
015500                     30  RMA-PREV-ILO-DA PIC XX.                  9915845
015600             20  RMA-PREV-ILO-RATES      COMP-3.
015700                 25  RMA-PREV-ILO-ANN    PIC S9V9(8).             9915845
015800                 25  RMA-PREV-ILO-ANN-R  REDEFINES
015900                     RMA-PREV-ILO-ANN    PIC S999V9(6).           9915845
016100         15  RMA-FUT-ILO-CHG-FLAG        PIC X.
016200*            NOCH-TDY = '0', CHNG-TDY = '1'
016300         15  RMA-FUT-ILO-DATA.
016400             20  RMA-FUT-ILO-DT.
016500                 25  RMA-FUT-ILO-CENT    PIC XX.
016510                 25  RMA-FUT-ILO-D4.                              9915845
016600                     30  RMA-FUT-ILO-YR  PIC XX.                  9915845
016700                     30  RMA-FUT-ILO-MO  PIC XX.                  9915845
016800                     30  RMA-FUT-ILO-DA  PIC XX.                  9915845
016900             20  RMA-FUT-ILO-RATES       COMP-3.
017000                 25  RMA-FUT-ILO-ANN     PIC S9V9(8).             9915845
017100                 25  RMA-FUT-ILO-ANN-R   REDEFINES
017200                     RMA-FUT-ILO-ANN     PIC S999V9(6).           9915845
017400         15  RMA-DEL-ILO-DATA.
017500             20  RMA-DEL-ILO-DT.
017600                 25  RMA-DEL-ILO-CENT    PIC XX.
017610                 25  RMA-DEL-ILO-D4.                              9915845
017700                     30  RMA-DEL-ILO-YR  PIC XX.                  9915845
017800                     30  RMA-DEL-ILO-MO  PIC XX.                  9915845
017900                     30  RMA-DEL-ILO-DA  PIC XX.                  9915845
018000             20  RMA-DEL-ILO-RATES       COMP-3.
018100                 25  RMA-DEL-ILO-ANN     PIC S9V9(8).             9915845
018200                 25  RMA-DEL-ILO-ANN-R   REDEFINES
018300                     RMA-DEL-ILO-ANN     PIC S999V9(6).           9915845
018500         15  RMA-OLD-ILO-INFO.
018600             20  RMA-OLD-ILO-DATA        OCCURS 96 TIMES
018700                                 INDEXED BY RMA-ILO-IND.
018800                 25  RMA-OLD-ILO-DT.
018900                     30  RMA-OLD-ILO-CENT        PIC XX.
018910                     30  RMA-OLD-ILO-D4.                          9915845
019000                         35  RMA-OLD-ILO-YR      PIC XX.          9915845
019100                         35  RMA-OLD-ILO-MO      PIC XX.          9915845
019200                         35  RMA-OLD-ILO-DA      PIC XX.          9915845
019300                 25  RMA-OLD-ILO-RATES           COMP-3.
019400                     30  RMA-OLD-ILO-ANN         PIC S9V9(8).     9915845
019500                     30  RMA-OLD-ILO-ANN-R       REDEFINES
019600                         RMA-OLD-ILO-ANN         PIC S999V9(6).   9915845
019800         15  FILLER                    REDEFINES RMA-OLD-ILO-INFO.
019900             20  FILLER                PIC X(13).                 9915845
020000             20  RMA-OLD-ILO-2-96.
020100                 25  RMA-OLD-ILO-OCCURS          PIC X(13)        9915845
020200                                                 OCCURS 95 TIMES.
020300
020400         15  FILLER                  PIC X(10400).                9915845
020500*----------------------------------------------------------------*
020600*     THIS AREA IS USED WHEN RMA-IND = 'S' LOAN SPLIT RATES      *
020700*----------------------------------------------------------------*
020800     10  RMA-SPLIT-RATES             REDEFINES RMA-TIERED-RATES.
020900         15  RMA-CUR-SPLIT-CHG-FLAG              PIC X.
021000*            NOCH-TDY = '0', CHNG-TDY = '1',
021100*            BKDT-TDY = '2', CORR-TDY = '4'
021200         15  RMA-CUR-SPLIT-DATA.
021300             20  RMA-CUR-SPLIT-DT.
021400                 25  RMA-CUR-SPLIT-CENT          PIC XX.
021410                 25  RMA-CUR-SPLIT-D4.                            9915845
021500                     30  RMA-CUR-SPLIT-YR        PIC XX.          9915845
021600                     30  RMA-CUR-SPLIT-MO        PIC XX.          9915845
021700                     30  RMA-CUR-SPLIT-DA        PIC XX.          9915845
021800             20  RMA-CUR-SPLIT-RATES             COMP-3.
021900                 25  RMA-CUR-SPLIT-RT            OCCURS 3 TIMES.
022000                     30  RMA-CUR-SPLIT-ANN       PIC S9V9(8).     9915845
022100                     30  RMA-CUR-SPLIT-ANN-R     REDEFINES
022200                         RMA-CUR-SPLIT-ANN       PIC S999V9(6).   9915845
022400                 25  RMA-CUR-SPLIT-LMT           PIC S9(13)V99    9915845
022500                                                 OCCURS 2 TIMES.
022600         15  RMA-PREV-SPLIT-DATA.
022700             20  RMA-PREV-SPLIT-DT.
022800                 25  RMA-PREV-SPLIT-CENT         PIC XX.
022810                 25  RMA-PREV-SPLIT-D4.                           9915845
022900                     30  RMA-PREV-SPLIT-YR       PIC XX.          9915845
023000                     30  RMA-PREV-SPLIT-MO       PIC XX.          9915845
023100                     30  RMA-PREV-SPLIT-DA       PIC XX.          9915845
023200             20  RMA-PREV-SPLIT-RATES            COMP-3.
023300                 25  RMA-PREV-SPLIT-RT           OCCURS 3 TIMES.
023400                     30  RMA-PREV-SPLIT-ANN      PIC S9V9(8).     9915845
023500                     30  RMA-PREV-SPLIT-ANN-R    REDEFINES
023600                         RMA-PREV-SPLIT-ANN      PIC S999V9(6).   9915845
023800                 25  RMA-PREV-SPLIT-LMT          PIC S9(13)V99    9915845
023900                                                 OCCURS 2 TIMES.
024000         15  RMA-FUT-SPLIT-CHG-FLAG              PIC X.
024100*            NOCH-TDY = '0', CHNG-TDY = '1'
024200         15  RMA-FUT-SPLIT-DATA.
024300             20  RMA-FUT-SPLIT-DT.
024400                 25  RMA-FUT-SPLIT-CENT          PIC XX.
024410                 25  RMA-FUT-SPLIT-D4.                            9915845
024500                     30  RMA-FUT-SPLIT-YR        PIC XX.          9915845
024600                     30  RMA-FUT-SPLIT-MO        PIC XX.          9915845
024700                     30  RMA-FUT-SPLIT-DA        PIC XX.          9915845
024800             20  RMA-FUT-SPLIT-RATES             COMP-3.
024900                 25  RMA-FUT-SPLIT-RT            OCCURS 3 TIMES.
025000                     30  RMA-FUT-SPLIT-ANN       PIC S9V9(8).     9915845
025100                     30  RMA-FUT-SPLIT-ANN-R     REDEFINES
025200                         RMA-FUT-SPLIT-ANN       PIC S999V9(6).   9915845
025400                 25  RMA-FUT-SPLIT-LMT           PIC S9(13)V99    9915845
025500                                                 OCCURS 2 TIMES.
025600         15  RMA-DEL-SPLIT-DATA.
025700             20  RMA-DEL-SPLIT-DT.
025800                 25  RMA-DEL-SPLIT-CENT          PIC XX.
025810                 25  RMA-DEL-SPLIT-D4.                            9915845
025900                     30  RMA-DEL-SPLIT-YR        PIC XX.          9915845
026000                     30  RMA-DEL-SPLIT-MO        PIC XX.          9915845
026100                     30  RMA-DEL-SPLIT-DA        PIC XX.          9915845
026200             20  RMA-DEL-SPLIT-RATES             COMP-3.
026300                 25  RMA-DEL-SPLIT-RT            OCCURS 3 TIMES.
026400                     30  RMA-DEL-SPLIT-ANN       PIC S9V9(8).     9915845
026500                     30  RMA-DEL-SPLIT-ANN-R     REDEFINES
026600                         RMA-DEL-SPLIT-ANN       PIC S999V9(6).   9915845
026800                 25  RMA-DEL-SPLIT-LMT           PIC S9(13)V99    9915845
026900                                                 OCCURS 2 TIMES.
027000         15  RMA-OLD-SPLIT-INFO.
027100             20  RMA-OLD-SPLIT-DATA              OCCURS 96 TIMES
027200                                      INDEXED BY RMA-SPLIT-IND.
027300                 25  RMA-OLD-SPLIT-DT.
027400                     30  RMA-OLD-SPLIT-CENT      PIC XX.
027410                     30  RMA-OLD-SPLIT-D4.                        9915845
027500                         35  RMA-OLD-SPLIT-YR    PIC XX.          9915845
027600                         35  RMA-OLD-SPLIT-MO    PIC XX.          9915845
027700                         35  RMA-OLD-SPLIT-DA    PIC XX.          9915845
027800                 25  RMA-OLD-SPLIT-RATES         COMP-3.
027900                     30  RMA-OLD-SPLIT-RT        OCCURS 3 TIMES.
028000                         35  RMA-OLD-SPLIT-ANN   PIC S9V9(8).     9915845
028100                         35  RMA-OLD-SPLIT-ANN-R REDEFINES
028200                             RMA-OLD-SPLIT-ANN   PIC S999V9(6).   9915845
028400                     30  RMA-OLD-SPLIT-LMT       PIC S9(13)V99    9915845
028500                                                 OCCURS 2 TIMES.
028600         15  FILLER                  REDEFINES RMA-OLD-SPLIT-INFO.
028700             20  FILLER              PIC X(39).                   9915845
028800             20  RMA-OLD-SPLIT-2-96.
028900                 25  RMA-OLD-SPLIT-OCCURS        PIC X(39)        9915845
029000                                                 OCCURS 95 TIMES.
029100
029200         15  FILLER                  PIC X(7800).                 9915845
029300*----------------------------------------------------------------*
029400*     THIS AREA IS USED WHEN RMA-IND = 'P' LOAN PRIME RATES      *
029500*----------------------------------------------------------------*
029600     10  RMA-PRIME-RATES             REDEFINES RMA-TIERED-RATES.
029700         15  RMA-CUR-PRIME-CHG-FLAG              PIC X.
029800*            NOCH-TDY = '0', CHNG-TDY = '1',
029900*            BKDT-TDY = '2', CORR-TDY = '4'
030000         15  RMA-CUR-PRIME-DATA.
030100             20  RMA-CUR-PRIME-DT.
030200                 25  RMA-CUR-PRIME-CENT          PIC XX.
030210                 25  RMA-CUR-PRIME-D4.                            9915845
030300                     30  RMA-CUR-PRIME-YR        PIC XX.          9915845
030400                     30  RMA-CUR-PRIME-MO        PIC XX.          9915845
030500                     30  RMA-CUR-PRIME-DA        PIC XX.          9915845
030600             20  RMA-CUR-PRIME-RATES.
030700                 25  RMA-CUR-PRIME-ANN           PIC S9V9(8)      9915845
030800                                                 COMP-3.
030900                 25  RMA-CUR-PRIME-ANN-R         REDEFINES
031000                     RMA-CUR-PRIME-ANN           PIC S999V9(6)    9915845
031100                                                 COMP-3.
031300                 25  RMA-CUR-PRIME-ADJ-CODE      PIC X.
031400                 25  RMA-CUR-PRIME-BAL-CALC      PIC X.
031410                 25  RMA-CUR-PRIME-ADJUST        OCCURS 5 TIMES.  9915845
031500                     30  RMA-CUR-PRIME-ADJ       PIC S9V9(8)      9915845
031600                                                 COMP-3.          9915845
031710                     30  RMA-CUR-PRIME-ADJ-R REDEFINES            9915845
031720                         RMA-CUR-PRIME-ADJ       PIC S999V9(6)    9915845
031730                                                 COMP-3.          9915845
031800                 25  RMA-CUR-PRIME-LMT           PIC S9(13)V99    9915845
031900                                                 COMP-3
032000                                                 OCCURS 4 TIMES.
032100         15  RMA-PREV-PRIME-DATA.
032200             20  RMA-PREV-PRIME-DT.
032300                 25  RMA-PREV-PRIME-CENT         PIC XX.
032310                 25  RMA-PREV-PRIME-D4.                           9915845
032400                     30  RMA-PREV-PRIME-YR       PIC XX.          9915845
032500                     30  RMA-PREV-PRIME-MO       PIC XX.          9915845
032600                     30  RMA-PREV-PRIME-DA       PIC XX.          9915845
032700             20  RMA-PREV-PRIME-RATES.
032800                 25  RMA-PREV-PRIME-ANN          PIC S9V9(8)      9915845
032900                                                 COMP-3.
033000                 25  RMA-PREV-PRIME-ANN-R        REDEFINES
033100                     RMA-PREV-PRIME-ANN          PIC S999V9(6)    9915845
033200                                                 COMP-3.
033400                 25  RMA-PREV-PRIME-ADJ-CODE     PIC X.
033500                 25  RMA-PREV-PRIME-BAL-CALC     PIC X.
033510                 25  RMA-PREV-PRIME-ADJUST       OCCURS 5 TIMES.  9915845
033600                     30  RMA-PREV-PRIME-ADJ      PIC S9V9(8)      9915845
033700                                                 COMP-3.          9915845
033810                     30  RMA-PREV-PRIME-ADJ-R REDEFINES           9915845
033820                         RMA-PREV-PRIME-ADJ      PIC S999V9(6)    9915845
033830                                                 COMP-3.          9915845
033900             25  RMA-PREV-PRIME-LMT              PIC S9(13)V99    9915845
034000                                                 COMP-3
034100                                                 OCCURS 4 TIMES.
034200         15  RMA-FUT-PRIME-CHG-FLAG              PIC X.
034300*            NOCH-TDY = '0', CHNG-TDY = '1'
034400         15  RMA-FUT-PRIME-DATA.
034500             20  RMA-FUT-PRIME-DT.
034600                 25  RMA-FUT-PRIME-CENT          PIC XX.
034610                 25  RMA-FUT-PRIME-D4.                            9915845
034700                     30  RMA-FUT-PRIME-YR        PIC XX.          9915845
034800                     30  RMA-FUT-PRIME-MO        PIC XX.          9915845
034900                     30  RMA-FUT-PRIME-DA        PIC XX.          9915845
035000             20  RMA-FUT-PRIME-RATES.
035100                 25  RMA-FUT-PRIME-ANN           PIC S9V9(8)      9915845
035200                                                 COMP-3.
035300                 25  RMA-FUT-PRIME-ANN-R         REDEFINES
035400                     RMA-FUT-PRIME-ANN           PIC S999V9(6)    9915845
035500                                                 COMP-3.
035700                 25  RMA-FUT-PRIME-ADJ-CODE      PIC X.
035800                 25  RMA-FUT-PRIME-BAL-CALC      PIC X.
035810                 25  RMA-FUT-PRIME-ADJUST        OCCURS 5 TIMES.  9915845
035900                     30  RMA-FUT-PRIME-ADJ       PIC S9V9(8)      9915845
036000                                                 COMP-3.          9915845
036110                     30  RMA-FUT-PRIME-ADJ-R REDEFINES            9915845
036120                         RMA-FUT-PRIME-ADJ       PIC S999V9(6)    9915845
036130                                                 COMP-3.          9915845
036200                 25  RMA-FUT-PRIME-LMT           PIC S9(13)V99    9915845
036300                                                 COMP-3
036400                                                 OCCURS 4 TIMES.
036500         15  RMA-DEL-PRIME-DATA.
036600             20  RMA-DEL-PRIME-DT.
036700                 25  RMA-DEL-PRIME-CENT          PIC XX.
036710                 25  RMA-DEL-PRIME-D4.                            9915845
036800                     30  RMA-DEL-PRIME-YR        PIC XX.          9915845
036900                     30  RMA-DEL-PRIME-MO        PIC XX.          9915845
037000                     30  RMA-DEL-PRIME-DA        PIC XX.          9915845
037100             20  RMA-DEL-PRIME-RATES.
037200                 25  RMA-DEL-PRIME-ANN           PIC S9V9(8)      9915845
037300                                                 COMP-3.
037400                 25  RMA-DEL-PRIME-ANN-R         REDEFINES
037500                     RMA-DEL-PRIME-ANN           PIC S999V9(6)    9915845
037600                                                 COMP-3.
037800                 25  RMA-DEL-PRIME-ADJ-CODE      PIC X.
037900                 25  RMA-DEL-PRIME-BAL-CALC      PIC X.
037910                 25  RMA-DEL-PRIME-ADJUST        OCCURS 5 TIMES.  9915845
038000                     30  RMA-DEL-PRIME-ADJ       PIC S9V9(8)      9915845
038100                                                 COMP-3.          9915845
038210                     30  RMA-DEL-PRIME-ADJ-R REDEFINES            9915845
038220                         RMA-DEL-PRIME-ADJ       PIC S999V9(6)    9915845
038230                                                 COMP-3.          9915845
038300                 25  RMA-DEL-PRIME-LMT           PIC S9(13)V99    9915845
038400                                                 COMP-3
038500                                                 OCCURS 4 TIMES.
038600         15  RMA-OLD-PRIME-INFO.
038700             20  RMA-OLD-PRIME-DATA              OCCURS 96 TIMES
038800                                      INDEXED BY RMA-PRIME-IND.
038900                 25  RMA-OLD-PRIME-DT.
039000                     30  RMA-OLD-PRIME-CENT      PIC XX.
039010                     30  RMA-OLD-PRIME-D4.                        9915845
039100                         35  RMA-OLD-PRIME-YR    PIC XX.          9915845
039200                         35  RMA-OLD-PRIME-MO    PIC XX.          9915845
039300                         35  RMA-OLD-PRIME-DA    PIC XX.          9915845
039400                 25  RMA-OLD-PRIME-RATES.
039500                     30  RMA-OLD-PRIME-ANN   PIC S9V9(8) COMP-3.  9915845
039600                     30  RMA-OLD-PRIME-ANN-R     REDEFINES
039700                         RMA-OLD-PRIME-ANN       PIC S999V9(6)    9915845
039800                                                 COMP-3.
040000                     30  RMA-OLD-PRIME-ADJ-CODE  PIC X.
040100                     30  RMA-OLD-PRIME-BAL-CALC  PIC X.
040110                     30  RMA-OLD-PRIME-ADJUST    OCCURS 5 TIMES.  9915845
040200                         35  RMA-OLD-PRIME-ADJ   PIC S9V9(8)      9915845
040300                                                 COMP-3.          9915845
040410                         35  RMA-OLD-PRIME-ADJ-R REDEFINES        9915845
040420                             RMA-OLD-PRIME-ADJ   PIC S999V9(6)    9915845
040430                                                 COMP-3.          9915845
040500                     30  RMA-OLD-PRIME-LMT       PIC S9(13)V99    9915845
040600                                                 COMP-3
040700                                                 OCCURS 4 TIMES.
040800         15  FILLER                  REDEFINES RMA-OLD-PRIME-INFO.
040900             20  FILLER              PIC X(72).                   9915845
041000             20  RMA-OLD-PRIME-2-96.
041100                 25  RMA-OLD-PRIME-OCCURS        PIC X(72)        9915845
041200                                                 OCCURS 95 TIMES.
041300
041400         15  FILLER                  PIC X(4500).                 9915845
041500*
041600*HEADER RECORD
041700*
041800     05  RMA-ZERO-KEY-REC            REDEFINES RMA-AREA.
041900         10  FILLER                              PIC X(57).       9915845
042000         10  RMA-PROC-THRU-DATE-TABLE.
042100             15  RMA-PROC-THRU-TBL               OCCURS 99 TIMES.
042200                 20  RMA-LO-RUN-DATE             PIC X(8).
042300                 20  RMA-HI-PREV-PROC-THRU-DT    PIC X(8).
042400                 20  RMA-DATE-DIFF               PIC X(1).
042500                 20  RMA-RATE-HIST-RETEN         PIC S999 COMP-3.
042600                 20  RMA-IOD-INT-RATES           COMP-3.
042700                     25  RMA-MAX-IOD-INT         PIC S9V9(8).     9915845
042800                     25  RMA-MAX-IOD-INT-R       REDEFINES
042900                         RMA-MAX-IOD-INT         PIC S999V9(6).   9915845
043100                     25  RMA-MIN-IOD-INT         PIC S9V9(8).     9915845
043200                     25  RMA-MIN-IOD-INT-R       REDEFINES
043300                         RMA-MIN-IOD-INT         PIC S999V9(6).   9915845
043500                 20  RMA-TIERED-INT-RATES        COMP-3.
043600                     25  RMA-MAX-TIERED-INT      PIC S9V9(8).     9915845
043700                     25  RMA-MAX-TIERED-INT-R    REDEFINES
043800                         RMA-MAX-TIERED-INT      PIC S999V9(6).   9915845
044000                     25  RMA-MIN-TIERED-INT      PIC S9V9(8).     9915845
044100                     25  RMA-MIN-TIERED-INT-R    REDEFINES
044200                         RMA-MIN-TIERED-INT      PIC S999V9(6).   9915845
044400                 20  RMA-OD-INT-RATES            COMP-3.
044500                     25  RMA-MAX-OD-INT          PIC S9V9(8).     9915845
044600                     25  RMA-MAX-OD-INT-R        REDEFINES
044700                         RMA-MAX-OD-INT          PIC S999V9(6).   9915845
044900                     25  RMA-MIN-OD-INT          PIC S9V9(8).     9915845
045000                     25  RMA-MIN-OD-INT-R        REDEFINES
045100                         RMA-MIN-OD-INT          PIC S999V9(6).   9915845
045300                 20  RMA-LOAN-INT-RATES          COMP-3.
045400                     25  RMA-MAX-LOAN-INT        PIC S9V9(8).     9915845
045500                     25  RMA-MAX-LOAN-INT-R      REDEFINES
045600                         RMA-MAX-LOAN-INT        PIC S999V9(6).   9915845
045800                     25  RMA-MIN-LOAN-INT        PIC S9V9(8).     9915845
045900                     25  RMA-MIN-LOAN-INT-R      REDEFINES
046000                         RMA-MIN-LOAN-INT        PIC S999V9(6).   9915845
046200                 20  RMA-PRIME-MAX-RATES         COMP-3.
046300                     25  RMA-PRIME-MAX-RATE      OCCURS 5 TIMES.
046400                         30  RMA-PRIME-MAX-INT   PIC S9V9(8).     9915845
046500                         30  RMA-PRIME-MAX-INT-R REDEFINES
046600                             RMA-PRIME-MAX-INT   PIC S999V9(6).   9915845
046605                 20  RMA-TAX-INT-RATES           COMP-3.          2012254
046610                     25  RMA-MAX-TAX-INT         PIC S9V9(8).     2012254
046615                     25  RMA-MAX-TAX-INT-R       REDEFINES        2012254
046620                         RMA-MAX-TAX-INT         PIC S999V9(6).   2012254
046625                     25  RMA-MIN-TAX-INT         PIC S9V9(8).     2012254
046630                     25  RMA-MIN-TAX-INT-R       REDEFINES        2012254
046635                         RMA-MIN-TAX-INT         PIC S999V9(6).   2012254
046900         10  FILLER                  PIC X(2430).                 2012254
047000*
