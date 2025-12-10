*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*
000200*      RATE WORK FILE COPYBOOK                                   *
000300*----------------------------------------------------------------*
000400 01  RATE-WORK-FILE.
000500     03  RWF-REC-SIZE.
000600         05  RWF-LENGTH              PIC S9999       COMP.
000700         05  FILLER                  PIC XX.
000800     03  RWF-REC-KEY.
000900         05  RWF-CTL1                PIC XX.
001000         05  RWF-CURRENCY            PIC XXX.
001100         05  RWF-RATE-TYPE           PIC X.
001200         05  RWF-SEQ                 PIC S999 COMP-3.
001300*----------------------------------------------------------------*
001400*            I=IOD/SAV, L=LOAN, O=OD, P=PRIME, S=SPLIT, T=TIER   *
001500*----------------------------------------------------------------*
001600*    TIERED RATES                                                *
001700*----------------------------------------------------------------*
001800     03  RWF-RATE-INFO.
001900         05  RWF-TIER-INFO           OCCURS 80 TIMES.
002000             10  RWF-TIER-KEY.
002100                 15  RWF-TIER-REGION PIC X(10).
002200                 15  RWF-TIER-PROD   PIC XXX.
002300                 15  RWF-TIER-PTR    PIC 999.
002400*----------------------------------------------------------------*
002500*                    VALID PTR = 001-999                         *
002600*----------------------------------------------------------------*
002700             10  RWF-TIER-PERIOD     PIC X.
002800*----------------------------------------------------------------*
002900*                C=CURRENT, F=FUTURE                             *
003000*----------------------------------------------------------------*
003100             10  RWF-TIER-CHG        PIC X.
003200*----------------------------------------------------------------*
003300*                1=CHANGED TODAY     2=BACKDATED TODAY           *
003400*                4=CORRECTION TODAY  6=DELETED TODAY W/O DFLT RATE
003500*                7=DELETED TODAY W/ DFLT RATE                    *
003600*----------------------------------------------------------------*
003700             10  RWF-TIER-DATE       PIC X(8).
003800*----------------------------------------------------------------*
003900*                EFFECTIVE DATE                                  *
004000*----------------------------------------------------------------*
004100             10  RWF-TIER-RATES      OCCURS 9 TIMES   COMP-3.
004200                 15  RWF-TIER-ANN    PIC S9V9(8).
004300                 15  RWF-TIER-DAF    PIC SVP9(15).
004400             10  RWF-TIER-LMT        PIC S9(13)V99    COMP-3
004500                                     OCCURS 8 TIMES.
004600             10  RWF-TIER-INT-ADJ    PIC X.
004700             10  RWF-TIER-PRV-DAYS   PIC S9(5)        COMP-3.
004800             10  RWF-TIER-DEL-DAYS   PIC S9(5)        COMP-3.
004900             10  RWF-TIER-DEL-DATA.
005000                 15  RWF-TIER-DEL-DATE    PIC X(8).
005100                 15  RWF-TIER-DEL-RATES   COMP-3
005200                                          OCCURS 9 TIMES.
005300                     20  RWF-TIER-DEL-ANN PIC S9V9(8).
005400                     20  RWF-TIER-DEL-DAF PIC SVP9(15).
005500                 15  RWF-TIER-DEL-LMT     PIC S9(13)V99 COMP-3
005600                                          OCCURS 8 TIMES.
005700*----------------------------------------------------------------*
005800*    IOD RATES                                                   *
005900*----------------------------------------------------------------*
006000     03  RWF-DDA REDEFINES RWF-RATE-INFO.
006100         05  RWF-DDA-INFO            OCCURS 100 TIMES.
006200             10  RWF-DDA-KEY.
006300                 15  RWF-DDA-REGION  PIC X(10).
006400                 15  RWF-DDA-PROD    PIC XXX.
006500                 15  RWF-DDA-PTR     PIC 999.
006600*----------------------------------------------------------------*
006700*                    VALID PTR = 001-999                         *
006800*----------------------------------------------------------------*
006900             10  RWF-DDA-PERIOD      PIC X.
007000*----------------------------------------------------------------*
007100*                C=CURRENT, F=FUTURE                             *
007200*----------------------------------------------------------------*
007300             10  RWF-DDA-CHG         PIC X.
007400*----------------------------------------------------------------*
007500*                1=CHANGED TODAY     2=BACKDATED TODAY           *
007600*                4=CORRECTION TODAY  6=DELETED TODAY W/O DFLT RATE
007700*                7=DELETED TODAY W/ DFLT RATE                    *
007800*----------------------------------------------------------------*
007900             10  RWF-DDA-DATE        PIC X(8).
008000*----------------------------------------------------------------*
008100*                EFFECTIVE DATE                                  *
008200*----------------------------------------------------------------*
008300             10  RWF-DDA-RATES       COMP-3.
008400                 15  RWF-DDA-ANN     PIC S9V9(8).
008500                 15  RWF-DDA-DAF     PIC SVP9(15).
008600             10  RWF-DDA-INT-ADJ     PIC X.
008700             10  RWF-DDA-PRV-DAYS    PIC S9(5)        COMP-3.
008800             10  RWF-DDA-DEL-DAYS    PIC S9(5)        COMP-3.
008900             10  RWF-DDA-DEL-DATA.
009000                 15  RWF-DDA-DEL-DATE    PIC X(8).
009100                 15  RWF-DDA-DEL-RATES   COMP-3.
009200                     20  RWF-DDA-DEL-ANN PIC S9V9(8).
009300                     20  RWF-DDA-DEL-DAF PIC SVP9(15).
009400         05  FILLER                      PIC X(20800).
009500*----------------------------------------------------------------*
009600*    LOAN RATES                                                  *
009700*----------------------------------------------------------------*
009800     03  RWF-LN REDEFINES RWF-RATE-INFO.
009900         05  RWF-LN-INFO             OCCURS 100 TIMES.
010000             10  RWF-LN-KEY.
010100                 15  RWF-LN-REGION   PIC X(10).
010200                 15  RWF-LN-PROD     PIC XXX.
010300                 15  RWF-LN-PTR      PIC 999.
010400*----------------------------------------------------------------*
010500*                    VALID PTR = 001-005                         *
010600*----------------------------------------------------------------*
010700             10  RWF-LN-PERIOD       PIC X.
010800*----------------------------------------------------------------*
010900*                C=CURRENT, F=FUTURE                             *
011000*----------------------------------------------------------------*
011100             10  RWF-LN-CHG          PIC X.
011200*----------------------------------------------------------------*
011300*                1=CHANGED TODAY     2=BACKDATED TODAY           *
011400*                4=CORRECTION TODAY  6=DELETED TODAY W/O DFLT RATE
011500*                7=DELETED TODAY W/ DFLT RATE                    *
011600*----------------------------------------------------------------*
011700             10  RWF-LN-DATE         PIC X(8).
011800*----------------------------------------------------------------*
011900*                EFFECTIVE DATE                                  *
012000*----------------------------------------------------------------*
012100             10  RWF-LN-RATES        COMP-3.
012200                 15  RWF-LN-ANN      PIC S9V9(8).
012300                 15  RWF-LN-DAF      PIC SVP9(15).
012400             10  RWF-LN-INT-ADJ      PIC X.
012500             10  RWF-LN-PRV-DAYS     PIC S9(5)        COMP-3.
012600             10  RWF-LN-DEL-DAYS     PIC S9(5)        COMP-3.
012700             10  RWF-LN-DEL-DATA.
012800                 15  RWF-LN-DEL-DATE     PIC X(8).
012900                 15  RWF-LN-DEL-RATES    COMP-3.
013000                     20  RWF-LN-DEL-ANN  PIC S9V9(8).
013100                     20  RWF-LN-DEL-DAF  PIC SVP9(15).
013200         05  FILLER                      PIC X(20800).
013300*----------------------------------------------------------------*
013400*    OD ACCRUAL RATES                                            *
013500*----------------------------------------------------------------*
013600     03  RWF-OD REDEFINES RWF-RATE-INFO.
013700         05  RWF-OD-INFO             OCCURS 100 TIMES.
013800             10  RWF-OD-KEY.
013900                 15  RWF-OD-REGION   PIC X(10).
014000                 15  RWF-OD-PROD     PIC XXX.
014100                 15  RWF-OD-PTR      PIC 999.
014200*----------------------------------------------------------------*
014300*                    VALID PTR = 001-005                         *
014400*----------------------------------------------------------------*
014500             10  RWF-OD-PERIOD       PIC X.
014600*----------------------------------------------------------------*
014700*                C=CURRENT, F=FUTURE                             *
014800*----------------------------------------------------------------*
014900             10  RWF-OD-CHG          PIC X.
015000*----------------------------------------------------------------*
015100*                1=CHANGED TODAY     2=BACKDATED TODAY           *
015200*                4=CORRECTION TODAY  6=DELETED TODAY W/O DFLT RATE
015300*                7=DELETED TODAY W/ DFLT RATE                    *
015400*----------------------------------------------------------------*
015500             10  RWF-OD-DATE         PIC X(8).
015600*----------------------------------------------------------------*
015700*                EFFECTIVE DATE                                  *
015800*----------------------------------------------------------------*
015900             10  RWF-OD-RATES        COMP-3.
016000                 15  RWF-OD-ANN      PIC S9V9(8).
016100                 15  RWF-OD-DAF      PIC SVP9(15).
016200             10  RWF-OD-INT-ADJ      PIC X.
016300             10  RWF-OD-PRV-DAYS     PIC S9(5)        COMP-3.
016400             10  RWF-OD-DEL-DAYS     PIC S9(5)        COMP-3.
016500             10  RWF-OD-DEL-DATA.
016600                 15  RWF-OD-DEL-DATE     PIC X(8).
016700                 15  RWF-OD-DEL-RATES    COMP-3.
016800                     20  RWF-OD-DEL-ANN  PIC S9V9(8).
016900                     20  RWF-OD-DEL-DAF  PIC SVP9(15).
017000         05  FILLER                      PIC X(20800).
017100*----------------------------------------------------------------*
017200*    PRIME RATE                                                  *
017300*----------------------------------------------------------------*
017400     03  RWF-PRM REDEFINES RWF-RATE-INFO.
017500         05  RWF-PRM-INFO            OCCURS 100 TIMES.
017600             10  RWF-PRM-KEY.
017700                 15  RWF-PRM-REGION  PIC X(10).
017800                 15  RWF-PRM-PROD    PIC XXX.
017900                 15  RWF-PRM-PTR     PIC 999.
018000*----------------------------------------------------------------*
018100*                    VALID PTR = 001                             *
018200*----------------------------------------------------------------*
018300             10  RWF-PRM-PERIOD      PIC X.
018400*----------------------------------------------------------------*
018500*                C=CURRENT, F=FUTURE                             *
018600*----------------------------------------------------------------*
018700             10  RWF-PRM-CHG         PIC X.
018800*----------------------------------------------------------------*
018900*                1=CHANGED TODAY     2=BACKDATED TODAY           *
019000*                4=CORRECTION TODAY  6=DELETED TODAY W/O DFLT RATE
019100*                7=DELETED TODAY W/ DFLT RATE                    *
019200*----------------------------------------------------------------*
019300             10  RWF-PRM-DATE        PIC X(8).
019400*----------------------------------------------------------------*
019500*                EFFECTIVE DATE                                  *
019600*----------------------------------------------------------------*
019700             10  RWF-PRM-RATES.
019800                 15  RWF-PRM-ANN     PIC S9V9(8)    COMP-3.
019900                 15  RWF-PRM-DAF     PIC SVP9(15)   COMP-3.
020000                 15  RWF-PRM-ADC     PIC X.
020100                 15  RWF-PRM-BCL     PIC X.
020200                 15  RWF-PRM-ADJ     PIC S9V9(8)    COMP-3
020300                                     OCCURS 5 TIMES.
020400             10  RWF-PRM-LMT         PIC S9(13)V99  COMP-3
020500                                     OCCURS 4 TIMES.
020600             10  RWF-PRM-INT-ADJ     PIC X.
020700             10  RWF-PRM-PRV-DAYS    PIC S9(5)        COMP-3.
020800             10  RWF-PRM-DEL-DAYS    PIC S9(5)        COMP-3.
020900             10  RWF-PRM-DEL-DATA.
021000                 15  RWF-PRM-DEL-DATE    PIC X(8).
021100                 15  RWF-PRM-DEL-RATE.
021200                     20  RWF-PRM-DEL-ANN PIC S9V9(8)     COMP-3.
021300                     20  RWF-PRM-DEL-DAF PIC SVP9(15)    COMP-3.
021400                     20  RWF-PRM-DEL-ADC PIC X.
021500                     20  RWF-PRM-DEL-BCL PIC X.
021600                     20  RWF-PRM-DEL-ADJ PIC S9V9(8)     COMP-3
021700                                         OCCURS 5 TIMES.
021800                 15  RWF-PRM-DEL-LMT     PIC S9(13)V99   COMP-3
021900                                         OCCURS 4 TIMES.
022000         05  FILLER                      PIC X(09000).
022100*----------------------------------------------------------------*
022200*    SPLIT RATES                                                 *
022300*----------------------------------------------------------------*
022400     03  RWF-SPL REDEFINES RWF-RATE-INFO.
022500         05  RWF-SPL-INFO            OCCURS 100 TIMES.
022600             10  RWF-SPL-KEY.
022700                 15  RWF-SPL-REGION  PIC X(10).
022800                 15  RWF-SPL-PROD    PIC XXX.
022900                 15  RWF-SPL-PTR     PIC 999.
023000*----------------------------------------------------------------*
023100*                    VALID PTR = 001-003                         *
023200*----------------------------------------------------------------*
023300             10  RWF-SPL-PERIOD      PIC X.
023400*----------------------------------------------------------------*
023500*                C=CURRENT, F=FUTURE                             *
023600*----------------------------------------------------------------*
023700             10  RWF-SPL-CHG         PIC X.
023800*----------------------------------------------------------------*
023900*                1=CHANGED TODAY     2=BACKDATED TODAY           *
024000*                4=CORRECTION TODAY  6=DELETED TODAY W/O DFLT RATE
024100*                7=DELETED TODAY W/ DFLT RATE                    *
024200*----------------------------------------------------------------*
024300             10  RWF-SPL-DATE        PIC X(8).
024400*----------------------------------------------------------------*
024500*                EFFECTIVE DATE                                  *
024600*----------------------------------------------------------------*
024700             10  RWF-SPL-RATES       COMP-3 OCCURS 3 TIMES.
024800                 15  RWF-SPL-ANN     PIC S9V9(8).
024900                 15  RWF-SPL-DAF     PIC SVP9(15).
025000             10  RWF-SPL-LMT         PIC S9(13)V99  COMP-3
025100                                     OCCURS 2 TIMES.
025200             10  RWF-SPL-INT-ADJ     PIC X.
025300             10  RWF-SPL-PRV-DAYS    PIC S9(5)        COMP-3.
025400             10  RWF-SPL-DEL-DAYS    PIC S9(5)        COMP-3.
025500             10  RWF-SPL-DEL-DATA.
025600                 15  RWF-SPL-DEL-DATE        PIC X(8).
025700                 15  RWF-SPL-DEL-INFO.
025800                     20  RWF-SPL-DEL-RATES   COMP-3
025900                                             OCCURS 3 TIMES.
026000                         25  RWF-SPL-DEL-ANN PIC S9V9(8).
026100                         25  RWF-SPL-DEL-DAF PIC SVP9(15).
026200                     20  RWF-SPL-DEL-LMT     PIC S9(13)V99 COMP-3
026300                                             OCCURS 2 TIMES.
026400         05  FILLER                          PIC X(15600).
026500*----------------------------------------------------------------*
026600*    WORKING STORAGE AREA FOR WORK FILE                          *
026700*----------------------------------------------------------------*
026800*    TIERED RATES                                                *
026900*----------------------------------------------------------------*
027000 01  WS-RWF-TIER-TABLE.
027100     05  WS-RWF-TIER-INFO                OCCURS 800 TIMES.
027200         10  WS-RWF-TIER-KEY.
027300             15  WS-RWF-TIER-REGION      PIC X(10).
027400             15  WS-RWF-TIER-PROD        PIC XXX.
027500             15  WS-RWF-TIER-PTR         PIC 999.
027600         10  WS-RWF-TIER-PERIOD          PIC X.
027700         10  WS-RWF-TIER-CHG             PIC X.
027800         10  WS-RWF-TIER-DATA.
027900             15  WS-RWF-TIER-DATE        PIC X(8).
028000             15  WS-RWF-TIER-RATES       OCCURS 9 TIMES   COMP-3.
028100                 20  WS-RWF-TIER-ANN     PIC S9V9(8).
028200                 20  WS-RWF-TIER-DAF     PIC SVP9(15).
028300             15  WS-RWF-TIER-LMT         PIC S9(13)V99    COMP-3
028400                                         OCCURS 8 TIMES.
028500         10  WS-RWF-TIER-INT-ADJ         PIC X.
028600         10  WS-RWF-TIER-PRV-DAYS        PIC S9(5)    COMP-3.
028700         10  WS-RWF-TIER-DEL-DAYS        PIC S9(5)    COMP-3.
028800         10  WS-RWF-TIER-DEL-DATA.
028900             15  WS-RWF-TIER-DEL-DATE    PIC X(8).
029000             15  WS-RWF-TIER-DEL-RATES   COMP-3
029100                                         OCCURS 9 TIMES.
029200                 20  WS-RWF-TIER-DEL-ANN PIC S9V9(8).
029300                 20  WS-RWF-TIER-DEL-DAF PIC SVP9(15).
029400             15  WS-RWF-TIER-DEL-LMT     PIC S9(13)V99 COMP-3
029500                                         OCCURS 8 TIMES.
029600 01  FILLER REDEFINES WS-RWF-TIER-TABLE.
029700     05  WS-RWF-TIER-INFO-DATA           OCCURS 10 TIMES.
029800         10  FILLER                      PIC X(403)
029900                                         OCCURS 80 TIMES.
030000*----------------------------------------------------------------*
030100*    SAVINGS TIERED RATES                                        *
030200*----------------------------------------------------------------*
030300 01  WS-RWF-SAVT-TABLE.
030400     05  WS-RWF-SAVT-INFO                OCCURS 800 TIMES.
030500         10  WS-RWF-SAVT-KEY.
030600             15  WS-RWF-SAVT-REGION      PIC X(10).
030700             15  WS-RWF-SAVT-PROD        PIC XXX.
030800             15  WS-RWF-SAVT-PTR         PIC 999.
030900         10  WS-RWF-SAVT-PERIOD          PIC X.
031000         10  WS-RWF-SAVT-CHG             PIC X.
031100         10  WS-RWF-SAVT-DATA.
031200             15  WS-RWF-SAVT-DATE        PIC X(8).
031300             15  WS-RWF-SAVT-RATES       OCCURS 9 TIMES   COMP-3.
031400                 20  WS-RWF-SAVT-ANN     PIC S9V9(8).
031500                 20  WS-RWF-SAVT-DAF     PIC SVP9(15).
031600             15  WS-RWF-SAVT-LMT         PIC S9(13)V99    COMP-3
031700                                         OCCURS 8 TIMES.
031800         10  WS-RWF-SAVT-INT-ADJ         PIC X.
031900         10  WS-RWF-SAVT-PRV-DAYS        PIC S9(5)    COMP-3.
032000         10  WS-RWF-SAVT-DEL-DAYS        PIC S9(5)    COMP-3.
032100         10  WS-RWF-SAVT-DEL-DATA.
032200             15  WS-RWF-SAVT-DEL-DATE    PIC X(8).
032300             15  WS-RWF-SAVT-DEL-RATES   COMP-3
032400                                         OCCURS 9 TIMES.
032500                 20  WS-RWF-SAVT-DEL-ANN PIC S9V9(8).
032600                 20  WS-RWF-SAVT-DEL-DAF PIC SVP9(15).
032700             15  WS-RWF-SAVT-DEL-LMT     PIC S9(13)V99 COMP-3
032800                                         OCCURS 8 TIMES.
032900 01  FILLER REDEFINES WS-RWF-SAVT-TABLE.
033000     05  WS-RWF-SAVT-INFO-DATA           OCCURS 10 TIMES.
033100         10  FILLER                      PIC X(403)
033200                                         OCCURS 80 TIMES.
033300*----------------------------------------------------------------*
033400*    IOD RATES                                                   *
033500*----------------------------------------------------------------*
033600 01  WS-RWF-DDA-TABLE.
033700     05  WS-RWF-DDA-INFO             OCCURS 1000 TIMES.
033800         10  WS-RWF-DDA-KEY.
033900             15  WS-RWF-DDA-REGION   PIC X(10).
034000             15  WS-RWF-DDA-PROD     PIC XXX.
034100             15  WS-RWF-DDA-PTR      PIC 999.
034200         10  WS-RWF-DDA-PERIOD       PIC X.
034300         10  WS-RWF-DDA-CHG          PIC X.
034400         10  WS-RWF-DDA-DATA.
034500             15  WS-RWF-DDA-DATE     PIC X(8).
034600             15  WS-RWF-DDA-RATES    COMP-3.
034700                 20  WS-RWF-DDA-ANN  PIC S9V9(8).
034800                 20  WS-RWF-DDA-DAF  PIC SVP9(15).
034900         10  WS-RWF-DDA-INT-ADJ      PIC X.
035000         10  WS-RWF-DDA-PRV-DAYS     PIC S9(5)        COMP-3.
035100         10  WS-RWF-DDA-DEL-DAYS     PIC S9(5)        COMP-3.
035200         10  WS-RWF-DDA-DEL-DATA.
035300             15  WS-RWF-DDA-DEL-DATE     PIC X(8).
035400             15  WS-RWF-DDA-DEL-RATES    COMP-3.
035500                 20  WS-RWF-DDA-DEL-ANN  PIC S9V9(8).
035600                 20  WS-RWF-DDA-DEL-DAF  PIC SVP9(15).
035700 01  FILLER REDEFINES WS-RWF-DDA-TABLE.
035800     05  WS-RWF-DDA-INFO-DATA            OCCURS 10 TIMES.
035900         10  FILLER                      PIC X(67)
036000                                         OCCURS 100 TIMES.
036100*----------------------------------------------------------------*
036200*    SAVINGS RATES                                               *
036300*----------------------------------------------------------------*
036400 01  WS-RWF-SAV-TABLE.
036500     05  WS-RWF-SAV-INFO             OCCURS 1000 TIMES.
036600         10  WS-RWF-SAV-KEY.
036700             15  WS-RWF-SAV-REGION   PIC X(10).
036800             15  WS-RWF-SAV-PROD     PIC XXX.
036900             15  WS-RWF-SAV-PTR      PIC 999.
037000         10  WS-RWF-SAV-PERIOD       PIC X.
037100         10  WS-RWF-SAV-CHG          PIC X.
037200         10  WS-RWF-SAV-DATA.
037300             15  WS-RWF-SAV-DATE     PIC X(8).
037400             15  WS-RWF-SAV-RATES    COMP-3.
037500                 20  WS-RWF-SAV-ANN  PIC S9V9(8).
037600                 20  WS-RWF-SAV-DAF  PIC SVP9(15).
037700         10  WS-RWF-SAV-INT-ADJ      PIC X.
037800         10  WS-RWF-SAV-PRV-DAYS     PIC S9(5)        COMP-3.
037900         10  WS-RWF-SAV-DEL-DAYS     PIC S9(5)        COMP-3.
038000         10  WS-RWF-SAV-DEL-DATA.
038100             15  WS-RWF-SAV-DEL-DATE     PIC X(8).
038200             15  WS-RWF-SAV-DEL-RATES    COMP-3.
038300                 20  WS-RWF-SAV-DEL-ANN  PIC S9V9(8).
038400                 20  WS-RWF-SAV-DEL-DAF  PIC SVP9(15).
038500 01  FILLER REDEFINES WS-RWF-SAV-TABLE.
038600     05  WS-RWF-SAV-INFO-DATA        OCCURS 10 TIMES.
038700         10  FILLER                  PIC X(67)
038800                                     OCCURS 100 TIMES.
038900*----------------------------------------------------------------*
039000*    LOAN RATES                                                  *
039100*----------------------------------------------------------------*
039200 01  WS-RWF-LOAN-TABLE.
039300     05  WS-RWF-LN-INFO              OCCURS 1000 TIMES.
039400         10  WS-RWF-LN-KEY.
039500             15  WS-RWF-LN-REGION    PIC X(10).
039600             15  WS-RWF-LN-PROD      PIC XXX.
039700             15  WS-RWF-LN-PTR       PIC 999.
039800         10  WS-RWF-LN-PERIOD        PIC X.
039900         10  WS-RWF-LN-CHG           PIC X.
040000         10  WS-RWF-LN-DATA.
040100             15  WS-RWF-LN-DATE      PIC X(8).
040200             15  WS-RWF-LN-RATES     COMP-3.
040300                 20  WS-RWF-LN-ANN   PIC S9V9(8).
040400                 20  WS-RWF-LN-DAF   PIC SVP9(15).
040500         10  WS-RWF-LN-INT-ADJ       PIC X.
040600         10  WS-RWF-LN-PRV-DAYS      PIC S9(5)        COMP-3.
040700         10  WS-RWF-LN-DEL-DAYS      PIC S9(5)        COMP-3.
040800         10  WS-RWF-LN-DEL-DATA.
040900             15  WS-RWF-LN-DEL-DATE      PIC X(8).
041000             15  WS-RWF-LN-DEL-RATES     COMP-3.
041100                 20  WS-RWF-LN-DEL-ANN   PIC S9V9(8).
041200                 20  WS-RWF-LN-DEL-DAF   PIC SVP9(15).
041300 01  FILLER REDEFINES WS-RWF-LOAN-TABLE.
041400     05  WS-RWF-LN-INFO-DATA             OCCURS 10 TIMES.
041500         10  FILLER                      PIC X(67)
041600                                         OCCURS 100 TIMES.
041700*----------------------------------------------------------------*
041800*    OD ACCRUAL RATES                                            *
041900*----------------------------------------------------------------*
042000 01  WS-RWF-OD-TABLE.
042100     05  WS-RWF-OD-INFO              OCCURS 1000 TIMES.
042200         10  WS-RWF-OD-KEY.
042300             15  WS-RWF-OD-REGION    PIC X(10).
042400             15  WS-RWF-OD-PROD      PIC XXX.
042500             15  WS-RWF-OD-PTR       PIC 999.
042600         10  WS-RWF-OD-PERIOD        PIC X.
042700         10  WS-RWF-OD-CHG           PIC X.
042800         10  WS-RWF-OD-DATA.
042900             15  WS-RWF-OD-DATE      PIC X(8).
043000             15  WS-RWF-OD-RATES     COMP-3.
043100                 20  WS-RWF-OD-ANN   PIC S9V9(8).
043200                 20  WS-RWF-OD-DAF   PIC SVP9(15).
043300         10  WS-RWF-OD-INT-ADJ       PIC X.
043400         10  WS-RWF-OD-PRV-DAYS      PIC S9(5)        COMP-3.
043500         10  WS-RWF-OD-DEL-DAYS      PIC S9(5)        COMP-3.
043600         10  WS-RWF-OD-DEL-DATA.
043700             15  WS-RWF-OD-DEL-DATE      PIC X(8).
043800             15  WS-RWF-OD-DEL-RATES     COMP-3.
043900                 20  WS-RWF-OD-DEL-ANN   PIC S9V9(8).
044000                 20  WS-RWF-OD-DEL-DAF   PIC SVP9(15).
044100 01  FILLER REDEFINES WS-RWF-OD-TABLE.
044200     05  WS-RWF-OD-INFO-DATA         OCCURS 10 TIMES.
044300         10  FILLER                  PIC X(67)
044400                                     OCCURS 100 TIMES.
044500*----------------------------------------------------------------*
044600*    PRIME RATE                                                  *
044700*----------------------------------------------------------------*
044800 01  WS-RWF-PRIME-TABLE.
044900     05  WS-RWF-PRM-INFO             OCCURS 1000 TIMES.
045000         10  WS-RWF-PRM-KEY.
045100             15  WS-RWF-PRM-REGION   PIC X(10).
045200             15  WS-RWF-PRM-PROD     PIC XXX.
045300             15  WS-RWF-PRM-PTR      PIC 999.
045400         10  WS-RWF-PRM-PERIOD       PIC X.
045500         10  WS-RWF-PRM-CHG          PIC X.
045600         10  WS-RWF-PRM-DATA.
045700             15  WS-RWF-PRM-DATE     PIC X(8).
045800             15  WS-RWF-PRM-RATES.
045900                 20  WS-RWF-PRM-ANN  PIC S9V9(8)    COMP-3.
046000                 20  WS-RWF-PRM-DAF  PIC SVP9(15)   COMP-3.
046100                 20  WS-RWF-PRM-ADC  PIC X.
046200                 20  WS-RWF-PRM-BCL  PIC X.
046300                 20  WS-RWF-PRM-ADJ  PIC S9V9(8)    COMP-3
046400                                     OCCURS 5 TIMES.
046500             15  WS-RWF-PRM-LMT      PIC S9(13)V99  COMP-3
046600                                     OCCURS 4 TIMES.
046700         10  WS-RWF-PRM-INT-ADJ      PIC X.
046800         10  WS-RWF-PRM-PRV-DAYS     PIC S9(5)      COMP-3.
046900         10  WS-RWF-PRM-DEL-DAYS     PIC S9(5)      COMP-3.
047000         10  WS-RWF-PRM-DEL-DATA.
047100             15  WS-RWF-PRM-DEL-DATE    PIC X(8).
047200             15  WS-RWF-PRM-DEL-RATE.
047300                 20  WS-RWF-PRM-DEL-ANN PIC S9V9(8)     COMP-3.
047400                 20  WS-RWF-PRM-DEL-DAF PIC SVP9(15)    COMP-3.
047500                 20  WS-RWF-PRM-DEL-ADC PIC X.
047600                 20  WS-RWF-PRM-DEL-BCL PIC X.
047700                 20  WS-RWF-PRM-DEL-ADJ PIC S9V9(8)     COMP-3
047800                                        OCCURS 5 TIMES.
047900             15  WS-RWF-PRM-DEL-LMT     PIC S9(13)V99   COMP-3
048000                                        OCCURS 4 TIMES.
048100 01  FILLER REDEFINES WS-RWF-PRIME-TABLE.
048200     05  WS-RWF-PRM-INFO-DATA        OCCURS 10 TIMES.
048300         10  FILLER                  PIC X(185)
048400                                     OCCURS 100 TIMES.
048500*----------------------------------------------------------------*
048600*    SPLIT RATES                                                 *
048700*----------------------------------------------------------------*
048800 01  WS-RWF-SPLIT-TABLE.
048900     05  WS-RWF-SPL-INFO             OCCURS 1000 TIMES.
049000         10  WS-RWF-SPL-KEY.
049100             15  WS-RWF-SPL-REGION   PIC X(10).
049200             15  WS-RWF-SPL-PROD     PIC XXX.
049300             15  WS-RWF-SPL-PTR      PIC 999.
049400         10  WS-RWF-SPL-PERIOD       PIC X.
049500         10  WS-RWF-SPL-CHG          PIC X.
049600         10  WS-RWF-SPL-DATA.
049700             15  WS-RWF-SPL-DATE     PIC X(8).
049800             15  WS-RWF-SPL-RATES    COMP-3 OCCURS 3 TIMES.
049900                 20  WS-RWF-SPL-ANN  PIC S9V9(8).
050000                 20  WS-RWF-SPL-DAF  PIC SVP9(15).
050100             15  WS-RWF-SPL-LMT      PIC S9(13)V99  COMP-3
050200                                     OCCURS 2 TIMES.
050300         10  WS-RWF-SPL-INT-ADJ      PIC X.
050400         10  WS-RWF-SPL-PRV-DAYS     PIC S9(5)      COMP-3.
050500         10  WS-RWF-SPL-DEL-DAYS     PIC S9(5)      COMP-3.
050600         10  WS-RWF-SPL-DEL-DATA.
050700             15  WS-RWF-SPL-DEL-DATE     PIC X(8).
050800             15  WS-RWF-SPL-DEL-INFO.
050900                 20  WS-RWF-SPL-DEL-RATES   COMP-3
051000                                         OCCURS 3 TIMES.
051100                     25  WS-RWF-SPL-DEL-ANN PIC S9V9(8).
051200                     25  WS-RWF-SPL-DEL-DAF PIC SVP9(15).
051300                 20  WS-RWF-SPL-DEL-LMT     PIC S9(13)V99 COMP-3
051400                                         OCCURS 2 TIMES.
051500 01  FILLER REDEFINES WS-RWF-SPLIT-TABLE.
051600     05  WS-RWF-SPL-INFO-DATA        OCCURS 10 TIMES.
051700         10  FILLER                  PIC X(151)
051800                                     OCCURS 100 TIMES.
051900*----------------------------------------------------------------*
052000*    TAX RATES                                                   *
052100*----------------------------------------------------------------*
052200 01  WS-RWF-TAX-TABLE.
052300     05  WS-RWF-TAX-INFO                 OCCURS 800 TIMES.
052400         10  WS-RWF-TAX-KEY.
052500             15  WS-RWF-TAX-REGION       PIC X(10).
052600             15  WS-RWF-TAX-PROD         PIC XXX.
052700             15  WS-RWF-TAX-PTR          PIC 999.
052800         10  WS-RWF-TAX-PERIOD           PIC X.
052900         10  WS-RWF-TAX-CHG              PIC X.
053000         10  WS-RWF-TAX-DATA.
053100             15  WS-RWF-TAX-DATE         PIC X(8).
053200             15  WS-RWF-TAX-ANN          PIC S9V9(8)
053300                                         OCCURS 9 TIMES   COMP-3.
053400             15  WS-RWF-TAX-LMT          PIC S9(13)V99    COMP-3
053500                                         OCCURS 8 TIMES.
053600         10  WS-RWF-TAX-INT-ADJ          PIC X.
053700         10  WS-RWF-TAX-PRV-DAYS         PIC S9(5)    COMP-3.
053800         10  WS-RWF-TAX-DEL-DAYS         PIC S9(5)    COMP-3.
053900         10  WS-RWF-TAX-DEL-DATA.
054000             15  WS-RWF-TAX-DEL-DATE     PIC X(8).
054100             15  WS-RWF-TAX-DEL-RATES    COMP-3
054200                                         OCCURS 9 TIMES.
054300                 20  WS-RWF-TAX-DEL-ANN PIC S9V9(8).
054400                 20  WS-RWF-TAX-DEL-DAF PIC SVP9(15).
054500             15  WS-RWF-TAX-DEL-LMT      PIC S9(13)V99 COMP-3
054600                                         OCCURS 8 TIMES.
054700 01  FILLER REDEFINES WS-RWF-TAX-TABLE.
054800     05  WS-RWF-TAX-INFO-DATA            OCCURS 10 TIMES.
054900         10  FILLER                      PIC X(403)
055000                                         OCCURS 80 TIMES.
055100*----------------------------------------------------------------*
055200*    TIERED RATES--365 DAY ACCRUAL BASIS                         *
055300*----------------------------------------------------------------*
055400 01  WS-365-TIER-TABLE.
055500     05  WS-365-TIER-INFO                OCCURS 800 TIMES.
055600         10  WS-365-TIER-KEY.
055700             15  WS-365-TIER-REGION      PIC X(10).
055800             15  WS-365-TIER-PROD        PIC XXX.
055900             15  WS-365-TIER-PTR         PIC 999.
056000         10  WS-365-TIER-PERIOD          PIC X.
056100         10  WS-365-TIER-CHG             PIC X.
056200         10  WS-365-TIER-DATA.
056300             15  WS-365-TIER-DATE        PIC X(8).
056400             15  WS-365-TIER-RATES       OCCURS 9 TIMES   COMP-3.
056500                 20  WS-365-TIER-ANN     PIC S9V9(8).
056600                 20  WS-365-TIER-DAF     PIC SVP9(15).
056700             15  WS-365-TIER-LMT         PIC S9(13)V99    COMP-3
056800                                         OCCURS 8 TIMES.
056900         10  WS-365-TIER-INT-ADJ         PIC X.
057000         10  WS-365-TIER-PRV-DAYS        PIC S9(5)    COMP-3.
057100         10  WS-365-TIER-DEL-DAYS        PIC S9(5)    COMP-3.
057200         10  WS-365-TIER-DEL-DATA.
057300             15  WS-365-TIER-DEL-DATE    PIC X(8).
057400             15  WS-365-TIER-DEL-RATES   COMP-3
057500                                         OCCURS 9 TIMES.
057600                 20  WS-365-TIER-DEL-ANN PIC S9V9(8).
057700                 20  WS-365-TIER-DEL-DAF PIC SVP9(15).
057800             15  WS-365-TIER-DEL-LMT     PIC S9(13)V99 COMP-3
057900                                         OCCURS 8 TIMES.
058000 01  FILLER REDEFINES WS-365-TIER-TABLE.
058100     05  WS-365-TIER-INFO-DATA       OCCURS 10 TIMES.
058200         10  FILLER                  PIC X(403)
058300                                     OCCURS 80 TIMES.
058400*----------------------------------------------------------------*
058500*    TIERED SAVINGS RATES--365 DAY ACCRUAL BASIS                 *
058600*----------------------------------------------------------------*
058700 01  WS-365-SAVT-TABLE.
058800     05  WS-365-SAVT-INFO                OCCURS 800 TIMES.
058900         10  WS-365-SAVT-KEY.
059000             15  WS-365-SAVT-REGION      PIC X(10).
059100             15  WS-365-SAVT-PROD        PIC XXX.
059200             15  WS-365-SAVT-PTR         PIC 999.
059300         10  WS-365-SAVT-PERIOD          PIC X.
059400         10  WS-365-SAVT-CHG             PIC X.
059500         10  WS-365-SAVT-DATA.
059600             15  WS-365-SAVT-DATE        PIC X(8).
059700             15  WS-365-SAVT-RATES       OCCURS 9 TIMES   COMP-3.
059800                 20  WS-365-SAVT-ANN     PIC S9V9(8).
059900                 20  WS-365-SAVT-DAF     PIC SVP9(15).
060000             15  WS-365-SAVT-LMT         PIC S9(13)V99    COMP-3
060100                                         OCCURS 8 TIMES.
060200         10  WS-365-SAVT-INT-ADJ         PIC X.
060300         10  WS-365-SAVT-PRV-DAYS        PIC S9(5)    COMP-3.
060400         10  WS-365-SAVT-DEL-DAYS        PIC S9(5)    COMP-3.
060500         10  WS-365-SAVT-DEL-DATA.
060600             15  WS-365-SAVT-DEL-DATE    PIC X(8).
060700             15  WS-365-SAVT-DEL-RATES   COMP-3
060800                                         OCCURS 9 TIMES.
060900                 20  WS-365-SAVT-DEL-ANN PIC S9V9(8).
061000                 20  WS-365-SAVT-DEL-DAF PIC SVP9(15).
061100             15  WS-365-SAVT-DEL-LMT     PIC S9(13)V99 COMP-3
061200                                         OCCURS 8 TIMES.
061300 01  FILLER REDEFINES WS-365-SAVT-TABLE.
061400     05  WS-365-SAVT-INFO-DATA       OCCURS 10 TIMES.
061500         10  FILLER                  PIC X(403)
061600                                     OCCURS 80 TIMES.
061700*----------------------------------------------------------------*
061800*    IOD RATES--365 DAY ACCRUAL BASIS                            *
061900*----------------------------------------------------------------*
062000 01  WS-365-DDA-TABLE.
062100     05  WS-365-DDA-INFO             OCCURS 1000 TIMES.
062200         10  WS-365-DDA-KEY.
062300             15  WS-365-DDA-REGION   PIC X(10).
062400             15  WS-365-DDA-PROD     PIC XXX.
062500             15  WS-365-DDA-PTR      PIC 999.
062600         10  WS-365-DDA-PERIOD       PIC X.
062700         10  WS-365-DDA-CHG          PIC X.
062800         10  WS-365-DDA-DATA.
062900             15  WS-365-DDA-DATE     PIC X(8).
063000             15  WS-365-DDA-RATES    COMP-3.
063100                 20  WS-365-DDA-ANN  PIC S9V9(8).
063200                 20  WS-365-DDA-DAF  PIC SVP9(15).
063300         10  WS-365-DDA-INT-ADJ      PIC X.
063400         10  WS-365-DDA-PRV-DAYS     PIC S9(5)        COMP-3.
063500         10  WS-365-DDA-DEL-DAYS     PIC S9(5)        COMP-3.
063600         10  WS-365-DDA-DEL-DATA.
063700             15  WS-365-DDA-DEL-DATE     PIC X(8).
063800             15  WS-365-DDA-DEL-RATES    COMP-3.
063900                 20  WS-365-DDA-DEL-ANN  PIC S9V9(8).
064000                 20  WS-365-DDA-DEL-DAF  PIC SVP9(15).
064100 01  FILLER REDEFINES WS-365-DDA-TABLE.
064200     05  WS-365-DDA-INFO-DATA        OCCURS 10 TIMES.
064300         10  FILLER                  PIC X(67)
064400                                     OCCURS 100 TIMES.
064500*----------------------------------------------------------------*
064600*    SAVINGS RATES--365 DAY ACCRUAL BASIS                        *
064700*----------------------------------------------------------------*
064800 01  WS-365-SAV-TABLE.
064900     05  WS-365-SAV-INFO             OCCURS 1000 TIMES.
065000         10  WS-365-SAV-KEY.
065100             15  WS-365-SAV-REGION   PIC X(10).
065200             15  WS-365-SAV-PROD     PIC XXX.
065300             15  WS-365-SAV-PTR      PIC 999.
065400         10  WS-365-SAV-PERIOD       PIC X.
065500         10  WS-365-SAV-CHG          PIC X.
065600         10  WS-365-SAV-DATA.
065700             15  WS-365-SAV-DATE     PIC X(8).
065800             15  WS-365-SAV-RATES    COMP-3.
065900                 20  WS-365-SAV-ANN  PIC S9V9(8).
066000                 20  WS-365-SAV-DAF  PIC SVP9(15).
066100         10  WS-365-SAV-INT-ADJ      PIC X.
066200         10  WS-365-SAV-PRV-DAYS     PIC S9(5)        COMP-3.
066300         10  WS-365-SAV-DEL-DAYS     PIC S9(5)        COMP-3.
066400         10  WS-365-SAV-DEL-DATA.
066500             15  WS-365-SAV-DEL-DATE     PIC X(8).
066600             15  WS-365-SAV-DEL-RATES    COMP-3.
066700                 20  WS-365-SAV-DEL-ANN  PIC S9V9(8).
066800                 20  WS-365-SAV-DEL-DAF  PIC SVP9(15).
066900 01  FILLER REDEFINES WS-365-SAV-TABLE.
067000     05  WS-365-SAV-INFO-DATA        OCCURS 10 TIMES.
067100         10  FILLER                  PIC X(67)
067200                                     OCCURS 100 TIMES.
067300*----------------------------------------------------------------*
067400*    LOAN RATES--365 DAY ACCRUAL BASIS                           *
067500*----------------------------------------------------------------*
067600 01  WS-365-LOAN-TABLE.
067700     05  WS-365-LN-INFO              OCCURS 1000 TIMES.
067800         10  WS-365-LN-KEY.
067900             15  WS-365-LN-REGION    PIC X(10).
068000             15  WS-365-LN-PROD      PIC XXX.
068100             15  WS-365-LN-PTR       PIC 999.
068200         10  WS-365-LN-PERIOD        PIC X.
068300         10  WS-365-LN-CHG           PIC X.
068400         10  WS-365-LN-DATA.
068500             15  WS-365-LN-DATE      PIC X(8).
068600             15  WS-365-LN-RATES     COMP-3.
068700                 20  WS-365-LN-ANN   PIC S9V9(8).
068800                 20  WS-365-LN-DAF   PIC SVP9(15).
068900         10  WS-365-LN-INT-ADJ       PIC X.
069000         10  WS-365-LN-PRV-DAYS      PIC S9(5)        COMP-3.
069100         10  WS-365-LN-DEL-DAYS      PIC S9(5)        COMP-3.
069200         10  WS-365-LN-DEL-DATA.
069300             15  WS-365-LN-DEL-DATE      PIC X(8).
069400             15  WS-365-LN-DEL-RATES     COMP-3.
069500                 20  WS-365-LN-DEL-ANN   PIC S9V9(8).
069600                 20  WS-365-LN-DEL-DAF   PIC SVP9(15).
069700 01  FILLER REDEFINES WS-365-LOAN-TABLE.
069800     05  WS-365-LN-INFO-DATA             OCCURS 10 TIMES.
069900         10  FILLER                      PIC X(67)
070000                                         OCCURS 100 TIMES.
070100*----------------------------------------------------------------*
070200*    OD ACCRUAL RATES--365 DAY ACCRUAL BASIS                     *
070300*----------------------------------------------------------------*
070400 01  WS-365-OD-TABLE.
070500     05  WS-365-OD-INFO              OCCURS 1000 TIMES.
070600         10  WS-365-OD-KEY.
070700             15  WS-365-OD-REGION    PIC X(10).
070800             15  WS-365-OD-PROD      PIC XXX.
070900             15  WS-365-OD-PTR       PIC 999.
071000         10  WS-365-OD-PERIOD        PIC X.
071100         10  WS-365-OD-CHG           PIC X.
071200         10  WS-365-OD-DATA.
071300             15  WS-365-OD-DATE      PIC X(8).
071400             15  WS-365-OD-RATES     COMP-3.
071500                 20  WS-365-OD-ANN   PIC S9V9(8).
071600                 20  WS-365-OD-DAF   PIC SVP9(15).
071700         10  WS-365-OD-INT-ADJ       PIC X.
071800         10  WS-365-OD-PRV-DAYS      PIC S9(5)        COMP-3.
071900         10  WS-365-OD-DEL-DAYS      PIC S9(5)        COMP-3.
072000         10  WS-365-OD-DEL-DATA.
072100             15  WS-365-OD-DEL-DATE      PIC X(8).
072200             15  WS-365-OD-DEL-RATES     COMP-3.
072300                 20  WS-365-OD-DEL-ANN   PIC S9V9(8).
072400                 20  WS-365-OD-DEL-DAF   PIC SVP9(15).
072500 01  FILLER REDEFINES WS-365-OD-TABLE.
072600     05  WS-365-OD-INFO-DATA         OCCURS 10 TIMES.
072700         10  FILLER                  PIC X(67)
072800                                     OCCURS 100 TIMES.
072900*----------------------------------------------------------------*
073000*    PRIME RATES--365 DAY ACCRUAL BASIS                          *
073100*----------------------------------------------------------------*
073200 01  WS-365-PRIME-TABLE.
073300     05  WS-365-PRM-INFO             OCCURS 1000 TIMES.
073400         10  WS-365-PRM-KEY.
073500             15  WS-365-PRM-REGION   PIC X(10).
073600             15  WS-365-PRM-PROD     PIC XXX.
073700             15  WS-365-PRM-PTR      PIC 999.
073800         10  WS-365-PRM-PERIOD       PIC X.
073900         10  WS-365-PRM-CHG          PIC X.
074000         10  WS-365-PRM-DATA.
074100             15  WS-365-PRM-DATE     PIC X(8).
074200             15  WS-365-PRM-RATES.
074300                 20  WS-365-PRM-ANN  PIC S9V9(8)    COMP-3.
074400                 20  WS-365-PRM-DAF  PIC SVP9(15)   COMP-3.
074500                 20  WS-365-PRM-ADC  PIC X.
074600                 20  WS-365-PRM-BCL  PIC X.
074700                 20  WS-365-PRM-ADJ  PIC S9V9(8)    COMP-3
074800                                     OCCURS 5 TIMES.
074900             15  WS-365-PRM-LMT      PIC S9(13)V99  COMP-3
075000                                     OCCURS 4 TIMES.
075100         10  WS-365-PRM-INT-ADJ      PIC X.
075200         10  WS-365-PRM-PRV-DAYS     PIC S9(5)      COMP-3.
075300         10  WS-365-PRM-DEL-DAYS     PIC S9(5)      COMP-3.
075400         10  WS-365-PRM-DEL-DATA.
075500             15  WS-365-PRM-DEL-DATE    PIC X(8).
075600             15  WS-365-PRM-DEL-RATE.
075700                 20  WS-365-PRM-DEL-ANN PIC S9V9(8)     COMP-3.
075800                 20  WS-365-PRM-DEL-DAF PIC SVP9(15)    COMP-3.
075900                 20  WS-365-PRM-DEL-ADC PIC X.
076000                 20  WS-365-PRM-DEL-BCL PIC X.
076100                 20  WS-365-PRM-DEL-ADJ PIC S9V9(8)     COMP-3
076200                                        OCCURS 5 TIMES.
076300             15  WS-365-PRM-DEL-LMT     PIC S9(13)V99   COMP-3
076400                                        OCCURS 4 TIMES.
076500 01  FILLER REDEFINES WS-365-PRIME-TABLE.
076600     05  WS-365-PRM-INFO-DATA        OCCURS 10 TIMES.
076700         10  FILLER                  PIC X(185)
076800                                     OCCURS 100 TIMES.
076900*----------------------------------------------------------------*
077000*    SPLIT RATES--365 DAY ACCRUAL BASIS                          *
077100*----------------------------------------------------------------*
077200 01  WS-365-SPLIT-TABLE.
077300     05  WS-365-SPL-INFO             OCCURS 1000 TIMES.
077400         10  WS-365-SPL-KEY.
077500             15  WS-365-SPL-REGION   PIC X(10).
077600             15  WS-365-SPL-PROD     PIC XXX.
077700             15  WS-365-SPL-PTR      PIC 999.
077800         10  WS-365-SPL-PERIOD       PIC X.
077900         10  WS-365-SPL-CHG          PIC X.
078000         10  WS-365-SPL-DATA.
078100             15  WS-365-SPL-DATE     PIC X(8).
078200             15  WS-365-SPL-RATES    COMP-3 OCCURS 3 TIMES.
078300                 20  WS-365-SPL-ANN  PIC S9V9(8).
078400                 20  WS-365-SPL-DAF  PIC SVP9(15).
078500             15  WS-365-SPL-LMT      PIC S9(13)V99  COMP-3
078600                                     OCCURS 2 TIMES.
078700         10  WS-365-SPL-INT-ADJ      PIC X.
078800         10  WS-365-SPL-PRV-DAYS     PIC S9(5)      COMP-3.
078900         10  WS-365-SPL-DEL-DAYS     PIC S9(5)      COMP-3.
079000         10  WS-365-SPL-DEL-DATA.
079100             15  WS-365-SPL-DEL-DATE     PIC X(8).
079200             15  WS-365-SPL-DEL-INFO.
079300                 20  WS-365-SPL-DEL-RATES   COMP-3
079400                                         OCCURS 3 TIMES.
079500                     25  WS-365-SPL-DEL-ANN PIC S9V9(8).
079600                     25  WS-365-SPL-DEL-DAF PIC SVP9(15).
079700                 20  WS-365-SPL-DEL-LMT     PIC S9(13)V99 COMP-3
079800                                         OCCURS 2 TIMES.
079900 01  FILLER REDEFINES WS-365-SPLIT-TABLE.
080000     05  WS-365-SPL-INFO-DATA        OCCURS 10 TIMES.
080100         10  FILLER                  PIC X(151)
080200                                     OCCURS 100 TIMES.
080300*----------------------------------------------------------------*
080400*    TIERED RATES--366 DAY ACCRUAL BASIS                         *
080500*----------------------------------------------------------------*
080600 01  WS-366-TIER-TABLE.
080700     05  WS-366-TIER-INFO                OCCURS 800 TIMES.
080800         10  WS-366-TIER-KEY.
080900             15  WS-366-TIER-REGION      PIC X(10).
081000             15  WS-366-TIER-PROD        PIC XXX.
081100             15  WS-366-TIER-PTR         PIC 999.
081200         10  WS-366-TIER-PERIOD          PIC X.
081300         10  WS-366-TIER-CHG             PIC X.
081400         10  WS-366-TIER-DATA.
081500             15  WS-366-TIER-DATE        PIC X(8).
081600             15  WS-366-TIER-RATES       OCCURS 9 TIMES   COMP-3.
081700                 20  WS-366-TIER-ANN     PIC S9V9(8).
081800                 20  WS-366-TIER-DAF     PIC SVP9(15).
081900             15  WS-366-TIER-LMT         PIC S9(13)V99    COMP-3
082000                                         OCCURS 8 TIMES.
082100         10  WS-366-TIER-INT-ADJ         PIC X.
082200         10  WS-366-TIER-PRV-DAYS        PIC S9(5)    COMP-3.
082300         10  WS-366-TIER-DEL-DAYS        PIC S9(5)    COMP-3.
082400         10  WS-366-TIER-DEL-DATA.
082500             15  WS-366-TIER-DEL-DATE    PIC X(8).
082600             15  WS-366-TIER-DEL-RATES   COMP-3
082700                                         OCCURS 9 TIMES.
082800                 20  WS-366-TIER-DEL-ANN PIC S9V9(8).
082900                 20  WS-366-TIER-DEL-DAF PIC SVP9(15).
083000             15  WS-366-TIER-DEL-LMT     PIC S9(13)V99 COMP-3
083100                                         OCCURS 8 TIMES.
083200 01  FILLER REDEFINES WS-366-TIER-TABLE.
083300     05  WS-366-TIER-INFO-DATA       OCCURS 10 TIMES.
083400         10  FILLER                  PIC X(403)
083500                                     OCCURS 80 TIMES.
083600*----------------------------------------------------------------*
083700*    TIERED SAVINGS RATES--366 DAY ACCRUAL BASIS                 *
083800*----------------------------------------------------------------*
083900 01  WS-366-SAVT-TABLE.
084000     05  WS-366-SAVT-INFO                OCCURS 800 TIMES.
084100         10  WS-366-SAVT-KEY.
084200             15  WS-366-SAVT-REGION      PIC X(10).
084300             15  WS-366-SAVT-PROD        PIC XXX.
084400             15  WS-366-SAVT-PTR         PIC 999.
084500         10  WS-366-SAVT-PERIOD          PIC X.
084600         10  WS-366-SAVT-CHG             PIC X.
084700         10  WS-366-SAVT-DATA.
084800             15  WS-366-SAVT-DATE        PIC X(8).
084900             15  WS-366-SAVT-RATES       OCCURS 9 TIMES   COMP-3.
085000                 20  WS-366-SAVT-ANN     PIC S9V9(8).
085100                 20  WS-366-SAVT-DAF     PIC SVP9(15).
085200             15  WS-366-SAVT-LMT         PIC S9(13)V99    COMP-3
085300                                         OCCURS 8 TIMES.
085400         10  WS-366-SAVT-INT-ADJ         PIC X.
085500         10  WS-366-SAVT-PRV-DAYS        PIC S9(5)    COMP-3.
085600         10  WS-366-SAVT-DEL-DAYS        PIC S9(5)    COMP-3.
085700         10  WS-366-SAVT-DEL-DATA.
085800             15  WS-366-SAVT-DEL-DATE    PIC X(8).
085900             15  WS-366-SAVT-DEL-RATES   COMP-3
086000                                         OCCURS 9 TIMES.
086100                 20  WS-366-SAVT-DEL-ANN PIC S9V9(8).
086200                 20  WS-366-SAVT-DEL-DAF PIC SVP9(15).
086300             15  WS-366-SAVT-DEL-LMT     PIC S9(13)V99 COMP-3
086400                                         OCCURS 8 TIMES.
086500 01  FILLER REDEFINES WS-366-SAVT-TABLE.
086600     05  WS-366-SAVT-INFO-DATA       OCCURS 10 TIMES.
086700         10  FILLER                  PIC X(403)
086800                                     OCCURS 80 TIMES.
086900*----------------------------------------------------------------*
087000*    IOD RATES--366 DAY ACCRUAL BASIS                         *
087100*----------------------------------------------------------------*
087200 01  WS-366-DDA-TABLE.
087300     05  WS-366-DDA-INFO             OCCURS 1000 TIMES.
087400         10  WS-366-DDA-KEY.
087500             15  WS-366-DDA-REGION   PIC X(10).
087600             15  WS-366-DDA-PROD     PIC XXX.
087700             15  WS-366-DDA-PTR      PIC 999.
087800         10  WS-366-DDA-PERIOD       PIC X.
087900         10  WS-366-DDA-CHG          PIC X.
088000         10  WS-366-DDA-DATA.
088100             15  WS-366-DDA-DATE     PIC X(8).
088200             15  WS-366-DDA-RATES    COMP-3.
088300                 20  WS-366-DDA-ANN  PIC S9V9(8).
088400                 20  WS-366-DDA-DAF  PIC SVP9(15).
088500         10  WS-366-DDA-INT-ADJ      PIC X.
088600         10  WS-366-DDA-PRV-DAYS     PIC S9(5)        COMP-3.
088700         10  WS-366-DDA-DEL-DAYS     PIC S9(5)        COMP-3.
088800         10  WS-366-DDA-DEL-DATA.
088900             15  WS-366-DDA-DEL-DATE     PIC X(8).
089000             15  WS-366-DDA-DEL-RATES    COMP-3.
089100                 20  WS-366-DDA-DEL-ANN  PIC S9V9(8).
089200                 20  WS-366-DDA-DEL-DAF  PIC SVP9(15).
089300 01  FILLER REDEFINES WS-366-DDA-TABLE.
089400     05  WS-366-DDA-INFO-DATA        OCCURS 10 TIMES.
089500         10  FILLER                  PIC X(67)
089600                                     OCCURS 100 TIMES.
089700*----------------------------------------------------------------*
089800*    SAVINGS RATES--366 DAY ACCRUAL BASIS                        *
089900*----------------------------------------------------------------*
090000 01  WS-366-SAV-TABLE.
090100     05  WS-366-SAV-INFO             OCCURS 1000 TIMES.
090200         10  WS-366-SAV-KEY.
090300             15  WS-366-SAV-REGION   PIC X(10).
090400             15  WS-366-SAV-PROD     PIC XXX.
090500             15  WS-366-SAV-PTR      PIC 999.
090600         10  WS-366-SAV-PERIOD       PIC X.
090700         10  WS-366-SAV-CHG          PIC X.
090800         10  WS-366-SAV-DATA.
090900             15  WS-366-SAV-DATE     PIC X(8).
091000             15  WS-366-SAV-RATES    COMP-3.
091100                 20  WS-366-SAV-ANN  PIC S9V9(8).
091200                 20  WS-366-SAV-DAF  PIC SVP9(15).
091300         10  WS-366-SAV-INT-ADJ      PIC X.
091400         10  WS-366-SAV-PRV-DAYS     PIC S9(5)        COMP-3.
091500         10  WS-366-SAV-DEL-DAYS     PIC S9(5)        COMP-3.
091600         10  WS-366-SAV-DEL-DATA.
091700             15  WS-366-SAV-DEL-DATE     PIC X(8).
091800             15  WS-366-SAV-DEL-RATES    COMP-3.
091900                 20  WS-366-SAV-DEL-ANN  PIC S9V9(8).
092000                 20  WS-366-SAV-DEL-DAF  PIC SVP9(15).
092100 01  FILLER REDEFINES WS-366-SAV-TABLE.
092200     05  WS-366-SAV-INFO-DATA        OCCURS 10 TIMES.
092300         10  FILLER                  PIC X(67)
092400                                     OCCURS 100 TIMES.
092500*----------------------------------------------------------------*
092600*    LOAN RATES--366 DAY ACCRUAL BASIS                           *
092700*----------------------------------------------------------------*
092800 01  WS-366-LOAN-TABLE.
092900     05  WS-366-LN-INFO              OCCURS 1000 TIMES.
093000         10  WS-366-LN-KEY.
093100             15  WS-366-LN-REGION    PIC X(10).
093200             15  WS-366-LN-PROD      PIC XXX.
093300             15  WS-366-LN-PTR       PIC 999.
093400         10  WS-366-LN-PERIOD        PIC X.
093500         10  WS-366-LN-CHG           PIC X.
093600         10  WS-366-LN-DATA.
093700             15  WS-366-LN-DATE      PIC X(8).
093800             15  WS-366-LN-RATES     COMP-3.
093900                 20  WS-366-LN-ANN   PIC S9V9(8).
094000                 20  WS-366-LN-DAF   PIC SVP9(15).
094100         10  WS-366-LN-INT-ADJ       PIC X.
094200         10  WS-366-LN-PRV-DAYS      PIC S9(5)        COMP-3.
094300         10  WS-366-LN-DEL-DAYS      PIC S9(5)        COMP-3.
094400         10  WS-366-LN-DEL-DATA.
094500             15  WS-366-LN-DEL-DATE      PIC X(8).
094600             15  WS-366-LN-DEL-RATES     COMP-3.
094700                 20  WS-366-LN-DEL-ANN   PIC S9V9(8).
094800                 20  WS-366-LN-DEL-DAF   PIC SVP9(15).
094900 01  FILLER REDEFINES WS-366-LOAN-TABLE.
095000     05  WS-366-LN-INFO-DATA             OCCURS 10 TIMES.
095100         10  FILLER                      PIC X(67)
095200                                         OCCURS 100 TIMES.
095300*----------------------------------------------------------------*
095400*    OD ACCRUAL RATES--366 DAY ACCRUAL BASIS                     *
095500*----------------------------------------------------------------*
095600 01  WS-366-OD-TABLE.
095700     05  WS-366-OD-INFO              OCCURS 1000 TIMES.
095800         10  WS-366-OD-KEY.
095900             15  WS-366-OD-REGION    PIC X(10).
096000             15  WS-366-OD-PROD      PIC XXX.
096100             15  WS-366-OD-PTR       PIC 999.
096200         10  WS-366-OD-PERIOD        PIC X.
096300         10  WS-366-OD-CHG           PIC X.
096400         10  WS-366-OD-DATA.
096500             15  WS-366-OD-DATE      PIC X(8).
096600             15  WS-366-OD-RATES     COMP-3.
096700                 20  WS-366-OD-ANN   PIC S9V9(8).
096800                 20  WS-366-OD-DAF   PIC SVP9(15).
096900         10  WS-366-OD-INT-ADJ       PIC X.
097000         10  WS-366-OD-PRV-DAYS      PIC S9(5)        COMP-3.
097100         10  WS-366-OD-DEL-DAYS      PIC S9(5)        COMP-3.
097200         10  WS-366-OD-DEL-DATA.
097300             15  WS-366-OD-DEL-DATE      PIC X(8).
097400             15  WS-366-OD-DEL-RATES     COMP-3.
097500                 20  WS-366-OD-DEL-ANN   PIC S9V9(8).
097600                 20  WS-366-OD-DEL-DAF   PIC SVP9(15).
097700 01  FILLER REDEFINES WS-366-OD-TABLE.
097800     05  WS-366-OD-INFO-DATA         OCCURS 10 TIMES.
097900         10  FILLER                  PIC X(67)
098000                                     OCCURS 100 TIMES.
098100*----------------------------------------------------------------*
098200*    PRIME RATES--366 DAY ACCRUAL BASIS                          *
098300*----------------------------------------------------------------*
098400 01  WS-366-PRIME-TABLE.
098500     05  WS-366-PRM-INFO             OCCURS 1000 TIMES.
098600         10  WS-366-PRM-KEY.
098700             15  WS-366-PRM-REGION   PIC X(10).
098800             15  WS-366-PRM-PROD     PIC XXX.
098900             15  WS-366-PRM-PTR      PIC 999.
099000         10  WS-366-PRM-PERIOD       PIC X.
099100         10  WS-366-PRM-CHG          PIC X.
099200         10  WS-366-PRM-DATA.
099300             15  WS-366-PRM-DATE     PIC X(8).
099400             15  WS-366-PRM-RATES.
099500                 20  WS-366-PRM-ANN  PIC S9V9(8)    COMP-3.
099600                 20  WS-366-PRM-DAF  PIC SVP9(15)   COMP-3.
099700                 20  WS-366-PRM-ADC  PIC X.
099800                 20  WS-366-PRM-BCL  PIC X.
099900                 20  WS-366-PRM-ADJ  PIC S9V9(8)    COMP-3
100000                                     OCCURS 5 TIMES.
100100             15  WS-366-PRM-LMT      PIC S9(13)V99  COMP-3
100200                                     OCCURS 4 TIMES.
100300         10  WS-366-PRM-INT-ADJ      PIC X.
100400         10  WS-366-PRM-PRV-DAYS     PIC S9(5)      COMP-3.
100500         10  WS-366-PRM-DEL-DAYS     PIC S9(5)      COMP-3.
100600         10  WS-366-PRM-DEL-DATA.
100700             15  WS-366-PRM-DEL-DATE    PIC X(8).
100800             15  WS-366-PRM-DEL-RATE.
100900                 20  WS-366-PRM-DEL-ANN PIC S9V9(8)     COMP-3.
101000                 20  WS-366-PRM-DEL-DAF PIC SVP9(15)    COMP-3.
101100                 20  WS-366-PRM-DEL-ADC PIC X.
101200                 20  WS-366-PRM-DEL-BCL PIC X.
101300                 20  WS-366-PRM-DEL-ADJ PIC S9V9(8)     COMP-3
101400                                        OCCURS 5 TIMES.
101500             15  WS-366-PRM-DEL-LMT     PIC S9(13)V99   COMP-3
101600                                        OCCURS 4 TIMES.
101700 01  FILLER REDEFINES WS-366-PRIME-TABLE.
101800     05  WS-366-PRM-INFO-DATA        OCCURS 10 TIMES.
101900         10  FILLER                  PIC X(185)
102000                                     OCCURS 100 TIMES.
102100*----------------------------------------------------------------*
102200*    SPLIT RATES--366 DAY ACCRUAL BASIS                          *
102300*----------------------------------------------------------------*
102400 01  WS-366-SPLIT-TABLE.
102500     05  WS-366-SPL-INFO             OCCURS 1000 TIMES.
102600         10  WS-366-SPL-KEY.
102700             15  WS-366-SPL-REGION   PIC X(10).
102800             15  WS-366-SPL-PROD     PIC XXX.
102900             15  WS-366-SPL-PTR      PIC 999.
103000         10  WS-366-SPL-PERIOD       PIC X.
103100         10  WS-366-SPL-CHG          PIC X.
103200         10  WS-366-SPL-DATA.
103300             15  WS-366-SPL-DATE     PIC X(8).
103400             15  WS-366-SPL-RATES    COMP-3 OCCURS 3 TIMES.
103500                 20  WS-366-SPL-ANN  PIC S9V9(8).
103600                 20  WS-366-SPL-DAF  PIC SVP9(15).
103700             15  WS-366-SPL-LMT      PIC S9(13)V99  COMP-3
103800                                     OCCURS 2 TIMES.
103900         10  WS-366-SPL-INT-ADJ      PIC X.
104000         10  WS-366-SPL-PRV-DAYS     PIC S9(5)      COMP-3.
104100         10  WS-366-SPL-DEL-DAYS     PIC S9(5)      COMP-3.
104200         10  WS-366-SPL-DEL-DATA.
104300             15  WS-366-SPL-DEL-DATE     PIC X(8).
104400             15  WS-366-SPL-DEL-INFO.
104500                 20  WS-366-SPL-DEL-RATES   COMP-3
104600                                         OCCURS 3 TIMES.
104700                     25  WS-366-SPL-DEL-ANN PIC S9V9(8).
104800                     25  WS-366-SPL-DEL-DAF PIC SVP9(15).
104900                 20  WS-366-SPL-DEL-LMT     PIC S9(13)V99 COMP-3
105000                                         OCCURS 2 TIMES.
105100 01  FILLER REDEFINES WS-366-SPLIT-TABLE.
105200     05  WS-366-SPL-INFO-DATA        OCCURS 10 TIMES.
105300         10  FILLER                  PIC X(151)
105400                                     OCCURS 100 TIMES.
105500*----------------------------------------------------------------*
105600*    TIERED RATES--360 DAY ACCRUAL BASIS                         *
105700*----------------------------------------------------------------*
105800 01  WS-360-TIER-TABLE.
105900     05  WS-360-TIER-INFO                OCCURS 800 TIMES.
106000         10  WS-360-TIER-KEY.
106100             15  WS-360-TIER-REGION      PIC X(10).
106200             15  WS-360-TIER-PROD        PIC XXX.
106300             15  WS-360-TIER-PTR         PIC 999.
106400         10  WS-360-TIER-PERIOD          PIC X.
106500         10  WS-360-TIER-CHG             PIC X.
106600         10  WS-360-TIER-DATA.
106700             15  WS-360-TIER-DATE        PIC X(8).
106800             15  WS-360-TIER-RATES       OCCURS 9 TIMES   COMP-3.
106900                 20  WS-360-TIER-ANN     PIC S9V9(8).
107000                 20  WS-360-TIER-DAF     PIC SVP9(15).
107100             15  WS-360-TIER-LMT         PIC S9(13)V99    COMP-3
107200                                         OCCURS 8 TIMES.
107300         10  WS-360-TIER-INT-ADJ         PIC X.
107400         10  WS-360-TIER-PRV-DAYS        PIC S9(5)    COMP-3.
107500         10  WS-360-TIER-DEL-DAYS        PIC S9(5)    COMP-3.
107600         10  WS-360-TIER-DEL-DATA.
107700             15  WS-360-TIER-DEL-DATE    PIC X(8).
107800             15  WS-360-TIER-DEL-RATES   COMP-3
107900                                         OCCURS 9 TIMES.
108000                 20  WS-360-TIER-DEL-ANN PIC S9V9(8).
108100                 20  WS-360-TIER-DEL-DAF PIC SVP9(15).
108200             15  WS-360-TIER-DEL-LMT     PIC S9(13)V99 COMP-3
108300                                         OCCURS 8 TIMES.
108400 01  FILLER REDEFINES WS-360-TIER-TABLE.
108500     05  WS-360-TIER-INFO-DATA       OCCURS 10 TIMES.
108600         10  FILLER                  PIC X(403)
108700                                     OCCURS 80 TIMES.
108800*----------------------------------------------------------------*
108900*    TIERED SAVINGS RATES--360 DAY ACCRUAL BASIS                 *
109000*----------------------------------------------------------------*
109100 01  WS-360-SAVT-TABLE.
109200     05  WS-360-SAVT-INFO                OCCURS 800 TIMES.
109300         10  WS-360-SAVT-KEY.
109400             15  WS-360-SAVT-REGION      PIC X(10).
109500             15  WS-360-SAVT-PROD        PIC XXX.
109600             15  WS-360-SAVT-PTR         PIC 999.
109700         10  WS-360-SAVT-PERIOD          PIC X.
109800         10  WS-360-SAVT-CHG             PIC X.
109900         10  WS-360-SAVT-DATA.
110000             15  WS-360-SAVT-DATE        PIC X(8).
110100             15  WS-360-SAVT-RATES       OCCURS 9 TIMES   COMP-3.
110200                 20  WS-360-SAVT-ANN     PIC S9V9(8).
110300                 20  WS-360-SAVT-DAF     PIC SVP9(15).
110400             15  WS-360-SAVT-LMT         PIC S9(13)V99    COMP-3
110500                                         OCCURS 8 TIMES.
110600         10  WS-360-SAVT-INT-ADJ         PIC X.
110700         10  WS-360-SAVT-PRV-DAYS        PIC S9(5)    COMP-3.
110800         10  WS-360-SAVT-DEL-DAYS        PIC S9(5)    COMP-3.
110900         10  WS-360-SAVT-DEL-DATA.
111000             15  WS-360-SAVT-DEL-DATE    PIC X(8).
111100             15  WS-360-SAVT-DEL-RATES   COMP-3
111200                                         OCCURS 9 TIMES.
111300                 20  WS-360-SAVT-DEL-ANN PIC S9V9(8).
111400                 20  WS-360-SAVT-DEL-DAF PIC SVP9(15).
111500             15  WS-360-SAVT-DEL-LMT     PIC S9(13)V99 COMP-3
111600                                         OCCURS 8 TIMES.
111700 01  FILLER REDEFINES WS-360-SAVT-TABLE.
111800     05  WS-360-SAVT-INFO-DATA       OCCURS 10 TIMES.
111900         10  FILLER                  PIC X(403)
112000                                     OCCURS 80 TIMES.
112100*----------------------------------------------------------------*
112200*    IOD RATES--360 DAY ACCRUAL BASIS                          *
112300*----------------------------------------------------------------*
112400 01  WS-360-DDA-TABLE.
112500     05  WS-360-DDA-INFO             OCCURS 1000 TIMES.
112600         10  WS-360-DDA-KEY.
112700             15  WS-360-DDA-REGION   PIC X(10).
112800             15  WS-360-DDA-PROD     PIC XXX.
112900             15  WS-360-DDA-PTR      PIC 999.
113000         10  WS-360-DDA-PERIOD       PIC X.
113100         10  WS-360-DDA-CHG          PIC X.
113200         10  WS-360-DDA-DATA.
113300             15  WS-360-DDA-DATE     PIC X(8).
113400             15  WS-360-DDA-RATES    COMP-3.
113500                 20  WS-360-DDA-ANN  PIC S9V9(8).
113600                 20  WS-360-DDA-DAF  PIC SVP9(15).
113700         10  WS-360-DDA-INT-ADJ      PIC X.
113800         10  WS-360-DDA-PRV-DAYS     PIC S9(5)        COMP-3.
113900         10  WS-360-DDA-DEL-DAYS     PIC S9(5)        COMP-3.
114000         10  WS-360-DDA-DEL-DATA.
114100             15  WS-360-DDA-DEL-DATE     PIC X(8).
114200             15  WS-360-DDA-DEL-RATES    COMP-3.
114300                 20  WS-360-DDA-DEL-ANN  PIC S9V9(8).
114400                 20  WS-360-DDA-DEL-DAF  PIC SVP9(15).
114500 01  FILLER REDEFINES WS-360-DDA-TABLE.
114600     05  WS-360-DDA-INFO-DATA        OCCURS 10 TIMES.
114700         10  FILLER                  PIC X(67)
114800                                     OCCURS 100 TIMES.
114900*----------------------------------------------------------------*
115000*    SAVINGS RATES--360 DAY ACCRUAL BASIS                        *
115100*----------------------------------------------------------------*
115200 01  WS-360-SAV-TABLE.
115300     05  WS-360-SAV-INFO             OCCURS 1000 TIMES.
115400         10  WS-360-SAV-KEY.
115500             15  WS-360-SAV-REGION   PIC X(10).
115600             15  WS-360-SAV-PROD     PIC XXX.
115700             15  WS-360-SAV-PTR      PIC 999.
115800         10  WS-360-SAV-PERIOD       PIC X.
115900         10  WS-360-SAV-CHG          PIC X.
116000         10  WS-360-SAV-DATA.
116100             15  WS-360-SAV-DATE     PIC X(8).
116200             15  WS-360-SAV-RATES    COMP-3.
116300                 20  WS-360-SAV-ANN  PIC S9V9(8).
116400                 20  WS-360-SAV-DAF  PIC SVP9(15).
116500         10  WS-360-SAV-INT-ADJ      PIC X.
116600         10  WS-360-SAV-PRV-DAYS     PIC S9(5)        COMP-3.
116700         10  WS-360-SAV-DEL-DAYS     PIC S9(5)        COMP-3.
116800         10  WS-360-SAV-DEL-DATA.
116900             15  WS-360-SAV-DEL-DATE     PIC X(8).
117000             15  WS-360-SAV-DEL-RATES    COMP-3.
117100                 20  WS-360-SAV-DEL-ANN  PIC S9V9(8).
117200                 20  WS-360-SAV-DEL-DAF  PIC SVP9(15).
117300 01  FILLER REDEFINES WS-360-SAV-TABLE.
117400     05  WS-360-SAV-INFO-DATA        OCCURS 10 TIMES.
117500         10  FILLER                  PIC X(67)
117600                                     OCCURS 100 TIMES.
117700*----------------------------------------------------------------*
117800*    LOAN RATES--360 DAY ACCRUAL BASIS                           *
117900*----------------------------------------------------------------*
118000 01  WS-360-LOAN-TABLE.
118100     05  WS-360-LN-INFO              OCCURS 1000 TIMES.
118200         10  WS-360-LN-KEY.
118300             15  WS-360-LN-REGION    PIC X(10).
118400             15  WS-360-LN-PROD      PIC XXX.
118500             15  WS-360-LN-PTR       PIC 999.
118600         10  WS-360-LN-PERIOD        PIC X.
118700         10  WS-360-LN-CHG           PIC X.
118800         10  WS-360-LN-DATA.
118900             15  WS-360-LN-DATE      PIC X(8).
119000             15  WS-360-LN-RATES     COMP-3.
119100                 20  WS-360-LN-ANN   PIC S9V9(8).
119200                 20  WS-360-LN-DAF   PIC SVP9(15).
119300         10  WS-360-LN-INT-ADJ       PIC X.
119400         10  WS-360-LN-PRV-DAYS      PIC S9(5)        COMP-3.
119500         10  WS-360-LN-DEL-DAYS      PIC S9(5)        COMP-3.
119600         10  WS-360-LN-DEL-DATA.
119700             15  WS-360-LN-DEL-DATE      PIC X(8).
119800             15  WS-360-LN-DEL-RATES     COMP-3.
119900                 20  WS-360-LN-DEL-ANN   PIC S9V9(8).
120000                 20  WS-360-LN-DEL-DAF   PIC SVP9(15).
120100 01  FILLER REDEFINES WS-360-LOAN-TABLE.
120200     05  WS-360-LN-INFO-DATA             OCCURS 10 TIMES.
120300         10  FILLER                      PIC X(67)
120400                                         OCCURS 100 TIMES.
120500*----------------------------------------------------------------*
120600*    OD ACCRUAL RATES--360 DAY ACCRUAL BASIS                     *
120700*----------------------------------------------------------------*
120800 01  WS-360-OD-TABLE.
120900     05  WS-360-OD-INFO              OCCURS 1000 TIMES.
121000         10  WS-360-OD-KEY.
121100             15  WS-360-OD-REGION    PIC X(10).
121200             15  WS-360-OD-PROD      PIC XXX.
121300             15  WS-360-OD-PTR       PIC 999.
121400         10  WS-360-OD-PERIOD        PIC X.
121500         10  WS-360-OD-CHG           PIC X.
121600         10  WS-360-OD-DATA.
121700             15  WS-360-OD-DATE      PIC X(8).
121800             15  WS-360-OD-RATES     COMP-3.
121900                 20  WS-360-OD-ANN   PIC S9V9(8).
122000                 20  WS-360-OD-DAF   PIC SVP9(15).
122100         10  WS-360-OD-INT-ADJ       PIC X.
122200         10  WS-360-OD-PRV-DAYS      PIC S9(5)        COMP-3.
122300         10  WS-360-OD-DEL-DAYS      PIC S9(5)        COMP-3.
122400         10  WS-360-OD-DEL-DATA.
122500             15  WS-360-OD-DEL-DATE      PIC X(8).
122600             15  WS-360-OD-DEL-RATES     COMP-3.
122700                 20  WS-360-OD-DEL-ANN   PIC S9V9(8).
122800                 20  WS-360-OD-DEL-DAF   PIC SVP9(15).
122900 01  FILLER REDEFINES WS-360-OD-TABLE.
123000     05  WS-360-OD-INFO-DATA         OCCURS 10 TIMES.
123100         10  FILLER                  PIC X(67)
123200                                     OCCURS 100 TIMES.
123300*----------------------------------------------------------------*
123400*    PRIME RATES--360 DAY ACCRUAL BASIS                          *
123500*----------------------------------------------------------------*
123600 01  WS-360-PRIME-TABLE.
123700     05  WS-360-PRM-INFO             OCCURS 1000 TIMES.
123800         10  WS-360-PRM-KEY.
123900             15  WS-360-PRM-REGION   PIC X(10).
124000             15  WS-360-PRM-PROD     PIC XXX.
124100             15  WS-360-PRM-PTR      PIC 999.
124200         10  WS-360-PRM-PERIOD       PIC X.
124300         10  WS-360-PRM-CHG          PIC X.
124400         10  WS-360-PRM-DATA.
124500             15  WS-360-PRM-DATE     PIC X(8).
124600             15  WS-360-PRM-RATES.
124700                 20  WS-360-PRM-ANN  PIC S9V9(8)    COMP-3.
124800                 20  WS-360-PRM-DAF  PIC SVP9(15)   COMP-3.
124900                 20  WS-360-PRM-ADC  PIC X.
125000                 20  WS-360-PRM-BCL  PIC X.
125100                 20  WS-360-PRM-ADJ  PIC S9V9(8)    COMP-3
125200                                     OCCURS 5 TIMES.
125300             15  WS-360-PRM-LMT      PIC S9(13)V99  COMP-3
125400                                     OCCURS 4 TIMES.
125500         10  WS-360-PRM-INT-ADJ      PIC X.
125600         10  WS-360-PRM-PRV-DAYS     PIC S9(5)      COMP-3.
125700         10  WS-360-PRM-DEL-DAYS     PIC S9(5)      COMP-3.
125800         10  WS-360-PRM-DEL-DATA.
125900             15  WS-360-PRM-DEL-DATE    PIC X(8).
126000             15  WS-360-PRM-DEL-RATE.
126100                 20  WS-360-PRM-DEL-ANN PIC S9V9(8)     COMP-3.
126200                 20  WS-360-PRM-DEL-DAF PIC SVP9(15)    COMP-3.
126300                 20  WS-360-PRM-DEL-ADC PIC X.
126400                 20  WS-360-PRM-DEL-BCL PIC X.
126500                 20  WS-360-PRM-DEL-ADJ PIC S9V9(8)     COMP-3
126600                                        OCCURS 5 TIMES.
126700             15  WS-360-PRM-DEL-LMT     PIC S9(13)V99   COMP-3
126800                                        OCCURS 4 TIMES.
126900 01  FILLER REDEFINES WS-360-PRIME-TABLE.
127000     05  WS-360-PRM-INFO-DATA        OCCURS 10 TIMES.
127100         10  FILLER                  PIC X(185)
127200                                     OCCURS 100 TIMES.
127300*----------------------------------------------------------------*
127400*    SPLIT RATES--360 DAY ACCRUAL BASIS                          *
127500*----------------------------------------------------------------*
127600 01  WS-360-SPLIT-TABLE.
127700     05  WS-360-SPL-INFO             OCCURS 1000 TIMES.
127800         10  WS-360-SPL-KEY.
127900             15  WS-360-SPL-REGION   PIC X(10).
128000             15  WS-360-SPL-PROD     PIC XXX.
128100             15  WS-360-SPL-PTR      PIC 999.
128200         10  WS-360-SPL-PERIOD       PIC X.
128300         10  WS-360-SPL-CHG          PIC X.
128400         10  WS-360-SPL-DATA.
128500             15  WS-360-SPL-DATE     PIC X(8).
128600             15  WS-360-SPL-RATES    COMP-3 OCCURS 3 TIMES.
128700                 20  WS-360-SPL-ANN  PIC S9V9(8).
128800                 20  WS-360-SPL-DAF  PIC SVP9(15).
128900             15  WS-360-SPL-LMT      PIC S9(13)V99  COMP-3
129000                                     OCCURS 2 TIMES.
129100         10  WS-360-SPL-INT-ADJ      PIC X.
129200         10  WS-360-SPL-PRV-DAYS     PIC S9(5)      COMP-3.
129300         10  WS-360-SPL-DEL-DAYS     PIC S9(5)      COMP-3.
129400         10  WS-360-SPL-DEL-DATA.
129500             15  WS-360-SPL-DEL-DATE     PIC X(8).
129600             15  WS-360-SPL-DEL-INFO.
129700                 20  WS-360-SPL-DEL-RATES   COMP-3
129800                                         OCCURS 3 TIMES.
129900                     25  WS-360-SPL-DEL-ANN PIC S9V9(8).
130000                     25  WS-360-SPL-DEL-DAF PIC SVP9(15).
130100                 20  WS-360-SPL-DEL-LMT     PIC S9(13)V99 COMP-3
130200                                         OCCURS 2 TIMES.
130300 01  FILLER REDEFINES WS-360-SPLIT-TABLE.
130400     05  WS-360-SPL-INFO-DATA        OCCURS 10 TIMES.
130500         10  FILLER                  PIC X(151)
130600                                     OCCURS 100 TIMES.
