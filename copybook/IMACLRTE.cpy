*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*
000200*----------------------------------------------------------------*
000300*            CLEAR RATE TRAILER PORTION OF THE MASTER            *
000400*----------------------------------------------------------------*
000500*
000600 01  CLEAR-RATE-TRAILER.
000700     05  CRT-RATE-TRAILER-FLAGS.
000800         10  CRT-DDA-RATE-FLAG       PIC X       VALUE 'N'.
000900         10  CRT-SAV-RATE-FLAG       PIC X       VALUE 'N'.
001000         10  CRT-OD-RATE-FLAG        PIC X       VALUE 'N'.
001100         10  CRT-LN-RATE-FLAG        PIC X       VALUE 'N'.
001110         10  CRT-FED-TAX-RATE-FLAG   PIC X       VALUE 'N'.       2012254
001120         10  CRT-ST-TAX-RATE-FLAG    PIC X       VALUE 'N'.       2012254
001130         10  CRT-LOC-TAX-RATE-FLAG   PIC X       VALUE 'N'.       2012254
001200
001300     05  CRT-IOD-RATES.
001400         10  CRT-IOD-RATE-KEY-LEVEL   PIC X         VALUE SPACE.
001500         10  CRT-IOD-CUR-DATA.
001600             15  CRT-IOD-CUR-DATE     PIC X(8)      VALUE SPACES.
001700             15  CRT-IOD-CUR-RATES    COMP-3.
001800                 20  CRT-IOD-CUR-ANN  PIC S9V9(8)   VALUE +.0.
001900                 20  CRT-IOD-CUR-DAF  PIC SVP9(15)  VALUE +.0.
002000         10  CRT-IOD-PREV-DATA.
002100             15  CRT-IOD-PREV-DATE    PIC X(8)      VALUE SPACES.
002200             15  CRT-IOD-PREV-RATES   COMP-3.
002300                 20  CRT-IOD-PREV-ANN PIC S9V9(8)   VALUE +.0.
002400                 20  CRT-IOD-PREV-DAF PIC SVP9(15)  VALUE +.0.
002500         10  FILLER                   PIC X(378)    VALUE SPACES.
002600     05  CRT-TIER-RATES.
002700         10  CRT-TIER-RATE-KEY-LEVEL   PIC X     VALUE SPACE.
002800         10  CRT-TIER-CUR-DATA.
002900             15  CRT-TIER-CUR-DATE     PIC X(8)  VALUE SPACES.
003000             15  CRT-TIER-CUR-RATES1   COMP-3.
003100                 20  CRT-TIER-CUR-ANN1 PIC S9V9(8)     VALUE +.0.
003200                 20  CRT-TIER-CUR-DAF1 PIC SVP9(15)    VALUE +.0.
003300             15  CRT-TIER-CUR-RATES2   COMP-3.
003400                 20  CRT-TIER-CUR-ANN2 PIC S9V9(8)     VALUE +.0.
003500                 20  CRT-TIER-CUR-DAF2 PIC SVP9(15)    VALUE +.0.
003600             15  CRT-TIER-CUR-RATES3   COMP-3.
003700                 20  CRT-TIER-CUR-ANN3 PIC S9V9(8)     VALUE +.0.
003800                 20  CRT-TIER-CUR-DAF3 PIC SVP9(15)    VALUE +.0.
003900             15  CRT-TIER-CUR-RATES4   COMP-3.
004000                 20  CRT-TIER-CUR-ANN4 PIC S9V9(8)     VALUE +.0.
004100                 20  CRT-TIER-CUR-DAF4 PIC SVP9(15)    VALUE +.0.
004200             15  CRT-TIER-CUR-RATES5   COMP-3.
004300                 20  CRT-TIER-CUR-ANN5 PIC S9V9(8)     VALUE +.0.
004400                 20  CRT-TIER-CUR-DAF5 PIC SVP9(15)    VALUE +.0.
004500             15  CRT-TIER-CUR-RATES6   COMP-3.
004600                 20  CRT-TIER-CUR-ANN6 PIC S9V9(8)     VALUE +.0.
004700                 20  CRT-TIER-CUR-DAF6 PIC SVP9(15)    VALUE +.0.
004800             15  CRT-TIER-CUR-RATES7   COMP-3.
004900                 20  CRT-TIER-CUR-ANN7 PIC S9V9(8)     VALUE +.0.
005000                 20  CRT-TIER-CUR-DAF7 PIC SVP9(15)    VALUE +.0.
005100             15  CRT-TIER-CUR-RATES8   COMP-3.
005200                 20  CRT-TIER-CUR-ANN8 PIC S9V9(8)     VALUE +.0.
005300                 20  CRT-TIER-CUR-DAF8 PIC SVP9(15)    VALUE +.0.
005400             15  CRT-TIER-CUR-RATES9   COMP-3.
005500                 20  CRT-TIER-CUR-ANN9 PIC S9V9(8)     VALUE +.0.
005600                 20  CRT-TIER-CUR-DAF9 PIC SVP9(15)    VALUE +.0.
005700             15  CRT-TIER-CUR-LMT1 PIC S9(13)V99 COMP-3 VALUE +0.
005800             15  CRT-TIER-CUR-LMT2 PIC S9(13)V99 COMP-3 VALUE +0.
005900             15  CRT-TIER-CUR-LMT3 PIC S9(13)V99 COMP-3 VALUE +0.
006000             15  CRT-TIER-CUR-LMT4 PIC S9(13)V99 COMP-3 VALUE +0.
006100             15  CRT-TIER-CUR-LMT5 PIC S9(13)V99 COMP-3 VALUE +0.
006200             15  CRT-TIER-CUR-LMT6 PIC S9(13)V99 COMP-3 VALUE +0.
006300             15  CRT-TIER-CUR-LMT7 PIC S9(13)V99 COMP-3 VALUE +0.
006400             15  CRT-TIER-CUR-LMT8 PIC S9(13)V99 COMP-3 VALUE +0.
006500         10  CRT-DFLT-IOD-CUR-DATA.
006600             15  CRT-DFLT-IOD-CUR-DATE    PIC X(8) VALUE SPACES.
006700             15  CRT-DFLT-IOD-CUR-RATES   COMP-3.
006800                 20  CRT-DFLT-IOD-CUR-ANN PIC S9V9(8)   VALUE +.0.
006900                 20  CRT-DFLT-IOD-CUR-DAF PIC SVP9(15)  VALUE +.0.
007000         10  CRT-TIER-PREV-DATA.
007100             15  CRT-TIER-PREV-DATE     PIC X(8) VALUE SPACES.
007200             15  CRT-TIER-PREV-RATES1   COMP-3.
007300                 20  CRT-TIER-PREV-ANN1 PIC S9V9(8)    VALUE +.0.
007400                 20  CRT-TIER-PREV-DAF1 PIC SVP9(15)   VALUE +.0.
007500             15  CRT-TIER-PREV-RATES2   COMP-3.
007600                 20  CRT-TIER-PREV-ANN2 PIC S9V9(8)    VALUE +.0.
007700                 20  CRT-TIER-PREV-DAF2 PIC SVP9(15)   VALUE +.0.
007800             15  CRT-TIER-PREV-RATES3   COMP-3.
007900                 20  CRT-TIER-PREV-ANN3 PIC S9V9(8)    VALUE +.0.
008000                 20  CRT-TIER-PREV-DAF3 PIC SVP9(15)   VALUE +.0.
008100             15  CRT-TIER-PREV-RATES4   COMP-3.
008200                 20  CRT-TIER-PREV-ANN4 PIC S9V9(8)    VALUE +.0.
008300                 20  CRT-TIER-PREV-DAF4 PIC SVP9(15)   VALUE +.0.
008400             15  CRT-TIER-PREV-RATES5   COMP-3.
008500                 20  CRT-TIER-PREV-ANN5 PIC S9V9(8)    VALUE +.0.
008600                 20  CRT-TIER-PREV-DAF5 PIC SVP9(15)   VALUE +.0.
008700             15  CRT-TIER-PREV-RATES6   COMP-3.
008800                 20  CRT-TIER-PREV-ANN6 PIC S9V9(8)    VALUE +.0.
008900                 20  CRT-TIER-PREV-DAF6 PIC SVP9(15)   VALUE +.0.
009000             15  CRT-TIER-PREV-RATES7   COMP-3.
009100                 20  CRT-TIER-PREV-ANN7 PIC S9V9(8)    VALUE +.0.
009200                 20  CRT-TIER-PREV-DAF7 PIC SVP9(15)   VALUE +.0.
009300             15  CRT-TIER-PREV-RATES8   COMP-3.
009400                 20  CRT-TIER-PREV-ANN8 PIC S9V9(8)    VALUE +.0.
009500                 20  CRT-TIER-PREV-DAF8 PIC SVP9(15)   VALUE +.0.
009600             15  CRT-TIER-PREV-RATES9   COMP-3.
009700                 20  CRT-TIER-PREV-ANN9 PIC S9V9(8)    VALUE +.0.
009800                 20  CRT-TIER-PREV-DAF9 PIC SVP9(15)   VALUE +.0.
009900             15  CRT-TIER-PREV-LMT1 PIC S9(13)V99 COMP-3 VALUE +0.
010000             15  CRT-TIER-PREV-LMT2 PIC S9(13)V99 COMP-3 VALUE +0.
010100             15  CRT-TIER-PREV-LMT3 PIC S9(13)V99 COMP-3 VALUE +0.
010200             15  CRT-TIER-PREV-LMT4 PIC S9(13)V99 COMP-3 VALUE +0.
010300             15  CRT-TIER-PREV-LMT5 PIC S9(13)V99 COMP-3 VALUE +0.
010400             15  CRT-TIER-PREV-LMT6 PIC S9(13)V99 COMP-3 VALUE +0.
010500             15  CRT-TIER-PREV-LMT7 PIC S9(13)V99 COMP-3 VALUE +0.
010600             15  CRT-TIER-PREV-LMT8 PIC S9(13)V99 COMP-3 VALUE +0.
010700         10  CRT-DFLT-IOD-PREV-DATA.
010800             15  CRT-DFLT-IOD-PREV-DATE    PIC X(8) VALUE SPACES.
010900             15  CRT-DFLT-IOD-PREV-RATES   COMP-3.
011000                 20  CRT-DFLT-IOD-PREV-ANN PIC S9V9(8) VALUE +.0.
011100                 20  CRT-DFLT-IOD-PREV-DAF PIC SVP9(15)
011200                                                       VALUE +.0.
011300
011400     05  CRT-SAV-RATES.
011500         10  CRT-SAV-RATE-KEY-LEVEL   PIC X         VALUE SPACE.
011600         10  CRT-SAV-CUR-DATA.
011700             15  CRT-SAV-CUR-DATE     PIC X(8)      VALUE SPACES.
011800             15  CRT-SAV-CUR-RATES    COMP-3.
011900                 20  CRT-SAV-CUR-ANN  PIC S9V9(8)   VALUE +.0.
012000                 20  CRT-SAV-CUR-DAF  PIC SVP9(15)  VALUE +.0.
012100         10  CRT-SAV-PREV-DATA.
012200             15  CRT-SAV-PREV-DATE    PIC X(8)      VALUE SPACES.
012300             15  CRT-SAV-PREV-RATES   COMP-3.
012400                 20  CRT-SAV-PREV-ANN PIC S9V9(8)   VALUE +.0.
012500                 20  CRT-SAV-PREV-DAF PIC SVP9(15)  VALUE +.0.
012600
012700     05  CRT-OD-RATES.
012800         10  CRT-OD-RATE-KEY-LEVEL   PIC X         VALUE SPACE.
012900         10  CRT-OD-CUR-DATA.
013000             15  CRT-OD-CUR-DATE1    PIC X(8)      VALUE SPACES.
013100             15  CRT-OD-CUR-RATES1   COMP-3.
013200                 20  CRT-OD-CUR-ANN1 PIC S9V9(8)   VALUE +.0.
013300                 20  CRT-OD-CUR-DAF1 PIC SVP9(15)  VALUE +.0.
013400             15  CRT-OD-CUR-DATE2    PIC X(8)      VALUE SPACES.
013500             15  CRT-OD-CUR-RATES2   COMP-3.
013600                 20  CRT-OD-CUR-ANN2 PIC S9V9(8)   VALUE +.0.
013700                 20  CRT-OD-CUR-DAF2 PIC SVP9(15)  VALUE +.0.
013800             15  CRT-OD-CUR-DATE3    PIC X(8)      VALUE SPACES.
013900             15  CRT-OD-CUR-RATES3   COMP-3.
014000                 20  CRT-OD-CUR-ANN3 PIC S9V9(8)   VALUE +.0.
014100                 20  CRT-OD-CUR-DAF3 PIC SVP9(15)  VALUE +.0.
014200             15  CRT-OD-CUR-DATE4    PIC X(8)      VALUE SPACES.
014300             15  CRT-OD-CUR-RATES4   COMP-3.
014400                 20  CRT-OD-CUR-ANN4 PIC S9V9(8)   VALUE +.0.
014500                 20  CRT-OD-CUR-DAF4 PIC SVP9(15)  VALUE +.0.
014600             15  CRT-OD-CUR-DATE5    PIC X(8)      VALUE SPACES.
014700             15  CRT-OD-CUR-RATES5   COMP-3.
014800                 20  CRT-OD-CUR-ANN5 PIC S9V9(8)   VALUE +.0.
014900                 20  CRT-OD-CUR-DAF5 PIC SVP9(15)  VALUE +.0.
015000             15  CRT-OD-CUR-DATE6    PIC X(8)      VALUE SPACES.
015100             15  CRT-OD-CUR-RATES6   COMP-3.
015200                 20  CRT-OD-CUR-ANN6 PIC S9V9(8)   VALUE +.0.
015300                 20  CRT-OD-CUR-DAF6 PIC SVP9(15)  VALUE +.0.
015400             15  CRT-OD-CUR-DATE7    PIC X(8)      VALUE SPACES.
015500             15  CRT-OD-CUR-RATES7   COMP-3.
015600                 20  CRT-OD-CUR-ANN7 PIC S9V9(8)   VALUE +.0.
015700                 20  CRT-OD-CUR-DAF7 PIC SVP9(15)  VALUE +.0.
015800             15  CRT-OD-CUR-DATE8    PIC X(8)      VALUE SPACES.
015900             15  CRT-OD-CUR-RATES8   COMP-3.
016000                 20  CRT-OD-CUR-ANN8 PIC S9V9(8)   VALUE +.0.
016100                 20  CRT-OD-CUR-DAF8 PIC SVP9(15)  VALUE +.0.
016200             15  CRT-OD-CUR-DATE9    PIC X(8)      VALUE SPACES.
016300             15  CRT-OD-CUR-RATES9   COMP-3.
016400                 20  CRT-OD-CUR-ANN9 PIC S9V9(8)   VALUE +.0.
016500                 20  CRT-OD-CUR-DAF9 PIC SVP9(15)  VALUE +.0.
016600         10  CRT-OD-PRM-CUR-DATA.
016700             15  CRT-OD-PRM-CUR-DATE      PIC X(8) VALUE SPACES.
016800             15  CRT-OD-CUR-PRIME         COMP-3.
016900                 20  CRT-OD-PRM-CUR-ANN   PIC S9V9(8)   VALUE +.0.
017000                 20  CRT-OD-PRM-CUR-DAF   PIC SVP9(15)  VALUE +.0.
017100         10  CRT-OD-PREV-DATA.
017200             15  CRT-OD-PREV-DATE1    PIC X(8)      VALUE SPACES.
017300             15  CRT-OD-PREV-RATES1   COMP-3.
017400                 20  CRT-OD-PREV-ANN1 PIC S9V9(8)   VALUE +.0.
017500                 20  CRT-OD-PREV-DAF1 PIC SVP9(15)  VALUE +.0.
017600             15  CRT-OD-PREV-DATE2    PIC X(8)      VALUE SPACES.
017700             15  CRT-OD-PREV-RATES2   COMP-3.
017800                 20  CRT-OD-PREV-ANN2 PIC S9V9(8)   VALUE +.0.
017900                 20  CRT-OD-PREV-DAF2 PIC SVP9(15)  VALUE +.0.
018000             15  CRT-OD-PREV-DATE3    PIC X(8)      VALUE SPACES.
018100             15  CRT-OD-PREV-RATES3   COMP-3.
018200                 20  CRT-OD-PREV-ANN3 PIC S9V9(8)   VALUE +.0.
018300                 20  CRT-OD-PREV-DAF3 PIC SVP9(15)  VALUE +.0.
018400             15  CRT-OD-PREV-DATE4    PIC X(8)      VALUE SPACES.
018500             15  CRT-OD-PREV-RATES4   COMP-3.
018600                 20  CRT-OD-PREV-ANN4 PIC S9V9(8)   VALUE +.0.
018700                 20  CRT-OD-PREV-DAF4 PIC SVP9(15)  VALUE +.0.
018800             15  CRT-OD-PREV-DATE5    PIC X(8)      VALUE SPACES.
018900             15  CRT-OD-PREV-RATES5   COMP-3.
019000                 20  CRT-OD-PREV-ANN5 PIC S9V9(8)   VALUE +.0.
019100                 20  CRT-OD-PREV-DAF5 PIC SVP9(15)  VALUE +.0.
019200             15  CRT-OD-PREV-DATE6    PIC X(8)      VALUE SPACES.
019300             15  CRT-OD-PREV-RATES6   COMP-3.
019400                 20  CRT-OD-PREV-ANN6 PIC S9V9(8)   VALUE +.0.
019500                 20  CRT-OD-PREV-DAF6 PIC SVP9(15)  VALUE +.0.
019600             15  CRT-OD-PREV-DATE7    PIC X(8)      VALUE SPACES.
019700             15  CRT-OD-PREV-RATES7   COMP-3.
019800                 20  CRT-OD-PREV-ANN7 PIC S9V9(8)   VALUE +.0.
019900                 20  CRT-OD-PREV-DAF7 PIC SVP9(15)  VALUE +.0.
020000             15  CRT-OD-PREV-DATE8    PIC X(8)      VALUE SPACES.
020100             15  CRT-OD-PREV-RATES8   COMP-3.
020200                 20  CRT-OD-PREV-ANN8 PIC S9V9(8)   VALUE +.0.
020300                 20  CRT-OD-PREV-DAF8 PIC SVP9(15)  VALUE +.0.
020400             15  CRT-OD-PREV-DATE9    PIC X(8)      VALUE SPACES.
020500             15  CRT-OD-PREV-RATES9   COMP-3.
020600                 20  CRT-OD-PREV-ANN9 PIC S9V9(8)   VALUE +.0.
020700                 20  CRT-OD-PREV-DAF9 PIC SVP9(15)  VALUE +.0.
020800         10  CRT-OD-PRM-PREV-DATA.
020900             15  CRT-OD-PRM-PREV-DATE     PIC X(8) VALUE SPACES.
021000             15  CRT-OD-PREV-PRIME        COMP-3.
021100                 20  CRT-OD-PRM-PREV-ANN  PIC S9V9(8)   VALUE +.0.
021200                 20  CRT-OD-PRM-PREV-DAF  PIC SVP9(15)  VALUE +0.
021300
021400     05  CRT-PRM-RATE.
021500         10  CRT-PRM-RATE-KEY-LEVEL  PIC X       VALUE SPACE.
021600         10  CRT-PRM-CUR-DATA.
021700             15  CRT-PRM-CUR-DATE    PIC X(8)    VALUE SPACES.
021800             15  CRT-PRM-CUR-RATES.
021900                 20  CRT-PRM-CUR-ANN PIC S9V9(8) COMP-3 VALUE +.0.
022000                 20  CRT-PRM-CUR-DAF PIC SVP9(15) COMP-3
022100                                                        VALUE +.0.
022200                 20  CRT-PRM-CUR-ADC PIC X        VALUE '1'.
022300                 20  CRT-PRM-CUR-BCL PIC X        VALUE '1'.
022400                 20  CRT-PRM-CUR-ADJ1 PIC S9V9(8) COMP-3 VALUE +0.
022500                 20  CRT-PRM-CUR-ADJ2 PIC S9V9(8) COMP-3 VALUE +0.
022600                 20  CRT-PRM-CUR-ADJ3 PIC S9V9(8) COMP-3 VALUE +0.
022700                 20  CRT-PRM-CUR-ADJ4 PIC S9V9(8) COMP-3 VALUE +0.
022800                 20  CRT-PRM-CUR-ADJ5 PIC S9V9(8) COMP-3 VALUE +0.
022900             15  CRT-PRM-CUR-LMT1   PIC S9(13)V99 COMP-3 VALUE +0.
023000             15  CRT-PRM-CUR-LMT2   PIC S9(13)V99 COMP-3 VALUE +0.
023100             15  CRT-PRM-CUR-LMT3   PIC S9(13)V99 COMP-3 VALUE +0.
023200             15  CRT-PRM-CUR-LMT4   PIC S9(13)V99 COMP-3 VALUE +0.
023300         10  CRT-PRM-PREV-DATA.
023400             15  CRT-PRM-PREV-DATE    PIC X(8)   VALUE SPACES.
023500             15  CRT-PRM-PREV-RATES.
023600                 20  CRT-PRM-PREV-ANN PIC S9V9(8) COMP-3 VALUE +0.
023700                 20  CRT-PRM-PREV-DAF PIC SVP9(15)  COMP-3
023800                                                 VALUE +0.
023900                 20  CRT-PRM-PREV-ADC PIC X      VALUE '1'.
024000                 20  CRT-PRM-PREV-BCL PIC X      VALUE '1'.
024100                 20  CRT-PRM-PREV-ADJ1 PIC S9V9(8) COMP-3
024200                                                 VALUE +0.
024300                 20  CRT-PRM-PREV-ADJ2 PIC S9V9(8) COMP-3
024400                                                 VALUE +0.
024500                 20  CRT-PRM-PREV-ADJ3 PIC S9V9(8) COMP-3
024600                                                 VALUE +0.
024700                 20  CRT-PRM-PREV-ADJ4 PIC S9V9(8) COMP-3
024800                                                 VALUE +0.
024900                 20  CRT-PRM-PREV-ADJ5 PIC S9V9(8) COMP-3
025000                                                 VALUE +0.
025100             15  CRT-PRM-PREV-LMT1 PIC S9(13)V99 COMP-3 VALUE +0.
025200             15  CRT-PRM-PREV-LMT2 PIC S9(13)V99 COMP-3 VALUE +0.
025300             15  CRT-PRM-PREV-LMT3 PIC S9(13)V99 COMP-3 VALUE +0.
025400             15  CRT-PRM-PREV-LMT4 PIC S9(13)V99 COMP-3 VALUE +0.
025500
025600     05  CRT-SPL-RATES.
025700         10  CRT-SPL-RATE-KEY-LEVEL   PIC X         VALUE SPACE.
025800         10  CRT-SPL-CUR-DATA.
025900             15  CRT-SPL-CUR-DATE     PIC X(8)      VALUE SPACES.
026000             15  CRT-SPL-CUR-RATES1   COMP-3.
026100                 20  CRT-SPL-CUR-ANN1 PIC S9V9(8)   VALUE +.0.
026200                 20  CRT-SPL-CUR-DAF1 PIC SVP9(15)  VALUE +.0.
026300             15  CRT-SPL-CUR-RATES2   COMP-3.
026400                 20  CRT-SPL-CUR-ANN2 PIC S9V9(8)   VALUE +.0.
026500                 20  CRT-SPL-CUR-DAF2 PIC SVP9(15)  VALUE +.0.
026600             15  CRT-SPL-CUR-RATES3   COMP-3.
026700                 20  CRT-SPL-CUR-ANN3 PIC S9V9(8)   VALUE +.0.
026800                 20  CRT-SPL-CUR-DAF3 PIC SVP9(15)  VALUE +.0.
026900             15  CRT-SPL-CUR-LMT1 PIC S9(13)V99 COMP-3 VALUE +0.
027000             15  CRT-SPL-CUR-LMT2 PIC S9(13)V99 COMP-3 VALUE +0.
027100         10  CRT-SPL-PREV-DATA.
027200             15  CRT-SPL-PREV-DATE     PIC X(8)      VALUE SPACES.
027300             15  CRT-SPL-PREV-RATES1   COMP-3.
027400                 20  CRT-SPL-PREV-ANN1 PIC S9V9(8)   VALUE +.0.
027500                 20  CRT-SPL-PREV-DAF1 PIC SVP9(15)  VALUE +.0.
027600             15  CRT-SPL-PREV-RATES2   COMP-3.
027700                 20  CRT-SPL-PREV-ANN2 PIC S9V9(8)   VALUE +.0.
027800                 20  CRT-SPL-PREV-DAF2 PIC SVP9(15)  VALUE +.0.
027900             15  CRT-SPL-PREV-RATES3   COMP-3.
028000                 20  CRT-SPL-PREV-ANN3 PIC S9V9(8)   VALUE +.0.
028100                 20  CRT-SPL-PREV-DAF3 PIC SVP9(15)  VALUE +.0.
028200             15  CRT-SPL-PREV-LMT1 PIC S9(13)V99 COMP-3 VALUE +0.
028300             15  CRT-SPL-PREV-LMT2 PIC S9(13)V99 COMP-3 VALUE +0.
028400         10  FILLER                    PIC X(34)     VALUE SPACES.
028500
028600     05  CRT-LN-RATES.
028700         10  CRT-LN-RATE-KEY-LEVEL   PIC X         VALUE SPACE.
028800         10  CRT-LN-CUR-DATA.
028900             15  CRT-LN-CUR-DATE     PIC X(8)      VALUE SPACES.
029000             15  CRT-LN-CUR-RATES    COMP-3.
029100                 20  CRT-LN-CUR-ANN  PIC S9V9(8)   VALUE +.0.
029200                 20  CRT-LN-CUR-DAF  PIC SVP9(15)  VALUE +.0.
029300         10  CRT-LN-PREV-DATA.
029400             15  CRT-LN-PREV-DATE    PIC X(8)      VALUE SPACES.
029500             15  CRT-LN-PREV-RATES   COMP-3.
029600                 20  CRT-LN-PREV-ANN PIC S9V9(8)   VALUE +.0.
029700                 20  CRT-LN-PREV-DAF PIC SVP9(15)  VALUE +.0.
029800         10  FILLER                  PIC X(118)    VALUE SPACES.
029900
030000     05  CRT-SPL-OLD-RATES.
030100         10  CRT-SPL-OLD-RATE-KEY-LEVEL   PIC X    VALUE SPACE.
030200         10  CRT-SPL-OLD-CUR-DATA.
030300             15  CRT-SPL-OLD-CUR-DATE     PIC X(8) VALUE SPACES.
030400             15  CRT-SPL-OLD-CUR-RATES1   COMP-3.
030500                 20  CRT-SPL-OLD-CUR-ANN1 PIC S9V9(8)   VALUE +.0.
030600                 20  CRT-SPL-OLD-CUR-DAF1 PIC SVP9(15)  VALUE +.0.
030700             15  CRT-SPL-OLD-CUR-RATES2   COMP-3.
030800                 20  CRT-SPL-OLD-CUR-ANN2 PIC S9V9(8)   VALUE +.0.
030900                 20  CRT-SPL-OLD-CUR-DAF2 PIC SVP9(15)  VALUE +.0.
031000             15  CRT-SPL-OLD-CUR-RATES3   COMP-3.
031100                 20  CRT-SPL-OLD-CUR-ANN3 PIC S9V9(8)   VALUE +.0.
031200                 20  CRT-SPL-OLD-CUR-DAF3 PIC SVP9(15)  VALUE +.0.
031300             15  CRT-SPL-OLD-CUR-LMT1 PIC S9(13)V99 COMP-3
031400                                                        VALUE +0.
031500             15  CRT-SPL-OLD-CUR-LMT2 PIC S9(13)V99 COMP-3
031600                                                        VALUE +0.
031700         10  CRT-SPL-OLD-PREV-DATA.
031800             15  CRT-SPL-OLD-PREV-DATE     PIC X(8) VALUE SPACES.
031900             15  CRT-SPL-OLD-PREV-RATES1   COMP-3.
032000                 20  CRT-SPL-OLD-PREV-ANN1 PIC S9V9(8) VALUE +.0.
032100                 20  CRT-SPL-OLD-PREV-DAF1 PIC SVP9(15)
032200                                                       VALUE +.0.
032300             15  CRT-SPL-OLD-PREV-RATES2   COMP-3.
032400                 20  CRT-SPL-OLD-PREV-ANN2 PIC S9V9(8) VALUE +.0.
032500                 20  CRT-SPL-OLD-PREV-DAF2 PIC SVP9(15)
032600                                                       VALUE +.0.
032700             15  CRT-SPL-OLD-PREV-RATES3   COMP-3.
032800                 20  CRT-SPL-OLD-PREV-ANN3 PIC S9V9(8) VALUE +.0.
032900                 20  CRT-SPL-OLD-PREV-DAF3 PIC SVP9(15)
033000                                                       VALUE +.0.
033100             15  CRT-SPL-OLD-PREV-LMT1 PIC S9(13)V99 COMP-3
033200                                                       VALUE +0.
033300             15  CRT-SPL-OLD-PREV-LMT2 PIC S9(13)V99 COMP-3
033400                                                       VALUE +0.
033500
033600     05  CRT-LN-OLD-RATES.
033700         10  CRT-LN-OLD-RATE-KEY-LEVEL   PIC X    VALUE SPACE.
033800         10  CRT-LN-OLD-CUR-DATA.
033900             15  CRT-LN-OLD-CUR-DATE     PIC X(8) VALUE SPACES.
034000             15  CRT-LN-OLD-CUR-RATES    COMP-3.
034100                 20  CRT-LN-OLD-CUR-ANN  PIC S9V9(8)    VALUE +.0.
034200                 20  CRT-LN-OLD-CUR-DAF  PIC SVP9(15)   VALUE +.0.
034300         10  CRT-LN-OLD-PREV-DATA.
034400             15  CRT-LN-OLD-PREV-DATE    PIC X(8) VALUE SPACES.
034500             15  CRT-LN-OLD-PREV-RATES   COMP-3.
034600                 20  CRT-LN-OLD-PREV-ANN PIC S9V9(8)    VALUE +.0.
034700                 20  CRT-LN-OLD-PREV-DAF PIC SVP9(15)   VALUE +.0.
034800         10  FILLER                      PIC X(84) VALUE SPACES.
034900
035000     05  CRT-FED-TAX-RATES.                                       2012254
035100         10  CRT-FED-TAX-RATE-KEY-LEVEL  PIC X       VALUE SPACE. 2012254
035200         10  CRT-FED-TAX-CUR-DATE        PIC X(8)    VALUE SPACES.2012254
035300         10  CRT-FED-TAX-CUR-ANN1        PIC S9V9(8)   COMP-3     2012254
035400                                                       VALUE +.0. 2012254
035500         10  CRT-FED-TAX-CUR-ANN2        PIC S9V9(8)   COMP-3     2012254
035600                                                       VALUE +.0. 2012254
035700         10  CRT-FED-TAX-CUR-ANN3        PIC S9V9(8)   COMP-3     2012254
035800                                                       VALUE +.0. 2012254
035900         10  CRT-FED-TAX-CUR-ANN4        PIC S9V9(8)   COMP-3     2012254
036000                                                       VALUE +.0. 2012254
036100         10  CRT-FED-TAX-CUR-ANN5        PIC S9V9(8)   COMP-3     2012254
036200                                                       VALUE +.0. 2012254
036300         10  CRT-FED-TAX-CUR-ANN6        PIC S9V9(8)   COMP-3     2012254
036400                                                       VALUE +.0. 2012254
036500         10  CRT-FED-TAX-CUR-ANN7        PIC S9V9(8)   COMP-3     2012254
036600                                                       VALUE +.0. 2012254
036700         10  CRT-FED-TAX-CUR-ANN8        PIC S9V9(8)   COMP-3     2012254
036800                                                       VALUE +.0. 2012254
036900         10  CRT-FED-TAX-CUR-ANN9        PIC S9V9(8)   COMP-3     2012254
037000                                                       VALUE +.0. 2012254
037100         10  CRT-FED-TAX-CUR-LMT1        PIC S9(13)V99 COMP-3     2012254
037200                                                       VALUE +0.  2012254
037300         10  CRT-FED-TAX-CUR-LMT2        PIC S9(13)V99 COMP-3     2012254
037400                                                       VALUE +0.  2012254
037500         10  CRT-FED-TAX-CUR-LMT3        PIC S9(13)V99 COMP-3     2012254
037600                                                       VALUE +0.  2012254
037700         10  CRT-FED-TAX-CUR-LMT4        PIC S9(13)V99 COMP-3     2012254
037800                                                       VALUE +0.  2012254
037900         10  CRT-FED-TAX-CUR-LMT5        PIC S9(13)V99 COMP-3     2012254
038000                                                       VALUE +0.  2012254
038100         10  CRT-FED-TAX-CUR-LMT6        PIC S9(13)V99 COMP-3     2012254
038200                                                       VALUE +0.  2012254
038300         10  CRT-FED-TAX-CUR-LMT7        PIC S9(13)V99 COMP-3     2012254
038400                                                       VALUE +0.  2012254
038500         10  CRT-FED-TAX-CUR-LMT8        PIC S9(13)V99 COMP-3     2012254
038600                                                       VALUE +0.  2012254
038610                                                                  2012254
038700     05  CRT-ST-TAX-RATES.                                        2012254
038800         10  CRT-ST-TAX-RATE-KEY-LEVEL  PIC X       VALUE SPACE.  2012254
038900         10  CRT-ST-TAX-CUR-DATE        PIC X(8)    VALUE SPACES. 2012254
039000         10  CRT-ST-TAX-CUR-ANN1        PIC S9V9(8)   COMP-3      2012254
039100                                                      VALUE +.0.  2012254
039200         10  CRT-ST-TAX-CUR-ANN2        PIC S9V9(8)   COMP-3      2012254
039300                                                      VALUE +.0.  2012254
039400         10  CRT-ST-TAX-CUR-ANN3        PIC S9V9(8)   COMP-3      2012254
039500                                                      VALUE +.0.  2012254
039600         10  CRT-ST-TAX-CUR-ANN4        PIC S9V9(8)   COMP-3      2012254
039700                                                      VALUE +.0.  2012254
039800         10  CRT-ST-TAX-CUR-ANN5        PIC S9V9(8)   COMP-3      2012254
039900                                                      VALUE +.0.  2012254
040000         10  CRT-ST-TAX-CUR-ANN6        PIC S9V9(8)   COMP-3      2012254
040100                                                      VALUE +.0.  2012254
040200         10  CRT-ST-TAX-CUR-ANN7        PIC S9V9(8)   COMP-3      2012254
040300                                                      VALUE +.0.  2012254
040400         10  CRT-ST-TAX-CUR-ANN8        PIC S9V9(8)   COMP-3      2012254
040500                                                      VALUE +.0.  2012254
040600         10  CRT-ST-TAX-CUR-ANN9        PIC S9V9(8)   COMP-3      2012254
040700                                                      VALUE +.0.  2012254
040800         10  CRT-ST-TAX-CUR-LMT1        PIC S9(13)V99 COMP-3      2012254
040900                                                      VALUE +0.   2012254
041000         10  CRT-ST-TAX-CUR-LMT2        PIC S9(13)V99 COMP-3      2012254
041100                                                      VALUE +0.   2012254
041200         10  CRT-ST-TAX-CUR-LMT3        PIC S9(13)V99 COMP-3      2012254
041300                                                      VALUE +0.   2012254
041400         10  CRT-ST-TAX-CUR-LMT4        PIC S9(13)V99 COMP-3      2012254
041500                                                      VALUE +0.   2012254
041600         10  CRT-ST-TAX-CUR-LMT5        PIC S9(13)V99 COMP-3      2012254
041700                                                      VALUE +0.   2012254
041800         10  CRT-ST-TAX-CUR-LMT6        PIC S9(13)V99 COMP-3      2012254
041900                                                      VALUE +0.   2012254
042000         10  CRT-ST-TAX-CUR-LMT7        PIC S9(13)V99 COMP-3      2012254
042100                                                      VALUE +0.   2012254
042200         10  CRT-ST-TAX-CUR-LMT8        PIC S9(13)V99 COMP-3      2012254
042300                                                      VALUE +0.   2012254
042310                                                                  2012254
042400     05  CRT-LOC-TAX-RATES.                                       2012254
042500         10  CRT-LOC-TAX-RATE-KEY-LEVEL  PIC X       VALUE SPACE. 2012254
042600         10  CRT-LOC-TAX-CUR-DATE        PIC X(8)    VALUE SPACES.2012254
042700         10  CRT-LOC-TAX-CUR-ANN1        PIC S9V9(8)   COMP-3     2012254
042800                                                       VALUE +.0. 2012254
042900         10  CRT-LOC-TAX-CUR-ANN2        PIC S9V9(8)   COMP-3     2012254
043000                                                       VALUE +.0. 2012254
043100         10  CRT-LOC-TAX-CUR-ANN3        PIC S9V9(8)   COMP-3     2012254
043200                                                       VALUE +.0. 2012254
043300         10  CRT-LOC-TAX-CUR-ANN4        PIC S9V9(8)   COMP-3     2012254
043400                                                       VALUE +.0. 2012254
043500         10  CRT-LOC-TAX-CUR-ANN5        PIC S9V9(8)   COMP-3     2012254
043600                                                       VALUE +.0. 2012254
043700         10  CRT-LOC-TAX-CUR-ANN6        PIC S9V9(8)   COMP-3     2012254
043800                                                       VALUE +.0. 2012254
043900         10  CRT-LOC-TAX-CUR-ANN7        PIC S9V9(8)   COMP-3     2012254
044000                                                       VALUE +.0. 2012254
044100         10  CRT-LOC-TAX-CUR-ANN8        PIC S9V9(8)   COMP-3     2012254
044200                                                       VALUE +.0. 2012254
044300         10  CRT-LOC-TAX-CUR-ANN9        PIC S9V9(8)   COMP-3     2012254
044400                                                       VALUE +.0. 2012254
044500         10  CRT-LOC-TAX-CUR-LMT1        PIC S9(13)V99 COMP-3     2012254
044600                                                       VALUE +0.  2012254
044700         10  CRT-LOC-TAX-CUR-LMT2        PIC S9(13)V99 COMP-3     2012254
044800                                                       VALUE +0.  2012254
044900         10  CRT-LOC-TAX-CUR-LMT3        PIC S9(13)V99 COMP-3     2012254
045000                                                       VALUE +0.  2012254
045100         10  CRT-LOC-TAX-CUR-LMT4        PIC S9(13)V99 COMP-3     2012254
045200                                                       VALUE +0.  2012254
045300         10  CRT-LOC-TAX-CUR-LMT5        PIC S9(13)V99 COMP-3     2012254
045400                                                       VALUE +0.  2012254
045500         10  CRT-LOC-TAX-CUR-LMT6        PIC S9(13)V99 COMP-3     2012254
045600                                                       VALUE +0.  2012254
045700         10  CRT-LOC-TAX-CUR-LMT7        PIC S9(13)V99 COMP-3     2012254
045800                                                       VALUE +0.  2012254
045900         10  CRT-LOC-TAX-CUR-LMT8        PIC S9(13)V99 COMP-3     2012254
046000                                                       VALUE +0.  2012254
