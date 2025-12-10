*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100******************************************************************
000200*  RATE HISTORY HOLD WORK COPYBOOK                               *
000300*        RATE HISTORY HOLD AREA                                  *
000400******************************************************************
000500 01  RATE-HIST-HOLD.                                              CMJ3625
000600     03  RHH-FUND-RATES.                                          CMJ3625
000700         05  RHH-FUND-DATA OCCURS 130 TIMES.                      2011446
000800             07  RHH-FUND-DT.                                     CMJ3625
000900                 09  RHH-FND-CENT    PIC XX.                      CMJ3625
001000                 09  RHH-FND-YR      PIC XX.                      CMJ3625
001100                 09  RHH-FND-MO      PIC XX.                      CMJ3625
001200                 09  RHH-FND-DA      PIC XX.                      CMJ3625
001300             07  RHH-FUND            COMP-3.                      CMJ3625
001400                 09  RHH-FND-RT      OCCURS 9 TIMES.              CMJ3625
001500                     11  RHH-FND-ANN PIC S9V9(8).                 9915845
001700                     11  RHH-FND-DAF PIC SVP9(15).                9915845
001800                 09  RHH-FND-LMT     PIC S9(13)V99                9915845
001900                                     OCCURS 8 TIMES.              CMJ3625
002000     03  RHH-PTR-RATES REDEFINES RHH-FUND-RATES.                  CMJ3625
002100         05  RHH-PTR-DATA OCCURS 130 TIMES.                       2011446
002200             07  RHH-PTR-DT.                                      CMJ3625
002300                 09  RHH-PTR-CENT    PIC XX.                      CMJ3625
002400                 09  RHH-PTR-YR      PIC XX.                      CMJ3625
002500                 09  RHH-PTR-MO      PIC XX.                      CMJ3625
002600                 09  RHH-PTR-DA      PIC XX.                      CMJ3625
002700             07  RHH-RATE            COMP-3.                      CMJ3625
002800                 09  RHH-PTR-ANN     PIC S9V9(8).                 9915845
003000                 09  RHH-PTR-DAF     PIC SVP9(15).                9915845
003100         05  FILLER                  PIC X(21840).                2011446
003200     03  RHH-SPLT-RATES REDEFINES RHH-FUND-RATES.                 CMJ3625
003300         05  RHH-SPLT-DATA OCCURS 130 TIMES.                      2011446
003400             07  RHH-SPLT-DT.                                     CMJ3625
003500                 09  RHH-SPL-CENT    PIC XX.                      CMJ3625
003600                 09  RHH-SPL-YR      PIC XX.                      CMJ3625
003700                 09  RHH-SPL-MO      PIC XX.                      CMJ3625
003800                 09  RHH-SPL-DA      PIC XX.                      CMJ3625
003900             07  RHH-SPLIT           COMP-3.                      CMJ3625
004000                 09  RHH-SPL-RT      OCCURS 3 TIMES.              CMJ3625
004100                     11  RHH-SPL-ANN PIC S9V9(8).                 9915845
004300                     11  RHH-SPL-DAF PIC SVP9(15).                9915845
004400                 09  RHH-SPL-LMT     PIC S9(13)V99                9915845
004500                                     OCCURS 2 TIMES.              CMJ3625
004600         05  FILLER                  PIC X(16380).                2011446
004700     03  RHH-PRIME-RATES REDEFINES RHH-FUND-RATES.                CMJ3625
004800         05  RHH-PRM-DATA OCCURS 130 TIMES.                       2011446
004900             07  RHH-PRM-DT.                                      CMJ3625
005000                 09  RHH-PRM-CENT    PIC XX.                      CMJ3625
005100                 09  RHH-PRM-YR      PIC XX.                      CMJ3625
005200                 09  RHH-PRM-MO      PIC XX.                      CMJ3625
005300                 09  RHH-PRM-DA      PIC XX.                      CMJ3625
005400             07  RHH-PRIME.                                       CMJ3625
005500                 09  RHH-PRM-ANN     PIC S9V9(8)     COMP-3.      9915845
005700                 09  RHH-PRM-DAF     PIC SVP9(15)    COMP-3.      9915845
005800                 09  RHH-ADJ-CODE    PIC X.                       CMJ3625
005900                 09  RHH-BAL-CALC    PIC X.                       CMJ3625
006000                 09  RHH-PRM-ADJ     PIC S9V9(8)     COMP-3       9915845
006100                                     OCCURS 5 TIMES.              CMJ3625
006200                 09  RHH-PRM-LMT     PIC S9(13)V99   COMP-3       9915845
006300                                     OCCURS 4 TIMES.              CMJ3625
006400         05  FILLER                  PIC X(14170).                2011446
006500     03  RNX-FUND-RATES.                                          CMJ3625
006600         05  RNX-FUND-DATA.                                       CMJ3625
006700             07  RNX-FUND-DT.                                     CMJ3625
006800                 09  RNX-FND-CENT    PIC XX.                      CMJ3625
006900                 09  RNX-FND-YR      PIC XX.                      CMJ3625
007000                 09  RNX-FND-MO      PIC XX.                      CMJ3625
007100                 09  RNX-FND-DA      PIC XX.                      CMJ3625
007200             07  RNX-FUND            COMP-3.                      CMJ3625
007300                 09  RNX-FND-RT      OCCURS 9 TIMES.              CMJ3625
007400                     11  RNX-FND-ANN PIC S9V9(8).                 9915845
007600                     11  RNX-FND-DAF PIC SVP9(15).                9915845
007700                 09  RNX-FND-LMT     PIC S9(13)V99                9915845
007800                                     OCCURS 8 TIMES.              CMJ3625
007900     03  RNX-PTR-RATES REDEFINES RNX-FUND-RATES.                  CMJ3625
008000         05  RNX-PTR-DATA.                                        CMJ3625
008100             07  RNX-PTR-DT.                                      CMJ3625
008200                 09  RNX-PTR-CENT    PIC XX.                      CMJ3625
008300                 09  RNX-PTR-YR      PIC XX.                      CMJ3625
008400                 09  RNX-PTR-MO      PIC XX.                      CMJ3625
008500                 09  RNX-PTR-DA      PIC XX.                      CMJ3625
008600             07  RNX-RATE            COMP-3.                      CMJ3625
008700                 09  RNX-PTR-ANN     PIC S9V9(8).                 9915845
008900                 09  RNX-PTR-DAF     PIC SVP9(15).                9915845
009000         05  FILLER                  PIC X(168).                  9915845
009100     03  RNX-SPLT-RATES REDEFINES RNX-FUND-RATES.                 CMJ3625
009200         05  RNX-SPLT-DATA.                                       CMJ3625
009300             07  RNX-SPLT-DT.                                     CMJ3625
009400                 09  RNX-SPL-CENT    PIC XX.                      CMJ3625
009500                 09  RNX-SPL-YR      PIC XX.                      CMJ3625
009600                 09  RNX-SPL-MO      PIC XX.                      CMJ3625
009700                 09  RNX-SPL-DA      PIC XX.                      CMJ3625
009800             07  RNX-SPLIT           COMP-3.                      CMJ3625
009900                 09  RNX-SPL-RT      OCCURS 3 TIMES.              CMJ3625
010000                     11  RNX-SPL-ANN PIC S9V9(8).                 9915845
010200                     11  RNX-SPL-DAF PIC SVP9(15).                9915845
010300                 09  RNX-SPL-LMT     PIC S9(13)V99                9915845
010400                                     OCCURS 2 TIMES.              CMJ3625
010500         05  FILLER                  PIC X(126).                  9915845
010600     03  RNX-PRIME-RATES REDEFINES RNX-FUND-RATES.                CMJ3625
010700         05  RNX-PRM-DATA.                                        CMJ3625
010800             07  RNX-PRM-DT.                                      CMJ3625
010900                 09  RNX-PRM-CENT    PIC XX.                      CMJ3625
011000                 09  RNX-PRM-YR      PIC XX.                      CMJ3625
011100                 09  RNX-PRM-MO      PIC XX.                      CMJ3625
011200                 09  RNX-PRM-DA      PIC XX.                      CMJ3625
011300             07  RNX-PRIME.                                       CMJ3625
011400                 09  RNX-PRM-ANN     PIC S9V9(8)     COMP-3.      9915845
011600                 09  RNX-PRM-DAF     PIC SVP9(15)    COMP-3.      9915845
011700                 09  RNX-ADJ-CODE    PIC X.                       CMJ3625
011800                 09  RNX-BAL-CALC    PIC X.                       CMJ3625
011900                 09  RNX-PRM-ADJ     PIC S9V9(8)     COMP-3       9915845
012000                                     OCCURS 5 TIMES.              CMJ3625
012100                 09  RNX-PRM-LMT     PIC S9(13)V99   COMP-3       9915845
012200                                     OCCURS 4 TIMES.              CMJ3625
012300         05  FILLER                  PIC X(109).                  9915845
012400                                                                  CMJ3625
