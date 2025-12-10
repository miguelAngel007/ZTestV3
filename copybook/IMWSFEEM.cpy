*     * FO5238*12/05/11 JCTE PROYECTO UPGRADE SYSTEMAT
000100*----------------------------------------------------------------*
000200*  MULTIPLE PRICE FEE TABLE FILE                                 *
000300*----------------------------------------------------------------*
000400 01  MULTIPLE-PRICE-FEE-TABLE.
000500     05  WMF-CONTROL-KEY.
000600         10  WMF-CONTROL-1           PIC  X(02).
000610         10  WMF-CURRENCY            PIC  X(03).                  9915845
000700         10  WMF-SC-REGION           PIC  X(10).
000800         10  WMF-CHG-TYPE            PIC  X(03).
000900         10  WMF-FEE-TYPE            PIC  X.
001000     05  WMF-FEE-AREA.
001100         10  WMF-FEE-DATA            OCCURS 200 TIMES
001200                                     ASCENDING KEY
001300                                     WMF-FEE-NUMBER
001400                                     INDEXED BY FEE-INDX.
001500             15  WMF-FEE-NUMBER      PIC  X(03).
001600             15  WMF-FEE-DESC        PIC  X(24).
001700             15  WMF-CHG-FLAG        PIC  X.
001800             15  WMF-WVE-FLAG        PIC  X.
001900             15  WMF-TIER-TYPE       PIC  X.
002000             15  WMF-TIERS           OCCURS  4 TIMES
002100                                     INDEXED BY TIER-INDX.
002200                 20  WMF-TIER-CTR    PIC S9(07)     COMP-3.       9915845
002300                 20  WMF-TIER-AMT    PIC S9(13)V99  COMP-3.       9915845
002400             15  WMF-TISA-IND        PIC  X.
002500             15  WMF-ANALYSIS        PIC  X.
002600             15  WMF-STMT-SYM        PIC  X(02).
002700             15  WMF-CONTROL-FEE.
002800                 20  WMF-CNTL-FEE-TYP PIC X.
002900                 20  WMF-CNTL-FEE-NBR PIC X(03).
003000             15  WMF-MIN-IND         PIC  X.
003100             15  WMF-PARENT-IND      PIC  X.
003200             15  WMF-EFF-DATE.
003300                 20  WMF-EFF-MM      PIC  X(02).
003400                 20  WMF-EFF-DD      PIC  X(02).
003500                 20  WMF-EFF-YY      PIC  X(02).
003600             15  FILLER              PIC  X(10).
